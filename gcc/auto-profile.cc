/* Read and annotate call graph profile from the auto profile data file.
   Copyright (C) 2014-2025 Free Software Foundation, Inc.
   Contributed by Dehao Chen (dehao@google.com)

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#include "config.h"
#define INCLUDE_MAP
#define INCLUDE_SET
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "tree.h"
#include "gimple.h"
#include "predict.h"
#include "alloc-pool.h"
#include "tree-pass.h"
#include "ssa.h"
#include "cgraph.h"
#include "gcov-io.h"
#include "diagnostic-core.h"
#include "profile.h"
#include "langhooks.h"
#include "context.h"
#include "pass_manager.h"
#include "cfgloop.h"
#include "tree-cfg.h"
#include "tree-cfgcleanup.h"
#include "tree-into-ssa.h"
#include "gimple-iterator.h"
#include "value-prof.h"
#include "symbol-summary.h"
#include "sreal.h"
#include "ipa-cp.h"
#include "ipa-prop.h"
#include "ipa-fnsummary.h"
#include "ipa-inline.h"
#include "tree-inline.h"
#include "auto-profile.h"
#include "tree-pretty-print.h"
#include "gimple-pretty-print.h"
#include "output.h"

/* The following routines implement AutoFDO optimization.

   This optimization uses sampling profiles to annotate basic block counts
   and uses heuristics to estimate branch probabilities.

   There are three phases in AutoFDO:

   Phase 1: At startup.
     Read profile from the profile data file.
     The following info is read from the profile datafile:
        * string_table: a map between function name and its index.
        * autofdo_source_profile: a map from function_instance name to
          function_instance. This is represented as a forest of
          function_instances.
        * WorkingSet: a histogram of how many instructions are covered for a
          given percentage of total cycles. This is describing the binary
          level information (not source level). This info is used to help
          decide if we want aggressive optimizations that could increase
          code footprint (e.g. loop unroll etc.)
     A function instance is an instance of function that could either be a
     standalone symbol, or a clone of a function that is inlined into another
     function.

   Phase 2: In afdo_offline pass.
     Remove function instances from other translation units
     and offline all cross-translation unit inlining done during train
     run compilation.  This is necessary to not lose profiles with
     LTO train run.

   Phase 3: During early optimization.
     AFDO inline + value profile transformation.
     This happens during early optimization.
     During early inlining AFDO inliner is executed which
     uses autofdo_source_profile to find if a callsite is:
        * inlined in the profiled binary.
        * callee body is hot in the profiling run.
     If both condition satisfies, early inline will inline the callsite
     regardless of the code growth.

     Performing this early has benefit of doing early optimizations
     before read IPA passes and getting more "context sensitivity" of
     the profile read.  Profile of inlined functions may differ
     significantly from one inline instance to another and from the
     offline version.

     This is controlled by -fauto-profile-inlining and is independent
     of -fearly-inlining.

   Phase 4: In AFDO pass.
     Offline all functions that has been inlined in the
     train run but were not inlined in early inlining nor AFDO
     inline.

   Phase 5: In AFDO pass.
     Annotate control flow graph.
        * Annotate basic block count
        * Estimate branch probability
	* Use earlier static profile to fill in the gaps
	  if AFDO profile is ambiguous

   After the above 5 phases, all profile is readily annotated on the GCC IR.
   AutoFDO tries to reuse all FDO infrastructure as much as possible to make
   use of the profile. E.g. it uses existing mechanism to calculate the basic
   block/edge frequency, as well as the cgraph node/edge count.
*/

#define DEFAULT_AUTO_PROFILE_FILE "fbdata.afdo"
#define AUTO_PROFILE_VERSION 2

/* profile counts determined by AFDO smaller than afdo_hot_bb_threshold are
   considered cols.  */
gcov_type afdo_hot_bb_threshold = -1;

/* Return true if COUNT is possibly hot.  */
bool
maybe_hot_afdo_count_p (profile_count count)
{
  gcc_checking_assert (count.ipa ().initialized_p ());
  return count.ipa ().to_gcov_type () >= afdo_hot_bb_threshold;
}

/* Return true if location of STMT may be expressed by debug info.  */

static bool
stmt_loc_used_by_debug_info (gimple *stmt)
{
  /* Only inline_entry and gimple_bind's locations
     are not output into debug output.  */
  if (is_gimple_debug (stmt))
    return gimple_debug_begin_stmt_p (stmt);
  if (gimple_code (stmt) == GIMPLE_LABEL
      || gimple_code (stmt) == GIMPLE_NOP
      || gimple_code (stmt) == GIMPLE_PREDICT)
    return false;
  if (gimple_clobber_p (stmt))
    return false;
  return true;
}

namespace autofdo
{

/* Intermediate edge info used when propagating AutoFDO profile information.
   We can't edge->count() directly since it's computed from edge's probability
   while probability is yet not decided during propagation.  */
#define AFDO_EINFO(e)                     ((class edge_info *) e->aux)
class edge_info
{
public:
  edge_info () : count_ (profile_count::zero ().afdo ()), annotated_ (false) {}
  bool is_annotated () const { return annotated_; }
  void set_annotated () { annotated_ = true; }
  profile_count get_count () const { return count_; }
  void set_count (profile_count count) { count_ = count; }
private:
  profile_count count_;
  bool annotated_;
};

/* Represent a source location: (function_decl, lineno).  */
struct decl_lineno
{
  tree decl;
  /* Relative locations stored in auto-profile.  */
  unsigned int afdo_loc;
  /* Actual location afdo_loc was computed from used to output diagnostics.  */
  location_t location;
};

/* Represent an inline stack. vector[0] is the leaf node.  */
typedef auto_vec<decl_lineno, 20> inline_stack;

/* String array that stores function names.  */
typedef auto_vec<char *> string_vector;

/* Map from function name's index in string_table to target's
   execution count.  */
typedef std::map<unsigned, gcov_type> icall_target_map;

/* Set of gimple stmts. Used to track if the stmt has already been promoted
   to direct call.  */
typedef std::set<gimple *> stmt_set;

/* Set and map used to translate name indexes.  */
typedef hash_set<int_hash <int, -1, -2>> name_index_set;
typedef hash_map<int_hash <int, -1, -2>, int> name_index_map;

/* Represent count info of an inline stack.  */
class count_info
{
public:
  /* Sampled count of the inline stack.  */
  gcov_type count;

  /* Map from indirect call target to its sample count.  */
  icall_target_map targets;

  /* Whether this inline stack is already used in annotation.

     Each inline stack should only be used to annotate IR once.
     This will be enforced when instruction-level discriminator
     is supported.  */
};

/* operator< for "const char *".  */
struct string_compare
{
  bool operator()(const char *a, const char *b) const
  {
    return strcmp (a, b) < 0;
  }
};

/* Store a string array, indexed by string position in the array.  */
class string_table
{
public:
  string_table ()
  {}

  ~string_table ();

  /* For a given string, returns its index.  */
  int get_index (const char *name) const;

  /* For a given decl, returns the index of the decl name.  */
  int get_index_by_decl (tree decl) const;

  /* For a given index, returns the string.  */
  const char *get_name (int index) const;

  /* Read profile, return TRUE on success.  */
  bool read ();

  /* Return number of entries.  */
  size_t num_entries ()
  {
    return vector_.length ();
  }

  /* Add new name and return its index.  */
  int add_name (char *);

  /* Return cgraph node corresponding to given name index.  */
  cgraph_node *get_cgraph_node (int);
private:
  typedef std::map<const char *, unsigned, string_compare> string_index_map;
  string_vector vector_;
  string_index_map map_;
};

/* Profile of a function instance:
     1. total_count of the function.
     2. head_count (entry basic block count) of the function (only valid when
        function is a top-level function_instance, i.e. it is the original copy
        instead of the inlined copy).
     3. map from source location (decl_lineno) to profile (count_info).
     4. map from callsite to callee function_instance.  */
class function_instance
{
public:
  typedef auto_vec<function_instance *> function_instance_stack;

  /* Read the profile and return a function_instance with head count as
     HEAD_COUNT. Recursively read callsites to create nested function_instances
     too. STACK is used to track the recursive creation process.  */
  static function_instance *
  read_function_instance (function_instance_stack *stack,
                          gcov_type head_count);

  /* Recursively deallocate all callsites (nested function_instances).  */
  ~function_instance ();

  /* Accessors.  */
  int
  name () const
  {
    return name_;
  }
  int
  set_name (int index)
  {
    return name_ = index;
  }
  gcov_type
  total_count () const
  {
    return total_count_;
  }

  /* Return head count or -1 if unknown.  */
  gcov_type
  head_count () const
  {
    return head_count_;
  }

  /* Traverse callsites of the current function_instance to find one at the
     location of LINENO and callee name represented in DECL.
     LOCATION should match LINENO and is used to output diagnostics.  */
  function_instance *get_function_instance_by_decl (unsigned lineno,
						    tree decl,
						    location_t location) const;

  /* Merge profile of clones.  Note that cloning hasn't been performed when
     we annotate the CFG (at this stage).  */
  void merge (function_instance *other,
	      vec <function_instance *> &new_functions);

  /* Look for inline instances that was not realized and
     remove them while possibly merging them to offline variants.  */
  void offline_if_not_realized (vec <function_instance *> &new_functions);

  /* Match function instance with gimple body.  */
  bool match (cgraph_node *node, vec <function_instance *> &new_functions,
	      name_index_map &to_symbol_name);

  /* Offline all inlined functions with name in SEEN.
     If new toplevel functions are created, add them to NEW_FUNCTIONS.  */
  void offline_if_in_set (name_index_set &seen,
			  vec <function_instance *> &new_functions);

  /* Walk inlined functions and if their name is not in SEEN
     remove it.  */

  void remove_external_functions (name_index_set &seen,
				  name_index_map &to_symbol_name,
				  vec <function_instance *> &new_functions);

  /* Store the profile info for LOC in INFO. Return TRUE if profile info
     is found.  */
  bool get_count_info (location_t loc, count_info *info) const;

  /* Read the inlined indirect call target profile for STMT in FN and store it
     in MAP, return the total count for all inlined indirect calls.  */
  gcov_type find_icall_target_map (tree fn, gcall *stmt,
				   icall_target_map *map) const;

  /* Remove inlined indirect call target profile for STMT in FN.  */
  void remove_icall_target (tree fn, gcall *stmt);

  /* Mark LOC as annotated.  */
  void mark_annotated (location_t loc);

  void dump (FILE *f, int indent = 0, bool nested = false) const;

  void dump_inline_stack (FILE *f) const;

  DEBUG_FUNCTION void debug () const;

  /* Mark function as removed from indir target list.  */
  void
  remove_icall_target ()
  {
    removed_icall_target_ = true;
  }

  /* Return true if function is removed from indir target list.  */
  bool
  removed_icall_target ()
  {
    return removed_icall_target_;
  }

  /* Set inlined_to pointer.  */
  void
  set_inlined_to (function_instance *inlined_to)
  {
    gcc_checking_assert (inlined_to != this);
    inlined_to_ = inlined_to;
  }

  /* Return pointer to the function instance this function is inlined
     to or NULL if it is outer instance.  */
  function_instance *
  inlined_to () const
  {
    return inlined_to_;
  }

  /* Mark function as realized.  */
  void
  set_realized ()
  {
    realized_ = true;
  }

  /* Return true if function is realized.  */
  bool
  realized_p ()
  {
    return realized_;
  }

  /* Mark function as in_worklist.  */
  void
  set_in_worklist ()
  {
    gcc_checking_assert (!inlined_to_ && !in_worklist_p ());
    in_worklist_ = true;
  }

  void
  clear_in_worklist ()
  {
    gcc_checking_assert (!inlined_to_ && in_worklist_p ());
    in_worklist_ = false;
  }


  /* Return true if function is in_worklist.  */
  bool
  in_worklist_p ()
  {
    return in_worklist_;
  }

  /* Return corresponding cgraph node.  */
  cgraph_node *get_cgraph_node ();

  void
  set_location (location_t l)
  {
    gcc_checking_assert (location_ == UNKNOWN_LOCATION);
    location_= l;
  }

  location_t
  get_location ()
  {
    return location_;
  }

  void
  set_call_location (location_t l)
  {
    gcc_checking_assert (call_location_ == UNKNOWN_LOCATION
			 && l != UNKNOWN_LOCATION);
    call_location_= l;
  }

  location_t
  get_call_location ()
  {
    return call_location_;
  }

  /* Lookup count and warn about duplicates.  */
  count_info *lookup_count (location_t loc, inline_stack &stack,
			    cgraph_node *node);
private:
  /* Callsite, represented as (decl_lineno, callee_function_name_index).  */
  typedef std::pair<unsigned, unsigned> callsite;

  /* Map from callsite to callee function_instance.  */
  typedef std::map<callsite, function_instance *> callsite_map;

  function_instance (unsigned name, gcov_type head_count)
	  : name_ (name), total_count_ (0), head_count_ (head_count),
      removed_icall_target_ (false), realized_ (false),
      in_worklist_ (false), inlined_to_ (NULL),
      location_ (UNKNOWN_LOCATION), call_location_ (UNKNOWN_LOCATION)
  {
  }

  /* Map from source location (decl_lineno) to profile (count_info).  */
  typedef std::map<unsigned, count_info> position_count_map;

  /* function_instance name index in the string_table.  */
  unsigned name_;

  /* Total sample count.  */
  gcov_type total_count_;

  /* Entry BB's sample count.  */
  gcov_type head_count_;

  /* Map from callsite location to callee function_instance.  */
  callsite_map callsites;

  /* Map from source location to count_info.  */
  position_count_map pos_counts;

  /* True if function was removed from indir target list.  */
  bool removed_icall_target_;

  /* True if function exists in IL.  I.e. for toplevel instance we
     have corresponding symbol and for inline instance we inlined
     to it.  */
  bool realized_;

  /* True if function is in worklist for merging/offlining.  */
  bool in_worklist_;

  /* Pointer to outer function instance or NULL if this
     is a toplevel one.  */
  function_instance *inlined_to_;

  /* Location of function and its call (in case it is inlined).  */
  location_t location_, call_location_;

  /* Turn inline instance to offline.  */
  static bool offline (function_instance *fn,
		       vec <function_instance *> &new_functions);
};

/* Profile for all functions.  */
class autofdo_source_profile
{
public:
  static autofdo_source_profile *
  create ()
  {
    autofdo_source_profile *map = new autofdo_source_profile ();

    if (map->read ())
      return map;
    delete map;
    return NULL;
  }

  ~autofdo_source_profile ();

  /* For a given DECL, returns the top-level function_instance.  */
  function_instance *get_function_instance_by_decl (tree decl) const;

  /* For a given name index, returns the top-level function_instance.  */
  function_instance *get_function_instance_by_name_index (int) const;

  void add_function_instance (function_instance *);

  /* Find count_info for a given gimple STMT. If found, store the count_info
     in INFO and return true; otherwise return false.
     NODE can be used to specify particular inline clone.  */
  bool get_count_info (gimple *stmt, count_info *info,
		       cgraph_node *node = NULL) const;

  /* Find count_info for a given gimple location GIMPLE_LOC. If found,
     store the count_info in INFO and return true; otherwise return false.
     NODE can be used to specify particular inline clone.  */
  bool get_count_info (location_t gimple_loc, count_info *info,
		       cgraph_node *node = NULL) const;

  /* Find total count of the callee of EDGE.  */
  gcov_type get_callsite_total_count (struct cgraph_edge *edge) const;

  /* Update value profile INFO for STMT within NODE from the inlined indirect
     callsite.  Return true if INFO is updated.  */
  bool update_inlined_ind_target (gcall *stmt, count_info *info,
				  cgraph_node *node);

  void remove_icall_target (cgraph_edge *e);

  /* Offline all functions not defined in the current translation unit.  */
  void offline_external_functions ();

  void offline_unrealized_inlines ();
private:
  /* Map from function_instance name index (in string_table) to
     function_instance.  */
  typedef std::map<unsigned, function_instance *> name_function_instance_map;

  autofdo_source_profile () {}

  /* Read AutoFDO profile and returns TRUE on success.  */
  bool read ();

  /* Return the function_instance in the profile that correspond to the
     inline STACK.  */
  function_instance *
  get_function_instance_by_inline_stack (const inline_stack &stack) const;

  name_function_instance_map map_;

  auto_vec <function_instance *> duplicate_functions_;
};

/* Store the strings read from the profile data file.  */
static string_table *afdo_string_table;

/* Store the AutoFDO source profile.  */
static autofdo_source_profile *afdo_source_profile;

/* gcov_summary structure to store the profile_info.  */
static gcov_summary *afdo_profile_info;

/* Scaling factor for afdo data.  Compared to normal profile
   AFDO profile counts are much lower, depending on sampling
   frequency.  We scale data up to reduce effects of roundoff
   errors.  */

static gcov_type afdo_count_scale = 1;

/* Helper functions.  */


/* Return the original name of NAME: strip the suffix that starts
   with '.' for names that are generated after auto-profile pass.
   This is to match profiled names with the names in the IR at this stage.
   Note that we only have to strip suffix and not in the middle.
   Caller is responsible for freeing RET.  */

static char *
get_original_name (const char *name, bool alloc = true)
{
  char *ret = alloc ? xstrdup (name) : const_cast<char *> (name);
  char *last_dot = strrchr (ret, '.');
  if (last_dot == NULL)
    return ret;
  bool only_digits = true;
  char *ptr = last_dot;
  while (*(++ptr) != 0)
    if (*ptr < '0' || *ptr > '9')
      {
	only_digits = false;
	break;
      }
  if (only_digits)
    *last_dot = 0;
  char *next_dot = strrchr (ret, '.');
  /* if nested function such as foo.0, return foo.0  */
  if (next_dot == NULL)
    {
      *last_dot = '.';
      return ret;
    }
  /* Suffixes of clones that compiler generates after auto-profile.  */
  const char *suffixes[] = {"isra", "constprop", "lto_priv", "part", "cold"};
  for (unsigned i = 0; i < sizeof (suffixes) / sizeof (const char *); ++i)
    {
      int len = strlen (suffixes[i]);
      if (len == last_dot - next_dot - 1
	  && strncmp (next_dot + 1, suffixes[i], strlen (suffixes[i])) == 0)
	{
	  *next_dot = 0;
	  return get_original_name (ret, false);
	}
    }
  /* Otherwise, it is for clones such as .omp_fn.N that was done before
     auto-profile and should be kept as it is.  */
  *last_dot = '.';
  return ret;
}

/* Return the combined location, which is a 32bit integer in which
   higher 16 bits stores the line offset of LOC to the start lineno
   of DECL, The lower 16 bits stores the discriminator.  */

static unsigned
get_combined_location (location_t loc, tree decl)
{
  bool warned = false;
  /* TODO: allow more bits for line and less bits for discriminator.  */
  if ((LOCATION_LINE (loc) - DECL_SOURCE_LINE (decl)) >= (1<<15)
      || (LOCATION_LINE (loc) - DECL_SOURCE_LINE (decl)) <= -(1<<15))
    warned = warning_at (loc, OPT_Wauto_profile,
			 "auto-profile cannot encode offset %i "
			 "that exceeds 16 bytes",
			 LOCATION_LINE (loc) - DECL_SOURCE_LINE (decl));
  if (warned)
    inform (DECL_SOURCE_LOCATION (decl), "location offset is related to");
  if ((unsigned)get_discriminator_from_loc (loc) >= (1u << 16))
    warning_at (loc, OPT_Wauto_profile,
		"auto-profile cannot encode discriminators "
		"that exceeds 16 bytes");
  return ((unsigned)(LOCATION_LINE (loc) - DECL_SOURCE_LINE (decl)) << 16)
	 | get_discriminator_from_loc (loc);
}

/* Return the function decl of a given lexical BLOCK.  */

static tree
get_function_decl_from_block (tree block)
{
  if (!inlined_function_outer_scope_p (block))
    return NULL_TREE;

  return BLOCK_ABSTRACT_ORIGIN (block);
}

/* Dump LOC to F.  */

static void
dump_afdo_loc (FILE *f, unsigned loc)
{
  if (loc & 65535)
    fprintf (f, "%i.%i", loc >> 16, loc & 65535);
  else
    fprintf (f, "%i", loc >> 16);
}

/* Return assembler name as in symbol table and DW_AT_linkage_name.  */

static const char *
raw_symbol_name (const char *asmname)
{
  /* If we start supporting user_label_prefixes, add_linkage_attr will also
     need to be fixed.  */
  if (strlen (user_label_prefix))
    sorry ("auto-profile is not supported for targets with user label prefix");
  return asmname + (asmname[0] == '*');
}

/* Convenience wrapper that looks up assembler name.  */

static const char *
raw_symbol_name (tree decl)
{
  return raw_symbol_name (IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (decl)));
}

/* Dump STACK to F.  */

static void
dump_inline_stack (FILE *f, inline_stack *stack)
{
  bool first = true;
  for (decl_lineno &p : *stack)
    {
      fprintf (f, "%s%s:",
	       first ? "" : "; ",
	       raw_symbol_name (p.decl));
      dump_afdo_loc (f, p.afdo_loc);
      first = false;
    }
  fprintf (f, "\n");
}

/* Store inline stack for STMT in STACK.  */

static void
get_inline_stack (location_t locus, inline_stack *stack,
		  tree fn = current_function_decl)
{
  if (LOCATION_LOCUS (locus) == UNKNOWN_LOCATION)
    return;

  tree block = LOCATION_BLOCK (locus);
  if (block && TREE_CODE (block) == BLOCK)
    {
      for (block = BLOCK_SUPERCONTEXT (block);
           block && (TREE_CODE (block) == BLOCK);
           block = BLOCK_SUPERCONTEXT (block))
        {
          location_t tmp_locus = BLOCK_SOURCE_LOCATION (block);
          if (LOCATION_LOCUS (tmp_locus) == UNKNOWN_LOCATION)
            continue;

          tree decl = get_function_decl_from_block (block);
          stack->safe_push (
	      {decl, get_combined_location (locus, decl), locus});
          locus = tmp_locus;
        }
    }
  stack->safe_push ({fn, get_combined_location (locus, fn), locus});
}

/* Same as get_inline_stack for a given node which may be
   an inline clone.  If NODE is NULL, assume current_function_decl.  */
static void
get_inline_stack_in_node (location_t locus, inline_stack *stack,
			  cgraph_node *node)
{
  if (!node)
    return get_inline_stack (locus, stack);
  do
    {
      get_inline_stack (locus, stack, node->decl);
      /* If caller is inlined, continue building stack.  */
      if (!node->inlined_to)
	node = NULL;
      else
	{
	  locus = gimple_location (node->callers->call_stmt);
	  node = node->callers->caller;
	}
    }
  while (node);
}

/* Return combined location of LOCUS within BLOCK that is in
   function FN.

   This is a 32bit integer in which higher 16 bits stores the line offset of
   LOC to the start lineno of DECL, The lower 16 bits stores the
   discriminator.  */

static unsigned
get_relative_location_for_locus (tree fn, tree block, location_t locus)
{
  if (LOCATION_LOCUS (locus) == UNKNOWN_LOCATION)
    return -1;

  for (; block && (TREE_CODE (block) == BLOCK);
       block = BLOCK_SUPERCONTEXT (block))
    if (inlined_function_outer_scope_p (block))
      return get_combined_location (locus,
				    get_function_decl_from_block (block));
  return get_combined_location (locus, fn);
}

/* Return combined location of STMT in function FN.  */

static unsigned
get_relative_location_for_stmt (tree fn, gimple *stmt)
{
  return get_relative_location_for_locus
	  (fn, LOCATION_BLOCK (gimple_location (stmt)),
	   gimple_location (stmt));
}

/* Member functions for string_table.  */

/* Deconstructor.  */

string_table::~string_table ()
{
  for (unsigned i = 0; i < vector_.length (); i++)
    free (vector_[i]);
}


/* Return the index of a given function NAME. Return -1 if NAME is not
   found in string table.  */

int
string_table::get_index (const char *name) const
{
  if (name == NULL)
    return -1;
  string_index_map::const_iterator iter = map_.find (name);
  if (iter == map_.end ())
    return -1;

  return iter->second;
}

/* Return the index of a given function DECL. Return -1 if DECL is not
   found in string table.  */

int
string_table::get_index_by_decl (tree decl) const
{
  const char *name = raw_symbol_name (decl);
  int ret = get_index (name);
  if (ret != -1)
    return ret;
  if (DECL_FROM_INLINE (decl))
    return get_index_by_decl (DECL_ABSTRACT_ORIGIN (decl));

  return -1;
}

/* Return the function name of a given INDEX.  */

const char *
string_table::get_name (int index) const
{
  gcc_assert (index > 0 && index < (int)vector_.length ());
  return vector_[index];
}

/* Add new name SRRING and return its index.  */

int
string_table::add_name (char *string)
{
  vector_.safe_push (string);
  map_[vector_.last ()] = vector_.length () - 1;
  return vector_.length () - 1;
}

/* Read the string table. Return TRUE if reading is successful.  */

bool
string_table::read ()
{
  if (gcov_read_unsigned () != GCOV_TAG_AFDO_FILE_NAMES)
    return false;
  /* Skip the length of the section.  */
  gcov_read_unsigned ();
  /* Read in the file name table.  */
  unsigned string_num = gcov_read_unsigned ();
  vector_.reserve (string_num);
  for (unsigned i = 0; i < string_num; i++)
    {
      vector_.quick_push (xstrdup (gcov_read_string ()));
      map_[vector_.last ()] = i;
      if (gcov_is_error ())
	return false;
    }
  return true;
}

/* Return cgraph node corresponding to given NAME_INDEX,
   NULL if unavailable.  */
cgraph_node *
string_table::get_cgraph_node (int name_index)
{
  const char *sname = get_name (name_index);

  symtab_node *n = cgraph_node::get_for_asmname (get_identifier (sname));
  for (;n; n = n->next_sharing_asm_name)
    if (cgraph_node *cn = dyn_cast <cgraph_node *> (n))
      if (cn->definition && cn->has_gimple_body_p ())
	return cn;
  return NULL;
}

/* Return corresponding cgraph node.  */

cgraph_node *
function_instance::get_cgraph_node ()
{
  return afdo_string_table->get_cgraph_node (name ());
}

/* Member functions for function_instance.  */

function_instance::~function_instance ()
{
  gcc_assert (!in_worklist_p ());
  for (callsite_map::iterator iter = callsites.begin ();
       iter != callsites.end (); ++iter)
    delete iter->second;
}

/* Traverse callsites of the current function_instance to find one at the
   location of LINENO and callee name represented in DECL.  */

function_instance *
function_instance::get_function_instance_by_decl (unsigned lineno,
						  tree decl,
						  location_t location) const
{
  int func_name_idx = afdo_string_table->get_index_by_decl (decl);
  if (func_name_idx != -1)
    {
      callsite_map::const_iterator ret
          = callsites.find (std::make_pair (lineno, func_name_idx));
      if (ret != callsites.end ())
        return ret->second;
    }
  if (DECL_FROM_INLINE (decl))
    {
      function_instance
       	*ret =  get_function_instance_by_decl (lineno,
						DECL_ABSTRACT_ORIGIN (decl),
						location);
      return ret;
    }
  if (dump_enabled_p ())
    {
      for (auto const &iter : callsites)
	if (iter.first.first == lineno)
	  dump_printf_loc (MSG_NOTE | MSG_PRIORITY_INTERNALS,
			   dump_user_location_t::from_location_t (location),
			   "auto-profile has mismatched function name %s"
			   " instead of %s at loc %i:%i",
			   afdo_string_table->get_name (iter.first.second),
			   raw_symbol_name (decl),
			   lineno >> 16,
			   lineno & 65535);
    }

  return NULL;
}

/* Merge profile of OTHER to THIS.  Note that cloning hasn't been performed
   when we annotate the CFG (at this stage).  */

void
function_instance::merge (function_instance *other,
			  vec <function_instance *> &new_functions)
{
  /* Do not merge to itself and only merge functions of same name.  */
  gcc_checking_assert (other != this && other->name () == name ());
  total_count_ += other->total_count_;
  if (other->total_count () && total_count () && other->head_count () == -1)
    head_count_ = -1;
  else if (head_count_ != -1)
    head_count_ += other->head_count_;

  bool changed = true;

  while (changed)
    {
      changed = false;
      /* If both function instances agree on particular inlined function,
	 merge profiles. Otherwise offline the instance.  */
      for (callsite_map::const_iterator iter = other->callsites.begin ();
	   iter != other->callsites.end ();)
	if (callsites.count (iter->first) == 0)
	  {
	    function_instance *f = iter->second;
	    if (dump_file)
	      {
		fprintf (dump_file, "  Mismatch in inlined functions;"
			 " offlining in merge source:");
		f->dump_inline_stack (dump_file);
		fprintf (dump_file, "\n");
	      }
	    /* We already merged outer part of the function accounting
	       the inlined call; compensate.  */
	    for (function_instance *s = this; s; s = s->inlined_to ())
	      {
		s->total_count_ -= f->total_count ();
		gcc_checking_assert (s->total_count_ >= 0);
	      }
	    other->callsites.erase (iter);
	    function_instance::offline (f, new_functions);
	    /* Start from beginning as merging might have offlined
	       some functions in the case of recursive inlining.  */
	    iter = other->callsites.begin ();
	  }
	else
	  ++iter;
      for (callsite_map::const_iterator iter = callsites.begin ();
	   iter != callsites.end ();)
	if (other->callsites.count (iter->first) == 0)
	  {
	    function_instance *f = iter->second;
	    if (dump_file)
	      {
		fprintf (dump_file, "  Mismatch in inlined functions;"
			 " offlining in merge destination:");
		f->dump_inline_stack (dump_file);
		fprintf (dump_file, "\n");
	      }
	    callsites.erase (iter);
	    function_instance::offline (f, new_functions);
	    iter = callsites.begin ();
	    changed = true;
	  }
	else
	  ++iter;
    }
  for (callsite_map::const_iterator iter = other->callsites.begin ();
       iter != other->callsites.end (); ++iter)
    {
      if (dump_file)
	{
	  fprintf (dump_file, "    Merging profile for inlined function\n"
		   "      from: ");
	  iter->second->dump_inline_stack (dump_file);
	  fprintf (dump_file, " total:%" PRIu64 "\n      to  : ",
		   (int64_t)iter->second->total_count ());
	  callsites[iter->first]->dump_inline_stack (dump_file);
	  fprintf (dump_file, " total:%" PRIu64 "\n",
		   (int64_t)callsites[iter->first]->total_count ());
	}

      callsites[iter->first]->merge (iter->second, new_functions);
    }

  for (position_count_map::const_iterator iter = other->pos_counts.begin ();
       iter != other->pos_counts.end (); ++iter)
    if (pos_counts.count (iter->first) == 0)
      pos_counts[iter->first] = iter->second;
    else
      {
        pos_counts[iter->first].count += iter->second.count;
	for (icall_target_map::const_iterator titer
	       = iter->second.targets.begin ();
	     titer != iter->second.targets.end (); ++titer)
	  if (pos_counts[iter->first].targets.count (titer->first) == 0)
	    pos_counts[iter->first].targets[titer->first]
	      = titer->second;
	  else
	    pos_counts[iter->first].targets[titer->first]
	      += titer->second;
      }
}

/* Make inline function FN offline.
   If toplevel function of same name already exists, then merge profiles.
   Otherwise turn FN toplevel.  Return true if new toplevel function
   was introduced.
   If new toplevel functions are created and NEW_FUNCTIONS != NULL,
   add them to NEW_FUNCTIONS.

   TODO: When offlining indirect call we lose information about the
   call target.  It should be possible to add it into
   targets histogram.  */

bool
function_instance::offline (function_instance *fn,
			    vec <function_instance *> &new_functions)
{
  gcc_checking_assert (fn->inlined_to ());
  for (function_instance *s = fn->inlined_to (); s; s = s->inlined_to ())
    {
      s->total_count_ -= fn->total_count ();
      gcc_checking_assert (s->total_count_ >= 0);
    }
  function_instance *to
    = afdo_source_profile->get_function_instance_by_name_index (fn->name ());
  fn->set_inlined_to (NULL);
  /* If there is offline function of same name, we need to merge profile.
     Delay this by adding function to a worklist so we do not run into
     problem with recursive inlining.  */
  if (to)
    {
      if (fn->in_worklist_p ())
	return false;
      fn->set_in_worklist ();
      new_functions.safe_push (fn);
      if (dump_file)
	{
	  fprintf (dump_file, "  Recoding duplicate: ");
	  to->dump_inline_stack (dump_file);
	  fprintf (dump_file, "\n");
	}
      return true;
    }
  if (dump_file)
    {
      fprintf (dump_file, "  Added as offline instance: ");
      fn->dump_inline_stack (dump_file);
      fprintf (dump_file, "\n");
    }
  if (fn->total_count ())
    fn->head_count_ = -1;
  afdo_source_profile->add_function_instance (fn);
  fn->set_in_worklist ();
  new_functions.safe_push (fn);
  return true;
}

/* Offline all inlined functions with name in SEEN.
   If new toplevel functions are created, add them to NEW_FUNCTIONS.  */

void
function_instance::offline_if_in_set (name_index_set &seen,
				      vec <function_instance *> &new_functions)
{
  for (callsite_map::const_iterator iter = callsites.begin ();
       iter != callsites.end ();)
    if (seen.contains (iter->first.second))
      {
	function_instance *f = iter->second;
	if (dump_file)
	  {
	    fprintf (dump_file, "Offlining function inlined to other module: ");
	    f->dump_inline_stack (dump_file);
	    fprintf (dump_file, "\n");
	  }
	iter = callsites.erase (iter);
	function_instance::offline (f, new_functions);
	/* Start from beginning as merging might have offlined
	   some functions in the case of recursive inlining.  */
	iter = callsites.begin ();
      }
    else
      {
	iter->second->offline_if_in_set (seen, new_functions);
	++iter;
      }
}

/* Try to check if inlined_fn can correspond to a call of function N.
   Return non-zero if it corresponds and 2 if renaming was done.  */

static int
match_with_target (cgraph_node *n,
		   gimple *stmt,
		   function_instance *inlined_fn,
		   cgraph_node *orig_callee)
{
  cgraph_node *callee = orig_callee->ultimate_alias_target ();
  const char *symbol_name = raw_symbol_name (callee->decl);
  const char *name = afdo_string_table->get_name (inlined_fn->name ());
  if (strcmp (name, symbol_name))
    {
      int i;
      bool in_suffix = false;
      for (i = 0; i; i++)
	{
	  if (name[i] != symbol_name[i])
	    break;
	  if (name[i] == '.')
	    in_suffix = true;
	}
      /* Accept dwarf names and stripped suffixes.  */
      if (!strcmp (lang_hooks.dwarf_name (callee->decl, 0),
		   afdo_string_table->get_name (inlined_fn->name ()))
	  || (!name[i] && symbol_name[i] == '.')
	  || in_suffix)
	{
	  int index = afdo_string_table->get_index (symbol_name);
	  if (index == -1)
	    index = afdo_string_table->add_name (xstrdup (symbol_name));
	  if (dump_file)
	    fprintf (dump_file,
		     "  Renaming inlined call target %s to %s\n",
		     name, symbol_name);
	  inlined_fn->set_name (index);
	  return 2;
	}
      /* Only warn about declarations.  It is possible that the function
	 is declared as alias in other module and we inlined cross-module.  */
      if (callee->definition
	  && warning (OPT_Wauto_profile,
		      "auto-profile of %q+F contains inlined "
		      "function with symbol name %s instead of symbol name %s",
		      n->decl, name, symbol_name))
	inform (gimple_location (stmt), "corresponding call");
      return 0;
    }
  return 1;
}

static void
dump_stmt (gimple *stmt, count_info *info, function_instance *inlined_fn,
	   inline_stack &stack)
{
  if (dump_file)
    {
      fprintf (dump_file, "  ");
      if (!stack.length ())
	fprintf (dump_file, "                     ");
      else
	{
	  gcc_checking_assert (stack.length () == 1);
	  fprintf (dump_file, "%5i", stack[0].afdo_loc >> 16);
	  if (stack[0].afdo_loc & 65535)
	    fprintf (dump_file, ".%-5i", stack[0].afdo_loc & 65535);
	  else
	    fprintf (dump_file, "      ");
	  if (info)
	    fprintf (dump_file, "%9" PRIu64 " ", (int64_t)info->count);
	  else if (inlined_fn)
	    fprintf (dump_file, " inlined  ");
	  else
	    fprintf (dump_file, " no info  ");
	}
      print_gimple_stmt (dump_file, stmt, 0, TDF_SLIM);
    }
}

/* Lookup count and warn about duplicates.  */
count_info *
function_instance::lookup_count (location_t loc, inline_stack &stack,
				 cgraph_node *node)
{
  gcc_checking_assert (stack.length () < 2);
  if (stack.length ())
    {
      int c = pos_counts.count (stack[0].afdo_loc);
      if (c > 1
	  && warning (OPT_Wauto_profile,
		      "duplicated count information"
		      " in auto-profile of %q+F"
		      " with relative location %i discriminator %i",
		      node->decl, stack[0].afdo_loc >> 16,
		      stack[0].afdo_loc & 65535))
	  inform (loc, "corresponding source location");
      if (c)
	return &pos_counts[stack[0].afdo_loc];
    }
  return NULL;
}

/* Mark expr locations as used.  */
void
mark_expr_locations (function_instance *f, tree t, cgraph_node *node,
		     hash_set<const count_info *> &counts)
{
  inline_stack stack;
  return;
  if (!t)
    return;
  do
    {
      get_inline_stack_in_node (EXPR_LOCATION (t), &stack, node);
      /* FIXME: EXPR_LOCATION does not always originate from current
	 function.  */
      if (stack.length () > 1)
	break;
      count_info *info = f->lookup_count (EXPR_LOCATION (t), stack, node);
      if (info)
	counts.add (info);
      if (handled_component_p (t))
	t = TREE_OPERAND (t, 0);
      else
	break;
    }
  while (true);
}

/* Match function instance with gimple body.
   Report mismatches, attempt to fix them if possible and remove data we will
   not use.

   Set location and call_location so we can output diagnostics and know what
   functions was already matched.  */

bool
function_instance::match (cgraph_node *node,
			  vec <function_instance *> &new_functions,
			  name_index_map &to_symbol_name)
{
  if (get_location () != UNKNOWN_LOCATION)
    return false;
  set_location (DECL_SOURCE_LOCATION (node->decl));
  if (dump_file)
    {
      fprintf (dump_file,
	       "\nMatching gimple function %s with auto profile: ",
	       node->dump_name ());
      dump_inline_stack (dump_file);
      fprintf (dump_file, "\n");
    }
  basic_block bb;
  /* Sets used to track if entires in auto-profile are useful.  */
  hash_set<const count_info *> counts;
  hash_set<const count_info *> targets;
  hash_set<const function_instance *> functions;
  hash_set<const function_instance *> functions_to_offline;

  /* We try to fill in lost disciminator if there is unique call
     with given line number.  This map is used to record them.  */
  hash_map<int_hash <int, -1, -2>,auto_vec <gcall *>> lineno_to_call;
  bool lineno_to_call_computed = false;

  for (tree arg = DECL_ARGUMENTS (node->decl); arg; arg = DECL_CHAIN (arg))
    {
      inline_stack stack;

      get_inline_stack_in_node (DECL_SOURCE_LOCATION (arg), &stack, node);
      count_info *info = lookup_count (DECL_SOURCE_LOCATION (arg), stack, node);
      if (stack.length () && dump_file)
	{
	  gcc_checking_assert (stack.length () == 1);
	  fprintf (dump_file, "%5i", stack[0].afdo_loc >> 16);
	  if (stack[0].afdo_loc & 65535)
	    fprintf (dump_file, "  .%-5i arg", stack[0].afdo_loc & 65535);
	  else
	    fprintf (dump_file, "        arg ");
	  print_generic_expr (dump_file, arg);
	  fprintf (dump_file, "\n");
	}
      if (info)
	counts.add (info);
    }
  FOR_EACH_BB_FN (bb, DECL_STRUCT_FUNCTION (node->decl))
    {
      if (dump_file)
	fprintf (dump_file, " basic block %i\n", bb->index);
      for (gphi_iterator gpi = gsi_start_phis (bb);
	   !gsi_end_p (gpi);
	   gsi_next (&gpi))
	{
	  gphi *phi = gpi.phi ();
	  inline_stack stack;

	  /* We do not assign discriminators to PHI nodes.
	     In case we every start using them, we wil need to
	     update tree-cfg.cc::assign_discriminators.  */
	  gcc_assert (gimple_location (phi) == UNKNOWN_LOCATION);
	  get_inline_stack_in_node (gimple_location (phi), &stack, node);
	  count_info *info = lookup_count (gimple_location (phi), stack, node);
	  gcc_assert (!info);
	  dump_stmt (phi, info, NULL, stack);
	  counts.add (info);
	  for (edge e : bb->succs)
	    {
	      location_t phi_loc
		= gimple_phi_arg_location_from_edge (phi, e);
	      inline_stack stack;
	      get_inline_stack_in_node (phi_loc, &stack, node);
	      count_info *info = lookup_count (phi_loc, stack, node);
	      if (info)
		counts.add (info);
	      gcc_checking_assert (stack.length () < 2);
	      mark_expr_locations (this,
				   gimple_phi_arg_def_from_edge (phi, e),
				   node, counts);
	    }
	}
      /* TODO: goto locuses are not used for BB annotation.  */
      for (edge e : bb->succs)
	{
	  inline_stack stack;
	  get_inline_stack_in_node (e->goto_locus, &stack, node);
	  count_info *info = lookup_count (e->goto_locus, stack, node);
	  if (info)
	    counts.add (info);
	}
      for (gimple_stmt_iterator gsi = gsi_start_bb (bb);
	   !gsi_end_p (gsi); gsi_next (&gsi))
	{
	  inline_stack stack;
	  gimple *stmt = gsi_stmt (gsi);
	  get_inline_stack_in_node (gimple_location (stmt), &stack, node);

	  count_info *info = lookup_count (gimple_location (stmt), stack, node);
	  if (info)
	    counts.add (info);
	  for (unsigned int op = 0; op < gimple_num_ops (stmt); op++)
	    mark_expr_locations (this, gimple_op (stmt, op), node, counts);
	  if (gimple_code (stmt) == GIMPLE_CALL)
	    {
	      function_instance *inlined_fn = NULL;
	      function_instance *inlined_fn_nodisc = NULL;
	      /* Lookup callsite.  */
	      if (stack.length ())
		{
		  int c = 0;
		  int cnodis = 0;
		  for (auto const &iter : callsites)
		    if (iter.first.first == stack[0].afdo_loc)
		      {
			if (!c)
			  inlined_fn = iter.second;
			c++;
		      }
		    /* Discriminators are sometimes lost; try to find the
		       call without discriminator info.  */
		    else if (iter.first.first == (stack[0].afdo_loc & ~65535))
		      {
			if (!cnodis)
			  inlined_fn_nodisc = iter.second;
			cnodis++;
		      }
		  if ((c > 1 || (!c && cnodis > 1))
		      && warning (OPT_Wauto_profile,
				  "duplicated callsite in auto-profile of %q+F"
				  " with relative location %i,"
				  " discriminator %i",
				  node->decl, stack[0].afdo_loc >> 16,
				  stack[0].afdo_loc & 65535))
		    inform (gimple_location (stmt), "corresponding call");
		  if (inlined_fn && info && info->targets.size ()
		      && warning (OPT_Wauto_profile,
				  "both call targets and inline callsite"
				  " information is present in auto-profile"
				  " of function %q+F with relative location"
				  " %i, discriminator %i",
				  node->decl, stack[0].afdo_loc >> 16,
				  stack[0].afdo_loc & 65535))
		    inform (gimple_location (stmt), "corresponding call");
		  tree callee = gimple_call_fndecl (stmt);
		  cgraph_node *callee_node;
		  unsigned int loc = stack[0].afdo_loc;
		  bool lost_discriminator = false;
		  if (!inlined_fn && inlined_fn_nodisc)
		    {
		      if (!lineno_to_call_computed)
			{
			  basic_block bb2;
			  FOR_EACH_BB_FN (bb2,
					  DECL_STRUCT_FUNCTION (node->decl))
			  for (gimple_stmt_iterator gsi2
					  = gsi_start_bb (bb2);
			       !gsi_end_p (gsi2); gsi_next (&gsi2))
			    if (gcall *call
				    = dyn_cast <gcall *> (gsi_stmt (gsi2)))
			      {
				inline_stack stack2;
				get_inline_stack_in_node
				       	(gimple_location (call),
					 &stack2, node);
				if (stack2.length ())
				  lineno_to_call.get_or_insert
				    (stack2[0].afdo_loc >> 16).safe_push (call);
			      }
			  lineno_to_call_computed = true;
			}
		      /* If we can determine lost discriminator uniquely,
			 use it.  */
		      if (lineno_to_call.get
			      (stack[0].afdo_loc >> 16)->length () == 1)
			{
			  if (warning (OPT_Wauto_profile,
				       "auto-profile of %q+F seem to contain"
				       " lost discriminator %i for"
				       " call of %s at relative location %i",
				       node->decl,
				       loc & 65535,
				       afdo_string_table->get_name
					 (inlined_fn_nodisc->name ()),
				       loc >> 16))
			    inform (gimple_location (stmt),
				    "corresponding call");
			  inlined_fn = inlined_fn_nodisc;
			  if (dump_file)
			    fprintf (dump_file, "   Lost discriminator %i\n",
				     loc & 65535);
			  loc = loc & ~65535;
			}
		      lost_discriminator = true;
		    }
		  if (callee && (callee_node = cgraph_node::get (callee)))
		    {
		      if (inlined_fn)
			{
			  int old_name = inlined_fn->name ();
			  int r = match_with_target (node, stmt, inlined_fn,
						     callee_node);
			  if (r == 2)
			    {
			      auto iter = callsites.find ({loc, old_name});
			      gcc_checking_assert (old_name
						   != inlined_fn->name ()
						   && iter != callsites.end ()
						   && iter->second
						      == inlined_fn);
			      callsite key2 = {stack[0].afdo_loc,
						inlined_fn->name ()};
			      callsites.erase (iter);
			      callsites[key2] = inlined_fn;
			    }
			  if (r)
			    functions.add (inlined_fn);
			  else
			    functions_to_offline.add (inlined_fn);
			}

		      if (info && info->targets.size () > 1)
			warning_at (gimple_location (stmt), OPT_Wauto_profile,
				    "auto-profile of %q+F contains multiple"
				    " targets for a direct call with relative"
				    " location %i, discriminator %i",
				    node->decl, stack[0].afdo_loc >> 16,
				    stack[0].afdo_loc & 65535);
		      /* We do not need target profile for direct calls.  */
		      if (info)
			info->targets.clear ();
		    }
		  else
		    {
		      if (inlined_fn
			  && inlined_fn->get_call_location ()
				  != UNKNOWN_LOCATION)
			{
			  if (warning (OPT_Wauto_profile,
				       "function contains two calls of the same"
				       " relative location +%i,"
				       " discriminator %i,"
				       " that leads to lost auto-profile",
				       loc >> 16,
				       loc & 65535))
			    {
			      inform (gimple_location (stmt),
				      "location of the first call");
			      inform (inlined_fn->get_call_location (),
				      "location of the second call");
			    }
			  if (dump_file)
			    fprintf (dump_file,
				     "   Duplicated call location\n");
			  inlined_fn = NULL;
			}
		      if (inlined_fn)
			{
			  inlined_fn->set_call_location
			    (gimple_location (stmt));
			  /* Do renaming if needed so we can look up
			     cgraph node and recurse into inlined function.  */
			  int *newn = to_symbol_name.get (inlined_fn->name ());
			  gcc_checking_assert
			    (!newn || *newn != inlined_fn->name ());
			  if (newn || lost_discriminator)
			    {
			      auto iter = callsites.find
					    ({loc, inlined_fn->name ()});
			      gcc_checking_assert (iter != callsites.end ()
						   && iter->second
						      == inlined_fn);
			      callsite key2 = {stack[0].afdo_loc,
					       newn ? *newn
					       : inlined_fn->name ()};
			      callsites.erase (iter);
			      callsites[key2] = inlined_fn;
			      inlined_fn->set_name (newn ? *newn
						    : inlined_fn->name ());
			    }
			  functions.add (inlined_fn);
			}
		      if (info)
			targets.add (info);
		    }
		}
	      dump_stmt (stmt, info, inlined_fn, stack);
	    }
	  else
	    dump_stmt (stmt, info, NULL, stack);
	}
    }
  bool warned = false;
  for (auto &iter : pos_counts)
    if (iter.second.targets.size ()
	&& counts.contains (&iter.second)
       	&& !targets.contains (&iter.second))
      {
	if (!warned)
	  warned = warning_at
		       (DECL_SOURCE_LOCATION (node->decl),
			OPT_Wauto_profile,
			"auto-profile of %q+F contains indirect call targets"
			" not associated with an indirect call statement",
			node->decl);
	if (warned)
	  inform (DECL_SOURCE_LOCATION (node->decl),
		  "count %" PRIu64
		  " with relative location +%i, discriminator %i",
		  iter.second.count, iter.first >> 16, iter.first & 65535);
	if (dump_file)
	  {
	    fprintf (dump_file, "Removing targets of ");
	    dump_afdo_loc (dump_file, iter.first);
	    fprintf (dump_file, "\n");
	  }
	iter.second.targets.clear ();
      }
  warned = false;
  /* Profile sometimes contains extra location for start or end of function
     (prologue, epilogue).
     TODO: If present, perhaps it can be used to determine entry block
     and exit block counts.  */
  unsigned int end_location = get_combined_location
    (DECL_STRUCT_FUNCTION (node->decl)->function_end_locus, node->decl);
  unsigned int start_location = get_combined_location
    (DECL_STRUCT_FUNCTION (node->decl)->function_start_locus, node->decl);
  /* When outputting code to builtins location we use line number 0.
     create_gcov is stupid and happily computes offsets across files.
     Silently ignore it.  */
  unsigned int zero_location
	  = ((unsigned)(1-DECL_SOURCE_LINE (node->decl))) << 16;
  for (position_count_map::const_iterator iter = pos_counts.begin ();
       iter != pos_counts.end ();)
    if (!counts.contains (&iter->second))
      {
	if (iter->first != end_location
	    && iter->first != start_location
	    && (iter->first & 65535) != zero_location
	    && iter->first
	    /* FIXME: dwarf5 does not represent inline stack of debug
	       statements and consequently create_gcov is sometimes
	       mixing up statements from other functions.  Do not warn
	       user about this until this problem is solved.
	       We still write info into dump file.  */
	    && 0)
	  {
	    if (!warned)
	      warned = warning_at (DECL_SOURCE_LOCATION (node->decl),
			    OPT_Wauto_profile,
			    "auto-profile of %q+F contains extra statements",
			    node->decl);
	    if (warned)
	      inform (DECL_SOURCE_LOCATION (node->decl),
		      "count %" PRIu64 " with relative location +%i,"
		      " discriminator %i",
		      iter->second.count, iter->first >> 16,
		      iter->first & 65535);
	    if ((iter->first >> 16) > (end_location >> 16) && warned)
	      inform (DECL_SOURCE_LOCATION (node->decl),
		      "location is after end of function");
	  }
	if (dump_file)
	  {
	    fprintf (dump_file, "Removing unmatched count ");
	    dump_afdo_loc (dump_file, iter->first);
	    fprintf (dump_file, ":%" PRIu64, iter->second.count);
	    for (auto &titer : iter->second.targets)
	      fprintf (dump_file, " %s:%" PRIu64,
		       afdo_string_table->get_name (titer.first),
		       (int64_t)titer.second);
	    fprintf (dump_file, "\n");
	  }
	iter = pos_counts.erase (iter);
      }
    else
      iter++;
  warned = false;
  for (callsite_map::const_iterator iter = callsites.begin ();
       iter != callsites.end ();)
    if (!functions.contains (iter->second))
      {
	function_instance *f = iter->second;
	/* If we did not see the corresponding statement, warn.  */
	if (!functions_to_offline.contains (iter->second))
	  {
	    if (!warned)
	      warned = warning_at (DECL_SOURCE_LOCATION (node->decl),
				   OPT_Wauto_profile,
				   "auto-profile of %q+F contains"
				   " extra callsites",
				   node->decl);
	    if (warned)
	      inform (DECL_SOURCE_LOCATION (node->decl),
		      "call of %s with total count %" PRId64
		      ", relative location +%i, discriminator %i",
		      afdo_string_table->get_name (iter->first.second),
		      iter->second->total_count (),
		      iter->first.first >> 16, iter->first.first & 65535);
	    if ((iter->first.first >> 16) > (end_location >> 16) && warned)
	      inform (DECL_SOURCE_LOCATION (node->decl),
		      "location is after end of function");
	    if (dump_file)
	      {
		fprintf (dump_file,
			 "Offlining inline with no corresponding gimple stmt ");
		f->dump_inline_stack (dump_file);
		fprintf (dump_file, "\n");
	      }
	  }
	else if (dump_file)
	  {
	    fprintf (dump_file,
		     "Offlining mismatched inline ");
	    f->dump_inline_stack (dump_file);
	    fprintf (dump_file, "\n");
	  }
	callsites.erase (iter);
	offline (f, new_functions);
	iter = callsites.begin ();
      }
    else
      iter++;
  for (auto &iter : callsites)
    if (cgraph_node *n = iter.second->get_cgraph_node ())
      iter.second->match (n, new_functions, to_symbol_name);
  return true;
}

/* Walk inlined functions and if their name is not in SEEN
   remove it.  Also rename function names as given by
   to_symbol_name map.  */

void
function_instance::remove_external_functions
	(name_index_set &seen,
         name_index_map &to_symbol_name,
         vec <function_instance *> &new_functions)
{
  auto_vec <callsite, 20> to_rename;

  for (callsite_map::const_iterator iter = callsites.begin ();
       iter != callsites.end ();)
    if (!seen.contains (iter->first.second))
      {
	function_instance *f = iter->second;
	if (dump_file)
	  {
	    fprintf (dump_file, "  Removing external inline: ");
	    f->dump_inline_stack (dump_file);
	    fprintf (dump_file, "\n");
	  }
	iter = callsites.erase (iter);
	f->set_inlined_to (NULL);
	f->offline_if_in_set (seen, new_functions);
	delete f;
      }
    else
      {
	gcc_checking_assert ((int)iter->first.second
			     == iter->second->name ());
	int *newn = iter->second->get_call_location () == UNKNOWN_LOCATION
		    ? to_symbol_name.get (iter->first.second)
		    : NULL;
	if (newn)
	  {
	    gcc_checking_assert (iter->second->inlined_to ());
	    to_rename.safe_push (iter->first);
	  }
	iter->second->remove_external_functions
	  (seen, to_symbol_name, new_functions);
	++iter;
      }
  for (auto &key : to_rename)
    {
      auto iter = callsites.find (key);
      callsite key2 = key;
      key2.second = *to_symbol_name.get (key.second);
      iter->second->set_name (key2.second);
      callsites.erase (iter);
      callsites[key2] = iter->second;
    }
  auto_vec <int, 20> target_to_rename;
  for (auto &iter : pos_counts)
    {
      for (auto const &titer : iter.second.targets)
	{
	  int *ren = to_symbol_name.get (titer.first);
	  if (ren)
	    target_to_rename.safe_push (titer.first);
	}
      while (target_to_rename.length ())
	{
	  int key = target_to_rename.pop ();
	  int key2 = *to_symbol_name.get (key);
	  auto i = iter.second.targets.find (key);
	  if (iter.second.targets.count (key2) == 0)
	    iter.second.targets[key2] = i->second;
	  else
	    iter.second.targets[key2] += i->second;
	  iter.second.targets.erase (i);
	}
    }
}

/* Look for inline instances that was not realized and
   remove them while possibly merging them to offline variants.  */

void
function_instance::offline_if_not_realized
	(vec <function_instance *> &new_functions)
{
  for (callsite_map::const_iterator iter = callsites.begin ();
       iter != callsites.end ();)
    if (!iter->second->realized_p ())
      {
	function_instance *f = iter->second;
	if (dump_file)
	  {
	    fprintf (dump_file, "Offlining unrealized inline ");
	    f->dump_inline_stack (dump_file);
	    fprintf (dump_file, "\n");
	  }
	iter = callsites.erase (iter);
	offline (f, new_functions);
      }
    else
      {
	iter->second->offline_if_not_realized (new_functions);
	++iter;
      }
}

/* Dump instance to F indented by INDENT.  */

void
function_instance::dump (FILE *f, int indent, bool nested) const
{
  if (!nested)
    fprintf (f, "%*s%s total:%" PRIu64 " head:%" PRId64 "\n",
	     indent, "", afdo_string_table->get_name (name ()),
	     (int64_t)total_count (), (int64_t)head_count ());
  else
    fprintf (f, " total:%" PRIu64 "\n", (int64_t)total_count ());
  for (auto const &iter : pos_counts)
    {
      fprintf (f, "%*s", indent + 2, "");
      dump_afdo_loc (f, iter.first);
      fprintf (f, ": %" PRIu64, (int64_t)iter.second.count);

      for (auto const &titer : iter.second.targets)
	fprintf (f, "  %s:%" PRIu64,
		 afdo_string_table->get_name (titer.first),
		 (int64_t)titer.second);
      fprintf (f,"\n");
    }
  for (auto const &iter : callsites)
    {
      fprintf (f, "%*s", indent + 2, "");
      dump_afdo_loc (f, iter.first.first);
      fprintf (f, ": %s", afdo_string_table->get_name (iter.first.second));
      iter.second->dump (f, indent + 2, true);
      gcc_checking_assert ((int)iter.first.second == iter.second->name ());
    }
}

/* Dump inline path.  */

void
function_instance::dump_inline_stack (FILE *f) const
{
  auto_vec <callsite, 20> stack;
  const function_instance *p = this, *s = inlined_to ();
  while (s)
    {
      bool found = false;
      for (callsite_map::const_iterator iter = s->callsites.begin ();
	   iter != s->callsites.end (); ++iter)
	if (iter->second == p)
	  {
	    gcc_checking_assert (!found
				 && (int)iter->first.second == p->name ());
	    stack.safe_push ({iter->first.first, s->name ()});
	    found = true;
	  }
      gcc_checking_assert (found);
      p = s;
      s = s->inlined_to ();
    }
  for (callsite &s: stack)
    {
      fprintf (f, "%s:", afdo_string_table->get_name (s.second));
      dump_afdo_loc (f, s.first);
      fprintf (f, " ");
    }
  fprintf (f, "%s", afdo_string_table->get_name (name ()));
}

/* Dump instance to stderr.  */

void
function_instance::debug () const
{
  dump (stderr);
}

/* Return profile info for LOC in INFO.  */

bool
function_instance::get_count_info (location_t loc, count_info *info) const
{
  position_count_map::const_iterator iter = pos_counts.find (loc);
  if (iter == pos_counts.end ())
    return false;
  *info = iter->second;
  return true;
}

/* Read the inlined indirect call target profile for STMT and store it in
   MAP, return the total count for all inlined indirect calls.  */

gcov_type
function_instance::find_icall_target_map (tree fn, gcall *stmt,
                                          icall_target_map *map) const
{
  gcov_type ret = 0;
  unsigned stmt_offset = get_relative_location_for_stmt (fn, stmt);

  for (callsite_map::const_iterator iter = callsites.begin ();
       iter != callsites.end (); ++iter)
    {
      unsigned callee = iter->second->name ();
      /* Check if callsite location match the stmt.  */
      if (iter->first.first != stmt_offset
	  || iter->second->removed_icall_target ())
	continue;
      struct cgraph_node *node = cgraph_node::get_for_asmname (
          get_identifier (afdo_string_table->get_name (callee)));
      if (node == NULL)
        continue;
      (*map)[callee] = iter->second->total_count () * afdo_count_scale;
      ret += iter->second->total_count () * afdo_count_scale;
    }
  return ret;
}

/* Remove the inlined indirect call target profile for STMT.  */

void
function_instance::remove_icall_target (tree fn, gcall *stmt)
{
  unsigned stmt_offset = get_relative_location_for_stmt (fn, stmt);
  int n = 0;

  for (auto iter : callsites)
    if (iter.first.first == stmt_offset)
      {
	iter.second->remove_icall_target ();
	n++;
      }
  /* TODO: If we add support for multiple targets, we may want to
     remove only those we succesfully inlined.  */
  gcc_assert (n);
}

/* Offline all functions not defined in the current unit.
   We will not be able to early inline them.
   Doing so early will get VPT decisions more realistic.  */

void
autofdo_source_profile::offline_external_functions ()
{
  /* First check all available definitions and mark their names as
     visible.  */
  cgraph_node *node;
  name_index_set seen;
  name_index_map to_symbol_name;
  size_t last_name;

  /* Add renames erasing suffixes produced by late clones, such as
     .isra, .ipcp.  */
  for (size_t i = 1; i < afdo_string_table->num_entries (); i++)
    {
      const char *n1 = afdo_string_table->get_name (i);
      char *n2 = get_original_name (n1);
      if (!strcmp (n1, n2))
	{
	  free (n2);
	  /* Watch for duplicate entries.
	     This seems to happen in practice and may be useful to distinguish
	     multiple static symbols of the same name, but we do not realy
	     have a way to differentiate them in get_name lookup.  */
	  int index = afdo_string_table->get_index (n1);
	  if (index != (int)i)
	    {
	      if (dump_file)
		fprintf (dump_file,
			 "string table in auto-profile contains"
			 " duplicated name %s\n", n1);
	      to_symbol_name.put (i, index);
	    }
	  continue;
	}
      if (dump_file)
	fprintf (dump_file, "Adding rename removing clone suffixes %s -> %s\n",
		 n1, n2);
      int index = afdo_string_table->get_index (n2);
      if (index != -1)
	free (n2);
      else
	index = afdo_string_table->add_name (n2);
      to_symbol_name.put (i, index);
    }
  last_name = afdo_string_table->num_entries ();
  FOR_EACH_DEFINED_FUNCTION (node)
    {
      const char *name = raw_symbol_name (node->decl);
      const char *dwarf_name = lang_hooks.dwarf_name (node->decl, 0);
      int index = afdo_string_table->get_index (name);

      /* Inline function may be identified by its dwarf names;
	 rename them to symbol names.  With LTO dwarf names are
	 lost in free_lange_data.  */
      if (strcmp (name, dwarf_name))
	{
	  int index2 = afdo_string_table->get_index (dwarf_name);
	  if (index2 != -1)
	    {
	      if (index == -1)
		index = afdo_string_table->add_name (xstrdup (name));
	      if (dump_file)
		{
		  fprintf (dump_file, "Adding dwarf->symbol rename %s -> %s\n",
			   afdo_string_table->get_name (index2), name);
		  if (to_symbol_name.get (index2))
		    fprintf (dump_file, "Dwarf name is not unique");
		}
	      to_symbol_name.put (index2, index);
	      seen.add (index2);
	    }
	}
      if (index != -1)
	{
	  if (dump_file)
	    fprintf (dump_file, "%s is defined in node %s\n",
		     afdo_string_table->get_name (index),
		     node->dump_name ());
	  seen.add (index);
	}
      else
	{
	  if (dump_file)
	    {
	      if (dwarf_name && strcmp (dwarf_name, name))
		fprintf (dump_file,
			 "Node %s not in auto profile (%s neither %s)\n",
			 node->dump_name (),
			 name,
			 dwarf_name);
	      else
		fprintf (dump_file,
			 "Node %s (symbol %s) not in auto profile\n",
			 node->dump_name (),
			 name);
	    }
	}
    }

  for (auto iter : to_symbol_name)
    {
      /* In case dwarf name was duplicated and later renamed,
	 handle both.  No more than one hop should be needed.  */
      int *newn = to_symbol_name.get (iter.second);
      if (newn)
	iter.second = *newn;
      gcc_checking_assert (!to_symbol_name.get (iter.second));
      if (seen.contains (iter.second))
	seen.add (iter.first);
    }

  /* Now process all toplevel (offline) function instances.

     If instance has no definition in this translation unit,
     first offline all inlined functions which are defined here
     (so we do not lose profile due to cross-module inlining
     done by link-time optimizers).

     If instance has a definition, look into all inlined functions
     and remove external ones (result of cross-module inlining).

     TODO: after early-inlining we ought to offline all functions
     that were not inlined.  */
  vec <function_instance *>&fns = duplicate_functions_;
  auto_vec <function_instance *, 20>fns2;
  /* Populate worklist with all functions to process.  Processing
     may introduce new functions by offlining.  */
  for (auto const &iter : map_)
    {
      iter.second->set_in_worklist ();
      fns.safe_push (iter.second);
    }

  /* There are two worklists.  First all functions needs to be matched
     with gimple body and only then we want to do merging, since matching
     should be done on unmodified profile and merging works better if
     mismatches are already resolved both in source and destination.  */
  while (fns.length () || fns2.length ())
    {
      /* In case renaming introduced new name, keep seen up to date.  */
      for (; last_name < afdo_string_table->num_entries (); last_name++)
	{
	  const char *name = afdo_string_table->get_name (last_name);
	  symtab_node *n
	    = afdo_string_table->get_cgraph_node (last_name);
	  if (dump_file)
	    fprintf (dump_file, "New name %s %s\n", name,
		     n ? "wth corresponding definition"
		     : "with no corresponding definition");
	  if (n)
	    seen.add (last_name);
	}
      if (fns.length ())
	{
	  function_instance *f = fns.pop ();
	  if (f->get_location () == UNKNOWN_LOCATION)
	    {
	      int index = f->name ();
	      int *newn = to_symbol_name.get (index);
	      if (newn)
		{
		  f->set_name (*newn);
		  if (map_.count (index)
		      && map_[index] == f)
		    map_.erase (index);
		  if (!map_.count (*newn))
		    map_[*newn] = f;
		}
	      if (cgraph_node *n = f->get_cgraph_node ())
		{
		  gcc_checking_assert (seen.contains (f->name ()));
		  f->match (n, fns, to_symbol_name);
		}
	    }
	  fns2.safe_push (f);
	}
      else
	{
	  function_instance *f = fns2.pop ();
	  int index = f->name ();
	  gcc_checking_assert (f->in_worklist_p ());

	  /* If map has different function_instance of same name, then
	     this is a duplicated entry which needs to be merged.  */
	  if (map_.count (index) && map_[index] != f)
	    {
	      if (dump_file)
		{
		  fprintf (dump_file, "Merging duplicate instance: ");
		  f->dump_inline_stack (dump_file);
		  fprintf (dump_file, "\n");
		}
	      map_[index]->merge (f, fns);
	      gcc_checking_assert (!f->inlined_to ());
	      f->clear_in_worklist ();
	      delete f;
	    }
	  /* If name was not seen in the symbol table, remove it.  */
	  else if (!seen.contains (index))
	    {
	      f->offline_if_in_set (seen, fns);
	      f->clear_in_worklist ();
	      if (dump_file)
		fprintf (dump_file, "Removing external %s\n",
			 afdo_string_table->get_name (f->name ()));
	      if (map_.count (index) && map_[index] == f)
		map_.erase (f->name ());
	      delete f;
	    }
	  /* If this is offline function instance seen in this
	     translation unit offline external inlines and possibly
	     rename from dwarf name.  */
	  else
	    {
	      f->remove_external_functions (seen, to_symbol_name, fns);
	      f->clear_in_worklist ();
	    }
	}
    }
  if (dump_file)
    for (auto const &iter : map_)
      {
	seen.contains (iter.second->name ());
	iter.second->dump (dump_file);
      }
}

/* Walk scope block BLOCK and mark all inlined functions as realized.  */

static void
walk_block (tree fn, function_instance *s, tree block)
{
  if (inlined_function_outer_scope_p (block))
    {
      unsigned loc = get_relative_location_for_locus
		      (fn, BLOCK_SUPERCONTEXT (block),
		       BLOCK_SOURCE_LOCATION (block));
      function_instance *ns
	= s->get_function_instance_by_decl
		  (loc, BLOCK_ABSTRACT_ORIGIN (block),
		   BLOCK_SOURCE_LOCATION (block));
      if (!ns)
	{
	  if (dump_file)
	    {
	      fprintf (dump_file, " Failed to find inlined instance:");
	      s->dump_inline_stack (dump_file);
	      fprintf (dump_file, ":");
	      dump_afdo_loc (dump_file, loc);
	      fprintf (dump_file, " %s\n",
		       raw_symbol_name (BLOCK_ABSTRACT_ORIGIN (block)));
	    }
	  return;
	}
      s = ns;
      if (dump_file)
	{
	  fprintf (dump_file, " Marking realized inline: ");
	  s->dump_inline_stack (dump_file);
	  fprintf (dump_file, "\n");
	}
      s->set_realized ();
    }
  for (tree t = BLOCK_SUBBLOCKS (block); t ; t = BLOCK_CHAIN (t))
    walk_block (fn, s, t);
}

/* Offline all inline functions that are not marked as realized.
   This will merge their profile into offline versions where available.
   Also remove all functions we will no longer use.  */

void
autofdo_source_profile::offline_unrealized_inlines ()
{
  auto_vec <function_instance *>fns;
  /* Populate worklist with all functions to process.  Processing
     may introduce new functions by offlining.  */
  for (auto const &iter : map_)
    {
      fns.safe_push (iter.second);
      iter.second->set_in_worklist ();
    }
  while (fns.length ())
    {
      function_instance *f = fns.pop ();
      int index = f->name ();
      bool in_map = map_.count (index);
      if (in_map)
	if (cgraph_node *n = f->get_cgraph_node ())
	  {
	    if (dump_file)
	      fprintf (dump_file, "Marking realized %s\n",
		       afdo_string_table->get_name (index));
	    f->set_realized ();
	    if (DECL_INITIAL (n->decl)
		&& DECL_INITIAL (n->decl) != error_mark_node)
	      walk_block (n->decl, f, DECL_INITIAL (n->decl));
	  }
      f->offline_if_not_realized (fns);
      gcc_checking_assert ((in_map || !f->realized_p ())
			   && f->in_worklist_p ());

      /* If this is duplicated instance, merge it into one in map.  */
      if (in_map && map_[index] != f)
	{
	  if (dump_file)
	    {
	      fprintf (dump_file, "Merging duplicate instance: ");
	      f->dump_inline_stack (dump_file);
	      fprintf (dump_file, "\n");
	    }
	  map_[index]->merge (f, fns);
	  f->clear_in_worklist ();
	  gcc_checking_assert (!f->inlined_to ());
	  delete f;
	}
      /* If function is not in symbol table, remove it.  */
      else if (!f->realized_p ())
	{
	  if (dump_file)
	    fprintf (dump_file, "Removing optimized out function %s\n",
		     afdo_string_table->get_name (f->name ()));
	  map_.erase (index);
	  f->clear_in_worklist ();
	  delete f;
	}
      else
	f->clear_in_worklist ();
    }
  if (dump_file)
    for (auto const &iter : map_)
      iter.second->dump (dump_file);
}

/* Read the profile and create a function_instance with head count as
   HEAD_COUNT. Recursively read callsites to create nested function_instances
   too. STACK is used to track the recursive creation process.  */

/* function instance profile format:

   ENTRY_COUNT: 8 bytes
   NAME_INDEX: 4 bytes
   NUM_POS_COUNTS: 4 bytes
   NUM_CALLSITES: 4 byte
   POS_COUNT_1:
     POS_1_OFFSET: 4 bytes
     NUM_TARGETS: 4 bytes
     COUNT: 8 bytes
     TARGET_1:
       VALUE_PROFILE_TYPE: 4 bytes
       TARGET_IDX: 8 bytes
       COUNT: 8 bytes
     TARGET_2
     ...
     TARGET_n
   POS_COUNT_2
   ...
   POS_COUNT_N
   CALLSITE_1:
     CALLSITE_1_OFFSET: 4 bytes
     FUNCTION_INSTANCE_PROFILE (nested)
   CALLSITE_2
   ...
   CALLSITE_n.  */

function_instance *
function_instance::read_function_instance (function_instance_stack *stack,
					   gcov_type head_count)
{
  unsigned name = gcov_read_unsigned ();
  unsigned num_pos_counts = gcov_read_unsigned ();
  unsigned num_callsites = gcov_read_unsigned ();
  function_instance *s = new function_instance (name, head_count);
  if (!stack->is_empty ())
    s->set_inlined_to (stack->last ());
  stack->safe_push (s);

  for (unsigned i = 0; i < num_pos_counts; i++)
    {
      unsigned offset = gcov_read_unsigned ();
      unsigned num_targets = gcov_read_unsigned ();
      gcov_type count = gcov_read_counter ();
      s->pos_counts[offset].count = count;
      afdo_profile_info->sum_max = std::max (afdo_profile_info->sum_max,
					     count);

      for (unsigned j = 0; j < stack->length (); j++)
        (*stack)[j]->total_count_ += count;
      for (unsigned j = 0; j < num_targets; j++)
        {
	  /* Only indirect call target histogram is supported now.  */
	  gcov_read_unsigned ();
	  gcov_type target_idx = gcov_read_counter ();
	  s->pos_counts[offset].targets[target_idx] = gcov_read_counter ();
        }
    }
  for (unsigned i = 0; i < num_callsites; i++)
    {
      unsigned offset = gcov_read_unsigned ();
      function_instance *callee_function_instance
          = read_function_instance (stack, -1);
      s->callsites[std::make_pair (offset, callee_function_instance->name ())]
          = callee_function_instance;
    }
  stack->pop ();
  return s;
}

/* Member functions for autofdo_source_profile.  */

autofdo_source_profile::~autofdo_source_profile ()
{
  for (name_function_instance_map::const_iterator iter = map_.begin ();
       iter != map_.end (); ++iter)
    delete iter->second;
}

/* For a given DECL, returns the top-level function_instance.  */

function_instance *
autofdo_source_profile::get_function_instance_by_decl (tree decl) const
{
  int index = afdo_string_table->get_index_by_decl (decl);
  if (index == -1)
    return NULL;
  name_function_instance_map::const_iterator ret = map_.find (index);
  return ret == map_.end () ? NULL : ret->second;
}

/* For a given NAME_INDEX, returns the top-level function_instance.  */

function_instance *
autofdo_source_profile::get_function_instance_by_name_index (int name_index)
       	const
{
  name_function_instance_map::const_iterator ret = map_.find (name_index);
  return ret == map_.end () ? NULL : ret->second;
}

/* Add function instance FN.  */

void
autofdo_source_profile::add_function_instance (function_instance *fn)
{
  int index = fn->name ();
  gcc_checking_assert (map_.count (index) == 0);
  map_[index] = fn;
}

/* Find count_info for a given gimple STMT. If found, store the count_info
   in INFO and return true; otherwise return false.  */

bool
autofdo_source_profile::get_count_info (gimple *stmt, count_info *info,
					cgraph_node *node) const
{
  gcc_checking_assert (stmt_loc_used_by_debug_info (stmt));
  return get_count_info (gimple_location (stmt), info, node);
}

bool
autofdo_source_profile::get_count_info (location_t gimple_loc,
					count_info *info,
					cgraph_node *node) const
{
  if (LOCATION_LOCUS (gimple_loc) == cfun->function_end_locus)
    return false;

  inline_stack stack;
  get_inline_stack_in_node (gimple_loc, &stack, node);
  if (stack.length () == 0)
    return false;
  function_instance *s = get_function_instance_by_inline_stack (stack);
  if (s == NULL)
    return false;
  return s->get_count_info (stack[0].afdo_loc, info);
}

/* Update value profile INFO for STMT from the inlined indirect callsite.
   Return true if INFO is updated.  */

bool
autofdo_source_profile::update_inlined_ind_target (gcall *stmt,
						   count_info *info,
						   cgraph_node *node)
{
  if (dump_file)
    {
      fprintf (dump_file, "Checking indirect call -> direct call ");
      print_gimple_stmt (dump_file, stmt, 0, TDF_SLIM);
    }

  if (LOCATION_LOCUS (gimple_location (stmt)) == cfun->function_end_locus)
    {
      if (dump_file)
	fprintf (dump_file, " bad locus (function end)\n");
      return false;
    }

  count_info old_info;
  get_count_info (stmt, &old_info, node);
  gcov_type total = 0;
  for (icall_target_map::const_iterator iter = old_info.targets.begin ();
       iter != old_info.targets.end (); ++iter)
    total += iter->second;
  total *= afdo_count_scale;

  /* Program behavior changed, original promoted (and inlined) target is not
     hot any more. Will avoid promote the original target.

     To check if original promoted target is still hot, we check the total
     count of the unpromoted targets (stored in TOTAL). If a callsite count
     (stored in INFO) is smaller than half of the total count, the original
     promoted target is considered not hot any more.  */
  if (info->count < total / 2)
    {
      if (dump_file)
	fprintf (dump_file, " not hot anymore %ld < %ld",
		 (long)info->count,
		 (long)total /2);
      return false;
    }

  inline_stack stack;
  get_inline_stack_in_node (gimple_location (stmt), &stack, node);
  if (stack.length () == 0)
    {
      if (dump_file)
	fprintf (dump_file, " no inline stack\n");
      return false;
    }
  function_instance *s = get_function_instance_by_inline_stack (stack);
  if (s == NULL)
    {
      if (dump_file)
	{
	  fprintf (dump_file, " function not found in inline stack:");
	  dump_inline_stack (dump_file, &stack);
	}
      return false;
    }
  icall_target_map map;
  if (s->find_icall_target_map (node ? node->decl
				: current_function_decl,
				stmt, &map) == 0)
    {
      if (dump_file)
	{
	  fprintf (dump_file, " no target map for stack: ");
	  dump_inline_stack (dump_file, &stack);
	}
      return false;
    }
  for (icall_target_map::const_iterator iter = map.begin ();
       iter != map.end (); ++iter)
    info->targets[iter->first] = iter->second;
  if (dump_file)
    {
      fprintf (dump_file, " looks good; stack:");
      dump_inline_stack (dump_file, &stack);
    }
  return true;
}

void
autofdo_source_profile::remove_icall_target (cgraph_edge *e)
{
  autofdo::inline_stack stack;
  autofdo::get_inline_stack_in_node (gimple_location (e->call_stmt),
				     &stack, e->caller);
  autofdo::function_instance *s
	  = get_function_instance_by_inline_stack (stack);
  s->remove_icall_target (e->caller->decl, e->call_stmt);
}

/* Find total count of the callee of EDGE.  */

gcov_type
autofdo_source_profile::get_callsite_total_count (
    struct cgraph_edge *edge) const
{
  inline_stack stack;
  stack.safe_push ({edge->callee->decl, 0, UNKNOWN_LOCATION});

  get_inline_stack_in_node (gimple_location (edge->call_stmt), &stack,
			    edge->caller);
  if (dump_file)
    {
      if (!edge->caller->inlined_to)
	fprintf (dump_file, "Looking up afdo profile for call %s -> %s stack:",
		 edge->caller->dump_name (), edge->callee->dump_name ());
      else
	fprintf (dump_file, "Looking up afdo profile for call %s -> %s transitively %s stack:",
		 edge->caller->dump_name (), edge->callee->dump_name (),
		 edge->caller->inlined_to->dump_name ());
      dump_inline_stack (dump_file, &stack);
    }

  function_instance *s = get_function_instance_by_inline_stack (stack);
  if (s == NULL)
    {
      if (dump_file)
	fprintf (dump_file, "No function instance found\n");
      return 0;
    }
  if (afdo_string_table->get_index_by_decl (edge->callee->decl) != s->name ())
    {
      if (dump_file)
	fprintf (dump_file, "Mismatched name of callee %s and profile %s\n",
		 raw_symbol_name (edge->callee->decl),
		 afdo_string_table->get_name (s->name ()));
      return 0;
    }

  return s->total_count () * afdo_count_scale;
}

/* Read AutoFDO profile and returns TRUE on success.  */

/* source profile format:

   GCOV_TAG_AFDO_FUNCTION: 4 bytes
   LENGTH: 4 bytes
   NUM_FUNCTIONS: 4 bytes
   FUNCTION_INSTANCE_1
   FUNCTION_INSTANCE_2
   ...
   FUNCTION_INSTANCE_N.  */

bool
autofdo_source_profile::read ()
{
  if (gcov_read_unsigned () != GCOV_TAG_AFDO_FUNCTION)
    {
      inform (UNKNOWN_LOCATION, "Not expected TAG.");
      return false;
    }

  gcc_checking_assert (!afdo_source_profile);
  afdo_source_profile = this;

  /* Skip the length of the section.  */
  gcov_read_unsigned ();

  /* Read in the function/callsite profile, and store it in local
     data structure.  */
  unsigned function_num = gcov_read_unsigned ();
  for (unsigned i = 0; i < function_num; i++)
    {
      function_instance::function_instance_stack stack;
      function_instance *s = function_instance::read_function_instance (
          &stack, gcov_read_counter ());
      int fun_id = s->name ();
      /* If function_instance with get_original_name (without the clone
	 suffix) exits, merge the function instances.  */
      if (map_.count (fun_id) == 0)
	map_[fun_id] = s;
      else
	fatal_error (UNKNOWN_LOCATION,
		     "auto-profile contains duplicated function instance %s",
		     afdo_string_table->get_name (s->name ()));
    }
  int hot_frac = param_hot_bb_count_fraction;
  /* Scale up the profile, but leave some bits in case some counts gets
     bigger than sum_max eventually.  */
  if (afdo_profile_info->sum_max)
    afdo_count_scale
      = MAX (((gcov_type)1 << (profile_count::n_bits - 10))
	     / afdo_profile_info->sum_max, 1);
  afdo_profile_info->cutoff *= afdo_count_scale;
  afdo_hot_bb_threshold
    = hot_frac
      ? afdo_profile_info->sum_max * afdo_count_scale / hot_frac
      : (gcov_type)profile_count::max_count;
  set_hot_bb_threshold (afdo_hot_bb_threshold);
  if (dump_file)
    fprintf (dump_file, "Max count in profile %" PRIu64 "\n"
			"Setting scale %" PRIu64 "\n"
			"Scaled max count %" PRIu64 "\n"
			"Cutoff %" PRIu64 "\n"
			"Hot count threshold %" PRIu64 "\n\n",
	     (int64_t)afdo_profile_info->sum_max,
	     (int64_t)afdo_count_scale,
	     (int64_t)(afdo_profile_info->sum_max * afdo_count_scale),
	     (int64_t)afdo_profile_info->cutoff,
	     (int64_t)afdo_hot_bb_threshold);
  afdo_profile_info->sum_max *= afdo_count_scale;
  return true;
}

/* Return the function_instance in the profile that correspond to the
   inline STACK.  */

function_instance *
autofdo_source_profile::get_function_instance_by_inline_stack (
    const inline_stack &stack) const
{
  name_function_instance_map::const_iterator iter = map_.find (
      afdo_string_table->get_index_by_decl (stack[stack.length () - 1].decl));
  if (iter == map_.end ())
    {
      if (dump_file)
	fprintf (dump_file, "No offline instance for %s\n",
		 raw_symbol_name (stack[stack.length () - 1].decl));
      return NULL;
    }
  function_instance *s = iter->second;
  for (unsigned i = stack.length () - 1; i > 0; i--)
    {
      s = s->get_function_instance_by_decl (stack[i].afdo_loc,
					    stack[i - 1].decl,
					    stack[i].location);
      if (s == NULL)
	{
	  /* afdo inliner extends the stack by last entry with unknown
	     location while checking if function was inlined during train run.
	     We do not want to print diagnostics about every function
	     which is not inlined.  */
	  if (s && dump_enabled_p () && stack[i].location != UNKNOWN_LOCATION)
	    dump_printf_loc (MSG_NOTE | MSG_PRIORITY_INTERNALS,
			     dump_user_location_t::from_location_t
			       (stack[i].location),
			      "auto-profile has no inlined function instance "
			      "for inlined call of %s at relative "
			      " location +%i, discriminator %i\n",
			     raw_symbol_name (stack[i - 1].decl),
			     stack[i].afdo_loc >> 16,
			     stack[i].afdo_loc & 65535);
	  return NULL;
	}
    }
  return s;
}

/* Module profile is only used by LIPO. Here we simply ignore it.  */

static void
fake_read_autofdo_module_profile ()
{
  /* Read in the module info.  */
  gcov_read_unsigned ();

  /* Skip the length of the section.  */
  gcov_read_unsigned ();

  /* Read in the file name table.  */
  unsigned total_module_num = gcov_read_unsigned ();
  gcc_assert (total_module_num == 0);
}

/* Read data from profile data file.  */

static void
read_profile (void)
{
  if (gcov_open (auto_profile_file, 1) == 0)
    {
      error ("cannot open profile file %s", auto_profile_file);
      return;
    }

  if (gcov_read_unsigned () != GCOV_DATA_MAGIC)
    {
      error ("AutoFDO profile magic number does not match");
      return;
    }

  /* Skip the version number.  */
  unsigned version = gcov_read_unsigned ();
  if (version != AUTO_PROFILE_VERSION)
    {
      error ("AutoFDO profile version %u does not match %u",
	     version, AUTO_PROFILE_VERSION);
      return;
    }

  /* Skip the empty integer.  */
  gcov_read_unsigned ();

  /* string_table.  */
  afdo_string_table = new string_table ();
  if (!afdo_string_table->read ())
    {
      error ("cannot read string table from %s", auto_profile_file);
      return;
    }

  /* autofdo_source_profile.  */
  afdo_source_profile = autofdo_source_profile::create ();
  if (afdo_source_profile == NULL
      || gcov_is_error ())
    {
      error ("cannot read function profile from %s", auto_profile_file);
      delete afdo_source_profile;
      afdo_source_profile = NULL;
      return;
    }

  /* autofdo_module_profile.  */
  fake_read_autofdo_module_profile ();
  if (gcov_is_error ())
    {
      error ("cannot read module profile from %s", auto_profile_file);
      return;
    }
}

/* From AutoFDO profiles, find values inside STMT for that we want to measure
   histograms for indirect-call optimization.

   This function is actually served for 2 purposes:
     * before annotation, we need to mark histogram, promote and inline
     * after annotation, we just need to mark, and let follow-up logic to
       decide if it needs to promote and inline.  */

static bool
afdo_indirect_call (gcall *stmt, const icall_target_map &map,
		    bool transform, cgraph_edge *indirect_edge)
{
  tree callee;

  if (map.size () == 0)
    {
      if (dump_file)
	fprintf (dump_file, "No targets found\n");
      return false;
    }
  if (!stmt)
    {
      if (dump_file)
	fprintf (dump_file, "No call statement\n");
      return false;
    }
  if (gimple_call_internal_p (stmt))
    {
      if (dump_file)
	fprintf (dump_file, "Internal call\n");
      return false;
    }
  if (gimple_call_fndecl (stmt) != NULL_TREE)
    {
      if (dump_file)
	fprintf (dump_file, "Call is already direct\n");
      return false;
    }

  gcov_type total = 0;
  icall_target_map::const_iterator max_iter = map.end ();

  for (icall_target_map::const_iterator iter = map.begin ();
       iter != map.end (); ++iter)
    {
      total += iter->second;
      if (max_iter == map.end () || max_iter->second < iter->second)
        max_iter = iter;
    }
  total *= afdo_count_scale;
  struct cgraph_node *direct_call = cgraph_node::get_for_asmname (
      get_identifier (afdo_string_table->get_name (max_iter->first)));
  if (direct_call == NULL)
    {
      if (dump_file)
	fprintf (dump_file, "Failed to find cgraph node for %s\n",
		 afdo_string_table->get_name (max_iter->first));
      return false;
    }

  callee = gimple_call_fn (stmt);

  if (!transform)
    {
      if (!direct_call->profile_id)
	{
	  if (dump_file)
	    fprintf (dump_file, "No profile id\n");
	  return false;
	}
      histogram_value hist = gimple_alloc_histogram_value (
	  cfun, HIST_TYPE_INDIR_CALL, stmt, callee);
      hist->n_counters = 4;
      hist->hvalue.counters = XNEWVEC (gcov_type, hist->n_counters);
      gimple_add_histogram_value (cfun, stmt, hist);

      /* Total counter */
      hist->hvalue.counters[0] = total;
      /* Number of value/counter pairs */
      hist->hvalue.counters[1] = 1;
      /* Value */
      hist->hvalue.counters[2] = direct_call->profile_id;
      /* Counter */
      hist->hvalue.counters[3] = max_iter->second * afdo_count_scale;

      if (!direct_call->profile_id)
	{
	  if (dump_file)
	    fprintf (dump_file, "Histogram attached\n");
	  return false;
	}
      return false;
    }

  if (dump_file)
    {
      fprintf (dump_file, "Indirect call -> direct call ");
      print_generic_expr (dump_file, callee, TDF_SLIM);
      fprintf (dump_file, " => ");
      print_generic_expr (dump_file, direct_call->decl, TDF_SLIM);
    }

  if (!direct_call->definition)
    {
      if (dump_file)
	fprintf (dump_file, " no definition available\n");
      return false;
    }

  if (dump_file)
    {
      fprintf (dump_file, " transformation on insn ");
      print_gimple_stmt (dump_file, stmt, 0, TDF_SLIM);
      fprintf (dump_file, "\n");
    }

  indirect_edge->make_speculative
		(direct_call,
		 gimple_bb (stmt)->count.apply_scale (99, 100));
  return true;
}

/* From AutoFDO profiles, find values inside STMT for that we want to measure
   histograms and adds them to list VALUES.  */

static bool
afdo_vpt (gcall *gs, const icall_target_map &map,
	  bool transform, cgraph_edge *indirect_edge)
{
  return afdo_indirect_call (gs, map, transform, indirect_edge);
}

typedef std::set<basic_block> bb_set;

static bool
is_bb_annotated (const basic_block bb, const bb_set &annotated)
{
  if (annotated.find (bb) != annotated.end ())
    {
      gcc_checking_assert (bb->count.quality () == AFDO
			   || !bb->count.nonzero_p ());
      return true;
    }
  gcc_checking_assert (bb->count.quality () != AFDO
		       || !bb->count.nonzero_p ());
  return false;
}

static void
set_bb_annotated (basic_block bb, bb_set *annotated)
{
  gcc_checking_assert (bb->count.quality () == AFDO
		       || !bb->count.nonzero_p ());
  annotated->insert (bb);
}

/* Update COUNT by known autofdo count C.  */
static void
update_count_by_afdo_count (profile_count *count, gcov_type c)
{
  if (c)
    *count = profile_count::from_gcov_type (c).afdo ();
  /* In case we have guessed profile which is already zero, preserve
     quality info.  */
  else if (count->nonzero_p ()
	   || count->quality () == GUESSED
	   || count->quality () == GUESSED_LOCAL)
    *count = profile_count::zero ().afdo ();
}

/* Update COUNT by known autofdo count C.  */
static void
update_count_by_afdo_count (profile_count *count, profile_count c)
{
  if (c.nonzero_p ())
    *count = c;
  /* In case we have guessed profile which is already zero, preserve
     quality info.  */
  else if (count->nonzero_p ()
	   || count->quality () < c.quality ())
    *count = c;
}

/* Try to determine unscaled count of edge E.
   Return -1 if nothing is known.  */

static gcov_type
afdo_unscaled_edge_count (edge e)
{
  gcov_type max_count = -1;
  basic_block bb_succ = e->dest;
  count_info info;
  if (afdo_source_profile->get_count_info (e->goto_locus, &info))
    {
      if (info.count > max_count)
	max_count = info.count;
      if (dump_file && info.count)
	{
	  fprintf (dump_file,
		   "  goto location of edge %i->%i with count %" PRIu64"\n",
		   e->src->index, e->dest->index, (int64_t)info.count);
	}
    }
  for (gphi_iterator gpi = gsi_start_phis (bb_succ);
       !gsi_end_p (gpi); gsi_next (&gpi))
    {
      gphi *phi = gpi.phi ();
      location_t phi_loc
	= gimple_phi_arg_location_from_edge (phi, e);
      if (afdo_source_profile->get_count_info (phi_loc, &info))
	{
	  if (info.count > max_count)
	    max_count = info.count;
	  if (dump_file && info.count)
	    {
	      fprintf (dump_file,
		       "  phi op of edge %i->%i with count %" PRIu64": ",
		       e->src->index, e->dest->index, (int64_t)info.count);
	      print_gimple_stmt (dump_file, phi, 0, TDF_SLIM);
	    }
	}
    }
  return max_count;
}

/* For a given BB, set its execution count. Attach value profile if a stmt
   is not in PROMOTED, because we only want to promote an indirect call once.
   Return TRUE if BB is annotated.  */

static bool
afdo_set_bb_count (basic_block bb, hash_set <basic_block> &zero_bbs)
{
  gimple_stmt_iterator gsi;
  gcov_type max_count = -1;
  if (dump_file)
    fprintf (dump_file, " Looking up AFDO count of bb %i\n", bb->index);

  for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
    {
      count_info info;
      gimple *stmt = gsi_stmt (gsi);
      if (!stmt_loc_used_by_debug_info (stmt))
	continue;
      if (afdo_source_profile->get_count_info (stmt, &info))
	{
	  if (info.count > max_count)
	    max_count = info.count;
	  if (dump_file)
	    {
	      fprintf (dump_file, "  count %" PRIu64 " in stmt: ",
		       (int64_t)info.count);
	      print_gimple_stmt (dump_file, stmt, 0, TDF_SLIM);
	    }
	  gcall *call = dyn_cast <gcall *> (gsi_stmt (gsi));
	  /* TODO; if inlined early and indirect call was not optimized out,
	     we will end up speculating again.  Early inliner should remove
	     all targets for edges it speculated into safely.  */
	  if (call
	      && info.targets.size () > 0)
	    afdo_vpt (call, info.targets, false, NULL);
	}
    }

  if (max_count == -1 && single_succ_p (bb))
    max_count = afdo_unscaled_edge_count (single_succ_edge (bb));

  if (max_count == -1)
    return false;

  if (max_count)
    {
      update_count_by_afdo_count (&bb->count, max_count * afdo_count_scale);
      if (dump_file)
	fprintf (dump_file,
		 " Annotated bb %i with count %" PRId64
		 ", scaled to %" PRId64 "\n",
		 bb->index, (int64_t)max_count,
		 (int64_t)(max_count * afdo_count_scale));
      return true;
    }
  else
    {
      if (dump_file)
	fprintf (dump_file,
		 " bb %i has statements with 0 count\n", bb->index);
      zero_bbs.add (bb);
    }
  return false;
}

/* BB1 and BB2 are in an equivalent class iff:
   1. BB1 dominates BB2.
   2. BB2 post-dominates BB1.
   3. BB1 and BB2 are in the same loop nest.
   This function finds the equivalent class for each basic block, and
   stores a pointer to the first BB in its equivalent class. Meanwhile,
   set bb counts for the same equivalent class to be idenical. Update
   ANNOTATED_BB for the first BB in its equivalent class.  */

static void
afdo_find_equiv_class (bb_set *annotated_bb)
{
  basic_block bb;

  FOR_ALL_BB_FN (bb, cfun)
    bb->aux = NULL;

  FOR_ALL_BB_FN (bb, cfun)
  {
    if (bb->aux != NULL)
      continue;
    bb->aux = bb;
    for (basic_block bb1 : get_dominated_by (CDI_DOMINATORS, bb))
      if (bb1->aux == NULL && dominated_by_p (CDI_POST_DOMINATORS, bb, bb1)
	  && bb1->loop_father == bb->loop_father)
	{
	  bb1->aux = bb;
	  if (is_bb_annotated (bb1, *annotated_bb)
	      && (!is_bb_annotated (bb, *annotated_bb)
		  || bb1->count > bb->count))
	    {
	      if (dump_file)
		{
		  fprintf (dump_file,
			   "  Copying count of bb %i to bb %i; count is:",
			   bb1->index,
			   bb->index);
		  bb1->count.dump (dump_file);
		  fprintf (dump_file, "\n");
		}
	      update_count_by_afdo_count (&bb->count, bb1->count);
	      set_bb_annotated (bb, annotated_bb);
	    }
	}

    for (basic_block bb1 : get_dominated_by (CDI_POST_DOMINATORS, bb))
      if (bb1->aux == NULL && dominated_by_p (CDI_DOMINATORS, bb, bb1)
	  && bb1->loop_father == bb->loop_father)
	{
	  bb1->aux = bb;
	  if (is_bb_annotated (bb1, *annotated_bb)
	      && (!is_bb_annotated (bb, *annotated_bb)
		  || bb1->count > bb->count))
	    {
	      if (dump_file)
		{
		  fprintf (dump_file,
			   "  Copying count of bb %i to bb %i; count is:",
			   bb1->index,
			   bb->index);
		  bb1->count.dump (dump_file);
		  fprintf (dump_file, "\n");
		}
	      update_count_by_afdo_count (&bb->count, bb1->count);
	      set_bb_annotated (bb, annotated_bb);
	    }
	}
  }
}

/* If a basic block's count is known, and only one of its in/out edges' count
   is unknown, its count can be calculated. Meanwhile, if all of the in/out
   edges' counts are known, then the basic block's unknown count can also be
   calculated. Also, if a block has a single predecessor or successor, the block's
   count can be propagated to that predecessor or successor.
   IS_SUCC is true if out edges of a basic blocks are examined.
   Update ANNOTATED_BB accordingly.
   Return TRUE if any basic block/edge count is changed.  */

static bool
afdo_propagate_edge (bool is_succ, bb_set *annotated_bb)
{
  basic_block bb;
  bool changed = false;

  FOR_EACH_BB_FN (bb, cfun)
  {
    edge e, unknown_edge = NULL;
    edge_iterator ei;
    int num_unknown_edges = 0;
    int num_edges = 0;
    profile_count total_known_count = profile_count::zero ().afdo ();

    FOR_EACH_EDGE (e, ei, is_succ ? bb->succs : bb->preds)
      {
	gcc_assert (AFDO_EINFO (e) != NULL);
	if (! AFDO_EINFO (e)->is_annotated ())
	  num_unknown_edges++, unknown_edge = e;
	else
	  total_known_count += AFDO_EINFO (e)->get_count ();
	num_edges++;
      }
    if (dump_file)
      {
	fprintf (dump_file, "bb %i %s propagating %s edges %i, "
		 "unknown edges %i, known count ",
		 bb->index,
		 is_bb_annotated (bb, *annotated_bb) ? "(annotated)" : "",
		 is_succ ? "successors" : "predecessors", num_edges,
		 num_unknown_edges);
	total_known_count.dump (dump_file);
	fprintf (dump_file, " bb count ");
	bb->count.dump (dump_file);
	fprintf (dump_file, "\n");
      }

    /* Be careful not to annotate block with no successor in special cases.  */
    if (num_unknown_edges == 0 && num_edges
	&& !is_bb_annotated (bb, *annotated_bb))
      {
	if (dump_file)
	  {
	    fprintf (dump_file, "  Annotating bb %i with count ", bb->index);
	    total_known_count.dump (dump_file);
	    fprintf (dump_file, "\n");
	  }
	update_count_by_afdo_count (&bb->count, total_known_count);
	set_bb_annotated (bb, annotated_bb);
	changed = true;
      }
    else if (is_bb_annotated (bb, *annotated_bb)
	     /* We do not want to consider 0 (afdo) > 0 (precise)  */
	     && total_known_count.nonzero_p ()
	     && bb->count < total_known_count)
      {
	if (dump_file)
	  {
	    fprintf (dump_file, "  Increasing bb %i count from ",
		     bb->index);
	    bb->count.dump (dump_file);
	    fprintf (dump_file, " to ");
	    total_known_count.dump (dump_file);
	    fprintf (dump_file, " hoping to mitigate afdo inconsistency\n");
	  }
	bb->count = total_known_count;
	changed = true;
      }
    else if (num_unknown_edges == 1 && is_bb_annotated (bb, *annotated_bb))
      {
	if (bb->count > total_known_count)
	  {
	    profile_count new_count = bb->count - total_known_count;
	    AFDO_EINFO (unknown_edge)->set_count (new_count);
	  }
	else
	  AFDO_EINFO (unknown_edge)->set_count
		  (profile_count::zero ().afdo ());
	if (dump_file)
	  {
	    fprintf (dump_file, "  Annotated edge %i->%i with count ",
		     unknown_edge->src->index, unknown_edge->dest->index);
	    AFDO_EINFO (unknown_edge)->get_count ().dump (dump_file);
	    fprintf (dump_file, "\n");
	  }
	AFDO_EINFO (unknown_edge)->set_annotated ();
	changed = true;
      }
    else if (num_unknown_edges > 1
	     && is_bb_annotated (bb, *annotated_bb)
	     && (total_known_count >= bb->count || !bb->count.nonzero_p ()))
      {
	FOR_EACH_EDGE (e, ei, is_succ ? bb->succs : bb->preds)
	  {
	    gcc_assert (AFDO_EINFO (e) != NULL);
	    if (! AFDO_EINFO (e)->is_annotated ())
	      {
		AFDO_EINFO (e)->set_count
		       	(profile_count::zero ().afdo ());
		AFDO_EINFO (e)->set_annotated ();
		if (dump_file)
		  {
		    fprintf (dump_file, "  Annotated edge %i->%i with count ",
			     e->src->index, e->dest->index);
		    AFDO_EINFO (unknown_edge)->get_count ().dump (dump_file);
		    fprintf (dump_file, "\n");
		  }
	      }
	  }
      }
    else if (num_unknown_edges == 0
	     && is_bb_annotated (bb, *annotated_bb)
	     && (is_succ ? single_succ_p (bb) : single_pred_p (bb)))
      {
	edge e = is_succ ? single_succ_edge (bb) : single_pred_edge (bb);
	if (AFDO_EINFO (e)->is_annotated ()
	    && AFDO_EINFO (e)->get_count () < bb->count)
	  {
	    if (dump_file)
	      {
		fprintf (dump_file, "  Increasing edge %i->%i count from ",
			 e->src->index, e->dest->index);
		AFDO_EINFO (e)->get_count ().dump (dump_file);
		fprintf (dump_file, " to ");
		bb->count.dump (dump_file);
		fprintf (dump_file, " hoping to mitigate afdo inconsistency\n");
	      }
	    AFDO_EINFO (e)->set_count (bb->count);
	    changed = true;
	  }
      }
  }
  return changed;
}

/* Special propagation for circuit expressions. Because GCC translates
   control flow into data flow for circuit expressions. E.g.
   BB1:
   if (a && b)
     BB2
   else
     BB3

   will be translated into:

   BB1:
     if (a)
       goto BB.t1
     else
       goto BB.t3
   BB.t1:
     if (b)
       goto BB.t2
     else
       goto BB.t3
   BB.t2:
     goto BB.t3
   BB.t3:
     tmp = PHI (0 (BB1), 0 (BB.t1), 1 (BB.t2)
     if (tmp)
       goto BB2
     else
       goto BB3

   In this case, we need to propagate through PHI to determine the edge
   count of BB1->BB.t1, BB.t1->BB.t2.  */

static void
afdo_propagate_circuit (const bb_set &annotated_bb)
{
  basic_block bb;
  FOR_ALL_BB_FN (bb, cfun)
  {
    gimple *def_stmt;
    tree cmp_rhs, cmp_lhs;
    gimple *cmp_stmt = last_nondebug_stmt (bb);
    edge e;
    edge_iterator ei;

    if (!cmp_stmt || gimple_code (cmp_stmt) != GIMPLE_COND)
      continue;
    cmp_rhs = gimple_cond_rhs (cmp_stmt);
    cmp_lhs = gimple_cond_lhs (cmp_stmt);
    if (!TREE_CONSTANT (cmp_rhs)
        || !(integer_zerop (cmp_rhs) || integer_onep (cmp_rhs)))
      continue;
    if (TREE_CODE (cmp_lhs) != SSA_NAME)
      continue;
    if (!is_bb_annotated (bb, annotated_bb))
      continue;
    def_stmt = SSA_NAME_DEF_STMT (cmp_lhs);
    while (def_stmt && gimple_code (def_stmt) == GIMPLE_ASSIGN
           && gimple_assign_single_p (def_stmt)
           && TREE_CODE (gimple_assign_rhs1 (def_stmt)) == SSA_NAME)
      def_stmt = SSA_NAME_DEF_STMT (gimple_assign_rhs1 (def_stmt));
    if (!def_stmt)
      continue;
    gphi *phi_stmt = dyn_cast <gphi *> (def_stmt);
    if (!phi_stmt)
      continue;
    FOR_EACH_EDGE (e, ei, bb->succs)
    {
      unsigned i, total = 0;
      edge only_one;
      bool check_value_one = (((integer_onep (cmp_rhs))
                               ^ (gimple_cond_code (cmp_stmt) == EQ_EXPR))
                              ^ ((e->flags & EDGE_TRUE_VALUE) != 0));
      if (! AFDO_EINFO (e)->is_annotated ())
        continue;
      for (i = 0; i < gimple_phi_num_args (phi_stmt); i++)
        {
          tree val = gimple_phi_arg_def (phi_stmt, i);
          edge ep = gimple_phi_arg_edge (phi_stmt, i);

          if (!TREE_CONSTANT (val)
              || !(integer_zerop (val) || integer_onep (val)))
            continue;
          if (check_value_one ^ integer_onep (val))
            continue;
          total++;
          only_one = ep;
          if (! (AFDO_EINFO (e)->get_count ()).nonzero_p ()
	      && ! AFDO_EINFO (ep)->is_annotated ())
	    {
	      AFDO_EINFO (ep)->set_count (profile_count::zero ().afdo ());
	      AFDO_EINFO (ep)->set_annotated ();
	    }
	}
      if (total == 1 && ! AFDO_EINFO (only_one)->is_annotated ())
	{
	  AFDO_EINFO (only_one)->set_count (AFDO_EINFO (e)->get_count ());
	  AFDO_EINFO (only_one)->set_annotated ();
	}
    }
  }
}

/* Propagate the basic block count and edge count on the control flow
   graph.  We do the propagation iteratively until stabilize.  */

static void
afdo_propagate (bb_set *annotated_bb)
{
  bool changed = true;
  int i = 0;

  basic_block bb;
  FOR_ALL_BB_FN (bb, cfun)
    if (!is_bb_annotated (bb, *annotated_bb)
	&& is_bb_annotated ((basic_block)bb->aux, *annotated_bb))
      {
	update_count_by_afdo_count (&bb->count, ((basic_block)bb->aux)->count);
	set_bb_annotated (bb, annotated_bb);
	if (dump_file)
	  {
	    fprintf (dump_file,
		     "  Copying count of bb %i to bb %i; count is:",
		     ((basic_block)bb->aux)->index,
		     bb->index);
	    bb->count.dump (dump_file);
	    fprintf (dump_file, "\n");
	  }
      }

  while (changed && i++ < 100)
    {
      changed = false;

      if (afdo_propagate_edge (true, annotated_bb))
        changed = true;
      if (afdo_propagate_edge (false, annotated_bb))
        changed = true;
      afdo_propagate_circuit (*annotated_bb);
    }
  if (dump_file)
    fprintf (dump_file, "Propagation took %i iterations %s\n",
	     i, changed ? "; iteration limit reached\n" : "");
}

/* qsort comparator of sreals.  */
static int
cmp (const void *a, const void *b)
{
  if (*(const sreal *)a < *(const sreal *)b)
    return 1;
  if (*(const sreal *)a > *(const sreal *)b)
    return -1;
  return 0;
}

/* To scale a connected component of graph we collect desired scales of
   basic blocks on the boundary and then compute a robust average.  */

struct scale
{
  /* Scale desired.  */
  sreal scale;
  /* Weight for averaging computed from execution count of the edge
     scale originates from.  */
  uint64_t weight;
};

/* Add scale ORIG/ANNOTATED to SCALES.  */

static void
add_scale (vec <scale> *scales, profile_count annotated, profile_count orig)
{
  if (dump_file)
    {
      orig.dump (dump_file);
      fprintf (dump_file, " should be ");
      annotated.dump (dump_file);
      fprintf (dump_file, "\n");
    }
  if (orig.nonzero_p ())
    {
      sreal scale
	= annotated.guessed_local ()
		.to_sreal_scale (orig);
      if (dump_file)
	fprintf (dump_file, "    adding scale %.16f, weight %" PRId64 "\n",
		 scale.to_double (), annotated.value () + 1);
      scales->safe_push ({scale, annotated.value () + 1});
    }
}

/* Scale counts of all basic blocks in BBS by SCALE and convert them to
   IPA quality.  */

static void
scale_bbs (const vec <basic_block> &bbs, sreal scale)
{
  if (dump_file)
    fprintf (dump_file, "  Scaling by %.16f\n", scale.to_double ());
  for (basic_block b : bbs)
    if (!(b->count == profile_count::zero ())
	&& b->count.initialized_p ())
      {
	profile_count o = b->count;
	b->count = b->count.force_guessed () * scale;

	/* If we scaled to 0, make it auto-fdo since that is treated
	   less agressively.  */
	if (!b->count.nonzero_p () && o.nonzero_p ())
	  b->count = profile_count::zero ().afdo ();
	if (dump_file)
	  {
	    fprintf (dump_file, "    bb %i count updated ", b->index);
	    o.dump (dump_file);
	    fprintf (dump_file, " -> ");
	    b->count.dump (dump_file);
	    fprintf (dump_file, "\n");
	  }
      }
}

/* Determine scaling factor by taking robust average of SCALES
   and taking into account limits.
   MAX_COUNT is maximal guessed count to be scaled while MAC_COUNT_IN_FN
   is maximal count in function determined by auto-fdo.  */

sreal
determine_scale (vec <scale> *scales, profile_count max_count,
		 profile_count max_count_in_fn)
{
  scales->qsort (cmp);

  uint64_t overall_weight = 0;
  for (scale &e : *scales)
    overall_weight += e.weight;

  uint64_t cummulated = 0, weight_sum = 0;
  sreal scale_sum = 0;
  for (scale &e : *scales)
    {
      uint64_t prev = cummulated;
      cummulated += e.weight;
      if (cummulated >= overall_weight / 4
	  && prev <= 3 * overall_weight / 4)
	{
	  scale_sum += e.scale * e.weight;
	  weight_sum += e.weight;
	  if (dump_file)
	    fprintf (dump_file, "    accounting scale %.16f, weight %" PRId64 "\n",
		     e.scale.to_double (), e.weight);
	}
      else if (dump_file)
	fprintf (dump_file, "    ignoring scale %.16f, weight %" PRId64 "\n",
		 e.scale.to_double (), e.weight);
     }
  sreal scale = scale_sum / (sreal)weight_sum;

  /* Avoid scaled regions to have very large counts.
     Otherwise they may dominate ipa-profile's histogram computing cutoff
     of hot basic blocks.  */
  if (max_count * scale > max_count_in_fn.guessed_local ().apply_scale (128, 1))
    {
      if (dump_file)
	{
	  fprintf (dump_file, "Scaling by %.16f produces max count ",
		   scale.to_double ());
	  (max_count * scale).dump (dump_file);
	  fprintf (dump_file, " that exceeds max count in fn ");
	  max_count_in_fn.dump (dump_file);
	  fprintf (dump_file, "; capping\n");
	}
      scale = max_count_in_fn.guessed_local ().to_sreal_scale (max_count);
    }
  return scale;
}

/* Scale profile of the whole function to approximately match auto-profile.  */

bool
scale_bb_profile ()
{
  const function_instance *s
      = afdo_source_profile->get_function_instance_by_decl
	  (current_function_decl);

  /* In the first pass only store non-zero counts.  */
  gcov_type head_count = s->head_count () * autofdo::afdo_count_scale;
  hash_set <basic_block> zero_bbs;
  auto_vec <basic_block, 20> bbs (n_basic_blocks_for_fn (cfun));
  auto_vec <scale, 20> scales;
  basic_block bb;
  profile_count max_count = profile_count::zero ();
  profile_count max_count_in_fn = profile_count::zero ();
  bbs.quick_push (ENTRY_BLOCK_PTR_FOR_FN (cfun));
  bbs.quick_push (EXIT_BLOCK_PTR_FOR_FN (cfun));
  if (head_count > 0)
    {
      profile_count entry_count = ENTRY_BLOCK_PTR_FOR_FN (cfun)->count;
      max_count = entry_count;
      update_count_by_afdo_count (&entry_count, head_count);
      max_count_in_fn = entry_count;
      add_scale (&scales, entry_count, ENTRY_BLOCK_PTR_FOR_FN (cfun)->count);
    }
  FOR_EACH_BB_FN (bb, cfun)
    {
      profile_count cnt = bb->count;
      bbs.safe_push (bb);
      max_count = profile_count::max_prefer_initialized (max_count, cnt);
      if (afdo_set_bb_count (bb, zero_bbs))
	{
	  std::swap (cnt, bb->count);
	  max_count_in_fn
	    = profile_count::max_prefer_initialized (max_count_in_fn, cnt);
	  add_scale (&scales, cnt, bb->count);
	}
    }
  if (scales.length ())
    {
      sreal scale = determine_scale (&scales, max_count, max_count_in_fn);
      scale_bbs (bbs, scale);
      return true;
    }
  return false;
}

/* In case given basic block was fully optimized out, AutoFDO
   will have no data about it.  In this case try to preserve static profile.
   Identify connected components (in undirected form of CFG) which has
   no annotations at all.  Look at thir boundaries and try to determine
   scaling factor and scale.  */

void
afdo_adjust_guessed_profile (bb_set *annotated_bb)
{
  /* Basic blocks of connected component currently processed.  */
  auto_vec <basic_block, 20> bbs (n_basic_blocks_for_fn (cfun));
  /* Scale factors found.  */
  auto_vec <scale, 20> scales;
  auto_vec <basic_block, 20> stack (n_basic_blocks_for_fn (cfun));

  basic_block seed_bb;
  unsigned int component_id = 1;

  /* Map from basic block to its component.
     0   is used for univisited BBs,
     1   means that BB is annotated,
     >=2 is an id of the component BB belongs to.  */
  auto_vec <unsigned int, 20> component;
  component.safe_grow (last_basic_block_for_fn (cfun));
  profile_count max_count_in_fn = profile_count::zero ();
  FOR_ALL_BB_FN (seed_bb, cfun)
    if (is_bb_annotated (seed_bb, *annotated_bb))
      {
	component[seed_bb->index] = 1;
	max_count_in_fn
	  = profile_count::max_prefer_initialized (max_count_in_fn, seed_bb->count);
      }
    else
      component[seed_bb->index] = 0;
  FOR_ALL_BB_FN (seed_bb, cfun)
   if (!component[seed_bb->index])
     {
       stack.quick_push (seed_bb);
       component_id++;
       bbs.truncate (0);
       scales.truncate (0);
       component[seed_bb->index] = component_id;
       profile_count max_count = profile_count::zero ();

       /* Identify connected component starting in BB.  */
       if (dump_file)
	 fprintf (dump_file, "Starting connected component in bb %i\n",
		  seed_bb->index);
       do
	 {
	   basic_block b = stack.pop ();

	   bbs.quick_push (b);
	   max_count = profile_count::max_prefer_initialized (max_count, b->count);

	   for (edge e: b->preds)
	     if (!component[e->src->index])
	       {
		  stack.quick_push (e->src);
		  component[e->src->index] = component_id;
	       }
	   for (edge e: b->succs)
	     if (!component[e->dest->index])
	       {
		  stack.quick_push (e->dest);
		  component[e->dest->index] = component_id;
	       }
	 }
       while (!stack.is_empty ());

       /* If all blocks in components has 0 count, we do not need
	  to scale, only we must convert to IPA quality.  */
       if (!max_count.nonzero_p ())
	 {
	   if (dump_file)
	     fprintf (dump_file, "  All counts are 0; scale = 1\n");
	   scale_bbs (bbs, 1);
	   continue;
	 }

       /* Now visit the component and try to figure out its desired
	  frequency.  */
       for (basic_block b : bbs)
	 {
	   if (dump_file)
	     {
	       fprintf (dump_file, "  visiting bb %i with count ", b->index);
	       b->count.dump (dump_file);
	       fprintf (dump_file, "\n");
	     }
	   if (!b->count.nonzero_p ())
	     continue;
	   /* Sum of counts of annotated edges into B.  */
	   profile_count annotated_count = profile_count::zero ();
	   /* Sum of counts of edges into B with source in current
	      component.  */
	   profile_count current_component_count = profile_count::zero ();
	   bool boundary = false;

	   for (edge e: b->preds)
	     if (AFDO_EINFO (e)->is_annotated ())
	       {
		 if (dump_file)
		   {
		     fprintf (dump_file, "    Annotated pred edge to %i "
			      "with count ", e->src->index);
		     AFDO_EINFO (e)->get_count ().dump (dump_file);
		     fprintf (dump_file, "\n");
		   }
		 boundary = true;
		 annotated_count += AFDO_EINFO (e)->get_count ();
	       }
	     /* If source is anotated, combine with static
		probability prediction.
		TODO: We can do better in case some of edges out are
		annotated and distribute only remaining count out of BB.  */
	     else if (is_bb_annotated (e->src, *annotated_bb))
	       {
		 boundary = true;
		 if (dump_file)
		   {
		     fprintf (dump_file, "    Annotated predecessor %i "
			      "with count ", e->src->index);
		     e->src->count.dump (dump_file);
		     fprintf (dump_file, " edge count using static profile ");
		     e->count ().dump (dump_file);
		     fprintf (dump_file, "\n");
		   }
		 annotated_count += e->count ();
	       }
	     else
	       {
		 current_component_count += e->count ();
		 gcc_checking_assert (component[e->src->index] == component_id);
	       }
	   if (boundary && current_component_count.initialized_p ())
	     {
	       if (dump_file)
		 fprintf (dump_file, "    bb %i in count ", b->index);
	       add_scale (&scales,
			  annotated_count,
			  b->count - current_component_count);
	     }
	   for (edge e: b->succs)
	     if (AFDO_EINFO (e)->is_annotated ())
	       {
		 if (dump_file)
		   fprintf (dump_file, "    edge %i->%i count ",
			    b->index, e->dest->index);
		 add_scale (&scales, AFDO_EINFO (e)->get_count (), e->count ());
	       }
	     else if (is_bb_annotated (e->dest, *annotated_bb))
	       {
		 profile_count annotated_count = e->dest->count;
		 profile_count out_count = profile_count::zero ();
		 bool ok = true;

		 for (edge e2: e->dest->preds)
		   if (AFDO_EINFO (e2)->is_annotated ())
		     annotated_count -= AFDO_EINFO (e2)->get_count ();
		   else if (component[e2->src->index] == component_id)
		     out_count += e2->count ();
		   else if (is_bb_annotated (e2->src, *annotated_bb))
		     annotated_count -= e2->count ();
		   else if (e2->probability.nonzero_p ())
		     {
		       ok = false;
		       break;
		     }
		 if (!ok)
		   continue;
		 if (dump_file)
		   fprintf (dump_file,
			    "    edge %i->%i has annotated successor; count ",
			    b->index, e->dest->index);
		 add_scale (&scales, annotated_count, e->count ());
	       }

	 }

       /* If we failed to find annotated entry or exit edge,
	  look for exit edges and scale profile so the dest
	  BB get all flow it needs.  This is imprecise because
	  the edge is not annotated and thus BB has more than
	  one such predecessor.  */
       if (!scales.length ())
	 for (basic_block b : bbs)
	   if (b->count.nonzero_p ())
	     for (edge e: b->succs)
	       if (is_bb_annotated (e->dest, *annotated_bb))
		 {
		   profile_count annotated_count = e->dest->count;
		   for (edge e2: e->dest->preds)
		     if (AFDO_EINFO (e2)->is_annotated ())
		       annotated_count -= AFDO_EINFO (e2)->get_count ();
		   if (dump_file)
		     fprintf (dump_file,
			      "    edge %i->%i has annotated successor;"
			      " upper bound count ",
			      b->index, e->dest->index);
		   add_scale (&scales, annotated_count, e->count ());
		 }
       if (!scales.length ())
	 {
	   if (dump_file)
	     fprintf (dump_file,
		      "  Can not determine count from the boundary; giving up\n");
	   continue;
	 }
       gcc_checking_assert (scales.length ());
       sreal scale = determine_scale (&scales, max_count, max_count_in_fn);
       scale_bbs (bbs, scale);
     }
}

/* Propagate counts on control flow graph and calculate branch
   probabilities.  */

static void
afdo_calculate_branch_prob (bb_set *annotated_bb)
{
  edge e;
  edge_iterator ei;
  basic_block bb;

  FOR_ALL_BB_FN (bb, cfun)
    {
      gcc_assert (bb->aux == NULL);
      FOR_EACH_EDGE (e, ei, bb->succs)
	{
	  gcc_assert (e->aux == NULL);
	  e->aux = new edge_info ();
	  gcov_type c = afdo_unscaled_edge_count (e);
	  if (c == 0 && e->count () == profile_count::zero ())
	    {
	      AFDO_EINFO (e)->set_count (profile_count::zero ());
	      if (dump_file)
		fprintf (dump_file,
			 "  Annotating edge %i->%i with count 0;"
			 " static profile aggress",
			 e->src->index, e->dest->index);
	    }
	  else if (c > 0)
	    {
	      AFDO_EINFO (e)->set_count
	       	(profile_count::from_gcov_type
		   (c * autofdo::afdo_count_scale).afdo ());
	      if (dump_file)
		{
		  fprintf (dump_file,
			   "  Annotating edge %i->%i with count ",
			   e->src->index, e->dest->index);
		  AFDO_EINFO (e)->get_count ().dump (dump_file);
		  fprintf (dump_file, "\n");
		}
	    }
	}
    }

  afdo_find_equiv_class (annotated_bb);
  afdo_propagate (annotated_bb);

  FOR_EACH_BB_FN (bb, cfun)
    if (is_bb_annotated (bb, *annotated_bb))
      {
	bool all_known = true;
	profile_count total_count = profile_count::zero ().afdo ();

	FOR_EACH_EDGE (e, ei, bb->succs)
	  {
	    gcc_assert (AFDO_EINFO (e) != NULL);
	    if (! AFDO_EINFO (e)->is_annotated ())
	      {
		/* If by static profile this edge never happens,
		   still propagate the rest.  */
		if (e->probability.nonzero_p ())
		  {
		    all_known = false;
		    break;
		  }
	      }
	    else
	      total_count += AFDO_EINFO (e)->get_count ();
	  }
	if (!all_known || !total_count.nonzero_p ())
	  continue;
	if (dump_file)
	  {
	    fprintf (dump_file, "Total count of bb %i is ", bb->index);
	    total_count.dump (dump_file);
	    fprintf (dump_file, "\n");
	  }

	FOR_EACH_EDGE (e, ei, bb->succs)
	  if (AFDO_EINFO (e)->is_annotated ())
	    {
	      profile_count cnt = AFDO_EINFO (e)->get_count ();
	      /* If probability is 1, preserve reliable static prediction
		 (This is, for example the case of single fallthru edge
		  or single fallthru plus unlikely EH edge.)  */
	      if (cnt == total_count
		  && e->probability == profile_probability::always ())
		;
	      else if (cnt.nonzero_p ())
		e->probability
		  = cnt.probability_in (total_count);
	      /* If probability is zero, preserve reliable static
		 prediction.  */
	      else if (e->probability.nonzero_p ()
		       || e->probability.quality () == GUESSED)
		e->probability = profile_probability::never ().afdo ();
	      if (dump_file)
		{
		  fprintf (dump_file, "  probability of edge %i->%i"
			   " with count ",
			   e->src->index, e->dest->index);
		  cnt.dump (dump_file);
		  fprintf (dump_file, " set to ");
		  e->probability.dump (dump_file);
		  fprintf (dump_file, "\n");
		}
	    }
      }
  afdo_adjust_guessed_profile (annotated_bb);
  FOR_ALL_BB_FN (bb, cfun)
    {
      bb->aux = NULL;
      FOR_EACH_EDGE (e, ei, bb->succs)
	if (AFDO_EINFO (e) != NULL)
	  {
	    delete AFDO_EINFO (e);
	    e->aux = NULL;
	  }
    }
}

/* Annotate auto profile to the control flow graph.  */

static void
afdo_annotate_cfg (void)
{
  basic_block bb;
  bb_set annotated_bb;
  const function_instance *s
      = afdo_source_profile->get_function_instance_by_decl (
          current_function_decl);

  if (s == NULL)
    {
      if (dump_file)
	fprintf (dump_file, "No afdo profile for %s\n",
		 cgraph_node::get (current_function_decl)->dump_name ());
      /* create_gcov only dumps symbols with some samples in them.
	 This means that we get nonempty zero_bbs only if some
	 nonzero counts in profile were not matched with statements.  */
      if (!flag_profile_partial_training)
	{
	  FOR_ALL_BB_FN (bb, cfun)
	    if (bb->count.quality () == GUESSED_LOCAL)
	      bb->count = bb->count.global0afdo ();
	  update_max_bb_count ();
	}
      return;
    }

  calculate_dominance_info (CDI_POST_DOMINATORS);
  calculate_dominance_info (CDI_DOMINATORS);
  loop_optimizer_init (0);

  if (dump_file)
    {
      fprintf (dump_file, "\n\nAnnotating BB profile of %s\n",
	       cgraph_node::get (current_function_decl)->dump_name ());
      fprintf (dump_file, "\n");
      s->dump (dump_file);
      fprintf (dump_file, "\n");
    }
  bool profile_found = false;
  hash_set <basic_block> zero_bbs;
  gcov_type head_count = s->head_count () * autofdo::afdo_count_scale;

  if (!param_auto_profile_bbs)
    {
      if (scale_bb_profile ())
	return;
    }
  else
    {
      /* In the first pass only store non-zero counts.  */
      profile_found = head_count > 0;
      FOR_EACH_BB_FN (bb, cfun)
	{
	  if (afdo_set_bb_count (bb, zero_bbs))
	    {
	      if (bb->count.quality () == AFDO)
		{
		  gcc_assert (bb->count.nonzero_p ());
		  profile_found = true;
		}
	      set_bb_annotated (bb, &annotated_bb);
	    }
	}
    }
  /* Exit without clobbering static profile if there was no
     non-zero count.  */
  if (!profile_found)
    {
      /* create_gcov only dumps symbols with some samples in them.
	 This means that we get nonempty zero_bbs only if some
	 nonzero counts in profile were not matched with statements.
	 ??? We can adjust create_gcov to also recordinfo
	 about function with no samples.  Then we can distinguish
	 between lost profiles which should be kept local and
	 real functions with 0 samples during train run.  */
      if (zero_bbs.is_empty ())
	{
	  if (dump_file)
	    fprintf (dump_file, "No afdo samples found"
		     "; Setting global count to afdo0\n");
	}
      else
	{
	  if (dump_file)
	    fprintf (dump_file, "Setting global count to afdo0\n");
	}
      if (!flag_profile_partial_training)
	{
	  FOR_ALL_BB_FN (bb, cfun)
	    if (bb->count.quality () == GUESSED_LOCAL)
	      bb->count = bb->count.global0afdo ();
	  update_max_bb_count ();
	}

      loop_optimizer_finalize ();
      free_dominance_info (CDI_DOMINATORS);
      free_dominance_info (CDI_POST_DOMINATORS);
      return;
    }
  /* We try to preserve static profile for BBs with 0
     afdo samples, but if even static profile agrees with 0,
     consider it final so propagation works better.  */
  for (basic_block bb : zero_bbs)
    if (!bb->count.nonzero_p ())
      {
	update_count_by_afdo_count (&bb->count, 0);
	set_bb_annotated (bb, &annotated_bb);
	if (dump_file)
	  {
	    fprintf (dump_file, "  Annotating bb %i with count ", bb->index);
	    bb->count.dump (dump_file);
	    fprintf (dump_file,
		     " (has 0 count in both static and afdo profile)\n");
	  }
      }

  /* Update profile.  */
  if (head_count > 0)
    {
      update_count_by_afdo_count (&ENTRY_BLOCK_PTR_FOR_FN (cfun)->count,
				  head_count);
      set_bb_annotated (ENTRY_BLOCK_PTR_FOR_FN (cfun), &annotated_bb);
      if (!is_bb_annotated (ENTRY_BLOCK_PTR_FOR_FN (cfun)->next_bb, annotated_bb)
	  || ENTRY_BLOCK_PTR_FOR_FN (cfun)->count
	     > ENTRY_BLOCK_PTR_FOR_FN (cfun)->next_bb->count)
	{
	  ENTRY_BLOCK_PTR_FOR_FN (cfun)->next_bb->count
	      = ENTRY_BLOCK_PTR_FOR_FN (cfun)->count;
	  set_bb_annotated (ENTRY_BLOCK_PTR_FOR_FN (cfun)->next_bb,
			    &annotated_bb);
	}
      if (!is_bb_annotated (EXIT_BLOCK_PTR_FOR_FN (cfun), annotated_bb)
	  || ENTRY_BLOCK_PTR_FOR_FN (cfun)->count
	     > EXIT_BLOCK_PTR_FOR_FN (cfun)->prev_bb->count)
	{
	  EXIT_BLOCK_PTR_FOR_FN (cfun)->prev_bb->count
	      = ENTRY_BLOCK_PTR_FOR_FN (cfun)->count;
	  set_bb_annotated (EXIT_BLOCK_PTR_FOR_FN (cfun)->prev_bb, &annotated_bb);
	}
    }

  /* Calculate, propagate count and probability information on CFG.  */
  afdo_calculate_branch_prob (&annotated_bb);

 /* If we failed to turn some of original guessed profile to global,
     set basic blocks uninitialized.  */
  FOR_ALL_BB_FN (bb, cfun)
    if (!bb->count.ipa_p ())
      {
	/* We skip annotating entry profile if it is 0
	   in hope to be able to determine it better from the
	   static profile.

	   Now we know we can not derive it from other info,
	   so set it since it is better than UNKNOWN.  */
	if (bb == ENTRY_BLOCK_PTR_FOR_FN (cfun))
	  bb->count = profile_count::zero ().afdo ();
	else
	  bb->count = profile_count::uninitialized ();
	if (dump_file)
	  fprintf (dump_file, "  Unknown count of bb %i\n", bb->index);
	cfun->cfg->full_profile = false;
      }

  cgraph_node::get (current_function_decl)->count
      = ENTRY_BLOCK_PTR_FOR_FN (cfun)->count;
  update_max_bb_count ();
  profile_status_for_fn (cfun) = PROFILE_READ;
  if (flag_value_profile_transformations)
    {
      gimple_value_profile_transformations ();
      free_dominance_info (CDI_DOMINATORS);
      free_dominance_info (CDI_POST_DOMINATORS);
      update_ssa (TODO_update_ssa);
    }

  loop_optimizer_finalize ();
  free_dominance_info (CDI_DOMINATORS);
  free_dominance_info (CDI_POST_DOMINATORS);
}

/* Use AutoFDO profile to annotate the control flow graph.
   Return the todo flag.  */

static unsigned int
auto_profile (void)
{
  struct cgraph_node *node;

  if (symtab->state == FINISHED || !afdo_source_profile)
    return 0;

  init_node_map (true);
  profile_info = autofdo::afdo_profile_info;
  afdo_source_profile->offline_unrealized_inlines ();

  FOR_EACH_FUNCTION (node)
  {
    if (!gimple_has_body_p (node->decl))
      continue;

    /* Don't profile functions produced for builtin stuff.  */
    if (DECL_SOURCE_LOCATION (node->decl) == BUILTINS_LOCATION)
      continue;

    push_cfun (DECL_STRUCT_FUNCTION (node->decl));

    autofdo::afdo_annotate_cfg ();
    compute_function_frequency ();

    free_dominance_info (CDI_DOMINATORS);
    free_dominance_info (CDI_POST_DOMINATORS);
    cgraph_edge::rebuild_edges ();
    pop_cfun ();
  }

  return 0;
}
} /* namespace autofdo.  */

/* Read the profile from the profile data file.  */

void
read_autofdo_file (void)
{
  if (auto_profile_file == NULL)
    auto_profile_file = DEFAULT_AUTO_PROFILE_FILE;

  autofdo::afdo_profile_info = XNEW (gcov_summary);
  autofdo::afdo_profile_info->runs = 1;
  autofdo::afdo_profile_info->sum_max = 0;
  autofdo::afdo_profile_info->cutoff = 1;

  /* Read the profile from the profile file.  */
  autofdo::read_profile ();
}

/* Free the resources.  */

void
end_auto_profile (void)
{
  delete autofdo::afdo_source_profile;
  delete autofdo::afdo_string_table;
  profile_info = NULL;
}

/* Returns TRUE if EDGE is hot enough to be inlined early.  */

bool
afdo_callsite_hot_enough_for_early_inline (struct cgraph_edge *edge)
{
  gcov_type count
      = autofdo::afdo_source_profile->get_callsite_total_count (edge);

  if (count > 0)
    {
      bool is_hot;
      profile_count pcount = profile_count::from_gcov_type (count).afdo ();
      is_hot = maybe_hot_afdo_count_p (pcount);
      if (dump_file)
	{
	  fprintf (dump_file, "Call %s -> %s has %s afdo profile count ",
		   edge->caller->dump_name (), edge->callee->dump_name (),
		   is_hot ? "hot" : "cold");
	  pcount.dump (dump_file);
	  fprintf (dump_file, "\n");
	}
      return is_hot;
    }

  return false;
}

/* Do indirect call promotion during early inlining to make the
   IR match the profiled binary before actual annotation.

   This is needed because an indirect call might have been promoted
   and inlined in the profiled binary.  If we do not promote and
   inline these indirect calls before annotation, the profile for
   these promoted functions will be lost.

   e.g. foo() --indirect_call--> bar()
   In profiled binary, the callsite is promoted and inlined, making
   the profile look like:

   foo: {
     loc_foo_1: count_1
     bar@loc_foo_2: {
       loc_bar_1: count_2
       loc_bar_2: count_3
     }
   }

   Before AutoFDO pass, loc_foo_2 is not promoted thus not inlined.
   If we perform annotation on it, the profile inside bar@loc_foo2
   will be wasted.

   To avoid this, we promote loc_foo_2 and inline the promoted bar
   function before annotation, so the profile inside bar@loc_foo2
   will be useful.  */

bool
afdo_vpt_for_early_inline (cgraph_node *node)
{
  if (!node->indirect_calls)
    return false;
  bool changed = false;
  cgraph_node *outer = node->inlined_to ? node->inlined_to : node;
  if (autofdo::afdo_source_profile->get_function_instance_by_decl
	  (outer->decl) == NULL)
    return false;
  for (cgraph_edge *e = node->indirect_calls; e; e = e->next_callee)
    {
      gcov_type bb_count = 0;
      autofdo::count_info info;
      basic_block bb = gimple_bb (e->call_stmt);

      /* TODO: This is quadratic; cache the value.  */
      for (gimple_stmt_iterator gsi = gsi_start_bb (bb);
	   !gsi_end_p (gsi); gsi_next (&gsi))
	{
	  gimple *stmt = gsi_stmt (gsi);
	  if (!stmt_loc_used_by_debug_info (stmt))
	    continue;
	  autofdo::count_info info;
	  if (autofdo::afdo_source_profile->get_count_info (stmt, &info, node))
	    bb_count = MAX (bb_count, info.count);
	}
      autofdo::afdo_source_profile->get_count_info (e->call_stmt, &info, node);
      info.count = bb_count;
      if (!autofdo::afdo_source_profile->update_inlined_ind_target
		      (e->call_stmt, &info, node))
	continue;
      changed |= autofdo::afdo_vpt (e->call_stmt, info.targets, true, e);
    }
  return changed;
}

/* If speculation used during early inline, remove the target
   so we do not speculate the indirect edge again during afdo pass.  */

void
remove_afdo_speculative_target (cgraph_edge *e)
{
  autofdo::afdo_source_profile->remove_icall_target (e);
}

namespace
{

const pass_data pass_data_ipa_auto_profile = {
  SIMPLE_IPA_PASS, "afdo", /* name */
  OPTGROUP_NONE,           /* optinfo_flags */
  TV_IPA_AUTOFDO,          /* tv_id */
  0,                       /* properties_required */
  0,                       /* properties_provided */
  0,                       /* properties_destroyed */
  0,                       /* todo_flags_start */
  0,                       /* todo_flags_finish */
};

class pass_ipa_auto_profile : public simple_ipa_opt_pass
{
public:
  pass_ipa_auto_profile (gcc::context *ctxt)
      : simple_ipa_opt_pass (pass_data_ipa_auto_profile, ctxt)
  {
  }

  /* opt_pass methods: */
  bool
  gate (function *) final override
  {
    return flag_auto_profile;
  }
  unsigned int
  execute (function *) final override
  {
    return autofdo::auto_profile ();
  }
}; // class pass_ipa_auto_profile

} // anon namespace

simple_ipa_opt_pass *
make_pass_ipa_auto_profile (gcc::context *ctxt)
{
  return new pass_ipa_auto_profile (ctxt);
}

namespace
{

const pass_data pass_data_ipa_auto_profile_offline = {
  SIMPLE_IPA_PASS, "afdo_offline", /* name */
  OPTGROUP_NONE,           /* optinfo_flags */
  TV_IPA_AUTOFDO_OFFLINE,  /* tv_id */
  0,                       /* properties_required */
  0,                       /* properties_provided */
  0,                       /* properties_destroyed */
  0,                       /* todo_flags_start */
  0,                       /* todo_flags_finish */
};

class pass_ipa_auto_profile_offline : public simple_ipa_opt_pass
{
public:
  pass_ipa_auto_profile_offline (gcc::context *ctxt)
      : simple_ipa_opt_pass (pass_data_ipa_auto_profile_offline, ctxt)
  {
  }

  /* opt_pass methods: */
  bool
  gate (function *) final override
  {
    return flag_auto_profile;
  }
  unsigned int
  execute (function *) final override
  {
    read_autofdo_file ();
    if (autofdo::afdo_source_profile)
      autofdo::afdo_source_profile->offline_external_functions ();
    return 0;
  }
}; // class pass_ipa_auto_profile

} // anon namespace

simple_ipa_opt_pass *
make_pass_ipa_auto_profile_offline (gcc::context *ctxt)
{
  return new pass_ipa_auto_profile_offline (ctxt);
}
