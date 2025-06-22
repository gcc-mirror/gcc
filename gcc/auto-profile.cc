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

/* The following routines implements AutoFDO optimization.

   This optimization uses sampling profiles to annotate basic block counts
   and uses heuristics to estimate branch probabilities.

   There are three phases in AutoFDO:

   Phase 1: Read profile from the profile data file.
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

   Phase 2: Early inline + value profile transformation.
     Early inline uses autofdo_source_profile to find if a callsite is:
        * inlined in the profiled binary.
        * callee body is hot in the profiling run.
     If both condition satisfies, early inline will inline the callsite
     regardless of the code growth.
     Phase 2 is an iterative process. During each iteration, we also check
     if an indirect callsite is promoted and inlined in the profiling run.
     If yes, vpt will happen to force promote it and in the next iteration,
     einline will inline the promoted callsite in the next iteration.

   Phase 3: Annotate control flow graph.
     AutoFDO uses a separate pass to:
        * Annotate basic block count
        * Estimate branch probability

   After the above 3 phases, all profile is readily annotated on the GCC IR.
   AutoFDO tries to reuse all FDO infrastructure as much as possible to make
   use of the profile. E.g. it uses existing mechanism to calculate the basic
   block/edge frequency, as well as the cgraph node/edge count.
*/

#define DEFAULT_AUTO_PROFILE_FILE "fbdata.afdo"
#define AUTO_PROFILE_VERSION 2

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
typedef std::pair<tree, unsigned> decl_lineno;

/* Represent an inline stack. vector[0] is the leaf node.  */
typedef auto_vec<decl_lineno> inline_stack;

/* String array that stores function names.  */
typedef auto_vec<char *> string_vector;

/* Map from function name's index in string_table to target's
   execution count.  */
typedef std::map<unsigned, gcov_type> icall_target_map;

/* Set of gimple stmts. Used to track if the stmt has already been promoted
   to direct call.  */
typedef std::set<gimple *> stmt_set;

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
  gcov_type
  total_count () const
  {
    return total_count_;
  }
  gcov_type
  head_count () const
  {
    return head_count_;
  }

  /* Traverse callsites of the current function_instance to find one at the
     location of LINENO and callee name represented in DECL.  */
  function_instance *get_function_instance_by_decl (unsigned lineno,
                                                    tree decl) const;

  /* Merge profile of clones.  Note that cloning hasnt been performed when
     we annotate the CFG (at this stage).  */
  void merge (function_instance *other);

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

private:
  /* Callsite, represented as (decl_lineno, callee_function_name_index).  */
  typedef std::pair<unsigned, unsigned> callsite;

  /* Map from callsite to callee function_instance.  */
  typedef std::map<callsite, function_instance *> callsite_map;

  function_instance (unsigned name, gcov_type head_count)
      : name_ (name), total_count_ (0), head_count_ (head_count)
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
};

/* Store the strings read from the profile data file.  */
static string_table *afdo_string_table;

/* Store the AutoFDO source profile.  */
static autofdo_source_profile *afdo_source_profile;

/* gcov_summary structure to store the profile_info.  */
static gcov_summary *afdo_profile_info;

/* Scaling factor for afdo data.  Compared to normal profile
   AFDO profile counts are much lower, depending on sampling
   frequency.  We scale data up to reudce effects of roundoff
   errors.  */

static gcov_type afdo_count_scale = 1;

/* Helper functions.  */

/* Return the original name of NAME: strip the suffix that starts
   with '.' Caller is responsible for freeing RET.  */

static char *
get_original_name (const char *name)
{
  char *ret = xstrdup (name);
  char *find = strchr (ret, '.');
  if (find != NULL)
    *find = 0;
  return ret;
}

/* Return the combined location, which is a 32bit integer in which
   higher 16 bits stores the line offset of LOC to the start lineno
   of DECL, The lower 16 bits stores the discriminator.  */

static unsigned
get_combined_location (location_t loc, tree decl)
{
  /* TODO: allow more bits for line and less bits for discriminator.  */
  if (LOCATION_LINE (loc) - DECL_SOURCE_LINE (decl) >= (1<<16))
    warning_at (loc, OPT_Woverflow, "offset exceeds 16 bytes");
  return ((LOCATION_LINE (loc) - DECL_SOURCE_LINE (decl)) << 16)
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

/* Dump STACK to F.  */

static void
dump_inline_stack (FILE *f, inline_stack *stack)
{
  bool first = true;
  for (decl_lineno &p : *stack)
    {
      fprintf(f, "%s%s:%i.%i",
	      first ? "" : "; ",
	      IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (p.first)),
	      p.second >> 16,
	      p.second & 65535);
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
              std::make_pair (decl, get_combined_location (locus, decl)));
          locus = tmp_locus;
        }
    }
  stack->safe_push (std::make_pair (fn, get_combined_location (locus, fn)));
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

/* Return STMT's combined location, which is a 32bit integer in which
   higher 16 bits stores the line offset of LOC to the start lineno
   of DECL, The lower 16 bits stores the discriminator.  */

static unsigned
get_relative_location_for_stmt (tree fn, gimple *stmt)
{
  location_t locus = gimple_location (stmt);
  if (LOCATION_LOCUS (locus) == UNKNOWN_LOCATION)
    return UNKNOWN_LOCATION;

  for (tree block = gimple_block (stmt); block && (TREE_CODE (block) == BLOCK);
       block = BLOCK_SUPERCONTEXT (block))
    if (LOCATION_LOCUS (BLOCK_SOURCE_LOCATION (block)) != UNKNOWN_LOCATION)
      return get_combined_location (locus,
                                    get_function_decl_from_block (block));
  return get_combined_location (locus, fn);
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
  char *name
      = get_original_name (IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (decl)));
  int ret = get_index (name);
  free (name);
  if (ret != -1)
    return ret;
  ret = get_index (lang_hooks.dwarf_name (decl, 0));
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
  for (unsigned i = 0; i < string_num; i++)
    {
      vector_.safe_push (get_original_name (gcov_read_string ()));
      map_[vector_.last ()] = i;
    }
  return true;
}

/* Member functions for function_instance.  */

function_instance::~function_instance ()
{
  for (callsite_map::iterator iter = callsites.begin ();
       iter != callsites.end (); ++iter)
    delete iter->second;
}

/* Traverse callsites of the current function_instance to find one at the
   location of LINENO and callee name represented in DECL.  */

function_instance *
function_instance::get_function_instance_by_decl (unsigned lineno,
                                                  tree decl) const
{
  int func_name_idx = afdo_string_table->get_index_by_decl (decl);
  if (func_name_idx != -1)
    {
      callsite_map::const_iterator ret
          = callsites.find (std::make_pair (lineno, func_name_idx));
      if (ret != callsites.end ())
        return ret->second;
    }
  func_name_idx
      = afdo_string_table->get_index (lang_hooks.dwarf_name (decl, 0));
  if (func_name_idx != -1)
    {
      callsite_map::const_iterator ret
          = callsites.find (std::make_pair (lineno, func_name_idx));
      if (ret != callsites.end ())
        return ret->second;
    }
  if (DECL_FROM_INLINE (decl))
    return get_function_instance_by_decl (lineno, DECL_ABSTRACT_ORIGIN (decl));

  return NULL;
}

/* Merge profile of clones.  Note that cloning hasnt been performed when
   we annotate the CFG (at this stage).  */

void function_instance::merge (function_instance *other)
{
  total_count_ += other->total_count_;
  head_count_ += other->head_count_;

  for (callsite_map::const_iterator iter = other->callsites.begin ();
       iter != other->callsites.end (); ++iter)
    if (callsites.count (iter->first) == 0)
      callsites[iter->first] = iter->second;

  for (position_count_map::const_iterator iter = other->pos_counts.begin ();
       iter != other->pos_counts.end (); ++iter)
    if (pos_counts.count (iter->first) == 0)
      pos_counts[iter->first] = iter->second;
    else
      pos_counts[iter->first].count += iter->second.count;
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
      if (iter->first.first != stmt_offset)
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

  for (callsite_map::const_iterator iter = callsites.begin ();
       iter != callsites.end ();)
    if (iter->first.first != stmt_offset)
      ++iter;
    else
      {
	iter = callsites.erase (iter);
	n++;
      }
  /* TODO: If we add support for multiple targets, we may want to
     remove only those we succesfully inlined.  */
  gcc_assert (n);
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
          = read_function_instance (stack, 0);
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

/* Find count_info for a given gimple STMT. If found, store the count_info
   in INFO and return true; otherwise return false.  */

bool
autofdo_source_profile::get_count_info (gimple *stmt, count_info *info,
					cgraph_node *node) const
{
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
  return s->get_count_info (stack[0].second, info);
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
	fprintf (dump_file, " bad locus (funciton end)\n");
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
  stack.safe_push (std::make_pair (edge->callee->decl, 0));

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
		 IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (edge->callee->decl)),
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

  /* Skip the length of the section.  */
  gcov_read_unsigned ();

  /* Read in the function/callsite profile, and store it in local
     data structure.  */
  unsigned function_num = gcov_read_unsigned ();
  int profile_pass_num
	  = g->get_passes ()->get_pass_auto_profile ()->static_pass_number;
  g->get_dumps ()->dump_start (profile_pass_num, NULL);
  for (unsigned i = 0; i < function_num; i++)
    {
      function_instance::function_instance_stack stack;
      function_instance *s = function_instance::read_function_instance (
          &stack, gcov_read_counter ());
      int fun_id = afdo_string_table->get_index
	      (afdo_string_table->get_name (s->name ()));
      /* If function_instace with get_original_name (without the clone
	 suffix) exixts, merge the function instances.  */
      if (map_.count (fun_id) == 0)
	map_[fun_id] = s;
      else
	{
 	  /* Since this is invoked very early, before the pass
	     manager, we need to set up the dumping explicitly.  This is
     	     similar to the handling in finish_optimization_passes.  */
	  if (dump_enabled_p ())
	    {
	      dump_user_location_t loc
		      = dump_user_location_t::from_location_t (input_location);
	      dump_printf_loc (MSG_NOTE, loc, "Merging profile for %s\n",
			    afdo_string_table->get_name (s->name ()));
	    }
	  map_[fun_id]->merge (s);
	}
    }
  /* Scale up the profile, but leave some bits in case some counts gets
     bigger than sum_max eventually.  */
  if (afdo_profile_info->sum_max)
    afdo_count_scale
      = MAX (((gcov_type)1 << (profile_count::n_bits / 2))
	     / afdo_profile_info->sum_max, 1);
  if (dump_file)
    fprintf (dump_file, "Max count in profile %" PRIu64 "\n"
			"Setting scale %" PRIu64 "\n"
			"Scaled max count %" PRIu64 "\n"
			"Hot count threshold %" PRIu64 "\n",
	     (int64_t)afdo_profile_info->sum_max,
	     (int64_t)afdo_count_scale,
	     (int64_t)(afdo_profile_info->sum_max * afdo_count_scale),
	     (int64_t)(afdo_profile_info->sum_max * afdo_count_scale
		       / param_hot_bb_count_fraction));
  afdo_profile_info->sum_max *= afdo_count_scale;
  g->get_dumps ()->dump_finish (profile_pass_num);
  return true;
}

/* Return the function_instance in the profile that correspond to the
   inline STACK.  */

function_instance *
autofdo_source_profile::get_function_instance_by_inline_stack (
    const inline_stack &stack) const
{
  name_function_instance_map::const_iterator iter = map_.find (
      afdo_string_table->get_index_by_decl (stack[stack.length () - 1].first));
  if (iter == map_.end())
    return NULL;
  function_instance *s = iter->second;
  for (unsigned i = stack.length() - 1; i > 0; i--)
    {
      s = s->get_function_instance_by_decl (
          stack[i].second, stack[i - 1].first);
      if (s == NULL)
        return NULL;
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
  if (!afdo_string_table->read())
    {
      error ("cannot read string table from %s", auto_profile_file);
      return;
    }

  /* autofdo_source_profile.  */
  afdo_source_profile = autofdo_source_profile::create ();
  if (afdo_source_profile == NULL)
    {
      error ("cannot read function profile from %s", auto_profile_file);
      return;
    }

  /* autofdo_module_profile.  */
  fake_read_autofdo_module_profile ();
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

/* Update profile_count by known autofdo count.  */
void
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

/* For a given BB, set its execution count. Attach value profile if a stmt
   is not in PROMOTED, because we only want to promote an indirect call once.
   Return TRUE if BB is annotated.  */

static bool
afdo_set_bb_count (basic_block bb, hash_set <basic_block> &zero_bbs)
{
  gimple_stmt_iterator gsi;
  gcov_type max_count = 0;
  bool has_annotated = false;
  if (dump_file)
    fprintf (dump_file, " Looking up AFDO count of bb %i\n", bb->index);

  for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
    {
      count_info info;
      gimple *stmt = gsi_stmt (gsi);
      if (gimple_clobber_p (stmt) || is_gimple_debug (stmt))
	continue;
      if (afdo_source_profile->get_count_info (stmt, &info))
	{
	  if (info.count > max_count)
	    max_count = info.count;
	  if (dump_file && info.count)
	    {
	      fprintf (dump_file, "  count %" PRIu64 " in stmt: ",
		       (int64_t)info.count);
	      print_gimple_stmt (dump_file, stmt, 0, TDF_SLIM);
	    }
	  has_annotated = true;
	  gcall *call = dyn_cast <gcall *> (gsi_stmt (gsi));
	  /* TODO; if inlined early and indirect call was not optimized out,
	     we will end up speculating again.  Early inliner should remove
	     all targets for edges it speculated into safely.  */
	  if (call
	      && info.targets.size () > 0)
	    afdo_vpt (call, info.targets, false, NULL);
	}
    }

  if (!has_annotated)
    {
      /* For an empty BB with all debug stmt which assigne a value with
	 constant, check successors PHIs corresponding to the block and
	 use those counts.  */
      edge tmp_e;
      edge_iterator tmp_ei;
      FOR_EACH_EDGE (tmp_e, tmp_ei, bb->succs)
	{
	  basic_block bb_succ = tmp_e->dest;
	  for (gphi_iterator gpi = gsi_start_phis (bb_succ);
	       !gsi_end_p (gpi);
	       gsi_next (&gpi))
	    {
	      gphi *phi = gpi.phi ();
	      location_t phi_loc
		= gimple_phi_arg_location_from_edge (phi, tmp_e);
	      count_info info;
	      if (afdo_source_profile->get_count_info (phi_loc, &info)
		  && info.count != 0)
		{
		  if (info.count > max_count)
		    max_count = info.count;
		  if (dump_file && info.count)
		    {
		      fprintf (dump_file,
			       "  phi op in BB %i with count %" PRIu64": ",
			       bb_succ->index, (int64_t)info.count);
		      print_gimple_stmt (dump_file, phi, 0, TDF_SLIM);
		    }
		  has_annotated = true;
		}
	    }
	}

      if (!has_annotated)
	return false;
    }

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
	      bb->count = bb1->count;
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
	      bb->count = bb1->count;
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
		 is_succ ? "succesors" : "predecessors", num_edges,
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
	bb->count = total_known_count;
	set_bb_annotated (bb, annotated_bb);
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
	     && total_known_count >= bb->count)
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
   graph. We do the propagation iteratively until stablize.  */

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
	bb->count = ((basic_block)bb->aux)->count;
	set_bb_annotated (bb, annotated_bb);
	if (dump_file)
	  {
	    fprintf (dump_file,
		     "  Copying count of bb %i to bb %i; count is:",
		     ((basic_block)bb->aux)->index,
		     bb->index);
	    bb->count.dump (dump_file);
	  }
      }

  while (changed && i++ < 10)
    {
      changed = false;

      if (afdo_propagate_edge (true, annotated_bb))
        changed = true;
      if (afdo_propagate_edge (false, annotated_bb))
        changed = true;
      afdo_propagate_circuit (*annotated_bb);
    }
  if (dump_file)
    fprintf (dump_file, "Propated in %i iterations %s\n",
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

/* Add scale ORIG/ANNOTATED to SCALES.  */

static void
add_scale (vec <sreal> *scales, profile_count annotated, profile_count orig)
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
	fprintf (dump_file, "    adding scale %.16f\n",
		 scale.to_double ());
      scales->safe_push (scale);
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
  auto_vec <sreal, 20> scales;
  auto_vec <basic_block, 20> stack (n_basic_blocks_for_fn (cfun));

  basic_block seed_bb;
  unsigned int component_id = 1;

  /* Map from basic block to its component.
     0   is used for univisited BBs,
     1   means that BB is annotated,
     >=2 is an id of the component BB belongs to.  */
  auto_vec <unsigned int, 20> component;
  component.safe_grow (last_basic_block_for_fn (cfun));
  FOR_ALL_BB_FN (seed_bb, cfun)
    component[seed_bb->index]
	= is_bb_annotated (seed_bb, *annotated_bb) ? 1 : 0;
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
	   max_count = max_count.max (b->count);

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
		     fprintf (dump_file, "    Annotated predecesor %i "
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
		   else if (component[e->src->index] == component_id)
		     out_count += e->count ();
		   else if (e->probability.nonzero_p ())
		     {
		       ok = false;
		       break;
		     }
		 if (!ok)
		   continue;
		 if (dump_file)
		   fprintf (dump_file,
			    "    edge %i->%i has annotated sucessor; count ",
			    b->index, e->dest->index);
		 add_scale (&scales, annotated_count, e->count ());
	       }

	 }

       /* If we failed to find annotated entry or exit edge,
	  look for exit edges and scale profile so the dest
	  BB get all flow it needs.  This is inprecise because
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
			      "    edge %i->%i has annotated sucessor;"
			      " upper bound count ",
			      b->index, e->dest->index);
		   add_scale (&scales, annotated_count, e->count ());
		 }
       if (!scales.length ())
	 {
	   if (dump_file)
	     fprintf (dump_file,
		      "  Can not determine count from the boundary; giving up");
	   continue;
	 }
       gcc_checking_assert (scales.length ());
       scales.qsort (cmp);
       scale_bbs (bbs, scales[scales.length () / 2]);
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
	}
    }

  afdo_find_equiv_class (annotated_bb);
  afdo_propagate (annotated_bb);

  FOR_EACH_BB_FN (bb, cfun)
    if (is_bb_annotated (bb, *annotated_bb))
      {
	bool all_known = false;
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
		    all_known = true;
		    break;
		  }
	      }
	    else
	      total_count += AFDO_EINFO (e)->get_count ();
	  }
	if (!all_known || !total_count.nonzero_p ())
	  continue;

	FOR_EACH_EDGE (e, ei, bb->succs)
	  if (AFDO_EINFO (e)->is_annotated ())
	    {
	      /* If probability is 1, preserve reliable static prediction
		 (This is, for example the case of single fallthru edge
		  or single fallthru plus unlikely EH edge.)  */
	      if (AFDO_EINFO (e)->get_count () == total_count
		  && e->probability == profile_probability::always ())
		;
	      else if (AFDO_EINFO (e)->get_count ().nonzero_p ())
		e->probability
		  = AFDO_EINFO (e)->get_count ().probability_in (total_count);
	      /* If probability is zero, preserve reliable static
		 prediction.  */
	      else if (e->probability.nonzero_p ()
		       || e->probability.quality () == GUESSED)
		e->probability = profile_probability::never ().afdo ();
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
	fprintf (dump_file, "No afdo profile for %s",
		 cgraph_node::get (current_function_decl)->dump_name ());
      return;
    }

  calculate_dominance_info (CDI_POST_DOMINATORS);
  calculate_dominance_info (CDI_DOMINATORS);
  loop_optimizer_init (0);

  if (dump_file)
    fprintf (dump_file, "\n\nAnnotating BB profile of %s\n",
	     cgraph_node::get (current_function_decl)->dump_name ());

  /* In the first pass only store non-zero counts.  */
  gcov_type head_count = s->head_count () * autofdo::afdo_count_scale;
  bool profile_found = head_count > 0;
  hash_set <basic_block> zero_bbs;
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
      FOR_ALL_BB_FN (bb, cfun)
	if (bb->count.quality () == GUESSED_LOCAL)
	  bb->count = bb->count.global0afdo ();

      loop_optimizer_finalize ();
      free_dominance_info (CDI_DOMINATORS);
      free_dominance_info (CDI_POST_DOMINATORS);
      return;
    }

  /* Update profile.  */
  if (head_count)
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

  cgraph_node::get(current_function_decl)->count
      = ENTRY_BLOCK_PTR_FOR_FN(cfun)->count;
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

/* Wrapper function to invoke early inliner.  */

static unsigned int
early_inline ()
{
  compute_fn_summary (cgraph_node::get (current_function_decl), true);
  unsigned int todo = early_inliner (cfun);
  if (todo & TODO_update_ssa_any)
    update_ssa (TODO_update_ssa);
  return todo;
}

/* Use AutoFDO profile to annoate the control flow graph.
   Return the todo flag.  */

static unsigned int
auto_profile (void)
{
  struct cgraph_node *node;

  if (symtab->state == FINISHED)
    return 0;

  init_node_map (true);
  profile_info = autofdo::afdo_profile_info;

  FOR_EACH_FUNCTION (node)
  {
    if (!gimple_has_body_p (node->decl))
      continue;

    /* Don't profile functions produced for builtin stuff.  */
    if (DECL_SOURCE_LOCATION (node->decl) == BUILTINS_LOCATION)
      continue;

    push_cfun (DECL_STRUCT_FUNCTION (node->decl));

    unsigned int todo = early_inline ();
    autofdo::afdo_annotate_cfg ();
    compute_function_frequency ();

    /* Local pure-const may imply need to fixup the cfg.  */
    todo |= execute_fixup_cfg ();
    if (todo & TODO_cleanup_cfg)
      cleanup_tree_cfg ();

    free_dominance_info (CDI_DOMINATORS);
    free_dominance_info (CDI_POST_DOMINATORS);
    cgraph_edge::rebuild_edges ();
    compute_fn_summary (cgraph_node::get (current_function_decl), true);
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
      gcov_summary *saved_profile_info = profile_info;
      /* At early inline stage, profile_info is not set yet. We need to
         temporarily set it to afdo_profile_info to calculate hotness.  */
      profile_info = autofdo::afdo_profile_info;
      is_hot = maybe_hot_count_p (NULL, pcount);
      if (dump_file)
	{
	  fprintf (dump_file, "Call %s -> %s has %s afdo profile count ",
		   edge->caller->dump_name (), edge->callee->dump_name (),
		   is_hot ? "hot" : "cold");
	  pcount.dump (dump_file);
	  fprintf (dump_file, "\n");
	}
      profile_info = saved_profile_info;
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
	  autofdo::count_info info;
	  gimple *stmt = gsi_stmt (gsi);
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
