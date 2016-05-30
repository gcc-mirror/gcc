/* Read and annotate call graph profile from the auto profile data file.
   Copyright (C) 2014-2016 Free Software Foundation, Inc.
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
#include "cfgloop.h"
#include "tree-cfg.h"
#include "tree-cfgcleanup.h"
#include "tree-into-ssa.h"
#include "gimple-iterator.h"
#include "value-prof.h"
#include "params.h"
#include "symbol-summary.h"
#include "ipa-prop.h"
#include "ipa-inline.h"
#include "tree-inline.h"
#include "auto-profile.h"

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
#define AUTO_PROFILE_VERSION 1

namespace autofdo
{

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
struct count_info
{
  /* Sampled count of the inline stack.  */
  gcov_type count;

  /* Map from indirect call target to its sample count.  */
  icall_target_map targets;

  /* Whether this inline stack is already used in annotation.

     Each inline stack should only be used to annotate IR once.
     This will be enforced when instruction-level discriminator
     is supported.  */
  bool annotated;
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

  /* Store the profile info for LOC in INFO. Return TRUE if profile info
     is found.  */
  bool get_count_info (location_t loc, count_info *info) const;

  /* Read the inlined indirect call target profile for STMT and store it in
     MAP, return the total count for all inlined indirect calls.  */
  gcov_type find_icall_target_map (gcall *stmt, icall_target_map *map) const;

  /* Sum of counts that is used during annotation.  */
  gcov_type total_annotated_count () const;

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
     in INFO and return true; otherwise return false.  */
  bool get_count_info (gimple *stmt, count_info *info) const;

  /* Find total count of the callee of EDGE.  */
  gcov_type get_callsite_total_count (struct cgraph_edge *edge) const;

  /* Update value profile INFO for STMT from the inlined indirect callsite.
     Return true if INFO is updated.  */
  bool update_inlined_ind_target (gcall *stmt, count_info *info);

  /* Mark LOC as annotated.  */
  void mark_annotated (location_t loc);

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

/* gcov_ctr_summary structure to store the profile_info.  */
static struct gcov_ctr_summary *afdo_profile_info;

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
    warning_at (loc, OPT_Woverflow, "Offset exceeds 16 bytes.");
  return ((LOCATION_LINE (loc) - DECL_SOURCE_LINE (decl)) << 16);
}

/* Return the function decl of a given lexical BLOCK.  */

static tree
get_function_decl_from_block (tree block)
{
  tree decl;

  if (LOCATION_LOCUS (BLOCK_SOURCE_LOCATION (block) == UNKNOWN_LOCATION))
    return NULL_TREE;

  for (decl = BLOCK_ABSTRACT_ORIGIN (block);
       decl && (TREE_CODE (decl) == BLOCK);
       decl = BLOCK_ABSTRACT_ORIGIN (decl))
    if (TREE_CODE (decl) == FUNCTION_DECL)
      break;
  return decl;
}

/* Store inline stack for STMT in STACK.  */

static void
get_inline_stack (location_t locus, inline_stack *stack)
{
  if (LOCATION_LOCUS (locus) == UNKNOWN_LOCATION)
    return;

  tree block = LOCATION_BLOCK (locus);
  if (block && TREE_CODE (block) == BLOCK)
    {
      int level = 0;
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
          level++;
        }
    }
  stack->safe_push (
      std::make_pair (current_function_decl,
                      get_combined_location (locus, current_function_decl)));
}

/* Return STMT's combined location, which is a 32bit integer in which
   higher 16 bits stores the line offset of LOC to the start lineno
   of DECL, The lower 16 bits stores the discriminator.  */

static unsigned
get_relative_location_for_stmt (gimple *stmt)
{
  location_t locus = gimple_location (stmt);
  if (LOCATION_LOCUS (locus) == UNKNOWN_LOCATION)
    return UNKNOWN_LOCATION;

  for (tree block = gimple_block (stmt); block && (TREE_CODE (block) == BLOCK);
       block = BLOCK_SUPERCONTEXT (block))
    if (LOCATION_LOCUS (BLOCK_SOURCE_LOCATION (block)) != UNKNOWN_LOCATION)
      return get_combined_location (locus,
                                    get_function_decl_from_block (block));
  return get_combined_location (locus, current_function_decl);
}

/* Return true if BB contains indirect call.  */

static bool
has_indirect_call (basic_block bb)
{
  gimple_stmt_iterator gsi;

  for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
    {
      gimple *stmt = gsi_stmt (gsi);
      if (gimple_code (stmt) == GIMPLE_CALL && !gimple_call_internal_p (stmt)
          && (gimple_call_fn (stmt) == NULL
              || TREE_CODE (gimple_call_fn (stmt)) != FUNCTION_DECL))
        return true;
    }
  return false;
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
  if (DECL_ABSTRACT_ORIGIN (decl))
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
  if (DECL_ABSTRACT_ORIGIN (decl))
    return get_function_instance_by_decl (lineno, DECL_ABSTRACT_ORIGIN (decl));

  return NULL;
}

/* Store the profile info for LOC in INFO. Return TRUE if profile info
   is found.  */

bool
function_instance::get_count_info (location_t loc, count_info *info) const
{
  position_count_map::const_iterator iter = pos_counts.find (loc);
  if (iter == pos_counts.end ())
    return false;
  *info = iter->second;
  return true;
}

/* Mark LOC as annotated.  */

void
function_instance::mark_annotated (location_t loc)
{
  position_count_map::iterator iter = pos_counts.find (loc);
  if (iter == pos_counts.end ())
    return;
  iter->second.annotated = true;
}

/* Read the inlined indirect call target profile for STMT and store it in
   MAP, return the total count for all inlined indirect calls.  */

gcov_type
function_instance::find_icall_target_map (gcall *stmt,
                                          icall_target_map *map) const
{
  gcov_type ret = 0;
  unsigned stmt_offset = get_relative_location_for_stmt (stmt);

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
      if (!check_ic_target (stmt, node))
        continue;
      (*map)[callee] = iter->second->total_count ();
      ret += iter->second->total_count ();
    }
  return ret;
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
      unsigned offset = gcov_read_unsigned () & 0xffff0000;
      unsigned num_targets = gcov_read_unsigned ();
      gcov_type count = gcov_read_counter ();
      s->pos_counts[offset].count = count;
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

/* Sum of counts that is used during annotation.  */

gcov_type
function_instance::total_annotated_count () const
{
  gcov_type ret = 0;
  for (callsite_map::const_iterator iter = callsites.begin ();
       iter != callsites.end (); ++iter)
    ret += iter->second->total_annotated_count ();
  for (position_count_map::const_iterator iter = pos_counts.begin ();
       iter != pos_counts.end (); ++iter)
    if (iter->second.annotated)
      ret += iter->second.count;
  return ret;
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
autofdo_source_profile::get_count_info (gimple *stmt, count_info *info) const
{
  if (LOCATION_LOCUS (gimple_location (stmt)) == cfun->function_end_locus)
    return false;

  inline_stack stack;
  get_inline_stack (gimple_location (stmt), &stack);
  if (stack.length () == 0)
    return false;
  function_instance *s = get_function_instance_by_inline_stack (stack);
  if (s == NULL)
    return false;
  return s->get_count_info (stack[0].second, info);
}

/* Mark LOC as annotated.  */

void
autofdo_source_profile::mark_annotated (location_t loc)
{
  inline_stack stack;
  get_inline_stack (loc, &stack);
  if (stack.length () == 0)
    return;
  function_instance *s = get_function_instance_by_inline_stack (stack);
  if (s == NULL)
    return;
  s->mark_annotated (stack[0].second);
}

/* Update value profile INFO for STMT from the inlined indirect callsite.
   Return true if INFO is updated.  */

bool
autofdo_source_profile::update_inlined_ind_target (gcall *stmt,
                                                   count_info *info)
{
  if (LOCATION_LOCUS (gimple_location (stmt)) == cfun->function_end_locus)
    return false;

  count_info old_info;
  get_count_info (stmt, &old_info);
  gcov_type total = 0;
  for (icall_target_map::const_iterator iter = old_info.targets.begin ();
       iter != old_info.targets.end (); ++iter)
    total += iter->second;

  /* Program behavior changed, original promoted (and inlined) target is not
     hot any more. Will avoid promote the original target.

     To check if original promoted target is still hot, we check the total
     count of the unpromoted targets (stored in old_info). If it is no less
     than half of the callsite count (stored in INFO), the original promoted
     target is considered not hot any more.  */
  if (total >= info->count / 2)
    return false;

  inline_stack stack;
  get_inline_stack (gimple_location (stmt), &stack);
  if (stack.length () == 0)
    return false;
  function_instance *s = get_function_instance_by_inline_stack (stack);
  if (s == NULL)
    return false;
  icall_target_map map;
  if (s->find_icall_target_map (stmt, &map) == 0)
    return false;
  for (icall_target_map::const_iterator iter = map.begin ();
       iter != map.end (); ++iter)
    info->targets[iter->first] = iter->second;
  return true;
}

/* Find total count of the callee of EDGE.  */

gcov_type
autofdo_source_profile::get_callsite_total_count (
    struct cgraph_edge *edge) const
{
  inline_stack stack;
  stack.safe_push (std::make_pair (edge->callee->decl, 0));
  get_inline_stack (gimple_location (edge->call_stmt), &stack);

  function_instance *s = get_function_instance_by_inline_stack (stack);
  if (s == NULL
      || afdo_string_table->get_index (IDENTIFIER_POINTER (
             DECL_ASSEMBLER_NAME (edge->callee->decl))) != s->name ())
    return 0;

  return s->total_count ();
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
      inform (0, "Not expected TAG.");
      return false;
    }

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
      afdo_profile_info->sum_all += s->total_count ();
      map_[s->name ()] = s;
    }
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
      error ("Cannot open profile file %s.", auto_profile_file);
      return;
    }

  if (gcov_read_unsigned () != GCOV_DATA_MAGIC)
    {
      error ("AutoFDO profile magic number does not match.");
      return;
    }

  /* Skip the version number.  */
  unsigned version = gcov_read_unsigned ();
  if (version != AUTO_PROFILE_VERSION)
    {
      error ("AutoFDO profile version %u does match %u.",
	     version, AUTO_PROFILE_VERSION);
      return;
    }

  /* Skip the empty integer.  */
  gcov_read_unsigned ();

  /* string_table.  */
  afdo_string_table = new string_table ();
  if (!afdo_string_table->read())
    {
      error ("Cannot read string table from %s.", auto_profile_file);
      return;
    }

  /* autofdo_source_profile.  */
  afdo_source_profile = autofdo_source_profile::create ();
  if (afdo_source_profile == NULL)
    {
      error ("Cannot read function profile from %s.", auto_profile_file);
      return;
    }

  /* autofdo_module_profile.  */
  fake_read_autofdo_module_profile ();

  /* Read in the working set.  */
  if (gcov_read_unsigned () != GCOV_TAG_AFDO_WORKING_SET)
    {
      error ("Cannot read working set from %s.", auto_profile_file);
      return;
    }

  /* Skip the length of the section.  */
  gcov_read_unsigned ();
  gcov_working_set_t set[128];
  for (unsigned i = 0; i < 128; i++)
    {
      set[i].num_counters = gcov_read_unsigned ();
      set[i].min_counter = gcov_read_counter ();
    }
  add_working_set (set);
}

/* From AutoFDO profiles, find values inside STMT for that we want to measure
   histograms for indirect-call optimization.

   This function is actually served for 2 purposes:
     * before annotation, we need to mark histogram, promote and inline
     * after annotation, we just need to mark, and let follow-up logic to
       decide if it needs to promote and inline.  */

static void
afdo_indirect_call (gimple_stmt_iterator *gsi, const icall_target_map &map,
                    bool transform)
{
  gimple *gs = gsi_stmt (*gsi);
  tree callee;

  if (map.size () == 0)
    return;
  gcall *stmt = dyn_cast <gcall *> (gs);
  if ((!stmt) || gimple_call_fndecl (stmt) != NULL_TREE)
    return;

  callee = gimple_call_fn (stmt);

  histogram_value hist = gimple_alloc_histogram_value (
      cfun, HIST_TYPE_INDIR_CALL, stmt, callee);
  hist->n_counters = 3;
  hist->hvalue.counters = XNEWVEC (gcov_type, hist->n_counters);
  gimple_add_histogram_value (cfun, stmt, hist);

  gcov_type total = 0;
  icall_target_map::const_iterator max_iter = map.end ();

  for (icall_target_map::const_iterator iter = map.begin ();
       iter != map.end (); ++iter)
    {
      total += iter->second;
      if (max_iter == map.end () || max_iter->second < iter->second)
        max_iter = iter;
    }

  hist->hvalue.counters[0]
      = (unsigned long long)afdo_string_table->get_name (max_iter->first);
  hist->hvalue.counters[1] = max_iter->second;
  hist->hvalue.counters[2] = total;

  if (!transform)
    return;

  struct cgraph_edge *indirect_edge
      = cgraph_node::get (current_function_decl)->get_edge (stmt);
  struct cgraph_node *direct_call = cgraph_node::get_for_asmname (
      get_identifier ((const char *) hist->hvalue.counters[0]));

  if (direct_call == NULL || !check_ic_target (stmt, direct_call))
    return;
  if (DECL_STRUCT_FUNCTION (direct_call->decl) == NULL)
    return;
  struct cgraph_edge *new_edge
      = indirect_edge->make_speculative (direct_call, 0, 0);
  new_edge->redirect_call_stmt_to_callee ();
  gimple_remove_histogram_value (cfun, stmt, hist);
  inline_call (new_edge, true, NULL, NULL, false);
}

/* From AutoFDO profiles, find values inside STMT for that we want to measure
   histograms and adds them to list VALUES.  */

static void
afdo_vpt (gimple_stmt_iterator *gsi, const icall_target_map &map,
          bool transform)
{
  afdo_indirect_call (gsi, map, transform);
}

typedef std::set<basic_block> bb_set;
typedef std::set<edge> edge_set;

static bool
is_bb_annotated (const basic_block bb, const bb_set &annotated)
{
  return annotated.find (bb) != annotated.end ();
}

static void
set_bb_annotated (basic_block bb, bb_set *annotated)
{
  annotated->insert (bb);
}

static bool
is_edge_annotated (const edge e, const edge_set &annotated)
{
  return annotated.find (e) != annotated.end ();
}

static void
set_edge_annotated (edge e, edge_set *annotated)
{
  annotated->insert (e);
}

/* For a given BB, set its execution count. Attach value profile if a stmt
   is not in PROMOTED, because we only want to promote an indirect call once.
   Return TRUE if BB is annotated.  */

static bool
afdo_set_bb_count (basic_block bb, const stmt_set &promoted)
{
  gimple_stmt_iterator gsi;
  edge e;
  edge_iterator ei;
  gcov_type max_count = 0;
  bool has_annotated = false;

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
          has_annotated = true;
          if (info.targets.size () > 0
              && promoted.find (stmt) == promoted.end ())
            afdo_vpt (&gsi, info.targets, false);
        }
    }

  if (!has_annotated)
    return false;

  for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
    afdo_source_profile->mark_annotated (gimple_location (gsi_stmt (gsi)));
  for (gphi_iterator gpi = gsi_start_phis (bb);
       !gsi_end_p (gpi);
       gsi_next (&gpi))
    {
      gphi *phi = gpi.phi ();
      size_t i;
      for (i = 0; i < gimple_phi_num_args (phi); i++)
        afdo_source_profile->mark_annotated (gimple_phi_arg_location (phi, i));
    }
  FOR_EACH_EDGE (e, ei, bb->succs)
  afdo_source_profile->mark_annotated (e->goto_locus);

  bb->count = max_count;
  return true;
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
    vec<basic_block> dom_bbs;
    basic_block bb1;
    int i;

    if (bb->aux != NULL)
      continue;
    bb->aux = bb;
    dom_bbs = get_dominated_by (CDI_DOMINATORS, bb);
    FOR_EACH_VEC_ELT (dom_bbs, i, bb1)
      if (bb1->aux == NULL && dominated_by_p (CDI_POST_DOMINATORS, bb, bb1)
	  && bb1->loop_father == bb->loop_father)
	{
	  bb1->aux = bb;
	  if (bb1->count > bb->count && is_bb_annotated (bb1, *annotated_bb))
	    {
	      bb->count = bb1->count;
	      set_bb_annotated (bb, annotated_bb);
	    }
	}
    dom_bbs = get_dominated_by (CDI_POST_DOMINATORS, bb);
    FOR_EACH_VEC_ELT (dom_bbs, i, bb1)
      if (bb1->aux == NULL && dominated_by_p (CDI_DOMINATORS, bb, bb1)
	  && bb1->loop_father == bb->loop_father)
	{
	  bb1->aux = bb;
	  if (bb1->count > bb->count && is_bb_annotated (bb1, *annotated_bb))
	    {
	      bb->count = bb1->count;
	      set_bb_annotated (bb, annotated_bb);
	    }
	}
  }
}

/* If a basic block's count is known, and only one of its in/out edges' count
   is unknown, its count can be calculated. Meanwhile, if all of the in/out
   edges' counts are known, then the basic block's unknown count can also be
   calculated.
   IS_SUCC is true if out edges of a basic blocks are examined.
   Update ANNOTATED_BB and ANNOTATED_EDGE accordingly.
   Return TRUE if any basic block/edge count is changed.  */

static bool
afdo_propagate_edge (bool is_succ, bb_set *annotated_bb,
                     edge_set *annotated_edge)
{
  basic_block bb;
  bool changed = false;

  FOR_EACH_BB_FN (bb, cfun)
  {
    edge e, unknown_edge = NULL;
    edge_iterator ei;
    int num_unknown_edge = 0;
    gcov_type total_known_count = 0;

    FOR_EACH_EDGE (e, ei, is_succ ? bb->succs : bb->preds)
      if (!is_edge_annotated (e, *annotated_edge))
	num_unknown_edge++, unknown_edge = e;
      else
	total_known_count += e->count;

    if (num_unknown_edge == 0)
      {
        if (total_known_count > bb->count)
          {
            bb->count = total_known_count;
            changed = true;
          }
        if (!is_bb_annotated (bb, *annotated_bb))
          {
            set_bb_annotated (bb, annotated_bb);
            changed = true;
          }
      }
    else if (num_unknown_edge == 1 && is_bb_annotated (bb, *annotated_bb))
      {
        if (bb->count >= total_known_count)
          unknown_edge->count = bb->count - total_known_count;
        else
          unknown_edge->count = 0;
        set_edge_annotated (unknown_edge, annotated_edge);
        changed = true;
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
   count of BB1->BB.t1, BB.t1->BB.t2.
   Update ANNOTATED_EDGE accordingly.  */

static void
afdo_propagate_circuit (const bb_set &annotated_bb, edge_set *annotated_edge)
{
  basic_block bb;
  FOR_ALL_BB_FN (bb, cfun)
  {
    gimple *def_stmt;
    tree cmp_rhs, cmp_lhs;
    gimple *cmp_stmt = last_stmt (bb);
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
      if (!is_edge_annotated (e, *annotated_edge))
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
          if (e->probability == 0 && !is_edge_annotated (ep, *annotated_edge))
            {
              ep->probability = 0;
              ep->count = 0;
              set_edge_annotated (ep, annotated_edge);
            }
        }
      if (total == 1 && !is_edge_annotated (only_one, *annotated_edge))
        {
          only_one->probability = e->probability;
          only_one->count = e->count;
          set_edge_annotated (only_one, annotated_edge);
        }
    }
  }
}

/* Propagate the basic block count and edge count on the control flow
   graph. We do the propagation iteratively until stablize.  */

static void
afdo_propagate (bb_set *annotated_bb, edge_set *annotated_edge)
{
  basic_block bb;
  bool changed = true;
  int i = 0;

  FOR_ALL_BB_FN (bb, cfun)
  {
    bb->count = ((basic_block)bb->aux)->count;
    if (is_bb_annotated ((const basic_block)bb->aux, *annotated_bb))
      set_bb_annotated (bb, annotated_bb);
  }

  while (changed && i++ < 10)
    {
      changed = false;

      if (afdo_propagate_edge (true, annotated_bb, annotated_edge))
        changed = true;
      if (afdo_propagate_edge (false, annotated_bb, annotated_edge))
        changed = true;
      afdo_propagate_circuit (*annotated_bb, annotated_edge);
    }
}

/* Propagate counts on control flow graph and calculate branch
   probabilities.  */

static void
afdo_calculate_branch_prob (bb_set *annotated_bb, edge_set *annotated_edge)
{
  basic_block bb;
  bool has_sample = false;

  FOR_EACH_BB_FN (bb, cfun)
  {
    if (bb->count > 0)
      {
	has_sample = true;
	break;
      }
  }

  if (!has_sample)
    return;

  calculate_dominance_info (CDI_POST_DOMINATORS);
  calculate_dominance_info (CDI_DOMINATORS);
  loop_optimizer_init (0);

  afdo_find_equiv_class (annotated_bb);
  afdo_propagate (annotated_bb, annotated_edge);

  FOR_EACH_BB_FN (bb, cfun)
  {
    edge e;
    edge_iterator ei;
    int num_unknown_succ = 0;
    gcov_type total_count = 0;

    FOR_EACH_EDGE (e, ei, bb->succs)
    {
      if (!is_edge_annotated (e, *annotated_edge))
        num_unknown_succ++;
      else
        total_count += e->count;
    }
    if (num_unknown_succ == 0 && total_count > 0)
      {
        FOR_EACH_EDGE (e, ei, bb->succs)
        e->probability = (double)e->count * REG_BR_PROB_BASE / total_count;
      }
  }
  FOR_ALL_BB_FN (bb, cfun)
  {
    edge e;
    edge_iterator ei;

    FOR_EACH_EDGE (e, ei, bb->succs)
      e->count = (double)bb->count * e->probability / REG_BR_PROB_BASE;
    bb->aux = NULL;
  }

  loop_optimizer_finalize ();
  free_dominance_info (CDI_DOMINATORS);
  free_dominance_info (CDI_POST_DOMINATORS);
}

/* Perform value profile transformation using AutoFDO profile. Add the
   promoted stmts to PROMOTED_STMTS. Return TRUE if there is any
   indirect call promoted.  */

static bool
afdo_vpt_for_early_inline (stmt_set *promoted_stmts)
{
  basic_block bb;
  if (afdo_source_profile->get_function_instance_by_decl (
          current_function_decl) == NULL)
    return false;

  compute_inline_parameters (cgraph_node::get (current_function_decl), true);

  bool has_vpt = false;
  FOR_EACH_BB_FN (bb, cfun)
  {
    if (!has_indirect_call (bb))
      continue;
    gimple_stmt_iterator gsi;

    gcov_type bb_count = 0;
    for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
      {
        count_info info;
	gimple *stmt = gsi_stmt (gsi);
        if (afdo_source_profile->get_count_info (stmt, &info))
          bb_count = MAX (bb_count, info.count);
      }

    for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
      {
        gcall *stmt = dyn_cast <gcall *> (gsi_stmt (gsi));
        /* IC_promotion and early_inline_2 is done in multiple iterations.
           No need to promoted the stmt if its in promoted_stmts (means
           it is already been promoted in the previous iterations).  */
        if ((!stmt) || gimple_call_fn (stmt) == NULL
            || TREE_CODE (gimple_call_fn (stmt)) == FUNCTION_DECL
            || promoted_stmts->find (stmt) != promoted_stmts->end ())
          continue;

        count_info info;
        afdo_source_profile->get_count_info (stmt, &info);
        info.count = bb_count;
        if (afdo_source_profile->update_inlined_ind_target (stmt, &info))
          {
            /* Promote the indirect call and update the promoted_stmts.  */
            promoted_stmts->insert (stmt);
            afdo_vpt (&gsi, info.targets, true);
            has_vpt = true;
          }
      }
  }

  if (has_vpt)
    {
      optimize_inline_calls (current_function_decl);
      return true;
    }

  return false;
}

/* Annotate auto profile to the control flow graph. Do not annotate value
   profile for stmts in PROMOTED_STMTS.  */

static void
afdo_annotate_cfg (const stmt_set &promoted_stmts)
{
  basic_block bb;
  bb_set annotated_bb;
  edge_set annotated_edge;
  const function_instance *s
      = afdo_source_profile->get_function_instance_by_decl (
          current_function_decl);

  if (s == NULL)
    return;
  cgraph_node::get (current_function_decl)->count = s->head_count ();
  ENTRY_BLOCK_PTR_FOR_FN (cfun)->count = s->head_count ();
  gcov_type max_count = ENTRY_BLOCK_PTR_FOR_FN (cfun)->count;

  FOR_EACH_BB_FN (bb, cfun)
  {
    edge e;
    edge_iterator ei;

    bb->count = 0;
    FOR_EACH_EDGE (e, ei, bb->succs)
      e->count = 0;

    if (afdo_set_bb_count (bb, promoted_stmts))
      set_bb_annotated (bb, &annotated_bb);
    if (bb->count > max_count)
      max_count = bb->count;
  }
  if (ENTRY_BLOCK_PTR_FOR_FN (cfun)->count
      > ENTRY_BLOCK_PTR_FOR_FN (cfun)->next_bb->count)
    {
      ENTRY_BLOCK_PTR_FOR_FN (cfun)->next_bb->count
          = ENTRY_BLOCK_PTR_FOR_FN (cfun)->count;
      set_bb_annotated (ENTRY_BLOCK_PTR_FOR_FN (cfun)->next_bb, &annotated_bb);
    }
  if (ENTRY_BLOCK_PTR_FOR_FN (cfun)->count
      > EXIT_BLOCK_PTR_FOR_FN (cfun)->prev_bb->count)
    {
      EXIT_BLOCK_PTR_FOR_FN (cfun)->prev_bb->count
          = ENTRY_BLOCK_PTR_FOR_FN (cfun)->count;
      set_bb_annotated (EXIT_BLOCK_PTR_FOR_FN (cfun)->prev_bb, &annotated_bb);
    }
  afdo_source_profile->mark_annotated (
      DECL_SOURCE_LOCATION (current_function_decl));
  afdo_source_profile->mark_annotated (cfun->function_start_locus);
  afdo_source_profile->mark_annotated (cfun->function_end_locus);
  if (max_count > 0)
    {
      afdo_calculate_branch_prob (&annotated_bb, &annotated_edge);
      counts_to_freqs ();
      profile_status_for_fn (cfun) = PROFILE_READ;
    }
  if (flag_value_profile_transformations)
    {
      gimple_value_profile_transformations ();
      free_dominance_info (CDI_DOMINATORS);
      free_dominance_info (CDI_POST_DOMINATORS);
      update_ssa (TODO_update_ssa);
    }
}

/* Wrapper function to invoke early inliner.  */

static void
early_inline ()
{
  compute_inline_parameters (cgraph_node::get (current_function_decl), true);
  unsigned todo = early_inliner (cfun);
  if (todo & TODO_update_ssa_any)
    update_ssa (TODO_update_ssa);
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

    /* First do indirect call promotion and early inline to make the
       IR match the profiled binary before actual annotation.

       This is needed because an indirect call might have been promoted
       and inlined in the profiled binary. If we do not promote and
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
    autofdo::stmt_set promoted_stmts;
    for (int i = 0; i < PARAM_VALUE (PARAM_EARLY_INLINER_MAX_ITERATIONS); i++)
      {
        if (!flag_value_profile_transformations
            || !autofdo::afdo_vpt_for_early_inline (&promoted_stmts))
          break;
        early_inline ();
      }

    early_inline ();
    autofdo::afdo_annotate_cfg (promoted_stmts);
    compute_function_frequency ();

    /* Local pure-const may imply need to fixup the cfg.  */
    if (execute_fixup_cfg () & TODO_cleanup_cfg)
      cleanup_tree_cfg ();

    free_dominance_info (CDI_DOMINATORS);
    free_dominance_info (CDI_POST_DOMINATORS);
    cgraph_edge::rebuild_edges ();
    compute_inline_parameters (cgraph_node::get (current_function_decl), true);
    pop_cfun ();
  }

  return TODO_rebuild_cgraph_edges;
}
} /* namespace autofdo.  */

/* Read the profile from the profile data file.  */

void
read_autofdo_file (void)
{
  if (auto_profile_file == NULL)
    auto_profile_file = DEFAULT_AUTO_PROFILE_FILE;

  autofdo::afdo_profile_info = (struct gcov_ctr_summary *)xcalloc (
      1, sizeof (struct gcov_ctr_summary));
  autofdo::afdo_profile_info->runs = 1;
  autofdo::afdo_profile_info->sum_max = 0;
  autofdo::afdo_profile_info->sum_all = 0;

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
      const struct gcov_ctr_summary *saved_profile_info = profile_info;
      /* At early inline stage, profile_info is not set yet. We need to
         temporarily set it to afdo_profile_info to calculate hotness.  */
      profile_info = autofdo::afdo_profile_info;
      is_hot = maybe_hot_count_p (NULL, count);
      profile_info = saved_profile_info;
      return is_hot;
    }

  return false;
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
  virtual bool
  gate (function *)
  {
    return flag_auto_profile;
  }
  virtual unsigned int
  execute (function *)
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
