/* Alias analysis for trees.
   Copyright (C) 2004, 2005 Free Software Foundation, Inc.
   Contributed by Diego Novillo <dnovillo@redhat.com>

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "tree.h"
#include "rtl.h"
#include "tm_p.h"
#include "hard-reg-set.h"
#include "basic-block.h"
#include "timevar.h"
#include "expr.h"
#include "ggc.h"
#include "langhooks.h"
#include "flags.h"
#include "function.h"
#include "diagnostic.h"
#include "tree-dump.h"
#include "tree-gimple.h"
#include "tree-flow.h"
#include "tree-inline.h"
#include "tree-pass.h"
#include "convert.h"
#include "params.h"

/* 'true' after aliases have been computed (see compute_may_aliases).  */
bool aliases_computed_p;

/* Structure to map a variable to its alias set and keep track of the
   virtual operands that will be needed to represent it.  */
struct alias_map_d
{
  /* Variable and its alias set.  */
  tree var;
  HOST_WIDE_INT set;

  /* Total number of virtual operands that will be needed to represent
     all the aliases of VAR.  */
  long total_alias_vops;

  /* Nonzero if the aliases for this memory tag have been grouped
     already.  Used in group_aliases.  */
  unsigned int grouped_p : 1;

  /* Set of variables aliased with VAR.  This is the exact same
     information contained in VAR_ANN (VAR)->MAY_ALIASES, but in
     bitmap form to speed up alias grouping.  */
  sbitmap may_aliases;
};


/* Alias information used by compute_may_aliases and its helpers.  */
struct alias_info
{
  /* SSA names visited while collecting points-to information.  If bit I
     is set, it means that SSA variable with version I has already been
     visited.  */
  sbitmap ssa_names_visited;

  /* Array of SSA_NAME pointers processed by the points-to collector.  */
  varray_type processed_ptrs;

  /* Variables whose address is still needed.  */
  bitmap addresses_needed;

  /* ADDRESSABLE_VARS contains all the global variables and locals that
     have had their address taken.  */
  struct alias_map_d **addressable_vars;
  size_t num_addressable_vars;

  /* POINTERS contains all the _DECL pointers with unique memory tags
     that have been referenced in the program.  */
  struct alias_map_d **pointers;
  size_t num_pointers;

  /* Number of function calls found in the program.  */
  size_t num_calls_found;

  /* Number of const/pure function calls found in the program.  */
  size_t num_pure_const_calls_found;

  /* Array of counters to keep track of how many times each pointer has
     been dereferenced in the program.  This is used by the alias grouping
     heuristic in compute_flow_insensitive_aliasing.  */
  varray_type num_references;

  /* Total number of virtual operands that will be needed to represent
     all the aliases of all the pointers found in the program.  */
  long total_alias_vops;

  /* Variables that have been written to.  */
  bitmap written_vars;

  /* Pointers that have been used in an indirect store operation.  */
  bitmap dereferenced_ptrs_store;

  /* Pointers that have been used in an indirect load operation.  */
  bitmap dereferenced_ptrs_load;
};


/* Counters used to display statistics on alias analysis.  */
struct alias_stats_d
{
  unsigned int alias_queries;
  unsigned int alias_mayalias;
  unsigned int alias_noalias;
  unsigned int simple_queries;
  unsigned int simple_resolved;
  unsigned int tbaa_queries;
  unsigned int tbaa_resolved;
};


/* Local variables.  */
static struct alias_stats_d alias_stats;

/* Local functions.  */
static void compute_flow_insensitive_aliasing (struct alias_info *);
static void dump_alias_stats (FILE *);
static bool may_alias_p (tree, HOST_WIDE_INT, tree, HOST_WIDE_INT);
static tree create_memory_tag (tree type, bool is_type_tag);
static tree get_tmt_for (tree, struct alias_info *);
static tree get_nmt_for (tree);
static void add_may_alias (tree, tree);
static void replace_may_alias (tree, size_t, tree);
static struct alias_info *init_alias_info (void);
static void delete_alias_info (struct alias_info *);
static void compute_points_to_and_addr_escape (struct alias_info *);
static void compute_flow_sensitive_aliasing (struct alias_info *);
static void setup_pointers_and_addressables (struct alias_info *);
static bool collect_points_to_info_r (tree, tree, void *);
static bool is_escape_site (tree, struct alias_info *);
static void add_pointed_to_var (struct alias_info *, tree, tree);
static void create_global_var (void);
static void collect_points_to_info_for (struct alias_info *, tree);
static void maybe_create_global_var (struct alias_info *ai);
static void group_aliases (struct alias_info *);
static void set_pt_anything (tree ptr);
static void set_pt_malloc (tree ptr);

/* Global declarations.  */

/* Call clobbered variables in the function.  If bit I is set, then
   REFERENCED_VARS (I) is call-clobbered.  */
bitmap call_clobbered_vars;

/* Addressable variables in the function.  If bit I is set, then
   REFERENCED_VARS (I) has had its address taken.  Note that
   CALL_CLOBBERED_VARS and ADDRESSABLE_VARS are not related.  An
   addressable variable is not necessarily call-clobbered (e.g., a
   local addressable whose address does not escape) and not all
   call-clobbered variables are addressable (e.g., a local static
   variable).  */
bitmap addressable_vars;

/* When the program has too many call-clobbered variables and call-sites,
   this variable is used to represent the clobbering effects of function
   calls.  In these cases, all the call clobbered variables in the program
   are forced to alias this variable.  This reduces compile times by not
   having to keep track of too many V_MAY_DEF expressions at call sites.  */
tree global_var;


/* Compute may-alias information for every variable referenced in function
   FNDECL.

   Alias analysis proceeds in 3 main phases:

   1- Points-to and escape analysis.

   This phase walks the use-def chains in the SSA web looking for three
   things:

	* Assignments of the form P_i = &VAR
	* Assignments of the form P_i = malloc()
	* Pointers and ADDR_EXPR that escape the current function.

   The concept of 'escaping' is the same one used in the Java world.  When
   a pointer or an ADDR_EXPR escapes, it means that it has been exposed
   outside of the current function.  So, assignment to global variables,
   function arguments and returning a pointer are all escape sites, as are
   conversions between pointers and integers.

   This is where we are currently limited.  Since not everything is renamed
   into SSA, we lose track of escape properties when a pointer is stashed
   inside a field in a structure, for instance.  In those cases, we are
   assuming that the pointer does escape.

   We use escape analysis to determine whether a variable is
   call-clobbered.  Simply put, if an ADDR_EXPR escapes, then the variable
   is call-clobbered.  If a pointer P_i escapes, then all the variables
   pointed-to by P_i (and its memory tag) also escape.

   2- Compute flow-sensitive aliases

   We have two classes of memory tags.  Memory tags associated with the
   pointed-to data type of the pointers in the program.  These tags are
   called "type memory tag" (TMT).  The other class are those associated
   with SSA_NAMEs, called "name memory tag" (NMT). The basic idea is that
   when adding operands for an INDIRECT_REF *P_i, we will first check
   whether P_i has a name tag, if it does we use it, because that will have
   more precise aliasing information.  Otherwise, we use the standard type
   tag.

   In this phase, we go through all the pointers we found in points-to
   analysis and create alias sets for the name memory tags associated with
   each pointer P_i.  If P_i escapes, we mark call-clobbered the variables
   it points to and its tag.


   3- Compute flow-insensitive aliases

   This pass will compare the alias set of every type memory tag and every
   addressable variable found in the program.  Given a type memory tag TMT
   and an addressable variable V.  If the alias sets of TMT and V conflict
   (as computed by may_alias_p), then V is marked as an alias tag and added
   to the alias set of TMT.

   For instance, consider the following function:

	    foo (int i)
	    {
	      int *p, a, b;
	    
	      if (i > 10)
	        p = &a;
	      else
	        p = &b;
	    
	      *p = 3;
	      a = b + 2;
	      return *p;
	    }

   After aliasing analysis has finished, the type memory tag for pointer
   'p' will have two aliases, namely variables 'a' and 'b'.  Every time
   pointer 'p' is dereferenced, we want to mark the operation as a
   potential reference to 'a' and 'b'.

	    foo (int i)
	    {
	      int *p, a, b;

	      if (i_2 > 10)
		p_4 = &a;
	      else
		p_6 = &b;
	      # p_1 = PHI <p_4(1), p_6(2)>;

	      # a_7 = V_MAY_DEF <a_3>;
	      # b_8 = V_MAY_DEF <b_5>;
	      *p_1 = 3;

	      # a_9 = V_MAY_DEF <a_7>
	      # VUSE <b_8>
	      a_9 = b_8 + 2;

	      # VUSE <a_9>;
	      # VUSE <b_8>;
	      return *p_1;
	    }

   In certain cases, the list of may aliases for a pointer may grow too
   large.  This may cause an explosion in the number of virtual operands
   inserted in the code.  Resulting in increased memory consumption and
   compilation time.

   When the number of virtual operands needed to represent aliased
   loads and stores grows too large (configurable with @option{--param
   max-aliased-vops}), alias sets are grouped to avoid severe
   compile-time slow downs and memory consumption.  See group_aliases.  */

static void
compute_may_aliases (void)
{
  struct alias_info *ai;
  
  memset (&alias_stats, 0, sizeof (alias_stats));

  /* Initialize aliasing information.  */
  ai = init_alias_info ();

  /* For each pointer P_i, determine the sets of variables that P_i may
     point-to.  For every addressable variable V, determine whether the
     address of V escapes the current function, making V call-clobbered
     (i.e., whether &V is stored in a global variable or if its passed as a
     function call argument).  */
  compute_points_to_and_addr_escape (ai);

  /* Collect all pointers and addressable variables, compute alias sets,
     create memory tags for pointers and promote variables whose address is
     not needed anymore.  */
  setup_pointers_and_addressables (ai);

  /* Compute flow-sensitive, points-to based aliasing for all the name
     memory tags.  Note that this pass needs to be done before flow
     insensitive analysis because it uses the points-to information
     gathered before to mark call-clobbered type tags.  */
  compute_flow_sensitive_aliasing (ai);

  /* Compute type-based flow-insensitive aliasing for all the type
     memory tags.  */
  compute_flow_insensitive_aliasing (ai);

  /* If the program has too many call-clobbered variables and/or function
     calls, create .GLOBAL_VAR and use it to model call-clobbering
     semantics at call sites.  This reduces the number of virtual operands
     considerably, improving compile times at the expense of lost
     aliasing precision.  */
  maybe_create_global_var (ai);

  /* Debugging dumps.  */
  if (dump_file)
    {
      dump_referenced_vars (dump_file);
      if (dump_flags & TDF_STATS)
	dump_alias_stats (dump_file);
      dump_points_to_info (dump_file);
      dump_alias_info (dump_file);
    }

  /* Deallocate memory used by aliasing data structures.  */
  delete_alias_info (ai);
}

struct tree_opt_pass pass_may_alias = 
{
  "alias",				/* name */
  NULL,					/* gate */
  compute_may_aliases,			/* execute */
  NULL,					/* sub */
  NULL,					/* next */
  0,					/* static_pass_number */
  TV_TREE_MAY_ALIAS,			/* tv_id */
  PROP_cfg | PROP_ssa,			/* properties_required */
  PROP_alias,				/* properties_provided */
  0,					/* properties_destroyed */
  0,					/* todo_flags_start */
  TODO_dump_func | TODO_rename_vars
    | TODO_ggc_collect | TODO_verify_ssa
    | TODO_verify_stmts, 		/* todo_flags_finish */
  0					/* letter */
};


/* Data structure used to count the number of dereferences to PTR
   inside an expression.  */
struct count_ptr_d
{
  tree ptr;
  unsigned count;
};


/* Helper for count_uses_and_derefs.  Called by walk_tree to look for
   (ALIGN/MISALIGNED_)INDIRECT_REF nodes for the pointer passed in DATA.  */

static tree
count_ptr_derefs (tree *tp, int *walk_subtrees ATTRIBUTE_UNUSED, void *data)
{
  struct count_ptr_d *count_p = (struct count_ptr_d *) data;

  if (INDIRECT_REF_P (*tp) && TREE_OPERAND (*tp, 0) == count_p->ptr)
    count_p->count++;

  return NULL_TREE;
}


/* Count the number of direct and indirect uses for pointer PTR in
   statement STMT.  The two counts are stored in *NUM_USES_P and
   *NUM_DEREFS_P respectively.  *IS_STORE_P is set to 'true' if at
   least one of those dereferences is a store operation.  */

static void
count_uses_and_derefs (tree ptr, tree stmt, unsigned *num_uses_p,
		       unsigned *num_derefs_p, bool *is_store)
{
  ssa_op_iter i;
  tree use;

  *num_uses_p = 0;
  *num_derefs_p = 0;
  *is_store = false;

  /* Find out the total number of uses of PTR in STMT.  */
  FOR_EACH_SSA_TREE_OPERAND (use, stmt, i, SSA_OP_USE)
    if (use == ptr)
      (*num_uses_p)++;

  /* Now count the number of indirect references to PTR.  This is
     truly awful, but we don't have much choice.  There are no parent
     pointers inside INDIRECT_REFs, so an expression like
     '*x_1 = foo (x_1, *x_1)' needs to be traversed piece by piece to
     find all the indirect and direct uses of x_1 inside.  The only
     shortcut we can take is the fact that GIMPLE only allows
     INDIRECT_REFs inside the expressions below.  */
  if (TREE_CODE (stmt) == MODIFY_EXPR
      || (TREE_CODE (stmt) == RETURN_EXPR
	  && TREE_CODE (TREE_OPERAND (stmt, 0)) == MODIFY_EXPR)
      || TREE_CODE (stmt) == ASM_EXPR
      || TREE_CODE (stmt) == CALL_EXPR)
    {
      tree lhs, rhs;

      if (TREE_CODE (stmt) == MODIFY_EXPR)
	{
	  lhs = TREE_OPERAND (stmt, 0);
	  rhs = TREE_OPERAND (stmt, 1);
	}
      else if (TREE_CODE (stmt) == RETURN_EXPR)
	{
	  tree e = TREE_OPERAND (stmt, 0);
	  lhs = TREE_OPERAND (e, 0);
	  rhs = TREE_OPERAND (e, 1);
	}
      else if (TREE_CODE (stmt) == ASM_EXPR)
	{
	  lhs = ASM_OUTPUTS (stmt);
	  rhs = ASM_INPUTS (stmt);
	}
      else
	{
	  lhs = NULL_TREE;
	  rhs = stmt;
	}

      if (lhs && (TREE_CODE (lhs) == TREE_LIST || EXPR_P (lhs)))
	{
	  struct count_ptr_d count;
	  count.ptr = ptr;
	  count.count = 0;
	  walk_tree (&lhs, count_ptr_derefs, &count, NULL);
	  *is_store = true;
	  *num_derefs_p = count.count;
	}

      if (rhs && (TREE_CODE (rhs) == TREE_LIST || EXPR_P (rhs)))
	{
	  struct count_ptr_d count;
	  count.ptr = ptr;
	  count.count = 0;
	  walk_tree (&rhs, count_ptr_derefs, &count, NULL);
	  *num_derefs_p += count.count;
	}
    }

  gcc_assert (*num_uses_p >= *num_derefs_p);
}


/* Initialize the data structures used for alias analysis.  */

static struct alias_info *
init_alias_info (void)
{
  struct alias_info *ai;

  ai = xcalloc (1, sizeof (struct alias_info));
  ai->ssa_names_visited = sbitmap_alloc (num_ssa_names);
  sbitmap_zero (ai->ssa_names_visited);
  VARRAY_TREE_INIT (ai->processed_ptrs, 50, "processed_ptrs");
  ai->addresses_needed = BITMAP_ALLOC (NULL);
  VARRAY_UINT_INIT (ai->num_references, num_referenced_vars, "num_references");
  ai->written_vars = BITMAP_ALLOC (NULL);
  ai->dereferenced_ptrs_store = BITMAP_ALLOC (NULL);
  ai->dereferenced_ptrs_load = BITMAP_ALLOC (NULL);

  /* If aliases have been computed before, clear existing information.  */
  if (aliases_computed_p)
    {
      unsigned i;
      basic_block bb;
  
     /* Make sure that every statement has a valid set of operands.
	If a statement needs to be scanned for operands while we
	compute aliases, it may get erroneous operands because all
	the alias relations are not built at that point.
	FIXME: This code will become obsolete when operands are not
	lazily updated.  */
      FOR_EACH_BB (bb)
	{
	  block_stmt_iterator si;
	  for (si = bsi_start (bb); !bsi_end_p (si); bsi_next (&si))
	    get_stmt_operands (bsi_stmt (si));
	}

      /* Similarly, clear the set of addressable variables.  In this
	 case, we can just clear the set because addressability is
	 only computed here.  */
      bitmap_clear (addressable_vars);

      /* Clear flow-insensitive alias information from each symbol.  */
      for (i = 0; i < num_referenced_vars; i++)
	{
	  tree var = referenced_var (i);
	  var_ann_t ann = var_ann (var);

	  ann->is_alias_tag = 0;
	  ann->may_aliases = NULL;

	  /* Since we are about to re-discover call-clobbered
	     variables, clear the call-clobbered flag.  Variables that
	     are intrinsically call-clobbered (globals, local statics,
	     etc) will not be marked by the aliasing code, so we can't
	     remove them from CALL_CLOBBERED_VARS.  */
	  if (ann->mem_tag_kind != NOT_A_TAG || !is_global_var (var))
	    clear_call_clobbered (var);
	}

      /* Clear flow-sensitive points-to information from each SSA name.  */
      for (i = 1; i < num_ssa_names; i++)
	{
	  tree name = ssa_name (i);

	  if (!name || !POINTER_TYPE_P (TREE_TYPE (name)))
	    continue;

	  if (SSA_NAME_PTR_INFO (name))
	    {
	      struct ptr_info_def *pi = SSA_NAME_PTR_INFO (name);

	      /* Clear all the flags but keep the name tag to
		 avoid creating new temporaries unnecessarily.  If
		 this pointer is found to point to a subset or
		 superset of its former points-to set, then a new
		 tag will need to be created in create_name_tags.  */
	      pi->pt_anything = 0;
	      pi->pt_malloc = 0;
	      pi->pt_null = 0;
	      pi->value_escapes_p = 0;
	      pi->is_dereferenced = 0;
	      if (pi->pt_vars)
		bitmap_clear (pi->pt_vars);
	    }
	}
    }

  /* Next time, we will need to reset alias information.  */
  aliases_computed_p = true;

  return ai;
}


/* Deallocate memory used by alias analysis.  */

static void
delete_alias_info (struct alias_info *ai)
{
  size_t i;

  sbitmap_free (ai->ssa_names_visited);
  ai->processed_ptrs = NULL;
  BITMAP_FREE (ai->addresses_needed);

  for (i = 0; i < ai->num_addressable_vars; i++)
    {
      sbitmap_free (ai->addressable_vars[i]->may_aliases);
      free (ai->addressable_vars[i]);
    }
  free (ai->addressable_vars);

  for (i = 0; i < ai->num_pointers; i++)
    {
      sbitmap_free (ai->pointers[i]->may_aliases);
      free (ai->pointers[i]);
    }
  free (ai->pointers);

  ai->num_references = NULL;
  BITMAP_FREE (ai->written_vars);
  BITMAP_FREE (ai->dereferenced_ptrs_store);
  BITMAP_FREE (ai->dereferenced_ptrs_load);

  free (ai);
}


/* Walk use-def chains for pointer PTR to determine what variables is PTR
   pointing to.  */

static void
collect_points_to_info_for (struct alias_info *ai, tree ptr)
{
  gcc_assert (POINTER_TYPE_P (TREE_TYPE (ptr)));

  if (!TEST_BIT (ai->ssa_names_visited, SSA_NAME_VERSION (ptr)))
    {
      SET_BIT (ai->ssa_names_visited, SSA_NAME_VERSION (ptr));
      walk_use_def_chains (ptr, collect_points_to_info_r, ai, true);
      VARRAY_PUSH_TREE (ai->processed_ptrs, ptr);
    }
}


/* Traverse use-def links for all the pointers in the program to collect
   address escape and points-to information.
   
   This is loosely based on the same idea described in R. Hasti and S.
   Horwitz, ``Using static single assignment form to improve
   flow-insensitive pointer analysis,'' in SIGPLAN Conference on
   Programming Language Design and Implementation, pp. 97-105, 1998.  */

static void
compute_points_to_and_addr_escape (struct alias_info *ai)
{
  basic_block bb;
  unsigned i;
  tree op;
  ssa_op_iter iter;

  timevar_push (TV_TREE_PTA);

  FOR_EACH_BB (bb)
    {
      bb_ann_t block_ann = bb_ann (bb);
      block_stmt_iterator si;

      for (si = bsi_start (bb); !bsi_end_p (si); bsi_next (&si))
	{
	  bitmap addr_taken;
	  tree stmt = bsi_stmt (si);
	  bool stmt_escapes_p = is_escape_site (stmt, ai);
	  bitmap_iterator bi;

	  /* Mark all the variables whose address are taken by the
	     statement.  Note that this will miss all the addresses taken
	     in PHI nodes (those are discovered while following the use-def
	     chains).  */
	  get_stmt_operands (stmt);
	  addr_taken = addresses_taken (stmt);
	  if (addr_taken)
	    EXECUTE_IF_SET_IN_BITMAP (addr_taken, 0, i, bi)
	      {
		tree var = referenced_var (i);
		bitmap_set_bit (ai->addresses_needed, var_ann (var)->uid);
		if (stmt_escapes_p)
		  mark_call_clobbered (var);
	      }

	  if (stmt_escapes_p)
	    block_ann->has_escape_site = 1;

	  FOR_EACH_SSA_TREE_OPERAND (op, stmt, iter, SSA_OP_USE)
	    {
	      var_ann_t v_ann = var_ann (SSA_NAME_VAR (op));
	      struct ptr_info_def *pi;
	      bool is_store;
	      unsigned num_uses, num_derefs;

	      /* If the operand's variable may be aliased, keep track
		 of how many times we've referenced it.  This is used
		 for alias grouping in compute_flow_sensitive_aliasing.
		 Note that we don't need to grow AI->NUM_REFERENCES
		 because we are processing regular variables, not
		 memory tags (the array's initial size is set to
		 NUM_REFERENCED_VARS).  */
	      if (may_be_aliased (SSA_NAME_VAR (op)))
		(VARRAY_UINT (ai->num_references, v_ann->uid))++;

	      if (!POINTER_TYPE_P (TREE_TYPE (op)))
		continue;

	      collect_points_to_info_for (ai, op);

	      pi = SSA_NAME_PTR_INFO (op);
	      count_uses_and_derefs (op, stmt, &num_uses, &num_derefs,
				     &is_store);

	      if (num_derefs > 0)
		{
		  /* Mark OP as dereferenced.  In a subsequent pass,
		     dereferenced pointers that point to a set of
		     variables will be assigned a name tag to alias
		     all the variables OP points to.  */
		  pi->is_dereferenced = 1;

		  /* Keep track of how many time we've dereferenced each
		     pointer.  Again, we don't need to grow
		     AI->NUM_REFERENCES because we're processing
		     existing program variables.  */
		  (VARRAY_UINT (ai->num_references, v_ann->uid))++;

		  /* If this is a store operation, mark OP as being
		     dereferenced to store, otherwise mark it as being
		     dereferenced to load.  */
		  if (is_store)
		    bitmap_set_bit (ai->dereferenced_ptrs_store, v_ann->uid);
		  else
		    bitmap_set_bit (ai->dereferenced_ptrs_load, v_ann->uid);
		}

	      if (stmt_escapes_p && num_derefs < num_uses)
		{
		  /* If STMT is an escape point and STMT contains at
		     least one direct use of OP, then the value of OP
		     escapes and so the pointed-to variables need to
		     be marked call-clobbered.  */
		  pi->value_escapes_p = 1;

		  /* If the statement makes a function call, assume
		     that pointer OP will be dereferenced in a store
		     operation inside the called function.  */
		  if (get_call_expr_in (stmt))
		    {
		      bitmap_set_bit (ai->dereferenced_ptrs_store, v_ann->uid);
		      pi->is_dereferenced = 1;
		    }
		}
	    }

	  /* Update reference counter for definitions to any
	     potentially aliased variable.  This is used in the alias
	     grouping heuristics.  */
	  FOR_EACH_SSA_TREE_OPERAND (op, stmt, iter, SSA_OP_DEF)
	    {
	      tree var = SSA_NAME_VAR (op);
	      var_ann_t ann = var_ann (var);
	      bitmap_set_bit (ai->written_vars, ann->uid);
	      if (may_be_aliased (var))
		(VARRAY_UINT (ai->num_references, ann->uid))++;

	      if (POINTER_TYPE_P (TREE_TYPE (op)))
		collect_points_to_info_for (ai, op);
	    }

	  /* Mark variables in V_MAY_DEF operands as being written to.  */
	  FOR_EACH_SSA_TREE_OPERAND (op, stmt, iter, SSA_OP_VIRTUAL_DEFS)
	    {
	      tree var = SSA_NAME_VAR (op);
	      var_ann_t ann = var_ann (var);
	      bitmap_set_bit (ai->written_vars, ann->uid);
	    }
	    
	  /* After promoting variables and computing aliasing we will
	     need to re-scan most statements.  FIXME: Try to minimize the
	     number of statements re-scanned.  It's not really necessary to
	     re-scan *all* statements.  */
	  modify_stmt (stmt);
	}
    }

  timevar_pop (TV_TREE_PTA);
}


/* Create name tags for all the pointers that have been dereferenced.
   We only create a name tag for a pointer P if P is found to point to
   a set of variables (so that we can alias them to *P) or if it is
   the result of a call to malloc (which means that P cannot point to
   anything else nor alias any other variable).

   If two pointers P and Q point to the same set of variables, they
   are assigned the same name tag.  */

static void
create_name_tags (struct alias_info *ai)
{
  size_t i;

  for (i = 0; i < VARRAY_ACTIVE_SIZE (ai->processed_ptrs); i++)
    {
      tree ptr = VARRAY_TREE (ai->processed_ptrs, i);
      struct ptr_info_def *pi = SSA_NAME_PTR_INFO (ptr);

      if (pi->pt_anything || !pi->is_dereferenced)
	{
	  /* No name tags for pointers that have not been
	     dereferenced or point to an arbitrary location.  */
	  pi->name_mem_tag = NULL_TREE;
	  continue;
	}

      if (pi->pt_vars && !bitmap_empty_p (pi->pt_vars))
	{
	  size_t j;
	  tree old_name_tag = pi->name_mem_tag;

	  /* If PTR points to a set of variables, check if we don't
	     have another pointer Q with the same points-to set before
	     creating a tag.  If so, use Q's tag instead of creating a
	     new one.

	     This is important for not creating unnecessary symbols
	     and also for copy propagation.  If we ever need to
	     propagate PTR into Q or vice-versa, we would run into
	     problems if they both had different name tags because
	     they would have different SSA version numbers (which
	     would force us to take the name tags in and out of SSA).  */
	  for (j = 0; j < i; j++)
	    {
	      tree q = VARRAY_TREE (ai->processed_ptrs, j);
	      struct ptr_info_def *qi = SSA_NAME_PTR_INFO (q);

	      if (qi
		  && qi->pt_vars
		  && qi->name_mem_tag
		  && bitmap_equal_p (pi->pt_vars, qi->pt_vars))
		{
		  pi->name_mem_tag = qi->name_mem_tag;
		  break;
		}
	    }

	  /* If we didn't find a pointer with the same points-to set
	     as PTR, create a new name tag if needed.  */
	  if (pi->name_mem_tag == NULL_TREE)
	    pi->name_mem_tag = get_nmt_for (ptr);

	  /* If the new name tag computed for PTR is different than
	     the old name tag that it used to have, then the old tag
	     needs to be removed from the IL, so we mark it for
	     renaming.  */
	  if (old_name_tag && old_name_tag != pi->name_mem_tag)
	    bitmap_set_bit (vars_to_rename, var_ann (old_name_tag)->uid);
	}
      else if (pi->pt_malloc)
	{
	  /* Otherwise, create a unique name tag for this pointer.  */
	  pi->name_mem_tag = get_nmt_for (ptr);
	}
      else
	{
	  /* Only pointers that may point to malloc or other variables
	     may receive a name tag.  If the pointer does not point to
	     a known spot, we should use type tags.  */
	  set_pt_anything (ptr);
	  continue;
	}

      TREE_THIS_VOLATILE (pi->name_mem_tag)
	  |= TREE_THIS_VOLATILE (TREE_TYPE (TREE_TYPE (ptr)));

      /* Mark the new name tag for renaming.  */
      bitmap_set_bit (vars_to_rename, var_ann (pi->name_mem_tag)->uid);
    }
}



/* For every pointer P_i in AI->PROCESSED_PTRS, create may-alias sets for
   the name memory tag (NMT) associated with P_i.  If P_i escapes, then its
   name tag and the variables it points-to are call-clobbered.  Finally, if
   P_i escapes and we could not determine where it points to, then all the
   variables in the same alias set as *P_i are marked call-clobbered.  This
   is necessary because we must assume that P_i may take the address of any
   variable in the same alias set.  */

static void
compute_flow_sensitive_aliasing (struct alias_info *ai)
{
  size_t i;

  create_name_tags (ai);

  for (i = 0; i < VARRAY_ACTIVE_SIZE (ai->processed_ptrs); i++)
    {
      unsigned j;
      tree ptr = VARRAY_TREE (ai->processed_ptrs, i);
      struct ptr_info_def *pi = SSA_NAME_PTR_INFO (ptr);
      var_ann_t v_ann = var_ann (SSA_NAME_VAR (ptr));
      bitmap_iterator bi;

      if (pi->value_escapes_p || pi->pt_anything)
	{
	  /* If PTR escapes or may point to anything, then its associated
	     memory tags and pointed-to variables are call-clobbered.  */
	  if (pi->name_mem_tag)
	    mark_call_clobbered (pi->name_mem_tag);

	  if (v_ann->type_mem_tag)
	    mark_call_clobbered (v_ann->type_mem_tag);

	  if (pi->pt_vars)
	    EXECUTE_IF_SET_IN_BITMAP (pi->pt_vars, 0, j, bi)
	      {
		mark_call_clobbered (referenced_var (j));
	      }
	}

      /* Set up aliasing information for PTR's name memory tag (if it has
	 one).  Note that only pointers that have been dereferenced will
	 have a name memory tag.  */
      if (pi->name_mem_tag && pi->pt_vars)
	EXECUTE_IF_SET_IN_BITMAP (pi->pt_vars, 0, j, bi)
	  {
	    add_may_alias (pi->name_mem_tag, referenced_var (j));
	    add_may_alias (v_ann->type_mem_tag, referenced_var (j));
	  }

      /* If the name tag is call clobbered, so is the type tag
	 associated with the base VAR_DECL.  */
      if (pi->name_mem_tag
	  && v_ann->type_mem_tag
	  && is_call_clobbered (pi->name_mem_tag))
	mark_call_clobbered (v_ann->type_mem_tag);
    }
}


/* Compute type-based alias sets.  Traverse all the pointers and
   addressable variables found in setup_pointers_and_addressables.
   
   For every pointer P in AI->POINTERS and addressable variable V in
   AI->ADDRESSABLE_VARS, add V to the may-alias sets of P's type
   memory tag (TMT) if their alias sets conflict.  V is then marked as
   an alias tag so that the operand scanner knows that statements
   containing V have aliased operands.  */

static void
compute_flow_insensitive_aliasing (struct alias_info *ai)
{
  size_t i;

  /* Initialize counter for the total number of virtual operands that
     aliasing will introduce.  When AI->TOTAL_ALIAS_VOPS goes beyond the
     threshold set by --params max-alias-vops, we enable alias
     grouping.  */
  ai->total_alias_vops = 0;

  /* For every pointer P, determine which addressable variables may alias
     with P's type memory tag.  */
  for (i = 0; i < ai->num_pointers; i++)
    {
      size_t j;
      struct alias_map_d *p_map = ai->pointers[i];
      tree tag = var_ann (p_map->var)->type_mem_tag;
      var_ann_t tag_ann = var_ann (tag);

      p_map->total_alias_vops = 0;
      p_map->may_aliases = sbitmap_alloc (num_referenced_vars);
      sbitmap_zero (p_map->may_aliases);

      for (j = 0; j < ai->num_addressable_vars; j++)
	{
	  struct alias_map_d *v_map;
	  var_ann_t v_ann;
	  tree var;
	  bool tag_stored_p, var_stored_p;
	  
	  v_map = ai->addressable_vars[j];
	  var = v_map->var;
	  v_ann = var_ann (var);

	  /* Skip memory tags and variables that have never been
	     written to.  We also need to check if the variables are
	     call-clobbered because they may be overwritten by
	     function calls.

	     Note this is effectively random accessing elements in
	     the sparse bitset, which can be highly inefficient.
	     So we first check the call_clobbered status of the
	     tag and variable before querying the bitmap.  */
	  tag_stored_p = is_call_clobbered (tag)
			 || bitmap_bit_p (ai->written_vars, tag_ann->uid);
	  var_stored_p = is_call_clobbered (var)
			 || bitmap_bit_p (ai->written_vars, v_ann->uid);
	  if (!tag_stored_p && !var_stored_p)
	    continue;
	     
	  if (may_alias_p (p_map->var, p_map->set, var, v_map->set))
	    {
	      size_t num_tag_refs, num_var_refs;

	      num_tag_refs = VARRAY_UINT (ai->num_references, tag_ann->uid);
	      num_var_refs = VARRAY_UINT (ai->num_references, v_ann->uid);

	      /* Add VAR to TAG's may-aliases set.  */
	      add_may_alias (tag, var);

	      /* Update the total number of virtual operands due to
		 aliasing.  Since we are adding one more alias to TAG's
		 may-aliases set, the total number of virtual operands due
		 to aliasing will be increased by the number of references
		 made to VAR and TAG (every reference to TAG will also
		 count as a reference to VAR).  */
	      ai->total_alias_vops += (num_var_refs + num_tag_refs);
	      p_map->total_alias_vops += (num_var_refs + num_tag_refs);

	      /* Update the bitmap used to represent TAG's alias set
		 in case we need to group aliases.  */
	      SET_BIT (p_map->may_aliases, var_ann (var)->uid);
	    }
	}
    }

  /* Since this analysis is based exclusively on symbols, it fails to
     handle cases where two pointers P and Q have different memory
     tags with conflicting alias set numbers but no aliased symbols in
     common.

     For example, suppose that we have two memory tags TMT.1 and TMT.2
     such that
     
     		may-aliases (TMT.1) = { a }
		may-aliases (TMT.2) = { b }

     and the alias set number of TMT.1 conflicts with that of TMT.2.
     Since they don't have symbols in common, loads and stores from
     TMT.1 and TMT.2 will seem independent of each other, which will
     lead to the optimizers making invalid transformations (see
     testsuite/gcc.c-torture/execute/pr15262-[12].c).

     To avoid this problem, we do a final traversal of AI->POINTERS
     looking for pairs of pointers that have no aliased symbols in
     common and yet have conflicting alias set numbers.  */
  for (i = 0; i < ai->num_pointers; i++)
    {
      size_t j;
      struct alias_map_d *p_map1 = ai->pointers[i];
      tree tag1 = var_ann (p_map1->var)->type_mem_tag;
      sbitmap may_aliases1 = p_map1->may_aliases;

      for (j = i + 1; j < ai->num_pointers; j++)
	{
	  struct alias_map_d *p_map2 = ai->pointers[j];
	  tree tag2 = var_ann (p_map2->var)->type_mem_tag;
	  sbitmap may_aliases2 = p_map2->may_aliases;

	  /* If the pointers may not point to each other, do nothing.  */
	  if (!may_alias_p (p_map1->var, p_map1->set, p_map2->var, p_map2->set))
	    continue;

	  /* The two pointers may alias each other.  If they already have
	     symbols in common, do nothing.  */
	  if (sbitmap_any_common_bits (may_aliases1, may_aliases2))
	    continue;

	  if (sbitmap_first_set_bit (may_aliases2) >= 0)
	    {
	      size_t k;

	      /* Add all the aliases for TAG2 into TAG1's alias set.
		 FIXME, update grouping heuristic counters.  */
	      EXECUTE_IF_SET_IN_SBITMAP (may_aliases2, 0, k,
		  add_may_alias (tag1, referenced_var (k)));
	      sbitmap_a_or_b (may_aliases1, may_aliases1, may_aliases2);
	    }
	  else
	    {
	      /* Since TAG2 does not have any aliases of its own, add
		 TAG2 itself to the alias set of TAG1.  */
	      add_may_alias (tag1, tag2);
	      SET_BIT (may_aliases1, var_ann (tag2)->uid);
	    }
	}
    }

  if (dump_file)
    fprintf (dump_file, "%s: Total number of aliased vops: %ld\n",
	     get_name (current_function_decl),
	     ai->total_alias_vops);

  /* Determine if we need to enable alias grouping.  */
  if (ai->total_alias_vops >= MAX_ALIASED_VOPS)
    group_aliases (ai);
}


/* Comparison function for qsort used in group_aliases.  */

static int
total_alias_vops_cmp (const void *p, const void *q)
{
  const struct alias_map_d **p1 = (const struct alias_map_d **)p;
  const struct alias_map_d **p2 = (const struct alias_map_d **)q;
  long n1 = (*p1)->total_alias_vops;
  long n2 = (*p2)->total_alias_vops;

  /* We want to sort in descending order.  */
  return (n1 > n2 ? -1 : (n1 == n2) ? 0 : 1);
}

/* Group all the aliases for TAG to make TAG represent all the
   variables in its alias set.  Update the total number
   of virtual operands due to aliasing (AI->TOTAL_ALIAS_VOPS).  This
   function will make TAG be the unique alias tag for all the
   variables in its may-aliases.  So, given:

   	may-aliases(TAG) = { V1, V2, V3 }

   This function will group the variables into:

   	may-aliases(V1) = { TAG }
	may-aliases(V2) = { TAG }
	may-aliases(V2) = { TAG }  */

static void
group_aliases_into (tree tag, sbitmap tag_aliases, struct alias_info *ai)
{
  size_t i;
  var_ann_t tag_ann = var_ann (tag);
  size_t num_tag_refs = VARRAY_UINT (ai->num_references, tag_ann->uid);

  EXECUTE_IF_SET_IN_SBITMAP (tag_aliases, 0, i,
    {
      tree var = referenced_var (i);
      var_ann_t ann = var_ann (var);

      /* Make TAG the unique alias of VAR.  */
      ann->is_alias_tag = 0;
      ann->may_aliases = NULL;

      /* Note that VAR and TAG may be the same if the function has no
	 addressable variables (see the discussion at the end of
	 setup_pointers_and_addressables).  */
      if (var != tag)
	add_may_alias (var, tag);

      /* Reduce total number of virtual operands contributed
	 by TAG on behalf of VAR.  Notice that the references to VAR
	 itself won't be removed.  We will merely replace them with
	 references to TAG.  */
      ai->total_alias_vops -= num_tag_refs;
    });

  /* We have reduced the number of virtual operands that TAG makes on
     behalf of all the variables formerly aliased with it.  However,
     we have also "removed" all the virtual operands for TAG itself,
     so we add them back.  */
  ai->total_alias_vops += num_tag_refs;

  /* TAG no longer has any aliases.  */
  tag_ann->may_aliases = NULL;
}


/* Group may-aliases sets to reduce the number of virtual operands due
   to aliasing.

     1- Sort the list of pointers in decreasing number of contributed
	virtual operands.

     2- Take the first entry in AI->POINTERS and revert the role of
	the memory tag and its aliases.  Usually, whenever an aliased
	variable Vi is found to alias with a memory tag T, we add Vi
	to the may-aliases set for T.  Meaning that after alias
	analysis, we will have:

		may-aliases(T) = { V1, V2, V3, ..., Vn }

	This means that every statement that references T, will get 'n'
	virtual operands for each of the Vi tags.  But, when alias
	grouping is enabled, we make T an alias tag and add it to the
	alias set of all the Vi variables:

		may-aliases(V1) = { T }
		may-aliases(V2) = { T }
		...
		may-aliases(Vn) = { T }

	This has two effects: (a) statements referencing T will only get
	a single virtual operand, and, (b) all the variables Vi will now
	appear to alias each other.  So, we lose alias precision to
	improve compile time.  But, in theory, a program with such a high
	level of aliasing should not be very optimizable in the first
	place.

     3- Since variables may be in the alias set of more than one
	memory tag, the grouping done in step (2) needs to be extended
	to all the memory tags that have a non-empty intersection with
	the may-aliases set of tag T.  For instance, if we originally
	had these may-aliases sets:

		may-aliases(T) = { V1, V2, V3 }
		may-aliases(R) = { V2, V4 }

	In step (2) we would have reverted the aliases for T as:

		may-aliases(V1) = { T }
		may-aliases(V2) = { T }
		may-aliases(V3) = { T }

	But note that now V2 is no longer aliased with R.  We could
	add R to may-aliases(V2), but we are in the process of
	grouping aliases to reduce virtual operands so what we do is
	add V4 to the grouping to obtain:

		may-aliases(V1) = { T }
		may-aliases(V2) = { T }
		may-aliases(V3) = { T }
		may-aliases(V4) = { T }

     4- If the total number of virtual operands due to aliasing is
	still above the threshold set by max-alias-vops, go back to (2).  */

static void
group_aliases (struct alias_info *ai)
{
  size_t i;

  /* Sort the POINTERS array in descending order of contributed
     virtual operands.  */
  qsort (ai->pointers, ai->num_pointers, sizeof (struct alias_map_d *),
         total_alias_vops_cmp);

  /* For every pointer in AI->POINTERS, reverse the roles of its tag
     and the tag's may-aliases set.  */
  for (i = 0; i < ai->num_pointers; i++)
    {
      size_t j;
      tree tag1 = var_ann (ai->pointers[i]->var)->type_mem_tag;
      sbitmap tag1_aliases = ai->pointers[i]->may_aliases;

      /* Skip tags that have been grouped already.  */
      if (ai->pointers[i]->grouped_p)
	continue;

      /* See if TAG1 had any aliases in common with other type tags.
	 If we find a TAG2 with common aliases with TAG1, add TAG2's
	 aliases into TAG1.  */
      for (j = i + 1; j < ai->num_pointers; j++)
	{
	  sbitmap tag2_aliases = ai->pointers[j]->may_aliases;

          if (sbitmap_any_common_bits (tag1_aliases, tag2_aliases))
	    {
	      tree tag2 = var_ann (ai->pointers[j]->var)->type_mem_tag;

	      sbitmap_a_or_b (tag1_aliases, tag1_aliases, tag2_aliases);

	      /* TAG2 does not need its aliases anymore.  */
	      sbitmap_zero (tag2_aliases);
	      var_ann (tag2)->may_aliases = NULL;

	      /* TAG1 is the unique alias of TAG2.  */
	      add_may_alias (tag2, tag1);

	      ai->pointers[j]->grouped_p = true;
	    }
	}

      /* Now group all the aliases we collected into TAG1.  */
      group_aliases_into (tag1, tag1_aliases, ai);

      /* If we've reduced total number of virtual operands below the
	 threshold, stop.  */
      if (ai->total_alias_vops < MAX_ALIASED_VOPS)
	break;
    }

  /* Finally, all the variables that have been grouped cannot be in
     the may-alias set of name memory tags.  Suppose that we have
     grouped the aliases in this code so that may-aliases(a) = TMT.20

     	p_5 = &a;
	...
	# a_9 = V_MAY_DEF <a_8>
	p_5->field = 0
	... Several modifications to TMT.20 ... 
	# VUSE <a_9>
	x_30 = p_5->field

     Since p_5 points to 'a', the optimizers will try to propagate 0
     into p_5->field, but that is wrong because there have been
     modifications to 'TMT.20' in between.  To prevent this we have to
     replace 'a' with 'TMT.20' in the name tag of p_5.  */
  for (i = 0; i < VARRAY_ACTIVE_SIZE (ai->processed_ptrs); i++)
    {
      size_t j;
      tree ptr = VARRAY_TREE (ai->processed_ptrs, i);
      tree name_tag = SSA_NAME_PTR_INFO (ptr)->name_mem_tag;
      varray_type aliases;
      
      if (name_tag == NULL_TREE)
	continue;

      aliases = var_ann (name_tag)->may_aliases;
      for (j = 0; aliases && j < VARRAY_ACTIVE_SIZE (aliases); j++)
	{
	  tree alias = VARRAY_TREE (aliases, j);
	  var_ann_t ann = var_ann (alias);

	  if (ann->mem_tag_kind == NOT_A_TAG && ann->may_aliases)
	    {
	      tree new_alias;

	      gcc_assert (VARRAY_ACTIVE_SIZE (ann->may_aliases) == 1);

	      new_alias = VARRAY_TREE (ann->may_aliases, 0);
	      replace_may_alias (name_tag, j, new_alias);
	    }
	}
    }

  if (dump_file)
    fprintf (dump_file,
	     "%s: Total number of aliased vops after grouping: %ld%s\n",
	     get_name (current_function_decl),
	     ai->total_alias_vops,
	     (ai->total_alias_vops < 0) ? " (negative values are OK)" : "");
}


/* Create a new alias set entry for VAR in AI->ADDRESSABLE_VARS.  */

static void
create_alias_map_for (tree var, struct alias_info *ai)
{
  struct alias_map_d *alias_map;
  alias_map = xcalloc (1, sizeof (*alias_map));
  alias_map->var = var;
  alias_map->set = get_alias_set (var);
  ai->addressable_vars[ai->num_addressable_vars++] = alias_map;
}


/* Create memory tags for all the dereferenced pointers and build the
   ADDRESSABLE_VARS and POINTERS arrays used for building the may-alias
   sets.  Based on the address escape and points-to information collected
   earlier, this pass will also clear the TREE_ADDRESSABLE flag from those
   variables whose address is not needed anymore.  */

static void
setup_pointers_and_addressables (struct alias_info *ai)
{
  size_t i, n_vars, num_addressable_vars, num_pointers;

  /* Size up the arrays ADDRESSABLE_VARS and POINTERS.  */
  num_addressable_vars = num_pointers = 0;
  for (i = 0; i < num_referenced_vars; i++)
    {
      tree var = referenced_var (i);

      if (may_be_aliased (var))
	num_addressable_vars++;

      if (POINTER_TYPE_P (TREE_TYPE (var)))
	{
	  /* Since we don't keep track of volatile variables, assume that
	     these pointers are used in indirect store operations.  */
	  if (TREE_THIS_VOLATILE (var))
	    bitmap_set_bit (ai->dereferenced_ptrs_store, var_ann (var)->uid);

	  num_pointers++;
	}
    }

  /* Create ADDRESSABLE_VARS and POINTERS.  Note that these arrays are
     always going to be slightly bigger than we actually need them
     because some TREE_ADDRESSABLE variables will be marked
     non-addressable below and only pointers with unique type tags are
     going to be added to POINTERS.  */
  ai->addressable_vars = xcalloc (num_addressable_vars,
				  sizeof (struct alias_map_d *));
  ai->pointers = xcalloc (num_pointers, sizeof (struct alias_map_d *));
  ai->num_addressable_vars = 0;
  ai->num_pointers = 0;

  /* Since we will be creating type memory tags within this loop, cache the
     value of NUM_REFERENCED_VARS to avoid processing the additional tags
     unnecessarily.  */
  n_vars = num_referenced_vars;

  for (i = 0; i < n_vars; i++)
    {
      tree var = referenced_var (i);
      var_ann_t v_ann = var_ann (var);

      /* Name memory tags already have flow-sensitive aliasing
	 information, so they need not be processed by
	 compute_flow_insensitive_aliasing.  Similarly, type memory
	 tags are already accounted for when we process their
	 associated pointer.  */
      if (v_ann->mem_tag_kind != NOT_A_TAG)
	continue;

      /* Remove the ADDRESSABLE flag from every addressable variable whose
         address is not needed anymore.  This is caused by the propagation
         of ADDR_EXPR constants into INDIRECT_REF expressions and the
         removal of dead pointer assignments done by the early scalar
         cleanup passes.  */
      if (TREE_ADDRESSABLE (var))
	{
	  if (!bitmap_bit_p (ai->addresses_needed, v_ann->uid)
	      && TREE_CODE (var) != RESULT_DECL
	      && !is_global_var (var))
	    {
	      /* The address of VAR is not needed, remove the
		 addressable bit, so that it can be optimized as a
		 regular variable.  */
	      mark_non_addressable (var);

	      /* Since VAR is now a regular GIMPLE register, we will need
		 to rename VAR into SSA afterwards.  */
	      bitmap_set_bit (vars_to_rename, v_ann->uid);
	    }
	  else
	    {
	      /* Add the variable to the set of addressables.  Mostly
		 used when scanning operands for ASM_EXPRs that
		 clobber memory.  In those cases, we need to clobber
		 all call-clobbered variables and all addressables.  */
	      bitmap_set_bit (addressable_vars, v_ann->uid);
	    }
	}

      /* Global variables and addressable locals may be aliased.  Create an
         entry in ADDRESSABLE_VARS for VAR.  */
      if (may_be_aliased (var))
	{
	  create_alias_map_for (var, ai);
	  bitmap_set_bit (vars_to_rename, var_ann (var)->uid);
	}

      /* Add pointer variables that have been dereferenced to the POINTERS
         array and create a type memory tag for them.  */
      if (POINTER_TYPE_P (TREE_TYPE (var)))
	{
	  if ((bitmap_bit_p (ai->dereferenced_ptrs_store, v_ann->uid)
		|| bitmap_bit_p (ai->dereferenced_ptrs_load, v_ann->uid)))
	    {
	      tree tag;
	      var_ann_t t_ann;

	      /* If pointer VAR still doesn't have a memory tag
		 associated with it, create it now or re-use an
		 existing one.  */
	      tag = get_tmt_for (var, ai);
	      t_ann = var_ann (tag);

	      /* The type tag will need to be renamed into SSA
		 afterwards. Note that we cannot do this inside
		 get_tmt_for because aliasing may run multiple times
		 and we only create type tags the first time.  */
	      bitmap_set_bit (vars_to_rename, t_ann->uid);

	      /* Associate the tag with pointer VAR.  */
	      v_ann->type_mem_tag = tag;

	      /* If pointer VAR has been used in a store operation,
		 then its memory tag must be marked as written-to.  */
	      if (bitmap_bit_p (ai->dereferenced_ptrs_store, v_ann->uid))
		bitmap_set_bit (ai->written_vars, t_ann->uid);

	      /* If pointer VAR is a global variable or a PARM_DECL,
		 then its memory tag should be considered a global
		 variable.  */
	      if (TREE_CODE (var) == PARM_DECL || is_global_var (var))
		mark_call_clobbered (tag);

	      /* All the dereferences of pointer VAR count as
		 references of TAG.  Since TAG can be associated with
		 several pointers, add the dereferences of VAR to the
		 TAG.  We may need to grow AI->NUM_REFERENCES because
		 we have been adding name and type tags.  */
	      if (t_ann->uid >= VARRAY_SIZE (ai->num_references))
		VARRAY_GROW (ai->num_references, t_ann->uid + 10);

	      VARRAY_UINT (ai->num_references, t_ann->uid)
		+= VARRAY_UINT (ai->num_references, v_ann->uid);
	    }
	  else
	    {
	      /* The pointer has not been dereferenced.  If it had a
		 type memory tag, remove it and mark the old tag for
		 renaming to remove it out of the IL.  */
	      var_ann_t ann = var_ann (var);
	      tree tag = ann->type_mem_tag;
	      if (tag)
		{
		  bitmap_set_bit (vars_to_rename, var_ann (tag)->uid);
		  ann->type_mem_tag = NULL_TREE;
		}
	    }
	}
    }
}


/* Determine whether to use .GLOBAL_VAR to model call clobbering semantics. At
   every call site, we need to emit V_MAY_DEF expressions to represent the
   clobbering effects of the call for variables whose address escapes the
   current function.

   One approach is to group all call-clobbered variables into a single
   representative that is used as an alias of every call-clobbered variable
   (.GLOBAL_VAR).  This works well, but it ties the optimizer hands because
   references to any call clobbered variable is a reference to .GLOBAL_VAR.

   The second approach is to emit a clobbering V_MAY_DEF for every 
   call-clobbered variable at call sites.  This is the preferred way in terms 
   of optimization opportunities but it may create too many V_MAY_DEF operands
   if there are many call clobbered variables and function calls in the 
   function.

   To decide whether or not to use .GLOBAL_VAR we multiply the number of
   function calls found by the number of call-clobbered variables.  If that
   product is beyond a certain threshold, as determined by the parameterized
   values shown below, we use .GLOBAL_VAR.

   FIXME.  This heuristic should be improved.  One idea is to use several
   .GLOBAL_VARs of different types instead of a single one.  The thresholds
   have been derived from a typical bootstrap cycle, including all target
   libraries. Compile times were found increase by ~1% compared to using
   .GLOBAL_VAR.  */

static void
maybe_create_global_var (struct alias_info *ai)
{
  unsigned i, n_clobbered;
  bitmap_iterator bi;
  
  /* No need to create it, if we have one already.  */
  if (global_var == NULL_TREE)
    {
      /* Count all the call-clobbered variables.  */
      n_clobbered = 0;
      EXECUTE_IF_SET_IN_BITMAP (call_clobbered_vars, 0, i, bi)
	{
	  n_clobbered++;
	}

      /* If the number of virtual operands that would be needed to
	 model all the call-clobbered variables is larger than
	 GLOBAL_VAR_THRESHOLD, create .GLOBAL_VAR.

	 Also create .GLOBAL_VAR if there are no call-clobbered
	 variables and the program contains a mixture of pure/const
	 and regular function calls.  This is to avoid the problem
	 described in PR 20115:

	      int X;
	      int func_pure (void) { return X; }
	      int func_non_pure (int a) { X += a; }
	      int foo ()
	      {
	 	int a = func_pure ();
		func_non_pure (a);
		a = func_pure ();
		return a;
	      }

	 Since foo() has no call-clobbered variables, there is
	 no relationship between the calls to func_pure and
	 func_non_pure.  Since func_pure has no side-effects, value
	 numbering optimizations elide the second call to func_pure.
	 So, if we have some pure/const and some regular calls in the
	 program we create .GLOBAL_VAR to avoid missing these
	 relations.  */
      if (ai->num_calls_found * n_clobbered >= (size_t) GLOBAL_VAR_THRESHOLD
	  || (n_clobbered == 0
	      && ai->num_calls_found > 0
	      && ai->num_pure_const_calls_found > 0
	      && ai->num_calls_found > ai->num_pure_const_calls_found))
	create_global_var ();
    }

  /* Mark all call-clobbered symbols for renaming.  Since the initial
     rewrite into SSA ignored all call sites, we may need to rename
     .GLOBAL_VAR and the call-clobbered variables.  */
  EXECUTE_IF_SET_IN_BITMAP (call_clobbered_vars, 0, i, bi)
    {
      tree var = referenced_var (i);

      /* If the function has calls to clobbering functions and
	 .GLOBAL_VAR has been created, make it an alias for all
	 call-clobbered variables.  */
      if (global_var && var != global_var)
	add_may_alias (var, global_var);
      
      bitmap_set_bit (vars_to_rename, var_ann (var)->uid);
    }
}


/* Return TRUE if pointer PTR may point to variable VAR.
   
   MEM_ALIAS_SET is the alias set for the memory location pointed-to by PTR
	This is needed because when checking for type conflicts we are
	interested in the alias set of the memory location pointed-to by
	PTR.  The alias set of PTR itself is irrelevant.
   
   VAR_ALIAS_SET is the alias set for VAR.  */

static bool
may_alias_p (tree ptr, HOST_WIDE_INT mem_alias_set,
	     tree var, HOST_WIDE_INT var_alias_set)
{
  tree mem;
  var_ann_t v_ann, m_ann;

  alias_stats.alias_queries++;
  alias_stats.simple_queries++;

  /* By convention, a variable cannot alias itself.  */
  mem = var_ann (ptr)->type_mem_tag;
  if (mem == var)
    {
      alias_stats.alias_noalias++;
      alias_stats.simple_resolved++;
      return false;
    }

  v_ann = var_ann (var);
  m_ann = var_ann (mem);

  gcc_assert (m_ann->mem_tag_kind == TYPE_TAG);

  alias_stats.tbaa_queries++;

  /* If the alias sets don't conflict then MEM cannot alias VAR.  */
  if (!alias_sets_conflict_p (mem_alias_set, var_alias_set))
    {
      alias_stats.alias_noalias++;
      alias_stats.tbaa_resolved++;
      return false;
    }

  alias_stats.alias_mayalias++;
  return true;
}


/* Add ALIAS to the set of variables that may alias VAR.  */

static void
add_may_alias (tree var, tree alias)
{
  size_t i;
  var_ann_t v_ann = get_var_ann (var);
  var_ann_t a_ann = get_var_ann (alias);

  gcc_assert (var != alias);

  if (v_ann->may_aliases == NULL)
    VARRAY_TREE_INIT (v_ann->may_aliases, 2, "aliases");

  /* Avoid adding duplicates.  */
  for (i = 0; i < VARRAY_ACTIVE_SIZE (v_ann->may_aliases); i++)
    if (alias == VARRAY_TREE (v_ann->may_aliases, i))
      return;

  /* If VAR is a call-clobbered variable, so is its new ALIAS.
     FIXME, call-clobbering should only depend on whether an address
     escapes.  It should be independent of aliasing.  */
  if (is_call_clobbered (var))
    mark_call_clobbered (alias);

  /* Likewise.  If ALIAS is call-clobbered, so is VAR.  */
  else if (is_call_clobbered (alias))
    mark_call_clobbered (var);

  VARRAY_PUSH_TREE (v_ann->may_aliases, alias);
  a_ann->is_alias_tag = 1;
}


/* Replace alias I in the alias sets of VAR with NEW_ALIAS.  */

static void
replace_may_alias (tree var, size_t i, tree new_alias)
{
  var_ann_t v_ann = var_ann (var);
  VARRAY_TREE (v_ann->may_aliases, i) = new_alias;

  /* If VAR is a call-clobbered variable, so is NEW_ALIAS.
     FIXME, call-clobbering should only depend on whether an address
     escapes.  It should be independent of aliasing.  */
  if (is_call_clobbered (var))
    mark_call_clobbered (new_alias);

  /* Likewise.  If NEW_ALIAS is call-clobbered, so is VAR.  */
  else if (is_call_clobbered (new_alias))
    mark_call_clobbered (var);
}


/* Mark pointer PTR as pointing to an arbitrary memory location.  */

static void
set_pt_anything (tree ptr)
{
  struct ptr_info_def *pi = get_ptr_info (ptr);

  pi->pt_anything = 1;
  pi->pt_malloc = 0;

  /* The pointer used to have a name tag, but we now found it pointing
     to an arbitrary location.  The name tag needs to be renamed and
     disassociated from PTR.  */
  if (pi->name_mem_tag)
    {
      bitmap_set_bit (vars_to_rename, var_ann (pi->name_mem_tag)->uid);
      pi->name_mem_tag = NULL_TREE;
    }
}


/* Mark pointer PTR as pointing to a malloc'd memory area.  */

static void
set_pt_malloc (tree ptr)
{
  struct ptr_info_def *pi = SSA_NAME_PTR_INFO (ptr);

  /* If the pointer has already been found to point to arbitrary
     memory locations, it is unsafe to mark it as pointing to malloc.  */
  if (pi->pt_anything)
    return;

  pi->pt_malloc = 1;
}


/* Given two different pointers DEST and ORIG.  Merge the points-to
   information in ORIG into DEST.  AI contains all the alias
   information collected up to this point.  */

static void
merge_pointed_to_info (struct alias_info *ai, tree dest, tree orig)
{
  struct ptr_info_def *dest_pi, *orig_pi;

  gcc_assert (dest != orig);

  /* Make sure we have points-to information for ORIG.  */
  collect_points_to_info_for (ai, orig);

  dest_pi = get_ptr_info (dest);
  orig_pi = SSA_NAME_PTR_INFO (orig);

  if (orig_pi)
    {
      gcc_assert (orig_pi != dest_pi);

      /* Notice that we never merge PT_MALLOC.  This attribute is only
	 true if the pointer is the result of a malloc() call.
	 Otherwise, we can end up in this situation:

	 P_i = malloc ();
	 ...
	 P_j = P_i + X;

	 P_j would be marked as PT_MALLOC, however we currently do not
	 handle cases of more than one pointer pointing to the same
	 malloc'd area.

	 FIXME: If the merging comes from an expression that preserves
	 the PT_MALLOC attribute (copy assignment, address
	 arithmetic), we ought to merge PT_MALLOC, but then both
	 pointers would end up getting different name tags because
	 create_name_tags is not smart enough to determine that the
	 two come from the same malloc call.  Copy propagation before
	 aliasing should cure this.  */
      dest_pi->pt_malloc = 0;
      if (orig_pi->pt_malloc || orig_pi->pt_anything)
	set_pt_anything (dest);

      dest_pi->pt_null |= orig_pi->pt_null;

      if (!dest_pi->pt_anything
	  && orig_pi->pt_vars
	  && !bitmap_empty_p (orig_pi->pt_vars))
	{
	  if (dest_pi->pt_vars == NULL)
	    {
	      dest_pi->pt_vars = BITMAP_GGC_ALLOC ();
	      bitmap_copy (dest_pi->pt_vars, orig_pi->pt_vars);
	    }
	  else
	    bitmap_ior_into (dest_pi->pt_vars, orig_pi->pt_vars);
	}
    }
  else
    set_pt_anything (dest);
}


/* Add EXPR to the list of expressions pointed-to by PTR.  */

static void
add_pointed_to_expr (struct alias_info *ai, tree ptr, tree expr)
{
  if (TREE_CODE (expr) == WITH_SIZE_EXPR)
    expr = TREE_OPERAND (expr, 0);

  get_ptr_info (ptr);

  if (TREE_CODE (expr) == CALL_EXPR
      && (call_expr_flags (expr) & (ECF_MALLOC | ECF_MAY_BE_ALLOCA)))
    {
      /* If EXPR is a malloc-like call, then the area pointed to PTR
	 is guaranteed to not alias with anything else.  */
      set_pt_malloc (ptr);
    }
  else if (TREE_CODE (expr) == ADDR_EXPR)
    {
      /* Found P_i = ADDR_EXPR  */
      add_pointed_to_var (ai, ptr, expr);
    }
  else if (TREE_CODE (expr) == SSA_NAME && POINTER_TYPE_P (TREE_TYPE (expr)))
    {
      /* Found P_i = Q_j.  */
      merge_pointed_to_info (ai, ptr, expr);
    }
  else if (TREE_CODE (expr) == PLUS_EXPR || TREE_CODE (expr) == MINUS_EXPR)
    {
      /* Found P_i = PLUS_EXPR or P_i = MINUS_EXPR  */
      tree op0 = TREE_OPERAND (expr, 0);
      tree op1 = TREE_OPERAND (expr, 1);

      /* Both operands may be of pointer type.  FIXME: Shouldn't
	 we just expect PTR + OFFSET always?  */
      if (POINTER_TYPE_P (TREE_TYPE (op0))
	  && TREE_CODE (op0) != INTEGER_CST)
	{
	  if (TREE_CODE (op0) == SSA_NAME)
	    merge_pointed_to_info (ai, ptr, op0);
	  else if (TREE_CODE (op0) == ADDR_EXPR)
	    add_pointed_to_var (ai, ptr, op0);
	  else
	    set_pt_anything (ptr);
	}

      if (POINTER_TYPE_P (TREE_TYPE (op1))
	  && TREE_CODE (op1) != INTEGER_CST)
	{
	  if (TREE_CODE (op1) == SSA_NAME)
	    merge_pointed_to_info (ai, ptr, op1);
	  else if (TREE_CODE (op1) == ADDR_EXPR)
	    add_pointed_to_var (ai, ptr, op1);
	  else
	    set_pt_anything (ptr);
	}

      /* Neither operand is a pointer?  VAR can be pointing anywhere.
	 FIXME: Shouldn't we abort here?  If we get here, we found
	 PTR = INT_CST + INT_CST, which should not be a valid pointer
	 expression.  */
      if (!(POINTER_TYPE_P (TREE_TYPE (op0))
	    && TREE_CODE (op0) != INTEGER_CST)
	  && !(POINTER_TYPE_P (TREE_TYPE (op1))
	       && TREE_CODE (op1) != INTEGER_CST))
	set_pt_anything (ptr);
    }
  else if (integer_zerop (expr))
    {
      /* EXPR is the NULL pointer.  Mark PTR as pointing to NULL.  */
      SSA_NAME_PTR_INFO (ptr)->pt_null = 1;
    }
  else
    {
      /* If we can't recognize the expression, assume that PTR may
	 point anywhere.  */
      set_pt_anything (ptr);
    }
}


/* If VALUE is of the form &DECL, add DECL to the set of variables
   pointed-to by PTR.  Otherwise, add VALUE as a pointed-to expression by
   PTR.  AI points to the collected alias information.  */

static void
add_pointed_to_var (struct alias_info *ai, tree ptr, tree value)
{
  struct ptr_info_def *pi = get_ptr_info (ptr);
  tree pt_var;
  size_t uid;

  gcc_assert (TREE_CODE (value) == ADDR_EXPR);

  pt_var = TREE_OPERAND (value, 0);
  if (REFERENCE_CLASS_P (pt_var))
    pt_var = get_base_address (pt_var);

  if (pt_var == NULL)
    {
      pi->pt_anything = 1;
    }
  else if (SSA_VAR_P (pt_var))
    {
      uid = var_ann (pt_var)->uid;
      bitmap_set_bit (ai->addresses_needed, uid);

      if (pi->pt_vars == NULL)
	pi->pt_vars = BITMAP_GGC_ALLOC ();
      bitmap_set_bit (pi->pt_vars, uid);

      /* If the variable is a global, mark the pointer as pointing to
	 global memory (which will make its tag a global variable).  */
      if (is_global_var (pt_var))
	pi->pt_global_mem = 1;
    }
  else if (TREE_CODE (pt_var) == INDIRECT_REF
           && TREE_CODE (TREE_OPERAND (pt_var, 0)) == SSA_NAME)
    {
      /* If VALUE is of the form &(*P_j), then PTR will have the same
	 points-to information as P_j.  */
      add_pointed_to_expr (ai, ptr, TREE_OPERAND (pt_var, 0));
    }
  else
    {
      /* Give up.  PTR points anywhere.  */
      set_pt_anything (ptr);
    }
}


/* Callback for walk_use_def_chains to gather points-to information from the
   SSA web.
   
   VAR is an SSA variable or a GIMPLE expression.
   
   STMT is the statement that generates the SSA variable or, if STMT is a
      PHI_NODE, VAR is one of the PHI arguments.

   DATA is a pointer to a structure of type ALIAS_INFO.  */

static bool
collect_points_to_info_r (tree var, tree stmt, void *data)
{
  struct alias_info *ai = (struct alias_info *) data;

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "Visiting use-def links for ");
      print_generic_expr (dump_file, var, dump_flags);
      fprintf (dump_file, "\n");
    }

  switch (TREE_CODE (stmt))
    {
    case RETURN_EXPR:
      if (TREE_CODE (TREE_OPERAND (stmt, 0)) != MODIFY_EXPR)
	abort ();
      stmt = TREE_OPERAND (stmt, 0);
      /* FALLTHRU  */

    case MODIFY_EXPR:
      {
	tree rhs = TREE_OPERAND (stmt, 1);
	STRIP_NOPS (rhs);
	add_pointed_to_expr (ai, var, rhs);
	break;
      }

    case ASM_EXPR:
      /* Pointers defined by __asm__ statements can point anywhere.  */
      set_pt_anything (var);
      break;

    case NOP_EXPR:
      if (IS_EMPTY_STMT (stmt))
	{
	  tree decl = SSA_NAME_VAR (var);
	  
	  if (TREE_CODE (decl) == PARM_DECL)
	    add_pointed_to_expr (ai, var, decl);
	  else if (DECL_INITIAL (decl))
	    add_pointed_to_expr (ai, var, DECL_INITIAL (decl));
	  else
	    add_pointed_to_expr (ai, var, decl);
	}
      break;

    case PHI_NODE:
      {
        /* It STMT is a PHI node, then VAR is one of its arguments.  The
	   variable that we are analyzing is the LHS of the PHI node.  */
	tree lhs = PHI_RESULT (stmt);

	switch (TREE_CODE (var))
	  {
	  case ADDR_EXPR:
	    add_pointed_to_var (ai, lhs, var);
	    break;
	    
	  case SSA_NAME:
	    /* Avoid unnecessary merges.  */
	    if (lhs != var)
	      merge_pointed_to_info (ai, lhs, var);
	    break;
	    
	  default:
	    gcc_assert (is_gimple_min_invariant (var));
	    add_pointed_to_expr (ai, lhs, var);
	    break;
	  }
	break;
      }

    default:
      gcc_unreachable ();
    }
  
  return false;
}


/* Return true if STMT is an "escape" site from the current function.  Escape
   sites those statements which might expose the address of a variable
   outside the current function.  STMT is an escape site iff:

   	1- STMT is a function call, or
	2- STMT is an __asm__ expression, or
	3- STMT is an assignment to a non-local variable, or
	4- STMT is a return statement.

   AI points to the alias information collected so far.  */

static bool
is_escape_site (tree stmt, struct alias_info *ai)
{
  tree call = get_call_expr_in (stmt);
  if (call != NULL_TREE)
    {
      ai->num_calls_found++;

      if (!TREE_SIDE_EFFECTS (call))
	ai->num_pure_const_calls_found++;

      return true;
    }
  else if (TREE_CODE (stmt) == ASM_EXPR)
    return true;
  else if (TREE_CODE (stmt) == MODIFY_EXPR)
    {
      tree lhs = TREE_OPERAND (stmt, 0);

      /* Get to the base of _REF nodes.  */
      if (TREE_CODE (lhs) != SSA_NAME)
	lhs = get_base_address (lhs);

      /* If we couldn't recognize the LHS of the assignment, assume that it
	 is a non-local store.  */
      if (lhs == NULL_TREE)
	return true;

      /* If the RHS is a conversion between a pointer and an integer, the
	 pointer escapes since we can't track the integer.  */
      if ((TREE_CODE (TREE_OPERAND (stmt, 1)) == NOP_EXPR
	   || TREE_CODE (TREE_OPERAND (stmt, 1)) == CONVERT_EXPR
	   || TREE_CODE (TREE_OPERAND (stmt, 1)) == VIEW_CONVERT_EXPR)
	  && POINTER_TYPE_P (TREE_TYPE (TREE_OPERAND
					(TREE_OPERAND (stmt, 1), 0)))
	  && !POINTER_TYPE_P (TREE_TYPE (TREE_OPERAND (stmt, 1))))
	return true;

      /* If the LHS is an SSA name, it can't possibly represent a non-local
	 memory store.  */
      if (TREE_CODE (lhs) == SSA_NAME)
	return false;

      /* FIXME: LHS is not an SSA_NAME.  Even if it's an assignment to a
	 local variables we cannot be sure if it will escape, because we
	 don't have information about objects not in SSA form.  Need to
	 implement something along the lines of

	 J.-D. Choi, M. Gupta, M. J. Serrano, V. C. Sreedhar, and S. P.
	 Midkiff, ``Escape analysis for java,'' in Proceedings of the
	 Conference on Object-Oriented Programming Systems, Languages, and
	 Applications (OOPSLA), pp. 1-19, 1999.  */
      return true;
    }
  else if (TREE_CODE (stmt) == RETURN_EXPR)
    return true;

  return false;
}


/* Create a new memory tag of type TYPE.  If IS_TYPE_TAG is true, the tag
   is considered to represent all the pointers whose pointed-to types are
   in the same alias set class.  Otherwise, the tag represents a single
   SSA_NAME pointer variable.  */

static tree
create_memory_tag (tree type, bool is_type_tag)
{
  var_ann_t ann;
  tree tag = create_tmp_var_raw (type, (is_type_tag) ? "TMT" : "NMT");

  /* By default, memory tags are local variables.  Alias analysis will
     determine whether they should be considered globals.  */
  DECL_CONTEXT (tag) = current_function_decl;

  /* Memory tags are by definition addressable.  This also prevents
     is_gimple_ref frome confusing memory tags with optimizable
     variables.  */
  TREE_ADDRESSABLE (tag) = 1;

  ann = get_var_ann (tag);
  ann->mem_tag_kind = (is_type_tag) ? TYPE_TAG : NAME_TAG;
  ann->type_mem_tag = NULL_TREE;

  /* Add the tag to the symbol table.  */
  add_referenced_tmp_var (tag);

  return tag;
}


/* Create a name memory tag to represent a specific SSA_NAME pointer P_i.
   This is used if P_i has been found to point to a specific set of
   variables or to a non-aliased memory location like the address returned
   by malloc functions.  */

static tree
get_nmt_for (tree ptr)
{
  struct ptr_info_def *pi = get_ptr_info (ptr);
  tree tag = pi->name_mem_tag;

  if (tag == NULL_TREE)
    tag = create_memory_tag (TREE_TYPE (TREE_TYPE (ptr)), false);

  /* If PTR is a PARM_DECL, it points to a global variable or malloc,
     then its name tag should be considered a global variable.  */
  if (TREE_CODE (SSA_NAME_VAR (ptr)) == PARM_DECL
      || pi->pt_malloc
      || pi->pt_global_mem)
    mark_call_clobbered (tag);

  return tag;
}


/* Return the type memory tag associated to pointer PTR.  A memory tag is an
   artificial variable that represents the memory location pointed-to by
   PTR.  It is used to model the effects of pointer de-references on
   addressable variables.
   
   AI points to the data gathered during alias analysis.  This function
   populates the array AI->POINTERS.  */

static tree
get_tmt_for (tree ptr, struct alias_info *ai)
{
  size_t i;
  tree tag;
  tree tag_type = TREE_TYPE (TREE_TYPE (ptr));
  HOST_WIDE_INT tag_set = get_alias_set (tag_type);

  /* To avoid creating unnecessary memory tags, only create one memory tag
     per alias set class.  Note that it may be tempting to group
     memory tags based on conflicting alias sets instead of
     equivalence.  That would be wrong because alias sets are not
     necessarily transitive (as demonstrated by the libstdc++ test
     23_containers/vector/cons/4.cc).  Given three alias sets A, B, C
     such that conflicts (A, B) == true and conflicts (A, C) == true,
     it does not necessarily follow that conflicts (B, C) == true.  */
  for (i = 0, tag = NULL_TREE; i < ai->num_pointers; i++)
    {
      struct alias_map_d *curr = ai->pointers[i];
      if (tag_set == curr->set)
	{
	  tag = var_ann (curr->var)->type_mem_tag;
	  break;
	}
    }

  /* If VAR cannot alias with any of the existing memory tags, create a new
     tag for PTR and add it to the POINTERS array.  */
  if (tag == NULL_TREE)
    {
      struct alias_map_d *alias_map;

      /* If PTR did not have a type tag already, create a new TMT.*
	 artificial variable representing the memory location
	 pointed-to by PTR.  */
      if (var_ann (ptr)->type_mem_tag == NULL_TREE)
	tag = create_memory_tag (tag_type, true);
      else
	tag = var_ann (ptr)->type_mem_tag;

      /* Add PTR to the POINTERS array.  Note that we are not interested in
	 PTR's alias set.  Instead, we cache the alias set for the memory that
	 PTR points to.  */
      alias_map = xcalloc (1, sizeof (*alias_map));
      alias_map->var = ptr;
      alias_map->set = tag_set;
      ai->pointers[ai->num_pointers++] = alias_map;
    }

  /* If the pointed-to type is volatile, so is the tag.  */
  TREE_THIS_VOLATILE (tag) |= TREE_THIS_VOLATILE (tag_type);

  /* Make sure that the type tag has the same alias set as the
     pointed-to type.  */
  gcc_assert (tag_set == get_alias_set (tag));

  return tag;
}


/* Create GLOBAL_VAR, an artificial global variable to act as a
   representative of all the variables that may be clobbered by function
   calls.  */

static void
create_global_var (void)
{
  global_var = build_decl (VAR_DECL, get_identifier (".GLOBAL_VAR"),
                           void_type_node);
  DECL_ARTIFICIAL (global_var) = 1;
  TREE_READONLY (global_var) = 0;
  DECL_EXTERNAL (global_var) = 1;
  TREE_STATIC (global_var) = 1;
  TREE_USED (global_var) = 1;
  DECL_CONTEXT (global_var) = NULL_TREE;
  TREE_THIS_VOLATILE (global_var) = 0;
  TREE_ADDRESSABLE (global_var) = 0;

  add_referenced_tmp_var (global_var);
  bitmap_set_bit (vars_to_rename, var_ann (global_var)->uid);
}


/* Dump alias statistics on FILE.  */

static void 
dump_alias_stats (FILE *file)
{
  const char *funcname
    = lang_hooks.decl_printable_name (current_function_decl, 2);
  fprintf (file, "\nAlias statistics for %s\n\n", funcname);
  fprintf (file, "Total alias queries:\t%u\n", alias_stats.alias_queries);
  fprintf (file, "Total alias mayalias results:\t%u\n", 
	   alias_stats.alias_mayalias);
  fprintf (file, "Total alias noalias results:\t%u\n",
	   alias_stats.alias_noalias);
  fprintf (file, "Total simple queries:\t%u\n",
	   alias_stats.simple_queries);
  fprintf (file, "Total simple resolved:\t%u\n",
	   alias_stats.simple_resolved);
  fprintf (file, "Total TBAA queries:\t%u\n",
	   alias_stats.tbaa_queries);
  fprintf (file, "Total TBAA resolved:\t%u\n",
	   alias_stats.tbaa_resolved);
}
  

/* Dump alias information on FILE.  */

void
dump_alias_info (FILE *file)
{
  size_t i;
  const char *funcname
    = lang_hooks.decl_printable_name (current_function_decl, 2);

  fprintf (file, "\nFlow-insensitive alias information for %s\n\n", funcname);

  fprintf (file, "Aliased symbols\n\n");
  for (i = 0; i < num_referenced_vars; i++)
    {
      tree var = referenced_var (i);
      if (may_be_aliased (var))
	dump_variable (file, var);
    }

  fprintf (file, "\nDereferenced pointers\n\n");
  for (i = 0; i < num_referenced_vars; i++)
    {
      tree var = referenced_var (i);
      var_ann_t ann = var_ann (var);
      if (ann->type_mem_tag)
	dump_variable (file, var);
    }

  fprintf (file, "\nType memory tags\n\n");
  for (i = 0; i < num_referenced_vars; i++)
    {
      tree var = referenced_var (i);
      var_ann_t ann = var_ann (var);
      if (ann->mem_tag_kind == TYPE_TAG)
	dump_variable (file, var);
    }

  fprintf (file, "\n\nFlow-sensitive alias information for %s\n\n", funcname);

  fprintf (file, "SSA_NAME pointers\n\n");
  for (i = 1; i < num_ssa_names; i++)
    {
      tree ptr = ssa_name (i);
      struct ptr_info_def *pi;
      
      if (ptr == NULL_TREE)
	continue;

      pi = SSA_NAME_PTR_INFO (ptr);
      if (!SSA_NAME_IN_FREE_LIST (ptr)
	  && pi
	  && pi->name_mem_tag)
	dump_points_to_info_for (file, ptr);
    }

  fprintf (file, "\nName memory tags\n\n");
  for (i = 0; i < num_referenced_vars; i++)
    {
      tree var = referenced_var (i);
      var_ann_t ann = var_ann (var);
      if (ann->mem_tag_kind == NAME_TAG)
	dump_variable (file, var);
    }

  fprintf (file, "\n");
}


/* Dump alias information on stderr.  */

void
debug_alias_info (void)
{
  dump_alias_info (stderr);
}


/* Return the alias information associated with pointer T.  It creates a
   new instance if none existed.  */

struct ptr_info_def *
get_ptr_info (tree t)
{
  struct ptr_info_def *pi;

  gcc_assert (POINTER_TYPE_P (TREE_TYPE (t)));

  pi = SSA_NAME_PTR_INFO (t);
  if (pi == NULL)
    {
      pi = ggc_alloc (sizeof (*pi));
      memset ((void *)pi, 0, sizeof (*pi));
      SSA_NAME_PTR_INFO (t) = pi;
    }

  return pi;
}


/* Dump points-to information for SSA_NAME PTR into FILE.  */

void
dump_points_to_info_for (FILE *file, tree ptr)
{
  struct ptr_info_def *pi = SSA_NAME_PTR_INFO (ptr);

  print_generic_expr (file, ptr, dump_flags);

  if (pi)
    {
      if (pi->name_mem_tag)
	{
	  fprintf (file, ", name memory tag: ");
	  print_generic_expr (file, pi->name_mem_tag, dump_flags);
	}

      if (pi->is_dereferenced)
	fprintf (file, ", is dereferenced");

      if (pi->value_escapes_p)
	fprintf (file, ", its value escapes");

      if (pi->pt_anything)
	fprintf (file, ", points-to anything");

      if (pi->pt_malloc)
	fprintf (file, ", points-to malloc");

      if (pi->pt_null)
	fprintf (file, ", points-to NULL");

      if (pi->pt_vars)
	{
	  unsigned ix;
	  bitmap_iterator bi;

	  fprintf (file, ", points-to vars: { ");
	  EXECUTE_IF_SET_IN_BITMAP (pi->pt_vars, 0, ix, bi)
	    {
	      print_generic_expr (file, referenced_var (ix), dump_flags);
	      fprintf (file, " ");
	    }
	  fprintf (file, "}");
	}
    }

  fprintf (file, "\n");
}


/* Dump points-to information for VAR into stderr.  */

void
debug_points_to_info_for (tree var)
{
  dump_points_to_info_for (stderr, var);
}


/* Dump points-to information into FILE.  NOTE: This function is slow, as
   it needs to traverse the whole CFG looking for pointer SSA_NAMEs.  */

void
dump_points_to_info (FILE *file)
{
  basic_block bb;
  block_stmt_iterator si;
  size_t i;
  ssa_op_iter iter;
  const char *fname =
    lang_hooks.decl_printable_name (current_function_decl, 2);

  fprintf (file, "\n\nPointed-to sets for pointers in %s\n\n", fname);

  /* First dump points-to information for the default definitions of
     pointer variables.  This is necessary because default definitions are
     not part of the code.  */
  for (i = 0; i < num_referenced_vars; i++)
    {
      tree var = referenced_var (i);
      if (POINTER_TYPE_P (TREE_TYPE (var)))
	{
	  var_ann_t ann = var_ann (var);
	  if (ann->default_def)
	    dump_points_to_info_for (file, ann->default_def);
	}
    }

  /* Dump points-to information for every pointer defined in the program.  */
  FOR_EACH_BB (bb)
    {
      tree phi;

      for (phi = phi_nodes (bb); phi; phi = PHI_CHAIN (phi))
	{
	  tree ptr = PHI_RESULT (phi);
	  if (POINTER_TYPE_P (TREE_TYPE (ptr)))
	    dump_points_to_info_for (file, ptr);
	}

	for (si = bsi_start (bb); !bsi_end_p (si); bsi_next (&si))
	  {
	    tree stmt = bsi_stmt (si);
	    tree def;
	    FOR_EACH_SSA_TREE_OPERAND (def, stmt, iter, SSA_OP_DEF)
	      if (POINTER_TYPE_P (TREE_TYPE (def)))
		dump_points_to_info_for (file, def);
	  }
    }

  fprintf (file, "\n");
}


/* Dump points-to info pointed by PTO into STDERR.  */

void
debug_points_to_info (void)
{
  dump_points_to_info (stderr);
}

/* Dump to FILE the list of variables that may be aliasing VAR.  */

void
dump_may_aliases_for (FILE *file, tree var)
{
  varray_type aliases;
  
  if (TREE_CODE (var) == SSA_NAME)
    var = SSA_NAME_VAR (var);

  aliases = var_ann (var)->may_aliases;
  if (aliases)
    {
      size_t i;
      fprintf (file, "{ ");
      for (i = 0; i < VARRAY_ACTIVE_SIZE (aliases); i++)
	{
	  print_generic_expr (file, VARRAY_TREE (aliases, i), dump_flags);
	  fprintf (file, " ");
	}
      fprintf (file, "}");
    }
}


/* Dump to stderr the list of variables that may be aliasing VAR.  */

void
debug_may_aliases_for (tree var)
{
  dump_may_aliases_for (stderr, var);
}

/* Return true if VAR may be aliased.  */

bool
may_be_aliased (tree var)
{
  /* Obviously.  */
  if (TREE_ADDRESSABLE (var))
    return true;

  /* Globally visible variables can have their addresses taken by other
     translation units.  */
  if (DECL_EXTERNAL (var) || TREE_PUBLIC (var))
    return true;

  /* Automatic variables can't have their addresses escape any other way.
     This must be after the check for global variables, as extern declarations
     do not have TREE_STATIC set.  */
  if (!TREE_STATIC (var))
    return false;

  /* If we're in unit-at-a-time mode, then we must have seen all occurrences
     of address-of operators, and so we can trust TREE_ADDRESSABLE.  Otherwise
     we can only be sure the variable isn't addressable if it's local to the
     current function.  */
  if (flag_unit_at_a_time)
    return false;
  if (decl_function_context (var) == current_function_decl)
    return false;

  return true;
}
