/* Procedure integration for GCC.
   Copyright (C) 1988, 1991, 1993, 1994, 1995, 1996, 1997, 1998, 1999,
   2000, 2001, 2002, 2003, 2004 Free Software Foundation, Inc.
   Contributed by Michael Tiemann (tiemann@cygnus.com)

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 2, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to the Free
Software Foundation, 59 Temple Place - Suite 330, Boston, MA
02111-1307, USA.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"

#include "rtl.h"
#include "tree.h"
#include "tm_p.h"
#include "regs.h"
#include "flags.h"
#include "debug.h"
#include "insn-config.h"
#include "expr.h"
#include "output.h"
#include "recog.h"
#include "integrate.h"
#include "real.h"
#include "except.h"
#include "function.h"
#include "toplev.h"
#include "intl.h"
#include "loop.h"
#include "params.h"
#include "ggc.h"
#include "target.h"
#include "langhooks.h"

/* Similar, but round to the next highest integer that meets the
   alignment.  */
#define CEIL_ROUND(VALUE,ALIGN)	(((VALUE) + (ALIGN) - 1) & ~((ALIGN)- 1))

/* Default max number of insns a function can have and still be inline.
   This is overridden on RISC machines.  */
#ifndef INTEGRATE_THRESHOLD
/* Inlining small functions might save more space then not inlining at
   all.  Assume 1 instruction for the call and 1.5 insns per argument.  */
#define INTEGRATE_THRESHOLD(DECL) \
  (optimize_size \
   ? (1 + (3 * list_length (DECL_ARGUMENTS (DECL))) / 2) \
   : (8 * (8 + list_length (DECL_ARGUMENTS (DECL)))))
#endif


/* Private type used by {get/has}_func_hard_reg_initial_val.  */
typedef struct initial_value_pair GTY(()) {
  rtx hard_reg;
  rtx pseudo;
} initial_value_pair;
typedef struct initial_value_struct GTY(()) {
  int num_entries;
  int max_entries;
  initial_value_pair * GTY ((length ("%h.num_entries"))) entries;
} initial_value_struct;

static void setup_initial_hard_reg_value_integration (struct function *,
						      struct inline_remap *);

static rtvec initialize_for_inline (tree);
static void note_modified_parmregs (rtx, rtx, void *);
static void integrate_parm_decls (tree, struct inline_remap *, rtvec);
static tree integrate_decl_tree (tree, struct inline_remap *);
static void subst_constants (rtx *, rtx, struct inline_remap *, int);
static void set_block_origin_self (tree);
static void set_block_abstract_flags (tree, int);
static void process_reg_param (struct inline_remap *, rtx, rtx);
static void mark_stores (rtx, rtx, void *);
static void save_parm_insns (rtx, rtx);
static void copy_insn_list (rtx, struct inline_remap *, rtx);
static void copy_insn_notes (rtx, struct inline_remap *, int);
static int compare_blocks (const void *, const void *);
static int find_block (const void *, const void *);

/* Used by copy_rtx_and_substitute; this indicates whether the function is
   called for the purpose of inlining or some other purpose (i.e. loop
   unrolling).  This affects how constant pool references are handled.
   This variable contains the FUNCTION_DECL for the inlined function.  */
static struct function *inlining = 0;

/* Returns the Ith entry in the label_map contained in MAP.  If the
   Ith entry has not yet been set, return a fresh label.  This function
   performs a lazy initialization of label_map, thereby avoiding huge memory
   explosions when the label_map gets very large.  */

rtx
get_label_from_map (struct inline_remap *map, int i)
{
  rtx x = map->label_map[i];

  if (x == NULL_RTX)
    x = map->label_map[i] = gen_label_rtx ();

  return x;
}

/* Return false if the function FNDECL cannot be inlined on account of its
   attributes, true otherwise.  */
bool
function_attribute_inlinable_p (tree fndecl)
{
  if (targetm.attribute_table)
    {
      tree a;

      for (a = DECL_ATTRIBUTES (fndecl); a; a = TREE_CHAIN (a))
	{
	  tree name = TREE_PURPOSE (a);
	  int i;

	  for (i = 0; targetm.attribute_table[i].name != NULL; i++)
	    if (is_attribute_p (targetm.attribute_table[i].name, name))
	      return (*targetm.function_attribute_inlinable_p) (fndecl);
	}
    }

  return true;
}

/* Zero if the current function (whose FUNCTION_DECL is FNDECL)
   is safe and reasonable to integrate into other functions.
   Nonzero means value is a warning msgid with a single %s
   for the function's name.  */

const char *
function_cannot_inline_p (tree fndecl)
{
  rtx insn;
  tree last = tree_last (TYPE_ARG_TYPES (TREE_TYPE (fndecl)));

  /* For functions marked as inline increase the maximum size to
     MAX_INLINE_INSNS_RTL (--param max-inline-insn-rtl=<n>). For
     regular functions use the limit given by INTEGRATE_THRESHOLD.
     Note that the RTL inliner is not used by the languages that use
     the tree inliner (C, C++).  */

  int max_insns = (DECL_INLINE (fndecl))
		   ? (MAX_INLINE_INSNS_RTL
		      + 8 * list_length (DECL_ARGUMENTS (fndecl)))
		   : INTEGRATE_THRESHOLD (fndecl);

  int ninsns = 0;
  tree parms;

  if (DECL_UNINLINABLE (fndecl))
    return N_("function cannot be inline");

  /* No inlines with varargs.  */
  if (last && TREE_VALUE (last) != void_type_node)
    return N_("varargs function cannot be inline");

  if (current_function_calls_alloca)
    return N_("function using alloca cannot be inline");

  if (current_function_calls_longjmp)
    return N_("function using longjmp cannot be inline");
  
  if (current_function_calls_setjmp)
    return N_("function using setjmp cannot be inline");

  if (current_function_calls_eh_return)
    return N_("function uses __builtin_eh_return");

  if (current_function_contains_functions)
    return N_("function with nested functions cannot be inline");

  if (forced_labels)
    return
      N_("function with label addresses used in initializers cannot inline");

  if (current_function_cannot_inline)
    return current_function_cannot_inline;

  /* If its not even close, don't even look.  */
  if (get_max_uid () > 3 * max_insns)
    return N_("function too large to be inline");

#if 0
  /* Don't inline functions which do not specify a function prototype and
     have BLKmode argument or take the address of a parameter.  */
  for (parms = DECL_ARGUMENTS (fndecl); parms; parms = TREE_CHAIN (parms))
    {
      if (TYPE_MODE (TREE_TYPE (parms)) == BLKmode)
	TREE_ADDRESSABLE (parms) = 1;
      if (last == NULL_TREE && TREE_ADDRESSABLE (parms))
	return N_("no prototype, and parameter address used; cannot be inline");
    }
#endif

  /* We can't inline functions that return structures
     the old-fashioned PCC way, copying into a static block.  */
  if (current_function_returns_pcc_struct)
    return N_("inline functions not supported for this return value type");

  /* We can't inline functions that return structures of varying size.  */
  if (TREE_CODE (TREE_TYPE (TREE_TYPE (fndecl))) != VOID_TYPE
      && int_size_in_bytes (TREE_TYPE (TREE_TYPE (fndecl))) < 0)
    return N_("function with varying-size return value cannot be inline");

  /* Cannot inline a function with a varying size argument or one that
     receives a transparent union.  */
  for (parms = DECL_ARGUMENTS (fndecl); parms; parms = TREE_CHAIN (parms))
    {
      if (int_size_in_bytes (TREE_TYPE (parms)) < 0)
	return N_("function with varying-size parameter cannot be inline");
      else if (TREE_CODE (TREE_TYPE (parms)) == UNION_TYPE
	       && TYPE_TRANSPARENT_UNION (TREE_TYPE (parms)))
	return N_("function with transparent unit parameter cannot be inline");
    }

  if (get_max_uid () > max_insns)
    {
      for (ninsns = 0, insn = get_first_nonparm_insn ();
	   insn && ninsns < max_insns;
	   insn = NEXT_INSN (insn))
	if (INSN_P (insn))
	  ninsns++;

      if (ninsns >= max_insns)
	return N_("function too large to be inline");
    }

  /* We will not inline a function which uses computed goto.  The addresses of
     its local labels, which may be tucked into global storage, are of course
     not constant across instantiations, which causes unexpected behavior.  */
  if (current_function_has_computed_jump)
    return N_("function with computed jump cannot inline");

  /* We cannot inline a nested function that jumps to a nonlocal label.  */
  if (current_function_has_nonlocal_goto)
    return N_("function with nonlocal goto cannot be inline");

  /* We can't inline functions that return a PARALLEL rtx.  */
  if (DECL_RTL_SET_P (DECL_RESULT (fndecl)))
    {
      rtx result = DECL_RTL (DECL_RESULT (fndecl));
      if (GET_CODE (result) == PARALLEL)
	return N_("inline functions not supported for this return value type");
    }

  /* If the function has a target specific attribute attached to it,
     then we assume that we should not inline it.  This can be overridden
     by the target if it defines TARGET_FUNCTION_ATTRIBUTE_INLINABLE_P.  */
  if (!function_attribute_inlinable_p (fndecl))
    return N_("function with target specific attribute(s) cannot be inlined");

  return NULL;
}

/* Map pseudo reg number into the PARM_DECL for the parm living in the reg.
   Zero for a reg that isn't a parm's home.
   Only reg numbers less than max_parm_reg are mapped here.  */
static tree *parmdecl_map;

/* In save_for_inline, nonzero if past the parm-initialization insns.  */
static int in_nonparm_insns;

/* Subroutine for `save_for_inline'.  Performs initialization
   needed to save FNDECL's insns and info for future inline expansion.  */

static rtvec
initialize_for_inline (tree fndecl)
{
  int i;
  rtvec arg_vector;
  tree parms;

  /* Clear out PARMDECL_MAP.  It was allocated in the caller's frame.  */
  memset (parmdecl_map, 0, max_parm_reg * sizeof (tree));
  arg_vector = rtvec_alloc (list_length (DECL_ARGUMENTS (fndecl)));

  for (parms = DECL_ARGUMENTS (fndecl), i = 0;
       parms;
       parms = TREE_CHAIN (parms), i++)
    {
      rtx p = DECL_RTL (parms);

      /* If we have (mem (addressof (mem ...))), use the inner MEM since
	 otherwise the copy_rtx call below will not unshare the MEM since
	 it shares ADDRESSOF.  */
      if (GET_CODE (p) == MEM && GET_CODE (XEXP (p, 0)) == ADDRESSOF
	  && GET_CODE (XEXP (XEXP (p, 0), 0)) == MEM)
	p = XEXP (XEXP (p, 0), 0);

      RTVEC_ELT (arg_vector, i) = p;

      if (GET_CODE (p) == REG)
	parmdecl_map[REGNO (p)] = parms;
      else if (GET_CODE (p) == CONCAT)
	{
	  rtx preal = gen_realpart (GET_MODE (XEXP (p, 0)), p);
	  rtx pimag = gen_imagpart (GET_MODE (preal), p);

	  if (GET_CODE (preal) == REG)
	    parmdecl_map[REGNO (preal)] = parms;
	  if (GET_CODE (pimag) == REG)
	    parmdecl_map[REGNO (pimag)] = parms;
	}

      /* This flag is cleared later
	 if the function ever modifies the value of the parm.  */
      TREE_READONLY (parms) = 1;
    }

  return arg_vector;
}

/* Copy NODE (which must be a DECL, but not a PARM_DECL).  The DECL
   originally was in the FROM_FN, but now it will be in the
   TO_FN.  */

tree
copy_decl_for_inlining (tree decl, tree from_fn, tree to_fn)
{
  tree copy;

  /* Copy the declaration.  */
  if (TREE_CODE (decl) == PARM_DECL || TREE_CODE (decl) == RESULT_DECL)
    {
      tree type;
      int invisiref = 0;

      /* See if the frontend wants to pass this by invisible reference.  */
      if (TREE_CODE (decl) == PARM_DECL
	  && DECL_ARG_TYPE (decl) != TREE_TYPE (decl)
	  && POINTER_TYPE_P (DECL_ARG_TYPE (decl))
	  && TREE_TYPE (DECL_ARG_TYPE (decl)) == TREE_TYPE (decl))
	{
	  invisiref = 1;
	  type = DECL_ARG_TYPE (decl);
	}
      else
	type = TREE_TYPE (decl);

      /* For a parameter, we must make an equivalent VAR_DECL, not a
	 new PARM_DECL.  */
      copy = build_decl (VAR_DECL, DECL_NAME (decl), type);
      if (!invisiref)
	{
	  TREE_ADDRESSABLE (copy) = TREE_ADDRESSABLE (decl);
	  TREE_READONLY (copy) = TREE_READONLY (decl);
	  TREE_THIS_VOLATILE (copy) = TREE_THIS_VOLATILE (decl);
	}
      else
	{
	  TREE_ADDRESSABLE (copy) = 0;
	  TREE_READONLY (copy) = 1;
	  TREE_THIS_VOLATILE (copy) = 0;
	}
    }
  else
    {
      copy = copy_node (decl);
      /* The COPY is not abstract; it will be generated in TO_FN.  */
      DECL_ABSTRACT (copy) = 0;
      (*lang_hooks.dup_lang_specific_decl) (copy);

      /* TREE_ADDRESSABLE isn't used to indicate that a label's
	 address has been taken; it's for internal bookkeeping in
	 expand_goto_internal.  */
      if (TREE_CODE (copy) == LABEL_DECL)
	TREE_ADDRESSABLE (copy) = 0;
    }

  /* Set the DECL_ABSTRACT_ORIGIN so the debugging routines know what
     declaration inspired this copy.  */
  DECL_ABSTRACT_ORIGIN (copy) = DECL_ORIGIN (decl);

  /* The new variable/label has no RTL, yet.  */
  if (!TREE_STATIC (copy) && !DECL_EXTERNAL (copy))
    SET_DECL_RTL (copy, NULL_RTX);

  /* These args would always appear unused, if not for this.  */
  TREE_USED (copy) = 1;

  /* Set the context for the new declaration.  */
  if (!DECL_CONTEXT (decl))
    /* Globals stay global.  */
    ;
  else if (DECL_CONTEXT (decl) != from_fn)
    /* Things that weren't in the scope of the function we're inlining
       from aren't in the scope we're inlining to, either.  */
    ;
  else if (TREE_STATIC (decl))
    /* Function-scoped static variables should stay in the original
       function.  */
    ;
  else
    /* Ordinary automatic local variables are now in the scope of the
       new function.  */
    DECL_CONTEXT (copy) = to_fn;

  return copy;
}

/* Make the insns and PARM_DECLs of the current function permanent
   and record other information in DECL_SAVED_INSNS to allow inlining
   of this function in subsequent calls.

   This routine need not copy any insns because we are not going
   to immediately compile the insns in the insn chain.  There
   are two cases when we would compile the insns for FNDECL:
   (1) when FNDECL is expanded inline, and (2) when FNDECL needs to
   be output at the end of other compilation, because somebody took
   its address.  In the first case, the insns of FNDECL are copied
   as it is expanded inline, so FNDECL's saved insns are not
   modified.  In the second case, FNDECL is used for the last time,
   so modifying the rtl is not a problem.

   We don't have to worry about FNDECL being inline expanded by
   other functions which are written at the end of compilation
   because flag_no_inline is turned on when we begin writing
   functions at the end of compilation.  */

void
save_for_inline (tree fndecl)
{
  rtx insn;
  rtvec argvec;
  rtx first_nonparm_insn;

  /* Set up PARMDECL_MAP which maps pseudo-reg number to its PARM_DECL.
     Later we set TREE_READONLY to 0 if the parm is modified inside the fn.
     Also set up ARG_VECTOR, which holds the unmodified DECL_RTX values
     for the parms, prior to elimination of virtual registers.
     These values are needed for substituting parms properly.  */
  if (! flag_no_inline)
    parmdecl_map = xmalloc (max_parm_reg * sizeof (tree));

  /* Make and emit a return-label if we have not already done so.  */

  if (return_label == 0)
    {
      return_label = gen_label_rtx ();
      emit_label (return_label);
    }

  if (! flag_no_inline)
    argvec = initialize_for_inline (fndecl);
  else
    argvec = NULL;

  /* Delete basic block notes created by early run of find_basic_block.
     The notes would be later used by find_basic_blocks to reuse the memory
     for basic_block structures on already freed obstack.  */
  for (insn = get_insns (); insn ; insn = NEXT_INSN (insn))
    if (GET_CODE (insn) == NOTE && NOTE_LINE_NUMBER (insn) == NOTE_INSN_BASIC_BLOCK)
      delete_related_insns (insn);

  /* If there are insns that copy parms from the stack into pseudo registers,
     those insns are not copied.  `expand_inline_function' must
     emit the correct code to handle such things.  */

  insn = get_insns ();
  if (GET_CODE (insn) != NOTE)
    abort ();

  if (! flag_no_inline)
    {
      /* Get the insn which signals the end of parameter setup code.  */
      first_nonparm_insn = get_first_nonparm_insn ();

      /* Now just scan the chain of insns to see what happens to our
	 PARM_DECLs.  If a PARM_DECL is used but never modified, we
	 can substitute its rtl directly when expanding inline (and
	 perform constant folding when its incoming value is
	 constant).  Otherwise, we have to copy its value into a new
	 register and track the new register's life.  */
      in_nonparm_insns = 0;
      save_parm_insns (insn, first_nonparm_insn);

      cfun->inl_max_label_num = max_label_num ();
      cfun->inl_last_parm_insn = cfun->x_last_parm_insn;
      cfun->original_arg_vector = argvec;
    }
  cfun->original_decl_initial = DECL_INITIAL (fndecl);
  cfun->no_debugging_symbols = (write_symbols == NO_DEBUG);
  cfun->saved_for_inline = 1;

  /* Clean up.  */
  if (! flag_no_inline)
    free (parmdecl_map);
}

/* Scan the chain of insns to see what happens to our PARM_DECLs.  If a
   PARM_DECL is used but never modified, we can substitute its rtl directly
   when expanding inline (and perform constant folding when its incoming
   value is constant). Otherwise, we have to copy its value into a new
   register and track the new register's life.  */

static void
save_parm_insns (rtx insn, rtx first_nonparm_insn)
{
  if (insn == NULL_RTX)
    return;

  for (insn = NEXT_INSN (insn); insn; insn = NEXT_INSN (insn))
    {
      if (insn == first_nonparm_insn)
	in_nonparm_insns = 1;

      if (INSN_P (insn))
	{
	  /* Record what interesting things happen to our parameters.  */
	  note_stores (PATTERN (insn), note_modified_parmregs, NULL);

	  /* If this is a CALL_PLACEHOLDER insn then we need to look into the
	     three attached sequences: normal call, sibling call and tail
	     recursion.  */
	  if (GET_CODE (insn) == CALL_INSN
	      && GET_CODE (PATTERN (insn)) == CALL_PLACEHOLDER)
	    {
	      int i;

	      for (i = 0; i < 3; i++)
		save_parm_insns (XEXP (PATTERN (insn), i),
				 first_nonparm_insn);
	    }
	}
    }
}

/* Note whether a parameter is modified or not.  */

static void
note_modified_parmregs (rtx reg, rtx x ATTRIBUTE_UNUSED, void *data ATTRIBUTE_UNUSED)
{
  if (GET_CODE (reg) == REG && in_nonparm_insns
      && REGNO (reg) < max_parm_reg
      && REGNO (reg) >= FIRST_PSEUDO_REGISTER
      && parmdecl_map[REGNO (reg)] != 0)
    TREE_READONLY (parmdecl_map[REGNO (reg)]) = 0;
}

/* Unfortunately, we need a global copy of const_equiv map for communication
   with a function called from note_stores.  Be *very* careful that this
   is used properly in the presence of recursion.  */

varray_type global_const_equiv_varray;

#define FIXED_BASE_PLUS_P(X) \
  (GET_CODE (X) == PLUS && GET_CODE (XEXP (X, 1)) == CONST_INT	\
   && GET_CODE (XEXP (X, 0)) == REG				\
   && REGNO (XEXP (X, 0)) >= FIRST_VIRTUAL_REGISTER		\
   && REGNO (XEXP (X, 0)) <= LAST_VIRTUAL_REGISTER)

/* Called to set up a mapping for the case where a parameter is in a
   register.  If it is read-only and our argument is a constant, set up the
   constant equivalence.

   If LOC is REG_USERVAR_P, the usual case, COPY must also have that flag set
   if it is a register.

   Also, don't allow hard registers here; they might not be valid when
   substituted into insns.  */
static void
process_reg_param (struct inline_remap *map, rtx loc, rtx copy)
{
  if ((GET_CODE (copy) != REG && GET_CODE (copy) != SUBREG)
      || (GET_CODE (copy) == REG && REG_USERVAR_P (loc)
	  && ! REG_USERVAR_P (copy))
      || (GET_CODE (copy) == REG
	  && REGNO (copy) < FIRST_PSEUDO_REGISTER))
    {
      rtx temp = copy_to_mode_reg (GET_MODE (loc), copy);
      REG_USERVAR_P (temp) = REG_USERVAR_P (loc);
      if (CONSTANT_P (copy) || FIXED_BASE_PLUS_P (copy))
	SET_CONST_EQUIV_DATA (map, temp, copy, CONST_AGE_PARM);
      copy = temp;
    }
  map->reg_map[REGNO (loc)] = copy;
}

/* Compare two BLOCKs for qsort.  The key we sort on is the
   BLOCK_ABSTRACT_ORIGIN of the blocks.  We cannot just subtract the
   two pointers, because it may overflow sizeof(int).  */

static int
compare_blocks (const void *v1, const void *v2)
{
  tree b1 = *((const tree *) v1);
  tree b2 = *((const tree *) v2);
  char *p1 = (char *) BLOCK_ABSTRACT_ORIGIN (b1);
  char *p2 = (char *) BLOCK_ABSTRACT_ORIGIN (b2);

  if (p1 == p2)
    return 0;
  return p1 < p2 ? -1 : 1;
}

/* Compare two BLOCKs for bsearch.  The first pointer corresponds to
   an original block; the second to a remapped equivalent.  */

static int
find_block (const void *v1, const void *v2)
{
  const union tree_node *b1 = (const union tree_node *) v1;
  tree b2 = *((const tree *) v2);
  char *p1 = (char *) b1;
  char *p2 = (char *) BLOCK_ABSTRACT_ORIGIN (b2);

  if (p1 == p2)
    return 0;
  return p1 < p2 ? -1 : 1;
}

/* Integrate the procedure defined by FNDECL.  Note that this function
   may wind up calling itself.  Since the static variables are not
   reentrant, we do not assign them until after the possibility
   of recursion is eliminated.

   If IGNORE is nonzero, do not produce a value.
   Otherwise store the value in TARGET if it is nonzero and that is convenient.

   Value is:
   (rtx)-1 if we could not substitute the function
   0 if we substituted it and it does not produce a value
   else an rtx for where the value is stored.  */

rtx
expand_inline_function (tree fndecl, tree parms, rtx target, int ignore,
			tree type, rtx structure_value_addr)
{
  struct function *inlining_previous;
  struct function *inl_f = DECL_SAVED_INSNS (fndecl);
  tree formal, actual, block;
  rtx parm_insns = inl_f->emit->x_first_insn;
  rtx insns = (inl_f->inl_last_parm_insn
	       ? NEXT_INSN (inl_f->inl_last_parm_insn)
	       : parm_insns);
  tree *arg_trees;
  rtx *arg_vals;
  int max_regno;
  int i;
  int min_labelno = inl_f->emit->x_first_label_num;
  int max_labelno = inl_f->inl_max_label_num;
  int nargs;
  rtx loc;
  rtx stack_save = 0;
  rtx temp;
  struct inline_remap *map = 0;
  rtvec arg_vector = inl_f->original_arg_vector;
  rtx static_chain_value = 0;
  int inl_max_uid;
  int eh_region_offset;

  /* The pointer used to track the true location of the memory used
     for MAP->LABEL_MAP.  */
  rtx *real_label_map = 0;

  /* Allow for equivalences of the pseudos we make for virtual fp and ap.  */
  max_regno = inl_f->emit->x_reg_rtx_no + 3;
  if (max_regno < FIRST_PSEUDO_REGISTER)
    abort ();

  /* Pull out the decl for the function definition; fndecl may be a
     local declaration, which would break DECL_ABSTRACT_ORIGIN.  */
  fndecl = inl_f->decl;

  nargs = list_length (DECL_ARGUMENTS (fndecl));

  if (cfun->preferred_stack_boundary < inl_f->preferred_stack_boundary)
    cfun->preferred_stack_boundary = inl_f->preferred_stack_boundary;

  /* Check that the parms type match and that sufficient arguments were
     passed.  Since the appropriate conversions or default promotions have
     already been applied, the machine modes should match exactly.  */

  for (formal = DECL_ARGUMENTS (fndecl), actual = parms;
       formal;
       formal = TREE_CHAIN (formal), actual = TREE_CHAIN (actual))
    {
      tree arg;
      enum machine_mode mode;

      if (actual == 0)
	return (rtx) (size_t) -1;

      arg = TREE_VALUE (actual);
      mode = TYPE_MODE (DECL_ARG_TYPE (formal));

      if (arg == error_mark_node
	  || mode != TYPE_MODE (TREE_TYPE (arg))
	  /* If they are block mode, the types should match exactly.
	     They don't match exactly if TREE_TYPE (FORMAL) == ERROR_MARK_NODE,
	     which could happen if the parameter has incomplete type.  */
	  || (mode == BLKmode
	      && (TYPE_MAIN_VARIANT (TREE_TYPE (arg))
		  != TYPE_MAIN_VARIANT (TREE_TYPE (formal)))))
	return (rtx) (size_t) -1;
    }

  /* If there is a TARGET which is a readonly BLKmode MEM and DECL_RESULT
     is also a mem, we are going to lose the readonly on the stores, so don't
     inline.  */
  if (target != 0 && GET_CODE (target) == MEM && GET_MODE (target) == BLKmode
      && RTX_UNCHANGING_P (target) && DECL_RTL_SET_P (DECL_RESULT (fndecl))
      && GET_CODE (DECL_RTL (DECL_RESULT (fndecl))) == MEM)
    return (rtx) (size_t) -1;

  /* Extra arguments are valid, but will be ignored below, so we must
     evaluate them here for side-effects.  */
  for (; actual; actual = TREE_CHAIN (actual))
    expand_expr (TREE_VALUE (actual), const0_rtx,
		 TYPE_MODE (TREE_TYPE (TREE_VALUE (actual))), 0);

  /* Expand the function arguments.  Do this first so that any
     new registers get created before we allocate the maps.  */

  arg_vals = xmalloc (nargs * sizeof (rtx));
  arg_trees = xmalloc (nargs * sizeof (tree));

  for (formal = DECL_ARGUMENTS (fndecl), actual = parms, i = 0;
       formal;
       formal = TREE_CHAIN (formal), actual = TREE_CHAIN (actual), i++)
    {
      /* Actual parameter, converted to the type of the argument within the
	 function.  */
      tree arg = convert (TREE_TYPE (formal), TREE_VALUE (actual));
      /* Mode of the variable used within the function.  */
      enum machine_mode mode = TYPE_MODE (TREE_TYPE (formal));
      int invisiref = 0;

      arg_trees[i] = arg;
      loc = RTVEC_ELT (arg_vector, i);

      /* If this is an object passed by invisible reference, we copy the
	 object into a stack slot and save its address.  If this will go
	 into memory, we do nothing now.  Otherwise, we just expand the
	 argument.  */
      if (GET_CODE (loc) == MEM && GET_CODE (XEXP (loc, 0)) == REG
	  && REGNO (XEXP (loc, 0)) > LAST_VIRTUAL_REGISTER)
	{
	  rtx stack_slot = assign_temp (TREE_TYPE (arg), 1, 1, 1);

	  store_expr (arg, stack_slot, 0);
	  arg_vals[i] = XEXP (stack_slot, 0);
	  invisiref = 1;
	}
      else if (GET_CODE (loc) != MEM)
	{
	  if (GET_MODE (loc) != TYPE_MODE (TREE_TYPE (arg)))
	    {
	      int unsignedp = TREE_UNSIGNED (TREE_TYPE (formal));
	      enum machine_mode pmode = TYPE_MODE (TREE_TYPE (formal));

	      pmode = promote_mode (TREE_TYPE (formal), pmode,
				    &unsignedp, 0);

	      if (GET_MODE (loc) != pmode)
		abort ();

	      /* The mode if LOC and ARG can differ if LOC was a variable
		 that had its mode promoted.  */
	      arg_vals[i] = convert_modes (pmode,
					   TYPE_MODE (TREE_TYPE (arg)),
					   expand_expr (arg, NULL_RTX, mode,
							EXPAND_SUM),
					   unsignedp);
	    }
	  else
	    arg_vals[i] = expand_expr (arg, NULL_RTX, mode, EXPAND_SUM);
	}
      else
	arg_vals[i] = 0;

      /* If the formal type was const but the actual was not, we might
	 end up here with an rtx wrongly tagged unchanging in the caller's
	 context.  Fix that.  */
      if (arg_vals[i] != 0
	  && (GET_CODE (arg_vals[i]) == REG || GET_CODE (arg_vals[i]) == MEM)
	  && ! TREE_READONLY (TREE_VALUE (actual)))
	RTX_UNCHANGING_P (arg_vals[i]) = 0;

      if (arg_vals[i] != 0
	  && (! TREE_READONLY (formal)
	      /* If the parameter is not read-only, copy our argument through
		 a register.  Also, we cannot use ARG_VALS[I] if it overlaps
		 TARGET in any way.  In the inline function, they will likely
		 be two different pseudos, and `safe_from_p' will make all
		 sorts of smart assumptions about their not conflicting.
		 But if ARG_VALS[I] overlaps TARGET, these assumptions are
		 wrong, so put ARG_VALS[I] into a fresh register.
		 Don't worry about invisible references, since their stack
		 temps will never overlap the target.  */
	      || (target != 0
		  && ! invisiref
		  && (GET_CODE (arg_vals[i]) == REG
		      || GET_CODE (arg_vals[i]) == SUBREG
		      || GET_CODE (arg_vals[i]) == MEM)
		  && reg_overlap_mentioned_p (arg_vals[i], target))
	      /* ??? We must always copy a SUBREG into a REG, because it might
		 get substituted into an address, and not all ports correctly
		 handle SUBREGs in addresses.  */
	      || (GET_CODE (arg_vals[i]) == SUBREG)))
	arg_vals[i] = copy_to_mode_reg (GET_MODE (loc), arg_vals[i]);

      if (arg_vals[i] != 0 && GET_CODE (arg_vals[i]) == REG
	  && POINTER_TYPE_P (TREE_TYPE (formal)))
	mark_reg_pointer (arg_vals[i],
			  TYPE_ALIGN (TREE_TYPE (TREE_TYPE (formal))));
    }

  /* Allocate the structures we use to remap things.  */

  map = xcalloc (1, sizeof (struct inline_remap));
  map->fndecl = fndecl;

  VARRAY_TREE_INIT (map->block_map, 10, "block_map");
  map->reg_map = xcalloc (max_regno, sizeof (rtx));

  /* We used to use alloca here, but the size of what it would try to
     allocate would occasionally cause it to exceed the stack limit and
     cause unpredictable core dumps.  */
  real_label_map = xmalloc ((max_labelno) * sizeof (rtx));
  map->label_map = real_label_map;
  map->local_return_label = NULL_RTX;

  inl_max_uid = (inl_f->emit->x_cur_insn_uid + 1);
  map->insn_map = xcalloc (inl_max_uid, sizeof (rtx));
  map->min_insnno = 0;
  map->max_insnno = inl_max_uid;

  map->integrating = 1;
  map->compare_src = NULL_RTX;
  map->compare_mode = VOIDmode;

  /* const_equiv_varray maps pseudos in our routine to constants, so
     it needs to be large enough for all our pseudos.  This is the
     number we are currently using plus the number in the called
     routine, plus 15 for each arg, five to compute the virtual frame
     pointer, and five for the return value.  This should be enough
     for most cases.  We do not reference entries outside the range of
     the map.

     ??? These numbers are quite arbitrary and were obtained by
     experimentation.  At some point, we should try to allocate the
     table after all the parameters are set up so we can more accurately
     estimate the number of pseudos we will need.  */

  VARRAY_CONST_EQUIV_INIT (map->const_equiv_varray,
			   (max_reg_num ()
			    + (max_regno - FIRST_PSEUDO_REGISTER)
			    + 15 * nargs
			    + 10),
			   "expand_inline_function");
  map->const_age = 0;

  /* Record the current insn in case we have to set up pointers to frame
     and argument memory blocks.  If there are no insns yet, add a dummy
     insn that can be used as an insertion point.  */
  map->insns_at_start = get_last_insn ();
  if (map->insns_at_start == 0)
    map->insns_at_start = emit_note (NOTE_INSN_DELETED);

  map->regno_pointer_align = inl_f->emit->regno_pointer_align;
  map->x_regno_reg_rtx = inl_f->emit->x_regno_reg_rtx;

  /* Update the outgoing argument size to allow for those in the inlined
     function.  */
  if (inl_f->outgoing_args_size > current_function_outgoing_args_size)
    current_function_outgoing_args_size = inl_f->outgoing_args_size;

  /* If the inline function needs to make PIC references, that means
     that this function's PIC offset table must be used.  */
  if (inl_f->uses_pic_offset_table)
    current_function_uses_pic_offset_table = 1;

  /* If this function needs a context, set it up.  */
  if (inl_f->needs_context)
    static_chain_value = lookup_static_chain (fndecl);

  /* If the inlined function calls __builtin_constant_p, then we'll
     need to call purge_builtin_constant_p on this function.  */
  if (inl_f->calls_constant_p)
    current_function_calls_constant_p = 1;

  if (GET_CODE (parm_insns) == NOTE
      && NOTE_LINE_NUMBER (parm_insns) > 0)
    {
      rtx note = emit_note_copy (parm_insns);

      if (note)
	RTX_INTEGRATED_P (note) = 1;
    }

  /* Process each argument.  For each, set up things so that the function's
     reference to the argument will refer to the argument being passed.
     We only replace REG with REG here.  Any simplifications are done
     via const_equiv_map.

     We make two passes:  In the first, we deal with parameters that will
     be placed into registers, since we need to ensure that the allocated
     register number fits in const_equiv_map.  Then we store all non-register
     parameters into their memory location.  */

  /* Don't try to free temp stack slots here, because we may put one of the
     parameters into a temp stack slot.  */

  for (i = 0; i < nargs; i++)
    {
      rtx copy = arg_vals[i];

      loc = RTVEC_ELT (arg_vector, i);

      /* There are three cases, each handled separately.  */
      if (GET_CODE (loc) == MEM && GET_CODE (XEXP (loc, 0)) == REG
	  && REGNO (XEXP (loc, 0)) > LAST_VIRTUAL_REGISTER)
	{
	  /* This must be an object passed by invisible reference (it could
	     also be a variable-sized object, but we forbid inlining functions
	     with variable-sized arguments).  COPY is the address of the
	     actual value (this computation will cause it to be copied).  We
	     map that address for the register, noting the actual address as
	     an equivalent in case it can be substituted into the insns.  */

	  if (GET_CODE (copy) != REG)
	    {
	      temp = copy_addr_to_reg (copy);
	      if (CONSTANT_P (copy) || FIXED_BASE_PLUS_P (copy))
		SET_CONST_EQUIV_DATA (map, temp, copy, CONST_AGE_PARM);
	      copy = temp;
	    }
	  map->reg_map[REGNO (XEXP (loc, 0))] = copy;
	}
      else if (GET_CODE (loc) == MEM)
	{
	  /* This is the case of a parameter that lives in memory.  It
	     will live in the block we allocate in the called routine's
	     frame that simulates the incoming argument area.  Do nothing
	     with the parameter now; we will call store_expr later.  In
	     this case, however, we must ensure that the virtual stack and
	     incoming arg rtx values are expanded now so that we can be
	     sure we have enough slots in the const equiv map since the
	     store_expr call can easily blow the size estimate.  */
	  if (DECL_SAVED_INSNS (fndecl)->args_size != 0)
	    copy_rtx_and_substitute (virtual_incoming_args_rtx, map, 0);
	}
      else if (GET_CODE (loc) == REG)
	process_reg_param (map, loc, copy);
      else if (GET_CODE (loc) == CONCAT)
	{
	  rtx locreal = gen_realpart (GET_MODE (XEXP (loc, 0)), loc);
	  rtx locimag = gen_imagpart (GET_MODE (XEXP (loc, 0)), loc);
	  rtx copyreal = gen_realpart (GET_MODE (locreal), copy);
	  rtx copyimag = gen_imagpart (GET_MODE (locimag), copy);

	  process_reg_param (map, locreal, copyreal);
	  process_reg_param (map, locimag, copyimag);
	}
      else
	abort ();
    }

  /* Tell copy_rtx_and_substitute to handle constant pool SYMBOL_REFs
     specially.  This function can be called recursively, so we need to
     save the previous value.  */
  inlining_previous = inlining;
  inlining = inl_f;

  /* Now do the parameters that will be placed in memory.  */

  for (formal = DECL_ARGUMENTS (fndecl), i = 0;
       formal; formal = TREE_CHAIN (formal), i++)
    {
      loc = RTVEC_ELT (arg_vector, i);

      if (GET_CODE (loc) == MEM
	  /* Exclude case handled above.  */
	  && ! (GET_CODE (XEXP (loc, 0)) == REG
		&& REGNO (XEXP (loc, 0)) > LAST_VIRTUAL_REGISTER))
	{
	  rtx note = emit_line_note (DECL_SOURCE_LOCATION (formal));

	  if (note)
	    RTX_INTEGRATED_P (note) = 1;

	  /* Compute the address in the area we reserved and store the
	     value there.  */
	  temp = copy_rtx_and_substitute (loc, map, 1);
	  subst_constants (&temp, NULL_RTX, map, 1);
	  apply_change_group ();
	  if (! memory_address_p (GET_MODE (temp), XEXP (temp, 0)))
	    temp = change_address (temp, VOIDmode, XEXP (temp, 0));
	  store_expr (arg_trees[i], temp, 0);
	}
    }

  /* Deal with the places that the function puts its result.
     We are driven by what is placed into DECL_RESULT.

     Initially, we assume that we don't have anything special handling for
     REG_FUNCTION_RETURN_VALUE_P.  */

  map->inline_target = 0;
  loc = (DECL_RTL_SET_P (DECL_RESULT (fndecl))
	 ? DECL_RTL (DECL_RESULT (fndecl)) : NULL_RTX);

  if (TYPE_MODE (type) == VOIDmode)
    /* There is no return value to worry about.  */
    ;
  else if (GET_CODE (loc) == MEM)
    {
      if (GET_CODE (XEXP (loc, 0)) == ADDRESSOF)
	{
	  temp = copy_rtx_and_substitute (loc, map, 1);
	  subst_constants (&temp, NULL_RTX, map, 1);
	  apply_change_group ();
	  target = temp;
	}
      else
	{
	  if (! structure_value_addr
	      || ! aggregate_value_p (DECL_RESULT (fndecl), fndecl))
	    abort ();

	  /* Pass the function the address in which to return a structure
	     value.  Note that a constructor can cause someone to call us
	     with STRUCTURE_VALUE_ADDR, but the initialization takes place
	     via the first parameter, rather than the struct return address.

	     We have two cases: If the address is a simple register
	     indirect, use the mapping mechanism to point that register to
	     our structure return address.  Otherwise, store the structure
	     return value into the place that it will be referenced from.  */

	  if (GET_CODE (XEXP (loc, 0)) == REG)
	    {
	      temp = force_operand (structure_value_addr, NULL_RTX);
	      temp = force_reg (Pmode, temp);
	      /* A virtual register might be invalid in an insn, because
		 it can cause trouble in reload.  Since we don't have access
		 to the expanders at map translation time, make sure we have
		 a proper register now.
		 If a virtual register is actually valid, cse or combine
		 can put it into the mapped insns.  */
	      if (REGNO (temp) >= FIRST_VIRTUAL_REGISTER
		  && REGNO (temp) <= LAST_VIRTUAL_REGISTER)
	      temp = copy_to_mode_reg (Pmode, temp);
	      map->reg_map[REGNO (XEXP (loc, 0))] = temp;

	      if (CONSTANT_P (structure_value_addr)
		  || GET_CODE (structure_value_addr) == ADDRESSOF
		  || (GET_CODE (structure_value_addr) == PLUS
		      && (XEXP (structure_value_addr, 0)
			  == virtual_stack_vars_rtx)
		      && (GET_CODE (XEXP (structure_value_addr, 1))
			  == CONST_INT)))
		{
		  SET_CONST_EQUIV_DATA (map, temp, structure_value_addr,
					CONST_AGE_PARM);
		}
	    }
	  else
	    {
	      temp = copy_rtx_and_substitute (loc, map, 1);
	      subst_constants (&temp, NULL_RTX, map, 0);
	      apply_change_group ();
	      emit_move_insn (temp, structure_value_addr);
	    }
	}
    }
  else if (ignore)
    /* We will ignore the result value, so don't look at its structure.
       Note that preparations for an aggregate return value
       do need to be made (above) even if it will be ignored.  */
    ;
  else if (GET_CODE (loc) == REG)
    {
      /* The function returns an object in a register and we use the return
	 value.  Set up our target for remapping.  */

      /* Machine mode function was declared to return.  */
      enum machine_mode departing_mode = TYPE_MODE (type);
      /* (Possibly wider) machine mode it actually computes
	 (for the sake of callers that fail to declare it right).
	 We have to use the mode of the result's RTL, rather than
	 its type, since expand_function_start may have promoted it.  */
      enum machine_mode arriving_mode
	= GET_MODE (DECL_RTL (DECL_RESULT (fndecl)));
      rtx reg_to_map;

      /* Don't use MEMs as direct targets because on some machines
	 substituting a MEM for a REG makes invalid insns.
	 Let the combiner substitute the MEM if that is valid.  */
      if (target == 0 || GET_CODE (target) != REG
	  || GET_MODE (target) != departing_mode)
	{
	  /* Don't make BLKmode registers.  If this looks like
	     a BLKmode object being returned in a register, get
	     the mode from that, otherwise abort.  */
	  if (departing_mode == BLKmode)
	    {
	      if (REG == GET_CODE (DECL_RTL (DECL_RESULT (fndecl))))
		{
		  departing_mode = GET_MODE (DECL_RTL (DECL_RESULT (fndecl)));
		  arriving_mode = departing_mode;
		}
	      else
		abort ();
	    }

	  target = gen_reg_rtx (departing_mode);
	}

      /* If function's value was promoted before return,
	 avoid machine mode mismatch when we substitute INLINE_TARGET.
	 But TARGET is what we will return to the caller.  */
      if (arriving_mode != departing_mode)
	{
	  /* Avoid creating a paradoxical subreg wider than
	     BITS_PER_WORD, since that is illegal.  */
	  if (GET_MODE_BITSIZE (arriving_mode) > BITS_PER_WORD)
	    {
	      if (!TRULY_NOOP_TRUNCATION (GET_MODE_BITSIZE (departing_mode),
					  GET_MODE_BITSIZE (arriving_mode)))
		/* Maybe could be handled by using convert_move () ?  */
		abort ();
	      reg_to_map = gen_reg_rtx (arriving_mode);
	      target = gen_lowpart (departing_mode, reg_to_map);
	    }
	  else
	    reg_to_map = gen_rtx_SUBREG (arriving_mode, target, 0);
	}
      else
	reg_to_map = target;

      /* Usually, the result value is the machine's return register.
	 Sometimes it may be a pseudo. Handle both cases.  */
      if (REG_FUNCTION_VALUE_P (loc))
	map->inline_target = reg_to_map;
      else
	map->reg_map[REGNO (loc)] = reg_to_map;
    }
  else if (GET_CODE (loc) == CONCAT)
    {
      enum machine_mode departing_mode = TYPE_MODE (type);
      enum machine_mode arriving_mode
	= GET_MODE (DECL_RTL (DECL_RESULT (fndecl)));

      if (departing_mode != arriving_mode)
	abort ();
      if (GET_CODE (XEXP (loc, 0)) != REG
	  || GET_CODE (XEXP (loc, 1)) != REG)
	abort ();

      /* Don't use MEMs as direct targets because on some machines
	 substituting a MEM for a REG makes invalid insns.
	 Let the combiner substitute the MEM if that is valid.  */
      if (target == 0 || GET_CODE (target) != REG
	  || GET_MODE (target) != departing_mode)
	target = gen_reg_rtx (departing_mode);

      if (GET_CODE (target) != CONCAT)
	abort ();

      map->reg_map[REGNO (XEXP (loc, 0))] = XEXP (target, 0);
      map->reg_map[REGNO (XEXP (loc, 1))] = XEXP (target, 1);
    }
  else
    abort ();

  /* Remap the exception handler data pointer from one to the other.  */
  temp = get_exception_pointer (inl_f);
  if (temp)
    map->reg_map[REGNO (temp)] = get_exception_pointer (cfun);

  /* Initialize label_map.  get_label_from_map will actually make
     the labels.  */
  memset (&map->label_map[min_labelno], 0,
	  (max_labelno - min_labelno) * sizeof (rtx));

  /* Make copies of the decls of the symbols in the inline function, so that
     the copies of the variables get declared in the current function.  Set
     up things so that lookup_static_chain knows that to interpret registers
     in SAVE_EXPRs for TYPE_SIZEs as local.  */
  inline_function_decl = fndecl;
  integrate_parm_decls (DECL_ARGUMENTS (fndecl), map, arg_vector);
  block = integrate_decl_tree (inl_f->original_decl_initial, map);
  BLOCK_ABSTRACT_ORIGIN (block) = DECL_ORIGIN (fndecl);
  inline_function_decl = 0;

  /* Make a fresh binding contour that we can easily remove.  Do this after
     expanding our arguments so cleanups are properly scoped.  */
  expand_start_bindings_and_block (0, block);

  /* Sort the block-map so that it will be easy to find remapped
     blocks later.  */
  qsort (&VARRAY_TREE (map->block_map, 0),
	 map->block_map->elements_used,
	 sizeof (tree),
	 compare_blocks);

  /* Perform postincrements before actually calling the function.  */
  emit_queue ();

  /* Clean up stack so that variables might have smaller offsets.  */
  do_pending_stack_adjust ();

  /* Save a copy of the location of const_equiv_varray for
     mark_stores, called via note_stores.  */
  global_const_equiv_varray = map->const_equiv_varray;

  /* If the called function does an alloca, save and restore the
     stack pointer around the call.  This saves stack space, but
     also is required if this inline is being done between two
     pushes.  */
  if (inl_f->calls_alloca)
    emit_stack_save (SAVE_BLOCK, &stack_save, NULL_RTX);

  /* Map pseudos used for initial hard reg values.  */
  setup_initial_hard_reg_value_integration (inl_f, map);

  /* Now copy the insns one by one.  */
  copy_insn_list (insns, map, static_chain_value);

  /* Duplicate the EH regions.  This will create an offset from the
     region numbers in the function we're inlining to the region
     numbers in the calling function.  This must wait until after
     copy_insn_list, as we need the insn map to be complete.  */
  eh_region_offset = duplicate_eh_regions (inl_f, map);

  /* Now copy the REG_NOTES for those insns.  */
  copy_insn_notes (insns, map, eh_region_offset);

  /* If the insn sequence required one, emit the return label.  */
  if (map->local_return_label)
    emit_label (map->local_return_label);

  /* Restore the stack pointer if we saved it above.  */
  if (inl_f->calls_alloca)
    emit_stack_restore (SAVE_BLOCK, stack_save, NULL_RTX);

  if (! cfun->x_whole_function_mode_p)
    /* In statement-at-a-time mode, we just tell the front-end to add
       this block to the list of blocks at this binding level.  We
       can't do it the way it's done for function-at-a-time mode the
       superblocks have not been created yet.  */
    (*lang_hooks.decls.insert_block) (block);
  else
    {
      BLOCK_CHAIN (block)
	= BLOCK_CHAIN (DECL_INITIAL (current_function_decl));
      BLOCK_CHAIN (DECL_INITIAL (current_function_decl)) = block;
    }

  /* End the scope containing the copied formal parameter variables
     and copied LABEL_DECLs.  We pass NULL_TREE for the variables list
     here so that expand_end_bindings will not check for unused
     variables.  That's already been checked for when the inlined
     function was defined.  */
  expand_end_bindings (NULL_TREE, 1, 1);

  /* Must mark the line number note after inlined functions as a repeat, so
     that the test coverage code can avoid counting the call twice.  This
     just tells the code to ignore the immediately following line note, since
     there already exists a copy of this note before the expanded inline call.
     This line number note is still needed for debugging though, so we can't
     delete it.  */
  if (flag_test_coverage)
    emit_note (NOTE_INSN_REPEATED_LINE_NUMBER);

  emit_line_note (input_location);

  /* If the function returns a BLKmode object in a register, copy it
     out of the temp register into a BLKmode memory object.  */
  if (target
      && TYPE_MODE (TREE_TYPE (TREE_TYPE (fndecl))) == BLKmode
      && ! aggregate_value_p (TREE_TYPE (TREE_TYPE (fndecl)), fndecl))
    target = copy_blkmode_from_reg (0, target, TREE_TYPE (TREE_TYPE (fndecl)));

  if (structure_value_addr)
    {
      target = gen_rtx_MEM (TYPE_MODE (type),
			    memory_address (TYPE_MODE (type),
					    structure_value_addr));
      set_mem_attributes (target, type, 1);
    }

  /* Make sure we free the things we explicitly allocated with xmalloc.  */
  if (real_label_map)
    free (real_label_map);
  VARRAY_FREE (map->const_equiv_varray);
  free (map->reg_map);
  free (map->insn_map);
  free (map);
  free (arg_vals);
  free (arg_trees);

  inlining = inlining_previous;

  return target;
}

/* Make copies of each insn in the given list using the mapping
   computed in expand_inline_function. This function may call itself for
   insns containing sequences.

   Copying is done in two passes, first the insns and then their REG_NOTES.

   If static_chain_value is nonzero, it represents the context-pointer
   register for the function.  */

static void
copy_insn_list (rtx insns, struct inline_remap *map, rtx static_chain_value)
{
  int i;
  rtx insn;
  rtx temp;
#ifdef HAVE_cc0
  rtx cc0_insn = 0;
#endif
  rtx static_chain_mem = 0;

  /* Copy the insns one by one.  Do this in two passes, first the insns and
     then their REG_NOTES.  */

  /* This loop is very similar to the loop in copy_loop_body in unroll.c.  */

  for (insn = insns; insn; insn = NEXT_INSN (insn))
    {
      rtx copy, pattern, set;

      map->orig_asm_operands_vector = 0;

      switch (GET_CODE (insn))
	{
	case INSN:
	  pattern = PATTERN (insn);
	  set = single_set (insn);
	  copy = 0;
	  if (GET_CODE (pattern) == USE
	      && GET_CODE (XEXP (pattern, 0)) == REG
	      && REG_FUNCTION_VALUE_P (XEXP (pattern, 0)))
	    /* The (USE (REG n)) at return from the function should
	       be ignored since we are changing (REG n) into
	       inline_target.  */
	    break;

	  /* Ignore setting a function value that we don't want to use.  */
	  if (map->inline_target == 0
	      && set != 0
	      && GET_CODE (SET_DEST (set)) == REG
	      && REG_FUNCTION_VALUE_P (SET_DEST (set)))
	    {
	      if (volatile_refs_p (SET_SRC (set)))
		{
		  rtx new_set;

		  /* If we must not delete the source,
		     load it into a new temporary.  */
		  copy = emit_insn (copy_rtx_and_substitute (pattern, map, 0));

		  new_set = single_set (copy);
		  if (new_set == 0)
		    abort ();

		  SET_DEST (new_set)
		    = gen_reg_rtx (GET_MODE (SET_DEST (new_set)));
		}
	      /* If the source and destination are the same and it
		 has a note on it, keep the insn.  */
	      else if (rtx_equal_p (SET_DEST (set), SET_SRC (set))
		       && REG_NOTES (insn) != 0)
		copy = emit_insn (copy_rtx_and_substitute (pattern, map, 0));
	      else
		break;
	    }

	  /* Similarly if an ignored return value is clobbered.  */
	  else if (map->inline_target == 0
		   && GET_CODE (pattern) == CLOBBER
		   && GET_CODE (XEXP (pattern, 0)) == REG
		   && REG_FUNCTION_VALUE_P (XEXP (pattern, 0)))
	    break;

	  /* Look for the address of the static chain slot. The
             rtx_equal_p comparisons against the
             static_chain_incoming_rtx below may fail if the static
             chain is in memory and the address specified is not
             "legitimate".  This happens on Xtensa where the static
             chain is at a negative offset from argp and where only
             positive offsets are legitimate.  When the RTL is
             generated, the address is "legitimized" by copying it
             into a register, causing the rtx_equal_p comparisons to
             fail.  This workaround looks for code that sets a
             register to the address of the static chain.  Subsequent
             memory references via that register can then be
             identified as static chain references.  We assume that
             the register is only assigned once, and that the static
             chain address is only live in one register at a time.  */

	  else if (static_chain_value != 0
		   && set != 0
		   && GET_CODE (static_chain_incoming_rtx) == MEM
		   && GET_CODE (SET_DEST (set)) == REG
		   && rtx_equal_p (SET_SRC (set),
				   XEXP (static_chain_incoming_rtx, 0)))
	    {
	      static_chain_mem =
		  gen_rtx_MEM (GET_MODE (static_chain_incoming_rtx),
			       SET_DEST (set));

	      /* Emit the instruction in case it is used for something
		 other than setting the static chain; if it's not used,
		 it can always be removed as dead code */
	      copy = emit_insn (copy_rtx_and_substitute (pattern, map, 0));
	    }

	  /* If this is setting the static chain rtx, omit it.  */
	  else if (static_chain_value != 0
		   && set != 0
		   && (rtx_equal_p (SET_DEST (set),
				    static_chain_incoming_rtx)
		       || (static_chain_mem
			   && rtx_equal_p (SET_DEST (set), static_chain_mem))))
	    break;

	  /* If this is setting the static chain pseudo, set it from
	     the value we want to give it instead.  */
	  else if (static_chain_value != 0
		   && set != 0
		   && (rtx_equal_p (SET_SRC (set),
				    static_chain_incoming_rtx)
		       || (static_chain_mem
			   && rtx_equal_p (SET_SRC (set), static_chain_mem))))
	    {
	      rtx newdest = copy_rtx_and_substitute (SET_DEST (set), map, 1);

	      copy = emit_move_insn (newdest, static_chain_value);
	      if (GET_CODE (static_chain_incoming_rtx) != MEM)
		static_chain_value = 0;
	    }

	  /* If this is setting the virtual stack vars register, this must
	     be the code at the handler for a builtin longjmp.  The value
	     saved in the setjmp buffer will be the address of the frame
	     we've made for this inlined instance within our frame.  But we
	     know the offset of that value so we can use it to reconstruct
	     our virtual stack vars register from that value.  If we are
	     copying it from the stack pointer, leave it unchanged.  */
	  else if (set != 0
		   && rtx_equal_p (SET_DEST (set), virtual_stack_vars_rtx))
	    {
	      HOST_WIDE_INT offset;
	      temp = map->reg_map[REGNO (SET_DEST (set))];
	      temp = VARRAY_CONST_EQUIV (map->const_equiv_varray,
					 REGNO (temp)).rtx;

	      if (rtx_equal_p (temp, virtual_stack_vars_rtx))
		offset = 0;
	      else if (GET_CODE (temp) == PLUS
		       && rtx_equal_p (XEXP (temp, 0), virtual_stack_vars_rtx)
		       && GET_CODE (XEXP (temp, 1)) == CONST_INT)
		offset = INTVAL (XEXP (temp, 1));
	      else
		abort ();

	      if (rtx_equal_p (SET_SRC (set), stack_pointer_rtx))
		temp = SET_SRC (set);
	      else
		temp = force_operand (plus_constant (SET_SRC (set),
						     - offset),
				      NULL_RTX);

	      copy = emit_move_insn (virtual_stack_vars_rtx, temp);
	    }

	  else
	    copy = emit_insn (copy_rtx_and_substitute (pattern, map, 0));
	  /* REG_NOTES will be copied later.  */

#ifdef HAVE_cc0
	  /* If this insn is setting CC0, it may need to look at
	     the insn that uses CC0 to see what type of insn it is.
	     In that case, the call to recog via validate_change will
	     fail.  So don't substitute constants here.  Instead,
	     do it when we emit the following insn.

	     For example, see the pyr.md file.  That machine has signed and
	     unsigned compares.  The compare patterns must check the
	     following branch insn to see which what kind of compare to
	     emit.

	     If the previous insn set CC0, substitute constants on it as
	     well.  */
	  if (sets_cc0_p (PATTERN (copy)) != 0)
	    cc0_insn = copy;
	  else
	    {
	      if (cc0_insn)
		try_constants (cc0_insn, map);
	      cc0_insn = 0;
	      try_constants (copy, map);
	    }
#else
	  try_constants (copy, map);
#endif
	  INSN_LOCATOR (copy) = INSN_LOCATOR (insn);
	  break;

	case JUMP_INSN:
	  if (map->integrating && returnjump_p (insn))
	    {
	      if (map->local_return_label == 0)
		map->local_return_label = gen_label_rtx ();
	      pattern = gen_jump (map->local_return_label);
	    }
	  else
	    pattern = copy_rtx_and_substitute (PATTERN (insn), map, 0);

	  copy = emit_jump_insn (pattern);

#ifdef HAVE_cc0
	  if (cc0_insn)
	    try_constants (cc0_insn, map);
	  cc0_insn = 0;
#endif
	  try_constants (copy, map);
	  INSN_LOCATOR (copy) = INSN_LOCATOR (insn);

	  /* If this used to be a conditional jump insn but whose branch
	     direction is now know, we must do something special.  */
	  if (any_condjump_p (insn) && onlyjump_p (insn) && map->last_pc_value)
	    {
#ifdef HAVE_cc0
	      /* If the previous insn set cc0 for us, delete it.  */
	      if (only_sets_cc0_p (PREV_INSN (copy)))
		delete_related_insns (PREV_INSN (copy));
#endif

	      /* If this is now a no-op, delete it.  */
	      if (map->last_pc_value == pc_rtx)
		{
		  delete_related_insns (copy);
		  copy = 0;
		}
	      else
		/* Otherwise, this is unconditional jump so we must put a
		   BARRIER after it.  We could do some dead code elimination
		   here, but jump.c will do it just as well.  */
		emit_barrier ();
	    }
	  break;

	case CALL_INSN:
	  /* If this is a CALL_PLACEHOLDER insn then we need to copy the
	     three attached sequences: normal call, sibling call and tail
	     recursion.  */
	  if (GET_CODE (PATTERN (insn)) == CALL_PLACEHOLDER)
	    {
	      rtx sequence[3];
	      rtx tail_label;

	      for (i = 0; i < 3; i++)
		{
		  rtx seq;

		  sequence[i] = NULL_RTX;
		  seq = XEXP (PATTERN (insn), i);
		  if (seq)
		    {
		      start_sequence ();
		      copy_insn_list (seq, map, static_chain_value);
		      sequence[i] = get_insns ();
		      end_sequence ();
		    }
		}

	      /* Find the new tail recursion label.
	         It will already be substituted into sequence[2].  */
	      tail_label = copy_rtx_and_substitute (XEXP (PATTERN (insn), 3),
						    map, 0);

	      copy = emit_call_insn (gen_rtx_CALL_PLACEHOLDER (VOIDmode,
							       sequence[0],
							       sequence[1],
							       sequence[2],
							       tail_label));
	      break;
	    }

	  pattern = copy_rtx_and_substitute (PATTERN (insn), map, 0);
	  copy = emit_call_insn (pattern);

	  SIBLING_CALL_P (copy) = SIBLING_CALL_P (insn);
	  CONST_OR_PURE_CALL_P (copy) = CONST_OR_PURE_CALL_P (insn);
	  INSN_LOCATOR (copy) = INSN_LOCATOR (insn);

	  /* Because the USAGE information potentially contains objects other
	     than hard registers, we need to copy it.  */

	  CALL_INSN_FUNCTION_USAGE (copy)
	    = copy_rtx_and_substitute (CALL_INSN_FUNCTION_USAGE (insn),
				       map, 0);

#ifdef HAVE_cc0
	  if (cc0_insn)
	    try_constants (cc0_insn, map);
	  cc0_insn = 0;
#endif
	  try_constants (copy, map);

	  /* Be lazy and assume CALL_INSNs clobber all hard registers.  */
	  for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
	    VARRAY_CONST_EQUIV (map->const_equiv_varray, i).rtx = 0;
	  break;

	case CODE_LABEL:
	  copy = emit_label (get_label_from_map (map,
						 CODE_LABEL_NUMBER (insn)));
	  LABEL_NAME (copy) = LABEL_NAME (insn);
	  map->const_age++;
	  break;

	case BARRIER:
	  copy = emit_barrier ();
	  break;

	case NOTE:
	  if (NOTE_LINE_NUMBER (insn) == NOTE_INSN_DELETED_LABEL)
	    {
	      copy = emit_label (get_label_from_map (map,
						    CODE_LABEL_NUMBER (insn)));
	      LABEL_NAME (copy) = NOTE_SOURCE_FILE (insn);
	      map->const_age++;
	      break;
	    }

	  /* NOTE_INSN_FUNCTION_END and NOTE_INSN_FUNCTION_BEG are
	     discarded because it is important to have only one of
	     each in the current function.

	     NOTE_INSN_DELETED notes aren't useful.  */

	  if (NOTE_LINE_NUMBER (insn) != NOTE_INSN_FUNCTION_END
	      && NOTE_LINE_NUMBER (insn) != NOTE_INSN_FUNCTION_BEG
	      && NOTE_LINE_NUMBER (insn) != NOTE_INSN_DELETED)
	    {
	      copy = emit_note_copy (insn);
	      if (!copy)
		/*Copied a line note, but line numbering is off*/;
	      else if ((NOTE_LINE_NUMBER (copy) == NOTE_INSN_BLOCK_BEG
			|| NOTE_LINE_NUMBER (copy) == NOTE_INSN_BLOCK_END)
		       && NOTE_BLOCK (insn))
		{
		  tree *mapped_block_p;

		  mapped_block_p
		    = (tree *) bsearch (NOTE_BLOCK (insn),
					&VARRAY_TREE (map->block_map, 0),
					map->block_map->elements_used,
					sizeof (tree),
					find_block);

		  if (!mapped_block_p)
		    abort ();
		  else
		    NOTE_BLOCK (copy) = *mapped_block_p;
		}
	      else if (NOTE_LINE_NUMBER (copy) == NOTE_INSN_EXPECTED_VALUE)
		NOTE_EXPECTED_VALUE (copy)
		  = copy_rtx_and_substitute (NOTE_EXPECTED_VALUE (insn),
					     map, 0);
	    }
	  else
	    copy = 0;
	  break;

	default:
	  abort ();
	}

      if (copy)
	RTX_INTEGRATED_P (copy) = 1;

      map->insn_map[INSN_UID (insn)] = copy;
    }
}

/* Copy the REG_NOTES.  Increment const_age, so that only constants
   from parameters can be substituted in.  These are the only ones
   that are valid across the entire function.  */

static void
copy_insn_notes (rtx insns, struct inline_remap *map, int eh_region_offset)
{
  rtx insn, new_insn;

  map->const_age++;
  for (insn = insns; insn; insn = NEXT_INSN (insn))
    {
      if (! INSN_P (insn))
	continue;

      new_insn = map->insn_map[INSN_UID (insn)];
      if (! new_insn)
	continue;

      if (REG_NOTES (insn))
        {
	  rtx next, note = copy_rtx_and_substitute (REG_NOTES (insn), map, 0);

	  /* We must also do subst_constants, in case one of our parameters
	     has const type and constant value.  */
	  subst_constants (&note, NULL_RTX, map, 0);
	  apply_change_group ();
	  REG_NOTES (new_insn) = note;

	  /* Delete any REG_LABEL notes from the chain.  Remap any
             REG_EH_REGION notes.  */
	  for (; note; note = next)
	    {
	      next = XEXP (note, 1);
	      if (REG_NOTE_KIND (note) == REG_LABEL)
	        remove_note (new_insn, note);
	      else if (REG_NOTE_KIND (note) == REG_EH_REGION
		       && INTVAL (XEXP (note, 0)) > 0)
	        XEXP (note, 0) = GEN_INT (INTVAL (XEXP (note, 0))
					  + eh_region_offset);
	    }
        }

      if (GET_CODE (insn) == CALL_INSN
	  && GET_CODE (PATTERN (insn)) == CALL_PLACEHOLDER)
	{
	  int i;
	  for (i = 0; i < 3; i++)
	    copy_insn_notes (XEXP (PATTERN (insn), i), map, eh_region_offset);
	}

      if (GET_CODE (insn) == JUMP_INSN
	  && GET_CODE (PATTERN (insn)) == RESX)
	XINT (PATTERN (new_insn), 0) += eh_region_offset;
    }
}

/* Given a chain of PARM_DECLs, ARGS, copy each decl into a VAR_DECL,
   push all of those decls and give each one the corresponding home.  */

static void
integrate_parm_decls (tree args, struct inline_remap *map, rtvec arg_vector)
{
  tree tail;
  int i;

  for (tail = args, i = 0; tail; tail = TREE_CHAIN (tail), i++)
    {
      tree decl = copy_decl_for_inlining (tail, map->fndecl,
					  current_function_decl);
      rtx new_decl_rtl
	= copy_rtx_and_substitute (RTVEC_ELT (arg_vector, i), map, 1);

      /* We really should be setting DECL_INCOMING_RTL to something reasonable
	 here, but that's going to require some more work.  */
      /* DECL_INCOMING_RTL (decl) = ?; */
      /* Fully instantiate the address with the equivalent form so that the
	 debugging information contains the actual register, instead of the
	 virtual register.   Do this by not passing an insn to
	 subst_constants.  */
      subst_constants (&new_decl_rtl, NULL_RTX, map, 1);
      apply_change_group ();
      SET_DECL_RTL (decl, new_decl_rtl);
    }
}

/* Given a BLOCK node LET, push decls and levels so as to construct in the
   current function a tree of contexts isomorphic to the one that is given.

   MAP, if nonzero, is a pointer to an inline_remap map which indicates how
   registers used in the DECL_RTL field should be remapped.  If it is zero,
   no mapping is necessary.  */

static tree
integrate_decl_tree (tree let, struct inline_remap *map)
{
  tree t;
  tree new_block;
  tree *next;

  new_block = make_node (BLOCK);
  VARRAY_PUSH_TREE (map->block_map, new_block);
  next = &BLOCK_VARS (new_block);

  for (t = BLOCK_VARS (let); t; t = TREE_CHAIN (t))
    {
      tree d;

      d = copy_decl_for_inlining (t, map->fndecl, current_function_decl);

      if (DECL_RTL_SET_P (t))
	{
	  rtx r;

	  SET_DECL_RTL (d, copy_rtx_and_substitute (DECL_RTL (t), map, 1));

	  /* Fully instantiate the address with the equivalent form so that the
	     debugging information contains the actual register, instead of the
	     virtual register.   Do this by not passing an insn to
	     subst_constants.  */
	  r = DECL_RTL (d);
	  subst_constants (&r, NULL_RTX, map, 1);
	  SET_DECL_RTL (d, r);

	  apply_change_group ();
	}

      /* Add this declaration to the list of variables in the new
	 block.  */
      *next = d;
      next = &TREE_CHAIN (d);
    }

  next = &BLOCK_SUBBLOCKS (new_block);
  for (t = BLOCK_SUBBLOCKS (let); t; t = BLOCK_CHAIN (t))
    {
      *next = integrate_decl_tree (t, map);
      BLOCK_SUPERCONTEXT (*next) = new_block;
      next = &BLOCK_CHAIN (*next);
    }

  TREE_USED (new_block) = TREE_USED (let);
  BLOCK_ABSTRACT_ORIGIN (new_block) = let;

  return new_block;
}

/* Create a new copy of an rtx. Recursively copies the operands of the rtx,
   except for those few rtx codes that are sharable.

   We always return an rtx that is similar to that incoming rtx, with the
   exception of possibly changing a REG to a SUBREG or vice versa.  No
   rtl is ever emitted.

   If FOR_LHS is nonzero, if means we are processing something that will
   be the LHS of a SET.  In that case, we copy RTX_UNCHANGING_P even if
   inlining since we need to be conservative in how it is set for
   such cases.

   Handle constants that need to be placed in the constant pool by
   calling `force_const_mem'.  */

rtx
copy_rtx_and_substitute (rtx orig, struct inline_remap *map, int for_lhs)
{
  rtx copy, temp;
  int i, j;
  RTX_CODE code;
  enum machine_mode mode;
  const char *format_ptr;
  int regno;

  if (orig == 0)
    return 0;

  code = GET_CODE (orig);
  mode = GET_MODE (orig);

  switch (code)
    {
    case REG:
      /* If the stack pointer register shows up, it must be part of
	 stack-adjustments (*not* because we eliminated the frame pointer!).
	 Small hard registers are returned as-is.  Pseudo-registers
	 go through their `reg_map'.  */
      regno = REGNO (orig);
      if (regno <= LAST_VIRTUAL_REGISTER
	  || (map->integrating
	      && DECL_SAVED_INSNS (map->fndecl)->internal_arg_pointer == orig))
	{
	  /* Some hard registers are also mapped,
	     but others are not translated.  */
	  if (map->reg_map[regno] != 0)
	    return map->reg_map[regno];

	  /* If this is the virtual frame pointer, make space in current
	     function's stack frame for the stack frame of the inline function.

	     Copy the address of this area into a pseudo.  Map
	     virtual_stack_vars_rtx to this pseudo and set up a constant
	     equivalence for it to be the address.  This will substitute the
	     address into insns where it can be substituted and use the new
	     pseudo where it can't.  */
	  else if (regno == VIRTUAL_STACK_VARS_REGNUM)
	    {
	      rtx loc, seq;
	      int size = get_func_frame_size (DECL_SAVED_INSNS (map->fndecl));
#ifdef FRAME_GROWS_DOWNWARD
	      int alignment
		= (DECL_SAVED_INSNS (map->fndecl)->stack_alignment_needed
		   / BITS_PER_UNIT);

	      /* In this case, virtual_stack_vars_rtx points to one byte
		 higher than the top of the frame area.  So make sure we
		 allocate a big enough chunk to keep the frame pointer
		 aligned like a real one.  */
	      if (alignment)
		size = CEIL_ROUND (size, alignment);
#endif
	      start_sequence ();
	      loc = assign_stack_temp (BLKmode, size, 1);
	      loc = XEXP (loc, 0);
#ifdef FRAME_GROWS_DOWNWARD
	      /* In this case, virtual_stack_vars_rtx points to one byte
		 higher than the top of the frame area.  So compute the offset
		 to one byte higher than our substitute frame.  */
	      loc = plus_constant (loc, size);
#endif
	      map->reg_map[regno] = temp
		= force_reg (Pmode, force_operand (loc, NULL_RTX));

#ifdef STACK_BOUNDARY
	      mark_reg_pointer (map->reg_map[regno], STACK_BOUNDARY);
#endif

	      SET_CONST_EQUIV_DATA (map, temp, loc, CONST_AGE_PARM);

	      seq = get_insns ();
	      end_sequence ();
	      emit_insn_after (seq, map->insns_at_start);
	      return temp;
	    }
	  else if (regno == VIRTUAL_INCOMING_ARGS_REGNUM
		   || (map->integrating
		       && (DECL_SAVED_INSNS (map->fndecl)->internal_arg_pointer
			   == orig)))
	    {
	      /* Do the same for a block to contain any arguments referenced
		 in memory.  */
	      rtx loc, seq;
	      int size = DECL_SAVED_INSNS (map->fndecl)->args_size;

	      start_sequence ();
	      loc = assign_stack_temp (BLKmode, size, 1);
	      loc = XEXP (loc, 0);
	      /* When arguments grow downward, the virtual incoming
		 args pointer points to the top of the argument block,
		 so the remapped location better do the same.  */
#ifdef ARGS_GROW_DOWNWARD
	      loc = plus_constant (loc, size);
#endif
	      map->reg_map[regno] = temp
		= force_reg (Pmode, force_operand (loc, NULL_RTX));

#ifdef STACK_BOUNDARY
	      mark_reg_pointer (map->reg_map[regno], STACK_BOUNDARY);
#endif

	      SET_CONST_EQUIV_DATA (map, temp, loc, CONST_AGE_PARM);

	      seq = get_insns ();
	      end_sequence ();
	      emit_insn_after (seq, map->insns_at_start);
	      return temp;
	    }
	  else if (REG_FUNCTION_VALUE_P (orig))
	    {
	      /* This is a reference to the function return value.  If
		 the function doesn't have a return value, error.  If the
		 mode doesn't agree, and it ain't BLKmode, make a SUBREG.  */
	      if (map->inline_target == 0)
		{
		  if (rtx_equal_function_value_matters)
		    /* This is an ignored return value.  We must not
		       leave it in with REG_FUNCTION_VALUE_P set, since
		       that would confuse subsequent inlining of the
		       current function into a later function.  */
		    return gen_rtx_REG (GET_MODE (orig), regno);
		  else
		    /* Must be unrolling loops or replicating code if we
		       reach here, so return the register unchanged.  */
		    return orig;
		}
	      else if (GET_MODE (map->inline_target) != BLKmode
		       && mode != GET_MODE (map->inline_target))
		return gen_lowpart (mode, map->inline_target);
	      else
		return map->inline_target;
	    }
#if defined (LEAF_REGISTERS) && defined (LEAF_REG_REMAP)
	  /* If leaf_renumber_regs_insn() might remap this register to
	     some other number, make sure we don't share it with the
	     inlined function, otherwise delayed optimization of the
	     inlined function may change it in place, breaking our
	     reference to it.  We may still shared it within the
	     function, so create an entry for this register in the
	     reg_map.  */
	  if (map->integrating && regno < FIRST_PSEUDO_REGISTER
	      && LEAF_REGISTERS[regno] && LEAF_REG_REMAP (regno) != regno)
	    {
	      if (!map->leaf_reg_map[regno][mode])
		map->leaf_reg_map[regno][mode] = gen_rtx_REG (mode, regno);
	      return map->leaf_reg_map[regno][mode];
	    }
#endif
	  else
	    return orig;

	  abort ();
	}
      if (map->reg_map[regno] == NULL)
	{
	  map->reg_map[regno] = gen_reg_rtx (mode);
	  REG_USERVAR_P (map->reg_map[regno]) = REG_USERVAR_P (orig);
	  REG_LOOP_TEST_P (map->reg_map[regno]) = REG_LOOP_TEST_P (orig);
	  RTX_UNCHANGING_P (map->reg_map[regno]) = RTX_UNCHANGING_P (orig);
	  /* A reg with REG_FUNCTION_VALUE_P true will never reach here.  */

	  if (REG_POINTER (map->x_regno_reg_rtx[regno]))
	    mark_reg_pointer (map->reg_map[regno],
			      map->regno_pointer_align[regno]);
	}
      return map->reg_map[regno];

    case SUBREG:
      copy = copy_rtx_and_substitute (SUBREG_REG (orig), map, for_lhs);
      return simplify_gen_subreg (GET_MODE (orig), copy,
				  GET_MODE (SUBREG_REG (orig)),
				  SUBREG_BYTE (orig));

    case ADDRESSOF:
      copy = gen_rtx_ADDRESSOF (mode,
				copy_rtx_and_substitute (XEXP (orig, 0),
							 map, for_lhs),
				0, ADDRESSOF_DECL (orig));
      regno = ADDRESSOF_REGNO (orig);
      if (map->reg_map[regno])
	regno = REGNO (map->reg_map[regno]);
      else if (regno > LAST_VIRTUAL_REGISTER)
	{
	  temp = XEXP (orig, 0);
	  map->reg_map[regno] = gen_reg_rtx (GET_MODE (temp));
	  REG_USERVAR_P (map->reg_map[regno]) = REG_USERVAR_P (temp);
	  REG_LOOP_TEST_P (map->reg_map[regno]) = REG_LOOP_TEST_P (temp);
	  RTX_UNCHANGING_P (map->reg_map[regno]) = RTX_UNCHANGING_P (temp);
	  /* A reg with REG_FUNCTION_VALUE_P true will never reach here.  */

	  /* Objects may initially be represented as registers, but
	     but turned into a MEM if their address is taken by
	     put_var_into_stack.  Therefore, the register table may have
	     entries which are MEMs.

	     We briefly tried to clear such entries, but that ended up
	     cascading into many changes due to the optimizers not being
	     prepared for empty entries in the register table.  So we've
	     decided to allow the MEMs in the register table for now.  */
	  if (REG_P (map->x_regno_reg_rtx[regno])
	      && REG_POINTER (map->x_regno_reg_rtx[regno]))
	    mark_reg_pointer (map->reg_map[regno],
			      map->regno_pointer_align[regno]);
	  regno = REGNO (map->reg_map[regno]);
	}
      ADDRESSOF_REGNO (copy) = regno;
      return copy;

    case USE:
    case CLOBBER:
      /* USE and CLOBBER are ordinary, but we convert (use (subreg foo))
	 to (use foo) if the original insn didn't have a subreg.
	 Removing the subreg distorts the VAX movstrhi pattern
	 by changing the mode of an operand.  */
      copy = copy_rtx_and_substitute (XEXP (orig, 0), map, code == CLOBBER);
      if (GET_CODE (copy) == SUBREG && GET_CODE (XEXP (orig, 0)) != SUBREG)
	copy = SUBREG_REG (copy);
      return gen_rtx_fmt_e (code, VOIDmode, copy);

    /* We need to handle "deleted" labels that appear in the DECL_RTL
       of a LABEL_DECL.  */
    case NOTE:
      if (NOTE_LINE_NUMBER (orig) != NOTE_INSN_DELETED_LABEL)
	break;

      /* Fall through.  */
    case CODE_LABEL:
      LABEL_PRESERVE_P (get_label_from_map (map, CODE_LABEL_NUMBER (orig)))
	= LABEL_PRESERVE_P (orig);
      return get_label_from_map (map, CODE_LABEL_NUMBER (orig));

    case LABEL_REF:
      copy
	= gen_rtx_LABEL_REF
	  (mode,
	   LABEL_REF_NONLOCAL_P (orig) ? XEXP (orig, 0)
	   : get_label_from_map (map, CODE_LABEL_NUMBER (XEXP (orig, 0))));

      LABEL_OUTSIDE_LOOP_P (copy) = LABEL_OUTSIDE_LOOP_P (orig);

      /* The fact that this label was previously nonlocal does not mean
	 it still is, so we must check if it is within the range of
	 this function's labels.  */
      LABEL_REF_NONLOCAL_P (copy)
	= (LABEL_REF_NONLOCAL_P (orig)
	   && ! (CODE_LABEL_NUMBER (XEXP (copy, 0)) >= get_first_label_num ()
		 && CODE_LABEL_NUMBER (XEXP (copy, 0)) < max_label_num ()));

      /* If we have made a nonlocal label local, it means that this
	 inlined call will be referring to our nonlocal goto handler.
	 So make sure we create one for this block; we normally would
	 not since this is not otherwise considered a "call".  */
      if (LABEL_REF_NONLOCAL_P (orig) && ! LABEL_REF_NONLOCAL_P (copy))
	function_call_count++;

      return copy;

    case PC:
    case CC0:
    case CONST_INT:
    case CONST_VECTOR:
      return orig;

    case SYMBOL_REF:
      /* Symbols which represent the address of a label stored in the constant
	 pool must be modified to point to a constant pool entry for the
	 remapped label.  Otherwise, symbols are returned unchanged.  */
      if (CONSTANT_POOL_ADDRESS_P (orig))
	{
	  struct function *f = inlining ? inlining : cfun;
	  rtx constant = get_pool_constant_for_function (f, orig);
	  enum machine_mode const_mode = get_pool_mode_for_function (f, orig);
	  if (inlining)
	    {
	      rtx temp = force_const_mem (const_mode,
					  copy_rtx_and_substitute (constant,
								   map, 0));

#if 0
	      /* Legitimizing the address here is incorrect.

		 Since we had a SYMBOL_REF before, we can assume it is valid
		 to have one in this position in the insn.

		 Also, change_address may create new registers.  These
		 registers will not have valid reg_map entries.  This can
		 cause try_constants() to fail because assumes that all
		 registers in the rtx have valid reg_map entries, and it may
		 end up replacing one of these new registers with junk.  */

	      if (! memory_address_p (GET_MODE (temp), XEXP (temp, 0)))
		temp = change_address (temp, GET_MODE (temp), XEXP (temp, 0));
#endif

	      temp = XEXP (temp, 0);
	      temp = convert_memory_address (GET_MODE (orig), temp);
	      return temp;
	    }
	  else if (GET_CODE (constant) == LABEL_REF)
	    return XEXP (force_const_mem
			 (GET_MODE (orig),
			  copy_rtx_and_substitute (constant, map, for_lhs)),
			 0);
	}
      else if (TREE_CONSTANT_POOL_ADDRESS_P (orig) && inlining)
	notice_rtl_inlining_of_deferred_constant ();

      return orig;

    case CONST_DOUBLE:
      /* We have to make a new copy of this CONST_DOUBLE because don't want
	 to use the old value of CONST_DOUBLE_MEM.  Also, this may be a
	 duplicate of a CONST_DOUBLE we have already seen.  */
      if (GET_MODE_CLASS (GET_MODE (orig)) == MODE_FLOAT)
	{
	  REAL_VALUE_TYPE d;

	  REAL_VALUE_FROM_CONST_DOUBLE (d, orig);
	  return CONST_DOUBLE_FROM_REAL_VALUE (d, GET_MODE (orig));
	}
      else
	return immed_double_const (CONST_DOUBLE_LOW (orig),
				   CONST_DOUBLE_HIGH (orig), VOIDmode);

    case CONST:
      /* Make new constant pool entry for a constant
	 that was in the pool of the inline function.  */
      if (RTX_INTEGRATED_P (orig))
	abort ();
      break;

    case ASM_OPERANDS:
      /* If a single asm insn contains multiple output operands then
	 it contains multiple ASM_OPERANDS rtx's that share the input
	 and constraint vecs.  We must make sure that the copied insn
	 continues to share it.  */
      if (map->orig_asm_operands_vector == ASM_OPERANDS_INPUT_VEC (orig))
	{
	  copy = rtx_alloc (ASM_OPERANDS);
	  RTX_FLAG (copy, volatil) = RTX_FLAG (orig, volatil);
	  PUT_MODE (copy, GET_MODE (orig));
	  ASM_OPERANDS_TEMPLATE (copy) = ASM_OPERANDS_TEMPLATE (orig);
	  ASM_OPERANDS_OUTPUT_CONSTRAINT (copy)
	    = ASM_OPERANDS_OUTPUT_CONSTRAINT (orig);
	  ASM_OPERANDS_OUTPUT_IDX (copy) = ASM_OPERANDS_OUTPUT_IDX (orig);
	  ASM_OPERANDS_INPUT_VEC (copy) = map->copy_asm_operands_vector;
	  ASM_OPERANDS_INPUT_CONSTRAINT_VEC (copy)
	    = map->copy_asm_constraints_vector;
	  ASM_OPERANDS_SOURCE_FILE (copy) = ASM_OPERANDS_SOURCE_FILE (orig);
	  ASM_OPERANDS_SOURCE_LINE (copy) = ASM_OPERANDS_SOURCE_LINE (orig);
	  return copy;
	}
      break;

    case CALL:
      /* This is given special treatment because the first
	 operand of a CALL is a (MEM ...) which may get
	 forced into a register for cse.  This is undesirable
	 if function-address cse isn't wanted or if we won't do cse.  */
#ifndef NO_FUNCTION_CSE
      if (! (optimize && ! flag_no_function_cse))
#endif
	{
	  rtx copy
	    = gen_rtx_MEM (GET_MODE (XEXP (orig, 0)),
			   copy_rtx_and_substitute (XEXP (XEXP (orig, 0), 0),
						    map, 0));

	  MEM_COPY_ATTRIBUTES (copy, XEXP (orig, 0));

	  return
	    gen_rtx_CALL (GET_MODE (orig), copy,
			  copy_rtx_and_substitute (XEXP (orig, 1), map, 0));
	}
      break;

#if 0
      /* Must be ifdefed out for loop unrolling to work.  */
    case RETURN:
      abort ();
#endif

    case SET:
      /* If this is setting fp or ap, it means that we have a nonlocal goto.
	 Adjust the setting by the offset of the area we made.
	 If the nonlocal goto is into the current function,
	 this will result in unnecessarily bad code, but should work.  */
      if (SET_DEST (orig) == virtual_stack_vars_rtx
	  || SET_DEST (orig) == virtual_incoming_args_rtx)
	{
	  /* In case a translation hasn't occurred already, make one now.  */
	  rtx equiv_reg;
	  rtx equiv_loc;
	  HOST_WIDE_INT loc_offset;

	  copy_rtx_and_substitute (SET_DEST (orig), map, for_lhs);
	  equiv_reg = map->reg_map[REGNO (SET_DEST (orig))];
	  equiv_loc = VARRAY_CONST_EQUIV (map->const_equiv_varray,
					  REGNO (equiv_reg)).rtx;
	  loc_offset
	    = GET_CODE (equiv_loc) == REG ? 0 : INTVAL (XEXP (equiv_loc, 1));

	  return gen_rtx_SET (VOIDmode, SET_DEST (orig),
			      force_operand
			      (plus_constant
			       (copy_rtx_and_substitute (SET_SRC (orig),
							 map, 0),
				- loc_offset),
			       NULL_RTX));
	}
      else
	return gen_rtx_SET (VOIDmode,
			    copy_rtx_and_substitute (SET_DEST (orig), map, 1),
			    copy_rtx_and_substitute (SET_SRC (orig), map, 0));
      break;

    case MEM:
      if (inlining
	  && GET_CODE (XEXP (orig, 0)) == SYMBOL_REF
	  && CONSTANT_POOL_ADDRESS_P (XEXP (orig, 0)))
	{
	  enum machine_mode const_mode
	    = get_pool_mode_for_function (inlining, XEXP (orig, 0));
	  rtx constant
	    = get_pool_constant_for_function (inlining, XEXP (orig, 0));

	  constant = copy_rtx_and_substitute (constant, map, 0);

	  /* If this was an address of a constant pool entry that itself
	     had to be placed in the constant pool, it might not be a
	     valid address.  So the recursive call might have turned it
	     into a register.  In that case, it isn't a constant any
	     more, so return it.  This has the potential of changing a
	     MEM into a REG, but we'll assume that it safe.  */
	  if (! CONSTANT_P (constant))
	    return constant;

	  return validize_mem (force_const_mem (const_mode, constant));
	}

      copy = gen_rtx_MEM (mode, copy_rtx_and_substitute (XEXP (orig, 0),
							 map, 0));
      MEM_COPY_ATTRIBUTES (copy, orig);

      /* If inlining and this is not for the LHS, turn off RTX_UNCHANGING_P
	 since this may be an indirect reference to a parameter and the
	 actual may not be readonly.  */
      if (inlining && !for_lhs)
	RTX_UNCHANGING_P (copy) = 0;

      /* If inlining, squish aliasing data that references the subroutine's
	 parameter list, since that's no longer applicable.  */
      if (inlining && MEM_EXPR (copy)
	  && TREE_CODE (MEM_EXPR (copy)) == INDIRECT_REF
	  && TREE_CODE (TREE_OPERAND (MEM_EXPR (copy), 0)) == PARM_DECL)
	set_mem_expr (copy, NULL_TREE);

      return copy;

    default:
      break;
    }

  copy = rtx_alloc (code);
  PUT_MODE (copy, mode);
  RTX_FLAG (copy, in_struct) = RTX_FLAG (orig, in_struct);
  RTX_FLAG (copy, volatil) = RTX_FLAG (orig, volatil);
  RTX_FLAG (copy, unchanging) = RTX_FLAG (orig, unchanging);

  format_ptr = GET_RTX_FORMAT (GET_CODE (copy));

  for (i = 0; i < GET_RTX_LENGTH (GET_CODE (copy)); i++)
    {
      switch (*format_ptr++)
	{
	case '0':
	  X0ANY (copy, i) = X0ANY (orig, i);
	  break;

	case 'e':
	  XEXP (copy, i)
	    = copy_rtx_and_substitute (XEXP (orig, i), map, for_lhs);
	  break;

	case 'u':
	  /* Change any references to old-insns to point to the
	     corresponding copied insns.  */
	  XEXP (copy, i) = map->insn_map[INSN_UID (XEXP (orig, i))];
	  break;

	case 'E':
	  XVEC (copy, i) = XVEC (orig, i);
	  if (XVEC (orig, i) != NULL && XVECLEN (orig, i) != 0)
	    {
	      XVEC (copy, i) = rtvec_alloc (XVECLEN (orig, i));
	      for (j = 0; j < XVECLEN (copy, i); j++)
		XVECEXP (copy, i, j)
		  = copy_rtx_and_substitute (XVECEXP (orig, i, j),
					     map, for_lhs);
	    }
	  break;

	case 'w':
	  XWINT (copy, i) = XWINT (orig, i);
	  break;

	case 'i':
	  XINT (copy, i) = XINT (orig, i);
	  break;

	case 's':
	  XSTR (copy, i) = XSTR (orig, i);
	  break;

	case 't':
	  XTREE (copy, i) = XTREE (orig, i);
	  break;

	default:
	  abort ();
	}
    }

  if (code == ASM_OPERANDS && map->orig_asm_operands_vector == 0)
    {
      map->orig_asm_operands_vector = ASM_OPERANDS_INPUT_VEC (orig);
      map->copy_asm_operands_vector = ASM_OPERANDS_INPUT_VEC (copy);
      map->copy_asm_constraints_vector
	= ASM_OPERANDS_INPUT_CONSTRAINT_VEC (copy);
    }

  return copy;
}

/* Substitute known constant values into INSN, if that is valid.  */

void
try_constants (rtx insn, struct inline_remap *map)
{
  int i;

  map->num_sets = 0;

  /* First try just updating addresses, then other things.  This is
     important when we have something like the store of a constant
     into memory and we can update the memory address but the machine
     does not support a constant source.  */
  subst_constants (&PATTERN (insn), insn, map, 1);
  apply_change_group ();
  subst_constants (&PATTERN (insn), insn, map, 0);
  apply_change_group ();

  /* Enforce consistency between the addresses in the regular insn flow
     and the ones in CALL_INSN_FUNCTION_USAGE lists, if any.  */
  if (GET_CODE (insn) == CALL_INSN && CALL_INSN_FUNCTION_USAGE (insn))
    {
      subst_constants (&CALL_INSN_FUNCTION_USAGE (insn), insn, map, 1);
      apply_change_group ();
    }

  /* Show we don't know the value of anything stored or clobbered.  */
  note_stores (PATTERN (insn), mark_stores, NULL);
  map->last_pc_value = 0;
#ifdef HAVE_cc0
  map->last_cc0_value = 0;
#endif

  /* Set up any constant equivalences made in this insn.  */
  for (i = 0; i < map->num_sets; i++)
    {
      if (GET_CODE (map->equiv_sets[i].dest) == REG)
	{
	  int regno = REGNO (map->equiv_sets[i].dest);

	  MAYBE_EXTEND_CONST_EQUIV_VARRAY (map, regno);
	  if (VARRAY_CONST_EQUIV (map->const_equiv_varray, regno).rtx == 0
	      /* Following clause is a hack to make case work where GNU C++
		 reassigns a variable to make cse work right.  */
	      || ! rtx_equal_p (VARRAY_CONST_EQUIV (map->const_equiv_varray,
						    regno).rtx,
				map->equiv_sets[i].equiv))
	    SET_CONST_EQUIV_DATA (map, map->equiv_sets[i].dest,
				  map->equiv_sets[i].equiv, map->const_age);
	}
      else if (map->equiv_sets[i].dest == pc_rtx)
	map->last_pc_value = map->equiv_sets[i].equiv;
#ifdef HAVE_cc0
      else if (map->equiv_sets[i].dest == cc0_rtx)
	map->last_cc0_value = map->equiv_sets[i].equiv;
#endif
    }
}

/* Substitute known constants for pseudo regs in the contents of LOC,
   which are part of INSN.
   If INSN is zero, the substitution should always be done (this is used to
   update DECL_RTL).
   These changes are taken out by try_constants if the result is not valid.

   Note that we are more concerned with determining when the result of a SET
   is a constant, for further propagation, than actually inserting constants
   into insns; cse will do the latter task better.

   This function is also used to adjust address of items previously addressed
   via the virtual stack variable or virtual incoming arguments registers.

   If MEMONLY is nonzero, only make changes inside a MEM.  */

static void
subst_constants (rtx *loc, rtx insn, struct inline_remap *map, int memonly)
{
  rtx x = *loc;
  int i, j;
  enum rtx_code code;
  const char *format_ptr;
  int num_changes = num_validated_changes ();
  rtx new = 0;
  enum machine_mode op0_mode = MAX_MACHINE_MODE;

  code = GET_CODE (x);

  switch (code)
    {
    case PC:
    case CONST_INT:
    case CONST_DOUBLE:
    case CONST_VECTOR:
    case SYMBOL_REF:
    case CONST:
    case LABEL_REF:
    case ADDRESS:
      return;

#ifdef HAVE_cc0
    case CC0:
      if (! memonly)
	validate_change (insn, loc, map->last_cc0_value, 1);
      return;
#endif

    case USE:
    case CLOBBER:
      /* The only thing we can do with a USE or CLOBBER is possibly do
	 some substitutions in a MEM within it.  */
      if (GET_CODE (XEXP (x, 0)) == MEM)
	subst_constants (&XEXP (XEXP (x, 0), 0), insn, map, 0);
      return;

    case REG:
      /* Substitute for parms and known constants.  Don't replace
	 hard regs used as user variables with constants.  */
      if (! memonly)
	{
	  int regno = REGNO (x);
	  struct const_equiv_data *p;

	  if (! (regno < FIRST_PSEUDO_REGISTER && REG_USERVAR_P (x))
	      && (size_t) regno < VARRAY_SIZE (map->const_equiv_varray)
	      && (p = &VARRAY_CONST_EQUIV (map->const_equiv_varray, regno),
		  p->rtx != 0)
	      && p->age >= map->const_age)
	    validate_change (insn, loc, p->rtx, 1);
	}
      return;

    case SUBREG:
      /* SUBREG applied to something other than a reg
	 should be treated as ordinary, since that must
	 be a special hack and we don't know how to treat it specially.
	 Consider for example mulsidi3 in m68k.md.
	 Ordinary SUBREG of a REG needs this special treatment.  */
      if (! memonly && GET_CODE (SUBREG_REG (x)) == REG)
	{
	  rtx inner = SUBREG_REG (x);
	  rtx new = 0;

	  /* We can't call subst_constants on &SUBREG_REG (x) because any
	     constant or SUBREG wouldn't be valid inside our SUBEG.  Instead,
	     see what is inside, try to form the new SUBREG and see if that is
	     valid.  We handle two cases: extracting a full word in an
	     integral mode and extracting the low part.  */
	  subst_constants (&inner, NULL_RTX, map, 0);
	  new = simplify_gen_subreg (GET_MODE (x), inner,
				     GET_MODE (SUBREG_REG (x)),
				     SUBREG_BYTE (x));

	  if (new)
	    validate_change (insn, loc, new, 1);
	  else
	    cancel_changes (num_changes);

	  return;
	}
      break;

    case MEM:
      subst_constants (&XEXP (x, 0), insn, map, 0);

      /* If a memory address got spoiled, change it back.  */
      if (! memonly && insn != 0 && num_validated_changes () != num_changes
	  && ! memory_address_p (GET_MODE (x), XEXP (x, 0)))
	cancel_changes (num_changes);
      return;

    case SET:
      {
	/* Substitute constants in our source, and in any arguments to a
	   complex (e..g, ZERO_EXTRACT) destination, but not in the destination
	   itself.  */
	rtx *dest_loc = &SET_DEST (x);
	rtx dest = *dest_loc;
	rtx src, tem;
	enum machine_mode compare_mode = VOIDmode;

	/* If SET_SRC is a COMPARE which subst_constants would turn into
	   COMPARE of 2 VOIDmode constants, note the mode in which comparison
	   is to be done.  */
	if (GET_CODE (SET_SRC (x)) == COMPARE)
	  {
	    src = SET_SRC (x);
	    if (GET_MODE_CLASS (GET_MODE (src)) == MODE_CC
		|| CC0_P (dest))
	      {
		compare_mode = GET_MODE (XEXP (src, 0));
		if (compare_mode == VOIDmode)
		  compare_mode = GET_MODE (XEXP (src, 1));
	      }
	  }

	subst_constants (&SET_SRC (x), insn, map, memonly);
	src = SET_SRC (x);

	while (GET_CODE (*dest_loc) == ZERO_EXTRACT
	       || GET_CODE (*dest_loc) == SUBREG
	       || GET_CODE (*dest_loc) == STRICT_LOW_PART)
	  {
	    if (GET_CODE (*dest_loc) == ZERO_EXTRACT)
	      {
		subst_constants (&XEXP (*dest_loc, 1), insn, map, memonly);
		subst_constants (&XEXP (*dest_loc, 2), insn, map, memonly);
	      }
	    dest_loc = &XEXP (*dest_loc, 0);
	  }

	/* Do substitute in the address of a destination in memory.  */
	if (GET_CODE (*dest_loc) == MEM)
	  subst_constants (&XEXP (*dest_loc, 0), insn, map, 0);

	/* Check for the case of DEST a SUBREG, both it and the underlying
	   register are less than one word, and the SUBREG has the wider mode.
	   In the case, we are really setting the underlying register to the
	   source converted to the mode of DEST.  So indicate that.  */
	if (GET_CODE (dest) == SUBREG
	    && GET_MODE_SIZE (GET_MODE (dest)) <= UNITS_PER_WORD
	    && GET_MODE_SIZE (GET_MODE (SUBREG_REG (dest))) <= UNITS_PER_WORD
	    && (GET_MODE_SIZE (GET_MODE (SUBREG_REG (dest)))
		      <= GET_MODE_SIZE (GET_MODE (dest)))
	    && (tem = gen_lowpart_if_possible (GET_MODE (SUBREG_REG (dest)),
					       src)))
	  src = tem, dest = SUBREG_REG (dest);

	/* If storing a recognizable value save it for later recording.  */
	if ((map->num_sets < MAX_RECOG_OPERANDS)
	    && (CONSTANT_P (src)
		|| (GET_CODE (src) == REG
		    && (REGNO (src) == VIRTUAL_INCOMING_ARGS_REGNUM
			|| REGNO (src) == VIRTUAL_STACK_VARS_REGNUM))
		|| (GET_CODE (src) == PLUS
		    && GET_CODE (XEXP (src, 0)) == REG
		    && (REGNO (XEXP (src, 0)) == VIRTUAL_INCOMING_ARGS_REGNUM
			|| REGNO (XEXP (src, 0)) == VIRTUAL_STACK_VARS_REGNUM)
		    && CONSTANT_P (XEXP (src, 1)))
		|| GET_CODE (src) == COMPARE
		|| CC0_P (dest)
		|| (dest == pc_rtx
		    && (src == pc_rtx || GET_CODE (src) == RETURN
			|| GET_CODE (src) == LABEL_REF))))
	  {
	    /* Normally, this copy won't do anything.  But, if SRC is a COMPARE
	       it will cause us to save the COMPARE with any constants
	       substituted, which is what we want for later.  */
	    rtx src_copy = copy_rtx (src);
	    map->equiv_sets[map->num_sets].equiv = src_copy;
	    map->equiv_sets[map->num_sets++].dest = dest;
	    if (compare_mode != VOIDmode
		&& GET_CODE (src) == COMPARE
		&& (GET_MODE_CLASS (GET_MODE (src)) == MODE_CC
		    || CC0_P (dest))
		&& GET_MODE (XEXP (src, 0)) == VOIDmode
		&& GET_MODE (XEXP (src, 1)) == VOIDmode)
	      {
		map->compare_src = src_copy;
		map->compare_mode = compare_mode;
	      }
	  }
      }
      return;

    default:
      break;
    }

  format_ptr = GET_RTX_FORMAT (code);

  /* If the first operand is an expression, save its mode for later.  */
  if (*format_ptr == 'e')
    op0_mode = GET_MODE (XEXP (x, 0));

  for (i = 0; i < GET_RTX_LENGTH (code); i++)
    {
      switch (*format_ptr++)
	{
	case '0':
	  break;

	case 'e':
	  if (XEXP (x, i))
	    subst_constants (&XEXP (x, i), insn, map, memonly);
	  break;

	case 'u':
	case 'i':
	case 's':
	case 'w':
	case 'n':
	case 't':
	case 'B':
	  break;

	case 'E':
	  if (XVEC (x, i) != NULL && XVECLEN (x, i) != 0)
	    for (j = 0; j < XVECLEN (x, i); j++)
	      subst_constants (&XVECEXP (x, i, j), insn, map, memonly);

	  break;

	default:
	  abort ();
	}
    }

  /* If this is a commutative operation, move a constant to the second
     operand unless the second operand is already a CONST_INT.  */
  if (! memonly
      && (GET_RTX_CLASS (code) == 'c' || code == NE || code == EQ)
      && CONSTANT_P (XEXP (x, 0)) && GET_CODE (XEXP (x, 1)) != CONST_INT)
    {
      rtx tem = XEXP (x, 0);
      validate_change (insn, &XEXP (x, 0), XEXP (x, 1), 1);
      validate_change (insn, &XEXP (x, 1), tem, 1);
    }

  /* Simplify the expression in case we put in some constants.  */
  if (! memonly)
    switch (GET_RTX_CLASS (code))
      {
      case '1':
	if (op0_mode == MAX_MACHINE_MODE)
	  abort ();
	new = simplify_unary_operation (code, GET_MODE (x),
					XEXP (x, 0), op0_mode);
	break;

      case '<':
	{
	  enum machine_mode op_mode = GET_MODE (XEXP (x, 0));

	  if (op_mode == VOIDmode)
	    op_mode = GET_MODE (XEXP (x, 1));
	  new = simplify_relational_operation (code, op_mode,
					       XEXP (x, 0), XEXP (x, 1));
#ifdef FLOAT_STORE_FLAG_VALUE
	  if (new != 0 && GET_MODE_CLASS (GET_MODE (x)) == MODE_FLOAT)
	    {
	      enum machine_mode mode = GET_MODE (x);
	      if (new == const0_rtx)
		new = CONST0_RTX (mode);
	      else
		{
		  REAL_VALUE_TYPE val;

		  /* Avoid automatic aggregate initialization.  */
		  val = FLOAT_STORE_FLAG_VALUE (mode);
		  new = CONST_DOUBLE_FROM_REAL_VALUE (val, mode);
		}
	    }
#endif
	  break;
	}

      case '2':
      case 'c':
	new = simplify_binary_operation (code, GET_MODE (x),
					 XEXP (x, 0), XEXP (x, 1));
	break;

      case 'b':
      case '3':
	if (op0_mode == MAX_MACHINE_MODE)
	  abort ();

	if (code == IF_THEN_ELSE)
	  {
	    rtx op0 = XEXP (x, 0);

	    if (GET_RTX_CLASS (GET_CODE (op0)) == '<'
		&& GET_MODE (op0) == VOIDmode
		&& ! side_effects_p (op0)
		&& XEXP (op0, 0) == map->compare_src
		&& GET_MODE (XEXP (op0, 1)) == VOIDmode)
	      {
		/* We have compare of two VOIDmode constants for which
		   we recorded the comparison mode.  */
		rtx temp =
		  simplify_relational_operation (GET_CODE (op0),
						 map->compare_mode,
						 XEXP (op0, 0),
						 XEXP (op0, 1));

		if (temp == const0_rtx)
		  new = XEXP (x, 2);
		else if (temp == const1_rtx)
		  new = XEXP (x, 1);
	      }
	  }
	if (!new)
	  new = simplify_ternary_operation (code, GET_MODE (x), op0_mode,
					    XEXP (x, 0), XEXP (x, 1),
					    XEXP (x, 2));
	break;
      }

  if (new)
    validate_change (insn, loc, new, 1);
}

/* Show that register modified no longer contain known constants.  We are
   called from note_stores with parts of the new insn.  */

static void
mark_stores (rtx dest, rtx x ATTRIBUTE_UNUSED, void *data ATTRIBUTE_UNUSED)
{
  int regno = -1;
  enum machine_mode mode = VOIDmode;

  /* DEST is always the innermost thing set, except in the case of
     SUBREGs of hard registers.  */

  if (GET_CODE (dest) == REG)
    regno = REGNO (dest), mode = GET_MODE (dest);
  else if (GET_CODE (dest) == SUBREG && GET_CODE (SUBREG_REG (dest)) == REG)
    {
      regno = REGNO (SUBREG_REG (dest));
      if (regno < FIRST_PSEUDO_REGISTER)
	regno += subreg_regno_offset (REGNO (SUBREG_REG (dest)),
				      GET_MODE (SUBREG_REG (dest)),
				      SUBREG_BYTE (dest),
				      GET_MODE (dest));
      mode = GET_MODE (SUBREG_REG (dest));
    }

  if (regno >= 0)
    {
      unsigned int uregno = regno;
      unsigned int last_reg = (uregno >= FIRST_PSEUDO_REGISTER ? uregno
			       : uregno + HARD_REGNO_NREGS (uregno, mode) - 1);
      unsigned int i;

      /* Ignore virtual stack var or virtual arg register since those
	 are handled separately.  */
      if (uregno != VIRTUAL_INCOMING_ARGS_REGNUM
	  && uregno != VIRTUAL_STACK_VARS_REGNUM)
	for (i = uregno; i <= last_reg; i++)
	  if ((size_t) i < VARRAY_SIZE (global_const_equiv_varray))
	    VARRAY_CONST_EQUIV (global_const_equiv_varray, i).rtx = 0;
    }
}

/* Given a pointer to some BLOCK node, if the BLOCK_ABSTRACT_ORIGIN for the
   given BLOCK node is NULL, set the BLOCK_ABSTRACT_ORIGIN for the node so
   that it points to the node itself, thus indicating that the node is its
   own (abstract) origin.  Additionally, if the BLOCK_ABSTRACT_ORIGIN for
   the given node is NULL, recursively descend the decl/block tree which
   it is the root of, and for each other ..._DECL or BLOCK node contained
   therein whose DECL_ABSTRACT_ORIGINs or BLOCK_ABSTRACT_ORIGINs are also
   still NULL, set *their* DECL_ABSTRACT_ORIGIN or BLOCK_ABSTRACT_ORIGIN
   values to point to themselves.  */

static void
set_block_origin_self (tree stmt)
{
  if (BLOCK_ABSTRACT_ORIGIN (stmt) == NULL_TREE)
    {
      BLOCK_ABSTRACT_ORIGIN (stmt) = stmt;

      {
	tree local_decl;

	for (local_decl = BLOCK_VARS (stmt);
	     local_decl != NULL_TREE;
	     local_decl = TREE_CHAIN (local_decl))
	  set_decl_origin_self (local_decl);	/* Potential recursion.  */
      }

      {
	tree subblock;

	for (subblock = BLOCK_SUBBLOCKS (stmt);
	     subblock != NULL_TREE;
	     subblock = BLOCK_CHAIN (subblock))
	  set_block_origin_self (subblock);	/* Recurse.  */
      }
    }
}

/* Given a pointer to some ..._DECL node, if the DECL_ABSTRACT_ORIGIN for
   the given ..._DECL node is NULL, set the DECL_ABSTRACT_ORIGIN for the
   node to so that it points to the node itself, thus indicating that the
   node represents its own (abstract) origin.  Additionally, if the
   DECL_ABSTRACT_ORIGIN for the given node is NULL, recursively descend
   the decl/block tree of which the given node is the root of, and for
   each other ..._DECL or BLOCK node contained therein whose
   DECL_ABSTRACT_ORIGINs or BLOCK_ABSTRACT_ORIGINs are also still NULL,
   set *their* DECL_ABSTRACT_ORIGIN or BLOCK_ABSTRACT_ORIGIN values to
   point to themselves.  */

void
set_decl_origin_self (tree decl)
{
  if (DECL_ABSTRACT_ORIGIN (decl) == NULL_TREE)
    {
      DECL_ABSTRACT_ORIGIN (decl) = decl;
      if (TREE_CODE (decl) == FUNCTION_DECL)
	{
	  tree arg;

	  for (arg = DECL_ARGUMENTS (decl); arg; arg = TREE_CHAIN (arg))
	    DECL_ABSTRACT_ORIGIN (arg) = arg;
	  if (DECL_INITIAL (decl) != NULL_TREE
	      && DECL_INITIAL (decl) != error_mark_node)
	    set_block_origin_self (DECL_INITIAL (decl));
	}
    }
}

/* Given a pointer to some BLOCK node, and a boolean value to set the
   "abstract" flags to, set that value into the BLOCK_ABSTRACT flag for
   the given block, and for all local decls and all local sub-blocks
   (recursively) which are contained therein.  */

static void
set_block_abstract_flags (tree stmt, int setting)
{
  tree local_decl;
  tree subblock;

  BLOCK_ABSTRACT (stmt) = setting;

  for (local_decl = BLOCK_VARS (stmt);
       local_decl != NULL_TREE;
       local_decl = TREE_CHAIN (local_decl))
    set_decl_abstract_flags (local_decl, setting);

  for (subblock = BLOCK_SUBBLOCKS (stmt);
       subblock != NULL_TREE;
       subblock = BLOCK_CHAIN (subblock))
    set_block_abstract_flags (subblock, setting);
}

/* Given a pointer to some ..._DECL node, and a boolean value to set the
   "abstract" flags to, set that value into the DECL_ABSTRACT flag for the
   given decl, and (in the case where the decl is a FUNCTION_DECL) also
   set the abstract flags for all of the parameters, local vars, local
   blocks and sub-blocks (recursively) to the same setting.  */

void
set_decl_abstract_flags (tree decl, int setting)
{
  DECL_ABSTRACT (decl) = setting;
  if (TREE_CODE (decl) == FUNCTION_DECL)
    {
      tree arg;

      for (arg = DECL_ARGUMENTS (decl); arg; arg = TREE_CHAIN (arg))
	DECL_ABSTRACT (arg) = setting;
      if (DECL_INITIAL (decl) != NULL_TREE
	  && DECL_INITIAL (decl) != error_mark_node)
	set_block_abstract_flags (DECL_INITIAL (decl), setting);
    }
}

/* Output the assembly language code for the function FNDECL
   from its DECL_SAVED_INSNS.  Used for inline functions that are output
   at end of compilation instead of where they came in the source.  */

static GTY(()) struct function *old_cfun;

void
output_inline_function (tree fndecl)
{
  enum debug_info_type old_write_symbols = write_symbols;
  const struct gcc_debug_hooks *const old_debug_hooks = debug_hooks;
  struct function *f = DECL_SAVED_INSNS (fndecl);

  old_cfun = cfun;
  cfun = f;
  current_function_decl = fndecl;

  set_new_last_label_num (f->inl_max_label_num);

  /* We're not deferring this any longer.  */
  DECL_DEFER_OUTPUT (fndecl) = 0;

  /* If requested, suppress debugging information.  */
  if (f->no_debugging_symbols)
    {
      write_symbols = NO_DEBUG;
      debug_hooks = &do_nothing_debug_hooks;
    }

  /* Make sure warnings emitted by the optimizers (e.g. control reaches
     end of non-void function) is not wildly incorrect.  */
  input_location = DECL_SOURCE_LOCATION (fndecl);

  /* Compile this function all the way down to assembly code.  As a
     side effect this destroys the saved RTL representation, but
     that's okay, because we don't need to inline this anymore.  */
  rest_of_compilation (fndecl);
  DECL_INLINE (fndecl) = 0;

  cfun = old_cfun;
  current_function_decl = old_cfun ? old_cfun->decl : 0;
  write_symbols = old_write_symbols;
  debug_hooks = old_debug_hooks;
}


/* Functions to keep track of the values hard regs had at the start of
   the function.  */

rtx
get_hard_reg_initial_reg (struct function *fun, rtx reg)
{
  struct initial_value_struct *ivs = fun->hard_reg_initial_vals;
  int i;

  if (ivs == 0)
    return NULL_RTX;

  for (i = 0; i < ivs->num_entries; i++)
    if (rtx_equal_p (ivs->entries[i].pseudo, reg))
      return ivs->entries[i].hard_reg;

  return NULL_RTX;
}

rtx
has_func_hard_reg_initial_val (struct function *fun, rtx reg)
{
  struct initial_value_struct *ivs = fun->hard_reg_initial_vals;
  int i;

  if (ivs == 0)
    return NULL_RTX;

  for (i = 0; i < ivs->num_entries; i++)
    if (rtx_equal_p (ivs->entries[i].hard_reg, reg))
      return ivs->entries[i].pseudo;

  return NULL_RTX;
}

rtx
get_func_hard_reg_initial_val (struct function *fun, rtx reg)
{
  struct initial_value_struct *ivs = fun->hard_reg_initial_vals;
  rtx rv = has_func_hard_reg_initial_val (fun, reg);

  if (rv)
    return rv;

  if (ivs == 0)
    {
      fun->hard_reg_initial_vals = ggc_alloc (sizeof (initial_value_struct));
      ivs = fun->hard_reg_initial_vals;
      ivs->num_entries = 0;
      ivs->max_entries = 5;
      ivs->entries = ggc_alloc (5 * sizeof (initial_value_pair));
    }

  if (ivs->num_entries >= ivs->max_entries)
    {
      ivs->max_entries += 5;
      ivs->entries = ggc_realloc (ivs->entries,
				  ivs->max_entries
				  * sizeof (initial_value_pair));
    }

  ivs->entries[ivs->num_entries].hard_reg = reg;
  ivs->entries[ivs->num_entries].pseudo = gen_reg_rtx (GET_MODE (reg));

  return ivs->entries[ivs->num_entries++].pseudo;
}

rtx
get_hard_reg_initial_val (enum machine_mode mode, int regno)
{
  return get_func_hard_reg_initial_val (cfun, gen_rtx_REG (mode, regno));
}

rtx
has_hard_reg_initial_val (enum machine_mode mode, int regno)
{
  return has_func_hard_reg_initial_val (cfun, gen_rtx_REG (mode, regno));
}

static void
setup_initial_hard_reg_value_integration (struct function *inl_f, struct inline_remap *remap)
{
  struct initial_value_struct *ivs = inl_f->hard_reg_initial_vals;
  int i;

  if (ivs == 0)
    return;

  for (i = 0; i < ivs->num_entries; i ++)
    remap->reg_map[REGNO (ivs->entries[i].pseudo)]
      = get_func_hard_reg_initial_val (cfun, ivs->entries[i].hard_reg);
}


void
emit_initial_value_sets (void)
{
  struct initial_value_struct *ivs = cfun->hard_reg_initial_vals;
  int i;
  rtx seq;

  if (ivs == 0)
    return;

  start_sequence ();
  for (i = 0; i < ivs->num_entries; i++)
    emit_move_insn (ivs->entries[i].pseudo, ivs->entries[i].hard_reg);
  seq = get_insns ();
  end_sequence ();

  emit_insn_after (seq, get_insns ());
}

/* If the backend knows where to allocate pseudos for hard
   register initial values, register these allocations now.  */
void
allocate_initial_values (rtx *reg_equiv_memory_loc ATTRIBUTE_UNUSED)
{
#ifdef ALLOCATE_INITIAL_VALUE
  struct initial_value_struct *ivs = cfun->hard_reg_initial_vals;
  int i;

  if (ivs == 0)
    return;

  for (i = 0; i < ivs->num_entries; i++)
    {
      int regno = REGNO (ivs->entries[i].pseudo);
      rtx x = ALLOCATE_INITIAL_VALUE (ivs->entries[i].hard_reg);

      if (x == NULL_RTX || REG_N_SETS (REGNO (ivs->entries[i].pseudo)) > 1)
	; /* Do nothing.  */
      else if (GET_CODE (x) == MEM)
	reg_equiv_memory_loc[regno] = x;
      else if (GET_CODE (x) == REG)
	{
	  reg_renumber[regno] = REGNO (x);
	  /* Poke the regno right into regno_reg_rtx
	     so that even fixed regs are accepted.  */
	  REGNO (ivs->entries[i].pseudo) = REGNO (x);
	}
      else abort ();
    }
#endif
}

#include "gt-integrate.h"
