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
#include "params.h"
#include "ggc.h"
#include "target.h"
#include "langhooks.h"

/* Round to the next highest integer that meets the alignment.  */
#define CEIL_ROUND(VALUE,ALIGN)	(((VALUE) + (ALIGN) - 1) & ~((ALIGN)- 1))


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

static void subst_constants (rtx *, rtx, struct inline_remap *, int);
static void set_block_origin_self (tree);
static void set_block_abstract_flags (tree, int);
static void mark_stores (rtx, rtx, void *);

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
	      return targetm.function_attribute_inlinable_p (fndecl);
	}
    }

  return true;
}

/* Copy NODE (which must be a DECL).  The DECL originally was in the FROM_FN,
   but now it will be in the TO_FN.  */

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

      /* For a parameter or result, we must make an equivalent VAR_DECL, not a
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
      lang_hooks.dup_lang_specific_decl (copy);

      /* TREE_ADDRESSABLE isn't used to indicate that a label's
	 address has been taken; it's for internal bookkeeping in
	 expand_goto_internal.  */
      if (TREE_CODE (copy) == LABEL_DECL)
	{
	  TREE_ADDRESSABLE (copy) = 0;
	  DECL_TOO_LATE (copy) = 0;
	}
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

/* Unfortunately, we need a global copy of const_equiv map for communication
   with a function called from note_stores.  Be *very* careful that this
   is used properly in the presence of recursion.  */

varray_type global_const_equiv_varray;

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
      if (regno <= LAST_VIRTUAL_REGISTER)
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
	      int size
		= get_func_frame_size (DECL_STRUCT_FUNCTION (map->fndecl));
#ifdef FRAME_GROWS_DOWNWARD
	      int alignment
		= (DECL_STRUCT_FUNCTION (map->fndecl)->stack_alignment_needed
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
	  else if (regno == VIRTUAL_INCOMING_ARGS_REGNUM)
	    {
	      /* Do the same for a block to contain any arguments referenced
		 in memory.  */
	      rtx loc, seq;
	      int size = DECL_STRUCT_FUNCTION (map->fndecl)->args_size;

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
	  struct function *f = cfun;
	  rtx constant = get_pool_constant_for_function (f, orig);
	  if (GET_CODE (constant) == LABEL_REF)
	    return XEXP (force_const_mem
			 (GET_MODE (orig),
			  copy_rtx_and_substitute (constant, map, for_lhs)),
			 0);
	}
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
#ifdef USE_MAPPED_LOCATION
	  ASM_OPERANDS_SOURCE_LOCATION (copy)
	    = ASM_OPERANDS_SOURCE_LOCATION (orig);
#else
	  ASM_OPERANDS_SOURCE_FILE (copy) = ASM_OPERANDS_SOURCE_FILE (orig);
	  ASM_OPERANDS_SOURCE_LINE (copy) = ASM_OPERANDS_SOURCE_LINE (orig);
#endif
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
    /* ??? Is this for the old or the new unroller?  */
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
	    = REG_P (equiv_loc) ? 0 : INTVAL (XEXP (equiv_loc, 1));

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
      copy = gen_rtx_MEM (mode, copy_rtx_and_substitute (XEXP (orig, 0),
							 map, 0));
      MEM_COPY_ATTRIBUTES (copy, orig);
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
      if (REG_P (map->equiv_sets[i].dest))
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
      if (MEM_P (XEXP (x, 0)))
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
      if (! memonly && REG_P (SUBREG_REG (x)))
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
	if (MEM_P (*dest_loc))
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
		|| (REG_P (src)
		    && (REGNO (src) == VIRTUAL_INCOMING_ARGS_REGNUM
			|| REGNO (src) == VIRTUAL_STACK_VARS_REGNUM))
		|| (GET_CODE (src) == PLUS
		    && REG_P (XEXP (src, 0))
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
      && (GET_RTX_CLASS (code) == RTX_COMM_ARITH
	  || GET_RTX_CLASS (code) == RTX_COMM_COMPARE)
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
      case RTX_UNARY:
	if (op0_mode == MAX_MACHINE_MODE)
	  abort ();
	new = simplify_unary_operation (code, GET_MODE (x),
					XEXP (x, 0), op0_mode);
	break;

      case RTX_COMPARE:
      case RTX_COMM_COMPARE:
	{
	  enum machine_mode op_mode = GET_MODE (XEXP (x, 0));

	  if (op_mode == VOIDmode)
	    op_mode = GET_MODE (XEXP (x, 1));

	  new = simplify_relational_operation (code, GET_MODE (x), op_mode,
					       XEXP (x, 0), XEXP (x, 1));
	  break;
	}

      case RTX_BIN_ARITH:
      case RTX_COMM_ARITH:
	new = simplify_binary_operation (code, GET_MODE (x),
					 XEXP (x, 0), XEXP (x, 1));
	break;

      case RTX_BITFIELD_OPS:
      case RTX_TERNARY:
	if (op0_mode == MAX_MACHINE_MODE)
	  abort ();

	if (code == IF_THEN_ELSE)
	  {
	    rtx op0 = XEXP (x, 0);

	    if (COMPARISON_P (op0)
		&& GET_MODE (op0) == VOIDmode
		&& ! side_effects_p (op0)
		&& XEXP (op0, 0) == map->compare_src
		&& GET_MODE (XEXP (op0, 1)) == VOIDmode)
	      {
		/* We have compare of two VOIDmode constants for which
		   we recorded the comparison mode.  */
		rtx tem =
		  simplify_gen_relational (GET_CODE (op0), GET_MODE (op0),
					   map->compare_mode, XEXP (op0, 0),
					   XEXP (op0, 1));

		if (GET_CODE (tem) != CONST_INT)
		  new = simplify_ternary_operation (code, GET_MODE (x),
				  		    op0_mode, tem, XEXP (x, 1),
						    XEXP (x, 2));
		else if (tem == const0_rtx)
		  new = XEXP (x, 2);
		else
		  new = XEXP (x, 1);
	      }
	  }
	if (!new)
	  new = simplify_ternary_operation (code, GET_MODE (x), op0_mode,
					    XEXP (x, 0), XEXP (x, 1),
					    XEXP (x, 2));
	break;

      default:
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

  if (REG_P (dest))
    regno = REGNO (dest), mode = GET_MODE (dest);
  else if (GET_CODE (dest) == SUBREG && REG_P (SUBREG_REG (dest)))
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
			       : uregno + hard_regno_nregs[uregno][mode] - 1);
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

  emit_insn_after (seq, entry_of_function ());
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
      else if (MEM_P (x))
	reg_equiv_memory_loc[regno] = x;
      else if (REG_P (x))
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
