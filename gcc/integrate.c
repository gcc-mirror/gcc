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

static void set_block_origin_self (tree);
static void set_block_abstract_flags (tree, int);

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
      tree type = TREE_TYPE (decl);

      /* For a parameter or result, we must make an equivalent VAR_DECL, not a
	 new PARM_DECL.  */
      copy = build_decl (VAR_DECL, DECL_NAME (decl), type);
      TREE_ADDRESSABLE (copy) = TREE_ADDRESSABLE (decl);
      TREE_READONLY (copy) = TREE_READONLY (decl);
      TREE_THIS_VOLATILE (copy) = TREE_THIS_VOLATILE (decl);
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
	}
    }

  /* Don't generate debug information for the copy if we wouldn't have
     generated it for the copy either.  */
  DECL_ARTIFICIAL (copy) = DECL_ARTIFICIAL (decl);
  DECL_IGNORED_P (copy) = DECL_IGNORED_P (decl);

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
   be the LHS of a SET.

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
	  else
	    return orig;

	  abort ();
	}
      if (map->reg_map[regno] == NULL)
	{
	  map->reg_map[regno] = gen_reg_rtx (mode);
	  REG_USERVAR_P (map->reg_map[regno]) = REG_USERVAR_P (orig);
	  REG_LOOP_TEST_P (map->reg_map[regno]) = REG_LOOP_TEST_P (orig);
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
	 Removing the subreg distorts the VAX movmemhi pattern
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
