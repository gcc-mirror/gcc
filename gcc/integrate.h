/* Function integration definitions for GNU C-Compiler
   Copyright (C) 1990, 1995, 1998, 1999, 2000, 2001 Free Software Foundation, Inc.

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

#include "varray.h"

/* This structure is used to remap objects in the function being inlined to
   those belonging to the calling function.  It is passed by
   expand_inline_function to its children.

   This structure is also used when unrolling loops and otherwise
   replicating code, although not all fields are needed in this case;
   only those fields needed by copy_rtx_and_substitute() and its children
   are used.

   This structure is used instead of static variables because
   expand_inline_function may be called recursively via expand_expr.  */

struct inline_remap
{
  /* True if we are doing function integration, false otherwise.
     Used to control whether RTX_UNCHANGING bits are copied by
     copy_rtx_and_substitute.  */
  int integrating;
  /* Definition of function be inlined.  */
  union tree_node *fndecl;
  /* Place to put insns needed at start of function.  */
  rtx insns_at_start;
  /* Mapping from old BLOCKs to new BLOCKs.  */
  varray_type block_map;
  /* Mapping from old registers to new registers.
     It is allocated and deallocated in `expand_inline_function' */
  rtx *reg_map;
#if defined (LEAF_REGISTERS) && defined (LEAF_REG_REMAP)
  /* Mapping from old leaf registers to new leaf registers.  */
  rtx leaf_reg_map[FIRST_PSEUDO_REGISTER][NUM_MACHINE_MODES];
#endif
  /* Mapping from old code-labels to new code-labels.
     The first element of this map is label_map[min_labelno].  */
  rtx *label_map;
  /* Mapping from old insn uid's to copied insns.  The first element
   of this map is insn_map[min_insnno]; the last element is
   insn_map[max_insnno].  We keep the bounds here for when the map
   only covers a partial range of insns (such as loop unrolling or
   code replication).  */
  rtx *insn_map;
  int min_insnno, max_insnno;

  /* Map pseudo reg number in calling function to equivalent constant.  We
     cannot in general substitute constants into parameter pseudo registers,
     since some machine descriptions (many RISCs) won't always handle
     the resulting insns.  So if an incoming parameter has a constant
     equivalent, we record it here, and if the resulting insn is
     recognizable, we go with it.

     We also use this mechanism to convert references to incoming arguments
     and stacked variables.  copy_rtx_and_substitute will replace the virtual
     incoming argument and virtual stacked variables registers with new
     pseudos that contain pointers into the replacement area allocated for
     this inline instance.  These pseudos are then marked as being equivalent
     to the appropriate address and substituted if valid.  */
  varray_type const_equiv_varray;
  /* This is incremented for each new basic block.
     It is used to store in the age field to record the domain of validity
     of each entry in const_equiv_varray.
     A value of -1 indicates an entry for a reg which is a parm.
     All other values are "positive".  */
#define CONST_AGE_PARM (-1)
  unsigned int const_age;

  /* Target of the inline function being expanded, or NULL if none.  */
  rtx inline_target;
  /* When an insn is being copied by copy_rtx_and_substitute,
     this is nonzero if we have copied an ASM_OPERANDS.
     In that case, it is the original input-operand vector.  */
  rtvec orig_asm_operands_vector;
  /* When an insn is being copied by copy_rtx_and_substitute,
     this is nonzero if we have copied an ASM_OPERANDS.
     In that case, it is the copied input-operand vector.  */
  rtvec copy_asm_operands_vector;
  /* Likewise, this is the copied constraints vector.  */
  rtvec copy_asm_constraints_vector;

  /* Target of a return insn, if needed and inlining.  */
  rtx local_return_label;

  /* Indications for regs being pointers and their alignment.  */
  unsigned char *regno_pointer_align;
  rtx *x_regno_reg_rtx;

  /* The next few fields are used for subst_constants to record the SETs
     that it saw.  */
  int num_sets;
  struct equiv_table
    {
      rtx dest;
      rtx equiv;
    }  equiv_sets[MAX_RECOG_OPERANDS];
  /* Record the last thing assigned to pc.  This is used for folded 
     conditional branch insns.  */
  rtx last_pc_value;
#ifdef HAVE_cc0
  /* Record the last thing assigned to cc0.  */
  rtx last_cc0_value;
#endif
  /* Note mode of COMPARE if the mode would be otherwise lost (comparing of
     two VOIDmode constants.  */
  rtx compare_src;
  enum machine_mode compare_mode;
};

/* Return a copy of an rtx (as needed), substituting pseudo-register,
   labels, and frame-pointer offsets as necessary.  */
extern rtx copy_rtx_and_substitute PARAMS ((rtx, struct inline_remap *, int));

/* Return a pseudo that corresponds to the value in the specified hard
   reg as of the start of the function (for inlined functions, the
   value at the start of the parent function).  */
extern rtx get_hard_reg_initial_val		PARAMS ((enum machine_mode, int));
/* Likewise, but for a different than the current function, or
   arbitrary expression.  */
extern rtx get_func_hard_reg_initial_val	PARAMS ((struct function *, rtx));
/* Likewise, but iff someone else has caused it to become allocated.  */
extern rtx has_func_hard_reg_initial_val	PARAMS ((struct function *, rtx));
/* Likewise, but for common cases.  */
extern rtx has_hard_reg_initial_val		PARAMS ((enum machine_mode, int));
/* If a pseudo represents an initial hard reg (or expression), return
   it, else return NULL_RTX.  */
extern rtx get_hard_reg_initial_reg		PARAMS ((struct function *, rtx));
/* Called from rest_of_compilation.  */
extern void emit_initial_value_sets		PARAMS ((void));
extern void allocate_initial_values		PARAMS ((rtx *));

/* Copy a declaration when one function is substituted inline into
   another.  */
extern union tree_node *copy_decl_for_inlining PARAMS ((union tree_node *,
						      union tree_node *,
						      union tree_node *));

/* Check whether there's any attribute in a function declaration that
   makes the function uninlinable.  Returns false if it finds any,
   true otherwise.  */
extern bool function_attribute_inlinable_p PARAMS ((union tree_node *));

extern void try_constants PARAMS ((rtx, struct inline_remap *));

/* Return the label indicated.  */
extern rtx get_label_from_map PARAMS ((struct inline_remap *, int));

/* Set the label indicated.  */
#define set_label_in_map(MAP, I, X) ((MAP)->label_map[I] = (X))

/* Unfortunately, we need a global copy of const_equiv varray for
   communication with a function called from note_stores.  Be *very*
   careful that this is used properly in the presence of recursion.  */

extern varray_type global_const_equiv_varray;

#define MAYBE_EXTEND_CONST_EQUIV_VARRAY(MAP,MAX)			\
  {									\
    if ((size_t)(MAX) >= VARRAY_SIZE ((MAP)->const_equiv_varray))	\
      {									\
        int is_global = (global_const_equiv_varray			\
			 == (MAP)->const_equiv_varray);			\
        VARRAY_GROW ((MAP)->const_equiv_varray, (MAX)+1);		\
	if (is_global)							\
	   global_const_equiv_varray = (MAP)->const_equiv_varray;	\
      }									\
  }

#define SET_CONST_EQUIV_DATA(MAP,REG,RTX,AGE)				\
  {									\
    struct const_equiv_data *p;						\
    MAYBE_EXTEND_CONST_EQUIV_VARRAY ((MAP), REGNO (REG));		\
    p = &VARRAY_CONST_EQUIV ((MAP)->const_equiv_varray, REGNO (REG));	\
    p->rtx = (RTX);							\
    p->age = (AGE);							\
  }
