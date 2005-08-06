/* Output routines for GCC for CRX.
   Copyright (C) 1991, 1993, 1994, 1995, 1996, 1997, 1998, 1999, 2000, 2001,
   2002, 2003, 2004  Free Software Foundation, Inc.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published
   by the Free Software Foundation; either version 2, or (at your
   option) any later version.

   GCC is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
   or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
   License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING.  If not, write to
   the Free Software Foundation, 51 Franklin Street, Fifth Floor,
   Boston, MA 02110-1301, USA.  */

/*****************************************************************************/
/* HEADER INCLUDES							     */
/*****************************************************************************/

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "rtl.h"
#include "tree.h"
#include "tm_p.h"
#include "regs.h"
#include "hard-reg-set.h"
#include "real.h"
#include "insn-config.h"
#include "conditions.h"
#include "output.h"
#include "insn-codes.h"
#include "insn-attr.h"
#include "flags.h"
#include "except.h"
#include "function.h"
#include "recog.h"
#include "expr.h"
#include "optabs.h"
#include "toplev.h"
#include "basic-block.h"
#include "target.h"
#include "target-def.h"

/*****************************************************************************/
/* DEFINITIONS								     */
/*****************************************************************************/

/* Maximum number of register used for passing parameters.  */
#define MAX_REG_FOR_PASSING_ARGS 5

/* Minimum number register used for passing parameters.  */
#define MIN_REG_FOR_PASSING_ARGS 2

/* The maximum count of words supported in the assembly of the architecture in
 * a push/pop instruction.  */
#define MAX_COUNT		8

/* Predicate is true if the current function is a 'noreturn' function, i.e. it
 * is qualified as volatile.  */
#define FUNC_IS_NORETURN_P(decl)  (TREE_THIS_VOLATILE (decl))

/* The following 3 macros are used in crx_legitimate_address_p() */

/* Returns 1 if the scale factor of an index address is valid.  */
#define SCALE_FOR_INDEX_P(X)					\
  (GET_CODE (X) == CONST_INT					\
   && (INTVAL (X) == 1 || INTVAL (X) == 2			\
       || INTVAL(X) == 4 || INTVAL (X) == 8))

/* Nonzero if the rtx X is a signed const int of n bits */
#define RTX_SIGNED_INT_FITS_N_BITS(X,n)			\
  ((GET_CODE(X) == CONST_INT				\
    && SIGNED_INT_FITS_N_BITS(INTVAL(X),n)) ? 1 : 0)

/* Nonzero if the rtx X is an unsigned const int of n bits.  */
#define RTX_UNSIGNED_INT_FITS_N_BITS(X,n)		\
  ((GET_CODE(X) == CONST_INT				\
    && UNSIGNED_INT_FITS_N_BITS(INTVAL(X),n)) ? 1 : 0)


/* Register relative legal displacement */
#define CRX_REGISTER_RELATIVE_DISP_P(X)				\
 (CONSTANT_ADDRESS_P(X)						\
  && (GET_CODE (X) != CONST_INT					\
      || RTX_SIGNED_INT_FITS_N_BITS(X, GET_MODE_BITSIZE (Pmode))))

/*****************************************************************************/
/* STATIC VARIABLES							     */
/*****************************************************************************/

/* Nonzero if the last param processed is passed in a register.  */
static int last_parm_in_reg;

/* Will hold the number of the last register the prologue saves, -1 if no
 * register is saved. */
static int last_reg_to_save;

/* Each object in the array is a register number. Mark 1 for registers that
 * need to be saved.  */
static int save_regs[FIRST_PSEUDO_REGISTER];

/* Number of bytes saved on the stack for non-scratch registers */
static int sum_regs = 0;

/* Number of bytes saved on the stack for local variables. */
static int local_vars_size;

/* The sum of 2 sizes: locals vars and padding byte for saving the registers.
 * Used in expand_prologue() and expand_epilogue().  */
static int size_for_adjusting_sp;

/* In case of a POST_INC or POST_DEC memory reference, we must report the mode
 * of the memory reference from PRINT_OPERAND to PRINT_OPERAND_ADDRESS. */
static enum machine_mode output_memory_reference_mode;

/*****************************************************************************/
/* GLOBAL VARIABLES							     */
/*****************************************************************************/

/* Table of machine attributes.  */
const struct attribute_spec crx_attribute_table[];

/* Test and compare insns use these globals to generate branch insns.  */
rtx crx_compare_op0 = NULL_RTX;
rtx crx_compare_op1 = NULL_RTX;

/*****************************************************************************/
/* TARGETM FUNCTION PROTOTYPES						     */
/*****************************************************************************/

static bool crx_fixed_condition_code_regs (unsigned int *, unsigned int *);
static rtx crx_struct_value_rtx (tree fntype ATTRIBUTE_UNUSED,
				 int incoming ATTRIBUTE_UNUSED);
static bool crx_return_in_memory (tree type, tree fntype ATTRIBUTE_UNUSED);

/*****************************************************************************/
/* STACK LAYOUT AND CALLING CONVENTIONS					     */
/*****************************************************************************/

#undef	TARGET_FIXED_CONDITION_CODE_REGS
#define	TARGET_FIXED_CONDITION_CODE_REGS crx_fixed_condition_code_regs

#undef	TARGET_STRUCT_VALUE_RTX
#define	TARGET_STRUCT_VALUE_RTX		crx_struct_value_rtx

#undef	TARGET_RETURN_IN_MEMORY
#define	TARGET_RETURN_IN_MEMORY		crx_return_in_memory

/*****************************************************************************/
/* TARGET-SPECIFIC USES OF `__attribute__'				     */
/*****************************************************************************/

#undef  TARGET_ATTRIBUTE_TABLE
#define TARGET_ATTRIBUTE_TABLE		crx_attribute_table

const struct attribute_spec crx_attribute_table[] = {
  /* ISRs have special prologue and epilogue requirements. */
  {"interrupt", 0, 0, false, true, true, NULL},
  {NULL, 0, 0, false, false, false, NULL}
};


/* Initialize 'targetm' variable which contains pointers to functions and data
 * relating to the target machine.  */

struct gcc_target targetm = TARGET_INITIALIZER;


/*****************************************************************************/
/* TARGET HOOK IMPLEMENTATIONS						     */
/*****************************************************************************/

/* Return the fixed registers used for condition codes.  */

static bool
crx_fixed_condition_code_regs (unsigned int *p1, unsigned int *p2)
{
    *p1 = CC_REGNUM;
    *p2 = INVALID_REGNUM;
    return true;
}

/* Implements hook TARGET_STRUCT_VALUE_RTX.  */

static rtx
crx_struct_value_rtx (tree fntype ATTRIBUTE_UNUSED,
		      int incoming ATTRIBUTE_UNUSED)
{
  return gen_rtx_REG (Pmode, CRX_STRUCT_VALUE_REGNUM);
}

/* Implements hook TARGET_RETURN_IN_MEMORY.  */

static bool
crx_return_in_memory (tree type, tree fntype ATTRIBUTE_UNUSED)
{
  if (TYPE_MODE (type) == BLKmode)
    {
      HOST_WIDE_INT size = int_size_in_bytes (type);
      return (size == -1 || size > 8);
    }
  else
    return false;
}


/*****************************************************************************/
/* MACRO IMPLEMENTATIONS						     */
/*****************************************************************************/

/* STACK LAYOUT AND CALLING CONVENTIONS ROUTINES */
/* --------------------------------------------- */

/* Return nonzero if the current function being compiled is an interrupt
 * function as specified by the "interrupt" attribute.  */

int
crx_interrupt_function_p (void)
{
  tree attributes;

  attributes = TYPE_ATTRIBUTES (TREE_TYPE (current_function_decl));
  return lookup_attribute ("interrupt", attributes) != NULL_TREE;
}

/* Compute values for the array save_regs and the variable sum_regs.  The index
 * of save_regs is numbers of register, each will get 1 if we need to save it
 * in the current function, 0 if not. sum_regs is the total sum of the
 * registers being saved. */

static void
crx_compute_save_regs (void)
{
  unsigned int regno;

  /* initialize here so in case the function is no-return it will be -1. */
  last_reg_to_save = -1;

  /* No need to save any registers if the function never returns.  */
  if (FUNC_IS_NORETURN_P (current_function_decl))
    return;

  /* Initialize the number of bytes to be saved. */
  sum_regs = 0;

  for (regno = 0; regno < FIRST_PSEUDO_REGISTER; regno++)
    {
      if (fixed_regs[regno])
	{
	  save_regs[regno] = 0;
	  continue;
	}

      /* If this reg is used and not call-used (except RA), save it. */
      if (crx_interrupt_function_p())
	{
	  if (!current_function_is_leaf && call_used_regs[regno])
	    /* this is a volatile reg in a non-leaf interrupt routine - save it
	     * for the sake of its sons.  */
	    save_regs[regno] = 1;

	  else if (regs_ever_live[regno])
	    /* This reg is used - save it.  */
	    save_regs[regno] = 1;
	  else
	    /* This reg is not used, and is not a volatile - don't save. */
      	    save_regs[regno] = 0;
	}
      else
	{
	  /* If this reg is used and not call-used (except RA), save it. */
	  if (regs_ever_live[regno]
	      && (!call_used_regs[regno] || regno == RETURN_ADDRESS_REGNUM))
	    save_regs[regno] = 1;
	  else
	    save_regs[regno] = 0;
	}
    }

  for (regno = 0; regno < FIRST_PSEUDO_REGISTER; regno++)
    if (save_regs[regno] == 1)
      {
	last_reg_to_save = regno;
	sum_regs += UNITS_PER_WORD;
      }
}

/* Compute the size of the local area and the size to be adjusted by the
 * prologue and epilogue. */

static void
crx_compute_frame (void)
{
  /* For aligning the local variables. */
  int stack_alignment = STACK_BOUNDARY / BITS_PER_UNIT;
  int padding_locals;

  /* Padding needed for each element of the frame.  */
  local_vars_size = get_frame_size ();

  /* Align to the stack alignment. */
  padding_locals = local_vars_size % stack_alignment;
  if (padding_locals)
    padding_locals = stack_alignment - padding_locals;

  local_vars_size += padding_locals;

  size_for_adjusting_sp = local_vars_size + (ACCUMULATE_OUTGOING_ARGS ?
				     current_function_outgoing_args_size : 0);
}

/* Implements the macro INITIAL_ELIMINATION_OFFSET, return the OFFSET. */

int
crx_initial_elimination_offset (int from, int to)
{
  /* Compute this since we need to use sum_regs.  */
  crx_compute_save_regs ();

  /* Compute this since we need to use local_vars_size.  */
  crx_compute_frame ();

  if ((from) == FRAME_POINTER_REGNUM && (to) == STACK_POINTER_REGNUM)
    return (ACCUMULATE_OUTGOING_ARGS ?
	    current_function_outgoing_args_size : 0);
  else if ((from) == ARG_POINTER_REGNUM && (to) == FRAME_POINTER_REGNUM)
    return (sum_regs + local_vars_size);
  else if ((from) == ARG_POINTER_REGNUM && (to) == STACK_POINTER_REGNUM)
    return (sum_regs + local_vars_size +
	    (ACCUMULATE_OUTGOING_ARGS ?
	     current_function_outgoing_args_size : 0));
  else
    abort ();
}

/* REGISTER USAGE */
/* -------------- */

/* Return the class number of the smallest class containing reg number REGNO.
 * This could be a conditional expression or could index an array. */

enum reg_class
crx_regno_reg_class (int regno)
{
  if (regno >= 0 && regno < SP_REGNUM)
    return NOSP_REGS;

  if (regno == SP_REGNUM) return GENERAL_REGS;

  if (regno == LO_REGNUM) return LO_REGS;
  if (regno == HI_REGNUM) return HI_REGS;

  return NO_REGS;
}

/* Transfer between HILO_REGS and memory via secondary reloading. */

enum reg_class
crx_secondary_reload_class (enum reg_class class,
			    enum machine_mode mode ATTRIBUTE_UNUSED,
			    rtx x ATTRIBUTE_UNUSED)
{
  if (reg_classes_intersect_p (class, HILO_REGS)
      && true_regnum (x) == -1)
    return GENERAL_REGS;

  return NO_REGS;
}

/* Return 1 if hard register REGNO can hold a value of machine-mode MODE. */

int
crx_hard_regno_mode_ok (int regno, enum machine_mode mode)
{
  /* CC can only hold CCmode values.  */
  if (regno == CC_REGNUM)
    return GET_MODE_CLASS (mode) == MODE_CC;
  if (GET_MODE_CLASS (mode) == MODE_CC)
    return 0;
  /* HILO registers can only hold SImode and DImode */
  if (HILO_REGNO_P (regno))
    return mode == SImode || mode == DImode;
  return 1;
}

/* PASSING FUNCTION ARGUMENTS */
/* -------------------------- */

/* If enough param regs are available for passing the param of type TYPE return
 * the number of registers needed else 0.  */

static int
enough_regs_for_param (CUMULATIVE_ARGS * cum, tree type,
		       enum machine_mode mode)
{
  int type_size;
  int remaining_size;

  if (mode != BLKmode)
    type_size = GET_MODE_BITSIZE (mode);
  else
    type_size = int_size_in_bytes (type) * BITS_PER_UNIT;

  remaining_size =
    BITS_PER_WORD * (MAX_REG_FOR_PASSING_ARGS -
    (MIN_REG_FOR_PASSING_ARGS + cum->ints) + 1);

  /* Any variable which is too big to pass in two registers, will pass on
   * stack. */
  if ((remaining_size >= type_size) && (type_size <= 2 * BITS_PER_WORD))
    return (type_size + BITS_PER_WORD - 1) / BITS_PER_WORD;

  return 0;
}

/* Implements the macro FUNCTION_ARG defined in crx.h.  */

rtx
crx_function_arg (CUMULATIVE_ARGS * cum, enum machine_mode mode, tree type,
	      int named ATTRIBUTE_UNUSED)
{
  last_parm_in_reg = 0;

  /* Function_arg() is called with this type just after all the args have had
   * their registers assigned. The rtx that function_arg returns from this type
   * is supposed to pass to 'gen_call' but currently it is not implemented (see
   * macro GEN_CALL).  */
  if (type == void_type_node)
    return NULL_RTX;

  if (targetm.calls.must_pass_in_stack (mode, type) || (cum->ints < 0))
    return NULL_RTX;

  if (mode == BLKmode)
    {
      /* Enable structures that need padding bytes at the end to pass to a
       * function in registers. */
      if (enough_regs_for_param (cum, type, mode) != 0)
	{
	  last_parm_in_reg = 1;
	  return gen_rtx_REG (mode, MIN_REG_FOR_PASSING_ARGS + cum->ints);
	}
    }

  if (MIN_REG_FOR_PASSING_ARGS + cum->ints > MAX_REG_FOR_PASSING_ARGS)
    return NULL_RTX;
  else
    {
      if (enough_regs_for_param (cum, type, mode) != 0)
	{
	  last_parm_in_reg = 1;
	  return gen_rtx_REG (mode, MIN_REG_FOR_PASSING_ARGS + cum->ints);
	}
    }

  return NULL_RTX;
}

/* Implements the macro INIT_CUMULATIVE_ARGS defined in crx.h.  */

void
crx_init_cumulative_args (CUMULATIVE_ARGS * cum, tree fntype,
		      rtx libfunc ATTRIBUTE_UNUSED)
{
  tree param, next_param;

  cum->ints = 0;

  /* Determine if this function has variable arguments.  This is indicated by
   * the last argument being 'void_type_mode' if there are no variable
   * arguments.  Change here for a different vararg.  */
  for (param = (fntype) ? TYPE_ARG_TYPES (fntype) : 0;
       param != (tree) 0; param = next_param)
    {
      next_param = TREE_CHAIN (param);
      if (next_param == (tree) 0 && TREE_VALUE (param) != void_type_node)
	{
	  cum->ints = -1;
	  return;
	}
    }
}

/* Implements the macro FUNCTION_ARG_ADVANCE defined in crx.h.  */

void
crx_function_arg_advance (CUMULATIVE_ARGS * cum, enum machine_mode mode,
		      tree type, int named ATTRIBUTE_UNUSED)
{
  /* l holds the number of registers required */
  int l = GET_MODE_BITSIZE (mode) / BITS_PER_WORD;

  /* If the parameter isn't passed on a register don't advance cum.  */
  if (!last_parm_in_reg)
    return;

  if (targetm.calls.must_pass_in_stack (mode, type) || (cum->ints < 0))
    return;

  if (mode == SImode || mode == HImode || mode == QImode || mode == DImode)
    {
      if (l <= 1)
	cum->ints += 1;
      else
	cum->ints += l;
    }
  else if (mode == SFmode || mode == DFmode)
    cum->ints += l;
  else if ((mode) == BLKmode)
    {
      if ((l = enough_regs_for_param (cum, type, mode)) != 0)
	cum->ints += l;
    }

}

/* Implements the macro FUNCTION_ARG_REGNO_P defined in crx.h.  Return nonzero
 * if N is a register used for passing parameters.  */

int
crx_function_arg_regno_p (int n)
{
  return (n <= MAX_REG_FOR_PASSING_ARGS && n >= MIN_REG_FOR_PASSING_ARGS);
}

/* ADDRESSING MODES */
/* ---------------- */

/* Implements the macro GO_IF_LEGITIMATE_ADDRESS defined in crx.h.
 * The legitimate addressing modes for the CRX are:
 *
 * Relocations		--> const | symbol_ref | label_ref
 * Absolute address	--> 32 bit absolute
 * Post increment	--> reg + 12 bit disp.
 * Post modify		--> reg + 12 bit disp.
 * Register relative	--> reg | 32 bit disp. + reg | 4 bit + reg
 * Scaled index		--> reg + reg | 22 bit disp. + reg + reg |
 *			    22 disp. + reg + reg + (2 | 4 | 8) */

int
crx_legitimate_address_p (enum machine_mode mode ATTRIBUTE_UNUSED,
			  rtx x, int strict)
{
  /* Absolute address */
  if (RTX_UNSIGNED_INT_FITS_N_BITS(x, GET_MODE_BITSIZE(Pmode)))
    return 1;

  /* Label */
  if (GET_CODE (x) == CONST
      || GET_CODE (x) == SYMBOL_REF
      || GET_CODE (x) == LABEL_REF
      || (GET_CODE (x) == REG && (strict ? STRICT_REG_OK_FOR_BASE_P (x)
				  : NONSTRICT_REG_OK_FOR_BASE_P (x))))
    return 1;

  /* Post increment - The first argument is a register and the second is
   * 12-bit long int. */
  if (GET_CODE (x) == POST_INC || GET_CODE (x) == POST_DEC)
    {
      /* Don't allow modes to be referenced through post autoinc/dec that
       * cannot be loaded/stored with a single instruction */
      if (GET_MODE_SIZE (mode) > UNITS_PER_WORD)
	return 0;

      if((GET_CODE (XEXP (x, 0)) == REG)
	 && (strict ? STRICT_REG_OK_FOR_BASE_P (XEXP (x, 0))
	     : NONSTRICT_REG_OK_FOR_BASE_P (XEXP (x, 0))))
	return 1;
    }

  /* Post modify */
  if (GET_CODE (x) == POST_MODIFY)
    {
      /* Don't allow modes to be referenced through post autoinc/dec that
       * cannot be loaded/stored with a single instruction */
      if (GET_MODE_SIZE (mode) > UNITS_PER_WORD)
	return 0;

      if (!(GET_CODE (XEXP (x, 0)) == REG
	  && (strict ? STRICT_REG_OK_FOR_BASE_P (XEXP (x, 0))
	      : NONSTRICT_REG_OK_FOR_BASE_P (XEXP (x, 0)))
	  && RTX_SIGNED_INT_FITS_N_BITS(XEXP (XEXP (x, 1), 1), 12)))
	return 0;

      if(!(GET_CODE (XEXP (x, 1)) == PLUS || GET_CODE (XEXP (x, 1)) == MINUS))
	return 0;

      if(!rtx_equal_p(XEXP (x, 0), XEXP (XEXP (x, 1), 0)))
	return 0;

      return 1;
    }

  if (GET_CODE (x) == PLUS)
    {
      /* Register relative */
      if (GET_CODE (XEXP (x, 0)) == REG
	  && (strict ? STRICT_REG_OK_FOR_BASE_P (XEXP (x, 0))
	      : NONSTRICT_REG_OK_FOR_BASE_P (XEXP (x, 0)))
	  && (CRX_REGISTER_RELATIVE_DISP_P (XEXP (x, 1))))
	return 1;

      /* Scaled index with factor 1 */
      /* 1a. reg + reg */
      if (GET_CODE (XEXP (x, 0)) == REG
	  && (strict ? STRICT_REG_OK_FOR_BASE_P (XEXP (x, 0))
	      : REG_OK_FOR_INDEX_P (XEXP (x, 0)))
	  && GET_CODE (XEXP (x, 1)) == REG
	  && (strict ? STRICT_REG_OK_FOR_BASE_P (XEXP (x, 1))
	      : REG_OK_FOR_INDEXED_BASE_P (XEXP (x, 1))))
	return 1;

      /* Scaled index with different factor */
      /* 1b. reg * scale + reg */
      if (GET_CODE (XEXP (x, 0)) == MULT
	  && GET_CODE (XEXP( XEXP (x, 0), 0)) == REG
	  && (strict ? STRICT_REG_OK_FOR_INDEX_P (XEXP( XEXP (x, 0), 0))
	      : NONSTRICT_REG_OK_FOR_INDEX_P (XEXP( XEXP (x, 0), 0)))
	  && (SCALE_FOR_INDEX_P (XEXP( XEXP (x, 0), 1)))
	  && GET_CODE (XEXP (x, 1)) == REG
	  && (strict ? STRICT_REG_OK_FOR_BASE_P (XEXP (x, 1))
	      : NONSTRICT_REG_OK_FOR_BASE_P (XEXP (x, 1))))
	return 1;

      if (GET_CODE (XEXP (x, 0)) == PLUS)
	{
	  /* 2. reg + reg + 22 bit disp.  */
	  if (GET_CODE (XEXP (XEXP (x, 0), 0)) == REG
	      && (strict ? STRICT_REG_OK_FOR_BASE_P (XEXP (XEXP (x, 0), 0))
		  : REG_OK_FOR_INDEX_P (XEXP (XEXP (x, 0), 0)))
	      && GET_CODE (XEXP (XEXP (x, 0), 1)) == REG
	      && (strict ? STRICT_REG_OK_FOR_BASE_P (XEXP (XEXP (x, 0), 1))
		  : REG_OK_FOR_INDEXED_BASE_P (XEXP (XEXP (x, 0), 1)))
	      && (RTX_SIGNED_INT_FITS_N_BITS(XEXP (x, 1), 22)))
	    return 1;

	  /* 3. reg * scale + reg + 22 bit disp. */
	  if ((GET_CODE (XEXP (XEXP (x, 0), 0)) == MULT)
	      && (GET_CODE (XEXP (XEXP (XEXP (x, 0), 0), 0)) == REG)
	      && (strict ?
		  STRICT_REG_OK_FOR_BASE_P (XEXP (XEXP (XEXP (x, 0), 0), 0))
		  :
		  REG_OK_FOR_INDEXED_BASE_P (XEXP (XEXP (XEXP (x, 0), 0), 0)))
	      && (SCALE_FOR_INDEX_P (XEXP (XEXP (XEXP (x, 0), 0), 1)))
	      && (GET_CODE (XEXP (XEXP (x, 0), 1)) == REG)
	      && (strict ? STRICT_REG_OK_FOR_BASE_P (XEXP (XEXP (x, 0), 1)) :
		  REG_OK_FOR_INDEX_P (XEXP (XEXP (x, 0), 1)))
	      && (RTX_SIGNED_INT_FITS_N_BITS(XEXP (x, 1), 22)))
	    return 1;
	}
    }

  return 0;
}

/* ROUTINES TO COMPUTE COSTS */
/* ------------------------- */

/* Return the cost of moving data of mode MODE between a register of class
 * CLASS and memory; IN is zero if the value is to be written to memory,
 * nonzero if it is to be read in. This cost is relative to those in
 * REGISTER_MOVE_COST.  */

int
crx_memory_move_cost (enum machine_mode mode,
		  enum reg_class class ATTRIBUTE_UNUSED,
		  int in ATTRIBUTE_UNUSED)
{
  /* One LD or ST takes twice the time of a simple reg-reg move */
  if (reg_classes_intersect_p (class, GENERAL_REGS))
    {
      /* printf("GENERAL_REGS LD/ST = %d\n", 4 * HARD_REGNO_NREGS (0, mode));*/
      return 4 * HARD_REGNO_NREGS (0, mode);
    }	
  else if (reg_classes_intersect_p (class, HILO_REGS))
    {
      /* HILO to memory and vice versa */
      /* printf("HILO_REGS %s = %d\n",in ? "LD" : "ST",
	     (REGISTER_MOVE_COST(mode,
				 in ? GENERAL_REGS : HILO_REGS,
				 in ? HILO_REGS : GENERAL_REGS) + 4)
	* HARD_REGNO_NREGS (0, mode)); */
      return (REGISTER_MOVE_COST(mode,
				 in ? GENERAL_REGS : HILO_REGS,
				 in ? HILO_REGS : GENERAL_REGS) + 4)
	* HARD_REGNO_NREGS (0, mode);
    }
  else /* default (like in i386) */
    {
      /* printf("ANYREGS = 100\n"); */
      return 100;
    }
}

/* INSTRUCTION OUTPUT */
/* ------------------ */

/* Print to FILE addr expression of the form post_inc/dec (in this case
 * post_offset is zero) or post_inc/dec + post_offset */

static void
print_post_operand_address (FILE * file, rtx addr, int post_offset)
{
  int displmnt;

  if (GET_CODE (addr) == POST_MODIFY)
    {
      displmnt = INTVAL(XEXP( XEXP (addr, 1), 1));
      if (GET_CODE (XEXP (addr, 1)) == MINUS) displmnt = (-1) * displmnt;
    }
  else
    {
      displmnt = GET_MODE_SIZE (output_memory_reference_mode);
      /* Make the displacement negative for POST_DEC */
      if (GET_CODE (addr) == POST_DEC) displmnt = (-1) * displmnt;
    }

  if (GET_CODE (XEXP (addr, 0)) != REG)
    abort ();

  displmnt += post_offset;

  fprintf (file, "%d(%s)+", displmnt, reg_names[REGNO (XEXP (addr, 0))]);
}

/* Check if constant rtx contains label_ref.  */

static rtx
const_and_contains_label_ref (rtx x)
{
  if (!x)
    return NULL_RTX;

  if (GET_CODE (x) == LABEL_REF)
    return x;

  /* Check within enclosing const.  */
  if (GET_CODE (x) == CONST)
    x = XEXP (x, 0);

  if ((GET_CODE (x) == PLUS || GET_CODE (x) == MINUS)
      && GET_CODE (XEXP (x, 1)) == CONST_INT
      && GET_CODE (XEXP (x, 0)) == LABEL_REF)
    return XEXP (x, 0);

  return NULL_RTX;
}

/* Check if rtx contains symbol_ref. */

static rtx
const_and_contains_symbol_ref (rtx x)
{
  if (!x)
    return NULL_RTX;

  if (GET_CODE (x) == SYMBOL_REF)
    return x;

  /* Check within enclosing const.  */
  if (GET_CODE (x) == CONST)
    x = XEXP (x, 0);

  if ((GET_CODE (x) == PLUS || GET_CODE (x) == MINUS)
      && GET_CODE (XEXP (x, 1)) == CONST_INT
      && GET_CODE (XEXP (x, 0)) == SYMBOL_REF)
    return XEXP (x, 0);

  return NULL_RTX;
}

/* Check if a const_double is ok for crx store-immediate instructions */

int
crx_const_double_ok (rtx op)
{
  if (GET_MODE (op) == DFmode)
  {
    REAL_VALUE_TYPE r;
    long l[2];
    REAL_VALUE_FROM_CONST_DOUBLE (r, op);
    REAL_VALUE_TO_TARGET_DOUBLE (r, l);
    return (UNSIGNED_INT_FITS_N_BITS(l[0], 4) &&
	    UNSIGNED_INT_FITS_N_BITS(l[1], 4)) ? 1 : 0;
  }

  if (GET_MODE (op) == SFmode)
  {
    REAL_VALUE_TYPE r;
    long l;
    REAL_VALUE_FROM_CONST_DOUBLE (r, op);
    REAL_VALUE_TO_TARGET_SINGLE (r, l);
    return UNSIGNED_INT_FITS_N_BITS(l, 4) ? 1 : 0;
  }

  return (UNSIGNED_INT_FITS_N_BITS (CONST_DOUBLE_LOW (op), 4) &&
	  UNSIGNED_INT_FITS_N_BITS (CONST_DOUBLE_HIGH (op), 4)) ? 1 : 0;
}

/* Implements the macro PRINT_OPERAND defined in crx.h.  */

void
crx_print_operand (FILE * file, rtx x, int code)
{
  switch (code)
    {
    case 'p' :
      if (GET_CODE (x) == REG) {
	if (GET_MODE (x) == DImode || GET_MODE (x) == DFmode)
	  {
	    int regno = REGNO (x);
	    if (regno + 1 >= SP_REGNUM) abort ();
	    fprintf (file, "{%s, %s}", reg_names[regno], reg_names[regno + 1]);
	    return;
	  }
	else
	  {
	    if (REGNO (x) >= SP_REGNUM) abort ();
	    fprintf (file, "%s", reg_names[REGNO (x)]);
	    return;
	  }
      }

    case 'd' :
	{
	  const char *crx_cmp_str;
	  switch (GET_CODE (x))
	    { /* MD: compare(reg, reg or imm) but CRX: cmp(reg or imm, reg)
	       * -> swap all non symmetric ops */
	    case EQ  : crx_cmp_str = "eq"; break;
	    case NE  : crx_cmp_str = "ne"; break;
	    case GT  : crx_cmp_str = "lt"; break;
	    case GTU : crx_cmp_str = "lo"; break;
	    case LT  : crx_cmp_str = "gt"; break;
	    case LTU : crx_cmp_str = "hi"; break;
	    case GE  : crx_cmp_str = "le"; break;
	    case GEU : crx_cmp_str = "ls"; break;
	    case LE  : crx_cmp_str = "ge"; break;
	    case LEU : crx_cmp_str = "hs"; break;
	    default : abort ();
	    }
	  fprintf (file, "%s", crx_cmp_str);
	  return;
	}

    case 'H':
      /* Print high part of a double precision value. */
      switch (GET_CODE (x))
	{
	case CONST_DOUBLE:
	  if (GET_MODE (x) == SFmode) abort ();
	  if (GET_MODE (x) == DFmode)
	    {
	      /* High part of a DF const. */
	      REAL_VALUE_TYPE r;
	      long l[2];

	      REAL_VALUE_FROM_CONST_DOUBLE (r, x);
	      REAL_VALUE_TO_TARGET_DOUBLE (r, l);

	      fprintf (file, "$0x%lx", l[1]);
	      return;
	    }

	  /* -- Fallthrough to handle DI consts -- */

	case CONST_INT:
	    {
	      rtx high, low;
	      split_double (x, &low, &high);
	      putc ('$', file);
	      output_addr_const (file, high);
	      return;
	    }

	case REG:
	  if (REGNO (x) + 1 >= FIRST_PSEUDO_REGISTER) abort ();
	  fprintf (file, "%s", reg_names[REGNO (x) + 1]);
	  return;

	case MEM:
	  /* Adjust memory address to high part.  */
	    {
	      rtx adj_mem = x;
	      adj_mem = adjust_address (adj_mem, GET_MODE (adj_mem), 4);

	      output_memory_reference_mode = GET_MODE (adj_mem);
	      output_address (XEXP (adj_mem, 0));
	      return;
	    }

	default:
	  abort ();
	}

    case 'L':
      /* Print low part of a double precision value. */
      switch (GET_CODE (x))
	{
	case CONST_DOUBLE:
	  if (GET_MODE (x) == SFmode) abort ();
	  if (GET_MODE (x) == DFmode)
	    {
	      /* High part of a DF const. */
	      REAL_VALUE_TYPE r;
	      long l[2];

	      REAL_VALUE_FROM_CONST_DOUBLE (r, x);
	      REAL_VALUE_TO_TARGET_DOUBLE (r, l);

	      fprintf (file, "$0x%lx", l[0]);
	      return;
	    }

	  /* -- Fallthrough to handle DI consts -- */

	case CONST_INT:
	    {
	      rtx high, low;
	      split_double (x, &low, &high);
	      putc ('$', file);
	      output_addr_const (file, low);
	      return;
	    }

	case REG:
	  fprintf (file, "%s", reg_names[REGNO (x)]);
	  return;

	case MEM:
	  output_memory_reference_mode = GET_MODE (x);
	  output_address (XEXP (x, 0));
	  return;

	default:
	  abort ();
	}

    case 0 : /* default */
      switch (GET_CODE(x))
	{
	case REG:
	  fprintf (file, "%s", reg_names[REGNO (x)]);
	  return;

	case MEM:
	  output_memory_reference_mode = GET_MODE (x);
	  output_address (XEXP (x, 0));
	  return;

	case CONST_DOUBLE:
	    {
	      REAL_VALUE_TYPE r;
	      long l;

	      /* Always use H and L for double precision - see above */
	      gcc_assert(GET_MODE (x) == SFmode);

	      REAL_VALUE_FROM_CONST_DOUBLE (r, x);
	      REAL_VALUE_TO_TARGET_SINGLE (r, l);

	      fprintf (file, "$0x%lx", l);
	      return;
	    }

	default:
	  putc ('$', file);
	  output_addr_const (file, x);
	  return;
	}

    default:
      output_operand_lossage ("invalid %%xn code");
    }

  abort ();
}

/* Implements the macro PRINT_OPERAND_ADDRESS defined in crx.h.  */

void
crx_print_operand_address (FILE * file, rtx addr)
{
  rtx breg = 0, ireg = 0;
  rtx offset = 0;
  rtx post_offset = 0;
  rtx scale = 0;
  int mem = 0;

retry:
  switch (GET_CODE (addr))
    {
    case MEM:
      fprintf (file, "0(");
      addr = XEXP (addr, 0);
      mem = 1;
      goto retry;
    case REG:
      fprintf (file, "0(%s)", reg_names[REGNO (addr)]);
      break;
    case MULT:
      abort ();
      break;
    case PLUS:
      switch (GET_CODE (XEXP (addr, 0)))
	{
	case REG:
	  if (GET_CODE (XEXP (addr, 1)) == REG)
	    {
	      ireg = XEXP (addr, 0);
	      breg = XEXP (addr, 1);
	    }
	  else if (CONSTANT_ADDRESS_P (XEXP (addr, 1)))
	    {
	      if (REG_OK_FOR_BASE_P (XEXP (addr, 0))
		  && ((GET_CODE (XEXP (addr, 1)) == CONST_INT)
		      || (const_and_contains_symbol_ref (XEXP (addr, 1)))
		      || (const_and_contains_label_ref (XEXP (addr, 1)))))
		ireg = XEXP (addr, 0);
	      else
		breg = XEXP (addr, 0);

	      offset = XEXP (addr, 1);
	    }
	  else
	    abort ();
	  break;
	case MULT:
	  ireg = XEXP (XEXP (addr, 0), 0);
	  scale = XEXP (XEXP (addr, 0), 1);
	  breg = XEXP (addr, 1);
	  break;
	case PLUS:
	  if ((GET_CODE (XEXP (XEXP (addr, 0), 0)) == MULT)
	      && (GET_CODE (XEXP (XEXP (XEXP (addr, 0), 0), 0)) == REG)
	      && (SCALE_FOR_INDEX_P (XEXP (XEXP (XEXP (addr, 0), 0), 1)))
	      && (GET_CODE (XEXP (XEXP (addr, 0), 1)) == REG)
	      && (GET_CODE (XEXP (addr, 1)) == CONST_INT))
	    {
	      ireg = XEXP (XEXP (XEXP (addr, 0), 0), 0);
	      breg = XEXP (XEXP (addr, 0), 1);
	      scale = (XEXP (XEXP (XEXP (addr, 0), 0), 1));
	      offset = XEXP (addr, 1);
	    }
	  else if (GET_CODE (XEXP (XEXP (addr, 0), 0)) == REG
		   && GET_CODE (XEXP (XEXP (addr, 0), 1)) == REG
		   && CONSTANT_ADDRESS_P (XEXP (addr, 1)))
	    {
	      ireg = XEXP (XEXP (addr, 0), 0);
	      breg = XEXP (XEXP (addr, 0), 1);
	      offset = XEXP (addr, 1);
	    }
	  else if (GET_CODE (XEXP (XEXP (addr, 0), 0)) == REG
		   && GET_CODE (XEXP (addr, 1)) == REG
		   && CONSTANT_ADDRESS_P (XEXP (XEXP (addr, 0), 1)))
	    {
	      ireg = XEXP (XEXP (addr, 0), 0);
	      breg = XEXP (addr, 1);
	      offset = XEXP (XEXP (addr, 0), 1);
	    }
	  else
	    abort ();
	  break;
	default:
	  if (CONSTANT_ADDRESS_P (XEXP (addr, 1)))
	    {
	      if (CONSTANT_ADDRESS_P (XEXP (addr, 0)))
		offset = addr;
	      else if (GET_CODE (XEXP (addr, 0)) == POST_INC
		       || GET_CODE (XEXP (addr, 0)) == POST_DEC)
		post_offset = XEXP (addr, 1);
	      else
		abort ();
	    }

	  break;
	}

      if (scale)
	{
	  fprintf (file, "%ld(%s,%s,%ld)", offset ? INTVAL (offset) : 0,
		   reg_names[REGNO (breg)], reg_names[REGNO (ireg)],
		   INTVAL (scale));
	}
      else
	{
	  /* If this is (POST_DEC/INC expression + post_offset) make addr =
	   * POST_DEC/INC expression  */
	  if (post_offset != 0)
	    {
	      addr = XEXP (addr, 0);
	      print_post_operand_address (file, addr, INTVAL (post_offset));
	      break;
	    }

	  if (ireg != 0)
	    {
	      if (offset != 0)
		{
		  output_addr_const (file, offset);
		  /* Print modifier if relevant.  */
		}
	      else
		{
		  fprintf (file, "0");
		}
	      /* Print address string */
	      if (breg != 0)
		{
		  fprintf (file, "(%s,%s)", reg_names[REGNO (breg)],
			   reg_names[REGNO (ireg)]);
		}
	      else
		fprintf (file, "(%s)", reg_names[REGNO (ireg)]);
	    }
	  else
	    {
	      if (offset != 0)
		{
		  output_addr_const (file, offset);
		}
	      else
		{
		  fprintf (file, "0");
		}

	      if (breg != 0)
		{
		  if (offset == 0)
		    fprintf (file, "0");
		  fprintf (file, "(%s)", reg_names[REGNO (breg)]);
		}
	    }
	}
      break;

    case POST_DEC:
    case POST_INC:
    case POST_MODIFY:
      print_post_operand_address (file, addr, 0);
      break;

    default:

      output_addr_const (file, addr);
    }

  if (mem)
    fprintf (file, ")");
}


/*****************************************************************************/
/* MACHINE DESCRIPTION HELPER-FUNCTIONS					     */
/*****************************************************************************/

void crx_expand_movmem_single (rtx src, rtx srcbase, rtx dst, rtx dstbase,
			       rtx tmp_reg, unsigned HOST_WIDE_INT *offset_p)
{
  rtx addr, mem;
  unsigned HOST_WIDE_INT offset = *offset_p;

  /* Load */
  addr = plus_constant (src, offset);
  mem = adjust_automodify_address (srcbase, SImode, addr, offset);
  emit_move_insn (tmp_reg, mem);

  /* Store */
  addr = plus_constant (dst, offset);
  mem = adjust_automodify_address (dstbase, SImode, addr, offset);
  emit_move_insn (mem, tmp_reg);

  *offset_p = offset + 4;
}

int
crx_expand_movmem (rtx dstbase, rtx srcbase, rtx count_exp, rtx align_exp)
{
  unsigned HOST_WIDE_INT count = 0, offset, si_moves, i;
  HOST_WIDE_INT align = 0;

  rtx src, dst;
  rtx tmp_reg;

  if (GET_CODE (align_exp) == CONST_INT)
    { /* Only if aligned */
      align = INTVAL (align_exp);
      if (align & 3) return 0;
    }

  if (GET_CODE (count_exp) == CONST_INT)
    { /* No more than 16 SImode moves */
      count = INTVAL (count_exp);
      if (count > 64) return 0;
    }

  tmp_reg = gen_reg_rtx(SImode);

  /* Create psrs for the src and dest pointers */
  dst = copy_to_mode_reg (Pmode, XEXP (dstbase, 0));
  if (dst != XEXP (dstbase, 0))
    dstbase = replace_equiv_address_nv (dstbase, dst);
  src = copy_to_mode_reg (Pmode, XEXP (srcbase, 0));
  if (src != XEXP (srcbase, 0))
    srcbase = replace_equiv_address_nv (srcbase, src);

  offset = 0;

  /* Emit SImode moves */
  si_moves = count >> 2;
  for (i = 0; i < si_moves; i++)
    crx_expand_movmem_single (src, srcbase, dst, dstbase, tmp_reg, &offset);

  /* Special cases */
  if (count & 3)
    {
      offset = count - 4;
      crx_expand_movmem_single (src, srcbase, dst, dstbase, tmp_reg, &offset);
    }

  gcc_assert (offset == count);

  return 1;
}

rtx
crx_expand_compare (enum rtx_code code, enum machine_mode mode)
{
  rtx op0, op1, cc_reg, ret;

  op0 = crx_compare_op0;
  op1 = crx_compare_op1;

  /* Emit the compare that writes into CC_REGNUM) */
  cc_reg = gen_rtx_REG (CCmode, CC_REGNUM);
  ret = gen_rtx_COMPARE (CCmode, op0, op1);
  emit_insn (gen_rtx_SET (VOIDmode, cc_reg, ret));
  /* debug_rtx (get_last_insn ()); */

  /* Return the rtx for using the result in CC_REGNUM */
  return gen_rtx_fmt_ee (code, mode, cc_reg, const0_rtx);
}

void
crx_expand_branch (enum rtx_code code, rtx label)
{
  rtx tmp = crx_expand_compare (code, VOIDmode);
  tmp = gen_rtx_IF_THEN_ELSE (VOIDmode, tmp,
			      gen_rtx_LABEL_REF (VOIDmode, label),
			      pc_rtx);
  emit_jump_insn (gen_rtx_SET (VOIDmode, pc_rtx, tmp));
  /* debug_rtx (get_last_insn ()); */
}

void
crx_expand_scond (enum rtx_code code, rtx dest)
{
  rtx tmp = crx_expand_compare (code, GET_MODE (dest));
  emit_move_insn (dest, tmp);
  /* debug_rtx (get_last_insn ()); */
}

static void
mpushpop_str (char *stringbuffer, const char *mnemonic, char *mask)
{
  if(strlen(mask) > 2 || crx_interrupt_function_p ()) /* needs 2-word instr. */
    sprintf (stringbuffer, "\n\t%s\tsp, {%s}", mnemonic, mask);
  else /* single word instruction */
    sprintf (stringbuffer, "\n\t%s\t%s", mnemonic, mask);
}

/* Called from crx.md. The return value depends on the parameter push_or_pop:
 * When push_or_pop is zero -> string for push instructions of prologue.
 * When push_or_pop is nonzero -> string for pop/popret/retx in epilogue.
 * Relies on the assumptions:
 * 1. RA is the last register to be saved.
 * 2. The maximal value of the counter is MAX_COUNT. */

char *
crx_prepare_push_pop_string (int push_or_pop)
{
  /* j is the number of registers being saved, takes care that there won't be
   * more than 8 in one push/pop instruction */

  /* For the register mask string */
  static char mask_str[50];

  /* i is the index of save_regs[], going from 0 until last_reg_to_save */
  int i = 0;

  int ra_in_bitmask = 0;

  char *return_str;

  /* For reversing on the push instructions if there are more than one. */
  char *temp_str;

  return_str = (char *) xmalloc (120);
  temp_str = (char *) xmalloc (120);

  /* Initialize */
  memset (return_str, 0, 3);

  while (i <= last_reg_to_save)
    {
      /* Prepare mask for one instruction. */
      mask_str[0] = 0;

      if (i <= SP_REGNUM)
	{ /* Add regs unit full or SP register reached */
	  int j = 0;
	  while (j < MAX_COUNT && i <= SP_REGNUM)
	    {
	      if (save_regs[i])
		{
		  /* TODO to use ra_in_bitmask for detecting last pop is not
		   * smart it prevents things like:  popret r5 */
		  if (i == RETURN_ADDRESS_REGNUM) ra_in_bitmask = 1;
		  if (j > 0) strcat (mask_str, ", ");
		  strcat (mask_str, reg_names[i]);
		  ++j;
		}
	      ++i;
	    }
	}
      else
	{
	  /* Handle hi/lo savings */
	  while (i <= last_reg_to_save)
	    {
	      if (save_regs[i])
		{
		  strcat (mask_str, "lo, hi");
		  i = last_reg_to_save + 1;
		  break;
		}
	      ++i;
	    }
	}

      if (strlen(mask_str) == 0) continue;
       	
      if (push_or_pop == 1)
	{
	  if (crx_interrupt_function_p ())
	    mpushpop_str (temp_str, "popx", mask_str);
	  else
	    {
	      if (ra_in_bitmask)
		{
		  mpushpop_str (temp_str, "popret", mask_str);
		  ra_in_bitmask = 0;
		}
	      else mpushpop_str (temp_str, "pop", mask_str);
	    }

	  strcat (return_str, temp_str);
	}
      else
	{
	  /* push - We need to reverse the order of the instructions if there
	   * are more than one. (since the pop will not be reversed in the
	   * epilogue */
      	  if (crx_interrupt_function_p ())
	    mpushpop_str (temp_str, "pushx", mask_str);
	  else
	    mpushpop_str (temp_str, "push", mask_str);
	  strcat (temp_str, return_str);
	  strcpy (strcat (return_str, "\t"), temp_str);
	}

    }

  if (push_or_pop == 1)
    {
      /* pop */
      if (crx_interrupt_function_p ())
	strcat (return_str, "\n\tretx\n");

      else if (!FUNC_IS_NORETURN_P (current_function_decl)
	       && !save_regs[RETURN_ADDRESS_REGNUM])
	strcat (return_str, "\n\tjump\tra\n");
    }

  /* Skip the newline and the tab in the start of return_str. */
  return_str += 2;
  return return_str;
}

/*  CompactRISC CRX Architecture stack layout:

     0 +---------------------
	|
	.
	.
	|
	+==================== Sp(x)=Ap(x+1)
      A | Args for functions
      | | called by X and      Dynamically
      | | Dynamic allocations  allocated and
      | | (alloca, variable    deallocated
  Stack | length arrays).
  grows +-------------------- Fp(x)
  down| | Local variables of X
  ward| +--------------------
      | | Regs saved for X-1
      | +==================== Sp(x-1)=Ap(x)
	| Args for func X
	| pushed by X-1
	+-------------------- Fp(x-1)
	|
	|
	V

*/

void
crx_expand_prologue (void)
{
  crx_compute_frame ();
  crx_compute_save_regs ();

  /* If there is no need in push and adjustment to sp, return. */
  if (size_for_adjusting_sp + sum_regs == 0)
    return;

  if (last_reg_to_save != -1)
    /* If there are registers to push.  */
    emit_insn (gen_push_for_prologue (GEN_INT (sum_regs)));

  if (size_for_adjusting_sp > 0)
    emit_insn (gen_addsi3 (stack_pointer_rtx, stack_pointer_rtx,
			   GEN_INT (-(size_for_adjusting_sp))));

  if (frame_pointer_needed)
    /* Initialize the frame pointer with the value of the stack pointer
     * pointing now to the locals. */
    emit_move_insn (frame_pointer_rtx, stack_pointer_rtx);
}

/* Generate insn that updates the stack for local variables and padding for
 * registers we save. - Generate the appropriate return insn. */

void
crx_expand_epilogue (void)
{
  rtx return_reg;

  /* Nonzero if we need to return and pop only RA. This will generate a
   * different insn. This differentiate is for the peepholes for call as last
   * statement in function. */
  int only_popret_RA = (save_regs[RETURN_ADDRESS_REGNUM]
			&& (sum_regs == UNITS_PER_WORD));

  /* Return register.  */
  return_reg = gen_rtx_REG (Pmode, RETURN_ADDRESS_REGNUM);

  if (frame_pointer_needed)
    /* Restore the stack pointer with the frame pointers value */
    emit_move_insn (stack_pointer_rtx, frame_pointer_rtx);

  if (size_for_adjusting_sp > 0)
    emit_insn (gen_addsi3 (stack_pointer_rtx, stack_pointer_rtx,
			   GEN_INT (size_for_adjusting_sp)));

  if (crx_interrupt_function_p ())
    emit_jump_insn (gen_interrupt_return ());
  else if (last_reg_to_save == -1)
    /* Nothing to pop */
    /* Don't output jump for interrupt routine, only retx.  */
    emit_jump_insn (gen_indirect_jump_return ());
  else if (only_popret_RA)
    emit_jump_insn (gen_popret_RA_return ());
  else
    emit_jump_insn (gen_pop_and_popret_return (GEN_INT (sum_regs)));
}

