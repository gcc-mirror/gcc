/* Target definitions for the MorphoRISC1
   Copyright (C) 2005 Free Software Foundation, Inc.
   Contributed by Red Hat, Inc.

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
   along with GCC; see the file COPYING.  If not, write to the Free
   Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA
   02110-1301, USA.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "rtl.h"
#include "regs.h"
#include "hard-reg-set.h"
#include "real.h"
#include "insn-config.h"
#include "conditions.h"
#include "insn-attr.h"
#include "recog.h"
#include "toplev.h"
#include "output.h"
#include "integrate.h"
#include "tree.h"
#include "function.h"
#include "expr.h"
#include "optabs.h"
#include "libfuncs.h"
#include "flags.h"
#include "tm_p.h"
#include "ggc.h"
#include "insn-flags.h"
#include "obstack.h"
#include "except.h"
#include "target.h"
#include "target-def.h"

/* Frame pointer register mask.  */
#define FP_MASK		 	 (1 << (GPR_FP))

/* Link register mask.  */
#define LINK_MASK	 	 (1 << (GPR_LINK))

/* First GPR.  */
#define MS1_INT_ARG_FIRST 1

/* Given a SIZE in bytes, advance to the next word.  */
#define ROUND_ADVANCE(SIZE) (((SIZE) + UNITS_PER_WORD - 1) / UNITS_PER_WORD)

/* A C structure for machine-specific, per-function data.
   This is added to the cfun structure.  */
struct machine_function GTY(())
{
  /* Flags if __builtin_return_address (n) with n >= 1 was used.  */
  int ra_needs_full_frame;
  struct rtx_def * eh_stack_adjust;
  int interrupt_handler;
};

/* Define the information needed to generate branch and scc insns.
   This is stored from the compare operation.  */
struct rtx_def * ms1_compare_op0;
struct rtx_def * ms1_compare_op1;

/* Current frame information calculated by compute_frame_size.  */
struct ms1_frame_info current_frame_info;

/* Zero structure to initialize current_frame_info.  */
struct ms1_frame_info zero_frame_info;

/* ms1 doesn't have unsigned compares need a library call for this.  */
struct rtx_def * ms1_ucmpsi3_libcall;


static rtx
ms1_struct_value_rtx (tree fndecl ATTRIBUTE_UNUSED,
			 int incoming ATTRIBUTE_UNUSED)
{
  return gen_rtx_REG (Pmode, RETVAL_REGNUM);
}

/* Implement RETURN_ADDR_RTX.  */
rtx
ms1_return_addr_rtx (int count)
{
  if (count != 0)
    return NULL_RTX;

  return get_hard_reg_initial_val (Pmode, GPR_LINK);
}

/* The following variable value indicates the number of nops required
   between the current instruction and the next instruction to avoid
   any pipeline hazards.  */
static int ms1_nops_required = 0;
static const char * ms1_nop_reasons = "";

/* Implement ASM_OUTPUT_OPCODE.  */
const char *
ms1_asm_output_opcode (FILE *f ATTRIBUTE_UNUSED, const char *ptr)
{
  if (ms1_nops_required)
    fprintf (f, ";# need %d nops because of %s\n\t",
	     ms1_nops_required, ms1_nop_reasons);
  
  while (ms1_nops_required)
    {
      fprintf (f, "or r0, r0, r0\n\t");
      -- ms1_nops_required;
    }
  
  return ptr;
}

/* Return TRUE if INSN is a memory load.  */
static bool
ms1_memory_load (rtx insn)
{
  return ((GET_CODE (insn) == SET) && (GET_CODE (XEXP (insn,1)) == MEM));
}

/* Given an insn, return whether it's a memory operation or a branch
   operation, otherwise return TYPE_ARITH.  */
static enum attr_type
ms1_get_attr_type (rtx complete_insn)
{
  rtx insn = PATTERN (complete_insn);

  if ((GET_CODE (insn) == SET)
      && ((GET_CODE (XEXP (insn, 0)) == MEM)
	  || (GET_CODE (XEXP (insn, 1)) == MEM)))
    return TYPE_MEM;

  else if (((GET_CODE (insn) == SET) && (XEXP (insn, 0) == pc_rtx))
	   || (GET_CODE (complete_insn) == JUMP_INSN)
	   || (GET_CODE (complete_insn) == CALL_INSN))
    return TYPE_BRANCH;

  else
    return TYPE_ARITH;
}

/* A helper routine for insn_dependent_p called through note_stores.  */

static void
insn_dependent_p_1 (rtx x, rtx pat ATTRIBUTE_UNUSED, void *data)
{
  rtx * pinsn = (rtx *) data;

  if (*pinsn && reg_mentioned_p (x, *pinsn))
    *pinsn = NULL_RTX;
}

/* Return true if anything in insn X is (anti,output,true)
   dependent on anything in insn Y.  */

static bool
insn_dependent_p (rtx x, rtx y)
{
  rtx tmp;

  if (! INSN_P (x) || ! INSN_P (y))
    return 0;

  tmp = PATTERN (y);
  note_stores (PATTERN (x), insn_dependent_p_1, &tmp);
  if (tmp == NULL_RTX)
    return true;

  tmp = PATTERN (x);
  note_stores (PATTERN (y), insn_dependent_p_1, &tmp);
  return (tmp == NULL_RTX);
}


/* Return true if anything in insn X is true dependent on anything in
   insn Y.  */
static bool
insn_true_dependent_p (rtx x, rtx y)
{
  rtx tmp;

  if (! INSN_P (x) || ! INSN_P (y))
    return 0;

  tmp = PATTERN (y);
  note_stores (PATTERN (x), insn_dependent_p_1, &tmp);
  return (tmp == NULL_RTX);
}

/* The following determines the number of nops that need to be
   inserted between the previous instructions and current instruction
   to avoid pipeline hazards on the ms1 processor.  Remember that
   the function is not called for asm insns.  */

void
ms1_final_prescan_insn (rtx   insn,
			rtx * opvec ATTRIBUTE_UNUSED,
			int   noperands ATTRIBUTE_UNUSED)
{
  rtx prev_i;

  ms1_nops_required = 0;
  ms1_nop_reasons = "";

  /* Only worry about real instructions.  */
  if (! INSN_P (insn))
    return;

  /* Find the previous real instructions.  */
  prev_i = PREV_INSN (insn);
  while (prev_i != NULL
	 && (! INSN_P (prev_i)
	     || GET_CODE (PATTERN (prev_i)) == USE
	     || GET_CODE (PATTERN (prev_i)) == CLOBBER))
    prev_i = PREV_INSN (prev_i);

  /* If there isn't one then there is nothing that we need do.  */
  if (prev_i == NULL || ! INSN_P (prev_i))
    return;

  /* Delayed branch slots already taken care of by delay branch scheduling.  */
  if (ms1_get_attr_type (prev_i) == TYPE_BRANCH)
    return;

  switch (ms1_get_attr_type (insn))
    {
    case TYPE_MEM:
      /* Avoid consecutive memory operation.  */
      if  (ms1_get_attr_type (prev_i) == TYPE_MEM
	   && ms1_cpu == PROCESSOR_MS1_64_001)
	{
	  ms1_nops_required = 1;
	  ms1_nop_reasons = "consecutive mem ops";
	}
      /* Drop through.  */

    case TYPE_ARITH:
    case TYPE_COMPLEX:
      /* One cycle of delay is required between load
	 and the dependent arithmetic instruction.  */
      if (ms1_memory_load (PATTERN (prev_i))
	  && insn_true_dependent_p (prev_i, insn))
	{
	  ms1_nops_required = 1;
	  ms1_nop_reasons = "load->arith dependency delay";
	}
      break;

    case TYPE_BRANCH:
      if (insn_dependent_p (prev_i, insn))
	{
	  if (ms1_get_attr_type (prev_i) == TYPE_ARITH
	      && ms1_cpu == PROCESSOR_MS1_64_001)
	    {
	      /* One cycle of delay between arith
		 instructions and branch dependent on arith.  */
	      ms1_nops_required = 1;
	      ms1_nop_reasons = "arith->branch dependency delay";
	    }
	  else if (ms1_memory_load (PATTERN (prev_i)))
	    {
	      /* Two cycles of delay are required
		 between load and dependent branch.  */
	      if (ms1_cpu == PROCESSOR_MS1_64_001)
		ms1_nops_required = 2;
	      else
		ms1_nops_required = 1;
	      ms1_nop_reasons = "load->branch dependency delay";
	    }
	}
      break;

    default:
      fatal_insn ("ms1_final_prescan_insn, invalid insn #1", insn);
      break;
    }
}

/* Print debugging information for a frame.  */
static void
ms1_debug_stack (struct ms1_frame_info * info)
{
  int regno;

  if (!info)
    {
      error ("info pointer NULL");
      gcc_unreachable ();
    }

  fprintf (stderr, "\nStack information for function %s:\n",
	   ((current_function_decl && DECL_NAME (current_function_decl))
	    ? IDENTIFIER_POINTER (DECL_NAME (current_function_decl))
	    : "<unknown>"));

  fprintf (stderr, "\ttotal_size       = %d\n", info->total_size);
  fprintf (stderr, "\tpretend_size     = %d\n", info->pretend_size);
  fprintf (stderr, "\targs_size        = %d\n", info->args_size);
  fprintf (stderr, "\textra_size       = %d\n", info->extra_size);
  fprintf (stderr, "\treg_size         = %d\n", info->reg_size);
  fprintf (stderr, "\tvar_size         = %d\n", info->var_size);
  fprintf (stderr, "\tframe_size       = %d\n", info->frame_size);
  fprintf (stderr, "\treg_mask         = 0x%x\n", info->reg_mask);
  fprintf (stderr, "\tsave_fp          = %d\n", info->save_fp);
  fprintf (stderr, "\tsave_lr          = %d\n", info->save_lr);
  fprintf (stderr, "\tinitialized      = %d\n", info->initialized);
  fprintf (stderr, "\tsaved registers =");

  /* Print out reg_mask in a more readable format.  */
  for (regno = GPR_R0; regno <= GPR_LAST; regno++)
    if ( (1 << regno) & info->reg_mask)
      fprintf (stderr, " %s", reg_names[regno]);

  putc ('\n', stderr);
  fflush (stderr);
}

/* Print a memory address as an operand to reference that memory location.  */

static void
ms1_print_operand_simple_address (FILE * file, rtx addr)
{
  if (!addr)
    error ("PRINT_OPERAND_ADDRESS, null pointer");

  else
    switch (GET_CODE (addr))
      {
      case REG:
	fprintf (file, "%s, #0", reg_names [REGNO (addr)]);
	break;
	
      case PLUS:
	{
	  rtx reg = 0;
	  rtx offset = 0;
	  rtx arg0 = XEXP (addr, 0);
	  rtx arg1 = XEXP (addr, 1);

	  if (GET_CODE (arg0) == REG)
	    {
	      reg = arg0;
	      offset = arg1;
	      if (GET_CODE (offset) == REG)
		fatal_insn ("PRINT_OPERAND_ADDRESS, 2 regs", addr);
	    }

	  else if (GET_CODE (arg1) == REG)
	      reg = arg1, offset = arg0;
	  else if (CONSTANT_P (arg0) && CONSTANT_P (arg1))
	    {
	      fprintf (file, "%s, #", reg_names [GPR_R0]);
	      output_addr_const (file, addr);
	      break;
	    }
	  fprintf (file, "%s, #", reg_names [REGNO (reg)]);
	  output_addr_const (file, offset);
	  break;
	}

      case LABEL_REF:
      case SYMBOL_REF:
      case CONST_INT:
      case CONST:
	output_addr_const (file, addr);
	break;

      default:
	fatal_insn ("PRINT_OPERAND_ADDRESS, invalid insn #1", addr);
	break;
      }
}

/* Implement PRINT_OPERAND_ADDRESS.  */
void
ms1_print_operand_address (FILE * file, rtx addr)
{
  if (GET_CODE (addr) == AND
      && GET_CODE (XEXP (addr, 1)) == CONST_INT
      && INTVAL (XEXP (addr, 1)) == -3)
    ms1_print_operand_simple_address (file, XEXP (addr, 0));
  else
    ms1_print_operand_simple_address (file, addr);
}

/* Implement PRINT_OPERAND.  */
void
ms1_print_operand (FILE * file, rtx x, int code)
{
  switch (code)
    {
    case '#':
      /* Output a nop if there's nothing for the delay slot.  */
      if (dbr_sequence_length () == 0)
	fputs ("\n\tor r0, r0, r0", file);
      return;
      
    case 'H': 
      fprintf(file, "#%%hi16(");
      output_addr_const (file, x);
      fprintf(file, ")");
      return;
      
    case 'L': 
      fprintf(file, "#%%lo16(");
      output_addr_const (file, x);
      fprintf(file, ")");
      return;

    case 'N': 
      fprintf(file, "#%ld", ~INTVAL (x));
      return;

    case 'z':
      if (GET_CODE (x) == CONST_INT && INTVAL (x) == 0)
	{
	  fputs (reg_names[GPR_R0], file);
	  return;
	}

    case 0:
      /* Handled below.  */
      break;

    default:
      /* output_operand_lossage ("ms1_print_operand: unknown code"); */
      fprintf (file, "unknown code");
      return;
    }

  switch (GET_CODE (x))
    {
    case REG:
      fputs (reg_names [REGNO (x)], file);
      break;

    case CONST:
    case CONST_INT:
      fprintf(file, "#%ld", INTVAL (x));
      break;

    case MEM:
      ms1_print_operand_address(file, XEXP (x,0));
      break;

    case LABEL_REF:
    case SYMBOL_REF:
      output_addr_const (file, x);
      break;
      
    default:
      fprintf(file, "Uknown code: %d", GET_CODE (x));
      break;
    }

  return;
}

/* Implement INIT_CUMULATIVE_ARGS.  */
void
ms1_init_cumulative_args (CUMULATIVE_ARGS * cum, tree fntype, rtx libname,
			     tree fndecl ATTRIBUTE_UNUSED, int incoming)
{
  *cum = 0;

  if (TARGET_DEBUG_ARG)
    {
      fprintf (stderr, "\nms1_init_cumulative_args:");

      if (incoming)
	fputs (" incoming", stderr);

      if (fntype)
	{
	  tree ret_type = TREE_TYPE (fntype);
	  fprintf (stderr, " return = %s,",
		   tree_code_name[ (int)TREE_CODE (ret_type) ]);
	}

      if (libname && GET_CODE (libname) == SYMBOL_REF)
	fprintf (stderr, " libname = %s", XSTR (libname, 0));

      if (cfun->returns_struct)
	fprintf (stderr, " return-struct");

      putc ('\n', stderr);
    }
}

/* Compute the slot number to pass an argument in.
   Returns the slot number or -1 if passing on the stack.

   CUM is a variable of type CUMULATIVE_ARGS which gives info about
    the preceding args and about the function being called.
   MODE is the argument's machine mode.
   TYPE is the data type of the argument (as a tree).
    This is null for libcalls where that information may
    not be available.
   NAMED is nonzero if this argument is a named parameter
    (otherwise it is an extra parameter matching an ellipsis).
   INCOMING_P is zero for FUNCTION_ARG, nonzero for FUNCTION_INCOMING_ARG.
   *PREGNO records the register number to use if scalar type.  */

static int
ms1_function_arg_slotno (const CUMULATIVE_ARGS * cum,
			    enum machine_mode mode,
			    tree type,
			    int named ATTRIBUTE_UNUSED,
			    int incoming_p ATTRIBUTE_UNUSED,
			    int * pregno)
{
  int regbase = MS1_INT_ARG_FIRST;
  int slotno  = * cum;

  if (mode == VOIDmode || targetm.calls.must_pass_in_stack (mode, type))
    return -1;

  if (slotno >= MS1_NUM_ARG_REGS)
    return -1;

  * pregno = regbase + slotno;

  return slotno;
}

/* Implement FUNCTION_ARG.  */
rtx
ms1_function_arg (const CUMULATIVE_ARGS * cum,
		  enum machine_mode mode,
		  tree type,
		  int named,
		  int incoming_p)
{
  int slotno, regno;
  rtx reg;

  slotno = ms1_function_arg_slotno (cum, mode, type, named, incoming_p, 
				       & regno);

  if (slotno == -1)
    reg = NULL_RTX;
  else
    reg = gen_rtx_REG (mode, regno);

  return reg;
}

/* Implement FUNCTION_ARG_ADVANCE.  */
void
ms1_function_arg_advance (CUMULATIVE_ARGS * cum,
			  enum machine_mode mode,
			  tree type ATTRIBUTE_UNUSED,
			  int named)
{
  int slotno, regno;

  /* We pass 0 for incoming_p here, it doesn't matter.  */
  slotno = ms1_function_arg_slotno (cum, mode, type, named, 0, &regno);

  * cum += (mode != BLKmode
	    ? ROUND_ADVANCE (GET_MODE_SIZE (mode))
	    : ROUND_ADVANCE (int_size_in_bytes (type)));

  if (TARGET_DEBUG_ARG)
    fprintf (stderr,
	     "ms1_function_arg_advance: words = %2d, mode = %4s, named = %d, size = %3d\n",
	     *cum, GET_MODE_NAME (mode), named, 
	     (*cum) * UNITS_PER_WORD);
}

/* Implement hook TARGET_ARG_PARTIAL_BYTES.

   Returns the number of bytes at the beginning of an argument that
   must be put in registers.  The value must be zero for arguments
   that are passed entirely in registers or that are entirely pushed
   on the stack.  */
static int
ms1_arg_partial_bytes (CUMULATIVE_ARGS * pcum,
		       enum machine_mode mode,
		       tree type,
		       bool named ATTRIBUTE_UNUSED)
{
  int cum = * pcum;
  int words;

  if (mode == BLKmode)
    words = ((int_size_in_bytes (type) + UNITS_PER_WORD - 1)
	     / UNITS_PER_WORD);
  else
    words = (GET_MODE_SIZE (mode) + UNITS_PER_WORD - 1) / UNITS_PER_WORD;

  if (! targetm.calls.pass_by_reference (& cum, mode, type, named)
      && cum < MS1_NUM_ARG_REGS
      && (cum + words) > MS1_NUM_ARG_REGS)
    {
      int bytes = (MS1_NUM_ARG_REGS - cum) * UNITS_PER_WORD; 

      if (TARGET_DEBUG)
	fprintf (stderr, "function_arg_partial_nregs = %d\n", bytes);
      return bytes;
    }

  return 0;
}


/* Implement TARGET_PASS_BY_REFERENCE hook.  */
static bool
ms1_pass_by_reference (CUMULATIVE_ARGS * cum ATTRIBUTE_UNUSED,
		       enum machine_mode mode ATTRIBUTE_UNUSED,
		       tree type,
		       bool named ATTRIBUTE_UNUSED)
{
  return (type && int_size_in_bytes (type) > 4 * UNITS_PER_WORD);
}

/* Implement FUNCTION_ARG_BOUNDARY.  */
int
ms1_function_arg_boundary (enum machine_mode mode ATTRIBUTE_UNUSED,
			   tree type ATTRIBUTE_UNUSED)
{
  return BITS_PER_WORD;
}

/* Implement REG_OK_FOR_BASE_P.  */
int
ms1_reg_ok_for_base_p (rtx x, int strict)
{
  if (strict)
    return  (((unsigned) REGNO (x)) < FIRST_PSEUDO_REGISTER);
  return 1;
}

/* Helper function of ms1_legitimate_address_p.  Return true if XINSN
   is a simple address, otherwise false.  */
static bool
ms1_legitimate_simple_address_p (enum machine_mode mode ATTRIBUTE_UNUSED,
				 rtx xinsn,
				 int strict)
{
  if (TARGET_DEBUG)						
    {									
      fprintf (stderr, "\n========== GO_IF_LEGITIMATE_ADDRESS, %sstrict\n",
	       strict ? "" : "not ");
      debug_rtx (xinsn);
    }

  if (GET_CODE (xinsn) == REG && ms1_reg_ok_for_base_p (xinsn, strict))
    return true;

  if (GET_CODE (xinsn) == PLUS
      && GET_CODE (XEXP (xinsn, 0)) == REG
      && ms1_reg_ok_for_base_p (XEXP (xinsn, 0), strict)
      && GET_CODE (XEXP (xinsn, 1)) == CONST_INT
      && SMALL_INT (XEXP (xinsn, 1)))
    return true;

  return false;
}


/* Helper function of GO_IF_LEGITIMATE_ADDRESS.  Return non-zero if
   XINSN is a legitimate address on MS1.  */
int
ms1_legitimate_address_p (enum machine_mode mode,
			  rtx xinsn,
			  int strict)
{
  if (ms1_legitimate_simple_address_p (mode, xinsn, strict))
    return 1;

  if ((mode) == SImode
      && GET_CODE (xinsn) == AND
      && GET_CODE (XEXP (xinsn, 1)) == CONST_INT
      && INTVAL (XEXP (xinsn, 1)) == -3)
    return ms1_legitimate_simple_address_p (mode, XEXP (xinsn, 0), strict);
  else
    return 0;
}

/* Return truth value of whether OP can be used as an operands where a
   register or 16 bit unsigned integer is needed.  */

int
uns_arith_operand (rtx op, enum machine_mode mode)
{
  if (GET_CODE (op) == CONST_INT && SMALL_INT_UNSIGNED (op))
    return 1;

  return register_operand (op, mode);
}

/* Return truth value of whether OP can be used as an operands where a
   16 bit integer is needed.  */

int
arith_operand (rtx op, enum machine_mode mode)
{
  if (GET_CODE (op) == CONST_INT && SMALL_INT (op))
    return 1;

  return register_operand (op, mode);
}

/* Return truth value of whether OP is a register or the constant 0.  */

int
reg_or_0_operand (rtx op, enum machine_mode mode)
{
  switch (GET_CODE (op))
    {
    case CONST_INT:
      return INTVAL (op) == 0;

    case REG:
    case SUBREG:
      return register_operand (op, mode);

    default:
      break;
    }

  return 0;
}

/* Return truth value of whether OP is a constant that requires two
   loads to put in a register.  */

int
big_const_operand (rtx op, enum machine_mode mode ATTRIBUTE_UNUSED)
{
  if (GET_CODE (op) == CONST_INT && CONST_OK_FOR_LETTER_P (INTVAL (op), 'M'))
    return 1;

  return 0;
}

/* Return truth value of whether OP is a constant that require only
   one load to put in a register.  */

int
single_const_operand (rtx op, enum machine_mode mode ATTRIBUTE_UNUSED)
{
  if (big_const_operand (op, mode)
      || GET_CODE (op) == CONST
      || GET_CODE (op) == LABEL_REF
      || GET_CODE (op) == SYMBOL_REF)
    return 0;

  return 1;
}

/* True if the current function is an interrupt handler
   (either via #pragma or an attribute specification).  */
int interrupt_handler;
enum processor_type ms1_cpu;

static struct machine_function *
ms1_init_machine_status (void)
{
  struct machine_function *f;

  f = ggc_alloc_cleared (sizeof (struct machine_function));

  return f;
}

/* Implement OVERRIDE_OPTIONS.  */
void
ms1_override_options (void)
{
  if (ms1_cpu_string != NULL)
    {
      if (!strcasecmp (ms1_cpu_string, "MS1-64-001"))
	ms1_cpu = PROCESSOR_MS1_64_001;
      else if (!strcasecmp (ms1_cpu_string, "MS1-16-002"))
	ms1_cpu = PROCESSOR_MS1_16_002;
      else if  (!strcasecmp (ms1_cpu_string, "MS1-16-003"))
	{
	  ms1_cpu = PROCESSOR_MS1_16_003;
	  target_flags |= MASK_MUL;
	}
      else
	error ("bad value (%s) for -march= switch", ms1_cpu_string);
    }
  else
    ms1_cpu = PROCESSOR_MS1_64_001;

  if (flag_exceptions)
    {
      flag_omit_frame_pointer = 0;
      flag_gcse = 0;
    }

  init_machine_status = ms1_init_machine_status;
}

/* Do what is necessary for `va_start'.  We look at the current function
   to determine if stdarg or varargs is used and return the address of the
   first unnamed parameter.  */

static rtx
ms1_builtin_saveregs (void)
{
  int first_reg = 0;
  rtx address;
  int regno;

  for (regno = first_reg; regno < MS1_NUM_ARG_REGS; regno ++)
    emit_move_insn (gen_rtx_MEM (word_mode,
				 gen_rtx_PLUS (Pmode,
					       gen_rtx_REG (SImode, ARG_POINTER_REGNUM),
					       GEN_INT (UNITS_PER_WORD * regno))),
		    gen_rtx_REG (word_mode,
				 MS1_INT_ARG_FIRST + regno));

  address = gen_rtx_PLUS (Pmode,
			  gen_rtx_REG (SImode, ARG_POINTER_REGNUM),
			  GEN_INT (UNITS_PER_WORD * first_reg));
  return address;
}

/* Implement `va_start'.  */

void
ms1_va_start (tree valist, rtx nextarg)
{
  ms1_builtin_saveregs ();
  std_expand_builtin_va_start (valist, nextarg);
}

/* Returns the number of bytes offset between the frame pointer and the stack
   pointer for the current function.  SIZE is the number of bytes of space
   needed for local variables.  */

unsigned int
ms1_compute_frame_size (int size)
{
  int           regno;
  unsigned int  total_size;
  unsigned int  var_size;
  unsigned int  args_size;
  unsigned int  pretend_size;
  unsigned int  extra_size;
  unsigned int  reg_size;
  unsigned int  frame_size;
  unsigned int  reg_mask;

  var_size      = size;
  args_size     = current_function_outgoing_args_size;
  pretend_size  = current_function_pretend_args_size;
  extra_size    = FIRST_PARM_OFFSET (0);
  total_size    = extra_size + pretend_size + args_size + var_size;
  reg_size      = 0;
  reg_mask	= 0;

  /* Calculate space needed for registers.  */
  for (regno = GPR_R0; regno <= GPR_LAST; regno++)
    {
      if (MUST_SAVE_REGISTER (regno))
        {
          reg_size += UNITS_PER_WORD;
          reg_mask |= 1 << regno;
        }
    }

  current_frame_info.save_fp = (regs_ever_live [GPR_FP]
				|| frame_pointer_needed
				|| interrupt_handler);
  current_frame_info.save_lr = (regs_ever_live [GPR_LINK]
				|| profile_flag
				|| interrupt_handler);
 
  reg_size += (current_frame_info.save_fp + current_frame_info.save_lr)
               * UNITS_PER_WORD;
  total_size += reg_size;
  total_size = ((total_size + 3) & ~3);

  frame_size = total_size;

  /* Save computed information.  */
  current_frame_info.pretend_size = pretend_size;
  current_frame_info.var_size     = var_size;
  current_frame_info.args_size    = args_size;
  current_frame_info.reg_size     = reg_size;
  current_frame_info.frame_size   = args_size + var_size;
  current_frame_info.total_size   = total_size;
  current_frame_info.extra_size   = extra_size;
  current_frame_info.reg_mask     = reg_mask;
  current_frame_info.initialized  = reload_completed;
 
  return total_size;
}

/* Emit code to save REG in stack offset pointed to by MEM.
   STACK_OFFSET is the offset from the SP where the save will happen.
   This function sets the REG_FRAME_RELATED_EXPR note accordingly.  */
static void
ms1_emit_save_restore (enum save_direction direction,
		       rtx reg,
		       rtx mem,
		       int stack_offset)
{
  if (direction == FROM_PROCESSOR_TO_MEM)
    {
      rtx insn;
  
      insn = emit_move_insn (mem, reg);
      RTX_FRAME_RELATED_P (insn) = 1;
      REG_NOTES (insn)
	= gen_rtx_EXPR_LIST (REG_FRAME_RELATED_EXPR,
			     gen_rtx_SET (VOIDmode,
					  gen_rtx_MEM
					  (SImode,
					   gen_rtx_PLUS (SImode,
							 stack_pointer_rtx,
							 GEN_INT (stack_offset))),
					  reg),
			     REG_NOTES (insn));
    }
  else
    emit_move_insn (reg, mem);
}


/* Emit code to save the frame pointer in the prologue and restore
   frame pointer in epilogue.  */

static void
ms1_emit_save_fp (enum save_direction direction,
		  struct ms1_frame_info info)
{
  rtx base_reg;
  int reg_mask = info.reg_mask  & ~(FP_MASK | LINK_MASK);
  int offset = info.total_size;
  int stack_offset = info.total_size;

  /* If there is nothing to save, get out now.  */
  if (! info.save_fp && ! info.save_lr && ! reg_mask)
    return;

  /* If offset doesn't fit in a 15-bit signed integer,
     uses a scratch registers to get a smaller offset.  */
  if (CONST_OK_FOR_LETTER_P(offset, 'O'))
    base_reg = stack_pointer_rtx;
  else
    {
      /* Use the scratch register R9 that holds old stack pointer.  */
      base_reg = gen_rtx_REG (SImode, GPR_R9);
      offset = 0;
    }

  if (info.save_fp)
    {
      offset -= UNITS_PER_WORD;
      stack_offset -= UNITS_PER_WORD;
      ms1_emit_save_restore (direction, gen_rtx_REG (SImode, GPR_FP),
	     gen_rtx_MEM (SImode, 
			  gen_rtx_PLUS (SImode, base_reg, GEN_INT (offset))),
	     stack_offset);
    }
}

/* Emit code to save registers in the prologue and restore register
   in epilogue.  */

static void
ms1_emit_save_regs (enum save_direction direction,
		    struct ms1_frame_info info)
{
  rtx base_reg;
  int regno;
  int reg_mask = info.reg_mask  & ~(FP_MASK | LINK_MASK);
  int offset = info.total_size;
  int stack_offset = info.total_size;

  /* If there is nothing to save, get out now.  */
  if (! info.save_fp && ! info.save_lr && ! reg_mask)
    return;

  /* If offset doesn't fit in a 15-bit signed integer,
     uses a scratch registers to get a smaller offset.  */
  if (CONST_OK_FOR_LETTER_P(offset, 'O'))
    base_reg = stack_pointer_rtx;
  else
    {
      /* Use the scratch register R9 that holds old stack pointer.  */
      base_reg = gen_rtx_REG (SImode, GPR_R9);
      offset = 0;
    }

  if (info.save_fp)
    {
      /* This just records the space for it, the actual move generated in
	 ms1_emit_save_fp ().  */
      offset -= UNITS_PER_WORD;
      stack_offset -= UNITS_PER_WORD;
    }

  if (info.save_lr)
    {
      offset -= UNITS_PER_WORD;
      stack_offset -= UNITS_PER_WORD;
      ms1_emit_save_restore (direction, gen_rtx_REG (SImode, GPR_LINK), 
		gen_rtx_MEM (SImode,
			     gen_rtx_PLUS (SImode, base_reg, GEN_INT (offset))),
		stack_offset);
    }

  /* Save any needed call-saved regs.  */
  for (regno = GPR_R0; regno <= GPR_LAST; regno++)
    {
      if ((reg_mask & (1 << regno)) != 0)
	{
	  offset -= UNITS_PER_WORD;
	  stack_offset -= UNITS_PER_WORD;
	  ms1_emit_save_restore (direction, gen_rtx_REG (SImode, regno),
				    gen_rtx_MEM (SImode,
						 gen_rtx_PLUS (SImode, base_reg, GEN_INT (offset))),
				    stack_offset);
	}
    }
}

/* Return true if FUNC is a function with the 'interrupt' attribute.  */
static bool
ms1_interrupt_function_p (tree func)
{
  tree a;

  if (TREE_CODE (func) != FUNCTION_DECL)
    return false;

  a = lookup_attribute ("interrupt", DECL_ATTRIBUTES (func));
  return a != NULL_TREE;
}

/* Generate prologue code.  */
void
ms1_expand_prologue (void)
{
  rtx size_rtx, insn;
  unsigned int frame_size;

  if (ms1_interrupt_function_p (current_function_decl))
    {
      interrupt_handler = 1;
      if (cfun->machine)
	cfun->machine->interrupt_handler = 1;
    }

  ms1_compute_frame_size (get_frame_size ());

  if (TARGET_DEBUG_STACK)
    ms1_debug_stack (&current_frame_info);

  /* Compute size of stack adjustment.  */
  frame_size = current_frame_info.total_size;

  /* If offset doesn't fit in a 15-bit signed integer,
     uses a scratch registers to get a smaller offset.  */
  if (CONST_OK_FOR_LETTER_P(frame_size, 'O'))
    size_rtx = GEN_INT (frame_size);
  else
    {
      /* We do not have any scratch registers.  */
      gcc_assert (!interrupt_handler);

      size_rtx = gen_rtx_REG (SImode, GPR_R9);
      insn = emit_move_insn (size_rtx, GEN_INT (frame_size & 0xffff0000));
      insn = emit_insn (gen_iorsi3 (size_rtx, size_rtx,
				    GEN_INT (frame_size & 0x0000ffff)));
    }

  /* Allocate stack for this frame.  */
  /* Make stack adjustment and use scratch register if constant too
     large to fit as immediate.  */
  if (frame_size)
    {
      insn = emit_insn (gen_subsi3 (stack_pointer_rtx,
				 stack_pointer_rtx,
				 size_rtx));
      RTX_FRAME_RELATED_P (insn) = 1;
      REG_NOTES (insn)
	= gen_rtx_EXPR_LIST (REG_FRAME_RELATED_EXPR,
			     gen_rtx_SET (VOIDmode,
					  stack_pointer_rtx,
					  gen_rtx_MINUS (SImode,
							stack_pointer_rtx,
							GEN_INT (frame_size))),
			     REG_NOTES (insn));
    }

  /* Set R9 to point to old sp if required for access to register save area.  */
  if ( current_frame_info.reg_size != 0
       && !CONST_OK_FOR_LETTER_P (frame_size, 'O'))
      emit_insn (gen_addsi3 (size_rtx, size_rtx, stack_pointer_rtx));
  
  /* Save the frame pointer.  */
  ms1_emit_save_fp (FROM_PROCESSOR_TO_MEM, current_frame_info);

  /* Now put the frame pointer into the frame pointer register.  */
  if (frame_pointer_needed)
    {
      insn = emit_move_insn (frame_pointer_rtx, stack_pointer_rtx);
      RTX_FRAME_RELATED_P (insn) = 1;
    }

  /* Save the registers.  */
  ms1_emit_save_regs (FROM_PROCESSOR_TO_MEM, current_frame_info);

  /* If we are profiling, make sure no instructions are scheduled before
     the call to mcount.  */
  if (profile_flag)
    emit_insn (gen_blockage ());
}

/* Implement EPILOGUE_USES.  */
int
ms1_epilogue_uses (int regno)
{
  if (cfun->machine && cfun->machine->interrupt_handler && reload_completed)
    return 1;
  return regno == GPR_LINK;
}

/* Generate epilogue.  EH_MODE is NORMAL_EPILOGUE when generating a
   function epilogue, or EH_EPILOGUE when generating an EH
   epilogue.  */
void
ms1_expand_epilogue (enum epilogue_type eh_mode)
{
  rtx size_rtx, insn;
  unsigned frame_size;

  ms1_compute_frame_size (get_frame_size ());

  if (TARGET_DEBUG_STACK)
    ms1_debug_stack (& current_frame_info);

  /* Compute size of stack adjustment.  */
  frame_size = current_frame_info.total_size;

  /* If offset doesn't fit in a 15-bit signed integer,
     uses a scratch registers to get a smaller offset.  */
  if (CONST_OK_FOR_LETTER_P(frame_size, 'O'))
    size_rtx = GEN_INT (frame_size);
  else
    {
      /* We do not have any scratch registers.  */
      gcc_assert (!interrupt_handler);

      size_rtx = gen_rtx_REG (SImode, GPR_R9);
      insn = emit_move_insn (size_rtx, GEN_INT (frame_size & 0xffff0000));
      insn = emit_insn (gen_iorsi3 (size_rtx, size_rtx,
				    GEN_INT (frame_size & 0x0000ffff)));
      /* Set R9 to point to old sp if required for access to register
	 save area.  */
      emit_insn (gen_addsi3 (size_rtx, size_rtx, stack_pointer_rtx));
    }

  /* Restore sp if there was some possible change to it.  */
  if (frame_pointer_needed)
    insn = emit_move_insn (stack_pointer_rtx, frame_pointer_rtx);

  /* Restore the registers.  */
  ms1_emit_save_fp (FROM_MEM_TO_PROCESSOR, current_frame_info);
  ms1_emit_save_regs (FROM_MEM_TO_PROCESSOR, current_frame_info);

  /* Make stack adjustment and use scratch register if constant too
     large to fit as immediate.  */
  if (frame_size)
    {
      if (CONST_OK_FOR_LETTER_P(frame_size, 'O'))
	/* Can handle this with simple add.  */
	insn = emit_insn (gen_addsi3 (stack_pointer_rtx,
				      stack_pointer_rtx,
				      size_rtx));
      else
	/* Scratch reg R9 has the old sp value.  */
	insn = emit_move_insn (stack_pointer_rtx, 
			       gen_rtx_REG (SImode, GPR_R9));

      REG_NOTES (insn)
	= gen_rtx_EXPR_LIST (REG_FRAME_RELATED_EXPR,
			     gen_rtx_SET (VOIDmode,
					  stack_pointer_rtx,
					  gen_rtx_PLUS (SImode,
							stack_pointer_rtx,
							GEN_INT (frame_size))),
			     REG_NOTES (insn));
    }

  if (cfun->machine && cfun->machine->eh_stack_adjust != NULL_RTX)
    /* Perform the additional bump for __throw.  */
    emit_insn (gen_addsi3 (stack_pointer_rtx,
			   stack_pointer_rtx,
			   cfun->machine->eh_stack_adjust));

  /* Generate the appropriate return.  */
  if (eh_mode == EH_EPILOGUE)
    {
      emit_jump_insn (gen_eh_return_internal ());
      emit_barrier ();
    }
  else if (interrupt_handler)
    emit_jump_insn (gen_return_interrupt_internal ());
  else
    emit_jump_insn (gen_return_internal ());

  /* Reset state info for each function.  */
  interrupt_handler = 0;
  current_frame_info = zero_frame_info;
  if (cfun->machine)
    cfun->machine->eh_stack_adjust = NULL_RTX;
}


/* Generate code for the "eh_return" pattern.  */
void
ms1_expand_eh_return (rtx * operands)
{
  if (GET_CODE (operands[0]) != REG
      || REGNO (operands[0]) != EH_RETURN_STACKADJ_REGNO)
    {
      rtx sp = EH_RETURN_STACKADJ_RTX;

      emit_move_insn (sp, operands[0]);
      operands[0] = sp;
    }

  emit_insn (gen_eh_epilogue (operands[0]));
}

/* Generate code for the "eh_epilogue" pattern.  */
void
ms1_emit_eh_epilogue (rtx * operands ATTRIBUTE_UNUSED)
{
  cfun->machine->eh_stack_adjust = EH_RETURN_STACKADJ_RTX; /* operands[0]; */
  ms1_expand_epilogue (EH_EPILOGUE);
}

/* Handle an "interrupt" attribute.  */
static tree
ms1_handle_interrupt_attribute (tree * node,
			  tree   name,
			  tree   args  ATTRIBUTE_UNUSED,
			  int    flags ATTRIBUTE_UNUSED,
			  bool * no_add_attrs)
{
  if (TREE_CODE (*node) != FUNCTION_DECL)
    {
      warning (OPT_Wattributes,
	       "%qs attribute only applies to functions",
	       IDENTIFIER_POINTER (name));
      *no_add_attrs = true;
    }

  return NULL_TREE;
}

/* Table of machine attributes.  */
const struct attribute_spec ms1_attribute_table[] =
{
  /* name,        min, max, decl?, type?, func?, handler  */
  { "interrupt",  0,   0,   false, false, false, ms1_handle_interrupt_attribute },
  { NULL,         0,   0,   false, false, false, NULL }
};

/* Implement INITIAL_ELIMINATION_OFFSET.  */
int
ms1_initial_elimination_offset (int from, int to)
{
  ms1_compute_frame_size (get_frame_size ());

  if (from == FRAME_POINTER_REGNUM && to == STACK_POINTER_REGNUM)
    return 0;

  else if (from == ARG_POINTER_REGNUM && to == STACK_POINTER_REGNUM)
    return current_frame_info.total_size;

  else if (from == ARG_POINTER_REGNUM && to == FRAME_POINTER_REGNUM)
    return current_frame_info.total_size;

  else
    gcc_unreachable ();
}

/* Generate a compare for CODE.  Return a brand-new rtx that
   represents the result of the compare.  */

static rtx
ms1_generate_compare (enum rtx_code code, rtx op0, rtx op1)
{
  rtx scratch0, scratch1, const_scratch;

  switch (code)
    {
    case GTU:
    case LTU:
    case GEU:
    case LEU:
      /* Need to adjust ranges for faking unsigned compares.  */
      scratch0 = gen_reg_rtx (SImode);
      scratch1 = gen_reg_rtx (SImode);
      const_scratch = force_reg (SImode, GEN_INT(MS1_MIN_INT));
      emit_insn (gen_addsi3 (scratch0, const_scratch, op0));
      emit_insn (gen_addsi3 (scratch1, const_scratch, op1));
      break;
    default:
      scratch0 = op0;
      scratch1 = op1;
      break;
    }
    
  /* Adjust compare operator to fake unsigned compares.  */
  switch (code)
    {
    case GTU:
      code = GT; break;
    case LTU:
      code = LT; break;
    case GEU:
      code = GE; break;
    case LEU:
      code = LE; break;
    default:
      /* do nothing */
      break;
    }

  /* Generate the actual compare.  */
  return gen_rtx_fmt_ee (code, VOIDmode, scratch0, scratch1);
}

/* Emit a branch of kind CODE to location LOC.  */

void
ms1_emit_cbranch (enum rtx_code code, rtx loc, rtx op0, rtx op1)
{
  rtx condition_rtx, loc_ref;

  if (! reg_or_0_operand (op0, SImode))
    op0 = copy_to_mode_reg (SImode, op0);

  if (! reg_or_0_operand (op1, SImode))
    op1 = copy_to_mode_reg (SImode, op1);

  condition_rtx = ms1_generate_compare (code, op0, op1);
  loc_ref = gen_rtx_LABEL_REF (VOIDmode, loc);
  emit_jump_insn (gen_rtx_SET (VOIDmode, pc_rtx,
			       gen_rtx_IF_THEN_ELSE (VOIDmode, condition_rtx,
						     loc_ref, pc_rtx)));
}

/* Subfunction of the following function.  Update the flags of any MEM
   found in part of X.  */

static void
ms1_set_memflags_1 (rtx x, int in_struct_p, int volatile_p)
{
  int i;

  switch (GET_CODE (x))
    {
    case SEQUENCE:
    case PARALLEL:
      for (i = XVECLEN (x, 0) - 1; i >= 0; i--)
	ms1_set_memflags_1 (XVECEXP (x, 0, i), in_struct_p, volatile_p);
      break;

    case INSN:
      ms1_set_memflags_1 (PATTERN (x), in_struct_p, volatile_p);
      break;

    case SET:
      ms1_set_memflags_1 (SET_DEST (x), in_struct_p, volatile_p);
      ms1_set_memflags_1 (SET_SRC (x), in_struct_p, volatile_p);
      break;

    case MEM:
      MEM_IN_STRUCT_P (x) = in_struct_p;
      MEM_VOLATILE_P (x) = volatile_p;
      /* Sadly, we cannot use alias sets because the extra aliasing
	 produced by the AND interferes.  Given that two-byte quantities
	 are the only thing we would be able to differentiate anyway,
	 there does not seem to be any point in convoluting the early
	 out of the alias check.  */
      /* set_mem_alias_set (x, alias_set); */
      break;

    default:
      break;
    }
}

/* Look for any MEMs in the current sequence of insns and set the
   in-struct, unchanging, and volatile flags from the flags in REF.
   If REF is not a MEM, don't do anything.  */

void
ms1_set_memflags (rtx ref)
{
  rtx insn;
  int in_struct_p, volatile_p;

  if (GET_CODE (ref) != MEM)
    return;

  in_struct_p = MEM_IN_STRUCT_P (ref);
  volatile_p = MEM_VOLATILE_P (ref);

  /* This is only called from ms1.md, after having had something 
     generated from one of the insn patterns.  So if everything is
     zero, the pattern is already up-to-date.  */
  if (! in_struct_p && ! volatile_p)
    return;

  for (insn = get_insns (); insn; insn = NEXT_INSN (insn))
    ms1_set_memflags_1 (insn, in_struct_p, volatile_p);
}

/* Implement SECONDARY_RELOAD_CLASS.  */
enum reg_class
ms1_secondary_reload_class (enum reg_class class ATTRIBUTE_UNUSED,
			    enum machine_mode mode,
			    rtx x)
{
  if ((mode == QImode && (!TARGET_BYTE_ACCESS)) || mode == HImode)
    {
      if (GET_CODE (x) == MEM
	  || (GET_CODE (x) == REG && true_regnum (x) == -1)
	  || (GET_CODE (x) == SUBREG
	      && (GET_CODE (SUBREG_REG (x)) == MEM
		  || (GET_CODE (SUBREG_REG (x)) == REG
		      && true_regnum (SUBREG_REG (x)) == -1))))
	return GENERAL_REGS;
    }

  return NO_REGS;
}

/* Handle FUNCTION_VALUE, FUNCTION_OUTGOING_VALUE, and LIBCALL_VALUE
   macros.  */
rtx
ms1_function_value (tree valtype, enum machine_mode mode, tree func_decl ATTRIBUTE_UNUSED)
{
  if ((mode) == DImode || (mode) == DFmode)
    return gen_rtx_MEM (mode, gen_rtx_REG (mode, RETURN_VALUE_REGNUM));

  if (valtype)
    mode = TYPE_MODE (valtype);

  return gen_rtx_REG (mode, RETURN_VALUE_REGNUM);
}

/* Split a move into two smaller pieces.
   MODE indicates the reduced mode.  OPERANDS[0] is the original destination
   OPERANDS[1] is the original src.  The new destinations are
   OPERANDS[2] and OPERANDS[4], while the new sources are OPERANDS[3]
   and OPERANDS[5].  */

void
ms1_split_words (enum machine_mode nmode,
		 enum machine_mode omode,
		 rtx *operands)
{
  rtx dl,dh;	/* src/dest pieces.  */
  rtx sl,sh;
  int	move_high_first = 0;	/* Assume no overlap.  */

  switch (GET_CODE (operands[0])) /* Dest.  */
    {
    case SUBREG:
    case REG:
      if ((GET_CODE (operands[1]) == REG
	   || GET_CODE (operands[1]) == SUBREG)
	  && true_regnum (operands[0]) <= true_regnum (operands[1]))
	move_high_first = 1;

      if (GET_CODE (operands[0]) == SUBREG)
	{
	  dl = gen_rtx_SUBREG (nmode, SUBREG_REG (operands[0]),
			       SUBREG_BYTE (operands[0]) + GET_MODE_SIZE (nmode));
	  dh = gen_rtx_SUBREG (nmode, SUBREG_REG (operands[0]), SUBREG_BYTE (operands[0]));
	}
      else if (GET_CODE (operands[0]) == REG && ! IS_PSEUDO_P (operands[0]))
	{
	  int	r = REGNO (operands[0]);
	  dh = gen_rtx_REG (nmode, r);
	  dl = gen_rtx_REG (nmode, r + HARD_REGNO_NREGS (r, nmode));
	}
      else
	{
	  dh = gen_rtx_SUBREG (nmode, operands[0], 0);
	  dl = gen_rtx_SUBREG (nmode, operands[0], GET_MODE_SIZE (nmode));
	}
      break;

    case MEM:
      switch (GET_CODE (XEXP (operands[0], 0)))
	{
	case POST_INC:
	case POST_DEC:
	  gcc_unreachable ();
	default:
	  dl = operand_subword (operands[0],
				GET_MODE_SIZE (nmode)/UNITS_PER_WORD,
				0, omode);
	  dh = operand_subword (operands[0], 0, 0, omode);
	}
      break;
    default:
      gcc_unreachable ();
    }

  switch (GET_CODE (operands[1]))
    {
    case REG:
      if (! IS_PSEUDO_P (operands[1]))
	{
	  int r = REGNO (operands[1]);

	  sh = gen_rtx_REG (nmode, r);
	  sl = gen_rtx_REG (nmode, r + HARD_REGNO_NREGS (r, nmode));
	}
      else
	{
	  sh = gen_rtx_SUBREG (nmode, operands[1], 0);
	  sl = gen_rtx_SUBREG (nmode, operands[1], GET_MODE_SIZE (nmode));
	}
      break;

    case CONST_DOUBLE:
      if (operands[1] == const0_rtx)
	sh = sl = const0_rtx;
      else
	split_double (operands[1], & sh, & sl);
      break;

    case CONST_INT:
      if (operands[1] == const0_rtx)
	sh = sl = const0_rtx;
      else
	{
	  int vl, vh;

	  switch (nmode)
	    {
	    default:
	      gcc_unreachable ();
	    }
	    
	  sl = GEN_INT (vl);
	  sh = GEN_INT (vh);
	}
      break;

    case SUBREG:
      sl = gen_rtx_SUBREG (nmode,
			   SUBREG_REG (operands[1]),
			   SUBREG_BYTE (operands[1]) + GET_MODE_SIZE (nmode));
      sh = gen_rtx_SUBREG (nmode,
			   SUBREG_REG (operands[1]),
			   SUBREG_BYTE (operands[1]));
      break;

    case MEM:
      switch (GET_CODE (XEXP (operands[1], 0)))
	{
	case POST_DEC:
	case POST_INC:
	  gcc_unreachable ();
	  break;
	default:
	  sl = operand_subword (operands[1], 
				GET_MODE_SIZE (nmode)/UNITS_PER_WORD,
				0, omode);
	  sh = operand_subword (operands[1], 0, 0, omode);
	  
	  /* Check if the DF load is going to clobber the register
             used for the address, and if so make sure that is going
             to be the second move.  */
	  if (GET_CODE (dl) == REG
	      && true_regnum (dl)
	      == true_regnum (XEXP (XEXP (sl, 0 ), 0)))
	    move_high_first = 1;
	}
      break;
    default:
      gcc_unreachable ();
    }

  if (move_high_first)
    {
      operands[2] = dh;
      operands[3] = sh;
      operands[4] = dl;
      operands[5] = sl;
    }
  else
    {
      operands[2] = dl;
      operands[3] = sl;
      operands[4] = dh;
      operands[5] = sh;
    }
  return;
}

/* Implement TARGET_MUST_PASS_IN_STACK hook.  */
static bool
ms1_pass_in_stack (enum machine_mode mode ATTRIBUTE_UNUSED, tree type)
{
  return (((type) != 0
	   && (TREE_CODE (TYPE_SIZE (type)) != INTEGER_CST
	       || TREE_ADDRESSABLE (type))));
}

/* Initialize the GCC target structure.  */
const struct attribute_spec ms1_attribute_table[];

#undef  TARGET_ATTRIBUTE_TABLE
#define TARGET_ATTRIBUTE_TABLE 		ms1_attribute_table
#undef  TARGET_STRUCT_VALUE_RTX
#define TARGET_STRUCT_VALUE_RTX		ms1_struct_value_rtx
#undef  TARGET_PROMOTE_PROTOTYPES
#define TARGET_PROMOTE_PROTOTYPES	hook_bool_tree_true
#undef  TARGET_PASS_BY_REFERENCE
#define TARGET_PASS_BY_REFERENCE	ms1_pass_by_reference
#undef  TARGET_MUST_PASS_IN_STACK
#define TARGET_MUST_PASS_IN_STACK       ms1_pass_in_stack
#undef  TARGET_ARG_PARTIAL_BYTES
#define TARGET_ARG_PARTIAL_BYTES	ms1_arg_partial_bytes

struct gcc_target targetm = TARGET_INITIALIZER;

#include "gt-ms1.h"
