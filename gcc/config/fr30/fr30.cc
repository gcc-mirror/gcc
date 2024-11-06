/* FR30 specific functions.
   Copyright (C) 1998-2024 Free Software Foundation, Inc.
   Contributed by Cygnus Solutions.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   GCC is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */

/*{{{  Includes */

#define IN_TARGET_CODE 1

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "target.h"
#include "rtl.h"
#include "tree.h"
#include "stringpool.h"
#include "attribs.h"
#include "df.h"
#include "memmodel.h"
#include "emit-rtl.h"
#include "stor-layout.h"
#include "varasm.h"
#include "output.h"
#include "expr.h"
#include "builtins.h"
#include "calls.h"

/* This file should be included last.  */
#include "target-def.h"

/*}}}*/
/*{{{  Function Prologues & Epilogues */

/* The FR30 stack looks like this:

             Before call                       After call
   FP ->|                       |       |                       |
        +-----------------------+       +-----------------------+       high
        |                       |       |                       |       memory
        |  local variables,     |       |  local variables,     |
        |  reg save area, etc.  |       |  reg save area, etc.  |
        |                       |       |                       |
        +-----------------------+       +-----------------------+
        |                       |       |                       |
        | args to the func that |       |  args to this func.   |
        | is being called that  |       |                       |
   SP ->| do not fit in regs    |       |                       |
        +-----------------------+       +-----------------------+
                                        |  args that used to be |  \
                                        | in regs; only created |   |  pretend_size
                                   AP-> |   for vararg funcs    |  /
                                        +-----------------------+
                                        |                       |  \
                                        |  register save area   |   |
                                        |                       |   |
					+-----------------------+   |  reg_size
                                        |    return address     |   |
					+-----------------------+   |
                                   FP ->|   previous frame ptr  |  /
                                        +-----------------------+
                                        |                       |  \
                                        |  local variables      |   |  var_size
                                        |                       |  /
                                        +-----------------------+
                                        |                       |  \
     low                                |  room for args to     |   |
     memory                             |  other funcs called   |   |  args_size
                                        |  from this one        |   |
                                   SP ->|                       |  /
                                        +-----------------------+

   Note, AP is a fake hard register.  It will be eliminated in favor of
   SP or FP as appropriate.

   Note, Some or all of the stack sections above may be omitted if they
   are not needed.  */

/* Structure to be filled in by fr30_compute_frame_size() with register
   save masks, and offsets for the current function.  */
struct fr30_frame_info
{
  unsigned int total_size;	/* # Bytes that the entire frame takes up.  */
  unsigned int pretend_size;	/* # Bytes we push and pretend caller did.  */
  unsigned int args_size;	/* # Bytes that outgoing arguments take up.  */
  unsigned int reg_size;	/* # Bytes needed to store regs.  */
  unsigned int var_size;	/* # Bytes that variables take up.  */
  unsigned int frame_size;      /* # Bytes in current frame.  */
  unsigned int gmask;		/* Mask of saved registers.  */
  unsigned int save_fp;		/* Nonzero if frame pointer must be saved.  */
  unsigned int save_rp;		/* Nonzero if return pointer must be saved.  */
  int          initialised;	/* Nonzero if frame size already calculated.  */
};

/* Current frame information calculated by fr30_compute_frame_size().  */
static struct fr30_frame_info 	current_frame_info;

/* Zero structure to initialize current_frame_info.  */
static struct fr30_frame_info 	zero_frame_info;

static void fr30_setup_incoming_varargs (cumulative_args_t,
					 const function_arg_info &,
					 int *, int);
static bool fr30_must_pass_in_stack (const function_arg_info &);
static int fr30_arg_partial_bytes (cumulative_args_t,
				   const function_arg_info &);
static rtx fr30_function_arg (cumulative_args_t, const function_arg_info &);
static void fr30_function_arg_advance (cumulative_args_t,
				       const function_arg_info &);
static bool fr30_frame_pointer_required (void);
static rtx fr30_function_value (const_tree, const_tree, bool);
static rtx fr30_libcall_value (machine_mode, const_rtx);
static bool fr30_function_value_regno_p (const unsigned int);
static bool fr30_can_eliminate (const int, const int);
static void fr30_asm_trampoline_template (FILE *);
static void fr30_trampoline_init (rtx, tree, rtx);
static int fr30_num_arg_regs (const function_arg_info &);

#define FRAME_POINTER_MASK 	(1 << (FRAME_POINTER_REGNUM))
#define RETURN_POINTER_MASK 	(1 << (RETURN_POINTER_REGNUM))

/* Tell prologue and epilogue if register REGNO should be saved / restored.
   The return address and frame pointer are treated separately.
   Don't consider them here.  */
#define MUST_SAVE_REGISTER(regno)      \
  (   (regno) != RETURN_POINTER_REGNUM \
   && (regno) != FRAME_POINTER_REGNUM  \
   && df_regs_ever_live_p (regno)      \
   && ! call_used_or_fixed_reg_p (regno))

#define MUST_SAVE_FRAME_POINTER	 (df_regs_ever_live_p (FRAME_POINTER_REGNUM)  || frame_pointer_needed)
#define MUST_SAVE_RETURN_POINTER (df_regs_ever_live_p (RETURN_POINTER_REGNUM) || crtl->profile)

#if UNITS_PER_WORD == 4
#define WORD_ALIGN(SIZE) (((SIZE) + 3) & ~3)
#endif

/* Initialize the GCC target structure.  */
#undef  TARGET_ASM_ALIGNED_HI_OP
#define TARGET_ASM_ALIGNED_HI_OP "\t.hword\t"
#undef  TARGET_ASM_ALIGNED_SI_OP
#define TARGET_ASM_ALIGNED_SI_OP "\t.word\t"

#undef  TARGET_PROMOTE_PROTOTYPES
#define TARGET_PROMOTE_PROTOTYPES hook_bool_const_tree_true
#undef  TARGET_PASS_BY_REFERENCE
#define TARGET_PASS_BY_REFERENCE hook_pass_by_reference_must_pass_in_stack
#undef  TARGET_ARG_PARTIAL_BYTES
#define TARGET_ARG_PARTIAL_BYTES fr30_arg_partial_bytes
#undef  TARGET_FUNCTION_ARG
#define TARGET_FUNCTION_ARG fr30_function_arg
#undef  TARGET_FUNCTION_ARG_ADVANCE
#define TARGET_FUNCTION_ARG_ADVANCE fr30_function_arg_advance

#undef TARGET_FUNCTION_VALUE
#define TARGET_FUNCTION_VALUE fr30_function_value
#undef TARGET_LIBCALL_VALUE
#define TARGET_LIBCALL_VALUE fr30_libcall_value
#undef TARGET_FUNCTION_VALUE_REGNO_P
#define TARGET_FUNCTION_VALUE_REGNO_P fr30_function_value_regno_p

#undef  TARGET_SETUP_INCOMING_VARARGS
#define TARGET_SETUP_INCOMING_VARARGS fr30_setup_incoming_varargs
#undef  TARGET_MUST_PASS_IN_STACK
#define TARGET_MUST_PASS_IN_STACK fr30_must_pass_in_stack

#undef TARGET_FRAME_POINTER_REQUIRED
#define TARGET_FRAME_POINTER_REQUIRED fr30_frame_pointer_required

#undef TARGET_CAN_ELIMINATE
#define TARGET_CAN_ELIMINATE fr30_can_eliminate

#undef TARGET_LRA_P
#define TARGET_LRA_P hook_bool_void_false

#undef TARGET_ASM_TRAMPOLINE_TEMPLATE
#define TARGET_ASM_TRAMPOLINE_TEMPLATE fr30_asm_trampoline_template
#undef TARGET_TRAMPOLINE_INIT
#define TARGET_TRAMPOLINE_INIT fr30_trampoline_init

#undef TARGET_CONSTANT_ALIGNMENT
#define TARGET_CONSTANT_ALIGNMENT constant_alignment_word_strings

#undef  TARGET_HAVE_SPECULATION_SAFE_VALUE
#define TARGET_HAVE_SPECULATION_SAFE_VALUE speculation_safe_value_not_needed

struct gcc_target targetm = TARGET_INITIALIZER;


/* Worker function for TARGET_CAN_ELIMINATE.  */

bool
fr30_can_eliminate (const int from ATTRIBUTE_UNUSED, const int to)
{
  return (to == FRAME_POINTER_REGNUM || ! frame_pointer_needed);
}

/* Returns the number of bytes offset between FROM_REG and TO_REG
   for the current function.  As a side effect it fills in the
   current_frame_info structure, if the data is available.  */
unsigned int
fr30_compute_frame_size (int from_reg, int to_reg)
{
  int 		regno;
  unsigned int 	return_value;
  unsigned int	var_size;
  unsigned int	args_size;
  unsigned int	pretend_size;
  unsigned int 	reg_size;
  unsigned int 	gmask;

  var_size	= WORD_ALIGN (get_frame_size ());
  args_size	= WORD_ALIGN (crtl->outgoing_args_size);
  pretend_size	= crtl->args.pretend_args_size;

  reg_size	= 0;
  gmask		= 0;

  /* Calculate space needed for registers.  */
  for (regno = 0; regno < FIRST_PSEUDO_REGISTER; regno ++)
    {
      if (MUST_SAVE_REGISTER (regno))
	{
	  reg_size += UNITS_PER_WORD;
	  gmask |= 1 << regno;
	}
    }

  current_frame_info.save_fp = MUST_SAVE_FRAME_POINTER;
  current_frame_info.save_rp = MUST_SAVE_RETURN_POINTER;

  reg_size += (current_frame_info.save_fp + current_frame_info.save_rp)
	       * UNITS_PER_WORD;

  /* Save computed information.  */
  current_frame_info.pretend_size = pretend_size;
  current_frame_info.var_size     = var_size;
  current_frame_info.args_size    = args_size;
  current_frame_info.reg_size	  = reg_size;
  current_frame_info.frame_size   = args_size + var_size;
  current_frame_info.total_size   = args_size + var_size + reg_size + pretend_size;
  current_frame_info.gmask	  = gmask;
  current_frame_info.initialised  = reload_completed;

  /* Calculate the required distance.  */
  return_value = 0;

  if (to_reg == STACK_POINTER_REGNUM)
    return_value += args_size + var_size;

  if (from_reg == ARG_POINTER_REGNUM)
    return_value += reg_size;

  return return_value;
}

/* Called after register allocation to add any instructions needed for the
   prologue.  Using a prologue insn is favored compared to putting all of the
   instructions in output_function_prologue(), since it allows the scheduler
   to intermix instructions with the saves of the caller saved registers.  In
   some cases, it might be necessary to emit a barrier instruction as the last
   insn to prevent such scheduling.  */

void
fr30_expand_prologue (void)
{
  int regno;
  rtx insn;

  if (! current_frame_info.initialised)
    fr30_compute_frame_size (0, 0);

  /* This cases shouldn't happen.  Catch it now.  */
  gcc_assert (current_frame_info.total_size || !current_frame_info.gmask);

  /* Allocate space for register arguments if this is a variadic function.  */
  if (current_frame_info.pretend_size)
    {
      int regs_to_save = current_frame_info.pretend_size / UNITS_PER_WORD;

      /* Push argument registers into the pretend arg area.  */
      for (regno = FIRST_ARG_REGNUM + FR30_NUM_ARG_REGS; regno --, regs_to_save --;)
        {
	  insn = emit_insn (gen_movsi_push (gen_rtx_REG (Pmode, regno)));
	  RTX_FRAME_RELATED_P (insn) = 1;
	}
    }

  if (current_frame_info.gmask)
    {
      /* Save any needed call-saved regs.  */
      for (regno = STACK_POINTER_REGNUM; regno--;)
	{
	  if ((current_frame_info.gmask & (1 << regno)) != 0)
	    {
	      insn = emit_insn (gen_movsi_push (gen_rtx_REG (Pmode, regno)));
	      RTX_FRAME_RELATED_P (insn) = 1;
	    }
	}
    }

  /* Save return address if necessary.  */
  if (current_frame_info.save_rp)
    {
      insn = emit_insn (gen_movsi_push (gen_rtx_REG (Pmode,
      						     RETURN_POINTER_REGNUM)));
      RTX_FRAME_RELATED_P (insn) = 1;
    }

  /* Save old frame pointer and create new one, if necessary.  */
  if (current_frame_info.save_fp)
    {
      if (current_frame_info.frame_size < ((1 << 10) - UNITS_PER_WORD))
        {
	  int enter_size = current_frame_info.frame_size + UNITS_PER_WORD;
	  rtx pattern;

	  insn = emit_insn (gen_enter_func (GEN_INT (enter_size)));
          RTX_FRAME_RELATED_P (insn) = 1;

	  pattern = PATTERN (insn);

	  /* Also mark all 3 subexpressions as RTX_FRAME_RELATED_P. */
          if (GET_CODE (pattern) == PARALLEL)
            {
              int x;
              for (x = XVECLEN (pattern, 0); x--;)
		{
		  rtx part = XVECEXP (pattern, 0, x);

		  /* One of the insns in the ENTER pattern updates the
		     frame pointer.  If we do not actually need the frame
		     pointer in this function then this is a side effect
		     rather than a desired effect, so we do not mark that
		     insn as being related to the frame set up.  Doing this
		     allows us to compile the crash66.C test file in the
		     G++ testsuite.  */
		  if (! frame_pointer_needed
		      && GET_CODE (part) == SET
		      && SET_DEST (part) == hard_frame_pointer_rtx)
		    RTX_FRAME_RELATED_P (part) = 0;
		  else
		    RTX_FRAME_RELATED_P (part) = 1;
		}
            }
	}
      else
	{
	  insn = emit_insn (gen_movsi_push (frame_pointer_rtx));
          RTX_FRAME_RELATED_P (insn) = 1;

	  if (frame_pointer_needed)
	    {
	      insn = emit_insn (gen_movsi (frame_pointer_rtx, stack_pointer_rtx));
	      RTX_FRAME_RELATED_P (insn) = 1;
	    }
	}
    }

  /* Allocate the stack frame.  */
  if (current_frame_info.frame_size == 0)
    ; /* Nothing to do.  */
  else if (current_frame_info.save_fp
	   && current_frame_info.frame_size < ((1 << 10) - UNITS_PER_WORD))
    ; /* Nothing to do.  */
  else if (current_frame_info.frame_size <= 512)
    {
      insn = emit_insn (gen_add_to_stack
			 (GEN_INT (- (signed) current_frame_info.frame_size)));
      RTX_FRAME_RELATED_P (insn) = 1;
    }
  else
    {
      rtx tmp = gen_rtx_REG (Pmode, PROLOGUE_TMP_REGNUM);
      insn = emit_insn (gen_movsi (tmp, GEN_INT (current_frame_info.frame_size)));
      RTX_FRAME_RELATED_P (insn) = 1;
      insn = emit_insn (gen_subsi3 (stack_pointer_rtx, stack_pointer_rtx, tmp));
      RTX_FRAME_RELATED_P (insn) = 1;
    }

  if (crtl->profile)
    emit_insn (gen_blockage ());
}

/* Called after register allocation to add any instructions needed for the
   epilogue.  Using an epilogue insn is favored compared to putting all of the
   instructions in output_function_epilogue(), since it allows the scheduler
   to intermix instructions with the restores of the caller saved registers.
   In some cases, it might be necessary to emit a barrier instruction as the
   first insn to prevent such scheduling.  */
void
fr30_expand_epilogue (void)
{
  int regno;

  /* Perform the inversion operations of the prologue.  */
  gcc_assert (current_frame_info.initialised);

  /* Pop local variables and arguments off the stack.
     If frame_pointer_needed is TRUE then the frame pointer register
     has actually been used as a frame pointer, and we can recover
     the stack pointer from it, otherwise we must unwind the stack
     manually.  */
  if (current_frame_info.frame_size > 0)
    {
      if (current_frame_info.save_fp && frame_pointer_needed)
	{
	  emit_insn (gen_leave_func ());
	  current_frame_info.save_fp = 0;
	}
      else if (current_frame_info.frame_size <= 508)
	emit_insn (gen_add_to_stack
		   (GEN_INT (current_frame_info.frame_size)));
      else
	{
	  rtx tmp = gen_rtx_REG (Pmode, PROLOGUE_TMP_REGNUM);
	  emit_insn (gen_movsi (tmp, GEN_INT (current_frame_info.frame_size)));
	  emit_insn (gen_addsi3 (stack_pointer_rtx, stack_pointer_rtx, tmp));
	}
    }

  if (current_frame_info.save_fp)
    emit_insn (gen_movsi_pop (frame_pointer_rtx));

  /* Pop all the registers that were pushed.  */
  if (current_frame_info.save_rp)
    emit_insn (gen_movsi_pop (gen_rtx_REG (Pmode, RETURN_POINTER_REGNUM)));

  for (regno = 0; regno < STACK_POINTER_REGNUM; regno ++)
    if (current_frame_info.gmask & (1 << regno))
      emit_insn (gen_movsi_pop (gen_rtx_REG (Pmode, regno)));

  if (current_frame_info.pretend_size)
    emit_insn (gen_add_to_stack (GEN_INT (current_frame_info.pretend_size)));

  /* Reset state info for each function.  */
  current_frame_info = zero_frame_info;

  emit_jump_insn (gen_return_from_func ());
}

/* Do any needed setup for a variadic function.  We must create a register
   parameter block, and then copy any anonymous arguments, plus the last
   named argument, from registers into memory.  * copying actually done in
   fr30_expand_prologue().

   CUM has not been updated for the last named argument which has type TYPE
   and mode MODE, and we rely on this fact.  */
void
fr30_setup_incoming_varargs (cumulative_args_t arg_regs_used_so_far_v,
			     const function_arg_info &arg,
			     int *pretend_size,
			     int second_time ATTRIBUTE_UNUSED)
{
  CUMULATIVE_ARGS *arg_regs_used_so_far
    = get_cumulative_args (arg_regs_used_so_far_v);
  int size;

  if (!TYPE_NO_NAMED_ARGS_STDARG_P (TREE_TYPE (current_function_decl)))
    {
      /* All BLKmode values are passed by reference.  */
      gcc_assert (arg.mode != BLKmode);

      /* ??? This run-time test as well as the code inside the if
	 statement is probably unnecessary.  */
      if (targetm.calls.strict_argument_naming (arg_regs_used_so_far_v))
	/* If TARGET_STRICT_ARGUMENT_NAMING returns true, then the last named
	   arg must not be treated as an anonymous arg.  */
	/* ??? This is a pointer increment, which makes no sense.  */
	arg_regs_used_so_far += fr30_num_arg_regs (arg);
    }

  size = FR30_NUM_ARG_REGS - (* arg_regs_used_so_far);

  if (size <= 0)
    return;

  * pretend_size = (size * UNITS_PER_WORD);
}

/*}}}*/
/*{{{  Printing operands */

/* Print a memory address as an operand to reference that memory location.  */

void
fr30_print_operand_address (FILE *stream, rtx address)
{
  switch (GET_CODE (address))
    {
    case SYMBOL_REF:
      output_addr_const (stream, address);
      break;

    default:
      fprintf (stderr, "code = %x\n", GET_CODE (address));
      debug_rtx (address);
      output_operand_lossage ("fr30_print_operand_address: unhandled address");
      break;
    }
}

/* Print an operand.  */

void
fr30_print_operand (FILE *file, rtx x, int code)
{
  rtx x0;

  switch (code)
    {
    case '#':
      /* Output a :D if this instruction is delayed.  */
      if (dbr_sequence_length () != 0)
	fputs (":D", file);
      return;

    case 'p':
      /* Compute the register name of the second register in a hi/lo
	 register pair.  */
      if (GET_CODE (x) != REG)
	output_operand_lossage ("fr30_print_operand: unrecognized %%p code");
      else
	fprintf (file, "r%d", REGNO (x) + 1);
      return;

    case 'b':
      /* Convert GCC's comparison operators into FR30 comparison codes.  */
      switch (GET_CODE (x))
	{
	case EQ:  fprintf (file, "eq"); break;
	case NE:  fprintf (file, "ne"); break;
	case LT:  fprintf (file, "lt"); break;
	case LE:  fprintf (file, "le"); break;
	case GT:  fprintf (file, "gt"); break;
	case GE:  fprintf (file, "ge"); break;
	case LTU: fprintf (file, "c"); break;
	case LEU: fprintf (file, "ls"); break;
	case GTU: fprintf (file, "hi"); break;
	case GEU: fprintf (file, "nc");  break;
	default:
	  output_operand_lossage ("fr30_print_operand: unrecognized %%b code");
	  break;
	}
      return;

    case 'B':
      /* Convert GCC's comparison operators into the complimentary FR30
	 comparison codes.  */
      switch (GET_CODE (x))
	{
	case EQ:  fprintf (file, "ne"); break;
	case NE:  fprintf (file, "eq"); break;
	case LT:  fprintf (file, "ge"); break;
	case LE:  fprintf (file, "gt"); break;
	case GT:  fprintf (file, "le"); break;
	case GE:  fprintf (file, "lt"); break;
	case LTU: fprintf (file, "nc"); break;
	case LEU: fprintf (file, "hi"); break;
	case GTU: fprintf (file, "ls"); break;
	case GEU: fprintf (file, "c"); break;
	default:
	  output_operand_lossage ("fr30_print_operand: unrecognized %%B code");
	  break;
	}
      return;

    case 'A':
      /* Print a signed byte value as an unsigned value.  */
      if (GET_CODE (x) != CONST_INT)
	output_operand_lossage ("fr30_print_operand: invalid operand to %%A code");
      else
	{
	  HOST_WIDE_INT val;

	  val = INTVAL (x);

	  val &= 0xff;

	  fprintf (file, HOST_WIDE_INT_PRINT_DEC, val);
	}
      return;

    case 'x':
      if (GET_CODE (x) != CONST_INT
	  || INTVAL (x) < 16
	  || INTVAL (x) > 32)
	output_operand_lossage ("fr30_print_operand: invalid %%x code");
      else
	fprintf (file, HOST_WIDE_INT_PRINT_DEC, INTVAL (x) - 16);
      return;

    case 'F':
      if (GET_CODE (x) != CONST_DOUBLE)
	output_operand_lossage ("fr30_print_operand: invalid %%F code");
      else
	{
	  char str[30];

	  real_to_decimal (str, CONST_DOUBLE_REAL_VALUE (x),
			   sizeof (str), 0, 1);
	  fputs (str, file);
	}
      return;

    case 0:
      /* Handled below.  */
      break;

    default:
      fprintf (stderr, "unknown code = %x\n", code);
      output_operand_lossage ("fr30_print_operand: unknown code");
      return;
    }

  switch (GET_CODE (x))
    {
    case REG:
      fputs (reg_names [REGNO (x)], file);
      break;

    case MEM:
      x0 = XEXP (x,0);

      switch (GET_CODE (x0))
	{
	case REG:
	  gcc_assert ((unsigned) REGNO (x0) < ARRAY_SIZE (reg_names));
	  fprintf (file, "@%s", reg_names [REGNO (x0)]);
	  break;

	case PLUS:
	  if (GET_CODE (XEXP (x0, 0)) != REG
	      || REGNO (XEXP (x0, 0)) < FRAME_POINTER_REGNUM
	      || REGNO (XEXP (x0, 0)) > STACK_POINTER_REGNUM
	      || GET_CODE (XEXP (x0, 1)) != CONST_INT)
	    {
	      fprintf (stderr, "bad INDEXed address:");
	      debug_rtx (x);
	      output_operand_lossage ("fr30_print_operand: unhandled MEM");
	    }
	  else if (REGNO (XEXP (x0, 0)) == FRAME_POINTER_REGNUM)
	    {
	      HOST_WIDE_INT val = INTVAL (XEXP (x0, 1));
	      if (val < -(1 << 9) || val > ((1 << 9) - 4))
		{
		  fprintf (stderr, "frame INDEX out of range:");
		  debug_rtx (x);
		  output_operand_lossage ("fr30_print_operand: unhandled MEM");
		}
	      fprintf (file, "@(r14, #" HOST_WIDE_INT_PRINT_DEC ")", val);
	    }
	  else
	    {
	      HOST_WIDE_INT val = INTVAL (XEXP (x0, 1));
	      if (val < 0 || val > ((1 << 6) - 4))
		{
		  fprintf (stderr, "stack INDEX out of range:");
		  debug_rtx (x);
		  output_operand_lossage ("fr30_print_operand: unhandled MEM");
		}
	      fprintf (file, "@(r15, #" HOST_WIDE_INT_PRINT_DEC ")", val);
	    }
	  break;

	case SYMBOL_REF:
	  output_address (VOIDmode, x0);
	  break;

	default:
	  fprintf (stderr, "bad MEM code = %x\n", GET_CODE (x0));
	  debug_rtx (x);
	  output_operand_lossage ("fr30_print_operand: unhandled MEM");
	  break;
	}
      break;

    case CONST_DOUBLE :
      /* We handle SFmode constants here as output_addr_const doesn't.  */
      if (GET_MODE (x) == SFmode)
	{
	  long l;

	  REAL_VALUE_TO_TARGET_SINGLE (*CONST_DOUBLE_REAL_VALUE (x), l);
	  fprintf (file, "0x%08lx", l);
	  break;
	}

      /* FALLTHRU */
      /* Let output_addr_const deal with it.  */
    default:
      output_addr_const (file, x);
      break;
    }

  return;
}

/*}}}*/

/* Implements TARGET_FUNCTION_VALUE.  */

static rtx
fr30_function_value (const_tree valtype,
		     const_tree fntype_or_decli ATTRIBUTE_UNUSED,
		     bool outgoing ATTRIBUTE_UNUSED)
{
  return gen_rtx_REG (TYPE_MODE (valtype), RETURN_VALUE_REGNUM);
}

/* Implements TARGET_LIBCALL_VALUE.  */

static rtx
fr30_libcall_value (machine_mode mode,
		    const_rtx fun ATTRIBUTE_UNUSED)
{
  return gen_rtx_REG (mode, RETURN_VALUE_REGNUM);
}

/* Implements TARGET_FUNCTION_VALUE_REGNO_P.  */

static bool
fr30_function_value_regno_p (const unsigned int regno)
{
  return (regno == RETURN_VALUE_REGNUM);
}

/*{{{  Function arguments */

/* Return true if we should pass an argument on the stack rather than
   in registers.  */

static bool
fr30_must_pass_in_stack (const function_arg_info &arg)
{
  return arg.mode == BLKmode || arg.aggregate_type_p ();
}

/* Compute the number of word sized registers needed to hold function
   argument ARG.  */
static int
fr30_num_arg_regs (const function_arg_info &arg)
{
  if (targetm.calls.must_pass_in_stack (arg))
    return 0;

  int size = arg.promoted_size_in_bytes ();
  return (size + UNITS_PER_WORD - 1) / UNITS_PER_WORD;
}

/* Returns the number of bytes of argument registers required to hold *part*
   of argument ARG.  If the argument fits entirely in the argument registers,
   or entirely on the stack, then 0 is returned.  CUM is the number of
   argument registers already used by earlier parameters to the function.  */

static int
fr30_arg_partial_bytes (cumulative_args_t cum_v, const function_arg_info &arg)
{
  CUMULATIVE_ARGS *cum = get_cumulative_args (cum_v);

  /* Unnamed arguments, i.e. those that are prototyped as ...
     are always passed on the stack.
     Also check here to see if all the argument registers are full.  */
  if (!arg.named || *cum >= FR30_NUM_ARG_REGS)
    return 0;

  /* Work out how many argument registers would be needed if this
     parameter were to be passed entirely in registers.  If there
     are sufficient argument registers available (or if no registers
     are needed because the parameter must be passed on the stack)
     then return zero, as this parameter does not require partial
     register, partial stack space.  */
  if (*cum + fr30_num_arg_regs (arg) <= FR30_NUM_ARG_REGS)
    return 0;

  return (FR30_NUM_ARG_REGS - *cum) * UNITS_PER_WORD;
}

static rtx
fr30_function_arg (cumulative_args_t cum_v, const function_arg_info &arg)
{
  CUMULATIVE_ARGS *cum = get_cumulative_args (cum_v);

  if (!arg.named
      || fr30_must_pass_in_stack (arg)
      || *cum >= FR30_NUM_ARG_REGS)
    return NULL_RTX;
  else
    return gen_rtx_REG (arg.mode, *cum + FIRST_ARG_REGNUM);
}

/* Implement TARGET_FUNCTION_ARG_ADVANCE.  */
static void
fr30_function_arg_advance (cumulative_args_t cum,
			   const function_arg_info &arg)
{
  if (arg.named)
    *get_cumulative_args (cum) += fr30_num_arg_regs (arg);
}

/*}}}*/
/*{{{  Operand predicates */

#ifndef Mmode
#define Mmode machine_mode
#endif

/* Returns true iff all the registers in the operands array
   are in descending or ascending order.  */
int
fr30_check_multiple_regs (rtx *operands, int num_operands, int descending)
{
  if (descending)
    {
      unsigned int prev_regno = 0;

      while (num_operands --)
	{
	  if (GET_CODE (operands [num_operands]) != REG)
	    return 0;

	  if (REGNO (operands [num_operands]) < prev_regno)
	    return 0;

	  prev_regno = REGNO (operands [num_operands]);
	}
    }
  else
    {
      unsigned int prev_regno = CONDITION_CODE_REGNUM;

      while (num_operands --)
	{
	  if (GET_CODE (operands [num_operands]) != REG)
	    return 0;

	  if (REGNO (operands [num_operands]) > prev_regno)
	    return 0;

	  prev_regno = REGNO (operands [num_operands]);
	}
    }

  return 1;
}

int
fr30_const_double_is_zero (rtx operand)
{
  if (operand == NULL || GET_CODE (operand) != CONST_DOUBLE)
    return 0;

  return real_equal (CONST_DOUBLE_REAL_VALUE (operand), &dconst0);
}

/*}}}*/
/*{{{  Instruction Output Routines  */

/* Output a double word move.
   It must be REG<-REG, REG<-MEM, MEM<-REG or REG<-CONST.
   On the FR30 we are constrained by the fact that it does not
   support offsetable addresses, and so we have to load the
   address of the secnd word into the second destination register
   before we can use it.  */

rtx
fr30_move_double (rtx * operands)
{
  rtx src  = operands[1];
  rtx dest = operands[0];
  enum rtx_code src_code = GET_CODE (src);
  enum rtx_code dest_code = GET_CODE (dest);
  machine_mode mode = GET_MODE (dest);
  rtx val;

  start_sequence ();

  if (dest_code == REG)
    {
      if (src_code == REG)
	{
	  int reverse = (REGNO (dest) == REGNO (src) + 1);

	  /* We normally copy the low-numbered register first.  However, if
	     the first register of operand 0 is the same as the second register
	     of operand 1, we must copy in the opposite order.  */
	  emit_insn (gen_rtx_SET (operand_subword (dest, reverse, TRUE, mode),
				  operand_subword (src,  reverse, TRUE, mode)));

	  emit_insn
	    (gen_rtx_SET (operand_subword (dest, !reverse, TRUE, mode),
			  operand_subword (src,  !reverse, TRUE, mode)));
	}
      else if (src_code == MEM)
	{
	  rtx addr = XEXP (src, 0);
	  rtx dest0 = operand_subword (dest, 0, TRUE, mode);
	  rtx dest1 = operand_subword (dest, 1, TRUE, mode);
	  rtx new_mem;

	  gcc_assert (GET_CODE (addr) == REG);

	  /* Copy the address before clobbering it.  See PR 34174.  */
	  emit_insn (gen_rtx_SET (dest1, addr));
	  emit_insn (gen_rtx_SET (dest0, adjust_address (src, SImode, 0)));
	  emit_insn (gen_rtx_SET (dest1, plus_constant (SImode, dest1,
							UNITS_PER_WORD)));

	  new_mem = gen_rtx_MEM (SImode, dest1);
	  MEM_COPY_ATTRIBUTES (new_mem, src);

	  emit_insn (gen_rtx_SET (dest1, new_mem));
	}
      else if (src_code == CONST_INT || src_code == CONST_DOUBLE)
	{
	  rtx words[2];
	  split_double (src, &words[0], &words[1]);
	  emit_insn (gen_rtx_SET (operand_subword (dest, 0, TRUE, mode),
				  words[0]));

	  emit_insn (gen_rtx_SET (operand_subword (dest, 1, TRUE, mode),
				  words[1]));
	}
    }
  else if (src_code == REG && dest_code == MEM)
    {
      rtx addr = XEXP (dest, 0);
      rtx src0;
      rtx src1;

      gcc_assert (GET_CODE (addr) == REG);

      src0 = operand_subword (src, 0, TRUE, mode);
      src1 = operand_subword (src, 1, TRUE, mode);

      emit_move_insn (adjust_address (dest, SImode, 0), src0);

      if (REGNO (addr) == STACK_POINTER_REGNUM
	  || REGNO (addr) == FRAME_POINTER_REGNUM)
	emit_insn (gen_rtx_SET (adjust_address (dest, SImode, UNITS_PER_WORD),
				src1));
      else
	{
	  rtx new_mem;
	  rtx scratch_reg_r0 = gen_rtx_REG (SImode, 0);

	  /* We need a scratch register to hold the value of 'address + 4'.
	     We use r0 for this purpose. It is used for example for long
	     jumps and is already marked to not be used by normal register
	     allocation.  */
	  emit_insn (gen_movsi_internal (scratch_reg_r0, addr));
	  emit_insn (gen_addsi_small_int (scratch_reg_r0, scratch_reg_r0,
					  GEN_INT (UNITS_PER_WORD)));
	  new_mem = gen_rtx_MEM (SImode, scratch_reg_r0);
	  MEM_COPY_ATTRIBUTES (new_mem, dest);
	  emit_move_insn (new_mem, src1);
	  emit_insn (gen_blockage ());
	}
    }
  else
    /* This should have been prevented by the constraints on movdi_insn.  */
    gcc_unreachable ();

  val = get_insns ();
  end_sequence ();

  return val;
}

/* Implement TARGET_FRAME_POINTER_REQUIRED.  */

bool
fr30_frame_pointer_required (void)
{
  return (flag_omit_frame_pointer == 0 || crtl->args.pretend_args_size > 0);
}

/*}}}*/
/*{{{  Trampoline Output Routines  */

/* Implement TARGET_ASM_TRAMPOLINE_TEMPLATE.
   On the FR30, the trampoline is:

   nop
   ldi:32 STATIC, r12
   nop
   ldi:32 FUNCTION, r0
   jmp    @r0

   The no-ops are to guarantee that the static chain and final
   target are 32 bit aligned within the trampoline.  That allows us to
   initialize those locations with simple SImode stores.   The alternative
   would be to use HImode stores.  */

static void
fr30_asm_trampoline_template (FILE *f)
{
  fprintf (f, "\tnop\n");
  fprintf (f, "\tldi:32\t#0, %s\n", reg_names [STATIC_CHAIN_REGNUM]);
  fprintf (f, "\tnop\n");
  fprintf (f, "\tldi:32\t#0, %s\n", reg_names [COMPILER_SCRATCH_REGISTER]);
  fprintf (f, "\tjmp\t@%s\n", reg_names [COMPILER_SCRATCH_REGISTER]);
}

/* Implement TARGET_TRAMPOLINE_INIT.  */

static void
fr30_trampoline_init (rtx m_tramp, tree fndecl, rtx chain_value)
{
  rtx fnaddr = XEXP (DECL_RTL (fndecl), 0);
  rtx mem;

  emit_block_move (m_tramp, assemble_trampoline_template (),
		   GEN_INT (TRAMPOLINE_SIZE), BLOCK_OP_NORMAL);

  mem = adjust_address (m_tramp, SImode, 4);
  emit_move_insn (mem, chain_value);
  mem = adjust_address (m_tramp, SImode, 12);
  emit_move_insn (mem, fnaddr);
}

/*}}}*/
/* Local Variables: */
/* folded-file: t   */
/* End:		    */
