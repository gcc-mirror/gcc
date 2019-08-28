#define IN_TARGET_CODE 1

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "memmodel.h"
#include "backend.h"
#include "target.h"
#include "rtl.h"
#include "tree.h"
#include "cfghooks.h"
#include "df.h"
#include "tm_p.h"
#include "stringpool.h"
#include "attribs.h"
#include "optabs.h"
#include "regs.h"
#include "emit-rtl.h"
#include "recog.h"
#include "diagnostic.h"
#include "fold-const.h"
#include "varasm.h"
#include "stor-layout.h"
#include "calls.h"
#include "output.h"
#include "insn-attr.h"
#include "flags.h"
#include "explow.h"
#include "expr.h"
#include "langhooks.h"
#include "tm-constrs.h"
#include "cfgrtl.h"
#include "tree-pass.h"
#include "context.h"
#include "builtins.h"
#include "rtl-iter.h"
#include "alias.h"
#include "opts.h"
#include "dwarf2.h"
#include "hw-doloop.h"

/* This file should be included last.  */
#include "target-def.h"

/* Return true if REGNO is suited for short instructions.  */
#define COMPACT_REG_P(REGNO)						\
  (((signed)(REGNO) >= R0_REGNUM && (REGNO) <= R3_REGNUM)		\
   || ((REGNO) >= R12_REGNUM && (REGNO) <= R15_REGNUM))

/* Use ARC64_LPIC only if dealing with 64-bit variant of arc64.  */
#define ARC64_MAYBE_LPIC (TARGET_64BIT ? ARC64_LPIC : ARC64_PIC)
#define ARC64_MAYBE_LARGE (TARGET_64BIT ? ARC64_LARGE : ARC64_LO32)

/* Maximum size of a loop.  */
#define MAX_LOOP_LENGTH 4094
#define MIN_LOOP_LENGTH -4092

#define UNITS_PER_LIMM 4

#define DOUBLE_LOAD_STORE ((!TARGET_64BIT && TARGET_LL64) \
			   || (TARGET_64BIT && TARGET_WIDE_LDST))

/* Logic:

   HS5x (32-bit arch):
     - no 64-bit loads and stores   -> 32-bit moves
       - use_fpu && fpu_exists      ->   fpr
       - else			    ->   gpr
     - 64-bit loads and stores      -> 64-bit moves
       - use_fpu && fpu{s,d}_exists ->   fpr
       - else			    ->   gpr

   HS6x (64-bit arch):
     - no 128-bit loads and stores  -> 64-bit moves
       - use_fpu && fpu_exists      ->   fpr
       - else			    ->   gpr
     - 128-bit loads and stores     -> 128-bit moves
       - use_fpu && fpud_exists     ->   fpr
       - else			    ->   gpr.  */

static machine_mode cpymem_copy_mode (void)
{
  /* HS6x.  */
  if (TARGET_64BIT)
    {
      if (!TARGET_WIDE_LDST)
	{
	  if (TARGET_FP_MOVE && ARC64_HAS_FPUD)
	    return DFmode;
	  else if (TARGET_FP_MOVE && ARC64_VFP_64)
	    return V2SFmode;

	  return DImode;
	}

      if (TARGET_FP_MOVE)
	{
	  if (ARC64_VFP_128)
	    return V2DFmode;
	}

      return TImode;
    }
  /* HS5x.  */
  else
    {
      if (!TARGET_LL64)
	{
	  if (TARGET_FP_MOVE && ARC64_HAS_FPUS)
	    return SFmode;

	  return SImode;
	}

      if (TARGET_FP_MOVE)
	{
	  /* ARC64_VFP_64 does not cover all cases YET.  */
	  if (ARC64_VFP_64)
	    return DFmode;
	}

      return DImode;
    }
}

#define ARC_INVERSE_CONDITION_CODE(X)  ((X) ^ 1)

/* Implement REGNO_REG_CLASS.  */
const enum reg_class arc64_regno_to_regclass[FIRST_PSEUDO_REGISTER] =
  {
   AC16_REGS, AC16_REGS, AC16_REGS, AC16_REGS,
   CORE_REGS, CORE_REGS, CORE_REGS, CORE_REGS,
   CORE_REGS, CORE_REGS, CORE_REGS, CORE_REGS,
   AC16_REGS, AC16_REGS, AC16_REGS, AC16_REGS,
   CORE_REGS, CORE_REGS, CORE_REGS, CORE_REGS,
   CORE_REGS, CORE_REGS, CORE_REGS, CORE_REGS,
   CORE_REGS, CORE_REGS, CORE_REGS, CORE_REGS,
   CORE_REGS, NO_REGS,   CORE_REGS, CORE_REGS,

   NO_REGS, NO_REGS, NO_REGS, NO_REGS,
   NO_REGS, NO_REGS, NO_REGS, NO_REGS,
   NO_REGS, NO_REGS, NO_REGS, NO_REGS,
   NO_REGS, NO_REGS, NO_REGS, NO_REGS,

   NO_REGS, NO_REGS, NO_REGS, NO_REGS,
   NO_REGS, NO_REGS, NO_REGS, NO_REGS,
   NO_REGS, NO_REGS, GENERAL_REGS, GENERAL_REGS,
   NO_REGS, NO_REGS, NO_REGS, NO_REGS,

   FP_REGS, FP_REGS, FP_REGS, FP_REGS,
   FP_REGS, FP_REGS, FP_REGS, FP_REGS,
   FP_REGS, FP_REGS, FP_REGS, FP_REGS,
   FP_REGS, FP_REGS, FP_REGS, FP_REGS,
   FP_REGS, FP_REGS, FP_REGS, FP_REGS,
   FP_REGS, FP_REGS, FP_REGS, FP_REGS,
   FP_REGS, FP_REGS, FP_REGS, FP_REGS,
   FP_REGS, FP_REGS, FP_REGS, FP_REGS,

   GENERAL_REGS, GENERAL_REGS, NO_REGS,
  };

enum arc_cc_code_index
{
  ARC_CC_AL, ARC_CC_EQ = ARC_CC_AL+2, ARC_CC_NE, ARC_CC_P, ARC_CC_N,
  ARC_CC_C,  ARC_CC_NC, ARC_CC_V, ARC_CC_NV,
  ARC_CC_GT, ARC_CC_LE, ARC_CC_GE, ARC_CC_LT, ARC_CC_HI, ARC_CC_LS, ARC_CC_PNZ,
  ARC_CC_LO = ARC_CC_C, ARC_CC_HS = ARC_CC_NC
};

typedef enum arc64_symb_type
{
  ARC64_UNK = 0, ARC64_LO32, ARC64_LARGE, ARC64_PIC, ARC64_LPIC, ARC64_TLS,
  ARC64_PCREL
} arc64_symb;

/* Information about single argument.  */
struct arc64_arg_info {
  /* Number of integer registers allocated to this argument.  */
  unsigned int ngpr;
  /* Number of floating-point registers allocated to this argument.  */
  unsigned int nfpr;

  /* Offset.  */
  unsigned int off_gpr;
  unsigned int off_fpr;

  /* Goes on stack.  */
  bool stack_p;
};

/* Frame and machine specific info.  */

struct GTY (()) arc64_frame
{
  HOST_WIDE_INT reg_offset[FIRST_PSEUDO_REGISTER];

  /* The size of the saved callee-save int/FP registers. */
  HOST_WIDE_INT saved_regs_size;

  /* The number of extra stack bytes taken up by register varargs.
     This area is allocated by the callee at the very top of the
     frame.  This value is rounded up to a multiple of
     STACK_BOUNDARY.  */
  HOST_WIDE_INT saved_varargs_size;

  HOST_WIDE_INT saved_outargs_size;

  HOST_WIDE_INT saved_locals_size;

  /* The size of the frame.  This value is the offset from base of the
     frame (incomming SP) to the stack_pointer.  This value is always
     a multiple of STACK_BOUNDARY.  */
  HOST_WIDE_INT frame_size;

  bool layout_p;
};


/* ARC64 function types.   */
enum arc_function_type {
  /* No function should have the unknown type.  This value is used to
   indicate the that function type has not yet been computed.  */
  ARC64_FUNCTION_UNKNOWN  = 0,

  /* The normal function type indicates that the function has the
   standard prologue and epilogue.  */
  ARC64_FUNCTION_NORMAL  = 1L << 0,

  /* These are interrupt handlers.  The name corresponds to the register
     name that contains the return address.  */
  ARC64_FUNCTION_ILINK   = 1L << 1,

  /* The naked function type indicates that the function does not have
   prologue or epilogue, and that no stack frame is available.  */
  ARC64_FUNCTION_NAKED   = 1L << 2
};

/* Check if a function is an interrupt function.  */
#define ARC_INTERRUPT_P(TYPE)  (((TYPE) & ARC64_FUNCTION_ILINK) != 0)

/* Check if a function is normal, that is, has standard prologue and
   epilogue.  */
#define ARC_NORMAL_P(TYPE) (((TYPE) & ARC64_FUNCTION_NORMAL) != 0)

/* Check if a function is naked.  */
#define ARC_NAKED_P(TYPE) (((TYPE) & ARC64_FUNCTION_NAKED) != 0)

typedef struct GTY (()) machine_function
{
  struct arc64_frame frame;
  /* Record if the function has a variable argument list.  */
  int uses_anonymous_args;
  /* Record if the type of the current function.  */
  unsigned int fn_type;
} machine_function;

/* IDs for all the ARC builtins.  */

enum arc64_builtin_id
  {
#define DEF_BUILTIN(NAME, N_ARGS, TYPE, ICODE, MASK)	\
    ARC64_BUILTIN_ ## NAME,
#include "builtins.def"
#undef DEF_BUILTIN

    ARC64_BUILTIN_COUNT
  };

struct GTY(()) arc64_builtin_description
{
  enum insn_code icode;
  int n_args;
  tree fndecl;
};

static GTY(()) struct arc64_builtin_description
arc_bdesc[ARC64_BUILTIN_COUNT] =
{
#define DEF_BUILTIN(NAME, N_ARGS, TYPE, ICODE, MASK)		\
  { (enum insn_code) CODE_FOR_ ## ICODE, N_ARGS, NULL_TREE },
#include "builtins.def"
#undef DEF_BUILTIN
};

/* vec_perm support.  */
struct e_vec_perm_d
{
  rtx target, op0, op1;
  vec_perm_indices perm;
  machine_mode vmode;
  bool one_vector_p;
  bool testing_p;
};

static tree arc64_fndecl_attribute (tree *, tree, tree, int, bool *);
static tree arc64_interrupt_attribute (tree *, tree, tree, int, bool *);

/* { name, min_len, max_len, decl_req, type_req, fn_type_req,
   affects_type_identity, handler, exclude } */
const struct attribute_spec arc64_attribute_table[] =
{
  /* Functions which are used for ISR, return address is using ILINK reg.  */
  { "interrupt", 0, 1, false, true, true, false, arc64_interrupt_attribute,
    NULL },

  /* Function which are not having the prologue and epilogue generated
     by the compiler.  */
  { "naked", 0, 0, true, false, false,  false, arc64_fndecl_attribute,
    NULL },

  { NULL, 0, 0, false, false, false, false, NULL, NULL }
};

/* Local variable true if we output scalled address.  */
static bool scalled_p = false;
/* Simple LUT for log2.  */
static const int lutlog2[] = {0, 0, 1, 0, 2, 0, 0, 0,
			      3, 0, 0, 0, 0, 0, 0, 0 };

/* Safe access lut log2 table.  */
#define ARC64LOG2(X) (((X) > 15) ? 3 : lutlog2[((X) & 0x0f)])

/* Check if an offset is scalled.  */
#define ARC64_CHECK_SCALLED_IMMEDIATE(offset, mode)			\
  (ARC64LOG2 (GET_MODE_SIZE (mode))					\
   && VERIFY_SHIFT (INTVAL (offset), ARC64LOG2 (GET_MODE_SIZE (mode)))	\
   && SIGNED_INT9 (INTVAL (offset) >> ARC64LOG2 (GET_MODE_SIZE (mode))))

/* ALIGN FRAMES on word boundaries.  */
#define ARC64_STACK_ALIGN(LOC)						\
  (((LOC) + STACK_BOUNDARY / BITS_PER_UNIT - 1) & -STACK_BOUNDARY/BITS_PER_UNIT)


/* Callback function used for function attributes.  */

static tree
arc64_fndecl_attribute (tree *node, tree name,
			tree args ATTRIBUTE_UNUSED,
			int flags ATTRIBUTE_UNUSED, bool *no_add_attrs)
{
  if (TREE_CODE (*node) != FUNCTION_DECL)
    {
      warning (OPT_Wattributes, "%qE attribute only applies to functions",
	       name);
      *no_add_attrs = true;
    }

  return NULL_TREE;
}

/* Handle an "interrupt" attribute; arguments as in
   struct attribute_spec.handler.  */

static tree
arc64_interrupt_attribute (tree *, tree name, tree args, int,
			   bool *no_add_attrs)
{

  if (is_attribute_p ("interrupt", name))
    {
      if (args)
	{
	  tree value = TREE_VALUE (args);

	  if (TREE_CODE (value) != STRING_CST)
	    {
	      warning (OPT_Wattributes,
		       "argument of %qE attribute is not a string constant",
		       name);
	      *no_add_attrs = true;
	    }
	  else if (strcmp (TREE_STRING_POINTER (value), "ilink"))
	    {
	      warning (OPT_Wattributes,
		       "argument of %qE attribute is not \"ilink\"",
		       name);
	      *no_add_attrs = true;
	    }
	}
    }
  return NULL_TREE;
}

/* ARC64 stack frame generated by this compiler looks like:

	+-------------------------------+
	|                               |
	|  incoming stack arguments     |
	|                               |
	+-------------------------------+ <-- incoming stack pointer (aligned)
	|                               |
	|  callee-allocated save area   |
	|  for register varargs         |
	|                               |
	+-------------------------------+ <-- arg_pointer_rtx
	|                               |
	|  GPR save area                |
	|                               |
	+-------------------------------+
	|  Return address register      |
	|  (if required)                |
	+-------------------------------+
	|  FP (if required)             |
	+-------------------------------+ <-- (hard) frame_pointer_rtx
	|                               |
	|  Local variables              |
	|                               |
	+-------------------------------+
	|  outgoing stack arguments     |
	|                               |
	+-------------------------------+ <-- stack_pointer_rtx (aligned)

  Dynamic stack allocations such as alloca insert data after local
  variables.  */

/* Return TRUE if a register needs to be saved, exception making
   BLINK, and FP registers.  BLINK is never check by this routine,
   while FP is only checked if `frame_pointer_required` is FALSE.  */

static bool
arc64_save_reg_p (int regno)
{
  bool call_saved;
  bool might_clobber;
  bool eh_needed;

  gcc_assert (regno <= F31_REGNUM);
  gcc_assert (regno >= R0_REGNUM);

  switch (regno)
    {
    case R60_REGNUM:
    case R61_REGNUM:
    case R62_REGNUM:
    case R63_REGNUM:
    case ILINK_REGNUM:
    case BLINK_REGNUM:
    case SP_REGNUM:
      /* Special registers, they are handled separately.  */
      return false;

    case R27_REGNUM:
      if (frame_pointer_needed)
	return false;
      break;

    case F0_REGNUM:
    case F1_REGNUM:
    case F2_REGNUM:
    case F3_REGNUM:
    case F4_REGNUM:
    case F5_REGNUM:
    case F6_REGNUM:
    case F7_REGNUM:
    case F8_REGNUM:
    case F9_REGNUM:
    case F10_REGNUM:
    case F11_REGNUM:
    case F12_REGNUM:
    case F13_REGNUM:
    case F14_REGNUM:
    case F15_REGNUM:
    case F16_REGNUM:
    case F17_REGNUM:
    case F18_REGNUM:
    case F19_REGNUM:
    case F20_REGNUM:
    case F21_REGNUM:
    case F22_REGNUM:
    case F23_REGNUM:
    case F24_REGNUM:
    case F25_REGNUM:
    case F26_REGNUM:
    case F27_REGNUM:
    case F28_REGNUM:
    case F29_REGNUM:
    case F30_REGNUM:
    case F31_REGNUM:
      if (!ARC64_HAS_FP_BASE)
	return false;
      break;

    default:
      break;
    }

  call_saved = !global_regs[regno] && !call_used_or_fixed_reg_p (regno);
  might_clobber = df_regs_ever_live_p (regno) || crtl->saves_all_registers;

  /* In a frame that calls __builtin_eh_return two data registers are used to
     pass values back to the exception handler.  Ensure that these registers are
     spilled to the stack so that the exception throw code can find them, and
     update the saved values.  The handling code will then consume these
     reloaded values to handle the exception.  */
  eh_needed = crtl->calls_eh_return
    && (EH_RETURN_DATA_REGNO (regno) != INVALID_REGNUM);

  if ((call_saved && might_clobber) || eh_needed)
    return true;

  /* If this is an interrupt handler, then we must save extra registers.  */
  if (ARC_INTERRUPT_P (cfun->machine->fn_type))
    {
      /* ARCv3 has ACCUMULATOR register as baseline.  */
      if (regno == R58_REGNUM)
	return true;

      if (df_regs_ever_live_p (regno)
	  /* if this is not a leaf function, then we must save all temporary
	     registers.  */
	  || (!crtl->is_leaf && call_used_regs[regno] && !fixed_regs[regno]))
	return true;
    }
  return false;
}

/* Compute the frame info.  */

static void
arc64_compute_frame_info (void)
{
  int regno;
  HOST_WIDE_INT offset = 0;
  struct arc64_frame *frame = &cfun->machine->frame;

  gcc_assert (!frame->layout_p);

  memset (frame, 0, sizeof (*frame));

  if (!ARC_NAKED_P(cfun->machine->fn_type))
    {
      /* Find out which GPR need to be saved.  */
      for (regno = R0_REGNUM, offset = 0;
	   regno <= F31_REGNUM;
	   regno++)
	if (arc64_save_reg_p (regno))
	  {
	    /* TBI: probably I need to make the saving of the FP registers
	       separate bulk from GPIs such that I can use latter on enter/leave
	       instruction seamlessly (i.e. first save FPregs/latter GPI, the
	       leave return feature will not work).  */
	    /* TBI: the FPUS only configuration is having only 32bit registers,
	       thus I can stack 2 FP registers in one stack slot ;).  */
	    frame->reg_offset[regno] = offset;
	    offset += UNITS_PER_WORD;
	  }
	else
	  frame->reg_offset[regno] = -1;

      /* Check if we need to save the return address.  */
      if (!crtl->is_leaf
	  || df_regs_ever_live_p (BLINK_REGNUM)
	  || crtl->calls_eh_return)
	{
	  frame->reg_offset[BLINK_REGNUM] = offset;
	  offset += UNITS_PER_WORD;
	}

      /* Check if we need frame pointer.  It is mutual exclusive with
	 arc64_save_reg_p call.  */
      if (frame_pointer_needed)
	{
	  frame->reg_offset[R27_REGNUM] = offset;
	  offset += UNITS_PER_WORD;
	}
    }

  /* 1. At the bottom of the stack are any outgoing stack
     arguments.  */
  frame->saved_outargs_size = ARC64_STACK_ALIGN (crtl->outgoing_args_size);

  /* 2. Size of locals and temporaries.  */
  frame->saved_locals_size = ARC64_STACK_ALIGN (get_frame_size ());

  /* 3. Size of the saved registers (including FP/BLINK).
     FIXME! FPR registers.  */
  frame->saved_regs_size = ARC64_STACK_ALIGN (offset);

  /* 4. Size of the callee-allocated area for pretend stack
     arguments.  */
  frame->saved_varargs_size = ARC64_STACK_ALIGN (crtl->args.pretend_args_size);

  /* Total size.  */
  frame->frame_size = frame->saved_outargs_size + frame->saved_locals_size
    + frame->saved_regs_size + frame->saved_varargs_size;

  gcc_assert (frame->frame_size == ARC64_STACK_ALIGN (frame->frame_size));
  frame->layout_p = reload_completed;
}

/* Emit a frame insn which adjusts stack pointer by OFFSET.  */

static void
frame_stack_add (HOST_WIDE_INT offset)
{
  rtx tmp;
  HOST_WIDE_INT lo = sext_hwi (offset, 32);
  unsigned HOST_WIDE_INT hi = sext_hwi (offset >> 32, 32);

  if (hi != 0xffffffffULL || hi != 0ULL)
    tmp = gen_rtx_SET (stack_pointer_rtx,
		       gen_rtx_PLUS (Pmode, stack_pointer_rtx,
				     gen_rtx_HIGH (Pmode, GEN_INT (hi))));

  tmp = gen_rtx_SET (stack_pointer_rtx,
		     plus_constant (Pmode, stack_pointer_rtx, lo));
  tmp = emit_insn (tmp);
  RTX_FRAME_RELATED_P (tmp) = 1;
  add_reg_note (tmp, REG_FRAME_RELATED_EXPR,
		gen_rtx_SET (stack_pointer_rtx,
			     plus_constant (Pmode, stack_pointer_rtx,
					    offset)));
}

/* Helper for prologue: emit frame store with pre_modify or pre_dec to
   save register REG on stack.  An initial offset OFFSET can be passed
   to the function.  If a DISPLACEMENT is defined, it will be used to
   generate pre_modify instead of pre_dec.  */

static HOST_WIDE_INT
frame_save_reg (rtx reg, HOST_WIDE_INT offset, HOST_WIDE_INT displacement)
{
  rtx addr, tmp;

  if (offset)
    {
      tmp = plus_constant (Pmode, stack_pointer_rtx,
			       offset - GET_MODE_SIZE (GET_MODE (reg)));
      addr = gen_frame_mem (GET_MODE (reg),
			    gen_rtx_PRE_MODIFY (Pmode,
						stack_pointer_rtx,
						tmp));
    }
  else if (displacement)
    {
      tmp = plus_constant (Pmode, stack_pointer_rtx, (-displacement));
      addr = gen_frame_mem (GET_MODE (reg),
			    gen_rtx_PRE_MODIFY (Pmode,
						stack_pointer_rtx,
						tmp));
    }
  else
    addr = gen_frame_mem (GET_MODE (reg), gen_rtx_PRE_DEC (Pmode,
							   stack_pointer_rtx));
  tmp = emit_move_insn (addr, reg);
  RTX_FRAME_RELATED_P (tmp) = 1;

  return (displacement ? displacement : GET_MODE_SIZE (GET_MODE (reg)))
    - offset;
}

/* ARC prologue saving regs routine.   */

static HOST_WIDE_INT
arc64_save_callee_saves (void)
{
  struct arc64_frame *frame = &cfun->machine->frame;
  machine_mode save_mode = DImode;
  int regno;
  HOST_WIDE_INT offset = -frame->saved_varargs_size;
  HOST_WIDE_INT frame_allocated = 0;
  rtx reg;

  for (regno = F31_REGNUM; regno >= R0_REGNUM; regno--)
    {
      HOST_WIDE_INT disp = 0;
      if (frame->reg_offset[regno] == -1
	  /* Hard frame pointer is saved in a different place.  */
	  || (frame_pointer_needed && regno == R27_REGNUM)
	  /* blink register is saved in a different place.  */
	  || (regno == BLINK_REGNUM))
	continue;

      save_mode = word_mode;
      if (ARC64_HAS_FP_BASE && FP_REGNUM_P (regno))
	{
	  save_mode = ARC64_HAS_FPUD ? DFmode : SFmode;
	  disp = UNITS_PER_WORD;
	}
      else if (regno >= 1
	       && (((regno - 1) % 2) == 0)
	       && (frame->reg_offset[regno - 1] != -1))
	{
	  /* Use 64-bit double stores for context saving.  */
	  if (!TARGET_64BIT && TARGET_LL64)
	    {
	      save_mode = DImode;
	      --regno;
	    }
	  /* Use 128-bit double stores for context saving.  */
	  else if (TARGET_64BIT && TARGET_WIDE_LDST)
	    {
	      save_mode = TImode;
	      --regno;
	    }
	}

      reg = gen_rtx_REG (save_mode, regno);
      frame_allocated += frame_save_reg (reg, offset, disp);
      offset = 0;
    }

  /* Save BLINK if required.  */
  if (frame->reg_offset[BLINK_REGNUM] != -1)
    {
      reg = gen_rtx_REG (Pmode, BLINK_REGNUM);
      frame_allocated += frame_save_reg (reg, offset, 0);
      offset = 0;
    }

  /* Save FP if required.  */
  if (frame_pointer_needed)
    {
      frame_allocated += frame_save_reg (hard_frame_pointer_rtx, offset, 0);
      offset = 0;
    }

  /* Emit mov fp,sp, if required.  */
  if (frame_pointer_needed)
    {
      rtx tmp = emit_move_insn (hard_frame_pointer_rtx, stack_pointer_rtx);
      RTX_FRAME_RELATED_P (tmp) = 1;
    }

  return frame_allocated;
}

/* Helper for epilogue: emit frame load with post_modify or post_inc
   to restore register REG from stack.  The initial offset is passed
   via OFFSET.  */

static HOST_WIDE_INT
frame_restore_reg (rtx reg, HOST_WIDE_INT displacement)
{
  rtx addr, insn, tmp;

  if (displacement)
    {
      tmp = plus_constant (Pmode, stack_pointer_rtx, displacement);
      addr = gen_frame_mem (GET_MODE (reg),
			    gen_rtx_POST_MODIFY (Pmode,
						 stack_pointer_rtx,
						 tmp));
    }
  else
    addr = gen_frame_mem (GET_MODE (reg),
			  gen_rtx_POST_INC (Pmode, stack_pointer_rtx));
  insn = emit_move_insn (reg, addr);
  RTX_FRAME_RELATED_P (insn) = 1;
  add_reg_note (insn, REG_CFA_RESTORE, reg);

  if (reg == hard_frame_pointer_rtx)
    add_reg_note (insn, REG_CFA_DEF_CFA,
		  plus_constant (Pmode, stack_pointer_rtx,
				 GET_MODE_SIZE (GET_MODE (reg))));
  else
    add_reg_note (insn, REG_CFA_ADJUST_CFA,
		  gen_rtx_SET (stack_pointer_rtx,
			       plus_constant (Pmode, stack_pointer_rtx,
					      GET_MODE_SIZE (GET_MODE (reg)))));

  return displacement ? displacement : GET_MODE_SIZE (GET_MODE (reg));
}

/* ARC' epilogue restore regs routine.  */

static HOST_WIDE_INT
arc64_restore_callee_saves (bool sibcall_p ATTRIBUTE_UNUSED)
{
  struct arc64_frame *frame = &cfun->machine->frame;
  HOST_WIDE_INT offset, frame_deallocated = 0;
  rtx reg;
  int regno;
  machine_mode restore_mode = DImode;

  /* Recover the frame_pointer location for the current frame.  */
  offset = frame->frame_size - (frame->saved_regs_size
				+ frame->saved_varargs_size);

  /* Emit mov sp,fp if need.  Thus, we get rid of the offset without
     using a possible expensive add3 instruction.  */
  if (frame_pointer_needed)
    {
      rtx tmp = emit_move_insn (stack_pointer_rtx, hard_frame_pointer_rtx);
      RTX_FRAME_RELATED_P (tmp) = 1;
    }
  else if (offset)
    frame_stack_add (offset);

  frame_deallocated += offset;

  if (frame_pointer_needed)
    frame_deallocated += frame_restore_reg (hard_frame_pointer_rtx, 0);

  if (frame->reg_offset[BLINK_REGNUM] != -1)
    {
      reg = gen_rtx_REG (Pmode, BLINK_REGNUM);
      frame_deallocated += frame_restore_reg (reg, 0);
    }

  for (regno = R0_REGNUM; regno <= F31_REGNUM; regno++)
    {
      HOST_WIDE_INT disp = 0;
      bool double_load_p = false;

      if (frame->reg_offset[regno] == -1
	  /* Hard frame pointer has been restored.  */
	  || (frame_pointer_needed && regno == R27_REGNUM)
	  /* blink register has been restored.  */
	  || (regno == BLINK_REGNUM))
	continue;

      restore_mode = word_mode;
      if (ARC64_HAS_FP_BASE && FP_REGNUM_P (regno))
	{
	  restore_mode = ARC64_HAS_FPUD ? DFmode : SFmode;
	  disp = UNITS_PER_WORD;
	}
      else if ((regno % 2) == 0
	       && (!frame_pointer_needed || ((regno + 1) != R27_REGNUM))
	       && (frame->reg_offset[regno + 1] != -1
		   && ((regno + 1) != BLINK_REGNUM)))
	{
	  /* Use 64-bit double loads for context restoring.  */
	  if (!TARGET_64BIT && TARGET_LL64)
	    {
	      restore_mode = DImode;
	      double_load_p = true;
	    }
	  /* Use 128-bit double loads for context restoring.  */
	  else if (TARGET_64BIT && TARGET_WIDE_LDST)
	    {
	      restore_mode = TImode;
	      double_load_p = true;
	    }
	}

      reg = gen_rtx_REG (restore_mode, regno);
      frame_deallocated += frame_restore_reg (reg, disp);

      if (double_load_p)
	regno++;
    }

  return frame_deallocated;
}

/* Emit an insn that's a simple single-set.  Both the operands must be
   known to be valid.  */
inline static rtx_insn *
emit_set_insn (rtx x, rtx y)
{
  return emit_insn (gen_rtx_SET (x, y));
}

/* Given FROM and TO register numbers, say whether this elimination is allowed.
   Frame pointer elimination is automatically handled.

   All eliminations are permissible. If we need a frame
   pointer, we must eliminate ARG_POINTER_REGNUM into
   FRAME_POINTER_REGNUM and not into STACK_POINTER_REGNUM.  */

static bool
arc64_can_eliminate (const int from ATTRIBUTE_UNUSED, const int to)
{
  return ((to == HARD_FRAME_POINTER_REGNUM) || (to == STACK_POINTER_REGNUM));
}

/* We force all frames that call eh_return to require a frame pointer, this will
   ensure that the previous frame pointer is stored on entry to the function,
   and will then be reloaded at function exit.  */

static bool
arc64_frame_pointer_required (void)
{
 return cfun->calls_alloca || crtl->calls_eh_return;
}

/* Giving a symbol, return how it will be addressed.  */

static arc64_symb
arc64_get_symbol_type (rtx x)
{
  bool is_local = false, is_tls = false;

  /* Labels are always local, so a short access will suffice.  FIXME!
     For large model, we should use a pc-rel accessing.  */
  if (LABEL_REF_P (x))
    return flag_pic ? ARC64_PIC :
      (arc64_cmodel_var ==  ARC64_CMODEL_LARGE ? ARC64_MAYBE_LARGE :
       ARC64_LO32);

  /* FIXME! Maybe I should assert here.  */
  if (!SYMBOL_REF_P (x))
    return ARC64_UNK;

  is_local = SYMBOL_REF_DECL (x)
    ? targetm.binds_local_p (SYMBOL_REF_DECL (x))
    : SYMBOL_REF_LOCAL_P (x);
  is_tls = SYMBOL_REF_TLS_MODEL (x);

  if (is_tls)
    return ARC64_TLS;

  if (!flag_pic)
    switch (arc64_cmodel_var)
      {
      case ARC64_CMODEL_SMALL:
      case ARC64_CMODEL_MEDIUM:
	return ARC64_LO32;
      case ARC64_CMODEL_LARGE:
	return ARC64_MAYBE_LARGE;
      default:
	gcc_unreachable ();
      }
  else if (flag_pic == 1)
    return is_local ? ARC64_PCREL : ARC64_PIC;
  else if (flag_pic == 2)
    return is_local ? ARC64_PCREL : ARC64_MAYBE_LPIC;
  else
    gcc_unreachable ();
}

/* Helper legitimate address. Extra takes an input to discriminate
   among load or store addresses.  */
static bool
arc64_legitimate_address_1_p (machine_mode mode,
			      rtx x,
			      bool strict ATTRIBUTE_UNUSED,
			      bool load_p,
			      bool scaling_p)
{
  if (REG_P (x))
    return true;

  if (CONST_INT_P (x))
    return true;

  if (CONSTANT_P (x))
    {
      /* Don't allow constant + offset when we don't have native
	 ld/st, as the compiler may use very large offsets.  These
	 memory accesses are splited anyhow.  */
      if (GET_MODE_SIZE (mode) == UNITS_PER_WORD * 2)
	{
	  /* 32-bit and no double loads?  */
	  if (!TARGET_64BIT && !TARGET_LL64)
	    return false;
	  /* 64-bit and no double loads?  */
	  if (TARGET_64BIT && !TARGET_WIDE_LDST)
	    return false;
	  /* fall thru  */
	}
      if (GET_CODE (XEXP (x, 0)) == PLUS
	  && CONST_INT_P (XEXP (XEXP (x, 0), 1))
	  /* Reloc addendum is only 32bit.   */
	  && UNSIGNED_INT32 (INTVAL (XEXP (XEXP (x, 0), 1))))
	x = XEXP (XEXP (x, 0), 0);
    }

  if (GET_CODE (x) == SYMBOL_REF
      || GET_CODE (x) == LABEL_REF)
    return (arc64_get_symbol_type (x) == ARC64_LO32);

  /* Check register + offset address type.  */
  if (GET_CODE (x) == PLUS
      && REG_P (XEXP (x, 0))
      && CONST_INT_P (XEXP (x, 1)))
    {
      machine_mode scaling_mode = mode;
      rtx offset = XEXP (x, 1);
      HOST_WIDE_INT ioffset = INTVAL (offset);


      if (GET_MODE_SIZE (scaling_mode) == 2 * UNITS_PER_WORD)
	{
	  /* Double load/stores are not scaling with 128 bits but with the
	     register size.  */
	  scaling_mode = smallest_int_mode_for_size (BITS_PER_WORD);

	  /* Adjust the offset as we may need to split this address.  */
	  if (ioffset > 0)
	    ioffset += UNITS_PER_WORD;
	}
      scaling_mode = scaling_p ? scaling_mode : QImode;

      /* ST instruction can only accept a single register plus a small s9 offset
	 as address.  */
      if ((ARC64LOG2 (GET_MODE_SIZE (scaling_mode))
	   && VERIFY_SHIFT (ioffset, ARC64LOG2 (GET_MODE_SIZE (scaling_mode)))
	   && SIGNED_INT9 (ioffset >> ARC64LOG2 (GET_MODE_SIZE (scaling_mode))))
	  || SIGNED_INT9 (ioffset))
	return true;

      if (load_p
	  /* FIXME! we can use address scalling here to fit even more.  */
	  && (UNSIGNED_INT32 (INTVAL (offset))
	      || SIGNED_INT32 (INTVAL (offset)))
	  && !optimize_size)
	return true;
    }

  /* Indexed addresses.  */
  if (load_p
      && GET_CODE (x) == PLUS
      && REG_P (XEXP (x, 0))
      && REG_P (XEXP (x, 1)))
    {
      if (GET_MODE_SIZE (mode) >= 2 * UNITS_PER_WORD)
	{
	  if (!TARGET_64BIT)
	    return TARGET_LL64;
	  else
	    return TARGET_WIDE_LDST;
	}
      return true;
    }

  /* Scalled addresses.  Permitted variants:
     ld.as rx, [rb,ri]         addr = rb + ri * scaling
     ld.as rx, [offset32, ri]  addr = offset32 + ri * scalling

     The store address can have only immediate operands scalled.  This
     case toghether with its load variant are handled by above
     code.  */
  if (scaling_p
      && load_p
      && GET_CODE (x) == PLUS
      && (REG_P (XEXP (x, 1)) || CONST_INT_P (XEXP (x, 1)))
      /* Check multiplication.  */
      && GET_CODE (XEXP (x, 0)) == MULT
      && REG_P (XEXP (XEXP (x, 0), 0))
      && CONST_INT_P (XEXP (XEXP (x, 0), 1)))
    {
      /* x is plus(mult(index, scaling), base) => base + index*scaling  */
      const rtx mult = XEXP (x, 0);
      const int scaling = INTVAL (XEXP (mult, 1));

      switch (GET_MODE_SIZE (mode))
	{
	case 2:	  /* ldh  */
	case 4:	  /* ld   */
	  if (scaling == GET_MODE_SIZE (mode))
	    return true;
	  break;
	case 8:	  /* ldd or ldl  */
	  if (scaling == 4)
	    return (!TARGET_64BIT && TARGET_LL64);
	  if (scaling == 8)
	    return TARGET_64BIT;
	  break;
	case 16:  /* lddl  */
	  if (scaling == 8)
	    return TARGET_WIDE_LDST;
	  break;
	default:
	  break;
	}
    }

  if ((GET_CODE (x) == PRE_DEC || GET_CODE (x) == PRE_INC
       || GET_CODE (x) == POST_DEC || GET_CODE (x) == POST_INC)
      && REG_P (XEXP (x, 0)))
    return true;

  if ((GET_CODE (x) == PRE_MODIFY || GET_CODE (x) == POST_MODIFY))
    return arc64_legitimate_address_1_p (mode, XEXP (x, 1), strict,
					 load_p, false);

  /* PIC address (LARGE).  */
  if (GET_CODE (x) == LO_SUM
      && REG_P (XEXP (x, 0))
      && GET_CODE (XEXP (x, 1)) == UNSPEC)
    return true;

  /* PIC address (small) or local symbol.  */
  if (load_p
      && GET_CODE (x) == UNSPEC
      && (XINT (x, 1) == ARC64_UNSPEC_GOT32
	  || XINT (x, 1) == ARC64_UNSPEC_TLS_IE
	  || XINT (x, 1) == ARC64_UNSPEC_PCREL))
    return true;

  return false;
}

/* Return TRUE if X is a legitimate address for accessing memory in
   mode MODE.  We do recognize addresses like:
   - [Rb]
   - [Rb, s9]
   - [Rb, Ri] (ld only)
   - [Rb, limm] (ld only)
   - predec/postdec
   - preinc/postinc
   - premodif/postmodif
*/

static bool
arc64_legitimate_address_p (machine_mode mode,
			    rtx x,
			    bool strict ATTRIBUTE_UNUSED)
{
  /* Allow all the addresses accepted by load.  */
  return arc64_legitimate_address_1_p (mode, x, strict, true, true);
}

/* Helper for legitimate constant.  */
static bool
arc64_legitimate_constant1_p (machine_mode mode, rtx x, bool nosym)
{
  switch (GET_CODE (x))
    {
    case CONST_DOUBLE:
    case CONST_INT:
    case CONST_WIDE_INT:
    case HIGH:
      if (GET_MODE_SIZE (mode) > UNITS_PER_WORD)
	return false;
      return true;

    case SYMBOL_REF:
      /* TODO: We should use arc64_get_symbol_type function here and retun
	 true/false depending on the type of the symbol.  */
      if (SYMBOL_REF_TLS_MODEL (x))
	return false;
      if (nosym || flag_pic)
	return false;
      /* fallthrough  */
    case LABEL_REF:
      /* FIXME: Labels should be PC-rel when PIC, and make sure they are not
	 ending up in constant pool.  */
      return true;

    case CONST:
      if (GET_CODE (XEXP (x, 0)) == PLUS)
	{
	  rtx tmp = XEXP (x, 0);
	  /* Do not allow @symb + offset constants.  */
	  bool t1 = arc64_legitimate_constant1_p (mode, XEXP (tmp, 0), true);
	  bool t2 = arc64_legitimate_constant1_p (mode, XEXP (tmp, 1), true);
	  return (t1 && t2);
	}
      return false;

    default:
      return false;
    }
}

/* Implement TARGET_LEGITIMATE_CONSTANT_P hook.  Return true for constants
   that should be rematerialized rather than spilled.  */

static bool
arc64_legitimate_constant_p (machine_mode mode, rtx x)
{
  return arc64_legitimate_constant1_p (mode, x, false);
}

/* Giving a mode, return true if we can pass it in fp registers.  */

bool
arc64_use_fp_regs (machine_mode mode)
{
  if (!FLOAT_MODE_P (mode))
    return false;

  /* FPU unit can have either 32 or 64 bit wide data path.  */
  /* FIXME: Use macros for the sizes.  */
  if ((ARC64_HAS_FPUS && (GET_MODE_SIZE (mode) == (UNITS_PER_WORD / 2)))
      || (ARC64_HAS_FPUH && (GET_MODE_SIZE (mode) == (UNITS_PER_WORD / 4)))
      || ARC64_HAS_FPUD)
    return true;
  return false;
}

static rtx
arc64_gen_fp_pair (machine_mode mode, unsigned regno1,
		 machine_mode mode1, HOST_WIDE_INT offset1,
		 unsigned regno2, machine_mode mode2,
		 HOST_WIDE_INT offset2)
{
  return gen_rtx_PARALLEL
    (mode,
     gen_rtvec (2,
		gen_rtx_EXPR_LIST (VOIDmode,
				   gen_rtx_REG (mode1, regno1),
				   GEN_INT (offset1)),
		gen_rtx_EXPR_LIST (VOIDmode,
				   gen_rtx_REG (mode2, regno2),
				   GEN_INT (offset2))));
}

static rtx
arc64_layout_arg (struct arc64_arg_info *info, cumulative_args_t pcum_v,
		  machine_mode mode, const_tree type, bool named)
{
  CUMULATIVE_ARGS *pcum = get_cumulative_args (pcum_v);
  HOST_WIDE_INT size;
  unsigned int nregs;
  bool record_p = type ? (TREE_CODE (type) == RECORD_TYPE) : false;

  memset (info, 0, sizeof (*info));
  info->off_fpr = pcum->fregs;
  info->off_gpr = pcum->iregs;

  /* Find out the size of argument.  */
  size = type ? int_size_in_bytes (type) : GET_MODE_SIZE (mode);

  /* When named, we can pass FP types into FP registers if they exists and they
     have the right size, unless a record type is used.  */
  if (named
      && !record_p
      && arc64_use_fp_regs (mode))
    {
      size = ROUND_UP (size, UNITS_PER_FP_REG);
      nregs = size / UNITS_PER_FP_REG;

      if (info->off_fpr + nregs <= MAX_ARC64_PARM_REGS)
	{
	  int fregno = F0_REGNUM + info->off_fpr;
	  info->nfpr = nregs;
	  switch (GET_MODE_CLASS (mode))
	    {
	    case MODE_VECTOR_FLOAT:
	      /* FIXME! for double-sized vectors, we may need to use double
		 register.  */
	    case MODE_FLOAT:
	      return gen_rtx_REG (mode, fregno);

	    case MODE_COMPLEX_FLOAT:
	      gcc_assert (nregs == 2);
	      return arc64_gen_fp_pair (mode, fregno, GET_MODE_INNER (mode), 0,
					fregno + 1, GET_MODE_INNER (mode),
					GET_MODE_UNIT_SIZE (mode));

	    default:
	      gcc_unreachable ();
	    }
	}
      /* No free FP-reg, continue using R-regs for the remaining FP
	 arguments.  */
    }

  size = ROUND_UP (size, UNITS_PER_WORD);
  nregs = size / UNITS_PER_WORD;

  /* Partition the argument between register and stack.  */
  gcc_assert (info->nfpr == 0);
  info->ngpr = MIN (nregs, MAX_ARC64_PARM_REGS - info->off_gpr);
  info->stack_p = (nregs - info->ngpr) != 0;

  if (info->ngpr)
    return gen_rtx_REG (mode, R0_REGNUM + info->off_gpr);
  return NULL_RTX;
}

/* Worker for return_in_memory.  */
/* FIXME! shall we use pass_by_reference?  */

static bool
arc64_return_in_memory (const_tree type, const_tree fndecl ATTRIBUTE_UNUSED)
{
  HOST_WIDE_INT size;

  /* Maybe we may need to return simple scalar types in registers:
  if (!AGGREGATE_TYPE_P (type)
      && TREE_CODE (type) != COMPLEX_TYPE)
    return false;
  */
  if (AGGREGATE_TYPE_P (type) || TREE_ADDRESSABLE (type))
    return true;

  size = int_size_in_bytes (type);

  /* Double sized float vectors are mapped into even-odd register
     pair, hence use the stack when someone wants to pass them to
     the caller.  */
  if (VECTOR_FLOAT_TYPE_P (type) && size > UNITS_PER_WORD)
    return true;

  /* Types larger than 2 registers returned in memory.  */
  return ((size < 0) || (size > 2 * UNITS_PER_WORD));
}

/* Worker for pass_by_reference.  */

static bool
arc64_pass_by_reference (cumulative_args_t cum_v,
			 const function_arg_info &arg)
{
  HOST_WIDE_INT size = arg.type_size_in_bytes ();
  struct arc64_arg_info info;
  CUMULATIVE_ARGS *pcum = get_cumulative_args (cum_v);

  /* Double sized fp-vectors are passed on the stack.  */
  if (arg.type
      && VECTOR_FLOAT_TYPE_P (arg.type) && size > UNITS_PER_WORD)
    return true;

  /* N.B. std_gimplify_va_arg_expr passes NULL for cum.  However, we
     do not use variadic arguments in fp-regs.  */
  if (pcum != NULL)
    {
      /* Check if we can use fp regs.  */
      arc64_layout_arg (&info, cum_v, arg.mode, arg.type, arg.named);
      if (info.nfpr)
	return false;
    }

  /* In earlier passes, the *_pass_by_reference() hook is called with the
     "COMPLEX" as the "argument of the function" and later"COMPLEX.element"
     is considered to be the "argument of the function".  This check makes
     a unified decision in all those scenarios.  */
  if (COMPLEX_MODE_P (arg.mode))
    {
      const machine_mode mode = GET_MODE_INNER (arg.mode);
      size = GET_MODE_SIZE (mode);
    }

  /* Variable sized arguments are always returned by reference, and
     arguments which are variable sized or larger than 2 registers are
     passed by reference.  */
  return !IN_RANGE (size, 0, 2 * UNITS_PER_WORD);
}

/* The function to update the summarizer variable *CUM to advance past
   an argument in the argument list.  The values MODE, TYPE and NAMED
   describe that argument.  Once this is done, the variable *CUM is
   suitable for analyzing the *following* argument with
   `FUNCTION_ARG', etc.  */

static void
arc64_function_arg_advance (cumulative_args_t pcum_v,
			    const function_arg_info &arg)
{
  struct arc64_arg_info info;
  CUMULATIVE_ARGS *pcum = get_cumulative_args (pcum_v);

  arc64_layout_arg (&info, pcum_v, arg.mode, arg.type, arg.named);

  pcum->fregs = info.nfpr + info.off_fpr;
  pcum->iregs = info.ngpr + info.off_gpr;
}

/* Implement TARGET_ARG_PARTIAL_BYTES.  */

static int
arc64_arg_partial_bytes (cumulative_args_t pcum_v,
			 const function_arg_info &arg)
{
  struct arc64_arg_info info;

  arc64_layout_arg (&info, pcum_v, arg.mode, arg.type, arg.named);
  gcc_assert ((info.nfpr == 0) || (info.ngpr == 0));

  return info.stack_p ? info.ngpr * UNITS_PER_WORD : 0;
}

/* This function is used to control a function argument is passed in a
   register, and which register.

   The arguments are CUM, of type CUMULATIVE_ARGS, which summarizes
   (in a way defined by INIT_CUMULATIVE_ARGS and FUNCTION_ARG_ADVANCE)
   all of the previous arguments so far passed in registers; MODE, the
   machine mode of the argument; TYPE, the data type of the argument
   as a tree node or 0 if that is not known (which happens for C
   support library functions); and NAMED, which is 1 for an ordinary
   argument and 0 for nameless arguments that correspond to `...' in
   the called function's prototype.

   The returned value should either be a `reg' RTX for the hard
   register in which to pass the argument, or zero to pass the
   argument on the stack.  */

static rtx
arc64_function_arg (cumulative_args_t pcum_v,
		    const function_arg_info &arg)
{
  struct arc64_arg_info info;

  return arc64_layout_arg (&info, pcum_v, arg.mode, arg.type, arg.named);
}

/* Define how to find the value returned by a function.  VALTYPE is
   the data type of the value (as a tree).  If the precise function
   being called is known, FN_DECL_OR_TYPE is its FUNCTION_DECL;
   otherwise, FN_DECL_OR_TYPE is its type.  */

static rtx
arc64_function_value (const_tree type,
		      const_tree func,
		      bool outgoing ATTRIBUTE_UNUSED)
{
  machine_mode mode = TYPE_MODE (type);
  int unsignedp = TYPE_UNSIGNED (type);

  if (INTEGRAL_TYPE_P (type))
    mode = promote_function_mode (type, mode, &unsignedp, func, 1);

  if (arc64_use_fp_regs (mode))
    {
      switch (GET_MODE_CLASS (mode))
	{
	case MODE_VECTOR_FLOAT:
	  /* FIXME! for double-sized vectors, we may need to use double
	     register.  */
	case MODE_FLOAT:
	  return gen_rtx_REG (mode, F0_REGNUM);

	case MODE_COMPLEX_FLOAT:
	  return arc64_gen_fp_pair (mode, F0_REGNUM, GET_MODE_INNER (mode), 0,
				    F1_REGNUM, GET_MODE_INNER (mode),
				    GET_MODE_UNIT_SIZE (mode));

	default:
	  gcc_unreachable ();
	}

    }
  return gen_rtx_REG (mode, R0_REGNUM);
}

/* Implements TARGET_FUNCTION_VALUE_REGNO_P.
   Return true if REGNO is the number of a hard register in which the values
   of called function may come back.  */

static bool
arc64_function_value_regno_p (const unsigned int regno)
{
  /* Maximum of 16 bytes can be returned in the general registers.  Examples
     of 16-byte return values are: 128-bit integers and 16-byte small
     structures (excluding homogeneous floating-point aggregates).

     We need to implement untyped_call instruction pattern when
     returning more than one value.  */

  if (regno == R0_REGNUM)
    return true;

  if (regno == F0_REGNUM)
    return ARC64_HAS_FP_BASE;

  return false;
}

static bool
arc64_split_complex_arg (const_tree)
{
  return true;
}

/* Implement TARGET_SETUP_INCOMING_VARARGS.  */

static void
arc64_setup_incoming_varargs (cumulative_args_t cum_v,
			      const function_arg_info &arg,
			      int *pretend_size, int no_rtl)
{
  CUMULATIVE_ARGS cum = *get_cumulative_args (cum_v);
  int gpi_saved;

  /* The caller has advanced CUM up to, but not beyond, the last named
     argumend.  Advance a local copu of CUM past the last "real" named
     argument, to find out how many registers are left over.  */
  arc64_function_arg_advance (pack_cumulative_args (&cum), arg);

  cfun->machine->uses_anonymous_args = 1;
  if (!FUNCTION_ARG_REGNO_P (cum.iregs))
    return;

  gpi_saved = MAX_ARC64_PARM_REGS - cum.iregs;

  if (!no_rtl && gpi_saved > 0)
    {
      rtx ptr, mem;
      ptr = plus_constant (Pmode, arg_pointer_rtx, 0);
      mem = gen_frame_mem (BLKmode, ptr);
      set_mem_alias_set (mem, get_varargs_alias_set ());

      move_block_from_reg (R0_REGNUM + cum.iregs, mem, gpi_saved);
    }

  /* FIXME! do I need to ROUND_UP (pretend, STACK_BOUNDARY /
     BITS_PER_UNIT) ?  */
  *pretend_size = gpi_saved * UNITS_PER_WORD;
}

/* Implement TARGET_HARD_REGNO_NREGS.  */

static unsigned int
arc64_hard_regno_nregs (unsigned int regno,
			machine_mode mode)
{
  if (FP_REGNUM_P (regno))
    return CEIL (GET_MODE_SIZE (mode), UNITS_PER_FP_REG);
  return CEIL (GET_MODE_SIZE (mode), UNITS_PER_WORD);
}

/* Implement TARGET_HARD_REGNO_MODE_OK.  */

static bool
arc64_hard_regno_mode_ok (unsigned int regno, machine_mode mode)
{
  if (GET_MODE_CLASS (mode) == MODE_CC)
    return regno == CC_REGNUM;

  if (regno == SP_REGNUM
      || regno == FRAME_POINTER_REGNUM
      || regno == ARG_POINTER_REGNUM)
    return (mode == Pmode);

  if (regno <= R58_REGNUM)
    {
      if (GET_MODE_SIZE (mode) <= UNITS_PER_WORD)
	return true;
      else if (GET_MODE_SIZE (mode) <= (UNITS_PER_WORD * 2))
	return ((regno & 1) == 0);
    }
  else if (FLOAT_MODE_P (mode) && FP_REGNUM_P (regno))
    {
      /* FIXME! I should make the decision base on the WIDE option
	 alone, if we need double regs or not.  */
      if (ARC64_VFP_128
	  && (GET_MODE_SIZE (mode) <= (UNITS_PER_FP_REG * 2))
	  && (GET_MODE_CLASS (mode) == MODE_VECTOR_FLOAT))
	return ((regno & 1) == 0);
      return true;
    }

  return false;
}

/* Implement TARGET_MODES_TIEABLE_P.  Tie QI/HI/SI/DI modes together.  */

static bool
arc64_modes_tieable_p (machine_mode mode1, machine_mode mode2)
{
  if (GET_MODE_CLASS (mode1) == MODE_INT
      && GET_MODE_CLASS (mode2) == MODE_INT
      && GET_MODE_SIZE (mode1) <= UNITS_PER_WORD
      && GET_MODE_SIZE (mode2) <= UNITS_PER_WORD)
    return true;

  return false;
}

static inline bool
arc64_short_insn_p (rtx_insn *insn)
{
  enum attr_iscompact iscompact;

  iscompact = get_attr_iscompact (insn);
  if (iscompact == ISCOMPACT_YES)
    return true;

  if (iscompact == ISCOMPACT_MAYBE)
    return (get_attr_length (insn) == 2)
      || (get_attr_length (insn) == 6);

  return (get_attr_length (insn) == 2);
}

/* Returns the index of the ARC condition code string in
   `arc_condition_codes'.  COMPARISON should be an rtx like `(eq (...)
   (...))'.  */

static int
get_arc64_condition_code (rtx comparison)
{
  switch (GET_MODE (XEXP (comparison, 0)))
    {
    case E_DImode: /* brcc/bbit instructions.  */
    case E_SImode:
    case E_CCmode:
      switch (GET_CODE (comparison))
	{
	case EQ : return ARC_CC_EQ;
	case NE : return ARC_CC_NE;
	case GT : return ARC_CC_GT;
	case LE : return ARC_CC_LE;
	case GE : return ARC_CC_GE;
	case LT : return ARC_CC_LT;
	case GTU : return ARC_CC_HI;
	case LEU : return ARC_CC_LS;
	case LTU : return ARC_CC_LO;
	case GEU : return ARC_CC_HS;
	default : gcc_unreachable ();
	}
    case E_CC_ZNmode:
      switch (GET_CODE (comparison))
	{
	case EQ : return ARC_CC_EQ;
	case NE : return ARC_CC_NE;
	case GE: return ARC_CC_P;
	case LT: return ARC_CC_N;
	case GT : return ARC_CC_PNZ;
	default : gcc_unreachable ();
	}
    case E_CC_Zmode:
      switch (GET_CODE (comparison))
	{
	case EQ : return ARC_CC_EQ;
	case NE : return ARC_CC_NE;
	default : gcc_unreachable ();
	}
    case E_CC_Cmode:
      switch (GET_CODE (comparison))
	{
	case LTU : return ARC_CC_C;
	case GEU : return ARC_CC_NC;
	default : gcc_unreachable ();
	}
    case E_CC_Vmode:
      switch (GET_CODE (comparison))
	{
	case EQ : return ARC_CC_NV;
	case NE : return ARC_CC_V;
	default : gcc_unreachable ();
	}
    case E_CC_FPUmode:
    case E_CC_FPUEmode:
      switch (GET_CODE (comparison))
	{
	case EQ: return ARC_CC_EQ;
	case NE: return ARC_CC_NE;
	case GT: return ARC_CC_GT;
	case GE: return ARC_CC_GE;
	case LT:
	  /* Equivalent with N, short insn friendly.  */
	  return ARC_CC_C;
	case LE: return ARC_CC_LS;
	case UNORDERED: return ARC_CC_V;
	case ORDERED: return ARC_CC_NV;
	case UNGT: return ARC_CC_HI;
	case UNGE:
	   /* Equivalent with NV, short insn friendly.  */
	  return ARC_CC_HS;
	case UNLT: return ARC_CC_LT;
	case UNLE: return ARC_CC_LE;
	default: gcc_unreachable ();
	}
      break;
    default : gcc_unreachable ();
    }
  gcc_unreachable ();
}

/* Address scaling is a bit tricky in case of double loads/stores.
   In normal cases, the address scaling takes the element size
   of the data it is handling as the offset. However, in case of
   a double load/store the offset size is the same size of a single
   element and not the double of it. e.g.:

   ldb.as   r1, [r0, 1]	      offset is 1 (1*1), data is  1 byte
   ldw.as   r1, [r0, 1]	      offset is 2 (1*2), data is  2 bytes
   ld.as    r1, [r0, 1]	      offset is 4 (1*4), data is  4 bytes
   ldl.as   r1, [r0, 1]	      offset is 8 (1*8), data is  8 bytes

   ldd.as   r1, [r0, 1]	      offset is 4 (1*4), data is  8 bytes
   lddl.as  r1, [r0, 1]	      offset is 8 (1*8), data is 16 bytes
*/

static machine_mode
arc64_get_effective_mode_for_address_scaling (const machine_mode mode)
{
  if (GET_MODE_SIZE (mode) == (UNITS_PER_WORD * 2))
    {
      gcc_assert (DOUBLE_LOAD_STORE);
      return Pmode;
    }
  return mode;
}

/* Print operand X (an rtx) in assembler syntax to file FILE.  CODE is
   a letter or dot (`z' in `%z0') or 0 if no letter was specified.
   For `%' followed by punctuation, CODE is the punctuation and X is
   null.  Letters `acln' are reserved.  The acceptable formatting
   commands given by CODE are:
     '0': Print a normal operand, if it's a general register,
	  then we assume DImode.
     'U': Load/store update or scaling indicator.
     'm': output condition code without 'dot'.
     'M': output inverse condition code without 'dot'.
     'w': output proper condition code for emulated brcc with u6 immediate.
     'W': output proper condition code for emulated brcc with r/limm.
     '?': Short instruction suffix.
     '*': Delay slot suffix
     'L': Lower 32bit of immediate or symbol.
     'H': Higher 32bit of an immediate, 64b-register or symbol.
     'C': Constant address, switches on/off @plt.
     's': Scalled immediate.
     'S': Scalled immediate, to be used in pair with 's'.
     'N': Negative immediate, to be used in pair with 's'.
     'V': 2x16b vector immediate, hi lane is zero.
     'P': Constant address, swithces on/off _s to be used with 'C'
     'A': output aq, rl or aq.rl flags for atomic ops.
*/

static void
arc64_print_operand (FILE *file, rtx x, int code)
{
  HOST_WIDE_INT ival;
  const char * const arc_condition_codes[] =
    {
     "al", 0, "eq", "ne", "p", "n", "lo", "hs", "v", "nv",
     "gt", "le", "ge", "lt", "hi", "ls", "pnz", 0
    };
  const char * const ebrcc_u6ccodes[] =
    {
     "na", "na", "na", "na", "na", "na", "na", "na", "na", "na",
     "ge", "lt", "na", "na", "hs", "lo", "na", "na"
    };
  const char * const ebrcc_rccodes[] =
    {
     "na", "na", "na", "na", "na", "na", "na", "na", "na", "na",
     "lt", "ge", "na", "na", "lo", "hs", "na", "na"
    };

  int scalled = 0;
  int sign = 1;
  machine_mode effective_mode;

  switch (code)
    {
    case '*':
      if (final_sequence && final_sequence->len () != 1)
	{
	  rtx_insn *delay = final_sequence->insn (1);

	  if (delay->deleted ())
	    return;
	  fputs (".d", file);
	}
      return;

    case '?':
      if (arc64_short_insn_p (current_output_insn))
	fputs ("_s", file);
      break;

    case 'U' :
      /* Output a load/store with update indicator if appropriate.  */
      if (!MEM_P (x))
	{
	  output_operand_lossage ("invalid operand for %%U code");
	  return;
	}

      /* FIXME! consider volatile accesses as .di accesses, everything
	 under an option.  */
      if (MEM_VOLATILE_P (x) && TARGET_VOLATILE_DI)
	fputs (".di", file);

      switch (GET_CODE (XEXP (x, 0)))
	{
	case PRE_INC:
	case PRE_DEC:
	case PRE_MODIFY:
	  fputs (".a", file);
	  break;

	case POST_INC:
	case POST_DEC:
	case POST_MODIFY:
	  fputs (".ab", file);
	  break;

	case PLUS:
	  effective_mode =
	    arc64_get_effective_mode_for_address_scaling (GET_MODE (x));
	  if (GET_CODE (XEXP (XEXP (x, 0), 0)) == MULT)
	    fputs (".as", file);
	  else if (REG_P (XEXP (XEXP (x, 0), 0))
		   && CONST_INT_P (XEXP (XEXP (x, 0), 1))
		   && ARC64_CHECK_SCALLED_IMMEDIATE (XEXP (XEXP (x, 0), 1),
						     effective_mode))
	    {
	      fputs (".as", file);
	      scalled_p = true;
	    }
	default:
	  break;
	}
      break;

    case 'L':
      if (GET_CODE (x) == SYMBOL_REF
	  || GET_CODE (x) == LABEL_REF)
	{
	  output_addr_const (asm_out_file, x);
	  fputs ("@u32", file);
	  break;
	}
      else if (REG_P (x))
	{
	  asm_fprintf (file, "%s", reg_names [REGNO (x)]);
	  break;
	}
      else if (!CONST_INT_P (x))
	{
	  output_operand_lossage ("invalid operand for %%L code");
	  return;
	}
      ival = INTVAL (x);
      ival &= 0xffffffffULL;
      fprintf (file,"0x%08" PRIx32, (uint32_t) ival);
      break;

    case 'H':
      if (GET_CODE (x) == SYMBOL_REF
	  || GET_CODE (x) == LABEL_REF
	  || GET_CODE (x) == UNSPEC)
	{
	  output_addr_const (asm_out_file, x);
	  break;
	}
      else if (CONST_INT_P (x))
	{
	  ival = INTVAL (x);
	  ival >>= 32;
	  fprintf (file, "%d", (int32_t) ival);
	}
      else if (REG_P (x))
	asm_fprintf (file, "%s", reg_names [REGNO (x) + 1]);
      else
	{
	  output_operand_lossage ("invalid operand for %%H code");
	  return;
	}
      break;

    case 'V':
      if (!CONST_INT_P (x))
	{
	  output_operand_lossage ("invalid operand for %%V code");
	  return;
	}
      ival = INTVAL (x);
      ival &= 0xffffULL;
      fprintf (file, "0x%08" PRIx32, (uint32_t) ival);
      break;

    case 'm':
      fputs (arc_condition_codes[get_arc64_condition_code (x)], file);
      break;

    case 'M':
      fputs (arc_condition_codes[ARC_INVERSE_CONDITION_CODE
				 (get_arc64_condition_code (x))], file);
      break;

    case 'w':
      fputs (ebrcc_u6ccodes[get_arc64_condition_code (x)], file);
      break;

    case 'W':
      fputs (ebrcc_rccodes[get_arc64_condition_code (x)], file);
      break;

    case 'C':
      if (GET_CODE (x) != SYMBOL_REF
	  && GET_CODE (x) != LABEL_REF)
	{
	  output_operand_lossage ("invalid operand for %%C code");
	  return;
	}
      output_addr_const (asm_out_file, x);
      /* N.B. The instruction is valid, hence any symbol which its
	 type is LPIC is valid for instruction, see
	 arc64_is_long_call_p.  */
      switch (arc64_get_symbol_type (x))
	{
	case ARC64_PIC:
	  fputs ("@plt", file);
	  break;
	case ARC64_LPIC:
	  fputs ("@plt34", file);
	  break;
	default:
	  break;
	}
      break;

    case 'P':
      if (GET_CODE (x) != SYMBOL_REF
	  && GET_CODE (x) != LABEL_REF)
	{
	  output_operand_lossage ("invalid operand for %%P code");
	  return;
	}
      if (arc64_use_plt34_p (x))
	fputs ("_s", file);
      break;

    case 's':
      if (REG_P (x))
	break;
      if (!CONST_INT_P (x))
	{
	  output_operand_lossage ("invalid operand for %%s code");
	  return;
	}
      ival = INTVAL (x);
      if ((ival & 0x07) == 0)
	  scalled = 3;
      else if ((ival & 0x03) == 0)
	  scalled = 2;
      else if ((ival & 0x01) == 0)
	  scalled = 1;

      if (scalled)
	asm_fprintf (file, "%d", scalled);
      break;

    case 'N':
      if (REG_P (x))
	{
	  output_operand_lossage ("invalid operand for %%N code");
	  return;
	}
      sign = -1;
      /* fall through */
    case 'S':
      if (REG_P (x))
	{
	  asm_fprintf (file, "%s", reg_names [REGNO (x)]);
	  return;
	}
      if (!CONST_INT_P (x))
	{
	  output_operand_lossage ("invalid operand for %%N or %%S code");
	  return;
	}
      ival = sign * INTVAL (x);
      if ((ival & 0x07) == 0)
	  scalled = 3;
      else if ((ival & 0x03) == 0)
	  scalled = 2;
      else if ((ival & 0x01) == 0)
	  scalled = 1;

      asm_fprintf (file, "%wd", (ival >> scalled));
      break;

    case 'A':
      if (!ARC64_HAS_ATOMIC_3)
	return;
      if (!CONST_INT_P (x))
	{
	  output_operand_lossage ("invalid operand for %%A");
	  return;
	}
      ival = INTVAL (x);
      switch ((enum memmodel) ival)
	{
	case MEMMODEL_ACQ_REL:
	  fputs (".aq.rl", file);
	  break;

	case MEMMODEL_SEQ_CST:
	case MEMMODEL_SYNC_SEQ_CST:
	case MEMMODEL_ACQUIRE:
	case MEMMODEL_CONSUME:
	case MEMMODEL_SYNC_ACQUIRE:
	  fputs (".aq", file);
	  break;

	case MEMMODEL_RELEASE:
	case MEMMODEL_SYNC_RELEASE:
	  fputs (".rl", file);
	  break;

	case MEMMODEL_RELAXED:
	  break;

	default:
	  gcc_unreachable ();
	}
      break;

    case 0:
      if (x == NULL)
	{
	  output_operand_lossage ("missing operand");
	  return;
	}

      switch (GET_CODE (x))
	{
	case REG :
	  asm_fprintf (file, "%s", reg_names [REGNO (x)]);
	  break;

	case MEM :
	  fputc ('[', file);
	  output_address (GET_MODE (x), XEXP (x, 0));
	  fputc (']', file);
	  break;

	case CONST:
	case LABEL_REF:
	case SYMBOL_REF:
	case UNSPEC:
	  output_addr_const (asm_out_file, x);
	  break;

	case CONST_DOUBLE:
	  {
	    long l;
	    int msize;
	    machine_mode mode = GET_MODE (x);
	    /* Maybe I need to define TARGET_SUPPORTS_WIDE_INT.  */
	    gcc_assert (mode != VOIDmode);
	    /* GET_MODE_BITSIZE BITS_PER_WORD */
	    msize = GET_MODE_SIZE (mode);
	    if (msize > UNITS_PER_LIMM)
	      msize = UNITS_PER_LIMM;
	    msize *= 8;
	    l = real_to_target (NULL, CONST_DOUBLE_REAL_VALUE (x),
				float_mode_for_size (msize).require ());
	    asm_fprintf (file, "0x%08lx", l);
	    break;
	  }
	case CONST_INT:
	  asm_fprintf (file, HOST_WIDE_INT_PRINT_DEC, INTVAL (x));
	  break;

	default:
	  output_operand_lossage ("invalid operand");
	  return;
	}
      break;

    default:
      output_operand_lossage ("invalid operand prefix '%%%c'", code);
    }
}

/* Print address 'addr' of a memory access with mode 'mode'.  */

static void
arc64_print_operand_address (FILE *file , machine_mode mode, rtx addr)
{
  rtx base, index = 0;
  machine_mode effective_mode = mode;

  switch (GET_CODE (addr))
    {
    case REG :
      fputs (reg_names[REGNO (addr)], file);
      break;

    case CONST:
      output_address (mode, XEXP (addr, 0));
      break;

    case PLUS :
      if (GET_CODE (XEXP (addr, 0)) == MULT)
	index = XEXP (XEXP (addr, 0), 0), base = XEXP (addr, 1);
      else if (CONST_INT_P (XEXP (addr, 0)))
	index = XEXP (addr, 0), base = XEXP (addr, 1);
      else
	base = XEXP (addr, 0), index = XEXP (addr, 1);

      gcc_assert (OBJECT_P (base));
      effective_mode =
	arc64_get_effective_mode_for_address_scaling (mode);
      if (REG_P (base)
	  && scalled_p
	  && CONST_INT_P (index)
	  && ARC64_CHECK_SCALLED_IMMEDIATE (index, effective_mode))
	{
	  index = GEN_INT (INTVAL (index) >>
			   ARC64LOG2 (GET_MODE_SIZE (effective_mode)));
	}
      scalled_p = false;

      arc64_print_operand_address (file, mode, base);
      if (CONSTANT_P (base) && CONST_INT_P (index))
	fputc ('+', file);
      else
	fputc (',', file);
      gcc_assert (OBJECT_P (index));
      arc64_print_operand_address (file, mode, index);
      break;

    case PRE_INC:
    case POST_INC:
      output_address (VOIDmode,
		      plus_constant (Pmode, XEXP (addr, 0),
				     GET_MODE_SIZE (mode)));
      break;

    case PRE_DEC:
    case POST_DEC:
      output_address (VOIDmode,
		      plus_constant (Pmode, XEXP (addr, 0),
				     -GET_MODE_SIZE (mode)));
      break;

    case PRE_MODIFY:
    case POST_MODIFY:
      output_address (VOIDmode, XEXP (addr, 1));
      break;

    case LO_SUM:
      /* This type of address can be only accepted by LD instructions.  */
      base = XEXP (addr, 0);
      index = XEXP (addr, 1);
      arc64_print_operand_address (file, mode, base);
      fputc (',', file);
      output_addr_const (file, index);
      break;

    case UNSPEC:
      /* Small PIC.  */
      fputs ("pcl,", file);
      output_addr_const (file, addr);
      break;

    case LABEL_REF:
    case SYMBOL_REF:
    case CONST_INT:
      output_addr_const (file, addr);
      break;

    default:
      gcc_unreachable ();
      break;
    }
}

/* Target hook for indicating whether a punctuation character for
   TARGET_PRINT_OPERAND is valid.  */

static bool
arc64_print_operand_punct_valid_p (unsigned char code)
{
  return (code == '?' || code == '*');
}

/* Implement TARGET_ASM_OUTPUT_ADDR_CONST_EXTRA.  */

static bool
arc64_output_addr_const_extra (FILE *file, rtx x)
{
  rtx base, offset = NULL_RTX;

  if (GET_CODE (x) == UNSPEC)
    {
      base = XVECEXP (x, 0, 0);
      if (GET_CODE (base) == CONST
	  && GET_CODE (XEXP (base, 0)) == PLUS)
	{
	  offset = XEXP (XEXP (base, 0), 1);
	  base = XEXP (XEXP (base, 0), 0);
	}
      output_addr_const (file, base);
      switch (XINT (x, 1))
	{
	case ARC64_UNSPEC_PCREL:
	  fputs ("@pcl", file);
	  break;

	case ARC64_UNSPEC_GOT32:
	case ARC64_UNSPEC_GOT:
	  fputs ("@gotpc", file);
	  break;

	case ARC64_UNSPEC_TLS_GD:
	  fputs ("@tlsgd", file);
	  break;

	case ARC64_UNSPEC_TLS_IE:
	  fputs ("@tlsie", file);
	  break;

	case ARC64_UNSPEC_TLS_OFF:
	  fputs ("@tpoff", file);
	  break;

	default:
	  gcc_unreachable ();
	}

      if (offset != NULL_RTX)
	{
	  fputs ("+", file);
	  output_addr_const (file, offset);
	}
      return true;
    }

  return false;
}

/* Wrap X in an unspec of kind KIND.  */

static rtx
gen_sym_unspec (rtx x, int kind)
{
  return gen_rtx_UNSPEC (Pmode, gen_rtvec (1, x), kind);
}

/* The __tls_get_attr symbol.  */
static GTY(()) rtx arc_tls_symbol;

/* Emit a call to __tls_get_addr.  TI is the argument to this function.
   RET is an RTX for the return value location.  The entire insn sequence
   is returned.  */

static void
arc64_tls_call (rtx dest, rtx arg)
{
  rtx argreg = gen_reg_rtx (Pmode);
  if (!arc_tls_symbol)
    arc_tls_symbol = init_one_libfunc ("__tls_get_addr");

  df_set_regs_ever_live (BLINK_REGNUM, true);
  emit_insn (gen_rtx_SET (argreg, arg));
  emit_library_call_value (arc_tls_symbol, dest, LCT_CONST, Pmode,
			   argreg, Pmode);
}

/* Handle LARGE memory model for RTX.  */

static rtx
arc64_large_address (rtx base, rtx scratch)
{
  if (!TARGET_64BIT)
    return base;

  emit_insn (gen_rtx_SET (scratch, gen_rtx_HIGH (Pmode, base)));
  return gen_rtx_LO_SUM (Pmode, scratch, copy_rtx (base));
}

/* Create a legitimate mov instruction for the given BASE (unspec).  */

static rtx
arc64_legit_unspec (rtx base)
{
  rtx t1, ret;
  gcc_assert (can_create_pseudo_p ());

  switch (arc64_cmodel_var)
    {
    case ARC64_CMODEL_SMALL:
    case ARC64_CMODEL_MEDIUM:
      return base;

    case ARC64_CMODEL_LARGE:
      ret = gen_reg_rtx (Pmode);
      t1 = gen_reg_rtx (Pmode);
      emit_insn (gen_rtx_SET (ret, arc64_large_address (base, t1)));
      return ret;

    default:
      break;
    }
  gcc_unreachable ();
}

/* Return a legitimized TLS address to access ADDR, which is a
   SYMBOL_REF.  */

static rtx
arc64_legitimize_tls_address (rtx addr)
{
  rtx t1, t2;
  rtx base;
  enum tls_model model = SYMBOL_REF_TLS_MODEL (addr);

  gcc_assert (can_create_pseudo_p ());

  switch (model)
    {
    case TLS_MODEL_LOCAL_DYNAMIC:
    case TLS_MODEL_GLOBAL_DYNAMIC:
      /* Gen:
	 addl r0,pcl,@ADDR@tlsgd
	 bl __tls_get_addr@plt  */
      t2 = gen_reg_rtx (Pmode);
      base = gen_sym_unspec (addr, ARC64_UNSPEC_TLS_GD);
      t1 = arc64_legit_unspec (base);
      arc64_tls_call (t2, t1);
      return t2;

    case TLS_MODEL_INITIAL_EXEC:
      /* Gen:
	 ldl  rx,[pcl,@ADDR@tlsie]
	 addl rx,rx,r30  */
      addr = arc64_legit_unspec (gen_sym_unspec (addr, ARC64_UNSPEC_TLS_IE));
      addr = copy_to_mode_reg (Pmode, gen_const_mem (Pmode, addr));
      return gen_rtx_PLUS (Pmode, addr, gen_rtx_REG (Pmode, R30_REGNUM));

    case TLS_MODEL_LOCAL_EXEC:
      /* Gen:
	 addl rx,r30,@ADDR@tpoff  */
      addr = arc64_legit_unspec (gen_sym_unspec (addr, ARC64_UNSPEC_TLS_OFF));
      return gen_rtx_PLUS (Pmode, gen_rtx_REG (Pmode, R30_REGNUM), addr);

    default:
      gcc_unreachable ();
    }
}

/* Helper function.  Returns a valid ARC64 RTX that represents the
   argument X which is an invalid address RTX.  The argument SCRATCH
   may be used as a temp when building affresses.  */

static rtx
arc64_legitimize_address_1 (rtx x, rtx scratch)
{
  rtx base, addend, t1;
  bool is_local = true, ATTRIBUTE_UNUSED is_weak = false;

  switch (GET_CODE (x))
    {
    case SYMBOL_REF:
      is_local = SYMBOL_REF_DECL (x)
	? targetm.binds_local_p (SYMBOL_REF_DECL (x))
	: SYMBOL_REF_LOCAL_P (x);
      is_weak = SYMBOL_REF_WEAK (x);
      if (SYMBOL_REF_TLS_MODEL (x))
	return arc64_legitimize_tls_address (x);
      /* FALLTHRU */

    case LABEL_REF:
      t1 = can_create_pseudo_p () ? gen_reg_rtx (Pmode) : scratch;
      gcc_assert (t1);
      if (!flag_pic)
	{
	  switch (arc64_cmodel_var)
	    {
	    case ARC64_CMODEL_SMALL:
	    case ARC64_CMODEL_MEDIUM:
	      return x;
	    default:
	      return arc64_large_address (x, t1);
	    }
	}
      else if (is_local)
	{
	  /* Local symbol, we can access it using a simple
	     PCL-relative access.  */
	  base = gen_sym_unspec (x, ARC64_UNSPEC_PCREL);
	  return base;
	}
      else if (flag_pic)
	{
	  /* Global symbol, we access it via a load from the GOT
	     (small model).  I.e., load pointer address via GOT, do
	     the access of the datum using the loaded pointer.  */
	  /* FIXME! to enable LARGE/small pic models make the above
	     condition flag_pic == 1.  */
	  base = gen_sym_unspec (x, ARC64_UNSPEC_GOT32);
	  return gen_const_mem (Pmode, base);
	}
      else
	{
	  /* Global symbol, we access it via a load from the GOT
	     (LARGE model).  */
	  base = gen_sym_unspec (x, ARC64_UNSPEC_GOT);
	  t1 = arc64_large_address (base, t1);
	  return gen_const_mem (Pmode, t1);
	}

    case LO_SUM:
      return x;

    case CONST:
      /* We expect something like: const (plus (symbol_ref) (const_int))
	 A c-function which will generate this should be:
	 int a;
	 void b (void) { a = "" ? "" + 8 : 3; }
       */
      gcc_assert (can_create_pseudo_p ());
      split_const (x, &base, &addend);
      base = force_reg (Pmode, base);
      if (addend == const0_rtx)
	return base;
      return gen_rtx_PLUS (Pmode, base, addend);

    default:
      break;
    }

  gcc_unreachable ();
}


/* Nested function support.  */

/* Output assembler code for a block containing the constant parts of
   a trampoline, leaving space for variable parts.  */

static void
arc64_asm_trampoline_template (FILE *f)
{
  if (!TARGET_64BIT)  /* ARC32 */
    {
      /* ld_s r12,[pcl,8]
	 ld   r11,[pcl,12]
	 j_s [r12]  */
      asm_fprintf (f, "\tld_s\t%s,[pcl,8]\n", reg_names[R12_REGNUM]);
      asm_fprintf (f, "\tld\t%s,[pcl,12]\n", reg_names[STATIC_CHAIN_REGNUM]);
      asm_fprintf (f, "\tj_s\t[%s]\n", reg_names[R12_REGNUM]);
    }
  else /* TARGET_64BIT */
    {
      /* nop
	 ldl  r12,[pcl,12]
	 ldl  r11,[pcl,16]
	 j    [r12] */
      asm_fprintf (f, "\tnop\n");
      asm_fprintf (f, "\tldl\t%s,[pcl,12]\n", reg_names[R12_REGNUM]);
      asm_fprintf (f, "\tldl\t%s,[pcl,16]\n", reg_names[STATIC_CHAIN_REGNUM]);
      asm_fprintf (f, "\tj\t[%s]\n", reg_names[R12_REGNUM]);
    }
  /* .(x)word function's address
     .(x)word static chain value  */
  assemble_aligned_integer (POINTER_BYTES, const0_rtx);
  assemble_aligned_integer (POINTER_BYTES, const0_rtx);
}

/* Helper initialize trampoline.  */

static void
arc64_initialize_trampoline (rtx tramp, tree fndecl, rtx cxt)
{
  rtx fnaddr = XEXP (DECL_RTL (fndecl), 0);
  const int fnaddr_offset = TRAMPOLINE_CODE_SIZE;
  const int cxt_offset = TRAMPOLINE_CODE_SIZE + POINTER_BYTES;

  emit_block_move (tramp, assemble_trampoline_template (),
		   GEN_INT (TRAMPOLINE_SIZE), BLOCK_OP_NORMAL);
  emit_move_insn (adjust_address (tramp, Pmode, fnaddr_offset), fnaddr);
  emit_move_insn (adjust_address (tramp, Pmode, cxt_offset), cxt);
  /* FIXME: maybe it's good to use "maybe_emit_call_builtin___clear_cache"  */
  emit_library_call (gen_rtx_SYMBOL_REF (Pmode, "__clear_cache"),
		     LCT_NORMAL, VOIDmode, XEXP (tramp, 0), Pmode,
		     plus_constant (Pmode, XEXP (tramp, 0), TRAMPOLINE_SIZE),
		     Pmode);
}

/* Implement FUNCTION_OK_FOR_SIBCALL hook.  */

static bool
arc64_function_ok_for_sibcall (tree decl,
			       tree exp ATTRIBUTE_UNUSED)
{
  /* Don't use sibcall for naked functions.  */
  if (ARC_NAKED_P (cfun->machine->fn_type))
    return false;

  /* Don't use sibcall for ISR functions.  */
  if (ARC_INTERRUPT_P (cfun->machine->fn_type))
    return false;

  if (decl && targetm.binds_local_p (decl))
    return true;

  /* We don't have an instruction to do what bl_s sym@plt34 does.  */
  if (flag_pic == 2)
    return false;

  return true;
}

/* Implement INIT_LIBFUNCS hook.  */

static void
arc64_init_libfuncs (void)
{
  set_optab_libfunc (ffs_optab, SImode, "__ffssi2");
  set_optab_libfunc (clz_optab, SImode, "__clzsi2");
  set_optab_libfunc (ctz_optab, SImode, "__ctzsi2");
  set_optab_libfunc (popcount_optab, SImode, "__popcountsi2");
  set_optab_libfunc (parity_optab, SImode, "__paritysi2");
}

/* Helper evp_dump_stack_info.  */

static void
arc64_print_format_registers(FILE *stream,
			     unsigned regno,
			     enum machine_mode mode)
{
  unsigned int  j, nregs;
  unsigned int ll = 0;

  nregs = arc64_hard_regno_nregs (regno, mode);
  /* Make sure BLKmode has a number of regs attached.  */
  nregs = nregs ? nregs : 2;
  for (j = regno + nregs; j > regno; j--)
    {
      asm_fprintf (stream,"%s", reg_names[j - 1]);
      ll += strlen (reg_names[j - 1]);
    }
  asm_fprintf (stream,"`");
  for (j = ll; j < 20; j++)
    asm_fprintf (stream, " ");

  asm_fprintf (stream,"\t(%d)\n",
	   GET_MODE_SIZE (mode));
}

/* Place some comment into assembler stream describing the current
   function.  */

static void
arc64_output_function_prologue (FILE *f)
{
  int regno, i;
  struct arc64_frame *frame = &cfun->machine->frame;
  tree parm = DECL_ARGUMENTS (current_function_decl);

  asm_fprintf (f, "\t# args = %wd, pretend = %ld, frame = %wd\n",
	       (HOST_WIDE_INT) crtl->args.size,
	       frame->saved_varargs_size,
	       (HOST_WIDE_INT) get_frame_size ());
  asm_fprintf (f, "\t# frame_needed = %d, uses_anonymous_args = %d\n",
	       frame_pointer_needed,
	       cfun->machine->uses_anonymous_args);
  asm_fprintf (f, "\t# size = %wd bytes\n",
	       frame->frame_size);
  asm_fprintf (f, "\t# + outargs = %wd bytes\n",
	       frame->saved_outargs_size);
  asm_fprintf (f, "\t# + locals  = %wd bytes\n",
	       frame->saved_locals_size);
  asm_fprintf (f, "\t# + regs    = %wd bytes\n",
	       frame->saved_regs_size);
  asm_fprintf (f, "\t# + varargs = %wd bytes\n",
	       frame->saved_varargs_size);

  if (crtl->calls_eh_return)
    asm_fprintf (f, "\t# Calls __builtin_eh_return.\n");

  for (regno = R0_REGNUM; regno <= F31_REGNUM; regno++)
    if (frame->reg_offset[regno] != -1)
      asm_fprintf (f, "\t# regsave[%s] => %ld\n", reg_names[regno],
		   frame->reg_offset[regno]);

  asm_fprintf(f, "\t# Parameters:\n");
  while (parm)
    {
      rtx  rtl = DECL_INCOMING_RTL (parm);
      if (rtl)
	{
	  asm_fprintf(f,"\t#  ");
	  tree decl_name;
	  decl_name = DECL_NAME (parm);
	  if (decl_name != NULL && IDENTIFIER_POINTER (decl_name) != NULL)
	    {
	      const char *name =  lang_hooks.dwarf_name (parm, 0);
	      if(name)
		asm_fprintf(f, "%-20.20s =`", name);
	      else
		asm_fprintf(f, "N.A.`");
	    }
	  if (REG_P (rtl))
	    {
	      unsigned regno = REGNO (rtl);
	      enum machine_mode mode = GET_MODE (rtl);
	      arc64_print_format_registers (f, regno, mode);
	    }
	  else if (MEM_P (rtl))
	    {
	      rtx addr = XEXP (rtl, 0);
	      long argPtrOfs = frame->frame_size -
		arc64_initial_elimination_offset (ARG_POINTER_REGNUM,
						  (frame_pointer_needed ?
						   HARD_FRAME_POINTER_REGNUM :
						   STACK_POINTER_REGNUM));
	      if (GET_CODE (addr) == PLUS)
		{
		  rtx ofs = XEXP (addr, 1);
		  gcc_assert (CONST_INT_P (ofs));
		  argPtrOfs += INTVAL (ofs);
		}
	      asm_fprintf (f, "%s[%4ld]`                 (%d)\n",
			   (frame_pointer_needed ? "fp" : "sp"),
			   argPtrOfs,
			   GET_MODE_SIZE (GET_MODE (rtl)));
	    }
	  else if (GET_CODE (rtl) == PARALLEL)
	    {
	      asm_fprintf (f,"xvec`                 (%d)\n",
			   GET_MODE_SIZE (GET_MODE (rtl)));
	      for (i = 0; i < XVECLEN (rtl, 0); i++)
		{
		  rtx xv = XEXP (XVECEXP (rtl, 0, i), 0);
		  if (REG_P (xv))
		    {
		      unsigned regno = REGNO (xv);
		      enum machine_mode mode = GET_MODE (xv);
		      asm_fprintf (f,"#                         `");
		      arc64_print_format_registers (f, regno, mode);
		    }
		}
	    }
	  else if (GET_CODE (rtl) == CONCAT)
	    {
	      rtx op0 = XEXP (rtl, 0);
	      rtx op1 = XEXP (rtl, 1);
	      if (REG_P (op0))
		arc64_print_format_registers (f, REGNO (op0), GET_MODE (op0));
	      else
		asm_fprintf(f, "MEM`\n");
	      asm_fprintf(f,"\t#\t\t\t+`");
	      if (REG_P (op1))
		arc64_print_format_registers (f, REGNO (op1), GET_MODE (op1));
	      else
		asm_fprintf(f, "MEM`\n");
	    }
	  else
	    {
	      asm_fprintf(f,"N.A.`\n");
	    }
	}
      parm = TREE_CHAIN (parm);
    }
}

/* Helper for INSN_COST.

   Per Segher Boessenkool: rtx_costs computes the cost for any rtx (an
   insn, a set, a set source, any random piece of one).  set_src_cost,
   set_rtx_cost, etc. are helper functions that use that.

   Those functions do not work for parallels.  Also, costs are not
   additive like this simplified model assumes.  Also, more complex
   backends tend to miss many cases in their rtx_costs function.

   Many passes that want costs want to know the cost of a full insn.  Like
   combine.  That's why I created insn_cost: it solves all of the above
   problems.  */

static int
arc64_insn_cost (rtx_insn *insn, bool speed)
{
  int cost;

  /* Needed for ifcvt.  */
  if (GET_CODE (PATTERN (insn)) == USE)
    return 1;

  if (recog_memoized (insn) < 0)
    return 0;


  /* Use cost if provided.  */
  cost = get_attr_cost (insn);
  if (cost > 0)
    return cost;

  cost = pattern_cost (PATTERN (insn), speed);
  return cost;
#if 0
  /* If optimizing for size, we want the insn size.  */
  if (!speed)
    return get_attr_length (insn);

  /* Use cost if provided.  */
  cost = get_attr_cost (insn);
  if (cost > 0)
    return cost;

  /* For speed make a simple cost model: memory access is more
     expensive than any other instruction.  */
  enum attr_type type = get_attr_type (insn);

  switch (type)
    {
    case TYPE_LD:
    case TYPE_ST:
      cost = COSTS_N_INSNS (2);
      break;

    default:
      cost = COSTS_N_INSNS (1);
      break;
    }

  return cost;
#endif
}

/* Helper for arc64_short_access_p.  */

static bool
check_short_insn_register_p (rtx op, bool hclass_p)
{
  if (!REG_P (op))
    return false;

  return (REGNO (op) >= FIRST_PSEUDO_REGISTER
	  || COMPACT_REG_P (REGNO (op))
	  || (hclass_p && (REGNO (op) <= R30_REGNUM)));
}

/* Helper for arc64_short_access_p.  */

static bool
check_short_insn_constant_p (rtx op, machine_mode mode)
{
  HOST_WIDE_INT ival;

  if (!CONST_INT_P (op))
    return false;

  ival = INTVAL (op);

  /* Check u5, u6, u7 short immediates.  */
  if (VERIFY_SHIFT (ival, ARC64LOG2 (GET_MODE_SIZE (mode)))
      && UNSIGNED_INT5 (ival >> ARC64LOG2 (GET_MODE_SIZE (mode))))
    return true;

  return false;
}

/* Output code to add DELTA to the first argument, and then jump to
   FUNCTION.  Used for C++ multiple inheritance.  */

static void
arc64_output_mi_thunk (FILE *file,
		       tree thunk_fndecl,
		       HOST_WIDE_INT delta,
		       HOST_WIDE_INT vcall_offset,
		       tree function)
{
  const char *fnname = IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (thunk_fndecl));
  rtx this_rtx, fnaddr, temp1;
  rtx_insn *insn;

  /* Pretend to be a post-reload pass while generating rtl.  */
  reload_completed = 1;

  /* Mark the end of the (empty) prologue.  */
  emit_note (NOTE_INSN_PROLOGUE_END);

  /* Determine if we can use a sibcall to call FUNCTION directly.  */
  fnaddr = gen_rtx_MEM (FUNCTION_MODE, XEXP (DECL_RTL (function), 0));

  /* We need one temporary register in some cases.  */
  temp1 = gen_rtx_REG (Pmode, R12_REGNUM);

  /* Find out which register contains the "this" pointer.  */
  if (aggregate_value_p (TREE_TYPE (TREE_TYPE (function)), function))
    this_rtx = gen_rtx_REG (Pmode, R1_REGNUM);
  else
    this_rtx = gen_rtx_REG (Pmode, R0_REGNUM);

  /* Add DELTA to THIS_RTX.  */
  if (delta != 0)
    {
      rtx offset = GEN_INT (delta);
      /* FIXME! check if delta fits in 32bit immediate.  Also we can
	 switch from an ADD to a SUB instruction.  */
      gcc_assert (UNSIGNED_INT32 (delta) || SIGNED_INT32 (delta));
      emit_insn (gen_rtx_SET (this_rtx,
			      gen_rtx_PLUS (Pmode, this_rtx, offset)));
    }

  if (vcall_offset != 0)
    {
      rtx addr;

      /* Set TEMP1 to *THIS_RTX.  */
      emit_insn (gen_rtx_SET (temp1, gen_rtx_MEM (Pmode, this_rtx)));

      /* Set ADDR to a legitimate address for *THIS_RTX + VCALL_OFFSET.  */
      /* FIXME! check if vcall_offset fits in 32bit immediate. */
      gcc_assert (UNSIGNED_INT32 (vcall_offset) || SIGNED_INT32 (vcall_offset));
      addr = plus_constant (Pmode, temp1, vcall_offset);

      /* Load the offset and add it to THIS_RTX.  */
      emit_insn (gen_rtx_SET (temp1, gen_rtx_MEM (Pmode, addr)));
      emit_insn (gen_add3_insn (this_rtx, this_rtx, temp1));
    }

  /* Jump to the target function.  */
  insn = emit_call_insn (gen_sibcall (fnaddr, const0_rtx, const0_rtx));
  SIBLING_CALL_P (insn) = 1;

  /* Run just enough of rest_of_compilation.  This sequence was
     "borrowed" from alpha.c.  */
  insn = get_insns ();
  split_all_insns_noflow ();
  shorten_branches (insn);
  assemble_start_function (thunk_fndecl, fnname);
  final_start_function (insn, file, 1);
  final (insn, file, 1);
  final_end_function ();
  assemble_end_function (thunk_fndecl, fnname);

  /* Stop pretending to be a post-reload pass.  */
  reload_completed = 0;
}

/* Helper INIT_EXPANDERS.  */

static struct machine_function *
arc64_init_machine_status (void)
{
  struct machine_function *machine;
  machine = ggc_cleared_alloc<machine_function> ();
  return machine;
}

static tree
arc64_builtin_decl (unsigned id, bool initialize_p ATTRIBUTE_UNUSED)
{
  if (id < ARC64_BUILTIN_COUNT)
    return arc_bdesc[id].fndecl;

  return error_mark_node;
}

/* Transform UP into lowercase and write the result to LO.
   You must provide enough space for LO.  Return LO.  */

static char*
arc64_tolower (char *lo, const char *up)
{
  char *lo0 = lo;

  for (; *up; up++, lo++)
    *lo = TOLOWER (*up);

  *lo = '\0';

  return lo0;
}

/* Helper for adding the builtins.  */
static void
arc64_init_builtins (void)
{
  tree void_ftype_usint_usint
    = build_function_type_list (void_type_node, unsigned_type_node,
				unsigned_type_node, NULL_TREE);
  tree usint_ftype_usint
    = build_function_type_list  (long_unsigned_type_node,
				 unsigned_type_node, NULL_TREE);
  tree void_ftype_void
    = build_function_type_list (void_type_node, NULL_TREE);
  tree void_ftype_usint
    = build_function_type_list (void_type_node, unsigned_type_node,
				NULL_TREE);
  tree long_ftype_long
    = build_function_type_list (long_long_integer_type_node,
				long_long_integer_type_node, NULL_TREE);

  tree void_ftype_long_long
    = build_function_type_list (void_type_node, long_long_integer_type_node,
				long_long_integer_type_node, NULL_TREE);

  /* Add the builtins.  */
#define DEF_BUILTIN(NAME, N_ARGS, TYPE, ICODE, MASK)			\
  {									\
    int id = ARC64_BUILTIN_ ## NAME;					\
    const char *Name = "__builtin_arc_" #NAME;				\
    char *name = (char*) alloca (1 + strlen (Name));			\
									\
    gcc_assert (id < ARC64_BUILTIN_COUNT);				\
    if (MASK)								\
      arc_bdesc[id].fndecl						\
	= add_builtin_function (arc64_tolower(name, Name), TYPE, id,	\
				BUILT_IN_MD, NULL, NULL_TREE);		\
  }
#include "builtins.def"
#undef DEF_BUILTIN
}

/* Helper arc_expand_builtin, generates a pattern for the given icode
   and arguments.  */

static rtx_insn *
apply_GEN_FCN (enum insn_code icode, rtx *arg)
{
  switch (insn_data[icode].n_generator_args)
    {
    case 0:
      return GEN_FCN (icode) ();
    case 1:
      return GEN_FCN (icode) (arg[0]);
    case 2:
      return GEN_FCN (icode) (arg[0], arg[1]);
    case 3:
      return GEN_FCN (icode) (arg[0], arg[1], arg[2]);
    case 4:
      return GEN_FCN (icode) (arg[0], arg[1], arg[2], arg[3]);
    case 5:
      return GEN_FCN (icode) (arg[0], arg[1], arg[2], arg[3], arg[4]);
    default:
      gcc_unreachable ();
    }
}

/* Expand an expression EXP that calls a built-in function,
   with result going to TARGET if that's convenient
   (and in mode MODE if that's convenient).
   SUBTARGET may be used as the target for computing one of EXP's operands.
   IGNORE is nonzero if the value is to be ignored.  */

static rtx
arc64_expand_builtin (tree exp,
		      rtx target,
		      rtx subtarget ATTRIBUTE_UNUSED,
		      machine_mode mode ATTRIBUTE_UNUSED,
		      int ignore ATTRIBUTE_UNUSED)
{
  tree fndecl = TREE_OPERAND (CALL_EXPR_FN (exp), 0);
  unsigned int id = DECL_FUNCTION_CODE (fndecl);
  const struct arc64_builtin_description *d = &arc_bdesc[id];
  int i, j, n_args = call_expr_nargs (exp);
  rtx pat = NULL_RTX;
  rtx xop[5];
  enum insn_code icode = d->icode;
  machine_mode tmode = insn_data[icode].operand[0].mode;
  int nonvoid;
  tree arg0;
  rtx op0;

  if (id >= ARC64_BUILTIN_COUNT)
    internal_error ("bad builtin fcode");

  /* 1st part: Expand special builtins.  */
  switch (id)
    {
    case ARC64_BUILTIN_NOP:
      emit_insn (gen_nopv ());
      return NULL_RTX;

    case ARC64_BUILTIN_BRK:
      gcc_assert (icode != 0);
      emit_insn (GEN_FCN (icode) (const1_rtx));
      return NULL_RTX;

    case ARC64_BUILTIN_TRAP_S:
      arg0 = CALL_EXPR_ARG (exp, 0);
      fold (arg0);
      op0 = expand_expr (arg0, NULL_RTX, VOIDmode, EXPAND_NORMAL);

      gcc_assert (icode != 0);
      emit_insn (GEN_FCN (icode) (op0));
      return NULL_RTX;
    default:
      break;
    }

  /* 2nd part: Expand regular builtins.  */
  if (icode == 0)
    internal_error ("bad builtin fcode");

  nonvoid = TREE_TYPE (TREE_TYPE (fndecl)) != void_type_node;
  j = 0;

  if (nonvoid)
    {
      if (target == NULL_RTX
	  || GET_MODE (target) != tmode
	  || !insn_data[icode].operand[0].predicate (target, tmode))
	{
	  target = gen_reg_rtx (tmode);
	}
      xop[j++] = target;
    }

  gcc_assert (n_args <= 4);
  for (i = 0; i < n_args; i++, j++)
    {
      tree arg = CALL_EXPR_ARG (exp, i);
      machine_mode mode = insn_data[icode].operand[j].mode;
      rtx op = expand_expr (arg, NULL_RTX, mode, EXPAND_NORMAL);
      machine_mode opmode = GET_MODE (op);

      if (CONST_INT_P (op))
	opmode = mode;

      if ((opmode == SImode) && (mode == HImode))
	{
	  opmode = HImode;
	  op = gen_lowpart (HImode, op);
	}

      /* In case the insn wants input operands in modes different from
	 the result, abort.  */
      gcc_assert (opmode == mode || opmode == VOIDmode);

      if (!insn_data[icode].operand[i + nonvoid].predicate (op, mode))
	op = copy_to_mode_reg (mode, op);

      xop[j] = op;
    }

  pat = apply_GEN_FCN (icode, xop);
  if (pat == NULL_RTX)
    return NULL_RTX;

  emit_insn (pat);

  if (nonvoid)
    return target;
  else
    return const0_rtx;
}

/* A callback for the hw-doloop pass.  Called when a loop we have discovered
   turns out not to be optimizable; we have to split the loop_end pattern into
   a subtract and a test.  */

static void
hwloop_fail (hwloop_info loop)
{
  rtx test;
  rtx insn;

  if (TARGET_64BIT)
    emit_insn_before (gen_adddi_cmp0 (loop->iter_reg,
				      loop->iter_reg,
				      constm1_rtx),
		      loop->loop_end);
  else
    emit_insn_before (gen_addsi_cmp0 (loop->iter_reg,
				      loop->iter_reg,
				      constm1_rtx),
		      loop->loop_end);

  test = gen_rtx_NE (VOIDmode, gen_rtx_REG (CC_ZNmode, CC_REGNUM), const0_rtx);
  test = gen_rtx_IF_THEN_ELSE (VOIDmode, test,
			       gen_rtx_LABEL_REF (Pmode, loop->start_label),
			       pc_rtx);
  insn = emit_jump_insn_before (gen_rtx_SET (pc_rtx, test),
				loop->loop_end);

  JUMP_LABEL (insn) = loop->start_label;
  LABEL_NUSES (loop->start_label)++;
  delete_insn (loop->loop_end);
}

/* Optimize LOOP.  We just are checking that the loop isn't too long,
   returns true if so.  Return true if successful, false if the loop
   should be marked bad.  If it returns false, the FAIL function is
   called.  */

static bool
hwloop_optimize (hwloop_info loop)
{
  unsigned int length;

  /* Call shorten_branches to calculate the insn lengths.  */
  shorten_branches (get_insns());

  if (!INSN_ADDRESSES_SET_P ())
    {
      fprintf (dump_file, ";; loop %d has an unknown length\n", loop->loop_no);
      return false;
    }

  length = INSN_ADDRESSES (INSN_UID (loop->loop_end))
    - INSN_ADDRESSES (INSN_UID (loop->start_label));
  loop->length = length;
  if (dump_file)
    fprintf (dump_file, ";; loop %d with length %d\n", loop->loop_no,
	     loop->length);
  if (loop->length > MAX_LOOP_LENGTH
      || loop->length < MIN_LOOP_LENGTH)
    {
      if (dump_file)
	fprintf (dump_file, ";; loop %d is too long\n", loop->loop_no);
      return false;
    }
  if (loop->length == 0)
    {
      if (dump_file)
	fprintf (dump_file, ";; loop %d is empty\n", loop->loop_no);
      return false;
    }

  return true;
}

/* A callback for the hw-doloop pass.  This function examines INSN; if
   it is a loop_end pattern we recognize, return the reg rtx for the
   loop counter.  Otherwise, return NULL_RTX.  */

static rtx
hwloop_pattern_reg (rtx_insn *insn)
{
  rtx reg;

  if (!JUMP_P (insn)
      || (TARGET_64BIT && (recog_memoized (insn) != CODE_FOR_dbnzdi))
      || (!TARGET_64BIT && (recog_memoized (insn) != CODE_FOR_dbnzsi)))
    return NULL_RTX;

  reg = SET_DEST (XVECEXP (PATTERN (insn), 0, 1));
  if (!REG_P (reg))
    return NULL_RTX;
  return reg;
}

static struct hw_doloop_hooks arc64_doloop_hooks =
{
  hwloop_pattern_reg,
  hwloop_optimize,
  hwloop_fail
};

/* Machine specific reorg step.  */
static void
arc64_reorg (void)
{
  compute_bb_for_insn ();
  df_analyze ();
  reorg_loops (true, &arc64_doloop_hooks);

  /* Search MAC instructions and remove the super-flu move from
     accumulator to a register.  Hence, we try to repair what we do in
     madd expands or in mac* splits.  */
  for (rtx_insn *insn = get_insns (); insn; insn = next_real_insn (insn))
    {
      rtx op0, op1, op2, tmp;
      enum insn_code icode = CODE_FOR_nothing;
      machine_mode mode = E_VOIDmode;

      if (!INSN_P (insn))
	continue;

      /* 1st find the MAC instruction with null (accumulator)
	 output.  */
      switch (INSN_CODE (insn))
	{
	case CODE_FOR_umachi0:
	  icode = CODE_FOR_umachi;
	  mode = E_SImode;
	  break;

	case CODE_FOR_machi0:
	  icode = CODE_FOR_machi;
	  mode = E_SImode;
	  break;

	case CODE_FOR_umacd0:
	  icode = CODE_FOR_umacd;
	  mode = E_DImode;
	  break;

	case CODE_FOR_macd0:
	  icode = CODE_FOR_macd;
	  mode = E_DImode;
	  break;

	case CODE_FOR_macsi0:
	  icode = CODE_FOR_macsi;
	  mode = E_SImode;
	  break;

	case CODE_FOR_dmach0:
	  icode = CODE_FOR_dmach;
	  mode = E_HImode;
	  break;

	default:
	  continue;
	}

      gcc_assert (REGNO (SET_DEST (PATTERN (insn))) == R58_REGNUM);
      rtx_insn *nxt = next_real_insn (insn);

      /* 2nd Check if it is a move instruction.  */
      tmp = PATTERN (nxt);
      if (GET_CODE (tmp) != SET
	  || (GET_CODE (SET_SRC (tmp)) != REG)
	  || (GET_CODE (SET_DEST (tmp)) != REG))
	continue;

      op0 = SET_DEST (tmp);
      op1 = SET_SRC (tmp);
      if (REGNO (op1) != R58_REGNUM)
	continue;

      /* Make the new MAC instruction.  */
      switch (INSN_CODE (insn))
	{
	case CODE_FOR_umachi0:
	case CODE_FOR_umacd0:
	case CODE_FOR_machi0:
	case CODE_FOR_macd0:
	  if (!TARGET_64BIT && ((REGNO (op0) & 1) != 0))
	    continue;
	  tmp = SET_SRC (PATTERN (insn));
	  op1 = XEXP (XEXP (XEXP (tmp, 0), 0), 0);
	  op2 = XEXP (XEXP (XEXP (tmp, 0), 1), 0);
	  break;

	case CODE_FOR_dmach0:
	case CODE_FOR_macsi0:
	  tmp = SET_SRC (PATTERN (insn));
	  op1 = XEXP (XEXP (tmp, 0), 0);
	  op2 = XEXP (XEXP (tmp, 0), 1);
	  break;

	default:
	  gcc_unreachable ();
	}

      emit_insn_before (GEN_FCN (icode) (op0, op1, op2,
					 gen_rtx_REG (mode, R58_REGNUM)),
			insn);

      /* Remove the old MAC and MOV instruction.  */
      set_insn_deleted (insn);
      set_insn_deleted (nxt);
    }
}

/* Expand a compare and swap pattern.  */

static void
emit_unlikely_jump (rtx insn)
{
  rtx_insn *jump = emit_jump_insn (insn);
  add_reg_br_prob_note (jump, profile_probability::very_unlikely ());
}

/* Expand code to perform a 8 or 16-bit compare and swap by doing
   32-bit compare and swap on the word containing the byte or
   half-word.  The difference between a weak and a strong CAS is that
   the weak version may simply fail.  The strong version relies on two
   loops, one checks if the SCOND op is succsfully or not, the other
   checks if the 32 bit accessed location which contains the 8 or 16
   bit datum is not changed by other thread.  The first loop is
   implemented by the atomic_compare_and_swapsdi_1 pattern.  The second
   loops is implemented by this routine.  */

static void
arc_expand_compare_and_swap_qh (rtx bool_result, rtx result, rtx mem,
				rtx oldval, rtx newval, rtx weak,
				rtx mod_s, rtx mod_f)
{
  rtx addr1 = force_reg (Pmode, XEXP (mem, 0));
  rtx addr = gen_reg_rtx (Pmode);
  rtx off = gen_reg_rtx (SImode);
  rtx oldv = gen_reg_rtx (SImode);
  rtx newv = gen_reg_rtx (SImode);
  rtx oldvalue = gen_reg_rtx (SImode);
  rtx newvalue = gen_reg_rtx (SImode);
  rtx res = gen_reg_rtx (SImode);
  rtx resv = gen_reg_rtx (SImode);
  rtx memsi, val, mask, end_label, loop_label, cc, x;
  machine_mode mode;
  bool is_weak = (weak != const0_rtx);

  /* Truncate the address.  */
  emit_insn (gen_rtx_SET (addr,
			  gen_rtx_AND (Pmode, addr1, GEN_INT (-4))));

  /* Compute the datum offset.  */

  emit_insn (gen_rtx_SET (off, gen_rtx_AND (SImode,
					    gen_lowpart(SImode, addr1),
					    GEN_INT (3))));

  /* Normal read from truncated address.  */
  memsi = gen_rtx_MEM (SImode, addr);
  set_mem_alias_set (memsi, ALIAS_SET_MEMORY_BARRIER);
  MEM_VOLATILE_P (memsi) = MEM_VOLATILE_P (mem);

  val = copy_to_reg (memsi);

  /* Convert the offset in bits.  */
  emit_insn (gen_rtx_SET (off,
			  gen_rtx_ASHIFT (SImode, off, GEN_INT (3))));

  /* Get the proper mask.  */
  if (GET_MODE (mem) == QImode)
    mask = force_reg (SImode, GEN_INT (0xff));
  else
    mask = force_reg (SImode, GEN_INT (0xffff));

  emit_insn (gen_rtx_SET (mask,
			  gen_rtx_ASHIFT (SImode, mask, off)));

  /* Prepare the old and new values.  */
  emit_insn (gen_rtx_SET (val,
			  gen_rtx_AND (SImode, gen_rtx_NOT (SImode, mask),
				       val)));

  oldval = gen_lowpart (SImode, oldval);
  emit_insn (gen_rtx_SET (oldv,
			  gen_rtx_ASHIFT (SImode, oldval, off)));

  newval = gen_lowpart_common (SImode, newval);
  emit_insn (gen_rtx_SET (newv,
			  gen_rtx_ASHIFT (SImode, newval, off)));

  emit_insn (gen_rtx_SET (oldv,
			  gen_rtx_AND (SImode, oldv, mask)));

  emit_insn (gen_rtx_SET (newv,
			  gen_rtx_AND (SImode, newv, mask)));

  if (!is_weak)
    {
      end_label = gen_label_rtx ();
      loop_label = gen_label_rtx ();
      emit_label (loop_label);
    }

  /* Make the old and new values.  */
  emit_insn (gen_rtx_SET (oldvalue,
			  gen_rtx_IOR (SImode, oldv, val)));

  emit_insn (gen_rtx_SET (newvalue,
			  gen_rtx_IOR (SImode, newv, val)));

  /* Try an 32bit atomic compare and swap.  It clobbers the CC
     register.  */
  if (GET_MODE (mem) == SImode)
    emit_insn (gen_atomic_compare_and_swapsi_1 (res, memsi, oldvalue, newvalue,
						weak, mod_s, mod_f));
  else /* DImode */
    emit_insn (gen_atomic_compare_and_swapdi_1 (res, memsi, oldvalue, newvalue,
						weak, mod_s, mod_f));

  /* Regardless of the weakness of the operation, a proper boolean
     result needs to be provided.  */
  x = gen_rtx_REG (CC_Zmode, CC_REGNUM);
  x = gen_rtx_EQ (SImode, x, const0_rtx);
  emit_insn (gen_rtx_SET (bool_result, x));

  if (!is_weak)
    {
      /* Check the results: if the atomic op is successfully the goto
	 to end label.  */
      x = gen_rtx_REG (CC_Zmode, CC_REGNUM);
      x = gen_rtx_EQ (VOIDmode, x, const0_rtx);
      x = gen_rtx_IF_THEN_ELSE (VOIDmode, x,
				gen_rtx_LABEL_REF (Pmode, end_label), pc_rtx);
      emit_jump_insn (gen_rtx_SET (pc_rtx, x));

      /* Wait for the right moment when the accessed 32-bit location
	 is stable.  */
      emit_insn (gen_rtx_SET (resv,
			      gen_rtx_AND (SImode, gen_rtx_NOT (SImode, mask),
					   res)));
      mode = SELECT_CC_MODE (NE, resv, val);
      cc = gen_rtx_REG (mode, CC_REGNUM);
      emit_insn (gen_rtx_SET (cc, gen_rtx_COMPARE (mode, resv, val)));

      /* Set the new value of the 32 bit location, proper masked.  */
      emit_insn (gen_rtx_SET (val, resv));

      /* Try again if location is unstable.  Fall through if only
	 scond op failed.  */
      x = gen_rtx_NE (VOIDmode, cc, const0_rtx);
      x = gen_rtx_IF_THEN_ELSE (VOIDmode, x,
				gen_rtx_LABEL_REF (Pmode, loop_label), pc_rtx);
      emit_unlikely_jump (gen_rtx_SET (pc_rtx, x));

      emit_label (end_label);
    }

  /* End: proper return the result for the given mode.  */
  emit_insn (gen_rtx_SET (res,
			  gen_rtx_AND (SImode, res, mask)));

  emit_insn (gen_rtx_SET (res,
			  gen_rtx_LSHIFTRT (SImode, res, off)));

  emit_move_insn (result, gen_lowpart (GET_MODE (result), res));
}


/* This hook may conditionally modify five variables: fixed_regs,
   call_used_regs, global_regs, reg_names and reg_class_contents.  */

static void
arc64_conditional_register_usage (void)
{
  int regno;

  /* When having floating point, we enable the registers to be used by compiler
     and set the appropriate call used registers (i.e., f0-f15).  */
  if (ARC64_HAS_FP_BASE)
    {
      for (regno = F0_REGNUM; regno <= F31_REGNUM; regno++)
	{
	  fixed_regs[regno] = 0;
	  call_used_regs[regno] = (regno < F16_REGNUM) ? 1 : 0;
	}
    }
}

/* Implement TARGET_LIBGCC_FLOATING_POINT_MODE_SUPPORTED_P - return TRUE
   if MODE is HFmode, and punt to the generic implementation otherwise.  */

static bool
arc64_libgcc_floating_mode_supported_p (scalar_float_mode mode)
{
  return (mode == HFmode
	  ? ARC64_HAS_FPUH
	  : default_libgcc_floating_mode_supported_p (mode));
}

/* Implement TARGET_SCALAR_MODE_SUPPORTED_P - return TRUE
   if MODE is HFmode, and punt to the generic implementation otherwise.  */

static bool
arc64_scalar_mode_supported_p (scalar_mode mode)
{
  return (mode == HFmode
	  ? ARC64_HAS_FPUH
	  : default_scalar_mode_supported_p (mode));
}

/* Implements target hook vector_mode_supported_p.  */

static bool
arc64_vector_mode_supported_p (machine_mode mode)
{
  switch (mode)
    {
      /* 32-bit fp SIMD vectors.  */
    case E_V2HFmode:
      return ARC64_VFP_32;
      /* 64-bit fp SIMD vectors.  */
    case E_V4HFmode:
    case E_V2SFmode:
      return ARC64_VFP_64;
      /* 128-bit fp SIMD vectors.  */
    case E_V8HFmode:
    case E_V4SFmode:
    case E_V2DFmode:
      return ARC64_VFP_128;

      /* 32-bit SIMD vectors.  */
    case E_V2HImode:
      /* 64-bit SIMD vectors.  */
    case E_V4HImode:
    case E_V2SImode:
      return TARGET_SIMD;

    default:
      return false;
    }
}

/* Implements target hook TARGET_VECTORIZE_PREFERRED_SIMD_MODE.  */

static machine_mode
arc64_preferred_simd_mode (scalar_mode mode)
{
  switch (mode)
    {
    case E_HFmode:
      if (ARC64_VFP_128)
	return V8HFmode;
      if (ARC64_VFP_64)
	return V4HFmode;
      if (ARC64_VFP_32)
	return V2HFmode;
      return word_mode;

    case E_SFmode:
      if (ARC64_VFP_128)
	return V4SFmode;
      if (ARC64_VFP_64)
	return V2SFmode;
      return word_mode;

    case E_DFmode:
      if (ARC64_VFP_128)
	return V2DFmode;
      return word_mode;

    case E_HImode:
      return TARGET_SIMD ? V4HImode : word_mode;
    case E_SImode:
      return TARGET_SIMD ? V2SImode : word_mode;

    default:
      return word_mode;
    }
}

/* Implements target hook
   TARGET_VECTORIZE_AUTOVECTORIZE_VECTOR_MODES.  */

static unsigned int
arc64_autovectorize_vector_modes (vector_modes *modes, bool)
{
  if (ARC64_VFP_128)
    {
      modes->quick_push (V8HFmode);
      modes->quick_push (V4SFmode);
      modes->quick_push (V2DFmode);
    }
  else if (ARC64_VFP_64)
    {
      modes->quick_push (V4HFmode);
      modes->quick_push (V2SFmode);
    }
  else if (ARC64_VFP_32)
    modes->quick_push (V2HFmode);

  if (TARGET_SIMD)
    {
      modes->quick_push (V4HImode);
      modes->quick_push (V2SImode);
    }
  return 0;
}

/* Vectorization costs.  */
static int
arc64_builtin_vectorization_cost (enum vect_cost_for_stmt type_of_cost,
				  tree vectype,
				  int misalign ATTRIBUTE_UNUSED)
{
  unsigned elements;

  switch (type_of_cost)
    {
    case scalar_stmt:
      return 1;

    case scalar_load:
      return 1;

    case scalar_store:
      return 1;

    case vector_stmt:
      return 1; /* fp operations are more efficient than int.  */

    case vector_load:
      return 1;

    case vector_store:
      return 1;

    case vec_to_scalar:
      return 1; /* We have extract instructions.  */

    case scalar_to_vec:
      return 1; /* fp is more efficient than int.  */

    case unaligned_load:
    case vector_gather_load:
      return 1; /* Maybe I need to reflect unaligned flag here.  */

    case unaligned_store:
    case vector_scatter_store:
      return 1; /* Likewise.  */

    case cond_branch_taken:
      return 3; /* A jump is always expensive.  */

    case cond_branch_not_taken:
      return 1;

    case vec_perm:
      return 1; /* We don't really have vec_perm.  */

    case vec_promote_demote:
      return 1;

    case vec_construct:
      elements = estimated_poly_value (TYPE_VECTOR_SUBPARTS (vectype));
      return elements / 2;

    default:
      gcc_unreachable ();
    }
}

/* Return a new RTX holding the result of moving POINTER forward by
   AMOUNT bytes.  */

static rtx
arc64_move_pointer (rtx pointer, poly_int64 amount)
{
  rtx next = plus_constant (Pmode, XEXP (pointer, 0), amount);

  return adjust_automodify_address (pointer, GET_MODE (pointer),
				    next, amount);
}

/* Return a new RTX holding the result of moving POINTER forward by the
   size of the mode it points to.  */

static rtx
arc64_progress_pointer (rtx pointer)
{
  return arc64_move_pointer (pointer, GET_MODE_SIZE (GET_MODE (pointer)));
}

/* Copy one MODE sized block from SRC to DST, then progress SRC and DST by
   MODE bytes.  */

static void
arc64_copy_one_block_and_progress_pointers (rtx *src, rtx *dst,
					    machine_mode mode)
{
  rtx reg = gen_reg_rtx (mode);

  /* "Cast" the pointers to the correct mode.  */
  *src = adjust_address (*src, mode, 0);
  *dst = adjust_address (*dst, mode, 0);
  /* Emit the memcpy.  */
  emit_move_insn (reg, *src);
  emit_move_insn (*dst, reg);
  /* Move the pointers forward.  */
  *src = arc64_progress_pointer (*src);
  *dst = arc64_progress_pointer (*dst);
}

/* Moving f regs to r regs is not a very good idea. */
static int
arc64_register_move_cost (machine_mode,
			  reg_class_t from_class, reg_class_t to_class)
{
  if ((from_class == FP_REGS && to_class == GENERAL_REGS)
      || (to_class == FP_REGS && from_class == GENERAL_REGS))
    return 200;
  return 2;
}

/* Check/emit vector duplicate instructions.  */

static bool
arc64_simd_dup (struct e_vec_perm_d *d)
{
  machine_mode vmode = d->vmode;
  HOST_WIDE_INT elt;
  rtx t0, parallel, select;
  rtx in0 = d->op0;
  rtx out = d->target;

  if (!TARGET_64BIT
      || !d->one_vector_p
      || vmode == E_V2HImode
      || d->perm.encoding ().encoded_nelts () != 1
      || !d->perm[0].is_constant (&elt)
      /* elt is zero, then the vec_dup pattern does as good as we do here.  */
      || elt == 0)
    return false;

  if (d->testing_p)
    return true;

  switch (vmode)
    {
    case E_V8HFmode:
    case E_V4HFmode:
    case E_V2HFmode:
    case E_V2SFmode:
    case E_V4SFmode:
      if (elt != 0)
	{
	  t0 = gen_reg_rtx (GET_MODE_INNER (vmode));
	  parallel = gen_rtx_PARALLEL (vmode, gen_rtvec (1, GEN_INT (elt)));
	  select = gen_rtx_VEC_SELECT (GET_MODE_INNER (vmode), in0, parallel);
	  emit_set_insn (t0, select);
	  emit_set_insn (out, gen_rtx_VEC_DUPLICATE (vmode, t0));
	  return true;
	}

      /* FALLTHRU */
    case E_V2DFmode:
    case E_V2SImode:
      parallel = gen_rtx_PARALLEL (vmode, gen_rtvec (1, GEN_INT (elt)));
      select = gen_rtx_VEC_SELECT (GET_MODE_INNER (vmode), in0, parallel);
      emit_set_insn (out, gen_rtx_VEC_DUPLICATE (vmode, select));
      return true;

    case E_V4HImode:
      if (elt == 0)
	{
	  t0 = gen_reg_rtx (vmode);
	  emit_insn (gen_arc64_sel_lane2_0v4hi (t0, in0, in0));
	  emit_insn (gen_arc64_sel_lane2_0v4hi (out, t0, t0));
	  return true;
	}
      else if (elt == 1)
	{
	  t0 = gen_reg_rtx (vmode);
	  emit_insn (gen_arc64_sel_lane3_1v4hi (t0, in0, in0));
	  emit_insn (gen_arc64_sel_lane2_0v4hi (out, t0, t0));
	  return true;
	}
      else if (elt == 2)
	{
	  t0 = gen_reg_rtx (vmode);
	  emit_insn (gen_arc64_sel_lane2_0v4hi (t0, in0, in0));
	  emit_insn (gen_arc64_sel_lane3_1v4hi (out, t0, t0));
	  return true;
	}
      else if (elt == 3)
	{
	  t0 = gen_reg_rtx (vmode);
	  emit_insn (gen_arc64_sel_lane3_1v4hi (t0, in0, in0));
	  emit_insn (gen_arc64_sel_lane3_1v4hi (out, t0, t0));
	  return true;
	}
      break;
    default:
      gcc_unreachable ();
    }
  gcc_unreachable ();
}

/* Recognize VPACK instructions.  */

static bool
arc64_simd_vpack (struct e_vec_perm_d *d)
{
  HOST_WIDE_INT odd;
  poly_uint64 nelt = d->perm.length ();
  rtx out, in0, in1;
  machine_mode vmode = d->vmode;

  if (FLOAT_MODE_P (vmode)
      || !d->perm[0].is_constant (&odd)
      || (odd != 0 && odd != 1)
      || !d->perm.series_p (0, 1, odd, 2)
      || !d->perm.series_p (2, 1, nelt + odd, 2))
    return false;

  switch (vmode)
    {
    case E_V2SImode:
    case E_V4HImode:
      if (!TARGET_64BIT)
	return false;
      break;

    case E_V2HImode:
      break;

    default:
      return false;
    }

  /* Success!  */
  if (d->testing_p)
    return true;

  in0 = d->op0;
  in1 = d->op1;
  out = d->target;
  switch (vmode)
    {
    case E_V4HImode:
      if (odd)
	emit_insn (gen_arc64_sel_lane3_1v4hi (out, in0, in1));
      else
	emit_insn (gen_arc64_sel_lane2_0v4hi (out, in0, in1));
      break;

    case E_V2SImode:
      if (odd)
	emit_insn (gen_arc64_sel_lane1_v2si (out, in0, in1));
      else
	emit_insn (gen_arc64_sel_lane0_v2si (out, in0, in1));
      break;

    case E_V2HImode:
      if (odd)
	emit_insn (gen_arc64_sel_lane1_v2hi (out, in0, in1));
      else
	emit_insn (gen_arc64_sel_lane0_v2hi (out, in0, in1));
      break;

    default:
      gcc_unreachable ();
    }
  return true;
}

/* Reverse vector, recognize swapl and vfexch instructions.  */

static bool
arc64_simd_swapl (struct e_vec_perm_d *d)
{
  poly_uint64 nelt = d->perm.length ();
  machine_mode vmode = d->vmode;
  rtx t0, t1, t2, out, in0;
  rtx src;
  unsigned int unspec;

  if (GET_MODE_UNIT_SIZE (vmode) > 4
      || !TARGET_64BIT)
    return false;

  if (!d->one_vector_p)
    return false;

  if (!d->perm.series_p (0, 1, nelt - 1, -1))
    return false;

  /* Success! */
  if (d->testing_p)
    return true;

  in0 = d->op0;
  out = d->target;
  t0 = d->target;
  t1 = d->target;

  switch (vmode)
    {
    case E_V4HImode:
      t0 = gen_reg_rtx (vmode);
      t1 = gen_reg_rtx (vmode);
      t2 = gen_reg_rtx (vmode);
      emit_insn (gen_arc64_swapl (t0, in0));
      emit_insn (gen_arc64_swapv4hi (t1, in0));
      emit_insn (gen_arc64_swapv4hi (t2, t0));
      emit_insn (gen_arc64_swp_lane0_v4hi (out, t2, t1));
      break;

    case E_V2SImode:
      emit_insn (gen_arc64_swaplv2si (out, in0));
      break;

    case E_V2HImode:
      emit_insn (gen_arc64_swapv2hi (out, in0));
      break;

    case E_V8HFmode:
      t1 = gen_reg_rtx (vmode);
      /* Fall through.  */
    case E_V4SFmode:
      t0 = gen_reg_rtx (vmode);
      /* Fall through.  */
    case E_V2DFmode:
      unspec = ARC64_UNSPEC_DEXCH;
      src = gen_rtx_UNSPEC (vmode, gen_rtvec (1, in0), unspec);
      emit_set_insn (t0, src);
      if (vmode == E_V2DFmode)
	return true;

      unspec = ARC64_UNSPEC_SEXCH;
      src = gen_rtx_UNSPEC (vmode, gen_rtvec (1, t0), unspec);
      emit_set_insn (t1, src);
      if (vmode == E_V4SFmode)
	return true;

      unspec = ARC64_UNSPEC_HEXCH;
      src = gen_rtx_UNSPEC (vmode, gen_rtvec (1, t1), unspec);
      emit_set_insn (out, src);
      break;

    case E_V4HFmode:
      t1 = gen_reg_rtx (vmode);
      /* Fall through.  */
    case E_V2SFmode:
      unspec = ARC64_UNSPEC_SEXCH;
      src = gen_rtx_UNSPEC (vmode, gen_rtvec (1, in0), unspec);
      emit_set_insn (t1, src);
      if (vmode == E_V2SFmode)
	return true;
      in0 = t1;
      /* Fall through.  */

    case E_V2HFmode:
      unspec = ARC64_UNSPEC_HEXCH;
      src = gen_rtx_UNSPEC (vmode, gen_rtvec (1, in0), unspec);
      emit_set_insn (out, src);
      break;

    default:
      gcc_unreachable ();
    }
  return true;
}

/* Detect cases when we can use swap instruction.  */

static bool
arc64_simd_swap (struct e_vec_perm_d *d)
{
  rtx t0, t1, t2, out, in0;
  machine_mode vmode = d->vmode;

  if (vmode != E_V4HImode
      || !TARGET_64BIT)
    return false;

  if (!d->one_vector_p)
    return false;

  if (!d->perm.series_p (0, 2, 1, 2)
      || !d->perm.series_p (1, 2, 0, 2))
    return false;

  /* Success! */
  if (d->testing_p)
    return true;

  in0 = d->op0;
  out = d->target;

  t0 = gen_reg_rtx (vmode);
  t1 = gen_reg_rtx (vmode);
  t2 = gen_reg_rtx (vmode);
  emit_insn (gen_arc64_swapl (t0, in0));
  emit_insn (gen_arc64_swapv4hi (t1, in0));
  emit_insn (gen_arc64_swapv4hi (t2, t0));
  emit_insn (gen_arc64_swp_lane0_v4hi (out, t1, t2));
  return true;
}

/* Detect cases when we can use vapck2wl for 4xVectors.  */

static bool
arc64_simd_vpack2wl (struct e_vec_perm_d *d)
{
  machine_mode vmode = d->vmode;

  if (vmode != E_V4HImode
      || !TARGET_64BIT)
    return false;

  if (d->perm[0] != 0
      || d->perm[1] != 1
      || (d->perm[2] != 4 && d->perm[2] != 0)
      || (d->perm[3] != 5 && d->perm[3] != 1))
    return false;

  /* Success! */
  if (d->testing_p)
    return true;

  emit_insn (gen_arc64_swp_lane0_v4hi (d->target, d->op0, d->op1));
  return true;
}

static bool
arc64_simd_vpack2wm (struct e_vec_perm_d *d)
{
  machine_mode vmode = d->vmode;

  if (vmode != E_V4HImode
      || !TARGET_64BIT)
    return false;

  if (d->perm[0] != 2
      || d->perm[1] != 3
      || (d->perm[2] != 6 && d->perm[2] != 2)
      || (d->perm[3] != 7 && d->perm[3] != 3))
    return false;

  /* Success! */
  if (d->testing_p)
    return true;

  emit_insn (gen_arc64_swp_lane1_v4hi (d->target, d->op0, d->op1));
  return true;
}

/* Recognize patterns for {H,S,D}EXCH insns, which reverse elements:
   VFHEXCH (v2hf): h0 h1
   VFHEXCH (v4hf): h2 h3 h0 h1
   VFHEXCH (v8hf): h6 h7 h4 h5 h2 h3 h0 h1

   VFSEXCH (v4hf): h1h0 h3h2
   VFSEXCH (v8hf): h5h4 h7h6 h1h0 h3h2

   VFDEXCH (v8hf): h3h2h1h0 h7h6h5h4

   VFSEXCH (v2sf): s0 s1
   VFSEXCH (v4sf): s2 s3 s0 s1

   VFDEXCH (v4sf): s1s0 s3s2

   VFDEXCH (v2df): d0 d1
 */

static bool
arc64_simd_exch (struct e_vec_perm_d *d)
{
  HOST_WIDE_INT diff;
  unsigned int i, size, unspec;
  machine_mode vmode = d->vmode;

  if (!ARC64_HAS_FP_BASE
      || !FLOAT_MODE_P (vmode)
      || !d->one_vector_p
      || !d->perm[0].is_constant (&diff)
      || !diff)
    return false;

  size = diff * GET_MODE_UNIT_BITSIZE (vmode);
  if (size == 64)
    {
      if (!ARC64_HAS_FPUD)
	return false;
      unspec = ARC64_UNSPEC_DEXCH;
    }
  else if (size == 32)
    {
      unspec = ARC64_UNSPEC_SEXCH;
    }
  else if (size == 16)
    {
      unspec = ARC64_UNSPEC_HEXCH;
    }
  else
    return false;

  switch (diff)
    {
    case 1:
      for (i = 0; i < 2; i++)
	if (!d->perm.series_p (i, 2, diff - i, 2))
	  return false;
      break;

    case 2:
    case 4:
      for (i = 0; i < diff; i++)
	if (!d->perm.series_p (i, diff, diff + i, -diff))
	  return false;
      break;

    default:
      return false;
    }

  /* Success! */
  if (d->testing_p)
    return true;

  rtx src = gen_rtx_UNSPEC (vmode, gen_rtvec (1, d->op0), unspec);
  emit_set_insn (d->target, src);
  return true;
}

/* Recognize FV<P>UNPACKL/FV<P>UNPACKM instructions.

   VFHUNPKL (v2hf): Ch0 Bh0
   VFHUNPKL (v4hf): Ch2 Ch0 Bh2 Bh0
   VFHUNPKL (v8hf): Ch6 Ch4 Ch2 Ch0 Bh6 Bh4 Bh2 Bh0

   VFSUNPKL (v4hf): Ch1Ch0 Bh1Bh0
   VFSUNPKL (v8hf): Ch5Ch4 Ch1Ch0 Bh5Bh4 Bh1Bh0

   VFDUNPKL (v8hf): Ch3Ch2Ch1Ch0 Bh3Bh2Bh1Bh0

   VFSUNPKL (v2sf): Cs0 Bs0
   VFSUNPKL (v4sf): Cs2 Cs0 Bs2 Bs0

   VFDUNPKL (v4sf): Cs1Cs0 Bs1Bs0

   VFDUNPKL (v2df): Cd0 Bd0

   VFHUNPKM (v2hf): Ch1 Bh1
   VFHUNPKM (v4hf): Ch3 Ch1 Bh3 Bh1
   VFHUNPKM (v8hf): Ch7 Ch5 Ch3 Ch1 Bh7 Bh5 Bh3 Bh1

   VFSUNPKM (v4hf): Ch3Ch2 Bh3Bh2
   VFSUNPKM (v8hf): Ch7Ch6 Ch3Ch2 Bh7Bh6 Bh3Bh2

   VFDUNPKM (v8hf): Ch7Ch6Ch5Ch4 Bh7Bh6Bh5Bh4

   VFSUNPKM (v2sf): Cs1 Bs1
   VFSUNPKM (v4sf): Cs3 Cs1 Bs3 Bs1

   VFDUNPKM (v4sf): Cs3Cs2 Bs3Bs2

   VFDUNPKM (v2df): Cd1 Bd1
 */

static bool
arc64_simd_unpk (struct e_vec_perm_d *d)
{
  HOST_WIDE_INT odd, lo;
  poly_uint64 nelt = d->perm.length ();
  unsigned int i, j, size, unspec, diff = 0;
  machine_mode vmode = d->vmode;

  if (!ARC64_HAS_FP_BASE
      || !FLOAT_MODE_P (vmode)
      || !d->perm[0].is_constant (&odd)
      || (odd == 3)
      || (odd < 0 && odd > (HOST_WIDE_INT)(nelt >> 1)))
    return false;

  /* If ODD is set, then diff == odd.  Thus, the below condition should
     hold.  */
  lo = (odd == 0) ? 1 : odd;
  for (i = 4; (i >= lo) && (diff == 0); i >>= 1)
    {
      bool found = true;
      for (j = 0; (j < i) && found; j++)
	if (!d->perm.series_p (j, i, odd + j, i * 2 )
	    || !d->perm.series_p ((nelt >> 1) + j, i, nelt + odd + j, i * 2))
	  found = false;
      if (found)
	diff = i;
    }

  size = diff * GET_MODE_UNIT_BITSIZE (vmode);
  if (size == 64)
    {
      if (!ARC64_HAS_FPUD)
	return false;
      unspec = odd ? ARC64_UNSPEC_DUNPKM : ARC64_UNSPEC_DUNPKL;
    }
  else if (size == 32)
    {
      unspec = odd ? ARC64_UNSPEC_SUNPKM : ARC64_UNSPEC_SUNPKL;
    }
  else if (size == 16)
    {
      unspec = odd ? ARC64_UNSPEC_HUNPKM : ARC64_UNSPEC_HUNPKL;
    }
  else
    return false;

  /* Success!  */
  if (d->testing_p)
    return true;

  rtx src = gen_rtx_UNSPEC (vmode, gen_rtvec (2, d->op0, d->op1), unspec);
  emit_set_insn (d->target, src);
  return true;
}

/* Recognize VF<p>PACKL and VF<p>PACKM instructions.

   VFHPACKL (v2hf): Ch0 Bh0
   VFHPACKL (v4hf): Ch1 Bh1 Ch0 Bh0
   VFHPACKL (v8hf): Ch3 Bh3 Ch2 Bh2 Ch1 Bh1 Ch0 Bh0

   VFSPACKL (v4hf): Ch1Ch0 Bh1Bh0
   VFSPACKL (v8hf): Ch3Ch2 Bh3Bh2 Ch1Ch0 Bh1Bh0

   VFDPACKL (v8hf): Ch3Ch2Ch1Ch0 Bh3Bh2Bh1Bh0

   VFSPACKL (v2sf): Cs0 Bs0
   VFSPACKL (v4sf): Cs1 Bs1 Cs0 Bs0

   VFDPACKL (v4sf): Cs1Cs0 Bs1Bs0

   VFDPACKL (v2df): Cd0 Bd0


   VFHPACKM (v2hf): Ch1 Bh1
   VFHPACKM (v4hf): Ch3 Bh3 Ch2 Bh2
   VFHPACKM (v8hf): Ch7 Bh7 Ch6 Bh6 Ch5 Bh5 Ch4 Bh4

   VFSPACKM (v4hf): Ch3Ch2 Bh3Bh2
   VFSPACKM (v8hf): Ch7Ch6 Bh7Bh6 Ch5Ch4 Bh5Bh4

   VFDPACKM (v8hf): Ch7Ch6Ch5Ch4 Bh7Bh6Bh5Bh4

   VFSPACKM (v2sf): Cs1 Bs1
   VFSPACKM (v4sf): Cs3 Bs3 Cs2 Bs2

   VFDPACKM (v4sf): Cs3Cs2 Bs3Bs2

   VFDPACKM (v2df): Cd1 Bd1
 */

static bool
arc64_simd_pack (struct e_vec_perm_d *d)
{
  HOST_WIDE_INT odd;
  poly_uint64 nelt = d->perm.length ();
  unsigned int i, j, size, unspec, diff = 0;
  machine_mode vmode = d->vmode;

  if (!ARC64_HAS_FP_BASE
      || !FLOAT_MODE_P (vmode)
      || !d->perm[0].is_constant (&odd)
      || (odd != 0 && odd != (HOST_WIDE_INT)(nelt >> 1)))
    return false;

  for (i = 4; (i > 0) && (diff == 0); i >>= 1)
    {
      bool found = true;
      for (j = 0; (j < i) && found; j++)
	if (!d->perm.series_p (j, 2 * i, odd + j, i)
	    || !d->perm.series_p (i + j, 2 * i, nelt + odd + j, i))
	  found = false;
      if (found)
	diff = i;
    }

  size = diff * GET_MODE_UNIT_BITSIZE (vmode);
  if (size == 64)
    {
      if (!ARC64_HAS_FPUD)
	return false;
      unspec = odd ? ARC64_UNSPEC_DPACKM : ARC64_UNSPEC_DPACKL;
    }
  else if (size == 32)
    {
      unspec = odd ? ARC64_UNSPEC_SPACKM : ARC64_UNSPEC_SPACKL;
    }
  else if (size == 16)
    {
      unspec = odd ? ARC64_UNSPEC_HPACKM : ARC64_UNSPEC_HPACKL;
    }
  else
    return false;

  /* Success!  */
  if (d->testing_p)
    return true;

  rtx src = gen_rtx_UNSPEC (vmode, gen_rtvec (2, d->op0, d->op1), unspec);
  emit_set_insn (d->target, src);
  return true;
}

/* Recognize VF<p>BFLYL and VF<p>BFLYM instructions.

   VFHBFLYL (v2hf): Ch0 Bh0
   VFHBFLYL (v4hf): Ch2 Bh2 Ch0 Bh0
   VFHBFLYL (v8hf): Ch6 Bh6 Ch4 Bh4 Ch2 Bh2 Ch0 Bh0

   VFSBFLYL (v4hf): Ch1Ch0 Bh1Bh0
   VFSBFLYL (v8hf): Ch5Ch4 Bh5Bh4 Ch1Ch0 Bh1Bh0

   VFDBFLYL (v8hf): Ch3Ch2Ch1Ch0 Bh3Bh2Bh1Bh0

   VFSBFLYL (v2sf): Cs0 Bs0
   VFSBFLYL (v4sf): Cs2 Bs2 Cs0 Bs0

   VFDBFLYL (v4sf): Cs1Cs0 Bs1Bs0

   VFDBFLYL (v2df): Cd0 Bd0


   VFHBFLYM (v2hf): Ch1 Bh1
   VFHBFLYM (v4hf): Ch3 Bh3 Ch1 Bh1
   VFHBFLYM (v8hf): Ch7 Bh7 Ch5 Bh5 Ch3 Bh3 Ch1 Bh1

   VFSBFLYM (v4hf): Ch3Ch2 Bh3Bh2
   VFSBFLYM (v8hf): Ch7Ch6 Bh7Bh6 Ch3Ch2 Bh3Bh2

   VFDBFLYM (v8hf): Ch7Ch6Ch5Ch4 Bh7Bh6Bh5Bh4

   VFSBFLYM (v2sf): Cs1 Bs1
   VFSBFLYM (v4sf): Cs3 Bs3 Cs1 Bs1

   VFDBFLYM (v4sf): Cs3Cs2 Bs3Bs2

   VFDBFLYM (v2df): Cd1 Bd1
 */

static bool
arc64_simd_bfly (struct e_vec_perm_d *d)
{
  HOST_WIDE_INT odd;
  poly_uint64 nelt = d->perm.length ();
  unsigned int i, j, size, unspec, diff = 0;
  machine_mode vmode = d->vmode;

  if (!ARC64_HAS_FP_BASE
      || !FLOAT_MODE_P (vmode)
      || !d->perm[0].is_constant (&odd)
      || (odd == 3)
      || (odd < 0 && odd > (HOST_WIDE_INT)(nelt >> 1)))
    return false;

  for (i = 4; (i > 0) && (diff == 0); i >>= 1)
    {
      bool found = true;
      for (j = 0; (j < i) && found; j++)
	if (!d->perm.series_p (j, 2 * i, odd + j, 2 * i)
	    || !d->perm.series_p (i + j, 2 * i, nelt + odd + j, 2 * i))
	  found = false;
      if (found)
	diff = i;
    }

  size = diff * GET_MODE_UNIT_BITSIZE (vmode);
  if (size == 64)
    {
      if (!ARC64_HAS_FPUD)
	return false;
      unspec = odd ? ARC64_UNSPEC_DBFLYM : ARC64_UNSPEC_DBFLYL;
    }
  else if (size == 32)
    {
      unspec = odd ? ARC64_UNSPEC_SBFLYM : ARC64_UNSPEC_SBFLYL;
    }
  else if (size == 16)
    {
      unspec = odd ? ARC64_UNSPEC_HBFLYM : ARC64_UNSPEC_HBFLYL;
    }
  else
    return false;

  /* Success!  */
  if (d->testing_p)
    return true;

  rtx src = gen_rtx_UNSPEC (vmode, gen_rtvec (2, d->op0, d->op1), unspec);
  emit_set_insn (d->target, src);
  return true;
}

/* Implement combination of vpack4hl/vpack4hm instructions.  */

static bool
arc64_simd_lane_pack (struct e_vec_perm_d *d)
{
  machine_mode vmode = d->vmode;
  HOST_WIDE_INT elem;
  poly_uint64 nelt = d->perm.length ();
  rtx t0, t1;
  rtx in0 = d->op0;
  rtx in1 = d->op1;
  rtx out = d->target;

  if (vmode != E_V4HImode
      || !TARGET_64BIT
      || !d->perm[0].is_constant (&elem)
      || (elem != 0 && elem != 2)
      || !d->perm.series_p (0, 2, elem, 1)
      || !d->perm.series_p (1, 2, elem + nelt, 1))
    return false;

  /* Success! */
  if (d->testing_p)
    return true;

  t0 = gen_reg_rtx (vmode);
  t1 = gen_reg_rtx (vmode);
  emit_insn (gen_arc64_sel_lane2_0v4hi (t0, in0, in1));
  emit_insn (gen_arc64_sel_lane3_1v4hi (t1, in0, in1));
  if (elem == 0)
    emit_insn (gen_arc64_sel_lane2_0v4hi (out, t0, t1));
  else
    emit_insn (gen_arc64_sel_lane3_1v4hi (out, t0, t1));
  return true;
}

/* Implement TARGET_VECTORIZE_VEC_PERM_CONST.  */

static bool
arc64_vectorize_vec_perm_const (machine_mode vmode, rtx target, rtx op0,
				rtx op1, const vec_perm_indices &sel)
{
  struct e_vec_perm_d d;

  /* Check whether the mask can be applied to a single vector.  */
  if (sel.ninputs () == 1
      || (op0 && rtx_equal_p (op0, op1)))
    d.one_vector_p = true;
  else if (sel.all_from_input_p (0))
    {
      d.one_vector_p = true;
      op1 = op0;
    }
  else if (sel.all_from_input_p (1))
    {
      d.one_vector_p = true;
      op0 = op1;
    }
  else
    d.one_vector_p = false;

  d.perm.new_vector (sel.encoding (), d.one_vector_p ? 1 : 2,
		     sel.nelts_per_input ());
  d.vmode = vmode;
  d.target = target;
  d.op0 = op0 ? force_reg (vmode, op0) : NULL_RTX;
  if (op0 == op1)
    d.op1 = op1;
  else
    d.op1 = op1 ? force_reg (vmode, op1) : NULL_RTX;
  d.testing_p = !target;

  /* The pattern matching functions above are written to look for a small
     number to begin the sequence (0, 1, N/2).  If we begin with an index
     from the second operand, we can swap the operands.  */
  poly_int64 nelt = d.perm.length ();
  if (known_ge (d.perm[0], nelt))
    {
      d.perm.rotate_inputs (1);
      std::swap (d.op0, d.op1);
    }
  if (known_gt (nelt, 1))
    {
      if (arc64_simd_dup (&d))
	return true;
      else if (arc64_simd_vpack (&d))
	return true;
      else if (arc64_simd_swapl (&d))
	return true;
      else if (arc64_simd_swap (&d))
	return true;
      else if (arc64_simd_vpack2wl (&d))
	return true;
      else if (arc64_simd_vpack2wm (&d))
	return true;
      else if (arc64_simd_exch (&d))
	return true;
      else if (arc64_simd_unpk (&d))
	return true;
      else if (arc64_simd_pack (&d))
	return true;
      else if (arc64_simd_bfly (&d))
	return true;
      else if (arc64_simd_lane_pack (&d))
	return true;
    }
  return false;
}

/* Provide the costs of an addressing mode that contains ADDR.
   LOAD_P is true when address is used to load a value.  */

static int
arc64_address_cost (rtx addr, machine_mode mode,
		    addr_space_t as  ATTRIBUTE_UNUSED,
		    bool speed)
{
  const int cost_limm = speed ? 0 : COSTS_N_INSNS (1);

  if (CONSTANT_P (addr))
    return cost_limm;

  /* The cheapest construct are the addresses which fit a store
     instruction (or a fp load/store instruction).  */
  if (arc64_legitimate_address_1_p (mode, addr, true, false, true))
    switch (GET_CODE (addr))
      {
      case PRE_DEC:
      case PRE_INC:
      case POST_DEC:
      case POST_INC:
      case PRE_MODIFY:
      case POST_MODIFY:
	return 0;

      default:
	return 1;
      }

  /* Anything else has a limm.  */
  return cost_limm + 2;
}

/* Compute the rtx cost.  */

static bool
arc64_rtx_costs (rtx x, machine_mode mode, rtx_code outer,
                int opno ATTRIBUTE_UNUSED, int *cost, bool speed)
{
  rtx op0, op1;
  const int cost_limm = speed ? 0 : COSTS_N_INSNS (1);
  int factor;

  /* If we use a mode larger than UNITS_PER_WORD factor it in.  N.B. The cost is
     already factored in, however, the costs for MULT and DIV is too large.  */
  factor = CEIL (GET_MODE_SIZE (mode), UNITS_PER_WORD);

  switch (GET_CODE (x))
    {
    case SET:
      op0 = SET_DEST (x);
      op1 = SET_SRC (x);

      switch (GET_CODE (op0))
	{
	case MEM:
	  /* Store instruction.  */

	  if ((factor == 2) && DOUBLE_LOAD_STORE)
	    *cost = COSTS_N_INSNS (1);
	  *cost += arc64_address_cost (XEXP (op0, 0), mode, 0, speed);
	  if (CONST_INT_P (op1))
	    {
	      *cost += speed ? 0 :
		satisfies_constraint_S06S0 (op1) ? 0 : cost_limm;
	      return true;
	    }

	  *cost += rtx_cost (op1, mode, SET, 1, speed);
	  return true;

	case SUBREG:
	  if (!REG_P (SUBREG_REG (op0)))
	    *cost += rtx_cost (SUBREG_REG (op0), VOIDmode, SET, 0, speed);

	  /* Fall through.  */
	case REG:
	  /* Cost is just the cost of the RHS of the set.  */
	  *cost += rtx_cost (op1, mode, SET, 1, speed);
	  return true;

	default:
	  break;
	}
      return false;

    case MEM:
      /* Generic/loads.  */

      if ((factor == 2) && DOUBLE_LOAD_STORE)
	*cost = COSTS_N_INSNS (1);
      *cost += arc64_address_cost (XEXP (x, 0), mode, 0, speed);
      return true;

    case MINUS:
    case PLUS:
      op0 = XEXP (x, 0);
      op1 = XEXP (x, 1);

      if ((mode != SImode) && (mode != DImode))
	*cost += 1;

      /* Check if we have add{1,2,3} instruction.  */
      if ((GET_CODE (op0) == ASHIFT
	   && _1_2_3_operand (XEXP (op0, 1), VOIDmode))
	  || (GET_CODE (op0) == MULT
	      && _2_4_8_operand (XEXP (op0, 1), VOIDmode)))
	{
	  /* Check if 2nd instruction operand is constant int.  This
	     always goes as limm.  */
	  if (CONST_INT_P (op1))
	    *cost += cost_limm ;
	}
      return true;
      break;

    case COMPARE:
      op0 = XEXP (x, 0);
      op1 = XEXP (x, 1);

      /* Vitually, any instruction can do compare with zero.  */
      if (op1 == const0_rtx)
	*cost = 0;
      return true;

    case ZERO_EXTEND:
      op0 = XEXP (x, 0);

      /* Zero extending from an SI operation is cheap.  */
      if (MEM_P (op0))
	{
	  /* All loads can zero extend to any size for free.  */
	  *cost = rtx_cost (op0, VOIDmode, ZERO_EXTEND, 0, speed);
	  return true;
	}
      if (mode == DImode
	  && GET_MODE (op0) == SImode
	  && outer == SET)
	{
	  int op_cost = rtx_cost (op0, VOIDmode, ZERO_EXTEND, 0, speed);
	  if (op_cost)
	    *cost = op_cost;
	  return true;
	}
      break;

    case SIGN_EXTEND:
      op0 = XEXP (x, 0);
      if (MEM_P (op0))
	{
	  /* All loads can sign extend to any size for free.  */
	  *cost = rtx_cost (op0, VOIDmode, SIGN_EXTEND, 0, speed);
	  return true;
	}
      *cost += COSTS_N_INSNS (2);
      break;

    case CONST_INT:
      {
	HOST_WIDE_INT imm = INTVAL (x);

	/* In general any 32bit constant can be loaded immediately,
	   however, when we compile for speed, we try to avoid
	   them.  */
	*cost = 0;
	if (UNSIGNED_INT6 (imm))
	  return true;
	else
	  switch (outer)
	    {
	    case SET:
	      if (SIGNED_INT12 (imm))
		return true;
	      break;

	    default:
	      break;
	    }
      }
      /* FALLTHRU */
    case CONST:
    case LABEL_REF:
    case SYMBOL_REF:
      *cost = cost_limm;
      return true;

    case LSHIFTRT:
      op0 = XEXP (x, 0);
      if (REG_P (op0))
	return true;
      break;

    case ASHIFT:
    case ASHIFTRT:
      return true;

    case MULT:
      op0 = XEXP (x, 0);
      /* Multiplication has a large latency, use adds and shifts.  */
      *cost = COSTS_N_INSNS (2);
      /* 64x64 multiplication is expensive.  */
      if (GET_MODE_SIZE (mode) != UNITS_PER_WORD
	  && (GET_CODE (op0) != ZERO_EXTEND
	      || GET_CODE (op0) != SIGN_EXTEND))
	*cost = COSTS_N_INSNS (3);
      else if (GET_MODE_SIZE (mode) == UNITS_PER_WORD * 2)
	*cost = factor * COSTS_N_INSNS (4);

      return true;

    case MOD:
    case UMOD:
    case DIV:
    case UDIV:
      /* Fav synthetic divs.  */
      *cost = factor * COSTS_N_INSNS (12);
      return true;

    case EQ:
    case NE:
      if (outer == IF_THEN_ELSE
	  && (GET_CODE (XEXP (x, 0)) == AND
	      || GET_CODE (XEXP (x, 0)) == ZERO_EXTRACT)
	  && XEXP (x, 1) == const0_rtx)
	{
	  *cost = 0;
	  return true;
	}
      break;

    case AND:
    case XOR:
    case IOR:
      op0 = XEXP (x, 0);
      op1 = XEXP (x, 1);

      if ((REG_P (op0) || REG_P (op1))
	  && (CONST_INT_P (op0) || CONST_INT_P (op1)))
	return true;

      /* Detect VPACK2HL instructions.  */
      if (TARGET_SIMD
	  && GET_CODE (op0) == AND
	  && GET_CODE (op1) == ASHIFT
	  && mode == E_SImode)
	return true;

      break;

    default:
      break;
    }
  return false;
}

/* Wrapper around arc64_rtx_costs, dumps the partial, or total cost
   calculated for X.  This cost is stored in *COST.  Returns true
   if the total cost of X was calculated.  */
static bool
arc64_rtx_costs_wrapper (rtx x, machine_mode mode, int outer,
			 int param, int *cost, bool speed)
{
  bool result = arc64_rtx_costs (x, mode, (rtx_code) outer, param, cost, speed);

  if (dump_file)
    {
      print_rtl_single (dump_file, x);
      fprintf (dump_file, "\nARC: %s cost: %d (%s)\n",
	       speed ? "Speed" : "Size",
	       *cost, result ? "final" : "partial");
    }

  return result;
}

/* Implement TARGET_SCHED_MACRO_FUSION_P.  Return true if target supports
   instruction fusion of some sort.  */

static bool
arc64_macro_fusion_p (void)
{
  /* When we use accumulators, make sure we schedule the producer/consumer of
     accumulator close to each others.  */
  return TARGET_SIMD;
}

/* Implement TARGET_SCHED_MACRO_FUSION_PAIR_P.  Return true if PREV and CURR
   should be kept together during scheduling.  */

static bool
arc64_macro_fusion_pair_p (rtx_insn *prev, rtx_insn *curr)
{
  rtx prev_set = single_set (prev);
  rtx curr_set = single_set (curr);
  /* prev and curr are simple SET insns i.e. no flag setting or branching.  */
  bool simple_sets_p = prev_set && curr_set && !any_condjump_p (curr);

  if (!arc64_macro_fusion_p ())
    return false;

  /* Don't handle anything with a jump.  FIXME! maybe it is interesting to keep
     the cmp and jcc together for latter folding into BRcc insn.  */
  if (!simple_sets_p)
    return false;

  /* 1st We are trying to match any MPY instruction which can have implicit
     accumulator write and any mac instruction.  */
  if (get_attr_type (prev) == TYPE_MPY
      && get_attr_type (curr) == TYPE_MAC)
    return true;

  /* 2nd We try to match any back to back mac instruction.  */
  if (get_attr_type (prev) == TYPE_MAC
      && (get_attr_type (curr) == TYPE_MAC))
    return true;
  if (get_attr_type (prev) == TYPE_VMAC2H
      && (get_attr_type (curr) == TYPE_VMAC2H))
    return true;

  /* 3rd Keep close to each other the MAC and the following MOV(L) rx,r58.  This
     pattern will be match in machine reorg and simplified to a simple MAC
     instruction.  */
  if (get_attr_type (curr) == TYPE_MOVE
      && REG_P (SET_SRC (curr_set))
      && REGNO (SET_SRC (curr_set)) == R58_REGNUM
      && get_attr_type (prev) == TYPE_MAC)
    return true;

#if 0
  /* Try to keep r58 setting close to any previous related instruction.  We may
     be able to merge those two into one instruction.  */
  rtx set_dest;
  set_dest = SET_DEST (curr_set);
  if (get_attr_type (curr) == TYPE_MOVE
      && REG_P (set_dest)
      && REGNO (set_dest) == R58_REGNUM
      && REG_P (SET_DEST (prev_set))
      && REG_P (SET_SRC (curr_set))
      && REGNO (SET_DEST (prev_set)) == REGNO (SET_SRC (curr_set)))
    return true;

  /* Try to keep any mac and any previous instruction close, dependency on add
     operand.  */
  if (get_attr_type (curr) == TYPE_MAC
      && REG_P (SET_DEST (prev_set))
      && GET_CODE (SET_SRC (curr_set)) == PLUS
      && REG_P (XEXP (SET_SRC (curr_set), 1))
      && REGNO (SET_DEST (prev_set)) != R58_REGNUM
      && REGNO (SET_DEST (prev_set)) == REGNO (XEXP (SET_SRC (curr_set), 1)))
    return true;
#endif
  return false;
}

static void
arc64_override_options (void)
{
  if (arcv3_cpu_string)
    {
      const char *p = arcv3_cpu_string;
      if (strncmp (p, "hs5", 3) == 0)
	TARGET_64BIT = false;
      else if (strncmp (p, "hs6", 3) == 0)
	TARGET_64BIT = true;
      else
	error ("%<-mcpu=%s%>s is not a valid CPU option.", arcv3_cpu_string);
      p += 3;
      if ( *p == '8')
	{
	  if (TARGET_64BIT)
	    {
	      target_flags |= MASK_WIDE_LDST;
	    }
	  else
	    {
	      target_flags |= MASK_LL64;
	    }
	  target_flags |= MASK_SIMD;
	}
    }

  if (TARGET_LL64 && TARGET_64BIT)
    {
      target_flags &= ~MASK_LL64;
      warning (0, "Option -mll64 is ignored because the target"
	          " is not 32-bit.");
    }
}

/* Return the fixed registers used for condition codes.  */

static bool
arc64_fixed_condition_code_regs (unsigned int *p1, unsigned int *p2)
{
  *p1 = CC_REGNUM;
  *p2 = INVALID_REGNUM;
  return true;
}

/* Return true if FUNC is a naked function.  */
static bool
arc64_naked_function_p (tree func)
{
  tree func_decl = func;
  if (func == NULL_TREE)
    func_decl = current_function_decl;
  return NULL_TREE != lookup_attribute ("naked", DECL_ATTRIBUTES (func_decl));
}

/* Implement 'TARGET_SET_CURRENT_FUNCTION'.  */

static void
arc64_set_current_function (tree decl)
{
  unsigned int fn_type = ARC64_FUNCTION_UNKNOWN;
  tree func_decl = decl;

  if (decl == NULL_TREE
      || current_function_decl == NULL_TREE
      || current_function_decl == error_mark_node
      || ! cfun->machine
      || cfun->machine->fn_type != ARC64_FUNCTION_UNKNOWN)
    return;

  /* Check if it is a naked function.  */
  if (arc64_naked_function_p (decl))
    fn_type |= ARC64_FUNCTION_NAKED;

  if (func_decl == NULL_TREE)
    func_decl = current_function_decl;

  /* Now see if this is an interrupt handler.  */
  if (lookup_attribute ("interrupt",
			TYPE_ATTRIBUTES (TREE_TYPE (func_decl))) != NULL_TREE)
    fn_type |= ARC64_FUNCTION_ILINK;

  if (!ARC_NAKED_P (fn_type) && !ARC_INTERRUPT_P (fn_type))
    fn_type |= ARC64_FUNCTION_NORMAL;

  cfun->machine->fn_type = fn_type;

  if (ARC_NAKED_P (fn_type) && ARC_INTERRUPT_P (fn_type))
    error ("function attributes %qs and %qs are mutually exclusive",
	   "interrupt", "naked");
}

/* Implement TARGET_ALLOCATE_STACK_SLOTS_FOR_ARGS.  */
static bool
arc64_allocate_stack_slots_for_args ()
{
  /* Naked functions should not allocate stack slots for arguments.  */
  return !arc64_naked_function_p (current_function_decl);
}

/* Implement TARGET_WARN_FUNC_RETURN.  */
static bool
arc64_warn_func_return (tree decl)
{
  /* Naked functions are implemented entirely in assembly, including the
     return sequence, so suppress warnings about this.  */
  return !arc64_naked_function_p (decl);
}

/* Return false for selected jumps crossing between hot and cold partitions.  */

static bool
arc64_can_follow_jump (const rtx_insn *br1, const rtx_insn *br2)
{
  /* Avoid compiler warnings.  */
  union {const rtx_insn *c; rtx_insn *r;} u;

  u.c = br1;
  if (flag_reorder_blocks_and_partition
      && CROSSING_JUMP_P (br2))
    switch (get_attr_type (u.r))
      {
      case TYPE_BRANCHCC:
      case TYPE_BRCC:
	return false;
      case TYPE_BRANCH:
	if (get_attr_length (u.r) == 2)
	  return false;
	break;
      default:
	break;
      }

  return true;
}

/* Implements target hook TARGET_SCHED_ISSUE_RATE.  */

static int
arc64_sched_issue_rate (void)
{
  return 2;
}

/*
  Global functions.
*/

/* Returns TRUE if CALLEE should be treated as long-calls (i.e. called
   via a register).  */

bool
arc64_is_long_call_p (rtx sym)
{
  arc64_symb symb_t = arc64_get_symbol_type (sym);

  /* No subtleties for the time being, if user asks for large memory model,
     everything goes via regs.  */
  if (!TARGET_64BIT
      && (arc64_cmodel_var == ARC64_CMODEL_LARGE))
    return true;

  switch (symb_t)
    {
    case ARC64_UNK:
    case ARC64_LO32:
      return false;

    case ARC64_PCREL:
    case ARC64_PIC:
      return false;

    case ARC64_LPIC:
      /* fPIC + Large memory model forces everything in registers.  */
      return (arc64_cmodel_var == ARC64_CMODEL_LARGE) ? true : false;

    case ARC64_LARGE:
      return true;

    case ARC64_TLS:
    default:
      gcc_unreachable ();
    }
}

/* X and Y are two things to compare using CODE.  Emit the compare insn and
   return the rtx for the cc reg in the proper mode.  */

rtx
arc64_gen_compare_reg (enum rtx_code code, rtx x, rtx y)
{
  machine_mode mode = SELECT_CC_MODE (code, x, y);
  rtx cc_reg = gen_rtx_REG (mode, CC_REGNUM);

  if (CONSTANT_P (x) && CONSTANT_P (y))
    x = force_reg (word_mode, x);

  emit_set_insn (cc_reg, gen_rtx_COMPARE (mode, x, y));
  return cc_reg;
}

/* Prepare operands for move in MODE.  Return true iff the move has
   been emitted.  */

bool
arc64_prepare_move_operands (rtx op0, rtx op1, machine_mode mode)
{
  if (MEM_P (op0) && !REG_P (op1))
    {
      if (mode == E_DImode
	  || !satisfies_constraint_S06S0 (op1))
	op1 = force_reg (mode, op1);
    }
  else if (GET_MODE_SIZE (mode) == UNITS_PER_WORD
	   && CONSTANT_P (op1))
    {
      unsigned HOST_WIDE_INT lo;
      unsigned HOST_WIDE_INT hi;
      rtx tmp;

      switch (GET_CODE (op1))
	{
	case CONST_INT:
	  gcc_assert (mode == Pmode);
	  if (!SIGNED_INT32 (INTVAL (op1)) && !UNSIGNED_INT32 (INTVAL (op1)))
	    {
	      HOST_WIDE_INT val;
	      /* We have a large 64bit immediate:
		 movhl rA, (val64 >> 32)
		 orl   rA,rA, (val64 & 0xffffffff)
		 FIXME! add strategies to minimize the size.  */

	      val = INTVAL (op1);
	      lo = zext_hwi (val, 32);
	      hi = zext_hwi (val >> 32, 32);
	      tmp = op0;

	      if (can_create_pseudo_p ())
		tmp = gen_reg_rtx (mode);

	      /* Maybe do first a move cnst to movsi to get the
		 constants minimized.  */
	      emit_insn (gen_rtx_SET (tmp,
				      gen_rtx_ASHIFT (mode, GEN_INT (hi),
						      GEN_INT (32))));
	      emit_insn (gen_rtx_SET (op0,
				      plus_constant (mode, tmp, lo)));
	      return true;
	    }
	  break;

	case CONST_WIDE_INT:
	  gcc_unreachable ();

	case CONST_DOUBLE:
	  if (mode == SFmode)
	    return false;
	  else
	    {
	      long res[2];
	      unsigned HOST_WIDE_INT ival;
	      scalar_int_mode imode = int_mode_for_mode (mode).require ();

	      gcc_assert (mode == DFmode);

	      real_to_target (res, CONST_DOUBLE_REAL_VALUE (op1),
			      REAL_MODE_FORMAT (mode));
	      lo = zext_hwi (res[0], 32);
	      hi = zext_hwi (res[1], 32);

	      ival = lo | (hi << 32);
	      tmp = gen_reg_rtx (imode);
	      emit_move_insn (tmp, gen_int_mode (ival, imode));
	      emit_move_insn (op0, gen_lowpart (mode, tmp));
	      return true;
	    }

	case CONST:
	case SYMBOL_REF:
	case LABEL_REF:
	  op1 = arc64_legitimize_address_1 (op1, op0);
	  break;

	default:
	  break;
	}
    }

  /* Check and fix unsupported store addresses.  */
  if (MEM_P (op0)
      && !arc64_legitimate_address_1_p (mode, XEXP (op0, 0), false,
					false, true))
    {
      rtx tmp = gen_reg_rtx (Pmode);
      rtx addr = XEXP (op0, 0);
      rtx t0 = XEXP (addr, 0);
      rtx t1 = XEXP (addr, 1);

      if (GET_CODE (t0) == MULT)
	{
	  rtx ta = XEXP (t0, 0);
	  rtx tb = XEXP (t0, 1);
	  t0 = gen_rtx_ASHIFT (Pmode, ta,
			       GEN_INT (ARC64LOG2 (INTVAL (tb))));
	}

      emit_insn (gen_rtx_SET (tmp, gen_rtx_PLUS (Pmode, t0, t1)));
      op0 = replace_equiv_address (op0, tmp);
    }
  emit_insn (gen_rtx_SET (op0, op1));
  return true;
}

/* Split a mov with long immediate instruction into smaller, size
   friendly instructions.  */
#if 0
bool
arc64_split_mov_const (rtx *operands)
{
  unsigned HOST_WIDE_INT ival;
  HOST_WIDE_INT shimm;
  machine_mode mode = GET_MODE (operands[0]);

  /* Manage a constant.  */
  gcc_assert (CONST_INT_P (operands[1]));
  ival = INTVAL (operands[1]) & 0xffffffff;

  if (SIGNED_INT12 (ival))
    return false;

  /* 1. Check if we can just rotate limm by 8 but using ROR8.  */
  if (TARGET_BARREL_SHIFTER && ((ival & ~0x3f000000) == 0))
    {
      shimm = (ival >> 24) & 0x3f;
      emit_insn (gen_rtx_SET (operands[0],
			      gen_rtx_ROTATERT (mode, GEN_INT (shimm),
						GEN_INT (8))));
      return true;
    }
  /* 2. Check if we can just shift by 8 to fit into the u6 of LSL8.  */
  if (TARGET_BARREL_SHIFTER && ((ival & ~0x3f00) == 0))
    {
      shimm = (ival >> 8) & 0x3f;
      emit_insn (gen_rtx_SET (operands[0],
			      gen_rtx_ASHIFT (mode, GEN_INT (shimm),
					      GEN_INT (8))));
      return true;
    }

  /* 3. Check if we can just shift by 16 to fit into the u6 of LSL16.  */
  if (TARGET_BARREL_SHIFTER && ((ival & ~0x3f0000) == 0))
    {
      shimm = (ival >> 16) & 0x3f;
      emit_insn (gen_rtx_SET (operands[0],
			      gen_rtx_ASHIFT (mode, GEN_INT (shimm),
					      GEN_INT (16))));
      return true;
    }

  /* 4. Check if we can do something like mov_s h,u8 / asl_s ra,h,#nb.  */
  if (((ival >> (__builtin_ffs (ival) - 1)) & 0xffffff00) == 0
      && TARGET_BARREL_SHIFTER)
    {
      HOST_WIDE_INT shift = __builtin_ffs (ival);
      shimm = (ival >> (shift - 1)) & 0xff;
      emit_insn (gen_rtx_SET (operands[0], GEN_INT (shimm)));
      emit_insn (gen_rtx_SET (operands[0],
			      gen_rtx_ASHIFT (mode, operands[0],
					      GEN_INT (shift - 1))));
      return true;
    }

  /* 5. Check if we can just rotate the limm, useful when no barrel
     shifter is present.  */
  if ((ival & ~0x8000001f) == 0)
    {
      shimm = (ival * 2 + 1) & 0x3f;
      emit_insn (gen_rtx_SET (operands[0],
			      gen_rtx_ROTATERT (mode, GEN_INT (shimm),
						const1_rtx)));
      return true;
    }

  /* 6. Check if we can do something with bmask.  */
  if (IS_POWEROF2_P (ival + 1))
    {
      emit_insn (gen_rtx_SET (operands[0], constm1_rtx));
      emit_insn (gen_rtx_SET (operands[0],
			      gen_rtx_AND (mode, operands[0],
					   GEN_INT (ival))));
      return true;
    }

  return false;
}

/* Helper to check Cax constraint.  */

bool
arc64_check_mov_const (HOST_WIDE_INT ival)
{
  ival = ival & 0xffffffff;

  if ((ival & ~0x8000001f) == 0)
    return true;

  if (IS_POWEROF2_P (ival + 1))
    return true;

  /* The next rules requires a barrel shifter.  */
  if (!TARGET_BARREL_SHIFTER)
    return false;

  if (((ival >> (__builtin_ffs (ival) - 1)) & 0xffffff00) == 0)
    return true;

  if ((ival & ~0x3f00) == 0)
    return true;

  if ((ival & ~0x3f0000) == 0)
    return true;

  if ((ival & ~0x3f000000) == 0)
    return true;

  return false;
}
#endif

/* This function is used by the call expanders of the machine description.
   RESULT is the register in which the result is returned.  It's NULL for
   "call" and "sibcall".
   MEM is the location of the function call.
   SIBCALL indicates whether this function call is normal call or sibling call.
   It will generate different pattern accordingly.  */

void
arc64_expand_call (rtx result, rtx mem, bool sibcall)
{
  rtx call, callee, tmp;
  rtvec vec;
  machine_mode mode;

  gcc_assert (MEM_P (mem));
  callee = XEXP (mem, 0);
  mode = GET_MODE (callee);
  gcc_assert (mode == Pmode || CONST_INT_P (callee));

  /* Decide if we should generate indirect calls by loading the
     address of the callee into a register before performing the
     branch-and-link.  */
  if (arc64_is_long_call_p (callee) && !REG_P (callee))
    XEXP (mem, 0) = force_reg (mode, callee);

  call = gen_rtx_CALL (VOIDmode, mem, const0_rtx);

  if (result != NULL_RTX)
    call = gen_rtx_SET (result, call);

  if (sibcall)
    tmp = ret_rtx;
  else
    tmp = gen_rtx_CLOBBER (VOIDmode, gen_rtx_REG (Pmode, BLINK_REGNUM));

  vec = gen_rtvec (2, call, tmp);
  call = gen_rtx_PARALLEL (VOIDmode, vec);

  emit_call_insn (call);
}

/* Return nonzero if this function is known to have a null epilogue.
   This allows the optimizer to omit jumps to jumps if no stack
   was created.  */

bool
arc64_can_use_return_insn_p (void)
{
  return (reload_completed && cfun->machine->frame.frame_size == 0
	  && !ARC_INTERRUPT_P (cfun->machine->fn_type));
}


/* Return 1 if the register is used by the epilogue.  We need to say the
   return register is used, but only after epilogue generation is complete.
   Note that in the case of sibcalls, the values "used by the epilogue" are
   considered live at the start of the called function.  */

int
arc64_epilogue_uses (int regno)
{
#ifdef HAVE_AS_TLS
  if (regno == R30_REGNUM)
    return 1;
#endif

  if (epilogue_completed)
    {
      if (regno == BLINK_REGNUM)
	return 1;

      /* An interrupt restores more registers.  */
      if (ARC_INTERRUPT_P (cfun->machine->fn_type)
	  && (df_regs_ever_live_p (regno)
	      || (!crtl->is_leaf && call_used_or_fixed_reg_p (regno))))
	return 1;
    }

  return 0;
}

/* Return 1 if we use TP because it is alivel on entry to an exception
   edge.  */

int
arc64_eh_uses (int regno ATTRIBUTE_UNUSED)
{
#ifdef HAVE_AS_TLS
  if (regno == R30_REGNUM)
    return 1;
#endif
  return 0;
}


/* Implement INITIAL_ELIMINATION_OFFSET.  FROM is either the frame pointer
   or argument pointer.  TO is either the stack pointer or hard frame
   pointer.  */

HOST_WIDE_INT
arc64_initial_elimination_offset (unsigned from, unsigned to)
{
  struct arc64_frame *frame = &cfun->machine->frame;

  if (from == ARG_POINTER_REGNUM && to == HARD_FRAME_POINTER_REGNUM)
    return frame->saved_regs_size;

  if (from == ARG_POINTER_REGNUM && to == STACK_POINTER_REGNUM)
    return (frame->saved_regs_size + frame->saved_locals_size
	    + frame->saved_outargs_size);

  if ((from == FRAME_POINTER_REGNUM) && (to == STACK_POINTER_REGNUM))
    return (frame->saved_locals_size + frame->saved_outargs_size);

  if ((from == FRAME_POINTER_REGNUM) && (to == HARD_FRAME_POINTER_REGNUM))
    return 0;

  gcc_unreachable ();
}

/* Helper for INIT_EXPANDERS macro called to initialize any target
   specific information.  */

void arc64_init_expanders (void)
{
  init_machine_status = arc64_init_machine_status;
}

/* Given a comparison code (EQ, NE, etc.) and the first operand of a
   COMPARE, return the mode to be used for the comparison.  */

machine_mode
arc64_select_cc_mode (enum rtx_code op,
		      rtx x,
		      rtx y)
{
  machine_mode mode = GET_MODE (x);

  /* Matches all instructions which can do .f and clobbers only Z flag.  */
  if (GET_MODE_CLASS (mode) == MODE_INT
      && y == const0_rtx
      && GET_CODE (x) == MULT
      && (op == EQ || op == NE))
    return CC_Zmode;

  /* Matches all instructions which can do .f and clobbers Z and N
     flags.  Because we compare with zero, for LT we can use "mi" and
     for GT we can use "pl".  We cannot use GT with "pnz" because it
     cannot be reversed.  */
  if (GET_MODE_CLASS (mode) == MODE_INT
      && y == const0_rtx
      && (op == EQ || op == NE || op == LT || op == GE))
    return CC_ZNmode;

  /* All floating point compares return CC_FPU if it is an equality
     comparison, and CC_FPUE otherwise.  N.B. LTGT and UNEQ cannot be
     directly mapped to fcmp instructions.  */
  if (GET_MODE_CLASS (mode) == MODE_FLOAT)
    {
      switch (op)
	{
	case EQ:
	case NE:
	case UNORDERED:
	case ORDERED:
	case UNLT:
	case UNLE:
	case UNGT:
	case UNGE:
	case UNEQ:
	  return CC_FPUmode;

	case LT:
	case LE:
	case GT:
	case GE:
	case LTGT:
	  return CC_FPUEmode;

	default:
	  gcc_unreachable ();
	}
    }
  return CCmode;
}

/* Implement RETURN_ADDR_RTX.  We do not support moving back to a
   previous frame.  */

rtx
arc64_return_addr (int count , rtx frame ATTRIBUTE_UNUSED)
{
  if (count != 0)
    return const0_rtx;
  return get_hard_reg_initial_val (Pmode, BLINK_REGNUM);
}

/* Expand the "prologue" pattern.  */

void
arc64_expand_prologue (void)
{
  HOST_WIDE_INT frame_allocated;
  struct arc64_frame *frame = &cfun->machine->frame;

  if (flag_stack_usage_info)
    current_function_static_stack_size = frame->frame_size;

  if (ARC_NAKED_P (cfun->machine->fn_type))
    return;

  frame_allocated = frame->frame_size;

  frame_allocated -= arc64_save_callee_saves ();

  /* If something left, allocate.  */
  if (frame_allocated > 0)
    frame_stack_add ((HOST_WIDE_INT) 0 - frame_allocated);

  /* Emit a blockage.  */
  emit_insn (gen_blockage ());
}

/* Expand "epilogue" pattern.  */

void
arc64_expand_epilogue (bool sibcall_p)
{
  HOST_WIDE_INT frame_deallocated;
  struct arc64_frame *frame = &cfun->machine->frame;

  if (ARC_NAKED_P (cfun->machine->fn_type))
    {
      emit_jump_insn (gen_return ());
      return;
    }

  frame_deallocated = frame->frame_size;
  frame_deallocated -= arc64_restore_callee_saves (sibcall_p);

  if (frame_deallocated != 0)
    frame_stack_add (frame_deallocated);

  /* For frames that use __builtin_eh_return, the register defined by
     EH_RETURN_STACKADJ_RTX is set to 0 for all standard return paths.
     On eh_return paths however, the register is set to the value that
     should be added to the stack pointer in order to restore the
     correct stack pointer for the exception handling frame.

     For ARC64 we are going to use r4 for EH_RETURN_STACKADJ_RTX, add
     this onto the stack for eh_return frames.  */
  if (crtl->calls_eh_return)
    emit_insn (gen_add2_insn (stack_pointer_rtx,
			      EH_RETURN_STACKADJ_RTX));

  if (ARC_INTERRUPT_P (cfun->machine->fn_type))
    emit_jump_insn (gen_rtie ());
  else if (!sibcall_p)
    emit_jump_insn (gen_simple_return ());
}

/* Helper used to determine if an address requires a long immediate.
   To be used in computing the length of an load/store
   instruction.  */

bool
arc64_limm_addr_p (rtx op)
{
  rtx addr = XEXP (op, 0);

  if (!MEM_P (op))
    return false;

  switch (GET_CODE (addr))
    {
    case SYMBOL_REF:
    case LABEL_REF:
    case CONST_INT:
    case CONST:
    case UNSPEC:
    case LO_SUM:
      return true;

    case PRE_INC:
    case PRE_DEC:
    case POST_INC:
    case POST_DEC:
    case PRE_MODIFY:
    case POST_MODIFY:
    case PLUS:
      /* legitimate address doesn't recognize [b,limm] variant of st.
	 Hence, use it to determine if we have limm or not in
	 address.  */
      return !arc64_legitimate_address_1_p (GET_MODE (op), addr,
					    false, false, true);
    default:
      break;
    }
  return false;
}

/* Used by move_dest_operand predicate.  */

bool
arc64_legitimate_store_address_p (machine_mode mode, rtx addr)
{
  return arc64_legitimate_address_1_p (mode, addr, true, false, true);
}

/* Return true if an address fits a short load/store instruction.  */

bool
arc64_short_access_p (rtx op, machine_mode mode, bool load_p)
{
  rtx addr, plus0, plus1;
  bool f0, f1;

  /* Eliminate non-memory operations.  */
  if (GET_CODE (op) != MEM)
    return 0;

  /* FIXME! remove it when "uncached" attribute is added.  */
  if (MEM_VOLATILE_P (op) && TARGET_VOLATILE_DI)
    return false;

  if (mode == VOIDmode)
    mode = GET_MODE (op);

  /* Decode the address now.  */
  addr = XEXP (op, 0);
  switch (GET_CODE (addr))
    {
    case REG:
      return check_short_insn_register_p (addr, false);

    case PLUS:
      plus0 = XEXP (addr, 0);
      plus1 = XEXP (addr, 1);

      f0 = check_short_insn_register_p (plus0, false);
      f1 = check_short_insn_constant_p (plus1, mode);

      /* Check for [Rb + shimm].  */
      if (f0 && f1)
	return true;

      if (!load_p)
	return false;

      /* Check for [Rb + Ri].  */
      f1 = check_short_insn_register_p (plus1, false);

      if (f0 && f1)
	return true;

    default:
      break;
    }
  return false;
}

/* Return true if an address fits a floating point load/store
   instruction.  The next formats are allowed [b, s9], [b], [s32limm],
   and scaled [b, s9].  */

bool
arc64_fp_access_p (rtx op, machine_mode mode)
{
  rtx addr;

  /* Eliminate non-memory operations.  */
  if (GET_CODE (op) != MEM)
    return 0;

  /* FIXME! remove it when "uncached" attribute is added.  */
  if (MEM_VOLATILE_P (op) && TARGET_VOLATILE_DI)
    return false;

  if (mode == VOIDmode)
    mode = GET_MODE (op);

  /* Decode the address now.  */
  addr = XEXP (op, 0);

  return arc64_legitimate_address_1_p (mode, addr, true, false, false);
}

/* Implement EH_RETURN_HANDLER_RTX.  EH returns need to either return
   normally or return to a previous frame after unwinding.

   An EH return uses a single shared return sequence.  The epilogue is
   exactly like a normal epilogue except that it has an extra input
   register (EH_RETURN_STACKADJ_RTX) which contains the stack
   adjustment that must be applied after the frame has been destroyed.
   An extra label is inserted before the epilogue which initializes
   this register to zero, and this is the entry point for a normal
   return.

   An actual EH return updates the return address, initializes the
   stack adjustment and jumps directly into the epilogue (bypassing
   the zeroing of the adjustment).  Since the return address is
   typically saved on the stack when a function makes a call, the
   saved BLINK must be updated outside the epilogue.

   This poses problems as the store is generated well before the
   epilogue, so the offset of BLINK is not known yet.  Also
   optimizations will remove the store as it appears dead, even after
   the epilogue is generated (as the base or offset for loading BLINK
   is different in many cases).

   To avoid these problems this implementation forces the frame
   pointer in eh_return functions so that the location of BLINK is
   fixed and known early.  It also marks the store volatile, so no
   optimization is permitted to remove the store.  */

rtx
arc64_eh_return_handler_rtx (void)
{
  rtx tmp = gen_frame_mem (Pmode,
    plus_constant (Pmode, hard_frame_pointer_rtx, UNITS_PER_WORD));

  /* Mark the store volatile, so no optimization is permitted to remove it.  */
  MEM_VOLATILE_P (tmp) = true;
  return tmp;
}

/* Select a format to encode pointers in exception handling data.  */

int
arc64_asm_preferred_eh_data_format (int code ATTRIBUTE_UNUSED, int global)
{
   int type;

   if (!flag_pic)
     return DW_EH_PE_absptr;

   switch (arc64_cmodel_var)
     {
    case ARC64_CMODEL_SMALL:
    case ARC64_CMODEL_MEDIUM:
       /* text+got+data < 4Gb.  4-byte signed relocs are sufficient
	  for everything.  */
       type = DW_EH_PE_sdata4;
       break;
     default:
       /* No assumptions here.  8-byte relocs required.  */
       type = DW_EH_PE_sdata8;
       break;
     }
   return (global ? DW_EH_PE_indirect : 0) | DW_EH_PE_pcrel | type;
}

/* Emit a (pre) memory barrier around an atomic sequence according to
   MODEL.  */

void
arc64_pre_atomic_barrier (enum memmodel model)
{
  if (need_atomic_barrier_p (model, true))
    emit_insn (gen_memory_barrier ());
}

/* Emit a (post) memory barrier around an atomic sequence according to
   MODEL.  */

void
arc64_post_atomic_barrier (enum memmodel model)
{
  if (need_atomic_barrier_p (model, false))
    emit_insn (gen_memory_barrier ());
}

/* Expand an atomic fetch-and-operate pattern.  CODE is the binary operation
   to perform.  MEM is the memory on which to operate.  VAL is the second
   operand of the binary operator.  BEFORE and AFTER are optional locations to
   return the value of MEM either before of after the operation.  MODEL_RTX
   is a CONST_INT containing the memory model to use.  */

void
arc64_expand_atomic_op (enum rtx_code code, rtx mem, rtx val,
			 rtx orig_before, rtx orig_after, rtx model_rtx)
{
  enum memmodel model = (enum memmodel) INTVAL (model_rtx);
  machine_mode mode = GET_MODE (mem);
  rtx label, x, cond;
  rtx before = orig_before, after = orig_after;

  /* ARC atomic ops work only with 32-bit aligned memories.  */
  gcc_assert (mode == SImode || mode == DImode);

  arc64_pre_atomic_barrier (model);

  label = gen_label_rtx ();
  emit_label (label);
  label = gen_rtx_LABEL_REF (VOIDmode, label);

  if (before == NULL_RTX)
    before = gen_reg_rtx (mode);

  if (after == NULL_RTX)
    after = gen_reg_rtx (mode);

  /* Load exclusive.  */
  if(mode == SImode)
    emit_insn (gen_arc_load_exclusivesi (before, mem));
  else /* DImode */
    emit_insn (gen_arc_load_exclusivedi (before, mem));

  switch (code)
    {
    case NOT:
      x = gen_rtx_AND (mode, before, val);
      emit_insn (gen_rtx_SET (after, x));
      x = gen_rtx_NOT (mode, after);
      emit_insn (gen_rtx_SET (after, x));
      break;

    case MINUS:
      if (CONST_INT_P (val))
	{
	  val = GEN_INT (-INTVAL (val));
	  code = PLUS;
	}

      /* FALLTHRU.  */
    default:
      x = gen_rtx_fmt_ee (code, mode, before, val);
      emit_insn (gen_rtx_SET (after, x));
      break;
   }

  /* Exclusively store new item.  Store clobbers CC reg.  */
  if(mode == SImode)
    emit_insn (gen_arc_store_exclusivesi (mem, after));
  else /* DImode */
    emit_insn (gen_arc_store_exclusivedi (mem, after));

  /* Check the result of the store.  */
  cond = gen_rtx_REG (CC_Zmode, CC_REGNUM);
  x = gen_rtx_NE (VOIDmode, cond, const0_rtx);
  x = gen_rtx_IF_THEN_ELSE (VOIDmode, x,
			    label, pc_rtx);
  emit_unlikely_jump (gen_rtx_SET (pc_rtx, x));

  arc64_post_atomic_barrier (model);
}

/* Helper function used by "atomic_compare_and_swap" expand
   pattern.  */

void
arc64_expand_compare_and_swap (rtx operands[])
{
  rtx bval, rval, mem, oldval, newval, is_weak, mod_s, mod_f, x;
  machine_mode mode;

  bval = operands[0];
  rval = operands[1];
  mem = operands[2];
  oldval = operands[3];
  newval = operands[4];
  is_weak = operands[5];
  mod_s = operands[6];
  mod_f = operands[7];
  mode = GET_MODE (mem);

  if (reg_overlap_mentioned_p (rval, oldval))
    oldval = copy_to_reg (oldval);

  if (mode == SImode || mode == DImode)
    {
      if (mode == SImode)
	emit_insn (gen_atomic_compare_and_swapsi_1 (rval, mem, oldval, newval,
						    is_weak, mod_s, mod_f));
      else /* DImode */
	emit_insn (gen_atomic_compare_and_swapdi_1 (rval, mem, oldval, newval,
						    is_weak, mod_s, mod_f));

      x = gen_rtx_REG (CC_Zmode, CC_REGNUM);
      x = gen_rtx_EQ (SImode, x, const0_rtx);
      emit_insn (gen_rtx_SET (bval, x));
    }
  else
    {
      arc_expand_compare_and_swap_qh (bval, rval, mem, oldval, newval,
				      is_weak, mod_s, mod_f);
    }
}

/* Helper function used by the "atomic_compare_and_swapsdi_1"
   pattern.  */

void
arc64_split_compare_and_swap (rtx operands[])
{
  rtx rval, mem, oldval, newval;
  machine_mode mode, mode_cc;
  enum memmodel mod_s, mod_f;
  bool is_weak;
  rtx label1, label2, x, cond;

  rval = operands[0];
  mem = operands[1];
  oldval = operands[2];
  newval = operands[3];
  is_weak = (operands[4] != const0_rtx);
  mod_s = (enum memmodel) INTVAL (operands[5]);
  mod_f = (enum memmodel) INTVAL (operands[6]);
  mode = GET_MODE (mem);

  /* ARC atomic ops work only with 32-bit or 64-bit aligned memories.  */
  gcc_assert (mode == SImode || mode == DImode);

  arc64_pre_atomic_barrier (mod_s);

  label1 = NULL_RTX;
  if (!is_weak)
    {
      label1 = gen_label_rtx ();
      emit_label (label1);
    }
  label2 = gen_label_rtx ();

  /* Load exclusive.  */
  if(mode == SImode)
    emit_insn (gen_arc_load_exclusivesi (rval, mem));
  else /* DImode */
    emit_insn (gen_arc_load_exclusivedi (rval, mem));

  /* Check if it is oldval.  */
  mode_cc = SELECT_CC_MODE (NE, rval, oldval);
  cond = gen_rtx_REG (mode_cc, CC_REGNUM);
  emit_insn (gen_rtx_SET (cond, gen_rtx_COMPARE (mode_cc, rval, oldval)));

  x = gen_rtx_NE (VOIDmode, cond, const0_rtx);
  x = gen_rtx_IF_THEN_ELSE (VOIDmode, x,
			    gen_rtx_LABEL_REF (Pmode, label2), pc_rtx);
  emit_unlikely_jump (gen_rtx_SET (pc_rtx, x));

  /* Exclusively store new item.  Store clobbers CC reg.  */
  if(mode == SImode)
    emit_insn (gen_arc_store_exclusivesi (mem, newval));
  else /* DImode */
    emit_insn (gen_arc_store_exclusivedi (mem, newval));

  if (!is_weak)
    {
      /* Check the result of the store.  */
      cond = gen_rtx_REG (CC_Zmode, CC_REGNUM);
      x = gen_rtx_NE (VOIDmode, cond, const0_rtx);
      x = gen_rtx_IF_THEN_ELSE (VOIDmode, x,
				gen_rtx_LABEL_REF (Pmode, label1), pc_rtx);
      emit_unlikely_jump (gen_rtx_SET (pc_rtx, x));
    }

  if (mod_f != MEMMODEL_RELAXED)
    emit_label (label2);

  arc64_post_atomic_barrier (mod_s);

  if (mod_f == MEMMODEL_RELAXED)
    emit_label (label2);
}

/* Expander for casesi.  The vector table is always PC-relative, and
   it is made up of branch instructions.  When we have CODE_DENSITY
   option enabled, we use BI instruction, otherwise, depending on the
   memory model, an emulation of it.  We use the same emulation
   contruction, for PIC or LARGE memory model.  For a non-pic
   SMALL/MEDIUM memory model, we make use of a single add2 instruction
   which has one input the address of the start dispatch table, and
   the other input indicates where we jump in the table.  */

void arc64_expand_casesi (rtx operands[])
{
  rtx reg;

  if (operands[1] != const0_rtx)
    {
      reg = gen_reg_rtx (SImode);
      operands[1] = GEN_INT (trunc_int_for_mode (-INTVAL (operands[1]),
						 SImode));
      emit_insn (gen_addsi3 (reg, operands[0], operands[1]));
      operands[0] = reg;
    }
  emit_unlikely_jump (gen_cbranchsi4 (gen_rtx_GTU (SImode, operands[0],
						   operands[2]),
				      operands[0], operands[2], operands[4]));

  if (!TARGET_CODE_DENSITY)
    {
      switch (arc64_cmodel_var)
	{
	case ARC64_CMODEL_SMALL:
	  if (!flag_pic)
	    {
	      reg = gen_reg_rtx (SImode);
	      emit_insn (gen_casesi_addaddr (reg, operands[0], operands[3]));
	      operands[0] = reg;
	      break;
	    }
	  /* Fall through */
	case ARC64_CMODEL_MEDIUM:
	case ARC64_CMODEL_LARGE:
	  {
	    gcc_assert (word_mode == DImode);
	    /* Same code is used for PIC and large memory model.  */
	    rtx lbl = gen_rtx_LABEL_REF (VOIDmode, operands[3]);
	    rtx tmp = gen_reg_rtx (DImode);
	    reg = gen_reg_rtx (DImode);
	    emit_insn (gen_rtx_SET (reg,
				    gen_rtx_UNSPEC (DImode,
						    gen_rtvec (1, lbl),
						    ARC64_UNSPEC_PCREL)));
	    emit_insn (gen_casesi_addaddrdi (tmp, operands[0], reg));
	    emit_jump_insn (gen_casesi_dispatchdi (tmp, operands[3]));
	    return;
	  }
	default:
	  gcc_unreachable ();
	}
    }

  emit_jump_insn (gen_casesi_dispatch (operands[0], operands[3]));
}

bool
arc64_allow_direct_access_p (rtx op)
{
  return (arc64_get_symbol_type (op) == ARC64_LO32);
}

/* Decide if mov simd instruction needs to be split.  Return TRUE if
   so.  This procedure is required when the vector length is larger
   than 64 bit.  */
bool
arc64_split_double_move_p (rtx *operands, machine_mode mode)
{
  rtx op0 = operands[0];
  rtx op1 = operands[1];

  /* Split only double moves.  */
  if (GET_MODE_SIZE (mode) < (UNITS_PER_WORD * 2))
    return false;

  if (register_operand (op0, mode) && register_operand (op1, mode))
    {
      /* Check if we can use vadd2 instruction as a mov.  */
      if (TARGET_SIMD
	  && !FLOAT_MODE_P (mode)
	  && !TARGET_64BIT
	  && (GET_MODE_SIZE (mode) == (2 * UNITS_PER_WORD)))
	{
	  /* If both registers are even-numbered, fallback to vadd2.  */
	  if (((REGNO (op0) & 0x01) == 0) && ((REGNO (op1) & 0x01) == 0))
	    return false;
	  else
	    return true;
	}

      /* Check for r-reg to f-reg moves.  */
      if (GP_REGNUM_P (REGNO (op0)) || GP_REGNUM_P (REGNO (op1)))
	return true;

      /* Sanity check for vfmov instruction.  */
      gcc_assert (arc64_fsimd_register (op0, mode)
		  && arc64_fsimd_register (op1, mode));
      return false;
    }

  /* Check if we have 64/128bit moves.  */
  if (DOUBLE_LOAD_STORE
      && ((memory_operand (op0, mode) && REG_P (op1))
	  || (memory_operand (op1, mode) && REG_P (op0))))
    {
      gcc_assert (GET_MODE_SIZE (mode) == (UNITS_PER_WORD * 2));
      /* Sanity check for wide st/ld instructions.  */
      if (REG_P (op0) && ((REGNO (op0) & 0x01) != 0))
	return true;
      if (REG_P (op1) && ((REGNO (op1) & 0x01) != 0))
	return true;
      return false;
    }

  /* Evereything else is going for a split.  */
  return true;
}

/* This is the actual routine which splits a move simd to smaller
   bits.  */
void
arc64_split_double_move (rtx *operands, machine_mode mode)
{
  rtx op0 = operands[0];
  rtx op1 = operands[1];
  rtx lo, hi, mem_lo, mem_hi, src, dst;
  unsigned int rdst, rsrc, i;
  unsigned iregs = CEIL (GET_MODE_SIZE (mode), UNITS_PER_WORD);
  bool swap_p = false;
  machine_mode mvmode = smallest_int_mode_for_size (BITS_PER_WORD);

  /* Maximum size handled is twice UNITS_PER_WORD.  */
  gcc_assert (iregs <= 2);

  /* This procedure works as long as the width of the fp regs is the
     same as the width of r regs.  */
  if (FLOAT_MODE_P (mode))
    {
      gcc_assert (UNITS_PER_WORD == UNITS_PER_FP_REG);
      mvmode = float_mode_for_size (BITS_PER_WORD).require ();
    }

  /* Split reg-reg move.  */
  if (REG_P (op0) && REG_P (op1))
    {
      rdst = REGNO (op0);
      rsrc = REGNO (op1);

      if (!reg_overlap_mentioned_p (op0, op1)
	  || rdst < rsrc)
	/* The fp regs will never overlap r-regs.  However, this
	   procedure can be used also for r-reg to r-regs splits.  */
	for (i = 0; i < iregs; i++)
	  emit_move_insn (gen_rtx_REG (mvmode, rdst + i),
			  gen_rtx_REG (mvmode, rsrc + i));
      else
	for (i = 0; i < iregs; i++)
	  emit_move_insn (gen_rtx_REG (mvmode, rdst + iregs - i - 1),
			  gen_rtx_REG (mvmode, rsrc + iregs - i - 1));
      return;
    }

  /* Split mem-reg moves.  */
  gcc_assert (REG_P (op0) || REG_P (op1));

  if (REG_P (op1))
    {
      src = op1;
      dst = op0;
    }
  else
    {
      src = op0;
      dst = op1;
    }

  lo = gen_lowpart (mvmode, src);
  hi = gen_highpart_mode (mvmode, mode, src);

  if (auto_inc_p (XEXP (dst, 0)))
    {
      rtx offset, reg, next, addr = XEXP (dst, 0);
      enum rtx_code code = GET_CODE (addr);

      switch (code)
	{
	case PRE_INC:
	  offset = GEN_INT (GET_MODE_SIZE (mode));
	  code = PRE_MODIFY;
	  break;
	case PRE_DEC:
	  offset = GEN_INT (-GET_MODE_SIZE (mode));
	  code = PRE_MODIFY;
	  break;
	case POST_MODIFY:
	case PRE_MODIFY:
	  offset =  XEXP (XEXP (addr, 1), 1);
	  break;
	case POST_INC:
	  offset = GEN_INT (GET_MODE_SIZE (mode));
	  code = POST_MODIFY;
	  break;
	case POST_DEC:
	  offset = GEN_INT (-GET_MODE_SIZE (mode));
	  code = POST_MODIFY;
	  break;
	default:
	  gcc_unreachable ();
	}

      reg = XEXP (addr, 0);
      next = gen_rtx_fmt_ee (code, Pmode, reg,
			     gen_rtx_PLUS (Pmode, reg, offset));

      switch (code)
	{
	case POST_MODIFY:
	  /* We need to swap lo/hi order such that we emit first the
	     hi-load with an offset, and last the post modify
	     instruction.  Thus the code can handle any type of auto
	     increment address.  */
	  mem_lo = adjust_automodify_address (dst, mvmode, next, 0);
	  next = plus_constant (Pmode, reg, GET_MODE_SIZE (mvmode));
	  mem_hi = adjust_automodify_address (dst, mvmode, next,
					      GET_MODE_SIZE (mvmode));
	  swap_p = true;
	  break;
	case PRE_MODIFY:
	  mem_lo = adjust_automodify_address (dst, mvmode, next, 0);
	  next = plus_constant (Pmode, reg, GET_MODE_SIZE (mvmode));
	  mem_hi = adjust_automodify_address (dst, mvmode, next,
					      GET_MODE_SIZE (mvmode));
	  break;
	default:
	  gcc_unreachable ();
	}
    }
  else if (GET_CODE (XEXP (dst, 0)) == UNSPEC)
    {
      /* For rare situations when we need to split a PIC address.  */
      rtx addr = XEXP (dst, 0);
      switch (XINT (addr, 1))
	{
	case ARC64_UNSPEC_PCREL:
	  addr = XVECEXP (addr, 0, 0);
	  addr = plus_constant (Pmode, addr, GET_MODE_SIZE (mvmode));
	  addr = gen_sym_unspec (addr, ARC64_UNSPEC_PCREL);
	  break;

	default:
	  /* Fail for anything else.  */
	  gcc_unreachable ();
	}

      mem_lo = adjust_address (dst, mvmode, 0);
      mem_hi = adjust_automodify_address (mem_lo, GET_MODE (mem_lo),
					  addr, GET_MODE_SIZE (mvmode));
    }
  else
    {
      mem_lo = adjust_address (dst, mvmode, 0);
      mem_hi = arc64_move_pointer (mem_lo, GET_MODE_SIZE (mvmode));
      /* Catching scenarios like:
	 ld r0, [r0, 4]	  (ld lo, [mem_lo])
	 ld r1, [r0, 8]	  (ld hi, [mem_hi])

	 And setting the trigger (swap_p) to convert them to:
	 ld r1, [r0, 8]
	 ld r0, [r0, 4]  */
      if (reg_overlap_mentioned_p (lo, mem_lo))
	swap_p = true;
    }

  if (REG_P (op1))
    {
      if (!swap_p)
	emit_move_insn (mem_lo, lo);
      emit_move_insn (mem_hi, hi);
      if (swap_p)
	emit_move_insn (mem_lo, lo);
    }
  else
    {
      if (!swap_p)
	emit_move_insn (lo, mem_lo);
      emit_move_insn (hi, mem_hi);
      if (swap_p)
	emit_move_insn (lo, mem_lo);
    }
}

/* What mode to use when copying N-bits of data.

   HS5x
     n >= 64: copy_mode()
     n >= 32: SFmode	      if FP_MOVE
	      SImode	      otherwise
     n >= 16: HFmode	      if FP_MOVE
	      HImode	      otherwise
     n >=  8: QImode

   HS6x
     n >= 128: copy_mode()
     n >=  64: DFmode	      if FP_MOVE
i	       DImode	      otherwise
     n >=  32: SFmode	      if FP_MOVE
i	       SImode	      otherwise
     n >=  16: HFmode	      if FP_MOVE
i	       HImode	      otherwise
     n >=   8: QImode

  Note about the "return ((machine_mode) (FP ? Fmode : Imode))":
  GCC 8.3 gives a warning about "int to machine_mode" conversion if we
  don't use the explicit "((machine_mode) ...)" casting, while it is
  absolutely OK with "retun [F|I]mode;" separately.
*/

static machine_mode
cpymem_copy_mode_for_n (int n)
{
  /* HS6x.  */
  if (TARGET_64BIT)
    {
      if (n >= 128)
	return cpymem_copy_mode ();
      else if (n >= 64)
	return ((machine_mode) (TARGET_FP_MOVE ? DFmode : DImode));
      /* fall-thru.  */
    }
  /* HS5x.  */
  else
    {
      if (n >= 64)
	return cpymem_copy_mode ();
      /* fall-thru.  */
    }

  if (n >= 32)
    return ((machine_mode) (TARGET_FP_MOVE ? SFmode : SImode));
  else if (n >= 16)
    return ((machine_mode) (TARGET_FP_MOVE ? HFmode : HImode));
  else
    return QImode;
}

/* Returns the bit size (of a mode) that is big enough to
   handle the remaining N-bits of data.

   This function is not expected to be called for Ns that
   are too big for the architecture to swallow.  e.g. for
   an HS5x target without 64-bit load/store support, any
   N > 32 is not expected.  */

static int
cpymem_smallest_bigger_mode_bitsize (int n)
{
  if (n <= 8)
    return 8;		      /* QImode.  */
  else if (n <= 16)
    return 16;		      /* H{I|F}mode.  */
  else if (n <= 32)
    return 32;		      /* S{I|F}mode.  */
  else if (n <= 64)
    {
      /* a 64-bit arch or a 32-bit arch with double load/stores.  */
      if (TARGET_64BIT || TARGET_LL64)
	return 64;	      /* {DI|DF|V2SF}mode.  */

      /* This functions mustn't have been called.  */
      gcc_unreachable ();
    }
  else if (n <= 128)
    {
      if (TARGET_64BIT && TARGET_WIDE_LDST)
	return 128;	      /* {TI|V2DF}mode.  */
      /* Fall-thru.  */
    }

  gcc_unreachable ();
}

/* Expand cpymem, as if from a __builtin_memcpy.  Return true if
   we succeed, otherwise return false.  */

bool
arc64_expand_cpymem (rtx *operands)
{
  int n, mode_bits;
  rtx dst = operands[0];
  rtx src = operands[1];
  rtx base;
  machine_mode cur_mode;
  bool speed_p = !optimize_function_for_size_p (cfun);

  /* When optimizing for size, give a better estimate of the length of a
     memcpy call, but use the default otherwise.  Moves larger than 8 bytes
     will always require an even number of instructions to do now.  And each
     operation requires both a load+store, so devide the max number by 2.  */
  int max_num_moves = (speed_p ? 16 : ARC64_CALL_RATIO) / 2;
  /* In case of double moves, double the threshold.  */
  if (DOUBLE_LOAD_STORE)
    max_num_moves *= 2;

  /* We can't do anything smart if the amount to copy is not constant.  */
  if (!CONST_INT_P (operands[2]))
    return false;

  n = INTVAL (operands[2]);

  /* Try to keep the number of instructions low.  For all cases we will do at
     most two moves for the residual amount, since we'll always overlap the
     remainder.  */
  const int divisor = GET_MODE_SIZE (cpymem_copy_mode ());
  if (((n / divisor) + (n % divisor ? 2 : 0)) > max_num_moves)
    return false;

  base = copy_to_mode_reg (Pmode, XEXP (dst, 0));
  dst = adjust_automodify_address (dst, VOIDmode, base, 0);

  base = copy_to_mode_reg (Pmode, XEXP (src, 0));
  src = adjust_automodify_address (src, VOIDmode, base, 0);

  /* Convert n to bits to make the rest of the code simpler.  */
  n = n * BITS_PER_UNIT;

  while (n > 0)
    {
      cur_mode = cpymem_copy_mode_for_n (n);

      mode_bits = GET_MODE_BITSIZE (cur_mode);
      arc64_copy_one_block_and_progress_pointers (&src, &dst, cur_mode);

      n -= mode_bits;

      /* Do certain trailing copies as overlapping if it's going to be
	 cheaper.  i.e. less instructions to do so.  For instance doing a 15
	 byte copy it's more efficient to do two overlapping 8 byte copies than
	 8 + 4 + 2 + 1.  */
      if (n > 0 && n < (BITS_PER_UNIT * divisor))
	{
	  int n_bits = cpymem_smallest_bigger_mode_bitsize (n);
	  src = arc64_move_pointer (src, (n - n_bits) / BITS_PER_UNIT);
	  dst = arc64_move_pointer (dst, (n - n_bits) / BITS_PER_UNIT);
	  n = n_bits;
	}
    }

  return true;
}

/* Provide a mapping from gcc register numbers to dwarf register numbers.  */
unsigned
arc64_dbx_register_number (unsigned regno)
{
  if (GP_REGNUM_P (regno))
    return regno;
  else if (FP_REGNUM_P (regno))
    return 128 + regno - F0_REGNUM;

  /* Return values >= DWARF_FRAME_REGISTERS indicate that there is no
     equivalent DWARF register.  */
   return DWARF_FRAME_REGISTERS;
}

#if 0
/* Expand fp vector shift right pattern.  Can handle maximum 128bit
   SIMD vectors.

   +----+----+----+----+----+----+----+----+
   | h7 | h6 | h5 | h4 | h3 | h2 | h1 | h0 |
   |    s3   |    s2   |    s1   |    s0   |
   |         d1        |         d0        |
   +----+----+----+----+----+----+----+----+

 */

bool
arc64_expand_fvect_shr (rtx *operands)
{
  rtx op0 = operands[0];
  rtx op1 = operands[1];
  rtx op2 = operands[2];
  rtx t0;
  machine_mode mode = GET_MODE (op0);
  scalar_int_mode imode = int_mode_for_mode (mode).require ();
  unsigned int ival = INTVAL (op2);

  if (ARC64_VFP_128 && (ival == 64))
    {
      emit_move_insn (gen_lowpart (DFmode, op0), gen_highpart (DFmode, op1));
      return true;
    }
  else if (ARC64_VFP_64 && (ival == 32))
    {
      t0 = gen_reg_rtx (SFmode);

      emit_insn (gen_vec_extractv2sfsf (t0,
				      gen_lowpart (V2SFmode, op1),
				      GEN_INT (1)));
      emit_insn (gen_vec_setv2sf (gen_lowpart (V2SFmode, op0),
				  t0, GEN_INT (0)));
      return true;
    }
  else if (ARC64_VFP_32 && (ival == 16))
    {
      t0 = gen_reg_rtx (HFmode);

      emit_insn (gen_vec_extractv2hfhf (t0, op1, GEN_INT (1)));
      emit_insn (gen_vec_setv2hf (op0, t0, GEN_INT (0)));
      return true;
    }

  t0 = gen_reg_rtx (imode);
  rtx shift = expand_binop (imode, lshr_optab,
			    gen_lowpart (imode, op1), op2,
			    NULL_RTX, true, OPTAB_DIRECT);
  emit_move_insn (t0, shift);
  emit_move_insn (op0, gen_lowpart (mode, t0));
  return true;
}
#endif

/* Return TRUE if SYM requires a PLT34 reloc.  The instruction is
   valid, hence any symbol which its type is LPIC is valid for
   instruction, see arc64_is_long_call_p.  */

bool
arc64_use_plt34_p (rtx sym)
{
  return (arc64_get_symbol_type (sym) == ARC64_LPIC);
}

/* Determine if it's legal to put X into the constant pool.  By all means, it is
   not ok to put a symbol in a constant pool.  We arive here in the case of a
   TLS symbol which needs to be precomputed.  We force this in
   legitimize_constant_p.  */

static bool
arc64_cannot_force_const_mem (machine_mode mode ATTRIBUTE_UNUSED,
			      rtx x)
{
  return contains_symbol_ref_p (x) || tls_referenced_p (x);
}

/* Generate RTL for conditional branch with rtx comparison CODE in mode
   CC_MODE.  */

void
arc64_gen_unlikely_cbranch (enum rtx_code code, machine_mode cc_mode,
			    rtx label_ref)
{
  rtx x;
  x = gen_rtx_fmt_ee (code, VOIDmode, gen_rtx_REG (cc_mode, CC_REGNUM),
		      const0_rtx);
  x = gen_rtx_IF_THEN_ELSE (VOIDmode, x,
			    gen_rtx_LABEL_REF (VOIDmode, label_ref),
			    pc_rtx);
  emit_unlikely_jump (gen_rtx_SET (pc_rtx, x));
}

/* True if the dependency between OUT_INSN and IN_INSN is on the accumulator
   register.  IN_INSN is a mac type of instruction.  */

int
accumulator_bypass_p (rtx_insn *out_insn, rtx_insn *in_insn)
{
  rtx in_set = single_set (in_insn);
  rtx out_set = single_set (out_insn);

  if (!in_set || !out_set)
    return false;

  if (!REG_P (SET_DEST (out_set)) || (REGNO (SET_DEST (out_set)) != R58_REGNUM))
    return false;

  rtx tmp = SET_SRC (in_set);
  if (GET_CODE (tmp) == PLUS && GET_CODE (XEXP (tmp, 0)) == MULT)
    return true;
  return true;
}

/* True if IN_INSN is setting the accumulator.  */

int
set_accumulator_p (rtx_insn *out_insn ATTRIBUTE_UNUSED,
		   rtx_insn *in_insn)
{
  rtx in_set = single_set (in_insn);
  if (!in_set)
    return false;

  if (REG_P (SET_DEST (in_set)) && (REGNO (SET_DEST (in_set)) == R58_REGNUM))
    return true;
  return false;
}

/* Return 'return' instruction.  */

const char *
arc64_output_return (void)
{
  if (ARC_NAKED_P (cfun->machine->fn_type))
    return "";

  return "j_s%*\t[blink]";
}

/* Return nonzero if register FROM_REGNO can be renamed to register
   TO_REGNO.  */

bool
arc64_hard_regno_rename_ok (unsigned from_regno ATTRIBUTE_UNUSED,
			    unsigned to_regno)
{
  /* Interrupt functions can only use registers that have already been saved by
     the prologue, even if they would normally be call-clobbered.  */
  return (!ARC_INTERRUPT_P (cfun->machine->fn_type)
	  || df_regs_ever_live_p (to_regno));
}

/* Emit the RTX necessary to initialize the vector TARGET with values in
   VALS.  */

void
arc64_expand_vector_init (rtx target, rtx vals)
{
  machine_mode mode = GET_MODE (target);
  machine_mode inner_mode = GET_MODE_INNER (mode);
  int n_elts = GET_MODE_NUNITS (mode);
  int i;
  rtx elem[4], tmp[2];

  gcc_assert (n_elts <= 4);
  for (i = 0; i < n_elts; i++)
    {
      elem[i] = XVECEXP (vals, 0, i);
      if (!register_operand (elem[i], GET_MODE (elem[i])))
	elem[i] = force_reg (inner_mode, elem[i]);
    }

  switch (mode)
    {
    case V4HImode:
      tmp[0] = gen_reg_rtx (mode);
      tmp[1] = gen_reg_rtx (mode);
      emit_insn (gen_arc64_vpack_v4hihi (tmp[0], elem[0], elem[1]));
      emit_insn (gen_arc64_vpack_v4hihi (tmp[1], elem[2], elem[3]));
      emit_insn (gen_arc64_sel_lane2_0v4hi (target, tmp[0], tmp[1]));
      break;

    case V2SImode:
      emit_insn (gen_arc64_vpack_v2sisi (target, elem[0], elem[1]));
      break;

    case V2HImode:
      emit_insn (gen_arc64_vpack_v2hihi (target, elem[0], elem[1]));
      break;

    default:
      gcc_unreachable ();
    }
}

/* Target hooks.  */

#undef TARGET_ASM_ALIGNED_DI_OP
#define TARGET_ASM_ALIGNED_DI_OP "\t.xword\t"

#undef TARGET_ASM_ALIGNED_HI_OP
#define TARGET_ASM_ALIGNED_HI_OP "\t.hword\t"

#undef TARGET_ASM_ALIGNED_SI_OP
#define TARGET_ASM_ALIGNED_SI_OP "\t.word\t"

#undef  TARGET_ASM_CAN_OUTPUT_MI_THUNK
#define TARGET_ASM_CAN_OUTPUT_MI_THUNK \
  hook_bool_const_tree_hwi_hwi_const_tree_true

#undef TARGET_ASM_OUTPUT_MI_THUNK
#define TARGET_ASM_OUTPUT_MI_THUNK arc64_output_mi_thunk

#undef TARGET_CAN_ELIMINATE
#define TARGET_CAN_ELIMINATE arc64_can_eliminate

#undef TARGET_FRAME_POINTER_REQUIRED
#define TARGET_FRAME_POINTER_REQUIRED arc64_frame_pointer_required

#undef TARGET_LEGITIMATE_ADDRESS_P
#define TARGET_LEGITIMATE_ADDRESS_P arc64_legitimate_address_p

#undef TARGET_LEGITIMATE_CONSTANT_P
#define TARGET_LEGITIMATE_CONSTANT_P arc64_legitimate_constant_p

#undef TARGET_RETURN_IN_MEMORY
#define TARGET_RETURN_IN_MEMORY arc64_return_in_memory

/* Passing arguments.  */
#undef TARGET_PASS_BY_REFERENCE
#define TARGET_PASS_BY_REFERENCE arc64_pass_by_reference

#undef TARGET_SETUP_INCOMING_VARARGS
#define TARGET_SETUP_INCOMING_VARARGS arc64_setup_incoming_varargs

#undef TARGET_MUST_PASS_IN_STACK
#define TARGET_MUST_PASS_IN_STACK must_pass_in_stack_var_size

#undef TARGET_FUNCTION_VALUE
#define TARGET_FUNCTION_VALUE arc64_function_value

#undef TARGET_FUNCTION_VALUE_REGNO_P
#define TARGET_FUNCTION_VALUE_REGNO_P arc64_function_value_regno_p

#undef TARGET_FUNCTION_ARG
#define TARGET_FUNCTION_ARG arc64_function_arg

#undef TARGET_FUNCTION_ARG_ADVANCE
#define TARGET_FUNCTION_ARG_ADVANCE arc64_function_arg_advance

#undef TARGET_ARG_PARTIAL_BYTES
#define TARGET_ARG_PARTIAL_BYTES arc64_arg_partial_bytes

#undef TARGET_STRICT_ARGUMENT_NAMING
#define TARGET_STRICT_ARGUMENT_NAMING hook_bool_CUMULATIVE_ARGS_true

#undef TARGET_COMPUTE_FRAME_LAYOUT
#define TARGET_COMPUTE_FRAME_LAYOUT arc64_compute_frame_info

#undef TARGET_HARD_REGNO_NREGS
#define TARGET_HARD_REGNO_NREGS arc64_hard_regno_nregs

#undef TARGET_HARD_REGNO_MODE_OK
#define TARGET_HARD_REGNO_MODE_OK arc64_hard_regno_mode_ok

#undef TARGET_MODES_TIEABLE_P
#define TARGET_MODES_TIEABLE_P arc64_modes_tieable_p

#undef  TARGET_PRINT_OPERAND
#define TARGET_PRINT_OPERAND arc64_print_operand

#undef  TARGET_PRINT_OPERAND_ADDRESS
#define TARGET_PRINT_OPERAND_ADDRESS arc64_print_operand_address

#undef TARGET_PRINT_OPERAND_PUNCT_VALID_P
#define TARGET_PRINT_OPERAND_PUNCT_VALID_P arc64_print_operand_punct_valid_p

#undef TARGET_TRAMPOLINE_INIT
#define TARGET_TRAMPOLINE_INIT arc64_initialize_trampoline

#undef TARGET_ASM_TRAMPOLINE_TEMPLATE
#define TARGET_ASM_TRAMPOLINE_TEMPLATE arc64_asm_trampoline_template

#undef TARGET_HAVE_SPECULATION_SAFE_VALUE
#define TARGET_HAVE_SPECULATION_SAFE_VALUE speculation_safe_value_not_needed

#undef  TARGET_FUNCTION_OK_FOR_SIBCALL
#define TARGET_FUNCTION_OK_FOR_SIBCALL arc64_function_ok_for_sibcall

#undef TARGET_INIT_LIBFUNCS
#define TARGET_INIT_LIBFUNCS arc64_init_libfuncs

#undef TARGET_ASM_FILE_END
#define TARGET_ASM_FILE_END file_end_indicate_exec_stack

#undef  TARGET_ASM_FUNCTION_PROLOGUE
#define TARGET_ASM_FUNCTION_PROLOGUE arc64_output_function_prologue

#undef TARGET_CONSTANT_ALIGNMENT
#define TARGET_CONSTANT_ALIGNMENT constant_alignment_word_strings

#undef TARGET_PROMOTE_FUNCTION_MODE
#define TARGET_PROMOTE_FUNCTION_MODE \
  default_promote_function_mode_always_promote

/* To be checked if it is better without it.  */
#undef TARGET_PROMOTE_PROTOTYPES
#define TARGET_PROMOTE_PROTOTYPES hook_bool_const_tree_true

#undef TARGET_ASM_OUTPUT_ADDR_CONST_EXTRA
#define TARGET_ASM_OUTPUT_ADDR_CONST_EXTRA arc64_output_addr_const_extra

#undef  TARGET_INIT_BUILTINS
#define TARGET_INIT_BUILTINS  arc64_init_builtins

#undef  TARGET_EXPAND_BUILTIN
#define TARGET_EXPAND_BUILTIN arc64_expand_builtin

#undef  TARGET_BUILTIN_DECL
#define TARGET_BUILTIN_DECL arc64_builtin_decl

/* Having TLS support, we turn R30 fixed as well.  */
#ifdef HAVE_AS_TLS
#undef TARGET_HAVE_TLS
#define TARGET_HAVE_TLS HAVE_AS_TLS
#endif

#undef TARGET_LRA_P
#define TARGET_LRA_P hook_bool_void_true

#undef  TARGET_INSN_COST
#define TARGET_INSN_COST arc64_insn_cost

#undef  TARGET_MACHINE_DEPENDENT_REORG
#define TARGET_MACHINE_DEPENDENT_REORG arc64_reorg

#undef TARGET_CONDITIONAL_REGISTER_USAGE
#define TARGET_CONDITIONAL_REGISTER_USAGE arc64_conditional_register_usage

#undef TARGET_LIBGCC_FLOATING_MODE_SUPPORTED_P
#define TARGET_LIBGCC_FLOATING_MODE_SUPPORTED_P \
arc64_libgcc_floating_mode_supported_p

#undef TARGET_SCALAR_MODE_SUPPORTED_P
#define TARGET_SCALAR_MODE_SUPPORTED_P arc64_scalar_mode_supported_p

#undef TARGET_SPLIT_COMPLEX_ARG
#define TARGET_SPLIT_COMPLEX_ARG arc64_split_complex_arg

/* Vectors.  */
#undef TARGET_VECTOR_MODE_SUPPORTED_P
#define TARGET_VECTOR_MODE_SUPPORTED_P arc64_vector_mode_supported_p

#undef TARGET_VECTORIZE_PREFERRED_SIMD_MODE
#define TARGET_VECTORIZE_PREFERRED_SIMD_MODE arc64_preferred_simd_mode

#undef TARGET_VECTORIZE_AUTOVECTORIZE_VECTOR_MODES
#define TARGET_VECTORIZE_AUTOVECTORIZE_VECTOR_MODES	\
  arc64_autovectorize_vector_modes

#undef TARGET_VECTORIZE_BUILTIN_VECTORIZATION_COST
#define TARGET_VECTORIZE_BUILTIN_VECTORIZATION_COST	\
  arc64_builtin_vectorization_cost

#undef TARGET_REGISTER_MOVE_COST
#define TARGET_REGISTER_MOVE_COST arc64_register_move_cost

#undef TARGET_VECTORIZE_VEC_PERM_CONST
#define TARGET_VECTORIZE_VEC_PERM_CONST arc64_vectorize_vec_perm_const

#undef TARGET_RTX_COSTS
#define TARGET_RTX_COSTS arc64_rtx_costs_wrapper

#undef TARGET_ADDRESS_COST
#define TARGET_ADDRESS_COST arc64_address_cost

/* Scheduling.  */
#undef TARGET_SCHED_MACRO_FUSION_P
#define TARGET_SCHED_MACRO_FUSION_P arc64_macro_fusion_p

#undef TARGET_SCHED_MACRO_FUSION_PAIR_P
#define TARGET_SCHED_MACRO_FUSION_PAIR_P arc64_macro_fusion_pair_p

/* Disable the speculation when filling delay slots.  In general we get better
   (speed) results but not for EEMBC's text01 benchmark.  Disabling delay slot
   filler speculation is needed to conserve the loops body size as calculated in
   machine reorg phase.  More info see github issue#416.  */
#undef TARGET_NO_SPECULATION_IN_DELAY_SLOTS_P
#define TARGET_NO_SPECULATION_IN_DELAY_SLOTS_P hook_bool_void_true

#undef TARGET_CANNOT_FORCE_CONST_MEM
#define TARGET_CANNOT_FORCE_CONST_MEM arc64_cannot_force_const_mem

#undef TARGET_OPTION_OVERRIDE
#define TARGET_OPTION_OVERRIDE arc64_override_options

/* CC regs optimizations.  */
#undef TARGET_FIXED_CONDITION_CODE_REGS
#define TARGET_FIXED_CONDITION_CODE_REGS arc64_fixed_condition_code_regs

#undef TARGET_FLAGS_REGNUM
#define TARGET_FLAGS_REGNUM CC_REGNUM

#undef  TARGET_SET_CURRENT_FUNCTION
#define TARGET_SET_CURRENT_FUNCTION arc64_set_current_function

#undef TARGET_ATTRIBUTE_TABLE
#define TARGET_ATTRIBUTE_TABLE arc64_attribute_table

#undef TARGET_ALLOCATE_STACK_SLOTS_FOR_ARGS
#define TARGET_ALLOCATE_STACK_SLOTS_FOR_ARGS arc64_allocate_stack_slots_for_args

#undef TARGET_WARN_FUNC_RETURN
#define TARGET_WARN_FUNC_RETURN arc64_warn_func_return

#undef TARGET_CAN_FOLLOW_JUMP
#define TARGET_CAN_FOLLOW_JUMP arc64_can_follow_jump

#undef TARGET_SCHED_ISSUE_RATE
#define TARGET_SCHED_ISSUE_RATE arc64_sched_issue_rate

struct gcc_target targetm = TARGET_INITIALIZER;

#include "gt-arc64.h"
