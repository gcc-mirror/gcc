/* Target machine subroutines for Altera Nios II.
   Copyright (C) 2012-2014 Free Software Foundation, Inc.
   Contributed by Jonah Graham (jgraham@altera.com), 
   Will Reece (wreece@altera.com), and Jeff DaSilva (jdasilva@altera.com).
   Contributed by Mentor Graphics, Inc.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published
   by the Free Software Foundation; either version 3, or (at your
   option) any later version.

   GCC is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
   or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
   License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "rtl.h"
#include "tree.h"
#include "regs.h"
#include "hard-reg-set.h"
#include "insn-config.h"
#include "conditions.h"
#include "output.h"
#include "insn-attr.h"
#include "flags.h"
#include "recog.h"
#include "expr.h"
#include "optabs.h"
#include "function.h"
#include "ggc.h"
#include "basic-block.h"
#include "diagnostic-core.h"
#include "toplev.h"
#include "target.h"
#include "target-def.h"
#include "tm_p.h"
#include "langhooks.h"
#include "df.h"
#include "debug.h"
#include "real.h"
#include "reload.h"
#include "stor-layout.h"
#include "varasm.h"
#include "calls.h"

/* Forward function declarations.  */
static bool prologue_saved_reg_p (unsigned);
static void nios2_load_pic_register (void);
static void nios2_register_custom_code (unsigned int, enum nios2_ccs_code, int);
static const char *nios2_unspec_reloc_name (int);
static void nios2_register_builtin_fndecl (unsigned, tree);

/* Threshold for data being put into the small data/bss area, instead
   of the normal data area (references to the small data/bss area take
   1 instruction, and use the global pointer, references to the normal
   data area takes 2 instructions).  */
unsigned HOST_WIDE_INT nios2_section_threshold = NIOS2_DEFAULT_GVALUE;

struct GTY (()) machine_function
{
  /* Current frame information, to be filled in by nios2_compute_frame_layout
     with register save masks, and offsets for the current function.  */

  /* Mask of registers to save.  */
  unsigned int save_mask;
  /* Number of bytes that the entire frame takes up.  */
  int total_size;
  /* Number of bytes that variables take up.  */
  int var_size;
  /* Number of bytes that outgoing arguments take up.  */
  int args_size;
  /* Number of bytes needed to store registers in frame.  */
  int save_reg_size;
   /* Offset from new stack pointer to store registers.  */
  int save_regs_offset;
  /* != 0 if frame layout already calculated.  */
  int initialized;
};

/* State to track the assignment of custom codes to FPU/custom builtins.  */
static enum nios2_ccs_code custom_code_status[256];
static int custom_code_index[256];
/* Set to true if any conflicts (re-use of a code between 0-255) are found.  */
static bool custom_code_conflict = false;


/* Definition of builtin function types for nios2.  */

#define N2_FTYPES				\
  N2_FTYPE(1, (SF))				\
  N2_FTYPE(1, (VOID))				\
  N2_FTYPE(2, (DF, DF))				\
  N2_FTYPE(3, (DF, DF, DF))			\
  N2_FTYPE(2, (DF, SF))				\
  N2_FTYPE(2, (DF, SI))				\
  N2_FTYPE(2, (DF, UI))				\
  N2_FTYPE(2, (SF, DF))				\
  N2_FTYPE(2, (SF, SF))				\
  N2_FTYPE(3, (SF, SF, SF))			\
  N2_FTYPE(2, (SF, SI))				\
  N2_FTYPE(2, (SF, UI))				\
  N2_FTYPE(2, (SI, CVPTR))			\
  N2_FTYPE(2, (SI, DF))				\
  N2_FTYPE(3, (SI, DF, DF))			\
  N2_FTYPE(2, (SI, SF))				\
  N2_FTYPE(3, (SI, SF, SF))			\
  N2_FTYPE(2, (SI, SI))				\
  N2_FTYPE(2, (UI, CVPTR))			\
  N2_FTYPE(2, (UI, DF))				\
  N2_FTYPE(2, (UI, SF))				\
  N2_FTYPE(2, (VOID, DF))			\
  N2_FTYPE(2, (VOID, SF))			\
  N2_FTYPE(3, (VOID, SI, SI))			\
  N2_FTYPE(3, (VOID, VPTR, SI))

#define N2_FTYPE_OP1(R)         N2_FTYPE_ ## R ## _VOID
#define N2_FTYPE_OP2(R, A1)     N2_FTYPE_ ## R ## _ ## A1
#define N2_FTYPE_OP3(R, A1, A2) N2_FTYPE_ ## R ## _ ## A1 ## _ ## A2

/* Expand ftcode enumeration.  */
enum nios2_ftcode {
#define N2_FTYPE(N,ARGS) N2_FTYPE_OP ## N ARGS,
N2_FTYPES
#undef N2_FTYPE
N2_FTYPE_MAX
};

/* Return the tree function type, based on the ftcode.  */
static tree
nios2_ftype (enum nios2_ftcode ftcode)
{
  static tree types[(int) N2_FTYPE_MAX];

  tree N2_TYPE_SF = float_type_node;
  tree N2_TYPE_DF = double_type_node;
  tree N2_TYPE_SI = integer_type_node;
  tree N2_TYPE_UI = unsigned_type_node;
  tree N2_TYPE_VOID = void_type_node;

  static const_tree N2_TYPE_CVPTR, N2_TYPE_VPTR;
  if (!N2_TYPE_CVPTR)
    {
      /* const volatile void *.  */
      N2_TYPE_CVPTR
	= build_pointer_type (build_qualified_type (void_type_node,
						    (TYPE_QUAL_CONST
						     | TYPE_QUAL_VOLATILE)));
      /* volatile void *.  */
      N2_TYPE_VPTR
	= build_pointer_type (build_qualified_type (void_type_node,
						    TYPE_QUAL_VOLATILE));
    }
  if (types[(int) ftcode] == NULL_TREE)
    switch (ftcode)
      {
#define N2_FTYPE_ARGS1(R) N2_TYPE_ ## R
#define N2_FTYPE_ARGS2(R,A1) N2_TYPE_ ## R, N2_TYPE_ ## A1
#define N2_FTYPE_ARGS3(R,A1,A2) N2_TYPE_ ## R, N2_TYPE_ ## A1, N2_TYPE_ ## A2
#define N2_FTYPE(N,ARGS)						\
  case N2_FTYPE_OP ## N ARGS:						\
    types[(int) ftcode]							\
      = build_function_type_list (N2_FTYPE_ARGS ## N ARGS, NULL_TREE); \
    break;
	N2_FTYPES
#undef N2_FTYPE
      default: gcc_unreachable ();
      }
  return types[(int) ftcode];
}


/* Definition of FPU instruction descriptions.  */

struct nios2_fpu_insn_info
{
  const char *name;
  int num_operands, *optvar;
  int opt, no_opt;
#define N2F_DF            0x1
#define N2F_DFREQ         0x2
#define N2F_UNSAFE        0x4
#define N2F_FINITE        0x8
  unsigned int flags;
  enum insn_code icode;
  enum nios2_ftcode ftcode;
};

/* Base macro for defining FPU instructions.  */
#define N2FPU_INSN_DEF_BASE(insn, nop, flags, icode, args)	\
  { #insn, nop, &nios2_custom_ ## insn, OPT_mcustom_##insn##_,	\
    OPT_mno_custom_##insn, flags, CODE_FOR_ ## icode,		\
    N2_FTYPE_OP ## nop args }

/* Arithmetic and math functions; 2 or 3 operand FP operations.  */
#define N2FPU_OP2(mode) (mode, mode)
#define N2FPU_OP3(mode) (mode, mode, mode)
#define N2FPU_INSN_DEF(code, icode, nop, flags, m, M)			\
  N2FPU_INSN_DEF_BASE (f ## code ## m, nop, flags,			\
		       icode ## m ## f ## nop, N2FPU_OP ## nop (M ## F))
#define N2FPU_INSN_SF(code, nop, flags)		\
  N2FPU_INSN_DEF (code, code, nop, flags, s, S)
#define N2FPU_INSN_DF(code, nop, flags)		\
  N2FPU_INSN_DEF (code, code, nop, flags | N2F_DF, d, D)

/* Compare instructions, 3 operand FP operation with a SI result.  */
#define N2FPU_CMP_DEF(code, flags, m, M)				\
  N2FPU_INSN_DEF_BASE (fcmp ## code ## m, 3, flags,			\
		       nios2_s ## code ## m ## f, (SI, M ## F, M ## F))
#define N2FPU_CMP_SF(code) N2FPU_CMP_DEF (code, 0, s, S)
#define N2FPU_CMP_DF(code) N2FPU_CMP_DEF (code, N2F_DF, d, D)

/* The order of definition needs to be maintained consistent with
   enum n2fpu_code in nios2-opts.h.  */
struct nios2_fpu_insn_info nios2_fpu_insn[] =
  {
    /* Single precision instructions.  */
    N2FPU_INSN_SF (add, 3, 0),
    N2FPU_INSN_SF (sub, 3, 0),
    N2FPU_INSN_SF (mul, 3, 0),
    N2FPU_INSN_SF (div, 3, 0),
    /* Due to textual difference between min/max and smin/smax.  */
    N2FPU_INSN_DEF (min, smin, 3, N2F_FINITE, s, S),
    N2FPU_INSN_DEF (max, smax, 3, N2F_FINITE, s, S),
    N2FPU_INSN_SF (neg, 2, 0),
    N2FPU_INSN_SF (abs, 2, 0),
    N2FPU_INSN_SF (sqrt, 2, 0),
    N2FPU_INSN_SF (sin, 2, N2F_UNSAFE),
    N2FPU_INSN_SF (cos, 2, N2F_UNSAFE),
    N2FPU_INSN_SF (tan, 2, N2F_UNSAFE),
    N2FPU_INSN_SF (atan, 2, N2F_UNSAFE),
    N2FPU_INSN_SF (exp, 2, N2F_UNSAFE),
    N2FPU_INSN_SF (log, 2, N2F_UNSAFE),
    /* Single precision compares.  */
    N2FPU_CMP_SF (eq), N2FPU_CMP_SF (ne),
    N2FPU_CMP_SF (lt), N2FPU_CMP_SF (le),
    N2FPU_CMP_SF (gt), N2FPU_CMP_SF (ge),

    /* Double precision instructions.  */
    N2FPU_INSN_DF (add, 3, 0),
    N2FPU_INSN_DF (sub, 3, 0),
    N2FPU_INSN_DF (mul, 3, 0),
    N2FPU_INSN_DF (div, 3, 0),
    /* Due to textual difference between min/max and smin/smax.  */
    N2FPU_INSN_DEF (min, smin, 3, N2F_FINITE, d, D),
    N2FPU_INSN_DEF (max, smax, 3, N2F_FINITE, d, D),
    N2FPU_INSN_DF (neg, 2, 0),
    N2FPU_INSN_DF (abs, 2, 0),
    N2FPU_INSN_DF (sqrt, 2, 0),
    N2FPU_INSN_DF (sin, 2, N2F_UNSAFE),
    N2FPU_INSN_DF (cos, 2, N2F_UNSAFE),
    N2FPU_INSN_DF (tan, 2, N2F_UNSAFE),
    N2FPU_INSN_DF (atan, 2, N2F_UNSAFE),
    N2FPU_INSN_DF (exp, 2, N2F_UNSAFE),
    N2FPU_INSN_DF (log, 2, N2F_UNSAFE),
    /* Double precision compares.  */
    N2FPU_CMP_DF (eq), N2FPU_CMP_DF (ne),
    N2FPU_CMP_DF (lt), N2FPU_CMP_DF (le),
    N2FPU_CMP_DF (gt), N2FPU_CMP_DF (ge),

    /* Conversion instructions.  */
    N2FPU_INSN_DEF_BASE (floatis,  2, 0, floatsisf2,    (SF, SI)),
    N2FPU_INSN_DEF_BASE (floatus,  2, 0, floatunssisf2, (SF, UI)),
    N2FPU_INSN_DEF_BASE (floatid,  2, 0, floatsidf2,    (DF, SI)),
    N2FPU_INSN_DEF_BASE (floatud,  2, 0, floatunssidf2, (DF, UI)),
    N2FPU_INSN_DEF_BASE (fixsi,    2, 0, fix_truncsfsi2,      (SI, SF)),
    N2FPU_INSN_DEF_BASE (fixsu,    2, 0, fixuns_truncsfsi2,   (UI, SF)),
    N2FPU_INSN_DEF_BASE (fixdi,    2, 0, fix_truncdfsi2,      (SI, DF)),
    N2FPU_INSN_DEF_BASE (fixdu,    2, 0, fixuns_truncdfsi2,   (UI, DF)),
    N2FPU_INSN_DEF_BASE (fextsd,   2, 0, extendsfdf2,   (DF, SF)),
    N2FPU_INSN_DEF_BASE (ftruncds, 2, 0, truncdfsf2,    (SF, DF)),

    /* X, Y access instructions.  */
    N2FPU_INSN_DEF_BASE (fwrx,     2, N2F_DFREQ, nios2_fwrx,   (VOID, DF)),
    N2FPU_INSN_DEF_BASE (fwry,     2, N2F_DFREQ, nios2_fwry,   (VOID, SF)),
    N2FPU_INSN_DEF_BASE (frdxlo,   1, N2F_DFREQ, nios2_frdxlo, (SF)),
    N2FPU_INSN_DEF_BASE (frdxhi,   1, N2F_DFREQ, nios2_frdxhi, (SF)),
    N2FPU_INSN_DEF_BASE (frdy,     1, N2F_DFREQ, nios2_frdy,   (SF))
  };

/* Some macros for ease of access.  */
#define N2FPU(code) nios2_fpu_insn[(int) code]
#define N2FPU_ENABLED_P(code) (N2FPU_N(code) >= 0)
#define N2FPU_N(code) (*N2FPU(code).optvar)
#define N2FPU_NAME(code) (N2FPU(code).name)
#define N2FPU_ICODE(code) (N2FPU(code).icode)
#define N2FPU_FTCODE(code) (N2FPU(code).ftcode)
#define N2FPU_FINITE_P(code) (N2FPU(code).flags & N2F_FINITE)
#define N2FPU_UNSAFE_P(code) (N2FPU(code).flags & N2F_UNSAFE)
#define N2FPU_DOUBLE_P(code) (N2FPU(code).flags & N2F_DF)
#define N2FPU_DOUBLE_REQUIRED_P(code) (N2FPU(code).flags & N2F_DFREQ)

/* Same as above, but for cases where using only the op part is shorter.  */
#define N2FPU_OP(op) N2FPU(n2fpu_ ## op)
#define N2FPU_OP_NAME(op) N2FPU_NAME(n2fpu_ ## op)
#define N2FPU_OP_ENABLED_P(op) N2FPU_ENABLED_P(n2fpu_ ## op)

/* Export the FPU insn enabled predicate to nios2.md.  */
bool
nios2_fpu_insn_enabled (enum n2fpu_code code)
{
  return N2FPU_ENABLED_P (code);
}

/* Return true if COND comparison for mode MODE is enabled under current
   settings.  */

static bool
nios2_fpu_compare_enabled (enum rtx_code cond, enum machine_mode mode)
{
  if (mode == SFmode)
    switch (cond) 
      {
      case EQ: return N2FPU_OP_ENABLED_P (fcmpeqs);
      case NE: return N2FPU_OP_ENABLED_P (fcmpnes);
      case GT: return N2FPU_OP_ENABLED_P (fcmpgts);
      case GE: return N2FPU_OP_ENABLED_P (fcmpges);
      case LT: return N2FPU_OP_ENABLED_P (fcmplts);
      case LE: return N2FPU_OP_ENABLED_P (fcmples);
      default: break;
      }
  else if (mode == DFmode)
    switch (cond) 
      {
      case EQ: return N2FPU_OP_ENABLED_P (fcmpeqd);
      case NE: return N2FPU_OP_ENABLED_P (fcmpned);
      case GT: return N2FPU_OP_ENABLED_P (fcmpgtd);
      case GE: return N2FPU_OP_ENABLED_P (fcmpged);
      case LT: return N2FPU_OP_ENABLED_P (fcmpltd);
      case LE: return N2FPU_OP_ENABLED_P (fcmpled);
      default: break;
      }
  return false;
}

/* Stack layout and calling conventions.  */

#define NIOS2_STACK_ALIGN(LOC)						\
  (((LOC) + ((PREFERRED_STACK_BOUNDARY / BITS_PER_UNIT) - 1))		\
   & ~((PREFERRED_STACK_BOUNDARY / BITS_PER_UNIT) - 1))

/* Return the bytes needed to compute the frame pointer from the current
   stack pointer.  */
static int
nios2_compute_frame_layout (void)
{
  unsigned int regno;
  unsigned int save_mask = 0;
  int total_size;
  int var_size;
  int out_args_size;
  int save_reg_size;

  if (cfun->machine->initialized)
    return cfun->machine->total_size;
  
  var_size = NIOS2_STACK_ALIGN (get_frame_size ());
  out_args_size = NIOS2_STACK_ALIGN (crtl->outgoing_args_size);
  total_size = var_size + out_args_size;

  /* Calculate space needed for gp registers.  */
  save_reg_size = 0;
  for (regno = 0; regno <= LAST_GP_REG; regno++)
    if (prologue_saved_reg_p (regno))
      {
	save_mask |= 1 << regno;
	save_reg_size += 4;
      }

  /* If we call eh_return, we need to save the EH data registers.  */
  if (crtl->calls_eh_return)
    {
      unsigned i;
      unsigned r;
      
      for (i = 0; (r = EH_RETURN_DATA_REGNO (i)) != INVALID_REGNUM; i++)
	if (!(save_mask & (1 << r)))
	  {
	    save_mask |= 1 << r;
	    save_reg_size += 4;
	  }
    }

  save_reg_size = NIOS2_STACK_ALIGN (save_reg_size);
  total_size += save_reg_size;
  total_size += NIOS2_STACK_ALIGN (crtl->args.pretend_args_size);

  /* Save other computed information.  */
  cfun->machine->save_mask = save_mask;
  cfun->machine->total_size = total_size;
  cfun->machine->var_size = var_size;
  cfun->machine->args_size = out_args_size;
  cfun->machine->save_reg_size = save_reg_size;
  cfun->machine->initialized = reload_completed;
  cfun->machine->save_regs_offset = out_args_size + var_size;

  return total_size;
}

/* Generate save/restore of register REGNO at SP + OFFSET.  Used by the
   prologue/epilogue expand routines.  */
static void
save_reg (int regno, unsigned offset)
{
  rtx reg = gen_rtx_REG (SImode, regno);
  rtx addr = gen_rtx_PLUS (Pmode, stack_pointer_rtx,
			   gen_int_mode (offset, Pmode));
  rtx insn = emit_move_insn (gen_frame_mem (Pmode, addr), reg);
  RTX_FRAME_RELATED_P (insn) = 1;
}

static void
restore_reg (int regno, unsigned offset)
{
  rtx reg = gen_rtx_REG (SImode, regno);
  rtx addr = gen_rtx_PLUS (Pmode, stack_pointer_rtx,
			   gen_int_mode (offset, Pmode));
  rtx insn = emit_move_insn (reg, gen_frame_mem (Pmode, addr));
  /* Tag epilogue unwind note.  */
  add_reg_note (insn, REG_CFA_RESTORE, reg);
  RTX_FRAME_RELATED_P (insn) = 1;
}

/* Emit conditional trap for checking stack limit.  */
static void
nios2_emit_stack_limit_check (void)
{
  if (REG_P (stack_limit_rtx))
    emit_insn (gen_ctrapsi4 (gen_rtx_LTU (VOIDmode, stack_pointer_rtx,
					  stack_limit_rtx),
			     stack_pointer_rtx, stack_limit_rtx, GEN_INT (3)));
  else
    sorry ("only register based stack limit is supported");
}

/* Temp regno used inside prologue/epilogue.  */
#define TEMP_REG_NUM 8

void
nios2_expand_prologue (void)
{
  unsigned int regno;
  int total_frame_size, save_offset;
  int sp_offset; /* offset from base_reg to final stack value.  */
  int fp_offset; /* offset from base_reg to final fp value.  */
  rtx insn;

  total_frame_size = nios2_compute_frame_layout ();

  if (flag_stack_usage_info)
    current_function_static_stack_size = total_frame_size;

  /* Decrement the stack pointer.  */
  if (!SMALL_INT (total_frame_size))
    {
      /* We need an intermediary point, this will point at the spill block.  */
      insn = emit_insn
	(gen_add2_insn (stack_pointer_rtx,
			gen_int_mode (cfun->machine->save_regs_offset
				      - total_frame_size, Pmode)));
      RTX_FRAME_RELATED_P (insn) = 1;

      fp_offset = 0;
      sp_offset = -cfun->machine->save_regs_offset;
    }
  else if (total_frame_size)
    {
      insn = emit_insn (gen_add2_insn (stack_pointer_rtx,
				       gen_int_mode (-total_frame_size,
						     Pmode)));
      RTX_FRAME_RELATED_P (insn) = 1;
      fp_offset = cfun->machine->save_regs_offset;
      sp_offset = 0;
    }
  else
    fp_offset = sp_offset = 0;

  if (crtl->limit_stack)
    nios2_emit_stack_limit_check ();

  save_offset = fp_offset + cfun->machine->save_reg_size;

  for (regno = LAST_GP_REG; regno > 0; regno--)
    if (cfun->machine->save_mask & (1 << regno))
      {
	save_offset -= 4;
	save_reg (regno, save_offset);
      }

  if (frame_pointer_needed)
    {
      insn = emit_insn (gen_add3_insn (hard_frame_pointer_rtx,
				       stack_pointer_rtx,
				       gen_int_mode (fp_offset, Pmode)));
      RTX_FRAME_RELATED_P (insn) = 1;
    }

  if (sp_offset)
    {
      rtx sp_adjust
	= gen_rtx_SET (VOIDmode, stack_pointer_rtx,
		       plus_constant (Pmode, stack_pointer_rtx, sp_offset));
      if (SMALL_INT (sp_offset))
	insn = emit_insn (sp_adjust);
      else
	{
	  rtx tmp = gen_rtx_REG (Pmode, TEMP_REG_NUM);
	  emit_move_insn (tmp, gen_int_mode (sp_offset, Pmode));
	  insn = emit_insn (gen_add2_insn (stack_pointer_rtx, tmp));
	  /* Attach the sp_adjust as a note indicating what happened.  */
	  add_reg_note (insn, REG_FRAME_RELATED_EXPR, sp_adjust);
	}
      RTX_FRAME_RELATED_P (insn) = 1;

      if (crtl->limit_stack)
	nios2_emit_stack_limit_check ();
    }

  /* Load the PIC register if needed.  */
  if (crtl->uses_pic_offset_table)
    nios2_load_pic_register ();

  /* If we are profiling, make sure no instructions are scheduled before
     the call to mcount.  */
  if (crtl->profile)
    emit_insn (gen_blockage ());
}

void
nios2_expand_epilogue (bool sibcall_p)
{
  rtx insn, cfa_adj;
  int total_frame_size;
  int sp_adjust, save_offset;
  unsigned int regno;

  if (!sibcall_p && nios2_can_use_return_insn ())
    {
      emit_jump_insn (gen_return ());
      return;
    }

  emit_insn (gen_blockage ());

  total_frame_size = nios2_compute_frame_layout ();
  if (frame_pointer_needed)
    {
      /* Recover the stack pointer.  */
      insn = emit_move_insn (stack_pointer_rtx, hard_frame_pointer_rtx);
      cfa_adj = plus_constant (Pmode, stack_pointer_rtx,
			       (total_frame_size
				- cfun->machine->save_regs_offset));
      add_reg_note (insn, REG_CFA_DEF_CFA, cfa_adj);
      RTX_FRAME_RELATED_P (insn) = 1;

      save_offset = 0;
      sp_adjust = total_frame_size - cfun->machine->save_regs_offset;
    }
  else if (!SMALL_INT (total_frame_size))
    {
      rtx tmp = gen_rtx_REG (Pmode, TEMP_REG_NUM);
      emit_move_insn (tmp, gen_int_mode (cfun->machine->save_regs_offset,
					 Pmode));
      insn = emit_insn (gen_add2_insn (stack_pointer_rtx, tmp));
      cfa_adj = gen_rtx_SET (VOIDmode, stack_pointer_rtx,
			     plus_constant (Pmode, stack_pointer_rtx,
					    cfun->machine->save_regs_offset));
      add_reg_note (insn, REG_CFA_ADJUST_CFA, cfa_adj);
      RTX_FRAME_RELATED_P (insn) = 1;
      save_offset = 0;
      sp_adjust = total_frame_size - cfun->machine->save_regs_offset;
    }
  else
    {
      save_offset = cfun->machine->save_regs_offset;
      sp_adjust = total_frame_size;
    }
  
  save_offset += cfun->machine->save_reg_size;

  for (regno = LAST_GP_REG; regno > 0; regno--)
    if (cfun->machine->save_mask & (1 << regno))
      {
	save_offset -= 4;
	restore_reg (regno, save_offset);
      }

  if (sp_adjust)
    {
      insn = emit_insn (gen_add2_insn (stack_pointer_rtx,
				       gen_int_mode (sp_adjust, Pmode)));
      cfa_adj = gen_rtx_SET (VOIDmode, stack_pointer_rtx,
			     plus_constant (Pmode, stack_pointer_rtx,
					    sp_adjust));
      add_reg_note (insn, REG_CFA_ADJUST_CFA, cfa_adj);
      RTX_FRAME_RELATED_P (insn) = 1;
    }

  /* Add in the __builtin_eh_return stack adjustment.  */
  if (crtl->calls_eh_return)
    emit_insn (gen_add2_insn (stack_pointer_rtx, EH_RETURN_STACKADJ_RTX));

  if (!sibcall_p)
    emit_jump_insn (gen_simple_return ());
}

/* Implement RETURN_ADDR_RTX.  Note, we do not support moving
   back to a previous frame.  */
rtx
nios2_get_return_address (int count)
{
  if (count != 0)
    return const0_rtx;

  return get_hard_reg_initial_val (Pmode, RA_REGNO);
}

/* Emit code to change the current function's return address to
   ADDRESS.  SCRATCH is available as a scratch register, if needed.
   ADDRESS and SCRATCH are both word-mode GPRs.  */
void
nios2_set_return_address (rtx address, rtx scratch)
{
  nios2_compute_frame_layout ();
  if (cfun->machine->save_mask & (1 << RA_REGNO))
    {
      unsigned offset = cfun->machine->save_reg_size - 4;
      rtx base;
      
      if (frame_pointer_needed)
	base = hard_frame_pointer_rtx;
      else
	{
	  base = stack_pointer_rtx;
	  offset += cfun->machine->save_regs_offset;

	  if (!SMALL_INT (offset))
	    {
	      emit_move_insn (scratch, gen_int_mode (offset, Pmode));
	      emit_insn (gen_add2_insn (scratch, base));
	      base = scratch;
	      offset = 0;
	    }
	}
      if (offset)
	base = plus_constant (Pmode, base, offset);
      emit_move_insn (gen_rtx_MEM (Pmode, base), address);
    }
  else
    emit_move_insn (gen_rtx_REG (Pmode, RA_REGNO), address);
}

/* Implement FUNCTION_PROFILER macro.  */
void
nios2_function_profiler (FILE *file, int labelno ATTRIBUTE_UNUSED)
{
  fprintf (file, "\tmov\tr8, ra\n");
  if (flag_pic)
    {
      fprintf (file, "\tnextpc\tr2\n");
      fprintf (file, "\t1: movhi\tr3, %%hiadj(_gp_got - 1b)\n");
      fprintf (file, "\taddi\tr3, r3, %%lo(_gp_got - 1b)\n");
      fprintf (file, "\tadd\tr2, r2, r3\n");
      fprintf (file, "\tldw\tr2, %%call(_mcount)(r2)\n");
      fprintf (file, "\tcallr\tr2\n");
    }
  else
    fprintf (file, "\tcall\t_mcount\n");
  fprintf (file, "\tmov\tra, r8\n");
}

/* Dump stack layout.  */
static void
nios2_dump_frame_layout (FILE *file)
{
  fprintf (file, "\t%s Current Frame Info\n", ASM_COMMENT_START);
  fprintf (file, "\t%s total_size = %d\n", ASM_COMMENT_START,
           cfun->machine->total_size);
  fprintf (file, "\t%s var_size = %d\n", ASM_COMMENT_START,
           cfun->machine->var_size);
  fprintf (file, "\t%s args_size = %d\n", ASM_COMMENT_START,
           cfun->machine->args_size);
  fprintf (file, "\t%s save_reg_size = %d\n", ASM_COMMENT_START,
           cfun->machine->save_reg_size);
  fprintf (file, "\t%s initialized = %d\n", ASM_COMMENT_START,
           cfun->machine->initialized);
  fprintf (file, "\t%s save_regs_offset = %d\n", ASM_COMMENT_START,
           cfun->machine->save_regs_offset);
  fprintf (file, "\t%s is_leaf = %d\n", ASM_COMMENT_START,
           crtl->is_leaf);
  fprintf (file, "\t%s frame_pointer_needed = %d\n", ASM_COMMENT_START,
           frame_pointer_needed);
  fprintf (file, "\t%s pretend_args_size = %d\n", ASM_COMMENT_START,
           crtl->args.pretend_args_size);
}

/* Return true if REGNO should be saved in the prologue.  */
static bool
prologue_saved_reg_p (unsigned regno)
{
  gcc_assert (GP_REG_P (regno));
  
  if (df_regs_ever_live_p (regno) && !call_used_regs[regno])
    return true;

  if (regno == HARD_FRAME_POINTER_REGNUM && frame_pointer_needed)
    return true;

  if (regno == PIC_OFFSET_TABLE_REGNUM && crtl->uses_pic_offset_table)
    return true;

  if (regno == RA_REGNO && df_regs_ever_live_p (RA_REGNO))
    return true;

  return false;
}

/* Implement TARGET_CAN_ELIMINATE.  */
static bool
nios2_can_eliminate (const int from ATTRIBUTE_UNUSED, const int to)
{
  if (to == STACK_POINTER_REGNUM)
    return !frame_pointer_needed;
  return true;
}

/* Implement INITIAL_ELIMINATION_OFFSET macro.  */
int
nios2_initial_elimination_offset (int from, int to)
{
  int offset;

  nios2_compute_frame_layout ();

  /* Set OFFSET to the offset from the stack pointer.  */
  switch (from)
    {
    case FRAME_POINTER_REGNUM:
      offset = cfun->machine->args_size;
      break;

    case ARG_POINTER_REGNUM:
      offset = cfun->machine->total_size;
      offset -= crtl->args.pretend_args_size;
      break;

    default:
      gcc_unreachable ();
    }

    /* If we are asked for the frame pointer offset, then adjust OFFSET
       by the offset from the frame pointer to the stack pointer.  */
  if (to == HARD_FRAME_POINTER_REGNUM)
    offset -= cfun->machine->save_regs_offset;

  return offset;
}

/* Return nonzero if this function is known to have a null epilogue.
   This allows the optimizer to omit jumps to jumps if no stack
   was created.  */
int
nios2_can_use_return_insn (void)
{
  if (!reload_completed || crtl->profile)
    return 0;

  return nios2_compute_frame_layout () == 0;
}


/* Check and signal some warnings/errors on FPU insn options.  */
static void
nios2_custom_check_insns (void)
{
  unsigned int i, j;
  bool errors = false;

  for (i = 0; i < ARRAY_SIZE (nios2_fpu_insn); i++)
    if (N2FPU_ENABLED_P (i) && N2FPU_DOUBLE_P (i))
      {
	for (j = 0; j < ARRAY_SIZE (nios2_fpu_insn); j++)
	  if (N2FPU_DOUBLE_REQUIRED_P (j) && ! N2FPU_ENABLED_P (j))
	    {
	      error ("switch %<-mcustom-%s%> is required for double "
		     "precision floating point", N2FPU_NAME (j));
	      errors = true;
	    }
	break;
      }

  /* Warn if the user has certain exotic operations that won't get used
     without -funsafe-math-optimizations.  See expand_builtin () in
     builtins.c.  */
  if (!flag_unsafe_math_optimizations)
    for (i = 0; i < ARRAY_SIZE (nios2_fpu_insn); i++)
      if (N2FPU_ENABLED_P (i) && N2FPU_UNSAFE_P (i))
	warning (0, "switch %<-mcustom-%s%> has no effect unless "
		 "-funsafe-math-optimizations is specified", N2FPU_NAME (i));

  /* Warn if the user is trying to use -mcustom-fmins et. al, that won't
     get used without -ffinite-math-only.  See fold_builtin_fmin_fmax ()
     in builtins.c.  */
  if (!flag_finite_math_only)
    for (i = 0; i < ARRAY_SIZE (nios2_fpu_insn); i++)
      if (N2FPU_ENABLED_P (i) && N2FPU_FINITE_P (i))
	warning (0, "switch %<-mcustom-%s%> has no effect unless "
		 "-ffinite-math-only is specified", N2FPU_NAME (i));

  if (errors || custom_code_conflict)
    fatal_error ("conflicting use of -mcustom switches, target attributes, "
		 "and/or __builtin_custom_ functions");
}

static void
nios2_set_fpu_custom_code (enum n2fpu_code code, int n, bool override_p)
{
  if (override_p || N2FPU_N (code) == -1)
    N2FPU_N (code) = n;
  nios2_register_custom_code (n, CCS_FPU, (int) code);
}

/* Type to represent a standard FPU config.  */
struct nios2_fpu_config
{
  const char *name;
  bool set_sp_constants;
  int code[n2fpu_code_num];
};

#define NIOS2_FPU_CONFIG_NUM 3
static struct nios2_fpu_config custom_fpu_config[NIOS2_FPU_CONFIG_NUM];

static void
nios2_init_fpu_configs (void)
{
  struct nios2_fpu_config* cfg;
  int i = 0;
#define NEXT_FPU_CONFIG				\
  do {						\
    cfg = &custom_fpu_config[i++];			\
    memset (cfg, -1, sizeof (struct nios2_fpu_config));\
  } while (0)

  NEXT_FPU_CONFIG;
  cfg->name = "60-1";
  cfg->set_sp_constants  = true;
  cfg->code[n2fpu_fmuls] = 252;
  cfg->code[n2fpu_fadds] = 253;
  cfg->code[n2fpu_fsubs] = 254;

  NEXT_FPU_CONFIG;
  cfg->name = "60-2";
  cfg->set_sp_constants  = true;
  cfg->code[n2fpu_fmuls] = 252;
  cfg->code[n2fpu_fadds] = 253;
  cfg->code[n2fpu_fsubs] = 254;
  cfg->code[n2fpu_fdivs] = 255;

  NEXT_FPU_CONFIG;
  cfg->name = "72-3";
  cfg->set_sp_constants    = true;
  cfg->code[n2fpu_floatus] = 243;
  cfg->code[n2fpu_fixsi]   = 244;
  cfg->code[n2fpu_floatis] = 245;
  cfg->code[n2fpu_fcmpgts] = 246;
  cfg->code[n2fpu_fcmples] = 249;
  cfg->code[n2fpu_fcmpeqs] = 250;
  cfg->code[n2fpu_fcmpnes] = 251;
  cfg->code[n2fpu_fmuls]   = 252;
  cfg->code[n2fpu_fadds]   = 253;
  cfg->code[n2fpu_fsubs]   = 254;
  cfg->code[n2fpu_fdivs]   = 255;

#undef NEXT_FPU_CONFIG
  gcc_assert (i == NIOS2_FPU_CONFIG_NUM);
}

static struct nios2_fpu_config *
nios2_match_custom_fpu_cfg (const char *cfgname, const char *endp)
{
  int i;
  for (i = 0; i < NIOS2_FPU_CONFIG_NUM; i++)
    {
      bool match = !(endp != NULL
		     ? strncmp (custom_fpu_config[i].name, cfgname,
				endp - cfgname)
		     : strcmp (custom_fpu_config[i].name, cfgname));
      if (match)
	return &custom_fpu_config[i];
    }
  return NULL;
}

/* Use CFGNAME to lookup FPU config, ENDP if not NULL marks end of string.
   OVERRIDE is true if loaded config codes should overwrite current state.  */
static void
nios2_handle_custom_fpu_cfg (const char *cfgname, const char *endp,
			     bool override)
{
  struct nios2_fpu_config *cfg = nios2_match_custom_fpu_cfg (cfgname, endp);
  if (cfg)
    {
      unsigned int i;
      for (i = 0; i < ARRAY_SIZE (nios2_fpu_insn); i++)
	if (cfg->code[i] >= 0)
	  nios2_set_fpu_custom_code ((enum n2fpu_code) i, cfg->code[i],
				     override);
      if (cfg->set_sp_constants)
	flag_single_precision_constant = 1;
    }
  else
    warning (0, "ignoring unrecognized switch %<-mcustom-fpu-cfg%> "
	     "value %<%s%>", cfg);    

  /* Guard against errors in the standard configurations.  */
  nios2_custom_check_insns ();
}

/* Check individual FPU insn options, and register custom code.  */
static void
nios2_handle_custom_fpu_insn_option (int fpu_insn_index)
{
  int param = N2FPU_N (fpu_insn_index);

  if (0 <= param && param <= 255)
    nios2_register_custom_code (param, CCS_FPU, fpu_insn_index);

  /* Valid values are 0-255, but also allow -1 so that the
     -mno-custom-<opt> switches work.  */
  else if (param != -1)
    error ("switch %<-mcustom-%s%> value %d must be between 0 and 255",
	   N2FPU_NAME (fpu_insn_index), param);
}

/* Allocate a chunk of memory for per-function machine-dependent data.  */
static struct machine_function *
nios2_init_machine_status (void)
{
  return ggc_alloc_cleared_machine_function ();
}

/* Implement TARGET_OPTION_OVERRIDE.  */
static void
nios2_option_override (void)
{
  unsigned int i;

#ifdef SUBTARGET_OVERRIDE_OPTIONS
  SUBTARGET_OVERRIDE_OPTIONS;
#endif

  /* Check for unsupported options.  */
  if (flag_pic && !TARGET_LINUX_ABI)
    sorry ("position-independent code requires the Linux ABI");

  /* Function to allocate machine-dependent function status.  */
  init_machine_status = &nios2_init_machine_status;

  nios2_section_threshold
    = (global_options_set.x_g_switch_value
       ? g_switch_value : NIOS2_DEFAULT_GVALUE);

  /* Default to -mgpopt unless -fpic or -fPIC.  */
  if (TARGET_GPOPT == -1 && flag_pic)
    TARGET_GPOPT = 0;

  /* If we don't have mul, we don't have mulx either!  */
  if (!TARGET_HAS_MUL && TARGET_HAS_MULX)
    target_flags &= ~MASK_HAS_MULX;

  /* Initialize default FPU configurations.  */
  nios2_init_fpu_configs ();

  /* Set up default handling for floating point custom instructions.

     Putting things in this order means that the -mcustom-fpu-cfg=
     switch will always be overridden by individual -mcustom-fadds=
     switches, regardless of the order in which they were specified
     on the command line.

     This behavior of prioritization of individual -mcustom-<insn>=
     options before the -mcustom-fpu-cfg= switch is maintained for
     compatibility.  */
  if (nios2_custom_fpu_cfg_string && *nios2_custom_fpu_cfg_string)
    nios2_handle_custom_fpu_cfg (nios2_custom_fpu_cfg_string, NULL, false);

  /* Handle options for individual FPU insns.  */
  for (i = 0; i < ARRAY_SIZE (nios2_fpu_insn); i++)
    nios2_handle_custom_fpu_insn_option (i);

  nios2_custom_check_insns ();

  /* Save the initial options in case the user does function specific
     options.  */
  target_option_default_node = target_option_current_node
    = build_target_option_node (&global_options);
}


/* Return true if CST is a constant within range of movi/movui/movhi.  */
static bool
nios2_simple_const_p (const_rtx cst)
{
  HOST_WIDE_INT val = INTVAL (cst);
  return SMALL_INT (val) || SMALL_INT_UNSIGNED (val) || UPPER16_INT (val);
}

/* Compute a (partial) cost for rtx X.  Return true if the complete
   cost has been computed, and false if subexpressions should be
   scanned.  In either case, *TOTAL contains the cost result.  */
static bool
nios2_rtx_costs (rtx x, int code, int outer_code ATTRIBUTE_UNUSED,
		 int opno ATTRIBUTE_UNUSED,
		 int *total, bool speed ATTRIBUTE_UNUSED)
{
  switch (code)
    {
      case CONST_INT:
        if (INTVAL (x) == 0)
          {
            *total = COSTS_N_INSNS (0);
            return true;
          }
        else if (nios2_simple_const_p (x))
          {
            *total = COSTS_N_INSNS (2);
            return true;
          }
        else
          {
            *total = COSTS_N_INSNS (4);
            return true;
          }

      case LABEL_REF:
      case SYMBOL_REF:
      case CONST:
      case CONST_DOUBLE:
        {
          *total = COSTS_N_INSNS (4);
          return true;
        }

      case AND:
	{
	  /* Recognize 'nor' insn pattern.  */
	  if (GET_CODE (XEXP (x, 0)) == NOT
	      && GET_CODE (XEXP (x, 1)) == NOT)
	    {
	      *total = COSTS_N_INSNS (1);
	      return true;
	    }
	  return false;
	}

      case MULT:
        {
          *total = COSTS_N_INSNS (1);
          return false;
        }
      case SIGN_EXTEND:
        {
          *total = COSTS_N_INSNS (3);
          return false;
        }
      case ZERO_EXTEND:
        {
          *total = COSTS_N_INSNS (1);
          return false;
        }

      default:
        return false;
    }
}

/* Implement TARGET_PREFERRED_RELOAD_CLASS.  */
static reg_class_t
nios2_preferred_reload_class (rtx x ATTRIBUTE_UNUSED, reg_class_t regclass)
{
  return regclass == NO_REGS ? GENERAL_REGS : regclass;
}

/* Emit a call to __tls_get_addr.  TI is the argument to this function.
   RET is an RTX for the return value location.  The entire insn sequence
   is returned.  */
static GTY(()) rtx nios2_tls_symbol;

static rtx
nios2_call_tls_get_addr (rtx ti)
{
  rtx arg = gen_rtx_REG (Pmode, FIRST_ARG_REGNO);
  rtx ret = gen_rtx_REG (Pmode, FIRST_RETVAL_REGNO);
  rtx fn, insn;
  
  if (!nios2_tls_symbol)
    nios2_tls_symbol = init_one_libfunc ("__tls_get_addr");

  emit_move_insn (arg, ti);
  fn = gen_rtx_MEM (QImode, nios2_tls_symbol);
  insn = emit_call_insn (gen_call_value (ret, fn, const0_rtx));
  RTL_CONST_CALL_P (insn) = 1;
  use_reg (&CALL_INSN_FUNCTION_USAGE (insn), ret);
  use_reg (&CALL_INSN_FUNCTION_USAGE (insn), arg);

  return ret;
}

static rtx
nios2_unspec_address (rtx loc, rtx base_reg, int unspec)
{
  rtx unspec_offset =
    gen_rtx_CONST (Pmode, gen_rtx_UNSPEC (Pmode, gen_rtvec (1, loc),
					  unspec));
  return gen_rtx_PLUS (Pmode, base_reg, unspec_offset);
}

static rtx
nios2_got_address (rtx loc, int unspec)
{
  crtl->uses_pic_offset_table = 1;
  return nios2_unspec_address (loc, pic_offset_table_rtx, unspec);
}

/* Generate the code to access LOC, a thread local SYMBOL_REF.  The
   return value will be a valid address and move_operand (either a REG
   or a LO_SUM).  */
static rtx
nios2_legitimize_tls_address (rtx loc)
{
  rtx tmp, mem, tp;
  enum tls_model model = SYMBOL_REF_TLS_MODEL (loc);

  switch (model)
    {
    case TLS_MODEL_GLOBAL_DYNAMIC:
      tmp = gen_reg_rtx (Pmode);
      emit_move_insn (tmp, nios2_got_address (loc, UNSPEC_ADD_TLS_GD));
      return nios2_call_tls_get_addr (tmp);

    case TLS_MODEL_LOCAL_DYNAMIC:
      tmp = gen_reg_rtx (Pmode);
      emit_move_insn (tmp, nios2_got_address (loc, UNSPEC_ADD_TLS_LDM));
      return nios2_unspec_address (loc, nios2_call_tls_get_addr (tmp),
				   UNSPEC_ADD_TLS_LDO);

    case TLS_MODEL_INITIAL_EXEC:
      tmp = gen_reg_rtx (Pmode);
      mem = gen_const_mem (Pmode, nios2_got_address (loc, UNSPEC_LOAD_TLS_IE));
      emit_move_insn (tmp, mem);
      tp = gen_rtx_REG (Pmode, TP_REGNO);
      return gen_rtx_PLUS (Pmode, tp, tmp);

    case TLS_MODEL_LOCAL_EXEC:
      tp = gen_rtx_REG (Pmode, TP_REGNO);
      return nios2_unspec_address (loc, tp, UNSPEC_ADD_TLS_LE);

    default:
      gcc_unreachable ();
    }
}

/* Divide Support

   If -O3 is used, we want to output a table lookup for
   divides between small numbers (both num and den >= 0
   and < 0x10).  The overhead of this method in the worst
   case is 40 bytes in the text section (10 insns) and
   256 bytes in the data section.  Additional divides do
   not incur additional penalties in the data section.

   Code speed is improved for small divides by about 5x
   when using this method in the worse case (~9 cycles
   vs ~45).  And in the worst case divides not within the
   table are penalized by about 10% (~5 cycles vs ~45).
   However in the typical case the penalty is not as bad
   because doing the long divide in only 45 cycles is
   quite optimistic.

   ??? would be nice to have some benchmarks other
   than Dhrystone to back this up.

   This bit of expansion is to create this instruction
   sequence as rtl.
        or      $8, $4, $5
        slli    $9, $4, 4
        cmpgeui $3, $8, 16
        beq     $3, $0, .L3
        or      $10, $9, $5
        add     $12, $11, divide_table
        ldbu    $2, 0($12)
        br      .L1
.L3:
        call    slow_div
.L1:
#       continue here with result in $2

   ??? Ideally I would like the libcall block to contain all
   of this code, but I don't know how to do that.  What it
   means is that if the divide can be eliminated, it may not
   completely disappear.

   ??? The __divsi3_table label should ideally be moved out
   of this block and into a global.  If it is placed into the
   sdata section we can save even more cycles by doing things
   gp relative.  */
void
nios2_emit_expensive_div (rtx *operands, enum machine_mode mode)
{
  rtx or_result, shift_left_result;
  rtx lookup_value;
  rtx lab1, lab3;
  rtx insns;
  rtx libfunc;
  rtx final_result;
  rtx tmp;
  rtx table;

  /* It may look a little generic, but only SImode is supported for now.  */
  gcc_assert (mode == SImode);
  libfunc = optab_libfunc (sdiv_optab, SImode);

  lab1 = gen_label_rtx ();
  lab3 = gen_label_rtx ();

  or_result = expand_simple_binop (SImode, IOR,
                                   operands[1], operands[2],
                                   0, 0, OPTAB_LIB_WIDEN);

  emit_cmp_and_jump_insns (or_result, GEN_INT (15), GTU, 0,
                           GET_MODE (or_result), 0, lab3);
  JUMP_LABEL (get_last_insn ()) = lab3;

  shift_left_result = expand_simple_binop (SImode, ASHIFT,
                                           operands[1], GEN_INT (4),
                                           0, 0, OPTAB_LIB_WIDEN);

  lookup_value = expand_simple_binop (SImode, IOR,
                                      shift_left_result, operands[2],
                                      0, 0, OPTAB_LIB_WIDEN);
  table = gen_rtx_PLUS (SImode, lookup_value,
			gen_rtx_SYMBOL_REF (SImode, "__divsi3_table"));
  convert_move (operands[0], gen_rtx_MEM (QImode, table), 1);

  tmp = emit_jump_insn (gen_jump (lab1));
  JUMP_LABEL (tmp) = lab1;
  emit_barrier ();

  emit_label (lab3);
  LABEL_NUSES (lab3) = 1;

  start_sequence ();
  final_result = emit_library_call_value (libfunc, NULL_RTX,
                                          LCT_CONST, SImode, 2,
                                          operands[1], SImode,
                                          operands[2], SImode);

  insns = get_insns ();
  end_sequence ();
  emit_libcall_block (insns, operands[0], final_result,
                      gen_rtx_DIV (SImode, operands[1], operands[2]));

  emit_label (lab1);
  LABEL_NUSES (lab1) = 1;
}


/* Branches and compares.  */

/* Return in *ALT_CODE and *ALT_OP, an alternate equivalent constant
   comparison, e.g. >= 1 into > 0.  */
static void
nios2_alternate_compare_const (enum rtx_code code, rtx op,
			       enum rtx_code *alt_code, rtx *alt_op,
			       enum machine_mode mode)
{
  HOST_WIDE_INT opval = INTVAL (op);
  enum rtx_code scode = signed_condition (code);
  bool dec_p = (scode == LT || scode == GE);

  if (code == EQ || code == NE)
    {
      *alt_code = code;
      *alt_op = op;
      return;
    }

  *alt_op = (dec_p
	     ? gen_int_mode (opval - 1, mode)
	     : gen_int_mode (opval + 1, mode));

  /* The required conversion between [>,>=] and [<,<=] is captured
     by a reverse + swap of condition codes.  */
  *alt_code = reverse_condition (swap_condition (code));

  {
    /* Test if the incremented/decremented value crosses the over/underflow
       boundary.  Supposedly, such boundary cases should already be transformed
       into always-true/false or EQ conditions, so use an assertion here.  */
    unsigned HOST_WIDE_INT alt_opval = INTVAL (*alt_op);
    if (code == scode)
      alt_opval ^= (1 << (GET_MODE_BITSIZE (mode) - 1));
    alt_opval &= GET_MODE_MASK (mode);
    gcc_assert (dec_p ? alt_opval != GET_MODE_MASK (mode) : alt_opval != 0);
  }
}

/* Return true if the constant comparison is supported by nios2.  */
static bool
nios2_valid_compare_const_p (enum rtx_code code, rtx op)
{
  switch (code)
    {
    case EQ: case NE: case GE: case LT:
      return SMALL_INT (INTVAL (op));
    case GEU: case LTU:
      return SMALL_INT_UNSIGNED (INTVAL (op));
    default:
      return false;
    }
}

/* Checks if the FPU comparison in *CMP, *OP1, and *OP2 can be supported in
   the current configuration.  Perform modifications if MODIFY_P is true.
   Returns true if FPU compare can be done.  */

bool
nios2_validate_fpu_compare (enum machine_mode mode, rtx *cmp, rtx *op1, rtx *op2,
			    bool modify_p)
{
  bool rev_p = false;
  enum rtx_code code = GET_CODE (*cmp);

  if (!nios2_fpu_compare_enabled (code, mode))
    {
      code = swap_condition (code);
      if (nios2_fpu_compare_enabled (code, mode))
	rev_p = true;
      else
	return false;
    }

  if (modify_p)
    {
      if (rev_p)
	{
	  rtx tmp = *op1;
	  *op1 = *op2;
	  *op2 = tmp;
	}
      *op1 = force_reg (mode, *op1);
      *op2 = force_reg (mode, *op2);
      *cmp = gen_rtx_fmt_ee (code, mode, *op1, *op2);
    }
  return true;
}

/* Checks and modifies the comparison in *CMP, *OP1, and *OP2 into valid
   nios2 supported form.  Returns true if success.  */
bool
nios2_validate_compare (enum machine_mode mode, rtx *cmp, rtx *op1, rtx *op2)
{
  enum rtx_code code = GET_CODE (*cmp);
  enum rtx_code alt_code;
  rtx alt_op2;

  if (GET_MODE_CLASS (mode) == MODE_FLOAT)
    return nios2_validate_fpu_compare (mode, cmp, op1, op2, true);

  if (!reg_or_0_operand (*op2, mode))
    {
      /* Create alternate constant compare.  */
      nios2_alternate_compare_const (code, *op2, &alt_code, &alt_op2, mode);

      /* If alterate op2 is zero(0), we can use it directly, possibly
	 swapping the compare code.  */
      if (alt_op2 == const0_rtx)
	{
	  code = alt_code;
	  *op2 = alt_op2;
	  goto check_rebuild_cmp;
	}

      /* Check if either constant compare can be used.  */
      if (nios2_valid_compare_const_p (code, *op2))
	return true;
      else if (nios2_valid_compare_const_p (alt_code, alt_op2))
	{
	  code = alt_code;
	  *op2 = alt_op2;
	  goto rebuild_cmp;
	}

      /* We have to force op2 into a register now.  Try to pick one
	 with a lower cost.  */
      if (! nios2_simple_const_p (*op2)
	  && nios2_simple_const_p (alt_op2))
	{
	  code = alt_code;
	  *op2 = alt_op2;
	}
      *op2 = force_reg (SImode, *op2);
    }
 check_rebuild_cmp:
  if (code == GT || code == GTU || code == LE || code == LEU)
    {
      rtx t = *op1; *op1 = *op2; *op2 = t;
      code = swap_condition (code);
    }
 rebuild_cmp:
  *cmp = gen_rtx_fmt_ee (code, mode, *op1, *op2);
  return true;
}


/* Addressing Modes.  */

/* Implement TARGET_LEGITIMATE_CONSTANT_P.  */
static bool
nios2_legitimate_constant_p (enum machine_mode mode ATTRIBUTE_UNUSED, rtx x)
{
  rtx base, offset;
  split_const (x, &base, &offset);
  return GET_CODE (base) != SYMBOL_REF || !SYMBOL_REF_TLS_MODEL (base);
}

/* Implement TARGET_CANNOT_FORCE_CONST_MEM.  */
static bool
nios2_cannot_force_const_mem (enum machine_mode mode ATTRIBUTE_UNUSED, rtx x)
{
  return nios2_legitimate_constant_p (mode, x) == false;
}

/* Return true if register REGNO is a valid base register.
   STRICT_P is true if REG_OK_STRICT is in effect.  */

bool
nios2_regno_ok_for_base_p (int regno, bool strict_p)
{
  if (!HARD_REGISTER_NUM_P (regno))
    {
      if (!strict_p)
	return true;

      if (!reg_renumber)
	return false;

      regno = reg_renumber[regno];
    }

  /* The fake registers will be eliminated to either the stack or
     hard frame pointer, both of which are usually valid base registers.
     Reload deals with the cases where the eliminated form isn't valid.  */
  return (GP_REG_P (regno)
	  || regno == FRAME_POINTER_REGNUM
	  || regno == ARG_POINTER_REGNUM);
}

/* Return true if the address expression formed by BASE + OFFSET is
   valid.  */
static bool
nios2_valid_addr_expr_p (rtx base, rtx offset, bool strict_p)
{
  if (!strict_p && GET_CODE (base) == SUBREG)
    base = SUBREG_REG (base);
  return (REG_P (base)
	  && nios2_regno_ok_for_base_p (REGNO (base), strict_p)
	  && (offset == NULL_RTX
	      || const_arith_operand (offset, Pmode)
	      || nios2_unspec_reloc_p (offset)));
}

/* Implement TARGET_LEGITIMATE_ADDRESS_P.  */
static bool
nios2_legitimate_address_p (enum machine_mode mode ATTRIBUTE_UNUSED,
			    rtx operand, bool strict_p)
{
  switch (GET_CODE (operand))
    {
      /* Direct.  */
    case SYMBOL_REF:
      if (SYMBOL_REF_TLS_MODEL (operand))
	return false;
      
      if (nios2_symbol_ref_in_small_data_p (operand))
	return true;

      /* Else, fall through.  */
    case LABEL_REF:
    case CONST_INT:
    case CONST:
    case CONST_DOUBLE:
      return false;

      /* Register indirect.  */
    case REG:
      return nios2_regno_ok_for_base_p (REGNO (operand), strict_p);

      /* Register indirect with displacement.  */
    case PLUS:
      {
        rtx op0 = XEXP (operand, 0);
        rtx op1 = XEXP (operand, 1);

	return (nios2_valid_addr_expr_p (op0, op1, strict_p)
		|| nios2_valid_addr_expr_p (op1, op0, strict_p));
      }

    default:
      break;
    }
  return false;
}

/* Return true if SECTION is a small section name.  */
static bool
nios2_small_section_name_p (const char *section)
{
  return (strcmp (section, ".sbss") == 0
	  || strncmp (section, ".sbss.", 6) == 0
	  || strcmp (section, ".sdata") == 0
	  || strncmp (section, ".sdata.", 7) == 0);
}

/* Return true if EXP should be placed in the small data section.  */
static bool
nios2_in_small_data_p (const_tree exp)
{
  /* We want to merge strings, so we never consider them small data.  */
  if (TREE_CODE (exp) == STRING_CST)
    return false;

  if (TREE_CODE (exp) == VAR_DECL)
    {
      if (DECL_SECTION_NAME (exp))
	{
	  const char *section = TREE_STRING_POINTER (DECL_SECTION_NAME (exp));
	  if (nios2_section_threshold > 0
	      && nios2_small_section_name_p (section))
	    return true;
	}
      else
	{
	  HOST_WIDE_INT size = int_size_in_bytes (TREE_TYPE (exp));

	  /* If this is an incomplete type with size 0, then we can't put it
	     in sdata because it might be too big when completed.  */
	  if (size > 0
	      && (unsigned HOST_WIDE_INT) size <= nios2_section_threshold)
	    return true;
	}
    }

  return false;
}

/* Return true if symbol is in small data section.  */

bool
nios2_symbol_ref_in_small_data_p (rtx sym)
{
  gcc_assert (GET_CODE (sym) == SYMBOL_REF);
  return
    (TARGET_GPOPT
     /* GP-relative access cannot be used for externally defined symbols,
	because the compilation unit that defines the symbol may place it
	in a section that cannot be reached from GP.  */
     && !SYMBOL_REF_EXTERNAL_P (sym)
     /* True if a symbol is both small and not weak.  */
     && SYMBOL_REF_SMALL_P (sym)
     && !(SYMBOL_REF_DECL (sym) && DECL_WEAK (SYMBOL_REF_DECL (sym)))
     /* TLS variables are not accessed through the GP.  */
     && SYMBOL_REF_TLS_MODEL (sym) == 0);

}

/* Implement TARGET_SECTION_TYPE_FLAGS.  */

static unsigned int
nios2_section_type_flags (tree decl, const char *name, int reloc)
{
  unsigned int flags;

  flags = default_section_type_flags (decl, name, reloc);

  if (nios2_small_section_name_p (name))
    flags |= SECTION_SMALL;

  return flags;
}


/* Position independent code related.  */

/* Emit code to load the PIC register.  */
static void
nios2_load_pic_register (void)
{
  rtx tmp = gen_rtx_REG (Pmode, TEMP_REG_NUM);

  emit_insn (gen_load_got_register (pic_offset_table_rtx, tmp));
  emit_insn (gen_add3_insn (pic_offset_table_rtx, pic_offset_table_rtx, tmp));
}

/* Generate a PIC address as a MEM rtx.  */
static rtx
nios2_load_pic_address (rtx sym, int unspec)
{
  rtx gotaddr = nios2_got_address (sym, unspec);
  return gen_const_mem (Pmode, gotaddr);
}

/* Nonzero if the constant value X is a legitimate general operand
   when generating PIC code.  It is given that flag_pic is on and
   that X satisfies CONSTANT_P or is a CONST_DOUBLE.  */
bool
nios2_legitimate_pic_operand_p (rtx x)
{
  return ! (GET_CODE (x) == SYMBOL_REF
	    || GET_CODE (x) == LABEL_REF || GET_CODE (x) == CONST);
}

/* Return TRUE if X is a thread-local symbol.  */
static bool
nios2_tls_symbol_p (rtx x)
{
  return (targetm.have_tls && GET_CODE (x) == SYMBOL_REF
	  && SYMBOL_REF_TLS_MODEL (x) != 0);
}

/* Legitimize addresses that are CONSTANT_P expressions.  */
static rtx
nios2_legitimize_constant_address (rtx addr)
{
  rtx base, offset;
  split_const (addr, &base, &offset);

  if (nios2_tls_symbol_p (base))
    base = nios2_legitimize_tls_address (base);
  else if (flag_pic)
    base = nios2_load_pic_address (base, UNSPEC_PIC_SYM);
  else
    return addr;

  if (offset != const0_rtx)
    {
      gcc_assert (can_create_pseudo_p ());
      return gen_rtx_PLUS (Pmode, force_reg (Pmode, base),
			   (CONST_INT_P (offset)
			    ? (SMALL_INT (INTVAL (offset))
			       ? offset : force_reg (Pmode, offset))
			    : offset));
    }
  return base;
}

/* Implement TARGET_LEGITIMIZE_ADDRESS.  */
static rtx
nios2_legitimize_address (rtx x, rtx oldx ATTRIBUTE_UNUSED,
			  enum machine_mode mode ATTRIBUTE_UNUSED)
{
  if (CONSTANT_P (x))
    return nios2_legitimize_constant_address (x);

  /* For the TLS LE (Local Exec) model, the compiler may try to
     combine constant offsets with unspec relocs, creating address RTXs
     looking like this:
     (plus:SI (reg:SI 23 r23)
              (const:SI
                (plus:SI
                  (unspec:SI [(symbol_ref:SI ("var"))] UNSPEC_ADD_TLS_LE)
                  (const_int 48 [0x30]))))

     This usually happens when 'var' is a thread-local struct variable,
     and access of a field in var causes the addend.

     We typically want this combining, so transform the above into this
     form, which is allowed:
     (plus:SI (reg:SI 23 r23)
              (const:SI
                (unspec:SI
                  [(const:SI
                     (plus:SI (symbol_ref:SI ("var"))
                              (const_int 48 [0x30])))] UNSPEC_ADD_TLS_LE)))

     Which will be output as '%tls_le(var+48)(r23)' in assembly.  */
  if (GET_CODE (x) == PLUS
      && GET_CODE (XEXP (x, 0)) == REG
      && GET_CODE (XEXP (x, 1)) == CONST)
    {
      rtx unspec, offset, reg = XEXP (x, 0);
      split_const (XEXP (x, 1), &unspec, &offset);
      if (GET_CODE (unspec) == UNSPEC
	  && nios2_unspec_reloc_name (XINT (unspec, 1)) != NULL
	  && offset != const0_rtx)
	{
	  unspec = copy_rtx (unspec);
	  XVECEXP (unspec, 0, 0)
	    = plus_constant (Pmode, XVECEXP (unspec, 0, 0), INTVAL (offset));
	  x = gen_rtx_PLUS (Pmode, reg, gen_rtx_CONST (Pmode, unspec));
	}
    }

  return x;
}

/* Main expander function for RTL moves.  */
int
nios2_emit_move_sequence (rtx *operands, enum machine_mode mode)
{
  rtx to = operands[0];
  rtx from = operands[1];

  if (!register_operand (to, mode) && !reg_or_0_operand (from, mode))
    {
      gcc_assert (can_create_pseudo_p ());
      from = copy_to_mode_reg (mode, from);
    }

  if (GET_CODE (from) == SYMBOL_REF || GET_CODE (from) == LABEL_REF
      || GET_CODE (from) == CONST)
    from = nios2_legitimize_constant_address (from);

  operands[0] = to;
  operands[1] = from;
  return 0;
}

/* The function with address *ADDR is being called.  If the address
   needs to be loaded from the GOT, emit the instruction to do so and
   update *ADDR to point to the rtx for the loaded value.  */
void
nios2_adjust_call_address (rtx *call_op)
{
  rtx addr;
  gcc_assert (MEM_P (*call_op));
  addr = XEXP (*call_op, 0);
  if (flag_pic && CONSTANT_P (addr))
    {
      rtx reg = gen_reg_rtx (Pmode);
      emit_move_insn (reg, nios2_load_pic_address (addr, UNSPEC_PIC_CALL_SYM));
      XEXP (*call_op, 0) = reg;
    }
}


/* Output assembly language related definitions.  */

/* Print the operand OP to file stream FILE modified by LETTER.
   LETTER can be one of:

     i: print "i" if OP is an immediate, except 0
     o: print "io" if OP is volatile
     z: for const0_rtx print $0 instead of 0
     H: for %hiadj
     L: for %lo
     U: for upper half of 32 bit value
     D: for the upper 32-bits of a 64-bit double value
     R: prints reverse condition.
*/
static void
nios2_print_operand (FILE *file, rtx op, int letter)
{

  switch (letter)
    {
    case 'i':
      if (CONSTANT_P (op) && op != const0_rtx)
        fprintf (file, "i");
      return;

    case 'o':
      if (GET_CODE (op) == MEM
	  && ((MEM_VOLATILE_P (op) && TARGET_BYPASS_CACHE_VOLATILE)
	      || TARGET_BYPASS_CACHE))
        fprintf (file, "io");
      return;

    default:
      break;
    }

  if (comparison_operator (op, VOIDmode))
    {
      enum rtx_code cond = GET_CODE (op);
      if (letter == 0)
	{
	  fprintf (file, "%s", GET_RTX_NAME (cond));
	  return;
	}
      if (letter == 'R')
	{
	  fprintf (file, "%s", GET_RTX_NAME (reverse_condition (cond)));
	  return;
	}
    }

  switch (GET_CODE (op))
    {
    case REG:
      if (letter == 0 || letter == 'z')
        {
          fprintf (file, "%s", reg_names[REGNO (op)]);
          return;
        }
      else if (letter == 'D')
        {
          fprintf (file, "%s", reg_names[REGNO (op)+1]);
          return;
        }
      break;

    case CONST_INT:
      if (INTVAL (op) == 0 && letter == 'z')
        {
          fprintf (file, "zero");
          return;
        }

      if (letter == 'U')
        {
          HOST_WIDE_INT val = INTVAL (op);
	  val = (val >> 16) & 0xFFFF;
	  output_addr_const (file, gen_int_mode (val, SImode));
          return;
        }
      /* Else, fall through.  */

    case CONST:
    case LABEL_REF:
    case SYMBOL_REF:
    case CONST_DOUBLE:
      if (letter == 0 || letter == 'z')
        {
          output_addr_const (file, op);
          return;
        }
      else if (letter == 'H')
        {
          fprintf (file, "%%hiadj(");
          output_addr_const (file, op);
          fprintf (file, ")");
          return;
        }
      else if (letter == 'L')
        {
          fprintf (file, "%%lo(");
          output_addr_const (file, op);
          fprintf (file, ")");
          return;
        }
      break;

    case SUBREG:
    case MEM:
      if (letter == 0)
        {
          output_address (op);
          return;
        }
      break;

    case CODE_LABEL:
      if (letter == 0)
        {
          output_addr_const (file, op);
          return;
        }
      break;

    default:
      break;
    }

  output_operand_lossage ("Unsupported operand for code '%c'", letter);
  gcc_unreachable ();
}

/* Return true if this is a GP-relative accessible reference.  */
static bool
gprel_constant_p (rtx op)
{
  if (GET_CODE (op) == SYMBOL_REF
      && nios2_symbol_ref_in_small_data_p (op))
    return true;
  else if (GET_CODE (op) == CONST
           && GET_CODE (XEXP (op, 0)) == PLUS)
    return gprel_constant_p (XEXP (XEXP (op, 0), 0));

  return false;
}

/* Return the name string for a supported unspec reloc offset.  */
static const char *
nios2_unspec_reloc_name (int unspec)
{
  switch (unspec)
    {
    case UNSPEC_PIC_SYM:
      return "got";
    case UNSPEC_PIC_CALL_SYM:
      return "call";
    case UNSPEC_LOAD_TLS_IE:
      return "tls_ie";
    case UNSPEC_ADD_TLS_LE:
      return "tls_le";
    case UNSPEC_ADD_TLS_GD:
      return "tls_gd";
    case UNSPEC_ADD_TLS_LDM:
      return "tls_ldm";
    case UNSPEC_ADD_TLS_LDO:
      return "tls_ldo";
    default:
      return NULL;
    }
}

/* Return true for conforming unspec relocations.  Also used in
   constraints.md and predicates.md.  */
bool
nios2_unspec_reloc_p (rtx op)
{
  return (GET_CODE (op) == CONST
	  && GET_CODE (XEXP (op, 0)) == UNSPEC
	  && nios2_unspec_reloc_name (XINT (XEXP (op, 0), 1)) != NULL);
}

/* Implement TARGET_ASM_OUTPUT_ADDR_CONST_EXTRA.  */
static bool
nios2_output_addr_const_extra (FILE *file, rtx op)
{
  const char *name;
  gcc_assert (GET_CODE (op) == UNSPEC);

  /* Support for printing out const unspec relocations.  */
  name = nios2_unspec_reloc_name (XINT (op, 1));
  if (name)
    {
      fprintf (file, "%%%s(", name);
      output_addr_const (file, XVECEXP (op, 0, 0));
      fprintf (file, ")");
      return true;
    }
  return false;
}

/* Implement TARGET_PRINT_OPERAND_ADDRESS.  */
static void
nios2_print_operand_address (FILE *file, rtx op)
{
  switch (GET_CODE (op))
    {
    case CONST:
    case CONST_INT:
    case LABEL_REF:
    case CONST_DOUBLE:
    case SYMBOL_REF:
      if (gprel_constant_p (op))
        {
          fprintf (file, "%%gprel(");
          output_addr_const (file, op);
          fprintf (file, ")(%s)", reg_names[GP_REGNO]);
          return;
        }

      break;

    case PLUS:
      {
        rtx op0 = XEXP (op, 0);
        rtx op1 = XEXP (op, 1);

        if (REG_P (op0) && CONSTANT_P (op1))
          {
            output_addr_const (file, op1);
            fprintf (file, "(%s)", reg_names[REGNO (op0)]);
            return;
          }
        else if (REG_P (op1) && CONSTANT_P (op0))
          {
            output_addr_const (file, op0);
            fprintf (file, "(%s)", reg_names[REGNO (op1)]);
            return;
          }
      }
      break;

    case REG:
      fprintf (file, "0(%s)", reg_names[REGNO (op)]);
      return;

    case MEM:
      {
        rtx base = XEXP (op, 0);
        nios2_print_operand_address (file, base);
        return;
      }
    default:
      break;
    }

  fprintf (stderr, "Missing way to print address\n");
  debug_rtx (op);
  gcc_unreachable ();
}

/* Implement TARGET_ASM_OUTPUT_DWARF_DTPREL.  */
static void
nios2_output_dwarf_dtprel (FILE *file, int size, rtx x)
{
  gcc_assert (size == 4);
  fprintf (file, "\t.4byte\t%%tls_ldo(");
  output_addr_const (file, x);
  fprintf (file, ")");
}

/* Implement TARGET_ASM_FUNCTION_PROLOGUE.  */
static void
nios2_asm_function_prologue (FILE *file, HOST_WIDE_INT size ATTRIBUTE_UNUSED)
{
  if (flag_verbose_asm || flag_debug_asm)
    {
      nios2_compute_frame_layout ();
      nios2_dump_frame_layout (file);
    }
}

/* Emit assembly of custom FPU instructions.  */
const char *
nios2_fpu_insn_asm (enum n2fpu_code code)
{
  static char buf[256];
  const char *op1, *op2, *op3;
  int ln = 256, n = 0;
  
  int N = N2FPU_N (code);
  int num_operands = N2FPU (code).num_operands;
  const char *insn_name = N2FPU_NAME (code);
  tree ftype = nios2_ftype (N2FPU_FTCODE (code));
  enum machine_mode dst_mode = TYPE_MODE (TREE_TYPE (ftype));
  enum machine_mode src_mode = TYPE_MODE (TREE_VALUE (TYPE_ARG_TYPES (ftype)));

  /* Prepare X register for DF input operands.  */
  if (GET_MODE_SIZE (src_mode) == 8 && num_operands == 3)
    n = snprintf (buf, ln, "custom\t%d, zero, %%1, %%D1 # fwrx %%1\n\t",
		  N2FPU_N (n2fpu_fwrx));

  if (src_mode == SFmode)
    {
      if (dst_mode == VOIDmode)
	{
	  /* The fwry case.  */
	  op1 = op3 = "zero";
	  op2 = "%0";
	  num_operands -= 1;
	}
      else
	{
	  op1 = (dst_mode == DFmode ? "%D0" : "%0");
	  op2 = "%1";
	  op3 = (num_operands == 2 ? "zero" : "%2");
	}
    }
  else if (src_mode == DFmode)
    {
      if (dst_mode == VOIDmode)
	{
	  /* The fwrx case.  */
	  op1 = "zero";
	  op2 = "%0";
	  op3 = "%D0";
	  num_operands -= 1;
	}
      else
	{
	  op1 = (dst_mode == DFmode ? "%D0" : "%0");
	  op2 = (num_operands == 2 ? "%1" : "%2");
	  op3 = (num_operands == 2 ? "%D1" : "%D2");
	}
    }
  else if (src_mode == VOIDmode)
    {
      /* frdxlo, frdxhi, frdy cases.  */
      gcc_assert (dst_mode == SFmode);
      op1 = "%0";
      op2 = op3 = "zero";
    }
  else if (src_mode == SImode)
    {
      /* Conversion operators.  */
      gcc_assert (num_operands == 2);
      op1 = (dst_mode == DFmode ? "%D0" : "%0");
      op2 = "%1";
      op3 = "zero";
    }
  else
    gcc_unreachable ();

  /* Main instruction string.  */
  n += snprintf (buf + n, ln - n, "custom\t%d, %s, %s, %s # %s %%0%s%s",
		 N, op1, op2, op3, insn_name,
		 (num_operands >= 2 ? ", %1" : ""),
		 (num_operands == 3 ? ", %2" : ""));

  /* Extraction of Y register for DF results.  */
  if (dst_mode == DFmode)
    snprintf (buf + n, ln - n, "\n\tcustom\t%d, %%0, zero, zero # frdy %%0",
	      N2FPU_N (n2fpu_frdy));
  return buf;
}



/* Function argument related.  */

/* Define where to put the arguments to a function.  Value is zero to
   push the argument on the stack, or a hard register in which to
   store the argument.

   MODE is the argument's machine mode.
   TYPE is the data type of the argument (as a tree).
   This is null for libcalls where that information may
   not be available.
   CUM is a variable of type CUMULATIVE_ARGS which gives info about
   the preceding args and about the function being called.
   NAMED is nonzero if this argument is a named parameter
   (otherwise it is an extra parameter matching an ellipsis).  */

static rtx
nios2_function_arg (cumulative_args_t cum_v, enum machine_mode mode,
		    const_tree type ATTRIBUTE_UNUSED,
		    bool named ATTRIBUTE_UNUSED)
{
  CUMULATIVE_ARGS *cum = get_cumulative_args (cum_v); 
  rtx return_rtx = NULL_RTX;

  if (cum->regs_used < NUM_ARG_REGS)
    return_rtx = gen_rtx_REG (mode, FIRST_ARG_REGNO + cum->regs_used);

  return return_rtx;
}

/* Return number of bytes, at the beginning of the argument, that must be
   put in registers.  0 is the argument is entirely in registers or entirely
   in memory.  */

static int
nios2_arg_partial_bytes (cumulative_args_t cum_v,
                         enum machine_mode mode, tree type ATTRIBUTE_UNUSED,
                         bool named ATTRIBUTE_UNUSED)
{
  CUMULATIVE_ARGS *cum = get_cumulative_args (cum_v); 
  HOST_WIDE_INT param_size;

  if (mode == BLKmode)
    {
      param_size = int_size_in_bytes (type);
      gcc_assert (param_size >= 0);
    }
  else
    param_size = GET_MODE_SIZE (mode);

  /* Convert to words (round up).  */
  param_size = (UNITS_PER_WORD - 1 + param_size) / UNITS_PER_WORD;

  if (cum->regs_used < NUM_ARG_REGS
      && cum->regs_used + param_size > NUM_ARG_REGS)
    return (NUM_ARG_REGS - cum->regs_used) * UNITS_PER_WORD;

  return 0;
}

/* Update the data in CUM to advance over an argument of mode MODE
   and data type TYPE; TYPE is null for libcalls where that information
   may not be available.  */

static void
nios2_function_arg_advance (cumulative_args_t cum_v, enum machine_mode mode,
			    const_tree type ATTRIBUTE_UNUSED,
			    bool named ATTRIBUTE_UNUSED)
{
  CUMULATIVE_ARGS *cum = get_cumulative_args (cum_v); 
  HOST_WIDE_INT param_size;

  if (mode == BLKmode)
    {
      param_size = int_size_in_bytes (type);
      gcc_assert (param_size >= 0);
    }
  else
    param_size = GET_MODE_SIZE (mode);

  /* Convert to words (round up).  */
  param_size = (UNITS_PER_WORD - 1 + param_size) / UNITS_PER_WORD;

  if (cum->regs_used + param_size > NUM_ARG_REGS)
    cum->regs_used = NUM_ARG_REGS;
  else
    cum->regs_used += param_size;
}

enum direction
nios2_function_arg_padding (enum machine_mode mode, const_tree type)
{
  /* On little-endian targets, the first byte of every stack argument
     is passed in the first byte of the stack slot.  */
  if (!BYTES_BIG_ENDIAN)
    return upward;

  /* Otherwise, integral types are padded downward: the last byte of a
     stack argument is passed in the last byte of the stack slot.  */
  if (type != 0
      ? INTEGRAL_TYPE_P (type) || POINTER_TYPE_P (type)
      : GET_MODE_CLASS (mode) == MODE_INT)
    return downward;

  /* Arguments smaller than a stack slot are padded downward.  */
  if (mode != BLKmode)
    return (GET_MODE_BITSIZE (mode) >= PARM_BOUNDARY) ? upward : downward;

  return ((int_size_in_bytes (type) >= (PARM_BOUNDARY / BITS_PER_UNIT))
	  ? upward : downward);
}

enum direction
nios2_block_reg_padding (enum machine_mode mode, tree type,
                         int first ATTRIBUTE_UNUSED)
{
  return nios2_function_arg_padding (mode, type);
}

/* Emit RTL insns to initialize the variable parts of a trampoline.
   FNADDR is an RTX for the address of the function's pure code.
   CXT is an RTX for the static chain value for the function.
   On Nios II, we handle this by a library call.  */
static void
nios2_trampoline_init (rtx m_tramp, tree fndecl, rtx cxt)
{
  rtx fnaddr = XEXP (DECL_RTL (fndecl), 0);
  rtx ctx_reg = force_reg (Pmode, cxt);
  rtx addr = force_reg (Pmode, XEXP (m_tramp, 0));

  emit_library_call (gen_rtx_SYMBOL_REF (Pmode, "__trampoline_setup"),
		     LCT_NORMAL, VOIDmode, 3, addr, Pmode, fnaddr, Pmode,
		     ctx_reg, Pmode);
}

/* Implement TARGET_FUNCTION_VALUE.  */
static rtx
nios2_function_value (const_tree ret_type, const_tree fn ATTRIBUTE_UNUSED,
		      bool outgoing ATTRIBUTE_UNUSED)
{
  return gen_rtx_REG (TYPE_MODE (ret_type), FIRST_RETVAL_REGNO);
}

/* Implement TARGET_LIBCALL_VALUE.  */
static rtx
nios2_libcall_value (enum machine_mode mode, const_rtx fun ATTRIBUTE_UNUSED)
{
  return gen_rtx_REG (mode, FIRST_RETVAL_REGNO);
}

/* Implement TARGET_FUNCTION_VALUE_REGNO_P.  */
static bool
nios2_function_value_regno_p (const unsigned int regno)
{
  return regno == FIRST_RETVAL_REGNO;
}

/* Implement TARGET_RETURN_IN_MEMORY.  */
static bool
nios2_return_in_memory (const_tree type, const_tree fntype ATTRIBUTE_UNUSED)
{
  return (int_size_in_bytes (type) > (2 * UNITS_PER_WORD)
	  || int_size_in_bytes (type) == -1);
}

/* TODO: It may be possible to eliminate the copyback and implement
   own va_arg type.  */
static void
nios2_setup_incoming_varargs (cumulative_args_t cum_v,
                              enum machine_mode mode, tree type,
                              int *pretend_size, int second_time)
{
  CUMULATIVE_ARGS *cum = get_cumulative_args (cum_v); 
  CUMULATIVE_ARGS local_cum;
  cumulative_args_t local_cum_v = pack_cumulative_args (&local_cum);
  int regs_to_push;
  int pret_size;

  local_cum = *cum;
  nios2_function_arg_advance (local_cum_v, mode, type, 1);

  regs_to_push = NUM_ARG_REGS - local_cum.regs_used;

  if (!second_time && regs_to_push > 0)
    {
      rtx ptr = virtual_incoming_args_rtx;
      rtx mem = gen_rtx_MEM (BLKmode, ptr);
      emit_insn (gen_blockage ());
      move_block_from_reg (local_cum.regs_used + FIRST_ARG_REGNO, mem,
			   regs_to_push);
      emit_insn (gen_blockage ());
    }

  pret_size = regs_to_push * UNITS_PER_WORD;
  if (pret_size)
    *pretend_size = pret_size;
}



/* Init FPU builtins.  */
static void
nios2_init_fpu_builtins (int start_code)
{
  tree fndecl;
  char builtin_name[64] = "__builtin_custom_";
  unsigned int i, n = strlen ("__builtin_custom_");

  for (i = 0; i < ARRAY_SIZE (nios2_fpu_insn); i++)
    {
      snprintf (builtin_name + n, sizeof (builtin_name) - n,
		"%s", N2FPU_NAME (i));
      fndecl =
	add_builtin_function (builtin_name, nios2_ftype (N2FPU_FTCODE (i)),
			      start_code + i, BUILT_IN_MD, NULL, NULL_TREE);
      nios2_register_builtin_fndecl (start_code + i, fndecl);
    }
}

/* Helper function for expanding FPU builtins.  */
static rtx
nios2_expand_fpu_builtin (tree exp, unsigned int code, rtx target)
{
  struct expand_operand ops[MAX_RECOG_OPERANDS];
  enum insn_code icode = N2FPU_ICODE (code);
  int nargs, argno, opno = 0;
  int num_operands = N2FPU (code).num_operands;
  enum machine_mode dst_mode = TYPE_MODE (TREE_TYPE (exp));
  bool has_target_p = (dst_mode != VOIDmode);

  if (N2FPU_N (code) < 0)
    fatal_error ("Cannot call %<__builtin_custom_%s%> without specifying switch"
		 " %<-mcustom-%s%>", N2FPU_NAME (code), N2FPU_NAME (code));
  if (has_target_p)
    create_output_operand (&ops[opno++], target, dst_mode);
  else
    /* Subtract away the count of the VOID return, mainly for fwrx/fwry.   */
    num_operands -= 1;
  nargs = call_expr_nargs (exp);
  for (argno = 0; argno < nargs; argno++)
    {
      tree arg = CALL_EXPR_ARG (exp, argno);
      create_input_operand (&ops[opno++], expand_normal (arg),
			    TYPE_MODE (TREE_TYPE (arg)));
    }
  if (!maybe_expand_insn (icode, num_operands, ops))
    {
      error ("invalid argument to built-in function");
      return has_target_p ? gen_reg_rtx (ops[0].mode) : const0_rtx;
    }
  return has_target_p ? ops[0].value : const0_rtx;
}

/* Nios II has custom instruction built-in functions of the forms:
   __builtin_custom_n
   __builtin_custom_nX
   __builtin_custom_nXX
   __builtin_custom_Xn
   __builtin_custom_XnX
   __builtin_custom_XnXX

   where each X could be either 'i' (int), 'f' (float), or 'p' (void*).
   Therefore with 0-1 return values, and 0-2 arguments, we have a
   total of (3 + 1) * (1 + 3 + 9) == 52 custom builtin functions.
*/
#define NUM_CUSTOM_BUILTINS ((3 + 1) * (1 + 3 + 9))
static char custom_builtin_name[NUM_CUSTOM_BUILTINS][5];

static void
nios2_init_custom_builtins (int start_code)
{
  tree builtin_ftype, ret_type, fndecl;
  char builtin_name[32] = "__builtin_custom_";
  int n = strlen ("__builtin_custom_");
  int builtin_code = 0;
  int lhs, rhs1, rhs2;

  struct { tree type; const char *c; } op[4];
  /* z */ op[0].c = "";  op[0].type = NULL_TREE;
  /* f */ op[1].c = "f"; op[1].type = float_type_node;
  /* i */ op[2].c = "i"; op[2].type = integer_type_node;
  /* p */ op[3].c = "p"; op[3].type = ptr_type_node;

  /* We enumerate through the possible operand types to create all the
     __builtin_custom_XnXX function tree types.  Note that these may slightly
     overlap with the function types created for other fixed builtins.  */

  for (lhs = 0; lhs < 4; lhs++)
    for (rhs1 = 0; rhs1 < 4; rhs1++)
      for (rhs2 = 0; rhs2 < 4; rhs2++)
	{
	  if (rhs1 == 0 && rhs2 != 0)
	    continue;
	  ret_type = (op[lhs].type ? op[lhs].type : void_type_node);
	  builtin_ftype
	    = build_function_type_list (ret_type, integer_type_node,
					op[rhs1].type, op[rhs2].type,
					NULL_TREE);
	  snprintf (builtin_name + n, 32 - n, "%sn%s%s",
		    op[lhs].c, op[rhs1].c, op[rhs2].c);
	  /* Save copy of parameter string into custom_builtin_name[].  */
	  strncpy (custom_builtin_name[builtin_code], builtin_name + n, 5);
	  fndecl =
	    add_builtin_function (builtin_name, builtin_ftype,
				  start_code + builtin_code,
				  BUILT_IN_MD, NULL, NULL_TREE);
	  nios2_register_builtin_fndecl (start_code + builtin_code, fndecl);
	  builtin_code += 1;
	}
}

/* Helper function for expanding custom builtins.  */
static rtx
nios2_expand_custom_builtin (tree exp, unsigned int index, rtx target)
{
  bool has_target_p = (TREE_TYPE (exp) != void_type_node);
  enum machine_mode tmode = VOIDmode;
  int nargs, argno;
  rtx value, insn, unspec_args[3];
  tree arg;

  /* XnXX form.  */
  if (has_target_p)
    {
      tmode = TYPE_MODE (TREE_TYPE (exp));
      if (!target || GET_MODE (target) != tmode
	  || !REG_P (target))
	target = gen_reg_rtx (tmode);
    }

  nargs = call_expr_nargs (exp);
  for (argno = 0; argno < nargs; argno++)
    {
      arg = CALL_EXPR_ARG (exp, argno);
      value = expand_normal (arg);
      unspec_args[argno] = value;
      if (argno == 0)
	{
	  if (!custom_insn_opcode (value, VOIDmode))
	    error ("custom instruction opcode must be compile time "
		   "constant in the range 0-255 for __builtin_custom_%s",
		   custom_builtin_name[index]);
	}
      else
	/* For other arguments, force into a register.  */
	unspec_args[argno] = force_reg (TYPE_MODE (TREE_TYPE (arg)),
					unspec_args[argno]);
    }
  /* Fill remaining unspec operands with zero.  */
  for (; argno < 3; argno++)
    unspec_args[argno] = const0_rtx;

  insn = (has_target_p
	  ? gen_rtx_SET (VOIDmode, target,
			 gen_rtx_UNSPEC_VOLATILE (tmode,
						  gen_rtvec_v (3, unspec_args),
						  UNSPECV_CUSTOM_XNXX))
	  : gen_rtx_UNSPEC_VOLATILE (VOIDmode, gen_rtvec_v (3, unspec_args),
				     UNSPECV_CUSTOM_NXX));
  emit_insn (insn);
  return has_target_p ? target : const0_rtx;
}




/* Main definition of built-in functions.  Nios II has a small number of fixed
   builtins, plus a large number of FPU insn builtins, and builtins for
   generating custom instructions.  */

struct nios2_builtin_desc
{
  enum insn_code icode;
  enum nios2_ftcode ftype;
  const char *name;
};

#define N2_BUILTINS					\
  N2_BUILTIN_DEF (sync,   N2_FTYPE_VOID_VOID)		\
  N2_BUILTIN_DEF (ldbio,  N2_FTYPE_SI_CVPTR)		\
  N2_BUILTIN_DEF (ldbuio, N2_FTYPE_UI_CVPTR)		\
  N2_BUILTIN_DEF (ldhio,  N2_FTYPE_SI_CVPTR)		\
  N2_BUILTIN_DEF (ldhuio, N2_FTYPE_UI_CVPTR)		\
  N2_BUILTIN_DEF (ldwio,  N2_FTYPE_SI_CVPTR)		\
  N2_BUILTIN_DEF (stbio,  N2_FTYPE_VOID_VPTR_SI)	\
  N2_BUILTIN_DEF (sthio,  N2_FTYPE_VOID_VPTR_SI)	\
  N2_BUILTIN_DEF (stwio,  N2_FTYPE_VOID_VPTR_SI)	\
  N2_BUILTIN_DEF (rdctl,  N2_FTYPE_SI_SI)		\
  N2_BUILTIN_DEF (wrctl,  N2_FTYPE_VOID_SI_SI)

enum nios2_builtin_code {
#define N2_BUILTIN_DEF(name, ftype) NIOS2_BUILTIN_ ## name,
  N2_BUILTINS
#undef N2_BUILTIN_DEF
  NUM_FIXED_NIOS2_BUILTINS
};

static const struct nios2_builtin_desc nios2_builtins[] = {
#define N2_BUILTIN_DEF(name, ftype)			\
  { CODE_FOR_ ## name, ftype, "__builtin_" #name },
  N2_BUILTINS
#undef N2_BUILTIN_DEF
};

/* Start/ends of FPU/custom insn builtin index ranges.  */
static unsigned int nios2_fpu_builtin_base;
static unsigned int nios2_custom_builtin_base;
static unsigned int nios2_custom_builtin_end;

/* Implement TARGET_INIT_BUILTINS.  */
static void
nios2_init_builtins (void)
{
  unsigned int i;

  /* Initialize fixed builtins.  */
  for (i = 0; i < ARRAY_SIZE (nios2_builtins); i++)
    {
      const struct nios2_builtin_desc *d = &nios2_builtins[i];
      tree fndecl =
	add_builtin_function (d->name, nios2_ftype (d->ftype), i,
			      BUILT_IN_MD, NULL, NULL);
      nios2_register_builtin_fndecl (i, fndecl);
    }

  /* Initialize FPU builtins.  */
  nios2_fpu_builtin_base = ARRAY_SIZE (nios2_builtins);
  nios2_init_fpu_builtins (nios2_fpu_builtin_base);

  /* Initialize custom insn builtins.  */
  nios2_custom_builtin_base
    = nios2_fpu_builtin_base + ARRAY_SIZE (nios2_fpu_insn);
  nios2_custom_builtin_end
    = nios2_custom_builtin_base + NUM_CUSTOM_BUILTINS;
  nios2_init_custom_builtins (nios2_custom_builtin_base);
}

/* Array of fndecls for TARGET_BUILTIN_DECL.  */
#define NIOS2_NUM_BUILTINS \
  (ARRAY_SIZE (nios2_builtins) + ARRAY_SIZE (nios2_fpu_insn) + NUM_CUSTOM_BUILTINS)
static GTY(()) tree nios2_builtin_decls[NIOS2_NUM_BUILTINS];

static void
nios2_register_builtin_fndecl (unsigned code, tree fndecl)
{
  nios2_builtin_decls[code] = fndecl;
}

/* Implement TARGET_BUILTIN_DECL.  */
static tree
nios2_builtin_decl (unsigned code, bool initialize_p ATTRIBUTE_UNUSED)
{
  gcc_assert (nios2_custom_builtin_end == ARRAY_SIZE (nios2_builtin_decls));

  if (code >= nios2_custom_builtin_end)
    return error_mark_node;

  if (code >= nios2_fpu_builtin_base
      && code < nios2_custom_builtin_base
      && ! N2FPU_ENABLED_P (code - nios2_fpu_builtin_base))
    return error_mark_node;

  return nios2_builtin_decls[code];
}


/* Low-level built-in expand routine.  */
static rtx
nios2_expand_builtin_insn (const struct nios2_builtin_desc *d, int n,
			   struct expand_operand *ops, bool has_target_p)
{
  if (maybe_expand_insn (d->icode, n, ops))
    return has_target_p ? ops[0].value : const0_rtx;
  else
    {
      error ("invalid argument to built-in function %s", d->name);
      return has_target_p ? gen_reg_rtx (ops[0].mode) : const0_rtx;	  
    } 
}

/* Expand ldio/stio form load-store instruction builtins.  */
static rtx
nios2_expand_ldstio_builtin (tree exp, rtx target,
			     const struct nios2_builtin_desc *d)
{
  bool has_target_p;
  rtx addr, mem, val;
  struct expand_operand ops[MAX_RECOG_OPERANDS];
  enum machine_mode mode = insn_data[d->icode].operand[0].mode;

  addr = expand_normal (CALL_EXPR_ARG (exp, 0));
  mem = gen_rtx_MEM (mode, addr);

  if (insn_data[d->icode].operand[0].allows_mem)
    {
      /* stxio.  */
      val = expand_normal (CALL_EXPR_ARG (exp, 1));
      if (CONST_INT_P (val))
	val = force_reg (mode, gen_int_mode (INTVAL (val), mode));
      val = simplify_gen_subreg (mode, val, GET_MODE (val), 0);
      create_output_operand (&ops[0], mem, mode);
      create_input_operand (&ops[1], val, mode);
      has_target_p = false;
    }
  else
    {
      /* ldxio.  */
      create_output_operand (&ops[0], target, mode);
      create_input_operand (&ops[1], mem, mode);
      has_target_p = true;
    }
  return nios2_expand_builtin_insn (d, 2, ops, has_target_p);
}

/* Expand rdctl/wrctl builtins.  */
static rtx
nios2_expand_rdwrctl_builtin (tree exp, rtx target,
			     const struct nios2_builtin_desc *d)
{
  bool has_target_p = (insn_data[d->icode].operand[0].predicate
		       == register_operand);
  rtx ctlcode = expand_normal (CALL_EXPR_ARG (exp, 0));
  struct expand_operand ops[MAX_RECOG_OPERANDS];
  if (!rdwrctl_operand (ctlcode, VOIDmode))
    {
      error ("Control register number must be in range 0-31 for %s",
	     d->name);
      return has_target_p ? gen_reg_rtx (SImode) : const0_rtx;
    }
  if (has_target_p)
    {
      create_output_operand (&ops[0], target, SImode);
      create_integer_operand (&ops[1], INTVAL (ctlcode));
    }
  else
    {
      rtx val = expand_normal (CALL_EXPR_ARG (exp, 1));
      create_integer_operand (&ops[0], INTVAL (ctlcode));
      create_input_operand (&ops[1], val, SImode);
    }
  return nios2_expand_builtin_insn (d, 2, ops, has_target_p);
}

/* Implement TARGET_EXPAND_BUILTIN.  Expand an expression EXP that calls
   a built-in function, with result going to TARGET if that's convenient
   (and in mode MODE if that's convenient).
   SUBTARGET may be used as the target for computing one of EXP's operands.
   IGNORE is nonzero if the value is to be ignored.  */

static rtx
nios2_expand_builtin (tree exp, rtx target, rtx subtarget ATTRIBUTE_UNUSED,
                      enum machine_mode mode ATTRIBUTE_UNUSED,
		      int ignore ATTRIBUTE_UNUSED)
{
  tree fndecl = TREE_OPERAND (CALL_EXPR_FN (exp), 0);
  unsigned int fcode = DECL_FUNCTION_CODE (fndecl);

  if (fcode < nios2_fpu_builtin_base)
    {
      const struct nios2_builtin_desc *d = &nios2_builtins[fcode];

      switch (fcode)
	{
	case NIOS2_BUILTIN_sync:
	  emit_insn (gen_sync ());
	  return const0_rtx;

	case NIOS2_BUILTIN_ldbio:
	case NIOS2_BUILTIN_ldbuio:
	case NIOS2_BUILTIN_ldhio:
	case NIOS2_BUILTIN_ldhuio:
	case NIOS2_BUILTIN_ldwio:
	case NIOS2_BUILTIN_stbio:
	case NIOS2_BUILTIN_sthio:
	case NIOS2_BUILTIN_stwio:
	  return nios2_expand_ldstio_builtin (exp, target, d);

	case NIOS2_BUILTIN_rdctl:
	case NIOS2_BUILTIN_wrctl:
	  return nios2_expand_rdwrctl_builtin (exp, target, d);

	default:
	  gcc_unreachable ();
	}
    }
  else if (fcode < nios2_custom_builtin_base)
    /* FPU builtin range.  */
    return nios2_expand_fpu_builtin (exp, fcode - nios2_fpu_builtin_base,
				     target);
  else if (fcode < nios2_custom_builtin_end)
    /* Custom insn builtin range.  */
    return nios2_expand_custom_builtin (exp, fcode - nios2_custom_builtin_base,
					target);
  else
    gcc_unreachable ();
}

/* Implement TARGET_INIT_LIBFUNCS.  */
static void
nios2_init_libfuncs (void)
{
  /* For Linux, we have access to kernel support for atomic operations.  */
  if (TARGET_LINUX_ABI)
    init_sync_libfuncs (UNITS_PER_WORD);
}



/* Register a custom code use, and signal error if a conflict was found.  */
static void
nios2_register_custom_code (unsigned int N, enum nios2_ccs_code status,
			    int index)
{
  gcc_assert (N <= 255);

  if (status == CCS_FPU)
    {
      if (custom_code_status[N] == CCS_FPU && index != custom_code_index[N])
	{
	  custom_code_conflict = true;
	  error ("switch %<-mcustom-%s%> conflicts with switch %<-mcustom-%s%>",
		 N2FPU_NAME (custom_code_index[N]), N2FPU_NAME (index));
	}
      else if (custom_code_status[N] == CCS_BUILTIN_CALL)
	{
	  custom_code_conflict = true;
	  error ("call to %<__builtin_custom_%s%> conflicts with switch "
		 "%<-mcustom-%s%>", custom_builtin_name[custom_code_index[N]],
		 N2FPU_NAME (index));
	}
    }
  else if (status == CCS_BUILTIN_CALL)
    {
      if (custom_code_status[N] == CCS_FPU)
	{
	  custom_code_conflict = true;
	  error ("call to %<__builtin_custom_%s%> conflicts with switch "
		 "%<-mcustom-%s%>", custom_builtin_name[index],
		 N2FPU_NAME (custom_code_index[N]));
	}
      else
	{
	  /* Note that code conflicts between different __builtin_custom_xnxx
	     calls are not checked.  */
	}
    }
  else
    gcc_unreachable ();

  custom_code_status[N] = status;
  custom_code_index[N] = index;
}

/* Mark a custom code as not in use.  */
static void
nios2_deregister_custom_code (unsigned int N)
{
  if (N <= 255)
    {
      custom_code_status[N] = CCS_UNUSED;
      custom_code_index[N] = 0;
    }
}

/* Target attributes can affect per-function option state, so we need to
   save/restore the custom code tracking info using the
   TARGET_OPTION_SAVE/TARGET_OPTION_RESTORE hooks.  */

static void
nios2_option_save (struct cl_target_option *ptr,
		   struct gcc_options *opts ATTRIBUTE_UNUSED)
{
  unsigned int i;
  for (i = 0; i < ARRAY_SIZE (nios2_fpu_insn); i++)
    ptr->saved_fpu_custom_code[i] = N2FPU_N (i);
  memcpy (ptr->saved_custom_code_status, custom_code_status,
	  sizeof (custom_code_status));
  memcpy (ptr->saved_custom_code_index, custom_code_index,
	  sizeof (custom_code_index));
}

static void
nios2_option_restore (struct gcc_options *opts ATTRIBUTE_UNUSED,
		      struct cl_target_option *ptr)
{
  unsigned int i;
  for (i = 0; i < ARRAY_SIZE (nios2_fpu_insn); i++)
    N2FPU_N (i) = ptr->saved_fpu_custom_code[i];
  memcpy (custom_code_status, ptr->saved_custom_code_status,
	  sizeof (custom_code_status));
  memcpy (custom_code_index, ptr->saved_custom_code_index,
	  sizeof (custom_code_index));
}

/* Inner function to process the attribute((target(...))), take an argument and
   set the current options from the argument.  If we have a list, recursively
   go over the list.  */

static bool
nios2_valid_target_attribute_rec (tree args)
{
  if (TREE_CODE (args) == TREE_LIST)
    {
      bool ret = true;
      for (; args; args = TREE_CHAIN (args))
	if (TREE_VALUE (args)
	    && !nios2_valid_target_attribute_rec (TREE_VALUE (args)))
	  ret = false;
      return ret;
    }
  else if (TREE_CODE (args) == STRING_CST)
    {
      char *argstr = ASTRDUP (TREE_STRING_POINTER (args));
      while (argstr && *argstr != '\0')
	{
	  bool no_opt = false, end_p = false;
	  char *eq = NULL, *p;
	  while (ISSPACE (*argstr))
	    argstr++;
	  p = argstr;
	  while (*p != '\0' && *p != ',')
	    {
	      if (!eq && *p == '=')
		eq = p;
	      ++p;
	    }
	  if (*p == '\0')
	    end_p = true;
	  else
	    *p = '\0';
	  if (eq) *eq = '\0';

	  if (!strncmp (argstr, "no-", 3))
	    {
	      no_opt = true;
	      argstr += 3;
	    }
	  if (!strncmp (argstr, "custom-fpu-cfg", 14))
	    {
	      char *end_eq = p;
	      if (no_opt)
		{
		  error ("custom-fpu-cfg option does not support %<no-%>");
		  return false;
		}
	      if (!eq)
		{
		  error ("custom-fpu-cfg option requires configuration"
			 " argument");
		  return false;
		}
	      /* Increment and skip whitespace.  */
	      while (ISSPACE (*(++eq))) ;
	      /* Decrement and skip to before any trailing whitespace.  */
	      while (ISSPACE (*(--end_eq))) ;

	      nios2_handle_custom_fpu_cfg (eq, end_eq + 1, true);
	    }
	  else if (!strncmp (argstr, "custom-", 7))
	    {
	      int code = -1;
	      unsigned int i;
	      for (i = 0; i < ARRAY_SIZE (nios2_fpu_insn); i++)
		if (!strncmp (argstr + 7, N2FPU_NAME (i),
			      strlen (N2FPU_NAME (i))))
		  {
		    /* Found insn.  */
		    code = i;
		    break;
		  }
	      if (code >= 0)
		{
		  if (no_opt)
		    {
		      if (eq)
			{
			  error ("%<no-custom-%s%> does not accept arguments",
				 N2FPU_NAME (code));
			  return false;
			}
		      /* Disable option by setting to -1.  */
		      nios2_deregister_custom_code (N2FPU_N (code));
		      N2FPU_N (code) = -1;
		    }
		  else
		    {
		      char *t;
		      if (eq)
			while (ISSPACE (*(++eq))) ;
		      if (!eq || eq == p)
			{
			  error ("%<custom-%s=%> requires argument",
				 N2FPU_NAME (code));
			  return false;
			}
		      for (t = eq; t != p; ++t)
			{
			  if (ISSPACE (*t))
			    continue;
			  if (!ISDIGIT (*t))
			    {			 
			      error ("`custom-%s=' argument requires "
				     "numeric digits", N2FPU_NAME (code));
			      return false;
			    }
			}
		      /* Set option to argument.  */
		      N2FPU_N (code) = atoi (eq);
		      nios2_handle_custom_fpu_insn_option (code);
		    }
		}
	      else
		{
		  error ("%<custom-%s=%> is not recognised as FPU instruction",
			 argstr + 7);
		  return false;
		}		
	    }
	  else
	    {
	      error ("%<%s%> is unknown", argstr);
	      return false;
	    }

	  if (end_p)
	    break;
	  else
	    argstr = p + 1;
	}
      return true;
    }
  else
    gcc_unreachable ();
}

/* Return a TARGET_OPTION_NODE tree of the target options listed or NULL.  */

static tree
nios2_valid_target_attribute_tree (tree args)
{
  if (!nios2_valid_target_attribute_rec (args))
    return NULL_TREE;
  nios2_custom_check_insns ();
  return build_target_option_node (&global_options);
}

/* Hook to validate attribute((target("string"))).  */

static bool
nios2_valid_target_attribute_p (tree fndecl, tree ARG_UNUSED (name),
				tree args, int ARG_UNUSED (flags))
{
  struct cl_target_option cur_target;
  bool ret = true;
  tree old_optimize = build_optimization_node (&global_options);
  tree new_target, new_optimize;
  tree func_optimize = DECL_FUNCTION_SPECIFIC_OPTIMIZATION (fndecl);

  /* If the function changed the optimization levels as well as setting target
     options, start with the optimizations specified.  */
  if (func_optimize && func_optimize != old_optimize)
    cl_optimization_restore (&global_options,
			     TREE_OPTIMIZATION (func_optimize));

  /* The target attributes may also change some optimization flags, so update
     the optimization options if necessary.  */
  cl_target_option_save (&cur_target, &global_options);
  new_target = nios2_valid_target_attribute_tree (args);
  new_optimize = build_optimization_node (&global_options);

  if (!new_target)
    ret = false;

  else if (fndecl)
    {
      DECL_FUNCTION_SPECIFIC_TARGET (fndecl) = new_target;

      if (old_optimize != new_optimize)
	DECL_FUNCTION_SPECIFIC_OPTIMIZATION (fndecl) = new_optimize;
    }

  cl_target_option_restore (&global_options, &cur_target);

  if (old_optimize != new_optimize)
    cl_optimization_restore (&global_options,
			     TREE_OPTIMIZATION (old_optimize));
  return ret;
}

/* Remember the last target of nios2_set_current_function.  */
static GTY(()) tree nios2_previous_fndecl;

/* Establish appropriate back-end context for processing the function
   FNDECL.  The argument might be NULL to indicate processing at top
   level, outside of any function scope.  */
static void
nios2_set_current_function (tree fndecl)
{
  tree old_tree = (nios2_previous_fndecl
		   ? DECL_FUNCTION_SPECIFIC_TARGET (nios2_previous_fndecl)
		   : NULL_TREE);

  tree new_tree = (fndecl
		   ? DECL_FUNCTION_SPECIFIC_TARGET (fndecl)
		   : NULL_TREE);

  if (fndecl && fndecl != nios2_previous_fndecl)
    {
      nios2_previous_fndecl = fndecl;
      if (old_tree == new_tree)
	;

      else if (new_tree)
	{
	  cl_target_option_restore (&global_options,
				    TREE_TARGET_OPTION (new_tree));
	  target_reinit ();
	}

      else if (old_tree)
	{
	  struct cl_target_option *def
	    = TREE_TARGET_OPTION (target_option_current_node);

	  cl_target_option_restore (&global_options, def);
	  target_reinit ();
	}
    }
}

/* Hook to validate the current #pragma GCC target and set the FPU custom
   code option state.  If ARGS is NULL, then POP_TARGET is used to reset
   the options.  */
static bool
nios2_pragma_target_parse (tree args, tree pop_target)
{
  tree cur_tree;
  if (! args)
    {
      cur_tree = ((pop_target)
		  ? pop_target
		  : target_option_default_node);
      cl_target_option_restore (&global_options,
				TREE_TARGET_OPTION (cur_tree));
    }
  else
    {
      cur_tree = nios2_valid_target_attribute_tree (args);
      if (!cur_tree)
	return false;
    }

  target_option_current_node = cur_tree;
  return true;
}

/* Implement TARGET_MERGE_DECL_ATTRIBUTES.
   We are just using this hook to add some additional error checking to
   the default behavior.  GCC does not provide a target hook for merging
   the target options, and only correctly handles merging empty vs non-empty
   option data; see merge_decls() in c-decl.c.
   So here we require either that at least one of the decls has empty
   target options, or that the target options/data be identical.  */
static tree
nios2_merge_decl_attributes (tree olddecl, tree newdecl)
{
  tree oldopts = lookup_attribute ("target", DECL_ATTRIBUTES (olddecl));
  tree newopts = lookup_attribute ("target", DECL_ATTRIBUTES (newdecl));
  if (newopts && oldopts && newopts != oldopts)
    {
      tree oldtree = DECL_FUNCTION_SPECIFIC_TARGET (olddecl);
      tree newtree = DECL_FUNCTION_SPECIFIC_TARGET (newdecl);
      if (oldtree && newtree && oldtree != newtree)
	{
	  struct cl_target_option *olddata = TREE_TARGET_OPTION (oldtree);
	  struct cl_target_option *newdata = TREE_TARGET_OPTION (newtree);
	  if (olddata != newdata
	      && memcmp (olddata, newdata, sizeof (struct cl_target_option)))
	    error ("%qE redeclared with conflicting %qs attributes",
		   DECL_NAME (newdecl), "target");
	}
    }
  return merge_attributes (DECL_ATTRIBUTES (olddecl),
			   DECL_ATTRIBUTES (newdecl));
}


/* Initialize the GCC target structure.  */
#undef TARGET_ASM_FUNCTION_PROLOGUE
#define TARGET_ASM_FUNCTION_PROLOGUE nios2_asm_function_prologue

#undef TARGET_IN_SMALL_DATA_P
#define TARGET_IN_SMALL_DATA_P nios2_in_small_data_p

#undef  TARGET_SECTION_TYPE_FLAGS
#define TARGET_SECTION_TYPE_FLAGS  nios2_section_type_flags

#undef TARGET_INIT_BUILTINS
#define TARGET_INIT_BUILTINS nios2_init_builtins
#undef TARGET_EXPAND_BUILTIN
#define TARGET_EXPAND_BUILTIN nios2_expand_builtin
#undef TARGET_BUILTIN_DECL
#define TARGET_BUILTIN_DECL nios2_builtin_decl

#undef TARGET_INIT_LIBFUNCS
#define TARGET_INIT_LIBFUNCS nios2_init_libfuncs

#undef TARGET_FUNCTION_OK_FOR_SIBCALL
#define TARGET_FUNCTION_OK_FOR_SIBCALL hook_bool_tree_tree_true

#undef TARGET_CAN_ELIMINATE
#define TARGET_CAN_ELIMINATE nios2_can_eliminate

#undef TARGET_FUNCTION_ARG
#define TARGET_FUNCTION_ARG nios2_function_arg

#undef TARGET_FUNCTION_ARG_ADVANCE
#define TARGET_FUNCTION_ARG_ADVANCE nios2_function_arg_advance

#undef TARGET_ARG_PARTIAL_BYTES
#define TARGET_ARG_PARTIAL_BYTES nios2_arg_partial_bytes

#undef TARGET_TRAMPOLINE_INIT
#define TARGET_TRAMPOLINE_INIT nios2_trampoline_init

#undef TARGET_FUNCTION_VALUE
#define TARGET_FUNCTION_VALUE nios2_function_value

#undef TARGET_LIBCALL_VALUE
#define TARGET_LIBCALL_VALUE nios2_libcall_value

#undef TARGET_FUNCTION_VALUE_REGNO_P
#define TARGET_FUNCTION_VALUE_REGNO_P nios2_function_value_regno_p

#undef TARGET_RETURN_IN_MEMORY
#define TARGET_RETURN_IN_MEMORY nios2_return_in_memory

#undef TARGET_PROMOTE_PROTOTYPES
#define TARGET_PROMOTE_PROTOTYPES hook_bool_const_tree_true

#undef TARGET_SETUP_INCOMING_VARARGS
#define TARGET_SETUP_INCOMING_VARARGS nios2_setup_incoming_varargs

#undef TARGET_MUST_PASS_IN_STACK
#define TARGET_MUST_PASS_IN_STACK must_pass_in_stack_var_size

#undef TARGET_LEGITIMATE_CONSTANT_P
#define TARGET_LEGITIMATE_CONSTANT_P nios2_legitimate_constant_p

#undef TARGET_LEGITIMIZE_ADDRESS
#define TARGET_LEGITIMIZE_ADDRESS nios2_legitimize_address

#undef TARGET_LEGITIMATE_ADDRESS_P
#define TARGET_LEGITIMATE_ADDRESS_P nios2_legitimate_address_p

#undef TARGET_PREFERRED_RELOAD_CLASS
#define TARGET_PREFERRED_RELOAD_CLASS nios2_preferred_reload_class

#undef TARGET_RTX_COSTS
#define TARGET_RTX_COSTS nios2_rtx_costs

#undef TARGET_HAVE_TLS
#define TARGET_HAVE_TLS TARGET_LINUX_ABI

#undef TARGET_CANNOT_FORCE_CONST_MEM
#define TARGET_CANNOT_FORCE_CONST_MEM nios2_cannot_force_const_mem

#undef TARGET_ASM_OUTPUT_DWARF_DTPREL
#define TARGET_ASM_OUTPUT_DWARF_DTPREL nios2_output_dwarf_dtprel

#undef TARGET_PRINT_OPERAND
#define TARGET_PRINT_OPERAND nios2_print_operand

#undef TARGET_PRINT_OPERAND_ADDRESS
#define TARGET_PRINT_OPERAND_ADDRESS nios2_print_operand_address

#undef TARGET_ASM_OUTPUT_ADDR_CONST_EXTRA
#define TARGET_ASM_OUTPUT_ADDR_CONST_EXTRA nios2_output_addr_const_extra

#undef TARGET_OPTION_OVERRIDE
#define TARGET_OPTION_OVERRIDE nios2_option_override

#undef TARGET_OPTION_SAVE
#define TARGET_OPTION_SAVE nios2_option_save

#undef TARGET_OPTION_RESTORE
#define TARGET_OPTION_RESTORE nios2_option_restore

#undef TARGET_SET_CURRENT_FUNCTION
#define TARGET_SET_CURRENT_FUNCTION nios2_set_current_function

#undef TARGET_OPTION_VALID_ATTRIBUTE_P
#define TARGET_OPTION_VALID_ATTRIBUTE_P nios2_valid_target_attribute_p

#undef TARGET_OPTION_PRAGMA_PARSE
#define TARGET_OPTION_PRAGMA_PARSE nios2_pragma_target_parse

#undef TARGET_MERGE_DECL_ATTRIBUTES
#define TARGET_MERGE_DECL_ATTRIBUTES nios2_merge_decl_attributes

struct gcc_target targetm = TARGET_INITIALIZER;

#include "gt-nios2.h"
