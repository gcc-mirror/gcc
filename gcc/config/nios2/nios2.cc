/* Target machine subroutines for Altera Nios II.
   Copyright (C) 2012-2024 Free Software Foundation, Inc.
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
#include "tm_p.h"
#include "optabs.h"
#include "regs.h"
#include "emit-rtl.h"
#include "recog.h"
#include "diagnostic-core.h"
#include "output.h"
#include "insn-attr.h"
#include "flags.h"
#include "explow.h"
#include "calls.h"
#include "varasm.h"
#include "expr.h"
#include "toplev.h"
#include "langhooks.h"
#include "stor-layout.h"
#include "builtins.h"
#include "tree-pass.h"
#include "xregex.h"
#include "opts.h"

/* This file should be included last.  */
#include "target-def.h"

/* Forward function declarations.  */
static bool nios2_symbolic_constant_p (rtx);
static bool prologue_saved_reg_p (unsigned);
static void nios2_load_pic_register (void);
static void nios2_register_custom_code (unsigned int, enum nios2_ccs_code, int);
static const char *nios2_unspec_reloc_name (int);
static void nios2_register_builtin_fndecl (unsigned, tree);
static rtx nios2_ldst_parallel (bool, bool, bool, rtx, int,
				unsigned HOST_WIDE_INT, bool);
static int nios2_address_cost (rtx, machine_mode, addr_space_t, bool);

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
  /* Number of bytes used to store callee-saved registers.  */
  int callee_save_reg_size;
  /* Offset from new stack pointer to store registers.  */
  int save_regs_offset;
  /* Offset from save_regs_offset to store frame pointer register.  */
  int fp_save_offset;
  /* != 0 if function has a variable argument list.  */
  int uses_anonymous_args;
  /* != 0 if frame layout already calculated.  */
  int initialized;
};

/* State to track the assignment of custom codes to FPU/custom builtins.  */
static enum nios2_ccs_code custom_code_status[256];
static int custom_code_index[256];
/* Set to true if any conflicts (re-use of a code between 0-255) are found.  */
static bool custom_code_conflict = false;

/* State for command-line options.  */
regex_t nios2_gprel_sec_regex;
regex_t nios2_r0rel_sec_regex;


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
  N2_FTYPE(3, (SI, SI, SI))			\
  N2_FTYPE(3, (SI, VPTR, SI))			\
  N2_FTYPE(2, (UI, CVPTR))			\
  N2_FTYPE(2, (UI, DF))				\
  N2_FTYPE(2, (UI, SF))				\
  N2_FTYPE(2, (VOID, DF))			\
  N2_FTYPE(2, (VOID, SF))			\
  N2_FTYPE(2, (VOID, SI))			\
  N2_FTYPE(3, (VOID, SI, SI))			\
  N2_FTYPE(2, (VOID, VPTR))			\
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
#define N2F_NO_ERRNO      0x10
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
    N2FPU_INSN_DEF_BASE (round,    2, N2F_NO_ERRNO, lroundsfsi2,   (SI, SF)),
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
#define N2FPU_NO_ERRNO_P(code) (N2FPU(code).flags & N2F_NO_ERRNO)
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
nios2_fpu_compare_enabled (enum rtx_code cond, machine_mode mode)
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
  int callee_save_reg_size;

  if (cfun->machine->initialized)
    return cfun->machine->total_size;
  
  /* Calculate space needed for gp registers.  */
  save_reg_size = 0;
  for (regno = 0; regno <= LAST_GP_REG; regno++)
    if (prologue_saved_reg_p (regno))
      {
	save_mask |= 1 << regno;
	save_reg_size += 4;
      }

  /* If we are saving any callee-save register, then assume
     push.n/pop.n should be used. Make sure RA is saved, and
     contiguous registers starting from r16-- are all saved.  */
  if (TARGET_HAS_CDX && save_reg_size != 0)
    {
      if ((save_mask & (1 << RA_REGNO)) == 0)
	{
	  save_mask |= 1 << RA_REGNO;
	  save_reg_size += 4;
	}

      for (regno = 23; regno >= 16; regno--)
	if ((save_mask & (1 << regno)) != 0)
	  {
	    /* Starting from highest numbered callee-saved
	       register that is used, make sure all regs down
	       to r16 is saved, to maintain contiguous range
	       for push.n/pop.n.  */
	    unsigned int i;
	    for (i = regno - 1; i >= 16; i--)
	      if ((save_mask & (1 << i)) == 0)
		{
		  save_mask |= 1 << i;
		  save_reg_size += 4;
		}
	    break;
	  }
    }

  callee_save_reg_size = save_reg_size;

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

  cfun->machine->fp_save_offset = 0;
  if (save_mask & (1 << HARD_FRAME_POINTER_REGNUM))
    {
      int fp_save_offset = 0;
      for (regno = 0; regno < HARD_FRAME_POINTER_REGNUM; regno++)
	if (save_mask & (1 << regno))
	  fp_save_offset += 4;

      cfun->machine->fp_save_offset = fp_save_offset;
    }

  var_size = NIOS2_STACK_ALIGN (get_frame_size ());
  out_args_size = NIOS2_STACK_ALIGN (crtl->outgoing_args_size);
  total_size = var_size + out_args_size;

  save_reg_size = NIOS2_STACK_ALIGN (save_reg_size);
  total_size += save_reg_size;
  total_size += NIOS2_STACK_ALIGN (crtl->args.pretend_args_size);

  /* Save other computed information.  */
  cfun->machine->save_mask = save_mask;
  cfun->machine->total_size = total_size;
  cfun->machine->var_size = var_size;
  cfun->machine->args_size = out_args_size;
  cfun->machine->save_reg_size = save_reg_size;
  cfun->machine->callee_save_reg_size = callee_save_reg_size;
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
  rtx addr = plus_constant (Pmode, stack_pointer_rtx, offset, false);
  rtx_insn *insn = emit_move_insn (gen_frame_mem (Pmode, addr), reg);
  RTX_FRAME_RELATED_P (insn) = 1;
}

static void
restore_reg (int regno, unsigned offset)
{
  rtx reg = gen_rtx_REG (SImode, regno);
  rtx addr = plus_constant (Pmode, stack_pointer_rtx, offset, false);
  rtx_insn *insn = emit_move_insn (reg, gen_frame_mem (Pmode, addr));
  /* Tag epilogue unwind note.  */
  add_reg_note (insn, REG_CFA_RESTORE, reg);
  RTX_FRAME_RELATED_P (insn) = 1;
}

/* This routine tests for the base register update SET in load/store
   multiple RTL insns, used in pop_operation_p and ldstwm_operation_p.  */
static bool
base_reg_adjustment_p (rtx set, rtx *base_reg, rtx *offset)
{
  if (GET_CODE (set) == SET
      && REG_P (SET_DEST (set))
      && GET_CODE (SET_SRC (set)) == PLUS
      && REG_P (XEXP (SET_SRC (set), 0))
      && rtx_equal_p (SET_DEST (set), XEXP (SET_SRC (set), 0))
      && CONST_INT_P (XEXP (SET_SRC (set), 1)))
    {
      *base_reg = XEXP (SET_SRC (set), 0);
      *offset = XEXP (SET_SRC (set), 1);
      return true;
    }
  return false;
}

/* Does the CFA note work for push/pop prologue/epilogue instructions.  */
static void
nios2_create_cfa_notes (rtx_insn *insn, bool epilogue_p)
{
  int i = 0;
  rtx base_reg, offset, elt, pat = PATTERN (insn);
  if (epilogue_p)
    {
      elt = XVECEXP (pat, 0, 0);
      if (GET_CODE (elt) == RETURN)
	i++;
      elt = XVECEXP (pat, 0, i);
      if (base_reg_adjustment_p (elt, &base_reg, &offset))
	{
	  add_reg_note (insn, REG_CFA_ADJUST_CFA, copy_rtx (elt));
	  i++;
	}
      for (; i < XVECLEN (pat, 0); i++)
	{
	  elt = SET_DEST (XVECEXP (pat, 0, i));
	  gcc_assert (REG_P (elt));
	  add_reg_note (insn, REG_CFA_RESTORE, elt);
	}
    }
  else
    {
      /* Tag each of the prologue sets.  */
      for (i = 0; i < XVECLEN (pat, 0); i++)
	RTX_FRAME_RELATED_P (XVECEXP (pat, 0, i)) = 1;
    }
}

/* Temp regno used inside prologue/epilogue.  */
#define TEMP_REG_NUM 8

/* Emit conditional trap for checking stack limit.  SIZE is the number of
   additional bytes required.  

   GDB prologue analysis depends on this generating a direct comparison
   to the SP register, so the adjustment to add SIZE needs to be done on
   the other operand to the comparison.  Use TEMP_REG_NUM as a temporary,
   if necessary.  */
static void
nios2_emit_stack_limit_check (int size)
{
  rtx sum = NULL_RTX;

  if (GET_CODE (stack_limit_rtx) == SYMBOL_REF)
    {
      /* This generates a %hiadj/%lo pair with the constant size
	 add handled by the relocations.  */
      sum = gen_rtx_REG (Pmode, TEMP_REG_NUM);
      emit_move_insn (sum, plus_constant (Pmode, stack_limit_rtx, size));
    }
  else if (!REG_P (stack_limit_rtx))
    sorry ("Unknown form for stack limit expression");
  else if (size == 0)
    sum = stack_limit_rtx;
  else if (SMALL_INT (size))
    {
      sum = gen_rtx_REG (Pmode, TEMP_REG_NUM);
      emit_move_insn (sum, plus_constant (Pmode, stack_limit_rtx, size));
    }
  else
    {
      sum = gen_rtx_REG (Pmode, TEMP_REG_NUM);
      emit_move_insn (sum, gen_int_mode (size, Pmode));
      emit_insn (gen_add2_insn (sum, stack_limit_rtx));
    }

  emit_insn (gen_ctrapsi4 (gen_rtx_LTU (VOIDmode, stack_pointer_rtx, sum),
			   stack_pointer_rtx, sum, GEN_INT (3)));
}

static rtx_insn *
nios2_emit_add_constant (rtx reg, HOST_WIDE_INT immed)
{
  rtx_insn *insn;
  if (SMALL_INT (immed))
    insn = emit_insn (gen_add2_insn (reg, gen_int_mode (immed, Pmode)));
  else
    {
      rtx tmp = gen_rtx_REG (Pmode, TEMP_REG_NUM);
      emit_move_insn (tmp, gen_int_mode (immed, Pmode));
      insn = emit_insn (gen_add2_insn (reg, tmp));
    }
  return insn;
}

static rtx_insn *
nios2_adjust_stack (int sp_adjust, bool epilogue_p)
{
  enum reg_note note_kind = REG_NOTE_MAX;
  rtx_insn *insn = NULL;
  if (sp_adjust)
    {
      if (SMALL_INT (sp_adjust))
	insn = emit_insn (gen_add2_insn (stack_pointer_rtx,
					 gen_int_mode (sp_adjust, Pmode)));
      else
	{
	  rtx tmp = gen_rtx_REG (Pmode, TEMP_REG_NUM);
	  emit_move_insn (tmp, gen_int_mode (sp_adjust, Pmode));
	  insn = emit_insn (gen_add2_insn (stack_pointer_rtx, tmp));
	  /* Attach a note indicating what happened.  */
	  if (!epilogue_p)
	    note_kind = REG_FRAME_RELATED_EXPR;
	}
      if (epilogue_p)
	note_kind = REG_CFA_ADJUST_CFA;
      if (note_kind != REG_NOTE_MAX)
	{
	  rtx cfa_adj = gen_rtx_SET (stack_pointer_rtx,
				     plus_constant (Pmode, stack_pointer_rtx,
						    sp_adjust));
	  add_reg_note (insn, note_kind, cfa_adj);
	}
      RTX_FRAME_RELATED_P (insn) = 1;
    }
  return insn;
}

void
nios2_expand_prologue (void)
{
  unsigned int regno;
  int total_frame_size, save_offset;
  int sp_offset;      /* offset from base_reg to final stack value.  */
  int save_regs_base; /* offset from base_reg to register save area.  */
  rtx_insn *insn;

  total_frame_size = nios2_compute_frame_layout ();

  if (flag_stack_usage_info)
    current_function_static_stack_size = total_frame_size;

  /* When R2 CDX push.n/stwm is available, arrange for stack frame to be built
     using them.  */
  if (TARGET_HAS_CDX
      && (cfun->machine->save_reg_size != 0
	  || cfun->machine->uses_anonymous_args))
    {
      unsigned int regmask = cfun->machine->save_mask;
      unsigned int callee_save_regs = regmask & 0xffff0000;
      unsigned int caller_save_regs = regmask & 0x0000ffff;
      int push_immed = 0;
      int pretend_args_size = NIOS2_STACK_ALIGN (crtl->args.pretend_args_size);
      rtx stack_mem =
	gen_frame_mem (SImode, plus_constant (Pmode, stack_pointer_rtx, -4));

      /* Check that there is room for the entire stack frame before doing
	 any SP adjustments or pushes.  */
      if (crtl->limit_stack)
	nios2_emit_stack_limit_check (total_frame_size);

      if (pretend_args_size)
	{
	  if (cfun->machine->uses_anonymous_args)
	    {
	      /* Emit a stwm to push copy of argument registers onto
	         the stack for va_arg processing.  */
	      unsigned int r, mask = 0, n = pretend_args_size / 4;
	      for (r = LAST_ARG_REGNO - n + 1; r <= LAST_ARG_REGNO; r++)
		mask |= (1 << r);
	      insn = emit_insn (nios2_ldst_parallel
				(false, false, false, stack_mem,
				 -pretend_args_size, mask, false));
	      /* Tag first SP adjustment as frame-related.  */
	      RTX_FRAME_RELATED_P (XVECEXP (PATTERN (insn), 0, 0)) = 1;
	      RTX_FRAME_RELATED_P (insn) = 1;
	    }
	  else
	    nios2_adjust_stack (-pretend_args_size, false);
	}
      if (callee_save_regs)
	{
	  /* Emit a push.n to save registers and optionally allocate
	     push_immed extra bytes on the stack.  */
	  int sp_adjust;
	  if (caller_save_regs)
	    /* Can't allocate extra stack space yet.  */
	    push_immed = 0;
	  else if (cfun->machine->save_regs_offset <= 60)
	    /* Stack adjustment fits entirely in the push.n.  */
	    push_immed = cfun->machine->save_regs_offset;
	  else if (frame_pointer_needed
		   && cfun->machine->fp_save_offset == 0)
	    /* Deferring the entire stack adjustment until later
	       allows us to use a mov.n instead of a 32-bit addi
	       instruction to set the frame pointer.  */
	    push_immed = 0;
	  else
	    /* Splitting the stack adjustment between the push.n
	       and an explicit adjustment makes it more likely that
	       we can use spdeci.n for the explicit part.  */
	    push_immed = 60;
	  sp_adjust = -(cfun->machine->callee_save_reg_size + push_immed);
	  insn = emit_insn (nios2_ldst_parallel (false, false, false,
						 stack_mem, sp_adjust,
						 callee_save_regs, false));
	  nios2_create_cfa_notes (insn, false);
	  RTX_FRAME_RELATED_P (insn) = 1;
	}

      if (caller_save_regs)
	{
	  /* Emit a stwm to save the EH data regs, r4-r7.  */
	  int caller_save_size = (cfun->machine->save_reg_size
				  - cfun->machine->callee_save_reg_size);
	  gcc_assert ((caller_save_regs & ~0xf0) == 0);
	  insn = emit_insn (nios2_ldst_parallel
			    (false, false, false, stack_mem,
			     -caller_save_size, caller_save_regs, false));
	  nios2_create_cfa_notes (insn, false);
	  RTX_FRAME_RELATED_P (insn) = 1;
	}

      save_regs_base = push_immed;
      sp_offset = -(cfun->machine->save_regs_offset - push_immed);
    }
  /* The non-CDX cases decrement the stack pointer, to prepare for individual
     register saves to the stack.  */
  else if (!SMALL_INT (total_frame_size))
    {
      /* We need an intermediary point, this will point at the spill block.  */
      nios2_adjust_stack (cfun->machine->save_regs_offset - total_frame_size,
			  false);
      save_regs_base = 0;
      sp_offset = -cfun->machine->save_regs_offset;
      if (crtl->limit_stack)
	nios2_emit_stack_limit_check (cfun->machine->save_regs_offset);
    }
  else if (total_frame_size)
    {
      nios2_adjust_stack (-total_frame_size, false);
      save_regs_base = cfun->machine->save_regs_offset;
      sp_offset = 0;
      if (crtl->limit_stack)
	nios2_emit_stack_limit_check (0);
    }
  else
    save_regs_base = sp_offset = 0;

  /* Save the registers individually in the non-CDX case.  */
  if (!TARGET_HAS_CDX)
    {
      save_offset = save_regs_base + cfun->machine->save_reg_size;

      for (regno = LAST_GP_REG; regno > 0; regno--)
	if (cfun->machine->save_mask & (1 << regno))
	  {
	    save_offset -= 4;
	    save_reg (regno, save_offset);
	  }
    }

  /* Set the hard frame pointer.  */
  if (frame_pointer_needed)
    {
      int fp_save_offset = save_regs_base + cfun->machine->fp_save_offset;
      insn =
	(fp_save_offset == 0
	 ? emit_move_insn (hard_frame_pointer_rtx, stack_pointer_rtx)
	 : emit_insn (gen_add3_insn (hard_frame_pointer_rtx,
				     stack_pointer_rtx,
				     gen_int_mode (fp_save_offset, Pmode))));
      RTX_FRAME_RELATED_P (insn) = 1;
    }

  /* Allocate sp_offset more bytes in the stack frame.  */
  nios2_adjust_stack (sp_offset, false);

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
  rtx_insn *insn;
  rtx cfa_adj;
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
      insn =
	(cfun->machine->fp_save_offset == 0
	 ? emit_move_insn (stack_pointer_rtx, hard_frame_pointer_rtx)
	 : emit_insn (gen_add3_insn
		      (stack_pointer_rtx, hard_frame_pointer_rtx,
		       gen_int_mode (-cfun->machine->fp_save_offset, Pmode))));
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
      nios2_adjust_stack (cfun->machine->save_regs_offset, true);
      save_offset = 0;
      sp_adjust = total_frame_size - cfun->machine->save_regs_offset;
    }
  else
    {
      save_offset = cfun->machine->save_regs_offset;
      sp_adjust = total_frame_size;
    }

  if (!TARGET_HAS_CDX)
    {
      /* Generate individual register restores.  */
      save_offset += cfun->machine->save_reg_size;

      for (regno = LAST_GP_REG; regno > 0; regno--)
	if (cfun->machine->save_mask & (1 << regno))
	  {
	    save_offset -= 4;
	    restore_reg (regno, save_offset);
	  }
      nios2_adjust_stack (sp_adjust, true);
    }
  else if (cfun->machine->save_reg_size == 0)
    {
      /* Nothing to restore, just recover the stack position.  */
      nios2_adjust_stack (sp_adjust, true);
    }
  else
    {
      /* Emit CDX pop.n/ldwm to restore registers and optionally return.  */
      unsigned int regmask = cfun->machine->save_mask;
      unsigned int callee_save_regs = regmask & 0xffff0000;
      unsigned int caller_save_regs = regmask & 0x0000ffff;
      int callee_save_size = cfun->machine->callee_save_reg_size;
      int caller_save_size = cfun->machine->save_reg_size - callee_save_size;
      int pretend_args_size = NIOS2_STACK_ALIGN (crtl->args.pretend_args_size);
      bool ret_p = (!pretend_args_size && !crtl->calls_eh_return
		    && !sibcall_p);

      if (!ret_p || caller_save_size > 0)
	sp_adjust = save_offset;
      else
	sp_adjust = (save_offset > 60 ? save_offset - 60 : 0);

      save_offset -= sp_adjust;

      nios2_adjust_stack (sp_adjust, true);

      if (caller_save_regs)
	{
	  /* Emit a ldwm to restore EH data regs.  */
	  rtx stack_mem = gen_frame_mem (SImode, stack_pointer_rtx);
	  insn = emit_insn (nios2_ldst_parallel
			    (true, true, true, stack_mem,
			     caller_save_size, caller_save_regs, false));
	  RTX_FRAME_RELATED_P (insn) = 1;
	  nios2_create_cfa_notes (insn, true);
	}

      if (callee_save_regs)
	{
	  int sp_adjust = save_offset + callee_save_size;
	  rtx stack_mem;
	  if (ret_p)
	    {
	      /* Emit a pop.n to restore regs and return.  */
	      stack_mem =
		gen_frame_mem (SImode,
			       gen_rtx_PLUS (Pmode, stack_pointer_rtx,
					     gen_int_mode (sp_adjust - 4,
							   Pmode)));
	      insn =
		emit_jump_insn (nios2_ldst_parallel (true, false, false,
						     stack_mem, sp_adjust,
						     callee_save_regs, ret_p));
	      RTX_FRAME_RELATED_P (insn) = 1;
	      /* No need to attach CFA notes since we cannot step over
		 a return.  */
	      return;
	    }
	  else
	    {
	      /* If no return, we have to use the ldwm form.  */
	      stack_mem = gen_frame_mem (SImode, stack_pointer_rtx);
	      insn =
		emit_insn (nios2_ldst_parallel (true, true, true,
						stack_mem, sp_adjust,
						callee_save_regs, ret_p));
	      RTX_FRAME_RELATED_P (insn) = 1;
	      nios2_create_cfa_notes (insn, true);
	    }
	}

      if (pretend_args_size)
	nios2_adjust_stack (pretend_args_size, true);
    }

  /* Add in the __builtin_eh_return stack adjustment.  */
  if (crtl->calls_eh_return)
    emit_insn (gen_add2_insn (stack_pointer_rtx, EH_RETURN_STACKADJ_RTX));

  if (!sibcall_p)
    emit_jump_insn (gen_simple_return ());
}

bool
nios2_expand_return (void)
{
  /* If CDX is available, generate a pop.n instruction to do both
     the stack pop and return.  */
  if (TARGET_HAS_CDX)
    {
      int total_frame_size = nios2_compute_frame_layout ();
      int sp_adjust = (cfun->machine->save_regs_offset
		       + cfun->machine->callee_save_reg_size);
      gcc_assert (sp_adjust == total_frame_size);
      if (sp_adjust != 0)
	{
	  rtx mem =
	    gen_frame_mem (SImode,
			   plus_constant (Pmode, stack_pointer_rtx,
					  sp_adjust - 4, false));
	  rtx_insn *insn =
	    emit_jump_insn (nios2_ldst_parallel (true, false, false,
						 mem, sp_adjust,
						 cfun->machine->save_mask,
						 true));
	  RTX_FRAME_RELATED_P (insn) = 1;
	  /* No need to create CFA notes since we can't step over
	     a return.  */
	  return true;
	}
    }
  return false;
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
  if (flag_pic == 1)
    {
      fprintf (file, "\tnextpc\tr2\n");
      fprintf (file, "\t1: movhi\tr3, %%hiadj(_gp_got - 1b)\n");
      fprintf (file, "\taddi\tr3, r3, %%lo(_gp_got - 1b)\n");
      fprintf (file, "\tadd\tr2, r2, r3\n");
      fprintf (file, "\tldw\tr2, %%call(_mcount)(r2)\n");
      fprintf (file, "\tcallr\tr2\n");
    }
  else if (flag_pic == 2)
    {
      fprintf (file, "\tnextpc\tr2\n");
      fprintf (file, "\t1: movhi\tr3, %%hiadj(_gp_got - 1b)\n");
      fprintf (file, "\taddi\tr3, r3, %%lo(_gp_got - 1b)\n");
      fprintf (file, "\tadd\tr2, r2, r3\n");
      fprintf (file, "\tmovhi\tr3, %%call_hiadj(_mcount)\n");
      fprintf (file, "\taddi\tr3, r3, %%call_lo(_mcount)\n");
      fprintf (file, "\tadd\tr3, r2, r3\n");
      fprintf (file, "\tldw\tr2, 0(r3)\n");
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
  
  if (df_regs_ever_live_p (regno) && !call_used_or_fixed_reg_p (regno))
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
      /* This is the high end of the local variable storage, not the
	 hard frame pointer.  */
      offset = cfun->machine->args_size + cfun->machine->var_size;
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
    offset -= (cfun->machine->save_regs_offset
	       + cfun->machine->fp_save_offset); 

  return offset;
}

/* Return nonzero if this function is known to have a null epilogue.
   This allows the optimizer to omit jumps to jumps if no stack
   was created.  */
int
nios2_can_use_return_insn (void)
{
  int total_frame_size;

  if (!reload_completed || crtl->profile)
    return 0;

  total_frame_size = nios2_compute_frame_layout ();

  /* If CDX is available, check if we can return using a
     single pop.n instruction.  */
  if (TARGET_HAS_CDX
      && !frame_pointer_needed
      && cfun->machine->save_regs_offset <= 60
      && (cfun->machine->save_mask & 0x80000000) != 0
      && (cfun->machine->save_mask & 0xffff) == 0
      && crtl->args.pretend_args_size == 0)
    return true;

  return total_frame_size == 0;
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
	      error ("switch %<-mcustom-%s%> is required for "
		     "double-precision floating-point", N2FPU_NAME (j));
	      errors = true;
	    }
	break;
      }

  if (errors || custom_code_conflict)
    fatal_error (input_location,
		 "conflicting use of %<-mcustom%> switches, "
		 "target attributes, "
		 "and/or %<__builtin_custom_%> functions");
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

#define NIOS2_FPU_CONFIG_NUM 4
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

  NEXT_FPU_CONFIG;
  cfg->name = "fph2";
  cfg->code[n2fpu_fabss]   = 224;
  cfg->code[n2fpu_fnegs]   = 225;
  cfg->code[n2fpu_fcmpnes] = 226;
  cfg->code[n2fpu_fcmpeqs] = 227;
  cfg->code[n2fpu_fcmpges] = 228;
  cfg->code[n2fpu_fcmpgts] = 229;
  cfg->code[n2fpu_fcmples] = 230;
  cfg->code[n2fpu_fcmplts] = 231;
  cfg->code[n2fpu_fmaxs]   = 232;
  cfg->code[n2fpu_fmins]   = 233;
  cfg->code[n2fpu_round]   = 248;
  cfg->code[n2fpu_fixsi]   = 249;
  cfg->code[n2fpu_floatis] = 250;
  cfg->code[n2fpu_fsqrts]  = 251;
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
	     "value %<%s%>", cfgname);

  /* Guard against errors in the standard configurations.  */
  nios2_custom_check_insns ();
}

/* Check individual FPU insn options, and register custom code.  */
static void
nios2_handle_custom_fpu_insn_option (int fpu_insn_index)
{
  int param = N2FPU_N (fpu_insn_index);

  if (param >= 0 && param <= 255)
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
  return ggc_cleared_alloc<machine_function> ();
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
  if (flag_pic && stack_limit_rtx
      && GET_CODE (stack_limit_rtx) == SYMBOL_REF)
    sorry ("PIC support for %<-fstack-limit-symbol%>");

  /* Function to allocate machine-dependent function status.  */
  init_machine_status = &nios2_init_machine_status;

  nios2_section_threshold
    = (OPTION_SET_P (g_switch_value)
       ? g_switch_value : NIOS2_DEFAULT_GVALUE);

  if (nios2_gpopt_option == gpopt_unspecified)
    {
      /* Default to -mgpopt unless -fpic or -fPIC.  */
      if (flag_pic)
	nios2_gpopt_option = gpopt_none;
      else
	nios2_gpopt_option = gpopt_local;
    }

  /* GP-relative and r0-relative addressing don't make sense for PIC.  */
  if (flag_pic)
    {
      if (nios2_gpopt_option != gpopt_none)
	error ("%<-mgpopt%> not supported with PIC");
      if (nios2_gprel_sec)
	error ("%<-mgprel-sec=%> not supported with PIC");
      if (nios2_r0rel_sec)
	error ("%<-mr0rel-sec=%> not supported with PIC");
    }

  /* Process -mgprel-sec= and -m0rel-sec=.  */
  if (nios2_gprel_sec)
    {
      if (regcomp (&nios2_gprel_sec_regex, nios2_gprel_sec, 
		   REG_EXTENDED | REG_NOSUB))
	error ("%<-mgprel-sec=%> argument is not a valid regular expression");
    }
  if (nios2_r0rel_sec)
    {
      if (regcomp (&nios2_r0rel_sec_regex, nios2_r0rel_sec, 
		   REG_EXTENDED | REG_NOSUB))
	error ("%<-mr0rel-sec=%> argument is not a valid regular expression");
    }

  /* If we don't have mul, we don't have mulx either!  */
  if (!TARGET_HAS_MUL && TARGET_HAS_MULX)
    target_flags &= ~MASK_HAS_MULX;

  /* Optional BMX and CDX instructions only make sense for R2.  */
  if (!TARGET_ARCH_R2)
    {
      if (TARGET_HAS_BMX)
	error ("BMX instructions are only supported with R2 architecture");
      if (TARGET_HAS_CDX)
	error ("CDX instructions are only supported with R2 architecture");
    }

  /* R2 is little-endian only.  */
  if (TARGET_ARCH_R2 && TARGET_BIG_ENDIAN)
    error ("R2 architecture is little-endian only");

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
    = build_target_option_node (&global_options, &global_options_set);
}


/* Return true if CST is a constant within range of movi/movui/movhi.  */
static bool
nios2_simple_const_p (const_rtx cst)
{
  if (!CONST_INT_P (cst))
    return false;
  HOST_WIDE_INT val = INTVAL (cst);
  return SMALL_INT (val) || SMALL_INT_UNSIGNED (val) || UPPER16_INT (val);
}

/* Compute a (partial) cost for rtx X.  Return true if the complete
   cost has been computed, and false if subexpressions should be
   scanned.  In either case, *TOTAL contains the cost result.  */
static bool
nios2_rtx_costs (rtx x, machine_mode mode,
		 int outer_code,
		 int opno,
		 int *total, bool speed)
{
  int code = GET_CODE (x);

  switch (code)
    {
      case CONST_INT:
        if (INTVAL (x) == 0 || nios2_simple_const_p (x))
          {
            *total = COSTS_N_INSNS (0);
            return true;
          }
        else
          {
	    /* High + lo_sum.  */
            *total = COSTS_N_INSNS (1);
            return true;
          }

      case LABEL_REF:
      case SYMBOL_REF:
      case CONST:
      case CONST_DOUBLE:
	if (gprel_constant_p (x) || r0rel_constant_p (x))
          {
            *total = COSTS_N_INSNS (1);
            return true;
          }
	else
	  {
	    /* High + lo_sum.  */
	    *total = COSTS_N_INSNS (1);
	    return true;
	  }

      case HIGH:
	{
	  /* This is essentially a constant.  */
	  *total = COSTS_N_INSNS (0);
	  return true;
	}

      case LO_SUM:
	{
	  *total = COSTS_N_INSNS (0);
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

      /* For insns that have an execution latency (3 cycles), don't
	 penalize by the full amount since we can often schedule
	 to avoid it.  */
      case MULT:
        {
	  if (!TARGET_HAS_MUL)
	    *total = COSTS_N_INSNS (5);  /* Guess?  */
	  else if (speed)
	    *total = COSTS_N_INSNS (2);  /* Latency adjustment.  */
	  else 
	    *total = COSTS_N_INSNS (1);
	  if (TARGET_HAS_MULX && GET_MODE (x) == DImode)
	    {
	      enum rtx_code c0 = GET_CODE (XEXP (x, 0));
	      enum rtx_code c1 = GET_CODE (XEXP (x, 1));
	      if ((c0 == SIGN_EXTEND && c1 == SIGN_EXTEND)
		  || (c0 == ZERO_EXTEND && c1 == ZERO_EXTEND))
		/* This is the <mul>sidi3 pattern, which expands into 4 insns,
		   2 multiplies and 2 moves.  */
		{
		  *total = *total * 2 + COSTS_N_INSNS (2);
		  return true;
		}
	    }
          return false;
        }

      case DIV:
        {
	  if (!TARGET_HAS_DIV)
	    *total = COSTS_N_INSNS (5);  /* Guess?  */
	  else if (speed)
	    *total = COSTS_N_INSNS (2);  /* Latency adjustment.  */
	  else 
	    *total = COSTS_N_INSNS (1);
          return false;
        }

      case ASHIFT:
      case ASHIFTRT:
      case LSHIFTRT:
      case ROTATE:
        {
	  if (!speed)
	    *total = COSTS_N_INSNS (1);
	  else 
	    *total = COSTS_N_INSNS (2);  /* Latency adjustment.  */
          return false;
        }
	
      case ZERO_EXTRACT:
	if (TARGET_HAS_BMX)
	  {
	    *total = COSTS_N_INSNS (1);
	    return true;
	  }
	return false;

      case SIGN_EXTEND:
        {
	  if (MEM_P (XEXP (x, 0)))
	    *total = COSTS_N_INSNS (1);
	  else
	    *total = COSTS_N_INSNS (3);
	  return false;
	}

      case MEM:
	{
	  rtx addr = XEXP (x, 0);

	  /* Account for cost of different addressing modes.  */
	  *total = nios2_address_cost (addr, mode, ADDR_SPACE_GENERIC, speed);

	  if (outer_code == SET && opno == 0)
	    /* Stores execute in 1 cycle accounted for by
	       the outer SET.  */
	    ;
	  else if (outer_code == SET || outer_code == SIGN_EXTEND
		   || outer_code == ZERO_EXTEND)
	    /* Latency adjustment.  */
	    {
	      if (speed)
		*total += COSTS_N_INSNS (1);
	    }
	  else
	    /* This is going to have to be split into a load.  */
	    *total += COSTS_N_INSNS (speed ? 2 : 1);
	  return true;
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
  rtx fn;
  rtx_insn *insn;
  
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

/* Return true for large offsets requiring hiadj/lo relocation pairs.  */
static bool
nios2_large_offset_p (int unspec)
{
  gcc_assert (nios2_unspec_reloc_name (unspec) != NULL);

  if (flag_pic == 2
      /* FIXME: TLS GOT offset relocations will eventually also get this
	 treatment, after binutils support for those are also completed.  */
      && (unspec == UNSPEC_PIC_SYM || unspec == UNSPEC_PIC_CALL_SYM))
    return true;

  /* 'gotoff' offsets are always hiadj/lo.  */
  if (unspec == UNSPEC_PIC_GOTOFF_SYM)
    return true;

  return false;
}

/* Return true for conforming unspec relocations.  Also used in
   constraints.md and predicates.md.  */
bool
nios2_unspec_reloc_p (rtx op)
{
  return (GET_CODE (op) == CONST
	  && GET_CODE (XEXP (op, 0)) == UNSPEC
	  && ! nios2_large_offset_p (XINT (XEXP (op, 0), 1)));
}

static bool
nios2_large_unspec_reloc_p (rtx op)
{
  return (GET_CODE (op) == CONST
	  && GET_CODE (XEXP (op, 0)) == UNSPEC
	  && nios2_large_offset_p (XINT (XEXP (op, 0), 1)));
}

/* Helper to generate unspec constant.  */
static rtx
nios2_unspec_offset (rtx loc, int unspec)
{
  return gen_rtx_CONST (Pmode, gen_rtx_UNSPEC (Pmode, gen_rtvec (1, loc),
					       unspec));
}

/* Generate GOT pointer based address with large offset.  */
static rtx
nios2_large_got_address (rtx offset, rtx tmp)
{
  if (!tmp)
    tmp = gen_reg_rtx (Pmode);
  emit_move_insn (tmp, offset);
  return gen_rtx_PLUS (Pmode, tmp, pic_offset_table_rtx);
}

/* Generate a GOT pointer based address.  */
static rtx
nios2_got_address (rtx loc, int unspec)
{
  rtx offset = nios2_unspec_offset (loc, unspec);
  crtl->uses_pic_offset_table = 1;

  if (nios2_large_offset_p (unspec))
    return force_reg (Pmode, nios2_large_got_address (offset, NULL_RTX));

  return gen_rtx_PLUS (Pmode, pic_offset_table_rtx, offset);
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
      return gen_rtx_PLUS (Pmode, nios2_call_tls_get_addr (tmp),
			   nios2_unspec_offset (loc, UNSPEC_ADD_TLS_LDO));

    case TLS_MODEL_INITIAL_EXEC:
      tmp = gen_reg_rtx (Pmode);
      mem = gen_const_mem (Pmode, nios2_got_address (loc, UNSPEC_LOAD_TLS_IE));
      emit_move_insn (tmp, mem);
      tp = gen_rtx_REG (Pmode, TP_REGNO);
      return gen_rtx_PLUS (Pmode, tp, tmp);

    case TLS_MODEL_LOCAL_EXEC:
      tp = gen_rtx_REG (Pmode, TP_REGNO);
      return gen_rtx_PLUS (Pmode, tp,
			   nios2_unspec_offset (loc, UNSPEC_ADD_TLS_LE));
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
nios2_emit_expensive_div (rtx *operands, machine_mode mode)
{
  rtx or_result, shift_left_result;
  rtx lookup_value;
  rtx_code_label *lab1, *lab3;
  rtx_insn *insns;
  rtx libfunc;
  rtx final_result;
  rtx_insn *tmp;
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
                                          LCT_CONST, SImode,
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
			       machine_mode mode)
{
  gcc_assert (CONST_INT_P (op));

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
  gcc_assert (CONST_INT_P (op));
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
nios2_validate_fpu_compare (machine_mode mode, rtx *cmp, rtx *op1, rtx *op2,
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
nios2_validate_compare (machine_mode mode, rtx *cmp, rtx *op1, rtx *op2)
{
  enum rtx_code code = GET_CODE (*cmp);
  enum rtx_code alt_code;
  rtx alt_op2;

  if (GET_MODE_CLASS (mode) == MODE_FLOAT)
    return nios2_validate_fpu_compare (mode, cmp, op1, op2, true);

  if (CONST_INT_P (*op2) && *op2 != const0_rtx)
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
      *op2 = force_reg (mode, *op2);
    }
    else if (!reg_or_0_operand (*op2, mode))
      *op2 = force_reg (mode, *op2);
    
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


/* Addressing modes and constants.  */

/* Symbol references and other 32-bit constants are split into
   high/lo_sum pairs during the split1 pass.  After that, they are not
   considered legitimate addresses.
   This function returns true if in a pre-split context where these
   constants are allowed.  */
static bool
nios2_large_constant_allowed (void)
{
  /* The reload_completed check is for the benefit of
     nios2_asm_output_mi_thunk and perhaps other places that try to
     emulate a post-reload pass.  */
  return !(cfun->curr_properties & PROP_rtl_split_insns) && !reload_completed;
}

/* Return true if X is constant expression with a reference to an
   "ordinary" symbol; not GOT-relative, not GP-relative, not TLS.  */
static bool
nios2_symbolic_constant_p (rtx x)
{
  rtx base, offset;

  if (flag_pic)
    return false;
  if (GET_CODE (x) == LABEL_REF)
    return true;
  else if (CONSTANT_P (x))
    {
      split_const (x, &base, &offset);
      return (SYMBOL_REF_P (base)
		&& !SYMBOL_REF_TLS_MODEL (base)
		&& !gprel_constant_p (base)
		&& !r0rel_constant_p (base)
		&& SMALL_INT (INTVAL (offset)));
    }
  return false;
}

/* Return true if X is an expression of the form 
   (PLUS reg large_constant).  */
static bool
nios2_plus_large_constant_p (rtx x)
{
  return (GET_CODE (x) == PLUS
	  && REG_P (XEXP (x, 0))
	  && nios2_large_constant_p (XEXP (x, 1)));
}

/* Implement TARGET_LEGITIMATE_CONSTANT_P.  */
static bool
nios2_legitimate_constant_p (machine_mode mode ATTRIBUTE_UNUSED, rtx x)
{
  rtx base, offset;
  split_const (x, &base, &offset);
  return GET_CODE (base) != SYMBOL_REF || !SYMBOL_REF_TLS_MODEL (base);
}

/* Implement TARGET_CANNOT_FORCE_CONST_MEM.  */
static bool
nios2_cannot_force_const_mem (machine_mode mode ATTRIBUTE_UNUSED, rtx x)
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

/* Return true if OFFSET is permitted in a load/store address expression.
   Normally any 16-bit value is permitted, but on R2 if we may be emitting
   the IO forms of these instructions we must restrict the offset to fit
   in a 12-bit field instead.  */

static bool
nios2_valid_addr_offset_p (rtx offset)
{
  return (CONST_INT_P (offset)
	  && ((TARGET_ARCH_R2 && (TARGET_BYPASS_CACHE
				  || TARGET_BYPASS_CACHE_VOLATILE))
	      ? SMALL_INT12 (INTVAL (offset))
	      : SMALL_INT (INTVAL (offset))));
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
	      || nios2_valid_addr_offset_p (offset)
	      || (nios2_large_constant_allowed () 
		  && nios2_symbolic_constant_p (offset))
	      || nios2_unspec_reloc_p (offset)));
}

/* Implement TARGET_LEGITIMATE_ADDRESS_P.  */
static bool
nios2_legitimate_address_p (machine_mode mode ATTRIBUTE_UNUSED, rtx operand,
			    bool strict_p,
			    code_helper = ERROR_MARK)
{
  switch (GET_CODE (operand))
    {
      /* Direct.  */
    case SYMBOL_REF:
      if (SYMBOL_REF_TLS_MODEL (operand))
	return false;

      /* Else, fall through.  */
    case CONST:
      if (gprel_constant_p (operand) || r0rel_constant_p (operand))
	return true;

      /* Else, fall through.  */
    case LABEL_REF:
      if (nios2_large_constant_allowed () 
	  && nios2_symbolic_constant_p (operand))
	return true;
      return false;

    case CONST_INT:
      if (r0rel_constant_p (operand))
	return true;
      return nios2_large_constant_allowed ();

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

	if (nios2_valid_addr_expr_p (op0, op1, strict_p) 
	    || nios2_valid_addr_expr_p (op1, op0, strict_p))
	  return true;
      }
      break;

      /* %lo(constant)(reg)
	 This requires a 16-bit relocation and isn't valid with R2
	 io-variant load/stores.  */
    case LO_SUM:
      if (TARGET_ARCH_R2 
	  && (TARGET_BYPASS_CACHE || TARGET_BYPASS_CACHE_VOLATILE))
	return false;
      else
	{
	  rtx op0 = XEXP (operand, 0);
	  rtx op1 = XEXP (operand, 1);

	  return (REG_P (op0)
		  && nios2_regno_ok_for_base_p (REGNO (op0), strict_p)
		  && nios2_large_constant_p (op1));
	}

    default:
      break;
    }
  return false;
}

/* Implement TARGET_ADDRESS_COST.
   Experimentation has shown that we get better code by penalizing the
   the (plus reg symbolic_constant) and (plus reg (const ...)) forms
   but giving (plus reg symbol_ref) address modes the same cost as those
   that don't require splitting.  Also, from a theoretical point of view:
   - This is in line with the recommendation in the GCC internals 
     documentation to make address forms involving multiple
     registers more expensive than single-register forms.  
   - OTOH it still encourages fwprop1 to propagate constants into 
     address expressions more aggressively.
   - We should discourage splitting (symbol + offset) into hi/lo pairs
     to allow CSE'ing the symbol when it's used with more than one offset,
     but not so heavily as to avoid this addressing mode at all.  */
static int
nios2_address_cost (rtx address, 
		    machine_mode mode ATTRIBUTE_UNUSED,
		    addr_space_t as ATTRIBUTE_UNUSED, 
		    bool speed ATTRIBUTE_UNUSED)
{
  if (nios2_plus_large_constant_p (address))
    return COSTS_N_INSNS (1);
  if (nios2_large_constant_p (address))
    {
      if (GET_CODE (address) == CONST)
	return COSTS_N_INSNS (1);
      else
	return COSTS_N_INSNS (0);
    }
  return COSTS_N_INSNS (0);
}

/* Return true if X is a MEM whose address expression involves a large (32-bit)
   constant.  */
bool
nios2_large_constant_memory_operand_p (rtx x)
{
  rtx addr;

  if (GET_CODE (x) != MEM)
    return false;
  addr = XEXP (x, 0);

  return (nios2_large_constant_p (addr)
	  || nios2_plus_large_constant_p (addr));
}


/* Return true if X is something that needs to be split into a 
   high/lo_sum pair.  */
bool
nios2_large_constant_p (rtx x)
{
  return (nios2_symbolic_constant_p (x)
	  || nios2_large_unspec_reloc_p (x)
	  || (CONST_INT_P (x) && !SMALL_INT (INTVAL (x))));
}

/* Given an RTX X that satisfies nios2_large_constant_p, split it into
   high and lo_sum parts using TEMP as a scratch register.  Emit the high 
   instruction and return the lo_sum expression.  
   Also handle special cases involving constant integers.  */
rtx
nios2_split_large_constant (rtx x, rtx temp)
{
  if (CONST_INT_P (x))
    {
      HOST_WIDE_INT val = INTVAL (x);
      if (SMALL_INT (val))
	return x;
      else if (SMALL_INT_UNSIGNED (val) || UPPER16_INT (val))
	{
	  emit_move_insn (temp, x);
	  return temp;
	}
      else
	{
	  HOST_WIDE_INT high = (val + 0x8000) & ~0xffff;
	  HOST_WIDE_INT low = val - high;
	  emit_move_insn (temp, gen_int_mode (high, Pmode));
	  return gen_rtx_PLUS (Pmode, temp, gen_int_mode (low, Pmode));
	}
    }
  
  emit_insn (gen_rtx_SET (temp, gen_rtx_HIGH (Pmode, copy_rtx (x))));
  return gen_rtx_LO_SUM (Pmode, temp, copy_rtx (x));
}

/* Split an RTX of the form
     (plus op0 op1)
   where op1 is a large constant into
     (set temp (high op1))
     (set temp (plus op0 temp))
     (lo_sum temp op1)
   returning the lo_sum expression as the value.  */
static rtx
nios2_split_plus_large_constant (rtx op0, rtx op1)
{
  rtx temp = gen_reg_rtx (Pmode);
  op0 = force_reg (Pmode, op0);

  emit_insn (gen_rtx_SET (temp, gen_rtx_HIGH (Pmode, copy_rtx (op1))));
  emit_insn (gen_rtx_SET (temp, gen_rtx_PLUS (Pmode, op0, temp)));
  return gen_rtx_LO_SUM (Pmode, temp, copy_rtx (op1));
}

/* Given a MEM OP with an address that includes a splittable symbol or
   other large constant, emit some instructions to do the split and 
   return a new MEM.  */
rtx
nios2_split_large_constant_memory_operand (rtx op)
{
  rtx addr = XEXP (op, 0);

  if (nios2_large_constant_p (addr))
    addr = nios2_split_large_constant (addr, gen_reg_rtx (Pmode));
  else if (nios2_plus_large_constant_p (addr))
    addr = nios2_split_plus_large_constant (XEXP (addr, 0), XEXP (addr, 1));
  else
    gcc_unreachable ();
  return replace_equiv_address (op, addr, false);
}

/* Return true if SECTION is a small section name.  */
static bool
nios2_small_section_name_p (const char *section)
{
  return (strcmp (section, ".sbss") == 0
	  || startswith (section, ".sbss.")
	  || strcmp (section, ".sdata") == 0
	  || startswith (section, ".sdata.")
	  || (nios2_gprel_sec 
	      && regexec (&nios2_gprel_sec_regex, section, 0, NULL, 0) == 0));
}

/* Return true if SECTION is a r0-relative section name.  */
static bool
nios2_r0rel_section_name_p (const char *section)
{
  return (nios2_r0rel_sec 
	  && regexec (&nios2_r0rel_sec_regex, section, 0, NULL, 0) == 0);
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
	  const char *section = DECL_SECTION_NAME (exp);
	  if (nios2_small_section_name_p (section))
	    return true;
	}
      else if (flexible_array_type_p (TREE_TYPE (exp))
	       && (!TREE_PUBLIC (exp) || DECL_EXTERNAL (exp)))
	{
	  /* We really should not consider any objects of any flexibly-sized
	     type to be small data, but pre-GCC 10 did not test
	     for this and just fell through to the next case.  Thus older
	     code compiled with -mgpopt=global could contain GP-relative
	     accesses to objects defined in this compilation unit with
	     external linkage.  We retain the possible small-data treatment
	     of such definitions for backward ABI compatibility, but
	     no longer generate GP-relative accesses for external
	     references (so that the ABI could be changed in the future
	     with less potential impact), or objects with internal
	     linkage.  */
	  return false;
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

static bool
nios2_symbol_ref_in_small_data_p (rtx sym)
{
  tree decl;

  gcc_assert (GET_CODE (sym) == SYMBOL_REF);
  decl = SYMBOL_REF_DECL (sym);

  /* TLS variables are not accessed through the GP.  */
  if (SYMBOL_REF_TLS_MODEL (sym) != 0)
    return false;

  /* On Nios II R2, there is no GP-relative relocation that can be
     used with "io" instructions.  So, if we are implicitly generating
     those instructions, we cannot emit GP-relative accesses.  */
  if (TARGET_ARCH_R2
      && (TARGET_BYPASS_CACHE || TARGET_BYPASS_CACHE_VOLATILE))
    return false;

  /* If the user has explicitly placed the symbol in a small data section
     via an attribute, generate gp-relative addressing even if the symbol
     is external, weak, or larger than we'd automatically put in the
     small data section.  OTOH, if the symbol is located in some
     non-small-data section, we can't use gp-relative accesses on it
     unless the user has requested gpopt_data or gpopt_all.  */

  switch (nios2_gpopt_option)
    {
    case gpopt_none:
      /* Don't generate a gp-relative addressing mode if that's been
	 disabled.  */
      return false;

    case gpopt_local:
      /* Use GP-relative addressing for small data symbols that are
	 not external or weak or uninitialized common, plus any symbols
	 that have explicitly been placed in a small data section.  */
      if (decl && DECL_SECTION_NAME (decl))
	return nios2_small_section_name_p (DECL_SECTION_NAME (decl));
      return (SYMBOL_REF_SMALL_P (sym)
	      && !SYMBOL_REF_EXTERNAL_P (sym)
	      && !(decl && DECL_WEAK (decl))
	      && !(decl && DECL_COMMON (decl)
		   && (DECL_INITIAL (decl) == NULL
		       || (!in_lto_p
			   && DECL_INITIAL (decl) == error_mark_node))));

    case gpopt_global:
      /* Use GP-relative addressing for small data symbols, even if
	 they are external or weak.  Note that SYMBOL_REF_SMALL_P
         is also true of symbols that have explicitly been placed
         in a small data section.  */
      return SYMBOL_REF_SMALL_P (sym);

    case gpopt_data:
      /* Use GP-relative addressing for all data symbols regardless
	 of the object size, but not for code symbols.  This option
	 is equivalent to the user asserting that the entire data
	 section is accessible from the GP.  */
      return !SYMBOL_REF_FUNCTION_P (sym);

    case gpopt_all:
      /* Use GP-relative addressing for everything, including code.
	 Effectively, the user has asserted that the entire program
	 fits within the 64K range of the GP offset.  */
      return true;

    default:
      /* We shouldn't get here.  */
      return false;
    }
}

/* Likewise for r0-relative addressing.  */
static bool
nios2_symbol_ref_in_r0rel_data_p (rtx sym)
{
  tree decl;

  gcc_assert (GET_CODE (sym) == SYMBOL_REF);
  decl = SYMBOL_REF_DECL (sym);

  /* TLS variables are not accessed through r0.  */
  if (SYMBOL_REF_TLS_MODEL (sym) != 0)
    return false;

  /* On Nios II R2, there is no r0-relative relocation that can be
     used with "io" instructions.  So, if we are implicitly generating
     those instructions, we cannot emit r0-relative accesses.  */
  if (TARGET_ARCH_R2
      && (TARGET_BYPASS_CACHE || TARGET_BYPASS_CACHE_VOLATILE))
    return false;

  /* If the user has explicitly placed the symbol in a r0rel section
     via an attribute, generate r0-relative addressing.  */
  if (decl && DECL_SECTION_NAME (decl))
    return nios2_r0rel_section_name_p (DECL_SECTION_NAME (decl));
  return false;
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

/* Return true if SYMBOL_REF X binds locally.  */

static bool
nios2_symbol_binds_local_p (const_rtx x)
{
  return (SYMBOL_REF_DECL (x)
	  ? targetm.binds_local_p (SYMBOL_REF_DECL (x))
	  : SYMBOL_REF_LOCAL_P (x));
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
nios2_load_pic_address (rtx sym, int unspec, rtx tmp)
{
  if (flag_pic == 2
      && GET_CODE (sym) == SYMBOL_REF
      && nios2_symbol_binds_local_p (sym))
    /* Under -fPIC, generate a GOTOFF address for local symbols.  */
    {
      rtx offset = nios2_unspec_offset (sym, UNSPEC_PIC_GOTOFF_SYM);
      crtl->uses_pic_offset_table = 1;
      return nios2_large_got_address (offset, tmp);
    }

  if (unspec == UNSPEC_PIC_CALL_SYM)
    return gen_rtx_MEM (Pmode, nios2_got_address (sym, unspec));
  else
    return gen_const_mem (Pmode, nios2_got_address (sym, unspec));
}

/* Nonzero if the constant value X is a legitimate general operand
   when generating PIC code.  It is given that flag_pic is on and
   that X satisfies CONSTANT_P or is a CONST_DOUBLE.  */
bool
nios2_legitimate_pic_operand_p (rtx x)
{
  if (nios2_large_unspec_reloc_p (x))
    return true;

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
    base = nios2_load_pic_address (base, UNSPEC_PIC_SYM, NULL_RTX);
  else if (!nios2_large_constant_allowed () 
	   && nios2_symbolic_constant_p (addr))
    return nios2_split_large_constant (addr, gen_reg_rtx (Pmode));
  else if (CONST_INT_P (addr))
    {
      HOST_WIDE_INT val = INTVAL (addr);
      if (SMALL_INT (val))
	/* Use r0-relative addressing.  */
	return addr;
      else if (!nios2_large_constant_allowed ())
	/* Split into high/lo pair.  */
	return nios2_split_large_constant (addr, gen_reg_rtx (Pmode));
    }
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
			  machine_mode mode ATTRIBUTE_UNUSED)
{
  rtx op0, op1;
  
  if (CONSTANT_P (x))
    return nios2_legitimize_constant_address (x);

  /* Remaining cases all involve something + a constant.  */
  if (GET_CODE (x) != PLUS)
    return x;

  op0 = XEXP (x, 0);
  op1 = XEXP (x, 1);

  /* Target-independent code turns (exp + constant) into plain
     register indirect.  Although subsequent optimization passes will
     eventually sort that out, ivopts uses the unoptimized form for
     computing its cost model, so we get better results by generating
     the correct form from the start.  */
  if (nios2_valid_addr_offset_p (op1))
    return gen_rtx_PLUS (Pmode, force_reg (Pmode, op0), copy_rtx (op1));

  /* We may need to split symbolic constants now.  */
  else if (nios2_symbolic_constant_p (op1))
    {
      if (nios2_large_constant_allowed ())
	return gen_rtx_PLUS (Pmode, force_reg (Pmode, op0), copy_rtx (op1));
      else
	return nios2_split_plus_large_constant (op0, op1);
    }

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
  else if (GET_CODE (op1) == CONST)
    {
      rtx unspec, offset;
      split_const (op1, &unspec, &offset);
      if (GET_CODE (unspec) == UNSPEC
	  && !nios2_large_offset_p (XINT (unspec, 1))
	  && offset != const0_rtx)
	{
	  rtx reg = force_reg (Pmode, op0);
	  unspec = copy_rtx (unspec);
	  XVECEXP (unspec, 0, 0)
	    = plus_constant (Pmode, XVECEXP (unspec, 0, 0), INTVAL (offset));
	  return gen_rtx_PLUS (Pmode, reg, gen_rtx_CONST (Pmode, unspec));
	}
    }

  return x;
}

static rtx
nios2_delegitimize_address (rtx x)
{
  x = delegitimize_mem_from_attrs (x);

  if (GET_CODE (x) == CONST && GET_CODE (XEXP (x, 0)) == UNSPEC)
    {
      switch (XINT (XEXP (x, 0), 1))
	{
	case UNSPEC_PIC_SYM:
	case UNSPEC_PIC_CALL_SYM:
	case UNSPEC_PIC_GOTOFF_SYM:
	case UNSPEC_ADD_TLS_GD:
	case UNSPEC_ADD_TLS_LDM:
	case UNSPEC_LOAD_TLS_IE:
	case UNSPEC_ADD_TLS_LE:
	  x = XVECEXP (XEXP (x, 0), 0, 0);
	  gcc_assert (CONSTANT_P (x));
	  break;
	}
    }
  return x;
}

/* Main expander function for RTL moves.  */
bool
nios2_emit_move_sequence (rtx *operands, machine_mode mode)
{
  rtx to = operands[0];
  rtx from = operands[1];

  if (!register_operand (to, mode) && !reg_or_0_operand (from, mode))
    {
      gcc_assert (can_create_pseudo_p ());
      from = copy_to_mode_reg (mode, from);
    }

  if (CONSTANT_P (from))
    {
      if (CONST_INT_P (from))
	{
	  if (!SMALL_INT (INTVAL (from))
	      && !SMALL_INT_UNSIGNED (INTVAL (from))
	      && !UPPER16_INT (INTVAL (from)))
	    {
	      HOST_WIDE_INT high = (INTVAL (from) + 0x8000) & ~0xffff;
	      HOST_WIDE_INT low = INTVAL (from) & 0xffff;
	      emit_move_insn (to, gen_int_mode (high, SImode));
	      emit_insn (gen_add2_insn (to, gen_int_mode (low, HImode)));
	      set_unique_reg_note (get_last_insn (), REG_EQUAL,
				   copy_rtx (from));
	      return true;
	    }
	}
      else if (gprel_constant_p (from) || r0rel_constant_p (from))
	/* Handled directly by movsi_internal as gp + offset 
	   or r0 + offset.  */
	;
      else if (nios2_large_constant_p (from))
	/* This case covers either a regular symbol reference or an UNSPEC
	   representing a 32-bit offset.  We split the former 
	   only conditionally and the latter always.  */
	{
	  if (!nios2_large_constant_allowed () 
	      || nios2_large_unspec_reloc_p (from))
	    {
	      rtx lo = nios2_split_large_constant (from, to);
	      emit_insn (gen_rtx_SET (to, lo));
	      set_unique_reg_note (get_last_insn (), REG_EQUAL,
				   copy_rtx (operands[1]));
	      return true;
	    }
	}
      else 
	/* This is a TLS or PIC symbol.  */
	{
	  from = nios2_legitimize_constant_address (from);
	  if (CONSTANT_P (from))
	    {
	      emit_insn (gen_rtx_SET (to,
				      gen_rtx_HIGH (Pmode, copy_rtx (from))));
	      emit_insn (gen_rtx_SET (to, gen_rtx_LO_SUM (Pmode, to, from)));
	      set_unique_reg_note (get_last_insn (), REG_EQUAL,
				   copy_rtx (operands[1]));
	      return true;
	    }
	}
    }

  operands[0] = to;
  operands[1] = from;
  return false;
}

/* The function with address *ADDR is being called.  If the address
   needs to be loaded from the GOT, emit the instruction to do so and
   update *ADDR to point to the rtx for the loaded value.
   If REG != NULL_RTX, it is used as the target/scratch register in the
   GOT address calculation.  */
void
nios2_adjust_call_address (rtx *call_op, rtx reg)
{
  if (MEM_P (*call_op))
    call_op = &XEXP (*call_op, 0);

  rtx addr = *call_op;
  if (flag_pic && CONSTANT_P (addr))
    {
      rtx tmp = reg ? reg : NULL_RTX;
      if (!reg)
	reg = gen_reg_rtx (Pmode);
      addr = nios2_load_pic_address (addr, UNSPEC_PIC_CALL_SYM, tmp);
      emit_insn (gen_rtx_SET (reg, addr));
      *call_op = reg;
    }
}


/* Output assembly language related definitions.  */

/* Implement TARGET_PRINT_OPERAND_PUNCT_VALID_P.  */
static bool
nios2_print_operand_punct_valid_p (unsigned char code)
{
  return (code == '.' || code == '!');
}


/* Print the operand OP to file stream FILE modified by LETTER.
   LETTER can be one of:

     i: print i/hi/ui suffixes (used for mov instruction variants),
        when OP is the appropriate immediate operand.

     u: like 'i', except without "ui" suffix case (used for cmpgeu/cmpltu)

     o: print "io" if OP needs volatile access (due to TARGET_BYPASS_CACHE
        or TARGET_BYPASS_CACHE_VOLATILE).

     x: print i/hi/ci/chi suffixes for the and instruction,
        when OP is the appropriate immediate operand.

     z: prints the third register immediate operand in assembly
        instructions.  Outputs const0_rtx as the 'zero' register
	instead of '0'.
	
     y: same as 'z', but for specifically for logical instructions,
        where the processing for immediates are slightly different.

     H: for %hiadj
     L: for %lo
     D: for the upper 32-bits of a 64-bit double value
     R: prints reverse condition.
     A: prints (reg) operand for ld[s]ex and st[s]ex.

     .: print .n suffix for 16-bit instructions.
     !: print r.n suffix for 16-bit instructions.  Used for jmpr.n.
*/
static void
nios2_print_operand (FILE *file, rtx op, int letter)
{

  /* First take care of the format letters that just insert a string
     into the output stream.  */
  switch (letter)
    {
    case '.':
      if (current_output_insn && get_attr_length (current_output_insn) == 2)
	fprintf (file, ".n");
      return;

    case '!':
      if (current_output_insn && get_attr_length (current_output_insn) == 2)
	fprintf (file, "r.n");
      return;

    case 'x':
      if (CONST_INT_P (op))
	{
	  HOST_WIDE_INT val = INTVAL (op);
	  HOST_WIDE_INT low = val & 0xffff;
	  HOST_WIDE_INT high = (val >> 16) & 0xffff;

	  if (val != 0)
	    {
	      if (high != 0)
		{
		  if (low != 0)
		    {
		      gcc_assert (TARGET_ARCH_R2);
		      if (high == 0xffff)
			fprintf (file, "c");
		      else if (low == 0xffff)
			fprintf (file, "ch");
		      else
			gcc_unreachable ();
		    }
		  else
		    fprintf (file, "h");
		}
	      fprintf (file, "i");
	    }
	}
      return;

    case 'u':
    case 'i':
      if (CONST_INT_P (op))
	{
	  HOST_WIDE_INT val = INTVAL (op);
	  HOST_WIDE_INT low = val & 0xffff;
	  HOST_WIDE_INT high = (val >> 16) & 0xffff;
	  if (val != 0)
	    {
	      if (low == 0 && high != 0)
		fprintf (file, "h");
	      else if (high == 0 && (low & 0x8000) != 0 && letter != 'u')
		fprintf (file, "u");
	    }
	}
      if (CONSTANT_P (op) && op != const0_rtx)
        fprintf (file, "i");
      return;

    case 'o':
      if (GET_CODE (op) == MEM
	  && ((MEM_VOLATILE_P (op) && TARGET_BYPASS_CACHE_VOLATILE)
	      || TARGET_BYPASS_CACHE))
	{
	  gcc_assert (current_output_insn
		      && get_attr_length (current_output_insn) == 4);
	  fprintf (file, "io");
	}
      return;

    default:
      break;
    }

  /* Handle comparison operator names.  */
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

  /* Now handle the cases where we actually need to format an operand.  */
  switch (GET_CODE (op))
    {
    case REG:
      if (letter == 0 || letter == 'z' || letter == 'y')
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
      {
	rtx int_rtx = op;
	HOST_WIDE_INT val = INTVAL (int_rtx);
	HOST_WIDE_INT low = val & 0xffff;
	HOST_WIDE_INT high = (val >> 16) & 0xffff;

	if (letter == 'y')
	  {
	    if (val == 0)
	      fprintf (file, "zero");
	    else
	      {
		if (high != 0)
		  {
		    if (low != 0)
		      {
			gcc_assert (TARGET_ARCH_R2);
			if (high == 0xffff)
			  /* andci.  */
			  int_rtx = gen_int_mode (low, SImode);
			else if (low == 0xffff)
			  /* andchi.  */
			  int_rtx = gen_int_mode (high, SImode);
			else
			  gcc_unreachable ();
		      }
		    else
		      /* andhi.  */
		      int_rtx = gen_int_mode (high, SImode);
		  }
		else
		  /* andi.  */
		  int_rtx = gen_int_mode (low, SImode);
		output_addr_const (file, int_rtx);
	      }
	    return;
	  }
	else if (letter == 'z')
	  {
	    if (val == 0)
	      fprintf (file, "zero");
	    else
	      {
		if (low == 0 && high != 0)
		  int_rtx = gen_int_mode (high, SImode);
		else if (low != 0)
		  {
		    gcc_assert (high == 0 || high == 0xffff);
		    int_rtx = gen_int_mode (low, high == 0 ? SImode : HImode);
		  }
		else
		  gcc_unreachable ();
		output_addr_const (file, int_rtx);
	      }
	    return;
	  }
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
      else if (letter == 'H' || letter == 'L')
	{
	  fprintf (file, "%%");
	  if (GET_CODE (op) == CONST
	      && GET_CODE (XEXP (op, 0)) == UNSPEC)
	    {
	      rtx unspec = XEXP (op, 0);
	      int unspec_reloc = XINT (unspec, 1);
	      gcc_assert (nios2_large_offset_p (unspec_reloc));
	      fprintf (file, "%s_", nios2_unspec_reloc_name (unspec_reloc));
	      op = XVECEXP (unspec, 0, 0);
	    }
          fprintf (file, letter == 'H' ? "hiadj(" : "lo(");
          output_addr_const (file, op);
          fprintf (file, ")");
          return;
	}
      break;

    case SUBREG:
    case MEM:
      if (letter == 'A')
	{
	  /* Address of '(reg)' form, with no index.  */
	  fprintf (file, "(%s)", reg_names[REGNO (XEXP (op, 0))]);
	  return;
	}
      if (letter == 0)
        {
          output_address (VOIDmode, op);
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

  debug_rtx (op);
  output_operand_lossage ("Unsupported operand for code '%c'", letter);
  gcc_unreachable ();
}

/* Return true if this is a GP-relative accessible reference.  */
bool
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

/* Likewise if this is a zero-relative accessible reference.  */
bool
r0rel_constant_p (rtx op)
{
  if (GET_CODE (op) == SYMBOL_REF
      && nios2_symbol_ref_in_r0rel_data_p (op))
    return true;
  else if (GET_CODE (op) == CONST
           && GET_CODE (XEXP (op, 0)) == PLUS)
    return r0rel_constant_p (XEXP (XEXP (op, 0), 0));
  else if (GET_CODE (op) == CONST_INT
	   && SMALL_INT (INTVAL (op)))
    return true;

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
    case UNSPEC_PIC_GOTOFF_SYM:
      return "gotoff";
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
nios2_print_operand_address (FILE *file, machine_mode mode, rtx op)
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
      else if (r0rel_constant_p (op))
        {
	  if (CONST_INT_P (op))
	    {
	      output_addr_const (file, op);
	      fprintf (file, "(r0)");
	      return;
	    }
	  else
	    {
	      fprintf (file, "%%lo(");
	      output_addr_const (file, op);
	      fprintf (file, ")(r0)");
	      return;
	    }
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

    case LO_SUM:
      {
        rtx op0 = XEXP (op, 0);
        rtx op1 = XEXP (op, 1);

	if (REG_P (op0) && CONSTANT_P (op1))
	  {
	    nios2_print_operand (file, op1, 'L');
	    fprintf (file, "(%s)", reg_names[REGNO (op0)]);
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
        nios2_print_operand_address (file, mode, base);
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

/* Implemet TARGET_ASM_FILE_END.  */

static void
nios2_asm_file_end (void)
{
  /* The Nios II Linux stack is mapped non-executable by default, so add a
     .note.GNU-stack section for switching to executable stacks only when
     trampolines are generated.  */
  if (TARGET_LINUX_ABI && trampolines_created)
    file_end_indicate_exec_stack ();
}

/* Implement TARGET_ASM_FUNCTION_PROLOGUE.  */
static void
nios2_asm_function_prologue (FILE *file)
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
  machine_mode dst_mode = TYPE_MODE (TREE_TYPE (ftype));
  machine_mode src_mode = TYPE_MODE (TREE_VALUE (TYPE_ARG_TYPES (ftype)));

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

   CUM is a variable of type CUMULATIVE_ARGS which gives info about
   the preceding args and about the function being called.
   ARG is a description of the argument.  */

static rtx
nios2_function_arg (cumulative_args_t cum_v, const function_arg_info &arg)
{
  CUMULATIVE_ARGS *cum = get_cumulative_args (cum_v); 
  rtx return_rtx = NULL_RTX;

  if (cum->regs_used < NUM_ARG_REGS)
    return_rtx = gen_rtx_REG (arg.mode, FIRST_ARG_REGNO + cum->regs_used);

  return return_rtx;
}

/* Return number of bytes, at the beginning of the argument, that must be
   put in registers.  0 is the argument is entirely in registers or entirely
   in memory.  */

static int
nios2_arg_partial_bytes (cumulative_args_t cum_v, const function_arg_info &arg)
{
  CUMULATIVE_ARGS *cum = get_cumulative_args (cum_v); 
  HOST_WIDE_INT param_size = arg.promoted_size_in_bytes ();
  gcc_assert (param_size >= 0);

  /* Convert to words (round up).  */
  param_size = (UNITS_PER_WORD - 1 + param_size) / UNITS_PER_WORD;

  if (cum->regs_used < NUM_ARG_REGS
      && cum->regs_used + param_size > NUM_ARG_REGS)
    return (NUM_ARG_REGS - cum->regs_used) * UNITS_PER_WORD;

  return 0;
}

/* Update the data in CUM to advance over argument ARG.  */

static void
nios2_function_arg_advance (cumulative_args_t cum_v,
			    const function_arg_info &arg)
{
  CUMULATIVE_ARGS *cum = get_cumulative_args (cum_v); 
  HOST_WIDE_INT param_size = arg.promoted_size_in_bytes ();
  gcc_assert (param_size >= 0);

  /* Convert to words (round up).  */
  param_size = (UNITS_PER_WORD - 1 + param_size) / UNITS_PER_WORD;

  if (cum->regs_used + param_size > NUM_ARG_REGS)
    cum->regs_used = NUM_ARG_REGS;
  else
    cum->regs_used += param_size;
}

static pad_direction
nios2_function_arg_padding (machine_mode mode, const_tree type)
{
  /* On little-endian targets, the first byte of every stack argument
     is passed in the first byte of the stack slot.  */
  if (!BYTES_BIG_ENDIAN)
    return PAD_UPWARD;

  /* Otherwise, integral types are padded downward: the last byte of a
     stack argument is passed in the last byte of the stack slot.  */
  if (type != 0
      ? INTEGRAL_TYPE_P (type) || POINTER_TYPE_P (type)
      : GET_MODE_CLASS (mode) == MODE_INT)
    return PAD_DOWNWARD;

  /* Arguments smaller than a stack slot are padded downward.  */
  if (mode != BLKmode)
    return (GET_MODE_BITSIZE (mode) >= PARM_BOUNDARY
	    ? PAD_UPWARD : PAD_DOWNWARD);

  return ((int_size_in_bytes (type) >= (PARM_BOUNDARY / BITS_PER_UNIT))
	  ? PAD_UPWARD : PAD_DOWNWARD);
}

pad_direction
nios2_block_reg_padding (machine_mode mode, tree type,
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
		     LCT_NORMAL, VOIDmode, addr, Pmode, fnaddr, Pmode,
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
nios2_libcall_value (machine_mode mode, const_rtx fun ATTRIBUTE_UNUSED)
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
			      const function_arg_info &arg,
			      int *pretend_size, int second_time)
{
  CUMULATIVE_ARGS *cum = get_cumulative_args (cum_v); 
  CUMULATIVE_ARGS local_cum;
  cumulative_args_t local_cum_v = pack_cumulative_args (&local_cum);
  int regs_to_push;
  int pret_size;

  cfun->machine->uses_anonymous_args = 1;
  local_cum = *cum;
  if (!TYPE_NO_NAMED_ARGS_STDARG_P (TREE_TYPE (current_function_decl))
      || arg.type != NULL_TREE)
    nios2_function_arg_advance (local_cum_v, arg);

  regs_to_push = NUM_ARG_REGS - local_cum.regs_used;

  /* If we can use CDX stwm to push the arguments on the stack,
     nios2_expand_prologue will do that instead.  */
  if (!TARGET_HAS_CDX && !second_time && regs_to_push > 0)
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
  machine_mode dst_mode = TYPE_MODE (TREE_TYPE (exp));
  bool has_target_p = (dst_mode != VOIDmode);

  if (N2FPU_N (code) < 0)
    fatal_error (input_location,
		 "cannot call %<__builtin_custom_%s%> without specifying "
		 "switch %<-mcustom-%s%>",
		 N2FPU_NAME (code), N2FPU_NAME (code));
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
	  /* Save copy of parameter string into custom_builtin_name[].  */
	  snprintf (custom_builtin_name[builtin_code], 5, "%sn%s%s",
		    op[lhs].c, op[rhs1].c, op[rhs2].c);
	  strncpy (builtin_name + n, custom_builtin_name[builtin_code], 5);
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
  machine_mode tmode = VOIDmode;
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
	    error ("custom instruction opcode must be a compile-time "
		   "constant in the range 0-255 for %<__builtin_custom_%s%>",
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
	  ? gen_rtx_SET (target,
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
  enum nios2_arch_type arch;
  enum nios2_ftcode ftype;
  const char *name;
};

#define N2_BUILTINS					\
  N2_BUILTIN_DEF (sync,    R1, N2_FTYPE_VOID_VOID)	\
  N2_BUILTIN_DEF (ldbio,   R1, N2_FTYPE_SI_CVPTR)	\
  N2_BUILTIN_DEF (ldbuio,  R1, N2_FTYPE_UI_CVPTR)	\
  N2_BUILTIN_DEF (ldhio,   R1, N2_FTYPE_SI_CVPTR)	\
  N2_BUILTIN_DEF (ldhuio,  R1, N2_FTYPE_UI_CVPTR)	\
  N2_BUILTIN_DEF (ldwio,   R1, N2_FTYPE_SI_CVPTR)	\
  N2_BUILTIN_DEF (stbio,   R1, N2_FTYPE_VOID_VPTR_SI)	\
  N2_BUILTIN_DEF (sthio,   R1, N2_FTYPE_VOID_VPTR_SI)	\
  N2_BUILTIN_DEF (stwio,   R1, N2_FTYPE_VOID_VPTR_SI)	\
  N2_BUILTIN_DEF (rdctl,   R1, N2_FTYPE_SI_SI)		\
  N2_BUILTIN_DEF (wrctl,   R1, N2_FTYPE_VOID_SI_SI)	\
  N2_BUILTIN_DEF (rdprs,   R1, N2_FTYPE_SI_SI_SI)	\
  N2_BUILTIN_DEF (flushd,  R1, N2_FTYPE_VOID_VPTR)	\
  N2_BUILTIN_DEF (flushda, R1, N2_FTYPE_VOID_VPTR)	\
  N2_BUILTIN_DEF (wrpie,   R2, N2_FTYPE_SI_SI)		\
  N2_BUILTIN_DEF (eni,     R2, N2_FTYPE_VOID_SI)	\
  N2_BUILTIN_DEF (ldex,    R2, N2_FTYPE_SI_CVPTR)	\
  N2_BUILTIN_DEF (ldsex,   R2, N2_FTYPE_SI_CVPTR)	\
  N2_BUILTIN_DEF (stex,    R2, N2_FTYPE_SI_VPTR_SI)	\
  N2_BUILTIN_DEF (stsex,   R2, N2_FTYPE_SI_VPTR_SI)

enum nios2_builtin_code {
#define N2_BUILTIN_DEF(name, arch, ftype) NIOS2_BUILTIN_ ## name,
  N2_BUILTINS
#undef N2_BUILTIN_DEF
  NUM_FIXED_NIOS2_BUILTINS
};

static const struct nios2_builtin_desc nios2_builtins[] = {
#define N2_BUILTIN_DEF(name, arch, ftype)		\
  { CODE_FOR_ ## name, ARCH_ ## arch, ftype, "__builtin_" #name },
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

/* Expand ldio/stio and ldex/ldsex/stex/stsex form load-store
   instruction builtins.  */
static rtx
nios2_expand_ldst_builtin (tree exp, rtx target,
			   const struct nios2_builtin_desc *d)
{
  bool has_target_p;
  rtx addr, mem, val;
  struct expand_operand ops[MAX_RECOG_OPERANDS];
  machine_mode mode = insn_data[d->icode].operand[0].mode;

  addr = expand_normal (CALL_EXPR_ARG (exp, 0));
  mem = gen_rtx_MEM (mode, addr);

  if (insn_data[d->icode].operand[0].allows_mem)
    {
      /* stxio/stex/stsex.  */
      val = expand_normal (CALL_EXPR_ARG (exp, 1));
      if (CONST_INT_P (val))
	val = force_reg (mode, gen_int_mode (INTVAL (val), mode));
      val = simplify_gen_subreg (mode, val, GET_MODE (val), 0);
      create_output_operand (&ops[0], mem, mode);
      create_input_operand (&ops[1], val, mode);
      if (insn_data[d->icode].n_operands == 3)
	{
	  /* stex/stsex status value, returned as result of function.  */
	  create_output_operand (&ops[2], target, mode);
	  has_target_p = true;
	}
      else
	has_target_p = false;
    }
  else
    {
      /* ldxio.  */
      create_output_operand (&ops[0], target, mode);
      create_input_operand (&ops[1], mem, mode);
      has_target_p = true;
    }
  return nios2_expand_builtin_insn (d, insn_data[d->icode].n_operands, ops,
				    has_target_p);
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
      error ("control register number must be in range 0-31 for %s",
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

static rtx
nios2_expand_rdprs_builtin (tree exp, rtx target,
			    const struct nios2_builtin_desc *d)
{
  rtx reg = expand_normal (CALL_EXPR_ARG (exp, 0));
  rtx imm = expand_normal (CALL_EXPR_ARG (exp, 1));
  struct expand_operand ops[MAX_RECOG_OPERANDS];

  if (!rdwrctl_operand (reg, VOIDmode))
    {
      error ("register number must be in range 0-31 for %s",
	     d->name);
      return gen_reg_rtx (SImode);
    }

  if (!rdprs_dcache_operand (imm, VOIDmode))
    {
      error ("immediate value must fit into a %d-bit integer for %s",
	     (TARGET_ARCH_R2) ? 12 : 16, d->name);
      return gen_reg_rtx (SImode);
    }

  create_output_operand (&ops[0], target, SImode);
  create_input_operand (&ops[1], reg, SImode);
  create_integer_operand (&ops[2], INTVAL (imm));

  return nios2_expand_builtin_insn (d, 3, ops, true);
}

static rtx
nios2_expand_cache_builtin (tree exp, rtx target ATTRIBUTE_UNUSED,
			    const struct nios2_builtin_desc *d)
{
  rtx mem, addr;
  struct expand_operand ops[MAX_RECOG_OPERANDS];

  addr = expand_normal (CALL_EXPR_ARG (exp, 0));
  mem = gen_rtx_MEM (SImode, addr);

  create_input_operand (&ops[0], mem, SImode);
 
  return nios2_expand_builtin_insn (d, 1, ops, false);
}

static rtx
nios2_expand_wrpie_builtin (tree exp, rtx target,
			    const struct nios2_builtin_desc *d)
{
  rtx val;
  struct expand_operand ops[MAX_RECOG_OPERANDS];

  val = expand_normal (CALL_EXPR_ARG (exp, 0));
  create_input_operand (&ops[1], val, SImode);
  create_output_operand (&ops[0], target, SImode);
 
  return nios2_expand_builtin_insn (d, 2, ops, true);
}

static rtx
nios2_expand_eni_builtin (tree exp, rtx target ATTRIBUTE_UNUSED,
			    const struct nios2_builtin_desc *d)
{
  rtx imm = expand_normal (CALL_EXPR_ARG (exp, 0));
  struct expand_operand ops[MAX_RECOG_OPERANDS];

  if (INTVAL (imm) != 0 && INTVAL (imm) != 1)
    {
      error ("the ENI instruction operand must be either 0 or 1");
      return const0_rtx;      
    }
  create_integer_operand (&ops[0], INTVAL (imm));
 
  return nios2_expand_builtin_insn (d, 1, ops, false);
}

/* Implement TARGET_EXPAND_BUILTIN.  Expand an expression EXP that calls
   a built-in function, with result going to TARGET if that's convenient
   (and in mode MODE if that's convenient).
   SUBTARGET may be used as the target for computing one of EXP's operands.
   IGNORE is nonzero if the value is to be ignored.  */

static rtx
nios2_expand_builtin (tree exp, rtx target, rtx subtarget ATTRIBUTE_UNUSED,
                      machine_mode mode ATTRIBUTE_UNUSED,
		      int ignore ATTRIBUTE_UNUSED)
{
  tree fndecl = TREE_OPERAND (CALL_EXPR_FN (exp), 0);
  unsigned int fcode = DECL_MD_FUNCTION_CODE (fndecl);

  if (fcode < nios2_fpu_builtin_base)
    {
      const struct nios2_builtin_desc *d = &nios2_builtins[fcode];

      if (d->arch > nios2_arch_option)
	{
	  error ("built-in function %s requires Nios II R%d",
		 d->name, (int) d->arch);
	  /* Given it is invalid, just generate a normal call.  */
	  return expand_call (exp, target, ignore);
	}

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
	case NIOS2_BUILTIN_ldex:
	case NIOS2_BUILTIN_ldsex:
	case NIOS2_BUILTIN_stex:
	case NIOS2_BUILTIN_stsex:
	  return nios2_expand_ldst_builtin (exp, target, d);

	case NIOS2_BUILTIN_rdctl:
	case NIOS2_BUILTIN_wrctl:
	  return nios2_expand_rdwrctl_builtin (exp, target, d);

	case NIOS2_BUILTIN_rdprs:
	  return nios2_expand_rdprs_builtin (exp, target, d);

	case NIOS2_BUILTIN_flushd:
	case NIOS2_BUILTIN_flushda:
	  return nios2_expand_cache_builtin (exp, target, d);

	case NIOS2_BUILTIN_wrpie:
	  return nios2_expand_wrpie_builtin (exp, target, d);

	case NIOS2_BUILTIN_eni:
	  return nios2_expand_eni_builtin (exp, target, d);

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
static void ATTRIBUTE_UNUSED
nios2_init_libfuncs (void)
{
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
	  error ("switch %<-mcustom-%s%> conflicts with "
		 "switch %<-mcustom-%s%>",
		 N2FPU_NAME (custom_code_index[N]), N2FPU_NAME (index));
	}
      else if (custom_code_status[N] == CCS_BUILTIN_CALL)
	{
	  custom_code_conflict = true;
	  error ("call to %<__builtin_custom_%s%> conflicts with "
		 "switch %<-mcustom-%s%>",
		 custom_builtin_name[custom_code_index[N]],
		 N2FPU_NAME (index));
	}
    }
  else if (status == CCS_BUILTIN_CALL)
    {
      if (custom_code_status[N] == CCS_FPU)
	{
	  custom_code_conflict = true;
	  error ("call to %<__builtin_custom_%s%> conflicts with "
		 "switch %<-mcustom-%s%>",
		 custom_builtin_name[index],
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
		   struct gcc_options *opts ATTRIBUTE_UNUSED,
		   struct gcc_options *opts_set ATTRIBUTE_UNUSED)
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
		      struct gcc_options *opts_set ATTRIBUTE_UNUSED,
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

static bool
nios2_can_inline_p (tree caller, tree callee)
{
  tree callee_opts = DECL_FUNCTION_SPECIFIC_TARGET (callee);
  tree caller_opts = DECL_FUNCTION_SPECIFIC_TARGET (caller);
  struct cl_target_option *callee_ptr, *caller_ptr;
  unsigned int i;

  if (! callee_opts)
    callee_opts = target_option_default_node;
  if (! caller_opts)
    caller_opts = target_option_default_node;

  /* If both caller and callee have attributes, assume that if the
     pointer is different, the two functions have different target
     options since build_target_option_node uses a hash table for the
     options.  */
  if (callee_opts == caller_opts)
    return true;

  /* The only target options we recognize via function attributes are
     those related to custom instructions.  If we failed the above test,
     check that any custom instructions enabled in the callee are also
     enabled with the same value in the caller.  */
  callee_ptr = TREE_TARGET_OPTION (callee_opts);
  caller_ptr = TREE_TARGET_OPTION (caller_opts);
  for (i = 0; i < ARRAY_SIZE (nios2_fpu_insn); i++)
    if (callee_ptr->saved_fpu_custom_code[i] != -1
	&& (callee_ptr->saved_fpu_custom_code[i]
	    != caller_ptr->saved_fpu_custom_code[i]))
      return false;
  return true;
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

	  if (startswith (argstr, "no-"))
	    {
	      no_opt = true;
	      argstr += 3;
	    }
	  if (startswith (argstr, "custom-fpu-cfg"))
	    {
	      char *end_eq = p;
	      if (no_opt)
		{
		  error ("%<custom-fpu-cfg%> option does not support %<no-%>");
		  return false;
		}
	      if (!eq)
		{
		  error ("%<custom-fpu-cfg%> option requires configuration "
			 "argument");
		  return false;
		}
	      /* Increment and skip whitespace.  */
	      while (ISSPACE (*(++eq))) ;
	      /* Decrement and skip to before any trailing whitespace.  */
	      while (ISSPACE (*(--end_eq))) ;

	      nios2_handle_custom_fpu_cfg (eq, end_eq + 1, true);
	    }
	  else if (startswith (argstr, "custom-"))
	    {
	      int code = -1;
	      unsigned int i;
	      for (i = 0; i < ARRAY_SIZE (nios2_fpu_insn); i++)
		if (startswith (argstr + 7, N2FPU_NAME (i)))
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
			      error ("%<custom-%s=%> argument should be "
				     "a non-negative integer", N2FPU_NAME (code));
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
		  error ("%<custom-%s=%> is not recognized as FPU instruction",
			 argstr + 7);
		  return false;
		}		
	    }
	  else
	    {
	      error ("invalid custom instruction option %qs", argstr);
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
  return build_target_option_node (&global_options, &global_options_set);
}

/* Hook to validate attribute((target("string"))).  */

static bool
nios2_valid_target_attribute_p (tree fndecl, tree ARG_UNUSED (name),
				tree args, int ARG_UNUSED (flags))
{
  struct cl_target_option cur_target;
  bool ret = true;
  tree old_optimize
    = build_optimization_node (&global_options, &global_options_set);
  tree new_target, new_optimize;
  tree func_optimize = DECL_FUNCTION_SPECIFIC_OPTIMIZATION (fndecl);

  /* If the function changed the optimization levels as well as setting target
     options, start with the optimizations specified.  */
  if (func_optimize && func_optimize != old_optimize)
    cl_optimization_restore (&global_options, &global_options_set,
			     TREE_OPTIMIZATION (func_optimize));

  /* The target attributes may also change some optimization flags, so update
     the optimization options if necessary.  */
  cl_target_option_save (&cur_target, &global_options, &global_options_set);
  new_target = nios2_valid_target_attribute_tree (args);
  new_optimize = build_optimization_node (&global_options, &global_options_set);

  if (!new_target)
    ret = false;

  else if (fndecl)
    {
      DECL_FUNCTION_SPECIFIC_TARGET (fndecl) = new_target;

      if (old_optimize != new_optimize)
	DECL_FUNCTION_SPECIFIC_OPTIMIZATION (fndecl) = new_optimize;
    }

  cl_target_option_restore (&global_options, &global_options_set, &cur_target);

  if (old_optimize != new_optimize)
    cl_optimization_restore (&global_options, &global_options_set,
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
	  cl_target_option_restore (&global_options, &global_options_set,
				    TREE_TARGET_OPTION (new_tree));
	  target_reinit ();
	}

      else if (old_tree)
	{
	  struct cl_target_option *def
	    = TREE_TARGET_OPTION (target_option_current_node);

	  cl_target_option_restore (&global_options, &global_options_set, def);
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
      cl_target_option_restore (&global_options, &global_options_set,
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
   option data; see merge_decls() in c-decl.cc.
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

/* Implement TARGET_ASM_OUTPUT_MI_THUNK.  */
static void
nios2_asm_output_mi_thunk (FILE *file, tree thunk_fndecl ATTRIBUTE_UNUSED,
			   HOST_WIDE_INT delta, HOST_WIDE_INT vcall_offset,
			   tree function)
{
  const char *fnname = IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (thunk_fndecl));
  rtx this_rtx, funexp;
  rtx_insn *insn;

  /* Pretend to be a post-reload pass while generating rtl.  */
  reload_completed = 1;

  if (flag_pic)
    nios2_load_pic_register ();

  /* Mark the end of the (empty) prologue.  */
  emit_note (NOTE_INSN_PROLOGUE_END);

  /* Find the "this" pointer.  If the function returns a structure,
     the structure return pointer is in $5.  */
  if (aggregate_value_p (TREE_TYPE (TREE_TYPE (function)), function))
    this_rtx = gen_rtx_REG (Pmode, FIRST_ARG_REGNO + 1);
  else
    this_rtx = gen_rtx_REG (Pmode, FIRST_ARG_REGNO);

  /* Add DELTA to THIS_RTX.  */
  nios2_emit_add_constant (this_rtx, delta);

  /* If needed, add *(*THIS_RTX + VCALL_OFFSET) to THIS_RTX.  */
  if (vcall_offset)
    {
      rtx tmp;

      tmp = gen_rtx_REG (Pmode, 2);
      emit_move_insn (tmp, gen_rtx_MEM (Pmode, this_rtx));
      nios2_emit_add_constant (tmp, vcall_offset);
      emit_move_insn (tmp, gen_rtx_MEM (Pmode, tmp));
      emit_insn (gen_add2_insn (this_rtx, tmp));
    }

  /* Generate a tail call to the target function.  */
  if (!TREE_USED (function))
    {
      assemble_external (function);
      TREE_USED (function) = 1;
    }
  funexp = XEXP (DECL_RTL (function), 0);
  /* Function address needs to be constructed under PIC,
     provide r2 to use here.  */
  nios2_adjust_call_address (&funexp, gen_rtx_REG (Pmode, 2));
  insn = emit_call_insn (gen_sibcall_internal (funexp, const0_rtx));
  SIBLING_CALL_P (insn) = 1;

  /* Run just enough of rest_of_compilation to get the insns emitted.
     There's not really enough bulk here to make other passes such as
     instruction scheduling worth while.  */
  insn = get_insns ();
  shorten_branches (insn);
  assemble_start_function (thunk_fndecl, fnname);
  final_start_function (insn, file, 1);
  final (insn, file, 1);
  final_end_function ();
  assemble_end_function (thunk_fndecl, fnname);

  /* Stop pretending to be a post-reload pass.  */
  reload_completed = 0;
}


/* Utility function to break a memory address into
   base register + constant offset.  Return false if something
   unexpected is seen.  */
static bool
split_mem_address (rtx addr, rtx *base_reg, rtx *offset)
{
  if (REG_P (addr))
    {
      *base_reg = addr;
      *offset = const0_rtx;
      return true;
    }
  else if (GET_CODE (addr) == PLUS)
    {
      *base_reg = XEXP (addr, 0);
      *offset = XEXP (addr, 1);
      return true;
    }
  return false;
}

/* Splits out the operands of an ALU insn, places them in *LHS, *RHS1, *RHS2.  */
static void
split_alu_insn (rtx_insn *insn, rtx *lhs, rtx *rhs1, rtx *rhs2)
{
  rtx pat = PATTERN (insn);
  gcc_assert (GET_CODE (pat) == SET);
  *lhs = SET_DEST (pat);
  *rhs1 = XEXP (SET_SRC (pat), 0);
  if (GET_RTX_CLASS (GET_CODE (SET_SRC (pat))) != RTX_UNARY)
    *rhs2 = XEXP (SET_SRC (pat), 1);
  return;
}

/* Returns true if OP is a REG and assigned a CDX reg.  */
static bool
cdxreg (rtx op)
{
  return REG_P (op) && (!reload_completed || CDX_REG_P (REGNO (op)));
}

/* Returns true if OP is within range of CDX addi.n immediates.  */
static bool
cdx_add_immed (rtx op)
{
  if (CONST_INT_P (op))
    {
      HOST_WIDE_INT ival = INTVAL (op);
      return ival <= 128 && ival > 0 && (ival & (ival - 1)) == 0;
    }
  return false;
}

/* Returns true if OP is within range of CDX andi.n immediates.  */
static bool
cdx_and_immed (rtx op)
{
  if (CONST_INT_P (op))
    {
      HOST_WIDE_INT ival = INTVAL (op);
      return (ival == 1 || ival == 2 || ival == 3 || ival == 4
	      || ival == 8 || ival == 0xf || ival == 0x10
	      || ival == 0x1f || ival == 0x20
	      || ival == 0x3f || ival == 0x7f
	      || ival == 0x80 || ival == 0xff || ival == 0x7ff
	      || ival == 0xff00 || ival == 0xffff);
    }
  return false;
}

/* Returns true if OP is within range of CDX movi.n immediates.  */
static bool
cdx_mov_immed (rtx op)
{
  if (CONST_INT_P (op))
    {
      HOST_WIDE_INT ival = INTVAL (op);
      return ((ival >= 0 && ival <= 124)
	      || ival == 0xff || ival == -2 || ival == -1);
    }
  return false;
}

/* Returns true if OP is within range of CDX slli.n/srli.n immediates.  */
static bool
cdx_shift_immed (rtx op)
{
  if (CONST_INT_P (op))
    {
      HOST_WIDE_INT ival = INTVAL (op);
      return (ival == 1 || ival == 2 || ival == 3 || ival == 8
	      || ival == 12 || ival == 16 || ival == 24
	      || ival == 31);
    }
  return false;
}



/* Classification of different kinds of add instructions.  */
enum nios2_add_insn_kind {
  nios2_add_n_kind,
  nios2_addi_n_kind,
  nios2_subi_n_kind,
  nios2_spaddi_n_kind,
  nios2_spinci_n_kind,
  nios2_spdeci_n_kind,
  nios2_add_kind,
  nios2_addi_kind
};

static const char *nios2_add_insn_names[] = {
  "add.n", "addi.n", "subi.n", "spaddi.n",  "spinci.n", "spdeci.n",
  "add", "addi" };
static bool nios2_add_insn_narrow[] = {
  true, true, true, true, true, true,
  false, false};

/* Function to classify kinds of add instruction patterns.  */
static enum nios2_add_insn_kind 
nios2_add_insn_classify (rtx_insn *insn ATTRIBUTE_UNUSED,
			 rtx lhs, rtx rhs1, rtx rhs2)
{
  if (TARGET_HAS_CDX)
    {
      if (cdxreg (lhs) && cdxreg (rhs1))
	{
	  if (cdxreg (rhs2))
	    return nios2_add_n_kind;
	  if (CONST_INT_P (rhs2))
	    {
	      HOST_WIDE_INT ival = INTVAL (rhs2);
	      if (ival > 0 && cdx_add_immed (rhs2))
		return nios2_addi_n_kind;
	      if (ival < 0 && cdx_add_immed (GEN_INT (-ival)))
		return nios2_subi_n_kind;
	    }
	}
      else if (rhs1 == stack_pointer_rtx
	       && CONST_INT_P (rhs2))
	{
	  HOST_WIDE_INT imm7 = INTVAL (rhs2) >> 2;
	  HOST_WIDE_INT rem = INTVAL (rhs2) & 3;
	  if (rem == 0 && (imm7 & ~0x7f) == 0)
	    {
	      if (cdxreg (lhs))
		return nios2_spaddi_n_kind;
	      if (lhs == stack_pointer_rtx)
		return nios2_spinci_n_kind;
	    }
	  imm7 = -INTVAL(rhs2) >> 2;
	  rem = -INTVAL (rhs2) & 3;
	  if (lhs == stack_pointer_rtx
	      && rem == 0 && (imm7 & ~0x7f) == 0)
	    return nios2_spdeci_n_kind;
	}
    }
  return ((REG_P (rhs2) || rhs2 == const0_rtx)
	  ? nios2_add_kind : nios2_addi_kind);
}

/* Emit assembly language for the different kinds of add instructions.  */
const char*
nios2_add_insn_asm (rtx_insn *insn, rtx *operands)
{
  static char buf[256];
  int ln = 256;
  enum nios2_add_insn_kind kind
    = nios2_add_insn_classify (insn, operands[0], operands[1], operands[2]);
  if (kind == nios2_subi_n_kind)
    snprintf (buf, ln, "subi.n\t%%0, %%1, %d", (int) -INTVAL (operands[2]));
  else if (kind == nios2_spaddi_n_kind)
    snprintf (buf, ln, "spaddi.n\t%%0, %%2");
  else if (kind == nios2_spinci_n_kind)
    snprintf (buf, ln, "spinci.n\t%%2");
  else if (kind == nios2_spdeci_n_kind)
    snprintf (buf, ln, "spdeci.n\t%d", (int) -INTVAL (operands[2]));
  else
    snprintf (buf, ln, "%s\t%%0, %%1, %%z2", nios2_add_insn_names[(int)kind]);
  return buf;
}

/* This routine, which the default "length" attribute computation is
   based on, encapsulates information about all the cases where CDX
   provides a narrow 2-byte instruction form.  */
bool
nios2_cdx_narrow_form_p (rtx_insn *insn)
{
  rtx pat, lhs, rhs1 = NULL_RTX, rhs2 = NULL_RTX;
  enum attr_type type;
  if (!TARGET_HAS_CDX)
    return false;
  type = get_attr_type (insn);
  pat = PATTERN (insn);
  gcc_assert (reload_completed);
  switch (type)
    {
    case TYPE_CONTROL:
      if (GET_CODE (pat) == SIMPLE_RETURN)
	return true;
      if (GET_CODE (pat) == PARALLEL)
	pat = XVECEXP (pat, 0, 0);
      if (GET_CODE (pat) == SET)
	pat = SET_SRC (pat);
      if (GET_CODE (pat) == IF_THEN_ELSE)
	{
	  /* Conditional branch patterns; for these we
	     only check the comparison to find beqz.n/bnez.n cases.
	     For the 'nios2_cbranch' pattern, we cannot also check
	     the branch range here. That will be done at the md
	     pattern "length" attribute computation.  */
	  rtx cmp = XEXP (pat, 0);
	  return ((GET_CODE (cmp) == EQ || GET_CODE (cmp) == NE)
		  && cdxreg (XEXP (cmp, 0))
		  && XEXP (cmp, 1) == const0_rtx);
	}
      if (GET_CODE (pat) == TRAP_IF)
	/* trap.n is always usable.  */
	return true;
      if (GET_CODE (pat) == CALL)
	pat = XEXP (XEXP (pat, 0), 0);
      if (REG_P (pat))
	/* Control instructions taking a register operand are indirect
	   jumps and calls.  The CDX instructions have a 5-bit register
	   field so any reg is valid.  */
	return true;
      else
	{
	  gcc_assert (!insn_variable_length_p (insn));
	  return false;
	}
    case TYPE_ADD:
      {
	enum nios2_add_insn_kind kind;
	split_alu_insn (insn, &lhs, &rhs1, &rhs2);
	kind = nios2_add_insn_classify (insn, lhs, rhs1, rhs2);
	return nios2_add_insn_narrow[(int)kind];
      }
    case TYPE_LD:
      {
	bool ret;
	HOST_WIDE_INT offset, rem = 0;
	rtx addr, reg = SET_DEST (pat), mem = SET_SRC (pat);
	if (GET_CODE (mem) == SIGN_EXTEND)
	  /* No CDX form for sign-extended load.  */
	  return false;
	if (GET_CODE (mem) == ZERO_EXTEND)
	  /* The load alternatives in the zero_extend* patterns.  */
	  mem = XEXP (mem, 0);
	if (MEM_P (mem))
	  {
	    /* ldxio.  */
	    if ((MEM_VOLATILE_P (mem) && TARGET_BYPASS_CACHE_VOLATILE)
		|| TARGET_BYPASS_CACHE)
	      return false;
	    addr = XEXP (mem, 0);
	    /* GP-based and R0-based references are never narrow.  */
	    if (gprel_constant_p (addr) || r0rel_constant_p (addr))
		return false;
	    /* %lo requires a 16-bit relocation and is never narrow.  */
	    if (GET_CODE (addr) == LO_SUM)
	      return false;
	    ret = split_mem_address (addr, &rhs1, &rhs2);
	    gcc_assert (ret);
	  }
	else
	  return false;

	offset = INTVAL (rhs2);
	if (GET_MODE (mem) == SImode)
	  {
	    rem = offset & 3;
	    offset >>= 2;
	    /* ldwsp.n case.  */
	    if (rtx_equal_p (rhs1, stack_pointer_rtx)
		&& rem == 0 && (offset & ~0x1f) == 0)
	      return true;
	  }
	else if (GET_MODE (mem) == HImode)
	  {
	    rem = offset & 1;
	    offset >>= 1;
	  }
	/* ldbu.n, ldhu.n, ldw.n cases.  */
	return (cdxreg (reg) && cdxreg (rhs1)
		&& rem == 0 && (offset & ~0xf) == 0);
      }
    case TYPE_ST:
      if (GET_CODE (pat) == PARALLEL)
	/* stex, stsex.  */
	return false;
      else
	{
	  bool ret;
	  HOST_WIDE_INT offset, rem = 0;
	  rtx addr, reg = SET_SRC (pat), mem = SET_DEST (pat);
	  if (!MEM_P (mem))
	    return false;
	  /* stxio.  */
	  if ((MEM_VOLATILE_P (mem) && TARGET_BYPASS_CACHE_VOLATILE)
	      || TARGET_BYPASS_CACHE)
	    return false;
	  addr = XEXP (mem, 0);
	  /* GP-based and r0-based references are never narrow.  */
	  if (gprel_constant_p (addr) || r0rel_constant_p (addr))
	    return false;
	  /* %lo requires a 16-bit relocation and is never narrow.  */
	  if (GET_CODE (addr) == LO_SUM)
	    return false;
	  ret = split_mem_address (addr, &rhs1, &rhs2);
	  gcc_assert (ret);
	  offset = INTVAL (rhs2);
	  if (GET_MODE (mem) == SImode)
	    {
	      rem = offset & 3;
	      offset >>= 2;
	      /* stwsp.n case.  */
	      if (rtx_equal_p (rhs1, stack_pointer_rtx)
		  && rem == 0 && (offset & ~0x1f) == 0)
		return true;
	      /* stwz.n case.  */
	      else if (reg == const0_rtx && cdxreg (rhs1)
		       && rem == 0 && (offset & ~0x3f) == 0)
		return true;
	    }
	  else if (GET_MODE (mem) == HImode)
	    {
	      rem = offset & 1;
	      offset >>= 1;
	    }
	  else
	    {
	      gcc_assert (GET_MODE (mem) == QImode);
	      /* stbz.n case.  */
	      if (reg == const0_rtx && cdxreg (rhs1)
		  && (offset & ~0x3f) == 0)
		return true;
	    }

	  /* stbu.n, sthu.n, stw.n cases.  */
	  return (cdxreg (reg) && cdxreg (rhs1)
		  && rem == 0 && (offset & ~0xf) == 0);
	}
    case TYPE_MOV:
      lhs = SET_DEST (pat);
      rhs1 = SET_SRC (pat);
      if (CONST_INT_P (rhs1))
	return (cdxreg (lhs) && cdx_mov_immed (rhs1));
      gcc_assert (REG_P (lhs) && REG_P (rhs1));
      return true;

    case TYPE_AND:
      /* Some zero_extend* alternatives are and insns.  */
      if (GET_CODE (SET_SRC (pat)) == ZERO_EXTEND)
	return (cdxreg (SET_DEST (pat))
		&& cdxreg (XEXP (SET_SRC (pat), 0)));
      split_alu_insn (insn, &lhs, &rhs1, &rhs2);
      if (CONST_INT_P (rhs2))
	return (cdxreg (lhs) && cdxreg (rhs1) && cdx_and_immed (rhs2));
      return (cdxreg (lhs) && cdxreg (rhs2)
	      && (!reload_completed || rtx_equal_p (lhs, rhs1)));

    case TYPE_OR:
    case TYPE_XOR:
      /* Note the two-address limitation for CDX form.  */
      split_alu_insn (insn, &lhs, &rhs1, &rhs2);
      return (cdxreg (lhs) && cdxreg (rhs2)
	      && (!reload_completed || rtx_equal_p (lhs, rhs1)));

    case TYPE_SUB:
      split_alu_insn (insn, &lhs, &rhs1, &rhs2);
      return (cdxreg (lhs) && cdxreg (rhs1) && cdxreg (rhs2));

    case TYPE_NEG:
    case TYPE_NOT:
      split_alu_insn (insn, &lhs, &rhs1, NULL);
      return (cdxreg (lhs) && cdxreg (rhs1));

    case TYPE_SLL:
    case TYPE_SRL:
      split_alu_insn (insn, &lhs, &rhs1, &rhs2);
      return (cdxreg (lhs)
	      && ((cdxreg (rhs1) && cdx_shift_immed (rhs2))
		  || (cdxreg (rhs2)
		      && (!reload_completed || rtx_equal_p (lhs, rhs1)))));
    case TYPE_NOP:
    case TYPE_PUSH:
    case TYPE_POP:
      return true;
    default:
      break;
    }
  return false;
}

/* Main function to implement the pop_operation predicate that
   check pop.n insn pattern integrity.  The CDX pop.n patterns mostly
   hardcode the restored registers, so the main checking is for the
   SP offsets.  */
bool
pop_operation_p (rtx op)
{
  int i;
  HOST_WIDE_INT last_offset = -1, len = XVECLEN (op, 0);
  rtx base_reg, offset;

  if (len < 3 /* At least has a return, SP-update, and RA restore.  */
      || GET_CODE (XVECEXP (op, 0, 0)) != RETURN
      || !base_reg_adjustment_p (XVECEXP (op, 0, 1), &base_reg, &offset)
      || !rtx_equal_p (base_reg, stack_pointer_rtx)
      || !CONST_INT_P (offset)
      || (INTVAL (offset) & 3) != 0)
    return false;

  for (i = len - 1; i > 1; i--)
    {
      rtx set = XVECEXP (op, 0, i);
      rtx curr_base_reg, curr_offset;

      if (GET_CODE (set) != SET || !MEM_P (SET_SRC (set))
	  || !split_mem_address (XEXP (SET_SRC (set), 0),
				 &curr_base_reg, &curr_offset)
	  || !rtx_equal_p (base_reg, curr_base_reg)
	  || !CONST_INT_P (curr_offset))
	return false;
      if (i == len - 1)
	{
	  last_offset = INTVAL (curr_offset);
	  if ((last_offset & 3) != 0 || last_offset > 60)
	    return false;
	}
      else
	{
	  last_offset += 4;
	  if (INTVAL (curr_offset) != last_offset)
	    return false;
	}
    }
  if (last_offset < 0 || last_offset + 4 != INTVAL (offset))
    return false;

  return true;
}


/* Masks of registers that are valid for CDX ldwm/stwm instructions.
   The instruction can encode subsets drawn from either R2-R13 or
   R14-R23 + FP + RA.  */
#define CDX_LDSTWM_VALID_REGS_0 0x00003ffc
#define CDX_LDSTWM_VALID_REGS_1 0x90ffc000

static bool
nios2_ldstwm_regset_p (unsigned int regno, unsigned int *regset)
{
  if (*regset == 0)
    {
      if (CDX_LDSTWM_VALID_REGS_0 & (1 << regno))
	*regset = CDX_LDSTWM_VALID_REGS_0;
      else if (CDX_LDSTWM_VALID_REGS_1 & (1 << regno))
	*regset = CDX_LDSTWM_VALID_REGS_1;
      else
	return false;
      return true;
    }
  else
    return (*regset & (1 << regno)) != 0;
}

/* Main function to implement ldwm_operation/stwm_operation
   predicates that check ldwm/stwm insn pattern integrity.  */
bool
ldstwm_operation_p (rtx op, bool load_p)
{
  int start, i, end = XVECLEN (op, 0) - 1, last_regno = -1;
  unsigned int regset = 0;
  rtx base_reg, offset;  
  rtx first_elt = XVECEXP (op, 0, 0);
  bool inc_p = true;
  bool wb_p = base_reg_adjustment_p (first_elt, &base_reg, &offset);
  if (GET_CODE (XVECEXP (op, 0, end)) == RETURN)
    end--;
  start = wb_p ? 1 : 0;
  for (i = start; i <= end; i++)
    {
      int regno;
      rtx reg, mem, elt = XVECEXP (op, 0, i);
      /* Return early if not a SET at all.  */
      if (GET_CODE (elt) != SET)
	return false;
      reg = load_p ? SET_DEST (elt) : SET_SRC (elt);
      mem = load_p ? SET_SRC (elt) : SET_DEST (elt);
      if (!REG_P (reg) || !MEM_P (mem))
	return false;
      regno = REGNO (reg);
      if (!nios2_ldstwm_regset_p (regno, &regset))
	return false;
      /* If no writeback to determine direction, use offset of first MEM.  */
      if (wb_p)
	inc_p = INTVAL (offset) > 0;
      else if (i == start)
	{
	  rtx first_base, first_offset;
	  if (!split_mem_address (XEXP (mem, 0),
				  &first_base, &first_offset))
	    return false;
	  if (!REG_P (first_base) || !CONST_INT_P (first_offset))
	    return false;
	  base_reg = first_base;
	  inc_p = INTVAL (first_offset) >= 0;
	}
      /* Ensure that the base register is not loaded into.  */
      if (load_p && regno == (int) REGNO (base_reg))
	return false;
      /* Check for register order inc/dec integrity.  */
      if (last_regno >= 0)
	{
	  if (inc_p && last_regno >= regno)
	    return false;
	  if (!inc_p && last_regno <= regno)
	    return false;
	}
      last_regno = regno;
    }
  return true;
}

/* Helper for nios2_ldst_parallel, for generating a parallel vector
   SET element.  */
static rtx
gen_ldst (bool load_p, int regno, rtx base_mem, int offset)
{
  rtx reg = gen_rtx_REG (SImode, regno);
  rtx mem = adjust_address_nv (base_mem, SImode, offset);
  return gen_rtx_SET (load_p ? reg : mem,
		      load_p ? mem : reg);
}

/* A general routine for creating the body RTL pattern of
   ldwm/stwm/push.n/pop.n insns.
   LOAD_P: true/false for load/store direction.
   REG_INC_P: whether registers are incrementing/decrementing in the
   *RTL vector* (not necessarily the order defined in the ISA specification).
   OFFSET_INC_P: Same as REG_INC_P, but for the memory offset order.
   BASE_MEM: starting MEM.
   BASE_UPDATE: amount to update base register; zero means no writeback.
   REGMASK: register mask to load/store.
   RET_P: true if to tag a (return) element at the end.

   Note that this routine does not do any checking. It's the job of the
   caller to do the right thing, and the insn patterns to do the
   safe-guarding.  */
static rtx
nios2_ldst_parallel (bool load_p, bool reg_inc_p, bool offset_inc_p,
		     rtx base_mem, int base_update,
		     unsigned HOST_WIDE_INT regmask, bool ret_p)
{
  rtvec p;
  int regno, b = 0, i = 0, n = 0, len = popcount_hwi (regmask);
  if (ret_p) len++, i++, b++;
  if (base_update != 0) len++, i++;
  p = rtvec_alloc (len);
  for (regno = (reg_inc_p ? 0 : 31);
       regno != (reg_inc_p ? 32 : -1);
       regno += (reg_inc_p ? 1 : -1))
    if ((regmask & (1 << regno)) != 0)
      {
	int offset = (offset_inc_p ? 4 : -4) * n++;
	RTVEC_ELT (p, i++) = gen_ldst (load_p, regno, base_mem, offset);
      }
  if (ret_p)
    RTVEC_ELT (p, 0) = ret_rtx;
  if (base_update != 0)
    {
      rtx reg, offset;
      if (!split_mem_address (XEXP (base_mem, 0), &reg, &offset))
	gcc_unreachable ();
      RTVEC_ELT (p, b) =
	gen_rtx_SET (reg, plus_constant (Pmode, reg, base_update));
    }
  return gen_rtx_PARALLEL (VOIDmode, p);
}

/* CDX ldwm/stwm peephole optimization pattern related routines.  */

/* Data structure and sorting function for ldwm/stwm peephole optimizers.  */
struct ldstwm_operand
{
  int offset;	/* Offset from base register.  */
  rtx reg;	/* Register to store at this offset.  */
  rtx mem;	/* Original mem.  */
  bool bad;	/* True if this load/store can't be combined.  */
  bool rewrite; /* True if we should rewrite using scratch.  */
};

static int
compare_ldstwm_operands (const void *arg1, const void *arg2)
{
  const struct ldstwm_operand *op1 = (const struct ldstwm_operand *) arg1;
  const struct ldstwm_operand *op2 = (const struct ldstwm_operand *) arg2;
  if (op1->bad)
    return op2->bad ? 0 : 1;
  else if (op2->bad)
    return -1;
  else
    return op1->offset - op2->offset;
}

/* Helper function: return true if a load/store using REGNO with address
   BASEREG and offset OFFSET meets the constraints for a 2-byte CDX ldw.n,
   stw.n, ldwsp.n, or stwsp.n instruction.  */
static bool
can_use_cdx_ldstw (int regno, int basereg, int offset)
{
  if (CDX_REG_P (regno) && CDX_REG_P (basereg)
      && (offset & 0x3) == 0 && offset >= 0 && offset < 0x40)
    return true;
  else if (basereg == SP_REGNO
	   && offset >= 0 && offset < 0x80 && (offset & 0x3) == 0)
    return true;
  return false;
}

/* This function is called from peephole2 optimizers to try to merge
   a series of individual loads and stores into a ldwm or stwm.  It
   can also rewrite addresses inside the individual loads and stores
   using a common base register using a scratch register and smaller
   offsets if that allows them to use CDX ldw.n or stw.n instructions
   instead of 4-byte loads or stores.
   N is the number of insns we are trying to merge.  SCRATCH is non-null
   if there is a scratch register available.  The OPERANDS array contains
   alternating REG (even) and MEM (odd) operands.  */
bool
gen_ldstwm_peep (bool load_p, int n, rtx scratch, rtx *operands)
{
  /* CDX ldwm/stwm instructions allow a maximum of 12 registers to be
     specified.  */
#define MAX_LDSTWM_OPS 12
  struct ldstwm_operand sort[MAX_LDSTWM_OPS];
  int basereg = -1;
  int baseoffset;
  int i, m, lastoffset, lastreg;
  unsigned int regmask = 0, usemask = 0, regset;
  bool needscratch;
  int newbasereg;
  int nbytes;

  if (!TARGET_HAS_CDX)
    return false;
  if (n < 2 || n > MAX_LDSTWM_OPS)
    return false;

  /* Check all the operands for validity and initialize the sort array.
     The places where we return false here are all situations that aren't
     expected to ever happen -- invalid patterns, invalid registers, etc.  */
  for (i = 0; i < n; i++)
    {
      rtx base, offset;
      rtx reg = operands[i];
      rtx mem = operands[i + n];
      int r, o, regno;
      bool bad = false;

      if (!REG_P (reg) || !MEM_P (mem))
	return false;

      regno = REGNO (reg);
      if (regno > 31)
	return false;
      if (load_p && (regmask & (1 << regno)) != 0)
	return false;
      regmask |= 1 << regno;

      if (!split_mem_address (XEXP (mem, 0), &base, &offset))
	return false;
      r = REGNO (base);
      o = INTVAL (offset);

      if (basereg == -1)
	basereg = r;
      else if (r != basereg)
	bad = true;
      usemask |= 1 << r;

      sort[i].bad = bad;
      sort[i].rewrite = false;
      sort[i].offset = o;
      sort[i].reg = reg;
      sort[i].mem = mem;
    }

  /* If we are doing a series of register loads, we can't safely reorder
     them if any of the regs used in addr expressions are also being set.  */
  if (load_p && (regmask & usemask))
    return false;

  /* Sort the array by increasing mem offset order, then check that
     offsets are valid and register order matches mem order.  At the
     end of this loop, m is the number of loads/stores we will try to
     combine; the rest are leftovers.  */
  qsort (sort, n, sizeof (struct ldstwm_operand), compare_ldstwm_operands);

  baseoffset = sort[0].offset;
  needscratch = baseoffset != 0;
  if (needscratch && !scratch)
    return false;

  lastreg = regmask = regset = 0;
  lastoffset = baseoffset;
  for (m = 0; m < n && !sort[m].bad; m++)
    {
      int thisreg = REGNO (sort[m].reg);
      if (sort[m].offset != lastoffset
	  || (m > 0 && lastreg >= thisreg)
	  || !nios2_ldstwm_regset_p (thisreg, &regset))
	break;
      lastoffset += 4;
      lastreg = thisreg;
      regmask |= (1 << thisreg);
    }

  /* For loads, make sure we are not overwriting the scratch reg.
     The peephole2 pattern isn't supposed to match unless the register is
     unused all the way through, so this isn't supposed to happen anyway.  */
  if (load_p
      && needscratch
      && ((1 << REGNO (scratch)) & regmask) != 0)
    return false;
  newbasereg = needscratch ? (int) REGNO (scratch) : basereg;

  /* We may be able to combine only the first m of the n total loads/stores
     into a single instruction.  If m < 2, there's no point in emitting
     a ldwm/stwm at all, but we might be able to do further optimizations
     if we have a scratch.  We will count the instruction lengths of the
     old and new patterns and store the savings in nbytes.  */
  if (m < 2)
    {
      if (!needscratch)
	return false;
      m = 0;
      nbytes = 0;
    }
  else
    nbytes = -4;  /* Size of ldwm/stwm.  */
  if (needscratch)
    {
      int bo = baseoffset > 0 ? baseoffset : -baseoffset;
      if (CDX_REG_P (newbasereg)
	  && CDX_REG_P (basereg)
	  && bo <= 128 && bo > 0 && (bo & (bo - 1)) == 0)
	nbytes -= 2;  /* Size of addi.n/subi.n.  */
      else
	nbytes -= 4;  /* Size of non-CDX addi.  */
    }

  /* Count the size of the input load/store instructions being replaced.  */
  for (i = 0; i < m; i++)
    if (can_use_cdx_ldstw (REGNO (sort[i].reg), basereg, sort[i].offset))
      nbytes += 2;
    else
      nbytes += 4;

  /* We may also be able to save a bit if we can rewrite non-CDX
     load/stores that can't be combined into the ldwm/stwm into CDX
     load/stores using the scratch reg.  For example, this might happen
     if baseoffset is large, by bringing in the offsets in the load/store
     instructions within the range that fits in the CDX instruction.  */
  if (needscratch && CDX_REG_P (newbasereg))
    for (i = m; i < n && !sort[i].bad; i++)
      if (!can_use_cdx_ldstw (REGNO (sort[i].reg), basereg, sort[i].offset)
	  && can_use_cdx_ldstw (REGNO (sort[i].reg), newbasereg,
				sort[i].offset - baseoffset))
	{
	  sort[i].rewrite = true;
	  nbytes += 2;
	}

  /* Are we good to go?  */
  if (nbytes <= 0)
    return false;

  /* Emit the scratch load.  */
  if (needscratch)
    emit_insn (gen_rtx_SET (scratch, XEXP (sort[0].mem, 0)));

  /* Emit the ldwm/stwm insn.  */
  if (m > 0)
    {
      rtvec p = rtvec_alloc (m);
      for (i = 0; i < m; i++)
	{
	  int offset = sort[i].offset;
	  rtx mem, reg = sort[i].reg;
	  rtx base_reg = gen_rtx_REG (Pmode, newbasereg);
	  if (needscratch)
	    offset -= baseoffset;
	  mem = gen_rtx_MEM (SImode, plus_constant (Pmode, base_reg, offset));
	  if (load_p)
	    RTVEC_ELT (p, i) = gen_rtx_SET (reg, mem);
	  else
	    RTVEC_ELT (p, i) = gen_rtx_SET (mem, reg);
	}
      emit_insn (gen_rtx_PARALLEL (VOIDmode, p));
    }

  /* Emit any leftover load/stores as individual instructions, doing
     the previously-noted rewrites to use the scratch reg.  */
  for (i = m; i < n; i++)
    {
      rtx reg = sort[i].reg;
      rtx mem = sort[i].mem;
      if (sort[i].rewrite)
	{
	  int offset = sort[i].offset - baseoffset;
	  mem = gen_rtx_MEM (SImode, plus_constant (Pmode, scratch, offset));
	}
      if (load_p)
	emit_move_insn (reg, mem);
      else
	emit_move_insn (mem, reg);
    }
  return true;
}

/* Implement TARGET_MACHINE_DEPENDENT_REORG:
   We use this hook when emitting CDX code to enforce the 4-byte
   alignment requirement for labels that are used as the targets of
   jmpi instructions.  CDX code can otherwise contain a mix of 16-bit
   and 32-bit instructions aligned on any 16-bit boundary, but functions
   and jmpi labels have to be 32-bit aligned because of the way the address
   is encoded in the instruction.  */

static unsigned char *label_align;
static int min_labelno, max_labelno;

static void
nios2_reorg (void)
{
  bool changed = true;
  rtx_insn *insn;

  if (!TARGET_HAS_CDX)
    return;

  /* Initialize the data structures.  */
  if (label_align)
    free (label_align);
  max_labelno = max_label_num ();
  min_labelno = get_first_label_num ();
  label_align = XCNEWVEC (unsigned char, max_labelno - min_labelno + 1);
  
  /* Iterate on inserting alignment and adjusting branch lengths until
     no more changes.  */
  while (changed)
    {
      changed = false;
      shorten_branches (get_insns ());

      for (insn = get_insns (); insn != 0; insn = NEXT_INSN (insn))
	if (JUMP_P (insn) && insn_variable_length_p (insn))
	  {
	    rtx label = JUMP_LABEL (insn);
	    /* We use the current fact that all cases of 'jmpi'
	       doing the actual branch in the machine description
	       has a computed length of 6 or 8.  Length 4 and below
	       are all PC-relative 'br' branches without the jump-align
	       problem.  */
	    if (label && LABEL_P (label) && get_attr_length (insn) > 4)
	      {
		int index = CODE_LABEL_NUMBER (label) - min_labelno;
		if (label_align[index] != 2)
		  {
		    label_align[index] = 2;
		    changed = true;
		  }
	      }
	  }
    }
}

/* Implement LABEL_ALIGN, using the information gathered in nios2_reorg.  */
int
nios2_label_align (rtx label)
{
  int n = CODE_LABEL_NUMBER (label);

  if (label_align && n >= min_labelno && n <= max_labelno)
    return MAX (label_align[n - min_labelno], align_labels.levels[0].log);
  return align_labels.levels[0].log;
}

/* Implement ADJUST_REG_ALLOC_ORDER.  We use the default ordering
   for R1 and non-CDX R2 code; for CDX we tweak thing to prefer
   the registers that can be used as operands to instructions that
   have 3-bit register fields.  */
void
nios2_adjust_reg_alloc_order (void)
{
  const int cdx_reg_alloc_order[] =
    {
      /* Call-clobbered GPRs within CDX 3-bit encoded range.  */
      2, 3, 4, 5, 6, 7, 
      /* Call-saved GPRs within CDX 3-bit encoded range.  */
      16, 17,
      /* Other call-clobbered GPRs.  */
      8, 9, 10, 11, 12, 13, 14, 15,
      /* Other call-saved GPRs. RA placed first since it is always saved.  */
      31, 18, 19, 20, 21, 22, 23, 28,
      /* Fixed GPRs, not used by the register allocator.  */
      0, 1, 24, 25, 26, 27, 29, 30, 32, 33, 34, 35, 36, 37, 38, 39
   };

  if (TARGET_HAS_CDX)
    memcpy (reg_alloc_order, cdx_reg_alloc_order,
	    sizeof (int) * FIRST_PSEUDO_REGISTER);
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

#undef TARGET_FUNCTION_OK_FOR_SIBCALL
#define TARGET_FUNCTION_OK_FOR_SIBCALL hook_bool_tree_tree_true

#undef TARGET_CAN_ELIMINATE
#define TARGET_CAN_ELIMINATE nios2_can_eliminate

#undef TARGET_FUNCTION_ARG
#define TARGET_FUNCTION_ARG nios2_function_arg

#undef TARGET_FUNCTION_ARG_ADVANCE
#define TARGET_FUNCTION_ARG_ADVANCE nios2_function_arg_advance

#undef TARGET_FUNCTION_ARG_PADDING
#define TARGET_FUNCTION_ARG_PADDING nios2_function_arg_padding

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

#undef TARGET_DELEGITIMIZE_ADDRESS
#define TARGET_DELEGITIMIZE_ADDRESS nios2_delegitimize_address

#undef TARGET_LEGITIMATE_ADDRESS_P
#define TARGET_LEGITIMATE_ADDRESS_P nios2_legitimate_address_p

#undef TARGET_PREFERRED_RELOAD_CLASS
#define TARGET_PREFERRED_RELOAD_CLASS nios2_preferred_reload_class

#undef TARGET_RTX_COSTS
#define TARGET_RTX_COSTS nios2_rtx_costs

#undef TARGET_ADDRESS_COST
#define TARGET_ADDRESS_COST nios2_address_cost

#undef TARGET_HAVE_TLS
#define TARGET_HAVE_TLS TARGET_LINUX_ABI

#undef TARGET_CANNOT_FORCE_CONST_MEM
#define TARGET_CANNOT_FORCE_CONST_MEM nios2_cannot_force_const_mem

#undef TARGET_ASM_OUTPUT_DWARF_DTPREL
#define TARGET_ASM_OUTPUT_DWARF_DTPREL nios2_output_dwarf_dtprel

#undef TARGET_PRINT_OPERAND_PUNCT_VALID_P
#define TARGET_PRINT_OPERAND_PUNCT_VALID_P nios2_print_operand_punct_valid_p

#undef TARGET_PRINT_OPERAND
#define TARGET_PRINT_OPERAND nios2_print_operand

#undef TARGET_PRINT_OPERAND_ADDRESS
#define TARGET_PRINT_OPERAND_ADDRESS nios2_print_operand_address

#undef TARGET_ASM_OUTPUT_ADDR_CONST_EXTRA
#define TARGET_ASM_OUTPUT_ADDR_CONST_EXTRA nios2_output_addr_const_extra

#undef TARGET_ASM_FILE_END
#define TARGET_ASM_FILE_END nios2_asm_file_end

#undef TARGET_OPTION_OVERRIDE
#define TARGET_OPTION_OVERRIDE nios2_option_override

#undef TARGET_OPTION_SAVE
#define TARGET_OPTION_SAVE nios2_option_save

#undef TARGET_OPTION_RESTORE
#define TARGET_OPTION_RESTORE nios2_option_restore

#undef TARGET_CAN_INLINE_P
#define TARGET_CAN_INLINE_P nios2_can_inline_p

#undef TARGET_SET_CURRENT_FUNCTION
#define TARGET_SET_CURRENT_FUNCTION nios2_set_current_function

#undef TARGET_OPTION_VALID_ATTRIBUTE_P
#define TARGET_OPTION_VALID_ATTRIBUTE_P nios2_valid_target_attribute_p

#undef TARGET_OPTION_PRAGMA_PARSE
#define TARGET_OPTION_PRAGMA_PARSE nios2_pragma_target_parse

#undef TARGET_MERGE_DECL_ATTRIBUTES
#define TARGET_MERGE_DECL_ATTRIBUTES nios2_merge_decl_attributes

#undef  TARGET_ASM_CAN_OUTPUT_MI_THUNK
#define TARGET_ASM_CAN_OUTPUT_MI_THUNK \
  hook_bool_const_tree_hwi_hwi_const_tree_true

#undef  TARGET_ASM_OUTPUT_MI_THUNK
#define TARGET_ASM_OUTPUT_MI_THUNK nios2_asm_output_mi_thunk

#undef TARGET_MACHINE_DEPENDENT_REORG
#define TARGET_MACHINE_DEPENDENT_REORG nios2_reorg

#undef TARGET_CONSTANT_ALIGNMENT
#define TARGET_CONSTANT_ALIGNMENT constant_alignment_word_strings

#undef TARGET_HAVE_SPECULATION_SAFE_VALUE
#define TARGET_HAVE_SPECULATION_SAFE_VALUE speculation_safe_value_not_needed

struct gcc_target targetm = TARGET_INITIALIZER;

#include "gt-nios2.h"
