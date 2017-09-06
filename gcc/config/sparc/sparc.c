/* Subroutines for insn-output.c for SPARC.
   Copyright (C) 1987-2017 Free Software Foundation, Inc.
   Contributed by Michael Tiemann (tiemann@cygnus.com)
   64-bit SPARC-V9 support by Michael Tiemann, Jim Wilson, and Doug Evans,
   at Cygnus Support.

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

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "target.h"
#include "rtl.h"
#include "tree.h"
#include "memmodel.h"
#include "gimple.h"
#include "df.h"
#include "tm_p.h"
#include "stringpool.h"
#include "attribs.h"
#include "expmed.h"
#include "optabs.h"
#include "regs.h"
#include "emit-rtl.h"
#include "recog.h"
#include "diagnostic-core.h"
#include "alias.h"
#include "fold-const.h"
#include "stor-layout.h"
#include "calls.h"
#include "varasm.h"
#include "output.h"
#include "insn-attr.h"
#include "explow.h"
#include "expr.h"
#include "debug.h"
#include "common/common-target.h"
#include "gimplify.h"
#include "langhooks.h"
#include "reload.h"
#include "params.h"
#include "tree-pass.h"
#include "context.h"
#include "builtins.h"

/* This file should be included last.  */
#include "target-def.h"

/* Processor costs */

struct processor_costs {
  /* Integer load */
  const int int_load;

  /* Integer signed load */
  const int int_sload;

  /* Integer zeroed load */
  const int int_zload;

  /* Float load */
  const int float_load;

  /* fmov, fneg, fabs */
  const int float_move;

  /* fadd, fsub */
  const int float_plusminus;

  /* fcmp */
  const int float_cmp;

  /* fmov, fmovr */
  const int float_cmove;

  /* fmul */
  const int float_mul;

  /* fdivs */
  const int float_div_sf;

  /* fdivd */
  const int float_div_df;

  /* fsqrts */
  const int float_sqrt_sf;

  /* fsqrtd */
  const int float_sqrt_df;

  /* umul/smul */
  const int int_mul;

  /* mulX */
  const int int_mulX;

  /* integer multiply cost for each bit set past the most
     significant 3, so the formula for multiply cost becomes:

	if (rs1 < 0)
	  highest_bit = highest_clear_bit(rs1);
	else
	  highest_bit = highest_set_bit(rs1);
	if (highest_bit < 3)
	  highest_bit = 3;
	cost = int_mul{,X} + ((highest_bit - 3) / int_mul_bit_factor);

     A value of zero indicates that the multiply costs is fixed,
     and not variable.  */
  const int int_mul_bit_factor;

  /* udiv/sdiv */
  const int int_div;

  /* divX */
  const int int_divX;

  /* movcc, movr */
  const int int_cmove;

  /* penalty for shifts, due to scheduling rules etc. */
  const int shift_penalty;
};

static const
struct processor_costs cypress_costs = {
  COSTS_N_INSNS (2), /* int load */
  COSTS_N_INSNS (2), /* int signed load */
  COSTS_N_INSNS (2), /* int zeroed load */
  COSTS_N_INSNS (2), /* float load */
  COSTS_N_INSNS (5), /* fmov, fneg, fabs */
  COSTS_N_INSNS (5), /* fadd, fsub */
  COSTS_N_INSNS (1), /* fcmp */
  COSTS_N_INSNS (1), /* fmov, fmovr */
  COSTS_N_INSNS (7), /* fmul */
  COSTS_N_INSNS (37), /* fdivs */
  COSTS_N_INSNS (37), /* fdivd */
  COSTS_N_INSNS (63), /* fsqrts */
  COSTS_N_INSNS (63), /* fsqrtd */
  COSTS_N_INSNS (1), /* imul */
  COSTS_N_INSNS (1), /* imulX */
  0, /* imul bit factor */
  COSTS_N_INSNS (1), /* idiv */
  COSTS_N_INSNS (1), /* idivX */
  COSTS_N_INSNS (1), /* movcc/movr */
  0, /* shift penalty */
};

static const
struct processor_costs supersparc_costs = {
  COSTS_N_INSNS (1), /* int load */
  COSTS_N_INSNS (1), /* int signed load */
  COSTS_N_INSNS (1), /* int zeroed load */
  COSTS_N_INSNS (0), /* float load */
  COSTS_N_INSNS (3), /* fmov, fneg, fabs */
  COSTS_N_INSNS (3), /* fadd, fsub */
  COSTS_N_INSNS (3), /* fcmp */
  COSTS_N_INSNS (1), /* fmov, fmovr */
  COSTS_N_INSNS (3), /* fmul */
  COSTS_N_INSNS (6), /* fdivs */
  COSTS_N_INSNS (9), /* fdivd */
  COSTS_N_INSNS (12), /* fsqrts */
  COSTS_N_INSNS (12), /* fsqrtd */
  COSTS_N_INSNS (4), /* imul */
  COSTS_N_INSNS (4), /* imulX */
  0, /* imul bit factor */
  COSTS_N_INSNS (4), /* idiv */
  COSTS_N_INSNS (4), /* idivX */
  COSTS_N_INSNS (1), /* movcc/movr */
  1, /* shift penalty */
};

static const
struct processor_costs hypersparc_costs = {
  COSTS_N_INSNS (1), /* int load */
  COSTS_N_INSNS (1), /* int signed load */
  COSTS_N_INSNS (1), /* int zeroed load */
  COSTS_N_INSNS (1), /* float load */
  COSTS_N_INSNS (1), /* fmov, fneg, fabs */
  COSTS_N_INSNS (1), /* fadd, fsub */
  COSTS_N_INSNS (1), /* fcmp */
  COSTS_N_INSNS (1), /* fmov, fmovr */
  COSTS_N_INSNS (1), /* fmul */
  COSTS_N_INSNS (8), /* fdivs */
  COSTS_N_INSNS (12), /* fdivd */
  COSTS_N_INSNS (17), /* fsqrts */
  COSTS_N_INSNS (17), /* fsqrtd */
  COSTS_N_INSNS (17), /* imul */
  COSTS_N_INSNS (17), /* imulX */
  0, /* imul bit factor */
  COSTS_N_INSNS (17), /* idiv */
  COSTS_N_INSNS (17), /* idivX */
  COSTS_N_INSNS (1), /* movcc/movr */
  0, /* shift penalty */
};

static const
struct processor_costs leon_costs = {
  COSTS_N_INSNS (1), /* int load */
  COSTS_N_INSNS (1), /* int signed load */
  COSTS_N_INSNS (1), /* int zeroed load */
  COSTS_N_INSNS (1), /* float load */
  COSTS_N_INSNS (1), /* fmov, fneg, fabs */
  COSTS_N_INSNS (1), /* fadd, fsub */
  COSTS_N_INSNS (1), /* fcmp */
  COSTS_N_INSNS (1), /* fmov, fmovr */
  COSTS_N_INSNS (1), /* fmul */
  COSTS_N_INSNS (15), /* fdivs */
  COSTS_N_INSNS (15), /* fdivd */
  COSTS_N_INSNS (23), /* fsqrts */
  COSTS_N_INSNS (23), /* fsqrtd */
  COSTS_N_INSNS (5), /* imul */
  COSTS_N_INSNS (5), /* imulX */
  0, /* imul bit factor */
  COSTS_N_INSNS (5), /* idiv */
  COSTS_N_INSNS (5), /* idivX */
  COSTS_N_INSNS (1), /* movcc/movr */
  0, /* shift penalty */
};

static const
struct processor_costs leon3_costs = {
  COSTS_N_INSNS (1), /* int load */
  COSTS_N_INSNS (1), /* int signed load */
  COSTS_N_INSNS (1), /* int zeroed load */
  COSTS_N_INSNS (1), /* float load */
  COSTS_N_INSNS (1), /* fmov, fneg, fabs */
  COSTS_N_INSNS (1), /* fadd, fsub */
  COSTS_N_INSNS (1), /* fcmp */
  COSTS_N_INSNS (1), /* fmov, fmovr */
  COSTS_N_INSNS (1), /* fmul */
  COSTS_N_INSNS (14), /* fdivs */
  COSTS_N_INSNS (15), /* fdivd */
  COSTS_N_INSNS (22), /* fsqrts */
  COSTS_N_INSNS (23), /* fsqrtd */
  COSTS_N_INSNS (5), /* imul */
  COSTS_N_INSNS (5), /* imulX */
  0, /* imul bit factor */
  COSTS_N_INSNS (35), /* idiv */
  COSTS_N_INSNS (35), /* idivX */
  COSTS_N_INSNS (1), /* movcc/movr */
  0, /* shift penalty */
};

static const
struct processor_costs sparclet_costs = {
  COSTS_N_INSNS (3), /* int load */
  COSTS_N_INSNS (3), /* int signed load */
  COSTS_N_INSNS (1), /* int zeroed load */
  COSTS_N_INSNS (1), /* float load */
  COSTS_N_INSNS (1), /* fmov, fneg, fabs */
  COSTS_N_INSNS (1), /* fadd, fsub */
  COSTS_N_INSNS (1), /* fcmp */
  COSTS_N_INSNS (1), /* fmov, fmovr */
  COSTS_N_INSNS (1), /* fmul */
  COSTS_N_INSNS (1), /* fdivs */
  COSTS_N_INSNS (1), /* fdivd */
  COSTS_N_INSNS (1), /* fsqrts */
  COSTS_N_INSNS (1), /* fsqrtd */
  COSTS_N_INSNS (5), /* imul */
  COSTS_N_INSNS (5), /* imulX */
  0, /* imul bit factor */
  COSTS_N_INSNS (5), /* idiv */
  COSTS_N_INSNS (5), /* idivX */
  COSTS_N_INSNS (1), /* movcc/movr */
  0, /* shift penalty */
};

static const
struct processor_costs ultrasparc_costs = {
  COSTS_N_INSNS (2), /* int load */
  COSTS_N_INSNS (3), /* int signed load */
  COSTS_N_INSNS (2), /* int zeroed load */
  COSTS_N_INSNS (2), /* float load */
  COSTS_N_INSNS (1), /* fmov, fneg, fabs */
  COSTS_N_INSNS (4), /* fadd, fsub */
  COSTS_N_INSNS (1), /* fcmp */
  COSTS_N_INSNS (2), /* fmov, fmovr */
  COSTS_N_INSNS (4), /* fmul */
  COSTS_N_INSNS (13), /* fdivs */
  COSTS_N_INSNS (23), /* fdivd */
  COSTS_N_INSNS (13), /* fsqrts */
  COSTS_N_INSNS (23), /* fsqrtd */
  COSTS_N_INSNS (4), /* imul */
  COSTS_N_INSNS (4), /* imulX */
  2, /* imul bit factor */
  COSTS_N_INSNS (37), /* idiv */
  COSTS_N_INSNS (68), /* idivX */
  COSTS_N_INSNS (2), /* movcc/movr */
  2, /* shift penalty */
};

static const
struct processor_costs ultrasparc3_costs = {
  COSTS_N_INSNS (2), /* int load */
  COSTS_N_INSNS (3), /* int signed load */
  COSTS_N_INSNS (3), /* int zeroed load */
  COSTS_N_INSNS (2), /* float load */
  COSTS_N_INSNS (3), /* fmov, fneg, fabs */
  COSTS_N_INSNS (4), /* fadd, fsub */
  COSTS_N_INSNS (5), /* fcmp */
  COSTS_N_INSNS (3), /* fmov, fmovr */
  COSTS_N_INSNS (4), /* fmul */
  COSTS_N_INSNS (17), /* fdivs */
  COSTS_N_INSNS (20), /* fdivd */
  COSTS_N_INSNS (20), /* fsqrts */
  COSTS_N_INSNS (29), /* fsqrtd */
  COSTS_N_INSNS (6), /* imul */
  COSTS_N_INSNS (6), /* imulX */
  0, /* imul bit factor */
  COSTS_N_INSNS (40), /* idiv */
  COSTS_N_INSNS (71), /* idivX */
  COSTS_N_INSNS (2), /* movcc/movr */
  0, /* shift penalty */
};

static const
struct processor_costs niagara_costs = {
  COSTS_N_INSNS (3), /* int load */
  COSTS_N_INSNS (3), /* int signed load */
  COSTS_N_INSNS (3), /* int zeroed load */
  COSTS_N_INSNS (9), /* float load */
  COSTS_N_INSNS (8), /* fmov, fneg, fabs */
  COSTS_N_INSNS (8), /* fadd, fsub */
  COSTS_N_INSNS (26), /* fcmp */
  COSTS_N_INSNS (8), /* fmov, fmovr */
  COSTS_N_INSNS (29), /* fmul */
  COSTS_N_INSNS (54), /* fdivs */
  COSTS_N_INSNS (83), /* fdivd */
  COSTS_N_INSNS (100), /* fsqrts - not implemented in hardware */
  COSTS_N_INSNS (100), /* fsqrtd - not implemented in hardware */
  COSTS_N_INSNS (11), /* imul */
  COSTS_N_INSNS (11), /* imulX */
  0, /* imul bit factor */
  COSTS_N_INSNS (72), /* idiv */
  COSTS_N_INSNS (72), /* idivX */
  COSTS_N_INSNS (1), /* movcc/movr */
  0, /* shift penalty */
};

static const
struct processor_costs niagara2_costs = {
  COSTS_N_INSNS (3), /* int load */
  COSTS_N_INSNS (3), /* int signed load */
  COSTS_N_INSNS (3), /* int zeroed load */
  COSTS_N_INSNS (3), /* float load */
  COSTS_N_INSNS (6), /* fmov, fneg, fabs */
  COSTS_N_INSNS (6), /* fadd, fsub */
  COSTS_N_INSNS (6), /* fcmp */
  COSTS_N_INSNS (6), /* fmov, fmovr */
  COSTS_N_INSNS (6), /* fmul */
  COSTS_N_INSNS (19), /* fdivs */
  COSTS_N_INSNS (33), /* fdivd */
  COSTS_N_INSNS (19), /* fsqrts */
  COSTS_N_INSNS (33), /* fsqrtd */
  COSTS_N_INSNS (5), /* imul */
  COSTS_N_INSNS (5), /* imulX */
  0, /* imul bit factor */
  COSTS_N_INSNS (26), /* idiv, average of 12 - 41 cycle range */
  COSTS_N_INSNS (26), /* idivX, average of 12 - 41 cycle range */
  COSTS_N_INSNS (1), /* movcc/movr */
  0, /* shift penalty */
};

static const
struct processor_costs niagara3_costs = {
  COSTS_N_INSNS (3), /* int load */
  COSTS_N_INSNS (3), /* int signed load */
  COSTS_N_INSNS (3), /* int zeroed load */
  COSTS_N_INSNS (3), /* float load */
  COSTS_N_INSNS (9), /* fmov, fneg, fabs */
  COSTS_N_INSNS (9), /* fadd, fsub */
  COSTS_N_INSNS (9), /* fcmp */
  COSTS_N_INSNS (9), /* fmov, fmovr */
  COSTS_N_INSNS (9), /* fmul */
  COSTS_N_INSNS (23), /* fdivs */
  COSTS_N_INSNS (37), /* fdivd */
  COSTS_N_INSNS (23), /* fsqrts */
  COSTS_N_INSNS (37), /* fsqrtd */
  COSTS_N_INSNS (9), /* imul */
  COSTS_N_INSNS (9), /* imulX */
  0, /* imul bit factor */
  COSTS_N_INSNS (31), /* idiv, average of 17 - 45 cycle range */
  COSTS_N_INSNS (30), /* idivX, average of 16 - 44 cycle range */
  COSTS_N_INSNS (1), /* movcc/movr */
  0, /* shift penalty */
};

static const
struct processor_costs niagara4_costs = {
  COSTS_N_INSNS (5), /* int load */
  COSTS_N_INSNS (5), /* int signed load */
  COSTS_N_INSNS (5), /* int zeroed load */
  COSTS_N_INSNS (5), /* float load */
  COSTS_N_INSNS (11), /* fmov, fneg, fabs */
  COSTS_N_INSNS (11), /* fadd, fsub */
  COSTS_N_INSNS (11), /* fcmp */
  COSTS_N_INSNS (11), /* fmov, fmovr */
  COSTS_N_INSNS (11), /* fmul */
  COSTS_N_INSNS (24), /* fdivs */
  COSTS_N_INSNS (37), /* fdivd */
  COSTS_N_INSNS (24), /* fsqrts */
  COSTS_N_INSNS (37), /* fsqrtd */
  COSTS_N_INSNS (12), /* imul */
  COSTS_N_INSNS (12), /* imulX */
  0, /* imul bit factor */
  COSTS_N_INSNS (50), /* idiv, average of 41 - 60 cycle range */
  COSTS_N_INSNS (35), /* idivX, average of 26 - 44 cycle range */
  COSTS_N_INSNS (1), /* movcc/movr */
  0, /* shift penalty */
};

static const
struct processor_costs niagara7_costs = {
  COSTS_N_INSNS (5), /* int load */
  COSTS_N_INSNS (5), /* int signed load */
  COSTS_N_INSNS (5), /* int zeroed load */
  COSTS_N_INSNS (5), /* float load */
  COSTS_N_INSNS (11), /* fmov, fneg, fabs */
  COSTS_N_INSNS (11), /* fadd, fsub */
  COSTS_N_INSNS (11), /* fcmp */
  COSTS_N_INSNS (11), /* fmov, fmovr */
  COSTS_N_INSNS (11), /* fmul */
  COSTS_N_INSNS (24), /* fdivs */
  COSTS_N_INSNS (37), /* fdivd */
  COSTS_N_INSNS (24), /* fsqrts */
  COSTS_N_INSNS (37), /* fsqrtd */
  COSTS_N_INSNS (12), /* imul */
  COSTS_N_INSNS (12), /* imulX */
  0, /* imul bit factor */
  COSTS_N_INSNS (51), /* idiv, average of 42 - 61 cycle range */
  COSTS_N_INSNS (35), /* idivX, average of 26 - 44 cycle range */
  COSTS_N_INSNS (1), /* movcc/movr */
  0, /* shift penalty */
};

static const
struct processor_costs m8_costs = {
  COSTS_N_INSNS (3), /* int load */
  COSTS_N_INSNS (3), /* int signed load */
  COSTS_N_INSNS (3), /* int zeroed load */
  COSTS_N_INSNS (3), /* float load */
  COSTS_N_INSNS (9), /* fmov, fneg, fabs */
  COSTS_N_INSNS (9), /* fadd, fsub */
  COSTS_N_INSNS (9), /* fcmp */
  COSTS_N_INSNS (9), /* fmov, fmovr */
  COSTS_N_INSNS (9), /* fmul */
  COSTS_N_INSNS (26), /* fdivs */
  COSTS_N_INSNS (30), /* fdivd */
  COSTS_N_INSNS (33), /* fsqrts */
  COSTS_N_INSNS (41), /* fsqrtd */
  COSTS_N_INSNS (12), /* imul */
  COSTS_N_INSNS (10), /* imulX */
  0, /* imul bit factor */
  COSTS_N_INSNS (57), /* udiv/sdiv */
  COSTS_N_INSNS (30), /* udivx/sdivx */
  COSTS_N_INSNS (1), /* movcc/movr */
  0, /* shift penalty */
};

static const struct processor_costs *sparc_costs = &cypress_costs;

#ifdef HAVE_AS_RELAX_OPTION
/* If 'as' and 'ld' are relaxing tail call insns into branch always, use
   "or %o7,%g0,X; call Y; or X,%g0,%o7" always, so that it can be optimized.
   With sethi/jmp, neither 'as' nor 'ld' has an easy way how to find out if
   somebody does not branch between the sethi and jmp.  */
#define LEAF_SIBCALL_SLOT_RESERVED_P 1
#else
#define LEAF_SIBCALL_SLOT_RESERVED_P \
  ((TARGET_ARCH64 && !TARGET_CM_MEDLOW) || flag_pic)
#endif

/* Vector to say how input registers are mapped to output registers.
   HARD_FRAME_POINTER_REGNUM cannot be remapped by this function to
   eliminate it.  You must use -fomit-frame-pointer to get that.  */
char leaf_reg_remap[] =
{ 0, 1, 2, 3, 4, 5, 6, 7,
  -1, -1, -1, -1, -1, -1, 14, -1,
  -1, -1, -1, -1, -1, -1, -1, -1,
  8, 9, 10, 11, 12, 13, -1, 15,

  32, 33, 34, 35, 36, 37, 38, 39,
  40, 41, 42, 43, 44, 45, 46, 47,
  48, 49, 50, 51, 52, 53, 54, 55,
  56, 57, 58, 59, 60, 61, 62, 63,
  64, 65, 66, 67, 68, 69, 70, 71,
  72, 73, 74, 75, 76, 77, 78, 79,
  80, 81, 82, 83, 84, 85, 86, 87,
  88, 89, 90, 91, 92, 93, 94, 95,
  96, 97, 98, 99, 100, 101, 102};

/* Vector, indexed by hard register number, which contains 1
   for a register that is allowable in a candidate for leaf
   function treatment.  */
char sparc_leaf_regs[] =
{ 1, 1, 1, 1, 1, 1, 1, 1,
  0, 0, 0, 0, 0, 0, 1, 0,
  0, 0, 0, 0, 0, 0, 0, 0,
  1, 1, 1, 1, 1, 1, 0, 1,
  1, 1, 1, 1, 1, 1, 1, 1,
  1, 1, 1, 1, 1, 1, 1, 1,
  1, 1, 1, 1, 1, 1, 1, 1,
  1, 1, 1, 1, 1, 1, 1, 1,
  1, 1, 1, 1, 1, 1, 1, 1,
  1, 1, 1, 1, 1, 1, 1, 1,
  1, 1, 1, 1, 1, 1, 1, 1,
  1, 1, 1, 1, 1, 1, 1, 1,
  1, 1, 1, 1, 1, 1, 1};

struct GTY(()) machine_function
{
  /* Size of the frame of the function.  */
  HOST_WIDE_INT frame_size;

  /* Size of the frame of the function minus the register window save area
     and the outgoing argument area.  */
  HOST_WIDE_INT apparent_frame_size;

  /* Register we pretend the frame pointer is allocated to.  Normally, this
     is %fp, but if we are in a leaf procedure, this is (%sp + offset).  We
     record "offset" separately as it may be too big for (reg + disp).  */
  rtx frame_base_reg;
  HOST_WIDE_INT frame_base_offset;

  /* Number of global or FP registers to be saved (as 4-byte quantities).  */
  int n_global_fp_regs;

  /* True if the current function is leaf and uses only leaf regs,
     so that the SPARC leaf function optimization can be applied.
     Private version of crtl->uses_only_leaf_regs, see
     sparc_expand_prologue for the rationale.  */
  int leaf_function_p;

  /* True if the prologue saves local or in registers.  */
  bool save_local_in_regs_p;

  /* True if the data calculated by sparc_expand_prologue are valid.  */
  bool prologue_data_valid_p;
};

#define sparc_frame_size		cfun->machine->frame_size
#define sparc_apparent_frame_size	cfun->machine->apparent_frame_size
#define sparc_frame_base_reg		cfun->machine->frame_base_reg
#define sparc_frame_base_offset		cfun->machine->frame_base_offset
#define sparc_n_global_fp_regs		cfun->machine->n_global_fp_regs
#define sparc_leaf_function_p		cfun->machine->leaf_function_p
#define sparc_save_local_in_regs_p	cfun->machine->save_local_in_regs_p
#define sparc_prologue_data_valid_p	cfun->machine->prologue_data_valid_p

/* 1 if the next opcode is to be specially indented.  */
int sparc_indent_opcode = 0;

static void sparc_option_override (void);
static void sparc_init_modes (void);
static int function_arg_slotno (const CUMULATIVE_ARGS *, machine_mode,
				const_tree, bool, bool, int *, int *);

static int supersparc_adjust_cost (rtx_insn *, int, rtx_insn *, int);
static int hypersparc_adjust_cost (rtx_insn *, int, rtx_insn *, int);

static void sparc_emit_set_const32 (rtx, rtx);
static void sparc_emit_set_const64 (rtx, rtx);
static void sparc_output_addr_vec (rtx);
static void sparc_output_addr_diff_vec (rtx);
static void sparc_output_deferred_case_vectors (void);
static bool sparc_legitimate_address_p (machine_mode, rtx, bool);
static bool sparc_legitimate_constant_p (machine_mode, rtx);
static rtx sparc_builtin_saveregs (void);
static int epilogue_renumber (rtx *, int);
static bool sparc_assemble_integer (rtx, unsigned int, int);
static int set_extends (rtx_insn *);
static void sparc_asm_function_prologue (FILE *);
static void sparc_asm_function_epilogue (FILE *);
#ifdef TARGET_SOLARIS
static void sparc_solaris_elf_asm_named_section (const char *, unsigned int,
						 tree) ATTRIBUTE_UNUSED;
#endif
static int sparc_adjust_cost (rtx_insn *, int, rtx_insn *, int, unsigned int);
static int sparc_issue_rate (void);
static void sparc_sched_init (FILE *, int, int);
static int sparc_use_sched_lookahead (void);

static void emit_soft_tfmode_libcall (const char *, int, rtx *);
static void emit_soft_tfmode_binop (enum rtx_code, rtx *);
static void emit_soft_tfmode_unop (enum rtx_code, rtx *);
static void emit_soft_tfmode_cvt (enum rtx_code, rtx *);
static void emit_hard_tfmode_operation (enum rtx_code, rtx *);

static bool sparc_function_ok_for_sibcall (tree, tree);
static void sparc_init_libfuncs (void);
static void sparc_init_builtins (void);
static void sparc_fpu_init_builtins (void);
static void sparc_vis_init_builtins (void);
static tree sparc_builtin_decl (unsigned, bool);
static rtx sparc_expand_builtin (tree, rtx, rtx, machine_mode, int);
static tree sparc_fold_builtin (tree, int, tree *, bool);
static void sparc_output_mi_thunk (FILE *, tree, HOST_WIDE_INT,
				   HOST_WIDE_INT, tree);
static bool sparc_can_output_mi_thunk (const_tree, HOST_WIDE_INT,
				       HOST_WIDE_INT, const_tree);
static struct machine_function * sparc_init_machine_status (void);
static bool sparc_cannot_force_const_mem (machine_mode, rtx);
static rtx sparc_tls_get_addr (void);
static rtx sparc_tls_got (void);
static int sparc_register_move_cost (machine_mode,
				     reg_class_t, reg_class_t);
static bool sparc_rtx_costs (rtx, machine_mode, int, int, int *, bool);
static rtx sparc_function_value (const_tree, const_tree, bool);
static rtx sparc_libcall_value (machine_mode, const_rtx);
static bool sparc_function_value_regno_p (const unsigned int);
static rtx sparc_struct_value_rtx (tree, int);
static machine_mode sparc_promote_function_mode (const_tree, machine_mode,
						      int *, const_tree, int);
static bool sparc_return_in_memory (const_tree, const_tree);
static bool sparc_strict_argument_naming (cumulative_args_t);
static void sparc_va_start (tree, rtx);
static tree sparc_gimplify_va_arg (tree, tree, gimple_seq *, gimple_seq *);
static bool sparc_vector_mode_supported_p (machine_mode);
static bool sparc_tls_referenced_p (rtx);
static rtx sparc_legitimize_tls_address (rtx);
static rtx sparc_legitimize_pic_address (rtx, rtx);
static rtx sparc_legitimize_address (rtx, rtx, machine_mode);
static rtx sparc_delegitimize_address (rtx);
static bool sparc_mode_dependent_address_p (const_rtx, addr_space_t);
static bool sparc_pass_by_reference (cumulative_args_t,
				     machine_mode, const_tree, bool);
static void sparc_function_arg_advance (cumulative_args_t,
					machine_mode, const_tree, bool);
static rtx sparc_function_arg_1 (cumulative_args_t,
				 machine_mode, const_tree, bool, bool);
static rtx sparc_function_arg (cumulative_args_t,
			       machine_mode, const_tree, bool);
static rtx sparc_function_incoming_arg (cumulative_args_t,
					machine_mode, const_tree, bool);
static pad_direction sparc_function_arg_padding (machine_mode, const_tree);
static unsigned int sparc_function_arg_boundary (machine_mode,
						 const_tree);
static int sparc_arg_partial_bytes (cumulative_args_t,
				    machine_mode, tree, bool);
static void sparc_output_dwarf_dtprel (FILE *, int, rtx) ATTRIBUTE_UNUSED;
static void sparc_file_end (void);
static bool sparc_frame_pointer_required (void);
static bool sparc_can_eliminate (const int, const int);
static rtx sparc_builtin_setjmp_frame_value (void);
static void sparc_conditional_register_usage (void);
#ifdef TARGET_ALTERNATE_LONG_DOUBLE_MANGLING
static const char *sparc_mangle_type (const_tree);
#endif
static void sparc_trampoline_init (rtx, tree, rtx);
static machine_mode sparc_preferred_simd_mode (scalar_mode);
static reg_class_t sparc_preferred_reload_class (rtx x, reg_class_t rclass);
static bool sparc_lra_p (void);
static bool sparc_print_operand_punct_valid_p (unsigned char);
static void sparc_print_operand (FILE *, rtx, int);
static void sparc_print_operand_address (FILE *, machine_mode, rtx);
static reg_class_t sparc_secondary_reload (bool, rtx, reg_class_t,
					   machine_mode,
					   secondary_reload_info *);
static scalar_int_mode sparc_cstore_mode (enum insn_code icode);
static void sparc_atomic_assign_expand_fenv (tree *, tree *, tree *);
static bool sparc_fixed_condition_code_regs (unsigned int *, unsigned int *);
static unsigned int sparc_min_arithmetic_precision (void);
static bool sparc_hard_regno_mode_ok (unsigned int, machine_mode);
static bool sparc_modes_tieable_p (machine_mode, machine_mode);


#ifdef SUBTARGET_ATTRIBUTE_TABLE
/* Table of valid machine attributes.  */
static const struct attribute_spec sparc_attribute_table[] =
{
  /* { name, min_len, max_len, decl_req, type_req, fn_type_req, handler,
       do_diagnostic } */
  SUBTARGET_ATTRIBUTE_TABLE,
  { NULL,        0, 0, false, false, false, NULL, false }
};
#endif

/* Option handling.  */

/* Parsed value.  */
enum cmodel sparc_cmodel;

char sparc_hard_reg_printed[8];

/* Initialize the GCC target structure.  */

/* The default is to use .half rather than .short for aligned HI objects.  */
#undef TARGET_ASM_ALIGNED_HI_OP
#define TARGET_ASM_ALIGNED_HI_OP "\t.half\t"

#undef TARGET_ASM_UNALIGNED_HI_OP
#define TARGET_ASM_UNALIGNED_HI_OP "\t.uahalf\t"
#undef TARGET_ASM_UNALIGNED_SI_OP
#define TARGET_ASM_UNALIGNED_SI_OP "\t.uaword\t"
#undef TARGET_ASM_UNALIGNED_DI_OP
#define TARGET_ASM_UNALIGNED_DI_OP "\t.uaxword\t"

/* The target hook has to handle DI-mode values.  */
#undef TARGET_ASM_INTEGER
#define TARGET_ASM_INTEGER sparc_assemble_integer

#undef TARGET_ASM_FUNCTION_PROLOGUE
#define TARGET_ASM_FUNCTION_PROLOGUE sparc_asm_function_prologue
#undef TARGET_ASM_FUNCTION_EPILOGUE
#define TARGET_ASM_FUNCTION_EPILOGUE sparc_asm_function_epilogue

#undef TARGET_SCHED_ADJUST_COST
#define TARGET_SCHED_ADJUST_COST sparc_adjust_cost
#undef TARGET_SCHED_ISSUE_RATE
#define TARGET_SCHED_ISSUE_RATE sparc_issue_rate
#undef TARGET_SCHED_INIT
#define TARGET_SCHED_INIT sparc_sched_init
#undef TARGET_SCHED_FIRST_CYCLE_MULTIPASS_DFA_LOOKAHEAD
#define TARGET_SCHED_FIRST_CYCLE_MULTIPASS_DFA_LOOKAHEAD sparc_use_sched_lookahead

#undef TARGET_FUNCTION_OK_FOR_SIBCALL
#define TARGET_FUNCTION_OK_FOR_SIBCALL sparc_function_ok_for_sibcall

#undef TARGET_INIT_LIBFUNCS
#define TARGET_INIT_LIBFUNCS sparc_init_libfuncs

#undef TARGET_LEGITIMIZE_ADDRESS
#define TARGET_LEGITIMIZE_ADDRESS sparc_legitimize_address
#undef TARGET_DELEGITIMIZE_ADDRESS
#define TARGET_DELEGITIMIZE_ADDRESS sparc_delegitimize_address
#undef TARGET_MODE_DEPENDENT_ADDRESS_P
#define TARGET_MODE_DEPENDENT_ADDRESS_P sparc_mode_dependent_address_p

#undef TARGET_INIT_BUILTINS
#define TARGET_INIT_BUILTINS sparc_init_builtins
#undef TARGET_BUILTIN_DECL
#define TARGET_BUILTIN_DECL sparc_builtin_decl
#undef TARGET_EXPAND_BUILTIN
#define TARGET_EXPAND_BUILTIN sparc_expand_builtin
#undef TARGET_FOLD_BUILTIN
#define TARGET_FOLD_BUILTIN sparc_fold_builtin

#if TARGET_TLS
#undef TARGET_HAVE_TLS
#define TARGET_HAVE_TLS true
#endif

#undef TARGET_CANNOT_FORCE_CONST_MEM
#define TARGET_CANNOT_FORCE_CONST_MEM sparc_cannot_force_const_mem

#undef TARGET_ASM_OUTPUT_MI_THUNK
#define TARGET_ASM_OUTPUT_MI_THUNK sparc_output_mi_thunk
#undef TARGET_ASM_CAN_OUTPUT_MI_THUNK
#define TARGET_ASM_CAN_OUTPUT_MI_THUNK sparc_can_output_mi_thunk

#undef TARGET_RTX_COSTS
#define TARGET_RTX_COSTS sparc_rtx_costs
#undef TARGET_ADDRESS_COST
#define TARGET_ADDRESS_COST hook_int_rtx_mode_as_bool_0
#undef TARGET_REGISTER_MOVE_COST
#define TARGET_REGISTER_MOVE_COST sparc_register_move_cost

#undef TARGET_PROMOTE_FUNCTION_MODE
#define TARGET_PROMOTE_FUNCTION_MODE sparc_promote_function_mode

#undef TARGET_FUNCTION_VALUE
#define TARGET_FUNCTION_VALUE sparc_function_value
#undef TARGET_LIBCALL_VALUE
#define TARGET_LIBCALL_VALUE sparc_libcall_value
#undef TARGET_FUNCTION_VALUE_REGNO_P
#define TARGET_FUNCTION_VALUE_REGNO_P sparc_function_value_regno_p

#undef TARGET_STRUCT_VALUE_RTX
#define TARGET_STRUCT_VALUE_RTX sparc_struct_value_rtx
#undef TARGET_RETURN_IN_MEMORY
#define TARGET_RETURN_IN_MEMORY sparc_return_in_memory
#undef TARGET_MUST_PASS_IN_STACK
#define TARGET_MUST_PASS_IN_STACK must_pass_in_stack_var_size
#undef TARGET_PASS_BY_REFERENCE
#define TARGET_PASS_BY_REFERENCE sparc_pass_by_reference
#undef TARGET_ARG_PARTIAL_BYTES
#define TARGET_ARG_PARTIAL_BYTES sparc_arg_partial_bytes
#undef TARGET_FUNCTION_ARG_ADVANCE
#define TARGET_FUNCTION_ARG_ADVANCE sparc_function_arg_advance
#undef TARGET_FUNCTION_ARG
#define TARGET_FUNCTION_ARG sparc_function_arg
#undef TARGET_FUNCTION_INCOMING_ARG
#define TARGET_FUNCTION_INCOMING_ARG sparc_function_incoming_arg
#undef TARGET_FUNCTION_ARG_PADDING
#define TARGET_FUNCTION_ARG_PADDING sparc_function_arg_padding
#undef TARGET_FUNCTION_ARG_BOUNDARY
#define TARGET_FUNCTION_ARG_BOUNDARY sparc_function_arg_boundary

#undef TARGET_EXPAND_BUILTIN_SAVEREGS
#define TARGET_EXPAND_BUILTIN_SAVEREGS sparc_builtin_saveregs
#undef TARGET_STRICT_ARGUMENT_NAMING
#define TARGET_STRICT_ARGUMENT_NAMING sparc_strict_argument_naming

#undef TARGET_EXPAND_BUILTIN_VA_START
#define TARGET_EXPAND_BUILTIN_VA_START sparc_va_start
#undef TARGET_GIMPLIFY_VA_ARG_EXPR
#define TARGET_GIMPLIFY_VA_ARG_EXPR sparc_gimplify_va_arg

#undef TARGET_VECTOR_MODE_SUPPORTED_P
#define TARGET_VECTOR_MODE_SUPPORTED_P sparc_vector_mode_supported_p

#undef TARGET_VECTORIZE_PREFERRED_SIMD_MODE
#define TARGET_VECTORIZE_PREFERRED_SIMD_MODE sparc_preferred_simd_mode

#ifdef SUBTARGET_INSERT_ATTRIBUTES
#undef TARGET_INSERT_ATTRIBUTES
#define TARGET_INSERT_ATTRIBUTES SUBTARGET_INSERT_ATTRIBUTES
#endif

#ifdef SUBTARGET_ATTRIBUTE_TABLE
#undef TARGET_ATTRIBUTE_TABLE
#define TARGET_ATTRIBUTE_TABLE sparc_attribute_table
#endif

#undef TARGET_OPTION_OVERRIDE
#define TARGET_OPTION_OVERRIDE sparc_option_override

#ifdef TARGET_THREAD_SSP_OFFSET
#undef TARGET_STACK_PROTECT_GUARD
#define TARGET_STACK_PROTECT_GUARD hook_tree_void_null
#endif

#if TARGET_GNU_TLS && defined(HAVE_AS_SPARC_UA_PCREL)
#undef TARGET_ASM_OUTPUT_DWARF_DTPREL
#define TARGET_ASM_OUTPUT_DWARF_DTPREL sparc_output_dwarf_dtprel
#endif

#undef TARGET_ASM_FILE_END
#define TARGET_ASM_FILE_END sparc_file_end

#undef TARGET_FRAME_POINTER_REQUIRED
#define TARGET_FRAME_POINTER_REQUIRED sparc_frame_pointer_required

#undef TARGET_BUILTIN_SETJMP_FRAME_VALUE
#define TARGET_BUILTIN_SETJMP_FRAME_VALUE sparc_builtin_setjmp_frame_value

#undef TARGET_CAN_ELIMINATE
#define TARGET_CAN_ELIMINATE sparc_can_eliminate

#undef  TARGET_PREFERRED_RELOAD_CLASS
#define TARGET_PREFERRED_RELOAD_CLASS sparc_preferred_reload_class

#undef TARGET_SECONDARY_RELOAD
#define TARGET_SECONDARY_RELOAD sparc_secondary_reload

#undef TARGET_CONDITIONAL_REGISTER_USAGE
#define TARGET_CONDITIONAL_REGISTER_USAGE sparc_conditional_register_usage

#ifdef TARGET_ALTERNATE_LONG_DOUBLE_MANGLING
#undef TARGET_MANGLE_TYPE
#define TARGET_MANGLE_TYPE sparc_mangle_type
#endif

#undef TARGET_LRA_P
#define TARGET_LRA_P sparc_lra_p

#undef TARGET_LEGITIMATE_ADDRESS_P
#define TARGET_LEGITIMATE_ADDRESS_P sparc_legitimate_address_p

#undef TARGET_LEGITIMATE_CONSTANT_P
#define TARGET_LEGITIMATE_CONSTANT_P sparc_legitimate_constant_p

#undef TARGET_TRAMPOLINE_INIT
#define TARGET_TRAMPOLINE_INIT sparc_trampoline_init

#undef TARGET_PRINT_OPERAND_PUNCT_VALID_P
#define TARGET_PRINT_OPERAND_PUNCT_VALID_P sparc_print_operand_punct_valid_p
#undef TARGET_PRINT_OPERAND
#define TARGET_PRINT_OPERAND sparc_print_operand
#undef TARGET_PRINT_OPERAND_ADDRESS
#define TARGET_PRINT_OPERAND_ADDRESS sparc_print_operand_address

/* The value stored by LDSTUB.  */
#undef TARGET_ATOMIC_TEST_AND_SET_TRUEVAL
#define TARGET_ATOMIC_TEST_AND_SET_TRUEVAL 0xff

#undef TARGET_CSTORE_MODE
#define TARGET_CSTORE_MODE sparc_cstore_mode

#undef TARGET_ATOMIC_ASSIGN_EXPAND_FENV
#define TARGET_ATOMIC_ASSIGN_EXPAND_FENV sparc_atomic_assign_expand_fenv

#undef TARGET_FIXED_CONDITION_CODE_REGS
#define TARGET_FIXED_CONDITION_CODE_REGS sparc_fixed_condition_code_regs

#undef TARGET_MIN_ARITHMETIC_PRECISION
#define TARGET_MIN_ARITHMETIC_PRECISION sparc_min_arithmetic_precision

#undef TARGET_CUSTOM_FUNCTION_DESCRIPTORS
#define TARGET_CUSTOM_FUNCTION_DESCRIPTORS 1

#undef TARGET_HARD_REGNO_MODE_OK
#define TARGET_HARD_REGNO_MODE_OK sparc_hard_regno_mode_ok

#undef TARGET_MODES_TIEABLE_P
#define TARGET_MODES_TIEABLE_P sparc_modes_tieable_p

struct gcc_target targetm = TARGET_INITIALIZER;

/* Return the memory reference contained in X if any, zero otherwise.  */

static rtx
mem_ref (rtx x)
{
  if (GET_CODE (x) == SIGN_EXTEND || GET_CODE (x) == ZERO_EXTEND)
    x = XEXP (x, 0);

  if (MEM_P (x))
    return x;

  return NULL_RTX;
}

/* We use a machine specific pass to enable workarounds for errata.

   We need to have the (essentially) final form of the insn stream in order
   to properly detect the various hazards.  Therefore, this machine specific
   pass runs as late as possible.  */

/* True if INSN is a md pattern or asm statement.  */
#define USEFUL_INSN_P(INSN)						\
  (NONDEBUG_INSN_P (INSN)						\
   && GET_CODE (PATTERN (INSN)) != USE					\
   && GET_CODE (PATTERN (INSN)) != CLOBBER)

static unsigned int
sparc_do_work_around_errata (void)
{
  rtx_insn *insn, *next;

  /* Force all instructions to be split into their final form.  */
  split_all_insns_noflow ();

  /* Now look for specific patterns in the insn stream.  */
  for (insn = get_insns (); insn; insn = next)
    {
      bool insert_nop = false;
      rtx set;

      /* Look into the instruction in a delay slot.  */
      if (NONJUMP_INSN_P (insn))
	if (rtx_sequence *seq = dyn_cast <rtx_sequence *> (PATTERN (insn)))
	  insn = seq->insn (1);

      /* Look for either of these two sequences:

	 Sequence A:
	 1. store of word size or less (e.g. st / stb / sth / stf)
	 2. any single instruction that is not a load or store
	 3. any store instruction (e.g. st / stb / sth / stf / std / stdf)

	 Sequence B:
	 1. store of double word size (e.g. std / stdf)
	 2. any store instruction (e.g. st / stb / sth / stf / std / stdf)  */
      if (sparc_fix_b2bst
	  && NONJUMP_INSN_P (insn)
	  && (set = single_set (insn)) != NULL_RTX
	  && MEM_P (SET_DEST (set)))
	{
	  /* Sequence B begins with a double-word store.  */
	  bool seq_b = GET_MODE_SIZE (GET_MODE (SET_DEST (set))) == 8;
	  rtx_insn *after;
	  int i;

	  next = next_active_insn (insn);
	  if (!next)
	    break;

	  for (after = next, i = 0; i < 2; i++)
	    {
	      /* Skip empty assembly statements.  */
	      if ((GET_CODE (PATTERN (after)) == UNSPEC_VOLATILE)
		  || (USEFUL_INSN_P (after)
		      && (asm_noperands (PATTERN (after))>=0)
		      && !strcmp (decode_asm_operands (PATTERN (after),
						       NULL, NULL, NULL,
						       NULL, NULL), "")))
		after = next_active_insn (after);
	      if (!after)
		break;

	      /* If the insn is a branch, then it cannot be problematic.  */
	      if (!NONJUMP_INSN_P (after)
		  || GET_CODE (PATTERN (after)) == SEQUENCE)
		break;

	      /* Sequence B is only two instructions long.  */
	      if (seq_b)
		{
		  /* Add NOP if followed by a store.  */
		  if ((set = single_set (after)) != NULL_RTX
		      && MEM_P (SET_DEST (set)))
		    insert_nop = true;

		  /* Otherwise it is ok.  */
		  break;
		}

	      /* If the second instruction is a load or a store,
		 then the sequence cannot be problematic.  */
	      if (i == 0)
		{
		  if (((set = single_set (after)) != NULL_RTX)
		      && (MEM_P (SET_DEST (set)) || MEM_P (SET_SRC (set))))
		    break;

		  after = next_active_insn (after);
		  if (!after)
		    break;
		}

	      /* Add NOP if third instruction is a store.  */
	      if (i == 1
		  && ((set = single_set (after)) != NULL_RTX)
		  && MEM_P (SET_DEST (set)))
		insert_nop = true;
	    }
	}
      else
      /* Look for a single-word load into an odd-numbered FP register.  */
      if (sparc_fix_at697f
	  && NONJUMP_INSN_P (insn)
	  && (set = single_set (insn)) != NULL_RTX
	  && GET_MODE_SIZE (GET_MODE (SET_SRC (set))) == 4
	  && MEM_P (SET_SRC (set))
	  && REG_P (SET_DEST (set))
	  && REGNO (SET_DEST (set)) > 31
	  && REGNO (SET_DEST (set)) % 2 != 0)
	{
	  /* The wrong dependency is on the enclosing double register.  */
	  const unsigned int x = REGNO (SET_DEST (set)) - 1;
	  unsigned int src1, src2, dest;
	  int code;

	  next = next_active_insn (insn);
	  if (!next)
	    break;
	  /* If the insn is a branch, then it cannot be problematic.  */
	  if (!NONJUMP_INSN_P (next) || GET_CODE (PATTERN (next)) == SEQUENCE)
	    continue;

	  extract_insn (next);
	  code = INSN_CODE (next);

	  switch (code)
	    {
	    case CODE_FOR_adddf3:
	    case CODE_FOR_subdf3:
	    case CODE_FOR_muldf3:
	    case CODE_FOR_divdf3:
	      dest = REGNO (recog_data.operand[0]);
	      src1 = REGNO (recog_data.operand[1]);
	      src2 = REGNO (recog_data.operand[2]);
	      if (src1 != src2)
		{
		  /* Case [1-4]:
				 ld [address], %fx+1
				 FPOPd %f{x,y}, %f{y,x}, %f{x,y}  */
		  if ((src1 == x || src2 == x)
		      && (dest == src1 || dest == src2))
		    insert_nop = true;
		}
	      else
		{
		  /* Case 5:
			     ld [address], %fx+1
			     FPOPd %fx, %fx, %fx  */
		  if (src1 == x
		      && dest == src1
		      && (code == CODE_FOR_adddf3 || code == CODE_FOR_muldf3))
		    insert_nop = true;
		}
	      break;

	    case CODE_FOR_sqrtdf2:
	      dest = REGNO (recog_data.operand[0]);
	      src1 = REGNO (recog_data.operand[1]);
	      /* Case 6:
			 ld [address], %fx+1
			 fsqrtd %fx, %fx  */
	      if (src1 == x && dest == src1)
		insert_nop = true;
	      break;

	    default:
	      break;
	    }
	}

      /* Look for a single-word load into an integer register.  */
      else if (sparc_fix_ut699
	       && NONJUMP_INSN_P (insn)
	       && (set = single_set (insn)) != NULL_RTX
	       && GET_MODE_SIZE (GET_MODE (SET_SRC (set))) <= 4
	       && mem_ref (SET_SRC (set)) != NULL_RTX
	       && REG_P (SET_DEST (set))
	       && REGNO (SET_DEST (set)) < 32)
	{
	  /* There is no problem if the second memory access has a data
	     dependency on the first single-cycle load.  */
	  rtx x = SET_DEST (set);

	  next = next_active_insn (insn);
	  if (!next)
	    break;
	  /* If the insn is a branch, then it cannot be problematic.  */
	  if (!NONJUMP_INSN_P (next) || GET_CODE (PATTERN (next)) == SEQUENCE)
	    continue;

	  /* Look for a second memory access to/from an integer register.  */
	  if ((set = single_set (next)) != NULL_RTX)
	    {
	      rtx src = SET_SRC (set);
	      rtx dest = SET_DEST (set);
	      rtx mem;

	      /* LDD is affected.  */
	      if ((mem = mem_ref (src)) != NULL_RTX
		  && REG_P (dest)
		  && REGNO (dest) < 32
		  && !reg_mentioned_p (x, XEXP (mem, 0)))
		insert_nop = true;

	      /* STD is *not* affected.  */
	      else if (MEM_P (dest)
		       && GET_MODE_SIZE (GET_MODE (dest)) <= 4
		       && (src == CONST0_RTX (GET_MODE (dest))
			   || (REG_P (src)
			       && REGNO (src) < 32
			       && REGNO (src) != REGNO (x)))
		       && !reg_mentioned_p (x, XEXP (dest, 0)))
		insert_nop = true;
	    }
	}

      /* Look for a single-word load/operation into an FP register.  */
      else if (sparc_fix_ut699
	       && NONJUMP_INSN_P (insn)
	       && (set = single_set (insn)) != NULL_RTX
	       && GET_MODE_SIZE (GET_MODE (SET_SRC (set))) == 4
	       && REG_P (SET_DEST (set))
	       && REGNO (SET_DEST (set)) > 31)
	{
	  /* Number of instructions in the problematic window.  */
	  const int n_insns = 4;
	  /* The problematic combination is with the sibling FP register.  */
	  const unsigned int x = REGNO (SET_DEST (set));
	  const unsigned int y = x ^ 1;
	  rtx_insn *after;
	  int i;

	  next = next_active_insn (insn);
	  if (!next)
	    break;
	  /* If the insn is a branch, then it cannot be problematic.  */
	  if (!NONJUMP_INSN_P (next) || GET_CODE (PATTERN (next)) == SEQUENCE)
	    continue;

	  /* Look for a second load/operation into the sibling FP register.  */
	  if (!((set = single_set (next)) != NULL_RTX
		&& GET_MODE_SIZE (GET_MODE (SET_SRC (set))) == 4
		&& REG_P (SET_DEST (set))
		&& REGNO (SET_DEST (set)) == y))
	    continue;

	  /* Look for a (possible) store from the FP register in the next N
	     instructions, but bail out if it is again modified or if there
	     is a store from the sibling FP register before this store.  */
	  for (after = next, i = 0; i < n_insns; i++)
	    {
	      bool branch_p;

	      after = next_active_insn (after);
	      if (!after)
		break;

	      /* This is a branch with an empty delay slot.  */
	      if (!NONJUMP_INSN_P (after))
		{
		  if (++i == n_insns)
		    break;
		  branch_p = true;
		  after = NULL;
		}
	      /* This is a branch with a filled delay slot.  */
	      else if (rtx_sequence *seq =
		         dyn_cast <rtx_sequence *> (PATTERN (after)))
		{
		  if (++i == n_insns)
		    break;
		  branch_p = true;
		  after = seq->insn (1);
		}
	      /* This is a regular instruction.  */
	      else
		branch_p = false;

	      if (after && (set = single_set (after)) != NULL_RTX)
		{
		  const rtx src = SET_SRC (set);
		  const rtx dest = SET_DEST (set);
		  const unsigned int size = GET_MODE_SIZE (GET_MODE (dest));

		  /* If the FP register is again modified before the store,
		     then the store isn't affected.  */
		  if (REG_P (dest)
		      && (REGNO (dest) == x
			  || (REGNO (dest) == y && size == 8)))
		    break;

		  if (MEM_P (dest) && REG_P (src))
		    {
		      /* If there is a store from the sibling FP register
			 before the store, then the store is not affected.  */
		      if (REGNO (src) == y || (REGNO (src) == x && size == 8))
			break;

		      /* Otherwise, the store is affected.  */
		      if (REGNO (src) == x && size == 4)
			{
			  insert_nop = true;
			  break;
			}
		    }
		}

	      /* If we have a branch in the first M instructions, then we
		 cannot see the (M+2)th instruction so we play safe.  */
	      if (branch_p && i <= (n_insns - 2))
		{
		  insert_nop = true;
		  break;
		}
	    }
	}

      else
	next = NEXT_INSN (insn);

      if (insert_nop)
	emit_insn_before (gen_nop (), next);
    }

  return 0;
}

namespace {

const pass_data pass_data_work_around_errata =
{
  RTL_PASS, /* type */
  "errata", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  TV_MACH_DEP, /* tv_id */
  0, /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  0, /* todo_flags_finish */
};

class pass_work_around_errata : public rtl_opt_pass
{
public:
  pass_work_around_errata(gcc::context *ctxt)
    : rtl_opt_pass(pass_data_work_around_errata, ctxt)
  {}

  /* opt_pass methods: */
  virtual bool gate (function *)
    {
      return sparc_fix_at697f || sparc_fix_ut699 || sparc_fix_b2bst;
    }

  virtual unsigned int execute (function *)
    {
      return sparc_do_work_around_errata ();
    }

}; // class pass_work_around_errata

} // anon namespace

rtl_opt_pass *
make_pass_work_around_errata (gcc::context *ctxt)
{
  return new pass_work_around_errata (ctxt);
}

/* Helpers for TARGET_DEBUG_OPTIONS.  */
static void
dump_target_flag_bits (const int flags)
{
  if (flags & MASK_64BIT)
    fprintf (stderr, "64BIT ");
  if (flags & MASK_APP_REGS)
    fprintf (stderr, "APP_REGS ");
  if (flags & MASK_FASTER_STRUCTS)
    fprintf (stderr, "FASTER_STRUCTS ");
  if (flags & MASK_FLAT)
    fprintf (stderr, "FLAT ");
  if (flags & MASK_FMAF)
    fprintf (stderr, "FMAF ");
  if (flags & MASK_FSMULD)
    fprintf (stderr, "FSMULD ");
  if (flags & MASK_FPU)
    fprintf (stderr, "FPU ");
  if (flags & MASK_HARD_QUAD)
    fprintf (stderr, "HARD_QUAD ");
  if (flags & MASK_POPC)
    fprintf (stderr, "POPC ");
  if (flags & MASK_PTR64)
    fprintf (stderr, "PTR64 ");
  if (flags & MASK_STACK_BIAS)
    fprintf (stderr, "STACK_BIAS ");
  if (flags & MASK_UNALIGNED_DOUBLES)
    fprintf (stderr, "UNALIGNED_DOUBLES ");
  if (flags & MASK_V8PLUS)
    fprintf (stderr, "V8PLUS ");
  if (flags & MASK_VIS)
    fprintf (stderr, "VIS ");
  if (flags & MASK_VIS2)
    fprintf (stderr, "VIS2 ");
  if (flags & MASK_VIS3)
    fprintf (stderr, "VIS3 ");
  if (flags & MASK_VIS4)
    fprintf (stderr, "VIS4 ");
  if (flags & MASK_VIS4B)
    fprintf (stderr, "VIS4B ");
  if (flags & MASK_CBCOND)
    fprintf (stderr, "CBCOND ");
  if (flags & MASK_DEPRECATED_V8_INSNS)
    fprintf (stderr, "DEPRECATED_V8_INSNS ");
  if (flags & MASK_SPARCLET)
    fprintf (stderr, "SPARCLET ");
  if (flags & MASK_SPARCLITE)
    fprintf (stderr, "SPARCLITE ");
  if (flags & MASK_V8)
    fprintf (stderr, "V8 ");
  if (flags & MASK_V9)
    fprintf (stderr, "V9 ");
}

static void
dump_target_flags (const char *prefix, const int flags)
{
  fprintf (stderr, "%s: (%08x) [ ", prefix, flags);
  dump_target_flag_bits (flags);
  fprintf(stderr, "]\n");
}

/* Validate and override various options, and do some machine dependent
   initialization.  */

static void
sparc_option_override (void)
{
  static struct code_model {
    const char *const name;
    const enum cmodel value;
  } const cmodels[] = {
    { "32", CM_32 },
    { "medlow", CM_MEDLOW },
    { "medmid", CM_MEDMID },
    { "medany", CM_MEDANY },
    { "embmedany", CM_EMBMEDANY },
    { NULL, (enum cmodel) 0 }
  };
  const struct code_model *cmodel;
  /* Map TARGET_CPU_DEFAULT to value for -m{cpu,tune}=.  */
  static struct cpu_default {
    const int cpu;
    const enum processor_type processor;
  } const cpu_default[] = {
    /* There must be one entry here for each TARGET_CPU value.  */
    { TARGET_CPU_sparc, PROCESSOR_CYPRESS },
    { TARGET_CPU_v8, PROCESSOR_V8 },
    { TARGET_CPU_supersparc, PROCESSOR_SUPERSPARC },
    { TARGET_CPU_hypersparc, PROCESSOR_HYPERSPARC },
    { TARGET_CPU_leon, PROCESSOR_LEON },
    { TARGET_CPU_leon3, PROCESSOR_LEON3 },
    { TARGET_CPU_leon3v7, PROCESSOR_LEON3V7 },
    { TARGET_CPU_sparclite, PROCESSOR_F930 },
    { TARGET_CPU_sparclite86x, PROCESSOR_SPARCLITE86X },
    { TARGET_CPU_sparclet, PROCESSOR_TSC701 },
    { TARGET_CPU_v9, PROCESSOR_V9 },
    { TARGET_CPU_ultrasparc, PROCESSOR_ULTRASPARC },
    { TARGET_CPU_ultrasparc3, PROCESSOR_ULTRASPARC3 },
    { TARGET_CPU_niagara, PROCESSOR_NIAGARA },
    { TARGET_CPU_niagara2, PROCESSOR_NIAGARA2 },
    { TARGET_CPU_niagara3, PROCESSOR_NIAGARA3 },
    { TARGET_CPU_niagara4, PROCESSOR_NIAGARA4 },
    { TARGET_CPU_niagara7, PROCESSOR_NIAGARA7 },
    { TARGET_CPU_m8, PROCESSOR_M8 },
    { -1, PROCESSOR_V7 }
  };
  const struct cpu_default *def;
  /* Table of values for -m{cpu,tune}=.  This must match the order of
     the enum processor_type in sparc-opts.h.  */
  static struct cpu_table {
    const char *const name;
    const int disable;
    const int enable;
  } const cpu_table[] = {
    { "v7",		MASK_ISA|MASK_FSMULD, 0 },
    { "cypress",	MASK_ISA|MASK_FSMULD, 0 },
    { "v8",		MASK_ISA, MASK_V8 },
    /* TI TMS390Z55 supersparc */
    { "supersparc",	MASK_ISA, MASK_V8 },
    { "hypersparc",	MASK_ISA, MASK_V8 },
    { "leon",		MASK_ISA|MASK_FSMULD, MASK_V8|MASK_LEON },
    { "leon3",		MASK_ISA, MASK_V8|MASK_LEON3 },
    { "leon3v7",	MASK_ISA|MASK_FSMULD, MASK_LEON3 },
    { "sparclite",	MASK_ISA|MASK_FSMULD, MASK_SPARCLITE },
    /* The Fujitsu MB86930 is the original sparclite chip, with no FPU.  */
    { "f930",		MASK_ISA|MASK_FPU, MASK_SPARCLITE },
    /* The Fujitsu MB86934 is the recent sparclite chip, with an FPU.  */
    { "f934",		MASK_ISA|MASK_FSMULD, MASK_SPARCLITE },
    { "sparclite86x",	MASK_ISA|MASK_FPU, MASK_SPARCLITE },
    { "sparclet",	MASK_ISA|MASK_FSMULD, MASK_SPARCLET },
    /* TEMIC sparclet */
    { "tsc701",		MASK_ISA|MASK_FSMULD, MASK_SPARCLET },
    { "v9",		MASK_ISA, MASK_V9 },
    /* UltraSPARC I, II, IIi */
    { "ultrasparc",	MASK_ISA,
    /* Although insns using %y are deprecated, it is a clear win.  */
      MASK_V9|MASK_DEPRECATED_V8_INSNS },
    /* UltraSPARC III */
    /* ??? Check if %y issue still holds true.  */
    { "ultrasparc3",	MASK_ISA,
      MASK_V9|MASK_DEPRECATED_V8_INSNS|MASK_VIS2 },
    /* UltraSPARC T1 */
    { "niagara",	MASK_ISA,
      MASK_V9|MASK_DEPRECATED_V8_INSNS },
    /* UltraSPARC T2 */
    { "niagara2",	MASK_ISA,
      MASK_V9|MASK_POPC|MASK_VIS2 },
    /* UltraSPARC T3 */
    { "niagara3",	MASK_ISA,
      MASK_V9|MASK_POPC|MASK_VIS3|MASK_FMAF },
    /* UltraSPARC T4 */
    { "niagara4",	MASK_ISA,
      MASK_V9|MASK_POPC|MASK_VIS3|MASK_FMAF|MASK_CBCOND },
    /* UltraSPARC M7 */
    { "niagara7",	MASK_ISA,
      MASK_V9|MASK_POPC|MASK_VIS4|MASK_FMAF|MASK_CBCOND|MASK_SUBXC },
    /* UltraSPARC M8 */
    { "m8",		MASK_ISA,
      MASK_V9|MASK_POPC|MASK_VIS4|MASK_FMAF|MASK_CBCOND|MASK_SUBXC|MASK_VIS4B }
  };
  const struct cpu_table *cpu;
  unsigned int i;

  if (sparc_debug_string != NULL)
    {
      const char *q;
      char *p;

      p = ASTRDUP (sparc_debug_string);
      while ((q = strtok (p, ",")) != NULL)
	{
	  bool invert;
	  int mask;

	  p = NULL;
	  if (*q == '!')
	    {
	      invert = true;
	      q++;
	    }
	  else
	    invert = false;

	  if (! strcmp (q, "all"))
	    mask = MASK_DEBUG_ALL;
	  else if (! strcmp (q, "options"))
	    mask = MASK_DEBUG_OPTIONS;
	  else
	    error ("unknown -mdebug-%s switch", q);

	  if (invert)
	    sparc_debug &= ~mask;
	  else
	    sparc_debug |= mask;
	}
    }

  /* Enable the FsMULd instruction by default if not explicitly specified by
     the user.  It may be later disabled by the CPU (explicitly or not).  */
  if (TARGET_FPU && !(target_flags_explicit & MASK_FSMULD))
    target_flags |= MASK_FSMULD;

  if (TARGET_DEBUG_OPTIONS)
    {
      dump_target_flags("Initial target_flags", target_flags);
      dump_target_flags("target_flags_explicit", target_flags_explicit);
    }

#ifdef SUBTARGET_OVERRIDE_OPTIONS
  SUBTARGET_OVERRIDE_OPTIONS;
#endif

#ifndef SPARC_BI_ARCH
  /* Check for unsupported architecture size.  */
  if (!TARGET_64BIT != DEFAULT_ARCH32_P)
    error ("%s is not supported by this configuration",
	   DEFAULT_ARCH32_P ? "-m64" : "-m32");
#endif

  /* We force all 64bit archs to use 128 bit long double */
  if (TARGET_ARCH64 && !TARGET_LONG_DOUBLE_128)
    {
      error ("-mlong-double-64 not allowed with -m64");
      target_flags |= MASK_LONG_DOUBLE_128;
    }

  /* Code model selection.  */
  sparc_cmodel = SPARC_DEFAULT_CMODEL;

#ifdef SPARC_BI_ARCH
  if (TARGET_ARCH32)
    sparc_cmodel = CM_32;
#endif

  if (sparc_cmodel_string != NULL)
    {
      if (TARGET_ARCH64)
	{
	  for (cmodel = &cmodels[0]; cmodel->name; cmodel++)
	    if (strcmp (sparc_cmodel_string, cmodel->name) == 0)
	      break;
	  if (cmodel->name == NULL)
	    error ("bad value (%s) for -mcmodel= switch", sparc_cmodel_string);
	  else
	    sparc_cmodel = cmodel->value;
	}
      else
	error ("-mcmodel= is not supported on 32-bit systems");
    }

  /* Check that -fcall-saved-REG wasn't specified for out registers.  */
  for (i = 8; i < 16; i++)
    if (!call_used_regs [i])
      {
	error ("-fcall-saved-REG is not supported for out registers");
        call_used_regs [i] = 1;
      }

  /* Set the default CPU if no -mcpu option was specified.  */
  if (!global_options_set.x_sparc_cpu_and_features)
    {
      for (def = &cpu_default[0]; def->cpu != -1; ++def)
	if (def->cpu == TARGET_CPU_DEFAULT)
	  break;
      gcc_assert (def->cpu != -1);
      sparc_cpu_and_features = def->processor;
    }

  /* Set the default CPU if no -mtune option was specified.  */
  if (!global_options_set.x_sparc_cpu)
    sparc_cpu = sparc_cpu_and_features;

  cpu = &cpu_table[(int) sparc_cpu_and_features];

  if (TARGET_DEBUG_OPTIONS)
    {
      fprintf (stderr, "sparc_cpu_and_features: %s\n", cpu->name);
      dump_target_flags ("cpu->disable", cpu->disable);
      dump_target_flags ("cpu->enable", cpu->enable);
    }

  target_flags &= ~cpu->disable;
  target_flags |= (cpu->enable
#ifndef HAVE_AS_FMAF_HPC_VIS3
		   & ~(MASK_FMAF | MASK_VIS3)
#endif
#ifndef HAVE_AS_SPARC4
		   & ~MASK_CBCOND
#endif
#ifndef HAVE_AS_SPARC5_VIS4
		   & ~(MASK_VIS4 | MASK_SUBXC)
#endif
#ifndef HAVE_AS_SPARC6
		   & ~(MASK_VIS4B)
#endif
#ifndef HAVE_AS_LEON
		   & ~(MASK_LEON | MASK_LEON3)
#endif
		   & ~(target_flags_explicit & MASK_FEATURES)
		   );

  /* -mvis2 implies -mvis.  */
  if (TARGET_VIS2)
    target_flags |= MASK_VIS;

  /* -mvis3 implies -mvis2 and -mvis.  */
  if (TARGET_VIS3)
    target_flags |= MASK_VIS2 | MASK_VIS;

  /* -mvis4 implies -mvis3, -mvis2 and -mvis.  */
  if (TARGET_VIS4)
    target_flags |= MASK_VIS3 | MASK_VIS2 | MASK_VIS;

  /* -mvis4b implies -mvis4, -mvis3, -mvis2 and -mvis */
  if (TARGET_VIS4B)
    target_flags |= MASK_VIS4 | MASK_VIS3 | MASK_VIS2 | MASK_VIS;

  /* Don't allow -mvis, -mvis2, -mvis3, -mvis4, -mvis4b, -mfmaf and -mfsmuld if
     FPU is disabled.  */
  if (!TARGET_FPU)
    target_flags &= ~(MASK_VIS | MASK_VIS2 | MASK_VIS3 | MASK_VIS4
		      | MASK_VIS4B | MASK_FMAF | MASK_FSMULD);

  /* -mvis assumes UltraSPARC+, so we are sure v9 instructions
     are available; -m64 also implies v9.  */
  if (TARGET_VIS || TARGET_ARCH64)
    {
      target_flags |= MASK_V9;
      target_flags &= ~(MASK_V8 | MASK_SPARCLET | MASK_SPARCLITE);
    }

  /* -mvis also implies -mv8plus on 32-bit.  */
  if (TARGET_VIS && !TARGET_ARCH64)
    target_flags |= MASK_V8PLUS;

  /* Use the deprecated v8 insns for sparc64 in 32-bit mode.  */
  if (TARGET_V9 && TARGET_ARCH32)
    target_flags |= MASK_DEPRECATED_V8_INSNS;

  /* V8PLUS requires V9 and makes no sense in 64-bit mode.  */
  if (!TARGET_V9 || TARGET_ARCH64)
    target_flags &= ~MASK_V8PLUS;

  /* Don't use stack biasing in 32-bit mode.  */
  if (TARGET_ARCH32)
    target_flags &= ~MASK_STACK_BIAS;

  /* Use LRA instead of reload, unless otherwise instructed.  */
  if (!(target_flags_explicit & MASK_LRA))
    target_flags |= MASK_LRA;

  /* Enable the back-to-back store errata workaround for LEON3FT.  */
  if (sparc_fix_ut699 || sparc_fix_ut700 || sparc_fix_gr712rc)
    sparc_fix_b2bst = 1;

  /* Disable FsMULd for the UT699 since it doesn't work correctly.  */
  if (sparc_fix_ut699)
    target_flags &= ~MASK_FSMULD;

  /* Supply a default value for align_functions.  */
  if (align_functions == 0)
    {
      if (sparc_cpu == PROCESSOR_ULTRASPARC
	  || sparc_cpu == PROCESSOR_ULTRASPARC3
	  || sparc_cpu == PROCESSOR_NIAGARA
	  || sparc_cpu == PROCESSOR_NIAGARA2
	  || sparc_cpu == PROCESSOR_NIAGARA3
	  || sparc_cpu == PROCESSOR_NIAGARA4)
	align_functions = 32;
      else if (sparc_cpu == PROCESSOR_NIAGARA7
	       || sparc_cpu == PROCESSOR_M8)
	align_functions = 64;
    }

  /* Validate PCC_STRUCT_RETURN.  */
  if (flag_pcc_struct_return == DEFAULT_PCC_STRUCT_RETURN)
    flag_pcc_struct_return = (TARGET_ARCH64 ? 0 : 1);

  /* Only use .uaxword when compiling for a 64-bit target.  */
  if (!TARGET_ARCH64)
    targetm.asm_out.unaligned_op.di = NULL;

  /* Do various machine dependent initializations.  */
  sparc_init_modes ();

  /* Set up function hooks.  */
  init_machine_status = sparc_init_machine_status;

  switch (sparc_cpu)
    {
    case PROCESSOR_V7:
    case PROCESSOR_CYPRESS:
      sparc_costs = &cypress_costs;
      break;
    case PROCESSOR_V8:
    case PROCESSOR_SPARCLITE:
    case PROCESSOR_SUPERSPARC:
      sparc_costs = &supersparc_costs;
      break;
    case PROCESSOR_F930:
    case PROCESSOR_F934:
    case PROCESSOR_HYPERSPARC:
    case PROCESSOR_SPARCLITE86X:
      sparc_costs = &hypersparc_costs;
      break;
    case PROCESSOR_LEON:
      sparc_costs = &leon_costs;
      break;
    case PROCESSOR_LEON3:
    case PROCESSOR_LEON3V7:
      sparc_costs = &leon3_costs;
      break;
    case PROCESSOR_SPARCLET:
    case PROCESSOR_TSC701:
      sparc_costs = &sparclet_costs;
      break;
    case PROCESSOR_V9:
    case PROCESSOR_ULTRASPARC:
      sparc_costs = &ultrasparc_costs;
      break;
    case PROCESSOR_ULTRASPARC3:
      sparc_costs = &ultrasparc3_costs;
      break;
    case PROCESSOR_NIAGARA:
      sparc_costs = &niagara_costs;
      break;
    case PROCESSOR_NIAGARA2:
      sparc_costs = &niagara2_costs;
      break;
    case PROCESSOR_NIAGARA3:
      sparc_costs = &niagara3_costs;
      break;
    case PROCESSOR_NIAGARA4:
      sparc_costs = &niagara4_costs;
      break;
    case PROCESSOR_NIAGARA7:
      sparc_costs = &niagara7_costs;
      break;
    case PROCESSOR_M8:
      sparc_costs = &m8_costs;
      break;
    case PROCESSOR_NATIVE:
      gcc_unreachable ();
    };

  if (sparc_memory_model == SMM_DEFAULT)
    {
      /* Choose the memory model for the operating system.  */
      enum sparc_memory_model_type os_default = SUBTARGET_DEFAULT_MEMORY_MODEL;
      if (os_default != SMM_DEFAULT)
	sparc_memory_model = os_default;
      /* Choose the most relaxed model for the processor.  */
      else if (TARGET_V9)
	sparc_memory_model = SMM_RMO;
      else if (TARGET_LEON3)
	sparc_memory_model = SMM_TSO;
      else if (TARGET_LEON)
	sparc_memory_model = SMM_SC;
      else if (TARGET_V8)
	sparc_memory_model = SMM_PSO;
      else
	sparc_memory_model = SMM_SC;
    }

#ifdef TARGET_DEFAULT_LONG_DOUBLE_128
  if (!(target_flags_explicit & MASK_LONG_DOUBLE_128))
    target_flags |= MASK_LONG_DOUBLE_128;
#endif

  if (TARGET_DEBUG_OPTIONS)
    dump_target_flags ("Final target_flags", target_flags);

  /* PARAM_SIMULTANEOUS_PREFETCHES is the number of prefetches that
     can run at the same time.  More important, it is the threshold
     defining when additional prefetches will be dropped by the
     hardware.

     The UltraSPARC-III features a documented prefetch queue with a
     size of 8.  Additional prefetches issued in the cpu are
     dropped.

     Niagara processors are different.  In these processors prefetches
     are handled much like regular loads.  The L1 miss buffer is 32
     entries, but prefetches start getting affected when 30 entries
     become occupied.  That occupation could be a mix of regular loads
     and prefetches though.  And that buffer is shared by all threads.
     Once the threshold is reached, if the core is running a single
     thread the prefetch will retry.  If more than one thread is
     running, the prefetch will be dropped.

     All this makes it very difficult to determine how many
     simultaneous prefetches can be issued simultaneously, even in a
     single-threaded program.  Experimental results show that setting
     this parameter to 32 works well when the number of threads is not
     high.  */
  maybe_set_param_value (PARAM_SIMULTANEOUS_PREFETCHES,
			 ((sparc_cpu == PROCESSOR_ULTRASPARC
			   || sparc_cpu == PROCESSOR_NIAGARA
			   || sparc_cpu == PROCESSOR_NIAGARA2
			   || sparc_cpu == PROCESSOR_NIAGARA3
			   || sparc_cpu == PROCESSOR_NIAGARA4)
			  ? 2
			  : (sparc_cpu == PROCESSOR_ULTRASPARC3
			     ? 8 : ((sparc_cpu == PROCESSOR_NIAGARA7
				     || sparc_cpu == PROCESSOR_M8)
				    ? 32 : 3))),
			 global_options.x_param_values,
			 global_options_set.x_param_values);

  /* PARAM_L1_CACHE_LINE_SIZE is the size of the L1 cache line, in
     bytes.

     The Oracle SPARC Architecture (previously the UltraSPARC
     Architecture) specification states that when a PREFETCH[A]
     instruction is executed an implementation-specific amount of data
     is prefetched, and that it is at least 64 bytes long (aligned to
     at least 64 bytes).

     However, this is not correct.  The M7 (and implementations prior
     to that) does not guarantee a 64B prefetch into a cache if the
     line size is smaller.  A single cache line is all that is ever
     prefetched.  So for the M7, where the L1D$ has 32B lines and the
     L2D$ and L3 have 64B lines, a prefetch will prefetch 64B into the
     L2 and L3, but only 32B are brought into the L1D$. (Assuming it
     is a read_n prefetch, which is the only type which allocates to
     the L1.)  */
  maybe_set_param_value (PARAM_L1_CACHE_LINE_SIZE,
			 (sparc_cpu == PROCESSOR_M8
			  ? 64 : 32),
			 global_options.x_param_values,
			 global_options_set.x_param_values);

  /* PARAM_L1_CACHE_SIZE is the size of the L1D$ (most SPARC chips use
     Hardvard level-1 caches) in kilobytes.  Both UltraSPARC and
     Niagara processors feature a L1D$ of 16KB.  */
  maybe_set_param_value (PARAM_L1_CACHE_SIZE,
			 ((sparc_cpu == PROCESSOR_ULTRASPARC
			   || sparc_cpu == PROCESSOR_ULTRASPARC3
			   || sparc_cpu == PROCESSOR_NIAGARA
			   || sparc_cpu == PROCESSOR_NIAGARA2
			   || sparc_cpu == PROCESSOR_NIAGARA3
			   || sparc_cpu == PROCESSOR_NIAGARA4
			   || sparc_cpu == PROCESSOR_NIAGARA7
			   || sparc_cpu == PROCESSOR_M8)
			  ? 16 : 64),
			 global_options.x_param_values,
			 global_options_set.x_param_values);


  /* PARAM_L2_CACHE_SIZE is the size fo the L2 in kilobytes.  Note
     that 512 is the default in params.def.  */
  maybe_set_param_value (PARAM_L2_CACHE_SIZE,
			 ((sparc_cpu == PROCESSOR_NIAGARA4
			   || sparc_cpu == PROCESSOR_M8)
			  ? 128 : (sparc_cpu == PROCESSOR_NIAGARA7
				   ? 256 : 512)),
			 global_options.x_param_values,
			 global_options_set.x_param_values);
  

  /* Disable save slot sharing for call-clobbered registers by default.
     The IRA sharing algorithm works on single registers only and this
     pessimizes for double floating-point registers.  */
  if (!global_options_set.x_flag_ira_share_save_slots)
    flag_ira_share_save_slots = 0;

  /* Only enable REE by default in 64-bit mode where it helps to eliminate
     redundant 32-to-64-bit extensions.  */
  if (!global_options_set.x_flag_ree && TARGET_ARCH32)
    flag_ree = 0;
}

/* Miscellaneous utilities.  */

/* Nonzero if CODE, a comparison, is suitable for use in v9 conditional move
   or branch on register contents instructions.  */

int
v9_regcmp_p (enum rtx_code code)
{
  return (code == EQ || code == NE || code == GE || code == LT
	  || code == LE || code == GT);
}

/* Nonzero if OP is a floating point constant which can
   be loaded into an integer register using a single
   sethi instruction.  */

int
fp_sethi_p (rtx op)
{
  if (GET_CODE (op) == CONST_DOUBLE)
    {
      long i;

      REAL_VALUE_TO_TARGET_SINGLE (*CONST_DOUBLE_REAL_VALUE (op), i);
      return !SPARC_SIMM13_P (i) && SPARC_SETHI_P (i);
    }

  return 0;
}

/* Nonzero if OP is a floating point constant which can
   be loaded into an integer register using a single
   mov instruction.  */

int
fp_mov_p (rtx op)
{
  if (GET_CODE (op) == CONST_DOUBLE)
    {
      long i;

      REAL_VALUE_TO_TARGET_SINGLE (*CONST_DOUBLE_REAL_VALUE (op), i);
      return SPARC_SIMM13_P (i);
    }

  return 0;
}

/* Nonzero if OP is a floating point constant which can
   be loaded into an integer register using a high/losum
   instruction sequence.  */

int
fp_high_losum_p (rtx op)
{
  /* The constraints calling this should only be in
     SFmode move insns, so any constant which cannot
     be moved using a single insn will do.  */
  if (GET_CODE (op) == CONST_DOUBLE)
    {
      long i;

      REAL_VALUE_TO_TARGET_SINGLE (*CONST_DOUBLE_REAL_VALUE (op), i);
      return !SPARC_SIMM13_P (i) && !SPARC_SETHI_P (i);
    }

  return 0;
}

/* Return true if the address of LABEL can be loaded by means of the
   mov{si,di}_pic_label_ref patterns in PIC mode.  */

static bool
can_use_mov_pic_label_ref (rtx label)
{
  /* VxWorks does not impose a fixed gap between segments; the run-time
     gap can be different from the object-file gap.  We therefore can't
     assume X - _GLOBAL_OFFSET_TABLE_ is a link-time constant unless we
     are absolutely sure that X is in the same segment as the GOT.
     Unfortunately, the flexibility of linker scripts means that we
     can't be sure of that in general, so assume that GOT-relative
     accesses are never valid on VxWorks.  */
  if (TARGET_VXWORKS_RTP)
    return false;

  /* Similarly, if the label is non-local, it might end up being placed
     in a different section than the current one; now mov_pic_label_ref
     requires the label and the code to be in the same section.  */
  if (LABEL_REF_NONLOCAL_P (label))
    return false;

  /* Finally, if we are reordering basic blocks and partition into hot
     and cold sections, this might happen for any label.  */
  if (flag_reorder_blocks_and_partition)
    return false;

  return true;
}

/* Expand a move instruction.  Return true if all work is done.  */

bool
sparc_expand_move (machine_mode mode, rtx *operands)
{
  /* Handle sets of MEM first.  */
  if (GET_CODE (operands[0]) == MEM)
    {
      /* 0 is a register (or a pair of registers) on SPARC.  */
      if (register_or_zero_operand (operands[1], mode))
	return false;

      if (!reload_in_progress)
	{
	  operands[0] = validize_mem (operands[0]);
	  operands[1] = force_reg (mode, operands[1]);
	}
    }

  /* Fixup TLS cases.  */
  if (TARGET_HAVE_TLS
      && CONSTANT_P (operands[1])
      && sparc_tls_referenced_p (operands [1]))
    {
      operands[1] = sparc_legitimize_tls_address (operands[1]);
      return false;
    }

  /* Fixup PIC cases.  */
  if (flag_pic && CONSTANT_P (operands[1]))
    {
      if (pic_address_needs_scratch (operands[1]))
	operands[1] = sparc_legitimize_pic_address (operands[1], NULL_RTX);

      /* We cannot use the mov{si,di}_pic_label_ref patterns in all cases.  */
      if (GET_CODE (operands[1]) == LABEL_REF
	  && can_use_mov_pic_label_ref (operands[1]))
	{
	  if (mode == SImode)
	    {
	      emit_insn (gen_movsi_pic_label_ref (operands[0], operands[1]));
	      return true;
	    }

	  if (mode == DImode)
	    {
	      gcc_assert (TARGET_ARCH64);
	      emit_insn (gen_movdi_pic_label_ref (operands[0], operands[1]));
	      return true;
	    }
	}

      if (symbolic_operand (operands[1], mode))
	{
	  operands[1]
	    = sparc_legitimize_pic_address (operands[1],
					    reload_in_progress
					    ? operands[0] : NULL_RTX);
	  return false;
	}
    }

  /* If we are trying to toss an integer constant into FP registers,
     or loading a FP or vector constant, force it into memory.  */
  if (CONSTANT_P (operands[1])
      && REG_P (operands[0])
      && (SPARC_FP_REG_P (REGNO (operands[0]))
	  || SCALAR_FLOAT_MODE_P (mode)
	  || VECTOR_MODE_P (mode)))
    {
      /* emit_group_store will send such bogosity to us when it is
         not storing directly into memory.  So fix this up to avoid
         crashes in output_constant_pool.  */
      if (operands [1] == const0_rtx)
	operands[1] = CONST0_RTX (mode);

      /* We can clear or set to all-ones FP registers if TARGET_VIS, and
	 always other regs.  */
      if ((TARGET_VIS || REGNO (operands[0]) < SPARC_FIRST_FP_REG)
	  && (const_zero_operand (operands[1], mode)
	      || const_all_ones_operand (operands[1], mode)))
	return false;

      if (REGNO (operands[0]) < SPARC_FIRST_FP_REG
	  /* We are able to build any SF constant in integer registers
	     with at most 2 instructions.  */
	  && (mode == SFmode
	      /* And any DF constant in integer registers if needed.  */
	      || (mode == DFmode && !can_create_pseudo_p ())))
	return false;

      operands[1] = force_const_mem (mode, operands[1]);
      if (!reload_in_progress)
	operands[1] = validize_mem (operands[1]);
      return false;
    }

  /* Accept non-constants and valid constants unmodified.  */
  if (!CONSTANT_P (operands[1])
      || GET_CODE (operands[1]) == HIGH
      || input_operand (operands[1], mode))
    return false;

  switch (mode)
    {
    case E_QImode:
      /* All QImode constants require only one insn, so proceed.  */
      break;

    case E_HImode:
    case E_SImode:
      sparc_emit_set_const32 (operands[0], operands[1]);
      return true;

    case E_DImode:
      /* input_operand should have filtered out 32-bit mode.  */
      sparc_emit_set_const64 (operands[0], operands[1]);
      return true;

    case E_TImode:
      {
	rtx high, low;
	/* TImode isn't available in 32-bit mode.  */
	split_double (operands[1], &high, &low);
	emit_insn (gen_movdi (operand_subword (operands[0], 0, 0, TImode),
			      high));
	emit_insn (gen_movdi (operand_subword (operands[0], 1, 0, TImode),
			      low));
      }
      return true;

    default:
      gcc_unreachable ();
    }

  return false;
}

/* Load OP1, a 32-bit constant, into OP0, a register.
   We know it can't be done in one insn when we get
   here, the move expander guarantees this.  */

static void
sparc_emit_set_const32 (rtx op0, rtx op1)
{
  machine_mode mode = GET_MODE (op0);
  rtx temp = op0;

  if (can_create_pseudo_p ())
    temp = gen_reg_rtx (mode);

  if (GET_CODE (op1) == CONST_INT)
    {
      gcc_assert (!small_int_operand (op1, mode)
		  && !const_high_operand (op1, mode));

      /* Emit them as real moves instead of a HIGH/LO_SUM,
	 this way CSE can see everything and reuse intermediate
	 values if it wants.  */
      emit_insn (gen_rtx_SET (temp, GEN_INT (INTVAL (op1)
					     & ~(HOST_WIDE_INT) 0x3ff)));

      emit_insn (gen_rtx_SET (op0,
			      gen_rtx_IOR (mode, temp,
					   GEN_INT (INTVAL (op1) & 0x3ff))));
    }
  else
    {
      /* A symbol, emit in the traditional way.  */
      emit_insn (gen_rtx_SET (temp, gen_rtx_HIGH (mode, op1)));
      emit_insn (gen_rtx_SET (op0, gen_rtx_LO_SUM (mode, temp, op1)));
    }
}

/* Load OP1, a symbolic 64-bit constant, into OP0, a DImode register.
   If TEMP is nonzero, we are forbidden to use any other scratch
   registers.  Otherwise, we are allowed to generate them as needed.

   Note that TEMP may have TImode if the code model is TARGET_CM_MEDANY
   or TARGET_CM_EMBMEDANY (see the reload_indi and reload_outdi patterns).  */

void
sparc_emit_set_symbolic_const64 (rtx op0, rtx op1, rtx temp)
{
  rtx temp1, temp2, temp3, temp4, temp5;
  rtx ti_temp = 0;

  if (temp && GET_MODE (temp) == TImode)
    {
      ti_temp = temp;
      temp = gen_rtx_REG (DImode, REGNO (temp));
    }

  /* SPARC-V9 code-model support.  */
  switch (sparc_cmodel)
    {
    case CM_MEDLOW:
      /* The range spanned by all instructions in the object is less
	 than 2^31 bytes (2GB) and the distance from any instruction
	 to the location of the label _GLOBAL_OFFSET_TABLE_ is less
	 than 2^31 bytes (2GB).

	 The executable must be in the low 4TB of the virtual address
	 space.

	 sethi	%hi(symbol), %temp1
	 or	%temp1, %lo(symbol), %reg  */
      if (temp)
	temp1 = temp;  /* op0 is allowed.  */
      else
	temp1 = gen_reg_rtx (DImode);

      emit_insn (gen_rtx_SET (temp1, gen_rtx_HIGH (DImode, op1)));
      emit_insn (gen_rtx_SET (op0, gen_rtx_LO_SUM (DImode, temp1, op1)));
      break;

    case CM_MEDMID:
      /* The range spanned by all instructions in the object is less
	 than 2^31 bytes (2GB) and the distance from any instruction
	 to the location of the label _GLOBAL_OFFSET_TABLE_ is less
	 than 2^31 bytes (2GB).

	 The executable must be in the low 16TB of the virtual address
	 space.

	 sethi	%h44(symbol), %temp1
	 or	%temp1, %m44(symbol), %temp2
	 sllx	%temp2, 12, %temp3
	 or	%temp3, %l44(symbol), %reg  */
      if (temp)
	{
	  temp1 = op0;
	  temp2 = op0;
	  temp3 = temp;  /* op0 is allowed.  */
	}
      else
	{
	  temp1 = gen_reg_rtx (DImode);
	  temp2 = gen_reg_rtx (DImode);
	  temp3 = gen_reg_rtx (DImode);
	}

      emit_insn (gen_seth44 (temp1, op1));
      emit_insn (gen_setm44 (temp2, temp1, op1));
      emit_insn (gen_rtx_SET (temp3,
			      gen_rtx_ASHIFT (DImode, temp2, GEN_INT (12))));
      emit_insn (gen_setl44 (op0, temp3, op1));
      break;

    case CM_MEDANY:
      /* The range spanned by all instructions in the object is less
	 than 2^31 bytes (2GB) and the distance from any instruction
	 to the location of the label _GLOBAL_OFFSET_TABLE_ is less
	 than 2^31 bytes (2GB).

	 The executable can be placed anywhere in the virtual address
	 space.

	 sethi	%hh(symbol), %temp1
	 sethi	%lm(symbol), %temp2
	 or	%temp1, %hm(symbol), %temp3
	 sllx	%temp3, 32, %temp4
	 or	%temp4, %temp2, %temp5
	 or	%temp5, %lo(symbol), %reg  */
      if (temp)
	{
	  /* It is possible that one of the registers we got for operands[2]
	     might coincide with that of operands[0] (which is why we made
	     it TImode).  Pick the other one to use as our scratch.  */
	  if (rtx_equal_p (temp, op0))
	    {
	      gcc_assert (ti_temp);
	      temp = gen_rtx_REG (DImode, REGNO (temp) + 1);
	    }
	  temp1 = op0;
	  temp2 = temp;  /* op0 is _not_ allowed, see above.  */
	  temp3 = op0;
	  temp4 = op0;
	  temp5 = op0;
	}
      else
	{
	  temp1 = gen_reg_rtx (DImode);
	  temp2 = gen_reg_rtx (DImode);
	  temp3 = gen_reg_rtx (DImode);
	  temp4 = gen_reg_rtx (DImode);
	  temp5 = gen_reg_rtx (DImode);
	}

      emit_insn (gen_sethh (temp1, op1));
      emit_insn (gen_setlm (temp2, op1));
      emit_insn (gen_sethm (temp3, temp1, op1));
      emit_insn (gen_rtx_SET (temp4,
			      gen_rtx_ASHIFT (DImode, temp3, GEN_INT (32))));
      emit_insn (gen_rtx_SET (temp5, gen_rtx_PLUS (DImode, temp4, temp2)));
      emit_insn (gen_setlo (op0, temp5, op1));
      break;

    case CM_EMBMEDANY:
      /* Old old old backwards compatibility kruft here.
	 Essentially it is MEDLOW with a fixed 64-bit
	 virtual base added to all data segment addresses.
	 Text-segment stuff is computed like MEDANY, we can't
	 reuse the code above because the relocation knobs
	 look different.

	 Data segment:	sethi	%hi(symbol), %temp1
			add	%temp1, EMBMEDANY_BASE_REG, %temp2
			or	%temp2, %lo(symbol), %reg  */
      if (data_segment_operand (op1, GET_MODE (op1)))
	{
	  if (temp)
	    {
	      temp1 = temp;  /* op0 is allowed.  */
	      temp2 = op0;
	    }
	  else
	    {
	      temp1 = gen_reg_rtx (DImode);
	      temp2 = gen_reg_rtx (DImode);
	    }

	  emit_insn (gen_embmedany_sethi (temp1, op1));
	  emit_insn (gen_embmedany_brsum (temp2, temp1));
	  emit_insn (gen_embmedany_losum (op0, temp2, op1));
	}

      /* Text segment:	sethi	%uhi(symbol), %temp1
			sethi	%hi(symbol), %temp2
			or	%temp1, %ulo(symbol), %temp3
			sllx	%temp3, 32, %temp4
			or	%temp4, %temp2, %temp5
			or	%temp5, %lo(symbol), %reg  */
      else
	{
	  if (temp)
	    {
	      /* It is possible that one of the registers we got for operands[2]
		 might coincide with that of operands[0] (which is why we made
		 it TImode).  Pick the other one to use as our scratch.  */
	      if (rtx_equal_p (temp, op0))
		{
		  gcc_assert (ti_temp);
		  temp = gen_rtx_REG (DImode, REGNO (temp) + 1);
		}
	      temp1 = op0;
	      temp2 = temp;  /* op0 is _not_ allowed, see above.  */
	      temp3 = op0;
	      temp4 = op0;
	      temp5 = op0;
	    }
	  else
	    {
	      temp1 = gen_reg_rtx (DImode);
	      temp2 = gen_reg_rtx (DImode);
	      temp3 = gen_reg_rtx (DImode);
	      temp4 = gen_reg_rtx (DImode);
	      temp5 = gen_reg_rtx (DImode);
	    }

	  emit_insn (gen_embmedany_textuhi (temp1, op1));
	  emit_insn (gen_embmedany_texthi  (temp2, op1));
	  emit_insn (gen_embmedany_textulo (temp3, temp1, op1));
	  emit_insn (gen_rtx_SET (temp4,
				  gen_rtx_ASHIFT (DImode, temp3, GEN_INT (32))));
	  emit_insn (gen_rtx_SET (temp5, gen_rtx_PLUS (DImode, temp4, temp2)));
	  emit_insn (gen_embmedany_textlo  (op0, temp5, op1));
	}
      break;

    default:
      gcc_unreachable ();
    }
}

/* These avoid problems when cross compiling.  If we do not
   go through all this hair then the optimizer will see
   invalid REG_EQUAL notes or in some cases none at all.  */
static rtx gen_safe_HIGH64 (rtx, HOST_WIDE_INT);
static rtx gen_safe_SET64 (rtx, HOST_WIDE_INT);
static rtx gen_safe_OR64 (rtx, HOST_WIDE_INT);
static rtx gen_safe_XOR64 (rtx, HOST_WIDE_INT);

/* The optimizer is not to assume anything about exactly
   which bits are set for a HIGH, they are unspecified.
   Unfortunately this leads to many missed optimizations
   during CSE.  We mask out the non-HIGH bits, and matches
   a plain movdi, to alleviate this problem.  */
static rtx
gen_safe_HIGH64 (rtx dest, HOST_WIDE_INT val)
{
  return gen_rtx_SET (dest, GEN_INT (val & ~(HOST_WIDE_INT)0x3ff));
}

static rtx
gen_safe_SET64 (rtx dest, HOST_WIDE_INT val)
{
  return gen_rtx_SET (dest, GEN_INT (val));
}

static rtx
gen_safe_OR64 (rtx src, HOST_WIDE_INT val)
{
  return gen_rtx_IOR (DImode, src, GEN_INT (val));
}

static rtx
gen_safe_XOR64 (rtx src, HOST_WIDE_INT val)
{
  return gen_rtx_XOR (DImode, src, GEN_INT (val));
}

/* Worker routines for 64-bit constant formation on arch64.
   One of the key things to be doing in these emissions is
   to create as many temp REGs as possible.  This makes it
   possible for half-built constants to be used later when
   such values are similar to something required later on.
   Without doing this, the optimizer cannot see such
   opportunities.  */

static void sparc_emit_set_const64_quick1 (rtx, rtx,
					   unsigned HOST_WIDE_INT, int);

static void
sparc_emit_set_const64_quick1 (rtx op0, rtx temp,
			       unsigned HOST_WIDE_INT low_bits, int is_neg)
{
  unsigned HOST_WIDE_INT high_bits;

  if (is_neg)
    high_bits = (~low_bits) & 0xffffffff;
  else
    high_bits = low_bits;

  emit_insn (gen_safe_HIGH64 (temp, high_bits));
  if (!is_neg)
    {
      emit_insn (gen_rtx_SET (op0, gen_safe_OR64 (temp, (high_bits & 0x3ff))));
    }
  else
    {
      /* If we are XOR'ing with -1, then we should emit a one's complement
	 instead.  This way the combiner will notice logical operations
	 such as ANDN later on and substitute.  */
      if ((low_bits & 0x3ff) == 0x3ff)
	{
	  emit_insn (gen_rtx_SET (op0, gen_rtx_NOT (DImode, temp)));
	}
      else
	{
	  emit_insn (gen_rtx_SET (op0,
				  gen_safe_XOR64 (temp,
						  (-(HOST_WIDE_INT)0x400
						   | (low_bits & 0x3ff)))));
	}
    }
}

static void sparc_emit_set_const64_quick2 (rtx, rtx, unsigned HOST_WIDE_INT,
					   unsigned HOST_WIDE_INT, int);

static void
sparc_emit_set_const64_quick2 (rtx op0, rtx temp,
			       unsigned HOST_WIDE_INT high_bits,
			       unsigned HOST_WIDE_INT low_immediate,
			       int shift_count)
{
  rtx temp2 = op0;

  if ((high_bits & 0xfffffc00) != 0)
    {
      emit_insn (gen_safe_HIGH64 (temp, high_bits));
      if ((high_bits & ~0xfffffc00) != 0)
	emit_insn (gen_rtx_SET (op0,
				gen_safe_OR64 (temp, (high_bits & 0x3ff))));
      else
	temp2 = temp;
    }
  else
    {
      emit_insn (gen_safe_SET64 (temp, high_bits));
      temp2 = temp;
    }

  /* Now shift it up into place.  */
  emit_insn (gen_rtx_SET (op0, gen_rtx_ASHIFT (DImode, temp2,
					       GEN_INT (shift_count))));

  /* If there is a low immediate part piece, finish up by
     putting that in as well.  */
  if (low_immediate != 0)
    emit_insn (gen_rtx_SET (op0, gen_safe_OR64 (op0, low_immediate)));
}

static void sparc_emit_set_const64_longway (rtx, rtx, unsigned HOST_WIDE_INT,
					    unsigned HOST_WIDE_INT);

/* Full 64-bit constant decomposition.  Even though this is the
   'worst' case, we still optimize a few things away.  */
static void
sparc_emit_set_const64_longway (rtx op0, rtx temp,
				unsigned HOST_WIDE_INT high_bits,
				unsigned HOST_WIDE_INT low_bits)
{
  rtx sub_temp = op0;

  if (can_create_pseudo_p ())
    sub_temp = gen_reg_rtx (DImode);

  if ((high_bits & 0xfffffc00) != 0)
    {
      emit_insn (gen_safe_HIGH64 (temp, high_bits));
      if ((high_bits & ~0xfffffc00) != 0)
	emit_insn (gen_rtx_SET (sub_temp,
				gen_safe_OR64 (temp, (high_bits & 0x3ff))));
      else
	sub_temp = temp;
    }
  else
    {
      emit_insn (gen_safe_SET64 (temp, high_bits));
      sub_temp = temp;
    }

  if (can_create_pseudo_p ())
    {
      rtx temp2 = gen_reg_rtx (DImode);
      rtx temp3 = gen_reg_rtx (DImode);
      rtx temp4 = gen_reg_rtx (DImode);

      emit_insn (gen_rtx_SET (temp4, gen_rtx_ASHIFT (DImode, sub_temp,
						     GEN_INT (32))));

      emit_insn (gen_safe_HIGH64 (temp2, low_bits));
      if ((low_bits & ~0xfffffc00) != 0)
	{
	  emit_insn (gen_rtx_SET (temp3,
				  gen_safe_OR64 (temp2, (low_bits & 0x3ff))));
	  emit_insn (gen_rtx_SET (op0, gen_rtx_PLUS (DImode, temp4, temp3)));
	}
      else
	{
	  emit_insn (gen_rtx_SET (op0, gen_rtx_PLUS (DImode, temp4, temp2)));
	}
    }
  else
    {
      rtx low1 = GEN_INT ((low_bits >> (32 - 12))          & 0xfff);
      rtx low2 = GEN_INT ((low_bits >> (32 - 12 - 12))     & 0xfff);
      rtx low3 = GEN_INT ((low_bits >> (32 - 12 - 12 - 8)) & 0x0ff);
      int to_shift = 12;

      /* We are in the middle of reload, so this is really
	 painful.  However we do still make an attempt to
	 avoid emitting truly stupid code.  */
      if (low1 != const0_rtx)
	{
	  emit_insn (gen_rtx_SET (op0, gen_rtx_ASHIFT (DImode, sub_temp,
						       GEN_INT (to_shift))));
	  emit_insn (gen_rtx_SET (op0, gen_rtx_IOR (DImode, op0, low1)));
	  sub_temp = op0;
	  to_shift = 12;
	}
      else
	{
	  to_shift += 12;
	}
      if (low2 != const0_rtx)
	{
	  emit_insn (gen_rtx_SET (op0, gen_rtx_ASHIFT (DImode, sub_temp,
						       GEN_INT (to_shift))));
	  emit_insn (gen_rtx_SET (op0, gen_rtx_IOR (DImode, op0, low2)));
	  sub_temp = op0;
	  to_shift = 8;
	}
      else
	{
	  to_shift += 8;
	}
      emit_insn (gen_rtx_SET (op0, gen_rtx_ASHIFT (DImode, sub_temp,
						   GEN_INT (to_shift))));
      if (low3 != const0_rtx)
	emit_insn (gen_rtx_SET (op0, gen_rtx_IOR (DImode, op0, low3)));
      /* phew...  */
    }
}

/* Analyze a 64-bit constant for certain properties.  */
static void analyze_64bit_constant (unsigned HOST_WIDE_INT,
				    unsigned HOST_WIDE_INT,
				    int *, int *, int *);

static void
analyze_64bit_constant (unsigned HOST_WIDE_INT high_bits,
			unsigned HOST_WIDE_INT low_bits,
			int *hbsp, int *lbsp, int *abbasp)
{
  int lowest_bit_set, highest_bit_set, all_bits_between_are_set;
  int i;

  lowest_bit_set = highest_bit_set = -1;
  i = 0;
  do
    {
      if ((lowest_bit_set == -1)
	  && ((low_bits >> i) & 1))
	lowest_bit_set = i;
      if ((highest_bit_set == -1)
	  && ((high_bits >> (32 - i - 1)) & 1))
	highest_bit_set = (64 - i - 1);
    }
  while (++i < 32
	 && ((highest_bit_set == -1)
	     || (lowest_bit_set == -1)));
  if (i == 32)
    {
      i = 0;
      do
	{
	  if ((lowest_bit_set == -1)
	      && ((high_bits >> i) & 1))
	    lowest_bit_set = i + 32;
	  if ((highest_bit_set == -1)
	      && ((low_bits >> (32 - i - 1)) & 1))
	    highest_bit_set = 32 - i - 1;
	}
      while (++i < 32
	     && ((highest_bit_set == -1)
		 || (lowest_bit_set == -1)));
    }
  /* If there are no bits set this should have gone out
     as one instruction!  */
  gcc_assert (lowest_bit_set != -1 && highest_bit_set != -1);
  all_bits_between_are_set = 1;
  for (i = lowest_bit_set; i <= highest_bit_set; i++)
    {
      if (i < 32)
	{
	  if ((low_bits & (1 << i)) != 0)
	    continue;
	}
      else
	{
	  if ((high_bits & (1 << (i - 32))) != 0)
	    continue;
	}
      all_bits_between_are_set = 0;
      break;
    }
  *hbsp = highest_bit_set;
  *lbsp = lowest_bit_set;
  *abbasp = all_bits_between_are_set;
}

static int const64_is_2insns (unsigned HOST_WIDE_INT, unsigned HOST_WIDE_INT);

static int
const64_is_2insns (unsigned HOST_WIDE_INT high_bits,
		   unsigned HOST_WIDE_INT low_bits)
{
  int highest_bit_set, lowest_bit_set, all_bits_between_are_set;

  if (high_bits == 0
      || high_bits == 0xffffffff)
    return 1;

  analyze_64bit_constant (high_bits, low_bits,
			  &highest_bit_set, &lowest_bit_set,
			  &all_bits_between_are_set);

  if ((highest_bit_set == 63
       || lowest_bit_set == 0)
      && all_bits_between_are_set != 0)
    return 1;

  if ((highest_bit_set - lowest_bit_set) < 21)
    return 1;

  return 0;
}

static unsigned HOST_WIDE_INT create_simple_focus_bits (unsigned HOST_WIDE_INT,
							unsigned HOST_WIDE_INT,
							int, int);

static unsigned HOST_WIDE_INT
create_simple_focus_bits (unsigned HOST_WIDE_INT high_bits,
			  unsigned HOST_WIDE_INT low_bits,
			  int lowest_bit_set, int shift)
{
  HOST_WIDE_INT hi, lo;

  if (lowest_bit_set < 32)
    {
      lo = (low_bits >> lowest_bit_set) << shift;
      hi = ((high_bits << (32 - lowest_bit_set)) << shift);
    }
  else
    {
      lo = 0;
      hi = ((high_bits >> (lowest_bit_set - 32)) << shift);
    }
  gcc_assert (! (hi & lo));
  return (hi | lo);
}

/* Here we are sure to be arch64 and this is an integer constant
   being loaded into a register.  Emit the most efficient
   insn sequence possible.  Detection of all the 1-insn cases
   has been done already.  */
static void
sparc_emit_set_const64 (rtx op0, rtx op1)
{
  unsigned HOST_WIDE_INT high_bits, low_bits;
  int lowest_bit_set, highest_bit_set;
  int all_bits_between_are_set;
  rtx temp = 0;

  /* Sanity check that we know what we are working with.  */
  gcc_assert (TARGET_ARCH64
	      && (GET_CODE (op0) == SUBREG
		  || (REG_P (op0) && ! SPARC_FP_REG_P (REGNO (op0)))));

  if (! can_create_pseudo_p ())
    temp = op0;

  if (GET_CODE (op1) != CONST_INT)
    {
      sparc_emit_set_symbolic_const64 (op0, op1, temp);
      return;
    }

  if (! temp)
    temp = gen_reg_rtx (DImode);

  high_bits = ((INTVAL (op1) >> 32) & 0xffffffff);
  low_bits = (INTVAL (op1) & 0xffffffff);

  /* low_bits	bits 0  --> 31
     high_bits	bits 32 --> 63  */

  analyze_64bit_constant (high_bits, low_bits,
			  &highest_bit_set, &lowest_bit_set,
			  &all_bits_between_are_set);

  /* First try for a 2-insn sequence.  */

  /* These situations are preferred because the optimizer can
   * do more things with them:
   * 1) mov	-1, %reg
   *    sllx	%reg, shift, %reg
   * 2) mov	-1, %reg
   *    srlx	%reg, shift, %reg
   * 3) mov	some_small_const, %reg
   *    sllx	%reg, shift, %reg
   */
  if (((highest_bit_set == 63
	|| lowest_bit_set == 0)
       && all_bits_between_are_set != 0)
      || ((highest_bit_set - lowest_bit_set) < 12))
    {
      HOST_WIDE_INT the_const = -1;
      int shift = lowest_bit_set;

      if ((highest_bit_set != 63
	   && lowest_bit_set != 0)
	  || all_bits_between_are_set == 0)
	{
	  the_const =
	    create_simple_focus_bits (high_bits, low_bits,
				      lowest_bit_set, 0);
	}
      else if (lowest_bit_set == 0)
	shift = -(63 - highest_bit_set);

      gcc_assert (SPARC_SIMM13_P (the_const));
      gcc_assert (shift != 0);

      emit_insn (gen_safe_SET64 (temp, the_const));
      if (shift > 0)
	emit_insn (gen_rtx_SET (op0, gen_rtx_ASHIFT (DImode, temp,
						     GEN_INT (shift))));
      else if (shift < 0)
	emit_insn (gen_rtx_SET (op0, gen_rtx_LSHIFTRT (DImode, temp,
						       GEN_INT (-shift))));
      return;
    }

  /* Now a range of 22 or less bits set somewhere.
   * 1) sethi	%hi(focus_bits), %reg
   *    sllx	%reg, shift, %reg
   * 2) sethi	%hi(focus_bits), %reg
   *    srlx	%reg, shift, %reg
   */
  if ((highest_bit_set - lowest_bit_set) < 21)
    {
      unsigned HOST_WIDE_INT focus_bits =
	create_simple_focus_bits (high_bits, low_bits,
				  lowest_bit_set, 10);

      gcc_assert (SPARC_SETHI_P (focus_bits));
      gcc_assert (lowest_bit_set != 10);

      emit_insn (gen_safe_HIGH64 (temp, focus_bits));

      /* If lowest_bit_set == 10 then a sethi alone could have done it.  */
      if (lowest_bit_set < 10)
	emit_insn (gen_rtx_SET (op0,
				gen_rtx_LSHIFTRT (DImode, temp,
						  GEN_INT (10 - lowest_bit_set))));
      else if (lowest_bit_set > 10)
	emit_insn (gen_rtx_SET (op0,
				gen_rtx_ASHIFT (DImode, temp,
						GEN_INT (lowest_bit_set - 10))));
      return;
    }

  /* 1) sethi	%hi(low_bits), %reg
   *    or	%reg, %lo(low_bits), %reg
   * 2) sethi	%hi(~low_bits), %reg
   *	xor	%reg, %lo(-0x400 | (low_bits & 0x3ff)), %reg
   */
  if (high_bits == 0
      || high_bits == 0xffffffff)
    {
      sparc_emit_set_const64_quick1 (op0, temp, low_bits,
				     (high_bits == 0xffffffff));
      return;
    }

  /* Now, try 3-insn sequences.  */

  /* 1) sethi	%hi(high_bits), %reg
   *    or	%reg, %lo(high_bits), %reg
   *    sllx	%reg, 32, %reg
   */
  if (low_bits == 0)
    {
      sparc_emit_set_const64_quick2 (op0, temp, high_bits, 0, 32);
      return;
    }

  /* We may be able to do something quick
     when the constant is negated, so try that.  */
  if (const64_is_2insns ((~high_bits) & 0xffffffff,
			 (~low_bits) & 0xfffffc00))
    {
      /* NOTE: The trailing bits get XOR'd so we need the
	 non-negated bits, not the negated ones.  */
      unsigned HOST_WIDE_INT trailing_bits = low_bits & 0x3ff;

      if ((((~high_bits) & 0xffffffff) == 0
	   && ((~low_bits) & 0x80000000) == 0)
	  || (((~high_bits) & 0xffffffff) == 0xffffffff
	      && ((~low_bits) & 0x80000000) != 0))
	{
	  unsigned HOST_WIDE_INT fast_int = (~low_bits & 0xffffffff);

	  if ((SPARC_SETHI_P (fast_int)
	       && (~high_bits & 0xffffffff) == 0)
	      || SPARC_SIMM13_P (fast_int))
	    emit_insn (gen_safe_SET64 (temp, fast_int));
	  else
	    sparc_emit_set_const64 (temp, GEN_INT (fast_int));
	}
      else
	{
	  rtx negated_const;
	  negated_const = GEN_INT (((~low_bits) & 0xfffffc00) |
				   (((HOST_WIDE_INT)((~high_bits) & 0xffffffff))<<32));
	  sparc_emit_set_const64 (temp, negated_const);
	}

      /* If we are XOR'ing with -1, then we should emit a one's complement
	 instead.  This way the combiner will notice logical operations
	 such as ANDN later on and substitute.  */
      if (trailing_bits == 0x3ff)
	{
	  emit_insn (gen_rtx_SET (op0, gen_rtx_NOT (DImode, temp)));
	}
      else
	{
	  emit_insn (gen_rtx_SET (op0,
				  gen_safe_XOR64 (temp,
						  (-0x400 | trailing_bits))));
	}
      return;
    }

  /* 1) sethi	%hi(xxx), %reg
   *    or	%reg, %lo(xxx), %reg
   *	sllx	%reg, yyy, %reg
   *
   * ??? This is just a generalized version of the low_bits==0
   * thing above, FIXME...
   */
  if ((highest_bit_set - lowest_bit_set) < 32)
    {
      unsigned HOST_WIDE_INT focus_bits =
	create_simple_focus_bits (high_bits, low_bits,
				  lowest_bit_set, 0);

      /* We can't get here in this state.  */
      gcc_assert (highest_bit_set >= 32 && lowest_bit_set < 32);

      /* So what we know is that the set bits straddle the
	 middle of the 64-bit word.  */
      sparc_emit_set_const64_quick2 (op0, temp,
				     focus_bits, 0,
				     lowest_bit_set);
      return;
    }

  /* 1) sethi	%hi(high_bits), %reg
   *    or	%reg, %lo(high_bits), %reg
   *    sllx	%reg, 32, %reg
   *	or	%reg, low_bits, %reg
   */
  if (SPARC_SIMM13_P (low_bits) && ((int)low_bits > 0))
    {
      sparc_emit_set_const64_quick2 (op0, temp, high_bits, low_bits, 32);
      return;
    }

  /* The easiest way when all else fails, is full decomposition.  */
  sparc_emit_set_const64_longway (op0, temp, high_bits, low_bits);
}

/* Implement TARGET_FIXED_CONDITION_CODE_REGS.  */

static bool
sparc_fixed_condition_code_regs (unsigned int *p1, unsigned int *p2)
{
  *p1 = SPARC_ICC_REG;
  *p2 = SPARC_FCC_REG;
  return true;
}

/* Implement TARGET_MIN_ARITHMETIC_PRECISION.  */

static unsigned int
sparc_min_arithmetic_precision (void)
{
  return 32;
}

/* Given a comparison code (EQ, NE, etc.) and the first operand of a COMPARE,
   return the mode to be used for the comparison.  For floating-point,
   CCFP[E]mode is used.  CCNZmode should be used when the first operand
   is a PLUS, MINUS, NEG, or ASHIFT.  CCmode should be used when no special
   processing is needed.  */

machine_mode
select_cc_mode (enum rtx_code op, rtx x, rtx y)
{
  if (GET_MODE_CLASS (GET_MODE (x)) == MODE_FLOAT)
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
	case LTGT:
	  return CCFPmode;

	case LT:
	case LE:
	case GT:
	case GE:
	  return CCFPEmode;

	default:
	  gcc_unreachable ();
	}
    }
  else if ((GET_CODE (x) == PLUS || GET_CODE (x) == MINUS
	    || GET_CODE (x) == NEG || GET_CODE (x) == ASHIFT)
	   && y == const0_rtx)
    {
      if (TARGET_ARCH64 && GET_MODE (x) == DImode)
	return CCXNZmode;
      else
	return CCNZmode;
    }
  else
    {
      /* This is for the cmp<mode>_sne pattern.  */
      if (GET_CODE (x) == NOT && y == constm1_rtx)
	{
	  if (TARGET_ARCH64 && GET_MODE (x) == DImode)
	    return CCXCmode;
	  else
	    return CCCmode;
	}

      /* This is for the [u]addvdi4_sp32 and [u]subvdi4_sp32 patterns.  */
      if (!TARGET_ARCH64 && GET_MODE (x) == DImode)
	{
	  if (GET_CODE (y) == UNSPEC
	      && (XINT (y, 1) == UNSPEC_ADDV
		 || XINT (y, 1) == UNSPEC_SUBV
	         || XINT (y, 1) == UNSPEC_NEGV))
	    return CCVmode;
	  else
	    return CCCmode;
	}

      if (TARGET_ARCH64 && GET_MODE (x) == DImode)
	return CCXmode;
      else
	return CCmode;
    }
}

/* Emit the compare insn and return the CC reg for a CODE comparison
   with operands X and Y.  */

static rtx
gen_compare_reg_1 (enum rtx_code code, rtx x, rtx y)
{
  machine_mode mode;
  rtx cc_reg;

  if (GET_MODE_CLASS (GET_MODE (x)) == MODE_CC)
    return x;

  mode = SELECT_CC_MODE (code, x, y);

  /* ??? We don't have movcc patterns so we cannot generate pseudo regs for the
     fcc regs (cse can't tell they're really call clobbered regs and will
     remove a duplicate comparison even if there is an intervening function
     call - it will then try to reload the cc reg via an int reg which is why
     we need the movcc patterns).  It is possible to provide the movcc
     patterns by using the ldxfsr/stxfsr v9 insns.  I tried it: you need two
     registers (say %g1,%g5) and it takes about 6 insns.  A better fix would be
     to tell cse that CCFPE mode registers (even pseudos) are call
     clobbered.  */

  /* ??? This is an experiment.  Rather than making changes to cse which may
     or may not be easy/clean, we do our own cse.  This is possible because
     we will generate hard registers.  Cse knows they're call clobbered (it
     doesn't know the same thing about pseudos). If we guess wrong, no big
     deal, but if we win, great!  */

  if (TARGET_V9 && GET_MODE_CLASS (GET_MODE (x)) == MODE_FLOAT)
#if 1 /* experiment */
    {
      int reg;
      /* We cycle through the registers to ensure they're all exercised.  */
      static int next_fcc_reg = 0;
      /* Previous x,y for each fcc reg.  */
      static rtx prev_args[4][2];

      /* Scan prev_args for x,y.  */
      for (reg = 0; reg < 4; reg++)
	if (prev_args[reg][0] == x && prev_args[reg][1] == y)
	  break;
      if (reg == 4)
	{
	  reg = next_fcc_reg;
	  prev_args[reg][0] = x;
	  prev_args[reg][1] = y;
	  next_fcc_reg = (next_fcc_reg + 1) & 3;
	}
      cc_reg = gen_rtx_REG (mode, reg + SPARC_FIRST_V9_FCC_REG);
    }
#else
    cc_reg = gen_reg_rtx (mode);
#endif /* ! experiment */
  else if (GET_MODE_CLASS (GET_MODE (x)) == MODE_FLOAT)
    cc_reg = gen_rtx_REG (mode, SPARC_FCC_REG);
  else
    cc_reg = gen_rtx_REG (mode, SPARC_ICC_REG);

  /* We shouldn't get there for TFmode if !TARGET_HARD_QUAD.  If we do, this
     will only result in an unrecognizable insn so no point in asserting.  */
  emit_insn (gen_rtx_SET (cc_reg, gen_rtx_COMPARE (mode, x, y)));

  return cc_reg;
}


/* Emit the compare insn and return the CC reg for the comparison in CMP.  */

rtx
gen_compare_reg (rtx cmp)
{
  return gen_compare_reg_1 (GET_CODE (cmp), XEXP (cmp, 0), XEXP (cmp, 1));
}

/* This function is used for v9 only.
   DEST is the target of the Scc insn.
   CODE is the code for an Scc's comparison.
   X and Y are the values we compare.

   This function is needed to turn

	   (set (reg:SI 110)
	       (gt (reg:CCX 100 %icc)
	           (const_int 0)))
   into
	   (set (reg:SI 110)
	       (gt:DI (reg:CCX 100 %icc)
	           (const_int 0)))

   IE: The instruction recognizer needs to see the mode of the comparison to
   find the right instruction. We could use "gt:DI" right in the
   define_expand, but leaving it out allows us to handle DI, SI, etc.  */

static int
gen_v9_scc (rtx dest, enum rtx_code compare_code, rtx x, rtx y)
{
  if (! TARGET_ARCH64
      && (GET_MODE (x) == DImode
	  || GET_MODE (dest) == DImode))
    return 0;

  /* Try to use the movrCC insns.  */
  if (TARGET_ARCH64
      && GET_MODE_CLASS (GET_MODE (x)) == MODE_INT
      && y == const0_rtx
      && v9_regcmp_p (compare_code))
    {
      rtx op0 = x;
      rtx temp;

      /* Special case for op0 != 0.  This can be done with one instruction if
	 dest == x.  */

      if (compare_code == NE
	  && GET_MODE (dest) == DImode
	  && rtx_equal_p (op0, dest))
	{
	  emit_insn (gen_rtx_SET (dest,
			      gen_rtx_IF_THEN_ELSE (DImode,
				       gen_rtx_fmt_ee (compare_code, DImode,
						       op0, const0_rtx),
				       const1_rtx,
				       dest)));
	  return 1;
	}

      if (reg_overlap_mentioned_p (dest, op0))
	{
	  /* Handle the case where dest == x.
	     We "early clobber" the result.  */
	  op0 = gen_reg_rtx (GET_MODE (x));
	  emit_move_insn (op0, x);
	}

      emit_insn (gen_rtx_SET (dest, const0_rtx));
      if (GET_MODE (op0) != DImode)
	{
	  temp = gen_reg_rtx (DImode);
	  convert_move (temp, op0, 0);
	}
      else
	temp = op0;
      emit_insn (gen_rtx_SET (dest,
			  gen_rtx_IF_THEN_ELSE (GET_MODE (dest),
				   gen_rtx_fmt_ee (compare_code, DImode,
						   temp, const0_rtx),
				   const1_rtx,
				   dest)));
      return 1;
    }
  else
    {
      x = gen_compare_reg_1 (compare_code, x, y);
      y = const0_rtx;

      emit_insn (gen_rtx_SET (dest, const0_rtx));
      emit_insn (gen_rtx_SET (dest,
			  gen_rtx_IF_THEN_ELSE (GET_MODE (dest),
				   gen_rtx_fmt_ee (compare_code,
						   GET_MODE (x), x, y),
				    const1_rtx, dest)));
      return 1;
    }
}


/* Emit an scc insn.  For seq, sne, sgeu, and sltu, we can do this
   without jumps using the addx/subx instructions.  */

bool
emit_scc_insn (rtx operands[])
{
  rtx tem, x, y;
  enum rtx_code code;
  machine_mode mode;

  /* The quad-word fp compare library routines all return nonzero to indicate
     true, which is different from the equivalent libgcc routines, so we must
     handle them specially here.  */
  if (GET_MODE (operands[2]) == TFmode && ! TARGET_HARD_QUAD)
    {
      operands[1] = sparc_emit_float_lib_cmp (operands[2], operands[3],
					      GET_CODE (operands[1]));
      operands[2] = XEXP (operands[1], 0);
      operands[3] = XEXP (operands[1], 1);
    }

  code = GET_CODE (operands[1]);
  x = operands[2];
  y = operands[3];
  mode = GET_MODE (x);

  /* For seq/sne on v9 we use the same code as v8 (the addx/subx method has
     more applications).  The exception to this is "reg != 0" which can
     be done in one instruction on v9 (so we do it).  */
  if ((code == EQ || code == NE) && (mode == SImode || mode == DImode))
    {
      if (y != const0_rtx)
	x = force_reg (mode, gen_rtx_XOR (mode, x, y));

      rtx pat = gen_rtx_SET (operands[0],
			     gen_rtx_fmt_ee (code, GET_MODE (operands[0]),
					     x, const0_rtx));

      /* If we can use addx/subx or addxc, add a clobber for CC.  */
      if (mode == SImode || (code == NE && TARGET_VIS3))
	{
	  rtx clobber
	    = gen_rtx_CLOBBER (VOIDmode,
			       gen_rtx_REG (mode == SImode ? CCmode : CCXmode,
					    SPARC_ICC_REG));
	  pat = gen_rtx_PARALLEL (VOIDmode, gen_rtvec (2, pat, clobber));
	}

      emit_insn (pat);
      return true;
    }

  /* We can do LTU in DImode using the addxc instruction with VIS3.  */
  if (TARGET_ARCH64
      && mode == DImode
      && !((code == LTU || code == GTU) && TARGET_VIS3)
      && gen_v9_scc (operands[0], code, x, y))
    return true;

  /* We can do LTU and GEU using the addx/subx instructions too.  And
     for GTU/LEU, if both operands are registers swap them and fall
     back to the easy case.  */
  if (code == GTU || code == LEU)
    {
      if ((GET_CODE (x) == REG || GET_CODE (x) == SUBREG)
          && (GET_CODE (y) == REG || GET_CODE (y) == SUBREG))
        {
          tem = x;
          x = y;
          y = tem;
          code = swap_condition (code);
        }
    }

  if (code == LTU || code == GEU)
    {
      emit_insn (gen_rtx_SET (operands[0],
			      gen_rtx_fmt_ee (code, GET_MODE (operands[0]),
					      gen_compare_reg_1 (code, x, y),
					      const0_rtx)));
      return true;
    }

  /* All the posibilities to use addx/subx based sequences has been
     exhausted, try for a 3 instruction sequence using v9 conditional
     moves.  */
  if (TARGET_V9 && gen_v9_scc (operands[0], code, x, y))
    return true;

  /* Nope, do branches.  */
  return false;
}

/* Emit a conditional jump insn for the v9 architecture using comparison code
   CODE and jump target LABEL.
   This function exists to take advantage of the v9 brxx insns.  */

static void
emit_v9_brxx_insn (enum rtx_code code, rtx op0, rtx label)
{
  emit_jump_insn (gen_rtx_SET (pc_rtx,
			   gen_rtx_IF_THEN_ELSE (VOIDmode,
				    gen_rtx_fmt_ee (code, GET_MODE (op0),
						    op0, const0_rtx),
				    gen_rtx_LABEL_REF (VOIDmode, label),
				    pc_rtx)));
}

/* Emit a conditional jump insn for the UA2011 architecture using
   comparison code CODE and jump target LABEL.  This function exists
   to take advantage of the UA2011 Compare and Branch insns.  */

static void
emit_cbcond_insn (enum rtx_code code, rtx op0, rtx op1, rtx label)
{
  rtx if_then_else;

  if_then_else = gen_rtx_IF_THEN_ELSE (VOIDmode,
				       gen_rtx_fmt_ee(code, GET_MODE(op0),
						      op0, op1),
				       gen_rtx_LABEL_REF (VOIDmode, label),
				       pc_rtx);

  emit_jump_insn (gen_rtx_SET (pc_rtx, if_then_else));
}

void
emit_conditional_branch_insn (rtx operands[])
{
  /* The quad-word fp compare library routines all return nonzero to indicate
     true, which is different from the equivalent libgcc routines, so we must
     handle them specially here.  */
  if (GET_MODE (operands[1]) == TFmode && ! TARGET_HARD_QUAD)
    {
      operands[0] = sparc_emit_float_lib_cmp (operands[1], operands[2],
					      GET_CODE (operands[0]));
      operands[1] = XEXP (operands[0], 0);
      operands[2] = XEXP (operands[0], 1);
    }

  /* If we can tell early on that the comparison is against a constant
     that won't fit in the 5-bit signed immediate field of a cbcond,
     use one of the other v9 conditional branch sequences.  */
  if (TARGET_CBCOND
      && GET_CODE (operands[1]) == REG
      && (GET_MODE (operands[1]) == SImode
	  || (TARGET_ARCH64 && GET_MODE (operands[1]) == DImode))
      && (GET_CODE (operands[2]) != CONST_INT
	  || SPARC_SIMM5_P (INTVAL (operands[2]))))
    {
      emit_cbcond_insn (GET_CODE (operands[0]), operands[1], operands[2], operands[3]);
      return;
    }

  if (TARGET_ARCH64 && operands[2] == const0_rtx
      && GET_CODE (operands[1]) == REG
      && GET_MODE (operands[1]) == DImode)
    {
      emit_v9_brxx_insn (GET_CODE (operands[0]), operands[1], operands[3]);
      return;
    }

  operands[1] = gen_compare_reg (operands[0]);
  operands[2] = const0_rtx;
  operands[0] = gen_rtx_fmt_ee (GET_CODE (operands[0]), VOIDmode,
				operands[1], operands[2]);
  emit_jump_insn (gen_cbranchcc4 (operands[0], operands[1], operands[2],
				  operands[3]));
}


/* Generate a DFmode part of a hard TFmode register.
   REG is the TFmode hard register, LOW is 1 for the
   low 64bit of the register and 0 otherwise.
 */
rtx
gen_df_reg (rtx reg, int low)
{
  int regno = REGNO (reg);

  if ((WORDS_BIG_ENDIAN == 0) ^ (low != 0))
    regno += (TARGET_ARCH64 && SPARC_INT_REG_P (regno)) ? 1 : 2;
  return gen_rtx_REG (DFmode, regno);
}

/* Generate a call to FUNC with OPERANDS.  Operand 0 is the return value.
   Unlike normal calls, TFmode operands are passed by reference.  It is
   assumed that no more than 3 operands are required.  */

static void
emit_soft_tfmode_libcall (const char *func_name, int nargs, rtx *operands)
{
  rtx ret_slot = NULL, arg[3], func_sym;
  int i;

  /* We only expect to be called for conversions, unary, and binary ops.  */
  gcc_assert (nargs == 2 || nargs == 3);

  for (i = 0; i < nargs; ++i)
    {
      rtx this_arg = operands[i];
      rtx this_slot;

      /* TFmode arguments and return values are passed by reference.  */
      if (GET_MODE (this_arg) == TFmode)
	{
	  int force_stack_temp;

	  force_stack_temp = 0;
	  if (TARGET_BUGGY_QP_LIB && i == 0)
	    force_stack_temp = 1;

	  if (GET_CODE (this_arg) == MEM
	      && ! force_stack_temp)
	    {
	      tree expr = MEM_EXPR (this_arg);
	      if (expr)
		mark_addressable (expr);
	      this_arg = XEXP (this_arg, 0);
	    }
	  else if (CONSTANT_P (this_arg)
		   && ! force_stack_temp)
	    {
	      this_slot = force_const_mem (TFmode, this_arg);
	      this_arg = XEXP (this_slot, 0);
	    }
	  else
	    {
	      this_slot = assign_stack_temp (TFmode, GET_MODE_SIZE (TFmode));

	      /* Operand 0 is the return value.  We'll copy it out later.  */
	      if (i > 0)
		emit_move_insn (this_slot, this_arg);
	      else
		ret_slot = this_slot;

	      this_arg = XEXP (this_slot, 0);
	    }
	}

      arg[i] = this_arg;
    }

  func_sym = gen_rtx_SYMBOL_REF (Pmode, func_name);

  if (GET_MODE (operands[0]) == TFmode)
    {
      if (nargs == 2)
	emit_library_call (func_sym, LCT_NORMAL, VOIDmode,
			   arg[0], GET_MODE (arg[0]),
			   arg[1], GET_MODE (arg[1]));
      else
	emit_library_call (func_sym, LCT_NORMAL, VOIDmode,
			   arg[0], GET_MODE (arg[0]),
			   arg[1], GET_MODE (arg[1]),
			   arg[2], GET_MODE (arg[2]));

      if (ret_slot)
	emit_move_insn (operands[0], ret_slot);
    }
  else
    {
      rtx ret;

      gcc_assert (nargs == 2);

      ret = emit_library_call_value (func_sym, operands[0], LCT_NORMAL,
				     GET_MODE (operands[0]),
				     arg[1], GET_MODE (arg[1]));

      if (ret != operands[0])
	emit_move_insn (operands[0], ret);
    }
}

/* Expand soft-float TFmode calls to sparc abi routines.  */

static void
emit_soft_tfmode_binop (enum rtx_code code, rtx *operands)
{
  const char *func;

  switch (code)
    {
    case PLUS:
      func = "_Qp_add";
      break;
    case MINUS:
      func = "_Qp_sub";
      break;
    case MULT:
      func = "_Qp_mul";
      break;
    case DIV:
      func = "_Qp_div";
      break;
    default:
      gcc_unreachable ();
    }

  emit_soft_tfmode_libcall (func, 3, operands);
}

static void
emit_soft_tfmode_unop (enum rtx_code code, rtx *operands)
{
  const char *func;

  gcc_assert (code == SQRT);
  func = "_Qp_sqrt";

  emit_soft_tfmode_libcall (func, 2, operands);
}

static void
emit_soft_tfmode_cvt (enum rtx_code code, rtx *operands)
{
  const char *func;

  switch (code)
    {
    case FLOAT_EXTEND:
      switch (GET_MODE (operands[1]))
	{
	case E_SFmode:
	  func = "_Qp_stoq";
	  break;
	case E_DFmode:
	  func = "_Qp_dtoq";
	  break;
	default:
	  gcc_unreachable ();
	}
      break;

    case FLOAT_TRUNCATE:
      switch (GET_MODE (operands[0]))
	{
	case E_SFmode:
	  func = "_Qp_qtos";
	  break;
	case E_DFmode:
	  func = "_Qp_qtod";
	  break;
	default:
	  gcc_unreachable ();
	}
      break;

    case FLOAT:
      switch (GET_MODE (operands[1]))
	{
	case E_SImode:
	  func = "_Qp_itoq";
	  if (TARGET_ARCH64)
	    operands[1] = gen_rtx_SIGN_EXTEND (DImode, operands[1]);
	  break;
	case E_DImode:
	  func = "_Qp_xtoq";
	  break;
	default:
	  gcc_unreachable ();
	}
      break;

    case UNSIGNED_FLOAT:
      switch (GET_MODE (operands[1]))
	{
	case E_SImode:
	  func = "_Qp_uitoq";
	  if (TARGET_ARCH64)
	    operands[1] = gen_rtx_ZERO_EXTEND (DImode, operands[1]);
	  break;
	case E_DImode:
	  func = "_Qp_uxtoq";
	  break;
	default:
	  gcc_unreachable ();
	}
      break;

    case FIX:
      switch (GET_MODE (operands[0]))
	{
	case E_SImode:
	  func = "_Qp_qtoi";
	  break;
	case E_DImode:
	  func = "_Qp_qtox";
	  break;
	default:
	  gcc_unreachable ();
	}
      break;

    case UNSIGNED_FIX:
      switch (GET_MODE (operands[0]))
	{
	case E_SImode:
	  func = "_Qp_qtoui";
	  break;
	case E_DImode:
	  func = "_Qp_qtoux";
	  break;
	default:
	  gcc_unreachable ();
	}
      break;

    default:
      gcc_unreachable ();
    }

  emit_soft_tfmode_libcall (func, 2, operands);
}

/* Expand a hard-float tfmode operation.  All arguments must be in
   registers.  */

static void
emit_hard_tfmode_operation (enum rtx_code code, rtx *operands)
{
  rtx op, dest;

  if (GET_RTX_CLASS (code) == RTX_UNARY)
    {
      operands[1] = force_reg (GET_MODE (operands[1]), operands[1]);
      op = gen_rtx_fmt_e (code, GET_MODE (operands[0]), operands[1]);
    }
  else
    {
      operands[1] = force_reg (GET_MODE (operands[1]), operands[1]);
      operands[2] = force_reg (GET_MODE (operands[2]), operands[2]);
      op = gen_rtx_fmt_ee (code, GET_MODE (operands[0]),
			   operands[1], operands[2]);
    }

  if (register_operand (operands[0], VOIDmode))
    dest = operands[0];
  else
    dest = gen_reg_rtx (GET_MODE (operands[0]));

  emit_insn (gen_rtx_SET (dest, op));

  if (dest != operands[0])
    emit_move_insn (operands[0], dest);
}

void
emit_tfmode_binop (enum rtx_code code, rtx *operands)
{
  if (TARGET_HARD_QUAD)
    emit_hard_tfmode_operation (code, operands);
  else
    emit_soft_tfmode_binop (code, operands);
}

void
emit_tfmode_unop (enum rtx_code code, rtx *operands)
{
  if (TARGET_HARD_QUAD)
    emit_hard_tfmode_operation (code, operands);
  else
    emit_soft_tfmode_unop (code, operands);
}

void
emit_tfmode_cvt (enum rtx_code code, rtx *operands)
{
  if (TARGET_HARD_QUAD)
    emit_hard_tfmode_operation (code, operands);
  else
    emit_soft_tfmode_cvt (code, operands);
}

/* Return nonzero if a branch/jump/call instruction will be emitting
   nop into its delay slot.  */

int
empty_delay_slot (rtx_insn *insn)
{
  rtx seq;

  /* If no previous instruction (should not happen), return true.  */
  if (PREV_INSN (insn) == NULL)
    return 1;

  seq = NEXT_INSN (PREV_INSN (insn));
  if (GET_CODE (PATTERN (seq)) == SEQUENCE)
    return 0;

  return 1;
}

/* Return nonzero if we should emit a nop after a cbcond instruction.
   The cbcond instruction does not have a delay slot, however there is
   a severe performance penalty if a control transfer appears right
   after a cbcond.  Therefore we emit a nop when we detect this
   situation.  */

int
emit_cbcond_nop (rtx_insn *insn)
{
  rtx next = next_active_insn (insn);

  if (!next)
    return 1;

  if (NONJUMP_INSN_P (next)
      && GET_CODE (PATTERN (next)) == SEQUENCE)
    next = XVECEXP (PATTERN (next), 0, 0);
  else if (CALL_P (next)
	   && GET_CODE (PATTERN (next)) == PARALLEL)
    {
      rtx delay = XVECEXP (PATTERN (next), 0, 1);

      if (GET_CODE (delay) == RETURN)
	{
	  /* It's a sibling call.  Do not emit the nop if we're going
	     to emit something other than the jump itself as the first
	     instruction of the sibcall sequence.  */
	  if (sparc_leaf_function_p || TARGET_FLAT)
	    return 0;
	}
    }

  if (NONJUMP_INSN_P (next))
    return 0;

  return 1;
}

/* Return nonzero if TRIAL can go into the call delay slot.  */

int
eligible_for_call_delay (rtx_insn *trial)
{
  rtx pat;

  if (get_attr_in_branch_delay (trial) == IN_BRANCH_DELAY_FALSE)
    return 0;

  /* Binutils allows
       call __tls_get_addr, %tgd_call (foo)
        add %l7, %o0, %o0, %tgd_add (foo)
     while Sun as/ld does not.  */
  if (TARGET_GNU_TLS || !TARGET_TLS)
    return 1;

  pat = PATTERN (trial);

  /* We must reject tgd_add{32|64}, i.e.
       (set (reg) (plus (reg) (unspec [(reg) (symbol_ref)] UNSPEC_TLSGD)))
     and tldm_add{32|64}, i.e.
       (set (reg) (plus (reg) (unspec [(reg) (symbol_ref)] UNSPEC_TLSLDM)))
     for Sun as/ld.  */
  if (GET_CODE (pat) == SET
      && GET_CODE (SET_SRC (pat)) == PLUS)
    {
      rtx unspec = XEXP (SET_SRC (pat), 1);

      if (GET_CODE (unspec) == UNSPEC
	  && (XINT (unspec, 1) == UNSPEC_TLSGD
	      || XINT (unspec, 1) == UNSPEC_TLSLDM))
	return 0;
    }

  return 1;
}

/* Return nonzero if TRIAL, an insn, can be combined with a 'restore'
   instruction.  RETURN_P is true if the v9 variant 'return' is to be
   considered in the test too.

   TRIAL must be a SET whose destination is a REG appropriate for the
   'restore' instruction or, if RETURN_P is true, for the 'return'
   instruction.  */

static int
eligible_for_restore_insn (rtx trial, bool return_p)
{
  rtx pat = PATTERN (trial);
  rtx src = SET_SRC (pat);
  bool src_is_freg = false;
  rtx src_reg;

  /* Since we now can do moves between float and integer registers when
     VIS3 is enabled, we have to catch this case.  We can allow such
     moves when doing a 'return' however.  */
  src_reg = src;
  if (GET_CODE (src_reg) == SUBREG)
    src_reg = SUBREG_REG (src_reg);
  if (GET_CODE (src_reg) == REG
      && SPARC_FP_REG_P (REGNO (src_reg)))
    src_is_freg = true;

  /* The 'restore src,%g0,dest' pattern for word mode and below.  */
  if (GET_MODE_CLASS (GET_MODE (src)) != MODE_FLOAT
      && arith_operand (src, GET_MODE (src))
      && ! src_is_freg)
    {
      if (TARGET_ARCH64)
        return GET_MODE_SIZE (GET_MODE (src)) <= GET_MODE_SIZE (DImode);
      else
        return GET_MODE_SIZE (GET_MODE (src)) <= GET_MODE_SIZE (SImode);
    }

  /* The 'restore src,%g0,dest' pattern for double-word mode.  */
  else if (GET_MODE_CLASS (GET_MODE (src)) != MODE_FLOAT
	   && arith_double_operand (src, GET_MODE (src))
	   && ! src_is_freg)
    return GET_MODE_SIZE (GET_MODE (src)) <= GET_MODE_SIZE (DImode);

  /* The 'restore src,%g0,dest' pattern for float if no FPU.  */
  else if (! TARGET_FPU && register_operand (src, SFmode))
    return 1;

  /* The 'restore src,%g0,dest' pattern for double if no FPU.  */
  else if (! TARGET_FPU && TARGET_ARCH64 && register_operand (src, DFmode))
    return 1;

  /* If we have the 'return' instruction, anything that does not use
     local or output registers and can go into a delay slot wins.  */
  else if (return_p && TARGET_V9 && !epilogue_renumber (&pat, 1))
    return 1;

  /* The 'restore src1,src2,dest' pattern for SImode.  */
  else if (GET_CODE (src) == PLUS
	   && register_operand (XEXP (src, 0), SImode)
	   && arith_operand (XEXP (src, 1), SImode))
    return 1;

  /* The 'restore src1,src2,dest' pattern for DImode.  */
  else if (GET_CODE (src) == PLUS
	   && register_operand (XEXP (src, 0), DImode)
	   && arith_double_operand (XEXP (src, 1), DImode))
    return 1;

  /* The 'restore src1,%lo(src2),dest' pattern.  */
  else if (GET_CODE (src) == LO_SUM
	   && ! TARGET_CM_MEDMID
	   && ((register_operand (XEXP (src, 0), SImode)
	        && immediate_operand (XEXP (src, 1), SImode))
	       || (TARGET_ARCH64
		   && register_operand (XEXP (src, 0), DImode)
		   && immediate_operand (XEXP (src, 1), DImode))))
    return 1;

  /* The 'restore src,src,dest' pattern.  */
  else if (GET_CODE (src) == ASHIFT
	   && (register_operand (XEXP (src, 0), SImode)
	       || register_operand (XEXP (src, 0), DImode))
	   && XEXP (src, 1) == const1_rtx)
    return 1;

  return 0;
}

/* Return nonzero if TRIAL can go into the function return's delay slot.  */

int
eligible_for_return_delay (rtx_insn *trial)
{
  int regno;
  rtx pat;

  /* If the function uses __builtin_eh_return, the eh_return machinery
     occupies the delay slot.  */
  if (crtl->calls_eh_return)
    return 0;

  if (get_attr_in_branch_delay (trial) == IN_BRANCH_DELAY_FALSE)
    return 0;

  /* In the case of a leaf or flat function, anything can go into the slot.  */
  if (sparc_leaf_function_p || TARGET_FLAT)
    return 1;

  if (!NONJUMP_INSN_P (trial))
    return 0;

  pat = PATTERN (trial);
  if (GET_CODE (pat) == PARALLEL)
    {
      int i;

      if (! TARGET_V9)
	return 0;
      for (i = XVECLEN (pat, 0) - 1; i >= 0; i--)
	{
	  rtx expr = XVECEXP (pat, 0, i);
	  if (GET_CODE (expr) != SET)
	    return 0;
	  if (GET_CODE (SET_DEST (expr)) != REG)
	    return 0;
	  regno = REGNO (SET_DEST (expr));
	  if (regno >= 8 && regno < 24)
	    return 0;
	}
      return !epilogue_renumber (&pat, 1);
    }

  if (GET_CODE (pat) != SET)
    return 0;

  if (GET_CODE (SET_DEST (pat)) != REG)
    return 0;

  regno = REGNO (SET_DEST (pat));

  /* Otherwise, only operations which can be done in tandem with
     a `restore' or `return' insn can go into the delay slot.  */
  if (regno >= 8 && regno < 24)
    return 0;

  /* If this instruction sets up floating point register and we have a return
     instruction, it can probably go in.  But restore will not work
     with FP_REGS.  */
  if (! SPARC_INT_REG_P (regno))
    return TARGET_V9 && !epilogue_renumber (&pat, 1);

  return eligible_for_restore_insn (trial, true);
}

/* Return nonzero if TRIAL can go into the sibling call's delay slot.  */

int
eligible_for_sibcall_delay (rtx_insn *trial)
{
  rtx pat;

  if (get_attr_in_branch_delay (trial) == IN_BRANCH_DELAY_FALSE)
    return 0;

  if (!NONJUMP_INSN_P (trial))
    return 0;

  pat = PATTERN (trial);

  if (sparc_leaf_function_p || TARGET_FLAT)
    {
      /* If the tail call is done using the call instruction,
	 we have to restore %o7 in the delay slot.  */
      if (LEAF_SIBCALL_SLOT_RESERVED_P)
	return 0;

      /* %g1 is used to build the function address */
      if (reg_mentioned_p (gen_rtx_REG (Pmode, 1), pat))
	return 0;

      return 1;
    }

  if (GET_CODE (pat) != SET)
    return 0;

  /* Otherwise, only operations which can be done in tandem with
     a `restore' insn can go into the delay slot.  */
  if (GET_CODE (SET_DEST (pat)) != REG
      || (REGNO (SET_DEST (pat)) >= 8 && REGNO (SET_DEST (pat)) < 24)
      || ! SPARC_INT_REG_P (REGNO (SET_DEST (pat))))
    return 0;

  /* If it mentions %o7, it can't go in, because sibcall will clobber it
     in most cases.  */
  if (reg_mentioned_p (gen_rtx_REG (Pmode, 15), pat))
    return 0;

  return eligible_for_restore_insn (trial, false);
}

/* Determine if it's legal to put X into the constant pool.  This
   is not possible if X contains the address of a symbol that is
   not constant (TLS) or not known at final link time (PIC).  */

static bool
sparc_cannot_force_const_mem (machine_mode mode, rtx x)
{
  switch (GET_CODE (x))
    {
    case CONST_INT:
    case CONST_WIDE_INT:
    case CONST_DOUBLE:
    case CONST_VECTOR:
      /* Accept all non-symbolic constants.  */
      return false;

    case LABEL_REF:
      /* Labels are OK iff we are non-PIC.  */
      return flag_pic != 0;

    case SYMBOL_REF:
      /* 'Naked' TLS symbol references are never OK,
	 non-TLS symbols are OK iff we are non-PIC.  */
      if (SYMBOL_REF_TLS_MODEL (x))
	return true;
      else
	return flag_pic != 0;

    case CONST:
      return sparc_cannot_force_const_mem (mode, XEXP (x, 0));
    case PLUS:
    case MINUS:
      return sparc_cannot_force_const_mem (mode, XEXP (x, 0))
         || sparc_cannot_force_const_mem (mode, XEXP (x, 1));
    case UNSPEC:
      return true;
    default:
      gcc_unreachable ();
    }
}

/* Global Offset Table support.  */
static GTY(()) rtx got_helper_rtx = NULL_RTX;
static GTY(()) rtx global_offset_table_rtx = NULL_RTX;

/* Return the SYMBOL_REF for the Global Offset Table.  */

static GTY(()) rtx sparc_got_symbol = NULL_RTX;

static rtx
sparc_got (void)
{
  if (!sparc_got_symbol)
    sparc_got_symbol = gen_rtx_SYMBOL_REF (Pmode, "_GLOBAL_OFFSET_TABLE_");

  return sparc_got_symbol;
}

/* Ensure that we are not using patterns that are not OK with PIC.  */

int
check_pic (int i)
{
  rtx op;

  switch (flag_pic)
    {
    case 1:
      op = recog_data.operand[i];
      gcc_assert (GET_CODE (op) != SYMBOL_REF
	  	  && (GET_CODE (op) != CONST
		      || (GET_CODE (XEXP (op, 0)) == MINUS
			  && XEXP (XEXP (op, 0), 0) == sparc_got ()
			  && GET_CODE (XEXP (XEXP (op, 0), 1)) == CONST)));
      /* fallthrough */
    case 2:
    default:
      return 1;
    }
}

/* Return true if X is an address which needs a temporary register when
   reloaded while generating PIC code.  */

int
pic_address_needs_scratch (rtx x)
{
  /* An address which is a symbolic plus a non SMALL_INT needs a temp reg.  */
  if (GET_CODE (x) == CONST && GET_CODE (XEXP (x, 0)) == PLUS
      && GET_CODE (XEXP (XEXP (x, 0), 0)) == SYMBOL_REF
      && GET_CODE (XEXP (XEXP (x, 0), 1)) == CONST_INT
      && ! SMALL_INT (XEXP (XEXP (x, 0), 1)))
    return 1;

  return 0;
}

/* Determine if a given RTX is a valid constant.  We already know this
   satisfies CONSTANT_P.  */

static bool
sparc_legitimate_constant_p (machine_mode mode, rtx x)
{
  switch (GET_CODE (x))
    {
    case CONST:
    case SYMBOL_REF:
      if (sparc_tls_referenced_p (x))
	return false;
      break;

    case CONST_DOUBLE:
      /* Floating point constants are generally not ok.
	 The only exception is 0.0 and all-ones in VIS.  */
      if (TARGET_VIS
	  && SCALAR_FLOAT_MODE_P (mode)
	  && (const_zero_operand (x, mode)
	      || const_all_ones_operand (x, mode)))
	return true;

      return false;

    case CONST_VECTOR:
      /* Vector constants are generally not ok.
	 The only exception is 0 or -1 in VIS.  */
      if (TARGET_VIS
	  && (const_zero_operand (x, mode)
	      || const_all_ones_operand (x, mode)))
	return true;

      return false;

    default:
      break;
    }

  return true;
}

/* Determine if a given RTX is a valid constant address.  */

bool
constant_address_p (rtx x)
{
  switch (GET_CODE (x))
    {
    case LABEL_REF:
    case CONST_INT:
    case HIGH:
      return true;

    case CONST:
      if (flag_pic && pic_address_needs_scratch (x))
	return false;
      return sparc_legitimate_constant_p (Pmode, x);

    case SYMBOL_REF:
      return !flag_pic && sparc_legitimate_constant_p (Pmode, x);

    default:
      return false;
    }
}

/* Nonzero if the constant value X is a legitimate general operand
   when generating PIC code.  It is given that flag_pic is on and
   that X satisfies CONSTANT_P.  */

bool
legitimate_pic_operand_p (rtx x)
{
  if (pic_address_needs_scratch (x))
    return false;
  if (sparc_tls_referenced_p (x))
    return false;
  return true;
}

#define RTX_OK_FOR_OFFSET_P(X, MODE)			\
  (CONST_INT_P (X)					\
   && INTVAL (X) >= -0x1000				\
   && INTVAL (X) <= (0x1000 - GET_MODE_SIZE (MODE)))

#define RTX_OK_FOR_OLO10_P(X, MODE)			\
  (CONST_INT_P (X)					\
   && INTVAL (X) >= -0x1000				\
   && INTVAL (X) <= (0xc00 - GET_MODE_SIZE (MODE)))

/* Handle the TARGET_LEGITIMATE_ADDRESS_P target hook.

   On SPARC, the actual legitimate addresses must be REG+REG or REG+SMALLINT
   ordinarily.  This changes a bit when generating PIC.  */

static bool
sparc_legitimate_address_p (machine_mode mode, rtx addr, bool strict)
{
  rtx rs1 = NULL, rs2 = NULL, imm1 = NULL;

  if (REG_P (addr) || GET_CODE (addr) == SUBREG)
    rs1 = addr;
  else if (GET_CODE (addr) == PLUS)
    {
      rs1 = XEXP (addr, 0);
      rs2 = XEXP (addr, 1);

      /* Canonicalize.  REG comes first, if there are no regs,
	 LO_SUM comes first.  */
      if (!REG_P (rs1)
	  && GET_CODE (rs1) != SUBREG
	  && (REG_P (rs2)
	      || GET_CODE (rs2) == SUBREG
	      || (GET_CODE (rs2) == LO_SUM && GET_CODE (rs1) != LO_SUM)))
	{
	  rs1 = XEXP (addr, 1);
	  rs2 = XEXP (addr, 0);
	}

      if ((flag_pic == 1
	   && rs1 == pic_offset_table_rtx
	   && !REG_P (rs2)
	   && GET_CODE (rs2) != SUBREG
	   && GET_CODE (rs2) != LO_SUM
	   && GET_CODE (rs2) != MEM
	   && !(GET_CODE (rs2) == SYMBOL_REF && SYMBOL_REF_TLS_MODEL (rs2))
	   && (! symbolic_operand (rs2, VOIDmode) || mode == Pmode)
	   && (GET_CODE (rs2) != CONST_INT || SMALL_INT (rs2)))
	  || ((REG_P (rs1)
	       || GET_CODE (rs1) == SUBREG)
	      && RTX_OK_FOR_OFFSET_P (rs2, mode)))
	{
	  imm1 = rs2;
	  rs2 = NULL;
	}
      else if ((REG_P (rs1) || GET_CODE (rs1) == SUBREG)
	       && (REG_P (rs2) || GET_CODE (rs2) == SUBREG))
	{
	  /* We prohibit REG + REG for TFmode when there are no quad move insns
	     and we consequently need to split.  We do this because REG+REG
	     is not an offsettable address.  If we get the situation in reload
	     where source and destination of a movtf pattern are both MEMs with
	     REG+REG address, then only one of them gets converted to an
	     offsettable address.  */
	  if (mode == TFmode
	      && ! (TARGET_ARCH64 && TARGET_HARD_QUAD))
	    return 0;

	  /* Likewise for TImode, but in all cases.  */
	  if (mode == TImode)
	    return 0;

	  /* We prohibit REG + REG on ARCH32 if not optimizing for
	     DFmode/DImode because then mem_min_alignment is likely to be zero
	     after reload and the  forced split would lack a matching splitter
	     pattern.  */
	  if (TARGET_ARCH32 && !optimize
	      && (mode == DFmode || mode == DImode))
	    return 0;
	}
      else if (USE_AS_OFFSETABLE_LO10
	       && GET_CODE (rs1) == LO_SUM
	       && TARGET_ARCH64
	       && ! TARGET_CM_MEDMID
	       && RTX_OK_FOR_OLO10_P (rs2, mode))
	{
	  rs2 = NULL;
	  imm1 = XEXP (rs1, 1);
	  rs1 = XEXP (rs1, 0);
	  if (!CONSTANT_P (imm1)
	      || (GET_CODE (rs1) == SYMBOL_REF && SYMBOL_REF_TLS_MODEL (rs1)))
	    return 0;
	}
    }
  else if (GET_CODE (addr) == LO_SUM)
    {
      rs1 = XEXP (addr, 0);
      imm1 = XEXP (addr, 1);

      if (!CONSTANT_P (imm1)
	  || (GET_CODE (rs1) == SYMBOL_REF && SYMBOL_REF_TLS_MODEL (rs1)))
	return 0;

      /* We can't allow TFmode in 32-bit mode, because an offset greater
	 than the alignment (8) may cause the LO_SUM to overflow.  */
      if (mode == TFmode && TARGET_ARCH32)
	return 0;

      /* During reload, accept the HIGH+LO_SUM construct generated by
	 sparc_legitimize_reload_address.  */
      if (reload_in_progress
	  && GET_CODE (rs1) == HIGH
	  && XEXP (rs1, 0) == imm1)
	return 1;
    }
  else if (GET_CODE (addr) == CONST_INT && SMALL_INT (addr))
    return 1;
  else
    return 0;

  if (GET_CODE (rs1) == SUBREG)
    rs1 = SUBREG_REG (rs1);
  if (!REG_P (rs1))
    return 0;

  if (rs2)
    {
      if (GET_CODE (rs2) == SUBREG)
	rs2 = SUBREG_REG (rs2);
      if (!REG_P (rs2))
	return 0;
    }

  if (strict)
    {
      if (!REGNO_OK_FOR_BASE_P (REGNO (rs1))
	  || (rs2 && !REGNO_OK_FOR_BASE_P (REGNO (rs2))))
	return 0;
    }
  else
    {
      if ((! SPARC_INT_REG_P (REGNO (rs1))
	   && REGNO (rs1) != FRAME_POINTER_REGNUM
	   && REGNO (rs1) < FIRST_PSEUDO_REGISTER)
	  || (rs2
	      && (! SPARC_INT_REG_P (REGNO (rs2))
		  && REGNO (rs2) != FRAME_POINTER_REGNUM
		  && REGNO (rs2) < FIRST_PSEUDO_REGISTER)))
	return 0;
    }
  return 1;
}

/* Return the SYMBOL_REF for the tls_get_addr function.  */

static GTY(()) rtx sparc_tls_symbol = NULL_RTX;

static rtx
sparc_tls_get_addr (void)
{
  if (!sparc_tls_symbol)
    sparc_tls_symbol = gen_rtx_SYMBOL_REF (Pmode, "__tls_get_addr");

  return sparc_tls_symbol;
}

/* Return the Global Offset Table to be used in TLS mode.  */

static rtx
sparc_tls_got (void)
{
  /* In PIC mode, this is just the PIC offset table.  */
  if (flag_pic)
    {
      crtl->uses_pic_offset_table = 1;
      return pic_offset_table_rtx;
    }

  /* In non-PIC mode, Sun as (unlike GNU as) emits PC-relative relocations for
     the GOT symbol with the 32-bit ABI, so we reload the GOT register.  */
  if (TARGET_SUN_TLS && TARGET_ARCH32)
    {
      load_got_register ();
      return global_offset_table_rtx;
    }

  /* In all other cases, we load a new pseudo with the GOT symbol.  */
  return copy_to_reg (sparc_got ());
}

/* Return true if X contains a thread-local symbol.  */

static bool
sparc_tls_referenced_p (rtx x)
{
  if (!TARGET_HAVE_TLS)
    return false;

  if (GET_CODE (x) == CONST && GET_CODE (XEXP (x, 0)) == PLUS)
    x = XEXP (XEXP (x, 0), 0);

  if (GET_CODE (x) == SYMBOL_REF && SYMBOL_REF_TLS_MODEL (x))
    return true;

  /* That's all we handle in sparc_legitimize_tls_address for now.  */
  return false;
}

/* ADDR contains a thread-local SYMBOL_REF.  Generate code to compute
   this (thread-local) address.  */

static rtx
sparc_legitimize_tls_address (rtx addr)
{
  rtx temp1, temp2, temp3, ret, o0, got;
  rtx_insn *insn;

  gcc_assert (can_create_pseudo_p ());

  if (GET_CODE (addr) == SYMBOL_REF)
    switch (SYMBOL_REF_TLS_MODEL (addr))
      {
      case TLS_MODEL_GLOBAL_DYNAMIC:
	start_sequence ();
	temp1 = gen_reg_rtx (SImode);
	temp2 = gen_reg_rtx (SImode);
	ret = gen_reg_rtx (Pmode);
	o0 = gen_rtx_REG (Pmode, 8);
	got = sparc_tls_got ();
	emit_insn (gen_tgd_hi22 (temp1, addr));
	emit_insn (gen_tgd_lo10 (temp2, temp1, addr));
	if (TARGET_ARCH32)
	  {
	    emit_insn (gen_tgd_add32 (o0, got, temp2, addr));
	    insn = emit_call_insn (gen_tgd_call32 (o0, sparc_tls_get_addr (),
						   addr, const1_rtx));
	  }
	else
	  {
	    emit_insn (gen_tgd_add64 (o0, got, temp2, addr));
	    insn = emit_call_insn (gen_tgd_call64 (o0, sparc_tls_get_addr (),
						   addr, const1_rtx));
	  }
	use_reg (&CALL_INSN_FUNCTION_USAGE (insn), o0);
	insn = get_insns ();
	end_sequence ();
	emit_libcall_block (insn, ret, o0, addr);
	break;

      case TLS_MODEL_LOCAL_DYNAMIC:
	start_sequence ();
	temp1 = gen_reg_rtx (SImode);
	temp2 = gen_reg_rtx (SImode);
	temp3 = gen_reg_rtx (Pmode);
	ret = gen_reg_rtx (Pmode);
	o0 = gen_rtx_REG (Pmode, 8);
	got = sparc_tls_got ();
	emit_insn (gen_tldm_hi22 (temp1));
	emit_insn (gen_tldm_lo10 (temp2, temp1));
	if (TARGET_ARCH32)
	  {
	    emit_insn (gen_tldm_add32 (o0, got, temp2));
	    insn = emit_call_insn (gen_tldm_call32 (o0, sparc_tls_get_addr (),
						    const1_rtx));
	  }
	else
	  {
	    emit_insn (gen_tldm_add64 (o0, got, temp2));
	    insn = emit_call_insn (gen_tldm_call64 (o0, sparc_tls_get_addr (),
						    const1_rtx));
	  }
	use_reg (&CALL_INSN_FUNCTION_USAGE (insn), o0);
	insn = get_insns ();
	end_sequence ();
	emit_libcall_block (insn, temp3, o0,
			    gen_rtx_UNSPEC (Pmode, gen_rtvec (1, const0_rtx),
					    UNSPEC_TLSLD_BASE));
	temp1 = gen_reg_rtx (SImode);
	temp2 = gen_reg_rtx (SImode);
	emit_insn (gen_tldo_hix22 (temp1, addr));
	emit_insn (gen_tldo_lox10 (temp2, temp1, addr));
	if (TARGET_ARCH32)
	  emit_insn (gen_tldo_add32 (ret, temp3, temp2, addr));
	else
	  emit_insn (gen_tldo_add64 (ret, temp3, temp2, addr));
	break;

      case TLS_MODEL_INITIAL_EXEC:
	temp1 = gen_reg_rtx (SImode);
	temp2 = gen_reg_rtx (SImode);
	temp3 = gen_reg_rtx (Pmode);
	got = sparc_tls_got ();
	emit_insn (gen_tie_hi22 (temp1, addr));
	emit_insn (gen_tie_lo10 (temp2, temp1, addr));
	if (TARGET_ARCH32)
	  emit_insn (gen_tie_ld32 (temp3, got, temp2, addr));
	else
	  emit_insn (gen_tie_ld64 (temp3, got, temp2, addr));
        if (TARGET_SUN_TLS)
	  {
	    ret = gen_reg_rtx (Pmode);
	    if (TARGET_ARCH32)
	      emit_insn (gen_tie_add32 (ret, gen_rtx_REG (Pmode, 7),
					temp3, addr));
	    else
	      emit_insn (gen_tie_add64 (ret, gen_rtx_REG (Pmode, 7),
					temp3, addr));
	  }
	else
	  ret = gen_rtx_PLUS (Pmode, gen_rtx_REG (Pmode, 7), temp3);
	break;

      case TLS_MODEL_LOCAL_EXEC:
	temp1 = gen_reg_rtx (Pmode);
	temp2 = gen_reg_rtx (Pmode);
	if (TARGET_ARCH32)
	  {
	    emit_insn (gen_tle_hix22_sp32 (temp1, addr));
	    emit_insn (gen_tle_lox10_sp32 (temp2, temp1, addr));
	  }
	else
	  {
	    emit_insn (gen_tle_hix22_sp64 (temp1, addr));
	    emit_insn (gen_tle_lox10_sp64 (temp2, temp1, addr));
	  }
	ret = gen_rtx_PLUS (Pmode, gen_rtx_REG (Pmode, 7), temp2);
	break;

      default:
	gcc_unreachable ();
      }

  else if (GET_CODE (addr) == CONST)
    {
      rtx base, offset;

      gcc_assert (GET_CODE (XEXP (addr, 0)) == PLUS);

      base = sparc_legitimize_tls_address (XEXP (XEXP (addr, 0), 0));
      offset = XEXP (XEXP (addr, 0), 1);

      base = force_operand (base, NULL_RTX);
      if (!(GET_CODE (offset) == CONST_INT && SMALL_INT (offset)))
	offset = force_reg (Pmode, offset);
      ret = gen_rtx_PLUS (Pmode, base, offset);
    }

  else
    gcc_unreachable ();  /* for now ... */

  return ret;
}

/* Legitimize PIC addresses.  If the address is already position-independent,
   we return ORIG.  Newly generated position-independent addresses go into a
   reg.  This is REG if nonzero, otherwise we allocate register(s) as
   necessary.  */

static rtx
sparc_legitimize_pic_address (rtx orig, rtx reg)
{
  bool gotdata_op = false;

  if (GET_CODE (orig) == SYMBOL_REF
      /* See the comment in sparc_expand_move.  */
      || (GET_CODE (orig) == LABEL_REF && !can_use_mov_pic_label_ref (orig)))
    {
      rtx pic_ref, address;
      rtx_insn *insn;

      if (reg == 0)
	{
	  gcc_assert (can_create_pseudo_p ());
	  reg = gen_reg_rtx (Pmode);
	}

      if (flag_pic == 2)
	{
	  /* If not during reload, allocate another temp reg here for loading
	     in the address, so that these instructions can be optimized
	     properly.  */
	  rtx temp_reg = (! can_create_pseudo_p ()
			  ? reg : gen_reg_rtx (Pmode));

	  /* Must put the SYMBOL_REF inside an UNSPEC here so that cse
	     won't get confused into thinking that these two instructions
	     are loading in the true address of the symbol.  If in the
	     future a PIC rtx exists, that should be used instead.  */
	  if (TARGET_ARCH64)
	    {
	      emit_insn (gen_movdi_high_pic (temp_reg, orig));
	      emit_insn (gen_movdi_lo_sum_pic (temp_reg, temp_reg, orig));
	    }
	  else
	    {
	      emit_insn (gen_movsi_high_pic (temp_reg, orig));
	      emit_insn (gen_movsi_lo_sum_pic (temp_reg, temp_reg, orig));
	    }
	  address = temp_reg;
	  gotdata_op = true;
	}
      else
	address = orig;

      crtl->uses_pic_offset_table = 1;
      if (gotdata_op)
	{
	  if (TARGET_ARCH64)
	    insn = emit_insn (gen_movdi_pic_gotdata_op (reg,
							pic_offset_table_rtx,
							address, orig));
	  else
	    insn = emit_insn (gen_movsi_pic_gotdata_op (reg,
							pic_offset_table_rtx,
							address, orig));
	}
      else
	{
	  pic_ref
	    = gen_const_mem (Pmode,
			     gen_rtx_PLUS (Pmode,
					   pic_offset_table_rtx, address));
	  insn = emit_move_insn (reg, pic_ref);
	}

      /* Put a REG_EQUAL note on this insn, so that it can be optimized
	 by loop.  */
      set_unique_reg_note (insn, REG_EQUAL, orig);
      return reg;
    }
  else if (GET_CODE (orig) == CONST)
    {
      rtx base, offset;

      if (GET_CODE (XEXP (orig, 0)) == PLUS
	  && XEXP (XEXP (orig, 0), 0) == pic_offset_table_rtx)
	return orig;

      if (reg == 0)
	{
	  gcc_assert (can_create_pseudo_p ());
	  reg = gen_reg_rtx (Pmode);
	}

      gcc_assert (GET_CODE (XEXP (orig, 0)) == PLUS);
      base = sparc_legitimize_pic_address (XEXP (XEXP (orig, 0), 0), reg);
      offset = sparc_legitimize_pic_address (XEXP (XEXP (orig, 0), 1),
			 		     base == reg ? NULL_RTX : reg);

      if (GET_CODE (offset) == CONST_INT)
	{
	  if (SMALL_INT (offset))
	    return plus_constant (Pmode, base, INTVAL (offset));
	  else if (can_create_pseudo_p ())
	    offset = force_reg (Pmode, offset);
	  else
	    /* If we reach here, then something is seriously wrong.  */
	    gcc_unreachable ();
	}
      return gen_rtx_PLUS (Pmode, base, offset);
    }
  else if (GET_CODE (orig) == LABEL_REF)
    /* ??? We ought to be checking that the register is live instead, in case
       it is eliminated.  */
    crtl->uses_pic_offset_table = 1;

  return orig;
}

/* Try machine-dependent ways of modifying an illegitimate address X
   to be legitimate.  If we find one, return the new, valid address.

   OLDX is the address as it was before break_out_memory_refs was called.
   In some cases it is useful to look at this to decide what needs to be done.

   MODE is the mode of the operand pointed to by X.

   On SPARC, change REG+N into REG+REG, and REG+(X*Y) into REG+REG.  */

static rtx
sparc_legitimize_address (rtx x, rtx oldx ATTRIBUTE_UNUSED,
			  machine_mode mode)
{
  rtx orig_x = x;

  if (GET_CODE (x) == PLUS && GET_CODE (XEXP (x, 0)) == MULT)
    x = gen_rtx_PLUS (Pmode, XEXP (x, 1),
		      force_operand (XEXP (x, 0), NULL_RTX));
  if (GET_CODE (x) == PLUS && GET_CODE (XEXP (x, 1)) == MULT)
    x = gen_rtx_PLUS (Pmode, XEXP (x, 0),
		      force_operand (XEXP (x, 1), NULL_RTX));
  if (GET_CODE (x) == PLUS && GET_CODE (XEXP (x, 0)) == PLUS)
    x = gen_rtx_PLUS (Pmode, force_operand (XEXP (x, 0), NULL_RTX),
		      XEXP (x, 1));
  if (GET_CODE (x) == PLUS && GET_CODE (XEXP (x, 1)) == PLUS)
    x = gen_rtx_PLUS (Pmode, XEXP (x, 0),
		      force_operand (XEXP (x, 1), NULL_RTX));

  if (x != orig_x && sparc_legitimate_address_p (mode, x, FALSE))
    return x;

  if (sparc_tls_referenced_p (x))
    x = sparc_legitimize_tls_address (x);
  else if (flag_pic)
    x = sparc_legitimize_pic_address (x, NULL_RTX);
  else if (GET_CODE (x) == PLUS && CONSTANT_ADDRESS_P (XEXP (x, 1)))
    x = gen_rtx_PLUS (Pmode, XEXP (x, 0),
		      copy_to_mode_reg (Pmode, XEXP (x, 1)));
  else if (GET_CODE (x) == PLUS && CONSTANT_ADDRESS_P (XEXP (x, 0)))
    x = gen_rtx_PLUS (Pmode, XEXP (x, 1),
		      copy_to_mode_reg (Pmode, XEXP (x, 0)));
  else if (GET_CODE (x) == SYMBOL_REF
	   || GET_CODE (x) == CONST
	   || GET_CODE (x) == LABEL_REF)
    x = copy_to_suggested_reg (x, NULL_RTX, Pmode);

  return x;
}

/* Delegitimize an address that was legitimized by the above function.  */

static rtx
sparc_delegitimize_address (rtx x)
{
  x = delegitimize_mem_from_attrs (x);

  if (GET_CODE (x) == LO_SUM && GET_CODE (XEXP (x, 1)) == UNSPEC)
    switch (XINT (XEXP (x, 1), 1))
      {
      case UNSPEC_MOVE_PIC:
      case UNSPEC_TLSLE:
	x = XVECEXP (XEXP (x, 1), 0, 0);
	gcc_assert (GET_CODE (x) == SYMBOL_REF);
	break;
      default:
	break;
      }

  /* This is generated by mov{si,di}_pic_label_ref in PIC mode.  */
  if (GET_CODE (x) == MINUS
      && REG_P (XEXP (x, 0))
      && REGNO (XEXP (x, 0)) == PIC_OFFSET_TABLE_REGNUM
      && GET_CODE (XEXP (x, 1)) == LO_SUM
      && GET_CODE (XEXP (XEXP (x, 1), 1)) == UNSPEC
      && XINT (XEXP (XEXP (x, 1), 1), 1) == UNSPEC_MOVE_PIC_LABEL)
    {
      x = XVECEXP (XEXP (XEXP (x, 1), 1), 0, 0);
      gcc_assert (GET_CODE (x) == LABEL_REF);
    }

  return x;
}

/* SPARC implementation of LEGITIMIZE_RELOAD_ADDRESS.  Returns a value to
   replace the input X, or the original X if no replacement is called for.
   The output parameter *WIN is 1 if the calling macro should goto WIN,
   0 if it should not.

   For SPARC, we wish to handle addresses by splitting them into
   HIGH+LO_SUM pairs, retaining the LO_SUM in the memory reference.
   This cuts the number of extra insns by one.

   Do nothing when generating PIC code and the address is a symbolic
   operand or requires a scratch register.  */

rtx
sparc_legitimize_reload_address (rtx x, machine_mode mode,
				 int opnum, int type,
				 int ind_levels ATTRIBUTE_UNUSED, int *win)
{
  /* Decompose SImode constants into HIGH+LO_SUM.  */
  if (CONSTANT_P (x)
      && (mode != TFmode || TARGET_ARCH64)
      && GET_MODE (x) == SImode
      && GET_CODE (x) != LO_SUM
      && GET_CODE (x) != HIGH
      && sparc_cmodel <= CM_MEDLOW
      && !(flag_pic
	   && (symbolic_operand (x, Pmode) || pic_address_needs_scratch (x))))
    {
      x = gen_rtx_LO_SUM (GET_MODE (x), gen_rtx_HIGH (GET_MODE (x), x), x);
      push_reload (XEXP (x, 0), NULL_RTX, &XEXP (x, 0), NULL,
		   BASE_REG_CLASS, GET_MODE (x), VOIDmode, 0, 0,
		   opnum, (enum reload_type)type);
      *win = 1;
      return x;
    }

  /* We have to recognize what we have already generated above.  */
  if (GET_CODE (x) == LO_SUM && GET_CODE (XEXP (x, 0)) == HIGH)
    {
      push_reload (XEXP (x, 0), NULL_RTX, &XEXP (x, 0), NULL,
		   BASE_REG_CLASS, GET_MODE (x), VOIDmode, 0, 0,
		   opnum, (enum reload_type)type);
      *win = 1;
      return x;
    }

  *win = 0;
  return x;
}

/* Return true if ADDR (a legitimate address expression)
   has an effect that depends on the machine mode it is used for.

   In PIC mode,

      (mem:HI [%l7+a])

   is not equivalent to

      (mem:QI [%l7+a]) (mem:QI [%l7+a+1])

   because [%l7+a+1] is interpreted as the address of (a+1).  */


static bool
sparc_mode_dependent_address_p (const_rtx addr,
				addr_space_t as ATTRIBUTE_UNUSED)
{
  if (flag_pic && GET_CODE (addr) == PLUS)
    {
      rtx op0 = XEXP (addr, 0);
      rtx op1 = XEXP (addr, 1);
      if (op0 == pic_offset_table_rtx
	  && symbolic_operand (op1, VOIDmode))
	return true;
    }

  return false;
}

#ifdef HAVE_GAS_HIDDEN
# define USE_HIDDEN_LINKONCE 1
#else
# define USE_HIDDEN_LINKONCE 0
#endif

static void
get_pc_thunk_name (char name[32], unsigned int regno)
{
  const char *reg_name = reg_names[regno];

  /* Skip the leading '%' as that cannot be used in a
     symbol name.  */
  reg_name += 1;

  if (USE_HIDDEN_LINKONCE)
    sprintf (name, "__sparc_get_pc_thunk.%s", reg_name);
  else
    ASM_GENERATE_INTERNAL_LABEL (name, "LADDPC", regno);
}

/* Wrapper around the load_pcrel_sym{si,di} patterns.  */

static rtx
gen_load_pcrel_sym (rtx op0, rtx op1, rtx op2, rtx op3)
{
  int orig_flag_pic = flag_pic;
  rtx insn;

  /* The load_pcrel_sym{si,di} patterns require absolute addressing.  */
  flag_pic = 0;
  if (TARGET_ARCH64)
    insn = gen_load_pcrel_symdi (op0, op1, op2, op3);
  else
    insn = gen_load_pcrel_symsi (op0, op1, op2, op3);
  flag_pic = orig_flag_pic;

  return insn;
}

/* Emit code to load the GOT register.  */

void
load_got_register (void)
{
  /* In PIC mode, this will retrieve pic_offset_table_rtx.  */
  if (!global_offset_table_rtx)
    global_offset_table_rtx = gen_rtx_REG (Pmode, GLOBAL_OFFSET_TABLE_REGNUM);

  if (TARGET_VXWORKS_RTP)
    emit_insn (gen_vxworks_load_got ());
  else
    {
      /* The GOT symbol is subject to a PC-relative relocation so we need a
	 helper function to add the PC value and thus get the final value.  */
      if (!got_helper_rtx)
	{
	  char name[32];
	  get_pc_thunk_name (name, GLOBAL_OFFSET_TABLE_REGNUM);
	  got_helper_rtx = gen_rtx_SYMBOL_REF (Pmode, ggc_strdup (name));
	}

      emit_insn (gen_load_pcrel_sym (global_offset_table_rtx, sparc_got (),
				     got_helper_rtx,
				     GEN_INT (GLOBAL_OFFSET_TABLE_REGNUM)));
    }

  /* Need to emit this whether or not we obey regdecls,
     since setjmp/longjmp can cause life info to screw up.
     ??? In the case where we don't obey regdecls, this is not sufficient
     since we may not fall out the bottom.  */
  emit_use (global_offset_table_rtx);
}

/* Emit a call instruction with the pattern given by PAT.  ADDR is the
   address of the call target.  */

void
sparc_emit_call_insn (rtx pat, rtx addr)
{
  rtx_insn *insn;

  insn = emit_call_insn (pat);

  /* The PIC register is live on entry to VxWorks PIC PLT entries.  */
  if (TARGET_VXWORKS_RTP
      && flag_pic
      && GET_CODE (addr) == SYMBOL_REF
      && (SYMBOL_REF_DECL (addr)
	  ? !targetm.binds_local_p (SYMBOL_REF_DECL (addr))
	  : !SYMBOL_REF_LOCAL_P (addr)))
    {
      use_reg (&CALL_INSN_FUNCTION_USAGE (insn), pic_offset_table_rtx);
      crtl->uses_pic_offset_table = 1;
    }
}

/* Return 1 if RTX is a MEM which is known to be aligned to at
   least a DESIRED byte boundary.  */

int
mem_min_alignment (rtx mem, int desired)
{
  rtx addr, base, offset;

  /* If it's not a MEM we can't accept it.  */
  if (GET_CODE (mem) != MEM)
    return 0;

  /* Obviously...  */
  if (!TARGET_UNALIGNED_DOUBLES
      && MEM_ALIGN (mem) / BITS_PER_UNIT >= (unsigned)desired)
    return 1;

  /* ??? The rest of the function predates MEM_ALIGN so
     there is probably a bit of redundancy.  */
  addr = XEXP (mem, 0);
  base = offset = NULL_RTX;
  if (GET_CODE (addr) == PLUS)
    {
      if (GET_CODE (XEXP (addr, 0)) == REG)
	{
	  base = XEXP (addr, 0);

	  /* What we are saying here is that if the base
	     REG is aligned properly, the compiler will make
	     sure any REG based index upon it will be so
	     as well.  */
	  if (GET_CODE (XEXP (addr, 1)) == CONST_INT)
	    offset = XEXP (addr, 1);
	  else
	    offset = const0_rtx;
	}
    }
  else if (GET_CODE (addr) == REG)
    {
      base = addr;
      offset = const0_rtx;
    }

  if (base != NULL_RTX)
    {
      int regno = REGNO (base);

      if (regno != HARD_FRAME_POINTER_REGNUM && regno != STACK_POINTER_REGNUM)
	{
	  /* Check if the compiler has recorded some information
	     about the alignment of the base REG.  If reload has
	     completed, we already matched with proper alignments.
	     If not running global_alloc, reload might give us
	     unaligned pointer to local stack though.  */
	  if (((cfun != 0
		&& REGNO_POINTER_ALIGN (regno) >= desired * BITS_PER_UNIT)
	       || (optimize && reload_completed))
	      && (INTVAL (offset) & (desired - 1)) == 0)
	    return 1;
	}
      else
	{
	  if (((INTVAL (offset) - SPARC_STACK_BIAS) & (desired - 1)) == 0)
	    return 1;
	}
    }
  else if (! TARGET_UNALIGNED_DOUBLES
	   || CONSTANT_P (addr)
	   || GET_CODE (addr) == LO_SUM)
    {
      /* Anything else we know is properly aligned unless TARGET_UNALIGNED_DOUBLES
	 is true, in which case we can only assume that an access is aligned if
	 it is to a constant address, or the address involves a LO_SUM.  */
      return 1;
    }

  /* An obviously unaligned address.  */
  return 0;
}


/* Vectors to keep interesting information about registers where it can easily
   be got.  We used to use the actual mode value as the bit number, but there
   are more than 32 modes now.  Instead we use two tables: one indexed by
   hard register number, and one indexed by mode.  */

/* The purpose of sparc_mode_class is to shrink the range of modes so that
   they all fit (as bit numbers) in a 32-bit word (again).  Each real mode is
   mapped into one sparc_mode_class mode.  */

enum sparc_mode_class {
  H_MODE, S_MODE, D_MODE, T_MODE, O_MODE,
  SF_MODE, DF_MODE, TF_MODE, OF_MODE,
  CC_MODE, CCFP_MODE
};

/* Modes for single-word and smaller quantities.  */
#define S_MODES \
  ((1 << (int) H_MODE) | (1 << (int) S_MODE) | (1 << (int) SF_MODE))

/* Modes for double-word and smaller quantities.  */
#define D_MODES (S_MODES | (1 << (int) D_MODE) | (1 << (int) DF_MODE))

/* Modes for quad-word and smaller quantities.  */
#define T_MODES (D_MODES | (1 << (int) T_MODE) | (1 << (int) TF_MODE))

/* Modes for 8-word and smaller quantities.  */
#define O_MODES (T_MODES | (1 << (int) O_MODE) | (1 << (int) OF_MODE))

/* Modes for single-float quantities.  */
#define SF_MODES ((1 << (int) S_MODE) | (1 << (int) SF_MODE))

/* Modes for double-float and smaller quantities.  */
#define DF_MODES (SF_MODES | (1 << (int) D_MODE) | (1 << (int) DF_MODE))

/* Modes for quad-float and smaller quantities.  */
#define TF_MODES (DF_MODES | (1 << (int) TF_MODE))

/* Modes for quad-float pairs and smaller quantities.  */
#define OF_MODES (TF_MODES | (1 << (int) OF_MODE))

/* Modes for double-float only quantities.  */
#define DF_MODES_NO_S ((1 << (int) D_MODE) | (1 << (int) DF_MODE))

/* Modes for quad-float and double-float only quantities.  */
#define TF_MODES_NO_S (DF_MODES_NO_S | (1 << (int) TF_MODE))

/* Modes for quad-float pairs and double-float only quantities.  */
#define OF_MODES_NO_S (TF_MODES_NO_S | (1 << (int) OF_MODE))

/* Modes for condition codes.  */
#define CC_MODES (1 << (int) CC_MODE)
#define CCFP_MODES (1 << (int) CCFP_MODE)

/* Value is 1 if register/mode pair is acceptable on sparc.

   The funny mixture of D and T modes is because integer operations
   do not specially operate on tetra quantities, so non-quad-aligned
   registers can hold quadword quantities (except %o4 and %i4 because
   they cross fixed registers).

   ??? Note that, despite the settings, non-double-aligned parameter
   registers can hold double-word quantities in 32-bit mode.  */

/* This points to either the 32-bit or the 64-bit version.  */
static const int *hard_regno_mode_classes;

static const int hard_32bit_mode_classes[] = {
  S_MODES, S_MODES, T_MODES, S_MODES, T_MODES, S_MODES, D_MODES, S_MODES,
  T_MODES, S_MODES, T_MODES, S_MODES, D_MODES, S_MODES, D_MODES, S_MODES,
  T_MODES, S_MODES, T_MODES, S_MODES, T_MODES, S_MODES, D_MODES, S_MODES,
  T_MODES, S_MODES, T_MODES, S_MODES, D_MODES, S_MODES, D_MODES, S_MODES,

  OF_MODES, SF_MODES, DF_MODES, SF_MODES, OF_MODES, SF_MODES, DF_MODES, SF_MODES,
  OF_MODES, SF_MODES, DF_MODES, SF_MODES, OF_MODES, SF_MODES, DF_MODES, SF_MODES,
  OF_MODES, SF_MODES, DF_MODES, SF_MODES, OF_MODES, SF_MODES, DF_MODES, SF_MODES,
  OF_MODES, SF_MODES, DF_MODES, SF_MODES, TF_MODES, SF_MODES, DF_MODES, SF_MODES,

  /* FP regs f32 to f63.  Only the even numbered registers actually exist,
     and none can hold SFmode/SImode values.  */
  OF_MODES_NO_S, 0, DF_MODES_NO_S, 0, OF_MODES_NO_S, 0, DF_MODES_NO_S, 0,
  OF_MODES_NO_S, 0, DF_MODES_NO_S, 0, OF_MODES_NO_S, 0, DF_MODES_NO_S, 0,
  OF_MODES_NO_S, 0, DF_MODES_NO_S, 0, OF_MODES_NO_S, 0, DF_MODES_NO_S, 0,
  OF_MODES_NO_S, 0, DF_MODES_NO_S, 0, TF_MODES_NO_S, 0, DF_MODES_NO_S, 0,

  /* %fcc[0123] */
  CCFP_MODES, CCFP_MODES, CCFP_MODES, CCFP_MODES,

  /* %icc, %sfp, %gsr */
  CC_MODES, 0, D_MODES
};

static const int hard_64bit_mode_classes[] = {
  D_MODES, D_MODES, T_MODES, D_MODES, T_MODES, D_MODES, T_MODES, D_MODES,
  O_MODES, D_MODES, T_MODES, D_MODES, T_MODES, D_MODES, T_MODES, D_MODES,
  T_MODES, D_MODES, T_MODES, D_MODES, T_MODES, D_MODES, T_MODES, D_MODES,
  O_MODES, D_MODES, T_MODES, D_MODES, T_MODES, D_MODES, T_MODES, D_MODES,

  OF_MODES, SF_MODES, DF_MODES, SF_MODES, OF_MODES, SF_MODES, DF_MODES, SF_MODES,
  OF_MODES, SF_MODES, DF_MODES, SF_MODES, OF_MODES, SF_MODES, DF_MODES, SF_MODES,
  OF_MODES, SF_MODES, DF_MODES, SF_MODES, OF_MODES, SF_MODES, DF_MODES, SF_MODES,
  OF_MODES, SF_MODES, DF_MODES, SF_MODES, TF_MODES, SF_MODES, DF_MODES, SF_MODES,

  /* FP regs f32 to f63.  Only the even numbered registers actually exist,
     and none can hold SFmode/SImode values.  */
  OF_MODES_NO_S, 0, DF_MODES_NO_S, 0, OF_MODES_NO_S, 0, DF_MODES_NO_S, 0,
  OF_MODES_NO_S, 0, DF_MODES_NO_S, 0, OF_MODES_NO_S, 0, DF_MODES_NO_S, 0,
  OF_MODES_NO_S, 0, DF_MODES_NO_S, 0, OF_MODES_NO_S, 0, DF_MODES_NO_S, 0,
  OF_MODES_NO_S, 0, DF_MODES_NO_S, 0, TF_MODES_NO_S, 0, DF_MODES_NO_S, 0,

  /* %fcc[0123] */
  CCFP_MODES, CCFP_MODES, CCFP_MODES, CCFP_MODES,

  /* %icc, %sfp, %gsr */
  CC_MODES, 0, D_MODES
};

static int sparc_mode_class [NUM_MACHINE_MODES];

enum reg_class sparc_regno_reg_class[FIRST_PSEUDO_REGISTER];

static void
sparc_init_modes (void)
{
  int i;

  for (i = 0; i < NUM_MACHINE_MODES; i++)
    {
      machine_mode m = (machine_mode) i;
      unsigned int size = GET_MODE_SIZE (m);

      switch (GET_MODE_CLASS (m))
	{
	case MODE_INT:
	case MODE_PARTIAL_INT:
	case MODE_COMPLEX_INT:
	  if (size < 4)
	    sparc_mode_class[i] = 1 << (int) H_MODE;
	  else if (size == 4)
	    sparc_mode_class[i] = 1 << (int) S_MODE;
	  else if (size == 8)
	    sparc_mode_class[i] = 1 << (int) D_MODE;
	  else if (size == 16)
	    sparc_mode_class[i] = 1 << (int) T_MODE;
	  else if (size == 32)
	    sparc_mode_class[i] = 1 << (int) O_MODE;
	  else
	    sparc_mode_class[i] = 0;
	  break;
	case MODE_VECTOR_INT:
	  if (size == 4)
	    sparc_mode_class[i] = 1 << (int) SF_MODE;
	  else if (size == 8)
	    sparc_mode_class[i] = 1 << (int) DF_MODE;
	  else
	    sparc_mode_class[i] = 0;
	  break;
	case MODE_FLOAT:
	case MODE_COMPLEX_FLOAT:
	  if (size == 4)
	    sparc_mode_class[i] = 1 << (int) SF_MODE;
	  else if (size == 8)
	    sparc_mode_class[i] = 1 << (int) DF_MODE;
	  else if (size == 16)
	    sparc_mode_class[i] = 1 << (int) TF_MODE;
	  else if (size == 32)
	    sparc_mode_class[i] = 1 << (int) OF_MODE;
	  else
	    sparc_mode_class[i] = 0;
	  break;
	case MODE_CC:
	  if (m == CCFPmode || m == CCFPEmode)
	    sparc_mode_class[i] = 1 << (int) CCFP_MODE;
	  else
	    sparc_mode_class[i] = 1 << (int) CC_MODE;
	  break;
	default:
	  sparc_mode_class[i] = 0;
	  break;
	}
    }

  if (TARGET_ARCH64)
    hard_regno_mode_classes = hard_64bit_mode_classes;
  else
    hard_regno_mode_classes = hard_32bit_mode_classes;

  /* Initialize the array used by REGNO_REG_CLASS.  */
  for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
    {
      if (i < 16 && TARGET_V8PLUS)
	sparc_regno_reg_class[i] = I64_REGS;
      else if (i < 32 || i == FRAME_POINTER_REGNUM)
	sparc_regno_reg_class[i] = GENERAL_REGS;
      else if (i < 64)
	sparc_regno_reg_class[i] = FP_REGS;
      else if (i < 96)
	sparc_regno_reg_class[i] = EXTRA_FP_REGS;
      else if (i < 100)
	sparc_regno_reg_class[i] = FPCC_REGS;
      else
	sparc_regno_reg_class[i] = NO_REGS;
    }
}

/* Return whether REGNO, a global or FP register, must be saved/restored.  */

static inline bool
save_global_or_fp_reg_p (unsigned int regno,
			 int leaf_function ATTRIBUTE_UNUSED)
{
  return !call_used_regs[regno] && df_regs_ever_live_p (regno);
}

/* Return whether the return address register (%i7) is needed.  */

static inline bool
return_addr_reg_needed_p (int leaf_function)
{
  /* If it is live, for example because of __builtin_return_address (0).  */
  if (df_regs_ever_live_p (RETURN_ADDR_REGNUM))
    return true;

  /* Otherwise, it is needed as save register if %o7 is clobbered.  */
  if (!leaf_function
      /* Loading the GOT register clobbers %o7.  */
      || crtl->uses_pic_offset_table
      || df_regs_ever_live_p (INCOMING_RETURN_ADDR_REGNUM))
    return true;

  return false;
}

/* Return whether REGNO, a local or in register, must be saved/restored.  */

static bool
save_local_or_in_reg_p (unsigned int regno, int leaf_function)
{
  /* General case: call-saved registers live at some point.  */
  if (!call_used_regs[regno] && df_regs_ever_live_p (regno))
    return true;

  /* Frame pointer register (%fp) if needed.  */
  if (regno == HARD_FRAME_POINTER_REGNUM && frame_pointer_needed)
    return true;

  /* Return address register (%i7) if needed.  */
  if (regno == RETURN_ADDR_REGNUM && return_addr_reg_needed_p (leaf_function))
    return true;

  /* GOT register (%l7) if needed.  */
  if (regno == PIC_OFFSET_TABLE_REGNUM && crtl->uses_pic_offset_table)
    return true;

  /* If the function accesses prior frames, the frame pointer and the return
     address of the previous frame must be saved on the stack.  */
  if (crtl->accesses_prior_frames
      && (regno == HARD_FRAME_POINTER_REGNUM || regno == RETURN_ADDR_REGNUM))
    return true;

  return false;
}

/* Compute the frame size required by the function.  This function is called
   during the reload pass and also by sparc_expand_prologue.  */

HOST_WIDE_INT
sparc_compute_frame_size (HOST_WIDE_INT size, int leaf_function)
{
  HOST_WIDE_INT frame_size, apparent_frame_size;
  int args_size, n_global_fp_regs = 0;
  bool save_local_in_regs_p = false;
  unsigned int i;

  /* If the function allocates dynamic stack space, the dynamic offset is
     computed early and contains REG_PARM_STACK_SPACE, so we need to cope.  */
  if (leaf_function && !cfun->calls_alloca)
    args_size = 0;
  else
    args_size = crtl->outgoing_args_size + REG_PARM_STACK_SPACE (cfun->decl);

  /* Calculate space needed for global registers.  */
  if (TARGET_ARCH64)
    {
      for (i = 0; i < 8; i++)
	if (save_global_or_fp_reg_p (i, 0))
	  n_global_fp_regs += 2;
    }
  else
    {
      for (i = 0; i < 8; i += 2)
	if (save_global_or_fp_reg_p (i, 0)
	    || save_global_or_fp_reg_p (i + 1, 0))
	  n_global_fp_regs += 2;
    }

  /* In the flat window model, find out which local and in registers need to
     be saved.  We don't reserve space in the current frame for them as they
     will be spilled into the register window save area of the caller's frame.
     However, as soon as we use this register window save area, we must create
     that of the current frame to make it the live one.  */
  if (TARGET_FLAT)
    for (i = 16; i < 32; i++)
      if (save_local_or_in_reg_p (i, leaf_function))
	{
	 save_local_in_regs_p = true;
	 break;
	}

  /* Calculate space needed for FP registers.  */
  for (i = 32; i < (TARGET_V9 ? 96 : 64); i += 2)
    if (save_global_or_fp_reg_p (i, 0) || save_global_or_fp_reg_p (i + 1, 0))
      n_global_fp_regs += 2;

  if (size == 0
      && n_global_fp_regs == 0
      && args_size == 0
      && !save_local_in_regs_p)
    frame_size = apparent_frame_size = 0;
  else
    {
      /* We subtract STARTING_FRAME_OFFSET, remember it's negative.  */
      apparent_frame_size = ROUND_UP (size - STARTING_FRAME_OFFSET, 8);
      apparent_frame_size += n_global_fp_regs * 4;

      /* We need to add the size of the outgoing argument area.  */
      frame_size = apparent_frame_size + ROUND_UP (args_size, 8);

      /* And that of the register window save area.  */
      frame_size += FIRST_PARM_OFFSET (cfun->decl);

      /* Finally, bump to the appropriate alignment.  */
      frame_size = SPARC_STACK_ALIGN (frame_size);
    }

  /* Set up values for use in prologue and epilogue.  */
  sparc_frame_size = frame_size;
  sparc_apparent_frame_size = apparent_frame_size;
  sparc_n_global_fp_regs = n_global_fp_regs;
  sparc_save_local_in_regs_p = save_local_in_regs_p;

  return frame_size;
}

/* Implement the macro INITIAL_ELIMINATION_OFFSET, return the OFFSET.  */

int
sparc_initial_elimination_offset (int to)
{
  int offset;

  if (to == STACK_POINTER_REGNUM)
    offset = sparc_compute_frame_size (get_frame_size (), crtl->is_leaf);
  else
    offset = 0;

  offset += SPARC_STACK_BIAS;
  return offset;
}

/* Output any necessary .register pseudo-ops.  */

void
sparc_output_scratch_registers (FILE *file ATTRIBUTE_UNUSED)
{
#ifdef HAVE_AS_REGISTER_PSEUDO_OP
  int i;

  if (TARGET_ARCH32)
    return;

  /* Check if %g[2367] were used without
     .register being printed for them already.  */
  for (i = 2; i < 8; i++)
    {
      if (df_regs_ever_live_p (i)
	  && ! sparc_hard_reg_printed [i])
	{
	  sparc_hard_reg_printed [i] = 1;
	  /* %g7 is used as TLS base register, use #ignore
	     for it instead of #scratch.  */
	  fprintf (file, "\t.register\t%%g%d, #%s\n", i,
		   i == 7 ? "ignore" : "scratch");
	}
      if (i == 3) i = 5;
    }
#endif
}

#define PROBE_INTERVAL (1 << STACK_CHECK_PROBE_INTERVAL_EXP)

#if PROBE_INTERVAL > 4096
#error Cannot use indexed addressing mode for stack probing
#endif

/* Emit code to probe a range of stack addresses from FIRST to FIRST+SIZE,
   inclusive.  These are offsets from the current stack pointer.

   Note that we don't use the REG+REG addressing mode for the probes because
   of the stack bias in 64-bit mode.  And it doesn't really buy us anything
   so the advantages of having a single code win here.  */

static void
sparc_emit_probe_stack_range (HOST_WIDE_INT first, HOST_WIDE_INT size)
{
  rtx g1 = gen_rtx_REG (Pmode, 1);

  /* See if we have a constant small number of probes to generate.  If so,
     that's the easy case.  */
  if (size <= PROBE_INTERVAL)
    {
      emit_move_insn (g1, GEN_INT (first));
      emit_insn (gen_rtx_SET (g1,
			      gen_rtx_MINUS (Pmode, stack_pointer_rtx, g1)));
      emit_stack_probe (plus_constant (Pmode, g1, -size));
    }

  /* The run-time loop is made up of 9 insns in the generic case while the
     compile-time loop is made up of 4+2*(n-2) insns for n # of intervals.  */
  else if (size <= 4 * PROBE_INTERVAL)
    {
      HOST_WIDE_INT i;

      emit_move_insn (g1, GEN_INT (first + PROBE_INTERVAL));
      emit_insn (gen_rtx_SET (g1,
			      gen_rtx_MINUS (Pmode, stack_pointer_rtx, g1)));
      emit_stack_probe (g1);

      /* Probe at FIRST + N * PROBE_INTERVAL for values of N from 2 until
	 it exceeds SIZE.  If only two probes are needed, this will not
	 generate any code.  Then probe at FIRST + SIZE.  */
      for (i = 2 * PROBE_INTERVAL; i < size; i += PROBE_INTERVAL)
	{
	  emit_insn (gen_rtx_SET (g1,
				  plus_constant (Pmode, g1, -PROBE_INTERVAL)));
	  emit_stack_probe (g1);
	}

      emit_stack_probe (plus_constant (Pmode, g1,
				       (i - PROBE_INTERVAL) - size));
    }

  /* Otherwise, do the same as above, but in a loop.  Note that we must be
     extra careful with variables wrapping around because we might be at
     the very top (or the very bottom) of the address space and we have
     to be able to handle this case properly; in particular, we use an
     equality test for the loop condition.  */
  else
    {
      HOST_WIDE_INT rounded_size;
      rtx g4 = gen_rtx_REG (Pmode, 4);

      emit_move_insn (g1, GEN_INT (first));


      /* Step 1: round SIZE to the previous multiple of the interval.  */

      rounded_size = ROUND_DOWN (size, PROBE_INTERVAL);
      emit_move_insn (g4, GEN_INT (rounded_size));


      /* Step 2: compute initial and final value of the loop counter.  */

      /* TEST_ADDR = SP + FIRST.  */
      emit_insn (gen_rtx_SET (g1,
			      gen_rtx_MINUS (Pmode, stack_pointer_rtx, g1)));

      /* LAST_ADDR = SP + FIRST + ROUNDED_SIZE.  */
      emit_insn (gen_rtx_SET (g4, gen_rtx_MINUS (Pmode, g1, g4)));


      /* Step 3: the loop

	 while (TEST_ADDR != LAST_ADDR)
	   {
	     TEST_ADDR = TEST_ADDR + PROBE_INTERVAL
	     probe at TEST_ADDR
	   }

	 probes at FIRST + N * PROBE_INTERVAL for values of N from 1
	 until it is equal to ROUNDED_SIZE.  */

      if (TARGET_ARCH64)
	emit_insn (gen_probe_stack_rangedi (g1, g1, g4));
      else
	emit_insn (gen_probe_stack_rangesi (g1, g1, g4));


      /* Step 4: probe at FIRST + SIZE if we cannot assert at compile-time
	 that SIZE is equal to ROUNDED_SIZE.  */

      if (size != rounded_size)
	emit_stack_probe (plus_constant (Pmode, g4, rounded_size - size));
    }

  /* Make sure nothing is scheduled before we are done.  */
  emit_insn (gen_blockage ());
}

/* Probe a range of stack addresses from REG1 to REG2 inclusive.  These are
   absolute addresses.  */

const char *
output_probe_stack_range (rtx reg1, rtx reg2)
{
  static int labelno = 0;
  char loop_lab[32];
  rtx xops[2];

  ASM_GENERATE_INTERNAL_LABEL (loop_lab, "LPSRL", labelno++);

  /* Loop.  */
  ASM_OUTPUT_INTERNAL_LABEL (asm_out_file, loop_lab);

  /* TEST_ADDR = TEST_ADDR + PROBE_INTERVAL.  */
  xops[0] = reg1;
  xops[1] = GEN_INT (-PROBE_INTERVAL);
  output_asm_insn ("add\t%0, %1, %0", xops);

  /* Test if TEST_ADDR == LAST_ADDR.  */
  xops[1] = reg2;
  output_asm_insn ("cmp\t%0, %1", xops);

  /* Probe at TEST_ADDR and branch.  */
  if (TARGET_ARCH64)
    fputs ("\tbne,pt\t%xcc,", asm_out_file);
  else
    fputs ("\tbne\t", asm_out_file);
  assemble_name_raw (asm_out_file, loop_lab);
  fputc ('\n', asm_out_file);
  xops[1] = GEN_INT (SPARC_STACK_BIAS);
  output_asm_insn (" st\t%%g0, [%0+%1]", xops);

  return "";
}

/* Emit code to save/restore registers from LOW to HIGH at BASE+OFFSET as
   needed.  LOW is supposed to be double-word aligned for 32-bit registers.
   SAVE_P decides whether a register must be saved/restored.  ACTION_TRUE
   is the action to be performed if SAVE_P returns true and ACTION_FALSE
   the action to be performed if it returns false.  Return the new offset.  */

typedef bool (*sorr_pred_t) (unsigned int, int);
typedef enum { SORR_NONE, SORR_ADVANCE, SORR_SAVE, SORR_RESTORE } sorr_act_t;

static int
emit_save_or_restore_regs (unsigned int low, unsigned int high, rtx base,
			   int offset, int leaf_function, sorr_pred_t save_p,
			   sorr_act_t action_true, sorr_act_t action_false)
{
  unsigned int i;
  rtx mem;
  rtx_insn *insn;

  if (TARGET_ARCH64 && high <= 32)
    {
      int fp_offset = -1;

      for (i = low; i < high; i++)
	{
	  if (save_p (i, leaf_function))
	    {
	      mem = gen_frame_mem (DImode, plus_constant (Pmode,
							  base, offset));
	      if (action_true == SORR_SAVE)
		{
		  insn = emit_move_insn (mem, gen_rtx_REG (DImode, i));
		  RTX_FRAME_RELATED_P (insn) = 1;
		}
	      else  /* action_true == SORR_RESTORE */
		{
		  /* The frame pointer must be restored last since its old
		     value may be used as base address for the frame.  This
		     is problematic in 64-bit mode only because of the lack
		     of double-word load instruction.  */
		  if (i == HARD_FRAME_POINTER_REGNUM)
		    fp_offset = offset;
		  else
		    emit_move_insn (gen_rtx_REG (DImode, i), mem);
		}
	      offset += 8;
	    }
	  else if (action_false == SORR_ADVANCE)
	    offset += 8;
	}

      if (fp_offset >= 0)
	{
	  mem = gen_frame_mem (DImode, plus_constant (Pmode, base, fp_offset));
	  emit_move_insn (hard_frame_pointer_rtx, mem);
	}
    }
  else
    {
      for (i = low; i < high; i += 2)
	{
	  bool reg0 = save_p (i, leaf_function);
	  bool reg1 = save_p (i + 1, leaf_function);
	  machine_mode mode;
	  int regno;

	  if (reg0 && reg1)
	    {
	      mode = SPARC_INT_REG_P (i) ? E_DImode : E_DFmode;
	      regno = i;
	    }
	  else if (reg0)
	    {
	      mode = SPARC_INT_REG_P (i) ? E_SImode : E_SFmode;
	      regno = i;
	    }
	  else if (reg1)
	    {
	      mode = SPARC_INT_REG_P (i) ? E_SImode : E_SFmode;
	      regno = i + 1;
	      offset += 4;
	    }
	  else
	    {
	      if (action_false == SORR_ADVANCE)
		offset += 8;
	      continue;
	    }

	  mem = gen_frame_mem (mode, plus_constant (Pmode, base, offset));
	  if (action_true == SORR_SAVE)
	    {
	      insn = emit_move_insn (mem, gen_rtx_REG (mode, regno));
	      RTX_FRAME_RELATED_P (insn) = 1;
	      if (mode == DImode)
		{
		  rtx set1, set2;
		  mem = gen_frame_mem (SImode, plus_constant (Pmode, base,
							      offset));
		  set1 = gen_rtx_SET (mem, gen_rtx_REG (SImode, regno));
		  RTX_FRAME_RELATED_P (set1) = 1;
		  mem
		    = gen_frame_mem (SImode, plus_constant (Pmode, base,
							    offset + 4));
		  set2 = gen_rtx_SET (mem, gen_rtx_REG (SImode, regno + 1));
		  RTX_FRAME_RELATED_P (set2) = 1;
		  add_reg_note (insn, REG_FRAME_RELATED_EXPR,
				gen_rtx_PARALLEL (VOIDmode,
						  gen_rtvec (2, set1, set2)));
		}
	    }
	  else  /* action_true == SORR_RESTORE */
	    emit_move_insn (gen_rtx_REG (mode, regno), mem);

	  /* Bump and round down to double word
	     in case we already bumped by 4.  */
	  offset = ROUND_DOWN (offset + 8, 8);
	}
    }

  return offset;
}

/* Emit code to adjust BASE to OFFSET.  Return the new base.  */

static rtx
emit_adjust_base_to_offset (rtx base, int offset)
{
  /* ??? This might be optimized a little as %g1 might already have a
     value close enough that a single add insn will do.  */
  /* ??? Although, all of this is probably only a temporary fix because
     if %g1 can hold a function result, then sparc_expand_epilogue will
     lose (the result will be clobbered).  */
  rtx new_base = gen_rtx_REG (Pmode, 1);
  emit_move_insn (new_base, GEN_INT (offset));
  emit_insn (gen_rtx_SET (new_base, gen_rtx_PLUS (Pmode, base, new_base)));
  return new_base;
}

/* Emit code to save/restore call-saved global and FP registers.  */

static void
emit_save_or_restore_global_fp_regs (rtx base, int offset, sorr_act_t action)
{
  if (offset < -4096 || offset + sparc_n_global_fp_regs * 4 > 4095)
    {
      base = emit_adjust_base_to_offset  (base, offset);
      offset = 0;
    }

  offset
    = emit_save_or_restore_regs (0, 8, base, offset, 0,
				 save_global_or_fp_reg_p, action, SORR_NONE);
  emit_save_or_restore_regs (32, TARGET_V9 ? 96 : 64, base, offset, 0,
			     save_global_or_fp_reg_p, action, SORR_NONE);
}

/* Emit code to save/restore call-saved local and in registers.  */

static void
emit_save_or_restore_local_in_regs (rtx base, int offset, sorr_act_t action)
{
  if (offset < -4096 || offset + 16 * UNITS_PER_WORD > 4095)
    {
      base = emit_adjust_base_to_offset  (base, offset);
      offset = 0;
    }

  emit_save_or_restore_regs (16, 32, base, offset, sparc_leaf_function_p,
			     save_local_or_in_reg_p, action, SORR_ADVANCE);
}

/* Emit a window_save insn.  */

static rtx_insn *
emit_window_save (rtx increment)
{
  rtx_insn *insn = emit_insn (gen_window_save (increment));
  RTX_FRAME_RELATED_P (insn) = 1;

  /* The incoming return address (%o7) is saved in %i7.  */
  add_reg_note (insn, REG_CFA_REGISTER,
		gen_rtx_SET (gen_rtx_REG (Pmode, RETURN_ADDR_REGNUM),
			     gen_rtx_REG (Pmode,
					  INCOMING_RETURN_ADDR_REGNUM)));

  /* The window save event.  */
  add_reg_note (insn, REG_CFA_WINDOW_SAVE, const0_rtx);

  /* The CFA is %fp, the hard frame pointer.  */
  add_reg_note (insn, REG_CFA_DEF_CFA,
		plus_constant (Pmode, hard_frame_pointer_rtx,
			       INCOMING_FRAME_SP_OFFSET));

  return insn;
}

/* Generate an increment for the stack pointer.  */

static rtx
gen_stack_pointer_inc (rtx increment)
{
  return gen_rtx_SET (stack_pointer_rtx,
		      gen_rtx_PLUS (Pmode,
				    stack_pointer_rtx,
				    increment));
}

/* Expand the function prologue.  The prologue is responsible for reserving
   storage for the frame, saving the call-saved registers and loading the
   GOT register if needed.  */

void
sparc_expand_prologue (void)
{
  HOST_WIDE_INT size;
  rtx_insn *insn;

  /* Compute a snapshot of crtl->uses_only_leaf_regs.  Relying
     on the final value of the flag means deferring the prologue/epilogue
     expansion until just before the second scheduling pass, which is too
     late to emit multiple epilogues or return insns.

     Of course we are making the assumption that the value of the flag
     will not change between now and its final value.  Of the three parts
     of the formula, only the last one can reasonably vary.  Let's take a
     closer look, after assuming that the first two ones are set to true
     (otherwise the last value is effectively silenced).

     If only_leaf_regs_used returns false, the global predicate will also
     be false so the actual frame size calculated below will be positive.
     As a consequence, the save_register_window insn will be emitted in
     the instruction stream; now this insn explicitly references %fp
     which is not a leaf register so only_leaf_regs_used will always
     return false subsequently.

     If only_leaf_regs_used returns true, we hope that the subsequent
     optimization passes won't cause non-leaf registers to pop up.  For
     example, the regrename pass has special provisions to not rename to
     non-leaf registers in a leaf function.  */
  sparc_leaf_function_p
    = optimize > 0 && crtl->is_leaf && only_leaf_regs_used ();

  size = sparc_compute_frame_size (get_frame_size(), sparc_leaf_function_p);

  if (flag_stack_usage_info)
    current_function_static_stack_size = size;

  if (flag_stack_check == STATIC_BUILTIN_STACK_CHECK)
    {
      if (crtl->is_leaf && !cfun->calls_alloca)
	{
	  if (size > PROBE_INTERVAL && size > STACK_CHECK_PROTECT)
	    sparc_emit_probe_stack_range (STACK_CHECK_PROTECT,
					  size - STACK_CHECK_PROTECT);
	}
      else if (size > 0)
	sparc_emit_probe_stack_range (STACK_CHECK_PROTECT, size);
    }

  if (size == 0)
    ; /* do nothing.  */
  else if (sparc_leaf_function_p)
    {
      rtx size_int_rtx = GEN_INT (-size);

      if (size <= 4096)
	insn = emit_insn (gen_stack_pointer_inc (size_int_rtx));
      else if (size <= 8192)
	{
	  insn = emit_insn (gen_stack_pointer_inc (GEN_INT (-4096)));
	  RTX_FRAME_RELATED_P (insn) = 1;

	  /* %sp is still the CFA register.  */
	  insn = emit_insn (gen_stack_pointer_inc (GEN_INT (4096 - size)));
	}
      else
	{
	  rtx size_rtx = gen_rtx_REG (Pmode, 1);
	  emit_move_insn (size_rtx, size_int_rtx);
	  insn = emit_insn (gen_stack_pointer_inc (size_rtx));
	  add_reg_note (insn, REG_FRAME_RELATED_EXPR,
			gen_stack_pointer_inc (size_int_rtx));
	}

      RTX_FRAME_RELATED_P (insn) = 1;
    }
  else
    {
      rtx size_int_rtx = GEN_INT (-size);

      if (size <= 4096)
	emit_window_save (size_int_rtx);
      else if (size <= 8192)
	{
	  emit_window_save (GEN_INT (-4096));

	  /* %sp is not the CFA register anymore.  */
	  emit_insn (gen_stack_pointer_inc (GEN_INT (4096 - size)));

	  /* Make sure no %fp-based store is issued until after the frame is
	     established.  The offset between the frame pointer and the stack
	     pointer is calculated relative to the value of the stack pointer
	     at the end of the function prologue, and moving instructions that
	     access the stack via the frame pointer between the instructions
	     that decrement the stack pointer could result in accessing the
	     register window save area, which is volatile.  */
	  emit_insn (gen_frame_blockage ());
	}
      else
	{
	  rtx size_rtx = gen_rtx_REG (Pmode, 1);
	  emit_move_insn (size_rtx, size_int_rtx);
	  emit_window_save (size_rtx);
	}
    }

  if (sparc_leaf_function_p)
    {
      sparc_frame_base_reg = stack_pointer_rtx;
      sparc_frame_base_offset = size + SPARC_STACK_BIAS;
    }
  else
    {
      sparc_frame_base_reg = hard_frame_pointer_rtx;
      sparc_frame_base_offset = SPARC_STACK_BIAS;
    }

  if (sparc_n_global_fp_regs > 0)
    emit_save_or_restore_global_fp_regs (sparc_frame_base_reg,
				         sparc_frame_base_offset
					   - sparc_apparent_frame_size,
					 SORR_SAVE);

  /* Load the GOT register if needed.  */
  if (crtl->uses_pic_offset_table)
    load_got_register ();

  /* Advertise that the data calculated just above are now valid.  */
  sparc_prologue_data_valid_p = true;
}

/* Expand the function prologue.  The prologue is responsible for reserving
   storage for the frame, saving the call-saved registers and loading the
   GOT register if needed.  */

void
sparc_flat_expand_prologue (void)
{
  HOST_WIDE_INT size;
  rtx_insn *insn;

  sparc_leaf_function_p = optimize > 0 && crtl->is_leaf;

  size = sparc_compute_frame_size (get_frame_size(), sparc_leaf_function_p);

  if (flag_stack_usage_info)
    current_function_static_stack_size = size;

  if (flag_stack_check == STATIC_BUILTIN_STACK_CHECK)
    {
      if (crtl->is_leaf && !cfun->calls_alloca)
	{
	  if (size > PROBE_INTERVAL && size > STACK_CHECK_PROTECT)
	    sparc_emit_probe_stack_range (STACK_CHECK_PROTECT,
					  size - STACK_CHECK_PROTECT);
	}
      else if (size > 0)
	sparc_emit_probe_stack_range (STACK_CHECK_PROTECT, size);
    }

  if (sparc_save_local_in_regs_p)
    emit_save_or_restore_local_in_regs (stack_pointer_rtx, SPARC_STACK_BIAS,
					SORR_SAVE);

  if (size == 0)
    ; /* do nothing.  */
  else
    {
      rtx size_int_rtx, size_rtx;

      size_rtx = size_int_rtx = GEN_INT (-size);

      /* We establish the frame (i.e. decrement the stack pointer) first, even
	 if we use a frame pointer, because we cannot clobber any call-saved
	 registers, including the frame pointer, if we haven't created a new
	 register save area, for the sake of compatibility with the ABI.  */
      if (size <= 4096)
	insn = emit_insn (gen_stack_pointer_inc (size_int_rtx));
      else if (size <= 8192 && !frame_pointer_needed)
	{
	  insn = emit_insn (gen_stack_pointer_inc (GEN_INT (-4096)));
	  RTX_FRAME_RELATED_P (insn) = 1;
	  insn = emit_insn (gen_stack_pointer_inc (GEN_INT (4096 - size)));
	}
      else
	{
	  size_rtx = gen_rtx_REG (Pmode, 1);
	  emit_move_insn (size_rtx, size_int_rtx);
	  insn = emit_insn (gen_stack_pointer_inc (size_rtx));
	  add_reg_note (insn, REG_CFA_ADJUST_CFA,
			gen_stack_pointer_inc (size_int_rtx));
	}
      RTX_FRAME_RELATED_P (insn) = 1;

      /* Ensure nothing is scheduled until after the frame is established.  */
      emit_insn (gen_blockage ());

      if (frame_pointer_needed)
	{
	  insn = emit_insn (gen_rtx_SET (hard_frame_pointer_rtx,
					 gen_rtx_MINUS (Pmode,
							stack_pointer_rtx,
							size_rtx)));
	  RTX_FRAME_RELATED_P (insn) = 1;

	  add_reg_note (insn, REG_CFA_ADJUST_CFA,
			gen_rtx_SET (hard_frame_pointer_rtx,
				     plus_constant (Pmode, stack_pointer_rtx,
						    size)));
	}

      if (return_addr_reg_needed_p (sparc_leaf_function_p))
	{
	  rtx o7 = gen_rtx_REG (Pmode, INCOMING_RETURN_ADDR_REGNUM);
	  rtx i7 = gen_rtx_REG (Pmode, RETURN_ADDR_REGNUM);

	  insn = emit_move_insn (i7, o7);
	  RTX_FRAME_RELATED_P (insn) = 1;

	  add_reg_note (insn, REG_CFA_REGISTER, gen_rtx_SET (i7, o7));

	  /* Prevent this instruction from ever being considered dead,
	     even if this function has no epilogue.  */
	  emit_use (i7);
	}
    }

  if (frame_pointer_needed)
    {
      sparc_frame_base_reg = hard_frame_pointer_rtx;
      sparc_frame_base_offset = SPARC_STACK_BIAS;
    }
  else
    {
      sparc_frame_base_reg = stack_pointer_rtx;
      sparc_frame_base_offset = size + SPARC_STACK_BIAS;
    }

  if (sparc_n_global_fp_regs > 0)
    emit_save_or_restore_global_fp_regs (sparc_frame_base_reg,
				         sparc_frame_base_offset
					   - sparc_apparent_frame_size,
					 SORR_SAVE);

  /* Load the GOT register if needed.  */
  if (crtl->uses_pic_offset_table)
    load_got_register ();

  /* Advertise that the data calculated just above are now valid.  */
  sparc_prologue_data_valid_p = true;
}

/* This function generates the assembly code for function entry, which boils
   down to emitting the necessary .register directives.  */

static void
sparc_asm_function_prologue (FILE *file)
{
  /* Check that the assumption we made in sparc_expand_prologue is valid.  */
  if (!TARGET_FLAT)
    gcc_assert (sparc_leaf_function_p == crtl->uses_only_leaf_regs);

  sparc_output_scratch_registers (file);
}

/* Expand the function epilogue, either normal or part of a sibcall.
   We emit all the instructions except the return or the call.  */

void
sparc_expand_epilogue (bool for_eh)
{
  HOST_WIDE_INT size = sparc_frame_size;

  if (cfun->calls_alloca)
    emit_insn (gen_frame_blockage ());

  if (sparc_n_global_fp_regs > 0)
    emit_save_or_restore_global_fp_regs (sparc_frame_base_reg,
				         sparc_frame_base_offset
					   - sparc_apparent_frame_size,
					 SORR_RESTORE);

  if (size == 0 || for_eh)
    ; /* do nothing.  */
  else if (sparc_leaf_function_p)
    {
      if (size <= 4096)
	emit_insn (gen_stack_pointer_inc (GEN_INT (size)));
      else if (size <= 8192)
	{
	  emit_insn (gen_stack_pointer_inc (GEN_INT (4096)));
	  emit_insn (gen_stack_pointer_inc (GEN_INT (size - 4096)));
	}
      else
	{
	  rtx reg = gen_rtx_REG (Pmode, 1);
	  emit_move_insn (reg, GEN_INT (size));
	  emit_insn (gen_stack_pointer_inc (reg));
	}
    }
}

/* Expand the function epilogue, either normal or part of a sibcall.
   We emit all the instructions except the return or the call.  */

void
sparc_flat_expand_epilogue (bool for_eh)
{
  HOST_WIDE_INT size = sparc_frame_size;

  if (sparc_n_global_fp_regs > 0)
    emit_save_or_restore_global_fp_regs (sparc_frame_base_reg,
				         sparc_frame_base_offset
					   - sparc_apparent_frame_size,
					 SORR_RESTORE);

  /* If we have a frame pointer, we'll need both to restore it before the
     frame is destroyed and use its current value in destroying the frame.
     Since we don't have an atomic way to do that in the flat window model,
     we save the current value into a temporary register (%g1).  */
  if (frame_pointer_needed && !for_eh)
    emit_move_insn (gen_rtx_REG (Pmode, 1), hard_frame_pointer_rtx);

  if (return_addr_reg_needed_p (sparc_leaf_function_p))
    emit_move_insn (gen_rtx_REG (Pmode, INCOMING_RETURN_ADDR_REGNUM),
		    gen_rtx_REG (Pmode, RETURN_ADDR_REGNUM));

  if (sparc_save_local_in_regs_p)
    emit_save_or_restore_local_in_regs (sparc_frame_base_reg,
					sparc_frame_base_offset,
					SORR_RESTORE);

  if (size == 0 || for_eh)
    ; /* do nothing.  */
  else if (frame_pointer_needed)
    {
      /* Make sure the frame is destroyed after everything else is done.  */
      emit_insn (gen_blockage ());

      emit_move_insn (stack_pointer_rtx, gen_rtx_REG (Pmode, 1));
    }
  else
    {
      /* Likewise.  */
      emit_insn (gen_blockage ());

      if (size <= 4096)
	emit_insn (gen_stack_pointer_inc (GEN_INT (size)));
      else if (size <= 8192)
	{
	  emit_insn (gen_stack_pointer_inc (GEN_INT (4096)));
	  emit_insn (gen_stack_pointer_inc (GEN_INT (size - 4096)));
	}
      else
	{
	  rtx reg = gen_rtx_REG (Pmode, 1);
	  emit_move_insn (reg, GEN_INT (size));
	  emit_insn (gen_stack_pointer_inc (reg));
	}
    }
}

/* Return true if it is appropriate to emit `return' instructions in the
   body of a function.  */

bool
sparc_can_use_return_insn_p (void)
{
  return sparc_prologue_data_valid_p
	 && sparc_n_global_fp_regs == 0
	 && TARGET_FLAT
	    ? (sparc_frame_size == 0 && !sparc_save_local_in_regs_p)
	    : (sparc_frame_size == 0 || !sparc_leaf_function_p);
}

/* This function generates the assembly code for function exit.  */

static void
sparc_asm_function_epilogue (FILE *file)
{
  /* If the last two instructions of a function are "call foo; dslot;"
     the return address might point to the first instruction in the next
     function and we have to output a dummy nop for the sake of sane
     backtraces in such cases.  This is pointless for sibling calls since
     the return address is explicitly adjusted.  */

  rtx_insn *insn = get_last_insn ();

  rtx last_real_insn = prev_real_insn (insn);
  if (last_real_insn
      && NONJUMP_INSN_P (last_real_insn)
      && GET_CODE (PATTERN (last_real_insn)) == SEQUENCE)
    last_real_insn = XVECEXP (PATTERN (last_real_insn), 0, 0);

  if (last_real_insn
      && CALL_P (last_real_insn)
      && !SIBLING_CALL_P (last_real_insn))
    fputs("\tnop\n", file);

  sparc_output_deferred_case_vectors ();
}

/* Output a 'restore' instruction.  */

static void
output_restore (rtx pat)
{
  rtx operands[3];

  if (! pat)
    {
      fputs ("\t restore\n", asm_out_file);
      return;
    }

  gcc_assert (GET_CODE (pat) == SET);

  operands[0] = SET_DEST (pat);
  pat = SET_SRC (pat);

  switch (GET_CODE (pat))
    {
      case PLUS:
	operands[1] = XEXP (pat, 0);
	operands[2] = XEXP (pat, 1);
	output_asm_insn (" restore %r1, %2, %Y0", operands);
	break;
      case LO_SUM:
	operands[1] = XEXP (pat, 0);
	operands[2] = XEXP (pat, 1);
	output_asm_insn (" restore %r1, %%lo(%a2), %Y0", operands);
	break;
      case ASHIFT:
	operands[1] = XEXP (pat, 0);
	gcc_assert (XEXP (pat, 1) == const1_rtx);
	output_asm_insn (" restore %r1, %r1, %Y0", operands);
	break;
      default:
	operands[1] = pat;
	output_asm_insn (" restore %%g0, %1, %Y0", operands);
	break;
    }
}

/* Output a return.  */

const char *
output_return (rtx_insn *insn)
{
  if (crtl->calls_eh_return)
    {
      /* If the function uses __builtin_eh_return, the eh_return
	 machinery occupies the delay slot.  */
      gcc_assert (!final_sequence);

      if (flag_delayed_branch)
	{
	  if (!TARGET_FLAT && TARGET_V9)
	    fputs ("\treturn\t%i7+8\n", asm_out_file);
	  else
	    {
	      if (!TARGET_FLAT)
		fputs ("\trestore\n", asm_out_file);

	      fputs ("\tjmp\t%o7+8\n", asm_out_file);
	    }

	  fputs ("\t add\t%sp, %g1, %sp\n", asm_out_file);
	}
      else
	{
	  if (!TARGET_FLAT)
	    fputs ("\trestore\n", asm_out_file);

	  fputs ("\tadd\t%sp, %g1, %sp\n", asm_out_file);
	  fputs ("\tjmp\t%o7+8\n\t nop\n", asm_out_file);
	}
    }
  else if (sparc_leaf_function_p || TARGET_FLAT)
    {
      /* This is a leaf or flat function so we don't have to bother restoring
	 the register window, which frees us from dealing with the convoluted
	 semantics of restore/return.  We simply output the jump to the
	 return address and the insn in the delay slot (if any).  */

      return "jmp\t%%o7+%)%#";
    }
  else
    {
      /* This is a regular function so we have to restore the register window.
	 We may have a pending insn for the delay slot, which will be either
	 combined with the 'restore' instruction or put in the delay slot of
	 the 'return' instruction.  */

      if (final_sequence)
	{
	  rtx delay, pat;

	  delay = NEXT_INSN (insn);
	  gcc_assert (delay);

	  pat = PATTERN (delay);

	  if (TARGET_V9 && ! epilogue_renumber (&pat, 1))
	    {
	      epilogue_renumber (&pat, 0);
	      return "return\t%%i7+%)%#";
	    }
	  else
	    {
	      output_asm_insn ("jmp\t%%i7+%)", NULL);
	      output_restore (pat);
	      PATTERN (delay) = gen_blockage ();
	      INSN_CODE (delay) = -1;
	    }
	}
      else
        {
	  /* The delay slot is empty.  */
	  if (TARGET_V9)
	    return "return\t%%i7+%)\n\t nop";
	  else if (flag_delayed_branch)
	    return "jmp\t%%i7+%)\n\t restore";
	  else
	    return "restore\n\tjmp\t%%o7+%)\n\t nop";
	}
    }

  return "";
}

/* Output a sibling call.  */

const char *
output_sibcall (rtx_insn *insn, rtx call_operand)
{
  rtx operands[1];

  gcc_assert (flag_delayed_branch);

  operands[0] = call_operand;

  if (sparc_leaf_function_p || TARGET_FLAT)
    {
      /* This is a leaf or flat function so we don't have to bother restoring
	 the register window.  We simply output the jump to the function and
	 the insn in the delay slot (if any).  */

      gcc_assert (!(LEAF_SIBCALL_SLOT_RESERVED_P && final_sequence));

      if (final_sequence)
	output_asm_insn ("sethi\t%%hi(%a0), %%g1\n\tjmp\t%%g1 + %%lo(%a0)%#",
			 operands);
      else
	/* Use or with rs2 %%g0 instead of mov, so that as/ld can optimize
	   it into branch if possible.  */
	output_asm_insn ("or\t%%o7, %%g0, %%g1\n\tcall\t%a0, 0\n\t or\t%%g1, %%g0, %%o7",
			 operands);
    }
  else
    {
      /* This is a regular function so we have to restore the register window.
	 We may have a pending insn for the delay slot, which will be combined
	 with the 'restore' instruction.  */

      output_asm_insn ("call\t%a0, 0", operands);

      if (final_sequence)
	{
	  rtx_insn *delay = NEXT_INSN (insn);
	  gcc_assert (delay);

	  output_restore (PATTERN (delay));

	  PATTERN (delay) = gen_blockage ();
	  INSN_CODE (delay) = -1;
	}
      else
	output_restore (NULL_RTX);
    }

  return "";
}

/* Functions for handling argument passing.

   For 32-bit, the first 6 args are normally in registers and the rest are
   pushed.  Any arg that starts within the first 6 words is at least
   partially passed in a register unless its data type forbids.

   For 64-bit, the argument registers are laid out as an array of 16 elements
   and arguments are added sequentially.  The first 6 int args and up to the
   first 16 fp args (depending on size) are passed in regs.

   Slot    Stack   Integral   Float   Float in structure   Double   Long Double
   ----    -----   --------   -----   ------------------   ------   -----------
    15   [SP+248]              %f31       %f30,%f31         %d30
    14   [SP+240]              %f29       %f28,%f29         %d28       %q28
    13   [SP+232]              %f27       %f26,%f27         %d26
    12   [SP+224]              %f25       %f24,%f25         %d24       %q24
    11   [SP+216]              %f23       %f22,%f23         %d22
    10   [SP+208]              %f21       %f20,%f21         %d20       %q20
     9   [SP+200]              %f19       %f18,%f19         %d18
     8   [SP+192]              %f17       %f16,%f17         %d16       %q16
     7   [SP+184]              %f15       %f14,%f15         %d14
     6   [SP+176]              %f13       %f12,%f13         %d12       %q12
     5   [SP+168]     %o5      %f11       %f10,%f11         %d10
     4   [SP+160]     %o4       %f9        %f8,%f9           %d8        %q8
     3   [SP+152]     %o3       %f7        %f6,%f7           %d6
     2   [SP+144]     %o2       %f5        %f4,%f5           %d4        %q4
     1   [SP+136]     %o1       %f3        %f2,%f3           %d2
     0   [SP+128]     %o0       %f1        %f0,%f1           %d0        %q0

   Here SP = %sp if -mno-stack-bias or %sp+stack_bias otherwise.

   Integral arguments are always passed as 64-bit quantities appropriately
   extended.

   Passing of floating point values is handled as follows.
   If a prototype is in scope:
     If the value is in a named argument (i.e. not a stdarg function or a
     value not part of the `...') then the value is passed in the appropriate
     fp reg.
     If the value is part of the `...' and is passed in one of the first 6
     slots then the value is passed in the appropriate int reg.
     If the value is part of the `...' and is not passed in one of the first 6
     slots then the value is passed in memory.
   If a prototype is not in scope:
     If the value is one of the first 6 arguments the value is passed in the
     appropriate integer reg and the appropriate fp reg.
     If the value is not one of the first 6 arguments the value is passed in
     the appropriate fp reg and in memory.


   Summary of the calling conventions implemented by GCC on the SPARC:

   32-bit ABI:
                                size      argument     return value

      small integer              <4       int. reg.      int. reg.
      word                        4       int. reg.      int. reg.
      double word                 8       int. reg.      int. reg.

      _Complex small integer     <8       int. reg.      int. reg.
      _Complex word               8       int. reg.      int. reg.
      _Complex double word       16        memory        int. reg.

      vector integer            <=8       int. reg.       FP reg.
      vector integer             >8        memory         memory

      float                       4       int. reg.       FP reg.
      double                      8       int. reg.       FP reg.
      long double                16        memory         memory

      _Complex float              8        memory         FP reg.
      _Complex double            16        memory         FP reg.
      _Complex long double       32        memory         FP reg.

      vector float              any        memory         memory

      aggregate                 any        memory         memory



    64-bit ABI:
                                size      argument     return value

      small integer              <8       int. reg.      int. reg.
      word                        8       int. reg.      int. reg.
      double word                16       int. reg.      int. reg.

      _Complex small integer    <16       int. reg.      int. reg.
      _Complex word              16       int. reg.      int. reg.
      _Complex double word       32        memory        int. reg.

      vector integer           <=16        FP reg.        FP reg.
      vector integer       16<s<=32        memory         FP reg.
      vector integer            >32        memory         memory

      float                       4        FP reg.        FP reg.
      double                      8        FP reg.        FP reg.
      long double                16        FP reg.        FP reg.

      _Complex float              8        FP reg.        FP reg.
      _Complex double            16        FP reg.        FP reg.
      _Complex long double       32        memory         FP reg.

      vector float             <=16        FP reg.        FP reg.
      vector float         16<s<=32        memory         FP reg.
      vector float              >32        memory         memory

      aggregate                <=16         reg.           reg.
      aggregate            16<s<=32        memory          reg.
      aggregate                 >32        memory         memory



Note #1: complex floating-point types follow the extended SPARC ABIs as
implemented by the Sun compiler.

Note #2: integral vector types follow the scalar floating-point types
conventions to match what is implemented by the Sun VIS SDK.

Note #3: floating-point vector types follow the aggregate types
conventions.  */


/* Maximum number of int regs for args.  */
#define SPARC_INT_ARG_MAX 6
/* Maximum number of fp regs for args.  */
#define SPARC_FP_ARG_MAX 16
/* Number of words (partially) occupied for a given size in units.  */
#define CEIL_NWORDS(SIZE) CEIL((SIZE), UNITS_PER_WORD)

/* Handle the INIT_CUMULATIVE_ARGS macro.
   Initialize a variable CUM of type CUMULATIVE_ARGS
   for a call to a function whose data type is FNTYPE.
   For a library call, FNTYPE is 0.  */

void
init_cumulative_args (struct sparc_args *cum, tree fntype, rtx, tree)
{
  cum->words = 0;
  cum->prototype_p = fntype && prototype_p (fntype);
  cum->libcall_p = !fntype;
}

/* Handle promotion of pointer and integer arguments.  */

static machine_mode
sparc_promote_function_mode (const_tree type, machine_mode mode,
			     int *punsignedp, const_tree, int)
{
  if (type && POINTER_TYPE_P (type))
    {
      *punsignedp = POINTERS_EXTEND_UNSIGNED;
      return Pmode;
    }

  /* Integral arguments are passed as full words, as per the ABI.  */
  if (GET_MODE_CLASS (mode) == MODE_INT
      && GET_MODE_SIZE (mode) < UNITS_PER_WORD)
    return word_mode;

  return mode;
}

/* Handle the TARGET_STRICT_ARGUMENT_NAMING target hook.  */

static bool
sparc_strict_argument_naming (cumulative_args_t ca ATTRIBUTE_UNUSED)
{
  return TARGET_ARCH64 ? true : false;
}

/* Traverse the record TYPE recursively and call FUNC on its fields.
   NAMED is true if this is for a named parameter.  DATA is passed
   to FUNC for each field.  OFFSET is the starting position and
   PACKED is true if we are inside a packed record.  */

template <typename T, void Func (const_tree, HOST_WIDE_INT, bool, T*)>
static void
traverse_record_type (const_tree type, bool named, T *data,
		      HOST_WIDE_INT offset = 0, bool packed = false)
{
  /* The ABI obviously doesn't specify how packed structures are passed.
     These are passed in integer regs if possible, otherwise memory.  */
  if (!packed)
    for (tree field = TYPE_FIELDS (type); field; field = DECL_CHAIN (field))
      if (TREE_CODE (field) == FIELD_DECL && DECL_PACKED (field))
	{
	  packed = true;
	  break;
	}

  /* Walk the real fields, but skip those with no size or a zero size.
     ??? Fields with variable offset are handled as having zero offset.  */
  for (tree field = TYPE_FIELDS (type); field; field = DECL_CHAIN (field))
    if (TREE_CODE (field) == FIELD_DECL)
      {
	if (!DECL_SIZE (field) || integer_zerop (DECL_SIZE (field)))
	  continue;

	HOST_WIDE_INT bitpos = offset;
	if (TREE_CODE (DECL_FIELD_OFFSET (field)) == INTEGER_CST)
	  bitpos += int_bit_position (field);

	tree field_type = TREE_TYPE (field);
	if (TREE_CODE (field_type) == RECORD_TYPE)
	  traverse_record_type<T, Func> (field_type, named, data, bitpos,
					 packed);
	else
	  {
	    const bool fp_type
	      = FLOAT_TYPE_P (field_type) || VECTOR_TYPE_P (field_type);
	    Func (field, bitpos, fp_type && named && !packed && TARGET_FPU,
		  data);
	  }
      }
}

/* Handle recursive register classifying for structure layout.  */

typedef struct
{
  bool fp_regs;		/* true if field eligible to FP registers.  */
  bool fp_regs_in_first_word;	/* true if such field in first word.  */
} classify_data_t;

/* A subroutine of function_arg_slotno.  Classify the field.  */

inline void
classify_registers (const_tree, HOST_WIDE_INT bitpos, bool fp,
		    classify_data_t *data)
{
  if (fp)
    {
      data->fp_regs = true;
      if (bitpos < BITS_PER_WORD)
	data->fp_regs_in_first_word = true;
    }
}

/* Compute the slot number to pass an argument in.
   Return the slot number or -1 if passing on the stack.

   CUM is a variable of type CUMULATIVE_ARGS which gives info about
    the preceding args and about the function being called.
   MODE is the argument's machine mode.
   TYPE is the data type of the argument (as a tree).
    This is null for libcalls where that information may
    not be available.
   NAMED is nonzero if this argument is a named parameter
    (otherwise it is an extra parameter matching an ellipsis).
   INCOMING is zero for FUNCTION_ARG, nonzero for FUNCTION_INCOMING_ARG.
   *PREGNO records the register number to use if scalar type.
   *PPADDING records the amount of padding needed in words.  */

static int
function_arg_slotno (const struct sparc_args *cum, machine_mode mode,
		     const_tree type, bool named, bool incoming,
		     int *pregno, int *ppadding)
{
  int regbase = (incoming
		 ? SPARC_INCOMING_INT_ARG_FIRST
		 : SPARC_OUTGOING_INT_ARG_FIRST);
  int slotno = cum->words;
  enum mode_class mclass;
  int regno;

  *ppadding = 0;

  if (type && TREE_ADDRESSABLE (type))
    return -1;

  if (TARGET_ARCH32
      && mode == BLKmode
      && type
      && TYPE_ALIGN (type) % PARM_BOUNDARY != 0)
    return -1;

  /* For SPARC64, objects requiring 16-byte alignment get it.  */
  if (TARGET_ARCH64
      && (type ? TYPE_ALIGN (type) : GET_MODE_ALIGNMENT (mode)) >= 128
      && (slotno & 1) != 0)
    slotno++, *ppadding = 1;

  mclass = GET_MODE_CLASS (mode);
  if (type && TREE_CODE (type) == VECTOR_TYPE)
    {
      /* Vector types deserve special treatment because they are
	 polymorphic wrt their mode, depending upon whether VIS
	 instructions are enabled.  */
      if (TREE_CODE (TREE_TYPE (type)) == REAL_TYPE)
	{
	  /* The SPARC port defines no floating-point vector modes.  */
	  gcc_assert (mode == BLKmode);
	}
      else
	{
	  /* Integral vector types should either have a vector
	     mode or an integral mode, because we are guaranteed
	     by pass_by_reference that their size is not greater
	     than 16 bytes and TImode is 16-byte wide.  */
	  gcc_assert (mode != BLKmode);

	  /* Vector integers are handled like floats according to
	     the Sun VIS SDK.  */
	  mclass = MODE_FLOAT;
	}
    }

  switch (mclass)
    {
    case MODE_FLOAT:
    case MODE_COMPLEX_FLOAT:
    case MODE_VECTOR_INT:
      if (TARGET_ARCH64 && TARGET_FPU && named)
	{
	  /* If all arg slots are filled, then must pass on stack.  */
	  if (slotno >= SPARC_FP_ARG_MAX)
	    return -1;

	  regno = SPARC_FP_ARG_FIRST + slotno * 2;
	  /* Arguments filling only one single FP register are
	     right-justified in the outer double FP register.  */
	  if (GET_MODE_SIZE (mode) <= 4)
	    regno++;
	  break;
	}
      /* fallthrough */

    case MODE_INT:
    case MODE_COMPLEX_INT:
      /* If all arg slots are filled, then must pass on stack.  */
      if (slotno >= SPARC_INT_ARG_MAX)
	return -1;

      regno = regbase + slotno;
      break;

    case MODE_RANDOM:
      if (mode == VOIDmode)
	/* MODE is VOIDmode when generating the actual call.  */
	return -1;

      gcc_assert (mode == BLKmode);

      if (TARGET_ARCH32
	  || !type
	  || (TREE_CODE (type) != RECORD_TYPE
	      && TREE_CODE (type) != VECTOR_TYPE))
	{
	  /* If all arg slots are filled, then must pass on stack.  */
	  if (slotno >= SPARC_INT_ARG_MAX)
	    return -1;

	  regno = regbase + slotno;
	}
      else  /* TARGET_ARCH64 && type */
	{
	  /* If all arg slots are filled, then must pass on stack.  */
	  if (slotno >= SPARC_FP_ARG_MAX)
	    return -1;

	  if (TREE_CODE (type) == RECORD_TYPE)
	    {
	      classify_data_t data = { false, false };
	      traverse_record_type<classify_data_t, classify_registers>
		(type, named, &data);

	      if (data.fp_regs)
		{
		  /* If all FP slots are filled except for the last one and
		     there is no FP field in the first word, then must pass
		     on stack.  */
		  if (slotno >= SPARC_FP_ARG_MAX - 1
		      && !data.fp_regs_in_first_word)
		    return -1;
		}
	      else
		{
		  /* If all int slots are filled, then must pass on stack.  */
		  if (slotno >= SPARC_INT_ARG_MAX)
		    return -1;
		}
	    }

	  /* PREGNO isn't set since both int and FP regs can be used.  */
	  return slotno;
	}
      break;

    default :
      gcc_unreachable ();
    }

  *pregno = regno;
  return slotno;
}

/* Handle recursive register counting/assigning for structure layout.  */

typedef struct
{
  int slotno;		/* slot number of the argument.  */
  int regbase;		/* regno of the base register.  */
  int intoffset;	/* offset of the first pending integer field.  */
  int nregs;		/* number of words passed in registers.  */
  bool stack;		/* true if part of the argument is on the stack.  */
  rtx ret;		/* return expression being built.  */
} assign_data_t;

/* A subroutine of function_arg_record_value.  Compute the number of integer
   registers to be assigned between PARMS->intoffset and BITPOS.  Return
   true if at least one integer register is assigned or false otherwise.  */

static bool
compute_int_layout (HOST_WIDE_INT bitpos, assign_data_t *data, int *pnregs)
{
  if (data->intoffset < 0)
    return false;

  const int intoffset = data->intoffset;
  data->intoffset = -1;

  const int this_slotno = data->slotno + intoffset / BITS_PER_WORD;
  const unsigned int startbit = ROUND_DOWN (intoffset, BITS_PER_WORD);
  const unsigned int endbit = ROUND_UP (bitpos, BITS_PER_WORD);
  int nregs = (endbit - startbit) / BITS_PER_WORD;

  if (nregs > 0 && nregs > SPARC_INT_ARG_MAX - this_slotno)
    {
      nregs = SPARC_INT_ARG_MAX - this_slotno;

      /* We need to pass this field (partly) on the stack.  */
      data->stack = 1;
    }

  if (nregs <= 0)
    return false;

  *pnregs = nregs;
  return true;
}

/* A subroutine of function_arg_record_value.  Compute the number and the mode
   of the FP registers to be assigned for FIELD.  Return true if at least one
   FP register is assigned or false otherwise.  */

static bool
compute_fp_layout (const_tree field, HOST_WIDE_INT bitpos,
		   assign_data_t *data,
		   int *pnregs, machine_mode *pmode)
{
  const int this_slotno = data->slotno + bitpos / BITS_PER_WORD;
  machine_mode mode = DECL_MODE (field);
  int nregs, nslots;

  /* Slots are counted as words while regs are counted as having the size of
     the (inner) mode.  */
  if (TREE_CODE (TREE_TYPE (field)) == VECTOR_TYPE && mode == BLKmode)
    {
      mode = TYPE_MODE (TREE_TYPE (TREE_TYPE (field)));
      nregs = TYPE_VECTOR_SUBPARTS (TREE_TYPE (field));
    }
  else if (TREE_CODE (TREE_TYPE (field)) == COMPLEX_TYPE)
    {
      mode = TYPE_MODE (TREE_TYPE (TREE_TYPE (field)));
      nregs = 2;
    }
  else
    nregs = 1;

  nslots = CEIL_NWORDS (nregs * GET_MODE_SIZE (mode));

  if (nslots > SPARC_FP_ARG_MAX - this_slotno)
    {
      nslots = SPARC_FP_ARG_MAX - this_slotno;
      nregs = (nslots * UNITS_PER_WORD) / GET_MODE_SIZE (mode);

      /* We need to pass this field (partly) on the stack.  */
      data->stack = 1;

      if (nregs <= 0)
	return false;
    }

  *pnregs = nregs;
  *pmode = mode;
  return true;
}

/* A subroutine of function_arg_record_value.  Count the number of registers
   to be assigned for FIELD and between PARMS->intoffset and BITPOS.  */

inline void
count_registers (const_tree field, HOST_WIDE_INT bitpos, bool fp,
		 assign_data_t *data)
{
  if (fp)
    {
      int nregs;
      machine_mode mode;

      if (compute_int_layout (bitpos, data, &nregs))
	data->nregs += nregs;

      if (compute_fp_layout (field, bitpos, data, &nregs, &mode))
	data->nregs += nregs;
    }
  else
    {
      if (data->intoffset < 0)
	data->intoffset = bitpos;
    }
}

/* A subroutine of function_arg_record_value.  Assign the bits of the
   structure between PARMS->intoffset and BITPOS to integer registers.  */

static void
assign_int_registers (HOST_WIDE_INT bitpos, assign_data_t *data)
{
  int intoffset = data->intoffset;
  machine_mode mode;
  int nregs;

  if (!compute_int_layout (bitpos, data, &nregs))
    return;

  /* If this is the trailing part of a word, only load that much into
     the register.  Otherwise load the whole register.  Note that in
     the latter case we may pick up unwanted bits.  It's not a problem
     at the moment but may wish to revisit.  */
  if (intoffset % BITS_PER_WORD != 0)
    mode = smallest_int_mode_for_size (BITS_PER_WORD
				       - intoffset % BITS_PER_WORD);
  else
    mode = word_mode;

  const int this_slotno = data->slotno + intoffset / BITS_PER_WORD;
  unsigned int regno = data->regbase + this_slotno;
  intoffset /= BITS_PER_UNIT;

  do
    {
      rtx reg = gen_rtx_REG (mode, regno);
      XVECEXP (data->ret, 0, data->stack + data->nregs)
	= gen_rtx_EXPR_LIST (VOIDmode, reg, GEN_INT (intoffset));
      data->nregs += 1;
      mode = word_mode;
      regno += 1;
      intoffset = (intoffset | (UNITS_PER_WORD - 1)) + 1;
    }
  while (--nregs > 0);
}

/* A subroutine of function_arg_record_value.  Assign FIELD at position
   BITPOS to FP registers.  */

static void
assign_fp_registers (const_tree field, HOST_WIDE_INT bitpos,
			     assign_data_t *data)
{
  int nregs;
  machine_mode mode;

  if (!compute_fp_layout (field, bitpos, data, &nregs, &mode))
    return;

  const int this_slotno = data->slotno + bitpos / BITS_PER_WORD;
  int regno = SPARC_FP_ARG_FIRST + this_slotno * 2;
  if (GET_MODE_SIZE (mode) <= 4 && (bitpos & 32) != 0)
    regno++;
  int pos = bitpos / BITS_PER_UNIT;

  do
    {
      rtx reg = gen_rtx_REG (mode, regno);
      XVECEXP (data->ret, 0, data->stack + data->nregs)
	= gen_rtx_EXPR_LIST (VOIDmode, reg, GEN_INT (pos));
      data->nregs += 1;
      regno += GET_MODE_SIZE (mode) / 4;
      pos += GET_MODE_SIZE (mode);
    }
  while (--nregs > 0);
}

/* A subroutine of function_arg_record_value.  Assign FIELD and the bits of
   the structure between PARMS->intoffset and BITPOS to registers.  */

inline void
assign_registers (const_tree field, HOST_WIDE_INT bitpos, bool fp,
		  assign_data_t *data)
{
  if (fp)
    {
      assign_int_registers (bitpos, data);

      assign_fp_registers (field, bitpos, data);
    }
  else
    {
      if (data->intoffset < 0)
	data->intoffset = bitpos;
    }
}

/* Used by function_arg and sparc_function_value_1 to implement the complex
   conventions of the 64-bit ABI for passing and returning structures.
   Return an expression valid as a return value for the FUNCTION_ARG
   and TARGET_FUNCTION_VALUE.

   TYPE is the data type of the argument (as a tree).
    This is null for libcalls where that information may
    not be available.
   MODE is the argument's machine mode.
   SLOTNO is the index number of the argument's slot in the parameter array.
   NAMED is true if this argument is a named parameter
    (otherwise it is an extra parameter matching an ellipsis).
   REGBASE is the regno of the base register for the parameter array.  */

static rtx
function_arg_record_value (const_tree type, machine_mode mode,
			   int slotno, bool named, int regbase)
{
  HOST_WIDE_INT typesize = int_size_in_bytes (type);
  assign_data_t data;
  int nregs;

  data.slotno = slotno;
  data.regbase = regbase;

  /* Count how many registers we need.  */
  data.nregs = 0;
  data.intoffset = 0;
  data.stack = false;
  traverse_record_type<assign_data_t, count_registers> (type, named, &data);

  /* Take into account pending integer fields.  */
  if (compute_int_layout (typesize * BITS_PER_UNIT, &data, &nregs))
    data.nregs += nregs;

  /* Allocate the vector and handle some annoying special cases.  */
  nregs = data.nregs;

  if (nregs == 0)
    {
      /* ??? Empty structure has no value?  Duh?  */
      if (typesize <= 0)
	{
	  /* Though there's nothing really to store, return a word register
	     anyway so the rest of gcc doesn't go nuts.  Returning a PARALLEL
	     leads to breakage due to the fact that there are zero bytes to
	     load.  */
	  return gen_rtx_REG (mode, regbase);
	}

      /* ??? C++ has structures with no fields, and yet a size.  Give up
	 for now and pass everything back in integer registers.  */
      nregs = (typesize + UNITS_PER_WORD - 1) / UNITS_PER_WORD;
      if (nregs + slotno > SPARC_INT_ARG_MAX)
	nregs = SPARC_INT_ARG_MAX - slotno;
    }

  gcc_assert (nregs > 0);

  data.ret = gen_rtx_PARALLEL (mode, rtvec_alloc (data.stack + nregs));

  /* If at least one field must be passed on the stack, generate
     (parallel [(expr_list (nil) ...) ...]) so that all fields will
     also be passed on the stack.  We can't do much better because the
     semantics of TARGET_ARG_PARTIAL_BYTES doesn't handle the case
     of structures for which the fields passed exclusively in registers
     are not at the beginning of the structure.  */
  if (data.stack)
    XVECEXP (data.ret, 0, 0)
      = gen_rtx_EXPR_LIST (VOIDmode, NULL_RTX, const0_rtx);

  /* Assign the registers.  */
  data.nregs = 0;
  data.intoffset = 0;
  traverse_record_type<assign_data_t, assign_registers> (type, named, &data);

  /* Assign pending integer fields.  */
  assign_int_registers (typesize * BITS_PER_UNIT, &data);

  gcc_assert (data.nregs == nregs);

  return data.ret;
}

/* Used by function_arg and sparc_function_value_1 to implement the conventions
   of the 64-bit ABI for passing and returning unions.
   Return an expression valid as a return value for the FUNCTION_ARG
   and TARGET_FUNCTION_VALUE.

   SIZE is the size in bytes of the union.
   MODE is the argument's machine mode.
   REGNO is the hard register the union will be passed in.  */

static rtx
function_arg_union_value (int size, machine_mode mode, int slotno,
			  int regno)
{
  int nwords = CEIL_NWORDS (size), i;
  rtx regs;

  /* See comment in previous function for empty structures.  */
  if (nwords == 0)
    return gen_rtx_REG (mode, regno);

  if (slotno == SPARC_INT_ARG_MAX - 1)
    nwords = 1;

  regs = gen_rtx_PARALLEL (mode, rtvec_alloc (nwords));

  for (i = 0; i < nwords; i++)
    {
      /* Unions are passed left-justified.  */
      XVECEXP (regs, 0, i)
	= gen_rtx_EXPR_LIST (VOIDmode,
			     gen_rtx_REG (word_mode, regno),
			     GEN_INT (UNITS_PER_WORD * i));
      regno++;
    }

  return regs;
}

/* Used by function_arg and sparc_function_value_1 to implement the conventions
   for passing and returning BLKmode vectors.
   Return an expression valid as a return value for the FUNCTION_ARG
   and TARGET_FUNCTION_VALUE.

   SIZE is the size in bytes of the vector.
   REGNO is the FP hard register the vector will be passed in.  */

static rtx
function_arg_vector_value (int size, int regno)
{
  const int nregs = MAX (1, size / 8);
  rtx regs = gen_rtx_PARALLEL (BLKmode, rtvec_alloc (nregs));

  if (size < 8)
    XVECEXP (regs, 0, 0)
      = gen_rtx_EXPR_LIST (VOIDmode,
			   gen_rtx_REG (SImode, regno),
			   const0_rtx);
  else
    for (int i = 0; i < nregs; i++)
      XVECEXP (regs, 0, i)
	= gen_rtx_EXPR_LIST (VOIDmode,
			     gen_rtx_REG (DImode, regno + 2*i),
			     GEN_INT (i*8));

  return regs;
}

/* Determine where to put an argument to a function.
   Value is zero to push the argument on the stack,
   or a hard register in which to store the argument.

   CUM is a variable of type CUMULATIVE_ARGS which gives info about
    the preceding args and about the function being called.
   MODE is the argument's machine mode.
   TYPE is the data type of the argument (as a tree).
    This is null for libcalls where that information may
    not be available.
   NAMED is true if this argument is a named parameter
    (otherwise it is an extra parameter matching an ellipsis).
   INCOMING_P is false for TARGET_FUNCTION_ARG, true for
    TARGET_FUNCTION_INCOMING_ARG.  */

static rtx
sparc_function_arg_1 (cumulative_args_t cum_v, machine_mode mode,
		      const_tree type, bool named, bool incoming)
{
  const CUMULATIVE_ARGS *cum = get_cumulative_args (cum_v);

  int regbase = (incoming
		 ? SPARC_INCOMING_INT_ARG_FIRST
		 : SPARC_OUTGOING_INT_ARG_FIRST);
  int slotno, regno, padding;
  enum mode_class mclass = GET_MODE_CLASS (mode);

  slotno = function_arg_slotno (cum, mode, type, named, incoming,
				&regno, &padding);
  if (slotno == -1)
    return 0;

  /* Vector types deserve special treatment because they are polymorphic wrt
     their mode, depending upon whether VIS instructions are enabled.  */
  if (type && TREE_CODE (type) == VECTOR_TYPE)
    {
      HOST_WIDE_INT size = int_size_in_bytes (type);
      gcc_assert ((TARGET_ARCH32 && size <= 8)
		  || (TARGET_ARCH64 && size <= 16));

      if (mode == BLKmode)
	return function_arg_vector_value (size, SPARC_FP_ARG_FIRST + 2*slotno);

      mclass = MODE_FLOAT;
    }

  if (TARGET_ARCH32)
    return gen_rtx_REG (mode, regno);

  /* Structures up to 16 bytes in size are passed in arg slots on the stack
     and are promoted to registers if possible.  */
  if (type && TREE_CODE (type) == RECORD_TYPE)
    {
      HOST_WIDE_INT size = int_size_in_bytes (type);
      gcc_assert (size <= 16);

      return function_arg_record_value (type, mode, slotno, named, regbase);
    }

  /* Unions up to 16 bytes in size are passed in integer registers.  */
  else if (type && TREE_CODE (type) == UNION_TYPE)
    {
      HOST_WIDE_INT size = int_size_in_bytes (type);
      gcc_assert (size <= 16);

      return function_arg_union_value (size, mode, slotno, regno);
    }

  /* v9 fp args in reg slots beyond the int reg slots get passed in regs
     but also have the slot allocated for them.
     If no prototype is in scope fp values in register slots get passed
     in two places, either fp regs and int regs or fp regs and memory.  */
  else if ((mclass == MODE_FLOAT || mclass == MODE_COMPLEX_FLOAT)
	   && SPARC_FP_REG_P (regno))
    {
      rtx reg = gen_rtx_REG (mode, regno);
      if (cum->prototype_p || cum->libcall_p)
	return reg;
      else
	{
	  rtx v0, v1;

	  if ((regno - SPARC_FP_ARG_FIRST) < SPARC_INT_ARG_MAX * 2)
	    {
	      int intreg;

	      /* On incoming, we don't need to know that the value
		 is passed in %f0 and %i0, and it confuses other parts
		 causing needless spillage even on the simplest cases.  */
	      if (incoming)
		return reg;

	      intreg = (SPARC_OUTGOING_INT_ARG_FIRST
			+ (regno - SPARC_FP_ARG_FIRST) / 2);

	      v0 = gen_rtx_EXPR_LIST (VOIDmode, reg, const0_rtx);
	      v1 = gen_rtx_EXPR_LIST (VOIDmode, gen_rtx_REG (mode, intreg),
				      const0_rtx);
	      return gen_rtx_PARALLEL (mode, gen_rtvec (2, v0, v1));
	    }
	  else
	    {
	      v0 = gen_rtx_EXPR_LIST (VOIDmode, NULL_RTX, const0_rtx);
	      v1 = gen_rtx_EXPR_LIST (VOIDmode, reg, const0_rtx);
	      return gen_rtx_PARALLEL (mode, gen_rtvec (2, v0, v1));
	    }
	}
    }

  /* All other aggregate types are passed in an integer register in a mode
     corresponding to the size of the type.  */
  else if (type && AGGREGATE_TYPE_P (type))
    {
      HOST_WIDE_INT size = int_size_in_bytes (type);
      gcc_assert (size <= 16);

      mode = int_mode_for_size (size * BITS_PER_UNIT, 0).else_blk ();
    }

  return gen_rtx_REG (mode, regno);
}

/* Handle the TARGET_FUNCTION_ARG target hook.  */

static rtx
sparc_function_arg (cumulative_args_t cum, machine_mode mode,
		    const_tree type, bool named)
{
  return sparc_function_arg_1 (cum, mode, type, named, false);
}

/* Handle the TARGET_FUNCTION_INCOMING_ARG target hook.  */

static rtx
sparc_function_incoming_arg (cumulative_args_t cum, machine_mode mode,
			     const_tree type, bool named)
{
  return sparc_function_arg_1 (cum, mode, type, named, true);
}

/* For sparc64, objects requiring 16 byte alignment are passed that way.  */

static unsigned int
sparc_function_arg_boundary (machine_mode mode, const_tree type)
{
  return ((TARGET_ARCH64
	   && (GET_MODE_ALIGNMENT (mode) == 128
	       || (type && TYPE_ALIGN (type) == 128)))
	  ? 128
	  : PARM_BOUNDARY);
}

/* For an arg passed partly in registers and partly in memory,
   this is the number of bytes of registers used.
   For args passed entirely in registers or entirely in memory, zero.

   Any arg that starts in the first 6 regs but won't entirely fit in them
   needs partial registers on v8.  On v9, structures with integer
   values in arg slots 5,6 will be passed in %o5 and SP+176, and complex fp
   values that begin in the last fp reg [where "last fp reg" varies with the
   mode] will be split between that reg and memory.  */

static int
sparc_arg_partial_bytes (cumulative_args_t cum, machine_mode mode,
			 tree type, bool named)
{
  int slotno, regno, padding;

  /* We pass false for incoming here, it doesn't matter.  */
  slotno = function_arg_slotno (get_cumulative_args (cum), mode, type, named,
				false, &regno, &padding);

  if (slotno == -1)
    return 0;

  if (TARGET_ARCH32)
    {
      if ((slotno + (mode == BLKmode
		     ? CEIL_NWORDS (int_size_in_bytes (type))
		     : CEIL_NWORDS (GET_MODE_SIZE (mode))))
	  > SPARC_INT_ARG_MAX)
	return (SPARC_INT_ARG_MAX - slotno) * UNITS_PER_WORD;
    }
  else
    {
      /* We are guaranteed by pass_by_reference that the size of the
	 argument is not greater than 16 bytes, so we only need to return
	 one word if the argument is partially passed in registers.  */

      if (type && AGGREGATE_TYPE_P (type))
	{
	  int size = int_size_in_bytes (type);

	  if (size > UNITS_PER_WORD
	      && (slotno == SPARC_INT_ARG_MAX - 1
		  || slotno == SPARC_FP_ARG_MAX - 1))
	    return UNITS_PER_WORD;
	}
      else if (GET_MODE_CLASS (mode) == MODE_COMPLEX_INT
	       || (GET_MODE_CLASS (mode) == MODE_COMPLEX_FLOAT
		   && ! (TARGET_FPU && named)))
	{
	  /* The complex types are passed as packed types.  */
	  if (GET_MODE_SIZE (mode) > UNITS_PER_WORD
	      && slotno == SPARC_INT_ARG_MAX - 1)
	    return UNITS_PER_WORD;
	}
      else if (GET_MODE_CLASS (mode) == MODE_COMPLEX_FLOAT)
	{
	  if ((slotno + GET_MODE_SIZE (mode) / UNITS_PER_WORD)
	      > SPARC_FP_ARG_MAX)
	    return UNITS_PER_WORD;
	}
    }

  return 0;
}

/* Handle the TARGET_PASS_BY_REFERENCE target hook.
   Specify whether to pass the argument by reference.  */

static bool
sparc_pass_by_reference (cumulative_args_t cum ATTRIBUTE_UNUSED,
			 machine_mode mode, const_tree type,
			 bool named ATTRIBUTE_UNUSED)
{
  if (TARGET_ARCH32)
    /* Original SPARC 32-bit ABI says that structures and unions,
       and quad-precision floats are passed by reference.  For Pascal,
       also pass arrays by reference.  All other base types are passed
       in registers.

       Extended ABI (as implemented by the Sun compiler) says that all
       complex floats are passed by reference.  Pass complex integers
       in registers up to 8 bytes.  More generally, enforce the 2-word
       cap for passing arguments in registers.

       Vector ABI (as implemented by the Sun VIS SDK) says that vector
       integers are passed like floats of the same size, that is in
       registers up to 8 bytes.  Pass all vector floats by reference
       like structure and unions.  */
    return ((type && (AGGREGATE_TYPE_P (type) || VECTOR_FLOAT_TYPE_P (type)))
	    || mode == SCmode
	    /* Catch CDImode, TFmode, DCmode and TCmode.  */
	    || GET_MODE_SIZE (mode) > 8
	    || (type
		&& TREE_CODE (type) == VECTOR_TYPE
		&& (unsigned HOST_WIDE_INT) int_size_in_bytes (type) > 8));
  else
    /* Original SPARC 64-bit ABI says that structures and unions
       smaller than 16 bytes are passed in registers, as well as
       all other base types.

       Extended ABI (as implemented by the Sun compiler) says that
       complex floats are passed in registers up to 16 bytes.  Pass
       all complex integers in registers up to 16 bytes.  More generally,
       enforce the 2-word cap for passing arguments in registers.

       Vector ABI (as implemented by the Sun VIS SDK) says that vector
       integers are passed like floats of the same size, that is in
       registers (up to 16 bytes).  Pass all vector floats like structure
       and unions.  */
    return ((type
	     && (AGGREGATE_TYPE_P (type) || TREE_CODE (type) == VECTOR_TYPE)
	     && (unsigned HOST_WIDE_INT) int_size_in_bytes (type) > 16)
	    /* Catch CTImode and TCmode.  */
	    || GET_MODE_SIZE (mode) > 16);
}

/* Handle the TARGET_FUNCTION_ARG_ADVANCE hook.
   Update the data in CUM to advance over an argument
   of mode MODE and data type TYPE.
   TYPE is null for libcalls where that information may not be available.  */

static void
sparc_function_arg_advance (cumulative_args_t cum_v, machine_mode mode,
			    const_tree type, bool named)
{
  CUMULATIVE_ARGS *cum = get_cumulative_args (cum_v);
  int regno, padding;

  /* We pass false for incoming here, it doesn't matter.  */
  function_arg_slotno (cum, mode, type, named, false, &regno, &padding);

  /* If argument requires leading padding, add it.  */
  cum->words += padding;

  if (TARGET_ARCH32)
    cum->words += (mode == BLKmode
		   ? CEIL_NWORDS (int_size_in_bytes (type))
		   : CEIL_NWORDS (GET_MODE_SIZE (mode)));
  else
    {
      if (type && AGGREGATE_TYPE_P (type))
	{
	  int size = int_size_in_bytes (type);

	  if (size <= 8)
	    ++cum->words;
	  else if (size <= 16)
	    cum->words += 2;
	  else /* passed by reference */
	    ++cum->words;
	}
      else
	cum->words += (mode == BLKmode
		       ? CEIL_NWORDS (int_size_in_bytes (type))
		       : CEIL_NWORDS (GET_MODE_SIZE (mode)));
    }
}

/* Implement TARGET_FUNCTION_ARG_PADDING.  For the 64-bit ABI structs
   are always stored left shifted in their argument slot.  */

static pad_direction
sparc_function_arg_padding (machine_mode mode, const_tree type)
{
  if (TARGET_ARCH64 && type && AGGREGATE_TYPE_P (type))
    return PAD_UPWARD;

  /* Fall back to the default.  */
  return default_function_arg_padding (mode, type);
}

/* Handle the TARGET_RETURN_IN_MEMORY target hook.
   Specify whether to return the return value in memory.  */

static bool
sparc_return_in_memory (const_tree type, const_tree fntype ATTRIBUTE_UNUSED)
{
  if (TARGET_ARCH32)
    /* Original SPARC 32-bit ABI says that structures and unions,
       and quad-precision floats are returned in memory.  All other
       base types are returned in registers.

       Extended ABI (as implemented by the Sun compiler) says that
       all complex floats are returned in registers (8 FP registers
       at most for '_Complex long double').  Return all complex integers
       in registers (4 at most for '_Complex long long').

       Vector ABI (as implemented by the Sun VIS SDK) says that vector
       integers are returned like floats of the same size, that is in
       registers up to 8 bytes and in memory otherwise.  Return all
       vector floats in memory like structure and unions; note that
       they always have BLKmode like the latter.  */
    return (TYPE_MODE (type) == BLKmode
	    || TYPE_MODE (type) == TFmode
	    || (TREE_CODE (type) == VECTOR_TYPE
		&& (unsigned HOST_WIDE_INT) int_size_in_bytes (type) > 8));
  else
    /* Original SPARC 64-bit ABI says that structures and unions
       smaller than 32 bytes are returned in registers, as well as
       all other base types.

       Extended ABI (as implemented by the Sun compiler) says that all
       complex floats are returned in registers (8 FP registers at most
       for '_Complex long double').  Return all complex integers in
       registers (4 at most for '_Complex TItype').

       Vector ABI (as implemented by the Sun VIS SDK) says that vector
       integers are returned like floats of the same size, that is in
       registers.  Return all vector floats like structure and unions;
       note that they always have BLKmode like the latter.  */
    return (TYPE_MODE (type) == BLKmode
	    && (unsigned HOST_WIDE_INT) int_size_in_bytes (type) > 32);
}

/* Handle the TARGET_STRUCT_VALUE target hook.
   Return where to find the structure return value address.  */

static rtx
sparc_struct_value_rtx (tree fndecl, int incoming)
{
  if (TARGET_ARCH64)
    return 0;
  else
    {
      rtx mem;

      if (incoming)
	mem = gen_frame_mem (Pmode, plus_constant (Pmode, frame_pointer_rtx,
						   STRUCT_VALUE_OFFSET));
      else
	mem = gen_frame_mem (Pmode, plus_constant (Pmode, stack_pointer_rtx,
						   STRUCT_VALUE_OFFSET));

      /* Only follow the SPARC ABI for fixed-size structure returns.
         Variable size structure returns are handled per the normal
         procedures in GCC. This is enabled by -mstd-struct-return */
      if (incoming == 2
	  && sparc_std_struct_return
	  && TYPE_SIZE_UNIT (TREE_TYPE (fndecl))
	  && TREE_CODE (TYPE_SIZE_UNIT (TREE_TYPE (fndecl))) == INTEGER_CST)
	{
	  /* We must check and adjust the return address, as it is optional
	     as to whether the return object is really provided.  */
	  rtx ret_reg = gen_rtx_REG (Pmode, RETURN_ADDR_REGNUM);
	  rtx scratch = gen_reg_rtx (SImode);
	  rtx_code_label *endlab = gen_label_rtx ();

	  /* Calculate the return object size.  */
	  tree size = TYPE_SIZE_UNIT (TREE_TYPE (fndecl));
	  rtx size_rtx = GEN_INT (TREE_INT_CST_LOW (size) & 0xfff);
	  /* Construct a temporary return value.  */
	  rtx temp_val
	    = assign_stack_local (Pmode, TREE_INT_CST_LOW (size), 0);

	  /* Implement SPARC 32-bit psABI callee return struct checking:

	     Fetch the instruction where we will return to and see if
	     it's an unimp instruction (the most significant 10 bits
	     will be zero).  */
	  emit_move_insn (scratch, gen_rtx_MEM (SImode,
						plus_constant (Pmode,
							       ret_reg, 8)));
	  /* Assume the size is valid and pre-adjust.  */
	  emit_insn (gen_add3_insn (ret_reg, ret_reg, GEN_INT (4)));
	  emit_cmp_and_jump_insns (scratch, size_rtx, EQ, const0_rtx, SImode,
				   0, endlab);
	  emit_insn (gen_sub3_insn (ret_reg, ret_reg, GEN_INT (4)));
	  /* Write the address of the memory pointed to by temp_val into
	     the memory pointed to by mem.  */
	  emit_move_insn (mem, XEXP (temp_val, 0));
	  emit_label (endlab);
	}

      return mem;
    }
}

/* Handle TARGET_FUNCTION_VALUE, and TARGET_LIBCALL_VALUE target hook.
   For v9, function return values are subject to the same rules as arguments,
   except that up to 32 bytes may be returned in registers.  */

static rtx
sparc_function_value_1 (const_tree type, machine_mode mode,
			bool outgoing)
{
  /* Beware that the two values are swapped here wrt function_arg.  */
  int regbase = (outgoing
		 ? SPARC_INCOMING_INT_ARG_FIRST
		 : SPARC_OUTGOING_INT_ARG_FIRST);
  enum mode_class mclass = GET_MODE_CLASS (mode);
  int regno;

  /* Vector types deserve special treatment because they are polymorphic wrt
     their mode, depending upon whether VIS instructions are enabled.  */
  if (type && TREE_CODE (type) == VECTOR_TYPE)
    {
      HOST_WIDE_INT size = int_size_in_bytes (type);
      gcc_assert ((TARGET_ARCH32 && size <= 8)
		  || (TARGET_ARCH64 && size <= 32));

      if (mode == BLKmode)
	return function_arg_vector_value (size, SPARC_FP_ARG_FIRST);

      mclass = MODE_FLOAT;
    }

  if (TARGET_ARCH64 && type)
    {
      /* Structures up to 32 bytes in size are returned in registers.  */
      if (TREE_CODE (type) == RECORD_TYPE)
	{
	  HOST_WIDE_INT size = int_size_in_bytes (type);
	  gcc_assert (size <= 32);

	  return function_arg_record_value (type, mode, 0, 1, regbase);
	}

      /* Unions up to 32 bytes in size are returned in integer registers.  */
      else if (TREE_CODE (type) == UNION_TYPE)
	{
	  HOST_WIDE_INT size = int_size_in_bytes (type);
	  gcc_assert (size <= 32);

	  return function_arg_union_value (size, mode, 0, regbase);
	}

      /* Objects that require it are returned in FP registers.  */
      else if (mclass == MODE_FLOAT || mclass == MODE_COMPLEX_FLOAT)
	;

      /* All other aggregate types are returned in an integer register in a
	 mode corresponding to the size of the type.  */
      else if (AGGREGATE_TYPE_P (type))
	{
	  /* All other aggregate types are passed in an integer register
	     in a mode corresponding to the size of the type.  */
	  HOST_WIDE_INT size = int_size_in_bytes (type);
	  gcc_assert (size <= 32);

	  mode = int_mode_for_size (size * BITS_PER_UNIT, 0).else_blk ();

	  /* ??? We probably should have made the same ABI change in
	     3.4.0 as the one we made for unions.   The latter was
	     required by the SCD though, while the former is not
	     specified, so we favored compatibility and efficiency.

	     Now we're stuck for aggregates larger than 16 bytes,
	     because OImode vanished in the meantime.  Let's not
	     try to be unduly clever, and simply follow the ABI
	     for unions in that case.  */
	  if (mode == BLKmode)
	    return function_arg_union_value (size, mode, 0, regbase);
	  else
	    mclass = MODE_INT;
	}

      /* We should only have pointer and integer types at this point.  This
	 must match sparc_promote_function_mode.  */
      else if (mclass == MODE_INT && GET_MODE_SIZE (mode) < UNITS_PER_WORD)
	mode = word_mode;
    }

  /* We should only have pointer and integer types at this point, except with
     -freg-struct-return.  This must match sparc_promote_function_mode.  */
  else if (TARGET_ARCH32
	   && !(type && AGGREGATE_TYPE_P (type))
	   && mclass == MODE_INT
	   && GET_MODE_SIZE (mode) < UNITS_PER_WORD)
    mode = word_mode;

  if ((mclass == MODE_FLOAT || mclass == MODE_COMPLEX_FLOAT) && TARGET_FPU)
    regno = SPARC_FP_ARG_FIRST;
  else
    regno = regbase;

  return gen_rtx_REG (mode, regno);
}

/* Handle TARGET_FUNCTION_VALUE.
   On the SPARC, the value is found in the first "output" register, but the
   called function leaves it in the first "input" register.  */

static rtx
sparc_function_value (const_tree valtype,
		      const_tree fn_decl_or_type ATTRIBUTE_UNUSED,
		      bool outgoing)
{
  return sparc_function_value_1 (valtype, TYPE_MODE (valtype), outgoing);
}

/* Handle TARGET_LIBCALL_VALUE.  */

static rtx
sparc_libcall_value (machine_mode mode,
		     const_rtx fun ATTRIBUTE_UNUSED)
{
  return sparc_function_value_1 (NULL_TREE, mode, false);
}

/* Handle FUNCTION_VALUE_REGNO_P.
   On the SPARC, the first "output" reg is used for integer values, and the
   first floating point register is used for floating point values.  */

static bool
sparc_function_value_regno_p (const unsigned int regno)
{
  return (regno == 8 || (TARGET_FPU && regno == 32));
}

/* Do what is necessary for `va_start'.  We look at the current function
   to determine if stdarg or varargs is used and return the address of
   the first unnamed parameter.  */

static rtx
sparc_builtin_saveregs (void)
{
  int first_reg = crtl->args.info.words;
  rtx address;
  int regno;

  for (regno = first_reg; regno < SPARC_INT_ARG_MAX; regno++)
    emit_move_insn (gen_rtx_MEM (word_mode,
				 gen_rtx_PLUS (Pmode,
					       frame_pointer_rtx,
					       GEN_INT (FIRST_PARM_OFFSET (0)
							+ (UNITS_PER_WORD
							   * regno)))),
		    gen_rtx_REG (word_mode,
				 SPARC_INCOMING_INT_ARG_FIRST + regno));

  address = gen_rtx_PLUS (Pmode,
			  frame_pointer_rtx,
			  GEN_INT (FIRST_PARM_OFFSET (0)
				   + UNITS_PER_WORD * first_reg));

  return address;
}

/* Implement `va_start' for stdarg.  */

static void
sparc_va_start (tree valist, rtx nextarg)
{
  nextarg = expand_builtin_saveregs ();
  std_expand_builtin_va_start (valist, nextarg);
}

/* Implement `va_arg' for stdarg.  */

static tree
sparc_gimplify_va_arg (tree valist, tree type, gimple_seq *pre_p,
		       gimple_seq *post_p)
{
  HOST_WIDE_INT size, rsize, align;
  tree addr, incr;
  bool indirect;
  tree ptrtype = build_pointer_type (type);

  if (pass_by_reference (NULL, TYPE_MODE (type), type, false))
    {
      indirect = true;
      size = rsize = UNITS_PER_WORD;
      align = 0;
    }
  else
    {
      indirect = false;
      size = int_size_in_bytes (type);
      rsize = ROUND_UP (size, UNITS_PER_WORD);
      align = 0;

      if (TARGET_ARCH64)
	{
	  /* For SPARC64, objects requiring 16-byte alignment get it.  */
	  if (TYPE_ALIGN (type) >= 2 * (unsigned) BITS_PER_WORD)
	    align = 2 * UNITS_PER_WORD;

	  /* SPARC-V9 ABI states that structures up to 16 bytes in size
	     are left-justified in their slots.  */
	  if (AGGREGATE_TYPE_P (type))
	    {
	      if (size == 0)
		size = rsize = UNITS_PER_WORD;
	      else
		size = rsize;
	    }
	}
    }

  incr = valist;
  if (align)
    {
      incr = fold_build_pointer_plus_hwi (incr, align - 1);
      incr = fold_convert (sizetype, incr);
      incr = fold_build2 (BIT_AND_EXPR, sizetype, incr,
			  size_int (-align));
      incr = fold_convert (ptr_type_node, incr);
    }

  gimplify_expr (&incr, pre_p, post_p, is_gimple_val, fb_rvalue);
  addr = incr;

  if (BYTES_BIG_ENDIAN && size < rsize)
    addr = fold_build_pointer_plus_hwi (incr, rsize - size);

  if (indirect)
    {
      addr = fold_convert (build_pointer_type (ptrtype), addr);
      addr = build_va_arg_indirect_ref (addr);
    }

  /* If the address isn't aligned properly for the type, we need a temporary.
     FIXME: This is inefficient, usually we can do this in registers.  */
  else if (align == 0 && TYPE_ALIGN (type) > BITS_PER_WORD)
    {
      tree tmp = create_tmp_var (type, "va_arg_tmp");
      tree dest_addr = build_fold_addr_expr (tmp);
      tree copy = build_call_expr (builtin_decl_implicit (BUILT_IN_MEMCPY),
				   3, dest_addr, addr, size_int (rsize));
      TREE_ADDRESSABLE (tmp) = 1;
      gimplify_and_add (copy, pre_p);
      addr = dest_addr;
    }

  else
    addr = fold_convert (ptrtype, addr);

  incr = fold_build_pointer_plus_hwi (incr, rsize);
  gimplify_assign (valist, incr, post_p);

  return build_va_arg_indirect_ref (addr);
}

/* Implement the TARGET_VECTOR_MODE_SUPPORTED_P target hook.
   Specify whether the vector mode is supported by the hardware.  */

static bool
sparc_vector_mode_supported_p (machine_mode mode)
{
  return TARGET_VIS && VECTOR_MODE_P (mode) ? true : false;
}

/* Implement the TARGET_VECTORIZE_PREFERRED_SIMD_MODE target hook.  */

static machine_mode
sparc_preferred_simd_mode (scalar_mode mode)
{
  if (TARGET_VIS)
    switch (mode)
      {
      case E_SImode:
	return V2SImode;
      case E_HImode:
	return V4HImode;
      case E_QImode:
	return V8QImode;

      default:;
      }

  return word_mode;
}

/* Return the string to output an unconditional branch to LABEL, which is
   the operand number of the label.

   DEST is the destination insn (i.e. the label), INSN is the source.  */

const char *
output_ubranch (rtx dest, rtx_insn *insn)
{
  static char string[64];
  bool v9_form = false;
  int delta;
  char *p;

  /* Even if we are trying to use cbcond for this, evaluate
     whether we can use V9 branches as our backup plan.  */

  delta = 5000000;
  if (INSN_ADDRESSES_SET_P ())
    delta = (INSN_ADDRESSES (INSN_UID (dest))
	     - INSN_ADDRESSES (INSN_UID (insn)));

  /* Leave some instructions for "slop".  */
  if (TARGET_V9 && delta >= -260000 && delta < 260000)
    v9_form = true;

  if (TARGET_CBCOND)
    {
      bool emit_nop = emit_cbcond_nop (insn);
      bool far = false;
      const char *rval;

      if (delta < -500 || delta > 500)
	far = true;

      if (far)
	{
	  if (v9_form)
	    rval = "ba,a,pt\t%%xcc, %l0";
	  else
	    rval = "b,a\t%l0";
	}
      else
	{
	  if (emit_nop)
	    rval = "cwbe\t%%g0, %%g0, %l0\n\tnop";
	  else
	    rval = "cwbe\t%%g0, %%g0, %l0";
	}
      return rval;
    }

  if (v9_form)
    strcpy (string, "ba%*,pt\t%%xcc, ");
  else
    strcpy (string, "b%*\t");

  p = strchr (string, '\0');
  *p++ = '%';
  *p++ = 'l';
  *p++ = '0';
  *p++ = '%';
  *p++ = '(';
  *p = '\0';

  return string;
}

/* Return the string to output a conditional branch to LABEL, which is
   the operand number of the label.  OP is the conditional expression.
   XEXP (OP, 0) is assumed to be a condition code register (integer or
   floating point) and its mode specifies what kind of comparison we made.

   DEST is the destination insn (i.e. the label), INSN is the source.

   REVERSED is nonzero if we should reverse the sense of the comparison.

   ANNUL is nonzero if we should generate an annulling branch.  */

const char *
output_cbranch (rtx op, rtx dest, int label, int reversed, int annul,
		rtx_insn *insn)
{
  static char string[64];
  enum rtx_code code = GET_CODE (op);
  rtx cc_reg = XEXP (op, 0);
  machine_mode mode = GET_MODE (cc_reg);
  const char *labelno, *branch;
  int spaces = 8, far;
  char *p;

  /* v9 branches are limited to +-1MB.  If it is too far away,
     change

     bne,pt %xcc, .LC30

     to

     be,pn %xcc, .+12
      nop
     ba .LC30

     and

     fbne,a,pn %fcc2, .LC29

     to

     fbe,pt %fcc2, .+16
      nop
     ba .LC29  */

  far = TARGET_V9 && (get_attr_length (insn) >= 3);
  if (reversed ^ far)
    {
      /* Reversal of FP compares takes care -- an ordered compare
	 becomes an unordered compare and vice versa.  */
      if (mode == CCFPmode || mode == CCFPEmode)
	code = reverse_condition_maybe_unordered (code);
      else
	code = reverse_condition (code);
    }

  /* Start by writing the branch condition.  */
  if (mode == CCFPmode || mode == CCFPEmode)
    {
      switch (code)
	{
	case NE:
	  branch = "fbne";
	  break;
	case EQ:
	  branch = "fbe";
	  break;
	case GE:
	  branch = "fbge";
	  break;
	case GT:
	  branch = "fbg";
	  break;
	case LE:
	  branch = "fble";
	  break;
	case LT:
	  branch = "fbl";
	  break;
	case UNORDERED:
	  branch = "fbu";
	  break;
	case ORDERED:
	  branch = "fbo";
	  break;
	case UNGT:
	  branch = "fbug";
	  break;
	case UNLT:
	  branch = "fbul";
	  break;
	case UNEQ:
	  branch = "fbue";
	  break;
	case UNGE:
	  branch = "fbuge";
	  break;
	case UNLE:
	  branch = "fbule";
	  break;
	case LTGT:
	  branch = "fblg";
	  break;
	default:
	  gcc_unreachable ();
	}

      /* ??? !v9: FP branches cannot be preceded by another floating point
	 insn.  Because there is currently no concept of pre-delay slots,
	 we can fix this only by always emitting a nop before a floating
	 point branch.  */

      string[0] = '\0';
      if (! TARGET_V9)
	strcpy (string, "nop\n\t");
      strcat (string, branch);
    }
  else
    {
      switch (code)
	{
	case NE:
	  if (mode == CCVmode || mode == CCXVmode)
	    branch = "bvs";
	  else
	    branch = "bne";
	  break;
	case EQ:
	  if (mode == CCVmode || mode == CCXVmode)
	    branch = "bvc";
	  else
	    branch = "be";
	  break;
	case GE:
	  if (mode == CCNZmode || mode == CCXNZmode)
	    branch = "bpos";
	  else
	    branch = "bge";
	  break;
	case GT:
	  branch = "bg";
	  break;
	case LE:
	  branch = "ble";
	  break;
	case LT:
	  if (mode == CCNZmode || mode == CCXNZmode)
	    branch = "bneg";
	  else
	    branch = "bl";
	  break;
	case GEU:
	  branch = "bgeu";
	  break;
	case GTU:
	  branch = "bgu";
	  break;
	case LEU:
	  branch = "bleu";
	  break;
	case LTU:
	  branch = "blu";
	  break;
	default:
	  gcc_unreachable ();
	}
      strcpy (string, branch);
    }
  spaces -= strlen (branch);
  p = strchr (string, '\0');

  /* Now add the annulling, the label, and a possible noop.  */
  if (annul && ! far)
    {
      strcpy (p, ",a");
      p += 2;
      spaces -= 2;
    }

  if (TARGET_V9)
    {
      rtx note;
      int v8 = 0;

      if (! far && insn && INSN_ADDRESSES_SET_P ())
	{
	  int delta = (INSN_ADDRESSES (INSN_UID (dest))
		       - INSN_ADDRESSES (INSN_UID (insn)));
	  /* Leave some instructions for "slop".  */
	  if (delta < -260000 || delta >= 260000)
	    v8 = 1;
	}

      switch (mode)
	{
	case E_CCmode:
	case E_CCNZmode:
	case E_CCCmode:
	case E_CCVmode:
	  labelno = "%%icc, ";
	  if (v8)
	    labelno = "";
	  break;
	case E_CCXmode:
	case E_CCXNZmode:
	case E_CCXCmode:
	case E_CCXVmode:
	  labelno = "%%xcc, ";
	  gcc_assert (!v8);
	  break;
	case E_CCFPmode:
	case E_CCFPEmode:
	  {
	    static char v9_fcc_labelno[] = "%%fccX, ";
	    /* Set the char indicating the number of the fcc reg to use.  */
	    v9_fcc_labelno[5] = REGNO (cc_reg) - SPARC_FIRST_V9_FCC_REG + '0';
	    labelno = v9_fcc_labelno;
	    if (v8)
	      {
		gcc_assert (REGNO (cc_reg) == SPARC_FCC_REG);
		labelno = "";
	      }
	  }
	  break;
	default:
	  gcc_unreachable ();
	}

      if (*labelno && insn && (note = find_reg_note (insn, REG_BR_PROB, NULL_RTX)))
	{
	  strcpy (p,
		  ((profile_probability::from_reg_br_prob_note (XINT (note, 0))
		   >= profile_probability::even ()) ^ far)
		  ? ",pt" : ",pn");
	  p += 3;
	  spaces -= 3;
	}
    }
  else
    labelno = "";

  if (spaces > 0)
    *p++ = '\t';
  else
    *p++ = ' ';
  strcpy (p, labelno);
  p = strchr (p, '\0');
  if (far)
    {
      strcpy (p, ".+12\n\t nop\n\tb\t");
      /* Skip the next insn if requested or
	 if we know that it will be a nop.  */
      if (annul || ! final_sequence)
        p[3] = '6';
      p += 14;
    }
  *p++ = '%';
  *p++ = 'l';
  *p++ = label + '0';
  *p++ = '%';
  *p++ = '#';
  *p = '\0';

  return string;
}

/* Emit a library call comparison between floating point X and Y.
   COMPARISON is the operator to compare with (EQ, NE, GT, etc).
   Return the new operator to be used in the comparison sequence.

   TARGET_ARCH64 uses _Qp_* functions, which use pointers to TFmode
   values as arguments instead of the TFmode registers themselves,
   that's why we cannot call emit_float_lib_cmp.  */

rtx
sparc_emit_float_lib_cmp (rtx x, rtx y, enum rtx_code comparison)
{
  const char *qpfunc;
  rtx slot0, slot1, result, tem, tem2, libfunc;
  machine_mode mode;
  enum rtx_code new_comparison;

  switch (comparison)
    {
    case EQ:
      qpfunc = (TARGET_ARCH64 ? "_Qp_feq" : "_Q_feq");
      break;

    case NE:
      qpfunc = (TARGET_ARCH64 ? "_Qp_fne" : "_Q_fne");
      break;

    case GT:
      qpfunc = (TARGET_ARCH64 ? "_Qp_fgt" : "_Q_fgt");
      break;

    case GE:
      qpfunc = (TARGET_ARCH64 ? "_Qp_fge" : "_Q_fge");
      break;

    case LT:
      qpfunc = (TARGET_ARCH64 ? "_Qp_flt" : "_Q_flt");
      break;

    case LE:
      qpfunc = (TARGET_ARCH64 ? "_Qp_fle" : "_Q_fle");
      break;

    case ORDERED:
    case UNORDERED:
    case UNGT:
    case UNLT:
    case UNEQ:
    case UNGE:
    case UNLE:
    case LTGT:
      qpfunc = (TARGET_ARCH64 ? "_Qp_cmp" : "_Q_cmp");
      break;

    default:
      gcc_unreachable ();
    }

  if (TARGET_ARCH64)
    {
      if (MEM_P (x))
	{
	  tree expr = MEM_EXPR (x);
	  if (expr)
	    mark_addressable (expr);
	  slot0 = x;
	}
      else
	{
	  slot0 = assign_stack_temp (TFmode, GET_MODE_SIZE(TFmode));
	  emit_move_insn (slot0, x);
	}

      if (MEM_P (y))
	{
	  tree expr = MEM_EXPR (y);
	  if (expr)
	    mark_addressable (expr);
	  slot1 = y;
	}
      else
	{
	  slot1 = assign_stack_temp (TFmode, GET_MODE_SIZE(TFmode));
	  emit_move_insn (slot1, y);
	}

      libfunc = gen_rtx_SYMBOL_REF (Pmode, qpfunc);
      emit_library_call (libfunc, LCT_NORMAL,
			 DImode,
			 XEXP (slot0, 0), Pmode,
			 XEXP (slot1, 0), Pmode);
      mode = DImode;
    }
  else
    {
      libfunc = gen_rtx_SYMBOL_REF (Pmode, qpfunc);
      emit_library_call (libfunc, LCT_NORMAL,
			 SImode,
			 x, TFmode, y, TFmode);
      mode = SImode;
    }


  /* Immediately move the result of the libcall into a pseudo
     register so reload doesn't clobber the value if it needs
     the return register for a spill reg.  */
  result = gen_reg_rtx (mode);
  emit_move_insn (result, hard_libcall_value (mode, libfunc));

  switch (comparison)
    {
    default:
      return gen_rtx_NE (VOIDmode, result, const0_rtx);
    case ORDERED:
    case UNORDERED:
      new_comparison = (comparison == UNORDERED ? EQ : NE);
      return gen_rtx_fmt_ee (new_comparison, VOIDmode, result, GEN_INT(3));
    case UNGT:
    case UNGE:
      new_comparison = (comparison == UNGT ? GT : NE);
      return gen_rtx_fmt_ee (new_comparison, VOIDmode, result, const1_rtx);
    case UNLE:
      return gen_rtx_NE (VOIDmode, result, const2_rtx);
    case UNLT:
      tem = gen_reg_rtx (mode);
      if (TARGET_ARCH32)
	emit_insn (gen_andsi3 (tem, result, const1_rtx));
      else
	emit_insn (gen_anddi3 (tem, result, const1_rtx));
      return gen_rtx_NE (VOIDmode, tem, const0_rtx);
    case UNEQ:
    case LTGT:
      tem = gen_reg_rtx (mode);
      if (TARGET_ARCH32)
	emit_insn (gen_addsi3 (tem, result, const1_rtx));
      else
	emit_insn (gen_adddi3 (tem, result, const1_rtx));
      tem2 = gen_reg_rtx (mode);
      if (TARGET_ARCH32)
	emit_insn (gen_andsi3 (tem2, tem, const2_rtx));
      else
	emit_insn (gen_anddi3 (tem2, tem, const2_rtx));
      new_comparison = (comparison == UNEQ ? EQ : NE);
      return gen_rtx_fmt_ee (new_comparison, VOIDmode, tem2, const0_rtx);
    }

  gcc_unreachable ();
}

/* Generate an unsigned DImode to FP conversion.  This is the same code
   optabs would emit if we didn't have TFmode patterns.  */

void
sparc_emit_floatunsdi (rtx *operands, machine_mode mode)
{
  rtx i0, i1, f0, in, out;

  out = operands[0];
  in = force_reg (DImode, operands[1]);
  rtx_code_label *neglab = gen_label_rtx ();
  rtx_code_label *donelab = gen_label_rtx ();
  i0 = gen_reg_rtx (DImode);
  i1 = gen_reg_rtx (DImode);
  f0 = gen_reg_rtx (mode);

  emit_cmp_and_jump_insns (in, const0_rtx, LT, const0_rtx, DImode, 0, neglab);

  emit_insn (gen_rtx_SET (out, gen_rtx_FLOAT (mode, in)));
  emit_jump_insn (gen_jump (donelab));
  emit_barrier ();

  emit_label (neglab);

  emit_insn (gen_lshrdi3 (i0, in, const1_rtx));
  emit_insn (gen_anddi3 (i1, in, const1_rtx));
  emit_insn (gen_iordi3 (i0, i0, i1));
  emit_insn (gen_rtx_SET (f0, gen_rtx_FLOAT (mode, i0)));
  emit_insn (gen_rtx_SET (out, gen_rtx_PLUS (mode, f0, f0)));

  emit_label (donelab);
}

/* Generate an FP to unsigned DImode conversion.  This is the same code
   optabs would emit if we didn't have TFmode patterns.  */

void
sparc_emit_fixunsdi (rtx *operands, machine_mode mode)
{
  rtx i0, i1, f0, in, out, limit;

  out = operands[0];
  in = force_reg (mode, operands[1]);
  rtx_code_label *neglab = gen_label_rtx ();
  rtx_code_label *donelab = gen_label_rtx ();
  i0 = gen_reg_rtx (DImode);
  i1 = gen_reg_rtx (DImode);
  limit = gen_reg_rtx (mode);
  f0 = gen_reg_rtx (mode);

  emit_move_insn (limit,
		  const_double_from_real_value (
		    REAL_VALUE_ATOF ("9223372036854775808.0", mode), mode));
  emit_cmp_and_jump_insns (in, limit, GE, NULL_RTX, mode, 0, neglab);

  emit_insn (gen_rtx_SET (out,
			  gen_rtx_FIX (DImode, gen_rtx_FIX (mode, in))));
  emit_jump_insn (gen_jump (donelab));
  emit_barrier ();

  emit_label (neglab);

  emit_insn (gen_rtx_SET (f0, gen_rtx_MINUS (mode, in, limit)));
  emit_insn (gen_rtx_SET (i0,
			  gen_rtx_FIX (DImode, gen_rtx_FIX (mode, f0))));
  emit_insn (gen_movdi (i1, const1_rtx));
  emit_insn (gen_ashldi3 (i1, i1, GEN_INT (63)));
  emit_insn (gen_xordi3 (out, i0, i1));

  emit_label (donelab);
}

/* Return the string to output a compare and branch instruction to DEST.
   DEST is the destination insn (i.e. the label), INSN is the source,
   and OP is the conditional expression.  */

const char *
output_cbcond (rtx op, rtx dest, rtx_insn *insn)
{
  machine_mode mode = GET_MODE (XEXP (op, 0));
  enum rtx_code code = GET_CODE (op);
  const char *cond_str, *tmpl;
  int far, emit_nop, len;
  static char string[64];
  char size_char;

  /* Compare and Branch is limited to +-2KB.  If it is too far away,
     change

     cxbne X, Y, .LC30

     to

     cxbe X, Y, .+16
     nop
     ba,pt xcc, .LC30
      nop  */

  len = get_attr_length (insn);

  far = len == 4;
  emit_nop = len == 2;

  if (far)
    code = reverse_condition (code);

  size_char = ((mode == SImode) ? 'w' : 'x');

  switch (code)
    {
    case NE:
      cond_str = "ne";
      break;

    case EQ:
      cond_str = "e";
      break;

    case GE:
      cond_str = "ge";
      break;

    case GT:
      cond_str = "g";
      break;

    case LE:
      cond_str = "le";
      break;

    case LT:
      cond_str = "l";
      break;

    case GEU:
      cond_str = "cc";
      break;

    case GTU:
      cond_str = "gu";
      break;

    case LEU:
      cond_str = "leu";
      break;

    case LTU:
      cond_str = "cs";
      break;

    default:
      gcc_unreachable ();
    }

  if (far)
    {
      int veryfar = 1, delta;

      if (INSN_ADDRESSES_SET_P ())
	{
	  delta = (INSN_ADDRESSES (INSN_UID (dest))
		   - INSN_ADDRESSES (INSN_UID (insn)));
	  /* Leave some instructions for "slop".  */
	  if (delta >= -260000 && delta < 260000)
	    veryfar = 0;
	}

      if (veryfar)
	tmpl = "c%cb%s\t%%1, %%2, .+16\n\tnop\n\tb\t%%3\n\tnop";
      else
	tmpl = "c%cb%s\t%%1, %%2, .+16\n\tnop\n\tba,pt\t%%%%xcc, %%3\n\tnop";
    }
  else
    {
      if (emit_nop)
	tmpl = "c%cb%s\t%%1, %%2, %%3\n\tnop";
      else
	tmpl = "c%cb%s\t%%1, %%2, %%3";
    }

  snprintf (string, sizeof(string), tmpl, size_char, cond_str);

  return string;
}

/* Return the string to output a conditional branch to LABEL, testing
   register REG.  LABEL is the operand number of the label; REG is the
   operand number of the reg.  OP is the conditional expression.  The mode
   of REG says what kind of comparison we made.

   DEST is the destination insn (i.e. the label), INSN is the source.

   REVERSED is nonzero if we should reverse the sense of the comparison.

   ANNUL is nonzero if we should generate an annulling branch.  */

const char *
output_v9branch (rtx op, rtx dest, int reg, int label, int reversed,
		 int annul, rtx_insn *insn)
{
  static char string[64];
  enum rtx_code code = GET_CODE (op);
  machine_mode mode = GET_MODE (XEXP (op, 0));
  rtx note;
  int far;
  char *p;

  /* branch on register are limited to +-128KB.  If it is too far away,
     change

     brnz,pt %g1, .LC30

     to

     brz,pn %g1, .+12
      nop
     ba,pt %xcc, .LC30

     and

     brgez,a,pn %o1, .LC29

     to

     brlz,pt %o1, .+16
      nop
     ba,pt %xcc, .LC29  */

  far = get_attr_length (insn) >= 3;

  /* If not floating-point or if EQ or NE, we can just reverse the code.  */
  if (reversed ^ far)
    code = reverse_condition (code);

  /* Only 64-bit versions of these instructions exist.  */
  gcc_assert (mode == DImode);

  /* Start by writing the branch condition.  */

  switch (code)
    {
    case NE:
      strcpy (string, "brnz");
      break;

    case EQ:
      strcpy (string, "brz");
      break;

    case GE:
      strcpy (string, "brgez");
      break;

    case LT:
      strcpy (string, "brlz");
      break;

    case LE:
      strcpy (string, "brlez");
      break;

    case GT:
      strcpy (string, "brgz");
      break;

    default:
      gcc_unreachable ();
    }

  p = strchr (string, '\0');

  /* Now add the annulling, reg, label, and nop.  */
  if (annul && ! far)
    {
      strcpy (p, ",a");
      p += 2;
    }

  if (insn && (note = find_reg_note (insn, REG_BR_PROB, NULL_RTX)))
    {
      strcpy (p,
	      ((profile_probability::from_reg_br_prob_note (XINT (note, 0))
	       >= profile_probability::even ()) ^ far)
	      ? ",pt" : ",pn");
      p += 3;
    }

  *p = p < string + 8 ? '\t' : ' ';
  p++;
  *p++ = '%';
  *p++ = '0' + reg;
  *p++ = ',';
  *p++ = ' ';
  if (far)
    {
      int veryfar = 1, delta;

      if (INSN_ADDRESSES_SET_P ())
	{
	  delta = (INSN_ADDRESSES (INSN_UID (dest))
		   - INSN_ADDRESSES (INSN_UID (insn)));
	  /* Leave some instructions for "slop".  */
	  if (delta >= -260000 && delta < 260000)
	    veryfar = 0;
	}

      strcpy (p, ".+12\n\t nop\n\t");
      /* Skip the next insn if requested or
	 if we know that it will be a nop.  */
      if (annul || ! final_sequence)
        p[3] = '6';
      p += 12;
      if (veryfar)
	{
	  strcpy (p, "b\t");
	  p += 2;
	}
      else
	{
	  strcpy (p, "ba,pt\t%%xcc, ");
	  p += 13;
	}
    }
  *p++ = '%';
  *p++ = 'l';
  *p++ = '0' + label;
  *p++ = '%';
  *p++ = '#';
  *p = '\0';

  return string;
}

/* Return 1, if any of the registers of the instruction are %l[0-7] or %o[0-7].
   Such instructions cannot be used in the delay slot of return insn on v9.
   If TEST is 0, also rename all %i[0-7] registers to their %o[0-7] counterparts.
 */

static int
epilogue_renumber (register rtx *where, int test)
{
  register const char *fmt;
  register int i;
  register enum rtx_code code;

  if (*where == 0)
    return 0;

  code = GET_CODE (*where);

  switch (code)
    {
    case REG:
      if (REGNO (*where) >= 8 && REGNO (*where) < 24)      /* oX or lX */
	return 1;
      if (! test && REGNO (*where) >= 24 && REGNO (*where) < 32)
	*where = gen_rtx_REG (GET_MODE (*where), OUTGOING_REGNO (REGNO(*where)));
      /* fallthrough */
    case SCRATCH:
    case CC0:
    case PC:
    case CONST_INT:
    case CONST_WIDE_INT:
    case CONST_DOUBLE:
      return 0;

      /* Do not replace the frame pointer with the stack pointer because
	 it can cause the delayed instruction to load below the stack.
	 This occurs when instructions like:

	 (set (reg/i:SI 24 %i0)
	     (mem/f:SI (plus:SI (reg/f:SI 30 %fp)
                       (const_int -20 [0xffffffec])) 0))

	 are in the return delayed slot.  */
    case PLUS:
      if (GET_CODE (XEXP (*where, 0)) == REG
	  && REGNO (XEXP (*where, 0)) == HARD_FRAME_POINTER_REGNUM
	  && (GET_CODE (XEXP (*where, 1)) != CONST_INT
	      || INTVAL (XEXP (*where, 1)) < SPARC_STACK_BIAS))
	return 1;
      break;

    case MEM:
      if (SPARC_STACK_BIAS
	  && GET_CODE (XEXP (*where, 0)) == REG
	  && REGNO (XEXP (*where, 0)) == HARD_FRAME_POINTER_REGNUM)
	return 1;
      break;

    default:
      break;
    }

  fmt = GET_RTX_FORMAT (code);

  for (i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
    {
      if (fmt[i] == 'E')
	{
	  register int j;
	  for (j = XVECLEN (*where, i) - 1; j >= 0; j--)
	    if (epilogue_renumber (&(XVECEXP (*where, i, j)), test))
	      return 1;
	}
      else if (fmt[i] == 'e'
	       && epilogue_renumber (&(XEXP (*where, i)), test))
	return 1;
    }
  return 0;
}

/* Leaf functions and non-leaf functions have different needs.  */

static const int
reg_leaf_alloc_order[] = REG_LEAF_ALLOC_ORDER;

static const int
reg_nonleaf_alloc_order[] = REG_ALLOC_ORDER;

static const int *const reg_alloc_orders[] = {
  reg_leaf_alloc_order,
  reg_nonleaf_alloc_order};

void
order_regs_for_local_alloc (void)
{
  static int last_order_nonleaf = 1;

  if (df_regs_ever_live_p (15) != last_order_nonleaf)
    {
      last_order_nonleaf = !last_order_nonleaf;
      memcpy ((char *) reg_alloc_order,
	      (const char *) reg_alloc_orders[last_order_nonleaf],
	      FIRST_PSEUDO_REGISTER * sizeof (int));
    }
}

/* Return 1 if REG and MEM are legitimate enough to allow the various
   MEM<-->REG splits to be run.  */

int
sparc_split_reg_mem_legitimate (rtx reg, rtx mem)
{
  /* Punt if we are here by mistake.  */
  gcc_assert (reload_completed);

  /* We must have an offsettable memory reference.  */
  if (!offsettable_memref_p (mem))
    return 0;

  /* If we have legitimate args for ldd/std, we do not want
     the split to happen.  */
  if ((REGNO (reg) % 2) == 0 && mem_min_alignment (mem, 8))
    return 0;

  /* Success.  */
  return 1;
}

/* Split a REG <-- MEM move into a pair of moves in MODE.  */

void
sparc_split_reg_mem (rtx dest, rtx src, machine_mode mode)
{
  rtx high_part = gen_highpart (mode, dest);
  rtx low_part = gen_lowpart (mode, dest);
  rtx word0 = adjust_address (src, mode, 0);
  rtx word1 = adjust_address (src, mode, 4);

  if (reg_overlap_mentioned_p (high_part, word1))
    {
      emit_move_insn_1 (low_part, word1);
      emit_move_insn_1 (high_part, word0);
    }
  else
    {
      emit_move_insn_1 (high_part, word0);
      emit_move_insn_1 (low_part, word1);
    }
}

/* Split a MEM <-- REG move into a pair of moves in MODE.  */

void
sparc_split_mem_reg (rtx dest, rtx src, machine_mode mode)
{
  rtx word0 = adjust_address (dest, mode, 0);
  rtx word1 = adjust_address (dest, mode, 4);
  rtx high_part = gen_highpart (mode, src);
  rtx low_part = gen_lowpart (mode, src);

  emit_move_insn_1 (word0, high_part);
  emit_move_insn_1 (word1, low_part);
}

/* Like sparc_split_reg_mem_legitimate but for REG <--> REG moves.  */

int
sparc_split_reg_reg_legitimate (rtx reg1, rtx reg2)
{
  /* Punt if we are here by mistake.  */
  gcc_assert (reload_completed);

  if (GET_CODE (reg1) == SUBREG)
    reg1 = SUBREG_REG (reg1);
  if (GET_CODE (reg1) != REG)
    return 0;
  const int regno1 = REGNO (reg1);

  if (GET_CODE (reg2) == SUBREG)
    reg2 = SUBREG_REG (reg2);
  if (GET_CODE (reg2) != REG)
    return 0;
  const int regno2 = REGNO (reg2);

  if (SPARC_INT_REG_P (regno1) && SPARC_INT_REG_P (regno2))
    return 1;

  if (TARGET_VIS3)
    {
      if ((SPARC_INT_REG_P (regno1) && SPARC_FP_REG_P (regno2))
	  || (SPARC_FP_REG_P (regno1) && SPARC_INT_REG_P (regno2)))
	return 1;
    }

  return 0;
}

/* Split a REG <--> REG move into a pair of moves in MODE.  */

void
sparc_split_reg_reg (rtx dest, rtx src, machine_mode mode)
{
  rtx dest1 = gen_highpart (mode, dest);
  rtx dest2 = gen_lowpart (mode, dest);
  rtx src1 = gen_highpart (mode, src);
  rtx src2 = gen_lowpart (mode, src);

  /* Now emit using the real source and destination we found, swapping
     the order if we detect overlap.  */
  if (reg_overlap_mentioned_p (dest1, src2))
    {
      emit_move_insn_1 (dest2, src2);
      emit_move_insn_1 (dest1, src1);
    }
  else
    {
      emit_move_insn_1 (dest1, src1);
      emit_move_insn_1 (dest2, src2);
    }
}

/* Return 1 if REGNO (reg1) is even and REGNO (reg1) == REGNO (reg2) - 1.
   This makes them candidates for using ldd and std insns.

   Note reg1 and reg2 *must* be hard registers.  */

int
registers_ok_for_ldd_peep (rtx reg1, rtx reg2)
{
  /* We might have been passed a SUBREG.  */
  if (GET_CODE (reg1) != REG || GET_CODE (reg2) != REG)
    return 0;

  if (REGNO (reg1) % 2 != 0)
    return 0;

  /* Integer ldd is deprecated in SPARC V9 */
  if (TARGET_V9 && SPARC_INT_REG_P (REGNO (reg1)))
    return 0;

  return (REGNO (reg1) == REGNO (reg2) - 1);
}

/* Return 1 if the addresses in mem1 and mem2 are suitable for use in
   an ldd or std insn.

   This can only happen when addr1 and addr2, the addresses in mem1
   and mem2, are consecutive memory locations (addr1 + 4 == addr2).
   addr1 must also be aligned on a 64-bit boundary.

   Also iff dependent_reg_rtx is not null it should not be used to
   compute the address for mem1, i.e. we cannot optimize a sequence
   like:
   	ld [%o0], %o0
	ld [%o0 + 4], %o1
   to
   	ldd [%o0], %o0
   nor:
	ld [%g3 + 4], %g3
	ld [%g3], %g2
   to
        ldd [%g3], %g2

   But, note that the transformation from:
	ld [%g2 + 4], %g3
        ld [%g2], %g2
   to
	ldd [%g2], %g2
   is perfectly fine.  Thus, the peephole2 patterns always pass us
   the destination register of the first load, never the second one.

   For stores we don't have a similar problem, so dependent_reg_rtx is
   NULL_RTX.  */

int
mems_ok_for_ldd_peep (rtx mem1, rtx mem2, rtx dependent_reg_rtx)
{
  rtx addr1, addr2;
  unsigned int reg1;
  HOST_WIDE_INT offset1;

  /* The mems cannot be volatile.  */
  if (MEM_VOLATILE_P (mem1) || MEM_VOLATILE_P (mem2))
    return 0;

  /* MEM1 should be aligned on a 64-bit boundary.  */
  if (MEM_ALIGN (mem1) < 64)
    return 0;

  addr1 = XEXP (mem1, 0);
  addr2 = XEXP (mem2, 0);

  /* Extract a register number and offset (if used) from the first addr.  */
  if (GET_CODE (addr1) == PLUS)
    {
      /* If not a REG, return zero.  */
      if (GET_CODE (XEXP (addr1, 0)) != REG)
	return 0;
      else
	{
          reg1 = REGNO (XEXP (addr1, 0));
	  /* The offset must be constant!  */
	  if (GET_CODE (XEXP (addr1, 1)) != CONST_INT)
            return 0;
          offset1 = INTVAL (XEXP (addr1, 1));
	}
    }
  else if (GET_CODE (addr1) != REG)
    return 0;
  else
    {
      reg1 = REGNO (addr1);
      /* This was a simple (mem (reg)) expression.  Offset is 0.  */
      offset1 = 0;
    }

  /* Make sure the second address is a (mem (plus (reg) (const_int).  */
  if (GET_CODE (addr2) != PLUS)
    return 0;

  if (GET_CODE (XEXP (addr2, 0)) != REG
      || GET_CODE (XEXP (addr2, 1)) != CONST_INT)
    return 0;

  if (reg1 != REGNO (XEXP (addr2, 0)))
    return 0;

  if (dependent_reg_rtx != NULL_RTX && reg1 == REGNO (dependent_reg_rtx))
    return 0;

  /* The first offset must be evenly divisible by 8 to ensure the
     address is 64-bit aligned.  */
  if (offset1 % 8 != 0)
    return 0;

  /* The offset for the second addr must be 4 more than the first addr.  */
  if (INTVAL (XEXP (addr2, 1)) != offset1 + 4)
    return 0;

  /* All the tests passed.  addr1 and addr2 are valid for ldd and std
     instructions.  */
  return 1;
}

/* Return the widened memory access made of MEM1 and MEM2 in MODE.  */

rtx
widen_mem_for_ldd_peep (rtx mem1, rtx mem2, machine_mode mode)
{
  rtx x = widen_memory_access (mem1, mode, 0);
  MEM_NOTRAP_P (x) = MEM_NOTRAP_P (mem1) && MEM_NOTRAP_P (mem2);
  return x;
}

/* Return 1 if reg is a pseudo, or is the first register in
   a hard register pair.  This makes it suitable for use in
   ldd and std insns.  */

int
register_ok_for_ldd (rtx reg)
{
  /* We might have been passed a SUBREG.  */
  if (!REG_P (reg))
    return 0;

  if (REGNO (reg) < FIRST_PSEUDO_REGISTER)
    return (REGNO (reg) % 2 == 0);

  return 1;
}

/* Return 1 if OP, a MEM, has an address which is known to be
   aligned to an 8-byte boundary.  */

int
memory_ok_for_ldd (rtx op)
{
  /* In 64-bit mode, we assume that the address is word-aligned.  */
  if (TARGET_ARCH32 && !mem_min_alignment (op, 8))
    return 0;

  if (! can_create_pseudo_p ()
      && !strict_memory_address_p (Pmode, XEXP (op, 0)))
    return 0;

  return 1;
}

/* Implement TARGET_PRINT_OPERAND_PUNCT_VALID_P.  */

static bool
sparc_print_operand_punct_valid_p (unsigned char code)
{
  if (code == '#'
      || code == '*'
      || code == '('
      || code == ')'
      || code == '_'
      || code == '&')
    return true;

  return false;
}

/* Implement TARGET_PRINT_OPERAND.
   Print operand X (an rtx) in assembler syntax to file FILE.
   CODE is a letter or dot (`z' in `%z0') or 0 if no letter was specified.
   For `%' followed by punctuation, CODE is the punctuation and X is null.  */

static void
sparc_print_operand (FILE *file, rtx x, int code)
{
  const char *s;

  switch (code)
    {
    case '#':
      /* Output an insn in a delay slot.  */
      if (final_sequence)
        sparc_indent_opcode = 1;
      else
	fputs ("\n\t nop", file);
      return;
    case '*':
      /* Output an annul flag if there's nothing for the delay slot and we
	 are optimizing.  This is always used with '(' below.
         Sun OS 4.1.1 dbx can't handle an annulled unconditional branch;
	 this is a dbx bug.  So, we only do this when optimizing.
         On UltraSPARC, a branch in a delay slot causes a pipeline flush.
	 Always emit a nop in case the next instruction is a branch.  */
      if (! final_sequence && (optimize && (int)sparc_cpu < PROCESSOR_V9))
	fputs (",a", file);
      return;
    case '(':
      /* Output a 'nop' if there's nothing for the delay slot and we are
	 not optimizing.  This is always used with '*' above.  */
      if (! final_sequence && ! (optimize && (int)sparc_cpu < PROCESSOR_V9))
	fputs ("\n\t nop", file);
      else if (final_sequence)
        sparc_indent_opcode = 1;
      return;
    case ')':
      /* Output the right displacement from the saved PC on function return.
	 The caller may have placed an "unimp" insn immediately after the call
	 so we have to account for it.  This insn is used in the 32-bit ABI
	 when calling a function that returns a non zero-sized structure.  The
	 64-bit ABI doesn't have it.  Be careful to have this test be the same
	 as that for the call.  The exception is when sparc_std_struct_return
	 is enabled, the psABI is followed exactly and the adjustment is made
	 by the code in sparc_struct_value_rtx.  The call emitted is the same
	 when sparc_std_struct_return is enabled. */
     if (!TARGET_ARCH64
	 && cfun->returns_struct
	 && !sparc_std_struct_return
	 && DECL_SIZE (DECL_RESULT (current_function_decl))
	 && TREE_CODE (DECL_SIZE (DECL_RESULT (current_function_decl)))
	     == INTEGER_CST
	 && !integer_zerop (DECL_SIZE (DECL_RESULT (current_function_decl))))
	fputs ("12", file);
      else
        fputc ('8', file);
      return;
    case '_':
      /* Output the Embedded Medium/Anywhere code model base register.  */
      fputs (EMBMEDANY_BASE_REG, file);
      return;
    case '&':
      /* Print some local dynamic TLS name.  */
      if (const char *name = get_some_local_dynamic_name ())
	assemble_name (file, name);
      else
	output_operand_lossage ("'%%&' used without any "
				"local dynamic TLS references");
      return;

    case 'Y':
      /* Adjust the operand to take into account a RESTORE operation.  */
      if (GET_CODE (x) == CONST_INT)
	break;
      else if (GET_CODE (x) != REG)
	output_operand_lossage ("invalid %%Y operand");
      else if (REGNO (x) < 8)
	fputs (reg_names[REGNO (x)], file);
      else if (REGNO (x) >= 24 && REGNO (x) < 32)
	fputs (reg_names[REGNO (x)-16], file);
      else
	output_operand_lossage ("invalid %%Y operand");
      return;
    case 'L':
      /* Print out the low order register name of a register pair.  */
      if (WORDS_BIG_ENDIAN)
	fputs (reg_names[REGNO (x)+1], file);
      else
	fputs (reg_names[REGNO (x)], file);
      return;
    case 'H':
      /* Print out the high order register name of a register pair.  */
      if (WORDS_BIG_ENDIAN)
	fputs (reg_names[REGNO (x)], file);
      else
	fputs (reg_names[REGNO (x)+1], file);
      return;
    case 'R':
      /* Print out the second register name of a register pair or quad.
	 I.e., R (%o0) => %o1.  */
      fputs (reg_names[REGNO (x)+1], file);
      return;
    case 'S':
      /* Print out the third register name of a register quad.
	 I.e., S (%o0) => %o2.  */
      fputs (reg_names[REGNO (x)+2], file);
      return;
    case 'T':
      /* Print out the fourth register name of a register quad.
	 I.e., T (%o0) => %o3.  */
      fputs (reg_names[REGNO (x)+3], file);
      return;
    case 'x':
      /* Print a condition code register.  */
      if (REGNO (x) == SPARC_ICC_REG)
	{
	  switch (GET_MODE (x))
	    {
	    case E_CCmode:
	    case E_CCNZmode:
	    case E_CCCmode:
	    case E_CCVmode:
	      s = "%icc";
	      break;
	    case E_CCXmode:
	    case E_CCXNZmode:
	    case E_CCXCmode:
	    case E_CCXVmode:
	      s = "%xcc";
	      break;
	    default:
	      gcc_unreachable ();
	    }
	  fputs (s, file);
	}
      else
	/* %fccN register */
	fputs (reg_names[REGNO (x)], file);
      return;
    case 'm':
      /* Print the operand's address only.  */
      output_address (GET_MODE (x), XEXP (x, 0));
      return;
    case 'r':
      /* In this case we need a register.  Use %g0 if the
	 operand is const0_rtx.  */
      if (x == const0_rtx
	  || (GET_MODE (x) != VOIDmode && x == CONST0_RTX (GET_MODE (x))))
	{
	  fputs ("%g0", file);
	  return;
	}
      else
	break;

    case 'A':
      switch (GET_CODE (x))
	{
	case IOR:
	  s = "or";
	  break;
	case AND:
	  s = "and";
	  break;
	case XOR:
	  s = "xor";
	  break;
	default:
	  output_operand_lossage ("invalid %%A operand");
	  s = "";
	  break;
	}
      fputs (s, file);
      return;

    case 'B':
      switch (GET_CODE (x))
	{
	case IOR:
	  s = "orn";
	  break;
	case AND:
	  s = "andn";
	  break;
	case XOR:
	  s = "xnor";
	  break;
	default:
	  output_operand_lossage ("invalid %%B operand");
	  s = "";
	  break;
	}
      fputs (s, file);
      return;

      /* This is used by the conditional move instructions.  */
    case 'C':
      {
	machine_mode mode = GET_MODE (XEXP (x, 0));
	switch (GET_CODE (x))
	  {
	  case NE:
	    if (mode == CCVmode || mode == CCXVmode)
	      s = "vs";
	    else
	      s = "ne";
	    break;
	  case EQ:
	    if (mode == CCVmode || mode == CCXVmode)
	      s = "vc";
	    else
	      s = "e";
	    break;
	  case GE:
	    if (mode == CCNZmode || mode == CCXNZmode)
	      s = "pos";
	    else
	      s = "ge";
	    break;
	  case GT:
	    s = "g";
	    break;
	  case LE:
	    s = "le";
	    break;
	  case LT:
	    if (mode == CCNZmode || mode == CCXNZmode)
	      s = "neg";
	    else
	      s = "l";
	    break;
	  case GEU:
	    s = "geu";
	    break;
	  case GTU:
	    s = "gu";
	    break;
	  case LEU:
	    s = "leu";
	    break;
	  case LTU:
	    s = "lu";
	    break;
	  case LTGT:
	    s = "lg";
	    break;
	  case UNORDERED:
	    s = "u";
	    break;
	  case ORDERED:
	    s = "o";
	    break;
	  case UNLT:
	    s = "ul";
	    break;
	  case UNLE:
	    s = "ule";
	    break;
	  case UNGT:
	    s = "ug";
	    break;
	  case UNGE:
	    s = "uge"
	    ; break;
	  case UNEQ:
	    s = "ue";
	    break;
	  default:
	    output_operand_lossage ("invalid %%C operand");
	    s = "";
	    break;
	  }
	fputs (s, file);
	return;
      }

      /* This are used by the movr instruction pattern.  */
    case 'D':
      {
	switch (GET_CODE (x))
	  {
	  case NE:
	    s = "ne";
	    break;
	  case EQ:
	    s = "e";
	    break;
	  case GE:
	    s = "gez";
	    break;
	  case LT:
	    s = "lz";
	    break;
	  case LE:
	    s = "lez";
	    break;
	  case GT:
	    s = "gz";
	    break;
	  default:
	    output_operand_lossage ("invalid %%D operand");
	    s = "";
	    break;
	  }
	fputs (s, file);
	return;
      }

    case 'b':
      {
	/* Print a sign-extended character.  */
	int i = trunc_int_for_mode (INTVAL (x), QImode);
	fprintf (file, "%d", i);
	return;
      }

    case 'f':
      /* Operand must be a MEM; write its address.  */
      if (GET_CODE (x) != MEM)
	output_operand_lossage ("invalid %%f operand");
      output_address (GET_MODE (x), XEXP (x, 0));
      return;

    case 's':
      {
	/* Print a sign-extended 32-bit value.  */
	HOST_WIDE_INT i;
	if (GET_CODE(x) == CONST_INT)
	  i = INTVAL (x);
	else
	  {
	    output_operand_lossage ("invalid %%s operand");
	    return;
	  }
	i = trunc_int_for_mode (i, SImode);
	fprintf (file, HOST_WIDE_INT_PRINT_DEC, i);
	return;
      }

    case 0:
      /* Do nothing special.  */
      break;

    default:
      /* Undocumented flag.  */
      output_operand_lossage ("invalid operand output code");
    }

  if (GET_CODE (x) == REG)
    fputs (reg_names[REGNO (x)], file);
  else if (GET_CODE (x) == MEM)
    {
      fputc ('[', file);
	/* Poor Sun assembler doesn't understand absolute addressing.  */
      if (CONSTANT_P (XEXP (x, 0)))
	fputs ("%g0+", file);
      output_address (GET_MODE (x), XEXP (x, 0));
      fputc (']', file);
    }
  else if (GET_CODE (x) == HIGH)
    {
      fputs ("%hi(", file);
      output_addr_const (file, XEXP (x, 0));
      fputc (')', file);
    }
  else if (GET_CODE (x) == LO_SUM)
    {
      sparc_print_operand (file, XEXP (x, 0), 0);
      if (TARGET_CM_MEDMID)
	fputs ("+%l44(", file);
      else
	fputs ("+%lo(", file);
      output_addr_const (file, XEXP (x, 1));
      fputc (')', file);
    }
  else if (GET_CODE (x) == CONST_DOUBLE)
    output_operand_lossage ("floating-point constant not a valid immediate operand");
  else
    output_addr_const (file, x);
}

/* Implement TARGET_PRINT_OPERAND_ADDRESS.  */

static void
sparc_print_operand_address (FILE *file, machine_mode /*mode*/, rtx x)
{
  register rtx base, index = 0;
  int offset = 0;
  register rtx addr = x;

  if (REG_P (addr))
    fputs (reg_names[REGNO (addr)], file);
  else if (GET_CODE (addr) == PLUS)
    {
      if (CONST_INT_P (XEXP (addr, 0)))
	offset = INTVAL (XEXP (addr, 0)), base = XEXP (addr, 1);
      else if (CONST_INT_P (XEXP (addr, 1)))
	offset = INTVAL (XEXP (addr, 1)), base = XEXP (addr, 0);
      else
	base = XEXP (addr, 0), index = XEXP (addr, 1);
      if (GET_CODE (base) == LO_SUM)
	{
	  gcc_assert (USE_AS_OFFSETABLE_LO10
		      && TARGET_ARCH64
		      && ! TARGET_CM_MEDMID);
	  output_operand (XEXP (base, 0), 0);
	  fputs ("+%lo(", file);
	  output_address (VOIDmode, XEXP (base, 1));
	  fprintf (file, ")+%d", offset);
	}
      else
	{
	  fputs (reg_names[REGNO (base)], file);
	  if (index == 0)
	    fprintf (file, "%+d", offset);
	  else if (REG_P (index))
	    fprintf (file, "+%s", reg_names[REGNO (index)]);
	  else if (GET_CODE (index) == SYMBOL_REF
		   || GET_CODE (index) == LABEL_REF
		   || GET_CODE (index) == CONST)
	    fputc ('+', file), output_addr_const (file, index);
	  else gcc_unreachable ();
	}
    }
  else if (GET_CODE (addr) == MINUS
	   && GET_CODE (XEXP (addr, 1)) == LABEL_REF)
    {
      output_addr_const (file, XEXP (addr, 0));
      fputs ("-(", file);
      output_addr_const (file, XEXP (addr, 1));
      fputs ("-.)", file);
    }
  else if (GET_CODE (addr) == LO_SUM)
    {
      output_operand (XEXP (addr, 0), 0);
      if (TARGET_CM_MEDMID)
        fputs ("+%l44(", file);
      else
        fputs ("+%lo(", file);
      output_address (VOIDmode, XEXP (addr, 1));
      fputc (')', file);
    }
  else if (flag_pic
	   && GET_CODE (addr) == CONST
	   && GET_CODE (XEXP (addr, 0)) == MINUS
	   && GET_CODE (XEXP (XEXP (addr, 0), 1)) == CONST
	   && GET_CODE (XEXP (XEXP (XEXP (addr, 0), 1), 0)) == MINUS
	   && XEXP (XEXP (XEXP (XEXP (addr, 0), 1), 0), 1) == pc_rtx)
    {
      addr = XEXP (addr, 0);
      output_addr_const (file, XEXP (addr, 0));
      /* Group the args of the second CONST in parenthesis.  */
      fputs ("-(", file);
      /* Skip past the second CONST--it does nothing for us.  */
      output_addr_const (file, XEXP (XEXP (addr, 1), 0));
      /* Close the parenthesis.  */
      fputc (')', file);
    }
  else
    {
      output_addr_const (file, addr);
    }
}

/* Target hook for assembling integer objects.  The sparc version has
   special handling for aligned DI-mode objects.  */

static bool
sparc_assemble_integer (rtx x, unsigned int size, int aligned_p)
{
  /* ??? We only output .xword's for symbols and only then in environments
     where the assembler can handle them.  */
  if (aligned_p && size == 8 && GET_CODE (x) != CONST_INT)
    {
      if (TARGET_V9)
	{
	  assemble_integer_with_op ("\t.xword\t", x);
	  return true;
	}
      else
	{
	  assemble_aligned_integer (4, const0_rtx);
	  assemble_aligned_integer (4, x);
	  return true;
	}
    }
  return default_assemble_integer (x, size, aligned_p);
}

/* Return the value of a code used in the .proc pseudo-op that says
   what kind of result this function returns.  For non-C types, we pick
   the closest C type.  */

#ifndef SHORT_TYPE_SIZE
#define SHORT_TYPE_SIZE (BITS_PER_UNIT * 2)
#endif

#ifndef INT_TYPE_SIZE
#define INT_TYPE_SIZE BITS_PER_WORD
#endif

#ifndef LONG_TYPE_SIZE
#define LONG_TYPE_SIZE BITS_PER_WORD
#endif

#ifndef LONG_LONG_TYPE_SIZE
#define LONG_LONG_TYPE_SIZE (BITS_PER_WORD * 2)
#endif

#ifndef FLOAT_TYPE_SIZE
#define FLOAT_TYPE_SIZE BITS_PER_WORD
#endif

#ifndef DOUBLE_TYPE_SIZE
#define DOUBLE_TYPE_SIZE (BITS_PER_WORD * 2)
#endif

#ifndef LONG_DOUBLE_TYPE_SIZE
#define LONG_DOUBLE_TYPE_SIZE (BITS_PER_WORD * 2)
#endif

unsigned long
sparc_type_code (register tree type)
{
  register unsigned long qualifiers = 0;
  register unsigned shift;

  /* Only the first 30 bits of the qualifier are valid.  We must refrain from
     setting more, since some assemblers will give an error for this.  Also,
     we must be careful to avoid shifts of 32 bits or more to avoid getting
     unpredictable results.  */

  for (shift = 6; shift < 30; shift += 2, type = TREE_TYPE (type))
    {
      switch (TREE_CODE (type))
	{
	case ERROR_MARK:
	  return qualifiers;

	case ARRAY_TYPE:
	  qualifiers |= (3 << shift);
	  break;

	case FUNCTION_TYPE:
	case METHOD_TYPE:
	  qualifiers |= (2 << shift);
	  break;

	case POINTER_TYPE:
	case REFERENCE_TYPE:
	case OFFSET_TYPE:
	  qualifiers |= (1 << shift);
	  break;

	case RECORD_TYPE:
	  return (qualifiers | 8);

	case UNION_TYPE:
	case QUAL_UNION_TYPE:
	  return (qualifiers | 9);

	case ENUMERAL_TYPE:
	  return (qualifiers | 10);

	case VOID_TYPE:
	  return (qualifiers | 16);

	case INTEGER_TYPE:
	  /* If this is a range type, consider it to be the underlying
	     type.  */
	  if (TREE_TYPE (type) != 0)
	    break;

	  /* Carefully distinguish all the standard types of C,
	     without messing up if the language is not C.  We do this by
	     testing TYPE_PRECISION and TYPE_UNSIGNED.  The old code used to
	     look at both the names and the above fields, but that's redundant.
	     Any type whose size is between two C types will be considered
	     to be the wider of the two types.  Also, we do not have a
	     special code to use for "long long", so anything wider than
	     long is treated the same.  Note that we can't distinguish
	     between "int" and "long" in this code if they are the same
	     size, but that's fine, since neither can the assembler.  */

	  if (TYPE_PRECISION (type) <= CHAR_TYPE_SIZE)
	    return (qualifiers | (TYPE_UNSIGNED (type) ? 12 : 2));

	  else if (TYPE_PRECISION (type) <= SHORT_TYPE_SIZE)
	    return (qualifiers | (TYPE_UNSIGNED (type) ? 13 : 3));

	  else if (TYPE_PRECISION (type) <= INT_TYPE_SIZE)
	    return (qualifiers | (TYPE_UNSIGNED (type) ? 14 : 4));

	  else
	    return (qualifiers | (TYPE_UNSIGNED (type) ? 15 : 5));

	case REAL_TYPE:
	  /* If this is a range type, consider it to be the underlying
	     type.  */
	  if (TREE_TYPE (type) != 0)
	    break;

	  /* Carefully distinguish all the standard types of C,
	     without messing up if the language is not C.  */

	  if (TYPE_PRECISION (type) == FLOAT_TYPE_SIZE)
	    return (qualifiers | 6);

	  else
	    return (qualifiers | 7);

	case COMPLEX_TYPE:	/* GNU Fortran COMPLEX type.  */
	  /* ??? We need to distinguish between double and float complex types,
	     but I don't know how yet because I can't reach this code from
	     existing front-ends.  */
	  return (qualifiers | 7);	/* Who knows? */

	case VECTOR_TYPE:
	case BOOLEAN_TYPE:	/* Boolean truth value type.  */
	case LANG_TYPE:
	case NULLPTR_TYPE:
	  return qualifiers;

	default:
	  gcc_unreachable ();		/* Not a type! */
        }
    }

  return qualifiers;
}

/* Nested function support.  */

/* Emit RTL insns to initialize the variable parts of a trampoline.
   FNADDR is an RTX for the address of the function's pure code.
   CXT is an RTX for the static chain value for the function.

   This takes 16 insns: 2 shifts & 2 ands (to split up addresses), 4 sethi
   (to load in opcodes), 4 iors (to merge address and opcodes), and 4 writes
   (to store insns).  This is a bit excessive.  Perhaps a different
   mechanism would be better here.

   Emit enough FLUSH insns to synchronize the data and instruction caches.  */

static void
sparc32_initialize_trampoline (rtx m_tramp, rtx fnaddr, rtx cxt)
{
  /* SPARC 32-bit trampoline:

 	sethi	%hi(fn), %g1
 	sethi	%hi(static), %g2
 	jmp	%g1+%lo(fn)
 	or	%g2, %lo(static), %g2

    SETHI i,r  = 00rr rrr1 00ii iiii iiii iiii iiii iiii
    JMPL r+i,d = 10dd ddd1 1100 0rrr rr1i iiii iiii iiii
   */

  emit_move_insn
    (adjust_address (m_tramp, SImode, 0),
     expand_binop (SImode, ior_optab,
		   expand_shift (RSHIFT_EXPR, SImode, fnaddr, 10, 0, 1),
		   GEN_INT (trunc_int_for_mode (0x03000000, SImode)),
		   NULL_RTX, 1, OPTAB_DIRECT));

  emit_move_insn
    (adjust_address (m_tramp, SImode, 4),
     expand_binop (SImode, ior_optab,
		   expand_shift (RSHIFT_EXPR, SImode, cxt, 10, 0, 1),
		   GEN_INT (trunc_int_for_mode (0x05000000, SImode)),
		   NULL_RTX, 1, OPTAB_DIRECT));

  emit_move_insn
    (adjust_address (m_tramp, SImode, 8),
     expand_binop (SImode, ior_optab,
		   expand_and (SImode, fnaddr, GEN_INT (0x3ff), NULL_RTX),
		   GEN_INT (trunc_int_for_mode (0x81c06000, SImode)),
		   NULL_RTX, 1, OPTAB_DIRECT));

  emit_move_insn
    (adjust_address (m_tramp, SImode, 12),
     expand_binop (SImode, ior_optab,
		   expand_and (SImode, cxt, GEN_INT (0x3ff), NULL_RTX),
		   GEN_INT (trunc_int_for_mode (0x8410a000, SImode)),
		   NULL_RTX, 1, OPTAB_DIRECT));

  /* On UltraSPARC a flush flushes an entire cache line.  The trampoline is
     aligned on a 16 byte boundary so one flush clears it all.  */
  emit_insn (gen_flushsi (validize_mem (adjust_address (m_tramp, SImode, 0))));
  if (sparc_cpu != PROCESSOR_ULTRASPARC
      && sparc_cpu != PROCESSOR_ULTRASPARC3
      && sparc_cpu != PROCESSOR_NIAGARA
      && sparc_cpu != PROCESSOR_NIAGARA2
      && sparc_cpu != PROCESSOR_NIAGARA3
      && sparc_cpu != PROCESSOR_NIAGARA4
      && sparc_cpu != PROCESSOR_NIAGARA7
      && sparc_cpu != PROCESSOR_M8)
    emit_insn (gen_flushsi (validize_mem (adjust_address (m_tramp, SImode, 8))));

  /* Call __enable_execute_stack after writing onto the stack to make sure
     the stack address is accessible.  */
#ifdef HAVE_ENABLE_EXECUTE_STACK
  emit_library_call (gen_rtx_SYMBOL_REF (Pmode, "__enable_execute_stack"),
                     LCT_NORMAL, VOIDmode, XEXP (m_tramp, 0), Pmode);
#endif

}

/* The 64-bit version is simpler because it makes more sense to load the
   values as "immediate" data out of the trampoline.  It's also easier since
   we can read the PC without clobbering a register.  */

static void
sparc64_initialize_trampoline (rtx m_tramp, rtx fnaddr, rtx cxt)
{
  /* SPARC 64-bit trampoline:

	rd	%pc, %g1
	ldx	[%g1+24], %g5
	jmp	%g5
	ldx	[%g1+16], %g5
	+16 bytes data
   */

  emit_move_insn (adjust_address (m_tramp, SImode, 0),
		  GEN_INT (trunc_int_for_mode (0x83414000, SImode)));
  emit_move_insn (adjust_address (m_tramp, SImode, 4),
		  GEN_INT (trunc_int_for_mode (0xca586018, SImode)));
  emit_move_insn (adjust_address (m_tramp, SImode, 8),
		  GEN_INT (trunc_int_for_mode (0x81c14000, SImode)));
  emit_move_insn (adjust_address (m_tramp, SImode, 12),
		  GEN_INT (trunc_int_for_mode (0xca586010, SImode)));
  emit_move_insn (adjust_address (m_tramp, DImode, 16), cxt);
  emit_move_insn (adjust_address (m_tramp, DImode, 24), fnaddr);
  emit_insn (gen_flushdi (validize_mem (adjust_address (m_tramp, DImode, 0))));

  if (sparc_cpu != PROCESSOR_ULTRASPARC
      && sparc_cpu != PROCESSOR_ULTRASPARC3
      && sparc_cpu != PROCESSOR_NIAGARA
      && sparc_cpu != PROCESSOR_NIAGARA2
      && sparc_cpu != PROCESSOR_NIAGARA3
      && sparc_cpu != PROCESSOR_NIAGARA4
      && sparc_cpu != PROCESSOR_NIAGARA7
      && sparc_cpu != PROCESSOR_M8)
    emit_insn (gen_flushdi (validize_mem (adjust_address (m_tramp, DImode, 8))));

  /* Call __enable_execute_stack after writing onto the stack to make sure
     the stack address is accessible.  */
#ifdef HAVE_ENABLE_EXECUTE_STACK
  emit_library_call (gen_rtx_SYMBOL_REF (Pmode, "__enable_execute_stack"),
                     LCT_NORMAL, VOIDmode, XEXP (m_tramp, 0), Pmode);
#endif
}

/* Worker for TARGET_TRAMPOLINE_INIT.  */

static void
sparc_trampoline_init (rtx m_tramp, tree fndecl, rtx cxt)
{
  rtx fnaddr = force_reg (Pmode, XEXP (DECL_RTL (fndecl), 0));
  cxt = force_reg (Pmode, cxt);
  if (TARGET_ARCH64)
    sparc64_initialize_trampoline (m_tramp, fnaddr, cxt);
  else
    sparc32_initialize_trampoline (m_tramp, fnaddr, cxt);
}

/* Adjust the cost of a scheduling dependency.  Return the new cost of
   a dependency LINK or INSN on DEP_INSN.  COST is the current cost.  */

static int
supersparc_adjust_cost (rtx_insn *insn, int dep_type, rtx_insn *dep_insn,
			int cost)
{
  enum attr_type insn_type;

  if (recog_memoized (insn) < 0)
    return cost;

  insn_type = get_attr_type (insn);

  if (dep_type == 0)
    {
      /* Data dependency; DEP_INSN writes a register that INSN reads some
	 cycles later.  */

      /* if a load, then the dependence must be on the memory address;
	 add an extra "cycle".  Note that the cost could be two cycles
	 if the reg was written late in an instruction group; we ca not tell
	 here.  */
      if (insn_type == TYPE_LOAD || insn_type == TYPE_FPLOAD)
	return cost + 3;

      /* Get the delay only if the address of the store is the dependence.  */
      if (insn_type == TYPE_STORE || insn_type == TYPE_FPSTORE)
	{
	  rtx pat = PATTERN(insn);
	  rtx dep_pat = PATTERN (dep_insn);

	  if (GET_CODE (pat) != SET || GET_CODE (dep_pat) != SET)
	    return cost;  /* This should not happen!  */

	  /* The dependency between the two instructions was on the data that
	     is being stored.  Assume that this implies that the address of the
	     store is not dependent.  */
	  if (rtx_equal_p (SET_DEST (dep_pat), SET_SRC (pat)))
	    return cost;

	  return cost + 3;  /* An approximation.  */
	}

      /* A shift instruction cannot receive its data from an instruction
	 in the same cycle; add a one cycle penalty.  */
      if (insn_type == TYPE_SHIFT)
	return cost + 3;   /* Split before cascade into shift.  */
    }
  else
    {
      /* Anti- or output- dependency; DEP_INSN reads/writes a register that
	 INSN writes some cycles later.  */

      /* These are only significant for the fpu unit; writing a fp reg before
         the fpu has finished with it stalls the processor.  */

      /* Reusing an integer register causes no problems.  */
      if (insn_type == TYPE_IALU || insn_type == TYPE_SHIFT)
	return 0;
    }
	
  return cost;
}

static int
hypersparc_adjust_cost (rtx_insn *insn, int dtype, rtx_insn *dep_insn,
			int cost)
{
  enum attr_type insn_type, dep_type;
  rtx pat = PATTERN(insn);
  rtx dep_pat = PATTERN (dep_insn);

  if (recog_memoized (insn) < 0 || recog_memoized (dep_insn) < 0)
    return cost;

  insn_type = get_attr_type (insn);
  dep_type = get_attr_type (dep_insn);

  switch (dtype)
    {
    case 0:
      /* Data dependency; DEP_INSN writes a register that INSN reads some
	 cycles later.  */

      switch (insn_type)
	{
	case TYPE_STORE:
	case TYPE_FPSTORE:
	  /* Get the delay iff the address of the store is the dependence.  */
	  if (GET_CODE (pat) != SET || GET_CODE (dep_pat) != SET)
	    return cost;

	  if (rtx_equal_p (SET_DEST (dep_pat), SET_SRC (pat)))
	    return cost;
	  return cost + 3;

	case TYPE_LOAD:
	case TYPE_SLOAD:
	case TYPE_FPLOAD:
	  /* If a load, then the dependence must be on the memory address.  If
	     the addresses aren't equal, then it might be a false dependency */
	  if (dep_type == TYPE_STORE || dep_type == TYPE_FPSTORE)
	    {
	      if (GET_CODE (pat) != SET || GET_CODE (dep_pat) != SET
		  || GET_CODE (SET_DEST (dep_pat)) != MEM
		  || GET_CODE (SET_SRC (pat)) != MEM
		  || ! rtx_equal_p (XEXP (SET_DEST (dep_pat), 0),
				    XEXP (SET_SRC (pat), 0)))
		return cost + 2;

	      return cost + 8;
	    }
	  break;

	case TYPE_BRANCH:
	  /* Compare to branch latency is 0.  There is no benefit from
	     separating compare and branch.  */
	  if (dep_type == TYPE_COMPARE)
	    return 0;
	  /* Floating point compare to branch latency is less than
	     compare to conditional move.  */
	  if (dep_type == TYPE_FPCMP)
	    return cost - 1;
	  break;
	default:
	  break;
	}
	break;

    case REG_DEP_ANTI:
      /* Anti-dependencies only penalize the fpu unit.  */
      if (insn_type == TYPE_IALU || insn_type == TYPE_SHIFT)
        return 0;
      break;

    default:
      break;
    }

  return cost;
}

static int
sparc_adjust_cost (rtx_insn *insn, int dep_type, rtx_insn *dep, int cost,
		   unsigned int)
{
  switch (sparc_cpu)
    {
    case PROCESSOR_SUPERSPARC:
      cost = supersparc_adjust_cost (insn, dep_type, dep, cost);
      break;
    case PROCESSOR_HYPERSPARC:
    case PROCESSOR_SPARCLITE86X:
      cost = hypersparc_adjust_cost (insn, dep_type, dep, cost);
      break;
    default:
      break;
    }
  return cost;
}

static void
sparc_sched_init (FILE *dump ATTRIBUTE_UNUSED,
		  int sched_verbose ATTRIBUTE_UNUSED,
		  int max_ready ATTRIBUTE_UNUSED)
{}

static int
sparc_use_sched_lookahead (void)
{
  if (sparc_cpu == PROCESSOR_NIAGARA
      || sparc_cpu == PROCESSOR_NIAGARA2
      || sparc_cpu == PROCESSOR_NIAGARA3)
    return 0;
  if (sparc_cpu == PROCESSOR_NIAGARA4
      || sparc_cpu == PROCESSOR_NIAGARA7
      || sparc_cpu == PROCESSOR_M8)
    return 2;
  if (sparc_cpu == PROCESSOR_ULTRASPARC
      || sparc_cpu == PROCESSOR_ULTRASPARC3)
    return 4;
  if ((1 << sparc_cpu) &
      ((1 << PROCESSOR_SUPERSPARC) | (1 << PROCESSOR_HYPERSPARC) |
       (1 << PROCESSOR_SPARCLITE86X)))
    return 3;
  return 0;
}

static int
sparc_issue_rate (void)
{
  switch (sparc_cpu)
    {
    case PROCESSOR_NIAGARA:
    case PROCESSOR_NIAGARA2:
    case PROCESSOR_NIAGARA3:
    default:
      return 1;
    case PROCESSOR_NIAGARA4:
    case PROCESSOR_NIAGARA7:
    case PROCESSOR_V9:
      /* Assume V9 processors are capable of at least dual-issue.  */
      return 2;
    case PROCESSOR_SUPERSPARC:
      return 3;
    case PROCESSOR_HYPERSPARC:
    case PROCESSOR_SPARCLITE86X:
      return 2;
    case PROCESSOR_ULTRASPARC:
    case PROCESSOR_ULTRASPARC3:
    case PROCESSOR_M8:
      return 4;
    }
}

static int
set_extends (rtx_insn *insn)
{
  register rtx pat = PATTERN (insn);

  switch (GET_CODE (SET_SRC (pat)))
    {
      /* Load and some shift instructions zero extend.  */
    case MEM:
    case ZERO_EXTEND:
      /* sethi clears the high bits */
    case HIGH:
      /* LO_SUM is used with sethi.  sethi cleared the high
	 bits and the values used with lo_sum are positive */
    case LO_SUM:
      /* Store flag stores 0 or 1 */
    case LT: case LTU:
    case GT: case GTU:
    case LE: case LEU:
    case GE: case GEU:
    case EQ:
    case NE:
      return 1;
    case AND:
      {
	rtx op0 = XEXP (SET_SRC (pat), 0);
	rtx op1 = XEXP (SET_SRC (pat), 1);
	if (GET_CODE (op1) == CONST_INT)
	  return INTVAL (op1) >= 0;
	if (GET_CODE (op0) != REG)
	  return 0;
	if (sparc_check_64 (op0, insn) == 1)
	  return 1;
	return (GET_CODE (op1) == REG && sparc_check_64 (op1, insn) == 1);
      }
    case IOR:
    case XOR:
      {
	rtx op0 = XEXP (SET_SRC (pat), 0);
	rtx op1 = XEXP (SET_SRC (pat), 1);
	if (GET_CODE (op0) != REG || sparc_check_64 (op0, insn) <= 0)
	  return 0;
	if (GET_CODE (op1) == CONST_INT)
	  return INTVAL (op1) >= 0;
	return (GET_CODE (op1) == REG && sparc_check_64 (op1, insn) == 1);
      }
    case LSHIFTRT:
      return GET_MODE (SET_SRC (pat)) == SImode;
      /* Positive integers leave the high bits zero.  */
    case CONST_INT:
      return !(INTVAL (SET_SRC (pat)) & 0x80000000);
    case ASHIFTRT:
    case SIGN_EXTEND:
      return - (GET_MODE (SET_SRC (pat)) == SImode);
    case REG:
      return sparc_check_64 (SET_SRC (pat), insn);
    default:
      return 0;
    }
}

/* We _ought_ to have only one kind per function, but...  */
static GTY(()) rtx sparc_addr_diff_list;
static GTY(()) rtx sparc_addr_list;

void
sparc_defer_case_vector (rtx lab, rtx vec, int diff)
{
  vec = gen_rtx_EXPR_LIST (VOIDmode, lab, vec);
  if (diff)
    sparc_addr_diff_list
      = gen_rtx_EXPR_LIST (VOIDmode, vec, sparc_addr_diff_list);
  else
    sparc_addr_list = gen_rtx_EXPR_LIST (VOIDmode, vec, sparc_addr_list);
}

static void
sparc_output_addr_vec (rtx vec)
{
  rtx lab = XEXP (vec, 0), body = XEXP (vec, 1);
  int idx, vlen = XVECLEN (body, 0);

#ifdef ASM_OUTPUT_ADDR_VEC_START
  ASM_OUTPUT_ADDR_VEC_START (asm_out_file);
#endif

#ifdef ASM_OUTPUT_CASE_LABEL
  ASM_OUTPUT_CASE_LABEL (asm_out_file, "L", CODE_LABEL_NUMBER (lab),
			 NEXT_INSN (lab));
#else
  (*targetm.asm_out.internal_label) (asm_out_file, "L", CODE_LABEL_NUMBER (lab));
#endif

  for (idx = 0; idx < vlen; idx++)
    {
      ASM_OUTPUT_ADDR_VEC_ELT
	(asm_out_file, CODE_LABEL_NUMBER (XEXP (XVECEXP (body, 0, idx), 0)));
    }

#ifdef ASM_OUTPUT_ADDR_VEC_END
  ASM_OUTPUT_ADDR_VEC_END (asm_out_file);
#endif
}

static void
sparc_output_addr_diff_vec (rtx vec)
{
  rtx lab = XEXP (vec, 0), body = XEXP (vec, 1);
  rtx base = XEXP (XEXP (body, 0), 0);
  int idx, vlen = XVECLEN (body, 1);

#ifdef ASM_OUTPUT_ADDR_VEC_START
  ASM_OUTPUT_ADDR_VEC_START (asm_out_file);
#endif

#ifdef ASM_OUTPUT_CASE_LABEL
  ASM_OUTPUT_CASE_LABEL (asm_out_file, "L", CODE_LABEL_NUMBER (lab),
			 NEXT_INSN (lab));
#else
  (*targetm.asm_out.internal_label) (asm_out_file, "L", CODE_LABEL_NUMBER (lab));
#endif

  for (idx = 0; idx < vlen; idx++)
    {
      ASM_OUTPUT_ADDR_DIFF_ELT
        (asm_out_file,
         body,
         CODE_LABEL_NUMBER (XEXP (XVECEXP (body, 1, idx), 0)),
         CODE_LABEL_NUMBER (base));
    }

#ifdef ASM_OUTPUT_ADDR_VEC_END
  ASM_OUTPUT_ADDR_VEC_END (asm_out_file);
#endif
}

static void
sparc_output_deferred_case_vectors (void)
{
  rtx t;
  int align;

  if (sparc_addr_list == NULL_RTX
      && sparc_addr_diff_list == NULL_RTX)
    return;

  /* Align to cache line in the function's code section.  */
  switch_to_section (current_function_section ());

  align = floor_log2 (FUNCTION_BOUNDARY / BITS_PER_UNIT);
  if (align > 0)
    ASM_OUTPUT_ALIGN (asm_out_file, align);

  for (t = sparc_addr_list; t ; t = XEXP (t, 1))
    sparc_output_addr_vec (XEXP (t, 0));
  for (t = sparc_addr_diff_list; t ; t = XEXP (t, 1))
    sparc_output_addr_diff_vec (XEXP (t, 0));

  sparc_addr_list = sparc_addr_diff_list = NULL_RTX;
}

/* Return 0 if the high 32 bits of X (the low word of X, if DImode) are
   unknown.  Return 1 if the high bits are zero, -1 if the register is
   sign extended.  */
int
sparc_check_64 (rtx x, rtx_insn *insn)
{
  /* If a register is set only once it is safe to ignore insns this
     code does not know how to handle.  The loop will either recognize
     the single set and return the correct value or fail to recognize
     it and return 0.  */
  int set_once = 0;
  rtx y = x;

  gcc_assert (GET_CODE (x) == REG);

  if (GET_MODE (x) == DImode)
    y = gen_rtx_REG (SImode, REGNO (x) + WORDS_BIG_ENDIAN);

  if (flag_expensive_optimizations
      && df && DF_REG_DEF_COUNT (REGNO (y)) == 1)
    set_once = 1;

  if (insn == 0)
    {
      if (set_once)
	insn = get_last_insn_anywhere ();
      else
	return 0;
    }

  while ((insn = PREV_INSN (insn)))
    {
      switch (GET_CODE (insn))
	{
	case JUMP_INSN:
	case NOTE:
	  break;
	case CODE_LABEL:
	case CALL_INSN:
	default:
	  if (! set_once)
	    return 0;
	  break;
	case INSN:
	  {
	    rtx pat = PATTERN (insn);
	    if (GET_CODE (pat) != SET)
	      return 0;
	    if (rtx_equal_p (x, SET_DEST (pat)))
	      return set_extends (insn);
	    if (y && rtx_equal_p (y, SET_DEST (pat)))
	      return set_extends (insn);
	    if (reg_overlap_mentioned_p (SET_DEST (pat), y))
	      return 0;
	  }
	}
    }
  return 0;
}

/* Output a wide shift instruction in V8+ mode.  INSN is the instruction,
   OPERANDS are its operands and OPCODE is the mnemonic to be used.  */

const char *
output_v8plus_shift (rtx_insn *insn, rtx *operands, const char *opcode)
{
  static char asm_code[60];

  /* The scratch register is only required when the destination
     register is not a 64-bit global or out register.  */
  if (which_alternative != 2)
    operands[3] = operands[0];

  /* We can only shift by constants <= 63. */
  if (GET_CODE (operands[2]) == CONST_INT)
    operands[2] = GEN_INT (INTVAL (operands[2]) & 0x3f);

  if (GET_CODE (operands[1]) == CONST_INT)
    {
      output_asm_insn ("mov\t%1, %3", operands);
    }
  else
    {
      output_asm_insn ("sllx\t%H1, 32, %3", operands);
      if (sparc_check_64 (operands[1], insn) <= 0)
	output_asm_insn ("srl\t%L1, 0, %L1", operands);
      output_asm_insn ("or\t%L1, %3, %3", operands);
    }

  strcpy (asm_code, opcode);

  if (which_alternative != 2)
    return strcat (asm_code, "\t%0, %2, %L0\n\tsrlx\t%L0, 32, %H0");
  else
    return
      strcat (asm_code, "\t%3, %2, %3\n\tsrlx\t%3, 32, %H0\n\tmov\t%3, %L0");
}

/* Output rtl to increment the profiler label LABELNO
   for profiling a function entry.  */

void
sparc_profile_hook (int labelno)
{
  char buf[32];
  rtx lab, fun;

  fun = gen_rtx_SYMBOL_REF (Pmode, MCOUNT_FUNCTION);
  if (NO_PROFILE_COUNTERS)
    {
      emit_library_call (fun, LCT_NORMAL, VOIDmode);
    }
  else
    {
      ASM_GENERATE_INTERNAL_LABEL (buf, "LP", labelno);
      lab = gen_rtx_SYMBOL_REF (Pmode, ggc_strdup (buf));
      emit_library_call (fun, LCT_NORMAL, VOIDmode, lab, Pmode);
    }
}

#ifdef TARGET_SOLARIS
/* Solaris implementation of TARGET_ASM_NAMED_SECTION.  */

static void
sparc_solaris_elf_asm_named_section (const char *name, unsigned int flags,
				     tree decl ATTRIBUTE_UNUSED)
{
  if (HAVE_COMDAT_GROUP && flags & SECTION_LINKONCE)
    {
      solaris_elf_asm_comdat_section (name, flags, decl);
      return;
    }

  fprintf (asm_out_file, "\t.section\t\"%s\"", name);

  if (!(flags & SECTION_DEBUG))
    fputs (",#alloc", asm_out_file);
  if (flags & SECTION_WRITE)
    fputs (",#write", asm_out_file);
  if (flags & SECTION_TLS)
    fputs (",#tls", asm_out_file);
  if (flags & SECTION_CODE)
    fputs (",#execinstr", asm_out_file);

  if (flags & SECTION_NOTYPE)
    ;
  else if (flags & SECTION_BSS)
    fputs (",#nobits", asm_out_file);
  else
    fputs (",#progbits", asm_out_file);

  fputc ('\n', asm_out_file);
}
#endif /* TARGET_SOLARIS */

/* We do not allow indirect calls to be optimized into sibling calls.

   We cannot use sibling calls when delayed branches are disabled
   because they will likely require the call delay slot to be filled.

   Also, on SPARC 32-bit we cannot emit a sibling call when the
   current function returns a structure.  This is because the "unimp
   after call" convention would cause the callee to return to the
   wrong place.  The generic code already disallows cases where the
   function being called returns a structure.

   It may seem strange how this last case could occur.  Usually there
   is code after the call which jumps to epilogue code which dumps the
   return value into the struct return area.  That ought to invalidate
   the sibling call right?  Well, in the C++ case we can end up passing
   the pointer to the struct return area to a constructor (which returns
   void) and then nothing else happens.  Such a sibling call would look
   valid without the added check here.

   VxWorks PIC PLT entries require the global pointer to be initialized
   on entry.  We therefore can't emit sibling calls to them.  */
static bool
sparc_function_ok_for_sibcall (tree decl, tree exp ATTRIBUTE_UNUSED)
{
  return (decl
	  && flag_delayed_branch
	  && (TARGET_ARCH64 || ! cfun->returns_struct)
	  && !(TARGET_VXWORKS_RTP
	       && flag_pic
	       && !targetm.binds_local_p (decl)));
}

/* libfunc renaming.  */

static void
sparc_init_libfuncs (void)
{
  if (TARGET_ARCH32)
    {
      /* Use the subroutines that Sun's library provides for integer
	 multiply and divide.  The `*' prevents an underscore from
	 being prepended by the compiler. .umul is a little faster
	 than .mul.  */
      set_optab_libfunc (smul_optab, SImode, "*.umul");
      set_optab_libfunc (sdiv_optab, SImode, "*.div");
      set_optab_libfunc (udiv_optab, SImode, "*.udiv");
      set_optab_libfunc (smod_optab, SImode, "*.rem");
      set_optab_libfunc (umod_optab, SImode, "*.urem");

      /* TFmode arithmetic.  These names are part of the SPARC 32bit ABI.  */
      set_optab_libfunc (add_optab, TFmode, "_Q_add");
      set_optab_libfunc (sub_optab, TFmode, "_Q_sub");
      set_optab_libfunc (neg_optab, TFmode, "_Q_neg");
      set_optab_libfunc (smul_optab, TFmode, "_Q_mul");
      set_optab_libfunc (sdiv_optab, TFmode, "_Q_div");

      /* We can define the TFmode sqrt optab only if TARGET_FPU.  This
	 is because with soft-float, the SFmode and DFmode sqrt
	 instructions will be absent, and the compiler will notice and
	 try to use the TFmode sqrt instruction for calls to the
	 builtin function sqrt, but this fails.  */
      if (TARGET_FPU)
	set_optab_libfunc (sqrt_optab, TFmode, "_Q_sqrt");

      set_optab_libfunc (eq_optab, TFmode, "_Q_feq");
      set_optab_libfunc (ne_optab, TFmode, "_Q_fne");
      set_optab_libfunc (gt_optab, TFmode, "_Q_fgt");
      set_optab_libfunc (ge_optab, TFmode, "_Q_fge");
      set_optab_libfunc (lt_optab, TFmode, "_Q_flt");
      set_optab_libfunc (le_optab, TFmode, "_Q_fle");

      set_conv_libfunc (sext_optab,   TFmode, SFmode, "_Q_stoq");
      set_conv_libfunc (sext_optab,   TFmode, DFmode, "_Q_dtoq");
      set_conv_libfunc (trunc_optab,  SFmode, TFmode, "_Q_qtos");
      set_conv_libfunc (trunc_optab,  DFmode, TFmode, "_Q_qtod");

      set_conv_libfunc (sfix_optab,   SImode, TFmode, "_Q_qtoi");
      set_conv_libfunc (ufix_optab,   SImode, TFmode, "_Q_qtou");
      set_conv_libfunc (sfloat_optab, TFmode, SImode, "_Q_itoq");
      set_conv_libfunc (ufloat_optab, TFmode, SImode, "_Q_utoq");

      if (DITF_CONVERSION_LIBFUNCS)
	{
	  set_conv_libfunc (sfix_optab,   DImode, TFmode, "_Q_qtoll");
	  set_conv_libfunc (ufix_optab,   DImode, TFmode, "_Q_qtoull");
	  set_conv_libfunc (sfloat_optab, TFmode, DImode, "_Q_lltoq");
	  set_conv_libfunc (ufloat_optab, TFmode, DImode, "_Q_ulltoq");
	}

      if (SUN_CONVERSION_LIBFUNCS)
	{
	  set_conv_libfunc (sfix_optab, DImode, SFmode, "__ftoll");
	  set_conv_libfunc (ufix_optab, DImode, SFmode, "__ftoull");
	  set_conv_libfunc (sfix_optab, DImode, DFmode, "__dtoll");
	  set_conv_libfunc (ufix_optab, DImode, DFmode, "__dtoull");
	}
    }
  if (TARGET_ARCH64)
    {
      /* In the SPARC 64bit ABI, SImode multiply and divide functions
	 do not exist in the library.  Make sure the compiler does not
	 emit calls to them by accident.  (It should always use the
         hardware instructions.)  */
      set_optab_libfunc (smul_optab, SImode, 0);
      set_optab_libfunc (sdiv_optab, SImode, 0);
      set_optab_libfunc (udiv_optab, SImode, 0);
      set_optab_libfunc (smod_optab, SImode, 0);
      set_optab_libfunc (umod_optab, SImode, 0);

      if (SUN_INTEGER_MULTIPLY_64)
	{
	  set_optab_libfunc (smul_optab, DImode, "__mul64");
	  set_optab_libfunc (sdiv_optab, DImode, "__div64");
	  set_optab_libfunc (udiv_optab, DImode, "__udiv64");
	  set_optab_libfunc (smod_optab, DImode, "__rem64");
	  set_optab_libfunc (umod_optab, DImode, "__urem64");
	}

      if (SUN_CONVERSION_LIBFUNCS)
	{
	  set_conv_libfunc (sfix_optab, DImode, SFmode, "__ftol");
	  set_conv_libfunc (ufix_optab, DImode, SFmode, "__ftoul");
	  set_conv_libfunc (sfix_optab, DImode, DFmode, "__dtol");
	  set_conv_libfunc (ufix_optab, DImode, DFmode, "__dtoul");
	}
    }
}

/* SPARC builtins.  */
enum sparc_builtins
{
  /* FPU builtins.  */
  SPARC_BUILTIN_LDFSR,
  SPARC_BUILTIN_STFSR,

  /* VIS 1.0 builtins.  */
  SPARC_BUILTIN_FPACK16,
  SPARC_BUILTIN_FPACK32,
  SPARC_BUILTIN_FPACKFIX,
  SPARC_BUILTIN_FEXPAND,
  SPARC_BUILTIN_FPMERGE,
  SPARC_BUILTIN_FMUL8X16,
  SPARC_BUILTIN_FMUL8X16AU,
  SPARC_BUILTIN_FMUL8X16AL,
  SPARC_BUILTIN_FMUL8SUX16,
  SPARC_BUILTIN_FMUL8ULX16,
  SPARC_BUILTIN_FMULD8SUX16,
  SPARC_BUILTIN_FMULD8ULX16,
  SPARC_BUILTIN_FALIGNDATAV4HI,
  SPARC_BUILTIN_FALIGNDATAV8QI,
  SPARC_BUILTIN_FALIGNDATAV2SI,
  SPARC_BUILTIN_FALIGNDATADI,
  SPARC_BUILTIN_WRGSR,
  SPARC_BUILTIN_RDGSR,
  SPARC_BUILTIN_ALIGNADDR,
  SPARC_BUILTIN_ALIGNADDRL,
  SPARC_BUILTIN_PDIST,
  SPARC_BUILTIN_EDGE8,
  SPARC_BUILTIN_EDGE8L,
  SPARC_BUILTIN_EDGE16,
  SPARC_BUILTIN_EDGE16L,
  SPARC_BUILTIN_EDGE32,
  SPARC_BUILTIN_EDGE32L,
  SPARC_BUILTIN_FCMPLE16,
  SPARC_BUILTIN_FCMPLE32,
  SPARC_BUILTIN_FCMPNE16,
  SPARC_BUILTIN_FCMPNE32,
  SPARC_BUILTIN_FCMPGT16,
  SPARC_BUILTIN_FCMPGT32,
  SPARC_BUILTIN_FCMPEQ16,
  SPARC_BUILTIN_FCMPEQ32,
  SPARC_BUILTIN_FPADD16,
  SPARC_BUILTIN_FPADD16S,
  SPARC_BUILTIN_FPADD32,
  SPARC_BUILTIN_FPADD32S,
  SPARC_BUILTIN_FPSUB16,
  SPARC_BUILTIN_FPSUB16S,
  SPARC_BUILTIN_FPSUB32,
  SPARC_BUILTIN_FPSUB32S,
  SPARC_BUILTIN_ARRAY8,
  SPARC_BUILTIN_ARRAY16,
  SPARC_BUILTIN_ARRAY32,

  /* VIS 2.0 builtins.  */
  SPARC_BUILTIN_EDGE8N,
  SPARC_BUILTIN_EDGE8LN,
  SPARC_BUILTIN_EDGE16N,
  SPARC_BUILTIN_EDGE16LN,
  SPARC_BUILTIN_EDGE32N,
  SPARC_BUILTIN_EDGE32LN,
  SPARC_BUILTIN_BMASK,
  SPARC_BUILTIN_BSHUFFLEV4HI,
  SPARC_BUILTIN_BSHUFFLEV8QI,
  SPARC_BUILTIN_BSHUFFLEV2SI,
  SPARC_BUILTIN_BSHUFFLEDI,

  /* VIS 3.0 builtins.  */
  SPARC_BUILTIN_CMASK8,
  SPARC_BUILTIN_CMASK16,
  SPARC_BUILTIN_CMASK32,
  SPARC_BUILTIN_FCHKSM16,
  SPARC_BUILTIN_FSLL16,
  SPARC_BUILTIN_FSLAS16,
  SPARC_BUILTIN_FSRL16,
  SPARC_BUILTIN_FSRA16,
  SPARC_BUILTIN_FSLL32,
  SPARC_BUILTIN_FSLAS32,
  SPARC_BUILTIN_FSRL32,
  SPARC_BUILTIN_FSRA32,
  SPARC_BUILTIN_PDISTN,
  SPARC_BUILTIN_FMEAN16,
  SPARC_BUILTIN_FPADD64,
  SPARC_BUILTIN_FPSUB64,
  SPARC_BUILTIN_FPADDS16,
  SPARC_BUILTIN_FPADDS16S,
  SPARC_BUILTIN_FPSUBS16,
  SPARC_BUILTIN_FPSUBS16S,
  SPARC_BUILTIN_FPADDS32,
  SPARC_BUILTIN_FPADDS32S,
  SPARC_BUILTIN_FPSUBS32,
  SPARC_BUILTIN_FPSUBS32S,
  SPARC_BUILTIN_FUCMPLE8,
  SPARC_BUILTIN_FUCMPNE8,
  SPARC_BUILTIN_FUCMPGT8,
  SPARC_BUILTIN_FUCMPEQ8,
  SPARC_BUILTIN_FHADDS,
  SPARC_BUILTIN_FHADDD,
  SPARC_BUILTIN_FHSUBS,
  SPARC_BUILTIN_FHSUBD,
  SPARC_BUILTIN_FNHADDS,
  SPARC_BUILTIN_FNHADDD,
  SPARC_BUILTIN_UMULXHI,
  SPARC_BUILTIN_XMULX,
  SPARC_BUILTIN_XMULXHI,

  /* VIS 4.0 builtins.  */
  SPARC_BUILTIN_FPADD8,
  SPARC_BUILTIN_FPADDS8,
  SPARC_BUILTIN_FPADDUS8,
  SPARC_BUILTIN_FPADDUS16,
  SPARC_BUILTIN_FPCMPLE8,
  SPARC_BUILTIN_FPCMPGT8,
  SPARC_BUILTIN_FPCMPULE16,
  SPARC_BUILTIN_FPCMPUGT16,
  SPARC_BUILTIN_FPCMPULE32,
  SPARC_BUILTIN_FPCMPUGT32,
  SPARC_BUILTIN_FPMAX8,
  SPARC_BUILTIN_FPMAX16,
  SPARC_BUILTIN_FPMAX32,
  SPARC_BUILTIN_FPMAXU8,
  SPARC_BUILTIN_FPMAXU16,
  SPARC_BUILTIN_FPMAXU32,
  SPARC_BUILTIN_FPMIN8,
  SPARC_BUILTIN_FPMIN16,
  SPARC_BUILTIN_FPMIN32,
  SPARC_BUILTIN_FPMINU8,
  SPARC_BUILTIN_FPMINU16,
  SPARC_BUILTIN_FPMINU32,
  SPARC_BUILTIN_FPSUB8,
  SPARC_BUILTIN_FPSUBS8,
  SPARC_BUILTIN_FPSUBUS8,
  SPARC_BUILTIN_FPSUBUS16,

  /* VIS 4.0B builtins.  */

  /* Note that all the DICTUNPACK* entries should be kept
     contiguous.  */
  SPARC_BUILTIN_FIRST_DICTUNPACK,
  SPARC_BUILTIN_DICTUNPACK8 = SPARC_BUILTIN_FIRST_DICTUNPACK,
  SPARC_BUILTIN_DICTUNPACK16,
  SPARC_BUILTIN_DICTUNPACK32,
  SPARC_BUILTIN_LAST_DICTUNPACK = SPARC_BUILTIN_DICTUNPACK32,

  /* Note that all the FPCMP*SHL entries should be kept
     contiguous.  */
  SPARC_BUILTIN_FIRST_FPCMPSHL,
  SPARC_BUILTIN_FPCMPLE8SHL = SPARC_BUILTIN_FIRST_FPCMPSHL,
  SPARC_BUILTIN_FPCMPGT8SHL,
  SPARC_BUILTIN_FPCMPEQ8SHL,
  SPARC_BUILTIN_FPCMPNE8SHL,
  SPARC_BUILTIN_FPCMPLE16SHL,
  SPARC_BUILTIN_FPCMPGT16SHL,
  SPARC_BUILTIN_FPCMPEQ16SHL,
  SPARC_BUILTIN_FPCMPNE16SHL,
  SPARC_BUILTIN_FPCMPLE32SHL,
  SPARC_BUILTIN_FPCMPGT32SHL,
  SPARC_BUILTIN_FPCMPEQ32SHL,
  SPARC_BUILTIN_FPCMPNE32SHL,
  SPARC_BUILTIN_FPCMPULE8SHL,
  SPARC_BUILTIN_FPCMPUGT8SHL,
  SPARC_BUILTIN_FPCMPULE16SHL,
  SPARC_BUILTIN_FPCMPUGT16SHL,
  SPARC_BUILTIN_FPCMPULE32SHL,
  SPARC_BUILTIN_FPCMPUGT32SHL,
  SPARC_BUILTIN_FPCMPDE8SHL,
  SPARC_BUILTIN_FPCMPDE16SHL,
  SPARC_BUILTIN_FPCMPDE32SHL,
  SPARC_BUILTIN_FPCMPUR8SHL,
  SPARC_BUILTIN_FPCMPUR16SHL,
  SPARC_BUILTIN_FPCMPUR32SHL,
  SPARC_BUILTIN_LAST_FPCMPSHL = SPARC_BUILTIN_FPCMPUR32SHL,
  
  SPARC_BUILTIN_MAX
};

static GTY (()) tree sparc_builtins[(int) SPARC_BUILTIN_MAX];
static enum insn_code sparc_builtins_icode[(int) SPARC_BUILTIN_MAX];

/* Return true if OPVAL can be used for operand OPNUM of instruction ICODE.
   The instruction should require a constant operand of some sort.  The
   function prints an error if OPVAL is not valid.  */

static int
check_constant_argument (enum insn_code icode, int opnum, rtx opval)
{
  if (GET_CODE (opval) != CONST_INT)
    {
      error ("%qs expects a constant argument", insn_data[icode].name);
      return false;
    }

  if (!(*insn_data[icode].operand[opnum].predicate) (opval, VOIDmode))
    {
      error ("constant argument out of range for %qs", insn_data[icode].name);
      return false;
    }
  return true;
}

/* Add a SPARC builtin function with NAME, ICODE, CODE and TYPE.  Return the
   function decl or NULL_TREE if the builtin was not added.  */

static tree
def_builtin (const char *name, enum insn_code icode, enum sparc_builtins code,
	     tree type)
{
  tree t
    = add_builtin_function (name, type, code, BUILT_IN_MD, NULL, NULL_TREE);

  if (t)
    {
      sparc_builtins[code] = t;
      sparc_builtins_icode[code] = icode;
    }

  return t;
}

/* Likewise, but also marks the function as "const".  */

static tree
def_builtin_const (const char *name, enum insn_code icode,
		   enum sparc_builtins code, tree type)
{
  tree t = def_builtin (name, icode, code, type);

  if (t)
    TREE_READONLY (t) = 1;

  return t;
}

/* Implement the TARGET_INIT_BUILTINS target hook.
   Create builtin functions for special SPARC instructions.  */

static void
sparc_init_builtins (void)
{
  if (TARGET_FPU)
    sparc_fpu_init_builtins ();

  if (TARGET_VIS)
    sparc_vis_init_builtins ();
}

/* Create builtin functions for FPU instructions.  */

static void
sparc_fpu_init_builtins (void)
{
  tree ftype
    = build_function_type_list (void_type_node,
				build_pointer_type (unsigned_type_node), 0);
  def_builtin ("__builtin_load_fsr", CODE_FOR_ldfsr,
	       SPARC_BUILTIN_LDFSR, ftype);
  def_builtin ("__builtin_store_fsr", CODE_FOR_stfsr,
	       SPARC_BUILTIN_STFSR, ftype);
}

/* Create builtin functions for VIS instructions.  */

static void
sparc_vis_init_builtins (void)
{
  tree v4qi = build_vector_type (unsigned_intQI_type_node, 4);
  tree v8qi = build_vector_type (unsigned_intQI_type_node, 8);
  tree v4hi = build_vector_type (intHI_type_node, 4);
  tree v2hi = build_vector_type (intHI_type_node, 2);
  tree v2si = build_vector_type (intSI_type_node, 2);
  tree v1si = build_vector_type (intSI_type_node, 1);

  tree v4qi_ftype_v4hi = build_function_type_list (v4qi, v4hi, 0);
  tree v8qi_ftype_v2si_v8qi = build_function_type_list (v8qi, v2si, v8qi, 0);
  tree v2hi_ftype_v2si = build_function_type_list (v2hi, v2si, 0);
  tree v4hi_ftype_v4qi = build_function_type_list (v4hi, v4qi, 0);
  tree v8qi_ftype_v4qi_v4qi = build_function_type_list (v8qi, v4qi, v4qi, 0);
  tree v4hi_ftype_v4qi_v4hi = build_function_type_list (v4hi, v4qi, v4hi, 0);
  tree v4hi_ftype_v4qi_v2hi = build_function_type_list (v4hi, v4qi, v2hi, 0);
  tree v2si_ftype_v4qi_v2hi = build_function_type_list (v2si, v4qi, v2hi, 0);
  tree v4hi_ftype_v8qi_v4hi = build_function_type_list (v4hi, v8qi, v4hi, 0);
  tree v4hi_ftype_v4hi_v4hi = build_function_type_list (v4hi, v4hi, v4hi, 0);
  tree v2si_ftype_v2si_v2si = build_function_type_list (v2si, v2si, v2si, 0);
  tree v8qi_ftype_v8qi_v8qi = build_function_type_list (v8qi, v8qi, v8qi, 0);
  tree v2hi_ftype_v2hi_v2hi = build_function_type_list (v2hi, v2hi, v2hi, 0);
  tree v1si_ftype_v1si_v1si = build_function_type_list (v1si, v1si, v1si, 0);
  tree di_ftype_v8qi_v8qi_di = build_function_type_list (intDI_type_node,
							 v8qi, v8qi,
							 intDI_type_node, 0);
  tree di_ftype_v8qi_v8qi = build_function_type_list (intDI_type_node,
						      v8qi, v8qi, 0);
  tree si_ftype_v8qi_v8qi = build_function_type_list (intSI_type_node,
						      v8qi, v8qi, 0);
  tree v8qi_ftype_df_si = build_function_type_list (v8qi, double_type_node,
						    intSI_type_node, 0);
  tree v4hi_ftype_df_si = build_function_type_list (v4hi, double_type_node,
						    intSI_type_node, 0);
  tree v2si_ftype_df_si = build_function_type_list (v2si, double_type_node,
						    intDI_type_node, 0);
  tree di_ftype_di_di = build_function_type_list (intDI_type_node,
						  intDI_type_node,
						  intDI_type_node, 0);
  tree si_ftype_si_si = build_function_type_list (intSI_type_node,
						  intSI_type_node,
						  intSI_type_node, 0);
  tree ptr_ftype_ptr_si = build_function_type_list (ptr_type_node,
		        			    ptr_type_node,
					            intSI_type_node, 0);
  tree ptr_ftype_ptr_di = build_function_type_list (ptr_type_node,
		        			    ptr_type_node,
					            intDI_type_node, 0);
  tree si_ftype_ptr_ptr = build_function_type_list (intSI_type_node,
		        			    ptr_type_node,
					            ptr_type_node, 0);
  tree di_ftype_ptr_ptr = build_function_type_list (intDI_type_node,
		        			    ptr_type_node,
					            ptr_type_node, 0);
  tree si_ftype_v4hi_v4hi = build_function_type_list (intSI_type_node,
						      v4hi, v4hi, 0);
  tree si_ftype_v2si_v2si = build_function_type_list (intSI_type_node,
						      v2si, v2si, 0);
  tree di_ftype_v4hi_v4hi = build_function_type_list (intDI_type_node,
						      v4hi, v4hi, 0);
  tree di_ftype_v2si_v2si = build_function_type_list (intDI_type_node,
						      v2si, v2si, 0);
  tree void_ftype_di = build_function_type_list (void_type_node,
						 intDI_type_node, 0);
  tree di_ftype_void = build_function_type_list (intDI_type_node,
						 void_type_node, 0);
  tree void_ftype_si = build_function_type_list (void_type_node,
						 intSI_type_node, 0);
  tree sf_ftype_sf_sf = build_function_type_list (float_type_node,
						  float_type_node,
						  float_type_node, 0);
  tree df_ftype_df_df = build_function_type_list (double_type_node,
						  double_type_node,
						  double_type_node, 0);

  /* Packing and expanding vectors.  */
  def_builtin ("__builtin_vis_fpack16", CODE_FOR_fpack16_vis,
	       SPARC_BUILTIN_FPACK16, v4qi_ftype_v4hi);
  def_builtin ("__builtin_vis_fpack32", CODE_FOR_fpack32_vis,
	       SPARC_BUILTIN_FPACK32, v8qi_ftype_v2si_v8qi);
  def_builtin ("__builtin_vis_fpackfix", CODE_FOR_fpackfix_vis,
	       SPARC_BUILTIN_FPACKFIX, v2hi_ftype_v2si);
  def_builtin_const ("__builtin_vis_fexpand", CODE_FOR_fexpand_vis,
		     SPARC_BUILTIN_FEXPAND, v4hi_ftype_v4qi);
  def_builtin_const ("__builtin_vis_fpmerge", CODE_FOR_fpmerge_vis,
		     SPARC_BUILTIN_FPMERGE, v8qi_ftype_v4qi_v4qi);

  /* Multiplications.  */
  def_builtin_const ("__builtin_vis_fmul8x16", CODE_FOR_fmul8x16_vis,
		     SPARC_BUILTIN_FMUL8X16, v4hi_ftype_v4qi_v4hi);
  def_builtin_const ("__builtin_vis_fmul8x16au", CODE_FOR_fmul8x16au_vis,
		     SPARC_BUILTIN_FMUL8X16AU, v4hi_ftype_v4qi_v2hi);
  def_builtin_const ("__builtin_vis_fmul8x16al", CODE_FOR_fmul8x16al_vis,
		     SPARC_BUILTIN_FMUL8X16AL, v4hi_ftype_v4qi_v2hi);
  def_builtin_const ("__builtin_vis_fmul8sux16", CODE_FOR_fmul8sux16_vis,
		     SPARC_BUILTIN_FMUL8SUX16, v4hi_ftype_v8qi_v4hi);
  def_builtin_const ("__builtin_vis_fmul8ulx16", CODE_FOR_fmul8ulx16_vis,
		     SPARC_BUILTIN_FMUL8ULX16, v4hi_ftype_v8qi_v4hi);
  def_builtin_const ("__builtin_vis_fmuld8sux16", CODE_FOR_fmuld8sux16_vis,
		     SPARC_BUILTIN_FMULD8SUX16, v2si_ftype_v4qi_v2hi);
  def_builtin_const ("__builtin_vis_fmuld8ulx16", CODE_FOR_fmuld8ulx16_vis,
		     SPARC_BUILTIN_FMULD8ULX16, v2si_ftype_v4qi_v2hi);

  /* Data aligning.  */
  def_builtin ("__builtin_vis_faligndatav4hi", CODE_FOR_faligndatav4hi_vis,
	       SPARC_BUILTIN_FALIGNDATAV4HI, v4hi_ftype_v4hi_v4hi);
  def_builtin ("__builtin_vis_faligndatav8qi", CODE_FOR_faligndatav8qi_vis,
	       SPARC_BUILTIN_FALIGNDATAV8QI, v8qi_ftype_v8qi_v8qi);
  def_builtin ("__builtin_vis_faligndatav2si", CODE_FOR_faligndatav2si_vis,
	       SPARC_BUILTIN_FALIGNDATAV2SI, v2si_ftype_v2si_v2si);
  def_builtin ("__builtin_vis_faligndatadi", CODE_FOR_faligndatav1di_vis,
	       SPARC_BUILTIN_FALIGNDATADI, di_ftype_di_di);

  def_builtin ("__builtin_vis_write_gsr", CODE_FOR_wrgsr_vis,
	       SPARC_BUILTIN_WRGSR, void_ftype_di);
  def_builtin ("__builtin_vis_read_gsr", CODE_FOR_rdgsr_vis,
	       SPARC_BUILTIN_RDGSR, di_ftype_void);

  if (TARGET_ARCH64)
    {
      def_builtin ("__builtin_vis_alignaddr", CODE_FOR_alignaddrdi_vis,
		   SPARC_BUILTIN_ALIGNADDR, ptr_ftype_ptr_di);
      def_builtin ("__builtin_vis_alignaddrl", CODE_FOR_alignaddrldi_vis,
		   SPARC_BUILTIN_ALIGNADDRL, ptr_ftype_ptr_di);
    }
  else
    {
      def_builtin ("__builtin_vis_alignaddr", CODE_FOR_alignaddrsi_vis,
		   SPARC_BUILTIN_ALIGNADDR, ptr_ftype_ptr_si);
      def_builtin ("__builtin_vis_alignaddrl", CODE_FOR_alignaddrlsi_vis,
		   SPARC_BUILTIN_ALIGNADDRL, ptr_ftype_ptr_si);
    }

  /* Pixel distance.  */
  def_builtin_const ("__builtin_vis_pdist", CODE_FOR_pdist_vis,
		     SPARC_BUILTIN_PDIST, di_ftype_v8qi_v8qi_di);

  /* Edge handling.  */
  if (TARGET_ARCH64)
    {
      def_builtin_const ("__builtin_vis_edge8", CODE_FOR_edge8di_vis,
			 SPARC_BUILTIN_EDGE8, di_ftype_ptr_ptr);
      def_builtin_const ("__builtin_vis_edge8l", CODE_FOR_edge8ldi_vis,
			 SPARC_BUILTIN_EDGE8L, di_ftype_ptr_ptr);
      def_builtin_const ("__builtin_vis_edge16", CODE_FOR_edge16di_vis,
			 SPARC_BUILTIN_EDGE16, di_ftype_ptr_ptr);
      def_builtin_const ("__builtin_vis_edge16l", CODE_FOR_edge16ldi_vis,
			 SPARC_BUILTIN_EDGE16L, di_ftype_ptr_ptr);
      def_builtin_const ("__builtin_vis_edge32", CODE_FOR_edge32di_vis,
			 SPARC_BUILTIN_EDGE32, di_ftype_ptr_ptr);
      def_builtin_const ("__builtin_vis_edge32l", CODE_FOR_edge32ldi_vis,
			 SPARC_BUILTIN_EDGE32L, di_ftype_ptr_ptr);
    }
  else
    {
      def_builtin_const ("__builtin_vis_edge8", CODE_FOR_edge8si_vis,
			 SPARC_BUILTIN_EDGE8, si_ftype_ptr_ptr);
      def_builtin_const ("__builtin_vis_edge8l", CODE_FOR_edge8lsi_vis,
			 SPARC_BUILTIN_EDGE8L, si_ftype_ptr_ptr);
      def_builtin_const ("__builtin_vis_edge16", CODE_FOR_edge16si_vis,
			 SPARC_BUILTIN_EDGE16, si_ftype_ptr_ptr);
      def_builtin_const ("__builtin_vis_edge16l", CODE_FOR_edge16lsi_vis,
			 SPARC_BUILTIN_EDGE16L, si_ftype_ptr_ptr);
      def_builtin_const ("__builtin_vis_edge32", CODE_FOR_edge32si_vis,
			 SPARC_BUILTIN_EDGE32, si_ftype_ptr_ptr);
      def_builtin_const ("__builtin_vis_edge32l", CODE_FOR_edge32lsi_vis,
			 SPARC_BUILTIN_EDGE32L, si_ftype_ptr_ptr);
    }

  /* Pixel compare.  */
  if (TARGET_ARCH64)
    {
      def_builtin_const ("__builtin_vis_fcmple16", CODE_FOR_fcmple16di_vis,
			 SPARC_BUILTIN_FCMPLE16, di_ftype_v4hi_v4hi);
      def_builtin_const ("__builtin_vis_fcmple32", CODE_FOR_fcmple32di_vis,
			 SPARC_BUILTIN_FCMPLE32, di_ftype_v2si_v2si);
      def_builtin_const ("__builtin_vis_fcmpne16", CODE_FOR_fcmpne16di_vis,
			 SPARC_BUILTIN_FCMPNE16, di_ftype_v4hi_v4hi);
      def_builtin_const ("__builtin_vis_fcmpne32", CODE_FOR_fcmpne32di_vis,
			 SPARC_BUILTIN_FCMPNE32, di_ftype_v2si_v2si);
      def_builtin_const ("__builtin_vis_fcmpgt16", CODE_FOR_fcmpgt16di_vis,
			 SPARC_BUILTIN_FCMPGT16, di_ftype_v4hi_v4hi);
      def_builtin_const ("__builtin_vis_fcmpgt32", CODE_FOR_fcmpgt32di_vis,
			 SPARC_BUILTIN_FCMPGT32, di_ftype_v2si_v2si);
      def_builtin_const ("__builtin_vis_fcmpeq16", CODE_FOR_fcmpeq16di_vis,
			 SPARC_BUILTIN_FCMPEQ16, di_ftype_v4hi_v4hi);
      def_builtin_const ("__builtin_vis_fcmpeq32", CODE_FOR_fcmpeq32di_vis,
			 SPARC_BUILTIN_FCMPEQ32, di_ftype_v2si_v2si);
    }
  else
    {
      def_builtin_const ("__builtin_vis_fcmple16", CODE_FOR_fcmple16si_vis,
			 SPARC_BUILTIN_FCMPLE16, si_ftype_v4hi_v4hi);
      def_builtin_const ("__builtin_vis_fcmple32", CODE_FOR_fcmple32si_vis,
			 SPARC_BUILTIN_FCMPLE32, si_ftype_v2si_v2si);
      def_builtin_const ("__builtin_vis_fcmpne16", CODE_FOR_fcmpne16si_vis,
			 SPARC_BUILTIN_FCMPNE16, si_ftype_v4hi_v4hi);
      def_builtin_const ("__builtin_vis_fcmpne32", CODE_FOR_fcmpne32si_vis,
			 SPARC_BUILTIN_FCMPNE32, si_ftype_v2si_v2si);
      def_builtin_const ("__builtin_vis_fcmpgt16", CODE_FOR_fcmpgt16si_vis,
			 SPARC_BUILTIN_FCMPGT16, si_ftype_v4hi_v4hi);
      def_builtin_const ("__builtin_vis_fcmpgt32", CODE_FOR_fcmpgt32si_vis,
			 SPARC_BUILTIN_FCMPGT32, si_ftype_v2si_v2si);
      def_builtin_const ("__builtin_vis_fcmpeq16", CODE_FOR_fcmpeq16si_vis,
			 SPARC_BUILTIN_FCMPEQ16, si_ftype_v4hi_v4hi);
      def_builtin_const ("__builtin_vis_fcmpeq32", CODE_FOR_fcmpeq32si_vis,
			 SPARC_BUILTIN_FCMPEQ32, si_ftype_v2si_v2si);
    }

  /* Addition and subtraction.  */
  def_builtin_const ("__builtin_vis_fpadd16", CODE_FOR_addv4hi3,
		     SPARC_BUILTIN_FPADD16, v4hi_ftype_v4hi_v4hi);
  def_builtin_const ("__builtin_vis_fpadd16s", CODE_FOR_addv2hi3,
		     SPARC_BUILTIN_FPADD16S, v2hi_ftype_v2hi_v2hi);
  def_builtin_const ("__builtin_vis_fpadd32", CODE_FOR_addv2si3,
		     SPARC_BUILTIN_FPADD32, v2si_ftype_v2si_v2si);
  def_builtin_const ("__builtin_vis_fpadd32s", CODE_FOR_addv1si3,
		     SPARC_BUILTIN_FPADD32S, v1si_ftype_v1si_v1si);
  def_builtin_const ("__builtin_vis_fpsub16", CODE_FOR_subv4hi3,
		     SPARC_BUILTIN_FPSUB16, v4hi_ftype_v4hi_v4hi);
  def_builtin_const ("__builtin_vis_fpsub16s", CODE_FOR_subv2hi3,
		     SPARC_BUILTIN_FPSUB16S, v2hi_ftype_v2hi_v2hi);
  def_builtin_const ("__builtin_vis_fpsub32", CODE_FOR_subv2si3,
		     SPARC_BUILTIN_FPSUB32, v2si_ftype_v2si_v2si);
  def_builtin_const ("__builtin_vis_fpsub32s", CODE_FOR_subv1si3,
		     SPARC_BUILTIN_FPSUB32S, v1si_ftype_v1si_v1si);

  /* Three-dimensional array addressing.  */
  if (TARGET_ARCH64)
    {
      def_builtin_const ("__builtin_vis_array8", CODE_FOR_array8di_vis,
			 SPARC_BUILTIN_ARRAY8, di_ftype_di_di);
      def_builtin_const ("__builtin_vis_array16", CODE_FOR_array16di_vis,
			 SPARC_BUILTIN_ARRAY16, di_ftype_di_di);
      def_builtin_const ("__builtin_vis_array32", CODE_FOR_array32di_vis,
			 SPARC_BUILTIN_ARRAY32, di_ftype_di_di);
    }
  else
    {
      def_builtin_const ("__builtin_vis_array8", CODE_FOR_array8si_vis,
			 SPARC_BUILTIN_ARRAY8, si_ftype_si_si);
      def_builtin_const ("__builtin_vis_array16", CODE_FOR_array16si_vis,
			 SPARC_BUILTIN_ARRAY16, si_ftype_si_si);
      def_builtin_const ("__builtin_vis_array32", CODE_FOR_array32si_vis,
			 SPARC_BUILTIN_ARRAY32, si_ftype_si_si);
    }

  if (TARGET_VIS2)
    {
      /* Edge handling.  */
      if (TARGET_ARCH64)
	{
	  def_builtin_const ("__builtin_vis_edge8n", CODE_FOR_edge8ndi_vis,
			     SPARC_BUILTIN_EDGE8N, di_ftype_ptr_ptr);
	  def_builtin_const ("__builtin_vis_edge8ln", CODE_FOR_edge8lndi_vis,
			     SPARC_BUILTIN_EDGE8LN, di_ftype_ptr_ptr);
	  def_builtin_const ("__builtin_vis_edge16n", CODE_FOR_edge16ndi_vis,
			     SPARC_BUILTIN_EDGE16N, di_ftype_ptr_ptr);
	  def_builtin_const ("__builtin_vis_edge16ln", CODE_FOR_edge16lndi_vis,
			     SPARC_BUILTIN_EDGE16LN, di_ftype_ptr_ptr);
	  def_builtin_const ("__builtin_vis_edge32n", CODE_FOR_edge32ndi_vis,
			     SPARC_BUILTIN_EDGE32N, di_ftype_ptr_ptr);
	  def_builtin_const ("__builtin_vis_edge32ln", CODE_FOR_edge32lndi_vis,
			     SPARC_BUILTIN_EDGE32LN, di_ftype_ptr_ptr);
	}
      else
	{
	  def_builtin_const ("__builtin_vis_edge8n", CODE_FOR_edge8nsi_vis,
			     SPARC_BUILTIN_EDGE8N, si_ftype_ptr_ptr);
	  def_builtin_const ("__builtin_vis_edge8ln", CODE_FOR_edge8lnsi_vis,
			     SPARC_BUILTIN_EDGE8LN, si_ftype_ptr_ptr);
	  def_builtin_const ("__builtin_vis_edge16n", CODE_FOR_edge16nsi_vis,
			     SPARC_BUILTIN_EDGE16N, si_ftype_ptr_ptr);
	  def_builtin_const ("__builtin_vis_edge16ln", CODE_FOR_edge16lnsi_vis,
			     SPARC_BUILTIN_EDGE16LN, si_ftype_ptr_ptr);
	  def_builtin_const ("__builtin_vis_edge32n", CODE_FOR_edge32nsi_vis,
			     SPARC_BUILTIN_EDGE32N, si_ftype_ptr_ptr);
	  def_builtin_const ("__builtin_vis_edge32ln", CODE_FOR_edge32lnsi_vis,
			     SPARC_BUILTIN_EDGE32LN, si_ftype_ptr_ptr);
	}

      /* Byte mask and shuffle.  */
      if (TARGET_ARCH64)
	def_builtin ("__builtin_vis_bmask", CODE_FOR_bmaskdi_vis,
		     SPARC_BUILTIN_BMASK, di_ftype_di_di);
      else
	def_builtin ("__builtin_vis_bmask", CODE_FOR_bmasksi_vis,
		     SPARC_BUILTIN_BMASK, si_ftype_si_si);
      def_builtin ("__builtin_vis_bshufflev4hi", CODE_FOR_bshufflev4hi_vis,
		   SPARC_BUILTIN_BSHUFFLEV4HI, v4hi_ftype_v4hi_v4hi);
      def_builtin ("__builtin_vis_bshufflev8qi", CODE_FOR_bshufflev8qi_vis,
		   SPARC_BUILTIN_BSHUFFLEV8QI, v8qi_ftype_v8qi_v8qi);
      def_builtin ("__builtin_vis_bshufflev2si", CODE_FOR_bshufflev2si_vis,
		   SPARC_BUILTIN_BSHUFFLEV2SI, v2si_ftype_v2si_v2si);
      def_builtin ("__builtin_vis_bshuffledi", CODE_FOR_bshufflev1di_vis,
		   SPARC_BUILTIN_BSHUFFLEDI, di_ftype_di_di);
    }

  if (TARGET_VIS3)
    {
      if (TARGET_ARCH64)
	{
	  def_builtin ("__builtin_vis_cmask8", CODE_FOR_cmask8di_vis,
		       SPARC_BUILTIN_CMASK8, void_ftype_di);
	  def_builtin ("__builtin_vis_cmask16", CODE_FOR_cmask16di_vis,
		       SPARC_BUILTIN_CMASK16, void_ftype_di);
	  def_builtin ("__builtin_vis_cmask32", CODE_FOR_cmask32di_vis,
		       SPARC_BUILTIN_CMASK32, void_ftype_di);
	}
      else
	{
	  def_builtin ("__builtin_vis_cmask8", CODE_FOR_cmask8si_vis,
		       SPARC_BUILTIN_CMASK8, void_ftype_si);
	  def_builtin ("__builtin_vis_cmask16", CODE_FOR_cmask16si_vis,
		       SPARC_BUILTIN_CMASK16, void_ftype_si);
	  def_builtin ("__builtin_vis_cmask32", CODE_FOR_cmask32si_vis,
		       SPARC_BUILTIN_CMASK32, void_ftype_si);
	}

      def_builtin_const ("__builtin_vis_fchksm16", CODE_FOR_fchksm16_vis,
			 SPARC_BUILTIN_FCHKSM16, v4hi_ftype_v4hi_v4hi);

      def_builtin_const ("__builtin_vis_fsll16", CODE_FOR_vashlv4hi3,
			 SPARC_BUILTIN_FSLL16, v4hi_ftype_v4hi_v4hi);
      def_builtin_const ("__builtin_vis_fslas16", CODE_FOR_vssashlv4hi3,
			 SPARC_BUILTIN_FSLAS16, v4hi_ftype_v4hi_v4hi);
      def_builtin_const ("__builtin_vis_fsrl16", CODE_FOR_vlshrv4hi3,
			 SPARC_BUILTIN_FSRL16, v4hi_ftype_v4hi_v4hi);
      def_builtin_const ("__builtin_vis_fsra16", CODE_FOR_vashrv4hi3,
			 SPARC_BUILTIN_FSRA16, v4hi_ftype_v4hi_v4hi);
      def_builtin_const ("__builtin_vis_fsll32", CODE_FOR_vashlv2si3,
			 SPARC_BUILTIN_FSLL32, v2si_ftype_v2si_v2si);
      def_builtin_const ("__builtin_vis_fslas32", CODE_FOR_vssashlv2si3,
			 SPARC_BUILTIN_FSLAS32, v2si_ftype_v2si_v2si);
      def_builtin_const ("__builtin_vis_fsrl32", CODE_FOR_vlshrv2si3,
			 SPARC_BUILTIN_FSRL32, v2si_ftype_v2si_v2si);
      def_builtin_const ("__builtin_vis_fsra32", CODE_FOR_vashrv2si3,
			 SPARC_BUILTIN_FSRA32, v2si_ftype_v2si_v2si);

      if (TARGET_ARCH64)
	def_builtin_const ("__builtin_vis_pdistn", CODE_FOR_pdistndi_vis,
			   SPARC_BUILTIN_PDISTN, di_ftype_v8qi_v8qi);
      else
	def_builtin_const ("__builtin_vis_pdistn", CODE_FOR_pdistnsi_vis,
			   SPARC_BUILTIN_PDISTN, si_ftype_v8qi_v8qi);

      def_builtin_const ("__builtin_vis_fmean16", CODE_FOR_fmean16_vis,
			 SPARC_BUILTIN_FMEAN16, v4hi_ftype_v4hi_v4hi);
      def_builtin_const ("__builtin_vis_fpadd64", CODE_FOR_fpadd64_vis,
			 SPARC_BUILTIN_FPADD64, di_ftype_di_di);
      def_builtin_const ("__builtin_vis_fpsub64", CODE_FOR_fpsub64_vis,
			 SPARC_BUILTIN_FPSUB64, di_ftype_di_di);

      def_builtin_const ("__builtin_vis_fpadds16", CODE_FOR_ssaddv4hi3,
			 SPARC_BUILTIN_FPADDS16, v4hi_ftype_v4hi_v4hi);
      def_builtin_const ("__builtin_vis_fpadds16s", CODE_FOR_ssaddv2hi3,
			 SPARC_BUILTIN_FPADDS16S, v2hi_ftype_v2hi_v2hi);
      def_builtin_const ("__builtin_vis_fpsubs16", CODE_FOR_sssubv4hi3,
			 SPARC_BUILTIN_FPSUBS16, v4hi_ftype_v4hi_v4hi);
      def_builtin_const ("__builtin_vis_fpsubs16s", CODE_FOR_sssubv2hi3,
			 SPARC_BUILTIN_FPSUBS16S, v2hi_ftype_v2hi_v2hi);
      def_builtin_const ("__builtin_vis_fpadds32", CODE_FOR_ssaddv2si3,
			 SPARC_BUILTIN_FPADDS32, v2si_ftype_v2si_v2si);
      def_builtin_const ("__builtin_vis_fpadds32s", CODE_FOR_ssaddv1si3,
			 SPARC_BUILTIN_FPADDS32S, v1si_ftype_v1si_v1si);
      def_builtin_const ("__builtin_vis_fpsubs32", CODE_FOR_sssubv2si3,
			 SPARC_BUILTIN_FPSUBS32, v2si_ftype_v2si_v2si);
      def_builtin_const ("__builtin_vis_fpsubs32s", CODE_FOR_sssubv1si3,
			 SPARC_BUILTIN_FPSUBS32S, v1si_ftype_v1si_v1si);

      if (TARGET_ARCH64)
	{
	  def_builtin_const ("__builtin_vis_fucmple8", CODE_FOR_fucmple8di_vis,
			     SPARC_BUILTIN_FUCMPLE8, di_ftype_v8qi_v8qi);
	  def_builtin_const ("__builtin_vis_fucmpne8", CODE_FOR_fucmpne8di_vis,
			     SPARC_BUILTIN_FUCMPNE8, di_ftype_v8qi_v8qi);
	  def_builtin_const ("__builtin_vis_fucmpgt8", CODE_FOR_fucmpgt8di_vis,
			     SPARC_BUILTIN_FUCMPGT8, di_ftype_v8qi_v8qi);
	  def_builtin_const ("__builtin_vis_fucmpeq8", CODE_FOR_fucmpeq8di_vis,
			     SPARC_BUILTIN_FUCMPEQ8, di_ftype_v8qi_v8qi);
	}
      else
	{
	  def_builtin_const ("__builtin_vis_fucmple8", CODE_FOR_fucmple8si_vis,
			     SPARC_BUILTIN_FUCMPLE8, si_ftype_v8qi_v8qi);
	  def_builtin_const ("__builtin_vis_fucmpne8", CODE_FOR_fucmpne8si_vis,
			     SPARC_BUILTIN_FUCMPNE8, si_ftype_v8qi_v8qi);
	  def_builtin_const ("__builtin_vis_fucmpgt8", CODE_FOR_fucmpgt8si_vis,
			     SPARC_BUILTIN_FUCMPGT8, si_ftype_v8qi_v8qi);
	  def_builtin_const ("__builtin_vis_fucmpeq8", CODE_FOR_fucmpeq8si_vis,
			     SPARC_BUILTIN_FUCMPEQ8, si_ftype_v8qi_v8qi);
	}

      def_builtin_const ("__builtin_vis_fhadds", CODE_FOR_fhaddsf_vis,
			 SPARC_BUILTIN_FHADDS, sf_ftype_sf_sf);
      def_builtin_const ("__builtin_vis_fhaddd", CODE_FOR_fhadddf_vis,
			 SPARC_BUILTIN_FHADDD, df_ftype_df_df);
      def_builtin_const ("__builtin_vis_fhsubs", CODE_FOR_fhsubsf_vis,
			 SPARC_BUILTIN_FHSUBS, sf_ftype_sf_sf);
      def_builtin_const ("__builtin_vis_fhsubd", CODE_FOR_fhsubdf_vis,
			 SPARC_BUILTIN_FHSUBD, df_ftype_df_df);
      def_builtin_const ("__builtin_vis_fnhadds", CODE_FOR_fnhaddsf_vis,
			 SPARC_BUILTIN_FNHADDS, sf_ftype_sf_sf);
      def_builtin_const ("__builtin_vis_fnhaddd", CODE_FOR_fnhadddf_vis,
			 SPARC_BUILTIN_FNHADDD, df_ftype_df_df);

      def_builtin_const ("__builtin_vis_umulxhi", CODE_FOR_umulxhi_vis,
			 SPARC_BUILTIN_UMULXHI, di_ftype_di_di);
      def_builtin_const ("__builtin_vis_xmulx", CODE_FOR_xmulx_vis,
			 SPARC_BUILTIN_XMULX, di_ftype_di_di);
      def_builtin_const ("__builtin_vis_xmulxhi", CODE_FOR_xmulxhi_vis,
			 SPARC_BUILTIN_XMULXHI, di_ftype_di_di);
    }

  if (TARGET_VIS4)
    {
      def_builtin_const ("__builtin_vis_fpadd8", CODE_FOR_addv8qi3,
			 SPARC_BUILTIN_FPADD8, v8qi_ftype_v8qi_v8qi);
      def_builtin_const ("__builtin_vis_fpadds8", CODE_FOR_ssaddv8qi3,
			 SPARC_BUILTIN_FPADDS8, v8qi_ftype_v8qi_v8qi);
      def_builtin_const ("__builtin_vis_fpaddus8", CODE_FOR_usaddv8qi3,
			 SPARC_BUILTIN_FPADDUS8, v8qi_ftype_v8qi_v8qi);
      def_builtin_const ("__builtin_vis_fpaddus16", CODE_FOR_usaddv4hi3,
			 SPARC_BUILTIN_FPADDUS16, v4hi_ftype_v4hi_v4hi);


      if (TARGET_ARCH64)
	{
	  def_builtin_const ("__builtin_vis_fpcmple8", CODE_FOR_fpcmple8di_vis,
			     SPARC_BUILTIN_FPCMPLE8, di_ftype_v8qi_v8qi);
	  def_builtin_const ("__builtin_vis_fpcmpgt8", CODE_FOR_fpcmpgt8di_vis,
			     SPARC_BUILTIN_FPCMPGT8, di_ftype_v8qi_v8qi);
	  def_builtin_const ("__builtin_vis_fpcmpule16", CODE_FOR_fpcmpule16di_vis,
			     SPARC_BUILTIN_FPCMPULE16, di_ftype_v4hi_v4hi);
	  def_builtin_const ("__builtin_vis_fpcmpugt16", CODE_FOR_fpcmpugt16di_vis,
			     SPARC_BUILTIN_FPCMPUGT16, di_ftype_v4hi_v4hi);
	  def_builtin_const ("__builtin_vis_fpcmpule32", CODE_FOR_fpcmpule32di_vis,
			     SPARC_BUILTIN_FPCMPULE32, di_ftype_v2si_v2si);
	  def_builtin_const ("__builtin_vis_fpcmpugt32", CODE_FOR_fpcmpugt32di_vis,
			     SPARC_BUILTIN_FPCMPUGT32, di_ftype_v2si_v2si);
	}
      else
	{
	  def_builtin_const ("__builtin_vis_fpcmple8", CODE_FOR_fpcmple8si_vis,
			     SPARC_BUILTIN_FPCMPLE8, si_ftype_v8qi_v8qi);
	  def_builtin_const ("__builtin_vis_fpcmpgt8", CODE_FOR_fpcmpgt8si_vis,
			     SPARC_BUILTIN_FPCMPGT8, si_ftype_v8qi_v8qi);
	  def_builtin_const ("__builtin_vis_fpcmpule16", CODE_FOR_fpcmpule16si_vis,
			     SPARC_BUILTIN_FPCMPULE16, si_ftype_v4hi_v4hi);
	  def_builtin_const ("__builtin_vis_fpcmpugt16", CODE_FOR_fpcmpugt16si_vis,
			     SPARC_BUILTIN_FPCMPUGT16, si_ftype_v4hi_v4hi);
	  def_builtin_const ("__builtin_vis_fpcmpule32", CODE_FOR_fpcmpule32si_vis,
			     SPARC_BUILTIN_FPCMPULE32, di_ftype_v2si_v2si);
	  def_builtin_const ("__builtin_vis_fpcmpugt32", CODE_FOR_fpcmpugt32si_vis,
			     SPARC_BUILTIN_FPCMPUGT32, di_ftype_v2si_v2si);
	}
      
      def_builtin_const ("__builtin_vis_fpmax8", CODE_FOR_maxv8qi3,
			 SPARC_BUILTIN_FPMAX8, v8qi_ftype_v8qi_v8qi);
      def_builtin_const ("__builtin_vis_fpmax16", CODE_FOR_maxv4hi3,
			 SPARC_BUILTIN_FPMAX16, v4hi_ftype_v4hi_v4hi);
      def_builtin_const ("__builtin_vis_fpmax32", CODE_FOR_maxv2si3,
			 SPARC_BUILTIN_FPMAX32, v2si_ftype_v2si_v2si);
      def_builtin_const ("__builtin_vis_fpmaxu8", CODE_FOR_maxuv8qi3,
			 SPARC_BUILTIN_FPMAXU8, v8qi_ftype_v8qi_v8qi);
      def_builtin_const ("__builtin_vis_fpmaxu16", CODE_FOR_maxuv4hi3,
			 SPARC_BUILTIN_FPMAXU16, v4hi_ftype_v4hi_v4hi);
      def_builtin_const ("__builtin_vis_fpmaxu32", CODE_FOR_maxuv2si3,
			 SPARC_BUILTIN_FPMAXU32, v2si_ftype_v2si_v2si);
      def_builtin_const ("__builtin_vis_fpmin8", CODE_FOR_minv8qi3,
			 SPARC_BUILTIN_FPMIN8, v8qi_ftype_v8qi_v8qi);
      def_builtin_const ("__builtin_vis_fpmin16", CODE_FOR_minv4hi3,
			 SPARC_BUILTIN_FPMIN16, v4hi_ftype_v4hi_v4hi);
      def_builtin_const ("__builtin_vis_fpmin32", CODE_FOR_minv2si3,
			 SPARC_BUILTIN_FPMIN32, v2si_ftype_v2si_v2si);
      def_builtin_const ("__builtin_vis_fpminu8", CODE_FOR_minuv8qi3,
			 SPARC_BUILTIN_FPMINU8, v8qi_ftype_v8qi_v8qi);
      def_builtin_const ("__builtin_vis_fpminu16", CODE_FOR_minuv4hi3,
			 SPARC_BUILTIN_FPMINU16, v4hi_ftype_v4hi_v4hi);
      def_builtin_const ("__builtin_vis_fpminu32", CODE_FOR_minuv2si3,
			 SPARC_BUILTIN_FPMINU32, v2si_ftype_v2si_v2si);
      def_builtin_const ("__builtin_vis_fpsub8", CODE_FOR_subv8qi3,
			 SPARC_BUILTIN_FPSUB8, v8qi_ftype_v8qi_v8qi);
      def_builtin_const ("__builtin_vis_fpsubs8", CODE_FOR_sssubv8qi3,
			 SPARC_BUILTIN_FPSUBS8, v8qi_ftype_v8qi_v8qi);
      def_builtin_const ("__builtin_vis_fpsubus8", CODE_FOR_ussubv8qi3,
			 SPARC_BUILTIN_FPSUBUS8, v8qi_ftype_v8qi_v8qi);
      def_builtin_const ("__builtin_vis_fpsubus16", CODE_FOR_ussubv4hi3,
			 SPARC_BUILTIN_FPSUBUS16, v4hi_ftype_v4hi_v4hi);
    }

  if (TARGET_VIS4B)
    {
      def_builtin_const ("__builtin_vis_dictunpack8", CODE_FOR_dictunpack8,
			 SPARC_BUILTIN_DICTUNPACK8, v8qi_ftype_df_si);
      def_builtin_const ("__builtin_vis_dictunpack16", CODE_FOR_dictunpack16,
			 SPARC_BUILTIN_DICTUNPACK16, v4hi_ftype_df_si);
      def_builtin_const ("__builtin_vis_dictunpack32", CODE_FOR_dictunpack32,
			 SPARC_BUILTIN_DICTUNPACK32, v2si_ftype_df_si);

      if (TARGET_ARCH64)
	{
	  tree di_ftype_v8qi_v8qi_si = build_function_type_list (intDI_type_node,
								 v8qi, v8qi,
								 intSI_type_node, 0);
	  tree di_ftype_v4hi_v4hi_si = build_function_type_list (intDI_type_node,
								 v4hi, v4hi,
								 intSI_type_node, 0);
	  tree di_ftype_v2si_v2si_si = build_function_type_list (intDI_type_node,
								 v2si, v2si,
								 intSI_type_node, 0);
	  
	  def_builtin_const ("__builtin_vis_fpcmple8shl", CODE_FOR_fpcmple8dishl,
			     SPARC_BUILTIN_FPCMPLE8SHL, di_ftype_v8qi_v8qi_si);
	  def_builtin_const ("__builtin_vis_fpcmpgt8shl", CODE_FOR_fpcmpgt8dishl,
			     SPARC_BUILTIN_FPCMPGT8SHL, di_ftype_v8qi_v8qi_si);
	  def_builtin_const ("__builtin_vis_fpcmpeq8shl", CODE_FOR_fpcmpeq8dishl,
			     SPARC_BUILTIN_FPCMPEQ8SHL, di_ftype_v8qi_v8qi_si);
	  def_builtin_const ("__builtin_vis_fpcmpne8shl", CODE_FOR_fpcmpne8dishl,
			     SPARC_BUILTIN_FPCMPNE8SHL, di_ftype_v8qi_v8qi_si);

	  def_builtin_const ("__builtin_vis_fpcmple16shl", CODE_FOR_fpcmple16dishl,
			     SPARC_BUILTIN_FPCMPLE16SHL, di_ftype_v4hi_v4hi_si);
	  def_builtin_const ("__builtin_vis_fpcmpgt16shl", CODE_FOR_fpcmpgt16dishl,
			     SPARC_BUILTIN_FPCMPGT16SHL, di_ftype_v4hi_v4hi_si);
	  def_builtin_const ("__builtin_vis_fpcmpeq16shl", CODE_FOR_fpcmpeq16dishl,
			     SPARC_BUILTIN_FPCMPEQ16SHL, di_ftype_v4hi_v4hi_si);
	  def_builtin_const ("__builtin_vis_fpcmpne16shl", CODE_FOR_fpcmpne16dishl,
			     SPARC_BUILTIN_FPCMPNE16SHL, di_ftype_v4hi_v4hi_si);

	  def_builtin_const ("__builtin_vis_fpcmple32shl", CODE_FOR_fpcmple32dishl,
			     SPARC_BUILTIN_FPCMPLE32SHL, di_ftype_v2si_v2si_si);
	  def_builtin_const ("__builtin_vis_fpcmpgt32shl", CODE_FOR_fpcmpgt32dishl,
			     SPARC_BUILTIN_FPCMPGT32SHL, di_ftype_v2si_v2si_si);
	  def_builtin_const ("__builtin_vis_fpcmpeq32shl", CODE_FOR_fpcmpeq32dishl,
			     SPARC_BUILTIN_FPCMPEQ32SHL, di_ftype_v2si_v2si_si);
	  def_builtin_const ("__builtin_vis_fpcmpne32shl", CODE_FOR_fpcmpne32dishl,
			     SPARC_BUILTIN_FPCMPNE32SHL, di_ftype_v2si_v2si_si);


	  def_builtin_const ("__builtin_vis_fpcmpule8shl", CODE_FOR_fpcmpule8dishl,
			     SPARC_BUILTIN_FPCMPULE8SHL, di_ftype_v8qi_v8qi_si);
	  def_builtin_const ("__builtin_vis_fpcmpugt8shl", CODE_FOR_fpcmpugt8dishl,
			     SPARC_BUILTIN_FPCMPUGT8SHL, di_ftype_v8qi_v8qi_si);

	  def_builtin_const ("__builtin_vis_fpcmpule16shl", CODE_FOR_fpcmpule16dishl,
			     SPARC_BUILTIN_FPCMPULE16SHL, di_ftype_v4hi_v4hi_si);
	  def_builtin_const ("__builtin_vis_fpcmpugt16shl", CODE_FOR_fpcmpugt16dishl,
			     SPARC_BUILTIN_FPCMPUGT16SHL, di_ftype_v4hi_v4hi_si);

	  def_builtin_const ("__builtin_vis_fpcmpule32shl", CODE_FOR_fpcmpule32dishl,
			     SPARC_BUILTIN_FPCMPULE32SHL, di_ftype_v2si_v2si_si);
	  def_builtin_const ("__builtin_vis_fpcmpugt32shl", CODE_FOR_fpcmpugt32dishl,
			     SPARC_BUILTIN_FPCMPUGT32SHL, di_ftype_v2si_v2si_si);

	  def_builtin_const ("__builtin_vis_fpcmpde8shl", CODE_FOR_fpcmpde8dishl,
			     SPARC_BUILTIN_FPCMPDE8SHL, di_ftype_v8qi_v8qi_si);
	  def_builtin_const ("__builtin_vis_fpcmpde16shl", CODE_FOR_fpcmpde16dishl,
			     SPARC_BUILTIN_FPCMPDE16SHL, di_ftype_v4hi_v4hi_si);
	  def_builtin_const ("__builtin_vis_fpcmpde32shl", CODE_FOR_fpcmpde32dishl,
			     SPARC_BUILTIN_FPCMPDE32SHL, di_ftype_v2si_v2si_si);

	  def_builtin_const ("__builtin_vis_fpcmpur8shl", CODE_FOR_fpcmpur8dishl,
			     SPARC_BUILTIN_FPCMPUR8SHL, di_ftype_v8qi_v8qi_si);
	  def_builtin_const ("__builtin_vis_fpcmpur16shl", CODE_FOR_fpcmpur16dishl,
			     SPARC_BUILTIN_FPCMPUR16SHL, di_ftype_v4hi_v4hi_si);
	  def_builtin_const ("__builtin_vis_fpcmpur32shl", CODE_FOR_fpcmpur32dishl,
			     SPARC_BUILTIN_FPCMPUR32SHL, di_ftype_v2si_v2si_si);

	}
      else
	{
	  tree si_ftype_v8qi_v8qi_si = build_function_type_list (intSI_type_node,
								 v8qi, v8qi,
								 intSI_type_node, 0);
	  tree si_ftype_v4hi_v4hi_si = build_function_type_list (intSI_type_node,
								 v4hi, v4hi,
								 intSI_type_node, 0);
	  tree si_ftype_v2si_v2si_si = build_function_type_list (intSI_type_node,
								 v2si, v2si,
								 intSI_type_node, 0);
	  
	  def_builtin_const ("__builtin_vis_fpcmple8shl", CODE_FOR_fpcmple8sishl,
			     SPARC_BUILTIN_FPCMPLE8SHL, si_ftype_v8qi_v8qi_si);
	  def_builtin_const ("__builtin_vis_fpcmpgt8shl", CODE_FOR_fpcmpgt8sishl,
			     SPARC_BUILTIN_FPCMPGT8SHL, si_ftype_v8qi_v8qi_si);
	  def_builtin_const ("__builtin_vis_fpcmpeq8shl", CODE_FOR_fpcmpeq8sishl,
			     SPARC_BUILTIN_FPCMPEQ8SHL, si_ftype_v8qi_v8qi_si);
	  def_builtin_const ("__builtin_vis_fpcmpne8shl", CODE_FOR_fpcmpne8sishl,
			     SPARC_BUILTIN_FPCMPNE8SHL, si_ftype_v8qi_v8qi_si);

	  def_builtin_const ("__builtin_vis_fpcmple16shl", CODE_FOR_fpcmple16sishl,
			     SPARC_BUILTIN_FPCMPLE16SHL, si_ftype_v4hi_v4hi_si);
	  def_builtin_const ("__builtin_vis_fpcmpgt16shl", CODE_FOR_fpcmpgt16sishl,
			     SPARC_BUILTIN_FPCMPGT16SHL, si_ftype_v4hi_v4hi_si);
	  def_builtin_const ("__builtin_vis_fpcmpeq16shl", CODE_FOR_fpcmpeq16sishl,
			     SPARC_BUILTIN_FPCMPEQ16SHL, si_ftype_v4hi_v4hi_si);
	  def_builtin_const ("__builtin_vis_fpcmpne16shl", CODE_FOR_fpcmpne16sishl,
			     SPARC_BUILTIN_FPCMPNE16SHL, si_ftype_v4hi_v4hi_si);

	  def_builtin_const ("__builtin_vis_fpcmple32shl", CODE_FOR_fpcmple32sishl,
			     SPARC_BUILTIN_FPCMPLE32SHL, si_ftype_v2si_v2si_si);
	  def_builtin_const ("__builtin_vis_fpcmpgt32shl", CODE_FOR_fpcmpgt32sishl,
			     SPARC_BUILTIN_FPCMPGT32SHL, si_ftype_v2si_v2si_si);
	  def_builtin_const ("__builtin_vis_fpcmpeq32shl", CODE_FOR_fpcmpeq32sishl,
			     SPARC_BUILTIN_FPCMPEQ32SHL, si_ftype_v2si_v2si_si);
	  def_builtin_const ("__builtin_vis_fpcmpne32shl", CODE_FOR_fpcmpne32sishl,
			     SPARC_BUILTIN_FPCMPNE32SHL, si_ftype_v2si_v2si_si);


	  def_builtin_const ("__builtin_vis_fpcmpule8shl", CODE_FOR_fpcmpule8sishl,
			     SPARC_BUILTIN_FPCMPULE8SHL, si_ftype_v8qi_v8qi_si);
	  def_builtin_const ("__builtin_vis_fpcmpugt8shl", CODE_FOR_fpcmpugt8sishl,
			     SPARC_BUILTIN_FPCMPUGT8SHL, si_ftype_v8qi_v8qi_si);

	  def_builtin_const ("__builtin_vis_fpcmpule16shl", CODE_FOR_fpcmpule16sishl,
			     SPARC_BUILTIN_FPCMPULE16SHL, si_ftype_v4hi_v4hi_si);
	  def_builtin_const ("__builtin_vis_fpcmpugt16shl", CODE_FOR_fpcmpugt16sishl,
			     SPARC_BUILTIN_FPCMPUGT16SHL, si_ftype_v4hi_v4hi_si);

	  def_builtin_const ("__builtin_vis_fpcmpule32shl", CODE_FOR_fpcmpule32sishl,
			     SPARC_BUILTIN_FPCMPULE32SHL, si_ftype_v2si_v2si_si);
	  def_builtin_const ("__builtin_vis_fpcmpugt32shl", CODE_FOR_fpcmpugt32sishl,
			     SPARC_BUILTIN_FPCMPUGT32SHL, si_ftype_v2si_v2si_si);

	  def_builtin_const ("__builtin_vis_fpcmpde8shl", CODE_FOR_fpcmpde8sishl,
			     SPARC_BUILTIN_FPCMPDE8SHL, si_ftype_v8qi_v8qi_si);
	  def_builtin_const ("__builtin_vis_fpcmpde16shl", CODE_FOR_fpcmpde16sishl,
			     SPARC_BUILTIN_FPCMPDE16SHL, si_ftype_v4hi_v4hi_si);
	  def_builtin_const ("__builtin_vis_fpcmpde32shl", CODE_FOR_fpcmpde32sishl,
			     SPARC_BUILTIN_FPCMPDE32SHL, si_ftype_v2si_v2si_si);

	  def_builtin_const ("__builtin_vis_fpcmpur8shl", CODE_FOR_fpcmpur8sishl,
			     SPARC_BUILTIN_FPCMPUR8SHL, si_ftype_v8qi_v8qi_si);
	  def_builtin_const ("__builtin_vis_fpcmpur16shl", CODE_FOR_fpcmpur16sishl,
			     SPARC_BUILTIN_FPCMPUR16SHL, si_ftype_v4hi_v4hi_si);
	  def_builtin_const ("__builtin_vis_fpcmpur32shl", CODE_FOR_fpcmpur32sishl,
			     SPARC_BUILTIN_FPCMPUR32SHL, si_ftype_v2si_v2si_si);
	}
    }
}

/* Implement TARGET_BUILTIN_DECL hook.  */

static tree
sparc_builtin_decl (unsigned code, bool initialize_p ATTRIBUTE_UNUSED)
{
  if (code >= SPARC_BUILTIN_MAX)
    return error_mark_node;

  return sparc_builtins[code];
}

/* Implemented TARGET_EXPAND_BUILTIN hook.  */

static rtx
sparc_expand_builtin (tree exp, rtx target,
		      rtx subtarget ATTRIBUTE_UNUSED,
		      machine_mode tmode ATTRIBUTE_UNUSED,
		      int ignore ATTRIBUTE_UNUSED)
{
  tree fndecl = TREE_OPERAND (CALL_EXPR_FN (exp), 0);
  enum sparc_builtins code = (enum sparc_builtins) DECL_FUNCTION_CODE (fndecl);
  enum insn_code icode = sparc_builtins_icode[code];
  bool nonvoid = TREE_TYPE (TREE_TYPE (fndecl)) != void_type_node;
  call_expr_arg_iterator iter;
  int arg_count = 0;
  rtx pat, op[4];
  tree arg;

  if (nonvoid)
    {
      machine_mode tmode = insn_data[icode].operand[0].mode;
      if (!target
	  || GET_MODE (target) != tmode
	  || ! (*insn_data[icode].operand[0].predicate) (target, tmode))
	op[0] = gen_reg_rtx (tmode);
      else
	op[0] = target;
    }

  FOR_EACH_CALL_EXPR_ARG (arg, iter, exp)
    {
      const struct insn_operand_data *insn_op;
      int idx;

      if (arg == error_mark_node)
	return NULL_RTX;

      arg_count++;
      idx = arg_count - !nonvoid;
      insn_op = &insn_data[icode].operand[idx];
      op[arg_count] = expand_normal (arg);

      /* Some of the builtins require constant arguments.  We check
	 for this here.  */
      if ((code >= SPARC_BUILTIN_FIRST_FPCMPSHL
	   && code <= SPARC_BUILTIN_LAST_FPCMPSHL
	   && arg_count == 3)
	  || (code >= SPARC_BUILTIN_FIRST_DICTUNPACK
	      && code <= SPARC_BUILTIN_LAST_DICTUNPACK
	      && arg_count == 2))
	{
	  if (!check_constant_argument (icode, idx, op[arg_count]))
	    return const0_rtx;
	}

      if (code == SPARC_BUILTIN_LDFSR || code == SPARC_BUILTIN_STFSR)
	{
	  if (!address_operand (op[arg_count], SImode))
	    {
	      op[arg_count] = convert_memory_address (Pmode, op[arg_count]);
	      op[arg_count] = copy_addr_to_reg (op[arg_count]);
	    }
	  op[arg_count] = gen_rtx_MEM (SImode, op[arg_count]);
	}

      else if (insn_op->mode == V1DImode
	       && GET_MODE (op[arg_count]) == DImode)
	op[arg_count] = gen_lowpart (V1DImode, op[arg_count]);

      else if (insn_op->mode == V1SImode
	       && GET_MODE (op[arg_count]) == SImode)
	op[arg_count] = gen_lowpart (V1SImode, op[arg_count]);

      if (! (*insn_data[icode].operand[idx].predicate) (op[arg_count],
							insn_op->mode))
	op[arg_count] = copy_to_mode_reg (insn_op->mode, op[arg_count]);
    }

  switch (arg_count)
    {
    case 0:
      pat = GEN_FCN (icode) (op[0]);
      break;
    case 1:
      if (nonvoid)
	pat = GEN_FCN (icode) (op[0], op[1]);
      else
	pat = GEN_FCN (icode) (op[1]);
      break;
    case 2:
      pat = GEN_FCN (icode) (op[0], op[1], op[2]);
      break;
    case 3:
      pat = GEN_FCN (icode) (op[0], op[1], op[2], op[3]);
      break;
    default:
      gcc_unreachable ();
    }

  if (!pat)
    return NULL_RTX;

  emit_insn (pat);

  return (nonvoid ? op[0] : const0_rtx);
}

/* Return the upper 16 bits of the 8x16 multiplication.  */

static int
sparc_vis_mul8x16 (int e8, int e16)
{
  return (e8 * e16 + 128) / 256;
}

/* Multiply the VECTOR_CSTs CST0 and CST1 as specified by FNCODE and put
   the result into the array N_ELTS, whose elements are of INNER_TYPE.  */

static void
sparc_handle_vis_mul8x16 (tree *n_elts, enum sparc_builtins fncode,
			  tree inner_type, tree cst0, tree cst1)
{
  unsigned i, num = VECTOR_CST_NELTS (cst0);
  int scale;

  switch (fncode)
    {
    case SPARC_BUILTIN_FMUL8X16:
      for (i = 0; i < num; ++i)
	{
	  int val
	    = sparc_vis_mul8x16 (TREE_INT_CST_LOW (VECTOR_CST_ELT (cst0, i)),
				 TREE_INT_CST_LOW (VECTOR_CST_ELT (cst1, i)));
	  n_elts[i] = build_int_cst (inner_type, val);
	}
      break;

    case SPARC_BUILTIN_FMUL8X16AU:
      scale = TREE_INT_CST_LOW (VECTOR_CST_ELT (cst1, 0));

      for (i = 0; i < num; ++i)
	{
	  int val
	    = sparc_vis_mul8x16 (TREE_INT_CST_LOW (VECTOR_CST_ELT (cst0, i)),
				 scale);
	  n_elts[i] = build_int_cst (inner_type, val);
	}
      break;

    case SPARC_BUILTIN_FMUL8X16AL:
      scale = TREE_INT_CST_LOW (VECTOR_CST_ELT (cst1, 1));

      for (i = 0; i < num; ++i)
	{
	  int val
	    = sparc_vis_mul8x16 (TREE_INT_CST_LOW (VECTOR_CST_ELT (cst0, i)),
				 scale);
	  n_elts[i] = build_int_cst (inner_type, val);
	}
      break;

    default:
      gcc_unreachable ();
    }
}

/* Implement TARGET_FOLD_BUILTIN hook.

   Fold builtin functions for SPARC intrinsics.  If IGNORE is true the
   result of the function call is ignored.  NULL_TREE is returned if the
   function could not be folded.  */

static tree
sparc_fold_builtin (tree fndecl, int n_args ATTRIBUTE_UNUSED,
		    tree *args, bool ignore)
{
  enum sparc_builtins code = (enum sparc_builtins) DECL_FUNCTION_CODE (fndecl);
  tree rtype = TREE_TYPE (TREE_TYPE (fndecl));
  tree arg0, arg1, arg2;

  if (ignore)
    switch (code)
      {
      case SPARC_BUILTIN_LDFSR:
      case SPARC_BUILTIN_STFSR:
      case SPARC_BUILTIN_ALIGNADDR:
      case SPARC_BUILTIN_WRGSR:
      case SPARC_BUILTIN_BMASK:
      case SPARC_BUILTIN_CMASK8:
      case SPARC_BUILTIN_CMASK16:
      case SPARC_BUILTIN_CMASK32:
	break;

      default:
	return build_zero_cst (rtype);
      }

  switch (code)
    {
    case SPARC_BUILTIN_FEXPAND:
      arg0 = args[0];
      STRIP_NOPS (arg0);

      if (TREE_CODE (arg0) == VECTOR_CST)
	{
	  tree inner_type = TREE_TYPE (rtype);
	  tree *n_elts;
	  unsigned i;

	  n_elts = XALLOCAVEC (tree, VECTOR_CST_NELTS (arg0));
	  for (i = 0; i < VECTOR_CST_NELTS (arg0); ++i)
	    n_elts[i] = build_int_cst (inner_type,
				       TREE_INT_CST_LOW
				         (VECTOR_CST_ELT (arg0, i)) << 4);
	  return build_vector (rtype, n_elts);
	}
      break;

    case SPARC_BUILTIN_FMUL8X16:
    case SPARC_BUILTIN_FMUL8X16AU:
    case SPARC_BUILTIN_FMUL8X16AL:
      arg0 = args[0];
      arg1 = args[1];
      STRIP_NOPS (arg0);
      STRIP_NOPS (arg1);

      if (TREE_CODE (arg0) == VECTOR_CST && TREE_CODE (arg1) == VECTOR_CST)
	{
	  tree inner_type = TREE_TYPE (rtype);
	  tree *n_elts = XALLOCAVEC (tree, VECTOR_CST_NELTS (arg0));
	  sparc_handle_vis_mul8x16 (n_elts, code, inner_type, arg0, arg1);
	  return build_vector (rtype, n_elts);
	}
      break;

    case SPARC_BUILTIN_FPMERGE:
      arg0 = args[0];
      arg1 = args[1];
      STRIP_NOPS (arg0);
      STRIP_NOPS (arg1);

      if (TREE_CODE (arg0) == VECTOR_CST && TREE_CODE (arg1) == VECTOR_CST)
	{
	  tree *n_elts = XALLOCAVEC (tree, 2 * VECTOR_CST_NELTS (arg0));
	  unsigned i;
	  for (i = 0; i < VECTOR_CST_NELTS (arg0); ++i)
	    {
	      n_elts[2*i] = VECTOR_CST_ELT (arg0, i);
	      n_elts[2*i+1] = VECTOR_CST_ELT (arg1, i);
	    }

	  return build_vector (rtype, n_elts);
	}
      break;

    case SPARC_BUILTIN_PDIST:
    case SPARC_BUILTIN_PDISTN:
      arg0 = args[0];
      arg1 = args[1];
      STRIP_NOPS (arg0);
      STRIP_NOPS (arg1);
      if (code == SPARC_BUILTIN_PDIST)
	{
	  arg2 = args[2];
	  STRIP_NOPS (arg2);
	}
      else
	arg2 = integer_zero_node;

      if (TREE_CODE (arg0) == VECTOR_CST
	  && TREE_CODE (arg1) == VECTOR_CST
	  && TREE_CODE (arg2) == INTEGER_CST)
	{
	  bool overflow = false;
	  widest_int result = wi::to_widest (arg2);
	  widest_int tmp;
	  unsigned i;

	  for (i = 0; i < VECTOR_CST_NELTS (arg0); ++i)
	    {
	      tree e0 = VECTOR_CST_ELT (arg0, i);
	      tree e1 = VECTOR_CST_ELT (arg1, i);

	      bool neg1_ovf, neg2_ovf, add1_ovf, add2_ovf;

	      tmp = wi::neg (wi::to_widest (e1), &neg1_ovf);
	      tmp = wi::add (wi::to_widest (e0), tmp, SIGNED, &add1_ovf);
	      if (wi::neg_p (tmp))
		tmp = wi::neg (tmp, &neg2_ovf);
	      else
		neg2_ovf = false;
	      result = wi::add (result, tmp, SIGNED, &add2_ovf);
	      overflow |= neg1_ovf | neg2_ovf | add1_ovf | add2_ovf;
	    }

	  gcc_assert (!overflow);

	  return wide_int_to_tree (rtype, result);
	}

    default:
      break;
    }

  return NULL_TREE;
}

/* ??? This duplicates information provided to the compiler by the
   ??? scheduler description.  Some day, teach genautomata to output
   ??? the latencies and then CSE will just use that.  */

static bool
sparc_rtx_costs (rtx x, machine_mode mode, int outer_code,
		 int opno ATTRIBUTE_UNUSED,
		 int *total, bool speed ATTRIBUTE_UNUSED)
{
  int code = GET_CODE (x);
  bool float_mode_p = FLOAT_MODE_P (mode);

  switch (code)
    {
    case CONST_INT:
      if (SMALL_INT (x))
	*total = 0;
      else
	*total = 2;
      return true;

    case CONST_WIDE_INT:
      *total = 0;
      if (!SPARC_SIMM13_P (CONST_WIDE_INT_ELT (x, 0)))
	*total += 2;
      if (!SPARC_SIMM13_P (CONST_WIDE_INT_ELT (x, 1)))
	*total += 2;
      return true;

    case HIGH:
      *total = 2;
      return true;

    case CONST:
    case LABEL_REF:
    case SYMBOL_REF:
      *total = 4;
      return true;

    case CONST_DOUBLE:
      *total = 8;
      return true;

    case MEM:
      /* If outer-code was a sign or zero extension, a cost
	 of COSTS_N_INSNS (1) was already added in.  This is
	 why we are subtracting it back out.  */
      if (outer_code == ZERO_EXTEND)
	{
	  *total = sparc_costs->int_zload - COSTS_N_INSNS (1);
	}
      else if (outer_code == SIGN_EXTEND)
	{
	  *total = sparc_costs->int_sload - COSTS_N_INSNS (1);
	}
      else if (float_mode_p)
	{
	  *total = sparc_costs->float_load;
	}
      else
	{
	  *total = sparc_costs->int_load;
	}

      return true;

    case PLUS:
    case MINUS:
      if (float_mode_p)
	*total = sparc_costs->float_plusminus;
      else
	*total = COSTS_N_INSNS (1);
      return false;

    case FMA:
      {
	rtx sub;

	gcc_assert (float_mode_p);
	*total = sparc_costs->float_mul;

	sub = XEXP (x, 0);
	if (GET_CODE (sub) == NEG)
	  sub = XEXP (sub, 0);
	*total += rtx_cost (sub, mode, FMA, 0, speed);

	sub = XEXP (x, 2);
	if (GET_CODE (sub) == NEG)
	  sub = XEXP (sub, 0);
	*total += rtx_cost (sub, mode, FMA, 2, speed);
	return true;
      }

    case MULT:
      if (float_mode_p)
	*total = sparc_costs->float_mul;
      else if (TARGET_ARCH32 && !TARGET_HARD_MUL)
	*total = COSTS_N_INSNS (25);
      else
	{
	  int bit_cost;

	  bit_cost = 0;
	  if (sparc_costs->int_mul_bit_factor)
	    {
	      int nbits;

	      if (GET_CODE (XEXP (x, 1)) == CONST_INT)
		{
		  unsigned HOST_WIDE_INT value = INTVAL (XEXP (x, 1));
		  for (nbits = 0; value != 0; value &= value - 1)
		    nbits++;
		}
	      else
		nbits = 7;

	      if (nbits < 3)
		nbits = 3;
	      bit_cost = (nbits - 3) / sparc_costs->int_mul_bit_factor;
	      bit_cost = COSTS_N_INSNS (bit_cost);
	    }

	  if (mode == DImode || !TARGET_HARD_MUL)
	    *total = sparc_costs->int_mulX + bit_cost;
	  else
	    *total = sparc_costs->int_mul + bit_cost;
	}
      return false;

    case ASHIFT:
    case ASHIFTRT:
    case LSHIFTRT:
      *total = COSTS_N_INSNS (1) + sparc_costs->shift_penalty;
      return false;

    case DIV:
    case UDIV:
    case MOD:
    case UMOD:
      if (float_mode_p)
	{
	  if (mode == DFmode)
	    *total = sparc_costs->float_div_df;
	  else
	    *total = sparc_costs->float_div_sf;
	}
      else
	{
	  if (mode == DImode)
	    *total = sparc_costs->int_divX;
	  else
	    *total = sparc_costs->int_div;
	}
      return false;

    case NEG:
      if (! float_mode_p)
	{
	  *total = COSTS_N_INSNS (1);
	  return false;
	}
      /* FALLTHRU */

    case ABS:
    case FLOAT:
    case UNSIGNED_FLOAT:
    case FIX:
    case UNSIGNED_FIX:
    case FLOAT_EXTEND:
    case FLOAT_TRUNCATE:
      *total = sparc_costs->float_move;
      return false;

    case SQRT:
      if (mode == DFmode)
	*total = sparc_costs->float_sqrt_df;
      else
	*total = sparc_costs->float_sqrt_sf;
      return false;

    case COMPARE:
      if (float_mode_p)
	*total = sparc_costs->float_cmp;
      else
	*total = COSTS_N_INSNS (1);
      return false;

    case IF_THEN_ELSE:
      if (float_mode_p)
	*total = sparc_costs->float_cmove;
      else
	*total = sparc_costs->int_cmove;
      return false;

    case IOR:
      /* Handle the NAND vector patterns.  */
      if (sparc_vector_mode_supported_p (mode)
	  && GET_CODE (XEXP (x, 0)) == NOT
	  && GET_CODE (XEXP (x, 1)) == NOT)
	{
	  *total = COSTS_N_INSNS (1);
	  return true;
	}
      else
        return false;

    default:
      return false;
    }
}

/* Return true if CLASS is either GENERAL_REGS or I64_REGS.  */

static inline bool
general_or_i64_p (reg_class_t rclass)
{
  return (rclass == GENERAL_REGS || rclass == I64_REGS);
}

/* Implement TARGET_REGISTER_MOVE_COST.  */

static int
sparc_register_move_cost (machine_mode mode ATTRIBUTE_UNUSED,
			  reg_class_t from, reg_class_t to)
{
  bool need_memory = false;

  /* This helps postreload CSE to eliminate redundant comparisons.  */
  if (from == NO_REGS || to == NO_REGS)
    return 100;

  if (from == FPCC_REGS || to == FPCC_REGS)
    need_memory = true;
  else if ((FP_REG_CLASS_P (from) && general_or_i64_p (to))
	   || (general_or_i64_p (from) && FP_REG_CLASS_P (to)))
    {
      if (TARGET_VIS3)
	{
	  int size = GET_MODE_SIZE (mode);
	  if (size == 8 || size == 4)
	    {
	      if (! TARGET_ARCH32 || size == 4)
		return 4;
	      else
		return 6;
	    }
	}
      need_memory = true;
    }

  if (need_memory)
    {
      if (sparc_cpu == PROCESSOR_ULTRASPARC
	  || sparc_cpu == PROCESSOR_ULTRASPARC3
	  || sparc_cpu == PROCESSOR_NIAGARA
	  || sparc_cpu == PROCESSOR_NIAGARA2
	  || sparc_cpu == PROCESSOR_NIAGARA3
	  || sparc_cpu == PROCESSOR_NIAGARA4
	  || sparc_cpu == PROCESSOR_NIAGARA7
	  || sparc_cpu == PROCESSOR_M8)
	return 12;

      return 6;
    }

  return 2;
}

/* Emit the sequence of insns SEQ while preserving the registers REG and REG2.
   This is achieved by means of a manual dynamic stack space allocation in
   the current frame.  We make the assumption that SEQ doesn't contain any
   function calls, with the possible exception of calls to the GOT helper.  */

static void
emit_and_preserve (rtx seq, rtx reg, rtx reg2)
{
  /* We must preserve the lowest 16 words for the register save area.  */
  HOST_WIDE_INT offset = 16*UNITS_PER_WORD;
  /* We really need only 2 words of fresh stack space.  */
  HOST_WIDE_INT size = SPARC_STACK_ALIGN (offset + 2*UNITS_PER_WORD);

  rtx slot
    = gen_rtx_MEM (word_mode, plus_constant (Pmode, stack_pointer_rtx,
					     SPARC_STACK_BIAS + offset));

  emit_insn (gen_stack_pointer_inc (GEN_INT (-size)));
  emit_insn (gen_rtx_SET (slot, reg));
  if (reg2)
    emit_insn (gen_rtx_SET (adjust_address (slot, word_mode, UNITS_PER_WORD),
			    reg2));
  emit_insn (seq);
  if (reg2)
    emit_insn (gen_rtx_SET (reg2,
			    adjust_address (slot, word_mode, UNITS_PER_WORD)));
  emit_insn (gen_rtx_SET (reg, slot));
  emit_insn (gen_stack_pointer_inc (GEN_INT (size)));
}

/* Output the assembler code for a thunk function.  THUNK_DECL is the
   declaration for the thunk function itself, FUNCTION is the decl for
   the target function.  DELTA is an immediate constant offset to be
   added to THIS.  If VCALL_OFFSET is nonzero, the word at address
   (*THIS + VCALL_OFFSET) should be additionally added to THIS.  */

static void
sparc_output_mi_thunk (FILE *file, tree thunk_fndecl ATTRIBUTE_UNUSED,
		       HOST_WIDE_INT delta, HOST_WIDE_INT vcall_offset,
		       tree function)
{
  rtx this_rtx, funexp;
  rtx_insn *insn;
  unsigned int int_arg_first;

  reload_completed = 1;
  epilogue_completed = 1;

  emit_note (NOTE_INSN_PROLOGUE_END);

  if (TARGET_FLAT)
    {
      sparc_leaf_function_p = 1;

      int_arg_first = SPARC_OUTGOING_INT_ARG_FIRST;
    }
  else if (flag_delayed_branch)
    {
      /* We will emit a regular sibcall below, so we need to instruct
	 output_sibcall that we are in a leaf function.  */
      sparc_leaf_function_p = crtl->uses_only_leaf_regs = 1;

      /* This will cause final.c to invoke leaf_renumber_regs so we
	 must behave as if we were in a not-yet-leafified function.  */
      int_arg_first = SPARC_INCOMING_INT_ARG_FIRST;
    }
  else
    {
      /* We will emit the sibcall manually below, so we will need to
	 manually spill non-leaf registers.  */
      sparc_leaf_function_p = crtl->uses_only_leaf_regs = 0;

      /* We really are in a leaf function.  */
      int_arg_first = SPARC_OUTGOING_INT_ARG_FIRST;
    }

  /* Find the "this" pointer.  Normally in %o0, but in ARCH64 if the function
     returns a structure, the structure return pointer is there instead.  */
  if (TARGET_ARCH64
      && aggregate_value_p (TREE_TYPE (TREE_TYPE (function)), function))
    this_rtx = gen_rtx_REG (Pmode, int_arg_first + 1);
  else
    this_rtx = gen_rtx_REG (Pmode, int_arg_first);

  /* Add DELTA.  When possible use a plain add, otherwise load it into
     a register first.  */
  if (delta)
    {
      rtx delta_rtx = GEN_INT (delta);

      if (! SPARC_SIMM13_P (delta))
	{
	  rtx scratch = gen_rtx_REG (Pmode, 1);
	  emit_move_insn (scratch, delta_rtx);
	  delta_rtx = scratch;
	}

      /* THIS_RTX += DELTA.  */
      emit_insn (gen_add2_insn (this_rtx, delta_rtx));
    }

  /* Add the word at address (*THIS_RTX + VCALL_OFFSET).  */
  if (vcall_offset)
    {
      rtx vcall_offset_rtx = GEN_INT (vcall_offset);
      rtx scratch = gen_rtx_REG (Pmode, 1);

      gcc_assert (vcall_offset < 0);

      /* SCRATCH = *THIS_RTX.  */
      emit_move_insn (scratch, gen_rtx_MEM (Pmode, this_rtx));

      /* Prepare for adding VCALL_OFFSET.  The difficulty is that we
	 may not have any available scratch register at this point.  */
      if (SPARC_SIMM13_P (vcall_offset))
	;
      /* This is the case if ARCH64 (unless -ffixed-g5 is passed).  */
      else if (! fixed_regs[5]
	       /* The below sequence is made up of at least 2 insns,
		  while the default method may need only one.  */
	       && vcall_offset < -8192)
	{
	  rtx scratch2 = gen_rtx_REG (Pmode, 5);
	  emit_move_insn (scratch2, vcall_offset_rtx);
	  vcall_offset_rtx = scratch2;
	}
      else
	{
	  rtx increment = GEN_INT (-4096);

	  /* VCALL_OFFSET is a negative number whose typical range can be
	     estimated as -32768..0 in 32-bit mode.  In almost all cases
	     it is therefore cheaper to emit multiple add insns than
	     spilling and loading the constant into a register (at least
	     6 insns).  */
	  while (! SPARC_SIMM13_P (vcall_offset))
	    {
	      emit_insn (gen_add2_insn (scratch, increment));
	      vcall_offset += 4096;
	    }
	  vcall_offset_rtx = GEN_INT (vcall_offset); /* cannot be 0 */
	}

      /* SCRATCH = *(*THIS_RTX + VCALL_OFFSET).  */
      emit_move_insn (scratch, gen_rtx_MEM (Pmode,
					    gen_rtx_PLUS (Pmode,
							  scratch,
							  vcall_offset_rtx)));

      /* THIS_RTX += *(*THIS_RTX + VCALL_OFFSET).  */
      emit_insn (gen_add2_insn (this_rtx, scratch));
    }

  /* Generate a tail call to the target function.  */
  if (! TREE_USED (function))
    {
      assemble_external (function);
      TREE_USED (function) = 1;
    }
  funexp = XEXP (DECL_RTL (function), 0);

  if (flag_delayed_branch)
    {
      funexp = gen_rtx_MEM (FUNCTION_MODE, funexp);
      insn = emit_call_insn (gen_sibcall (funexp));
      SIBLING_CALL_P (insn) = 1;
    }
  else
    {
      /* The hoops we have to jump through in order to generate a sibcall
	 without using delay slots...  */
      rtx spill_reg, seq, scratch = gen_rtx_REG (Pmode, 1);

      if (flag_pic)
        {
	  spill_reg = gen_rtx_REG (word_mode, 15);  /* %o7 */
	  start_sequence ();
	  load_got_register ();  /* clobbers %o7 */
	  scratch = sparc_legitimize_pic_address (funexp, scratch);
	  seq = get_insns ();
	  end_sequence ();
	  emit_and_preserve (seq, spill_reg, pic_offset_table_rtx);
	}
      else if (TARGET_ARCH32)
	{
	  emit_insn (gen_rtx_SET (scratch,
				  gen_rtx_HIGH (SImode, funexp)));
	  emit_insn (gen_rtx_SET (scratch,
				  gen_rtx_LO_SUM (SImode, scratch, funexp)));
	}
      else  /* TARGET_ARCH64 */
        {
	  switch (sparc_cmodel)
	    {
	    case CM_MEDLOW:
	    case CM_MEDMID:
	      /* The destination can serve as a temporary.  */
	      sparc_emit_set_symbolic_const64 (scratch, funexp, scratch);
	      break;

	    case CM_MEDANY:
	    case CM_EMBMEDANY:
	      /* The destination cannot serve as a temporary.  */
	      spill_reg = gen_rtx_REG (DImode, 15);  /* %o7 */
	      start_sequence ();
	      sparc_emit_set_symbolic_const64 (scratch, funexp, spill_reg);
	      seq = get_insns ();
	      end_sequence ();
	      emit_and_preserve (seq, spill_reg, 0);
	      break;

	    default:
	      gcc_unreachable ();
	    }
	}

      emit_jump_insn (gen_indirect_jump (scratch));
    }

  emit_barrier ();

  /* Run just enough of rest_of_compilation to get the insns emitted.
     There's not really enough bulk here to make other passes such as
     instruction scheduling worth while.  Note that use_thunk calls
     assemble_start_function and assemble_end_function.  */
  insn = get_insns ();
  shorten_branches (insn);
  final_start_function (insn, file, 1);
  final (insn, file, 1);
  final_end_function ();

  reload_completed = 0;
  epilogue_completed = 0;
}

/* Return true if sparc_output_mi_thunk would be able to output the
   assembler code for the thunk function specified by the arguments
   it is passed, and false otherwise.  */
static bool
sparc_can_output_mi_thunk (const_tree thunk_fndecl ATTRIBUTE_UNUSED,
			   HOST_WIDE_INT delta ATTRIBUTE_UNUSED,
			   HOST_WIDE_INT vcall_offset,
			   const_tree function ATTRIBUTE_UNUSED)
{
  /* Bound the loop used in the default method above.  */
  return (vcall_offset >= -32768 || ! fixed_regs[5]);
}

/* How to allocate a 'struct machine_function'.  */

static struct machine_function *
sparc_init_machine_status (void)
{
  return ggc_cleared_alloc<machine_function> ();
}

/* This is called from dwarf2out.c via TARGET_ASM_OUTPUT_DWARF_DTPREL.
   We need to emit DTP-relative relocations.  */

static void
sparc_output_dwarf_dtprel (FILE *file, int size, rtx x)
{
  switch (size)
    {
    case 4:
      fputs ("\t.word\t%r_tls_dtpoff32(", file);
      break;
    case 8:
      fputs ("\t.xword\t%r_tls_dtpoff64(", file);
      break;
    default:
      gcc_unreachable ();
    }
  output_addr_const (file, x);
  fputs (")", file);
}

/* Do whatever processing is required at the end of a file.  */

static void
sparc_file_end (void)
{
  /* If we need to emit the special GOT helper function, do so now.  */
  if (got_helper_rtx)
    {
      const char *name = XSTR (got_helper_rtx, 0);
      const char *reg_name = reg_names[GLOBAL_OFFSET_TABLE_REGNUM];
#ifdef DWARF2_UNWIND_INFO
      bool do_cfi;
#endif

      if (USE_HIDDEN_LINKONCE)
	{
	  tree decl = build_decl (BUILTINS_LOCATION, FUNCTION_DECL,
				  get_identifier (name),
				  build_function_type_list (void_type_node,
                                                            NULL_TREE));
	  DECL_RESULT (decl) = build_decl (BUILTINS_LOCATION, RESULT_DECL,
					   NULL_TREE, void_type_node);
	  TREE_PUBLIC (decl) = 1;
	  TREE_STATIC (decl) = 1;
	  make_decl_one_only (decl, DECL_ASSEMBLER_NAME (decl));
	  DECL_VISIBILITY (decl) = VISIBILITY_HIDDEN;
	  DECL_VISIBILITY_SPECIFIED (decl) = 1;
	  resolve_unique_section (decl, 0, flag_function_sections);
	  allocate_struct_function (decl, true);
	  cfun->is_thunk = 1;
	  current_function_decl = decl;
	  init_varasm_status ();
	  assemble_start_function (decl, name);
	}
      else
	{
	  const int align = floor_log2 (FUNCTION_BOUNDARY / BITS_PER_UNIT);
          switch_to_section (text_section);
	  if (align > 0)
	    ASM_OUTPUT_ALIGN (asm_out_file, align);
	  ASM_OUTPUT_LABEL (asm_out_file, name);
	}

#ifdef DWARF2_UNWIND_INFO
      do_cfi = dwarf2out_do_cfi_asm ();
      if (do_cfi)
	fprintf (asm_out_file, "\t.cfi_startproc\n");
#endif
      if (flag_delayed_branch)
	fprintf (asm_out_file, "\tjmp\t%%o7+8\n\t add\t%%o7, %s, %s\n",
		 reg_name, reg_name);
      else
	fprintf (asm_out_file, "\tadd\t%%o7, %s, %s\n\tjmp\t%%o7+8\n\t nop\n",
		 reg_name, reg_name);
#ifdef DWARF2_UNWIND_INFO
      if (do_cfi)
	fprintf (asm_out_file, "\t.cfi_endproc\n");
#endif
    }

  if (NEED_INDICATE_EXEC_STACK)
    file_end_indicate_exec_stack ();

#ifdef TARGET_SOLARIS
  solaris_file_end ();
#endif
}

#ifdef TARGET_ALTERNATE_LONG_DOUBLE_MANGLING
/* Implement TARGET_MANGLE_TYPE.  */

static const char *
sparc_mangle_type (const_tree type)
{
  if (TARGET_ARCH32
      && TYPE_MAIN_VARIANT (type) == long_double_type_node
      && TARGET_LONG_DOUBLE_128)
    return "g";

  /* For all other types, use normal C++ mangling.  */
  return NULL;
}
#endif

/* Expand a membar instruction for various use cases.  Both the LOAD_STORE
   and BEFORE_AFTER arguments of the form X_Y.  They are two-bit masks where
   bit 0 indicates that X is true, and bit 1 indicates Y is true.  */

void
sparc_emit_membar_for_model (enum memmodel model,
			     int load_store, int before_after)
{
  /* Bits for the MEMBAR mmask field.  */
  const int LoadLoad = 1;
  const int StoreLoad = 2;
  const int LoadStore = 4;
  const int StoreStore = 8;

  int mm = 0, implied = 0;

  switch (sparc_memory_model)
    {
    case SMM_SC:
      /* Sequential Consistency.  All memory transactions are immediately
	 visible in sequential execution order.  No barriers needed.  */
      implied = LoadLoad | StoreLoad | LoadStore | StoreStore;
      break;

    case SMM_TSO:
      /* Total Store Ordering: all memory transactions with store semantics
	 are followed by an implied StoreStore.  */
      implied |= StoreStore;

      /* If we're not looking for a raw barrer (before+after), then atomic
	 operations get the benefit of being both load and store.  */
      if (load_store == 3 && before_after == 1)
	implied |= StoreLoad;
      /* FALLTHRU */

    case SMM_PSO:
      /* Partial Store Ordering: all memory transactions with load semantics
	 are followed by an implied LoadLoad | LoadStore.  */
      implied |= LoadLoad | LoadStore;

      /* If we're not looking for a raw barrer (before+after), then atomic
	 operations get the benefit of being both load and store.  */
      if (load_store == 3 && before_after == 2)
	implied |= StoreLoad | StoreStore;
      /* FALLTHRU */

    case SMM_RMO:
      /* Relaxed Memory Ordering: no implicit bits.  */
      break;

    default:
      gcc_unreachable ();
    }

  if (before_after & 1)
    {
      if (is_mm_release (model) || is_mm_acq_rel (model)
	  || is_mm_seq_cst (model))
	{
	  if (load_store & 1)
	    mm |= LoadLoad | StoreLoad;
	  if (load_store & 2)
	    mm |= LoadStore | StoreStore;
	}
    }
  if (before_after & 2)
    {
      if (is_mm_acquire (model) || is_mm_acq_rel (model)
	  || is_mm_seq_cst (model))
	{
	  if (load_store & 1)
	    mm |= LoadLoad | LoadStore;
	  if (load_store & 2)
	    mm |= StoreLoad | StoreStore;
	}
    }

  /* Remove the bits implied by the system memory model.  */
  mm &= ~implied;

  /* For raw barriers (before+after), always emit a barrier.
     This will become a compile-time barrier if needed.  */
  if (mm || before_after == 3)
    emit_insn (gen_membar (GEN_INT (mm)));
}

/* Expand code to perform a 8 or 16-bit compare and swap by doing 32-bit
   compare and swap on the word containing the byte or half-word.  */

static void
sparc_expand_compare_and_swap_12 (rtx bool_result, rtx result, rtx mem,
				  rtx oldval, rtx newval)
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
  rtx memsi, val, mask, cc;

  emit_insn (gen_rtx_SET (addr, gen_rtx_AND (Pmode, addr1, GEN_INT (-4))));

  if (Pmode != SImode)
    addr1 = gen_lowpart (SImode, addr1);
  emit_insn (gen_rtx_SET (off, gen_rtx_AND (SImode, addr1, GEN_INT (3))));

  memsi = gen_rtx_MEM (SImode, addr);
  set_mem_alias_set (memsi, ALIAS_SET_MEMORY_BARRIER);
  MEM_VOLATILE_P (memsi) = MEM_VOLATILE_P (mem);

  val = copy_to_reg (memsi);

  emit_insn (gen_rtx_SET (off,
			  gen_rtx_XOR (SImode, off,
				       GEN_INT (GET_MODE (mem) == QImode
						? 3 : 2))));

  emit_insn (gen_rtx_SET (off, gen_rtx_ASHIFT (SImode, off, GEN_INT (3))));

  if (GET_MODE (mem) == QImode)
    mask = force_reg (SImode, GEN_INT (0xff));
  else
    mask = force_reg (SImode, GEN_INT (0xffff));

  emit_insn (gen_rtx_SET (mask, gen_rtx_ASHIFT (SImode, mask, off)));

  emit_insn (gen_rtx_SET (val,
			  gen_rtx_AND (SImode, gen_rtx_NOT (SImode, mask),
				       val)));

  oldval = gen_lowpart (SImode, oldval);
  emit_insn (gen_rtx_SET (oldv, gen_rtx_ASHIFT (SImode, oldval, off)));

  newval = gen_lowpart_common (SImode, newval);
  emit_insn (gen_rtx_SET (newv, gen_rtx_ASHIFT (SImode, newval, off)));

  emit_insn (gen_rtx_SET (oldv, gen_rtx_AND (SImode, oldv, mask)));

  emit_insn (gen_rtx_SET (newv, gen_rtx_AND (SImode, newv, mask)));

  rtx_code_label *end_label = gen_label_rtx ();
  rtx_code_label *loop_label = gen_label_rtx ();
  emit_label (loop_label);

  emit_insn (gen_rtx_SET (oldvalue, gen_rtx_IOR (SImode, oldv, val)));

  emit_insn (gen_rtx_SET (newvalue, gen_rtx_IOR (SImode, newv, val)));

  emit_move_insn (bool_result, const1_rtx);

  emit_insn (gen_atomic_compare_and_swapsi_1 (res, memsi, oldvalue, newvalue));

  emit_cmp_and_jump_insns (res, oldvalue, EQ, NULL, SImode, 0, end_label);

  emit_insn (gen_rtx_SET (resv,
			  gen_rtx_AND (SImode, gen_rtx_NOT (SImode, mask),
				       res)));

  emit_move_insn (bool_result, const0_rtx);

  cc = gen_compare_reg_1 (NE, resv, val);
  emit_insn (gen_rtx_SET (val, resv));

  /* Use cbranchcc4 to separate the compare and branch!  */
  emit_jump_insn (gen_cbranchcc4 (gen_rtx_NE (VOIDmode, cc, const0_rtx),
				  cc, const0_rtx, loop_label));

  emit_label (end_label);

  emit_insn (gen_rtx_SET (res, gen_rtx_AND (SImode, res, mask)));

  emit_insn (gen_rtx_SET (res, gen_rtx_LSHIFTRT (SImode, res, off)));

  emit_move_insn (result, gen_lowpart (GET_MODE (result), res));
}

/* Expand code to perform a compare-and-swap.  */

void
sparc_expand_compare_and_swap (rtx operands[])
{
  rtx bval, retval, mem, oldval, newval;
  machine_mode mode;
  enum memmodel model;

  bval = operands[0];
  retval = operands[1];
  mem = operands[2];
  oldval = operands[3];
  newval = operands[4];
  model = (enum memmodel) INTVAL (operands[6]);
  mode = GET_MODE (mem);

  sparc_emit_membar_for_model (model, 3, 1);

  if (reg_overlap_mentioned_p (retval, oldval))
    oldval = copy_to_reg (oldval);

  if (mode == QImode || mode == HImode)
    sparc_expand_compare_and_swap_12 (bval, retval, mem, oldval, newval);
  else
    {
      rtx (*gen) (rtx, rtx, rtx, rtx);
      rtx x;

      if (mode == SImode)
	gen = gen_atomic_compare_and_swapsi_1;
      else
	gen = gen_atomic_compare_and_swapdi_1;
      emit_insn (gen (retval, mem, oldval, newval));

      x = emit_store_flag (bval, EQ, retval, oldval, mode, 1, 1);
      if (x != bval)
	convert_move (bval, x, 1);
    }

  sparc_emit_membar_for_model (model, 3, 2);
}

void
sparc_expand_vec_perm_bmask (machine_mode vmode, rtx sel)
{
  rtx t_1, t_2, t_3;

  sel = gen_lowpart (DImode, sel);
  switch (vmode)
    {
    case E_V2SImode:
      /* inp = xxxxxxxAxxxxxxxB */
      t_1 = expand_simple_binop (DImode, LSHIFTRT, sel, GEN_INT (16),
				 NULL_RTX, 1, OPTAB_DIRECT);
      /* t_1 = ....xxxxxxxAxxx. */
      sel = expand_simple_binop (SImode, AND, gen_lowpart (SImode, sel),
				 GEN_INT (3), NULL_RTX, 1, OPTAB_DIRECT);
      t_1 = expand_simple_binop (SImode, AND, gen_lowpart (SImode, t_1),
				 GEN_INT (0x30000), NULL_RTX, 1, OPTAB_DIRECT);
      /* sel = .......B */
      /* t_1 = ...A.... */
      sel = expand_simple_binop (SImode, IOR, sel, t_1, sel, 1, OPTAB_DIRECT);
      /* sel = ...A...B */
      sel = expand_mult (SImode, sel, GEN_INT (0x4444), sel, 1);
      /* sel = AAAABBBB * 4 */
      t_1 = force_reg (SImode, GEN_INT (0x01230123));
      /* sel = { A*4, A*4+1, A*4+2, ... } */
      break;

    case E_V4HImode:
      /* inp = xxxAxxxBxxxCxxxD */
      t_1 = expand_simple_binop (DImode, LSHIFTRT, sel, GEN_INT (8),
				 NULL_RTX, 1, OPTAB_DIRECT);
      t_2 = expand_simple_binop (DImode, LSHIFTRT, sel, GEN_INT (16),
				 NULL_RTX, 1, OPTAB_DIRECT);
      t_3 = expand_simple_binop (DImode, LSHIFTRT, sel, GEN_INT (24),
				 NULL_RTX, 1, OPTAB_DIRECT);
      /* t_1 = ..xxxAxxxBxxxCxx */
      /* t_2 = ....xxxAxxxBxxxC */
      /* t_3 = ......xxxAxxxBxx */
      sel = expand_simple_binop (SImode, AND, gen_lowpart (SImode, sel),
				 GEN_INT (0x07),
				 NULL_RTX, 1, OPTAB_DIRECT);
      t_1 = expand_simple_binop (SImode, AND, gen_lowpart (SImode, t_1),
				 GEN_INT (0x0700),
				 NULL_RTX, 1, OPTAB_DIRECT);
      t_2 = expand_simple_binop (SImode, AND, gen_lowpart (SImode, t_2),
				 GEN_INT (0x070000),
				 NULL_RTX, 1, OPTAB_DIRECT);
      t_3 = expand_simple_binop (SImode, AND, gen_lowpart (SImode, t_3),
				 GEN_INT (0x07000000),
				 NULL_RTX, 1, OPTAB_DIRECT);
      /* sel = .......D */
      /* t_1 = .....C.. */
      /* t_2 = ...B.... */
      /* t_3 = .A...... */
      sel = expand_simple_binop (SImode, IOR, sel, t_1, sel, 1, OPTAB_DIRECT);
      t_2 = expand_simple_binop (SImode, IOR, t_2, t_3, t_2, 1, OPTAB_DIRECT);
      sel = expand_simple_binop (SImode, IOR, sel, t_2, sel, 1, OPTAB_DIRECT);
      /* sel = .A.B.C.D */
      sel = expand_mult (SImode, sel, GEN_INT (0x22), sel, 1);
      /* sel = AABBCCDD * 2 */
      t_1 = force_reg (SImode, GEN_INT (0x01010101));
      /* sel = { A*2, A*2+1, B*2, B*2+1, ... } */
      break;
  
    case E_V8QImode:
      /* input = xAxBxCxDxExFxGxH */
      sel = expand_simple_binop (DImode, AND, sel,
				 GEN_INT ((HOST_WIDE_INT)0x0f0f0f0f << 32
					  | 0x0f0f0f0f),
				 NULL_RTX, 1, OPTAB_DIRECT);
      /* sel = .A.B.C.D.E.F.G.H */
      t_1 = expand_simple_binop (DImode, LSHIFTRT, sel, GEN_INT (4),
				 NULL_RTX, 1, OPTAB_DIRECT);
      /* t_1 = ..A.B.C.D.E.F.G. */
      sel = expand_simple_binop (DImode, IOR, sel, t_1,
				 NULL_RTX, 1, OPTAB_DIRECT);
      /* sel = .AABBCCDDEEFFGGH */
      sel = expand_simple_binop (DImode, AND, sel,
				 GEN_INT ((HOST_WIDE_INT)0xff00ff << 32
					  | 0xff00ff),
				 NULL_RTX, 1, OPTAB_DIRECT);
      /* sel = ..AB..CD..EF..GH */
      t_1 = expand_simple_binop (DImode, LSHIFTRT, sel, GEN_INT (8),
				 NULL_RTX, 1, OPTAB_DIRECT);
      /* t_1 = ....AB..CD..EF.. */
      sel = expand_simple_binop (DImode, IOR, sel, t_1,
				 NULL_RTX, 1, OPTAB_DIRECT);
      /* sel = ..ABABCDCDEFEFGH */
      sel = expand_simple_binop (DImode, AND, sel,
				 GEN_INT ((HOST_WIDE_INT)0xffff << 32 | 0xffff),
				 NULL_RTX, 1, OPTAB_DIRECT);
      /* sel = ....ABCD....EFGH */
      t_1 = expand_simple_binop (DImode, LSHIFTRT, sel, GEN_INT (16),
				 NULL_RTX, 1, OPTAB_DIRECT);
      /* t_1 = ........ABCD.... */
      sel = gen_lowpart (SImode, sel);
      t_1 = gen_lowpart (SImode, t_1);
      break;

    default:
      gcc_unreachable ();
    }

  /* Always perform the final addition/merge within the bmask insn.  */
  emit_insn (gen_bmasksi_vis (gen_reg_rtx (SImode), sel, t_1));
}

/* Implement TARGET_FRAME_POINTER_REQUIRED.  */

static bool
sparc_frame_pointer_required (void)
{
  /* If the stack pointer is dynamically modified in the function, it cannot
     serve as the frame pointer.  */
  if (cfun->calls_alloca)
    return true;

  /* If the function receives nonlocal gotos, it needs to save the frame
     pointer in the nonlocal_goto_save_area object.  */
  if (cfun->has_nonlocal_label)
    return true;

  /* In flat mode, that's it.  */
  if (TARGET_FLAT)
    return false;

  /* Otherwise, the frame pointer is required if the function isn't leaf, but
     we cannot use sparc_leaf_function_p since it hasn't been computed yet.  */
  return !(optimize > 0 && crtl->is_leaf && only_leaf_regs_used ());
}

/* The way this is structured, we can't eliminate SFP in favor of SP
   if the frame pointer is required: we want to use the SFP->HFP elimination
   in that case.  But the test in update_eliminables doesn't know we are
   assuming below that we only do the former elimination.  */

static bool
sparc_can_eliminate (const int from ATTRIBUTE_UNUSED, const int to)
{
  return to == HARD_FRAME_POINTER_REGNUM || !sparc_frame_pointer_required ();
}

/* Return the hard frame pointer directly to bypass the stack bias.  */

static rtx
sparc_builtin_setjmp_frame_value (void)
{
  return hard_frame_pointer_rtx;
}

/* If !TARGET_FPU, then make the fp registers and fp cc regs fixed so that
   they won't be allocated.  */

static void
sparc_conditional_register_usage (void)
{
  if (PIC_OFFSET_TABLE_REGNUM != INVALID_REGNUM)
    {
      fixed_regs[PIC_OFFSET_TABLE_REGNUM] = 1;
      call_used_regs[PIC_OFFSET_TABLE_REGNUM] = 1;
    }
  /* If the user has passed -f{fixed,call-{used,saved}}-g5 */
  /* then honor it.  */
  if (TARGET_ARCH32 && fixed_regs[5])
    fixed_regs[5] = 1;
  else if (TARGET_ARCH64 && fixed_regs[5] == 2)
    fixed_regs[5] = 0;
  if (! TARGET_V9)
    {
      int regno;
      for (regno = SPARC_FIRST_V9_FP_REG;
	   regno <= SPARC_LAST_V9_FP_REG;
	   regno++)
	fixed_regs[regno] = 1;
      /* %fcc0 is used by v8 and v9.  */
      for (regno = SPARC_FIRST_V9_FCC_REG + 1;
	   regno <= SPARC_LAST_V9_FCC_REG;
	   regno++)
	fixed_regs[regno] = 1;
    }
  if (! TARGET_FPU)
    {
      int regno;
      for (regno = 32; regno < SPARC_LAST_V9_FCC_REG; regno++)
	fixed_regs[regno] = 1;
    }
  /* If the user has passed -f{fixed,call-{used,saved}}-g2 */
  /* then honor it.  Likewise with g3 and g4.  */
  if (fixed_regs[2] == 2)
    fixed_regs[2] = ! TARGET_APP_REGS;
  if (fixed_regs[3] == 2)
    fixed_regs[3] = ! TARGET_APP_REGS;
  if (TARGET_ARCH32 && fixed_regs[4] == 2)
    fixed_regs[4] = ! TARGET_APP_REGS;
  else if (TARGET_CM_EMBMEDANY)
    fixed_regs[4] = 1;
  else if (fixed_regs[4] == 2)
    fixed_regs[4] = 0;
  if (TARGET_FLAT)
    {
      int regno;
      /* Disable leaf functions.  */
      memset (sparc_leaf_regs, 0, FIRST_PSEUDO_REGISTER);
      for (regno = 0; regno < FIRST_PSEUDO_REGISTER; regno++)
	leaf_reg_remap [regno] = regno;
    }
  if (TARGET_VIS)
    global_regs[SPARC_GSR_REG] = 1;
}

/* Implement TARGET_PREFERRED_RELOAD_CLASS:

   - We can't load constants into FP registers.
   - We can't load FP constants into integer registers when soft-float,
     because there is no soft-float pattern with a r/F constraint.
   - We can't load FP constants into integer registers for TFmode unless
     it is 0.0L, because there is no movtf pattern with a r/F constraint.
   - Try and reload integer constants (symbolic or otherwise) back into
     registers directly, rather than having them dumped to memory.  */

static reg_class_t
sparc_preferred_reload_class (rtx x, reg_class_t rclass)
{
  machine_mode mode = GET_MODE (x);
  if (CONSTANT_P (x))
    {
      if (FP_REG_CLASS_P (rclass)
	  || rclass == GENERAL_OR_FP_REGS
	  || rclass == GENERAL_OR_EXTRA_FP_REGS
	  || (GET_MODE_CLASS (mode) == MODE_FLOAT && ! TARGET_FPU)
	  || (mode == TFmode && ! const_zero_operand (x, mode)))
	return NO_REGS;

      if (GET_MODE_CLASS (mode) == MODE_INT)
	return GENERAL_REGS;

      if (GET_MODE_CLASS (mode) == MODE_VECTOR_INT)
	{
	  if (! FP_REG_CLASS_P (rclass)
	      || !(const_zero_operand (x, mode)
		   || const_all_ones_operand (x, mode)))
	    return NO_REGS;
	}
    }

  if (TARGET_VIS3
      && ! TARGET_ARCH64
      && (rclass == EXTRA_FP_REGS
	  || rclass == GENERAL_OR_EXTRA_FP_REGS))
    {
      int regno = true_regnum (x);

      if (SPARC_INT_REG_P (regno))
	return (rclass == EXTRA_FP_REGS
		? FP_REGS : GENERAL_OR_FP_REGS);
    }

  return rclass;
}

/* Return true if we use LRA instead of reload pass.  */

static bool
sparc_lra_p (void)
{
  return TARGET_LRA;
}

/* Output a wide multiply instruction in V8+ mode.  INSN is the instruction,
   OPERANDS are its operands and OPCODE is the mnemonic to be used.  */

const char *
output_v8plus_mult (rtx_insn *insn, rtx *operands, const char *opcode)
{
  char mulstr[32];

  gcc_assert (! TARGET_ARCH64);

  if (sparc_check_64 (operands[1], insn) <= 0)
    output_asm_insn ("srl\t%L1, 0, %L1", operands);
  if (which_alternative == 1)
    output_asm_insn ("sllx\t%H1, 32, %H1", operands);
  if (GET_CODE (operands[2]) == CONST_INT)
    {
      if (which_alternative == 1)
	{
	  output_asm_insn ("or\t%L1, %H1, %H1", operands);
	  sprintf (mulstr, "%s\t%%H1, %%2, %%L0", opcode);
	  output_asm_insn (mulstr, operands);
	  return "srlx\t%L0, 32, %H0";
	}
      else
	{
	  output_asm_insn ("sllx\t%H1, 32, %3", operands);
          output_asm_insn ("or\t%L1, %3, %3", operands);
          sprintf (mulstr, "%s\t%%3, %%2, %%3", opcode);
	  output_asm_insn (mulstr, operands);
	  output_asm_insn ("srlx\t%3, 32, %H0", operands);
          return "mov\t%3, %L0";
	}
    }
  else if (rtx_equal_p (operands[1], operands[2]))
    {
      if (which_alternative == 1)
	{
	  output_asm_insn ("or\t%L1, %H1, %H1", operands);
          sprintf (mulstr, "%s\t%%H1, %%H1, %%L0", opcode);
	  output_asm_insn (mulstr, operands);
	  return "srlx\t%L0, 32, %H0";
	}
      else
	{
	  output_asm_insn ("sllx\t%H1, 32, %3", operands);
          output_asm_insn ("or\t%L1, %3, %3", operands);
	  sprintf (mulstr, "%s\t%%3, %%3, %%3", opcode);
	  output_asm_insn (mulstr, operands);
	  output_asm_insn ("srlx\t%3, 32, %H0", operands);
          return "mov\t%3, %L0";
	}
    }
  if (sparc_check_64 (operands[2], insn) <= 0)
    output_asm_insn ("srl\t%L2, 0, %L2", operands);
  if (which_alternative == 1)
    {
      output_asm_insn ("or\t%L1, %H1, %H1", operands);
      output_asm_insn ("sllx\t%H2, 32, %L1", operands);
      output_asm_insn ("or\t%L2, %L1, %L1", operands);
      sprintf (mulstr, "%s\t%%H1, %%L1, %%L0", opcode);
      output_asm_insn (mulstr, operands);
      return "srlx\t%L0, 32, %H0";
    }
  else
    {
      output_asm_insn ("sllx\t%H1, 32, %3", operands);
      output_asm_insn ("sllx\t%H2, 32, %4", operands);
      output_asm_insn ("or\t%L1, %3, %3", operands);
      output_asm_insn ("or\t%L2, %4, %4", operands);
      sprintf (mulstr, "%s\t%%3, %%4, %%3", opcode);
      output_asm_insn (mulstr, operands);
      output_asm_insn ("srlx\t%3, 32, %H0", operands);
      return "mov\t%3, %L0";
    }
}

/* Subroutine of sparc_expand_vector_init.  Emit code to initialize
   all fields of TARGET to ELT by means of VIS2 BSHUFFLE insn.  MODE
   and INNER_MODE are the modes describing TARGET.  */

static void
vector_init_bshuffle (rtx target, rtx elt, machine_mode mode,
		      machine_mode inner_mode)
{
  rtx t1, final_insn, sel;
  int bmask;

  t1 = gen_reg_rtx (mode);

  elt = convert_modes (SImode, inner_mode, elt, true);
  emit_move_insn (gen_lowpart(SImode, t1), elt);

  switch (mode)
    {
    case E_V2SImode:
      final_insn = gen_bshufflev2si_vis (target, t1, t1);
      bmask = 0x45674567;
      break;
    case E_V4HImode:
      final_insn = gen_bshufflev4hi_vis (target, t1, t1);
      bmask = 0x67676767;
      break;
    case E_V8QImode:
      final_insn = gen_bshufflev8qi_vis (target, t1, t1);
      bmask = 0x77777777;
      break;
    default:
      gcc_unreachable ();
    }

  sel = force_reg (SImode, GEN_INT (bmask));
  emit_insn (gen_bmasksi_vis (gen_reg_rtx (SImode), sel, const0_rtx));
  emit_insn (final_insn);
}

/* Subroutine of sparc_expand_vector_init.  Emit code to initialize
   all fields of TARGET to ELT in V8QI by means of VIS FPMERGE insn.  */

static void
vector_init_fpmerge (rtx target, rtx elt)
{
  rtx t1, t2, t2_low, t3, t3_low;

  t1 = gen_reg_rtx (V4QImode);
  elt = convert_modes (SImode, QImode, elt, true);
  emit_move_insn (gen_lowpart (SImode, t1), elt);

  t2 = gen_reg_rtx (V8QImode);
  t2_low = gen_lowpart (V4QImode, t2);
  emit_insn (gen_fpmerge_vis (t2, t1, t1));

  t3 = gen_reg_rtx (V8QImode);
  t3_low = gen_lowpart (V4QImode, t3);
  emit_insn (gen_fpmerge_vis (t3, t2_low, t2_low));

  emit_insn (gen_fpmerge_vis (target, t3_low, t3_low));
}

/* Subroutine of sparc_expand_vector_init.  Emit code to initialize
   all fields of TARGET to ELT in V4HI by means of VIS FALIGNDATA insn.  */

static void
vector_init_faligndata (rtx target, rtx elt)
{
  rtx t1 = gen_reg_rtx (V4HImode);
  int i;

  elt = convert_modes (SImode, HImode, elt, true);
  emit_move_insn (gen_lowpart (SImode, t1), elt);

  emit_insn (gen_alignaddrsi_vis (gen_reg_rtx (SImode),
				  force_reg (SImode, GEN_INT (6)),
				  const0_rtx));

  for (i = 0; i < 4; i++)
    emit_insn (gen_faligndatav4hi_vis (target, t1, target));
}

/* Emit code to initialize TARGET to values for individual fields VALS.  */

void
sparc_expand_vector_init (rtx target, rtx vals)
{
  const machine_mode mode = GET_MODE (target);
  const machine_mode inner_mode = GET_MODE_INNER (mode);
  const int n_elts = GET_MODE_NUNITS (mode);
  int i, n_var = 0;
  bool all_same = true;
  rtx mem;

  for (i = 0; i < n_elts; i++)
    {
      rtx x = XVECEXP (vals, 0, i);
      if (!(CONST_SCALAR_INT_P (x) || CONST_DOUBLE_P (x) || CONST_FIXED_P (x)))
	n_var++;

      if (i > 0 && !rtx_equal_p (x, XVECEXP (vals, 0, 0)))
	all_same = false;
    }

  if (n_var == 0)
    {
      emit_move_insn (target, gen_rtx_CONST_VECTOR (mode, XVEC (vals, 0)));
      return;
    }

  if (GET_MODE_SIZE (inner_mode) == GET_MODE_SIZE (mode))
    {
      if (GET_MODE_SIZE (inner_mode) == 4)
	{
	  emit_move_insn (gen_lowpart (SImode, target),
			  gen_lowpart (SImode, XVECEXP (vals, 0, 0)));
	  return;
	}
      else if (GET_MODE_SIZE (inner_mode) == 8)
	{
	  emit_move_insn (gen_lowpart (DImode, target),
			  gen_lowpart (DImode, XVECEXP (vals, 0, 0)));
	  return;
	}
    }
  else if (GET_MODE_SIZE (inner_mode) == GET_MODE_SIZE (word_mode)
	   && GET_MODE_SIZE (mode) == 2 * GET_MODE_SIZE (word_mode))
    {
      emit_move_insn (gen_highpart (word_mode, target),
		      gen_lowpart (word_mode, XVECEXP (vals, 0, 0)));
      emit_move_insn (gen_lowpart (word_mode, target),
		      gen_lowpart (word_mode, XVECEXP (vals, 0, 1)));
      return;
    }

  if (all_same && GET_MODE_SIZE (mode) == 8)
    {
      if (TARGET_VIS2)
	{
	  vector_init_bshuffle (target, XVECEXP (vals, 0, 0), mode, inner_mode);
	  return;
	}
      if (mode == V8QImode)
	{
	  vector_init_fpmerge (target, XVECEXP (vals, 0, 0));
	  return;
	}
      if (mode == V4HImode)
	{
	  vector_init_faligndata (target, XVECEXP (vals, 0, 0));
	  return;
	}
    }

  mem = assign_stack_temp (mode, GET_MODE_SIZE (mode));
  for (i = 0; i < n_elts; i++)
    emit_move_insn (adjust_address_nv (mem, inner_mode,
				       i * GET_MODE_SIZE (inner_mode)),
		    XVECEXP (vals, 0, i));
  emit_move_insn (target, mem);
}

/* Implement TARGET_SECONDARY_RELOAD.  */

static reg_class_t
sparc_secondary_reload (bool in_p, rtx x, reg_class_t rclass_i,
			machine_mode mode, secondary_reload_info *sri)
{
  enum reg_class rclass = (enum reg_class) rclass_i;

  sri->icode = CODE_FOR_nothing;
  sri->extra_cost = 0;

  /* We need a temporary when loading/storing a HImode/QImode value
     between memory and the FPU registers.  This can happen when combine puts
     a paradoxical subreg in a float/fix conversion insn.  */
  if (FP_REG_CLASS_P (rclass)
      && (mode == HImode || mode == QImode)
      && (GET_CODE (x) == MEM
	  || ((GET_CODE (x) == REG || GET_CODE (x) == SUBREG)
	      && true_regnum (x) == -1)))
    return GENERAL_REGS;

  /* On 32-bit we need a temporary when loading/storing a DFmode value
     between unaligned memory and the upper FPU registers.  */
  if (TARGET_ARCH32
      && rclass == EXTRA_FP_REGS
      && mode == DFmode
      && GET_CODE (x) == MEM
      && ! mem_min_alignment (x, 8))
    return FP_REGS;

  if (((TARGET_CM_MEDANY
	&& symbolic_operand (x, mode))
       || (TARGET_CM_EMBMEDANY
	   && text_segment_operand (x, mode)))
      && ! flag_pic)
    {
      if (in_p)
	sri->icode = direct_optab_handler (reload_in_optab, mode);
      else
	sri->icode = direct_optab_handler (reload_out_optab, mode);
      return NO_REGS;
    }

  if (TARGET_VIS3 && TARGET_ARCH32)
    {
      int regno = true_regnum (x);

      /* When using VIS3 fp<-->int register moves, on 32-bit we have
	 to move 8-byte values in 4-byte pieces.  This only works via
	 FP_REGS, and not via EXTRA_FP_REGS.  Therefore if we try to
	 move between EXTRA_FP_REGS and GENERAL_REGS, we will need
	 an FP_REGS intermediate move.  */
      if ((rclass == EXTRA_FP_REGS && SPARC_INT_REG_P (regno))
	  || ((general_or_i64_p (rclass)
	       || rclass == GENERAL_OR_FP_REGS)
	      && SPARC_FP_REG_P (regno)))
	{
	  sri->extra_cost = 2;
	  return FP_REGS;
	}
    }

  return NO_REGS;
}

/* Emit code to conditionally move either OPERANDS[2] or OPERANDS[3] into
   OPERANDS[0] in MODE.  OPERANDS[1] is the operator of the condition.  */

bool
sparc_expand_conditional_move (machine_mode mode, rtx *operands)
{
  enum rtx_code rc = GET_CODE (operands[1]);
  machine_mode cmp_mode;
  rtx cc_reg, dst, cmp;

  cmp = operands[1];
  if (GET_MODE (XEXP (cmp, 0)) == DImode && !TARGET_ARCH64)
    return false;

  if (GET_MODE (XEXP (cmp, 0)) == TFmode && !TARGET_HARD_QUAD)
    cmp = sparc_emit_float_lib_cmp (XEXP (cmp, 0), XEXP (cmp, 1), rc);

  cmp_mode = GET_MODE (XEXP (cmp, 0));
  rc = GET_CODE (cmp);

  dst = operands[0];
  if (! rtx_equal_p (operands[2], dst)
      && ! rtx_equal_p (operands[3], dst))
    {
      if (reg_overlap_mentioned_p (dst, cmp))
	dst = gen_reg_rtx (mode);

      emit_move_insn (dst, operands[3]);
    }
  else if (operands[2] == dst)
    {
      operands[2] = operands[3];

      if (GET_MODE_CLASS (cmp_mode) == MODE_FLOAT)
        rc = reverse_condition_maybe_unordered (rc);
      else
        rc = reverse_condition (rc);
    }

  if (XEXP (cmp, 1) == const0_rtx
      && GET_CODE (XEXP (cmp, 0)) == REG
      && cmp_mode == DImode
      && v9_regcmp_p (rc))
    cc_reg = XEXP (cmp, 0);
  else
    cc_reg = gen_compare_reg_1 (rc, XEXP (cmp, 0), XEXP (cmp, 1));

  cmp = gen_rtx_fmt_ee (rc, GET_MODE (cc_reg), cc_reg, const0_rtx);

  emit_insn (gen_rtx_SET (dst,
			  gen_rtx_IF_THEN_ELSE (mode, cmp, operands[2], dst)));

  if (dst != operands[0])
    emit_move_insn (operands[0], dst);

  return true;
}

/* Emit code to conditionally move a combination of OPERANDS[1] and OPERANDS[2]
   into OPERANDS[0] in MODE, depending on the outcome of the comparison of
   OPERANDS[4] and OPERANDS[5].  OPERANDS[3] is the operator of the condition.
   FCODE is the machine code to be used for OPERANDS[3] and CCODE the machine
   code to be used for the condition mask.  */

void
sparc_expand_vcond (machine_mode mode, rtx *operands, int ccode, int fcode)
{
  rtx mask, cop0, cop1, fcmp, cmask, bshuf, gsr;
  enum rtx_code code = GET_CODE (operands[3]);

  mask = gen_reg_rtx (Pmode);
  cop0 = operands[4];
  cop1 = operands[5];
  if (code == LT || code == GE)
    {
      rtx t;

      code = swap_condition (code);
      t = cop0; cop0 = cop1; cop1 = t;
    }

  gsr = gen_rtx_REG (DImode, SPARC_GSR_REG);

  fcmp = gen_rtx_UNSPEC (Pmode,
			 gen_rtvec (1, gen_rtx_fmt_ee (code, mode, cop0, cop1)),
			 fcode);

  cmask = gen_rtx_UNSPEC (DImode,
			  gen_rtvec (2, mask, gsr),
			  ccode);

  bshuf = gen_rtx_UNSPEC (mode,
			  gen_rtvec (3, operands[1], operands[2], gsr),
			  UNSPEC_BSHUFFLE);

  emit_insn (gen_rtx_SET (mask, fcmp));
  emit_insn (gen_rtx_SET (gsr, cmask));

  emit_insn (gen_rtx_SET (operands[0], bshuf));
}

/* On sparc, any mode which naturally allocates into the float
   registers should return 4 here.  */

unsigned int
sparc_regmode_natural_size (machine_mode mode)
{
  int size = UNITS_PER_WORD;

  if (TARGET_ARCH64)
    {
      enum mode_class mclass = GET_MODE_CLASS (mode);

      if (mclass == MODE_FLOAT || mclass == MODE_VECTOR_INT)
	size = 4;
    }

  return size;
}

/* Implement TARGET_HARD_REGNO_MODE_OK.

   ??? Because of the funny way we pass parameters we should allow certain
   ??? types of float/complex values to be in integer registers during
   ??? RTL generation.  This only matters on arch32.  */

static bool
sparc_hard_regno_mode_ok (unsigned int regno, machine_mode mode)
{
  return (hard_regno_mode_classes[regno] & sparc_mode_class[mode]) != 0;
}

/* Implement TARGET_MODES_TIEABLE_P.

   For V9 we have to deal with the fact that only the lower 32 floating
   point registers are 32-bit addressable.  */

static bool
sparc_modes_tieable_p (machine_mode mode1, machine_mode mode2)
{
  enum mode_class mclass1, mclass2;
  unsigned short size1, size2;

  if (mode1 == mode2)
    return true;

  mclass1 = GET_MODE_CLASS (mode1);
  mclass2 = GET_MODE_CLASS (mode2);
  if (mclass1 != mclass2)
    return false;

  if (! TARGET_V9)
    return true;

  /* Classes are the same and we are V9 so we have to deal with upper
     vs. lower floating point registers.  If one of the modes is a
     4-byte mode, and the other is not, we have to mark them as not
     tieable because only the lower 32 floating point register are
     addressable 32-bits at a time.

     We can't just test explicitly for SFmode, otherwise we won't
     cover the vector mode cases properly.  */

  if (mclass1 != MODE_FLOAT && mclass1 != MODE_VECTOR_INT)
    return true;

  size1 = GET_MODE_SIZE (mode1);
  size2 = GET_MODE_SIZE (mode2);
  if ((size1 > 4 && size2 == 4)
      || (size2 > 4 && size1 == 4))
    return false;

  return true;
}

/* Implement TARGET_CSTORE_MODE.  */

static scalar_int_mode
sparc_cstore_mode (enum insn_code icode ATTRIBUTE_UNUSED)
{
  return (TARGET_ARCH64 ? DImode : SImode);
}

/* Return the compound expression made of T1 and T2.  */

static inline tree
compound_expr (tree t1, tree t2)
{
  return build2 (COMPOUND_EXPR, void_type_node, t1, t2);
}

/* Implement TARGET_ATOMIC_ASSIGN_EXPAND_FENV hook.  */

static void
sparc_atomic_assign_expand_fenv (tree *hold, tree *clear, tree *update)
{
  if (!TARGET_FPU)
    return;

  const unsigned HOST_WIDE_INT accrued_exception_mask = 0x1f << 5;
  const unsigned HOST_WIDE_INT trap_enable_mask = 0x1f << 23;

  /* We generate the equivalent of feholdexcept (&fenv_var):

       unsigned int fenv_var;
       __builtin_store_fsr (&fenv_var);

       unsigned int tmp1_var;
       tmp1_var = fenv_var & ~(accrued_exception_mask | trap_enable_mask);

       __builtin_load_fsr (&tmp1_var);  */

  tree fenv_var = create_tmp_var_raw (unsigned_type_node);
  TREE_ADDRESSABLE (fenv_var) = 1;
  tree fenv_addr = build_fold_addr_expr (fenv_var);
  tree stfsr = sparc_builtins[SPARC_BUILTIN_STFSR];
  tree hold_stfsr
    = build4 (TARGET_EXPR, unsigned_type_node, fenv_var,
	      build_call_expr (stfsr, 1, fenv_addr), NULL_TREE, NULL_TREE);

  tree tmp1_var = create_tmp_var_raw (unsigned_type_node);
  TREE_ADDRESSABLE (tmp1_var) = 1;
  tree masked_fenv_var
    = build2 (BIT_AND_EXPR, unsigned_type_node, fenv_var,
	      build_int_cst (unsigned_type_node,
			     ~(accrued_exception_mask | trap_enable_mask)));
  tree hold_mask
    = build4 (TARGET_EXPR, unsigned_type_node, tmp1_var, masked_fenv_var,
	      NULL_TREE, NULL_TREE);

  tree tmp1_addr = build_fold_addr_expr (tmp1_var);
  tree ldfsr = sparc_builtins[SPARC_BUILTIN_LDFSR];
  tree hold_ldfsr = build_call_expr (ldfsr, 1, tmp1_addr);

  *hold = compound_expr (compound_expr (hold_stfsr, hold_mask), hold_ldfsr);

  /* We reload the value of tmp1_var to clear the exceptions:

       __builtin_load_fsr (&tmp1_var);  */

  *clear = build_call_expr (ldfsr, 1, tmp1_addr);

  /* We generate the equivalent of feupdateenv (&fenv_var):

       unsigned int tmp2_var;
       __builtin_store_fsr (&tmp2_var);

       __builtin_load_fsr (&fenv_var);

       if (SPARC_LOW_FE_EXCEPT_VALUES)
         tmp2_var >>= 5;
       __atomic_feraiseexcept ((int) tmp2_var);  */

  tree tmp2_var = create_tmp_var_raw (unsigned_type_node);
  TREE_ADDRESSABLE (tmp2_var) = 1;
  tree tmp2_addr = build_fold_addr_expr (tmp2_var);
  tree update_stfsr
    = build4 (TARGET_EXPR, unsigned_type_node, tmp2_var,
	      build_call_expr (stfsr, 1, tmp2_addr), NULL_TREE, NULL_TREE);

  tree update_ldfsr = build_call_expr (ldfsr, 1, fenv_addr);

  tree atomic_feraiseexcept
    = builtin_decl_implicit (BUILT_IN_ATOMIC_FERAISEEXCEPT);
  tree update_call
    = build_call_expr (atomic_feraiseexcept, 1,
		       fold_convert (integer_type_node, tmp2_var));

  if (SPARC_LOW_FE_EXCEPT_VALUES)
    {
      tree shifted_tmp2_var
	= build2 (RSHIFT_EXPR, unsigned_type_node, tmp2_var,
		  build_int_cst (unsigned_type_node, 5));
      tree update_shift
	= build2 (MODIFY_EXPR, void_type_node, tmp2_var, shifted_tmp2_var);
      update_call = compound_expr (update_shift, update_call);
    }

  *update
    = compound_expr (compound_expr (update_stfsr, update_ldfsr), update_call);
}

#include "gt-sparc.h"
