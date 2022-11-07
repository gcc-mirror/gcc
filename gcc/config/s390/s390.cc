/* Subroutines used for code generation on IBM S/390 and zSeries
   Copyright (C) 1999-2022 Free Software Foundation, Inc.
   Contributed by Hartmut Penner (hpenner@de.ibm.com) and
                  Ulrich Weigand (uweigand@de.ibm.com) and
                  Andreas Krebbel (Andreas.Krebbel@de.ibm.com).

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#define IN_TARGET_CODE 1

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "target.h"
#include "target-globals.h"
#include "rtl.h"
#include "tree.h"
#include "gimple.h"
#include "cfghooks.h"
#include "cfgloop.h"
#include "df.h"
#include "memmodel.h"
#include "tm_p.h"
#include "stringpool.h"
#include "attribs.h"
#include "expmed.h"
#include "optabs.h"
#include "regs.h"
#include "emit-rtl.h"
#include "recog.h"
#include "cgraph.h"
#include "diagnostic-core.h"
#include "diagnostic.h"
#include "alias.h"
#include "fold-const.h"
#include "print-tree.h"
#include "stor-layout.h"
#include "varasm.h"
#include "calls.h"
#include "conditions.h"
#include "output.h"
#include "insn-attr.h"
#include "flags.h"
#include "except.h"
#include "dojump.h"
#include "explow.h"
#include "stmt.h"
#include "expr.h"
#include "reload.h"
#include "cfgrtl.h"
#include "cfganal.h"
#include "lcm.h"
#include "cfgbuild.h"
#include "cfgcleanup.h"
#include "debug.h"
#include "langhooks.h"
#include "internal-fn.h"
#include "gimple-iterator.h"
#include "gimple-fold.h"
#include "tree-eh.h"
#include "gimplify.h"
#include "opts.h"
#include "tree-pass.h"
#include "context.h"
#include "builtins.h"
#include "rtl-iter.h"
#include "intl.h"
#include "tm-constrs.h"
#include "tree-vrp.h"
#include "symbol-summary.h"
#include "ipa-prop.h"
#include "ipa-fnsummary.h"
#include "sched-int.h"

/* This file should be included last.  */
#include "target-def.h"

static bool s390_hard_regno_mode_ok (unsigned int, machine_mode);

/* Remember the last target of s390_set_current_function.  */
static GTY(()) tree s390_previous_fndecl;

/* Define the specific costs for a given cpu.  */

struct processor_costs
{
  /* multiplication */
  const int m;        /* cost of an M instruction.  */
  const int mghi;     /* cost of an MGHI instruction.  */
  const int mh;       /* cost of an MH instruction.  */
  const int mhi;      /* cost of an MHI instruction.  */
  const int ml;       /* cost of an ML instruction.  */
  const int mr;       /* cost of an MR instruction.  */
  const int ms;       /* cost of an MS instruction.  */
  const int msg;      /* cost of an MSG instruction.  */
  const int msgf;     /* cost of an MSGF instruction.  */
  const int msgfr;    /* cost of an MSGFR instruction.  */
  const int msgr;     /* cost of an MSGR instruction.  */
  const int msr;      /* cost of an MSR instruction.  */
  const int mult_df;  /* cost of multiplication in DFmode.  */
  const int mxbr;
  /* square root */
  const int sqxbr;    /* cost of square root in TFmode.  */
  const int sqdbr;    /* cost of square root in DFmode.  */
  const int sqebr;    /* cost of square root in SFmode.  */
  /* multiply and add */
  const int madbr;    /* cost of multiply and add in DFmode.  */
  const int maebr;    /* cost of multiply and add in SFmode.  */
  /* division */
  const int dxbr;
  const int ddbr;
  const int debr;
  const int dlgr;
  const int dlr;
  const int dr;
  const int dsgfr;
  const int dsgr;
};

#define s390_cost ((const struct processor_costs *)(s390_cost_pointer))

static const
struct processor_costs z900_cost =
{
  COSTS_N_INSNS (5),     /* M     */
  COSTS_N_INSNS (10),    /* MGHI  */
  COSTS_N_INSNS (5),     /* MH    */
  COSTS_N_INSNS (4),     /* MHI   */
  COSTS_N_INSNS (5),     /* ML    */
  COSTS_N_INSNS (5),     /* MR    */
  COSTS_N_INSNS (4),     /* MS    */
  COSTS_N_INSNS (15),    /* MSG   */
  COSTS_N_INSNS (7),     /* MSGF  */
  COSTS_N_INSNS (7),     /* MSGFR */
  COSTS_N_INSNS (10),    /* MSGR  */
  COSTS_N_INSNS (4),     /* MSR   */
  COSTS_N_INSNS (7),     /* multiplication in DFmode */
  COSTS_N_INSNS (13),    /* MXBR */
  COSTS_N_INSNS (136),   /* SQXBR */
  COSTS_N_INSNS (44),    /* SQDBR */
  COSTS_N_INSNS (35),    /* SQEBR */
  COSTS_N_INSNS (18),    /* MADBR */
  COSTS_N_INSNS (13),    /* MAEBR */
  COSTS_N_INSNS (134),   /* DXBR */
  COSTS_N_INSNS (30),    /* DDBR */
  COSTS_N_INSNS (27),    /* DEBR */
  COSTS_N_INSNS (220),   /* DLGR */
  COSTS_N_INSNS (34),    /* DLR */
  COSTS_N_INSNS (34),    /* DR */
  COSTS_N_INSNS (32),    /* DSGFR */
  COSTS_N_INSNS (32),    /* DSGR */
};

static const
struct processor_costs z990_cost =
{
  COSTS_N_INSNS (4),     /* M     */
  COSTS_N_INSNS (2),     /* MGHI  */
  COSTS_N_INSNS (2),     /* MH    */
  COSTS_N_INSNS (2),     /* MHI   */
  COSTS_N_INSNS (4),     /* ML    */
  COSTS_N_INSNS (4),     /* MR    */
  COSTS_N_INSNS (5),     /* MS    */
  COSTS_N_INSNS (6),     /* MSG   */
  COSTS_N_INSNS (4),     /* MSGF  */
  COSTS_N_INSNS (4),     /* MSGFR */
  COSTS_N_INSNS (4),     /* MSGR  */
  COSTS_N_INSNS (4),     /* MSR   */
  COSTS_N_INSNS (1),     /* multiplication in DFmode */
  COSTS_N_INSNS (28),    /* MXBR */
  COSTS_N_INSNS (130),   /* SQXBR */
  COSTS_N_INSNS (66),    /* SQDBR */
  COSTS_N_INSNS (38),    /* SQEBR */
  COSTS_N_INSNS (1),     /* MADBR */
  COSTS_N_INSNS (1),     /* MAEBR */
  COSTS_N_INSNS (60),    /* DXBR */
  COSTS_N_INSNS (40),    /* DDBR */
  COSTS_N_INSNS (26),    /* DEBR */
  COSTS_N_INSNS (176),   /* DLGR */
  COSTS_N_INSNS (31),    /* DLR */
  COSTS_N_INSNS (31),    /* DR */
  COSTS_N_INSNS (31),    /* DSGFR */
  COSTS_N_INSNS (31),    /* DSGR */
};

static const
struct processor_costs z9_109_cost =
{
  COSTS_N_INSNS (4),     /* M     */
  COSTS_N_INSNS (2),     /* MGHI  */
  COSTS_N_INSNS (2),     /* MH    */
  COSTS_N_INSNS (2),     /* MHI   */
  COSTS_N_INSNS (4),     /* ML    */
  COSTS_N_INSNS (4),     /* MR    */
  COSTS_N_INSNS (5),     /* MS    */
  COSTS_N_INSNS (6),     /* MSG   */
  COSTS_N_INSNS (4),     /* MSGF  */
  COSTS_N_INSNS (4),     /* MSGFR */
  COSTS_N_INSNS (4),     /* MSGR  */
  COSTS_N_INSNS (4),     /* MSR   */
  COSTS_N_INSNS (1),     /* multiplication in DFmode */
  COSTS_N_INSNS (28),    /* MXBR */
  COSTS_N_INSNS (130),   /* SQXBR */
  COSTS_N_INSNS (66),    /* SQDBR */
  COSTS_N_INSNS (38),    /* SQEBR */
  COSTS_N_INSNS (1),     /* MADBR */
  COSTS_N_INSNS (1),     /* MAEBR */
  COSTS_N_INSNS (60),    /* DXBR */
  COSTS_N_INSNS (40),    /* DDBR */
  COSTS_N_INSNS (26),    /* DEBR */
  COSTS_N_INSNS (30),    /* DLGR */
  COSTS_N_INSNS (23),    /* DLR */
  COSTS_N_INSNS (23),    /* DR */
  COSTS_N_INSNS (24),    /* DSGFR */
  COSTS_N_INSNS (24),    /* DSGR */
};

static const
struct processor_costs z10_cost =
{
  COSTS_N_INSNS (10),    /* M     */
  COSTS_N_INSNS (10),    /* MGHI  */
  COSTS_N_INSNS (10),    /* MH    */
  COSTS_N_INSNS (10),    /* MHI   */
  COSTS_N_INSNS (10),    /* ML    */
  COSTS_N_INSNS (10),    /* MR    */
  COSTS_N_INSNS (10),    /* MS    */
  COSTS_N_INSNS (10),    /* MSG   */
  COSTS_N_INSNS (10),    /* MSGF  */
  COSTS_N_INSNS (10),    /* MSGFR */
  COSTS_N_INSNS (10),    /* MSGR  */
  COSTS_N_INSNS (10),    /* MSR   */
  COSTS_N_INSNS (1) ,    /* multiplication in DFmode */
  COSTS_N_INSNS (50),    /* MXBR */
  COSTS_N_INSNS (120),   /* SQXBR */
  COSTS_N_INSNS (52),    /* SQDBR */
  COSTS_N_INSNS (38),    /* SQEBR */
  COSTS_N_INSNS (1),     /* MADBR */
  COSTS_N_INSNS (1),     /* MAEBR */
  COSTS_N_INSNS (111),   /* DXBR */
  COSTS_N_INSNS (39),    /* DDBR */
  COSTS_N_INSNS (32),    /* DEBR */
  COSTS_N_INSNS (160),   /* DLGR */
  COSTS_N_INSNS (71),    /* DLR */
  COSTS_N_INSNS (71),    /* DR */
  COSTS_N_INSNS (71),    /* DSGFR */
  COSTS_N_INSNS (71),    /* DSGR */
};

static const
struct processor_costs z196_cost =
{
  COSTS_N_INSNS (7),     /* M     */
  COSTS_N_INSNS (5),     /* MGHI  */
  COSTS_N_INSNS (5),     /* MH    */
  COSTS_N_INSNS (5),     /* MHI   */
  COSTS_N_INSNS (7),     /* ML    */
  COSTS_N_INSNS (7),     /* MR    */
  COSTS_N_INSNS (6),     /* MS    */
  COSTS_N_INSNS (8),     /* MSG   */
  COSTS_N_INSNS (6),     /* MSGF  */
  COSTS_N_INSNS (6),     /* MSGFR */
  COSTS_N_INSNS (8),     /* MSGR  */
  COSTS_N_INSNS (6),     /* MSR   */
  COSTS_N_INSNS (1) ,    /* multiplication in DFmode */
  COSTS_N_INSNS (40),    /* MXBR B+40 */
  COSTS_N_INSNS (100),   /* SQXBR B+100 */
  COSTS_N_INSNS (42),    /* SQDBR B+42 */
  COSTS_N_INSNS (28),    /* SQEBR B+28 */
  COSTS_N_INSNS (1),     /* MADBR B */
  COSTS_N_INSNS (1),     /* MAEBR B */
  COSTS_N_INSNS (101),   /* DXBR B+101 */
  COSTS_N_INSNS (29),    /* DDBR */
  COSTS_N_INSNS (22),    /* DEBR */
  COSTS_N_INSNS (160),   /* DLGR cracked */
  COSTS_N_INSNS (160),   /* DLR cracked */
  COSTS_N_INSNS (160),   /* DR expanded */
  COSTS_N_INSNS (160),   /* DSGFR cracked */
  COSTS_N_INSNS (160),   /* DSGR cracked */
};

static const
struct processor_costs zEC12_cost =
{
  COSTS_N_INSNS (7),     /* M     */
  COSTS_N_INSNS (5),     /* MGHI  */
  COSTS_N_INSNS (5),     /* MH    */
  COSTS_N_INSNS (5),     /* MHI   */
  COSTS_N_INSNS (7),     /* ML    */
  COSTS_N_INSNS (7),     /* MR    */
  COSTS_N_INSNS (6),     /* MS    */
  COSTS_N_INSNS (8),     /* MSG   */
  COSTS_N_INSNS (6),     /* MSGF  */
  COSTS_N_INSNS (6),     /* MSGFR */
  COSTS_N_INSNS (8),     /* MSGR  */
  COSTS_N_INSNS (6),     /* MSR   */
  COSTS_N_INSNS (1) ,    /* multiplication in DFmode */
  COSTS_N_INSNS (40),    /* MXBR B+40 */
  COSTS_N_INSNS (100),   /* SQXBR B+100 */
  COSTS_N_INSNS (42),    /* SQDBR B+42 */
  COSTS_N_INSNS (28),    /* SQEBR B+28 */
  COSTS_N_INSNS (1),     /* MADBR B */
  COSTS_N_INSNS (1),     /* MAEBR B */
  COSTS_N_INSNS (131),   /* DXBR B+131 */
  COSTS_N_INSNS (29),    /* DDBR */
  COSTS_N_INSNS (22),    /* DEBR */
  COSTS_N_INSNS (160),   /* DLGR cracked */
  COSTS_N_INSNS (160),   /* DLR cracked */
  COSTS_N_INSNS (160),   /* DR expanded */
  COSTS_N_INSNS (160),   /* DSGFR cracked */
  COSTS_N_INSNS (160),   /* DSGR cracked */
};

const struct s390_processor processor_table[] =
{
  { "z900",   "z900",   PROCESSOR_2064_Z900,   &z900_cost,   5  },
  { "z990",   "z990",   PROCESSOR_2084_Z990,   &z990_cost,   6  },
  { "z9-109", "z9-109", PROCESSOR_2094_Z9_109, &z9_109_cost, 7  },
  { "z9-ec",  "z9-ec",  PROCESSOR_2094_Z9_EC,  &z9_109_cost, 7  },
  { "z10",    "z10",    PROCESSOR_2097_Z10,    &z10_cost,    8  },
  { "z196",   "z196",   PROCESSOR_2817_Z196,   &z196_cost,   9  },
  { "zEC12",  "zEC12",  PROCESSOR_2827_ZEC12,  &zEC12_cost,  10 },
  { "z13",    "z13",    PROCESSOR_2964_Z13,    &zEC12_cost,  11 },
  { "z14",    "arch12", PROCESSOR_3906_Z14,    &zEC12_cost,  12 },
  { "z15",    "arch13", PROCESSOR_8561_Z15,    &zEC12_cost,  13 },
  { "z16",    "arch14", PROCESSOR_3931_Z16,    &zEC12_cost,  14 },
  { "native", "",       PROCESSOR_NATIVE,      NULL,         0  }
};

extern int reload_completed;

/* Kept up to date using the SCHED_VARIABLE_ISSUE hook.  */
static rtx_insn *last_scheduled_insn;
#define NUM_SIDES 2

#define MAX_SCHED_UNITS 4
static int last_scheduled_unit_distance[MAX_SCHED_UNITS][NUM_SIDES];

/* Estimate of number of cycles a long-running insn occupies an
   execution unit.  */
static int fxd_longrunning[NUM_SIDES];
static int fpd_longrunning[NUM_SIDES];

/* The maximum score added for an instruction whose unit hasn't been
   in use for MAX_SCHED_MIX_DISTANCE steps.  Increase this value to
   give instruction mix scheduling more priority over instruction
   grouping.  */
#define MAX_SCHED_MIX_SCORE      2

/* The maximum distance up to which individual scores will be
   calculated.  Everything beyond this gives MAX_SCHED_MIX_SCORE.
   Increase this with the OOO windows size of the machine.  */
#define MAX_SCHED_MIX_DISTANCE 70

/* Structure used to hold the components of a S/390 memory
   address.  A legitimate address on S/390 is of the general
   form
          base + index + displacement
   where any of the components is optional.

   base and index are registers of the class ADDR_REGS,
   displacement is an unsigned 12-bit immediate constant.  */

/* The max number of insns of backend generated memset/memcpy/memcmp
   loops.  This value is used in the unroll adjust hook to detect such
   loops.  Current max is 9 coming from the memcmp loop.  */
#define BLOCK_MEM_OPS_LOOP_INSNS 9

struct s390_address
{
  rtx base;
  rtx indx;
  rtx disp;
  bool pointer;
  bool literal_pool;
};

/* Few accessor macros for struct cfun->machine->s390_frame_layout.  */

#define cfun_frame_layout (cfun->machine->frame_layout)
#define cfun_save_high_fprs_p (!!cfun_frame_layout.high_fprs)
#define cfun_save_arg_fprs_p (!!(TARGET_64BIT				\
				 ? cfun_frame_layout.fpr_bitmap & 0x0f	\
				 : cfun_frame_layout.fpr_bitmap & 0x03))
#define cfun_gprs_save_area_size ((cfun_frame_layout.last_save_gpr_slot - \
  cfun_frame_layout.first_save_gpr_slot + 1) * UNITS_PER_LONG)
#define cfun_set_fpr_save(REGNO) (cfun->machine->frame_layout.fpr_bitmap |=    \
  (1 << (REGNO - FPR0_REGNUM)))
#define cfun_fpr_save_p(REGNO) (!!(cfun->machine->frame_layout.fpr_bitmap &    \
  (1 << (REGNO - FPR0_REGNUM))))
#define cfun_gpr_save_slot(REGNO) \
  cfun->machine->frame_layout.gpr_save_slots[REGNO]

/* Number of GPRs and FPRs used for argument passing.  */
#define GP_ARG_NUM_REG 5
#define FP_ARG_NUM_REG (TARGET_64BIT? 4 : 2)
#define VEC_ARG_NUM_REG 8

/* A couple of shortcuts.  */
#define CONST_OK_FOR_J(x) \
	CONST_OK_FOR_CONSTRAINT_P((x), 'J', "J")
#define CONST_OK_FOR_K(x) \
	CONST_OK_FOR_CONSTRAINT_P((x), 'K', "K")
#define CONST_OK_FOR_Os(x) \
	CONST_OK_FOR_CONSTRAINT_P((x), 'O', "Os")
#define CONST_OK_FOR_Op(x) \
	CONST_OK_FOR_CONSTRAINT_P((x), 'O', "Op")
#define CONST_OK_FOR_On(x) \
	CONST_OK_FOR_CONSTRAINT_P((x), 'O', "On")

#define REGNO_PAIR_OK(REGNO, MODE)                               \
  (s390_hard_regno_nregs ((REGNO), (MODE)) == 1 || !((REGNO) & 1))

/* That's the read ahead of the dynamic branch prediction unit in
   bytes on a z10 (or higher) CPU.  */
#define PREDICT_DISTANCE (TARGET_Z10 ? 384 : 2048)

static int
s390_address_cost (rtx addr, machine_mode mode ATTRIBUTE_UNUSED,
		   addr_space_t as ATTRIBUTE_UNUSED,
		   bool speed ATTRIBUTE_UNUSED);

static unsigned int
s390_hard_regno_nregs (unsigned int regno, machine_mode mode);

/* Masks per jump target register indicating which thunk need to be
   generated.  */
static GTY(()) int indirect_branch_prez10thunk_mask = 0;
static GTY(()) int indirect_branch_z10thunk_mask = 0;

#define INDIRECT_BRANCH_NUM_OPTIONS 4

enum s390_indirect_branch_option
  {
    s390_opt_indirect_branch_jump = 0,
    s390_opt_indirect_branch_call,
    s390_opt_function_return_reg,
    s390_opt_function_return_mem
  };

static GTY(()) int indirect_branch_table_label_no[INDIRECT_BRANCH_NUM_OPTIONS] = { 0 };
const char *indirect_branch_table_label[INDIRECT_BRANCH_NUM_OPTIONS] = \
  { "LJUMP", "LCALL", "LRETREG", "LRETMEM" };
const char *indirect_branch_table_name[INDIRECT_BRANCH_NUM_OPTIONS] =	\
  { ".s390_indirect_jump", ".s390_indirect_call",
    ".s390_return_reg", ".s390_return_mem" };

bool
s390_return_addr_from_memory ()
{
  return cfun_gpr_save_slot(RETURN_REGNUM) == SAVE_SLOT_STACK;
}

/* Generate a SUBREG for the MODE lowpart of EXPR.

   In contrast to gen_lowpart it will always return a SUBREG
   expression.  This is useful to generate STRICT_LOW_PART
   expressions.  */
rtx
s390_gen_lowpart_subreg (machine_mode mode, rtx expr)
{
  rtx lowpart = gen_lowpart (mode, expr);

  /* There might be no SUBREG in case it could be applied to the hard
     REG rtx or it could be folded with a paradoxical subreg.  Bring
     it back.  */
  if (!SUBREG_P (lowpart))
    {
      machine_mode reg_mode = TARGET_ZARCH ? DImode : SImode;
      gcc_assert (REG_P (lowpart));
      lowpart = gen_lowpart_SUBREG (mode,
				    gen_rtx_REG (reg_mode,
						 REGNO (lowpart)));
    }

  return lowpart;
}

/* Return nonzero if it's OK to use fused multiply-add for MODE.  */
bool
s390_fma_allowed_p (machine_mode mode)
{
  if (TARGET_VXE && mode == TFmode)
    return flag_vx_long_double_fma;

  return true;
}

/* Indicate which ABI has been used for passing vector args.
   0 - no vector type arguments have been passed where the ABI is relevant
   1 - the old ABI has been used
   2 - a vector type argument has been passed either in a vector register
       or on the stack by value  */
static int s390_vector_abi = 0;

/* Set the vector ABI marker if TYPE is subject to the vector ABI
   switch.  The vector ABI affects only vector data types.  There are
   two aspects of the vector ABI relevant here:

   1. vectors >= 16 bytes have an alignment of 8 bytes with the new
   ABI and natural alignment with the old.

   2. vector <= 16 bytes are passed in VRs or by value on the stack
   with the new ABI but by reference on the stack with the old.

   If ARG_P is true TYPE is used for a function argument or return
   value.  The ABI marker then is set for all vector data types.  If
   ARG_P is false only type 1 vectors are being checked.  */

static void
s390_check_type_for_vector_abi (const_tree type, bool arg_p, bool in_struct_p)
{
  static hash_set<const_tree> visited_types_hash;

  if (s390_vector_abi)
    return;

  if (type == NULL_TREE || TREE_CODE (type) == ERROR_MARK)
    return;

  if (visited_types_hash.contains (type))
    return;

  visited_types_hash.add (type);

  if (VECTOR_TYPE_P (type))
    {
      int type_size = int_size_in_bytes (type);

      /* Outside arguments only the alignment is changing and this
	 only happens for vector types >= 16 bytes.  */
      if (!arg_p && type_size < 16)
	return;

      /* In arguments vector types > 16 are passed as before (GCC
	 never enforced the bigger alignment for arguments which was
	 required by the old vector ABI).  However, it might still be
	 ABI relevant due to the changed alignment if it is a struct
	 member.  */
      if (arg_p && type_size > 16 && !in_struct_p)
	return;

      s390_vector_abi = TARGET_VX_ABI ? 2 : 1;
    }
  else if (POINTER_TYPE_P (type) || TREE_CODE (type) == ARRAY_TYPE)
    {
      /* ARRAY_TYPE: Since with neither of the ABIs we have more than
	 natural alignment there will never be ABI dependent padding
	 in an array type.  That's why we do not set in_struct_p to
	 true here.  */
      s390_check_type_for_vector_abi (TREE_TYPE (type), arg_p, in_struct_p);
    }
  else if (TREE_CODE (type) == FUNCTION_TYPE || TREE_CODE (type) == METHOD_TYPE)
    {
      tree arg_chain;

      /* Check the return type.  */
      s390_check_type_for_vector_abi (TREE_TYPE (type), true, false);

      for (arg_chain = TYPE_ARG_TYPES (type);
	   arg_chain;
	   arg_chain = TREE_CHAIN (arg_chain))
	s390_check_type_for_vector_abi (TREE_VALUE (arg_chain), true, false);
    }
  else if (RECORD_OR_UNION_TYPE_P (type))
    {
      tree field;

      for (field = TYPE_FIELDS (type); field; field = DECL_CHAIN (field))
	{
	  if (TREE_CODE (field) != FIELD_DECL)
	    continue;

	  s390_check_type_for_vector_abi (TREE_TYPE (field), arg_p, true);
	}
    }
}


/* System z builtins.  */

#include "s390-builtins.h"

const unsigned int bflags_builtin[S390_BUILTIN_MAX + 1] =
  {
#undef B_DEF
#undef OB_DEF
#undef OB_DEF_VAR
#define B_DEF(NAME, PATTERN, ATTRS, BFLAGS, ...) BFLAGS,
#define OB_DEF(...)
#define OB_DEF_VAR(...)
#include "s390-builtins.def"
    0
  };

const unsigned int opflags_builtin[S390_BUILTIN_MAX + 1] =
  {
#undef B_DEF
#undef OB_DEF
#undef OB_DEF_VAR
#define B_DEF(NAME, PATTERN, ATTRS, BFLAGS, OPFLAGS, ...) OPFLAGS,
#define OB_DEF(...)
#define OB_DEF_VAR(...)
#include "s390-builtins.def"
    0
  };

const unsigned int bflags_overloaded_builtin[S390_OVERLOADED_BUILTIN_MAX + 1] =
  {
#undef B_DEF
#undef OB_DEF
#undef OB_DEF_VAR
#define B_DEF(...)
#define OB_DEF(NAME, FIRST_VAR_NAME, LAST_VAR_NAME, BFLAGS, ...) BFLAGS,
#define OB_DEF_VAR(...)
#include "s390-builtins.def"
    0
  };

const unsigned int
bflags_overloaded_builtin_var[S390_OVERLOADED_BUILTIN_VAR_MAX + 1] =
  {
#undef B_DEF
#undef OB_DEF
#undef OB_DEF_VAR
#define B_DEF(...)
#define OB_DEF(...)
#define OB_DEF_VAR(NAME, PATTERN, FLAGS, OPFLAGS, FNTYPE) FLAGS,
#include "s390-builtins.def"
    0
  };

const unsigned int
opflags_overloaded_builtin_var[S390_OVERLOADED_BUILTIN_VAR_MAX + 1] =
  {
#undef B_DEF
#undef OB_DEF
#undef OB_DEF_VAR
#define B_DEF(...)
#define OB_DEF(...)
#define OB_DEF_VAR(NAME, PATTERN, FLAGS, OPFLAGS, FNTYPE) OPFLAGS,
#include "s390-builtins.def"
    0
  };

tree s390_builtin_types[BT_MAX];
tree s390_builtin_fn_types[BT_FN_MAX];
tree s390_builtin_decls[S390_BUILTIN_MAX +
			S390_OVERLOADED_BUILTIN_MAX +
			S390_OVERLOADED_BUILTIN_VAR_MAX];

static enum insn_code const code_for_builtin[S390_BUILTIN_MAX + 1] = {
#undef B_DEF
#undef OB_DEF
#undef OB_DEF_VAR
#define B_DEF(NAME, PATTERN, ...) CODE_FOR_##PATTERN,
#define OB_DEF(...)
#define OB_DEF_VAR(...)

#include "s390-builtins.def"
  CODE_FOR_nothing
};

static void
s390_init_builtins (void)
{
  /* These definitions are being used in s390-builtins.def.  */
  tree returns_twice_attr = tree_cons (get_identifier ("returns_twice"),
				       NULL, NULL);
  tree noreturn_attr = tree_cons (get_identifier ("noreturn"), NULL, NULL);
  tree c_uint64_type_node;

  /* The uint64_type_node from tree.cc is not compatible to the C99
     uint64_t data type.  What we want is c_uint64_type_node from
     c-common.cc.  But since backend code is not supposed to interface
     with the frontend we recreate it here.  */
  if (TARGET_64BIT)
    c_uint64_type_node = long_unsigned_type_node;
  else
    c_uint64_type_node = long_long_unsigned_type_node;

#undef DEF_TYPE
#define DEF_TYPE(INDEX, NODE, CONST_P)			\
  if (s390_builtin_types[INDEX] == NULL)		\
    s390_builtin_types[INDEX] = (!CONST_P) ?		\
      (NODE) : build_type_variant ((NODE), 1, 0);

#undef DEF_POINTER_TYPE
#define DEF_POINTER_TYPE(INDEX, INDEX_BASE)				\
  if (s390_builtin_types[INDEX] == NULL)				\
    s390_builtin_types[INDEX] =						\
      build_pointer_type (s390_builtin_types[INDEX_BASE]);

#undef DEF_DISTINCT_TYPE
#define DEF_DISTINCT_TYPE(INDEX, INDEX_BASE)				\
  if (s390_builtin_types[INDEX] == NULL)				\
    s390_builtin_types[INDEX] =						\
      build_distinct_type_copy (s390_builtin_types[INDEX_BASE]);

#undef DEF_VECTOR_TYPE
#define DEF_VECTOR_TYPE(INDEX, INDEX_BASE, ELEMENTS)			\
  if (s390_builtin_types[INDEX] == NULL)				\
    s390_builtin_types[INDEX] =						\
      build_vector_type (s390_builtin_types[INDEX_BASE], ELEMENTS);

#undef DEF_OPAQUE_VECTOR_TYPE
#define DEF_OPAQUE_VECTOR_TYPE(INDEX, INDEX_BASE, ELEMENTS)		\
  if (s390_builtin_types[INDEX] == NULL)				\
    s390_builtin_types[INDEX] =						\
      build_opaque_vector_type (s390_builtin_types[INDEX_BASE], ELEMENTS);

#undef DEF_FN_TYPE
#define DEF_FN_TYPE(INDEX, args...)				\
  if (s390_builtin_fn_types[INDEX] == NULL)			\
    s390_builtin_fn_types[INDEX] =				\
      build_function_type_list (args, NULL_TREE);
#undef DEF_OV_TYPE
#define DEF_OV_TYPE(...)
#include "s390-builtin-types.def"

#undef B_DEF
#define B_DEF(NAME, PATTERN, ATTRS, BFLAGS, OPFLAGS, FNTYPE)		\
  if (s390_builtin_decls[S390_BUILTIN_##NAME] == NULL)			\
    s390_builtin_decls[S390_BUILTIN_##NAME] =				\
      add_builtin_function ("__builtin_" #NAME,				\
			    s390_builtin_fn_types[FNTYPE],		\
			    S390_BUILTIN_##NAME,			\
			    BUILT_IN_MD,				\
			    NULL,					\
			    ATTRS);
#undef OB_DEF
#define OB_DEF(NAME, FIRST_VAR_NAME, LAST_VAR_NAME, BFLAGS, FNTYPE)	\
  if (s390_builtin_decls[S390_OVERLOADED_BUILTIN_##NAME + S390_BUILTIN_MAX] \
      == NULL)								\
    s390_builtin_decls[S390_OVERLOADED_BUILTIN_##NAME + S390_BUILTIN_MAX] = \
      add_builtin_function ("__builtin_" #NAME,				\
			    s390_builtin_fn_types[FNTYPE],		\
			    S390_OVERLOADED_BUILTIN_##NAME + S390_BUILTIN_MAX, \
			    BUILT_IN_MD,				\
			    NULL,					\
			    0);
#undef OB_DEF_VAR
#define OB_DEF_VAR(...)
#include "s390-builtins.def"

}

/* Return true if ARG is appropriate as argument number ARGNUM of
   builtin DECL.  The operand flags from s390-builtins.def have to
   passed as OP_FLAGS.  */
bool
s390_const_operand_ok (tree arg, int argnum, int op_flags, tree decl)
{
  if (O_UIMM_P (op_flags))
    {
      unsigned HOST_WIDE_INT bitwidths[] = { 1, 2, 3, 4, 5, 8, 12, 16, 32, 4 };
      unsigned HOST_WIDE_INT bitmasks[]  = { 0, 0, 0, 0, 0, 0,  0,  0,  0, 12 };
      unsigned HOST_WIDE_INT bitwidth = bitwidths[op_flags - O_U1];
      unsigned HOST_WIDE_INT bitmask = bitmasks[op_flags - O_U1];

      gcc_assert(ARRAY_SIZE(bitwidths) == (O_M12 - O_U1 + 1));
      gcc_assert(ARRAY_SIZE(bitmasks) == (O_M12 - O_U1 + 1));

      if (!tree_fits_uhwi_p (arg)
	  || tree_to_uhwi (arg) > (HOST_WIDE_INT_1U << bitwidth) - 1
	  || (bitmask && tree_to_uhwi (arg) & ~bitmask))
	{
	  if (bitmask)
	    {
	      gcc_assert (bitmask < 16);
	      char values[120] = "";

	      for (unsigned HOST_WIDE_INT i = 0; i <= bitmask; i++)
		{
		  char buf[5];
		  if (i & ~bitmask)
		    continue;
		  int ret = snprintf (buf, 5, HOST_WIDE_INT_PRINT_UNSIGNED, i & bitmask);
		  gcc_assert (ret < 5);
		  strcat (values, buf);
		  if (i < bitmask)
		    strcat (values, ", ");
		}
	      error ("constant argument %d for builtin %qF is invalid (%s)",
		     argnum, decl, values);
	    }
	  else
	    error ("constant argument %d for builtin %qF is out of range (0-%wu)",
		   argnum, decl, (HOST_WIDE_INT_1U << bitwidth) - 1);

	  return false;
	}
    }

  if (O_SIMM_P (op_flags))
    {
      int bitwidths[] = { 2, 3, 4, 5, 8, 12, 16, 32 };
      int bitwidth = bitwidths[op_flags - O_S2];

      if (!tree_fits_shwi_p (arg)
	  || tree_to_shwi (arg) < -(HOST_WIDE_INT_1 << (bitwidth - 1))
	  || tree_to_shwi (arg) > ((HOST_WIDE_INT_1 << (bitwidth - 1)) - 1))
	{
	  error ("constant argument %d for builtin %qF is out of range "
		 "(%wd-%wd)", argnum, decl,
		 -(HOST_WIDE_INT_1 << (bitwidth - 1)),
		 (HOST_WIDE_INT_1 << (bitwidth - 1)) - 1);
	  return false;
	}
    }
  return true;
}

/* Expand an expression EXP that calls a built-in function,
   with result going to TARGET if that's convenient
   (and in mode MODE if that's convenient).
   SUBTARGET may be used as the target for computing one of EXP's operands.
   IGNORE is nonzero if the value is to be ignored.  */

static rtx
s390_expand_builtin (tree exp, rtx target, rtx subtarget ATTRIBUTE_UNUSED,
		     machine_mode mode ATTRIBUTE_UNUSED,
		     int ignore ATTRIBUTE_UNUSED)
{
#define MAX_ARGS 6

  tree fndecl = TREE_OPERAND (CALL_EXPR_FN (exp), 0);
  unsigned int fcode = DECL_MD_FUNCTION_CODE (fndecl);
  enum insn_code icode;
  rtx op[MAX_ARGS], pat;
  int arity;
  bool nonvoid;
  tree arg;
  call_expr_arg_iterator iter;
  unsigned int all_op_flags = opflags_for_builtin (fcode);
  machine_mode last_vec_mode = VOIDmode;

  if (TARGET_DEBUG_ARG)
    {
      fprintf (stderr,
	       "s390_expand_builtin, code = %4d, %s, bflags = 0x%x\n",
	       (int)fcode, IDENTIFIER_POINTER (DECL_NAME (fndecl)),
	       bflags_for_builtin (fcode));
    }

  if (S390_USE_TARGET_ATTRIBUTE)
    {
      unsigned int bflags;

      bflags = bflags_for_builtin (fcode);
      if ((bflags & B_HTM) && !TARGET_HTM)
	{
	  error ("builtin %qF is not supported without %<-mhtm%> "
		 "(default with %<-march=zEC12%> and higher)", fndecl);
	  return const0_rtx;
	}
      if (((bflags & B_VX) || (bflags & B_VXE)) && !TARGET_VX)
	{
	  error ("builtin %qF requires %<-mvx%> "
		 "(default with %<-march=z13%> and higher)", fndecl);
	  return const0_rtx;
	}

      if ((bflags & B_VXE) && !TARGET_VXE)
	{
	  error ("Builtin %qF requires z14 or higher", fndecl);
	  return const0_rtx;
	}

      if ((bflags & B_VXE2) && !TARGET_VXE2)
	{
	  error ("Builtin %qF requires z15 or higher", fndecl);
	  return const0_rtx;
	}
    }
  if (fcode >= S390_OVERLOADED_BUILTIN_VAR_OFFSET
      && fcode < S390_ALL_BUILTIN_MAX)
    {
      gcc_unreachable ();
    }
  else if (fcode < S390_OVERLOADED_BUILTIN_OFFSET)
    {
      icode = code_for_builtin[fcode];
      /* Set a flag in the machine specific cfun part in order to support
	 saving/restoring of FPRs.  */
      if (fcode == S390_BUILTIN_tbegin || fcode == S390_BUILTIN_tbegin_retry)
	cfun->machine->tbegin_p = true;
    }
  else if (fcode < S390_OVERLOADED_BUILTIN_VAR_OFFSET)
    {
      error ("unresolved overloaded builtin");
      return const0_rtx;
    }
  else
    internal_error ("bad builtin fcode");

  if (icode == 0)
    internal_error ("bad builtin icode");

  nonvoid = TREE_TYPE (TREE_TYPE (fndecl)) != void_type_node;

  if (nonvoid)
    {
      machine_mode tmode = insn_data[icode].operand[0].mode;
      if (!target
	  || GET_MODE (target) != tmode
	  || !(*insn_data[icode].operand[0].predicate) (target, tmode))
	target = gen_reg_rtx (tmode);

      /* There are builtins (e.g. vec_promote) with no vector
	 arguments but an element selector.  So we have to also look
	 at the vector return type when emitting the modulo
	 operation.  */
      if (VECTOR_MODE_P (insn_data[icode].operand[0].mode))
	last_vec_mode = insn_data[icode].operand[0].mode;
    }

  arity = 0;
  FOR_EACH_CALL_EXPR_ARG (arg, iter, exp)
    {
      rtx tmp_rtx;
      const struct insn_operand_data *insn_op;
      unsigned int op_flags = all_op_flags & ((1 << O_SHIFT) - 1);

      all_op_flags = all_op_flags >> O_SHIFT;

      if (arg == error_mark_node)
	return NULL_RTX;
      if (arity >= MAX_ARGS)
	return NULL_RTX;

      if (O_IMM_P (op_flags)
	  && TREE_CODE (arg) != INTEGER_CST)
	{
	  error ("constant value required for builtin %qF argument %d",
		 fndecl, arity + 1);
	  return const0_rtx;
	}

      if (!s390_const_operand_ok (arg, arity + 1, op_flags, fndecl))
	return const0_rtx;

      insn_op = &insn_data[icode].operand[arity + nonvoid];
      op[arity] = expand_expr (arg, NULL_RTX, insn_op->mode, EXPAND_NORMAL);

      /* expand_expr truncates constants to the target mode only if it
	 is "convenient".  However, our checks below rely on this
	 being done.  */
      if (CONST_INT_P (op[arity])
	  && SCALAR_INT_MODE_P (insn_op->mode)
	  && GET_MODE (op[arity]) != insn_op->mode)
	op[arity] = GEN_INT (trunc_int_for_mode (INTVAL (op[arity]),
						 insn_op->mode));

      /* Wrap the expanded RTX for pointer types into a MEM expr with
	 the proper mode.  This allows us to use e.g. (match_operand
	 "memory_operand"..) in the insn patterns instead of (mem
	 (match_operand "address_operand)).  This is helpful for
	 patterns not just accepting MEMs.  */
      if (POINTER_TYPE_P (TREE_TYPE (arg))
	  && insn_op->predicate != address_operand)
	op[arity] = gen_rtx_MEM (insn_op->mode, op[arity]);

      /* Expand the module operation required on element selectors.  */
      if (op_flags == O_ELEM)
	{
	  gcc_assert (last_vec_mode != VOIDmode);
	  op[arity] = simplify_expand_binop (SImode, code_to_optab (AND),
					     op[arity],
					     GEN_INT (GET_MODE_NUNITS (last_vec_mode) - 1),
					     NULL_RTX, 1, OPTAB_DIRECT);
	}

      /* Record the vector mode used for an element selector.  This assumes:
	 1. There is no builtin with two different vector modes and an element selector
	 2. The element selector comes after the vector type it is referring to.
	 This currently the true for all the builtins but FIXME we
	 should better check for that.  */
      if (VECTOR_MODE_P (insn_op->mode))
	last_vec_mode = insn_op->mode;

      if (insn_op->predicate (op[arity], insn_op->mode))
	{
	  arity++;
	  continue;
	}

      /* A memory operand is rejected by the memory_operand predicate.
	 Try making the address legal by copying it into a register.  */
      if (MEM_P (op[arity])
	  && insn_op->predicate == memory_operand
	  && (GET_MODE (XEXP (op[arity], 0)) == Pmode
	      || GET_MODE (XEXP (op[arity], 0)) == VOIDmode))
	{
	  op[arity] = replace_equiv_address (op[arity],
					     copy_to_mode_reg (Pmode,
					       XEXP (op[arity], 0)));
	}
      /* Some of the builtins require different modes/types than the
	 pattern in order to implement a specific API.  Instead of
	 adding many expanders which do the mode change we do it here.
	 E.g. s390_vec_add_u128 required to have vector unsigned char
	 arguments is mapped to addti3.  */
      else if (insn_op->mode != VOIDmode
	       && GET_MODE (op[arity]) != VOIDmode
	       && GET_MODE (op[arity]) != insn_op->mode
	       && ((tmp_rtx = simplify_gen_subreg (insn_op->mode, op[arity],
						   GET_MODE (op[arity]), 0))
		   != NULL_RTX))
	{
	  op[arity] = tmp_rtx;
	}

      /* The predicate rejects the operand although the mode is fine.
	 Copy the operand to register.  */
      if (!insn_op->predicate (op[arity], insn_op->mode)
	  && (GET_MODE (op[arity]) == insn_op->mode
	      || GET_MODE (op[arity]) == VOIDmode
	      || (insn_op->predicate == address_operand
		  && GET_MODE (op[arity]) == Pmode)))
	{
	  /* An address_operand usually has VOIDmode in the expander
	     so we cannot use this.  */
	  machine_mode target_mode =
	    (insn_op->predicate == address_operand
	     ? (machine_mode) Pmode : insn_op->mode);
	  op[arity] = copy_to_mode_reg (target_mode, op[arity]);
	}

      if (!insn_op->predicate (op[arity], insn_op->mode))
	{
	  error ("invalid argument %d for builtin %qF", arity + 1, fndecl);
	  return const0_rtx;
	}
      arity++;
    }

  switch (arity)
    {
    case 0:
      pat = GEN_FCN (icode) (target);
      break;
    case 1:
      if (nonvoid)
	pat = GEN_FCN (icode) (target, op[0]);
      else
	pat = GEN_FCN (icode) (op[0]);
      break;
    case 2:
      if (nonvoid)
	pat = GEN_FCN (icode) (target, op[0], op[1]);
      else
	pat = GEN_FCN (icode) (op[0], op[1]);
      break;
    case 3:
      if (nonvoid)
	pat = GEN_FCN (icode) (target, op[0], op[1], op[2]);
      else
	pat = GEN_FCN (icode) (op[0], op[1], op[2]);
      break;
    case 4:
      if (nonvoid)
	pat = GEN_FCN (icode) (target, op[0], op[1], op[2], op[3]);
      else
	pat = GEN_FCN (icode) (op[0], op[1], op[2], op[3]);
      break;
    case 5:
      if (nonvoid)
	pat = GEN_FCN (icode) (target, op[0], op[1], op[2], op[3], op[4]);
      else
	pat = GEN_FCN (icode) (op[0], op[1], op[2], op[3], op[4]);
      break;
    case 6:
      if (nonvoid)
	pat = GEN_FCN (icode) (target, op[0], op[1], op[2], op[3], op[4], op[5]);
      else
	pat = GEN_FCN (icode) (op[0], op[1], op[2], op[3], op[4], op[5]);
      break;
    default:
      gcc_unreachable ();
    }
  if (!pat)
    return NULL_RTX;
  emit_insn (pat);

  if (nonvoid)
    return target;
  else
    return const0_rtx;
}


static const int s390_hotpatch_hw_max = 1000000;
static int s390_hotpatch_hw_before_label = 0;
static int s390_hotpatch_hw_after_label = 0;

/* Check whether the hotpatch attribute is applied to a function and, if it has
   an argument, the argument is valid.  */

static tree
s390_handle_hotpatch_attribute (tree *node, tree name, tree args,
				int flags ATTRIBUTE_UNUSED, bool *no_add_attrs)
{
  tree expr;
  tree expr2;
  int err;

  if (TREE_CODE (*node) != FUNCTION_DECL)
    {
      warning (OPT_Wattributes, "%qE attribute only applies to functions",
	       name);
      *no_add_attrs = true;
    }
  if (args != NULL && TREE_CHAIN (args) != NULL)
    {
      expr = TREE_VALUE (args);
      expr2 = TREE_VALUE (TREE_CHAIN (args));
    }
  if (args == NULL || TREE_CHAIN (args) == NULL)
    err = 1;
  else if (TREE_CODE (expr) != INTEGER_CST
	   || !INTEGRAL_TYPE_P (TREE_TYPE (expr))
	   || wi::gtu_p (wi::to_wide (expr), s390_hotpatch_hw_max))
    err = 1;
  else if (TREE_CODE (expr2) != INTEGER_CST
	   || !INTEGRAL_TYPE_P (TREE_TYPE (expr2))
	   || wi::gtu_p (wi::to_wide (expr2), s390_hotpatch_hw_max))
    err = 1;
  else
    err = 0;
  if (err)
    {
      error ("requested %qE attribute is not a comma separated pair of"
	     " non-negative integer constants or too large (max. %d)", name,
	     s390_hotpatch_hw_max);
      *no_add_attrs = true;
    }

  return NULL_TREE;
}

/* Expand the s390_vector_bool type attribute.  */

static tree
s390_handle_vectorbool_attribute (tree *node, tree name ATTRIBUTE_UNUSED,
				  tree args ATTRIBUTE_UNUSED,
				  int flags ATTRIBUTE_UNUSED, bool *no_add_attrs)
{
  tree type = *node, result = NULL_TREE;
  machine_mode mode;

  while (POINTER_TYPE_P (type)
	 || TREE_CODE (type) == FUNCTION_TYPE
	 || TREE_CODE (type) == METHOD_TYPE
	 || TREE_CODE (type) == ARRAY_TYPE)
    type = TREE_TYPE (type);

  mode = TYPE_MODE (type);
  switch (mode)
    {
    case E_DImode: case E_V2DImode:
      result = s390_builtin_types[BT_BV2DI];
      break;
    case E_SImode: case E_V4SImode:
      result = s390_builtin_types[BT_BV4SI];
      break;
    case E_HImode: case E_V8HImode:
      result = s390_builtin_types[BT_BV8HI];
      break;
    case E_QImode: case E_V16QImode:
      result = s390_builtin_types[BT_BV16QI];
      break;
    default:
      break;
    }

  *no_add_attrs = true;  /* No need to hang on to the attribute.  */

  if (result)
    *node = lang_hooks.types.reconstruct_complex_type (*node, result);

  return NULL_TREE;
}

/* Check syntax of function decl attributes having a string type value.  */

static tree
s390_handle_string_attribute (tree *node, tree name ATTRIBUTE_UNUSED,
			      tree args ATTRIBUTE_UNUSED,
			      int flags ATTRIBUTE_UNUSED,
			      bool *no_add_attrs)
{
  tree cst;

  if (TREE_CODE (*node) != FUNCTION_DECL)
    {
      warning (OPT_Wattributes, "%qE attribute only applies to functions",
	       name);
      *no_add_attrs = true;
    }

  cst = TREE_VALUE (args);

  if (TREE_CODE (cst) != STRING_CST)
    {
      warning (OPT_Wattributes,
	       "%qE attribute requires a string constant argument",
	       name);
      *no_add_attrs = true;
    }

  if (is_attribute_p ("indirect_branch", name)
      || is_attribute_p ("indirect_branch_call", name)
      || is_attribute_p ("function_return", name)
      || is_attribute_p ("function_return_reg", name)
      || is_attribute_p ("function_return_mem", name))
    {
      if (strcmp (TREE_STRING_POINTER (cst), "keep") != 0
	  && strcmp (TREE_STRING_POINTER (cst), "thunk") != 0
	  && strcmp (TREE_STRING_POINTER (cst), "thunk-extern") != 0)
      {
	warning (OPT_Wattributes,
		 "argument to %qE attribute is not "
		 "(keep|thunk|thunk-extern)", name);
	*no_add_attrs = true;
      }
    }

  if (is_attribute_p ("indirect_branch_jump", name)
      && strcmp (TREE_STRING_POINTER (cst), "keep") != 0
      && strcmp (TREE_STRING_POINTER (cst), "thunk") != 0
      && strcmp (TREE_STRING_POINTER (cst), "thunk-inline") != 0
      && strcmp (TREE_STRING_POINTER (cst), "thunk-extern") != 0)
    {
      warning (OPT_Wattributes,
	       "argument to %qE attribute is not "
	       "(keep|thunk|thunk-inline|thunk-extern)", name);
      *no_add_attrs = true;
    }

  return NULL_TREE;
}

static const struct attribute_spec s390_attribute_table[] = {
  { "hotpatch", 2, 2, true, false, false, false,
    s390_handle_hotpatch_attribute, NULL },
  { "s390_vector_bool", 0, 0, false, true, false, true,
    s390_handle_vectorbool_attribute, NULL },
  { "indirect_branch", 1, 1, true, false, false, false,
    s390_handle_string_attribute, NULL },
  { "indirect_branch_jump", 1, 1, true, false, false, false,
    s390_handle_string_attribute, NULL },
  { "indirect_branch_call", 1, 1, true, false, false, false,
    s390_handle_string_attribute, NULL },
  { "function_return", 1, 1, true, false, false, false,
    s390_handle_string_attribute, NULL },
  { "function_return_reg", 1, 1, true, false, false, false,
    s390_handle_string_attribute, NULL },
  { "function_return_mem", 1, 1, true, false, false, false,
    s390_handle_string_attribute, NULL },

  /* End element.  */
  { NULL,        0, 0, false, false, false, false, NULL, NULL }
};

/* Return the alignment for LABEL.  We default to the -falign-labels
   value except for the literal pool base label.  */
int
s390_label_align (rtx_insn *label)
{
  rtx_insn *prev_insn = prev_active_insn (label);
  rtx set, src;

  if (prev_insn == NULL_RTX)
    goto old;

  set = single_set (prev_insn);

  if (set == NULL_RTX)
    goto old;

  src = SET_SRC (set);

  /* Don't align literal pool base labels.  */
  if (GET_CODE (src) == UNSPEC
      && XINT (src, 1) == UNSPEC_MAIN_BASE)
    return 0;

 old:
  return align_labels.levels[0].log;
}

static GTY(()) rtx got_symbol;

/* Return the GOT table symbol.  The symbol will be created when the
   function is invoked for the first time.  */

static rtx
s390_got_symbol (void)
{
  if (!got_symbol)
    {
      got_symbol = gen_rtx_SYMBOL_REF (Pmode, "_GLOBAL_OFFSET_TABLE_");
      SYMBOL_REF_FLAGS (got_symbol) = SYMBOL_FLAG_LOCAL;
    }

  return got_symbol;
}

static scalar_int_mode
s390_libgcc_cmp_return_mode (void)
{
  return TARGET_64BIT ? DImode : SImode;
}

static scalar_int_mode
s390_libgcc_shift_count_mode (void)
{
  return TARGET_64BIT ? DImode : SImode;
}

static scalar_int_mode
s390_unwind_word_mode (void)
{
  return TARGET_64BIT ? DImode : SImode;
}

/* Return true if the back end supports mode MODE.  */
static bool
s390_scalar_mode_supported_p (scalar_mode mode)
{
  /* In contrast to the default implementation reject TImode constants on 31bit
     TARGET_ZARCH for ABI compliance.  */
  if (!TARGET_64BIT && TARGET_ZARCH && mode == TImode)
    return false;

  if (DECIMAL_FLOAT_MODE_P (mode))
    return default_decimal_float_supported_p ();

  return default_scalar_mode_supported_p (mode);
}

/* Return true if the back end supports vector mode MODE.  */
static bool
s390_vector_mode_supported_p (machine_mode mode)
{
  machine_mode inner;

  if (!VECTOR_MODE_P (mode)
      || !TARGET_VX
      || GET_MODE_SIZE (mode) > 16)
    return false;

  inner = GET_MODE_INNER (mode);

  switch (inner)
    {
    case E_QImode:
    case E_HImode:
    case E_SImode:
    case E_DImode:
    case E_TImode:
    case E_SFmode:
    case E_DFmode:
    case E_TFmode:
      return true;
    default:
      return false;
    }
}

/* Set the has_landing_pad_p flag in struct machine_function to VALUE.  */

void
s390_set_has_landing_pad_p (bool value)
{
  cfun->machine->has_landing_pad_p = value;
}

/* If two condition code modes are compatible, return a condition code
   mode which is compatible with both.  Otherwise, return
   VOIDmode.  */

static machine_mode
s390_cc_modes_compatible (machine_mode m1, machine_mode m2)
{
  if (m1 == m2)
    return m1;

  switch (m1)
    {
    case E_CCZmode:
      if (m2 == CCUmode || m2 == CCTmode || m2 == CCZ1mode
	  || m2 == CCSmode || m2 == CCSRmode || m2 == CCURmode)
	return m2;
      return VOIDmode;

    case E_CCSmode:
    case E_CCUmode:
    case E_CCTmode:
    case E_CCSRmode:
    case E_CCURmode:
    case E_CCZ1mode:
      if (m2 == CCZmode)
	return m1;

      return VOIDmode;

    default:
      return VOIDmode;
    }
  return VOIDmode;
}

/* Return true if SET either doesn't set the CC register, or else
   the source and destination have matching CC modes and that
   CC mode is at least as constrained as REQ_MODE.  */

static bool
s390_match_ccmode_set (rtx set, machine_mode req_mode)
{
  machine_mode set_mode;

  gcc_assert (GET_CODE (set) == SET);

  /* These modes are supposed to be used only in CC consumer
     patterns.  */
  gcc_assert (req_mode != CCVIALLmode && req_mode != CCVIANYmode
	      && req_mode != CCVFALLmode && req_mode != CCVFANYmode);

  if (GET_CODE (SET_DEST (set)) != REG || !CC_REGNO_P (REGNO (SET_DEST (set))))
    return 1;

  set_mode = GET_MODE (SET_DEST (set));
  switch (set_mode)
    {
    case E_CCZ1mode:
    case E_CCSmode:
    case E_CCSRmode:
    case E_CCSFPSmode:
    case E_CCUmode:
    case E_CCURmode:
    case E_CCOmode:
    case E_CCLmode:
    case E_CCL1mode:
    case E_CCL2mode:
    case E_CCL3mode:
    case E_CCT1mode:
    case E_CCT2mode:
    case E_CCT3mode:
    case E_CCVEQmode:
    case E_CCVIHmode:
    case E_CCVIHUmode:
    case E_CCVFHmode:
    case E_CCVFHEmode:
      if (req_mode != set_mode)
	return 0;
      break;

    case E_CCZmode:
      if (req_mode != CCSmode && req_mode != CCUmode && req_mode != CCTmode
	  && req_mode != CCSRmode && req_mode != CCURmode
	  && req_mode != CCZ1mode)
	return 0;
      break;

    case E_CCAPmode:
    case E_CCANmode:
      if (req_mode != CCAmode)
	return 0;
      break;

    default:
      gcc_unreachable ();
    }

  return (GET_MODE (SET_SRC (set)) == set_mode);
}

/* Return true if every SET in INSN that sets the CC register
   has source and destination with matching CC modes and that
   CC mode is at least as constrained as REQ_MODE.
   If REQ_MODE is VOIDmode, always return false.  */

bool
s390_match_ccmode (rtx_insn *insn, machine_mode req_mode)
{
  int i;

  /* s390_tm_ccmode returns VOIDmode to indicate failure.  */
  if (req_mode == VOIDmode)
    return false;

  if (GET_CODE (PATTERN (insn)) == SET)
    return s390_match_ccmode_set (PATTERN (insn), req_mode);

  if (GET_CODE (PATTERN (insn)) == PARALLEL)
      for (i = 0; i < XVECLEN (PATTERN (insn), 0); i++)
	{
	  rtx set = XVECEXP (PATTERN (insn), 0, i);
	  if (GET_CODE (set) == SET)
	    if (!s390_match_ccmode_set (set, req_mode))
	      return false;
	}

  return true;
}

/* If a test-under-mask instruction can be used to implement
   (compare (and ... OP1) OP2), return the CC mode required
   to do that.  Otherwise, return VOIDmode.
   MIXED is true if the instruction can distinguish between
   CC1 and CC2 for mixed selected bits (TMxx), it is false
   if the instruction cannot (TM).  */

machine_mode
s390_tm_ccmode (rtx op1, rtx op2, bool mixed)
{
  int bit0, bit1;

  /* ??? Fixme: should work on CONST_WIDE_INT as well.  */
  if (GET_CODE (op1) != CONST_INT || GET_CODE (op2) != CONST_INT)
    return VOIDmode;

  /* Selected bits all zero: CC0.
     e.g.: int a; if ((a & (16 + 128)) == 0) */
  if (INTVAL (op2) == 0)
    return CCTmode;

  /* Selected bits all one: CC3.
     e.g.: int a; if ((a & (16 + 128)) == 16 + 128) */
  if (INTVAL (op2) == INTVAL (op1))
    return CCT3mode;

  /* Exactly two bits selected, mixed zeroes and ones: CC1 or CC2. e.g.:
     int a;
     if ((a & (16 + 128)) == 16)         -> CCT1
     if ((a & (16 + 128)) == 128)        -> CCT2  */
  if (mixed)
    {
      bit1 = exact_log2 (INTVAL (op2));
      bit0 = exact_log2 (INTVAL (op1) ^ INTVAL (op2));
      if (bit0 != -1 && bit1 != -1)
	return bit0 > bit1 ? CCT1mode : CCT2mode;
    }

  return VOIDmode;
}

/* Given a comparison code OP (EQ, NE, etc.) and the operands
   OP0 and OP1 of a COMPARE, return the mode to be used for the
   comparison.  */

machine_mode
s390_select_ccmode (enum rtx_code code, rtx op0, rtx op1)
{
  switch (code)
    {
      case EQ:
      case NE:
	if ((GET_CODE (op0) == NEG || GET_CODE (op0) == ABS)
	    && GET_MODE_CLASS (GET_MODE (op0)) == MODE_INT)
	  return CCAPmode;
	if (GET_CODE (op0) == PLUS && GET_CODE (XEXP (op0, 1)) == CONST_INT
	    && CONST_OK_FOR_K (INTVAL (XEXP (op0, 1))))
	  return CCAPmode;
	if ((GET_CODE (op0) == PLUS || GET_CODE (op0) == MINUS
	     || GET_CODE (op1) == NEG)
	    && GET_MODE_CLASS (GET_MODE (op0)) == MODE_INT)
	  return CCLmode;

	if (GET_CODE (op0) == AND)
	  {
	    /* Check whether we can potentially do it via TM.  */
	    machine_mode ccmode;
	    ccmode = s390_tm_ccmode (XEXP (op0, 1), op1, 1);
	    if (ccmode != VOIDmode)
	      {
		/* Relax CCTmode to CCZmode to allow fall-back to AND
		   if that turns out to be beneficial.  */
		return ccmode == CCTmode ? CCZmode : ccmode;
	      }
	  }

	if (register_operand (op0, HImode)
	    && GET_CODE (op1) == CONST_INT
	    && (INTVAL (op1) == -1 || INTVAL (op1) == 65535))
	  return CCT3mode;
	if (register_operand (op0, QImode)
	    && GET_CODE (op1) == CONST_INT
	    && (INTVAL (op1) == -1 || INTVAL (op1) == 255))
	  return CCT3mode;

	return CCZmode;

      case LE:
      case LT:
      case GE:
      case GT:
	/* The only overflow condition of NEG and ABS happens when
	   -INT_MAX is used as parameter, which stays negative. So
	   we have an overflow from a positive value to a negative.
	   Using CCAP mode the resulting cc can be used for comparisons.  */
	if ((GET_CODE (op0) == NEG || GET_CODE (op0) == ABS)
	    && GET_MODE_CLASS (GET_MODE (op0)) == MODE_INT)
	  return CCAPmode;

	/* If constants are involved in an add instruction it is possible to use
	   the resulting cc for comparisons with zero. Knowing the sign of the
	   constant the overflow behavior gets predictable. e.g.:
	     int a, b; if ((b = a + c) > 0)
	   with c as a constant value: c < 0 -> CCAN and c >= 0 -> CCAP  */
	if (GET_CODE (op0) == PLUS && GET_CODE (XEXP (op0, 1)) == CONST_INT
	    && (CONST_OK_FOR_K (INTVAL (XEXP (op0, 1)))
		|| (CONST_OK_FOR_CONSTRAINT_P (INTVAL (XEXP (op0, 1)), 'O', "Os")
		    /* Avoid INT32_MIN on 32 bit.  */
		    && (!TARGET_ZARCH || INTVAL (XEXP (op0, 1)) != -0x7fffffff - 1))))
	  {
	    if (INTVAL (XEXP((op0), 1)) < 0)
	      return CCANmode;
	    else
	      return CCAPmode;
	  }

	/* Fall through.  */
      case LTGT:
	if (HONOR_NANS (op0) || HONOR_NANS (op1))
	  return CCSFPSmode;

	/* Fall through.  */
      case UNORDERED:
      case ORDERED:
      case UNEQ:
      case UNLE:
      case UNLT:
      case UNGE:
      case UNGT:
	if ((GET_CODE (op0) == SIGN_EXTEND || GET_CODE (op0) == ZERO_EXTEND)
	    && GET_CODE (op1) != CONST_INT)
	  return CCSRmode;
	return CCSmode;

      case LTU:
      case GEU:
	if (GET_CODE (op0) == PLUS
	    && GET_MODE_CLASS (GET_MODE (op0)) == MODE_INT)
	  return CCL1mode;

	if ((GET_CODE (op0) == SIGN_EXTEND || GET_CODE (op0) == ZERO_EXTEND)
	    && GET_CODE (op1) != CONST_INT)
	  return CCURmode;
	return CCUmode;

      case LEU:
      case GTU:
	if (GET_CODE (op0) == MINUS
	    && GET_MODE_CLASS (GET_MODE (op0)) == MODE_INT)
	  return CCL2mode;

	if ((GET_CODE (op0) == SIGN_EXTEND || GET_CODE (op0) == ZERO_EXTEND)
	    && GET_CODE (op1) != CONST_INT)
	  return CCURmode;
	return CCUmode;

      default:
	gcc_unreachable ();
    }
}

/* Replace the comparison OP0 CODE OP1 by a semantically equivalent one
   that we can implement more efficiently.  */

static void
s390_canonicalize_comparison (int *code, rtx *op0, rtx *op1,
			      bool op0_preserve_value)
{
  if (op0_preserve_value)
    return;

  /* Convert ZERO_EXTRACT back to AND to enable TM patterns.  */
  if ((*code == EQ || *code == NE)
      && *op1 == const0_rtx
      && GET_CODE (*op0) == ZERO_EXTRACT
      && GET_CODE (XEXP (*op0, 1)) == CONST_INT
      && GET_CODE (XEXP (*op0, 2)) == CONST_INT
      && SCALAR_INT_MODE_P (GET_MODE (XEXP (*op0, 0))))
    {
      rtx inner = XEXP (*op0, 0);
      HOST_WIDE_INT modesize = GET_MODE_BITSIZE (GET_MODE (inner));
      HOST_WIDE_INT len = INTVAL (XEXP (*op0, 1));
      HOST_WIDE_INT pos = INTVAL (XEXP (*op0, 2));

      if (len > 0 && len < modesize
	  && pos >= 0 && pos + len <= modesize
	  && modesize <= HOST_BITS_PER_WIDE_INT)
	{
	  unsigned HOST_WIDE_INT block;
	  block = (HOST_WIDE_INT_1U << len) - 1;
	  block <<= modesize - pos - len;

	  *op0 = gen_rtx_AND (GET_MODE (inner), inner,
			      gen_int_mode (block, GET_MODE (inner)));
	}
    }

  /* Narrow AND of memory against immediate to enable TM.  */
  if ((*code == EQ || *code == NE)
      && *op1 == const0_rtx
      && GET_CODE (*op0) == AND
      && GET_CODE (XEXP (*op0, 1)) == CONST_INT
      && SCALAR_INT_MODE_P (GET_MODE (XEXP (*op0, 0))))
    {
      rtx inner = XEXP (*op0, 0);
      rtx mask = XEXP (*op0, 1);

      /* Ignore paradoxical SUBREGs if all extra bits are masked out.  */
      if (GET_CODE (inner) == SUBREG
	  && SCALAR_INT_MODE_P (GET_MODE (SUBREG_REG (inner)))
	  && (GET_MODE_SIZE (GET_MODE (inner))
	      >= GET_MODE_SIZE (GET_MODE (SUBREG_REG (inner))))
	  && ((INTVAL (mask)
	       & GET_MODE_MASK (GET_MODE (inner))
	       & ~GET_MODE_MASK (GET_MODE (SUBREG_REG (inner))))
	      == 0))
	inner = SUBREG_REG (inner);

      /* Do not change volatile MEMs.  */
      if (MEM_P (inner) && !MEM_VOLATILE_P (inner))
	{
	  int part = s390_single_part (XEXP (*op0, 1),
				       GET_MODE (inner), QImode, 0);
	  if (part >= 0)
	    {
	      mask = gen_int_mode (s390_extract_part (mask, QImode, 0), QImode);
	      inner = adjust_address_nv (inner, QImode, part);
	      *op0 = gen_rtx_AND (QImode, inner, mask);
	    }
	}
    }

  /* Narrow comparisons against 0xffff to HImode if possible.  */
  if ((*code == EQ || *code == NE)
      && GET_CODE (*op1) == CONST_INT
      && INTVAL (*op1) == 0xffff
      && SCALAR_INT_MODE_P (GET_MODE (*op0))
      && (nonzero_bits (*op0, GET_MODE (*op0))
	  & ~HOST_WIDE_INT_UC (0xffff)) == 0)
    {
      *op0 = gen_lowpart (HImode, *op0);
      *op1 = constm1_rtx;
    }

  /* Remove redundant UNSPEC_STRCMPCC_TO_INT conversions if possible.  */
  if (GET_CODE (*op0) == UNSPEC
      && XINT (*op0, 1) == UNSPEC_STRCMPCC_TO_INT
      && XVECLEN (*op0, 0) == 1
      && GET_MODE (XVECEXP (*op0, 0, 0)) == CCUmode
      && GET_CODE (XVECEXP (*op0, 0, 0)) == REG
      && REGNO (XVECEXP (*op0, 0, 0)) == CC_REGNUM
      && *op1 == const0_rtx)
    {
      enum rtx_code new_code = UNKNOWN;
      switch (*code)
	{
	  case EQ: new_code = EQ;  break;
	  case NE: new_code = NE;  break;
	  case LT: new_code = GTU; break;
	  case GT: new_code = LTU; break;
	  case LE: new_code = GEU; break;
	  case GE: new_code = LEU; break;
	  default: break;
	}

      if (new_code != UNKNOWN)
	{
	  *op0 = XVECEXP (*op0, 0, 0);
	  *code = new_code;
	}
    }

  /* Remove redundant UNSPEC_CC_TO_INT conversions if possible.  */
  if (GET_CODE (*op0) == UNSPEC
      && XINT (*op0, 1) == UNSPEC_CC_TO_INT
      && XVECLEN (*op0, 0) == 1
      && GET_CODE (XVECEXP (*op0, 0, 0)) == REG
      && REGNO (XVECEXP (*op0, 0, 0)) == CC_REGNUM
      && CONST_INT_P (*op1))
    {
      enum rtx_code new_code = UNKNOWN;
      switch (GET_MODE (XVECEXP (*op0, 0, 0)))
	{
	case E_CCZmode:
	case E_CCRAWmode:
	  switch (*code)
	    {
	    case EQ: new_code = EQ;  break;
	    case NE: new_code = NE;  break;
	    default: break;
	    }
	  break;
	default: break;
	}

      if (new_code != UNKNOWN)
	{
	  /* For CCRAWmode put the required cc mask into the second
	     operand.  */
	if (GET_MODE (XVECEXP (*op0, 0, 0)) == CCRAWmode
	    && INTVAL (*op1) >= 0 && INTVAL (*op1) <= 3)
	    *op1 = gen_rtx_CONST_INT (VOIDmode, 1 << (3 - INTVAL (*op1)));
	  *op0 = XVECEXP (*op0, 0, 0);
	  *code = new_code;
	}
    }

  /* Simplify cascaded EQ, NE with const0_rtx.  */
  if ((*code == NE || *code == EQ)
      && (GET_CODE (*op0) == EQ || GET_CODE (*op0) == NE)
      && GET_MODE (*op0) == SImode
      && GET_MODE (XEXP (*op0, 0)) == CCZ1mode
      && REG_P (XEXP (*op0, 0))
      && XEXP (*op0, 1) == const0_rtx
      && *op1 == const0_rtx)
    {
      if ((*code == EQ && GET_CODE (*op0) == NE)
	  || (*code == NE && GET_CODE (*op0) == EQ))
	*code = EQ;
      else
	*code = NE;
      *op0 = XEXP (*op0, 0);
    }

  /* Prefer register over memory as first operand.  */
  if (MEM_P (*op0) && REG_P (*op1))
    {
      rtx tem = *op0; *op0 = *op1; *op1 = tem;
      *code = (int)swap_condition ((enum rtx_code)*code);
    }

  /* A comparison result is compared against zero.  Replace it with
     the (perhaps inverted) original comparison.
     This probably should be done by simplify_relational_operation.  */
  if ((*code == EQ || *code == NE)
      && *op1 == const0_rtx
      && COMPARISON_P (*op0)
      && CC_REG_P (XEXP (*op0, 0)))
    {
      enum rtx_code new_code;

      if (*code == EQ)
	new_code = reversed_comparison_code_parts (GET_CODE (*op0),
						   XEXP (*op0, 0),
						   XEXP (*op0, 1), NULL);
      else
	new_code = GET_CODE (*op0);

      if (new_code != UNKNOWN)
	{
	  *code = new_code;
	  *op1 = XEXP (*op0, 1);
	  *op0 = XEXP (*op0, 0);
	}
    }

  /* ~a==b -> ~(a^b)==0   ~a!=b -> ~(a^b)!=0 */
  if (TARGET_Z15
      && (*code == EQ || *code == NE)
      && (GET_MODE (*op0) == DImode || GET_MODE (*op0) == SImode)
      && GET_CODE (*op0) == NOT)
    {
      machine_mode mode = GET_MODE (*op0);
      *op0 = gen_rtx_XOR (mode, XEXP (*op0, 0), *op1);
      *op0 = gen_rtx_NOT (mode, *op0);
      *op1 = const0_rtx;
    }

  /* a&b == -1 -> ~a|~b == 0    a|b == -1 -> ~a&~b == 0  */
  if (TARGET_Z15
      && (*code == EQ || *code == NE)
      && (GET_CODE (*op0) == AND || GET_CODE (*op0) == IOR)
      && (GET_MODE (*op0) == DImode || GET_MODE (*op0) == SImode)
      && CONST_INT_P (*op1)
      && *op1 == constm1_rtx)
    {
      machine_mode mode = GET_MODE (*op0);
      rtx op00 = gen_rtx_NOT (mode, XEXP (*op0, 0));
      rtx op01 = gen_rtx_NOT (mode, XEXP (*op0, 1));

      if (GET_CODE (*op0) == AND)
	*op0 = gen_rtx_IOR (mode, op00, op01);
      else
	*op0 = gen_rtx_AND (mode, op00, op01);

      *op1 = const0_rtx;
    }
}


/* Emit a compare instruction suitable to implement the comparison
   OP0 CODE OP1.  Return the correct condition RTL to be placed in
   the IF_THEN_ELSE of the conditional branch testing the result.  */

rtx
s390_emit_compare (enum rtx_code code, rtx op0, rtx op1)
{
  machine_mode mode = s390_select_ccmode (code, op0, op1);
  rtx cc;

  /* Force OP1 into register in order to satisfy VXE TFmode patterns.  */
  if (TARGET_VXE && GET_MODE (op1) == TFmode)
    op1 = force_reg (TFmode, op1);

  if (GET_MODE_CLASS (GET_MODE (op0)) == MODE_CC)
    {
      /* Do not output a redundant compare instruction if a
	 compare_and_swap pattern already computed the result and the
	 machine modes are compatible.  */
      gcc_assert (s390_cc_modes_compatible (GET_MODE (op0), mode)
		  == GET_MODE (op0));
      cc = op0;
    }
  else
    {
      cc = gen_rtx_REG (mode, CC_REGNUM);
      emit_insn (gen_rtx_SET (cc, gen_rtx_COMPARE (mode, op0, op1)));
    }

  return gen_rtx_fmt_ee (code, VOIDmode, cc, const0_rtx);
}

/* If MEM is not a legitimate compare-and-swap memory operand, return a new
   MEM, whose address is a pseudo containing the original MEM's address.  */

static rtx
s390_legitimize_cs_operand (rtx mem)
{
  rtx tmp;

  if (!contains_symbol_ref_p (mem))
    return mem;
  tmp = gen_reg_rtx (Pmode);
  emit_move_insn (tmp, copy_rtx (XEXP (mem, 0)));
  return change_address (mem, VOIDmode, tmp);
}

/* Emit a SImode compare and swap instruction setting MEM to NEW_RTX if OLD
   matches CMP.
   Return the correct condition RTL to be placed in the IF_THEN_ELSE of the
   conditional branch testing the result.  */

static rtx
s390_emit_compare_and_swap (enum rtx_code code, rtx old, rtx mem,
			    rtx cmp, rtx new_rtx, machine_mode ccmode)
{
  rtx cc;

  mem = s390_legitimize_cs_operand (mem);
  cc = gen_rtx_REG (ccmode, CC_REGNUM);
  switch (GET_MODE (mem))
    {
    case E_SImode:
      emit_insn (gen_atomic_compare_and_swapsi_internal (old, mem, cmp,
							 new_rtx, cc));
      break;
    case E_DImode:
      emit_insn (gen_atomic_compare_and_swapdi_internal (old, mem, cmp,
							 new_rtx, cc));
      break;
    case E_TImode:
	emit_insn (gen_atomic_compare_and_swapti_internal (old, mem, cmp,
							   new_rtx, cc));
      break;
    case E_QImode:
    case E_HImode:
    default:
      gcc_unreachable ();
    }
  return s390_emit_compare (code, cc, const0_rtx);
}

/* Emit a jump instruction to TARGET and return it.  If COND is
   NULL_RTX, emit an unconditional jump, else a conditional jump under
   condition COND.  */

rtx_insn *
s390_emit_jump (rtx target, rtx cond)
{
  rtx insn;

  target = gen_rtx_LABEL_REF (VOIDmode, target);
  if (cond)
    target = gen_rtx_IF_THEN_ELSE (VOIDmode, cond, target, pc_rtx);

  insn = gen_rtx_SET (pc_rtx, target);
  return emit_jump_insn (insn);
}

/* Return branch condition mask to implement a branch
   specified by CODE.  Return -1 for invalid comparisons.  */

int
s390_branch_condition_mask (rtx code)
{
  const int CC0 = 1 << 3;
  const int CC1 = 1 << 2;
  const int CC2 = 1 << 1;
  const int CC3 = 1 << 0;

  gcc_assert (GET_CODE (XEXP (code, 0)) == REG);
  gcc_assert (REGNO (XEXP (code, 0)) == CC_REGNUM);
  gcc_assert (XEXP (code, 1) == const0_rtx
	      || (GET_MODE (XEXP (code, 0)) == CCRAWmode
		  && CONST_INT_P (XEXP (code, 1))));


  switch (GET_MODE (XEXP (code, 0)))
    {
    case E_CCZmode:
    case E_CCZ1mode:
      switch (GET_CODE (code))
	{
	case EQ:	return CC0;
	case NE:	return CC1 | CC2 | CC3;
	default:	return -1;
	}
      break;

    case E_CCT1mode:
      switch (GET_CODE (code))
	{
	case EQ:	return CC1;
	case NE:	return CC0 | CC2 | CC3;
	default:	return -1;
	}
      break;

    case E_CCT2mode:
      switch (GET_CODE (code))
	{
	case EQ:	return CC2;
	case NE:	return CC0 | CC1 | CC3;
	default:	return -1;
	}
      break;

    case E_CCT3mode:
      switch (GET_CODE (code))
	{
	case EQ:	return CC3;
	case NE:	return CC0 | CC1 | CC2;
	default:	return -1;
	}
      break;

    case E_CCLmode:
      switch (GET_CODE (code))
	{
	case EQ:	return CC0 | CC2;
	case NE:	return CC1 | CC3;
	default:	return -1;
	}
      break;

    case E_CCL1mode:
      switch (GET_CODE (code))
	{
	case LTU:	return CC2 | CC3;  /* carry */
	case GEU:	return CC0 | CC1;  /* no carry */
	default:	return -1;
	}
      break;

    case E_CCL2mode:
      switch (GET_CODE (code))
	{
	case GTU:	return CC0 | CC1;  /* borrow */
	case LEU:	return CC2 | CC3;  /* no borrow */
	default:	return -1;
	}
      break;

    case E_CCL3mode:
      switch (GET_CODE (code))
	{
	case EQ:	return CC0 | CC2;
	case NE:	return CC1 | CC3;
	case LTU:	return CC1;
	case GTU:	return CC3;
	case LEU:	return CC1 | CC2;
	case GEU:	return CC2 | CC3;
	default:	return -1;
	}

    case E_CCUmode:
      switch (GET_CODE (code))
	{
	case EQ:	return CC0;
	case NE:	return CC1 | CC2 | CC3;
	case LTU:	return CC1;
	case GTU:	return CC2;
	case LEU:	return CC0 | CC1;
	case GEU:	return CC0 | CC2;
	default:	return -1;
	}
      break;

    case E_CCURmode:
      switch (GET_CODE (code))
	{
	case EQ:	return CC0;
	case NE:	return CC2 | CC1 | CC3;
	case LTU:	return CC2;
	case GTU:	return CC1;
	case LEU:	return CC0 | CC2;
	case GEU:	return CC0 | CC1;
	default:	return -1;
	}
      break;

    case E_CCAPmode:
      switch (GET_CODE (code))
	{
	case EQ:	return CC0;
	case NE:	return CC1 | CC2 | CC3;
	case LT:	return CC1 | CC3;
	case GT:	return CC2;
	case LE:	return CC0 | CC1 | CC3;
	case GE:	return CC0 | CC2;
	default:	return -1;
	}
      break;

    case E_CCANmode:
      switch (GET_CODE (code))
	{
	case EQ:	return CC0;
	case NE:	return CC1 | CC2 | CC3;
	case LT:	return CC1;
	case GT:	return CC2 | CC3;
	case LE:	return CC0 | CC1;
	case GE:	return CC0 | CC2 | CC3;
	default:	return -1;
	}
      break;

    case E_CCOmode:
      switch (GET_CODE (code))
	{
	case EQ:	return CC0 | CC1 | CC2;
	case NE:	return CC3;
	default:	return -1;
	}
      break;

    case E_CCSmode:
    case E_CCSFPSmode:
      switch (GET_CODE (code))
	{
	case EQ:	return CC0;
	case NE:	return CC1 | CC2 | CC3;
	case LT:	return CC1;
	case GT:	return CC2;
	case LE:	return CC0 | CC1;
	case GE:	return CC0 | CC2;
	case UNORDERED:	return CC3;
	case ORDERED:	return CC0 | CC1 | CC2;
	case UNEQ:	return CC0 | CC3;
	case UNLT:	return CC1 | CC3;
	case UNGT:	return CC2 | CC3;
	case UNLE:	return CC0 | CC1 | CC3;
	case UNGE:	return CC0 | CC2 | CC3;
	case LTGT:	return CC1 | CC2;
	default:	return -1;
	}
      break;

    case E_CCSRmode:
      switch (GET_CODE (code))
	{
	case EQ:	return CC0;
	case NE:	return CC2 | CC1 | CC3;
	case LT:	return CC2;
	case GT:	return CC1;
	case LE:	return CC0 | CC2;
	case GE:	return CC0 | CC1;
	case UNORDERED:	return CC3;
	case ORDERED:	return CC0 | CC2 | CC1;
	case UNEQ:	return CC0 | CC3;
	case UNLT:	return CC2 | CC3;
	case UNGT:	return CC1 | CC3;
	case UNLE:	return CC0 | CC2 | CC3;
	case UNGE:	return CC0 | CC1 | CC3;
	case LTGT:	return CC2 | CC1;
	default:	return -1;
	}
      break;

      /* Vector comparison modes.  */
      /* CC2 will never be set.  It however is part of the negated
	 masks.  */
    case E_CCVIALLmode:
      switch (GET_CODE (code))
	{
	case EQ:
	case GTU:
	case GT:
	case GE:        return CC0;
	  /* The inverted modes are in fact *any* modes.  */
	case NE:
	case LEU:
	case LE:
	case LT:        return CC3 | CC1 | CC2;
	default:        return -1;
	}

    case E_CCVIANYmode:
      switch (GET_CODE (code))
	{
	case EQ:
	case GTU:
	case GT:
	case GE:        return CC0 | CC1;
	  /* The inverted modes are in fact *all* modes.  */
	case NE:
	case LEU:
	case LE:
	case LT:        return CC3 | CC2;
	default:        return -1;
	}
    case E_CCVFALLmode:
      switch (GET_CODE (code))
	{
	case EQ:
	case GT:
	case GE:        return CC0;
	  /* The inverted modes are in fact *any* modes.  */
	case NE:
	case UNLE:
	case UNLT:      return CC3 | CC1 | CC2;
	default:        return -1;
	}

    case E_CCVFANYmode:
      switch (GET_CODE (code))
	{
	case EQ:
	case GT:
	case GE:        return CC0 | CC1;
	  /* The inverted modes are in fact *all* modes.  */
	case NE:
	case UNLE:
	case UNLT:      return CC3 | CC2;
	default:        return -1;
	}

    case E_CCRAWmode:
      switch (GET_CODE (code))
	{
	case EQ:
	  return INTVAL (XEXP (code, 1));
	case NE:
	  return (INTVAL (XEXP (code, 1))) ^ 0xf;
	default:
	  gcc_unreachable ();
	}

    default:
      return -1;
    }
}


/* Return branch condition mask to implement a compare and branch
   specified by CODE.  Return -1 for invalid comparisons.  */

int
s390_compare_and_branch_condition_mask (rtx code)
{
  const int CC0 = 1 << 3;
  const int CC1 = 1 << 2;
  const int CC2 = 1 << 1;

  switch (GET_CODE (code))
    {
    case EQ:
      return CC0;
    case NE:
      return CC1 | CC2;
    case LT:
    case LTU:
      return CC1;
    case GT:
    case GTU:
      return CC2;
    case LE:
    case LEU:
      return CC0 | CC1;
    case GE:
    case GEU:
      return CC0 | CC2;
    default:
      gcc_unreachable ();
    }
  return -1;
}

/* If INV is false, return assembler mnemonic string to implement
   a branch specified by CODE.  If INV is true, return mnemonic
   for the corresponding inverted branch.  */

static const char *
s390_branch_condition_mnemonic (rtx code, int inv)
{
  int mask;

  static const char *const mnemonic[16] =
    {
      NULL, "o", "h", "nle",
      "l", "nhe", "lh", "ne",
      "e", "nlh", "he", "nl",
      "le", "nh", "no", NULL
    };

  if (GET_CODE (XEXP (code, 0)) == REG
      && REGNO (XEXP (code, 0)) == CC_REGNUM
      && (XEXP (code, 1) == const0_rtx
	  || (GET_MODE (XEXP (code, 0)) == CCRAWmode
	      && CONST_INT_P (XEXP (code, 1)))))
    mask = s390_branch_condition_mask (code);
  else
    mask = s390_compare_and_branch_condition_mask (code);

  gcc_assert (mask >= 0);

  if (inv)
    mask ^= 15;

  gcc_assert (mask >= 1 && mask <= 14);

  return mnemonic[mask];
}

/* Return the part of op which has a value different from def.
   The size of the part is determined by mode.
   Use this function only if you already know that op really
   contains such a part.  */

unsigned HOST_WIDE_INT
s390_extract_part (rtx op, machine_mode mode, int def)
{
  unsigned HOST_WIDE_INT value = 0;
  int max_parts = HOST_BITS_PER_WIDE_INT / GET_MODE_BITSIZE (mode);
  int part_bits = GET_MODE_BITSIZE (mode);
  unsigned HOST_WIDE_INT part_mask = (HOST_WIDE_INT_1U << part_bits) - 1;
  int i;

  for (i = 0; i < max_parts; i++)
    {
      if (i == 0)
	value = UINTVAL (op);
      else
	value >>= part_bits;

      if ((value & part_mask) != (def & part_mask))
	return value & part_mask;
    }

  gcc_unreachable ();
}

/* If OP is an integer constant of mode MODE with exactly one
   part of mode PART_MODE unequal to DEF, return the number of that
   part. Otherwise, return -1.  */

int
s390_single_part (rtx op,
		  machine_mode mode,
		  machine_mode part_mode,
		  int def)
{
  unsigned HOST_WIDE_INT value = 0;
  int n_parts = GET_MODE_SIZE (mode) / GET_MODE_SIZE (part_mode);
  unsigned HOST_WIDE_INT part_mask
    = (HOST_WIDE_INT_1U << GET_MODE_BITSIZE (part_mode)) - 1;
  int i, part = -1;

  if (GET_CODE (op) != CONST_INT)
    return -1;

  for (i = 0; i < n_parts; i++)
    {
      if (i == 0)
	value = UINTVAL (op);
      else
	value >>= GET_MODE_BITSIZE (part_mode);

      if ((value & part_mask) != (def & part_mask))
	{
	  if (part != -1)
	    return -1;
	  else
	    part = i;
	}
    }
  return part == -1 ? -1 : n_parts - 1 - part;
}

/* Return true if IN contains a contiguous bitfield in the lower SIZE
   bits and no other bits are set in (the lower SIZE bits of) IN.

   PSTART and PEND can be used to obtain the start and end
   position (inclusive) of the bitfield relative to 64
   bits. *PSTART / *PEND gives the position of the first/last bit
   of the bitfield counting from the highest order bit starting
   with zero.  */

bool
s390_contiguous_bitmask_nowrap_p (unsigned HOST_WIDE_INT in, int size,
				  int *pstart, int *pend)
{
  int start;
  int end = -1;
  int lowbit = HOST_BITS_PER_WIDE_INT - 1;
  int highbit = HOST_BITS_PER_WIDE_INT - size;
  unsigned HOST_WIDE_INT bitmask = HOST_WIDE_INT_1U;

  gcc_assert (!!pstart == !!pend);
  for (start = lowbit; start >= highbit; bitmask <<= 1, start--)
    if (end == -1)
      {
	/* Look for the rightmost bit of a contiguous range of ones.  */
	if (bitmask & in)
	  /* Found it.  */
	  end = start;
      }
    else
      {
	/* Look for the firt zero bit after the range of ones.  */
	if (! (bitmask & in))
	  /* Found it.  */
	  break;
      }
  /* We're one past the last one-bit.  */
  start++;

  if (end == -1)
    /* No one bits found.  */
    return false;

  if (start > highbit)
    {
      unsigned HOST_WIDE_INT mask;

      /* Calculate a mask for all bits beyond the contiguous bits.  */
      mask = ((~HOST_WIDE_INT_0U >> highbit)
	      & (~HOST_WIDE_INT_0U << (lowbit - start + 1)));
      if (mask & in)
	/* There are more bits set beyond the first range of one bits.  */
	return false;
    }

  if (pstart)
    {
      *pstart = start;
      *pend = end;
    }

  return true;
}

/* Same as s390_contiguous_bitmask_nowrap_p but also returns true
   if ~IN contains a contiguous bitfield.  In that case, *END is <
   *START.

   If WRAP_P is true, a bitmask that wraps around is also tested.
   When a wraparoud occurs *START is greater than *END (in
   non-null pointers), and the uppermost (64 - SIZE) bits are thus
   part of the range.  If WRAP_P is false, no wraparound is
   tested.  */

bool
s390_contiguous_bitmask_p (unsigned HOST_WIDE_INT in, bool wrap_p,
			   int size, int *start, int *end)
{
  int bs = HOST_BITS_PER_WIDE_INT;
  bool b;

  gcc_assert (!!start == !!end);
  if ((in & ((~HOST_WIDE_INT_0U) >> (bs - size))) == 0)
    /* This cannot be expressed as a contiguous bitmask.  Exit early because
       the second call of s390_contiguous_bitmask_nowrap_p would accept this as
       a valid bitmask.  */
    return false;
  b = s390_contiguous_bitmask_nowrap_p (in, size, start, end);
  if (b)
    return true;
  if (! wrap_p)
    return false;
  b = s390_contiguous_bitmask_nowrap_p (~in, size, start, end);
  if (b && start)
    {
      int s = *start;
      int e = *end;

      gcc_assert (s >= 1);
      *start = ((e + 1) & (bs - 1));
      *end = ((s - 1 + bs) & (bs - 1));
    }

  return b;
}

/* Return true if OP contains the same contiguous bitfield in *all*
   its elements.  START and END can be used to obtain the start and
   end position of the bitfield.

   START/STOP give the position of the first/last bit of the bitfield
   counting from the lowest order bit starting with zero.  In order to
   use these values for S/390 instructions this has to be converted to
   "bits big endian" style.  */

bool
s390_contiguous_bitmask_vector_p (rtx op, int *start, int *end)
{
  unsigned HOST_WIDE_INT mask;
  int size;
  rtx elt;
  bool b;

  /* Handle floats by bitcasting them to ints.  */
  op = gen_lowpart (related_int_vector_mode (GET_MODE (op)).require (), op);

  gcc_assert (!!start == !!end);
  if (!const_vec_duplicate_p (op, &elt)
      || !CONST_INT_P (elt))
    return false;

  size = GET_MODE_UNIT_BITSIZE (GET_MODE (op));

  /* We cannot deal with V1TI/V1TF. This would require a vgmq.  */
  if (size > 64)
    return false;

  mask = UINTVAL (elt);

  b = s390_contiguous_bitmask_p (mask, true, size, start, end);
  if (b)
    {
      if (start)
	{
	  *start -= (HOST_BITS_PER_WIDE_INT - size);
	  *end -= (HOST_BITS_PER_WIDE_INT - size);
	}
      return true;
    }
  else
    return false;
}

/* Return true if C consists only of byte chunks being either 0 or
   0xff.  If MASK is !=NULL a byte mask is generated which is
   appropriate for the vector generate byte mask instruction.  */

bool
s390_bytemask_vector_p (rtx op, unsigned *mask)
{
  int i;
  unsigned tmp_mask = 0;
  int nunit, unit_size;

  if (!VECTOR_MODE_P (GET_MODE (op))
      || GET_CODE (op) != CONST_VECTOR
      || !CONST_INT_P (XVECEXP (op, 0, 0)))
    return false;

  nunit = GET_MODE_NUNITS (GET_MODE (op));
  unit_size = GET_MODE_UNIT_SIZE (GET_MODE (op));

  for (i = 0; i < nunit; i++)
    {
      unsigned HOST_WIDE_INT c;
      int j;

      if (!CONST_INT_P (XVECEXP (op, 0, i)))
	return false;

      c = UINTVAL (XVECEXP (op, 0, i));
      for (j = 0; j < unit_size; j++)
	{
	  if ((c & 0xff) != 0 && (c & 0xff) != 0xff)
	    return false;
	  tmp_mask |= (c & 1) << ((nunit - 1 - i) * unit_size + j);
	  c = c >> BITS_PER_UNIT;
	}
    }

  if (mask != NULL)
    *mask = tmp_mask;

  return true;
}

/* Check whether a rotate of ROTL followed by an AND of CONTIG is
   equivalent to a shift followed by the AND.  In particular, CONTIG
   should not overlap the (rotated) bit 0/bit 63 gap.  Negative values
   for ROTL indicate a rotate to the right.  */

bool
s390_extzv_shift_ok (int bitsize, int rotl, unsigned HOST_WIDE_INT contig)
{
  int start, end;
  bool ok;

  ok = s390_contiguous_bitmask_nowrap_p (contig, bitsize, &start, &end);
  gcc_assert (ok);

  if (rotl >= 0)
    return (64 - end >= rotl);
  else
    {
      /* Translate "- rotate right" in BITSIZE mode to "rotate left" in
	 DIMode.  */
      rotl = -rotl + (64 - bitsize);
      return (start >= rotl);
    }
}

/* Check whether we can (and want to) split a double-word
   move in mode MODE from SRC to DST into two single-word
   moves, moving the subword FIRST_SUBWORD first.  */

bool
s390_split_ok_p (rtx dst, rtx src, machine_mode mode, int first_subword)
{
  /* Floating point and vector registers cannot be split.  */
  if (FP_REG_P (src) || FP_REG_P (dst) || VECTOR_REG_P (src) || VECTOR_REG_P (dst))
    return false;

  /* Non-offsettable memory references cannot be split.  */
  if ((GET_CODE (src) == MEM && !offsettable_memref_p (src))
      || (GET_CODE (dst) == MEM && !offsettable_memref_p (dst)))
    return false;

  /* Moving the first subword must not clobber a register
     needed to move the second subword.  */
  if (register_operand (dst, mode))
    {
      rtx subreg = operand_subword (dst, first_subword, 0, mode);
      if (reg_overlap_mentioned_p (subreg, src))
	return false;
    }

  return true;
}

/* Return true if it can be proven that [MEM1, MEM1 + SIZE]
   and [MEM2, MEM2 + SIZE] do overlap and false
   otherwise.  */

bool
s390_overlap_p (rtx mem1, rtx mem2, HOST_WIDE_INT size)
{
  rtx addr1, addr2, addr_delta;
  HOST_WIDE_INT delta;

  if (GET_CODE (mem1) != MEM || GET_CODE (mem2) != MEM)
    return true;

  if (size == 0)
    return false;

  addr1 = XEXP (mem1, 0);
  addr2 = XEXP (mem2, 0);

  addr_delta = simplify_binary_operation (MINUS, Pmode, addr2, addr1);

  /* This overlapping check is used by peepholes merging memory block operations.
     Overlapping operations would otherwise be recognized by the S/390 hardware
     and would fall back to a slower implementation. Allowing overlapping
     operations would lead to slow code but not to wrong code. Therefore we are
     somewhat optimistic if we cannot prove that the memory blocks are
     overlapping.
     That's why we return false here although this may accept operations on
     overlapping memory areas.  */
  if (!addr_delta || GET_CODE (addr_delta) != CONST_INT)
    return false;

  delta = INTVAL (addr_delta);

  if (delta == 0
      || (delta > 0 && delta < size)
      || (delta < 0 && -delta < size))
    return true;

  return false;
}

/* Check whether the address of memory reference MEM2 equals exactly
   the address of memory reference MEM1 plus DELTA.  Return true if
   we can prove this to be the case, false otherwise.  */

bool
s390_offset_p (rtx mem1, rtx mem2, rtx delta)
{
  rtx addr1, addr2, addr_delta;

  if (GET_CODE (mem1) != MEM || GET_CODE (mem2) != MEM)
    return false;

  addr1 = XEXP (mem1, 0);
  addr2 = XEXP (mem2, 0);

  addr_delta = simplify_binary_operation (MINUS, Pmode, addr2, addr1);
  if (!addr_delta || !rtx_equal_p (addr_delta, delta))
    return false;

  return true;
}

/* Expand logical operator CODE in mode MODE with operands OPERANDS.  */

void
s390_expand_logical_operator (enum rtx_code code, machine_mode mode,
			      rtx *operands)
{
  machine_mode wmode = mode;
  rtx dst = operands[0];
  rtx src1 = operands[1];
  rtx src2 = operands[2];
  rtx op, clob, tem;

  /* If we cannot handle the operation directly, use a temp register.  */
  if (!s390_logical_operator_ok_p (operands))
    dst = gen_reg_rtx (mode);

  /* QImode and HImode patterns make sense only if we have a destination
     in memory.  Otherwise perform the operation in SImode.  */
  if ((mode == QImode || mode == HImode) && GET_CODE (dst) != MEM)
    wmode = SImode;

  /* Widen operands if required.  */
  if (mode != wmode)
    {
      if (GET_CODE (dst) == SUBREG
	  && (tem = simplify_subreg (wmode, dst, mode, 0)) != 0)
	dst = tem;
      else if (REG_P (dst))
	dst = gen_rtx_SUBREG (wmode, dst, 0);
      else
	dst = gen_reg_rtx (wmode);

      if (GET_CODE (src1) == SUBREG
	  && (tem = simplify_subreg (wmode, src1, mode, 0)) != 0)
	src1 = tem;
      else if (GET_MODE (src1) != VOIDmode)
	src1 = gen_rtx_SUBREG (wmode, force_reg (mode, src1), 0);

      if (GET_CODE (src2) == SUBREG
	  && (tem = simplify_subreg (wmode, src2, mode, 0)) != 0)
	src2 = tem;
      else if (GET_MODE (src2) != VOIDmode)
	src2 = gen_rtx_SUBREG (wmode, force_reg (mode, src2), 0);
    }

  /* Emit the instruction.  */
  op = gen_rtx_SET (dst, gen_rtx_fmt_ee (code, wmode, src1, src2));
  clob = gen_rtx_CLOBBER (VOIDmode, gen_rtx_REG (CCmode, CC_REGNUM));
  emit_insn (gen_rtx_PARALLEL (VOIDmode, gen_rtvec (2, op, clob)));

  /* Fix up the destination if needed.  */
  if (dst != operands[0])
    emit_move_insn (operands[0], gen_lowpart (mode, dst));
}

/* Check whether OPERANDS are OK for a logical operation (AND, IOR, XOR).  */

bool
s390_logical_operator_ok_p (rtx *operands)
{
  /* If the destination operand is in memory, it needs to coincide
     with one of the source operands.  After reload, it has to be
     the first source operand.  */
  if (GET_CODE (operands[0]) == MEM)
    return rtx_equal_p (operands[0], operands[1])
	   || (!reload_completed && rtx_equal_p (operands[0], operands[2]));

  return true;
}

/* Narrow logical operation CODE of memory operand MEMOP with immediate
   operand IMMOP to switch from SS to SI type instructions.  */

void
s390_narrow_logical_operator (enum rtx_code code, rtx *memop, rtx *immop)
{
  int def = code == AND ? -1 : 0;
  HOST_WIDE_INT mask;
  int part;

  gcc_assert (GET_CODE (*memop) == MEM);
  gcc_assert (!MEM_VOLATILE_P (*memop));

  mask = s390_extract_part (*immop, QImode, def);
  part = s390_single_part (*immop, GET_MODE (*memop), QImode, def);
  gcc_assert (part >= 0);

  *memop = adjust_address (*memop, QImode, part);
  *immop = gen_int_mode (mask, QImode);
}


/* How to allocate a 'struct machine_function'.  */

static struct machine_function *
s390_init_machine_status (void)
{
  return ggc_cleared_alloc<machine_function> ();
}

/* Map for smallest class containing reg regno.  */

const enum reg_class regclass_map[FIRST_PSEUDO_REGISTER] =
{ GENERAL_REGS, ADDR_REGS, ADDR_REGS, ADDR_REGS,  /*  0 */
  ADDR_REGS,    ADDR_REGS, ADDR_REGS, ADDR_REGS,  /*  4 */
  ADDR_REGS,    ADDR_REGS, ADDR_REGS, ADDR_REGS,  /*  8 */
  ADDR_REGS,    ADDR_REGS, ADDR_REGS, ADDR_REGS,  /* 12 */
  FP_REGS,      FP_REGS,   FP_REGS,   FP_REGS,    /* 16 */
  FP_REGS,      FP_REGS,   FP_REGS,   FP_REGS,    /* 20 */
  FP_REGS,      FP_REGS,   FP_REGS,   FP_REGS,    /* 24 */
  FP_REGS,      FP_REGS,   FP_REGS,   FP_REGS,    /* 28 */
  ADDR_REGS,    CC_REGS,   ADDR_REGS, ADDR_REGS,  /* 32 */
  ACCESS_REGS,	ACCESS_REGS, VEC_REGS, VEC_REGS,  /* 36 */
  VEC_REGS, VEC_REGS, VEC_REGS, VEC_REGS,         /* 40 */
  VEC_REGS, VEC_REGS, VEC_REGS, VEC_REGS,         /* 44 */
  VEC_REGS, VEC_REGS, VEC_REGS, VEC_REGS,         /* 48 */
  VEC_REGS, VEC_REGS                              /* 52 */
};

/* Return attribute type of insn.  */

static enum attr_type
s390_safe_attr_type (rtx_insn *insn)
{
  if (recog_memoized (insn) >= 0)
    return get_attr_type (insn);
  else
    return TYPE_NONE;
}

/* Return attribute relative_long of insn.  */

static bool
s390_safe_relative_long_p (rtx_insn *insn)
{
  if (recog_memoized (insn) >= 0)
    return get_attr_relative_long (insn) == RELATIVE_LONG_YES;
  else
    return false;
}

/* Return true if DISP is a valid short displacement.  */

static bool
s390_short_displacement (rtx disp)
{
  /* No displacement is OK.  */
  if (!disp)
    return true;

  /* Without the long displacement facility we don't need to
     distingiush between long and short displacement.  */
  if (!TARGET_LONG_DISPLACEMENT)
    return true;

  /* Integer displacement in range.  */
  if (GET_CODE (disp) == CONST_INT)
    return INTVAL (disp) >= 0 && INTVAL (disp) < 4096;

  /* GOT offset is not OK, the GOT can be large.  */
  if (GET_CODE (disp) == CONST
      && GET_CODE (XEXP (disp, 0)) == UNSPEC
      && (XINT (XEXP (disp, 0), 1) == UNSPEC_GOT
	  || XINT (XEXP (disp, 0), 1) == UNSPEC_GOTNTPOFF))
    return false;

  /* All other symbolic constants are literal pool references,
     which are OK as the literal pool must be small.  */
  if (GET_CODE (disp) == CONST)
    return true;

  return false;
}

/* Attempts to split `ref', which should be UNSPEC_LTREF, into (base + `disp').
   If successful, also determines the
   following characteristics of `ref': `is_ptr' - whether it can be an
   LA argument, `is_base_ptr' - whether the resulting base is a well-known
   base register (stack/frame pointer, etc), `is_pool_ptr` - whether it is
   considered a literal pool pointer for purposes of avoiding two different
   literal pool pointers per insn during or after reload (`B' constraint).  */
static bool
s390_decompose_constant_pool_ref (rtx *ref, rtx *disp, bool *is_ptr,
				  bool *is_base_ptr, bool *is_pool_ptr)
{
  if (!*ref)
    return true;

  if (GET_CODE (*ref) == UNSPEC)
    switch (XINT (*ref, 1))
      {
      case UNSPEC_LTREF:
	if (!*disp)
	  *disp = gen_rtx_UNSPEC (Pmode,
				  gen_rtvec (1, XVECEXP (*ref, 0, 0)),
				  UNSPEC_LTREL_OFFSET);
	else
	  return false;

	*ref = XVECEXP (*ref, 0, 1);
	break;

      default:
	return false;
      }

  if (!REG_P (*ref) || GET_MODE (*ref) != Pmode)
    return false;

  if (REGNO (*ref) == STACK_POINTER_REGNUM
      || REGNO (*ref) == FRAME_POINTER_REGNUM
      || ((reload_completed || reload_in_progress)
	  && frame_pointer_needed
	  && REGNO (*ref) == HARD_FRAME_POINTER_REGNUM)
      || REGNO (*ref) == ARG_POINTER_REGNUM
      || (flag_pic
	  && REGNO (*ref) == PIC_OFFSET_TABLE_REGNUM))
    *is_ptr = *is_base_ptr = true;

  if ((reload_completed || reload_in_progress)
      && *ref == cfun->machine->base_reg)
    *is_ptr = *is_base_ptr = *is_pool_ptr = true;

  return true;
}

/* Decompose a RTL expression ADDR for a memory address into
   its components, returned in OUT.

   Returns false if ADDR is not a valid memory address, true
   otherwise.  If OUT is NULL, don't return the components,
   but check for validity only.

   Note: Only addresses in canonical form are recognized.
   LEGITIMIZE_ADDRESS should convert non-canonical forms to the
   canonical form so that they will be recognized.  */

static int
s390_decompose_address (rtx addr, struct s390_address *out)
{
  HOST_WIDE_INT offset = 0;
  rtx base = NULL_RTX;
  rtx indx = NULL_RTX;
  rtx disp = NULL_RTX;
  rtx orig_disp;
  bool pointer = false;
  bool base_ptr = false;
  bool indx_ptr = false;
  bool literal_pool = false;

  /* We may need to substitute the literal pool base register into the address
     below.  However, at this point we do not know which register is going to
     be used as base, so we substitute the arg pointer register.  This is going
     to be treated as holding a pointer below -- it shouldn't be used for any
     other purpose.  */
  rtx fake_pool_base = gen_rtx_REG (Pmode, ARG_POINTER_REGNUM);

  /* Decompose address into base + index + displacement.  */

  if (GET_CODE (addr) == REG || GET_CODE (addr) == UNSPEC)
    base = addr;

  else if (GET_CODE (addr) == PLUS)
    {
      rtx op0 = XEXP (addr, 0);
      rtx op1 = XEXP (addr, 1);
      enum rtx_code code0 = GET_CODE (op0);
      enum rtx_code code1 = GET_CODE (op1);

      if (code0 == REG || code0 == UNSPEC)
	{
	  if (code1 == REG || code1 == UNSPEC)
	    {
	      indx = op0;	/* index + base */
	      base = op1;
	    }

	  else
	    {
	      base = op0;	/* base + displacement */
	      disp = op1;
	    }
	}

      else if (code0 == PLUS)
	{
	  indx = XEXP (op0, 0);	/* index + base + disp */
	  base = XEXP (op0, 1);
	  disp = op1;
	}

      else
	{
	  return false;
	}
    }

  else
    disp = addr;		/* displacement */

  /* Extract integer part of displacement.  */
  orig_disp = disp;
  if (disp)
    {
      if (GET_CODE (disp) == CONST_INT)
	{
	  offset = INTVAL (disp);
	  disp = NULL_RTX;
	}
      else if (GET_CODE (disp) == CONST
	       && GET_CODE (XEXP (disp, 0)) == PLUS
	       && GET_CODE (XEXP (XEXP (disp, 0), 1)) == CONST_INT)
	{
	  offset = INTVAL (XEXP (XEXP (disp, 0), 1));
	  disp = XEXP (XEXP (disp, 0), 0);
	}
    }

  /* Strip off CONST here to avoid special case tests later.  */
  if (disp && GET_CODE (disp) == CONST)
    disp = XEXP (disp, 0);

  /* We can convert literal pool addresses to
     displacements by basing them off the base register.  */
  if (disp && GET_CODE (disp) == SYMBOL_REF && CONSTANT_POOL_ADDRESS_P (disp))
    {
      if (base || indx)
	return false;

      base = fake_pool_base, literal_pool = true;

      /* Mark up the displacement.  */
      disp = gen_rtx_UNSPEC (Pmode, gen_rtvec (1, disp),
			     UNSPEC_LTREL_OFFSET);
    }

  /* Validate base register.  */
  if (!s390_decompose_constant_pool_ref (&base, &disp, &pointer, &base_ptr,
					 &literal_pool))
    return false;

  /* Validate index register.  */
  if (!s390_decompose_constant_pool_ref (&indx, &disp, &pointer, &indx_ptr,
					 &literal_pool))
    return false;

  /* Prefer to use pointer as base, not index.  */
  if (base && indx && !base_ptr
      && (indx_ptr || (!REG_POINTER (base) && REG_POINTER (indx))))
    {
      rtx tmp = base;
      base = indx;
      indx = tmp;
    }

  /* Validate displacement.  */
  if (!disp)
    {
      /* If virtual registers are involved, the displacement will change later
	 anyway as the virtual registers get eliminated.  This could make a
	 valid displacement invalid, but it is more likely to make an invalid
	 displacement valid, because we sometimes access the register save area
	 via negative offsets to one of those registers.
	 Thus we don't check the displacement for validity here.  If after
	 elimination the displacement turns out to be invalid after all,
	 this is fixed up by reload in any case.  */
      /* LRA maintains always displacements up to date and we need to
	 know the displacement is right during all LRA not only at the
	 final elimination.  */
      if (lra_in_progress
	  || (base != arg_pointer_rtx
	      && indx != arg_pointer_rtx
	      && base != return_address_pointer_rtx
	      && indx != return_address_pointer_rtx
	      && base != frame_pointer_rtx
	      && indx != frame_pointer_rtx
	      && base != virtual_stack_vars_rtx
	      && indx != virtual_stack_vars_rtx))
	if (!DISP_IN_RANGE (offset))
	  return false;
    }
  else
    {
      /* All the special cases are pointers.  */
      pointer = true;

      /* In the small-PIC case, the linker converts @GOT
	 and @GOTNTPOFF offsets to possible displacements.  */
      if (GET_CODE (disp) == UNSPEC
	  && (XINT (disp, 1) == UNSPEC_GOT
	      || XINT (disp, 1) == UNSPEC_GOTNTPOFF)
	  && flag_pic == 1)
	{
	  ;
	}

      /* Accept pool label offsets.  */
      else if (GET_CODE (disp) == UNSPEC
	       && XINT (disp, 1) == UNSPEC_POOL_OFFSET)
	;

      /* Accept literal pool references.  */
      else if (GET_CODE (disp) == UNSPEC
	       && XINT (disp, 1) == UNSPEC_LTREL_OFFSET)
	{
	  /* In case CSE pulled a non literal pool reference out of
	     the pool we have to reject the address.  This is
	     especially important when loading the GOT pointer on non
	     zarch CPUs.  In this case the literal pool contains an lt
	     relative offset to the _GLOBAL_OFFSET_TABLE_ label which
	     will most likely exceed the displacement.  */
	  if (GET_CODE (XVECEXP (disp, 0, 0)) != SYMBOL_REF
	      || !CONSTANT_POOL_ADDRESS_P (XVECEXP (disp, 0, 0)))
	    return false;

	  orig_disp = gen_rtx_CONST (Pmode, disp);
	  if (offset)
	    {
	      /* If we have an offset, make sure it does not
		 exceed the size of the constant pool entry.
		 Otherwise we might generate an out-of-range
		 displacement for the base register form.  */
	      rtx sym = XVECEXP (disp, 0, 0);
	      if (offset >= GET_MODE_SIZE (get_pool_mode (sym)))
		return false;

	      orig_disp = plus_constant (Pmode, orig_disp, offset);
	    }
	}

      else
	return false;
    }

  if (!base && !indx)
    pointer = true;

  if (out)
    {
      out->base = base;
      out->indx = indx;
      out->disp = orig_disp;
      out->pointer = pointer;
      out->literal_pool = literal_pool;
    }

  return true;
}

/* Decompose a RTL expression OP for an address style operand into its
   components, and return the base register in BASE and the offset in
   OFFSET.  While OP looks like an address it is never supposed to be
   used as such.

   Return true if OP is a valid address operand, false if not.  */

bool
s390_decompose_addrstyle_without_index (rtx op, rtx *base,
					HOST_WIDE_INT *offset)
{
  rtx off = NULL_RTX;

  /* We can have an integer constant, an address register,
     or a sum of the two.  */
  if (CONST_SCALAR_INT_P (op))
    {
      off = op;
      op = NULL_RTX;
    }
  if (op && GET_CODE (op) == PLUS && CONST_SCALAR_INT_P (XEXP (op, 1)))
    {
      off = XEXP (op, 1);
      op = XEXP (op, 0);
    }
  while (op && GET_CODE (op) == SUBREG)
    op = SUBREG_REG (op);

  if (op && GET_CODE (op) != REG)
    return false;

  if (offset)
    {
      if (off == NULL_RTX)
	*offset = 0;
      else if (CONST_INT_P (off))
	*offset = INTVAL (off);
      else if (CONST_WIDE_INT_P (off))
	/* The offset will anyway be cut down to 12 bits so take just
	   the lowest order chunk of the wide int.  */
	*offset = CONST_WIDE_INT_ELT (off, 0);
      else
	gcc_unreachable ();
    }
  if (base)
    *base = op;

   return true;
}

/*  Check that OP is a valid shift count operand.
    It should be of the following structure:
      (subreg (and (plus (reg imm_op)) 2^k-1) 7)
    where subreg, and and plus are optional.

    If IMPLICIT_MASK is > 0 and OP contains and
      (AND ... immediate)
    it is checked whether IMPLICIT_MASK and the immediate match.
    Otherwise, no checking is performed.
  */
bool
s390_valid_shift_count (rtx op, HOST_WIDE_INT implicit_mask)
{
  /* Strip subreg.  */
  while (GET_CODE (op) == SUBREG && subreg_lowpart_p (op))
    op = XEXP (op, 0);

  /* Check for an and with proper constant.  */
  if (GET_CODE (op) == AND)
  {
    rtx op1 = XEXP (op, 0);
    rtx imm = XEXP (op, 1);

    if (GET_CODE (op1) == SUBREG && subreg_lowpart_p (op1))
      op1 = XEXP (op1, 0);

    if (!(register_operand (op1, GET_MODE (op1)) || GET_CODE (op1) == PLUS))
      return false;

    if (!immediate_operand (imm, GET_MODE (imm)))
      return false;

    HOST_WIDE_INT val = INTVAL (imm);
    if (implicit_mask > 0
	&& (val & implicit_mask) != implicit_mask)
      return false;

    op = op1;
  }

  /* Check the rest.  */
  return s390_decompose_addrstyle_without_index (op, NULL, NULL);
}

/* Return true if CODE is a valid address without index.  */

bool
s390_legitimate_address_without_index_p (rtx op)
{
  struct s390_address addr;

  if (!s390_decompose_address (XEXP (op, 0), &addr))
    return false;
  if (addr.indx)
    return false;

  return true;
}


/* Return TRUE if ADDR is an operand valid for a load/store relative
   instruction.  Be aware that the alignment of the operand needs to
   be checked separately.
   Valid addresses are single references or a sum of a reference and a
   constant integer. Return these parts in SYMREF and ADDEND.  You can
   pass NULL in REF and/or ADDEND if you are not interested in these
   values.  */

static bool
s390_loadrelative_operand_p (rtx addr, rtx *symref, HOST_WIDE_INT *addend)
{
  HOST_WIDE_INT tmpaddend = 0;

  if (GET_CODE (addr) == CONST)
    addr = XEXP (addr, 0);

  if (GET_CODE (addr) == PLUS)
    {
      if (!CONST_INT_P (XEXP (addr, 1)))
	return false;

      tmpaddend = INTVAL (XEXP (addr, 1));
      addr = XEXP (addr, 0);
    }

  if (GET_CODE (addr) == SYMBOL_REF
      || (GET_CODE (addr) == UNSPEC
	  && (XINT (addr, 1) == UNSPEC_GOTENT
	      || XINT (addr, 1) == UNSPEC_PLT31)))
    {
      if (symref)
	*symref = addr;
      if (addend)
	*addend = tmpaddend;

      return true;
    }
  return false;
}

/* Return true if the address in OP is valid for constraint letter C
   if wrapped in a MEM rtx.  Set LIT_POOL_OK to true if it literal
   pool MEMs should be accepted.  Only the Q, R, S, T constraint
   letters are allowed for C.  */

static int
s390_check_qrst_address (char c, rtx op, bool lit_pool_ok)
{
  rtx symref;
  struct s390_address addr;
  bool decomposed = false;

  if (!address_operand (op, GET_MODE (op)))
    return 0;

  /* This check makes sure that no symbolic address (except literal
     pool references) are accepted by the R or T constraints.  */
  if (s390_loadrelative_operand_p (op, &symref, NULL)
      && (!lit_pool_ok
          || !SYMBOL_REF_P (symref)
          || !CONSTANT_POOL_ADDRESS_P (symref)))
    return 0;

  /* Ensure literal pool references are only accepted if LIT_POOL_OK.  */
  if (!lit_pool_ok)
    {
      if (!s390_decompose_address (op, &addr))
	return 0;
      if (addr.literal_pool)
	return 0;
      decomposed = true;
    }

  /* With reload, we sometimes get intermediate address forms that are
     actually invalid as-is, but we need to accept them in the most
     generic cases below ('R' or 'T'), since reload will in fact fix
     them up.  LRA behaves differently here; we never see such forms,
     but on the other hand, we need to strictly reject every invalid
     address form.  After both reload and LRA invalid address forms
     must be rejected, because nothing will fix them up later.  Perform
     this check right up front.  */
  if (lra_in_progress || reload_completed)
    {
      if (!decomposed && !s390_decompose_address (op, &addr))
	return 0;
      decomposed = true;
    }

  switch (c)
    {
    case 'Q': /* no index short displacement */
      if (!decomposed && !s390_decompose_address (op, &addr))
	return 0;
      if (addr.indx)
	return 0;
      if (!s390_short_displacement (addr.disp))
	return 0;
      break;

    case 'R': /* with index short displacement */
      if (TARGET_LONG_DISPLACEMENT)
	{
	  if (!decomposed && !s390_decompose_address (op, &addr))
	    return 0;
	  if (!s390_short_displacement (addr.disp))
	    return 0;
	}
      /* Any invalid address here will be fixed up by reload,
	 so accept it for the most generic constraint.  */
      break;

    case 'S': /* no index long displacement */
      if (!decomposed && !s390_decompose_address (op, &addr))
	return 0;
      if (addr.indx)
	return 0;
      break;

    case 'T': /* with index long displacement */
      /* Any invalid address here will be fixed up by reload,
	 so accept it for the most generic constraint.  */
      break;

    default:
      return 0;
    }
  return 1;
}


/* Evaluates constraint strings described by the regular expression
   ([A|B|Z](Q|R|S|T))|Y and returns 1 if OP is a valid operand for
   the constraint given in STR, or 0 else.  */

int
s390_mem_constraint (const char *str, rtx op)
{
  char c = str[0];

  switch (c)
    {
    case 'A':
      /* Check for offsettable variants of memory constraints.  */
      if (!MEM_P (op) || MEM_VOLATILE_P (op))
	return 0;
      if ((reload_completed || reload_in_progress)
	  ? !offsettable_memref_p (op) : !offsettable_nonstrict_memref_p (op))
	return 0;
      return s390_check_qrst_address (str[1], XEXP (op, 0), true);
    case 'B':
      /* Check for non-literal-pool variants of memory constraints.  */
      if (!MEM_P (op))
	return 0;
      return s390_check_qrst_address (str[1], XEXP (op, 0), false);
    case 'Q':
    case 'R':
    case 'S':
    case 'T':
      if (GET_CODE (op) != MEM)
	return 0;
      return s390_check_qrst_address (c, XEXP (op, 0), true);
    case 'Y':
      /* Simply check for the basic form of a shift count.  Reload will
	 take care of making sure we have a proper base register.  */
      if (!s390_decompose_addrstyle_without_index (op, NULL, NULL))
	return 0;
      break;
    case 'Z':
      return s390_check_qrst_address (str[1], op, true);
    default:
      return 0;
    }
  return 1;
}


/* Evaluates constraint strings starting with letter O.  Input
   parameter C is the second letter following the "O" in the constraint
   string. Returns 1 if VALUE meets the respective constraint and 0
   otherwise.  */

int
s390_O_constraint_str (const char c, HOST_WIDE_INT value)
{
  if (!TARGET_EXTIMM)
    return 0;

  switch (c)
    {
    case 's':
      return trunc_int_for_mode (value, SImode) == value;

    case 'p':
      return value == 0
	|| s390_single_part (GEN_INT (value), DImode, SImode, 0) == 1;

    case 'n':
      return s390_single_part (GEN_INT (value - 1), DImode, SImode, -1) == 1;

    default:
      gcc_unreachable ();
    }
}


/* Evaluates constraint strings starting with letter N.  Parameter STR
   contains the letters following letter "N" in the constraint string.
   Returns true if VALUE matches the constraint.  */

int
s390_N_constraint_str (const char *str, HOST_WIDE_INT value)
{
  machine_mode mode, part_mode;
  int def;
  int part, part_goal;


  if (str[0] == 'x')
    part_goal = -1;
  else
    part_goal = str[0] - '0';

  switch (str[1])
    {
    case 'Q':
      part_mode = QImode;
      break;
    case 'H':
      part_mode = HImode;
      break;
    case 'S':
      part_mode = SImode;
      break;
    default:
      return 0;
    }

  switch (str[2])
    {
    case 'H':
      mode = HImode;
      break;
    case 'S':
      mode = SImode;
      break;
    case 'D':
      mode = DImode;
      break;
    default:
      return 0;
    }

  switch (str[3])
    {
    case '0':
      def = 0;
      break;
    case 'F':
      def = -1;
      break;
    default:
      return 0;
    }

  if (GET_MODE_SIZE (mode) <= GET_MODE_SIZE (part_mode))
    return 0;

  part = s390_single_part (GEN_INT (value), mode, part_mode, def);
  if (part < 0)
    return 0;
  if (part_goal != -1 && part_goal != part)
    return 0;

  return 1;
}


/* Returns true if the input parameter VALUE is a float zero.  */

int
s390_float_const_zero_p (rtx value)
{
  return (GET_MODE_CLASS (GET_MODE (value)) == MODE_FLOAT
	  && value == CONST0_RTX (GET_MODE (value)));
}

/* Implement TARGET_REGISTER_MOVE_COST.  */

static int
s390_register_move_cost (machine_mode mode,
			 reg_class_t from, reg_class_t to)
{
  /* On s390, copy between fprs and gprs is expensive.  */

  /* It becomes somewhat faster having ldgr/lgdr.  */
  if (TARGET_Z10 && GET_MODE_SIZE (mode) == 8)
    {
      /* ldgr is single cycle. */
      if (reg_classes_intersect_p (from, GENERAL_REGS)
	  && reg_classes_intersect_p (to, FP_REGS))
	return 1;
      /* lgdr needs 3 cycles. */
      if (reg_classes_intersect_p (to, GENERAL_REGS)
	  && reg_classes_intersect_p (from, FP_REGS))
	return 3;
    }

  /* Otherwise copying is done via memory.  */
  if ((reg_classes_intersect_p (from, GENERAL_REGS)
       && reg_classes_intersect_p (to, FP_REGS))
      || (reg_classes_intersect_p (from, FP_REGS)
	  && reg_classes_intersect_p (to, GENERAL_REGS)))
    return 10;

  /* We usually do not want to copy via CC.  */
  if (reg_classes_intersect_p (from, CC_REGS)
       || reg_classes_intersect_p (to, CC_REGS))
    return 5;

  return 1;
}

/* Implement TARGET_MEMORY_MOVE_COST.  */

static int
s390_memory_move_cost (machine_mode mode ATTRIBUTE_UNUSED,
		       reg_class_t rclass ATTRIBUTE_UNUSED,
		       bool in ATTRIBUTE_UNUSED)
{
  return 2;
}

/* Compute a (partial) cost for rtx X.  Return true if the complete
   cost has been computed, and false if subexpressions should be
   scanned.  In either case, *TOTAL contains the cost result.  The
   initial value of *TOTAL is the default value computed by
   rtx_cost.  It may be left unmodified.  OUTER_CODE contains the
   code of the superexpression of x.  */

static bool
s390_rtx_costs (rtx x, machine_mode mode, int outer_code,
		int opno ATTRIBUTE_UNUSED,
		int *total, bool speed ATTRIBUTE_UNUSED)
{
  int code = GET_CODE (x);
  switch (code)
    {
    case CONST:
    case CONST_INT:
    case LABEL_REF:
    case SYMBOL_REF:
    case CONST_DOUBLE:
    case CONST_WIDE_INT:
    case MEM:
      *total = 0;
      return true;
      case SET: {
	rtx dst = SET_DEST (x);
	rtx src = SET_SRC (x);

	switch (GET_CODE (src))
	  {
	    case IF_THEN_ELSE: {
	      /* Without this a conditional move instruction would be
		 accounted as 3 * COSTS_N_INSNS (set, if_then_else,
		 comparison operator).  That's a bit pessimistic.  */

	      if (!TARGET_Z196)
		return false;

	      rtx cond = XEXP (src, 0);
	      if (!CC_REG_P (XEXP (cond, 0)) || !CONST_INT_P (XEXP (cond, 1)))
		return false;

	      /* It is going to be a load/store on condition.  Make it
		 slightly more expensive than a normal load.  */
	      *total = COSTS_N_INSNS (1) + 2;

	      rtx then = XEXP (src, 1);
	      rtx els = XEXP (src, 2);

	      /* It is a real IF-THEN-ELSE.  An additional move will be
		 needed to implement that.  */
	      if (!TARGET_Z15 && reload_completed && !rtx_equal_p (dst, then)
		  && !rtx_equal_p (dst, els))
		*total += COSTS_N_INSNS (1) / 2;

	      /* A minor penalty for constants we cannot directly handle.  */
	      if ((CONST_INT_P (then) || CONST_INT_P (els))
		  && (!TARGET_Z13 || MEM_P (dst)
		      || (CONST_INT_P (then) && !satisfies_constraint_K (then))
		      || (CONST_INT_P (els) && !satisfies_constraint_K (els))))
		*total += COSTS_N_INSNS (1) / 2;

	      /* A store on condition can only handle register src operands.  */
	      if (MEM_P (dst) && (!REG_P (then) || !REG_P (els)))
		*total += COSTS_N_INSNS (1) / 2;

	      return true;
	    }
	  default:
	    break;
	  }

	switch (GET_CODE (dst))
	  {
	  case SUBREG:
	    if (!REG_P (SUBREG_REG (dst)))
	      *total += rtx_cost (SUBREG_REG (src), VOIDmode, SET, 0, speed);
	    /* fallthrough */
	  case REG:
	    /* If this is a VR -> VR copy, count the number of
	       registers.  */
	    if (VECTOR_MODE_P (GET_MODE (dst)) && REG_P (src))
	      {
		int nregs = s390_hard_regno_nregs (VR0_REGNUM, GET_MODE (dst));
		*total = COSTS_N_INSNS (nregs);
	      }
	    /* Same for GPRs.  */
	    else if (REG_P (src))
	      {
		int nregs
		  = s390_hard_regno_nregs (GPR0_REGNUM, GET_MODE (dst));
		*total = COSTS_N_INSNS (nregs);
	      }
	    else
	      /* Otherwise just cost the src.  */
	      *total += rtx_cost (src, mode, SET, 1, speed);
	    return true;
	    case MEM: {
	      rtx address = XEXP (dst, 0);
	      rtx tmp;
	      HOST_WIDE_INT tmp2;
	      if (s390_loadrelative_operand_p (address, &tmp, &tmp2))
		*total = COSTS_N_INSNS (1);
	      else
		*total = s390_address_cost (address, mode, 0, speed);
	      return true;
	    }
	  default:
	    /* Not handled for now, assume default costs.  */
	    *total = COSTS_N_INSNS (1);
	    return false;
	  }

	return false;
      }
    case IOR:

      /* nnrk, nngrk */
      if (TARGET_Z15
	  && (mode == SImode || mode == DImode)
	  && GET_CODE (XEXP (x, 0)) == NOT
	  && GET_CODE (XEXP (x, 1)) == NOT)
	{
	  *total = COSTS_N_INSNS (1);
	  if (!REG_P (XEXP (XEXP (x, 0), 0)))
	    *total += 1;
	  if (!REG_P (XEXP (XEXP (x, 1), 0)))
	    *total += 1;
	  return true;
	}

      /* risbg */
      if (GET_CODE (XEXP (x, 0)) == AND
	  && GET_CODE (XEXP (x, 1)) == ASHIFT
	  && REG_P (XEXP (XEXP (x, 0), 0))
	  && REG_P (XEXP (XEXP (x, 1), 0))
	  && CONST_INT_P (XEXP (XEXP (x, 0), 1))
	  && CONST_INT_P (XEXP (XEXP (x, 1), 1))
	  && (UINTVAL (XEXP (XEXP (x, 0), 1)) ==
	      (HOST_WIDE_INT_1U << UINTVAL (XEXP (XEXP (x, 1), 1))) - 1))
	{
	  *total = COSTS_N_INSNS (2);
	  return true;
	}

      /* ~AND on a 128 bit mode.  This can be done using a vector
	 instruction.  */
      if (TARGET_VXE
	  && GET_CODE (XEXP (x, 0)) == NOT
	  && GET_CODE (XEXP (x, 1)) == NOT
	  && REG_P (XEXP (XEXP (x, 0), 0))
	  && REG_P (XEXP (XEXP (x, 1), 0))
	  && GET_MODE_SIZE (GET_MODE (XEXP (XEXP (x, 0), 0))) == 16
	  && s390_hard_regno_mode_ok (VR0_REGNUM,
				      GET_MODE (XEXP (XEXP (x, 0), 0))))
	{
	  *total = COSTS_N_INSNS (1);
	  return true;
	}

      *total = COSTS_N_INSNS (1);
      return false;

    case AND:
      /* nork, nogrk */
      if (TARGET_Z15
	  && (mode == SImode || mode == DImode)
	  && GET_CODE (XEXP (x, 0)) == NOT
	  && GET_CODE (XEXP (x, 1)) == NOT)
	{
	  *total = COSTS_N_INSNS (1);
	  if (!REG_P (XEXP (XEXP (x, 0), 0)))
	    *total += 1;
	  if (!REG_P (XEXP (XEXP (x, 1), 0)))
	    *total += 1;
	  return true;
	}
      /* fallthrough */
    case ASHIFT:
    case ASHIFTRT:
    case LSHIFTRT:
    case ROTATE:
    case ROTATERT:
    case XOR:
    case NEG:
    case NOT:
    case PLUS:
    case MINUS:
      *total = COSTS_N_INSNS (1);
      return false;

    case MULT:
      switch (mode)
	{
	case E_SImode:
	  {
	    rtx left = XEXP (x, 0);
	    rtx right = XEXP (x, 1);
	    if (GET_CODE (right) == CONST_INT
		&& CONST_OK_FOR_K (INTVAL (right)))
	      *total = s390_cost->mhi;
	    else if (GET_CODE (left) == SIGN_EXTEND)
	      *total = s390_cost->mh;
	    else
	      *total = s390_cost->ms;  /* msr, ms, msy */
	    break;
	  }
	case E_DImode:
	  {
	    rtx left = XEXP (x, 0);
	    rtx right = XEXP (x, 1);
	    if (TARGET_ZARCH)
	      {
		if (GET_CODE (right) == CONST_INT
		    && CONST_OK_FOR_K (INTVAL (right)))
		  *total = s390_cost->mghi;
		else if (GET_CODE (left) == SIGN_EXTEND)
		  *total = s390_cost->msgf;
		else
		  *total = s390_cost->msg;  /* msgr, msg */
	      }
	    else /* TARGET_31BIT */
	      {
		if (GET_CODE (left) == SIGN_EXTEND
		    && GET_CODE (right) == SIGN_EXTEND)
		  /* mulsidi case: mr, m */
		  *total = s390_cost->m;
		else if (GET_CODE (left) == ZERO_EXTEND
			 && GET_CODE (right) == ZERO_EXTEND)
		  /* umulsidi case: ml, mlr */
		  *total = s390_cost->ml;
		else
		  /* Complex calculation is required.  */
		  *total = COSTS_N_INSNS (40);
	      }
	    break;
	  }
	case E_SFmode:
	case E_DFmode:
	  *total = s390_cost->mult_df;
	  break;
	case E_TFmode:
	  *total = s390_cost->mxbr;
	  break;
	default:
	  return false;
	}
      return false;

    case FMA:
      switch (mode)
	{
	case E_DFmode:
	  *total = s390_cost->madbr;
	  break;
	case E_SFmode:
	  *total = s390_cost->maebr;
	  break;
	default:
	  return false;
	}
      /* Negate in the third argument is free: FMSUB.  */
      if (GET_CODE (XEXP (x, 2)) == NEG)
	{
	  *total += (rtx_cost (XEXP (x, 0), mode, FMA, 0, speed)
		     + rtx_cost (XEXP (x, 1), mode, FMA, 1, speed)
		     + rtx_cost (XEXP (XEXP (x, 2), 0), mode, FMA, 2, speed));
	  return true;
	}
      return false;

    case UDIV:
    case UMOD:
      if (mode == TImode)	       /* 128 bit division */
	*total = s390_cost->dlgr;
      else if (mode == DImode)
	{
	  rtx right = XEXP (x, 1);
	  if (GET_CODE (right) == ZERO_EXTEND) /* 64 by 32 bit division */
	    *total = s390_cost->dlr;
	  else				       /* 64 by 64 bit division */
	    *total = s390_cost->dlgr;
	}
      else if (mode == SImode)         /* 32 bit division */
	*total = s390_cost->dlr;
      return false;

    case DIV:
    case MOD:
      if (mode == DImode)
	{
	  rtx right = XEXP (x, 1);
	  if (GET_CODE (right) == ZERO_EXTEND) /* 64 by 32 bit division */
	    if (TARGET_ZARCH)
	      *total = s390_cost->dsgfr;
	    else
	      *total = s390_cost->dr;
	  else				       /* 64 by 64 bit division */
	    *total = s390_cost->dsgr;
	}
      else if (mode == SImode)         /* 32 bit division */
	*total = s390_cost->dlr;
      else if (mode == SFmode)
	{
	  *total = s390_cost->debr;
	}
      else if (mode == DFmode)
	{
	  *total = s390_cost->ddbr;
	}
      else if (mode == TFmode)
	{
	  *total = s390_cost->dxbr;
	}
      return false;

    case SQRT:
      if (mode == SFmode)
	*total = s390_cost->sqebr;
      else if (mode == DFmode)
	*total = s390_cost->sqdbr;
      else /* TFmode */
	*total = s390_cost->sqxbr;
      return false;

    case SIGN_EXTEND:
    case ZERO_EXTEND:
      if (outer_code == MULT || outer_code == DIV || outer_code == MOD
	  || outer_code == PLUS || outer_code == MINUS
	  || outer_code == COMPARE)
	*total = 0;
      return false;

    case COMPARE:
      *total = COSTS_N_INSNS (1);

      /* nxrk, nxgrk ~(a^b)==0 */
      if (TARGET_Z15
	  && GET_CODE (XEXP (x, 0)) == NOT
	  && XEXP (x, 1) == const0_rtx
	  && GET_CODE (XEXP (XEXP (x, 0), 0)) == XOR
	  && (GET_MODE (XEXP (x, 0)) == SImode || GET_MODE (XEXP (x, 0)) == DImode)
	  && mode == CCZmode)
	{
	  if (!REG_P (XEXP (XEXP (XEXP (x, 0), 0), 0)))
	    *total += 1;
	  if (!REG_P (XEXP (XEXP (XEXP (x, 0), 0), 1)))
	    *total += 1;
	  return true;
	}

      /* nnrk, nngrk, nork, nogrk */
      if (TARGET_Z15
	  && (GET_CODE (XEXP (x, 0)) == AND || GET_CODE (XEXP (x, 0)) == IOR)
	  && XEXP (x, 1) == const0_rtx
	  && (GET_MODE (XEXP (x, 0)) == SImode || GET_MODE (XEXP (x, 0)) == DImode)
	  && GET_CODE (XEXP (XEXP (x, 0), 0)) == NOT
	  && GET_CODE (XEXP (XEXP (x, 0), 1)) == NOT
	  && mode == CCZmode)
	{
	  if (!REG_P (XEXP (XEXP (XEXP (x, 0), 0), 0)))
	    *total += 1;
	  if (!REG_P (XEXP (XEXP (XEXP (x, 0), 1), 0)))
	    *total += 1;
	  return true;
	}

      if (GET_CODE (XEXP (x, 0)) == AND
	  && GET_CODE (XEXP (x, 1)) == CONST_INT
	  && GET_CODE (XEXP (XEXP (x, 0), 1)) == CONST_INT)
	{
	  rtx op0 = XEXP (XEXP (x, 0), 0);
	  rtx op1 = XEXP (XEXP (x, 0), 1);
	  rtx op2 = XEXP (x, 1);

	  if (memory_operand (op0, GET_MODE (op0))
	      && s390_tm_ccmode (op1, op2, 0) != VOIDmode)
	    return true;
	  if (register_operand (op0, GET_MODE (op0))
	      && s390_tm_ccmode (op1, op2, 1) != VOIDmode)
	    return true;
	}
      return false;

    default:
      return false;
    }
}

/* Return the cost of an address rtx ADDR.  */

static int
s390_address_cost (rtx addr, machine_mode mode ATTRIBUTE_UNUSED,
		   addr_space_t as ATTRIBUTE_UNUSED,
		   bool speed ATTRIBUTE_UNUSED)
{
  struct s390_address ad;
  if (!s390_decompose_address (addr, &ad))
    return 1000;

  return ad.indx? COSTS_N_INSNS (1) + 1 : COSTS_N_INSNS (1);
}

/* Implement targetm.vectorize.builtin_vectorization_cost.  */
static int
s390_builtin_vectorization_cost (enum vect_cost_for_stmt type_of_cost,
				 tree vectype,
				 int misalign ATTRIBUTE_UNUSED)
{
  switch (type_of_cost)
    {
      case scalar_stmt:
      case scalar_load:
      case scalar_store:
      case vector_stmt:
      case vector_load:
      case vector_store:
      case vector_gather_load:
      case vector_scatter_store:
      case vec_to_scalar:
      case scalar_to_vec:
      case cond_branch_not_taken:
      case vec_perm:
      case vec_promote_demote:
      case unaligned_load:
      case unaligned_store:
	return 1;

      case cond_branch_taken:
	return 3;

      case vec_construct:
	return TYPE_VECTOR_SUBPARTS (vectype) - 1;

      default:
	gcc_unreachable ();
    }
}

/* If OP is a SYMBOL_REF of a thread-local symbol, return its TLS mode,
   otherwise return 0.  */

int
tls_symbolic_operand (rtx op)
{
  if (GET_CODE (op) != SYMBOL_REF)
    return 0;
  return SYMBOL_REF_TLS_MODEL (op);
}

/* Split DImode access register reference REG (on 64-bit) into its constituent
   low and high parts, and store them into LO and HI.  Note that gen_lowpart/
   gen_highpart cannot be used as they assume all registers are word-sized,
   while our access registers have only half that size.  */

void
s390_split_access_reg (rtx reg, rtx *lo, rtx *hi)
{
  gcc_assert (TARGET_64BIT);
  gcc_assert (ACCESS_REG_P (reg));
  gcc_assert (GET_MODE (reg) == DImode);
  gcc_assert (!(REGNO (reg) & 1));

  *lo = gen_rtx_REG (SImode, REGNO (reg) + 1);
  *hi = gen_rtx_REG (SImode, REGNO (reg));
}

/* Return true if OP contains a symbol reference */

bool
symbolic_reference_mentioned_p (rtx op)
{
  const char *fmt;
  int i;

  if (GET_CODE (op) == SYMBOL_REF || GET_CODE (op) == LABEL_REF)
    return 1;

  fmt = GET_RTX_FORMAT (GET_CODE (op));
  for (i = GET_RTX_LENGTH (GET_CODE (op)) - 1; i >= 0; i--)
    {
      if (fmt[i] == 'E')
	{
	  int j;

	  for (j = XVECLEN (op, i) - 1; j >= 0; j--)
	    if (symbolic_reference_mentioned_p (XVECEXP (op, i, j)))
	      return 1;
	}

      else if (fmt[i] == 'e' && symbolic_reference_mentioned_p (XEXP (op, i)))
	return 1;
    }

  return 0;
}

/* Return true if OP contains a reference to a thread-local symbol.  */

bool
tls_symbolic_reference_mentioned_p (rtx op)
{
  const char *fmt;
  int i;

  if (GET_CODE (op) == SYMBOL_REF)
    return tls_symbolic_operand (op);

  fmt = GET_RTX_FORMAT (GET_CODE (op));
  for (i = GET_RTX_LENGTH (GET_CODE (op)) - 1; i >= 0; i--)
    {
      if (fmt[i] == 'E')
	{
	  int j;

	  for (j = XVECLEN (op, i) - 1; j >= 0; j--)
	    if (tls_symbolic_reference_mentioned_p (XVECEXP (op, i, j)))
	      return true;
	}

      else if (fmt[i] == 'e' && tls_symbolic_reference_mentioned_p (XEXP (op, i)))
	return true;
    }

  return false;
}


/* Return true if OP is a legitimate general operand when
   generating PIC code.  It is given that flag_pic is on
   and that OP satisfies CONSTANT_P.  */

int
legitimate_pic_operand_p (rtx op)
{
  /* Accept all non-symbolic constants.  */
  if (!SYMBOLIC_CONST (op))
    return 1;

  /* Accept addresses that can be expressed relative to (pc).  */
  if (larl_operand (op, VOIDmode))
    return 1;

  /* Reject everything else; must be handled
     via emit_symbolic_move.  */
  return 0;
}

/* Returns true if the constant value OP is a legitimate general operand.
   It is given that OP satisfies CONSTANT_P.  */

static bool
s390_legitimate_constant_p (machine_mode mode, rtx op)
{
  if (TARGET_VX && VECTOR_MODE_P (mode) && GET_CODE (op) == CONST_VECTOR)
    {
      if (GET_MODE_SIZE (mode) != 16)
	return 0;

      if (!satisfies_constraint_j00 (op)
	  && !satisfies_constraint_jm1 (op)
	  && !satisfies_constraint_jKK (op)
	  && !satisfies_constraint_jxx (op)
	  && !satisfies_constraint_jyy (op))
	return 0;
    }

  /* Accept all non-symbolic constants.  */
  if (!SYMBOLIC_CONST (op))
    return 1;

  /* Accept immediate LARL operands.  */
  if (larl_operand (op, mode))
    return 1;

  /* Thread-local symbols are never legal constants.  This is
     so that emit_call knows that computing such addresses
     might require a function call.  */
  if (TLS_SYMBOLIC_CONST (op))
    return 0;

  /* In the PIC case, symbolic constants must *not* be
     forced into the literal pool.  We accept them here,
     so that they will be handled by emit_symbolic_move.  */
  if (flag_pic)
    return 1;

  /* All remaining non-PIC symbolic constants are
     forced into the literal pool.  */
  return 0;
}

/* Determine if it's legal to put X into the constant pool.  This
   is not possible if X contains the address of a symbol that is
   not constant (TLS) or not known at final link time (PIC).  */

static bool
s390_cannot_force_const_mem (machine_mode mode, rtx x)
{
  switch (GET_CODE (x))
    {
    case CONST_INT:
    case CONST_DOUBLE:
    case CONST_WIDE_INT:
    case CONST_VECTOR:
      /* Accept all non-symbolic constants.  */
      return false;

    case NEG:
      /* Accept an unary '-' only on scalar numeric constants.  */
      switch (GET_CODE (XEXP (x, 0)))
	{
	case CONST_INT:
	case CONST_DOUBLE:
	case CONST_WIDE_INT:
	  return false;
	default:
	  return true;
	}

    case LABEL_REF:
      /* Labels are OK iff we are non-PIC.  */
      return flag_pic != 0;

    case SYMBOL_REF:
      /* 'Naked' TLS symbol references are never OK,
	 non-TLS symbols are OK iff we are non-PIC.  */
      if (tls_symbolic_operand (x))
	return true;
      else
	return flag_pic != 0;

    case CONST:
      return s390_cannot_force_const_mem (mode, XEXP (x, 0));
    case PLUS:
    case MINUS:
      return s390_cannot_force_const_mem (mode, XEXP (x, 0))
	     || s390_cannot_force_const_mem (mode, XEXP (x, 1));

    case UNSPEC:
      switch (XINT (x, 1))
	{
	/* Only lt-relative or GOT-relative UNSPECs are OK.  */
	case UNSPEC_LTREL_OFFSET:
	case UNSPEC_GOT:
	case UNSPEC_GOTOFF:
	case UNSPEC_PLTOFF:
	case UNSPEC_TLSGD:
	case UNSPEC_TLSLDM:
	case UNSPEC_NTPOFF:
	case UNSPEC_DTPOFF:
	case UNSPEC_GOTNTPOFF:
	case UNSPEC_INDNTPOFF:
	  return false;

	/* If the literal pool shares the code section, be put
	   execute template placeholders into the pool as well.  */
	case UNSPEC_INSN:
	default:
	  return true;
	}
      break;

    default:
      gcc_unreachable ();
    }
}

/* Returns true if the constant value OP is a legitimate general
   operand during and after reload.  The difference to
   legitimate_constant_p is that this function will not accept
   a constant that would need to be forced to the literal pool
   before it can be used as operand.
   This function accepts all constants which can be loaded directly
   into a GPR.  */

bool
legitimate_reload_constant_p (rtx op)
{
  /* Accept la(y) operands.  */
  if (GET_CODE (op) == CONST_INT
      && DISP_IN_RANGE (INTVAL (op)))
    return true;

  /* Accept l(g)hi/l(g)fi operands.  */
  if (GET_CODE (op) == CONST_INT
      && (CONST_OK_FOR_K (INTVAL (op)) || CONST_OK_FOR_Os (INTVAL (op))))
    return true;

  /* Accept lliXX operands.  */
  if (TARGET_ZARCH
      && GET_CODE (op) == CONST_INT
      && trunc_int_for_mode (INTVAL (op), word_mode) == INTVAL (op)
      && s390_single_part (op, word_mode, HImode, 0) >= 0)
  return true;

  if (TARGET_EXTIMM
      && GET_CODE (op) == CONST_INT
      && trunc_int_for_mode (INTVAL (op), word_mode) == INTVAL (op)
      && s390_single_part (op, word_mode, SImode, 0) >= 0)
    return true;

  /* Accept larl operands.  */
  if (larl_operand (op, VOIDmode))
    return true;

  /* Accept floating-point zero operands that fit into a single GPR.  */
  if (GET_CODE (op) == CONST_DOUBLE
      && s390_float_const_zero_p (op)
      && GET_MODE_SIZE (GET_MODE (op)) <= UNITS_PER_WORD)
    return true;

  /* Accept double-word operands that can be split.  */
  if (GET_CODE (op) == CONST_WIDE_INT
      || (GET_CODE (op) == CONST_INT
	  && trunc_int_for_mode (INTVAL (op), word_mode) != INTVAL (op)))
    {
      machine_mode dword_mode = word_mode == SImode ? DImode : TImode;
      rtx hi = operand_subword (op, 0, 0, dword_mode);
      rtx lo = operand_subword (op, 1, 0, dword_mode);
      return legitimate_reload_constant_p (hi)
	     && legitimate_reload_constant_p (lo);
    }

  /* Everything else cannot be handled without reload.  */
  return false;
}

/* Returns true if the constant value OP is a legitimate fp operand
   during and after reload.
   This function accepts all constants which can be loaded directly
   into an FPR.  */

static bool
legitimate_reload_fp_constant_p (rtx op)
{
  /* Accept floating-point zero operands if the load zero instruction
     can be used.  Prior to z196 the load fp zero instruction caused a
     performance penalty if the result is used as BFP number.  */
  if (TARGET_Z196
      && GET_CODE (op) == CONST_DOUBLE
      && s390_float_const_zero_p (op))
    return true;

  return false;
}

/* Returns true if the constant value OP is a legitimate vector operand
   during and after reload.
   This function accepts all constants which can be loaded directly
   into an VR.  */

static bool
legitimate_reload_vector_constant_p (rtx op)
{
  if (TARGET_VX && GET_MODE_SIZE (GET_MODE (op)) == 16
      && (satisfies_constraint_j00 (op)
	  || satisfies_constraint_jm1 (op)
	  || satisfies_constraint_jKK (op)
	  || satisfies_constraint_jxx (op)
	  || satisfies_constraint_jyy (op)))
    return true;

  return false;
}

/* Given an rtx OP being reloaded into a reg required to be in class RCLASS,
   return the class of reg to actually use.  */

static reg_class_t
s390_preferred_reload_class (rtx op, reg_class_t rclass)
{
  switch (GET_CODE (op))
    {
      /* Constants we cannot reload into general registers
	 must be forced into the literal pool.  */
      case CONST_VECTOR:
      case CONST_DOUBLE:
      case CONST_INT:
      case CONST_WIDE_INT:
	if (reg_class_subset_p (GENERAL_REGS, rclass)
	    && legitimate_reload_constant_p (op))
	  return GENERAL_REGS;
	else if (reg_class_subset_p (ADDR_REGS, rclass)
		 && legitimate_reload_constant_p (op))
	  return ADDR_REGS;
	else if (reg_class_subset_p (FP_REGS, rclass)
		 && legitimate_reload_fp_constant_p (op))
	  return FP_REGS;
	else if (reg_class_subset_p (VEC_REGS, rclass)
		 && legitimate_reload_vector_constant_p (op))
	  return VEC_REGS;

	return NO_REGS;

      /* If a symbolic constant or a PLUS is reloaded,
	 it is most likely being used as an address, so
	 prefer ADDR_REGS.  If 'class' is not a superset
	 of ADDR_REGS, e.g. FP_REGS, reject this reload.  */
      case CONST:
	/* Symrefs cannot be pushed into the literal pool with -fPIC
	   so we *MUST NOT* return NO_REGS for these cases
	   (s390_cannot_force_const_mem will return true).

	   On the other hand we MUST return NO_REGS for symrefs with
	   invalid addend which might have been pushed to the literal
	   pool (no -fPIC).  Usually we would expect them to be
	   handled via secondary reload but this does not happen if
	   they are used as literal pool slot replacement in reload
	   inheritance (see emit_input_reload_insns).  */
	if (GET_CODE (XEXP (op, 0)) == PLUS
	    && GET_CODE (XEXP (XEXP(op, 0), 0)) == SYMBOL_REF
	    && GET_CODE (XEXP (XEXP(op, 0), 1)) == CONST_INT)
	  {
	    if (flag_pic && reg_class_subset_p (ADDR_REGS, rclass))
	      return ADDR_REGS;
	    else
	      return NO_REGS;
	  }
	/* fallthrough */
      case LABEL_REF:
      case SYMBOL_REF:
	if (!legitimate_reload_constant_p (op))
	  return NO_REGS;
	/* fallthrough */
      case PLUS:
	/* load address will be used.  */
	if (reg_class_subset_p (ADDR_REGS, rclass))
	  return ADDR_REGS;
	else
	  return NO_REGS;

      default:
	break;
    }

  return rclass;
}

/* Return true if ADDR is SYMBOL_REF + addend with addend being a
   multiple of ALIGNMENT and the SYMBOL_REF being naturally
   aligned.  */

bool
s390_check_symref_alignment (rtx addr, HOST_WIDE_INT alignment)
{
  HOST_WIDE_INT addend;
  rtx symref;

  /* The "required alignment" might be 0 (e.g. for certain structs
     accessed via BLKmode).  Early abort in this case, as well as when
     an alignment > 8 is required.  */
  if (alignment < 2 || alignment > 8)
    return false;

  if (!s390_loadrelative_operand_p (addr, &symref, &addend))
    return false;

  if (addend & (alignment - 1))
    return false;

  if (GET_CODE (symref) == SYMBOL_REF)
    {
      /* s390_encode_section_info is not called for anchors, since they don't
	 have corresponding VAR_DECLs.  Therefore, we cannot rely on
	 SYMBOL_FLAG_NOTALIGN{2,4,8}_P returning useful information.  */
      if (SYMBOL_REF_ANCHOR_P (symref))
	{
	  HOST_WIDE_INT block_offset = SYMBOL_REF_BLOCK_OFFSET (symref);
	  unsigned int block_alignment = (SYMBOL_REF_BLOCK (symref)->alignment
					  / BITS_PER_UNIT);

	  gcc_assert (block_offset >= 0);
	  return ((block_offset & (alignment - 1)) == 0
		  && block_alignment >= alignment);
	}

      /* We have load-relative instructions for 2-byte, 4-byte, and
	 8-byte alignment so allow only these.  */
      switch (alignment)
	{
	case 8:	return !SYMBOL_FLAG_NOTALIGN8_P (symref);
	case 4:	return !SYMBOL_FLAG_NOTALIGN4_P (symref);
	case 2:	return !SYMBOL_FLAG_NOTALIGN2_P (symref);
	default: return false;
	}
    }

  if (GET_CODE (symref) == UNSPEC
      && alignment <= UNITS_PER_LONG)
    return true;

  return false;
}

/* ADDR is moved into REG using larl.  If ADDR isn't a valid larl
   operand SCRATCH is used to reload the even part of the address and
   adding one.  */

void
s390_reload_larl_operand (rtx reg, rtx addr, rtx scratch)
{
  HOST_WIDE_INT addend;
  rtx symref;

  if (!s390_loadrelative_operand_p (addr, &symref, &addend))
    gcc_unreachable ();

  if (!(addend & 1))
    /* Easy case.  The addend is even so larl will do fine.  */
    emit_move_insn (reg, addr);
  else
    {
      /* We can leave the scratch register untouched if the target
	 register is a valid base register.  */
      if (REGNO (reg) < FIRST_PSEUDO_REGISTER
	  && REGNO_REG_CLASS (REGNO (reg)) == ADDR_REGS)
	scratch = reg;

      gcc_assert (REGNO (scratch) < FIRST_PSEUDO_REGISTER);
      gcc_assert (REGNO_REG_CLASS (REGNO (scratch)) == ADDR_REGS);

      if (addend != 1)
	emit_move_insn (scratch,
			gen_rtx_CONST (Pmode,
				       gen_rtx_PLUS (Pmode, symref,
						     GEN_INT (addend - 1))));
      else
	emit_move_insn (scratch, symref);

      /* Increment the address using la in order to avoid clobbering cc.  */
      s390_load_address (reg, gen_rtx_PLUS (Pmode, scratch, const1_rtx));
    }
}

/* Generate what is necessary to move between REG and MEM using
   SCRATCH.  The direction is given by TOMEM.  */

void
s390_reload_symref_address (rtx reg, rtx mem, rtx scratch, bool tomem)
{
  /* Reload might have pulled a constant out of the literal pool.
     Force it back in.  */
  if (CONST_INT_P (mem) || GET_CODE (mem) == CONST_DOUBLE
      || GET_CODE (mem) == CONST_WIDE_INT
      || GET_CODE (mem) == CONST_VECTOR
      || GET_CODE (mem) == CONST)
    mem = force_const_mem (GET_MODE (reg), mem);

  gcc_assert (MEM_P (mem));

  /* For a load from memory we can leave the scratch register
     untouched if the target register is a valid base register.  */
  if (!tomem
      && REGNO (reg) < FIRST_PSEUDO_REGISTER
      && REGNO_REG_CLASS (REGNO (reg)) == ADDR_REGS
      && GET_MODE (reg) == GET_MODE (scratch))
    scratch = reg;

  /* Load address into scratch register.  Since we can't have a
     secondary reload for a secondary reload we have to cover the case
     where larl would need a secondary reload here as well.  */
  s390_reload_larl_operand (scratch, XEXP (mem, 0), scratch);

  /* Now we can use a standard load/store to do the move.  */
  if (tomem)
    emit_move_insn (replace_equiv_address (mem, scratch), reg);
  else
    emit_move_insn (reg, replace_equiv_address (mem, scratch));
}

/* Inform reload about cases where moving X with a mode MODE to a register in
   RCLASS requires an extra scratch or immediate register.  Return the class
   needed for the immediate register.  */

static reg_class_t
s390_secondary_reload (bool in_p, rtx x, reg_class_t rclass_i,
		       machine_mode mode, secondary_reload_info *sri)
{
  enum reg_class rclass = (enum reg_class) rclass_i;

  /* Intermediate register needed.  */
  if (reg_classes_intersect_p (CC_REGS, rclass))
    return GENERAL_REGS;

  if (TARGET_VX)
    {
      /* The vst/vl vector move instructions allow only for short
	 displacements.  */
      if (MEM_P (x)
	  && GET_CODE (XEXP (x, 0)) == PLUS
	  && GET_CODE (XEXP (XEXP (x, 0), 1)) == CONST_INT
	  && !SHORT_DISP_IN_RANGE(INTVAL (XEXP (XEXP (x, 0), 1)))
	  && reg_class_subset_p (rclass, VEC_REGS)
	  && (!reg_class_subset_p (rclass, FP_REGS)
	      || (GET_MODE_SIZE (mode) > 8
		  && s390_class_max_nregs (FP_REGS, mode) == 1)))
	{
	  if (in_p)
	    sri->icode = (TARGET_64BIT ?
			  CODE_FOR_reloaddi_la_in :
			  CODE_FOR_reloadsi_la_in);
	  else
	    sri->icode = (TARGET_64BIT ?
			  CODE_FOR_reloaddi_la_out :
			  CODE_FOR_reloadsi_la_out);
	}
    }

  if (TARGET_Z10)
    {
      HOST_WIDE_INT offset;
      rtx symref;

      /* On z10 several optimizer steps may generate larl operands with
	 an odd addend.  */
      if (in_p
	  && s390_loadrelative_operand_p (x, &symref, &offset)
	  && mode == Pmode
	  && !SYMBOL_FLAG_NOTALIGN2_P (symref)
	  && (offset & 1) == 1)
	sri->icode = ((mode == DImode) ? CODE_FOR_reloaddi_larl_odd_addend_z10
		      : CODE_FOR_reloadsi_larl_odd_addend_z10);

      /* Handle all the (mem (symref)) accesses we cannot use the z10
	 instructions for.  */
      if (MEM_P (x)
	  && s390_loadrelative_operand_p (XEXP (x, 0), NULL, NULL)
	  && (mode == QImode
	      || !reg_class_subset_p (rclass, GENERAL_REGS)
	      || GET_MODE_SIZE (mode) > UNITS_PER_WORD
	      || !s390_check_symref_alignment (XEXP (x, 0),
					       GET_MODE_SIZE (mode))))
	{
#define __SECONDARY_RELOAD_CASE(M,m)					\
	  case E_##M##mode:						\
	    if (TARGET_64BIT)						\
	      sri->icode = in_p ? CODE_FOR_reload##m##di_toreg_z10 :	\
				  CODE_FOR_reload##m##di_tomem_z10;	\
	    else							\
	      sri->icode = in_p ? CODE_FOR_reload##m##si_toreg_z10 :	\
				  CODE_FOR_reload##m##si_tomem_z10;	\
	  break;

	  switch (GET_MODE (x))
	    {
	      __SECONDARY_RELOAD_CASE (QI, qi);
	      __SECONDARY_RELOAD_CASE (HI, hi);
	      __SECONDARY_RELOAD_CASE (SI, si);
	      __SECONDARY_RELOAD_CASE (DI, di);
	      __SECONDARY_RELOAD_CASE (TI, ti);
	      __SECONDARY_RELOAD_CASE (SF, sf);
	      __SECONDARY_RELOAD_CASE (DF, df);
	      __SECONDARY_RELOAD_CASE (TF, tf);
	      __SECONDARY_RELOAD_CASE (SD, sd);
	      __SECONDARY_RELOAD_CASE (DD, dd);
	      __SECONDARY_RELOAD_CASE (TD, td);
	      __SECONDARY_RELOAD_CASE (V1QI, v1qi);
	      __SECONDARY_RELOAD_CASE (V2QI, v2qi);
	      __SECONDARY_RELOAD_CASE (V4QI, v4qi);
	      __SECONDARY_RELOAD_CASE (V8QI, v8qi);
	      __SECONDARY_RELOAD_CASE (V16QI, v16qi);
	      __SECONDARY_RELOAD_CASE (V1HI, v1hi);
	      __SECONDARY_RELOAD_CASE (V2HI, v2hi);
	      __SECONDARY_RELOAD_CASE (V4HI, v4hi);
	      __SECONDARY_RELOAD_CASE (V8HI, v8hi);
	      __SECONDARY_RELOAD_CASE (V1SI, v1si);
	      __SECONDARY_RELOAD_CASE (V2SI, v2si);
	      __SECONDARY_RELOAD_CASE (V4SI, v4si);
	      __SECONDARY_RELOAD_CASE (V1DI, v1di);
	      __SECONDARY_RELOAD_CASE (V2DI, v2di);
	      __SECONDARY_RELOAD_CASE (V1TI, v1ti);
	      __SECONDARY_RELOAD_CASE (V1SF, v1sf);
	      __SECONDARY_RELOAD_CASE (V2SF, v2sf);
	      __SECONDARY_RELOAD_CASE (V4SF, v4sf);
	      __SECONDARY_RELOAD_CASE (V1DF, v1df);
	      __SECONDARY_RELOAD_CASE (V2DF, v2df);
	      __SECONDARY_RELOAD_CASE (V1TF, v1tf);
	    default:
	      gcc_unreachable ();
	    }
#undef __SECONDARY_RELOAD_CASE
	}
    }

  /* We need a scratch register when loading a PLUS expression which
     is not a legitimate operand of the LOAD ADDRESS instruction.  */
  /* LRA can deal with transformation of plus op very well -- so we
     don't need to prompt LRA in this case.  */
  if (! lra_in_progress && in_p && s390_plus_operand (x, mode))
    sri->icode = (TARGET_64BIT ?
		  CODE_FOR_reloaddi_plus : CODE_FOR_reloadsi_plus);

  /* Performing a multiword move from or to memory we have to make sure the
     second chunk in memory is addressable without causing a displacement
     overflow.  If that would be the case we calculate the address in
     a scratch register.  */
  if (MEM_P (x)
      && GET_CODE (XEXP (x, 0)) == PLUS
      && GET_CODE (XEXP (XEXP (x, 0), 1)) == CONST_INT
      && !DISP_IN_RANGE (INTVAL (XEXP (XEXP (x, 0), 1))
			 + GET_MODE_SIZE (mode) - 1))
    {
      /* For GENERAL_REGS a displacement overflow is no problem if occurring
	 in a s_operand address since we may fallback to lm/stm.  So we only
	 have to care about overflows in the b+i+d case.  */
      if ((reg_classes_intersect_p (GENERAL_REGS, rclass)
	   && s390_class_max_nregs (GENERAL_REGS, mode) > 1
	   && GET_CODE (XEXP (XEXP (x, 0), 0)) == PLUS)
	  /* For FP_REGS no lm/stm is available so this check is triggered
	     for displacement overflows in b+i+d and b+d like addresses.  */
	  || (reg_classes_intersect_p (FP_REGS, rclass)
	      && s390_class_max_nregs (FP_REGS, mode) > 1))
	{
	  if (in_p)
	    sri->icode = (TARGET_64BIT ?
			  CODE_FOR_reloaddi_la_in :
			  CODE_FOR_reloadsi_la_in);
	  else
	    sri->icode = (TARGET_64BIT ?
			  CODE_FOR_reloaddi_la_out :
			  CODE_FOR_reloadsi_la_out);
	}
    }

  /* A scratch address register is needed when a symbolic constant is
     copied to r0 compiling with -fPIC.  In other cases the target
     register might be used as temporary (see legitimize_pic_address).  */
  if (in_p && SYMBOLIC_CONST (x) && flag_pic == 2 && rclass != ADDR_REGS)
    sri->icode = (TARGET_64BIT ?
		  CODE_FOR_reloaddi_PIC_addr :
		  CODE_FOR_reloadsi_PIC_addr);

  /* Either scratch or no register needed.  */
  return NO_REGS;
}

/* Implement TARGET_SECONDARY_MEMORY_NEEDED.

   We need secondary memory to move data between GPRs and FPRs.

   - With DFP the ldgr lgdr instructions are available.  Due to the
     different alignment we cannot use them for SFmode.  For 31 bit a
     64 bit value in GPR would be a register pair so here we still
     need to go via memory.

   - With z13 we can do the SF/SImode moves with vlgvf.  Due to the
     overlapping of FPRs and VRs we still disallow TF/TD modes to be
     in full VRs so as before also on z13 we do these moves via
     memory.

     FIXME: Should we try splitting it into two vlgvg's/vlvg's instead?  */

static bool
s390_secondary_memory_needed (machine_mode mode,
			      reg_class_t class1, reg_class_t class2)
{
  return (((reg_classes_intersect_p (class1, VEC_REGS)
	    && reg_classes_intersect_p (class2, GENERAL_REGS))
	   || (reg_classes_intersect_p (class1, GENERAL_REGS)
	       && reg_classes_intersect_p (class2, VEC_REGS)))
	  && (TARGET_TPF || !TARGET_DFP || !TARGET_64BIT
	      || GET_MODE_SIZE (mode) != 8)
	  && (!TARGET_VX || (SCALAR_FLOAT_MODE_P (mode)
			     && GET_MODE_SIZE (mode) > 8)));
}

/* Implement TARGET_SECONDARY_MEMORY_NEEDED_MODE.

   get_secondary_mem widens its argument to BITS_PER_WORD which loses on 64bit
   because the movsi and movsf patterns don't handle r/f moves.  */

static machine_mode
s390_secondary_memory_needed_mode (machine_mode mode)
{
  if (GET_MODE_BITSIZE (mode) < 32)
    return mode_for_size (32, GET_MODE_CLASS (mode), 0).require ();
  return mode;
}

/* Generate code to load SRC, which is PLUS that is not a
   legitimate operand for the LA instruction, into TARGET.
   SCRATCH may be used as scratch register.  */

void
s390_expand_plus_operand (rtx target, rtx src,
			  rtx scratch)
{
  rtx sum1, sum2;
  struct s390_address ad;

  /* src must be a PLUS; get its two operands.  */
  gcc_assert (GET_CODE (src) == PLUS);
  gcc_assert (GET_MODE (src) == Pmode);

  /* Check if any of the two operands is already scheduled
     for replacement by reload.  This can happen e.g. when
     float registers occur in an address.  */
  sum1 = find_replacement (&XEXP (src, 0));
  sum2 = find_replacement (&XEXP (src, 1));
  src = gen_rtx_PLUS (Pmode, sum1, sum2);

  /* If the address is already strictly valid, there's nothing to do.  */
  if (!s390_decompose_address (src, &ad)
      || (ad.base && !REGNO_OK_FOR_BASE_P (REGNO (ad.base)))
      || (ad.indx && !REGNO_OK_FOR_INDEX_P (REGNO (ad.indx))))
    {
      /* Otherwise, one of the operands cannot be an address register;
	 we reload its value into the scratch register.  */
      if (true_regnum (sum1) < 1 || true_regnum (sum1) > 15)
	{
	  emit_move_insn (scratch, sum1);
	  sum1 = scratch;
	}
      if (true_regnum (sum2) < 1 || true_regnum (sum2) > 15)
	{
	  emit_move_insn (scratch, sum2);
	  sum2 = scratch;
	}

      /* According to the way these invalid addresses are generated
	 in reload.cc, it should never happen (at least on s390) that
	 *neither* of the PLUS components, after find_replacements
	 was applied, is an address register.  */
      if (sum1 == scratch && sum2 == scratch)
	{
	  debug_rtx (src);
	  gcc_unreachable ();
	}

      src = gen_rtx_PLUS (Pmode, sum1, sum2);
    }

  /* Emit the LOAD ADDRESS pattern.  Note that reload of PLUS
     is only ever performed on addresses, so we can mark the
     sum as legitimate for LA in any case.  */
  s390_load_address (target, src);
}


/* Return true if ADDR is a valid memory address.
   STRICT specifies whether strict register checking applies.  */

static bool
s390_legitimate_address_p (machine_mode mode, rtx addr, bool strict)
{
  struct s390_address ad;

  if (TARGET_Z10
      && larl_operand (addr, VOIDmode)
      && (mode == VOIDmode
	  || s390_check_symref_alignment (addr, GET_MODE_SIZE (mode))))
    return true;

  if (!s390_decompose_address (addr, &ad))
    return false;

  /* The vector memory instructions only support short displacements.
     Reject invalid displacements early to prevent plenty of lay
     instructions to be generated later which then cannot be merged
     properly.  */
  if (TARGET_VX
      && VECTOR_MODE_P (mode)
      && ad.disp != NULL_RTX
      && CONST_INT_P (ad.disp)
      && !SHORT_DISP_IN_RANGE (INTVAL (ad.disp)))
    return false;

  if (strict)
    {
      if (ad.base && !REGNO_OK_FOR_BASE_P (REGNO (ad.base)))
	return false;

      if (ad.indx && !REGNO_OK_FOR_INDEX_P (REGNO (ad.indx)))
	return false;
    }
  else
    {
      if (ad.base
	  && !(REGNO (ad.base) >= FIRST_PSEUDO_REGISTER
	       || REGNO_REG_CLASS (REGNO (ad.base)) == ADDR_REGS))
	return false;

      if (ad.indx
	  && !(REGNO (ad.indx) >= FIRST_PSEUDO_REGISTER
	       || REGNO_REG_CLASS (REGNO (ad.indx)) == ADDR_REGS))
	  return false;
    }
  return true;
}

/* Return true if OP is a valid operand for the LA instruction.
   In 31-bit, we need to prove that the result is used as an
   address, as LA performs only a 31-bit addition.  */

bool
legitimate_la_operand_p (rtx op)
{
  struct s390_address addr;
  if (!s390_decompose_address (op, &addr))
    return false;

  return (TARGET_64BIT || addr.pointer);
}

/* Return true if it is valid *and* preferable to use LA to
   compute the sum of OP1 and OP2.  */

bool
preferred_la_operand_p (rtx op1, rtx op2)
{
  struct s390_address addr;

  if (op2 != const0_rtx)
    op1 = gen_rtx_PLUS (Pmode, op1, op2);

  if (!s390_decompose_address (op1, &addr))
    return false;
  if (addr.base && !REGNO_OK_FOR_BASE_P (REGNO (addr.base)))
    return false;
  if (addr.indx && !REGNO_OK_FOR_INDEX_P (REGNO (addr.indx)))
    return false;

  /* Avoid LA instructions with index (and base) register on z196 or
     later; it is preferable to use regular add instructions when
     possible.  Starting with zEC12 the la with index register is
     "uncracked" again but still slower than a regular add.  */
  if (addr.indx && s390_tune >= PROCESSOR_2817_Z196)
    return false;

  if (!TARGET_64BIT && !addr.pointer)
    return false;

  if (addr.pointer)
    return true;

  if ((addr.base && REG_P (addr.base) && REG_POINTER (addr.base))
      || (addr.indx && REG_P (addr.indx) && REG_POINTER (addr.indx)))
    return true;

  return false;
}

/* Emit a forced load-address operation to load SRC into DST.
   This will use the LOAD ADDRESS instruction even in situations
   where legitimate_la_operand_p (SRC) returns false.  */

void
s390_load_address (rtx dst, rtx src)
{
  if (TARGET_64BIT)
    emit_move_insn (dst, src);
  else
    emit_insn (gen_force_la_31 (dst, src));
}

/* Return true if it ok to use SYMBOL_REF in a relative address.  */

bool
s390_rel_address_ok_p (rtx symbol_ref)
{
  tree decl;

  if (symbol_ref == s390_got_symbol () || CONSTANT_POOL_ADDRESS_P (symbol_ref))
    return true;

  decl = SYMBOL_REF_DECL (symbol_ref);

  if (!flag_pic || SYMBOL_REF_LOCAL_P (symbol_ref))
    return (s390_pic_data_is_text_relative
	    || (decl
		&& TREE_CODE (decl) == FUNCTION_DECL));

  return false;
}

/* Return a legitimate reference for ORIG (an address) using the
   register REG.  If REG is 0, a new pseudo is generated.

   There are two types of references that must be handled:

   1. Global data references must load the address from the GOT, via
      the PIC reg.  An insn is emitted to do this load, and the reg is
      returned.

   2. Static data references, constant pool addresses, and code labels
      compute the address as an offset from the GOT, whose base is in
      the PIC reg.  Static data objects have SYMBOL_FLAG_LOCAL set to
      differentiate them from global data objects.  The returned
      address is the PIC reg + an unspec constant.

   TARGET_LEGITIMIZE_ADDRESS_P rejects symbolic references unless the PIC
   reg also appears in the address.  */

rtx
legitimize_pic_address (rtx orig, rtx reg)
{
  rtx addr = orig;
  rtx addend = const0_rtx;
  rtx new_rtx = orig;

  gcc_assert (!TLS_SYMBOLIC_CONST (addr));

  if (GET_CODE (addr) == CONST)
    addr = XEXP (addr, 0);

  if (GET_CODE (addr) == PLUS)
    {
      addend = XEXP (addr, 1);
      addr = XEXP (addr, 0);
    }

  if ((GET_CODE (addr) == LABEL_REF
       || (SYMBOL_REF_P (addr) && s390_rel_address_ok_p (addr))
       || (GET_CODE (addr) == UNSPEC &&
	   (XINT (addr, 1) == UNSPEC_GOTENT
	    || XINT (addr, 1) == UNSPEC_PLT31)))
      && GET_CODE (addend) == CONST_INT)
    {
      /* This can be locally addressed.  */

      /* larl_operand requires UNSPECs to be wrapped in a const rtx.  */
      rtx const_addr = (GET_CODE (addr) == UNSPEC ?
			gen_rtx_CONST (Pmode, addr) : addr);

      if (larl_operand (const_addr, VOIDmode)
	  && INTVAL (addend) < HOST_WIDE_INT_1 << 31
	  && INTVAL (addend) >= -(HOST_WIDE_INT_1 << 31))
	{
	  if (INTVAL (addend) & 1)
	    {
	      /* LARL can't handle odd offsets, so emit a pair of LARL
		 and LA.  */
	      rtx temp = reg? reg : gen_reg_rtx (Pmode);

	      if (!DISP_IN_RANGE (INTVAL (addend)))
		{
		  HOST_WIDE_INT even = INTVAL (addend) - 1;
		  addr = gen_rtx_PLUS (Pmode, addr, GEN_INT (even));
		  addr = gen_rtx_CONST (Pmode, addr);
		  addend = const1_rtx;
		}

	      emit_move_insn (temp, addr);
	      new_rtx = gen_rtx_PLUS (Pmode, temp, addend);

	      if (reg != 0)
		{
		  s390_load_address (reg, new_rtx);
		  new_rtx = reg;
		}
	    }
	  else
	    {
	      /* If the offset is even, we can just use LARL.  This
		 will happen automatically.  */
	    }
	}
      else
	{
	  /* No larl - Access local symbols relative to the GOT.  */

	  rtx temp = reg? reg : gen_reg_rtx (Pmode);

	  if (reload_in_progress || reload_completed)
	    df_set_regs_ever_live (PIC_OFFSET_TABLE_REGNUM, true);

	  addr = gen_rtx_UNSPEC (Pmode, gen_rtvec (1, addr), UNSPEC_GOTOFF);
	  if (addend != const0_rtx)
	    addr = gen_rtx_PLUS (Pmode, addr, addend);
	  addr = gen_rtx_CONST (Pmode, addr);
	  addr = force_const_mem (Pmode, addr);
	  emit_move_insn (temp, addr);

	  new_rtx = gen_rtx_PLUS (Pmode, pic_offset_table_rtx, temp);
	  if (reg != 0)
	    {
	      s390_load_address (reg, new_rtx);
	      new_rtx = reg;
	    }
	}
    }
  else if (GET_CODE (addr) == SYMBOL_REF && addend == const0_rtx)
    {
      /* A non-local symbol reference without addend.

	 The symbol ref is wrapped into an UNSPEC to make sure the
	 proper operand modifier (@GOT or @GOTENT) will be emitted.
	 This will tell the linker to put the symbol into the GOT.

	 Additionally the code dereferencing the GOT slot is emitted here.

	 An addend to the symref needs to be added afterwards.
	 legitimize_pic_address calls itself recursively to handle
	 that case.  So no need to do it here.  */

      if (reg == 0)
	reg = gen_reg_rtx (Pmode);

      if (TARGET_Z10)
	{
	  /* Use load relative if possible.
	     lgrl <target>, sym@GOTENT  */
	  new_rtx = gen_rtx_UNSPEC (Pmode, gen_rtvec (1, addr), UNSPEC_GOTENT);
	  new_rtx = gen_rtx_CONST (Pmode, new_rtx);
	  new_rtx = gen_const_mem (GET_MODE (reg), new_rtx);

	  emit_move_insn (reg, new_rtx);
	  new_rtx = reg;
	}
      else if (flag_pic == 1)
	{
	  /* Assume GOT offset is a valid displacement operand (< 4k
	     or < 512k with z990).  This is handled the same way in
	     both 31- and 64-bit code (@GOT).
	     lg <target>, sym@GOT(r12)  */

	  if (reload_in_progress || reload_completed)
	    df_set_regs_ever_live (PIC_OFFSET_TABLE_REGNUM, true);

	  new_rtx = gen_rtx_UNSPEC (Pmode, gen_rtvec (1, addr), UNSPEC_GOT);
	  new_rtx = gen_rtx_CONST (Pmode, new_rtx);
	  new_rtx = gen_rtx_PLUS (Pmode, pic_offset_table_rtx, new_rtx);
	  new_rtx = gen_const_mem (Pmode, new_rtx);
	  emit_move_insn (reg, new_rtx);
	  new_rtx = reg;
	}
      else
	{
	  /* If the GOT offset might be >= 4k, we determine the position
	     of the GOT entry via a PC-relative LARL (@GOTENT).
	     larl temp, sym@GOTENT
	     lg   <target>, 0(temp) */

	  rtx temp = reg ? reg : gen_reg_rtx (Pmode);

	  gcc_assert (REGNO (temp) >= FIRST_PSEUDO_REGISTER
		      || REGNO_REG_CLASS (REGNO (temp)) == ADDR_REGS);

	  new_rtx = gen_rtx_UNSPEC (Pmode, gen_rtvec (1, addr), UNSPEC_GOTENT);
	  new_rtx = gen_rtx_CONST (Pmode, new_rtx);
	  emit_move_insn (temp, new_rtx);
	  new_rtx = gen_const_mem (Pmode, temp);
	  emit_move_insn (reg, new_rtx);

	  new_rtx = reg;
	}
    }
  else if (GET_CODE (addr) == UNSPEC && GET_CODE (addend) == CONST_INT)
    {
      gcc_assert (XVECLEN (addr, 0) == 1);
      switch (XINT (addr, 1))
	{
	  /* These address symbols (or PLT slots) relative to the GOT
	     (not GOT slots!).  In general this will exceed the
	     displacement range so these value belong into the literal
	     pool.  */
	case UNSPEC_GOTOFF:
	case UNSPEC_PLTOFF:
	  new_rtx = force_const_mem (Pmode, orig);
	  break;

	  /* For -fPIC the GOT size might exceed the displacement
	     range so make sure the value is in the literal pool.  */
	case UNSPEC_GOT:
	  if (flag_pic == 2)
	    new_rtx = force_const_mem (Pmode, orig);
	  break;

	  /* For @GOTENT larl is used.  This is handled like local
	     symbol refs.  */
	case UNSPEC_GOTENT:
	  gcc_unreachable ();
	  break;

	  /* For @PLT larl is used.  This is handled like local
	     symbol refs.  */
	case UNSPEC_PLT31:
	  gcc_unreachable ();
	  break;

	  /* Everything else cannot happen.  */
	default:
	  gcc_unreachable ();
	}
    }
  else if (addend != const0_rtx)
    {
      /* Otherwise, compute the sum.  */

      rtx base = legitimize_pic_address (addr, reg);
      new_rtx  = legitimize_pic_address (addend,
					 base == reg ? NULL_RTX : reg);
      if (GET_CODE (new_rtx) == CONST_INT)
	new_rtx = plus_constant (Pmode, base, INTVAL (new_rtx));
      else
	{
	  if (GET_CODE (new_rtx) == PLUS && CONSTANT_P (XEXP (new_rtx, 1)))
	    {
	      base = gen_rtx_PLUS (Pmode, base, XEXP (new_rtx, 0));
	      new_rtx = XEXP (new_rtx, 1);
	    }
	  new_rtx = gen_rtx_PLUS (Pmode, base, new_rtx);
	}

      if (GET_CODE (new_rtx) == CONST)
	new_rtx = XEXP (new_rtx, 0);
      new_rtx = force_operand (new_rtx, 0);
    }

  return new_rtx;
}

/* Load the thread pointer into a register.  */

rtx
s390_get_thread_pointer (void)
{
  rtx tp = gen_reg_rtx (Pmode);

  emit_insn (gen_get_thread_pointer (Pmode, tp));

  mark_reg_pointer (tp, BITS_PER_WORD);

  return tp;
}

/* Emit a tls call insn. The call target is the SYMBOL_REF stored
   in s390_tls_symbol which always refers to __tls_get_offset.
   The returned offset is written to RESULT_REG and an USE rtx is
   generated for TLS_CALL.  */

static GTY(()) rtx s390_tls_symbol;

static void
s390_emit_tls_call_insn (rtx result_reg, rtx tls_call)
{
  rtx insn;

  if (!flag_pic)
    emit_insn (s390_load_got ());

  if (!s390_tls_symbol)
    {
      s390_tls_symbol = gen_rtx_SYMBOL_REF (Pmode, "__tls_get_offset");
      SYMBOL_REF_FLAGS (s390_tls_symbol) |= SYMBOL_FLAG_FUNCTION;
    }

  insn = s390_emit_call (s390_tls_symbol, tls_call, result_reg,
			 gen_rtx_REG (Pmode, RETURN_REGNUM));

  use_reg (&CALL_INSN_FUNCTION_USAGE (insn), result_reg);
  RTL_CONST_CALL_P (insn) = 1;
}

/* ADDR contains a thread-local SYMBOL_REF.  Generate code to compute
   this (thread-local) address.  REG may be used as temporary.  */

static rtx
legitimize_tls_address (rtx addr, rtx reg)
{
  rtx new_rtx, tls_call, temp, base, r2;
  rtx_insn *insn;

  if (GET_CODE (addr) == SYMBOL_REF)
    switch (tls_symbolic_operand (addr))
      {
      case TLS_MODEL_GLOBAL_DYNAMIC:
	start_sequence ();
	r2 = gen_rtx_REG (Pmode, 2);
	tls_call = gen_rtx_UNSPEC (Pmode, gen_rtvec (1, addr), UNSPEC_TLSGD);
	new_rtx = gen_rtx_CONST (Pmode, tls_call);
	new_rtx = force_const_mem (Pmode, new_rtx);
	emit_move_insn (r2, new_rtx);
	s390_emit_tls_call_insn (r2, tls_call);
	insn = get_insns ();
	end_sequence ();

	new_rtx = gen_rtx_UNSPEC (Pmode, gen_rtvec (1, addr), UNSPEC_NTPOFF);
	temp = gen_reg_rtx (Pmode);
	emit_libcall_block (insn, temp, r2, new_rtx);

	new_rtx = gen_rtx_PLUS (Pmode, s390_get_thread_pointer (), temp);
	if (reg != 0)
	  {
	    s390_load_address (reg, new_rtx);
	    new_rtx = reg;
	  }
	break;

      case TLS_MODEL_LOCAL_DYNAMIC:
	start_sequence ();
	r2 = gen_rtx_REG (Pmode, 2);
	tls_call = gen_rtx_UNSPEC (Pmode, gen_rtvec (1, const0_rtx), UNSPEC_TLSLDM);
	new_rtx = gen_rtx_CONST (Pmode, tls_call);
	new_rtx = force_const_mem (Pmode, new_rtx);
	emit_move_insn (r2, new_rtx);
	s390_emit_tls_call_insn (r2, tls_call);
	insn = get_insns ();
	end_sequence ();

	new_rtx = gen_rtx_UNSPEC (Pmode, gen_rtvec (1, const0_rtx), UNSPEC_TLSLDM_NTPOFF);
	temp = gen_reg_rtx (Pmode);
	emit_libcall_block (insn, temp, r2, new_rtx);

	new_rtx = gen_rtx_PLUS (Pmode, s390_get_thread_pointer (), temp);
	base = gen_reg_rtx (Pmode);
	s390_load_address (base, new_rtx);

	new_rtx = gen_rtx_UNSPEC (Pmode, gen_rtvec (1, addr), UNSPEC_DTPOFF);
	new_rtx = gen_rtx_CONST (Pmode, new_rtx);
	new_rtx = force_const_mem (Pmode, new_rtx);
	temp = gen_reg_rtx (Pmode);
	emit_move_insn (temp, new_rtx);

	new_rtx = gen_rtx_PLUS (Pmode, base, temp);
	if (reg != 0)
	  {
	    s390_load_address (reg, new_rtx);
	    new_rtx = reg;
	  }
	break;

      case TLS_MODEL_INITIAL_EXEC:
	if (flag_pic == 1)
	  {
	    /* Assume GOT offset < 4k.  This is handled the same way
	       in both 31- and 64-bit code.  */

	    if (reload_in_progress || reload_completed)
	      df_set_regs_ever_live (PIC_OFFSET_TABLE_REGNUM, true);

	    new_rtx = gen_rtx_UNSPEC (Pmode, gen_rtvec (1, addr), UNSPEC_GOTNTPOFF);
	    new_rtx = gen_rtx_CONST (Pmode, new_rtx);
	    new_rtx = gen_rtx_PLUS (Pmode, pic_offset_table_rtx, new_rtx);
	    new_rtx = gen_const_mem (Pmode, new_rtx);
	    temp = gen_reg_rtx (Pmode);
	    emit_move_insn (temp, new_rtx);
	  }
	else
	  {
	    /* If the GOT offset might be >= 4k, we determine the position
	       of the GOT entry via a PC-relative LARL.  */

	    new_rtx = gen_rtx_UNSPEC (Pmode, gen_rtvec (1, addr), UNSPEC_INDNTPOFF);
	    new_rtx = gen_rtx_CONST (Pmode, new_rtx);
	    temp = gen_reg_rtx (Pmode);
	    emit_move_insn (temp, new_rtx);

	    new_rtx = gen_const_mem (Pmode, temp);
	    temp = gen_reg_rtx (Pmode);
	    emit_move_insn (temp, new_rtx);
	  }

	new_rtx = gen_rtx_PLUS (Pmode, s390_get_thread_pointer (), temp);
	if (reg != 0)
	  {
	    s390_load_address (reg, new_rtx);
	    new_rtx = reg;
	  }
	break;

      case TLS_MODEL_LOCAL_EXEC:
	new_rtx = gen_rtx_UNSPEC (Pmode, gen_rtvec (1, addr), UNSPEC_NTPOFF);
	new_rtx = gen_rtx_CONST (Pmode, new_rtx);
	new_rtx = force_const_mem (Pmode, new_rtx);
	temp = gen_reg_rtx (Pmode);
	emit_move_insn (temp, new_rtx);

	new_rtx = gen_rtx_PLUS (Pmode, s390_get_thread_pointer (), temp);
	if (reg != 0)
	  {
	    s390_load_address (reg, new_rtx);
	    new_rtx = reg;
	  }
	break;

      default:
	gcc_unreachable ();
      }

  else if (GET_CODE (addr) == CONST && GET_CODE (XEXP (addr, 0)) == UNSPEC)
    {
      switch (XINT (XEXP (addr, 0), 1))
	{
	case UNSPEC_NTPOFF:
	case UNSPEC_INDNTPOFF:
	  new_rtx = addr;
	  break;

	default:
	  gcc_unreachable ();
	}
    }

  else if (GET_CODE (addr) == CONST && GET_CODE (XEXP (addr, 0)) == PLUS
	   && GET_CODE (XEXP (XEXP (addr, 0), 1)) == CONST_INT)
    {
      new_rtx = XEXP (XEXP (addr, 0), 0);
      if (GET_CODE (new_rtx) != SYMBOL_REF)
	new_rtx = gen_rtx_CONST (Pmode, new_rtx);

      new_rtx = legitimize_tls_address (new_rtx, reg);
      new_rtx = plus_constant (Pmode, new_rtx,
			       INTVAL (XEXP (XEXP (addr, 0), 1)));
      new_rtx = force_operand (new_rtx, 0);
    }

  /* (const (neg (unspec (symbol_ref)))) -> (neg (const (unspec (symbol_ref)))) */
  else if (GET_CODE (addr) == CONST && GET_CODE (XEXP (addr, 0)) == NEG)
    {
      new_rtx = XEXP (XEXP (addr, 0), 0);
      if (GET_CODE (new_rtx) != SYMBOL_REF)
	new_rtx = gen_rtx_CONST (Pmode, new_rtx);

      new_rtx = legitimize_tls_address (new_rtx, reg);
      new_rtx = gen_rtx_NEG (Pmode, new_rtx);
      new_rtx = force_operand (new_rtx, 0);
    }

  else
    gcc_unreachable ();  /* for now ... */

  return new_rtx;
}

/* Emit insns making the address in operands[1] valid for a standard
   move to operands[0].  operands[1] is replaced by an address which
   should be used instead of the former RTX to emit the move
   pattern.  */

void
emit_symbolic_move (rtx *operands)
{
  rtx temp = !can_create_pseudo_p () ? operands[0] : gen_reg_rtx (Pmode);

  if (GET_CODE (operands[0]) == MEM)
    operands[1] = force_reg (Pmode, operands[1]);
  else if (TLS_SYMBOLIC_CONST (operands[1]))
    operands[1] = legitimize_tls_address (operands[1], temp);
  else if (flag_pic)
    operands[1] = legitimize_pic_address (operands[1], temp);
}

/* Try machine-dependent ways of modifying an illegitimate address X
   to be legitimate.  If we find one, return the new, valid address.

   OLDX is the address as it was before break_out_memory_refs was called.
   In some cases it is useful to look at this to decide what needs to be done.

   MODE is the mode of the operand pointed to by X.

   When -fpic is used, special handling is needed for symbolic references.
   See comments by legitimize_pic_address for details.  */

static rtx
s390_legitimize_address (rtx x, rtx oldx ATTRIBUTE_UNUSED,
			 machine_mode mode ATTRIBUTE_UNUSED)
{
  rtx constant_term = const0_rtx;

  if (TLS_SYMBOLIC_CONST (x))
    {
      x = legitimize_tls_address (x, 0);

      if (s390_legitimate_address_p (mode, x, FALSE))
	return x;
    }
  else if (GET_CODE (x) == PLUS
	   && (TLS_SYMBOLIC_CONST (XEXP (x, 0))
	       || TLS_SYMBOLIC_CONST (XEXP (x, 1))))
    {
      return x;
    }
  else if (flag_pic)
    {
      if (SYMBOLIC_CONST (x)
	  || (GET_CODE (x) == PLUS
	      && (SYMBOLIC_CONST (XEXP (x, 0))
		  || SYMBOLIC_CONST (XEXP (x, 1)))))
	  x = legitimize_pic_address (x, 0);

      if (s390_legitimate_address_p (mode, x, FALSE))
	return x;
    }

  x = eliminate_constant_term (x, &constant_term);

  /* Optimize loading of large displacements by splitting them
     into the multiple of 4K and the rest; this allows the
     former to be CSE'd if possible.

     Don't do this if the displacement is added to a register
     pointing into the stack frame, as the offsets will
     change later anyway.  */

  if (GET_CODE (constant_term) == CONST_INT
      && !TARGET_LONG_DISPLACEMENT
      && !DISP_IN_RANGE (INTVAL (constant_term))
      && !(REG_P (x) && REGNO_PTR_FRAME_P (REGNO (x))))
    {
      HOST_WIDE_INT lower = INTVAL (constant_term) & 0xfff;
      HOST_WIDE_INT upper = INTVAL (constant_term) ^ lower;

      rtx temp = gen_reg_rtx (Pmode);
      rtx val  = force_operand (GEN_INT (upper), temp);
      if (val != temp)
	emit_move_insn (temp, val);

      x = gen_rtx_PLUS (Pmode, x, temp);
      constant_term = GEN_INT (lower);
    }

  if (GET_CODE (x) == PLUS)
    {
      if (GET_CODE (XEXP (x, 0)) == REG)
	{
	  rtx temp = gen_reg_rtx (Pmode);
	  rtx val  = force_operand (XEXP (x, 1), temp);
	  if (val != temp)
	    emit_move_insn (temp, val);

	  x = gen_rtx_PLUS (Pmode, XEXP (x, 0), temp);
	}

      else if (GET_CODE (XEXP (x, 1)) == REG)
	{
	  rtx temp = gen_reg_rtx (Pmode);
	  rtx val  = force_operand (XEXP (x, 0), temp);
	  if (val != temp)
	    emit_move_insn (temp, val);

	  x = gen_rtx_PLUS (Pmode, temp, XEXP (x, 1));
	}
    }

  if (constant_term != const0_rtx)
    x = gen_rtx_PLUS (Pmode, x, constant_term);

  return x;
}

/* Try a machine-dependent way of reloading an illegitimate address AD
   operand.  If we find one, push the reload and return the new address.

   MODE is the mode of the enclosing MEM.  OPNUM is the operand number
   and TYPE is the reload type of the current reload.  */

rtx
legitimize_reload_address (rtx ad, machine_mode mode ATTRIBUTE_UNUSED,
			   int opnum, int type)
{
  if (!optimize || TARGET_LONG_DISPLACEMENT)
    return NULL_RTX;

  if (GET_CODE (ad) == PLUS)
    {
      rtx tem = simplify_binary_operation (PLUS, Pmode,
					   XEXP (ad, 0), XEXP (ad, 1));
      if (tem)
	ad = tem;
    }

  if (GET_CODE (ad) == PLUS
      && GET_CODE (XEXP (ad, 0)) == REG
      && GET_CODE (XEXP (ad, 1)) == CONST_INT
      && !DISP_IN_RANGE (INTVAL (XEXP (ad, 1))))
    {
      HOST_WIDE_INT lower = INTVAL (XEXP (ad, 1)) & 0xfff;
      HOST_WIDE_INT upper = INTVAL (XEXP (ad, 1)) ^ lower;
      rtx cst, tem, new_rtx;

      cst = GEN_INT (upper);
      if (!legitimate_reload_constant_p (cst))
	cst = force_const_mem (Pmode, cst);

      tem = gen_rtx_PLUS (Pmode, XEXP (ad, 0), cst);
      new_rtx = gen_rtx_PLUS (Pmode, tem, GEN_INT (lower));

      push_reload (XEXP (tem, 1), 0, &XEXP (tem, 1), 0,
		   BASE_REG_CLASS, Pmode, VOIDmode, 0, 0,
		   opnum, (enum reload_type) type);
      return new_rtx;
    }

  return NULL_RTX;
}

/* Emit code to move LEN bytes from DST to SRC.  */

bool
s390_expand_cpymem (rtx dst, rtx src, rtx len)
{
  /* When tuning for z10 or higher we rely on the Glibc functions to
     do the right thing. Only for constant lengths below 64k we will
     generate inline code.  */
  if (s390_tune >= PROCESSOR_2097_Z10
      && (GET_CODE (len) != CONST_INT || INTVAL (len) > (1<<16)))
    return false;

  /* Expand memcpy for constant length operands without a loop if it
     is shorter that way.

     With a constant length argument a
     memcpy loop (without pfd) is 36 bytes -> 6 * mvc  */
  if (GET_CODE (len) == CONST_INT
      && INTVAL (len) >= 0
      && INTVAL (len) <= 256 * 6
      && (!TARGET_MVCLE || INTVAL (len) <= 256))
    {
      HOST_WIDE_INT o, l;

      for (l = INTVAL (len), o = 0; l > 0; l -= 256, o += 256)
	{
	  rtx newdst = adjust_address (dst, BLKmode, o);
	  rtx newsrc = adjust_address (src, BLKmode, o);
	  emit_insn (gen_cpymem_short (newdst, newsrc,
				       GEN_INT (l > 256 ? 255 : l - 1)));
	}
    }

  else if (TARGET_MVCLE)
    {
      emit_insn (gen_cpymem_long (dst, src, convert_to_mode (Pmode, len, 1)));
    }

  else
    {
      rtx dst_addr, src_addr, count, blocks, temp;
      rtx_code_label *loop_start_label = gen_label_rtx ();
      rtx_code_label *loop_end_label = gen_label_rtx ();
      rtx_code_label *end_label = gen_label_rtx ();
      machine_mode mode;

      mode = GET_MODE (len);
      if (mode == VOIDmode)
	mode = Pmode;

      dst_addr = gen_reg_rtx (Pmode);
      src_addr = gen_reg_rtx (Pmode);
      count = gen_reg_rtx (mode);
      blocks = gen_reg_rtx (mode);

      convert_move (count, len, 1);
      emit_cmp_and_jump_insns (count, const0_rtx,
			       EQ, NULL_RTX, mode, 1, end_label);

      emit_move_insn (dst_addr, force_operand (XEXP (dst, 0), NULL_RTX));
      emit_move_insn (src_addr, force_operand (XEXP (src, 0), NULL_RTX));
      dst = change_address (dst, VOIDmode, dst_addr);
      src = change_address (src, VOIDmode, src_addr);

      temp = expand_binop (mode, add_optab, count, constm1_rtx, count, 1,
			   OPTAB_DIRECT);
      if (temp != count)
	emit_move_insn (count, temp);

      temp = expand_binop (mode, lshr_optab, count, GEN_INT (8), blocks, 1,
			   OPTAB_DIRECT);
      if (temp != blocks)
	emit_move_insn (blocks, temp);

      emit_cmp_and_jump_insns (blocks, const0_rtx,
			       EQ, NULL_RTX, mode, 1, loop_end_label);

      emit_label (loop_start_label);

      if (TARGET_Z10
	  && (GET_CODE (len) != CONST_INT || INTVAL (len) > 768))
	{
	  rtx prefetch;

	  /* Issue a read prefetch for the +3 cache line.  */
	  prefetch = gen_prefetch (gen_rtx_PLUS (Pmode, src_addr, GEN_INT (768)),
				   const0_rtx, const0_rtx);
	  PREFETCH_SCHEDULE_BARRIER_P (prefetch) = true;
	  emit_insn (prefetch);

	  /* Issue a write prefetch for the +3 cache line.  */
	  prefetch = gen_prefetch (gen_rtx_PLUS (Pmode, dst_addr, GEN_INT (768)),
				   const1_rtx, const0_rtx);
	  PREFETCH_SCHEDULE_BARRIER_P (prefetch) = true;
	  emit_insn (prefetch);
	}

      emit_insn (gen_cpymem_short (dst, src, GEN_INT (255)));
      s390_load_address (dst_addr,
			 gen_rtx_PLUS (Pmode, dst_addr, GEN_INT (256)));
      s390_load_address (src_addr,
			 gen_rtx_PLUS (Pmode, src_addr, GEN_INT (256)));

      temp = expand_binop (mode, add_optab, blocks, constm1_rtx, blocks, 1,
			   OPTAB_DIRECT);
      if (temp != blocks)
	emit_move_insn (blocks, temp);

      emit_cmp_and_jump_insns (blocks, const0_rtx,
			       EQ, NULL_RTX, mode, 1, loop_end_label);

      emit_jump (loop_start_label);
      emit_label (loop_end_label);

      emit_insn (gen_cpymem_short (dst, src,
				   convert_to_mode (Pmode, count, 1)));
      emit_label (end_label);
    }
  return true;
}

/* Emit code to set LEN bytes at DST to VAL.
   Make use of clrmem if VAL is zero.  */

void
s390_expand_setmem (rtx dst, rtx len, rtx val)
{
  if (GET_CODE (len) == CONST_INT && INTVAL (len) <= 0)
    return;

  gcc_assert (GET_CODE (val) == CONST_INT || GET_MODE (val) == QImode);

  /* Expand setmem/clrmem for a constant length operand without a
     loop if it will be shorter that way.
     clrmem loop (with PFD)    is 30 bytes -> 5 * xc
     clrmem loop (without PFD) is 24 bytes -> 4 * xc
     setmem loop (with PFD)    is 38 bytes -> ~4 * (mvi/stc + mvc)
     setmem loop (without PFD) is 32 bytes -> ~4 * (mvi/stc + mvc) */
  if (GET_CODE (len) == CONST_INT
      && ((val == const0_rtx
	   && (INTVAL (len) <= 256 * 4
	       || (INTVAL (len) <= 256 * 5 && TARGET_SETMEM_PFD(val,len))))
	  || (val != const0_rtx && INTVAL (len) <= 257 * 4))
      && (!TARGET_MVCLE || INTVAL (len) <= 256))
    {
      HOST_WIDE_INT o, l;

      if (val == const0_rtx)
	/* clrmem: emit 256 byte blockwise XCs.  */
	for (l = INTVAL (len), o = 0; l > 0; l -= 256, o += 256)
	  {
	    rtx newdst = adjust_address (dst, BLKmode, o);
	    emit_insn (gen_clrmem_short (newdst,
					 GEN_INT (l > 256 ? 255 : l - 1)));
	  }
      else
	/* setmem: emit 1(mvi) + 256(mvc) byte blockwise memsets by
	   setting first byte to val and using a 256 byte mvc with one
	   byte overlap to propagate the byte.  */
	for (l = INTVAL (len), o = 0; l > 0; l -= 257, o += 257)
	  {
	    rtx newdst = adjust_address (dst, BLKmode, o);
	    emit_move_insn (adjust_address (dst, QImode, o), val);
	    if (l > 1)
	      {
		rtx newdstp1 = adjust_address (dst, BLKmode, o + 1);
		emit_insn (gen_cpymem_short (newdstp1, newdst,
					     GEN_INT (l > 257 ? 255 : l - 2)));
	      }
	  }
    }

  else if (TARGET_MVCLE)
    {
      val = force_not_mem (convert_modes (Pmode, QImode, val, 1));
      if (TARGET_64BIT)
	emit_insn (gen_setmem_long_di (dst, convert_to_mode (Pmode, len, 1),
				       val));
      else
	emit_insn (gen_setmem_long_si (dst, convert_to_mode (Pmode, len, 1),
				       val));
    }

  else
    {
      rtx dst_addr, count, blocks, temp, dstp1 = NULL_RTX;
      rtx_code_label *loop_start_label = gen_label_rtx ();
      rtx_code_label *onebyte_end_label = gen_label_rtx ();
      rtx_code_label *zerobyte_end_label = gen_label_rtx ();
      rtx_code_label *restbyte_end_label = gen_label_rtx ();
      machine_mode mode;

      mode = GET_MODE (len);
      if (mode == VOIDmode)
	mode = Pmode;

      dst_addr = gen_reg_rtx (Pmode);
      count = gen_reg_rtx (mode);
      blocks = gen_reg_rtx (mode);

      convert_move (count, len, 1);
      emit_cmp_and_jump_insns (count, const0_rtx,
			       EQ, NULL_RTX, mode, 1, zerobyte_end_label,
			       profile_probability::very_unlikely ());

      /* We need to make a copy of the target address since memset is
	 supposed to return it unmodified.  We have to make it here
	 already since the new reg is used at onebyte_end_label.  */
      emit_move_insn (dst_addr, force_operand (XEXP (dst, 0), NULL_RTX));
      dst = change_address (dst, VOIDmode, dst_addr);

      if (val != const0_rtx)
	{
	  /* When using the overlapping mvc the original target
	     address is only accessed as single byte entity (even by
	     the mvc reading this value).  */
	  set_mem_size (dst, 1);
	  dstp1 = adjust_address (dst, VOIDmode, 1);
	  emit_cmp_and_jump_insns (count,
				   const1_rtx, EQ, NULL_RTX, mode, 1,
				   onebyte_end_label,
				   profile_probability::very_unlikely ());
	}

      /* There is one unconditional (mvi+mvc)/xc after the loop
	 dealing with the rest of the bytes, subtracting two (mvi+mvc)
	 or one (xc) here leaves this number of bytes to be handled by
	 it.  */
      temp = expand_binop (mode, add_optab, count,
			   val == const0_rtx ? constm1_rtx : GEN_INT (-2),
			   count, 1, OPTAB_DIRECT);
      if (temp != count)
	emit_move_insn (count, temp);

      temp = expand_binop (mode, lshr_optab, count, GEN_INT (8), blocks, 1,
			   OPTAB_DIRECT);
      if (temp != blocks)
	emit_move_insn (blocks, temp);

      emit_cmp_and_jump_insns (blocks, const0_rtx,
			       EQ, NULL_RTX, mode, 1, restbyte_end_label);

      emit_jump (loop_start_label);

      if (val != const0_rtx)
	{
	  /* The 1 byte != 0 special case.  Not handled efficiently
	     since we require two jumps for that.  However, this
	     should be very rare.  */
	  emit_label (onebyte_end_label);
	  emit_move_insn (adjust_address (dst, QImode, 0), val);
	  emit_jump (zerobyte_end_label);
	}

      emit_label (loop_start_label);

      if (TARGET_SETMEM_PFD (val, len))
	{
	  /* Issue a write prefetch.  */
	  rtx distance = GEN_INT (TARGET_SETMEM_PREFETCH_DISTANCE);
	  rtx prefetch = gen_prefetch (gen_rtx_PLUS (Pmode, dst_addr, distance),
				       const1_rtx, const0_rtx);
	  emit_insn (prefetch);
	  PREFETCH_SCHEDULE_BARRIER_P (prefetch) = true;
	}

      if (val == const0_rtx)
	emit_insn (gen_clrmem_short (dst, GEN_INT (255)));
      else
	{
	  /* Set the first byte in the block to the value and use an
	     overlapping mvc for the block.  */
	  emit_move_insn (adjust_address (dst, QImode, 0), val);
	  emit_insn (gen_cpymem_short (dstp1, dst, GEN_INT (254)));
	}
      s390_load_address (dst_addr,
			 gen_rtx_PLUS (Pmode, dst_addr, GEN_INT (256)));

      temp = expand_binop (mode, add_optab, blocks, constm1_rtx, blocks, 1,
			   OPTAB_DIRECT);
      if (temp != blocks)
	emit_move_insn (blocks, temp);

      emit_cmp_and_jump_insns (blocks, const0_rtx,
			       NE, NULL_RTX, mode, 1, loop_start_label);

      emit_label (restbyte_end_label);

      if (val == const0_rtx)
	emit_insn (gen_clrmem_short (dst, convert_to_mode (Pmode, count, 1)));
      else
	{
	  /* Set the first byte in the block to the value and use an
	     overlapping mvc for the block.  */
	  emit_move_insn (adjust_address (dst, QImode, 0), val);
	  /* execute only uses the lowest 8 bits of count that's
	     exactly what we need here.  */
	  emit_insn (gen_cpymem_short (dstp1, dst,
				       convert_to_mode (Pmode, count, 1)));
	}

      emit_label (zerobyte_end_label);
    }
}

/* Emit code to compare LEN bytes at OP0 with those at OP1,
   and return the result in TARGET.  */

bool
s390_expand_cmpmem (rtx target, rtx op0, rtx op1, rtx len)
{
  rtx ccreg = gen_rtx_REG (CCUmode, CC_REGNUM);
  rtx tmp;

  /* When tuning for z10 or higher we rely on the Glibc functions to
     do the right thing. Only for constant lengths below 64k we will
     generate inline code.  */
  if (s390_tune >= PROCESSOR_2097_Z10
      && (GET_CODE (len) != CONST_INT || INTVAL (len) > (1<<16)))
    return false;

  /* As the result of CMPINT is inverted compared to what we need,
     we have to swap the operands.  */
  tmp = op0; op0 = op1; op1 = tmp;

  if (GET_CODE (len) == CONST_INT && INTVAL (len) >= 0 && INTVAL (len) <= 256)
    {
      if (INTVAL (len) > 0)
	{
	  emit_insn (gen_cmpmem_short (op0, op1, GEN_INT (INTVAL (len) - 1)));
	  emit_insn (gen_cmpint (target, ccreg));
	}
      else
	emit_move_insn (target, const0_rtx);
    }
  else if (TARGET_MVCLE)
    {
      emit_insn (gen_cmpmem_long (op0, op1, convert_to_mode (Pmode, len, 1)));
      emit_insn (gen_cmpint (target, ccreg));
    }
  else
    {
      rtx addr0, addr1, count, blocks, temp;
      rtx_code_label *loop_start_label = gen_label_rtx ();
      rtx_code_label *loop_end_label = gen_label_rtx ();
      rtx_code_label *end_label = gen_label_rtx ();
      machine_mode mode;

      mode = GET_MODE (len);
      if (mode == VOIDmode)
	mode = Pmode;

      addr0 = gen_reg_rtx (Pmode);
      addr1 = gen_reg_rtx (Pmode);
      count = gen_reg_rtx (mode);
      blocks = gen_reg_rtx (mode);

      convert_move (count, len, 1);
      emit_cmp_and_jump_insns (count, const0_rtx,
			       EQ, NULL_RTX, mode, 1, end_label);

      emit_move_insn (addr0, force_operand (XEXP (op0, 0), NULL_RTX));
      emit_move_insn (addr1, force_operand (XEXP (op1, 0), NULL_RTX));
      op0 = change_address (op0, VOIDmode, addr0);
      op1 = change_address (op1, VOIDmode, addr1);

      temp = expand_binop (mode, add_optab, count, constm1_rtx, count, 1,
			   OPTAB_DIRECT);
      if (temp != count)
	emit_move_insn (count, temp);

      temp = expand_binop (mode, lshr_optab, count, GEN_INT (8), blocks, 1,
			   OPTAB_DIRECT);
      if (temp != blocks)
	emit_move_insn (blocks, temp);

      emit_cmp_and_jump_insns (blocks, const0_rtx,
			       EQ, NULL_RTX, mode, 1, loop_end_label);

      emit_label (loop_start_label);

      if (TARGET_Z10
	  && (GET_CODE (len) != CONST_INT || INTVAL (len) > 512))
	{
	  rtx prefetch;

	  /* Issue a read prefetch for the +2 cache line of operand 1.  */
	  prefetch = gen_prefetch (gen_rtx_PLUS (Pmode, addr0, GEN_INT (512)),
				   const0_rtx, const0_rtx);
	  emit_insn (prefetch);
	  PREFETCH_SCHEDULE_BARRIER_P (prefetch) = true;

	  /* Issue a read prefetch for the +2 cache line of operand 2.  */
	  prefetch = gen_prefetch (gen_rtx_PLUS (Pmode, addr1, GEN_INT (512)),
				   const0_rtx, const0_rtx);
	  emit_insn (prefetch);
	  PREFETCH_SCHEDULE_BARRIER_P (prefetch) = true;
	}

      emit_insn (gen_cmpmem_short (op0, op1, GEN_INT (255)));
      temp = gen_rtx_NE (VOIDmode, ccreg, const0_rtx);
      temp = gen_rtx_IF_THEN_ELSE (VOIDmode, temp,
			gen_rtx_LABEL_REF (VOIDmode, end_label), pc_rtx);
      temp = gen_rtx_SET (pc_rtx, temp);
      emit_jump_insn (temp);

      s390_load_address (addr0,
			 gen_rtx_PLUS (Pmode, addr0, GEN_INT (256)));
      s390_load_address (addr1,
			 gen_rtx_PLUS (Pmode, addr1, GEN_INT (256)));

      temp = expand_binop (mode, add_optab, blocks, constm1_rtx, blocks, 1,
			   OPTAB_DIRECT);
      if (temp != blocks)
	emit_move_insn (blocks, temp);

      emit_cmp_and_jump_insns (blocks, const0_rtx,
			       EQ, NULL_RTX, mode, 1, loop_end_label);

      emit_jump (loop_start_label);
      emit_label (loop_end_label);

      emit_insn (gen_cmpmem_short (op0, op1,
				   convert_to_mode (Pmode, count, 1)));
      emit_label (end_label);

      emit_insn (gen_cmpint (target, ccreg));
    }
  return true;
}

/* Emit a conditional jump to LABEL for condition code mask MASK using
   comparsion operator COMPARISON.  Return the emitted jump insn.  */

static rtx_insn *
s390_emit_ccraw_jump (HOST_WIDE_INT mask, enum rtx_code comparison, rtx label)
{
  rtx temp;

  gcc_assert (comparison == EQ || comparison == NE);
  gcc_assert (mask > 0 && mask < 15);

  temp = gen_rtx_fmt_ee (comparison, VOIDmode,
			 gen_rtx_REG (CCRAWmode, CC_REGNUM), GEN_INT (mask));
  temp = gen_rtx_IF_THEN_ELSE (VOIDmode, temp,
			       gen_rtx_LABEL_REF (VOIDmode, label), pc_rtx);
  temp = gen_rtx_SET (pc_rtx, temp);
  return emit_jump_insn (temp);
}

/* Emit the instructions to implement strlen of STRING and store the
   result in TARGET.  The string has the known ALIGNMENT.  This
   version uses vector instructions and is therefore not appropriate
   for targets prior to z13.  */

void
s390_expand_vec_strlen (rtx target, rtx string, rtx alignment)
{
  rtx highest_index_to_load_reg = gen_reg_rtx (Pmode);
  rtx str_reg = gen_reg_rtx (V16QImode);
  rtx str_addr_base_reg = gen_reg_rtx (Pmode);
  rtx str_idx_reg = gen_reg_rtx (Pmode);
  rtx result_reg = gen_reg_rtx (V16QImode);
  rtx is_aligned_label = gen_label_rtx ();
  rtx into_loop_label = NULL_RTX;
  rtx loop_start_label = gen_label_rtx ();
  rtx temp;
  rtx len = gen_reg_rtx (QImode);
  rtx cond;
  rtx mem;

  s390_load_address (str_addr_base_reg, XEXP (string, 0));
  emit_move_insn (str_idx_reg, const0_rtx);

  if (INTVAL (alignment) < 16)
    {
      /* Check whether the address happens to be aligned properly so
	 jump directly to the aligned loop.  */
      emit_cmp_and_jump_insns (gen_rtx_AND (Pmode,
					    str_addr_base_reg, GEN_INT (15)),
			       const0_rtx, EQ, NULL_RTX,
			       Pmode, 1, is_aligned_label);

      temp = gen_reg_rtx (Pmode);
      temp = expand_binop (Pmode, and_optab, str_addr_base_reg,
			   GEN_INT (15), temp, 1, OPTAB_DIRECT);
      gcc_assert (REG_P (temp));
      highest_index_to_load_reg =
	expand_binop (Pmode, sub_optab, GEN_INT (15), temp,
		      highest_index_to_load_reg, 1, OPTAB_DIRECT);
      gcc_assert (REG_P (highest_index_to_load_reg));
      emit_insn (gen_vllv16qi (str_reg,
		   convert_to_mode (SImode, highest_index_to_load_reg, 1),
		   gen_rtx_MEM (BLKmode, str_addr_base_reg)));

      into_loop_label = gen_label_rtx ();
      s390_emit_jump (into_loop_label, NULL_RTX);
      emit_barrier ();
    }

  emit_label (is_aligned_label);
  LABEL_NUSES (is_aligned_label) = INTVAL (alignment) < 16 ? 2 : 1;

  /* Reaching this point we are only performing 16 bytes aligned
     loads.  */
  emit_move_insn (highest_index_to_load_reg, GEN_INT (15));

  emit_label (loop_start_label);
  LABEL_NUSES (loop_start_label) = 1;

  /* Load 16 bytes of the string into VR.  */
  mem = gen_rtx_MEM (V16QImode,
		     gen_rtx_PLUS (Pmode, str_idx_reg, str_addr_base_reg));
  set_mem_align (mem, 128);
  emit_move_insn (str_reg, mem);
  if (into_loop_label != NULL_RTX)
    {
      emit_label (into_loop_label);
      LABEL_NUSES (into_loop_label) = 1;
    }

  /* Increment string index by 16 bytes.  */
  expand_binop (Pmode, add_optab, str_idx_reg, GEN_INT (16),
		str_idx_reg, 1, OPTAB_DIRECT);

  emit_insn (gen_vec_vfenesv16qi (result_reg, str_reg, str_reg,
				  GEN_INT (VSTRING_FLAG_ZS | VSTRING_FLAG_CS)));

  add_int_reg_note (s390_emit_ccraw_jump (8, NE, loop_start_label),
		    REG_BR_PROB,
		    profile_probability::very_likely ().to_reg_br_prob_note ());
  emit_insn (gen_vec_extractv16qiqi (len, result_reg, GEN_INT (7)));

  /* If the string pointer wasn't aligned we have loaded less then 16
     bytes and the remaining bytes got filled with zeros (by vll).
     Now we have to check whether the resulting index lies within the
     bytes actually part of the string.  */

  cond = s390_emit_compare (GT, convert_to_mode (Pmode, len, 1),
			    highest_index_to_load_reg);
  s390_load_address (highest_index_to_load_reg,
		     gen_rtx_PLUS (Pmode, highest_index_to_load_reg,
				   const1_rtx));
  if (TARGET_64BIT)
    emit_insn (gen_movdicc (str_idx_reg, cond,
			    highest_index_to_load_reg, str_idx_reg));
  else
    emit_insn (gen_movsicc (str_idx_reg, cond,
			    highest_index_to_load_reg, str_idx_reg));

  add_reg_br_prob_note (s390_emit_jump (is_aligned_label, cond),
			profile_probability::very_unlikely ());

  expand_binop (Pmode, add_optab, str_idx_reg,
		GEN_INT (-16), str_idx_reg, 1, OPTAB_DIRECT);
  /* FIXME: len is already zero extended - so avoid the llgcr emitted
     here.  */
  temp = expand_binop (Pmode, add_optab, str_idx_reg,
		       convert_to_mode (Pmode, len, 1),
		       target, 1, OPTAB_DIRECT);
  if (temp != target)
    emit_move_insn (target, temp);
}

void
s390_expand_vec_movstr (rtx result, rtx dst, rtx src)
{
  rtx temp = gen_reg_rtx (Pmode);
  rtx src_addr = XEXP (src, 0);
  rtx dst_addr = XEXP (dst, 0);
  rtx src_addr_reg = gen_reg_rtx (Pmode);
  rtx dst_addr_reg = gen_reg_rtx (Pmode);
  rtx offset = gen_reg_rtx (Pmode);
  rtx vsrc = gen_reg_rtx (V16QImode);
  rtx vpos = gen_reg_rtx (V16QImode);
  rtx loadlen = gen_reg_rtx (SImode);
  rtx gpos_qi = gen_reg_rtx(QImode);
  rtx gpos = gen_reg_rtx (SImode);
  rtx done_label = gen_label_rtx ();
  rtx loop_label = gen_label_rtx ();
  rtx exit_label = gen_label_rtx ();
  rtx full_label = gen_label_rtx ();

  /* Perform a quick check for string ending on the first up to 16
     bytes and exit early if successful.  */

  emit_insn (gen_vlbb (vsrc, src, GEN_INT (6)));
  emit_insn (gen_lcbb (loadlen, src_addr, GEN_INT (6)));
  emit_insn (gen_vfenezv16qi (vpos, vsrc, vsrc));
  emit_insn (gen_vec_extractv16qiqi (gpos_qi, vpos, GEN_INT (7)));
  emit_move_insn (gpos, gen_rtx_SUBREG (SImode, gpos_qi, 0));
  /* gpos is the byte index if a zero was found and 16 otherwise.
     So if it is lower than the loaded bytes we have a hit.  */
  emit_cmp_and_jump_insns (gpos, loadlen, GE, NULL_RTX, SImode, 1,
			   full_label);
  emit_insn (gen_vstlv16qi (vsrc, gpos, dst));

  force_expand_binop (Pmode, add_optab, dst_addr, gpos, result,
		      1, OPTAB_DIRECT);
  emit_jump (exit_label);
  emit_barrier ();

  emit_label (full_label);
  LABEL_NUSES (full_label) = 1;

  /* Calculate `offset' so that src + offset points to the last byte
     before 16 byte alignment.  */

  /* temp = src_addr & 0xf */
  force_expand_binop (Pmode, and_optab, src_addr, GEN_INT (15), temp,
		      1, OPTAB_DIRECT);

  /* offset = 0xf - temp */
  emit_move_insn (offset, GEN_INT (15));
  force_expand_binop (Pmode, sub_optab, offset, temp, offset,
		      1, OPTAB_DIRECT);

  /* Store `offset' bytes in the dstination string.  The quick check
     has loaded at least `offset' bytes into vsrc.  */

  emit_insn (gen_vstlv16qi (vsrc, gen_lowpart (SImode, offset), dst));

  /* Advance to the next byte to be loaded.  */
  force_expand_binop (Pmode, add_optab, offset, const1_rtx, offset,
		      1, OPTAB_DIRECT);

  /* Make sure the addresses are single regs which can be used as a
     base.  */
  emit_move_insn (src_addr_reg, src_addr);
  emit_move_insn (dst_addr_reg, dst_addr);

  /* MAIN LOOP */

  emit_label (loop_label);
  LABEL_NUSES (loop_label) = 1;

  emit_move_insn (vsrc,
		  gen_rtx_MEM (V16QImode,
			       gen_rtx_PLUS (Pmode, src_addr_reg, offset)));

  emit_insn (gen_vec_vfenesv16qi (vpos, vsrc, vsrc,
				  GEN_INT (VSTRING_FLAG_ZS | VSTRING_FLAG_CS)));
  add_int_reg_note (s390_emit_ccraw_jump (8, EQ, done_label),
		    REG_BR_PROB, profile_probability::very_unlikely ()
				  .to_reg_br_prob_note ());

  emit_move_insn (gen_rtx_MEM (V16QImode,
			       gen_rtx_PLUS (Pmode, dst_addr_reg, offset)),
		  vsrc);
  /* offset += 16 */
  force_expand_binop (Pmode, add_optab, offset, GEN_INT (16),
		      offset,  1, OPTAB_DIRECT);

  emit_jump (loop_label);
  emit_barrier ();

  /* REGULAR EXIT */

  /* We are done.  Add the offset of the zero character to the dst_addr
     pointer to get the result.  */

  emit_label (done_label);
  LABEL_NUSES (done_label) = 1;

  force_expand_binop (Pmode, add_optab, dst_addr_reg, offset, dst_addr_reg,
		      1, OPTAB_DIRECT);

  emit_insn (gen_vec_extractv16qiqi (gpos_qi, vpos, GEN_INT (7)));
  emit_move_insn (gpos, gen_rtx_SUBREG (SImode, gpos_qi, 0));

  emit_insn (gen_vstlv16qi (vsrc, gpos, gen_rtx_MEM (BLKmode, dst_addr_reg)));

  force_expand_binop (Pmode, add_optab, dst_addr_reg, gpos, result,
		      1, OPTAB_DIRECT);

  /* EARLY EXIT */

  emit_label (exit_label);
  LABEL_NUSES (exit_label) = 1;
}


/* Expand conditional increment or decrement using alc/slb instructions.
   Should generate code setting DST to either SRC or SRC + INCREMENT,
   depending on the result of the comparison CMP_OP0 CMP_CODE CMP_OP1.
   Returns true if successful, false otherwise.

   That makes it possible to implement some if-constructs without jumps e.g.:
   (borrow = CC0 | CC1 and carry = CC2 | CC3)
   unsigned int a, b, c;
   if (a < b)  c++; -> CCU  b > a  -> CC2;    c += carry;
   if (a < b)  c--; -> CCL3 a - b  -> borrow; c -= borrow;
   if (a <= b) c++; -> CCL3 b - a  -> borrow; c += carry;
   if (a <= b) c--; -> CCU  a <= b -> borrow; c -= borrow;

   Checks for EQ and NE with a nonzero value need an additional xor e.g.:
   if (a == b) c++; -> CCL3 a ^= b; 0 - a  -> borrow;    c += carry;
   if (a == b) c--; -> CCU  a ^= b; a <= 0 -> CC0 | CC1; c -= borrow;
   if (a != b) c++; -> CCU  a ^= b; a > 0  -> CC2;       c += carry;
   if (a != b) c--; -> CCL3 a ^= b; 0 - a  -> borrow;    c -= borrow; */

bool
s390_expand_addcc (enum rtx_code cmp_code, rtx cmp_op0, rtx cmp_op1,
		   rtx dst, rtx src, rtx increment)
{
  machine_mode cmp_mode;
  machine_mode cc_mode;
  rtx op_res;
  rtx insn;
  rtvec p;
  int ret;

  if ((GET_MODE (cmp_op0) == SImode || GET_MODE (cmp_op0) == VOIDmode)
      && (GET_MODE (cmp_op1) == SImode || GET_MODE (cmp_op1) == VOIDmode))
    cmp_mode = SImode;
  else if ((GET_MODE (cmp_op0) == DImode || GET_MODE (cmp_op0) == VOIDmode)
	   && (GET_MODE (cmp_op1) == DImode || GET_MODE (cmp_op1) == VOIDmode))
    cmp_mode = DImode;
  else
    return false;

  /* Try ADD LOGICAL WITH CARRY.  */
  if (increment == const1_rtx)
    {
      /* Determine CC mode to use.  */
      if (cmp_code == EQ || cmp_code == NE)
	{
	  if (cmp_op1 != const0_rtx)
	    {
	      cmp_op0 = expand_simple_binop (cmp_mode, XOR, cmp_op0, cmp_op1,
					     NULL_RTX, 0, OPTAB_WIDEN);
	      cmp_op1 = const0_rtx;
	    }

	  cmp_code = cmp_code == EQ ? LEU : GTU;
	}

      if (cmp_code == LTU || cmp_code == LEU)
	{
	  rtx tem = cmp_op0;
	  cmp_op0 = cmp_op1;
	  cmp_op1 = tem;
	  cmp_code = swap_condition (cmp_code);
	}

      switch (cmp_code)
	{
	  case GTU:
	    cc_mode = CCUmode;
	    break;

	  case GEU:
	    cc_mode = CCL3mode;
	    break;

	  default:
	    return false;
	}

      /* Emit comparison instruction pattern. */
      if (!register_operand (cmp_op0, cmp_mode))
	cmp_op0 = force_reg (cmp_mode, cmp_op0);

      insn = gen_rtx_SET (gen_rtx_REG (cc_mode, CC_REGNUM),
			  gen_rtx_COMPARE (cc_mode, cmp_op0, cmp_op1));
      /* We use insn_invalid_p here to add clobbers if required.  */
      ret = insn_invalid_p (emit_insn (insn), false);
      gcc_assert (!ret);

      /* Emit ALC instruction pattern.  */
      op_res = gen_rtx_fmt_ee (cmp_code, GET_MODE (dst),
			       gen_rtx_REG (cc_mode, CC_REGNUM),
			       const0_rtx);

      if (src != const0_rtx)
	{
	  if (!register_operand (src, GET_MODE (dst)))
	    src = force_reg (GET_MODE (dst), src);

	  op_res = gen_rtx_PLUS (GET_MODE (dst), op_res, src);
	  op_res = gen_rtx_PLUS (GET_MODE (dst), op_res, const0_rtx);
	}

      p = rtvec_alloc (2);
      RTVEC_ELT (p, 0) =
	gen_rtx_SET (dst, op_res);
      RTVEC_ELT (p, 1) =
	gen_rtx_CLOBBER (VOIDmode, gen_rtx_REG (CCmode, CC_REGNUM));
      emit_insn (gen_rtx_PARALLEL (VOIDmode, p));

      return true;
    }

  /* Try SUBTRACT LOGICAL WITH BORROW.  */
  if (increment == constm1_rtx)
    {
      /* Determine CC mode to use.  */
      if (cmp_code == EQ || cmp_code == NE)
	{
	  if (cmp_op1 != const0_rtx)
	    {
	      cmp_op0 = expand_simple_binop (cmp_mode, XOR, cmp_op0, cmp_op1,
					     NULL_RTX, 0, OPTAB_WIDEN);
	      cmp_op1 = const0_rtx;
	    }

	  cmp_code = cmp_code == EQ ? LEU : GTU;
	}

      if (cmp_code == GTU || cmp_code == GEU)
	{
	  rtx tem = cmp_op0;
	  cmp_op0 = cmp_op1;
	  cmp_op1 = tem;
	  cmp_code = swap_condition (cmp_code);
	}

      switch (cmp_code)
	{
	  case LEU:
	    cc_mode = CCUmode;
	    break;

	  case LTU:
	    cc_mode = CCL3mode;
	    break;

	  default:
	    return false;
	}

      /* Emit comparison instruction pattern. */
      if (!register_operand (cmp_op0, cmp_mode))
	cmp_op0 = force_reg (cmp_mode, cmp_op0);

      insn = gen_rtx_SET (gen_rtx_REG (cc_mode, CC_REGNUM),
			  gen_rtx_COMPARE (cc_mode, cmp_op0, cmp_op1));
      /* We use insn_invalid_p here to add clobbers if required.  */
      ret = insn_invalid_p (emit_insn (insn), false);
      gcc_assert (!ret);

      /* Emit SLB instruction pattern.  */
      if (!register_operand (src, GET_MODE (dst)))
	src = force_reg (GET_MODE (dst), src);

      op_res = gen_rtx_MINUS (GET_MODE (dst),
			      gen_rtx_MINUS (GET_MODE (dst), src, const0_rtx),
			      gen_rtx_fmt_ee (cmp_code, GET_MODE (dst),
					      gen_rtx_REG (cc_mode, CC_REGNUM),
					      const0_rtx));
      p = rtvec_alloc (2);
      RTVEC_ELT (p, 0) =
	gen_rtx_SET (dst, op_res);
      RTVEC_ELT (p, 1) =
	gen_rtx_CLOBBER (VOIDmode, gen_rtx_REG (CCmode, CC_REGNUM));
      emit_insn (gen_rtx_PARALLEL (VOIDmode, p));

      return true;
    }

  return false;
}

/* Expand code for the insv template. Return true if successful.  */

bool
s390_expand_insv (rtx dest, rtx op1, rtx op2, rtx src)
{
  int bitsize = INTVAL (op1);
  int bitpos = INTVAL (op2);
  machine_mode mode = GET_MODE (dest);
  machine_mode smode;
  int smode_bsize, mode_bsize;
  rtx op, clobber;

  if (bitsize + bitpos > GET_MODE_BITSIZE (mode))
    return false;

  /* Just a move.  */
  if (bitpos == 0
      && bitsize == GET_MODE_BITSIZE (GET_MODE (src))
      && mode == GET_MODE (src))
    {
      emit_move_insn (dest, src);
      return true;
    }

  /* Generate INSERT IMMEDIATE (IILL et al).  */
  /* (set (ze (reg)) (const_int)).  */
  if (TARGET_ZARCH
      && register_operand (dest, word_mode)
      && (bitpos % 16) == 0
      && (bitsize % 16) == 0
      && const_int_operand (src, VOIDmode))
    {
      HOST_WIDE_INT val = INTVAL (src);
      int regpos = bitpos + bitsize;

      while (regpos > bitpos)
	{
	  machine_mode putmode;
	  int putsize;

	  if (TARGET_EXTIMM && (regpos % 32 == 0) && (regpos >= bitpos + 32))
	    putmode = SImode;
	  else
	    putmode = HImode;

	  putsize = GET_MODE_BITSIZE (putmode);
	  regpos -= putsize;
	  emit_move_insn (gen_rtx_ZERO_EXTRACT (word_mode, dest,
						GEN_INT (putsize),
						GEN_INT (regpos)),
			  gen_int_mode (val, putmode));
	  val >>= putsize;
	}
      gcc_assert (regpos == bitpos);
      return true;
    }

  smode = smallest_int_mode_for_size (bitsize);
  smode_bsize = GET_MODE_BITSIZE (smode);
  mode_bsize = GET_MODE_BITSIZE (mode);

  /* Generate STORE CHARACTERS UNDER MASK (STCM et al).  */
  if (bitpos == 0
      && (bitsize % BITS_PER_UNIT) == 0
      && MEM_P (dest)
      && (register_operand (src, word_mode)
	  || const_int_operand (src, VOIDmode)))
    {
      /* Emit standard pattern if possible.  */
      if (smode_bsize == bitsize)
	{
	  emit_move_insn (adjust_address (dest, smode, 0),
			  gen_lowpart (smode, src));
	  return true;
	}

      /* (set (ze (mem)) (const_int)).  */
      else if (const_int_operand (src, VOIDmode))
	{
	  int size = bitsize / BITS_PER_UNIT;
	  rtx src_mem = adjust_address (force_const_mem (word_mode, src),
					BLKmode,
					UNITS_PER_WORD - size);

	  dest = adjust_address (dest, BLKmode, 0);
	  set_mem_size (dest, size);
	  s390_expand_cpymem (dest, src_mem, GEN_INT (size));
	  return true;
	}

      /* (set (ze (mem)) (reg)).  */
      else if (register_operand (src, word_mode))
	{
	  if (bitsize <= 32)
	    emit_move_insn (gen_rtx_ZERO_EXTRACT (word_mode, dest, op1,
						  const0_rtx), src);
	  else
	    {
	      /* Emit st,stcmh sequence.  */
	      int stcmh_width = bitsize - 32;
	      int size = stcmh_width / BITS_PER_UNIT;

	      emit_move_insn (adjust_address (dest, SImode, size),
			      gen_lowpart (SImode, src));
	      set_mem_size (dest, size);
	      emit_move_insn (gen_rtx_ZERO_EXTRACT (word_mode, dest,
						    GEN_INT (stcmh_width),
						    const0_rtx),
			      gen_rtx_LSHIFTRT (word_mode, src, GEN_INT (32)));
	    }
	  return true;
	}
    }

  /* Generate INSERT CHARACTERS UNDER MASK (IC, ICM et al).  */
  if ((bitpos % BITS_PER_UNIT) == 0
      && (bitsize % BITS_PER_UNIT) == 0
      && (bitpos & 32) == ((bitpos + bitsize - 1) & 32)
      && MEM_P (src)
      && (mode == DImode || mode == SImode)
      && mode != smode
      && register_operand (dest, mode))
    {
      /* Emit a strict_low_part pattern if possible.  */
      if (smode_bsize == bitsize && bitpos == mode_bsize - smode_bsize)
	{
	  rtx low_dest = s390_gen_lowpart_subreg (smode, dest);
	  rtx low_src = gen_lowpart (smode, src);

	  switch (smode)
	    {
	    case E_QImode: emit_insn (gen_movstrictqi (low_dest, low_src)); return true;
	    case E_HImode: emit_insn (gen_movstricthi (low_dest, low_src)); return true;
	    case E_SImode: emit_insn (gen_movstrictsi (low_dest, low_src)); return true;
	    default: break;
	    }
	}

      /* ??? There are more powerful versions of ICM that are not
	 completely represented in the md file.  */
    }

  /* For z10, generate ROTATE THEN INSERT SELECTED BITS (RISBG et al).  */
  if (TARGET_Z10 && (mode == DImode || mode == SImode))
    {
      machine_mode mode_s = GET_MODE (src);

      if (CONSTANT_P (src))
	{
	  /* For constant zero values the representation with AND
	     appears to be folded in more situations than the (set
	     (zero_extract) ...).
	     We only do this when the start and end of the bitfield
	     remain in the same SImode chunk.  That way nihf or nilf
	     can be used.
	     The AND patterns might still generate a risbg for this.  */
	  if (src == const0_rtx && bitpos / 32  == (bitpos + bitsize - 1) / 32)
	    return false;
	  else
	    src = force_reg (mode, src);
	}
      else if (mode_s != mode)
	{
	  gcc_assert (GET_MODE_BITSIZE (mode_s) >= bitsize);
	  src = force_reg (mode_s, src);
	  src = gen_lowpart (mode, src);
	}

      op = gen_rtx_ZERO_EXTRACT (mode, dest, op1, op2),
      op = gen_rtx_SET (op, src);

      if (!TARGET_ZEC12)
	{
	  clobber = gen_rtx_CLOBBER (VOIDmode, gen_rtx_REG (CCmode, CC_REGNUM));
	  op = gen_rtx_PARALLEL (VOIDmode, gen_rtvec (2, op, clobber));
	}
      emit_insn (op);

      return true;
    }

  return false;
}

/* A subroutine of s390_expand_cs_hqi and s390_expand_atomic which returns a
   register that holds VAL of mode MODE shifted by COUNT bits.  */

static inline rtx
s390_expand_mask_and_shift (rtx val, machine_mode mode, rtx count)
{
  val = expand_simple_binop (SImode, AND, val, GEN_INT (GET_MODE_MASK (mode)),
			     NULL_RTX, 1, OPTAB_DIRECT);
  return expand_simple_binop (SImode, ASHIFT, val, count,
			      NULL_RTX, 1, OPTAB_DIRECT);
}

/* Generate a vector comparison COND of CMP_OP1 and CMP_OP2 and store
   the result in TARGET.  */

void
s390_expand_vec_compare (rtx target, enum rtx_code cond,
			 rtx cmp_op1, rtx cmp_op2)
{
  machine_mode mode = GET_MODE (target);
  bool neg_p = false, swap_p = false;
  rtx tmp;

  if (GET_MODE_CLASS (GET_MODE (cmp_op1)) == MODE_VECTOR_FLOAT)
    {
      cmp_op2 = force_reg (GET_MODE (cmp_op1), cmp_op2);
      switch (cond)
	{
	  /* NE a != b -> !(a == b) */
	case NE:   cond = EQ; neg_p = true;                break;
	case UNGT:
	  emit_insn (gen_vec_cmpungt (target, cmp_op1, cmp_op2));
	  return;
	case UNGE:
	  emit_insn (gen_vec_cmpunge (target, cmp_op1, cmp_op2));
	  return;
	case LE:   cond = GE;               swap_p = true; break;
	  /* UNLE: (a u<= b) -> (b u>= a).  */
	case UNLE:
	  emit_insn (gen_vec_cmpunge (target, cmp_op2, cmp_op1));
	  return;
	  /* LT: a < b -> b > a */
	case LT:   cond = GT;               swap_p = true; break;
	  /* UNLT: (a u< b) -> (b u> a).  */
	case UNLT:
	  emit_insn (gen_vec_cmpungt (target, cmp_op2, cmp_op1));
	  return;
	case UNEQ:
	  emit_insn (gen_vec_cmpuneq (target, cmp_op1, cmp_op2));
	  return;
	case LTGT:
	  emit_insn (gen_vec_cmpltgt (target, cmp_op1, cmp_op2));
	  return;
	case ORDERED:
	  emit_insn (gen_vec_cmpordered (target, cmp_op1, cmp_op2));
	  return;
	case UNORDERED:
	  emit_insn (gen_vec_cmpunordered (target, cmp_op1, cmp_op2));
	  return;
	default: break;
	}
    }
  else
    {
      /* Turn x < 0 into x >> (bits per element - 1)  */
      if (cond == LT && cmp_op2 == CONST0_RTX (mode))
	{
	  int shift = GET_MODE_BITSIZE (GET_MODE_INNER (mode)) - 1;
	  rtx res = expand_simple_binop (mode, ASHIFTRT, cmp_op1,
					 GEN_INT (shift), target,
					 0, OPTAB_DIRECT);
	  if (res != target)
	    emit_move_insn (target, res);
	  return;
	}
      cmp_op2 = force_reg (GET_MODE (cmp_op1), cmp_op2);

      switch (cond)
	{
	  /* NE: a != b -> !(a == b) */
	case NE:  cond = EQ;  neg_p = true;                break;
	  /* GE: a >= b -> !(b > a) */
	case GE:  cond = GT;  neg_p = true; swap_p = true; break;
	  /* GEU: a >= b -> !(b > a) */
	case GEU: cond = GTU; neg_p = true; swap_p = true; break;
	  /* LE: a <= b -> !(a > b) */
	case LE:  cond = GT;  neg_p = true;                break;
	  /* LEU: a <= b -> !(a > b) */
	case LEU: cond = GTU; neg_p = true;                break;
	  /* LT: a < b -> b > a */
	case LT:  cond = GT;                swap_p = true; break;
	  /* LTU: a < b -> b > a */
	case LTU: cond = GTU;               swap_p = true; break;
	default: break;
	}
    }

  if (swap_p)
    {
      tmp = cmp_op1; cmp_op1 = cmp_op2; cmp_op2 = tmp;
    }

  emit_insn (gen_rtx_SET (target, gen_rtx_fmt_ee (cond,
						  mode,
						  cmp_op1, cmp_op2)));
  if (neg_p)
    emit_insn (gen_rtx_SET (target, gen_rtx_NOT (mode, target)));
}

/* Expand the comparison CODE of CMP1 and CMP2 and copy 1 or 0 into
   TARGET if either all (ALL_P is true) or any (ALL_P is false) of the
   elements in CMP1 and CMP2 fulfill the comparison.
   This function is only used to emit patterns for the vx builtins and
   therefore only handles comparison codes required by the
   builtins.  */
void
s390_expand_vec_compare_cc (rtx target, enum rtx_code code,
			    rtx cmp1, rtx cmp2, bool all_p)
{
  machine_mode cc_producer_mode, cc_consumer_mode, scratch_mode;
  rtx tmp_reg = gen_reg_rtx (SImode);
  bool swap_p = false;

  if (GET_MODE_CLASS (GET_MODE (cmp1)) == MODE_VECTOR_INT)
    {
      switch (code)
	{
	case EQ:
	case NE:
	  cc_producer_mode = CCVEQmode;
	  break;
	case GE:
	case LT:
	  code = swap_condition (code);
	  swap_p = true;
	  /* fallthrough */
	case GT:
	case LE:
	  cc_producer_mode = CCVIHmode;
	  break;
	case GEU:
	case LTU:
	  code = swap_condition (code);
	  swap_p = true;
	  /* fallthrough */
	case GTU:
	case LEU:
	  cc_producer_mode = CCVIHUmode;
	  break;
	default:
	  gcc_unreachable ();
	}

      scratch_mode = GET_MODE (cmp1);
      /* These codes represent inverted CC interpretations.  Inverting
	 an ALL CC mode results in an ANY CC mode and the other way
	 around.  Invert the all_p flag here to compensate for
	 that.  */
      if (code == NE || code == LE || code == LEU)
	all_p = !all_p;

      cc_consumer_mode = all_p ? CCVIALLmode : CCVIANYmode;
    }
  else if (GET_MODE_CLASS (GET_MODE (cmp1)) == MODE_VECTOR_FLOAT)
    {
      bool inv_p = false;

      switch (code)
	{
	case EQ:   cc_producer_mode = CCVEQmode;  break;
	case NE:   cc_producer_mode = CCVEQmode;  inv_p = true; break;
	case GT:   cc_producer_mode = CCVFHmode;  break;
	case GE:   cc_producer_mode = CCVFHEmode; break;
	case UNLE: cc_producer_mode = CCVFHmode;  inv_p = true; break;
	case UNLT: cc_producer_mode = CCVFHEmode; inv_p = true; break;
	case LT:   cc_producer_mode = CCVFHmode;  code = GT; swap_p = true; break;
	case LE:   cc_producer_mode = CCVFHEmode; code = GE; swap_p = true; break;
	default: gcc_unreachable ();
	}
      scratch_mode = related_int_vector_mode (GET_MODE (cmp1)).require ();

      if (inv_p)
	all_p = !all_p;

      cc_consumer_mode = all_p ? CCVFALLmode : CCVFANYmode;
    }
  else
    gcc_unreachable ();

  if (swap_p)
    {
      rtx tmp = cmp2;
      cmp2 = cmp1;
      cmp1 = tmp;
    }

  emit_insn (gen_rtx_PARALLEL (VOIDmode,
	       gen_rtvec (2, gen_rtx_SET (
			       gen_rtx_REG (cc_producer_mode, CC_REGNUM),
			       gen_rtx_COMPARE (cc_producer_mode, cmp1, cmp2)),
			  gen_rtx_CLOBBER (VOIDmode,
					   gen_rtx_SCRATCH (scratch_mode)))));
  emit_move_insn (target, const0_rtx);
  emit_move_insn (tmp_reg, const1_rtx);

  emit_move_insn (target,
		  gen_rtx_IF_THEN_ELSE (SImode,
		    gen_rtx_fmt_ee (code, VOIDmode,
				    gen_rtx_REG (cc_consumer_mode, CC_REGNUM),
				    const0_rtx),
					tmp_reg, target));
}

/* Invert the comparison CODE applied to a CC mode.  This is only safe
   if we know whether there result was created by a floating point
   compare or not.  For the CCV modes this is encoded as part of the
   mode.  */
enum rtx_code
s390_reverse_condition (machine_mode mode, enum rtx_code code)
{
  /* Reversal of FP compares takes care -- an ordered compare
     becomes an unordered compare and vice versa.  */
  if (mode == CCVFALLmode || mode == CCVFANYmode || mode == CCSFPSmode)
    return reverse_condition_maybe_unordered (code);
  else if (mode == CCVIALLmode || mode == CCVIANYmode)
    return reverse_condition (code);
  else
    gcc_unreachable ();
}

/* Generate a vector comparison expression loading either elements of
   THEN or ELS into TARGET depending on the comparison COND of CMP_OP1
   and CMP_OP2.  */

void
s390_expand_vcond (rtx target, rtx then, rtx els,
		   enum rtx_code cond, rtx cmp_op1, rtx cmp_op2)
{
  rtx tmp;
  machine_mode result_mode;
  rtx result_target;

  machine_mode target_mode = GET_MODE (target);
  machine_mode cmp_mode = GET_MODE (cmp_op1);
  rtx op = (cond == LT) ? els : then;

  /* Try to optimize x < 0 ? -1 : 0 into (signed) x >> 31
     and x < 0 ? 1 : 0 into (unsigned) x >> 31.  Likewise
     for short and byte (x >> 15 and x >> 7 respectively).  */
  if ((cond == LT || cond == GE)
      && target_mode == cmp_mode
      && cmp_op2 == CONST0_RTX (cmp_mode)
      && op == CONST0_RTX (target_mode)
      && s390_vector_mode_supported_p (target_mode)
      && GET_MODE_CLASS (target_mode) == MODE_VECTOR_INT)
    {
      rtx negop = (cond == LT) ? then : els;

      int shift = GET_MODE_BITSIZE (GET_MODE_INNER (target_mode)) - 1;

      /* if x < 0 ? 1 : 0 or if x >= 0 ? 0 : 1 */
      if (negop == CONST1_RTX (target_mode))
	{
	  rtx res = expand_simple_binop (cmp_mode, LSHIFTRT, cmp_op1,
					 GEN_INT (shift), target,
					 1, OPTAB_DIRECT);
	  if (res != target)
	    emit_move_insn (target, res);
	  return;
	}

      /* if x < 0 ? -1 : 0 or if x >= 0 ? 0 : -1 */
      else if (all_ones_operand (negop, target_mode))
	{
	  rtx res = expand_simple_binop (cmp_mode, ASHIFTRT, cmp_op1,
					 GEN_INT (shift), target,
					 0, OPTAB_DIRECT);
	  if (res != target)
	    emit_move_insn (target, res);
	  return;
	}
    }

  /* We always use an integral type vector to hold the comparison
     result.  */
  result_mode = related_int_vector_mode (cmp_mode).require ();
  result_target = gen_reg_rtx (result_mode);

  /* We allow vector immediates as comparison operands that
     can be handled by the optimization above but not by the
     following code.  Hence, force them into registers here.  */
  if (!REG_P (cmp_op1))
    cmp_op1 = force_reg (GET_MODE (cmp_op1), cmp_op1);

  s390_expand_vec_compare (result_target, cond, cmp_op1, cmp_op2);

  /* If the results are supposed to be either -1 or 0 we are done
     since this is what our compare instructions generate anyway.  */
  if (all_ones_operand (then, GET_MODE (then))
      && const0_operand (els, GET_MODE (els)))
    {
      emit_move_insn (target, gen_rtx_SUBREG (target_mode,
					      result_target, 0));
      return;
    }

  /* Otherwise we will do a vsel afterwards.  */
  /* This gets triggered e.g.
     with gcc.c-torture/compile/pr53410-1.c */
  if (!REG_P (then))
    then = force_reg (target_mode, then);

  if (!REG_P (els))
    els = force_reg (target_mode, els);

  tmp = gen_rtx_fmt_ee (EQ, VOIDmode,
			result_target,
			CONST0_RTX (result_mode));

  /* We compared the result against zero above so we have to swap then
     and els here.  */
  tmp = gen_rtx_IF_THEN_ELSE (target_mode, tmp, els, then);

  gcc_assert (target_mode == GET_MODE (then));
  emit_insn (gen_rtx_SET (target, tmp));
}

/* Emit the RTX necessary to initialize the vector TARGET with values
   in VALS.  */
void
s390_expand_vec_init (rtx target, rtx vals)
{
  machine_mode mode = GET_MODE (target);
  machine_mode inner_mode = GET_MODE_INNER (mode);
  int n_elts = GET_MODE_NUNITS (mode);
  bool all_same = true, all_regs = true, all_const_int = true;
  rtx x;
  int i;

  for (i = 0; i < n_elts; ++i)
    {
      x = XVECEXP (vals, 0, i);

      if (!CONST_INT_P (x))
	all_const_int = false;

      if (i > 0 && !rtx_equal_p (x, XVECEXP (vals, 0, 0)))
	all_same = false;

      if (!REG_P (x))
	all_regs = false;
    }

  /* Use vector gen mask or vector gen byte mask if possible.  */
  if (all_same && all_const_int)
    {
      rtx vec = gen_rtx_CONST_VECTOR (mode, XVEC (vals, 0));
      if (XVECEXP (vals, 0, 0) == const0_rtx
	  || s390_contiguous_bitmask_vector_p (vec, NULL, NULL)
	  || s390_bytemask_vector_p (vec, NULL))
	{
	  emit_insn (gen_rtx_SET (target, vec));
	  return;
	}
    }

  /* Use vector replicate instructions.  vlrep/vrepi/vrep  */
  if (all_same)
    {
      rtx elem = XVECEXP (vals, 0, 0);

      /* vec_splats accepts general_operand as source.  */
      if (!general_operand (elem, GET_MODE (elem)))
	elem = force_reg (inner_mode, elem);

      emit_insn (gen_rtx_SET (target, gen_rtx_VEC_DUPLICATE (mode, elem)));
      return;
    }

  if (all_regs
      && REG_P (target)
      && n_elts == 2
      && GET_MODE_SIZE (inner_mode) == 8)
    {
      /* Use vector load pair.  */
      emit_insn (gen_rtx_SET (target,
			      gen_rtx_VEC_CONCAT (mode,
						  XVECEXP (vals, 0, 0),
						  XVECEXP (vals, 0, 1))));
      return;
    }

  /* Use vector load logical element and zero.  */
  if (TARGET_VXE && (mode == V4SImode || mode == V4SFmode))
    {
      bool found = true;

      x = XVECEXP (vals, 0, 0);
      if (memory_operand (x, inner_mode))
	{
	  for (i = 1; i < n_elts; ++i)
	    found = found && XVECEXP (vals, 0, i) == const0_rtx;

	  if (found)
	    {
	      machine_mode half_mode = (inner_mode == SFmode
					? V2SFmode : V2SImode);
	      emit_insn (gen_rtx_SET (target,
			      gen_rtx_VEC_CONCAT (mode,
						  gen_rtx_VEC_CONCAT (half_mode,
								      x,
								      const0_rtx),
						  gen_rtx_VEC_CONCAT (half_mode,
								      const0_rtx,
								      const0_rtx))));
	      return;
	    }
	}
    }

  /* We are about to set the vector elements one by one.  Zero out the
     full register first in order to help the data flow framework to
     detect it as full VR set.  */
  emit_insn (gen_rtx_SET (target, CONST0_RTX (mode)));

  /* Unfortunately the vec_init expander is not allowed to fail.  So
     we have to implement the fallback ourselves.  */
  for (i = 0; i < n_elts; i++)
    {
      rtx elem = XVECEXP (vals, 0, i);
      if (!general_operand (elem, GET_MODE (elem)))
	elem = force_reg (inner_mode, elem);

      emit_insn (gen_rtx_SET (target,
			      gen_rtx_UNSPEC (mode,
					      gen_rtvec (3, elem,
							 GEN_INT (i), target),
					      UNSPEC_VEC_SET)));
    }
}

/* Return a parallel of constant integers to be used as permutation
   vector for a vector merge operation in MODE.  If HIGH_P is true the
   left-most elements of the source vectors are merged otherwise the
   right-most elements.  */
rtx
s390_expand_merge_perm_const (machine_mode mode, bool high_p)
{
  int nelts = GET_MODE_NUNITS (mode);
  rtx perm[16];
  int addend = high_p ? 0 : nelts;

  for (int i = 0; i < nelts; i++)
    perm[i] = GEN_INT ((i + addend) / 2 + (i % 2) * nelts);

  return gen_rtx_PARALLEL (VOIDmode, gen_rtvec_v (nelts, perm));
}

/* Emit RTL to implement a vector merge operation of SRC1 and SRC2
   which creates the result in TARGET. HIGH_P determines whether a
   merge hi or lo will be generated.  */
void
s390_expand_merge (rtx target, rtx src1, rtx src2, bool high_p)
{
  machine_mode mode = GET_MODE (target);
  opt_machine_mode opt_mode_2x = mode_for_vector (GET_MODE_INNER (mode),
						  2 * GET_MODE_NUNITS (mode));
  gcc_assert (opt_mode_2x.exists ());
  machine_mode mode_double_nelts = opt_mode_2x.require ();
  rtx constv = s390_expand_merge_perm_const (mode, high_p);
  src1 = force_reg (GET_MODE (src1), src1);
  src2 = force_reg (GET_MODE (src2), src2);
  rtx x = gen_rtx_VEC_CONCAT (mode_double_nelts, src1, src2);
  x = gen_rtx_VEC_SELECT (mode, x, constv);
  emit_insn (gen_rtx_SET (target, x));
}

/* Emit a vector constant that contains 1s in each element's sign bit position
   and 0s in other positions.  MODE is the desired constant's mode.  */
extern rtx
s390_build_signbit_mask (machine_mode mode)
{
  if (mode == TFmode && TARGET_VXE)
    {
      wide_int mask_val = wi::set_bit_in_zero (127, 128);
      rtx mask = immed_wide_int_const (mask_val, TImode);
      return gen_lowpart (TFmode, mask);
    }

  /* Generate the integral element mask value.  */
  machine_mode inner_mode = GET_MODE_INNER (mode);
  int inner_bitsize = GET_MODE_BITSIZE (inner_mode);
  wide_int mask_val = wi::set_bit_in_zero (inner_bitsize - 1, inner_bitsize);

  /* Emit the element mask rtx.  Use gen_lowpart in order to cast the integral
     value to the desired mode.  */
  machine_mode int_mode = related_int_vector_mode (mode).require ();
  rtx mask = immed_wide_int_const (mask_val, GET_MODE_INNER (int_mode));
  mask = gen_lowpart (inner_mode, mask);

  /* Emit the vector mask rtx by mode the element mask rtx.  */
  int nunits = GET_MODE_NUNITS (mode);
  rtvec v = rtvec_alloc (nunits);
  for (int i = 0; i < nunits; i++)
    RTVEC_ELT (v, i) = mask;
  return gen_rtx_CONST_VECTOR (mode, v);
}

/* Structure to hold the initial parameters for a compare_and_swap operation
   in HImode and QImode.  */

struct alignment_context
{
  rtx memsi;	  /* SI aligned memory location.  */
  rtx shift;	  /* Bit offset with regard to lsb.  */
  rtx modemask;	  /* Mask of the HQImode shifted by SHIFT bits.  */
  rtx modemaski;  /* ~modemask */
  bool aligned;	  /* True if memory is aligned, false else.  */
};

/* A subroutine of s390_expand_cs_hqi and s390_expand_atomic to initialize
   structure AC for transparent simplifying, if the memory alignment is known
   to be at least 32bit.  MEM is the memory location for the actual operation
   and MODE its mode.  */

static void
init_alignment_context (struct alignment_context *ac, rtx mem,
			machine_mode mode)
{
  ac->shift = GEN_INT (GET_MODE_SIZE (SImode) - GET_MODE_SIZE (mode));
  ac->aligned = (MEM_ALIGN (mem) >= GET_MODE_BITSIZE (SImode));

  if (ac->aligned)
    ac->memsi = adjust_address (mem, SImode, 0); /* Memory is aligned.  */
  else
    {
      /* Alignment is unknown.  */
      rtx byteoffset, addr, align;

      /* Force the address into a register.  */
      addr = force_reg (Pmode, XEXP (mem, 0));

      /* Align it to SImode.  */
      align = expand_simple_binop (Pmode, AND, addr,
				   GEN_INT (-GET_MODE_SIZE (SImode)),
				   NULL_RTX, 1, OPTAB_DIRECT);
      /* Generate MEM.  */
      ac->memsi = gen_rtx_MEM (SImode, align);
      MEM_VOLATILE_P (ac->memsi) = MEM_VOLATILE_P (mem);
      set_mem_alias_set (ac->memsi, ALIAS_SET_MEMORY_BARRIER);
      set_mem_align (ac->memsi, GET_MODE_BITSIZE (SImode));

      /* Calculate shiftcount.  */
      byteoffset = expand_simple_binop (Pmode, AND, addr,
					GEN_INT (GET_MODE_SIZE (SImode) - 1),
					NULL_RTX, 1, OPTAB_DIRECT);
      /* As we already have some offset, evaluate the remaining distance.  */
      ac->shift = expand_simple_binop (SImode, MINUS, ac->shift, byteoffset,
				      NULL_RTX, 1, OPTAB_DIRECT);
    }

  /* Shift is the byte count, but we need the bitcount.  */
  ac->shift = expand_simple_binop (SImode, ASHIFT, ac->shift, GEN_INT (3),
				   NULL_RTX, 1, OPTAB_DIRECT);

  /* Calculate masks.  */
  ac->modemask = expand_simple_binop (SImode, ASHIFT,
				      GEN_INT (GET_MODE_MASK (mode)),
				      ac->shift, NULL_RTX, 1, OPTAB_DIRECT);
  ac->modemaski = expand_simple_unop (SImode, NOT, ac->modemask,
				      NULL_RTX, 1);
}

/* A subroutine of s390_expand_cs_hqi.  Insert INS into VAL.  If possible,
   use a single insv insn into SEQ2.  Otherwise, put prep insns in SEQ1 and
   perform the merge in SEQ2.  */

static rtx
s390_two_part_insv (struct alignment_context *ac, rtx *seq1, rtx *seq2,
		    machine_mode mode, rtx val, rtx ins)
{
  rtx tmp;

  if (ac->aligned)
    {
      start_sequence ();
      tmp = copy_to_mode_reg (SImode, val);
      if (s390_expand_insv (tmp, GEN_INT (GET_MODE_BITSIZE (mode)),
			    const0_rtx, ins))
	{
	  *seq1 = NULL;
	  *seq2 = get_insns ();
	  end_sequence ();
	  return tmp;
	}
      end_sequence ();
    }

  /* Failed to use insv.  Generate a two part shift and mask.  */
  start_sequence ();
  tmp = s390_expand_mask_and_shift (ins, mode, ac->shift);
  *seq1 = get_insns ();
  end_sequence ();

  start_sequence ();
  tmp = expand_simple_binop (SImode, IOR, tmp, val, NULL_RTX, 1, OPTAB_DIRECT);
  *seq2 = get_insns ();
  end_sequence ();

  return tmp;
}

/* Expand an atomic compare and swap operation for HImode and QImode.  MEM is
   the memory location, CMP the old value to compare MEM with and NEW_RTX the
   value to set if CMP == MEM.  */

static void
s390_expand_cs_hqi (machine_mode mode, rtx btarget, rtx vtarget, rtx mem,
		    rtx cmp, rtx new_rtx, bool is_weak)
{
  struct alignment_context ac;
  rtx cmpv, newv, val, cc, seq0, seq1, seq2, seq3;
  rtx res = gen_reg_rtx (SImode);
  rtx_code_label *csloop = NULL, *csend = NULL;

  gcc_assert (MEM_P (mem));

  init_alignment_context (&ac, mem, mode);

  /* Load full word.  Subsequent loads are performed by CS.  */
  val = expand_simple_binop (SImode, AND, ac.memsi, ac.modemaski,
			     NULL_RTX, 1, OPTAB_DIRECT);

  /* Prepare insertions of cmp and new_rtx into the loaded value.  When
     possible, we try to use insv to make this happen efficiently.  If
     that fails we'll generate code both inside and outside the loop.  */
  cmpv = s390_two_part_insv (&ac, &seq0, &seq2, mode, val, cmp);
  newv = s390_two_part_insv (&ac, &seq1, &seq3, mode, val, new_rtx);

  if (seq0)
    emit_insn (seq0);
  if (seq1)
    emit_insn (seq1);

  /* Start CS loop.  */
  if (!is_weak)
    {
      /* Begin assuming success.  */
      emit_move_insn (btarget, const1_rtx);

      csloop = gen_label_rtx ();
      csend = gen_label_rtx ();
      emit_label (csloop);
    }

  /* val = "<mem>00..0<mem>"
   * cmp = "00..0<cmp>00..0"
   * new = "00..0<new>00..0"
   */

  emit_insn (seq2);
  emit_insn (seq3);

  cc = s390_emit_compare_and_swap (EQ, res, ac.memsi, cmpv, newv, CCZ1mode);
  if (is_weak)
    emit_insn (gen_cstorecc4 (btarget, cc, XEXP (cc, 0), XEXP (cc, 1)));
  else
    {
      rtx tmp;

      /* Jump to end if we're done (likely?).  */
      s390_emit_jump (csend, cc);

      /* Check for changes outside mode, and loop internal if so.
	 Arrange the moves so that the compare is adjacent to the
	 branch so that we can generate CRJ.  */
      tmp = copy_to_reg (val);
      force_expand_binop (SImode, and_optab, res, ac.modemaski, val,
			  1, OPTAB_DIRECT);
      cc = s390_emit_compare (NE, val, tmp);
      s390_emit_jump (csloop, cc);

      /* Failed.  */
      emit_move_insn (btarget, const0_rtx);
      emit_label (csend);
    }

  /* Return the correct part of the bitfield.  */
  convert_move (vtarget, expand_simple_binop (SImode, LSHIFTRT, res, ac.shift,
					      NULL_RTX, 1, OPTAB_DIRECT), 1);
}

/* Variant of s390_expand_cs for SI, DI and TI modes.  */
static void
s390_expand_cs_tdsi (machine_mode mode, rtx btarget, rtx vtarget, rtx mem,
		     rtx cmp, rtx new_rtx, bool is_weak)
{
  rtx output = vtarget;
  rtx_code_label *skip_cs_label = NULL;
  bool do_const_opt = false;

  if (!register_operand (output, mode))
    output = gen_reg_rtx (mode);

  /* If IS_WEAK is true and the INPUT value is a constant, compare the memory
     with the constant first and skip the compare_and_swap because its very
     expensive and likely to fail anyway.
     Note 1: This is done only for IS_WEAK.  C11 allows optimizations that may
     cause spurious in that case.
     Note 2: It may be useful to do this also for non-constant INPUT.
     Note 3: Currently only targets with "load on condition" are supported
     (z196 and newer).  */

  if (TARGET_Z196
      && (mode == SImode || mode == DImode))
    do_const_opt = (is_weak && CONST_INT_P (cmp));

  if (do_const_opt)
    {
      rtx cc = gen_rtx_REG (CCZmode, CC_REGNUM);

      skip_cs_label = gen_label_rtx ();
      emit_move_insn (btarget, const0_rtx);
      if (CONST_INT_P (cmp) && INTVAL (cmp) == 0)
	{
	  rtvec lt = rtvec_alloc (2);

	  /* Load-and-test + conditional jump.  */
	  RTVEC_ELT (lt, 0)
	    = gen_rtx_SET (cc, gen_rtx_COMPARE (CCZmode, mem, cmp));
	  RTVEC_ELT (lt, 1) = gen_rtx_SET (output, mem);
	  emit_insn (gen_rtx_PARALLEL (VOIDmode, lt));
	}
      else
	{
	  emit_move_insn (output, mem);
	  emit_insn (gen_rtx_SET (cc, gen_rtx_COMPARE (CCZmode, output, cmp)));
	}
      s390_emit_jump (skip_cs_label, gen_rtx_NE (VOIDmode, cc, const0_rtx));
      add_reg_br_prob_note (get_last_insn (),
			    profile_probability::very_unlikely ());
      /* If the jump is not taken, OUTPUT is the expected value.  */
      cmp = output;
      /* Reload newval to a register manually, *after* the compare and jump
	 above.  Otherwise Reload might place it before the jump.  */
    }
  else
    cmp = force_reg (mode, cmp);
  new_rtx = force_reg (mode, new_rtx);
  s390_emit_compare_and_swap (EQ, output, mem, cmp, new_rtx,
			      (do_const_opt) ? CCZmode : CCZ1mode);
  if (skip_cs_label != NULL)
    emit_label (skip_cs_label);

  /* We deliberately accept non-register operands in the predicate
     to ensure the write back to the output operand happens *before*
     the store-flags code below.  This makes it easier for combine
     to merge the store-flags code with a potential test-and-branch
     pattern following (immediately!) afterwards.  */
  if (output != vtarget)
    emit_move_insn (vtarget, output);

  if (do_const_opt)
    {
      rtx cc, cond, ite;

      /* Do not use gen_cstorecc4 here because it writes either 1 or 0, but
	 btarget has already been initialized with 0 above.  */
      cc = gen_rtx_REG (CCZmode, CC_REGNUM);
      cond = gen_rtx_EQ (VOIDmode, cc, const0_rtx);
      ite = gen_rtx_IF_THEN_ELSE (SImode, cond, const1_rtx, btarget);
      emit_insn (gen_rtx_SET (btarget, ite));
    }
  else
    {
      rtx cc, cond;

      cc = gen_rtx_REG (CCZ1mode, CC_REGNUM);
      cond = gen_rtx_EQ (SImode, cc, const0_rtx);
      emit_insn (gen_cstorecc4 (btarget, cond, cc, const0_rtx));
    }
}

/* Expand an atomic compare and swap operation.  MEM is the memory location,
   CMP the old value to compare MEM with and NEW_RTX the value to set if
   CMP == MEM.  */

void
s390_expand_cs (machine_mode mode, rtx btarget, rtx vtarget, rtx mem,
		rtx cmp, rtx new_rtx, bool is_weak)
{
  switch (mode)
    {
    case E_TImode:
    case E_DImode:
    case E_SImode:
      s390_expand_cs_tdsi (mode, btarget, vtarget, mem, cmp, new_rtx, is_weak);
      break;
    case E_HImode:
    case E_QImode:
      s390_expand_cs_hqi (mode, btarget, vtarget, mem, cmp, new_rtx, is_weak);
      break;
    default:
      gcc_unreachable ();
    }
}

/* Expand an atomic_exchange operation simulated with a compare-and-swap loop.
   The memory location MEM is set to INPUT.  OUTPUT is set to the previous value
   of MEM.  */

void
s390_expand_atomic_exchange_tdsi (rtx output, rtx mem, rtx input)
{
  machine_mode mode = GET_MODE (mem);
  rtx_code_label *csloop;

  if (TARGET_Z196
      && (mode == DImode || mode == SImode)
      && CONST_INT_P (input) && INTVAL (input) == 0)
    {
      emit_move_insn (output, const0_rtx);
      if (mode == DImode)
	emit_insn (gen_atomic_fetch_anddi (output, mem, const0_rtx, input));
      else
	emit_insn (gen_atomic_fetch_andsi (output, mem, const0_rtx, input));
      return;
    }

  input = force_reg (mode, input);
  emit_move_insn (output, mem);
  csloop = gen_label_rtx ();
  emit_label (csloop);
  s390_emit_jump (csloop, s390_emit_compare_and_swap (NE, output, mem, output,
						      input, CCZ1mode));
}

/* Expand an atomic operation CODE of mode MODE.  MEM is the memory location
   and VAL the value to play with.  If AFTER is true then store the value
   MEM holds after the operation, if AFTER is false then store the value MEM
   holds before the operation.  If TARGET is zero then discard that value, else
   store it to TARGET.  */

void
s390_expand_atomic (machine_mode mode, enum rtx_code code,
		    rtx target, rtx mem, rtx val, bool after)
{
  struct alignment_context ac;
  rtx cmp;
  rtx new_rtx = gen_reg_rtx (SImode);
  rtx orig = gen_reg_rtx (SImode);
  rtx_code_label *csloop = gen_label_rtx ();

  gcc_assert (!target || register_operand (target, VOIDmode));
  gcc_assert (MEM_P (mem));

  init_alignment_context (&ac, mem, mode);

  /* Shift val to the correct bit positions.
     Preserve "icm", but prevent "ex icm".  */
  if (!(ac.aligned && code == SET && MEM_P (val)))
    val = s390_expand_mask_and_shift (val, mode, ac.shift);

  /* Further preparation insns.  */
  if (code == PLUS || code == MINUS)
    emit_move_insn (orig, val);
  else if (code == MULT || code == AND) /* val = "11..1<val>11..1" */
    val = expand_simple_binop (SImode, XOR, val, ac.modemaski,
			       NULL_RTX, 1, OPTAB_DIRECT);

  /* Load full word.  Subsequent loads are performed by CS.  */
  cmp = force_reg (SImode, ac.memsi);

  /* Start CS loop.  */
  emit_label (csloop);
  emit_move_insn (new_rtx, cmp);

  /* Patch new with val at correct position.  */
  switch (code)
    {
    case PLUS:
    case MINUS:
      val = expand_simple_binop (SImode, code, new_rtx, orig,
				 NULL_RTX, 1, OPTAB_DIRECT);
      val = expand_simple_binop (SImode, AND, val, ac.modemask,
				 NULL_RTX, 1, OPTAB_DIRECT);
      /* FALLTHRU */
    case SET:
      if (ac.aligned && MEM_P (val))
	store_bit_field (new_rtx, GET_MODE_BITSIZE (mode), 0,
			 0, 0, SImode, val, false, false);
      else
	{
	  new_rtx = expand_simple_binop (SImode, AND, new_rtx, ac.modemaski,
				     NULL_RTX, 1, OPTAB_DIRECT);
	  new_rtx = expand_simple_binop (SImode, IOR, new_rtx, val,
				     NULL_RTX, 1, OPTAB_DIRECT);
	}
      break;
    case AND:
    case IOR:
    case XOR:
      new_rtx = expand_simple_binop (SImode, code, new_rtx, val,
				 NULL_RTX, 1, OPTAB_DIRECT);
      break;
    case MULT: /* NAND */
      new_rtx = expand_simple_binop (SImode, AND, new_rtx, val,
				 NULL_RTX, 1, OPTAB_DIRECT);
      new_rtx = expand_simple_binop (SImode, XOR, new_rtx, ac.modemask,
				 NULL_RTX, 1, OPTAB_DIRECT);
      break;
    default:
      gcc_unreachable ();
    }

  s390_emit_jump (csloop, s390_emit_compare_and_swap (NE, cmp,
						      ac.memsi, cmp, new_rtx,
						      CCZ1mode));

  /* Return the correct part of the bitfield.  */
  if (target)
    convert_move (target, expand_simple_binop (SImode, LSHIFTRT,
					       after ? new_rtx : cmp, ac.shift,
					       NULL_RTX, 1, OPTAB_DIRECT), 1);
}

/* This is called from dwarf2out.cc via TARGET_ASM_OUTPUT_DWARF_DTPREL.
   We need to emit DTP-relative relocations.  */

static void s390_output_dwarf_dtprel (FILE *, int, rtx) ATTRIBUTE_UNUSED;

static void
s390_output_dwarf_dtprel (FILE *file, int size, rtx x)
{
  switch (size)
    {
    case 4:
      fputs ("\t.long\t", file);
      break;
    case 8:
      fputs ("\t.quad\t", file);
      break;
    default:
      gcc_unreachable ();
    }
  output_addr_const (file, x);
  fputs ("@DTPOFF", file);
}

/* Return the proper mode for REGNO being represented in the dwarf
   unwind table.  */
machine_mode
s390_dwarf_frame_reg_mode (int regno)
{
  machine_mode save_mode = default_dwarf_frame_reg_mode (regno);

  /* Make sure not to return DImode for any GPR with -m31 -mzarch.  */
  if (GENERAL_REGNO_P (regno))
    save_mode = Pmode;

  /* The rightmost 64 bits of vector registers are call-clobbered.  */
  if (GET_MODE_SIZE (save_mode) > 8)
    save_mode = DImode;

  return save_mode;
}

#ifdef TARGET_ALTERNATE_LONG_DOUBLE_MANGLING
/* Implement TARGET_MANGLE_TYPE.  */

static const char *
s390_mangle_type (const_tree type)
{
  type = TYPE_MAIN_VARIANT (type);

  if (TREE_CODE (type) != VOID_TYPE && TREE_CODE (type) != BOOLEAN_TYPE
      && TREE_CODE (type) != INTEGER_TYPE && TREE_CODE (type) != REAL_TYPE)
    return NULL;

  if (type == s390_builtin_types[BT_BV16QI]) return "U6__boolc";
  if (type == s390_builtin_types[BT_BV8HI]) return "U6__bools";
  if (type == s390_builtin_types[BT_BV4SI]) return "U6__booli";
  if (type == s390_builtin_types[BT_BV2DI]) return "U6__booll";

  if (type == long_double_type_node && TARGET_LONG_DOUBLE_128)
    return "g";

  /* For all other types, use normal C++ mangling.  */
  return NULL;
}
#endif

/* In the name of slightly smaller debug output, and to cater to
   general assembler lossage, recognize various UNSPEC sequences
   and turn them back into a direct symbol reference.  */

static rtx
s390_delegitimize_address (rtx orig_x)
{
  rtx x, y;

  orig_x = delegitimize_mem_from_attrs (orig_x);
  x = orig_x;

  /* Extract the symbol ref from:
     (plus:SI (reg:SI 12 %r12)
	      (const:SI (unspec:SI [(symbol_ref/f:SI ("*.LC0"))]
				    UNSPEC_GOTOFF/PLTOFF)))
     and
     (plus:SI (reg:SI 12 %r12)
	      (const:SI (plus:SI (unspec:SI [(symbol_ref:SI ("L"))]
					     UNSPEC_GOTOFF/PLTOFF)
				 (const_int 4 [0x4]))))  */
  if (GET_CODE (x) == PLUS
      && REG_P (XEXP (x, 0))
      && REGNO (XEXP (x, 0)) == PIC_OFFSET_TABLE_REGNUM
      && GET_CODE (XEXP (x, 1)) == CONST)
    {
      HOST_WIDE_INT offset = 0;

      /* The const operand.  */
      y = XEXP (XEXP (x, 1), 0);

      if (GET_CODE (y) == PLUS
	  && GET_CODE (XEXP (y, 1)) == CONST_INT)
	{
	  offset = INTVAL (XEXP (y, 1));
	  y = XEXP (y, 0);
	}

      if (GET_CODE (y) == UNSPEC
	  && (XINT (y, 1) == UNSPEC_GOTOFF
	      || XINT (y, 1) == UNSPEC_PLTOFF))
	return plus_constant (Pmode, XVECEXP (y, 0, 0), offset);
    }

  if (GET_CODE (x) != MEM)
    return orig_x;

  x = XEXP (x, 0);
  if (GET_CODE (x) == PLUS
      && GET_CODE (XEXP (x, 1)) == CONST
      && GET_CODE (XEXP (x, 0)) == REG
      && REGNO (XEXP (x, 0)) == PIC_OFFSET_TABLE_REGNUM)
    {
      y = XEXP (XEXP (x, 1), 0);
      if (GET_CODE (y) == UNSPEC
	  && XINT (y, 1) == UNSPEC_GOT)
	y = XVECEXP (y, 0, 0);
      else
	return orig_x;
    }
  else if (GET_CODE (x) == CONST)
    {
      /* Extract the symbol ref from:
	 (mem:QI (const:DI (unspec:DI [(symbol_ref:DI ("foo"))]
				       UNSPEC_PLT/GOTENT)))  */

      y = XEXP (x, 0);
      if (GET_CODE (y) == UNSPEC
	  && (XINT (y, 1) == UNSPEC_GOTENT
	      || XINT (y, 1) == UNSPEC_PLT31))
	y = XVECEXP (y, 0, 0);
      else
	return orig_x;
    }
  else
    return orig_x;

  if (GET_MODE (orig_x) != Pmode)
    {
      if (GET_MODE (orig_x) == BLKmode)
	return orig_x;
      y = lowpart_subreg (GET_MODE (orig_x), y, Pmode);
      if (y == NULL_RTX)
	return orig_x;
    }
  return y;
}

/* Output operand OP to stdio stream FILE.
   OP is an address (register + offset) which is not used to address data;
   instead the rightmost bits are interpreted as the value.  */

static void
print_addrstyle_operand (FILE *file, rtx op)
{
  HOST_WIDE_INT offset;
  rtx base;

  /* Extract base register and offset.  */
  if (!s390_decompose_addrstyle_without_index (op, &base, &offset))
    gcc_unreachable ();

  /* Sanity check.  */
  if (base)
    {
      gcc_assert (GET_CODE (base) == REG);
      gcc_assert (REGNO (base) < FIRST_PSEUDO_REGISTER);
      gcc_assert (REGNO_REG_CLASS (REGNO (base)) == ADDR_REGS);
    }

  /* Offsets are constricted to twelve bits.  */
  fprintf (file, HOST_WIDE_INT_PRINT_DEC, offset & ((1 << 12) - 1));
  if (base)
    fprintf (file, "(%s)", reg_names[REGNO (base)]);
}

/* Print the shift count operand OP to FILE.
   OP is an address-style operand in a form which
   s390_valid_shift_count permits.  Subregs and no-op
   and-masking of the operand are stripped.  */

static void
print_shift_count_operand (FILE *file, rtx op)
{
  /* No checking of the and mask required here.  */
  if (!s390_valid_shift_count (op, 0))
    gcc_unreachable ();

  while (op && GET_CODE (op) == SUBREG)
    op = SUBREG_REG (op);

  if (GET_CODE (op) == AND)
    op = XEXP (op, 0);

  print_addrstyle_operand (file, op);
}

/* Assigns the number of NOP halfwords to be emitted before and after the
   function label to *HW_BEFORE and *HW_AFTER.  Both pointers must not be NULL.
   If hotpatching is disabled for the function, the values are set to zero.
*/

static void
s390_function_num_hotpatch_hw (tree decl,
			       int *hw_before,
			       int *hw_after)
{
  tree attr;

  attr = lookup_attribute ("hotpatch", DECL_ATTRIBUTES (decl));

  /* Handle the arguments of the hotpatch attribute.  The values
     specified via attribute might override the cmdline argument
     values.  */
  if (attr)
    {
      tree args = TREE_VALUE (attr);

      *hw_before = TREE_INT_CST_LOW (TREE_VALUE (args));
      *hw_after = TREE_INT_CST_LOW (TREE_VALUE (TREE_CHAIN (args)));
    }
  else
    {
      /* Use the values specified by the cmdline arguments.  */
      *hw_before = s390_hotpatch_hw_before_label;
      *hw_after = s390_hotpatch_hw_after_label;
    }
}

/* Write the current .machine and .machinemode specification to the assembler
   file.  */

#ifdef HAVE_AS_MACHINE_MACHINEMODE
static void
s390_asm_output_machine_for_arch (FILE *asm_out_file)
{
  fprintf (asm_out_file, "\t.machinemode %s\n",
	   (TARGET_ZARCH) ? "zarch" : "esa");
  fprintf (asm_out_file, "\t.machine \"%s",
	   processor_table[s390_arch].binutils_name);
  if (S390_USE_ARCHITECTURE_MODIFIERS)
    {
      int cpu_flags;

      cpu_flags = processor_flags_table[(int) s390_arch];
      if (TARGET_HTM && !(cpu_flags & PF_TX))
	fprintf (asm_out_file, "+htm");
      else if (!TARGET_HTM && (cpu_flags & PF_TX))
	fprintf (asm_out_file, "+nohtm");
      if (TARGET_VX && !(cpu_flags & PF_VX))
	fprintf (asm_out_file, "+vx");
      else if (!TARGET_VX && (cpu_flags & PF_VX))
	fprintf (asm_out_file, "+novx");
    }
  fprintf (asm_out_file, "\"\n");
}

/* Write an extra function header before the very start of the function.  */

void
s390_asm_output_function_prefix (FILE *asm_out_file,
				 const char *fnname ATTRIBUTE_UNUSED)
{
  if (DECL_FUNCTION_SPECIFIC_TARGET (current_function_decl) == NULL)
    return;
  /* Since only the function specific options are saved but not the indications
     which options are set, it's too much work here to figure out which options
     have actually changed.  Thus, generate .machine and .machinemode whenever a
     function has the target attribute or pragma.  */
  fprintf (asm_out_file, "\t.machinemode push\n");
  fprintf (asm_out_file, "\t.machine push\n");
  s390_asm_output_machine_for_arch (asm_out_file);
}

/* Write an extra function footer after the very end of the function.  */

void
s390_asm_declare_function_size (FILE *asm_out_file,
				const char *fnname, tree decl)
{
  if (!flag_inhibit_size_directive)
    ASM_OUTPUT_MEASURED_SIZE (asm_out_file, fnname);
  if (DECL_FUNCTION_SPECIFIC_TARGET (decl) == NULL)
    return;
  fprintf (asm_out_file, "\t.machine pop\n");
  fprintf (asm_out_file, "\t.machinemode pop\n");
}
#endif

/* Write the extra assembler code needed to declare a function properly.  */

void
s390_asm_output_function_label (FILE *out_file, const char *fname,
				tree decl)
{
  int hw_before, hw_after;

  s390_function_num_hotpatch_hw (decl, &hw_before, &hw_after);
  if (hw_before > 0)
    {
      unsigned int function_alignment;
      int i;

      /* Add a trampoline code area before the function label and initialize it
	 with two-byte nop instructions.  This area can be overwritten with code
	 that jumps to a patched version of the function.  */
      asm_fprintf (out_file, "\tnopr\t%%r0"
		   "\t# pre-label NOPs for hotpatch (%d halfwords)\n",
		   hw_before);
      for (i = 1; i < hw_before; i++)
	fputs ("\tnopr\t%r0\n", out_file);

      /* Note:  The function label must be aligned so that (a) the bytes of the
	 following nop do not cross a cacheline boundary, and (b) a jump address
	 (eight bytes for 64 bit targets, 4 bytes for 32 bit targets) can be
	 stored directly before the label without crossing a cacheline
	 boundary.  All this is necessary to make sure the trampoline code can
	 be changed atomically.
	 This alignment is done automatically using the FOUNCTION_BOUNDARY, but
	 if there are NOPs before the function label, the alignment is placed
	 before them.  So it is necessary to duplicate the alignment after the
	 NOPs.  */
      function_alignment = MAX (8, DECL_ALIGN (decl) / BITS_PER_UNIT);
      if (! DECL_USER_ALIGN (decl))
	function_alignment
	  = MAX (function_alignment,
		 (unsigned int) align_functions.levels[0].get_value ());
      fputs ("\t# alignment for hotpatch\n", out_file);
      ASM_OUTPUT_ALIGN (out_file, align_functions.levels[0].log);
    }

  if (S390_USE_TARGET_ATTRIBUTE && TARGET_DEBUG_ARG)
    {
      asm_fprintf (out_file, "\t# fn:%s ar%d\n", fname, s390_arch);
      asm_fprintf (out_file, "\t# fn:%s tu%d\n", fname, s390_tune);
      asm_fprintf (out_file, "\t# fn:%s sg%d\n", fname, s390_stack_guard);
      asm_fprintf (out_file, "\t# fn:%s ss%d\n", fname, s390_stack_size);
      asm_fprintf (out_file, "\t# fn:%s bc%d\n", fname, s390_branch_cost);
      asm_fprintf (out_file, "\t# fn:%s wf%d\n", fname,
		   s390_warn_framesize);
      asm_fprintf (out_file, "\t# fn:%s ba%d\n", fname, TARGET_BACKCHAIN);
      asm_fprintf (out_file, "\t# fn:%s hd%d\n", fname, TARGET_HARD_DFP);
      asm_fprintf (out_file, "\t# fn:%s hf%d\n", fname, !TARGET_SOFT_FLOAT);
      asm_fprintf (out_file, "\t# fn:%s ht%d\n", fname, TARGET_OPT_HTM);
      asm_fprintf (out_file, "\t# fn:%s vx%d\n", fname, TARGET_OPT_VX);
      asm_fprintf (out_file, "\t# fn:%s ps%d\n", fname,
		   TARGET_PACKED_STACK);
      asm_fprintf (out_file, "\t# fn:%s se%d\n", fname, TARGET_SMALL_EXEC);
      asm_fprintf (out_file, "\t# fn:%s mv%d\n", fname, TARGET_MVCLE);
      asm_fprintf (out_file, "\t# fn:%s zv%d\n", fname, TARGET_ZVECTOR);
      asm_fprintf (out_file, "\t# fn:%s wd%d\n", fname,
		   s390_warn_dynamicstack_p);
    }
  ASM_OUTPUT_LABEL (out_file, fname);
  if (hw_after > 0)
    asm_fprintf (out_file,
		 "\t# post-label NOPs for hotpatch (%d halfwords)\n",
		 hw_after);
}

/* Output machine-dependent UNSPECs occurring in address constant X
   in assembler syntax to stdio stream FILE.  Returns true if the
   constant X could be recognized, false otherwise.  */

static bool
s390_output_addr_const_extra (FILE *file, rtx x)
{
  if (GET_CODE (x) == UNSPEC && XVECLEN (x, 0) == 1)
    switch (XINT (x, 1))
      {
      case UNSPEC_GOTENT:
	output_addr_const (file, XVECEXP (x, 0, 0));
	fprintf (file, "@GOTENT");
	return true;
      case UNSPEC_GOT:
	output_addr_const (file, XVECEXP (x, 0, 0));
	fprintf (file, "@GOT");
	return true;
      case UNSPEC_GOTOFF:
	output_addr_const (file, XVECEXP (x, 0, 0));
	fprintf (file, "@GOTOFF");
	return true;
      case UNSPEC_PLT31:
	output_addr_const (file, XVECEXP (x, 0, 0));
	fprintf (file, "@PLT");
	return true;
      case UNSPEC_PLTOFF:
	output_addr_const (file, XVECEXP (x, 0, 0));
	fprintf (file, "@PLTOFF");
	return true;
      case UNSPEC_TLSGD:
	output_addr_const (file, XVECEXP (x, 0, 0));
	fprintf (file, "@TLSGD");
	return true;
      case UNSPEC_TLSLDM:
	assemble_name (file, get_some_local_dynamic_name ());
	fprintf (file, "@TLSLDM");
	return true;
      case UNSPEC_DTPOFF:
	output_addr_const (file, XVECEXP (x, 0, 0));
	fprintf (file, "@DTPOFF");
	return true;
      case UNSPEC_NTPOFF:
	output_addr_const (file, XVECEXP (x, 0, 0));
	fprintf (file, "@NTPOFF");
	return true;
      case UNSPEC_GOTNTPOFF:
	output_addr_const (file, XVECEXP (x, 0, 0));
	fprintf (file, "@GOTNTPOFF");
	return true;
      case UNSPEC_INDNTPOFF:
	output_addr_const (file, XVECEXP (x, 0, 0));
	fprintf (file, "@INDNTPOFF");
	return true;
      }

  if (GET_CODE (x) == UNSPEC && XVECLEN (x, 0) == 2)
    switch (XINT (x, 1))
      {
      case UNSPEC_POOL_OFFSET:
	x = gen_rtx_MINUS (GET_MODE (x), XVECEXP (x, 0, 0), XVECEXP (x, 0, 1));
	output_addr_const (file, x);
	return true;
      }
  return false;
}

/* Output address operand ADDR in assembler syntax to
   stdio stream FILE.  */

void
print_operand_address (FILE *file, rtx addr)
{
  struct s390_address ad;
  memset (&ad, 0, sizeof (s390_address));

  if (s390_loadrelative_operand_p (addr, NULL, NULL))
    {
      if (!TARGET_Z10)
	{
	  output_operand_lossage ("symbolic memory references are "
				  "only supported on z10 or later");
	  return;
	}
      output_addr_const (file, addr);
      return;
    }

  if (!s390_decompose_address (addr, &ad)
      || (ad.base && !REGNO_OK_FOR_BASE_P (REGNO (ad.base)))
      || (ad.indx && !REGNO_OK_FOR_INDEX_P (REGNO (ad.indx))))
    output_operand_lossage ("cannot decompose address");

  if (ad.disp)
    output_addr_const (file, ad.disp);
  else
    fprintf (file, "0");

  if (ad.base && ad.indx)
    fprintf (file, "(%s,%s)", reg_names[REGNO (ad.indx)],
			      reg_names[REGNO (ad.base)]);
  else if (ad.base)
    fprintf (file, "(%s)", reg_names[REGNO (ad.base)]);
}

/* Output operand X in assembler syntax to stdio stream FILE.
   CODE specified the format flag.  The following format flags
   are recognized:

    'A': On z14 or higher: If operand is a mem print the alignment
	 hint usable with vl/vst prefixed by a comma.
    'C': print opcode suffix for branch condition.
    'D': print opcode suffix for inverse branch condition.
    'E': print opcode suffix for branch on index instruction.
    'G': print the size of the operand in bytes.
    'J': print tls_load/tls_gdcall/tls_ldcall suffix
    'K': print @PLT suffix for call targets and load address values.
    'M': print the second word of a TImode operand.
    'N': print the second word of a DImode operand.
    'O': print only the displacement of a memory reference or address.
    'R': print only the base register of a memory reference or address.
    'S': print S-type memory reference (base+displacement).
    'Y': print address style operand without index (e.g. shift count or setmem
	 operand).

    'b': print integer X as if it's an unsigned byte.
    'c': print integer X as if it's an signed byte.
    'e': "end" contiguous bitmask X in either DImode or vector inner mode.
    'f': "end" contiguous bitmask X in SImode.
    'h': print integer X as if it's a signed halfword.
    'i': print the first nonzero HImode part of X.
    'j': print the first HImode part unequal to -1 of X.
    'k': print the first nonzero SImode part of X.
    'm': print the first SImode part unequal to -1 of X.
    'o': print integer X as if it's an unsigned 32bit word.
    's': "start" of contiguous bitmask X in either DImode or vector inner mode.
    't': CONST_INT: "start" of contiguous bitmask X in SImode.
	 CONST_VECTOR: Generate a bitmask for vgbm instruction.
    'x': print integer X as if it's an unsigned halfword.
    'v': print register number as vector register (v1 instead of f1).
    'V': print the second word of a TFmode operand as vector register.
*/

void
print_operand (FILE *file, rtx x, int code)
{
  HOST_WIDE_INT ival;

  switch (code)
    {
    case 'A':
      if (TARGET_VECTOR_LOADSTORE_ALIGNMENT_HINTS && MEM_P (x))
	{
	  if (MEM_ALIGN (x) >= 128)
	    fprintf (file, ",4");
	  else if (MEM_ALIGN (x) == 64)
	    fprintf (file, ",3");
	}
      return;
    case 'C':
      fprintf (file, s390_branch_condition_mnemonic (x, FALSE));
      return;

    case 'D':
      fprintf (file, s390_branch_condition_mnemonic (x, TRUE));
      return;

    case 'E':
      if (GET_CODE (x) == LE)
	fprintf (file, "l");
      else if (GET_CODE (x) == GT)
	fprintf (file, "h");
      else
	output_operand_lossage ("invalid comparison operator "
				"for 'E' output modifier");
      return;

    case 'J':
      if (GET_CODE (x) == SYMBOL_REF)
	{
	  fprintf (file, "%s", ":tls_load:");
	  output_addr_const (file, x);
	}
      else if (GET_CODE (x) == UNSPEC && XINT (x, 1) == UNSPEC_TLSGD)
	{
	  fprintf (file, "%s", ":tls_gdcall:");
	  output_addr_const (file, XVECEXP (x, 0, 0));
	}
      else if (GET_CODE (x) == UNSPEC && XINT (x, 1) == UNSPEC_TLSLDM)
	{
	  fprintf (file, "%s", ":tls_ldcall:");
	  const char *name = get_some_local_dynamic_name ();
	  gcc_assert (name);
	  assemble_name (file, name);
	}
      else
	output_operand_lossage ("invalid reference for 'J' output modifier");
      return;

    case 'G':
      fprintf (file, "%u", GET_MODE_SIZE (GET_MODE (x)));
      return;

    case 'O':
      {
	struct s390_address ad;
	int ret;

	ret = s390_decompose_address (MEM_P (x) ? XEXP (x, 0) : x, &ad);

	if (!ret
	    || (ad.base && !REGNO_OK_FOR_BASE_P (REGNO (ad.base)))
	    || ad.indx)
	  {
	    output_operand_lossage ("invalid address for 'O' output modifier");
	    return;
	  }

	if (ad.disp)
	  output_addr_const (file, ad.disp);
	else
	  fprintf (file, "0");
      }
      return;

    case 'R':
      {
	struct s390_address ad;
	int ret;

	ret = s390_decompose_address (MEM_P (x) ? XEXP (x, 0) : x, &ad);

	if (!ret
	    || (ad.base && !REGNO_OK_FOR_BASE_P (REGNO (ad.base)))
	    || ad.indx)
	  {
	    output_operand_lossage ("invalid address for 'R' output modifier");
	    return;
	  }

	if (ad.base)
	  fprintf (file, "%s", reg_names[REGNO (ad.base)]);
	else
	  fprintf (file, "0");
      }
      return;

    case 'S':
      {
	struct s390_address ad;
	int ret;

	if (!MEM_P (x))
	  {
	    output_operand_lossage ("memory reference expected for "
				    "'S' output modifier");
	    return;
	  }
	ret = s390_decompose_address (XEXP (x, 0), &ad);

	if (!ret
	    || (ad.base && !REGNO_OK_FOR_BASE_P (REGNO (ad.base)))
	    || ad.indx)
	  {
	    output_operand_lossage ("invalid address for 'S' output modifier");
	    return;
	  }

	if (ad.disp)
	  output_addr_const (file, ad.disp);
	else
	  fprintf (file, "0");

	if (ad.base)
	  fprintf (file, "(%s)", reg_names[REGNO (ad.base)]);
      }
      return;

    case 'N':
      if (GET_CODE (x) == REG)
	x = gen_rtx_REG (GET_MODE (x), REGNO (x) + 1);
      else if (GET_CODE (x) == MEM)
	x = change_address (x, VOIDmode,
			    plus_constant (Pmode, XEXP (x, 0), 4));
      else
	output_operand_lossage ("register or memory expression expected "
				"for 'N' output modifier");
      break;

    case 'M':
      if (GET_CODE (x) == REG)
	x = gen_rtx_REG (GET_MODE (x), REGNO (x) + 1);
      else if (GET_CODE (x) == MEM)
	x = change_address (x, VOIDmode,
			    plus_constant (Pmode, XEXP (x, 0), 8));
      else
	output_operand_lossage ("register or memory expression expected "
				"for 'M' output modifier");
      break;

    case 'Y':
      print_shift_count_operand (file, x);
      return;

    case 'K':
      /* Append @PLT to both local and non-local symbols in order to support
	 Linux Kernel livepatching: patches contain individual functions and
	 are loaded further than 2G away from vmlinux, and therefore they must
	 call even static functions via PLT.  ld will optimize @PLT away for
	 normal code, and keep it for patches.

	 Do not indiscriminately add @PLT in 31-bit mode due to the %r12
	 restriction, use UNSPEC_PLT31 instead.

	 @PLT only makes sense for functions, data is taken care of by
	 -mno-pic-data-is-text-relative.

	 Adding @PLT interferes with handling of weak symbols in non-PIC code,
	 since their addresses are loaded with larl, which then always produces
	 a non-NULL result, so skip them here as well.  */
      if (TARGET_64BIT
	  && GET_CODE (x) == SYMBOL_REF
	  && SYMBOL_REF_FUNCTION_P (x)
	  && !(SYMBOL_REF_WEAK (x) && !flag_pic))
	fprintf (file, "@PLT");
      return;
    }

  switch (GET_CODE (x))
    {
    case REG:
      /* Print FP regs as fx instead of vx when they are accessed
	 through non-vector mode.  */
      if ((code == 'v' || code == 'V')
	  || VECTOR_NOFP_REG_P (x)
	  || (FP_REG_P (x) && VECTOR_MODE_P (GET_MODE (x)))
	  || (VECTOR_REG_P (x)
	      && (GET_MODE_SIZE (GET_MODE (x)) /
		  s390_class_max_nregs (FP_REGS, GET_MODE (x))) > 8))
	fprintf (file, "%%v%s", reg_names[REGNO (x) + (code == 'V')] + 2);
      else
	fprintf (file, "%s", reg_names[REGNO (x)]);
      break;

    case MEM:
      output_address (GET_MODE (x), XEXP (x, 0));
      break;

    case CONST:
    case CODE_LABEL:
    case LABEL_REF:
    case SYMBOL_REF:
      output_addr_const (file, x);
      break;

    case CONST_INT:
      ival = INTVAL (x);
      switch (code)
	{
	case 0:
	  break;
	case 'b':
	  ival &= 0xff;
	  break;
	case 'c':
	  ival = ((ival & 0xff) ^ 0x80) - 0x80;
	  break;
	case 'x':
	  ival &= 0xffff;
	  break;
	case 'h':
	  ival = ((ival & 0xffff) ^ 0x8000) - 0x8000;
	  break;
	case 'i':
	  ival = s390_extract_part (x, HImode, 0);
	  break;
	case 'j':
	  ival = s390_extract_part (x, HImode, -1);
	  break;
	case 'k':
	  ival = s390_extract_part (x, SImode, 0);
	  break;
	case 'm':
	  ival = s390_extract_part (x, SImode, -1);
	  break;
	case 'o':
	  ival &= 0xffffffff;
	  break;
	case 'e': case 'f':
	case 's': case 't':
	  {
	    int start, end;
	    int len;
	    bool ok;

	    len = (code == 's' || code == 'e' ? 64 : 32);
	    ok = s390_contiguous_bitmask_p (ival, true, len, &start, &end);
	    gcc_assert (ok);
	    if (code == 's' || code == 't')
	      ival = start;
	    else
	      ival = end;
	  }
	  break;
	default:
	  output_operand_lossage ("invalid constant for output modifier '%c'", code);
	}
      fprintf (file, HOST_WIDE_INT_PRINT_DEC, ival);
      break;

    case CONST_WIDE_INT:
      if (code == 'b')
	fprintf (file, HOST_WIDE_INT_PRINT_DEC,
		 CONST_WIDE_INT_ELT (x, 0) & 0xff);
      else if (code == 'x')
	fprintf (file, HOST_WIDE_INT_PRINT_DEC,
		 CONST_WIDE_INT_ELT (x, 0) & 0xffff);
      else if (code == 'h')
	fprintf (file, HOST_WIDE_INT_PRINT_DEC,
		 ((CONST_WIDE_INT_ELT (x, 0) & 0xffff) ^ 0x8000) - 0x8000);
      else
	{
	  if (code == 0)
	    output_operand_lossage ("invalid constant - try using "
				    "an output modifier");
	  else
	    output_operand_lossage ("invalid constant for output modifier '%c'",
				    code);
	}
      break;
    case CONST_VECTOR:
      switch (code)
	{
	case 'h':
	  gcc_assert (const_vec_duplicate_p (x));
	  fprintf (file, HOST_WIDE_INT_PRINT_DEC,
		   ((INTVAL (XVECEXP (x, 0, 0)) & 0xffff) ^ 0x8000) - 0x8000);
	  break;
	case 'e':
	case 's':
	  {
	    int start, end;
	    bool ok;

	    ok = s390_contiguous_bitmask_vector_p (x, &start, &end);
	    gcc_assert (ok);
	    ival = (code == 's') ? start : end;
	    fprintf (file, HOST_WIDE_INT_PRINT_DEC, ival);
	  }
	  break;
	case 't':
	  {
	    unsigned mask;
	    bool ok = s390_bytemask_vector_p (x, &mask);
	    gcc_assert (ok);
	    fprintf (file, "%u", mask);
	  }
	  break;

	default:
	  output_operand_lossage ("invalid constant vector for output "
				  "modifier '%c'", code);
	}
      break;

    default:
      if (code == 0)
	output_operand_lossage ("invalid expression - try using "
				"an output modifier");
      else
	output_operand_lossage ("invalid expression for output "
				"modifier '%c'", code);
      break;
    }
}

/* Target hook for assembling integer objects.  We need to define it
   here to work a round a bug in some versions of GAS, which couldn't
   handle values smaller than INT_MIN when printed in decimal.  */

static bool
s390_assemble_integer (rtx x, unsigned int size, int aligned_p)
{
  if (size == 8 && aligned_p
      && GET_CODE (x) == CONST_INT && INTVAL (x) < INT_MIN)
    {
      fprintf (asm_out_file, "\t.quad\t" HOST_WIDE_INT_PRINT_HEX "\n",
	       INTVAL (x));
      return true;
    }
  return default_assemble_integer (x, size, aligned_p);
}

/* Returns true if register REGNO is used  for forming
   a memory address in expression X.  */

static bool
reg_used_in_mem_p (int regno, rtx x)
{
  enum rtx_code code = GET_CODE (x);
  int i, j;
  const char *fmt;

  if (code == MEM)
    {
      if (refers_to_regno_p (regno, XEXP (x, 0)))
	return true;
    }
  else if (code == SET
	   && GET_CODE (SET_DEST (x)) == PC)
    {
      if (refers_to_regno_p (regno, SET_SRC (x)))
	return true;
    }

  fmt = GET_RTX_FORMAT (code);
  for (i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
    {
      if (fmt[i] == 'e'
	  && reg_used_in_mem_p (regno, XEXP (x, i)))
	return true;

      else if (fmt[i] == 'E')
	for (j = 0; j < XVECLEN (x, i); j++)
	  if (reg_used_in_mem_p (regno, XVECEXP (x, i, j)))
	    return true;
    }
  return false;
}

/* Returns true if expression DEP_RTX sets an address register
   used by instruction INSN to address memory.  */

static bool
addr_generation_dependency_p (rtx dep_rtx, rtx_insn *insn)
{
  rtx target, pat;

  if (NONJUMP_INSN_P (dep_rtx))
    dep_rtx = PATTERN (dep_rtx);

  if (GET_CODE (dep_rtx) == SET)
    {
      target = SET_DEST (dep_rtx);
      if (GET_CODE (target) == STRICT_LOW_PART)
	target = XEXP (target, 0);
      while (GET_CODE (target) == SUBREG)
	target = SUBREG_REG (target);

      if (GET_CODE (target) == REG)
	{
	  int regno = REGNO (target);

	  if (s390_safe_attr_type (insn) == TYPE_LA)
	    {
	      pat = PATTERN (insn);
	      if (GET_CODE (pat) == PARALLEL)
		{
		  gcc_assert (XVECLEN (pat, 0) == 2);
		  pat = XVECEXP (pat, 0, 0);
		}
	      gcc_assert (GET_CODE (pat) == SET);
	      return refers_to_regno_p (regno, SET_SRC (pat));
	    }
	  else if (get_attr_atype (insn) == ATYPE_AGEN)
	    return reg_used_in_mem_p (regno, PATTERN (insn));
	}
    }
  return false;
}

/* Return 1, if dep_insn sets register used in insn in the agen unit.  */

int
s390_agen_dep_p (rtx_insn *dep_insn, rtx_insn *insn)
{
  rtx dep_rtx = PATTERN (dep_insn);
  int i;

  if (GET_CODE (dep_rtx) == SET
      && addr_generation_dependency_p (dep_rtx, insn))
    return 1;
  else if (GET_CODE (dep_rtx) == PARALLEL)
    {
      for (i = 0; i < XVECLEN (dep_rtx, 0); i++)
	{
	  if (addr_generation_dependency_p (XVECEXP (dep_rtx, 0, i), insn))
	    return 1;
	}
    }
  return 0;
}


/* A C statement (sans semicolon) to update the integer scheduling priority
   INSN_PRIORITY (INSN).  Increase the priority to execute the INSN earlier,
   reduce the priority to execute INSN later.  Do not define this macro if
   you do not need to adjust the scheduling priorities of insns.

   A STD instruction should be scheduled earlier,
   in order to use the bypass.  */
static int
s390_adjust_priority (rtx_insn *insn, int priority)
{
  if (! INSN_P (insn))
    return priority;

  if (s390_tune <= PROCESSOR_2064_Z900)
    return priority;

  switch (s390_safe_attr_type (insn))
    {
      case TYPE_FSTOREDF:
      case TYPE_FSTORESF:
	priority = priority << 3;
	break;
      case TYPE_STORE:
      case TYPE_STM:
	priority = priority << 1;
	break;
      default:
	break;
    }
  return priority;
}


/* The number of instructions that can be issued per cycle.  */

static int
s390_issue_rate (void)
{
  switch (s390_tune)
    {
    case PROCESSOR_2084_Z990:
    case PROCESSOR_2094_Z9_109:
    case PROCESSOR_2094_Z9_EC:
    case PROCESSOR_2817_Z196:
      return 3;
    case PROCESSOR_2097_Z10:
      return 2;
    case PROCESSOR_2064_Z900:
      /* Starting with EC12 we use the sched_reorder hook to take care
	 of instruction dispatch constraints.  The algorithm only
	 picks the best instruction and assumes only a single
	 instruction gets issued per cycle.  */
    case PROCESSOR_2827_ZEC12:
    case PROCESSOR_2964_Z13:
    case PROCESSOR_3906_Z14:
    case PROCESSOR_8561_Z15:
    case PROCESSOR_3931_Z16:
    default:
      return 1;
    }
}

static int
s390_first_cycle_multipass_dfa_lookahead (void)
{
  return 4;
}

static void
annotate_constant_pool_refs_1 (rtx *x)
{
  int i, j;
  const char *fmt;

  gcc_assert (GET_CODE (*x) != SYMBOL_REF
	      || !CONSTANT_POOL_ADDRESS_P (*x));

  /* Literal pool references can only occur inside a MEM ...  */
  if (GET_CODE (*x) == MEM)
    {
      rtx memref = XEXP (*x, 0);

      if (GET_CODE (memref) == SYMBOL_REF
	  && CONSTANT_POOL_ADDRESS_P (memref))
	{
	  rtx base = cfun->machine->base_reg;
	  rtx addr = gen_rtx_UNSPEC (Pmode, gen_rtvec (2, memref, base),
				     UNSPEC_LTREF);

	  *x = replace_equiv_address (*x, addr);
	  return;
	}

      if (GET_CODE (memref) == CONST
	  && GET_CODE (XEXP (memref, 0)) == PLUS
	  && GET_CODE (XEXP (XEXP (memref, 0), 1)) == CONST_INT
	  && GET_CODE (XEXP (XEXP (memref, 0), 0)) == SYMBOL_REF
	  && CONSTANT_POOL_ADDRESS_P (XEXP (XEXP (memref, 0), 0)))
	{
	  HOST_WIDE_INT off = INTVAL (XEXP (XEXP (memref, 0), 1));
	  rtx sym = XEXP (XEXP (memref, 0), 0);
	  rtx base = cfun->machine->base_reg;
	  rtx addr = gen_rtx_UNSPEC (Pmode, gen_rtvec (2, sym, base),
				     UNSPEC_LTREF);

	  *x = replace_equiv_address (*x, plus_constant (Pmode, addr, off));
	  return;
	}
    }

  /* ... or a load-address type pattern.  */
  if (GET_CODE (*x) == SET)
    {
      rtx addrref = SET_SRC (*x);

      if (GET_CODE (addrref) == SYMBOL_REF
	  && CONSTANT_POOL_ADDRESS_P (addrref))
	{
	  rtx base = cfun->machine->base_reg;
	  rtx addr = gen_rtx_UNSPEC (Pmode, gen_rtvec (2, addrref, base),
				     UNSPEC_LTREF);

	  SET_SRC (*x) = addr;
	  return;
	}

      if (GET_CODE (addrref) == CONST
	  && GET_CODE (XEXP (addrref, 0)) == PLUS
	  && GET_CODE (XEXP (XEXP (addrref, 0), 1)) == CONST_INT
	  && GET_CODE (XEXP (XEXP (addrref, 0), 0)) == SYMBOL_REF
	  && CONSTANT_POOL_ADDRESS_P (XEXP (XEXP (addrref, 0), 0)))
	{
	  HOST_WIDE_INT off = INTVAL (XEXP (XEXP (addrref, 0), 1));
	  rtx sym = XEXP (XEXP (addrref, 0), 0);
	  rtx base = cfun->machine->base_reg;
	  rtx addr = gen_rtx_UNSPEC (Pmode, gen_rtvec (2, sym, base),
				     UNSPEC_LTREF);

	  SET_SRC (*x) = plus_constant (Pmode, addr, off);
	  return;
	}
    }

  fmt = GET_RTX_FORMAT (GET_CODE (*x));
  for (i = GET_RTX_LENGTH (GET_CODE (*x)) - 1; i >= 0; i--)
    {
      if (fmt[i] == 'e')
	{
	  annotate_constant_pool_refs_1 (&XEXP (*x, i));
	}
      else if (fmt[i] == 'E')
	{
	  for (j = 0; j < XVECLEN (*x, i); j++)
	    annotate_constant_pool_refs_1 (&XVECEXP (*x, i, j));
	}
    }
}

/* Annotate every literal pool reference in INSN by an UNSPEC_LTREF expression.
   Fix up MEMs as required.
   Skip insns which support relative addressing, because they do not use a base
   register.  */

static void
annotate_constant_pool_refs (rtx_insn *insn)
{
  if (s390_safe_relative_long_p (insn))
    return;
  annotate_constant_pool_refs_1 (&PATTERN (insn));
}

static void
find_constant_pool_ref_1 (rtx x, rtx *ref)
{
  int i, j;
  const char *fmt;

  /* Likewise POOL_ENTRY insns.  */
  if (GET_CODE (x) == UNSPEC_VOLATILE
      && XINT (x, 1) == UNSPECV_POOL_ENTRY)
    return;

  gcc_assert (GET_CODE (x) != SYMBOL_REF
	      || !CONSTANT_POOL_ADDRESS_P (x));

  if (GET_CODE (x) == UNSPEC && XINT (x, 1) == UNSPEC_LTREF)
    {
      rtx sym = XVECEXP (x, 0, 0);
      gcc_assert (GET_CODE (sym) == SYMBOL_REF
		  && CONSTANT_POOL_ADDRESS_P (sym));

      if (*ref == NULL_RTX)
	*ref = sym;
      else
	gcc_assert (*ref == sym);

      return;
    }

  fmt = GET_RTX_FORMAT (GET_CODE (x));
  for (i = GET_RTX_LENGTH (GET_CODE (x)) - 1; i >= 0; i--)
    {
      if (fmt[i] == 'e')
	{
	  find_constant_pool_ref_1 (XEXP (x, i), ref);
	}
      else if (fmt[i] == 'E')
	{
	  for (j = 0; j < XVECLEN (x, i); j++)
	    find_constant_pool_ref_1 (XVECEXP (x, i, j), ref);
	}
    }
}

/* Find an annotated literal pool symbol referenced in INSN,
   and store it at REF.  Will abort if INSN contains references to
   more than one such pool symbol; multiple references to the same
   symbol are allowed, however.

   The rtx pointed to by REF must be initialized to NULL_RTX
   by the caller before calling this routine.

   Skip insns which support relative addressing, because they do not use a base
   register.  */

static void
find_constant_pool_ref (rtx_insn *insn, rtx *ref)
{
  if (s390_safe_relative_long_p (insn))
    return;
  find_constant_pool_ref_1 (PATTERN (insn), ref);
}

static void
replace_constant_pool_ref_1 (rtx *x, rtx ref, rtx offset)
{
  int i, j;
  const char *fmt;

  gcc_assert (*x != ref);

  if (GET_CODE (*x) == UNSPEC
      && XINT (*x, 1) == UNSPEC_LTREF
      && XVECEXP (*x, 0, 0) == ref)
    {
      *x = gen_rtx_PLUS (Pmode, XVECEXP (*x, 0, 1), offset);
      return;
    }

  if (GET_CODE (*x) == PLUS
      && GET_CODE (XEXP (*x, 1)) == CONST_INT
      && GET_CODE (XEXP (*x, 0)) == UNSPEC
      && XINT (XEXP (*x, 0), 1) == UNSPEC_LTREF
      && XVECEXP (XEXP (*x, 0), 0, 0) == ref)
    {
      rtx addr = gen_rtx_PLUS (Pmode, XVECEXP (XEXP (*x, 0), 0, 1), offset);
      *x = plus_constant (Pmode, addr, INTVAL (XEXP (*x, 1)));
      return;
    }

  fmt = GET_RTX_FORMAT (GET_CODE (*x));
  for (i = GET_RTX_LENGTH (GET_CODE (*x)) - 1; i >= 0; i--)
    {
      if (fmt[i] == 'e')
	{
	  replace_constant_pool_ref_1 (&XEXP (*x, i), ref, offset);
	}
      else if (fmt[i] == 'E')
	{
	  for (j = 0; j < XVECLEN (*x, i); j++)
	    replace_constant_pool_ref_1 (&XVECEXP (*x, i, j), ref, offset);
	}
    }
}

/* Replace every reference to the annotated literal pool
   symbol REF in INSN by its base plus OFFSET.
   Skip insns which support relative addressing, because they do not use a base
   register.  */

static void
replace_constant_pool_ref (rtx_insn *insn, rtx ref, rtx offset)
{
  if (s390_safe_relative_long_p (insn))
    return;
  replace_constant_pool_ref_1 (&PATTERN (insn), ref, offset);
}

/* We keep a list of constants which we have to add to internal
   constant tables in the middle of large functions.  */

static machine_mode constant_modes[] =
{
  TFmode, FPRX2mode, TImode, TDmode,
  V16QImode, V8HImode, V4SImode, V2DImode, V1TImode,
  V4SFmode, V2DFmode, V1TFmode,
  DFmode, DImode, DDmode,
  V8QImode, V4HImode, V2SImode, V1DImode, V2SFmode, V1DFmode,
  SFmode, SImode, SDmode,
  V4QImode, V2HImode, V1SImode,  V1SFmode,
  HImode,
  V2QImode, V1HImode,
  QImode,
  V1QImode
};
#define NR_C_MODES (ARRAY_SIZE (constant_modes))

struct constant
{
  struct constant *next;
  rtx value;
  rtx_code_label *label;
};

struct constant_pool
{
  struct constant_pool *next;
  rtx_insn *first_insn;
  rtx_insn *pool_insn;
  bitmap insns;
  rtx_insn *emit_pool_after;

  struct constant *constants[NR_C_MODES];
  struct constant *execute;
  rtx_code_label *label;
  int size;
};

/* Allocate new constant_pool structure.  */

static struct constant_pool *
s390_alloc_pool (void)
{
  struct constant_pool *pool;
  size_t i;

  pool = (struct constant_pool *) xmalloc (sizeof *pool);
  pool->next = NULL;
  for (i = 0; i < NR_C_MODES; i++)
    pool->constants[i] = NULL;

  pool->execute = NULL;
  pool->label = gen_label_rtx ();
  pool->first_insn = NULL;
  pool->pool_insn = NULL;
  pool->insns = BITMAP_ALLOC (NULL);
  pool->size = 0;
  pool->emit_pool_after = NULL;

  return pool;
}

/* Create new constant pool covering instructions starting at INSN
   and chain it to the end of POOL_LIST.  */

static struct constant_pool *
s390_start_pool (struct constant_pool **pool_list, rtx_insn *insn)
{
  struct constant_pool *pool, **prev;

  pool = s390_alloc_pool ();
  pool->first_insn = insn;

  for (prev = pool_list; *prev; prev = &(*prev)->next)
    ;
  *prev = pool;

  return pool;
}

/* End range of instructions covered by POOL at INSN and emit
   placeholder insn representing the pool.  */

static void
s390_end_pool (struct constant_pool *pool, rtx_insn *insn)
{
  rtx pool_size = GEN_INT (pool->size + 8 /* alignment slop */);

  if (!insn)
    insn = get_last_insn ();

  pool->pool_insn = emit_insn_after (gen_pool (pool_size), insn);
  INSN_ADDRESSES_NEW (pool->pool_insn, -1);
}

/* Add INSN to the list of insns covered by POOL.  */

static void
s390_add_pool_insn (struct constant_pool *pool, rtx insn)
{
  bitmap_set_bit (pool->insns, INSN_UID (insn));
}

/* Return pool out of POOL_LIST that covers INSN.  */

static struct constant_pool *
s390_find_pool (struct constant_pool *pool_list, rtx insn)
{
  struct constant_pool *pool;

  for (pool = pool_list; pool; pool = pool->next)
    if (bitmap_bit_p (pool->insns, INSN_UID (insn)))
      break;

  return pool;
}

/* Add constant VAL of mode MODE to the constant pool POOL.  */

static void
s390_add_constant (struct constant_pool *pool, rtx val, machine_mode mode)
{
  struct constant *c;
  size_t i;

  for (i = 0; i < NR_C_MODES; i++)
    if (constant_modes[i] == mode)
      break;
  gcc_assert (i != NR_C_MODES);

  for (c = pool->constants[i]; c != NULL; c = c->next)
    if (rtx_equal_p (val, c->value))
      break;

  if (c == NULL)
    {
      c = (struct constant *) xmalloc (sizeof *c);
      c->value = val;
      c->label = gen_label_rtx ();
      c->next = pool->constants[i];
      pool->constants[i] = c;
      pool->size += GET_MODE_SIZE (mode);
    }
}

/* Return an rtx that represents the offset of X from the start of
   pool POOL.  */

static rtx
s390_pool_offset (struct constant_pool *pool, rtx x)
{
  rtx label;

  label = gen_rtx_LABEL_REF (GET_MODE (x), pool->label);
  x = gen_rtx_UNSPEC (GET_MODE (x), gen_rtvec (2, x, label),
		      UNSPEC_POOL_OFFSET);
  return gen_rtx_CONST (GET_MODE (x), x);
}

/* Find constant VAL of mode MODE in the constant pool POOL.
   Return an RTX describing the distance from the start of
   the pool to the location of the new constant.  */

static rtx
s390_find_constant (struct constant_pool *pool, rtx val,
		    machine_mode mode)
{
  struct constant *c;
  size_t i;

  for (i = 0; i < NR_C_MODES; i++)
    if (constant_modes[i] == mode)
      break;
  gcc_assert (i != NR_C_MODES);

  for (c = pool->constants[i]; c != NULL; c = c->next)
    if (rtx_equal_p (val, c->value))
      break;

  gcc_assert (c);

  return s390_pool_offset (pool, gen_rtx_LABEL_REF (Pmode, c->label));
}

/* Check whether INSN is an execute.  Return the label_ref to its
   execute target template if so, NULL_RTX otherwise.  */

static rtx
s390_execute_label (rtx insn)
{
  if (INSN_P (insn)
      && GET_CODE (PATTERN (insn)) == PARALLEL
      && GET_CODE (XVECEXP (PATTERN (insn), 0, 0)) == UNSPEC
      && (XINT (XVECEXP (PATTERN (insn), 0, 0), 1) == UNSPEC_EXECUTE
	  || XINT (XVECEXP (PATTERN (insn), 0, 0), 1) == UNSPEC_EXECUTE_JUMP))
    {
      if (XINT (XVECEXP (PATTERN (insn), 0, 0), 1) == UNSPEC_EXECUTE)
	return XVECEXP (XVECEXP (PATTERN (insn), 0, 0), 0, 2);
      else
	{
	  gcc_assert (JUMP_P (insn));
	  /* For jump insns as execute target:
	     - There is one operand less in the parallel (the
	       modification register of the execute is always 0).
	     - The execute target label is wrapped into an
	       if_then_else in order to hide it from jump analysis.  */
	  return XEXP (XVECEXP (XVECEXP (PATTERN (insn), 0, 0), 0, 0), 0);
	}
    }

  return NULL_RTX;
}

/* Find execute target for INSN in the constant pool POOL.
   Return an RTX describing the distance from the start of
   the pool to the location of the execute target.  */

static rtx
s390_find_execute (struct constant_pool *pool, rtx insn)
{
  struct constant *c;

  for (c = pool->execute; c != NULL; c = c->next)
    if (INSN_UID (insn) == INSN_UID (c->value))
      break;

  gcc_assert (c);

  return s390_pool_offset (pool, gen_rtx_LABEL_REF (Pmode, c->label));
}

/* For an execute INSN, extract the execute target template.  */

static rtx
s390_execute_target (rtx insn)
{
  rtx pattern = PATTERN (insn);
  gcc_assert (s390_execute_label (insn));

  if (XVECLEN (pattern, 0) == 2)
    {
      pattern = copy_rtx (XVECEXP (pattern, 0, 1));
    }
  else
    {
      rtvec vec = rtvec_alloc (XVECLEN (pattern, 0) - 1);
      int i;

      for (i = 0; i < XVECLEN (pattern, 0) - 1; i++)
	RTVEC_ELT (vec, i) = copy_rtx (XVECEXP (pattern, 0, i + 1));

      pattern = gen_rtx_PARALLEL (VOIDmode, vec);
    }

  return pattern;
}

/* Indicate that INSN cannot be duplicated.  This is the case for
   execute insns that carry a unique label.  */

static bool
s390_cannot_copy_insn_p (rtx_insn *insn)
{
  rtx label = s390_execute_label (insn);
  return label && label != const0_rtx;
}

/* Dump out the constants in POOL.  If REMOTE_LABEL is true,
   do not emit the pool base label.  */

static void
s390_dump_pool (struct constant_pool *pool, bool remote_label)
{
  struct constant *c;
  rtx_insn *insn = pool->pool_insn;
  size_t i;

  /* Switch to rodata section.  */
  insn = emit_insn_after (gen_pool_section_start (), insn);
  INSN_ADDRESSES_NEW (insn, -1);

  /* Ensure minimum pool alignment.  */
  insn = emit_insn_after (gen_pool_align (GEN_INT (8)), insn);
  INSN_ADDRESSES_NEW (insn, -1);

  /* Emit pool base label.  */
  if (!remote_label)
    {
      insn = emit_label_after (pool->label, insn);
      INSN_ADDRESSES_NEW (insn, -1);
    }

  /* Dump constants in descending alignment requirement order,
     ensuring proper alignment for every constant.  */
  for (i = 0; i < NR_C_MODES; i++)
    for (c = pool->constants[i]; c; c = c->next)
      {
	/* Convert UNSPEC_LTREL_OFFSET unspecs to pool-relative references.  */
	rtx value = copy_rtx (c->value);
	if (GET_CODE (value) == CONST
	    && GET_CODE (XEXP (value, 0)) == UNSPEC
	    && XINT (XEXP (value, 0), 1) == UNSPEC_LTREL_OFFSET
	    && XVECLEN (XEXP (value, 0), 0) == 1)
	  value = s390_pool_offset (pool, XVECEXP (XEXP (value, 0), 0, 0));

	insn = emit_label_after (c->label, insn);
	INSN_ADDRESSES_NEW (insn, -1);

	value = gen_rtx_UNSPEC_VOLATILE (constant_modes[i],
					 gen_rtvec (1, value),
					 UNSPECV_POOL_ENTRY);
	insn = emit_insn_after (value, insn);
	INSN_ADDRESSES_NEW (insn, -1);
      }

  /* Ensure minimum alignment for instructions.  */
  insn = emit_insn_after (gen_pool_align (GEN_INT (2)), insn);
  INSN_ADDRESSES_NEW (insn, -1);

  /* Output in-pool execute template insns.  */
  for (c = pool->execute; c; c = c->next)
    {
      insn = emit_label_after (c->label, insn);
      INSN_ADDRESSES_NEW (insn, -1);

      insn = emit_insn_after (s390_execute_target (c->value), insn);
      INSN_ADDRESSES_NEW (insn, -1);
    }

  /* Switch back to previous section.  */
  insn = emit_insn_after (gen_pool_section_end (), insn);
  INSN_ADDRESSES_NEW (insn, -1);

  insn = emit_barrier_after (insn);
  INSN_ADDRESSES_NEW (insn, -1);

  /* Remove placeholder insn.  */
  remove_insn (pool->pool_insn);
}

/* Free all memory used by POOL.  */

static void
s390_free_pool (struct constant_pool *pool)
{
  struct constant *c, *next;
  size_t i;

  for (i = 0; i < NR_C_MODES; i++)
    for (c = pool->constants[i]; c; c = next)
      {
	next = c->next;
	free (c);
      }

  for (c = pool->execute; c; c = next)
    {
      next = c->next;
      free (c);
    }

  BITMAP_FREE (pool->insns);
  free (pool);
}


/* Collect main literal pool.  Return NULL on overflow.  */

static struct constant_pool *
s390_mainpool_start (void)
{
  struct constant_pool *pool;
  rtx_insn *insn;

  pool = s390_alloc_pool ();

  for (insn = get_insns (); insn; insn = NEXT_INSN (insn))
    {
      if (NONJUMP_INSN_P (insn)
	  && GET_CODE (PATTERN (insn)) == SET
	  && GET_CODE (SET_SRC (PATTERN (insn))) == UNSPEC_VOLATILE
	  && XINT (SET_SRC (PATTERN (insn)), 1) == UNSPECV_MAIN_POOL)
	{
	  /* There might be two main_pool instructions if base_reg
	     is call-clobbered; one for shrink-wrapped code and one
	     for the rest.  We want to keep the first.  */
	  if (pool->pool_insn)
	    {
	      insn = PREV_INSN (insn);
	      delete_insn (NEXT_INSN (insn));
	      continue;
	    }
	  pool->pool_insn = insn;
	}

      if (NONJUMP_INSN_P (insn) || CALL_P (insn))
	{
	  rtx pool_ref = NULL_RTX;
	  find_constant_pool_ref (insn, &pool_ref);
	  if (pool_ref)
	    {
	      rtx constant = get_pool_constant (pool_ref);
	      machine_mode mode = get_pool_mode (pool_ref);
	      s390_add_constant (pool, constant, mode);
	    }
	}

      /* If hot/cold partitioning is enabled we have to make sure that
	 the literal pool is emitted in the same section where the
	 initialization of the literal pool base pointer takes place.
	 emit_pool_after is only used in the non-overflow case on non
	 Z cpus where we can emit the literal pool at the end of the
	 function body within the text section.  */
      if (NOTE_P (insn)
	  && NOTE_KIND (insn) == NOTE_INSN_SWITCH_TEXT_SECTIONS
	  && !pool->emit_pool_after)
	pool->emit_pool_after = PREV_INSN (insn);
    }

  gcc_assert (pool->pool_insn || pool->size == 0);

  if (pool->size >= 4096)
    {
      /* We're going to chunkify the pool, so remove the main
	 pool placeholder insn.  */
      remove_insn (pool->pool_insn);

      s390_free_pool (pool);
      pool = NULL;
    }

  /* If the functions ends with the section where the literal pool
     should be emitted set the marker to its end.  */
  if (pool && !pool->emit_pool_after)
    pool->emit_pool_after = get_last_insn ();

  return pool;
}

/* POOL holds the main literal pool as collected by s390_mainpool_start.
   Modify the current function to output the pool constants as well as
   the pool register setup instruction.  */

static void
s390_mainpool_finish (struct constant_pool *pool)
{
  rtx base_reg = cfun->machine->base_reg;
  rtx set;
  rtx_insn *insn;

  /* If the pool is empty, we're done.  */
  if (pool->size == 0)
    {
      /* We don't actually need a base register after all.  */
      cfun->machine->base_reg = NULL_RTX;

      if (pool->pool_insn)
	remove_insn (pool->pool_insn);
      s390_free_pool (pool);
      return;
    }

  /* We need correct insn addresses.  */
  shorten_branches (get_insns ());

  /* Use a LARL to load the pool register.  The pool is
     located in the .rodata section, so we emit it after the function.  */
  set = gen_main_base_64 (base_reg, pool->label);
  insn = emit_insn_after (set, pool->pool_insn);
  INSN_ADDRESSES_NEW (insn, -1);
  remove_insn (pool->pool_insn);

  insn = get_last_insn ();
  pool->pool_insn = emit_insn_after (gen_pool (const0_rtx), insn);
  INSN_ADDRESSES_NEW (pool->pool_insn, -1);

  s390_dump_pool (pool, 0);

  /* Replace all literal pool references.  */

  for (rtx_insn *insn = get_insns (); insn; insn = NEXT_INSN (insn))
    {
      if (NONJUMP_INSN_P (insn) || CALL_P (insn))
	{
	  rtx addr, pool_ref = NULL_RTX;
	  find_constant_pool_ref (insn, &pool_ref);
	  if (pool_ref)
	    {
	      if (s390_execute_label (insn))
		addr = s390_find_execute (pool, insn);
	      else
		addr = s390_find_constant (pool, get_pool_constant (pool_ref),
						 get_pool_mode (pool_ref));

	      replace_constant_pool_ref (insn, pool_ref, addr);
	      INSN_CODE (insn) = -1;
	    }
	}
    }


  /* Free the pool.  */
  s390_free_pool (pool);
}

/* Chunkify the literal pool.  */

#define S390_POOL_CHUNK_MIN	0xc00
#define S390_POOL_CHUNK_MAX	0xe00

static struct constant_pool *
s390_chunkify_start (void)
{
  struct constant_pool *curr_pool = NULL, *pool_list = NULL;
  bitmap far_labels;
  rtx_insn *insn;

  /* We need correct insn addresses.  */

  shorten_branches (get_insns ());

  /* Scan all insns and move literals to pool chunks.  */

  for (insn = get_insns (); insn; insn = NEXT_INSN (insn))
    {
      if (NONJUMP_INSN_P (insn) || CALL_P (insn))
	{
	  rtx pool_ref = NULL_RTX;
	  find_constant_pool_ref (insn, &pool_ref);
	  if (pool_ref)
	    {
	      rtx constant = get_pool_constant (pool_ref);
	      machine_mode mode = get_pool_mode (pool_ref);

	      if (!curr_pool)
		curr_pool = s390_start_pool (&pool_list, insn);

	      s390_add_constant (curr_pool, constant, mode);
	      s390_add_pool_insn (curr_pool, insn);
	    }
	}

      if (JUMP_P (insn) || JUMP_TABLE_DATA_P (insn) || LABEL_P (insn))
	{
	  if (curr_pool)
	    s390_add_pool_insn (curr_pool, insn);
	}

      if (NOTE_P (insn) && NOTE_KIND (insn) == NOTE_INSN_VAR_LOCATION)
	continue;

      if (!curr_pool
	  || INSN_ADDRESSES_SIZE () <= (size_t) INSN_UID (insn)
	  || INSN_ADDRESSES (INSN_UID (insn)) == -1)
	continue;

      if (curr_pool->size < S390_POOL_CHUNK_MAX)
	continue;

      s390_end_pool (curr_pool, NULL);
      curr_pool = NULL;
    }

  if (curr_pool)
    s390_end_pool (curr_pool, NULL);

  /* Find all labels that are branched into
     from an insn belonging to a different chunk.  */

  far_labels = BITMAP_ALLOC (NULL);

  for (insn = get_insns (); insn; insn = NEXT_INSN (insn))
    {
      rtx_jump_table_data *table;

      /* Labels marked with LABEL_PRESERVE_P can be target
	 of non-local jumps, so we have to mark them.
	 The same holds for named labels.

	 Don't do that, however, if it is the label before
	 a jump table.  */

      if (LABEL_P (insn)
	  && (LABEL_PRESERVE_P (insn) || LABEL_NAME (insn)))
	{
	  rtx_insn *vec_insn = NEXT_INSN (insn);
	  if (! vec_insn || ! JUMP_TABLE_DATA_P (vec_insn))
	    bitmap_set_bit (far_labels, CODE_LABEL_NUMBER (insn));
	}
      /* Check potential targets in a table jump (casesi_jump).  */
      else if (tablejump_p (insn, NULL, &table))
	{
	  rtx vec_pat = PATTERN (table);
	  int i, diff_p = GET_CODE (vec_pat) == ADDR_DIFF_VEC;

	  for (i = 0; i < XVECLEN (vec_pat, diff_p); i++)
	    {
	      rtx label = XEXP (XVECEXP (vec_pat, diff_p, i), 0);

	      if (s390_find_pool (pool_list, label)
		  != s390_find_pool (pool_list, insn))
		bitmap_set_bit (far_labels, CODE_LABEL_NUMBER (label));
	    }
	}
      /* If we have a direct jump (conditional or unconditional),
	 check all potential targets.  */
      else if (JUMP_P (insn))
	{
	  rtx pat = PATTERN (insn);

	  if (GET_CODE (pat) == PARALLEL)
	    pat = XVECEXP (pat, 0, 0);

	  if (GET_CODE (pat) == SET)
	    {
	      rtx label = JUMP_LABEL (insn);
	      if (label && !ANY_RETURN_P (label))
		{
		  if (s390_find_pool (pool_list, label)
		      != s390_find_pool (pool_list, insn))
		    bitmap_set_bit (far_labels, CODE_LABEL_NUMBER (label));
		}
	    }
	}
    }

  /* Insert base register reload insns before every pool.  */

  for (curr_pool = pool_list; curr_pool; curr_pool = curr_pool->next)
    {
      rtx new_insn = gen_reload_base_64 (cfun->machine->base_reg,
					 curr_pool->label);
      rtx_insn *insn = curr_pool->first_insn;
      INSN_ADDRESSES_NEW (emit_insn_before (new_insn, insn), -1);
    }

  /* Insert base register reload insns at every far label.  */

  for (insn = get_insns (); insn; insn = NEXT_INSN (insn))
    if (LABEL_P (insn)
	&& bitmap_bit_p (far_labels, CODE_LABEL_NUMBER (insn)))
      {
	struct constant_pool *pool = s390_find_pool (pool_list, insn);
	if (pool)
	  {
	    rtx new_insn = gen_reload_base_64 (cfun->machine->base_reg,
					       pool->label);
	    INSN_ADDRESSES_NEW (emit_insn_after (new_insn, insn), -1);
	  }
      }


  BITMAP_FREE (far_labels);


  /* Recompute insn addresses.  */

  init_insn_lengths ();
  shorten_branches (get_insns ());

  return pool_list;
}

/* POOL_LIST is a chunk list as prepared by s390_chunkify_start.
   After we have decided to use this list, finish implementing
   all changes to the current function as required.  */

static void
s390_chunkify_finish (struct constant_pool *pool_list)
{
  struct constant_pool *curr_pool = NULL;
  rtx_insn *insn;


  /* Replace all literal pool references.  */

  for (insn = get_insns (); insn; insn = NEXT_INSN (insn))
    {
      curr_pool = s390_find_pool (pool_list, insn);
      if (!curr_pool)
	continue;

      if (NONJUMP_INSN_P (insn) || CALL_P (insn))
	{
	  rtx addr, pool_ref = NULL_RTX;
	  find_constant_pool_ref (insn, &pool_ref);
	  if (pool_ref)
	    {
	      if (s390_execute_label (insn))
		addr = s390_find_execute (curr_pool, insn);
	      else
		addr = s390_find_constant (curr_pool,
					   get_pool_constant (pool_ref),
					   get_pool_mode (pool_ref));

	      replace_constant_pool_ref (insn, pool_ref, addr);
	      INSN_CODE (insn) = -1;
	    }
	}
    }

  /* Dump out all literal pools.  */

  for (curr_pool = pool_list; curr_pool; curr_pool = curr_pool->next)
    s390_dump_pool (curr_pool, 0);

  /* Free pool list.  */

  while (pool_list)
    {
      struct constant_pool *next = pool_list->next;
      s390_free_pool (pool_list);
      pool_list = next;
    }
}

/* Output the constant pool entry EXP in mode MODE with alignment ALIGN.  */

void
s390_output_pool_entry (rtx exp, machine_mode mode, unsigned int align)
{
  switch (GET_MODE_CLASS (mode))
    {
    case MODE_FLOAT:
    case MODE_DECIMAL_FLOAT:
      gcc_assert (GET_CODE (exp) == CONST_DOUBLE);

      assemble_real (*CONST_DOUBLE_REAL_VALUE (exp),
		     as_a <scalar_float_mode> (mode), align);
      break;

    case MODE_INT:
      assemble_integer (exp, GET_MODE_SIZE (mode), align, 1);
      mark_symbol_refs_as_used (exp);
      break;

    case MODE_VECTOR_INT:
    case MODE_VECTOR_FLOAT:
      {
	int i;
	machine_mode inner_mode;
	gcc_assert (GET_CODE (exp) == CONST_VECTOR);

	inner_mode = GET_MODE_INNER (GET_MODE (exp));
	for (i = 0; i < XVECLEN (exp, 0); i++)
	  s390_output_pool_entry (XVECEXP (exp, 0, i),
				  inner_mode,
				  i == 0
				  ? align
				  : GET_MODE_BITSIZE (inner_mode));
      }
      break;

    default:
      gcc_unreachable ();
    }
}

/* Return true if MEM refers to an integer constant in the literal pool.  If
   VAL is not nullptr, then also fill it with the constant's value.  */

bool
s390_const_int_pool_entry_p (rtx mem, HOST_WIDE_INT *val)
{
  /* Try to match the following:
     - (mem (unspec [(symbol_ref) (reg)] UNSPEC_LTREF)).
     - (mem (symbol_ref)).  */

  if (!MEM_P (mem))
    return false;

  rtx addr = XEXP (mem, 0);
  rtx sym;
  if (GET_CODE (addr) == UNSPEC && XINT (addr, 1) == UNSPEC_LTREF)
    sym = XVECEXP (addr, 0, 0);
  else
    sym = addr;

  if (!SYMBOL_REF_P (sym) || !CONSTANT_POOL_ADDRESS_P (sym))
    return false;

  rtx val_rtx = get_pool_constant (sym);
  if (!CONST_INT_P (val_rtx))
    return false;

  if (val != nullptr)
    *val = INTVAL (val_rtx);
  return true;
}

/* Return an RTL expression representing the value of the return address
   for the frame COUNT steps up from the current frame.  FRAME is the
   frame pointer of that frame.  */

rtx
s390_return_addr_rtx (int count, rtx frame ATTRIBUTE_UNUSED)
{
  int offset;
  rtx addr;

  /* Without backchain, we fail for all but the current frame.  */

  if (!TARGET_BACKCHAIN && count > 0)
    return NULL_RTX;

  /* For the current frame, we need to make sure the initial
     value of RETURN_REGNUM is actually saved.  */

  if (count == 0)
    return get_hard_reg_initial_val (Pmode, RETURN_REGNUM);

  if (TARGET_PACKED_STACK)
    offset = -2 * UNITS_PER_LONG;
  else
    offset = RETURN_REGNUM * UNITS_PER_LONG;

  addr = plus_constant (Pmode, frame, offset);
  addr = memory_address (Pmode, addr);
  return gen_rtx_MEM (Pmode, addr);
}

/* Return an RTL expression representing the back chain stored in
   the current stack frame.  */

rtx
s390_back_chain_rtx (void)
{
  rtx chain;

  gcc_assert (TARGET_BACKCHAIN);

  if (TARGET_PACKED_STACK)
    chain = plus_constant (Pmode, stack_pointer_rtx,
			   STACK_POINTER_OFFSET - UNITS_PER_LONG);
  else
    chain = stack_pointer_rtx;

  chain = gen_rtx_MEM (Pmode, chain);
  return chain;
}

/* Find first call clobbered register unused in a function.
   This could be used as base register in a leaf function
   or for holding the return address before epilogue.  */

static int
find_unused_clobbered_reg (void)
{
  int i;
  for (i = 0; i < 6; i++)
    if (!df_regs_ever_live_p (i))
      return i;
  return 0;
}


/* Helper function for s390_regs_ever_clobbered.  Sets the fields in DATA for all
   clobbered hard regs in SETREG.  */

static void
s390_reg_clobbered_rtx (rtx setreg, const_rtx set_insn ATTRIBUTE_UNUSED, void *data)
{
  char *regs_ever_clobbered = (char *)data;
  unsigned int i, regno;
  machine_mode mode = GET_MODE (setreg);

  if (GET_CODE (setreg) == SUBREG)
    {
      rtx inner = SUBREG_REG (setreg);
      if (!GENERAL_REG_P (inner) && !FP_REG_P (inner))
	return;
      regno = subreg_regno (setreg);
    }
  else if (GENERAL_REG_P (setreg) || FP_REG_P (setreg))
    regno = REGNO (setreg);
  else
    return;

  for (i = regno;
       i < end_hard_regno (mode, regno);
       i++)
    regs_ever_clobbered[i] = 1;
}

/* Walks through all basic blocks of the current function looking
   for clobbered hard regs using s390_reg_clobbered_rtx.  The fields
   of the passed integer array REGS_EVER_CLOBBERED are set to one for
   each of those regs.  */

static void
s390_regs_ever_clobbered (char regs_ever_clobbered[])
{
  basic_block cur_bb;
  rtx_insn *cur_insn;
  unsigned int i;

  memset (regs_ever_clobbered, 0, 32);

  /* For non-leaf functions we have to consider all call clobbered regs to be
     clobbered.  */
  if (!crtl->is_leaf)
    {
      for (i = 0; i < 32; i++)
	regs_ever_clobbered[i] = call_used_regs[i];
    }

  /* Make the "magic" eh_return registers live if necessary.  For regs_ever_live
     this work is done by liveness analysis (mark_regs_live_at_end).
     Special care is needed for functions containing landing pads.  Landing pads
     may use the eh registers, but the code which sets these registers is not
     contained in that function.  Hence s390_regs_ever_clobbered is not able to
     deal with this automatically.  */
  if (crtl->calls_eh_return || cfun->machine->has_landing_pad_p)
    for (i = 0; EH_RETURN_DATA_REGNO (i) != INVALID_REGNUM ; i++)
      if (crtl->calls_eh_return
	  || (cfun->machine->has_landing_pad_p
	      && df_regs_ever_live_p (EH_RETURN_DATA_REGNO (i))))
	regs_ever_clobbered[EH_RETURN_DATA_REGNO (i)] = 1;

  /* For nonlocal gotos all call-saved registers have to be saved.
     This flag is also set for the unwinding code in libgcc.
     See expand_builtin_unwind_init.  For regs_ever_live this is done by
     reload.  */
  if (crtl->saves_all_registers)
    for (i = 0; i < 32; i++)
      if (!call_used_regs[i])
	regs_ever_clobbered[i] = 1;

  FOR_EACH_BB_FN (cur_bb, cfun)
    {
      FOR_BB_INSNS (cur_bb, cur_insn)
	{
	  rtx pat;

	  if (!INSN_P (cur_insn))
	    continue;

	  pat = PATTERN (cur_insn);

	  /* Ignore GPR restore insns.  */
	  if (epilogue_completed && RTX_FRAME_RELATED_P (cur_insn))
	    {
	      if (GET_CODE (pat) == SET
		  && GENERAL_REG_P (SET_DEST (pat)))
		{
		  /* lgdr  */
		  if (GET_MODE (SET_SRC (pat)) == DImode
		      && FP_REG_P (SET_SRC (pat)))
		    continue;

		  /* l / lg  */
		  if (GET_CODE (SET_SRC (pat)) == MEM)
		    continue;
		}

	      /* lm / lmg */
	      if (GET_CODE (pat) == PARALLEL
		  && load_multiple_operation (pat, VOIDmode))
		continue;
	    }

	  note_stores (cur_insn,
		       s390_reg_clobbered_rtx,
		       regs_ever_clobbered);
	}
    }
}

/* Determine the frame area which actually has to be accessed
   in the function epilogue. The values are stored at the
   given pointers AREA_BOTTOM (address of the lowest used stack
   address) and AREA_TOP (address of the first item which does
   not belong to the stack frame).  */

static void
s390_frame_area (int *area_bottom, int *area_top)
{
  int b, t;

  b = INT_MAX;
  t = INT_MIN;

  if (cfun_frame_layout.first_restore_gpr != -1)
    {
      b = (cfun_frame_layout.gprs_offset
	   + cfun_frame_layout.first_restore_gpr * UNITS_PER_LONG);
      t = b + (cfun_frame_layout.last_restore_gpr
	       - cfun_frame_layout.first_restore_gpr + 1) * UNITS_PER_LONG;
    }

  if (TARGET_64BIT && cfun_save_high_fprs_p)
    {
      b = MIN (b, cfun_frame_layout.f8_offset);
      t = MAX (t, (cfun_frame_layout.f8_offset
		   + cfun_frame_layout.high_fprs * 8));
    }

  if (!TARGET_64BIT)
    {
      if (cfun_fpr_save_p (FPR4_REGNUM))
	{
	  b = MIN (b, cfun_frame_layout.f4_offset);
	  t = MAX (t, cfun_frame_layout.f4_offset + 8);
	}
      if (cfun_fpr_save_p (FPR6_REGNUM))
	{
	  b = MIN (b, cfun_frame_layout.f4_offset + 8);
	  t = MAX (t, cfun_frame_layout.f4_offset + 16);
	}
    }
  *area_bottom = b;
  *area_top = t;
}
/* Update gpr_save_slots in the frame layout trying to make use of
   FPRs as GPR save slots.
   This is a helper routine of s390_register_info.  */

static void
s390_register_info_gprtofpr ()
{
  int save_reg_slot = FPR0_REGNUM;
  int i, j;

  if (TARGET_TPF || !TARGET_Z10 || !TARGET_HARD_FLOAT || !crtl->is_leaf)
    return;

  /* builtin_eh_return needs to be able to modify the return address
     on the stack.  It could also adjust the FPR save slot instead but
     is it worth the trouble?!  */
  if (crtl->calls_eh_return)
    return;

  for (i = 15; i >= 6; i--)
    {
      if (cfun_gpr_save_slot (i) == SAVE_SLOT_NONE)
	continue;

      /* Advance to the next FP register which can be used as a
	 GPR save slot.  */
      while ((!call_used_regs[save_reg_slot]
	      || df_regs_ever_live_p (save_reg_slot)
	      || cfun_fpr_save_p (save_reg_slot))
	     && FP_REGNO_P (save_reg_slot))
	save_reg_slot++;
      if (!FP_REGNO_P (save_reg_slot))
	{
	  /* We only want to use ldgr/lgdr if we can get rid of
	     stm/lm entirely.  So undo the gpr slot allocation in
	     case we ran out of FPR save slots.  */
	  for (j = 6; j <= 15; j++)
	    if (FP_REGNO_P (cfun_gpr_save_slot (j)))
	      cfun_gpr_save_slot (j) = SAVE_SLOT_STACK;
	  break;
	}
      cfun_gpr_save_slot (i) = save_reg_slot++;
    }
}

/* Set the bits in fpr_bitmap for FPRs which need to be saved due to
   stdarg.
   This is a helper routine for s390_register_info.  */

static void
s390_register_info_stdarg_fpr ()
{
  int i;
  int min_fpr;
  int max_fpr;

  /* Save the FP argument regs for stdarg. f0, f2 for 31 bit and
     f0-f4 for 64 bit.  */
  if (!cfun->stdarg
      || !TARGET_HARD_FLOAT
      || !cfun->va_list_fpr_size
      || crtl->args.info.fprs >= FP_ARG_NUM_REG)
    return;

  min_fpr = crtl->args.info.fprs;
  max_fpr = min_fpr + cfun->va_list_fpr_size - 1;
  if (max_fpr >= FP_ARG_NUM_REG)
    max_fpr = FP_ARG_NUM_REG - 1;

  /* FPR argument regs start at f0.  */
  min_fpr += FPR0_REGNUM;
  max_fpr += FPR0_REGNUM;

  for (i = min_fpr; i <= max_fpr; i++)
    cfun_set_fpr_save (i);
}

/* Reserve the GPR save slots for GPRs which need to be saved due to
   stdarg.
   This is a helper routine for s390_register_info.  */

static void
s390_register_info_stdarg_gpr ()
{
  int i;
  int min_gpr;
  int max_gpr;

  if (!cfun->stdarg
      || !cfun->va_list_gpr_size
      || crtl->args.info.gprs >= GP_ARG_NUM_REG)
    return;

  min_gpr = crtl->args.info.gprs;
  max_gpr = min_gpr + cfun->va_list_gpr_size - 1;
  if (max_gpr >= GP_ARG_NUM_REG)
    max_gpr = GP_ARG_NUM_REG - 1;

  /* GPR argument regs start at r2.  */
  min_gpr += GPR2_REGNUM;
  max_gpr += GPR2_REGNUM;

  /* If r6 was supposed to be saved into an FPR and now needs to go to
     the stack for vararg we have to adjust the restore range to make
     sure that the restore is done from stack as well.  */
  if (FP_REGNO_P (cfun_gpr_save_slot (GPR6_REGNUM))
      && min_gpr <= GPR6_REGNUM
      && max_gpr >= GPR6_REGNUM)
    {
      if (cfun_frame_layout.first_restore_gpr == -1
	  || cfun_frame_layout.first_restore_gpr > GPR6_REGNUM)
	cfun_frame_layout.first_restore_gpr = GPR6_REGNUM;
      if (cfun_frame_layout.last_restore_gpr == -1
	  || cfun_frame_layout.last_restore_gpr < GPR6_REGNUM)
	cfun_frame_layout.last_restore_gpr = GPR6_REGNUM;
    }

  if (cfun_frame_layout.first_save_gpr == -1
      || cfun_frame_layout.first_save_gpr > min_gpr)
    cfun_frame_layout.first_save_gpr = min_gpr;

  if (cfun_frame_layout.last_save_gpr == -1
      || cfun_frame_layout.last_save_gpr < max_gpr)
    cfun_frame_layout.last_save_gpr = max_gpr;

  for (i = min_gpr; i <= max_gpr; i++)
    cfun_gpr_save_slot (i) = SAVE_SLOT_STACK;
}

/* Calculate the save and restore ranges for stm(g) and lm(g) in the
   prologue and epilogue.  */

static void
s390_register_info_set_ranges ()
{
  int i, j;

  /* Find the first and the last save slot supposed to use the stack
     to set the restore range.
     Vararg regs might be marked as save to stack but only the
     call-saved regs really need restoring (i.e. r6).  This code
     assumes that the vararg regs have not yet been recorded in
     cfun_gpr_save_slot.  */
  for (i = 0; i < 16 && cfun_gpr_save_slot (i) != SAVE_SLOT_STACK; i++);
  for (j = 15; j > i && cfun_gpr_save_slot (j) != SAVE_SLOT_STACK; j--);
  cfun_frame_layout.first_restore_gpr = (i == 16) ? -1 : i;
  cfun_frame_layout.last_restore_gpr = (i == 16) ? -1 : j;
  cfun_frame_layout.first_save_gpr = (i == 16) ? -1 : i;
  cfun_frame_layout.last_save_gpr = (i == 16) ? -1 : j;
}

/* The GPR and FPR save slots in cfun->machine->frame_layout are set
   for registers which need to be saved in function prologue.
   This function can be used until the insns emitted for save/restore
   of the regs are visible in the RTL stream.  */

static void
s390_register_info ()
{
  int i;
  char clobbered_regs[32];

  gcc_assert (!epilogue_completed);

  if (reload_completed)
    /* After reload we rely on our own routine to determine which
       registers need saving.  */
    s390_regs_ever_clobbered (clobbered_regs);
  else
    /* During reload we use regs_ever_live as a base since reload
       does changes in there which we otherwise would not be aware
       of.  */
    for (i = 0; i < 32; i++)
      clobbered_regs[i] = df_regs_ever_live_p (i);

  for (i = 0; i < 32; i++)
    clobbered_regs[i] = clobbered_regs[i] && !global_regs[i];

  /* Mark the call-saved FPRs which need to be saved.
     This needs to be done before checking the special GPRs since the
     stack pointer usage depends on whether high FPRs have to be saved
     or not.  */
  cfun_frame_layout.fpr_bitmap = 0;
  cfun_frame_layout.high_fprs = 0;
  for (i = FPR0_REGNUM; i <= FPR15_REGNUM; i++)
    if (clobbered_regs[i] && !call_used_regs[i])
      {
	cfun_set_fpr_save (i);
	if (i >= FPR8_REGNUM)
	  cfun_frame_layout.high_fprs++;
      }

  /* Register 12 is used for GOT address, but also as temp in prologue
     for split-stack stdarg functions (unless r14 is available).  */
  clobbered_regs[12]
    |= ((flag_pic && df_regs_ever_live_p (PIC_OFFSET_TABLE_REGNUM))
	|| (flag_split_stack && cfun->stdarg
	    && (crtl->is_leaf || TARGET_TPF_PROFILING
		|| has_hard_reg_initial_val (Pmode, RETURN_REGNUM))));

  clobbered_regs[BASE_REGNUM]
    |= (cfun->machine->base_reg
	&& REGNO (cfun->machine->base_reg) == BASE_REGNUM);

  clobbered_regs[HARD_FRAME_POINTER_REGNUM]
    |= !!frame_pointer_needed;

  /* On pre z900 machines this might take until machine dependent
     reorg to decide.
     save_return_addr_p will only be set on non-zarch machines so
     there is no risk that r14 goes into an FPR instead of a stack
     slot.  */
  clobbered_regs[RETURN_REGNUM]
    |= (!crtl->is_leaf
	|| TARGET_TPF_PROFILING
	|| cfun_frame_layout.save_return_addr_p
	|| crtl->calls_eh_return);

  clobbered_regs[STACK_POINTER_REGNUM]
    |= (!crtl->is_leaf
	|| TARGET_TPF_PROFILING
	|| cfun_save_high_fprs_p
	|| get_frame_size () > 0
	|| (reload_completed && cfun_frame_layout.frame_size > 0)
	|| cfun->calls_alloca);

  memset (cfun_frame_layout.gpr_save_slots, SAVE_SLOT_NONE, 16);

  for (i = 6; i < 16; i++)
    if (clobbered_regs[i])
      cfun_gpr_save_slot (i) = SAVE_SLOT_STACK;

  s390_register_info_stdarg_fpr ();
  s390_register_info_gprtofpr ();
  s390_register_info_set_ranges ();
  /* stdarg functions might need to save GPRs 2 to 6.  This might
     override the GPR->FPR save decision made by
     s390_register_info_gprtofpr for r6 since vararg regs must go to
     the stack.  */
  s390_register_info_stdarg_gpr ();
}

/* Return true if REGNO is a global register, but not one
   of the special ones that need to be saved/restored in anyway.  */

static inline bool
global_not_special_regno_p (int regno)
{
  return (global_regs[regno]
	  /* These registers are special and need to be
	     restored in any case.  */
	  && !(regno == STACK_POINTER_REGNUM
	       || regno == RETURN_REGNUM
	       || regno == BASE_REGNUM
	       || (flag_pic && regno == (int)PIC_OFFSET_TABLE_REGNUM)));
}

/* This function is called by s390_optimize_prologue in order to get
   rid of unnecessary GPR save/restore instructions.  The register info
   for the GPRs is re-computed and the ranges are re-calculated.  */

static void
s390_optimize_register_info ()
{
  char clobbered_regs[32];
  int i;

  gcc_assert (epilogue_completed);

  s390_regs_ever_clobbered (clobbered_regs);

  /* Global registers do not need to be saved and restored unless it
     is one of our special regs.  (r12, r13, r14, or r15).  */
  for (i = 0; i < 32; i++)
    clobbered_regs[i] = clobbered_regs[i] && !global_not_special_regno_p (i);

  /* There is still special treatment needed for cases invisible to
     s390_regs_ever_clobbered.  */
  clobbered_regs[RETURN_REGNUM]
    |= (TARGET_TPF_PROFILING
	/* When expanding builtin_return_addr in ESA mode we do not
	   know whether r14 will later be needed as scratch reg when
	   doing branch splitting.  So the builtin always accesses the
	   r14 save slot and we need to stick to the save/restore
	   decision for r14 even if it turns out that it didn't get
	   clobbered.  */
	|| cfun_frame_layout.save_return_addr_p
	|| crtl->calls_eh_return);

  memset (cfun_frame_layout.gpr_save_slots, SAVE_SLOT_NONE, 6);

  for (i = 6; i < 16; i++)
    if (!clobbered_regs[i])
      cfun_gpr_save_slot (i) = SAVE_SLOT_NONE;

  s390_register_info_set_ranges ();
  s390_register_info_stdarg_gpr ();
}

/* Fill cfun->machine with info about frame of current function.  */

static void
s390_frame_info (void)
{
  HOST_WIDE_INT lowest_offset;

  cfun_frame_layout.first_save_gpr_slot = cfun_frame_layout.first_save_gpr;
  cfun_frame_layout.last_save_gpr_slot = cfun_frame_layout.last_save_gpr;

  /* The va_arg builtin uses a constant distance of 16 *
     UNITS_PER_LONG (r0-r15) to reach the FPRs from the reg_save_area
     pointer.  So even if we are going to save the stack pointer in an
     FPR we need the stack space in order to keep the offsets
     correct.  */
  if (cfun->stdarg && cfun_save_arg_fprs_p)
    {
      cfun_frame_layout.last_save_gpr_slot = STACK_POINTER_REGNUM;

      if (cfun_frame_layout.first_save_gpr_slot == -1)
	cfun_frame_layout.first_save_gpr_slot = STACK_POINTER_REGNUM;
    }

  cfun_frame_layout.frame_size = get_frame_size ();
  if (!TARGET_64BIT && cfun_frame_layout.frame_size > 0x7fff0000)
    fatal_error (input_location,
		 "total size of local variables exceeds architecture limit");

  if (!TARGET_PACKED_STACK)
    {
      /* Fixed stack layout.  */
      cfun_frame_layout.backchain_offset = 0;
      cfun_frame_layout.f0_offset = 16 * UNITS_PER_LONG;
      cfun_frame_layout.f4_offset = cfun_frame_layout.f0_offset + 2 * 8;
      cfun_frame_layout.f8_offset = -cfun_frame_layout.high_fprs * 8;
      cfun_frame_layout.gprs_offset = (cfun_frame_layout.first_save_gpr_slot
				       * UNITS_PER_LONG);
    }
  else if (TARGET_BACKCHAIN)
    {
      /* Kernel stack layout - packed stack, backchain, no float  */
      gcc_assert (TARGET_SOFT_FLOAT);
      cfun_frame_layout.backchain_offset = (STACK_POINTER_OFFSET
					    - UNITS_PER_LONG);

      /* The distance between the backchain and the return address
	 save slot must not change.  So we always need a slot for the
	 stack pointer which resides in between.  */
      cfun_frame_layout.last_save_gpr_slot = STACK_POINTER_REGNUM;

      cfun_frame_layout.gprs_offset
	= cfun_frame_layout.backchain_offset - cfun_gprs_save_area_size;

      /* FPRs will not be saved.  Nevertheless pick sane values to
	 keep area calculations valid.  */
      cfun_frame_layout.f0_offset =
	cfun_frame_layout.f4_offset =
	cfun_frame_layout.f8_offset = cfun_frame_layout.gprs_offset;
    }
  else
    {
      int num_fprs;

      /* Packed stack layout without backchain.  */

      /* With stdarg FPRs need their dedicated slots.  */
      num_fprs = (TARGET_64BIT && cfun->stdarg ? 2
		  : (cfun_fpr_save_p (FPR4_REGNUM) +
		     cfun_fpr_save_p (FPR6_REGNUM)));
      cfun_frame_layout.f4_offset = STACK_POINTER_OFFSET - 8 * num_fprs;

      num_fprs = (cfun->stdarg ? 2
		  : (cfun_fpr_save_p (FPR0_REGNUM)
		     + cfun_fpr_save_p (FPR2_REGNUM)));
      cfun_frame_layout.f0_offset = cfun_frame_layout.f4_offset - 8 * num_fprs;

      cfun_frame_layout.gprs_offset
	= cfun_frame_layout.f0_offset - cfun_gprs_save_area_size;

      cfun_frame_layout.f8_offset = (cfun_frame_layout.gprs_offset
				     - cfun_frame_layout.high_fprs * 8);
    }

  if (cfun_save_high_fprs_p)
    cfun_frame_layout.frame_size += cfun_frame_layout.high_fprs * 8;

  if (!crtl->is_leaf)
    cfun_frame_layout.frame_size += crtl->outgoing_args_size;

  /* In the following cases we have to allocate a STACK_POINTER_OFFSET
     sized area at the bottom of the stack.  This is required also for
     leaf functions.  When GCC generates a local stack reference it
     will always add STACK_POINTER_OFFSET to all these references.  */
  if (crtl->is_leaf
      && !TARGET_TPF_PROFILING
      && cfun_frame_layout.frame_size == 0
      && !cfun->calls_alloca)
    return;

  /* Calculate the number of bytes we have used in our own register
     save area.  With the packed stack layout we can re-use the
     remaining bytes for normal stack elements.  */

  if (TARGET_PACKED_STACK)
    lowest_offset = MIN (MIN (cfun_frame_layout.f0_offset,
			      cfun_frame_layout.f4_offset),
			 cfun_frame_layout.gprs_offset);
  else
    lowest_offset = 0;

  if (TARGET_BACKCHAIN)
    lowest_offset = MIN (lowest_offset, cfun_frame_layout.backchain_offset);

  cfun_frame_layout.frame_size += STACK_POINTER_OFFSET - lowest_offset;

  /* If under 31 bit an odd number of gprs has to be saved we have to
     adjust the frame size to sustain 8 byte alignment of stack
     frames.  */
  cfun_frame_layout.frame_size = ((cfun_frame_layout.frame_size +
				   STACK_BOUNDARY / BITS_PER_UNIT - 1)
				  & ~(STACK_BOUNDARY / BITS_PER_UNIT - 1));
}

/* Generate frame layout.  Fills in register and frame data for the current
   function in cfun->machine.  This routine can be called multiple times;
   it will re-do the complete frame layout every time.  */

static void
s390_init_frame_layout (void)
{
  HOST_WIDE_INT frame_size;
  int base_used;

  /* After LRA the frame layout is supposed to be read-only and should
     not be re-computed.  */
  if (reload_completed)
    return;

  do
    {
      frame_size = cfun_frame_layout.frame_size;

      /* Try to predict whether we'll need the base register.  */
      base_used = crtl->uses_const_pool
		  || (!DISP_IN_RANGE (frame_size)
		      && !CONST_OK_FOR_K (frame_size));

      /* Decide which register to use as literal pool base.  In small
	 leaf functions, try to use an unused call-clobbered register
	 as base register to avoid save/restore overhead.  */
      if (!base_used)
	cfun->machine->base_reg = NULL_RTX;
      else
	{
	  int br = 0;

	  if (crtl->is_leaf)
	    /* Prefer r5 (most likely to be free).  */
	    for (br = 5; br >= 2 && df_regs_ever_live_p (br); br--)
	      ;
	  cfun->machine->base_reg =
	    gen_rtx_REG (Pmode, (br >= 2) ? br : BASE_REGNUM);
	}

      s390_register_info ();
      s390_frame_info ();
    }
  while (frame_size != cfun_frame_layout.frame_size);
}

/* Remove the FPR clobbers from a tbegin insn if it can be proven that
   the TX is nonescaping.  A transaction is considered escaping if
   there is at least one path from tbegin returning CC0 to the
   function exit block without an tend.

   The check so far has some limitations:
   - only single tbegin/tend BBs are supported
   - the first cond jump after tbegin must separate the CC0 path from ~CC0
   - when CC is copied to a GPR and the CC0 check is done with the GPR
     this is not supported
*/

static void
s390_optimize_nonescaping_tx (void)
{
  const unsigned int CC0 = 1 << 3;
  basic_block tbegin_bb = NULL;
  basic_block tend_bb = NULL;
  basic_block bb;
  rtx_insn *insn;
  bool result = true;
  int bb_index;
  rtx_insn *tbegin_insn = NULL;

  if (!cfun->machine->tbegin_p)
    return;

  for (bb_index = 0; bb_index < n_basic_blocks_for_fn (cfun); bb_index++)
    {
      bb = BASIC_BLOCK_FOR_FN (cfun, bb_index);

      if (!bb)
	continue;

      FOR_BB_INSNS (bb, insn)
	{
	  rtx ite, cc, pat, target;
	  unsigned HOST_WIDE_INT mask;

	  if (!INSN_P (insn) || INSN_CODE (insn) <= 0)
	    continue;

	  pat = PATTERN (insn);

	  if (GET_CODE (pat) == PARALLEL)
	    pat = XVECEXP (pat, 0, 0);

	  if (GET_CODE (pat) != SET
	      || GET_CODE (SET_SRC (pat)) != UNSPEC_VOLATILE)
	    continue;

	  if (XINT (SET_SRC (pat), 1) == UNSPECV_TBEGIN)
	    {
	      rtx_insn *tmp;

	      tbegin_insn = insn;

	      /* Just return if the tbegin doesn't have clobbers.  */
	      if (GET_CODE (PATTERN (insn)) != PARALLEL)
		return;

	      if (tbegin_bb != NULL)
		return;

	      /* Find the next conditional jump.  */
	      for (tmp = NEXT_INSN (insn);
		   tmp != NULL_RTX;
		   tmp = NEXT_INSN (tmp))
		{
		  if (reg_set_p (gen_rtx_REG (CCmode, CC_REGNUM), tmp))
		    return;
		  if (!JUMP_P (tmp))
		    continue;

		  ite = SET_SRC (PATTERN (tmp));
		  if (GET_CODE (ite) != IF_THEN_ELSE)
		    continue;

		  cc = XEXP (XEXP (ite, 0), 0);
		  if (!REG_P (cc) || !CC_REGNO_P (REGNO (cc))
		      || GET_MODE (cc) != CCRAWmode
		      || GET_CODE (XEXP (XEXP (ite, 0), 1)) != CONST_INT)
		    return;

		  if (bb->succs->length () != 2)
		    return;

		  mask = INTVAL (XEXP (XEXP (ite, 0), 1));
		  if (GET_CODE (XEXP (ite, 0)) == NE)
		    mask ^= 0xf;

		  if (mask == CC0)
		    target = XEXP (ite, 1);
		  else if (mask == (CC0 ^ 0xf))
		    target = XEXP (ite, 2);
		  else
		    return;

		  {
		    edge_iterator ei;
		    edge e1, e2;

		    ei = ei_start (bb->succs);
		    e1 = ei_safe_edge (ei);
		    ei_next (&ei);
		    e2 = ei_safe_edge (ei);

		    if (e2->flags & EDGE_FALLTHRU)
		      {
			e2 = e1;
			e1 = ei_safe_edge (ei);
		      }

		    if (!(e1->flags & EDGE_FALLTHRU))
		      return;

		    tbegin_bb = (target == pc_rtx) ? e1->dest : e2->dest;
		  }
		  if (tmp == BB_END (bb))
		    break;
		}
	    }

	  if (XINT (SET_SRC (pat), 1) == UNSPECV_TEND)
	    {
	      if (tend_bb != NULL)
		return;
	      tend_bb = bb;
	    }
	}
    }

  /* Either we successfully remove the FPR clobbers here or we are not
     able to do anything for this TX.  Both cases don't qualify for
     another look.  */
  cfun->machine->tbegin_p = false;

  if (tbegin_bb == NULL || tend_bb == NULL)
    return;

  calculate_dominance_info (CDI_POST_DOMINATORS);
  result = dominated_by_p (CDI_POST_DOMINATORS, tbegin_bb, tend_bb);
  free_dominance_info (CDI_POST_DOMINATORS);

  if (!result)
    return;

  PATTERN (tbegin_insn) = gen_rtx_PARALLEL (VOIDmode,
			    gen_rtvec (2,
				       XVECEXP (PATTERN (tbegin_insn), 0, 0),
				       XVECEXP (PATTERN (tbegin_insn), 0, 1)));
  INSN_CODE (tbegin_insn) = -1;
  df_insn_rescan (tbegin_insn);

  return;
}

/* Implement TARGET_HARD_REGNO_NREGS.  Because all registers in a class
   have the same size, this is equivalent to CLASS_MAX_NREGS.  */

static unsigned int
s390_hard_regno_nregs (unsigned int regno, machine_mode mode)
{
  return s390_class_max_nregs (REGNO_REG_CLASS (regno), mode);
}

/* Implement TARGET_HARD_REGNO_MODE_OK.

   Integer modes <= word size fit into any GPR.
   Integer modes > word size fit into successive GPRs, starting with
   an even-numbered register.
   SImode and DImode fit into FPRs as well.

   Floating point modes <= word size fit into any FPR or GPR.
   Floating point modes > word size (i.e. DFmode on 32-bit) fit
   into any FPR, or an even-odd GPR pair.
   TFmode fits only into an even-odd FPR pair.

   Complex floating point modes fit either into two FPRs, or into
   successive GPRs (again starting with an even number).
   TCmode fits only into two successive even-odd FPR pairs.

   Condition code modes fit only into the CC register.  */

static bool
s390_hard_regno_mode_ok (unsigned int regno, machine_mode mode)
{
  if (!TARGET_VX && VECTOR_NOFP_REGNO_P (regno))
    return false;

  switch (REGNO_REG_CLASS (regno))
    {
    case VEC_REGS:
      return ((GET_MODE_CLASS (mode) == MODE_INT
	       && s390_class_max_nregs (VEC_REGS, mode) == 1)
	      || mode == DFmode
	      || (TARGET_VXE && mode == SFmode)
	      || s390_vector_mode_supported_p (mode));
      break;
    case FP_REGS:
      if (TARGET_VX
	  && ((GET_MODE_CLASS (mode) == MODE_INT
	       && s390_class_max_nregs (FP_REGS, mode) == 1)
	      || mode == DFmode
	      || s390_vector_mode_supported_p (mode)))
	return true;

      if (REGNO_PAIR_OK (regno, mode))
	{
	  if (mode == SImode || mode == DImode)
	    return true;

	  if (FLOAT_MODE_P (mode) && GET_MODE_CLASS (mode) != MODE_VECTOR_FLOAT)
	    return true;
	}
      break;
    case ADDR_REGS:
      if (FRAME_REGNO_P (regno) && mode == Pmode)
	return true;

      /* fallthrough */
    case GENERAL_REGS:
      if (REGNO_PAIR_OK (regno, mode))
	{
	  if (TARGET_ZARCH
	      || (mode != TFmode && mode != TCmode && mode != TDmode))
	    return true;
	}
      break;
    case CC_REGS:
      if (GET_MODE_CLASS (mode) == MODE_CC)
	return true;
      break;
    case ACCESS_REGS:
      if (REGNO_PAIR_OK (regno, mode))
	{
	  if (mode == SImode || mode == Pmode)
	    return true;
	}
      break;
    default:
      return false;
    }

  return false;
}

/* Implement TARGET_MODES_TIEABLE_P.  */

static bool
s390_modes_tieable_p (machine_mode mode1, machine_mode mode2)
{
  return ((mode1 == SFmode || mode1 == DFmode)
	  == (mode2 == SFmode || mode2 == DFmode));
}

/* Return nonzero if register OLD_REG can be renamed to register NEW_REG.  */

bool
s390_hard_regno_rename_ok (unsigned int old_reg, unsigned int new_reg)
{
   /* Once we've decided upon a register to use as base register, it must
      no longer be used for any other purpose.  */
  if (cfun->machine->base_reg)
    if (REGNO (cfun->machine->base_reg) == old_reg
	|| REGNO (cfun->machine->base_reg) == new_reg)
      return false;

  /* Prevent regrename from using call-saved regs which haven't
     actually been saved.  This is necessary since regrename assumes
     the backend save/restore decisions are based on
     df_regs_ever_live.  Since we have our own routine we have to tell
     regrename manually about it.  */
  if (GENERAL_REGNO_P (new_reg)
      && !call_used_regs[new_reg]
      && cfun_gpr_save_slot (new_reg) == SAVE_SLOT_NONE)
    return false;

  return true;
}

/* Return nonzero if register REGNO can be used as a scratch register
   in peephole2.  */

static bool
s390_hard_regno_scratch_ok (unsigned int regno)
{
  /* See s390_hard_regno_rename_ok.  */
  if (GENERAL_REGNO_P (regno)
      && !call_used_regs[regno]
      && cfun_gpr_save_slot (regno) == SAVE_SLOT_NONE)
    return false;

  return true;
}

/* Implement TARGET_HARD_REGNO_CALL_PART_CLOBBERED.  When generating
   code that runs in z/Architecture mode, but conforms to the 31-bit
   ABI, GPRs can hold 8 bytes; the ABI guarantees only that the lower 4
   bytes are saved across calls, however.  */

static bool
s390_hard_regno_call_part_clobbered (unsigned int, unsigned int regno,
				     machine_mode mode)
{
  /* For r12 we know that the only bits we actually care about are
     preserved across function calls.  Since r12 is a fixed reg all
     accesses to r12 are generated by the backend.

     This workaround is necessary until gcse implements proper
     tracking of partially clobbered registers.  */
  if (!TARGET_64BIT
      && TARGET_ZARCH
      && GET_MODE_SIZE (mode) > 4
      && (!flag_pic || regno != PIC_OFFSET_TABLE_REGNUM)
      && ((regno >= 6 && regno <= 15) || regno == 32))
    return true;

  if (TARGET_VX
      && GET_MODE_SIZE (mode) > 8
      && (((TARGET_64BIT && regno >= 24 && regno <= 31))
	  || (!TARGET_64BIT && (regno == 18 || regno == 19))))
    return true;

  return false;
}

/* Maximum number of registers to represent a value of mode MODE
   in a register of class RCLASS.  */

int
s390_class_max_nregs (enum reg_class rclass, machine_mode mode)
{
  int reg_size;
  bool reg_pair_required_p = false;

  switch (rclass)
    {
    case FP_REGS:
    case VEC_REGS:
      reg_size = TARGET_VX ? 16 : 8;

      /* TF and TD modes would fit into a VR but we put them into a
	 register pair since we do not have 128bit FP instructions on
	 full VRs.  */
      if (TARGET_VX
	  && SCALAR_FLOAT_MODE_P (mode)
	  && GET_MODE_SIZE (mode) >= 16
	  && !(TARGET_VXE && mode == TFmode))
	reg_pair_required_p = true;

      /* Even if complex types would fit into a single FPR/VR we force
	 them into a register pair to deal with the parts more easily.
	 (FIXME: What about complex ints?)  */
      if (GET_MODE_CLASS (mode) == MODE_COMPLEX_FLOAT)
	reg_pair_required_p = true;
      break;
    case ACCESS_REGS:
      reg_size = 4;
      break;
    default:
      reg_size = UNITS_PER_WORD;
      break;
    }

  if (reg_pair_required_p)
    return 2 * ((GET_MODE_SIZE (mode) / 2 + reg_size - 1) / reg_size);

  return (GET_MODE_SIZE (mode) + reg_size - 1) / reg_size;
}

/* Return nonzero if mode M describes a 128-bit float in a floating point
   register pair.  */

static bool
s390_is_fpr128 (machine_mode m)
{
  return m == FPRX2mode || (!TARGET_VXE && m == TFmode);
}

/* Return nonzero if mode M describes a 128-bit float in a vector
   register.  */

static bool
s390_is_vr128 (machine_mode m)
{
  return m == V1TFmode || (TARGET_VXE && m == TFmode);
}

/* Implement TARGET_CAN_CHANGE_MODE_CLASS.  */

static bool
s390_can_change_mode_class (machine_mode from_mode,
			    machine_mode to_mode,
			    reg_class_t rclass)
{
  machine_mode small_mode;
  machine_mode big_mode;

  /* 128-bit values have different representations in floating point and
     vector registers.  */
  if (reg_classes_intersect_p (VEC_REGS, rclass)
      && ((s390_is_fpr128 (from_mode) && s390_is_vr128 (to_mode))
	  || (s390_is_vr128 (from_mode) && s390_is_fpr128 (to_mode))))
    return false;

  if (GET_MODE_SIZE (from_mode) == GET_MODE_SIZE (to_mode))
    return true;

  if (GET_MODE_SIZE (from_mode) < GET_MODE_SIZE (to_mode))
    {
      small_mode = from_mode;
      big_mode = to_mode;
    }
  else
    {
      small_mode = to_mode;
      big_mode = from_mode;
    }

  /* Values residing in VRs are little-endian style.  All modes are
     placed left-aligned in an VR.  This means that we cannot allow
     switching between modes with differing sizes.  Also if the vector
     facility is available we still place TFmode values in VR register
     pairs, since the only instructions we have operating on TFmodes
     only deal with register pairs.  Therefore we have to allow DFmode
     subregs of TFmodes to enable the TFmode splitters.  */
  if (reg_classes_intersect_p (VEC_REGS, rclass)
      && (GET_MODE_SIZE (small_mode) < 8
	  || s390_class_max_nregs (VEC_REGS, big_mode) == 1))
    return false;

  /* Likewise for access registers, since they have only half the
     word size on 64-bit.  */
  if (reg_classes_intersect_p (ACCESS_REGS, rclass))
    return false;

  return true;
}

/* Return true if we use LRA instead of reload pass.  */
static bool
s390_lra_p (void)
{
  return s390_lra_flag;
}

/* Return true if register FROM can be eliminated via register TO.  */

static bool
s390_can_eliminate (const int from, const int to)
{
  /* We have not marked the base register as fixed.
     Instead, we have an elimination rule BASE_REGNUM -> BASE_REGNUM.
     If a function requires the base register, we say here that this
     elimination cannot be performed.  This will cause reload to free
     up the base register (as if it were fixed).  On the other hand,
     if the current function does *not* require the base register, we
     say here the elimination succeeds, which in turn allows reload
     to allocate the base register for any other purpose.  */
  if (from == BASE_REGNUM && to == BASE_REGNUM)
    {
      s390_init_frame_layout ();
      return cfun->machine->base_reg == NULL_RTX;
    }

  /* Everything else must point into the stack frame.  */
  gcc_assert (to == STACK_POINTER_REGNUM
	      || to == HARD_FRAME_POINTER_REGNUM);

  gcc_assert (from == FRAME_POINTER_REGNUM
	      || from == ARG_POINTER_REGNUM
	      || from == RETURN_ADDRESS_POINTER_REGNUM);

  /* Make sure we actually saved the return address.  */
  if (from == RETURN_ADDRESS_POINTER_REGNUM)
    if (!crtl->calls_eh_return
	&& !cfun->stdarg
	&& !cfun_frame_layout.save_return_addr_p)
      return false;

  return true;
}

/* Return offset between register FROM and TO initially after prolog.  */

HOST_WIDE_INT
s390_initial_elimination_offset (int from, int to)
{
  HOST_WIDE_INT offset;

  /* ??? Why are we called for non-eliminable pairs?  */
  if (!s390_can_eliminate (from, to))
    return 0;

  switch (from)
    {
    case FRAME_POINTER_REGNUM:
      offset = (get_frame_size()
		+ STACK_POINTER_OFFSET
		+ crtl->outgoing_args_size);
      break;

    case ARG_POINTER_REGNUM:
      s390_init_frame_layout ();
      offset = cfun_frame_layout.frame_size + STACK_POINTER_OFFSET;
      break;

    case RETURN_ADDRESS_POINTER_REGNUM:
      s390_init_frame_layout ();

      if (cfun_frame_layout.first_save_gpr_slot == -1)
	{
	  /* If it turns out that for stdarg nothing went into the reg
	     save area we also do not need the return address
	     pointer.  */
	  if (cfun->stdarg && !cfun_save_arg_fprs_p)
	    return 0;

	  gcc_unreachable ();
	}

      /* In order to make the following work it is not necessary for
	 r14 to have a save slot.  It is sufficient if one other GPR
	 got one.  Since the GPRs are always stored without gaps we
	 are able to calculate where the r14 save slot would
	 reside.  */
      offset = (cfun_frame_layout.frame_size + cfun_frame_layout.gprs_offset +
		(RETURN_REGNUM - cfun_frame_layout.first_save_gpr_slot) *
		UNITS_PER_LONG);
      break;

    case BASE_REGNUM:
      offset = 0;
      break;

    default:
      gcc_unreachable ();
    }

  return offset;
}

/* Emit insn to save fpr REGNUM at offset OFFSET relative
   to register BASE.  Return generated insn.  */

static rtx
save_fpr (rtx base, int offset, int regnum)
{
  rtx addr;
  addr = gen_rtx_MEM (DFmode, plus_constant (Pmode, base, offset));

  if (regnum >= 16 && regnum <= (16 + FP_ARG_NUM_REG))
    set_mem_alias_set (addr, get_varargs_alias_set ());
  else
    set_mem_alias_set (addr, get_frame_alias_set ());

  return emit_move_insn (addr, gen_rtx_REG (DFmode, regnum));
}

/* Emit insn to restore fpr REGNUM from offset OFFSET relative
   to register BASE.  Return generated insn.  */

static rtx
restore_fpr (rtx base, int offset, int regnum)
{
  rtx addr;
  addr = gen_rtx_MEM (DFmode, plus_constant (Pmode, base, offset));
  set_mem_alias_set (addr, get_frame_alias_set ());

  return emit_move_insn (gen_rtx_REG (DFmode, regnum), addr);
}

/* Generate insn to save registers FIRST to LAST into
   the register save area located at offset OFFSET
   relative to register BASE.  */

static rtx
save_gprs (rtx base, int offset, int first, int last)
{
  rtx addr, insn, note;
  int i;

  addr = plus_constant (Pmode, base, offset);
  addr = gen_rtx_MEM (Pmode, addr);

  set_mem_alias_set (addr, get_frame_alias_set ());

  /* Special-case single register.  */
  if (first == last)
    {
      if (TARGET_64BIT)
	insn = gen_movdi (addr, gen_rtx_REG (Pmode, first));
      else
	insn = gen_movsi (addr, gen_rtx_REG (Pmode, first));

      if (!global_not_special_regno_p (first))
	RTX_FRAME_RELATED_P (insn) = 1;
      return insn;
    }


  insn = gen_store_multiple (addr,
			     gen_rtx_REG (Pmode, first),
			     GEN_INT (last - first + 1));

  if (first <= 6 && cfun->stdarg)
    for (i = 0; i < XVECLEN (PATTERN (insn), 0); i++)
      {
	rtx mem = XEXP (XVECEXP (PATTERN (insn), 0, i), 0);

	if (first + i <= 6)
	  set_mem_alias_set (mem, get_varargs_alias_set ());
      }

  /* We need to set the FRAME_RELATED flag on all SETs
     inside the store-multiple pattern.

     However, we must not emit DWARF records for registers 2..5
     if they are stored for use by variable arguments ...

     ??? Unfortunately, it is not enough to simply not the
     FRAME_RELATED flags for those SETs, because the first SET
     of the PARALLEL is always treated as if it had the flag
     set, even if it does not.  Therefore we emit a new pattern
     without those registers as REG_FRAME_RELATED_EXPR note.  */

  if (first >= 6 && !global_not_special_regno_p (first))
    {
      rtx pat = PATTERN (insn);

      for (i = 0; i < XVECLEN (pat, 0); i++)
	if (GET_CODE (XVECEXP (pat, 0, i)) == SET
	    && !global_not_special_regno_p (REGNO (SET_SRC (XVECEXP (pat,
								     0, i)))))
	  RTX_FRAME_RELATED_P (XVECEXP (pat, 0, i)) = 1;

      RTX_FRAME_RELATED_P (insn) = 1;
    }
  else if (last >= 6)
    {
      int start;

      for (start = first >= 6 ? first : 6; start <= last; start++)
	if (!global_not_special_regno_p (start))
	  break;

      if (start > last)
	return insn;

      addr = plus_constant (Pmode, base,
			    offset + (start - first) * UNITS_PER_LONG);

      if (start == last)
	{
	  if (TARGET_64BIT)
	    note = gen_movdi (gen_rtx_MEM (Pmode, addr),
			      gen_rtx_REG (Pmode, start));
	  else
	    note = gen_movsi (gen_rtx_MEM (Pmode, addr),
			      gen_rtx_REG (Pmode, start));
	  note = PATTERN (note);

	  add_reg_note (insn, REG_FRAME_RELATED_EXPR, note);
	  RTX_FRAME_RELATED_P (insn) = 1;

	  return insn;
	}

      note = gen_store_multiple (gen_rtx_MEM (Pmode, addr),
				 gen_rtx_REG (Pmode, start),
				 GEN_INT (last - start + 1));
      note = PATTERN (note);

      add_reg_note (insn, REG_FRAME_RELATED_EXPR, note);

      for (i = 0; i < XVECLEN (note, 0); i++)
	if (GET_CODE (XVECEXP (note, 0, i)) == SET
	    && !global_not_special_regno_p (REGNO (SET_SRC (XVECEXP (note,
								     0, i)))))
	  RTX_FRAME_RELATED_P (XVECEXP (note, 0, i)) = 1;

      RTX_FRAME_RELATED_P (insn) = 1;
    }

  return insn;
}

/* Generate insn to restore registers FIRST to LAST from
   the register save area located at offset OFFSET
   relative to register BASE.  */

static rtx
restore_gprs (rtx base, int offset, int first, int last)
{
  rtx addr, insn;

  addr = plus_constant (Pmode, base, offset);
  addr = gen_rtx_MEM (Pmode, addr);
  set_mem_alias_set (addr, get_frame_alias_set ());

  /* Special-case single register.  */
  if (first == last)
    {
      if (TARGET_64BIT)
	insn = gen_movdi (gen_rtx_REG (Pmode, first), addr);
      else
	insn = gen_movsi (gen_rtx_REG (Pmode, first), addr);

      RTX_FRAME_RELATED_P (insn) = 1;
      return insn;
    }

  insn = gen_load_multiple (gen_rtx_REG (Pmode, first),
			    addr,
			    GEN_INT (last - first + 1));
  RTX_FRAME_RELATED_P (insn) = 1;
  return insn;
}

/* Return insn sequence to load the GOT register.  */

rtx_insn *
s390_load_got (void)
{
  rtx_insn *insns;

  /* We cannot use pic_offset_table_rtx here since we use this
     function also for non-pic if __tls_get_offset is called and in
     that case PIC_OFFSET_TABLE_REGNUM as well as pic_offset_table_rtx
     aren't usable.  */
  rtx got_rtx = gen_rtx_REG (Pmode, 12);

  start_sequence ();

  emit_move_insn (got_rtx, s390_got_symbol ());

  insns = get_insns ();
  end_sequence ();
  return insns;
}

/* This ties together stack memory (MEM with an alias set of frame_alias_set)
   and the change to the stack pointer.  */

static void
s390_emit_stack_tie (void)
{
  rtx mem = gen_frame_mem (BLKmode,
			   gen_rtx_REG (Pmode, STACK_POINTER_REGNUM));

  emit_insn (gen_stack_tie (mem));
}

/* Copy GPRS into FPR save slots.  */

static void
s390_save_gprs_to_fprs (void)
{
  int i;

  if (!TARGET_Z10 || !TARGET_HARD_FLOAT || !crtl->is_leaf)
    return;

  for (i = 6; i < 16; i++)
    {
      if (FP_REGNO_P (cfun_gpr_save_slot (i)))
	{
	  rtx_insn *insn =
	    emit_move_insn (gen_rtx_REG (DImode, cfun_gpr_save_slot (i)),
			    gen_rtx_REG (DImode, i));
	  RTX_FRAME_RELATED_P (insn) = 1;
	  /* This prevents dwarf2cfi from interpreting the set.  Doing
	     so it might emit def_cfa_register infos setting an FPR as
	     new CFA.  */
	  add_reg_note (insn, REG_CFA_REGISTER, copy_rtx (PATTERN (insn)));
	}
    }
}

/* Restore GPRs from FPR save slots.  */

static void
s390_restore_gprs_from_fprs (void)
{
  int i;

  if (!TARGET_Z10 || !TARGET_HARD_FLOAT || !crtl->is_leaf)
    return;

  /* Restore the GPRs starting with the stack pointer.  That way the
     stack pointer already has its original value when it comes to
     restoring the hard frame pointer.  So we can set the cfa reg back
     to the stack pointer.  */
  for (i = STACK_POINTER_REGNUM; i >= 6; i--)
    {
      rtx_insn *insn;

      if (!FP_REGNO_P (cfun_gpr_save_slot (i)))
	continue;

      rtx fpr = gen_rtx_REG (DImode, cfun_gpr_save_slot (i));

      if (i == STACK_POINTER_REGNUM)
	insn = emit_insn (gen_stack_restore_from_fpr (fpr));
      else
	insn = emit_move_insn (gen_rtx_REG (DImode, i), fpr);

      df_set_regs_ever_live (i, true);
      add_reg_note (insn, REG_CFA_RESTORE, gen_rtx_REG (DImode, i));

      /* If either the stack pointer or the frame pointer get restored
	 set the CFA value to its value at function start.  Doing this
	 for the frame pointer results in .cfi_def_cfa_register 15
	 what is ok since if the stack pointer got modified it has
	 been restored already.  */
      if (i == STACK_POINTER_REGNUM || i == HARD_FRAME_POINTER_REGNUM)
	add_reg_note (insn, REG_CFA_DEF_CFA,
		      plus_constant (Pmode, stack_pointer_rtx,
				     STACK_POINTER_OFFSET));
      RTX_FRAME_RELATED_P (insn) = 1;
    }
}


/* A pass run immediately before shrink-wrapping and prologue and epilogue
   generation.  */

namespace {

const pass_data pass_data_s390_early_mach =
{
  RTL_PASS, /* type */
  "early_mach", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  TV_MACH_DEP, /* tv_id */
  0, /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  ( TODO_df_verify | TODO_df_finish ), /* todo_flags_finish */
};

class pass_s390_early_mach : public rtl_opt_pass
{
public:
  pass_s390_early_mach (gcc::context *ctxt)
    : rtl_opt_pass (pass_data_s390_early_mach, ctxt)
  {}

  /* opt_pass methods: */
  virtual unsigned int execute (function *);

}; // class pass_s390_early_mach

unsigned int
pass_s390_early_mach::execute (function *fun)
{
  rtx_insn *insn;

  /* Try to get rid of the FPR clobbers.  */
  s390_optimize_nonescaping_tx ();

  /* Re-compute register info.  */
  s390_register_info ();

  /* If we're using a base register, ensure that it is always valid for
     the first non-prologue instruction.  */
  if (fun->machine->base_reg)
    emit_insn_at_entry (gen_main_pool (fun->machine->base_reg));

  /* Annotate all constant pool references to let the scheduler know
     they implicitly use the base register.  */
  for (insn = get_insns (); insn; insn = NEXT_INSN (insn))
    if (INSN_P (insn))
      {
	annotate_constant_pool_refs (insn);
	df_insn_rescan (insn);
      }
  return 0;
}

} // anon namespace

rtl_opt_pass *
make_pass_s390_early_mach (gcc::context *ctxt)
{
  return new pass_s390_early_mach (ctxt);
}

/* Calculate TARGET = REG + OFFSET as s390_emit_prologue would do it.
   - push too big immediates to the literal pool and annotate the refs
   - emit frame related notes for stack pointer changes.  */

static rtx
s390_prologue_plus_offset (rtx target, rtx reg, rtx offset, bool frame_related_p)
{
  rtx_insn *insn;
  rtx orig_offset = offset;

  gcc_assert (REG_P (target));
  gcc_assert (REG_P (reg));
  gcc_assert (CONST_INT_P (offset));

  if (offset == const0_rtx)                               /* lr/lgr */
    {
      insn = emit_move_insn (target, reg);
    }
  else if (DISP_IN_RANGE (INTVAL (offset)))               /* la */
    {
      insn = emit_move_insn (target, gen_rtx_PLUS (Pmode, reg,
						   offset));
    }
  else
    {
      if (!satisfies_constraint_K (offset)                /* ahi/aghi */
	  && (!TARGET_EXTIMM
	      || (!satisfies_constraint_Op (offset)       /* alfi/algfi */
		  && !satisfies_constraint_On (offset)))) /* slfi/slgfi */
	offset = force_const_mem (Pmode, offset);

      if (target != reg)
	{
	  insn = emit_move_insn (target, reg);
	  RTX_FRAME_RELATED_P (insn) = frame_related_p ? 1 : 0;
	}

      insn = emit_insn (gen_add2_insn (target, offset));

      if (!CONST_INT_P (offset))
	{
	  annotate_constant_pool_refs (insn);

	  if (frame_related_p)
	    add_reg_note (insn, REG_FRAME_RELATED_EXPR,
			  gen_rtx_SET (target,
				       gen_rtx_PLUS (Pmode, target,
						     orig_offset)));
	}
    }

  RTX_FRAME_RELATED_P (insn) = frame_related_p ? 1 : 0;

  /* If this is a stack adjustment and we are generating a stack clash
     prologue, then add a REG_STACK_CHECK note to signal that this insn
     should be left alone.  */
  if (flag_stack_clash_protection && target == stack_pointer_rtx)
    add_reg_note (insn, REG_STACK_CHECK, const0_rtx);

  return insn;
}

/* Emit a compare instruction with a volatile memory access as stack
   probe.  It does not waste store tags and does not clobber any
   registers apart from the condition code.  */
static void
s390_emit_stack_probe (rtx addr)
{
  rtx mem = gen_rtx_MEM (word_mode, addr);
  MEM_VOLATILE_P (mem) = 1;
  emit_insn (gen_probe_stack (mem));
}

/* Use a runtime loop if we have to emit more probes than this.  */
#define MIN_UNROLL_PROBES 3

/* Allocate SIZE bytes of stack space, using TEMP_REG as a temporary
   if necessary.  LAST_PROBE_OFFSET contains the offset of the closest
   probe relative to the stack pointer.

   Note that SIZE is negative.

   The return value is true if TEMP_REG has been clobbered.  */
static bool
allocate_stack_space (rtx size, HOST_WIDE_INT last_probe_offset,
		      rtx temp_reg)
{
  bool temp_reg_clobbered_p = false;
  HOST_WIDE_INT probe_interval
    = 1 << param_stack_clash_protection_probe_interval;
  HOST_WIDE_INT guard_size
    = 1 << param_stack_clash_protection_guard_size;

  if (flag_stack_clash_protection)
    {
      if (last_probe_offset + -INTVAL (size) < guard_size)
	dump_stack_clash_frame_info (NO_PROBE_SMALL_FRAME, true);
      else
	{
	  rtx offset = GEN_INT (probe_interval - UNITS_PER_LONG);
	  HOST_WIDE_INT rounded_size = -INTVAL (size) & -probe_interval;
	  HOST_WIDE_INT num_probes = rounded_size / probe_interval;
	  HOST_WIDE_INT residual = -INTVAL (size) - rounded_size;

	  if (num_probes < MIN_UNROLL_PROBES)
	    {
	      /* Emit unrolled probe statements.  */

	      for (unsigned int i = 0; i < num_probes; i++)
		{
		  s390_prologue_plus_offset (stack_pointer_rtx,
					     stack_pointer_rtx,
					     GEN_INT (-probe_interval), true);
		  s390_emit_stack_probe (gen_rtx_PLUS (Pmode,
						       stack_pointer_rtx,
						       offset));
		}
	      if (num_probes > 0)
		last_probe_offset = INTVAL (offset);
	      dump_stack_clash_frame_info (PROBE_INLINE, residual != 0);
	    }
	  else
	    {
	      /* Emit a loop probing the pages.  */

	      rtx_code_label *loop_start_label = gen_label_rtx ();

	      /* From now on temp_reg will be the CFA register.  */
	      s390_prologue_plus_offset (temp_reg, stack_pointer_rtx,
					 GEN_INT (-rounded_size), true);
	      emit_label (loop_start_label);

	      s390_prologue_plus_offset (stack_pointer_rtx,
					 stack_pointer_rtx,
					 GEN_INT (-probe_interval), false);
	      s390_emit_stack_probe (gen_rtx_PLUS (Pmode,
						   stack_pointer_rtx,
						   offset));
	      emit_cmp_and_jump_insns (stack_pointer_rtx, temp_reg,
				       GT, NULL_RTX,
				       Pmode, 1, loop_start_label);

	      /* Without this make_edges ICEes.  */
	      JUMP_LABEL (get_last_insn ()) = loop_start_label;
	      LABEL_NUSES (loop_start_label) = 1;

	      /* That's going to be a NOP since stack pointer and
		 temp_reg are supposed to be the same here.  We just
		 emit it to set the CFA reg back to r15.  */
	      s390_prologue_plus_offset (stack_pointer_rtx, temp_reg,
					 const0_rtx, true);
	      temp_reg_clobbered_p = true;
	      last_probe_offset = INTVAL (offset);
	      dump_stack_clash_frame_info (PROBE_LOOP, residual != 0);
	    }

	  /* Handle any residual allocation request.  */
	  s390_prologue_plus_offset (stack_pointer_rtx,
				     stack_pointer_rtx,
				     GEN_INT (-residual), true);
	  last_probe_offset += residual;
	  if (last_probe_offset >= probe_interval)
	    s390_emit_stack_probe (gen_rtx_PLUS (Pmode,
						 stack_pointer_rtx,
						 GEN_INT (residual
							  - UNITS_PER_LONG)));

	  return temp_reg_clobbered_p;
	}
    }

  /* Subtract frame size from stack pointer.  */
  s390_prologue_plus_offset (stack_pointer_rtx,
			     stack_pointer_rtx,
			     size, true);

  return temp_reg_clobbered_p;
}

/* Expand the prologue into a bunch of separate insns.  */

void
s390_emit_prologue (void)
{
  rtx insn, addr;
  rtx temp_reg;
  int i;
  int offset;
  int next_fpr = 0;

  /* Choose best register to use for temp use within prologue.
     TPF with profiling must avoid the register 14 - the tracing function
     needs the original contents of r14 to be preserved.  */

  if (!has_hard_reg_initial_val (Pmode, RETURN_REGNUM)
      && !crtl->is_leaf
      && !TARGET_TPF_PROFILING)
    temp_reg = gen_rtx_REG (Pmode, RETURN_REGNUM);
  else if (flag_split_stack && cfun->stdarg)
    temp_reg = gen_rtx_REG (Pmode, 12);
  else
    temp_reg = gen_rtx_REG (Pmode, 1);

  /* When probing for stack-clash mitigation, we have to track the distance
     between the stack pointer and closest known reference.

     Most of the time we have to make a worst case assumption.  The
     only exception is when TARGET_BACKCHAIN is active, in which case
     we know *sp (offset 0) was written.  */
  HOST_WIDE_INT probe_interval
    = 1 << param_stack_clash_protection_probe_interval;
  HOST_WIDE_INT last_probe_offset
    = (TARGET_BACKCHAIN
       ? (TARGET_PACKED_STACK ? STACK_POINTER_OFFSET - UNITS_PER_LONG : 0)
       : probe_interval - (STACK_BOUNDARY / UNITS_PER_WORD));

  s390_save_gprs_to_fprs ();

  /* Save call saved gprs.  */
  if (cfun_frame_layout.first_save_gpr != -1)
    {
      insn = save_gprs (stack_pointer_rtx,
			cfun_frame_layout.gprs_offset +
			UNITS_PER_LONG * (cfun_frame_layout.first_save_gpr
					  - cfun_frame_layout.first_save_gpr_slot),
			cfun_frame_layout.first_save_gpr,
			cfun_frame_layout.last_save_gpr);

      /* This is not 100% correct.  If we have more than one register saved,
	 then LAST_PROBE_OFFSET can move even closer to sp.  */
      last_probe_offset
	= (cfun_frame_layout.gprs_offset +
	   UNITS_PER_LONG * (cfun_frame_layout.first_save_gpr
			     - cfun_frame_layout.first_save_gpr_slot));

      emit_insn (insn);
    }

  /* Dummy insn to mark literal pool slot.  */

  if (cfun->machine->base_reg)
    emit_insn (gen_main_pool (cfun->machine->base_reg));

  offset = cfun_frame_layout.f0_offset;

  /* Save f0 and f2.  */
  for (i = FPR0_REGNUM; i <= FPR0_REGNUM + 1; i++)
    {
      if (cfun_fpr_save_p (i))
	{
	  save_fpr (stack_pointer_rtx, offset, i);
	  if (offset < last_probe_offset)
	    last_probe_offset = offset;
	  offset += 8;
	}
      else if (!TARGET_PACKED_STACK || cfun->stdarg)
	offset += 8;
    }

  /* Save f4 and f6.  */
  offset = cfun_frame_layout.f4_offset;
  for (i = FPR4_REGNUM; i <= FPR4_REGNUM + 1; i++)
    {
      if (cfun_fpr_save_p (i))
	{
	  insn = save_fpr (stack_pointer_rtx, offset, i);
	  if (offset < last_probe_offset)
	    last_probe_offset = offset;
	  offset += 8;

	  /* If f4 and f6 are call clobbered they are saved due to
	     stdargs and therefore are not frame related.  */
	  if (!call_used_regs[i])
	    RTX_FRAME_RELATED_P (insn) = 1;
	}
      else if (!TARGET_PACKED_STACK || call_used_regs[i])
	offset += 8;
    }

  if (TARGET_PACKED_STACK
      && cfun_save_high_fprs_p
      && cfun_frame_layout.f8_offset + cfun_frame_layout.high_fprs * 8 > 0)
    {
      offset = (cfun_frame_layout.f8_offset
		+ (cfun_frame_layout.high_fprs - 1) * 8);

      for (i = FPR15_REGNUM; i >= FPR8_REGNUM && offset >= 0; i--)
	if (cfun_fpr_save_p (i))
	  {
	    insn = save_fpr (stack_pointer_rtx, offset, i);
	    if (offset < last_probe_offset)
	      last_probe_offset = offset;

	    RTX_FRAME_RELATED_P (insn) = 1;
	    offset -= 8;
	  }
      if (offset >= cfun_frame_layout.f8_offset)
	next_fpr = i;
    }

  if (!TARGET_PACKED_STACK)
    next_fpr = cfun_save_high_fprs_p ? FPR15_REGNUM : 0;

  if (flag_stack_usage_info)
    current_function_static_stack_size = cfun_frame_layout.frame_size;

  /* Decrement stack pointer.  */

  if (cfun_frame_layout.frame_size > 0)
    {
      rtx frame_off = GEN_INT (-cfun_frame_layout.frame_size);
      rtx_insn *stack_pointer_backup_loc;
      bool temp_reg_clobbered_p;

      if (s390_stack_size)
	{
	  HOST_WIDE_INT stack_guard;

	  if (s390_stack_guard)
	    stack_guard = s390_stack_guard;
	  else
	    {
	      /* If no value for stack guard is provided the smallest power of 2
		 larger than the current frame size is chosen.  */
	      stack_guard = 1;
	      while (stack_guard < cfun_frame_layout.frame_size)
		stack_guard <<= 1;
	    }

	  if (cfun_frame_layout.frame_size >= s390_stack_size)
	    {
	      warning (0, "frame size of function %qs is %wd"
		       " bytes exceeding user provided stack limit of "
		       "%d bytes; "
		       "an unconditional trap is added",
		       current_function_name(), cfun_frame_layout.frame_size,
		       s390_stack_size);
	      emit_insn (gen_trap ());
	      emit_barrier ();
	    }
	  else
	    {
	      /* stack_guard has to be smaller than s390_stack_size.
		 Otherwise we would emit an AND with zero which would
		 not match the test under mask pattern.  */
	      if (stack_guard >= s390_stack_size)
		{
		  warning (0, "frame size of function %qs is %wd"
			   " bytes which is more than half the stack size; "
			   "the dynamic check would not be reliable; "
			   "no check emitted for this function",
			   current_function_name(),
			   cfun_frame_layout.frame_size);
		}
	      else
		{
		  HOST_WIDE_INT stack_check_mask = ((s390_stack_size - 1)
						    & ~(stack_guard - 1));

		  rtx t = gen_rtx_AND (Pmode, stack_pointer_rtx,
				       GEN_INT (stack_check_mask));
		  if (TARGET_64BIT)
		    emit_insn (gen_ctrapdi4 (gen_rtx_EQ (VOIDmode,
							 t, const0_rtx),
					     t, const0_rtx, const0_rtx));
		  else
		    emit_insn (gen_ctrapsi4 (gen_rtx_EQ (VOIDmode,
							 t, const0_rtx),
					     t, const0_rtx, const0_rtx));
		}
	    }
	}

      if (s390_warn_framesize > 0
	  && cfun_frame_layout.frame_size >= s390_warn_framesize)
	warning (0, "frame size of %qs is %wd bytes",
		 current_function_name (), cfun_frame_layout.frame_size);

      if (s390_warn_dynamicstack_p && cfun->calls_alloca)
	warning (0, "%qs uses dynamic stack allocation", current_function_name ());

      /* Save the location where we could backup the incoming stack
	 pointer.  */
      stack_pointer_backup_loc = get_last_insn ();

      temp_reg_clobbered_p = allocate_stack_space (frame_off, last_probe_offset,
						   temp_reg);

      if (TARGET_BACKCHAIN || next_fpr)
	{
	  if (temp_reg_clobbered_p)
	    {
	      /* allocate_stack_space had to make use of temp_reg and
		 we need it to hold a backup of the incoming stack
		 pointer.  Calculate back that value from the current
		 stack pointer.  */
	      s390_prologue_plus_offset (temp_reg, stack_pointer_rtx,
					 GEN_INT (cfun_frame_layout.frame_size),
					 false);
	    }
	  else
	    {
	      /* allocate_stack_space didn't actually required
		 temp_reg.  Insert the stack pointer backup insn
		 before the stack pointer decrement code - knowing now
		 that the value will survive.  */
	      emit_insn_after (gen_move_insn (temp_reg, stack_pointer_rtx),
			       stack_pointer_backup_loc);
	    }
	}

      /* Set backchain.  */

      if (TARGET_BACKCHAIN)
	{
	  if (cfun_frame_layout.backchain_offset)
	    addr = gen_rtx_MEM (Pmode,
				plus_constant (Pmode, stack_pointer_rtx,
				  cfun_frame_layout.backchain_offset));
	  else
	    addr = gen_rtx_MEM (Pmode, stack_pointer_rtx);
	  set_mem_alias_set (addr, get_frame_alias_set ());
	  insn = emit_insn (gen_move_insn (addr, temp_reg));
	}

      /* If we support non-call exceptions (e.g. for Java),
	 we need to make sure the backchain pointer is set up
	 before any possibly trapping memory access.  */
      if (TARGET_BACKCHAIN && cfun->can_throw_non_call_exceptions)
	{
	  addr = gen_rtx_MEM (BLKmode, gen_rtx_SCRATCH (VOIDmode));
	  emit_clobber (addr);
	}
    }
  else if (flag_stack_clash_protection)
    dump_stack_clash_frame_info (NO_PROBE_NO_FRAME, false);

  /* Save fprs 8 - 15 (64 bit ABI).  */

  if (cfun_save_high_fprs_p && next_fpr)
    {
      /* If the stack might be accessed through a different register
	 we have to make sure that the stack pointer decrement is not
	 moved below the use of the stack slots.  */
      s390_emit_stack_tie ();

      insn = emit_insn (gen_add2_insn (temp_reg,
				       GEN_INT (cfun_frame_layout.f8_offset)));

      offset = 0;

      for (i = FPR8_REGNUM; i <= next_fpr; i++)
	if (cfun_fpr_save_p (i))
	  {
	    rtx addr = plus_constant (Pmode, stack_pointer_rtx,
				      cfun_frame_layout.frame_size
				      + cfun_frame_layout.f8_offset
				      + offset);

	    insn = save_fpr (temp_reg, offset, i);
	    offset += 8;
	    RTX_FRAME_RELATED_P (insn) = 1;
	    add_reg_note (insn, REG_FRAME_RELATED_EXPR,
			  gen_rtx_SET (gen_rtx_MEM (DFmode, addr),
				       gen_rtx_REG (DFmode, i)));
	  }
    }

  /* Set frame pointer, if needed.  */

  if (frame_pointer_needed)
    {
      insn = emit_move_insn (hard_frame_pointer_rtx, stack_pointer_rtx);
      RTX_FRAME_RELATED_P (insn) = 1;
    }

  /* Set up got pointer, if needed.  */

  if (flag_pic && df_regs_ever_live_p (PIC_OFFSET_TABLE_REGNUM))
    {
      rtx_insn *insns = s390_load_got ();

      for (rtx_insn *insn = insns; insn; insn = NEXT_INSN (insn))
	annotate_constant_pool_refs (insn);

      emit_insn (insns);
    }

#if TARGET_TPF != 0
  if (TARGET_TPF_PROFILING)
    {
      /* Generate a BAS instruction to serve as a function entry
	 intercept to facilitate the use of tracing algorithms located
	 at the branch target.  */
      emit_insn (gen_prologue_tpf (
		   GEN_INT (s390_tpf_trace_hook_prologue_check),
		   GEN_INT (s390_tpf_trace_hook_prologue_target)));

      /* Emit a blockage here so that all code lies between the
	 profiling mechanisms.  */
      emit_insn (gen_blockage ());
    }
#endif
}

/* Expand the epilogue into a bunch of separate insns.  */

void
s390_emit_epilogue (bool sibcall)
{
  rtx frame_pointer, return_reg = NULL_RTX, cfa_restores = NULL_RTX;
  int area_bottom, area_top, offset = 0;
  int next_offset;
  int i;

#if TARGET_TPF != 0
  if (TARGET_TPF_PROFILING)
    {
      /* Generate a BAS instruction to serve as a function entry
	 intercept to facilitate the use of tracing algorithms located
	 at the branch target.  */

      /* Emit a blockage here so that all code lies between the
	 profiling mechanisms.  */
      emit_insn (gen_blockage ());

      emit_insn (gen_epilogue_tpf (
		   GEN_INT (s390_tpf_trace_hook_epilogue_check),
		   GEN_INT (s390_tpf_trace_hook_epilogue_target)));
    }
#endif

  /* Check whether to use frame or stack pointer for restore.  */

  frame_pointer = (frame_pointer_needed
		   ? hard_frame_pointer_rtx : stack_pointer_rtx);

  s390_frame_area (&area_bottom, &area_top);

  /* Check whether we can access the register save area.
     If not, increment the frame pointer as required.  */

  if (area_top <= area_bottom)
    {
      /* Nothing to restore.  */
    }
  else if (DISP_IN_RANGE (cfun_frame_layout.frame_size + area_bottom)
	   && DISP_IN_RANGE (cfun_frame_layout.frame_size + area_top - 1))
    {
      /* Area is in range.  */
      offset = cfun_frame_layout.frame_size;
    }
  else
    {
      rtx_insn *insn;
      rtx frame_off, cfa;

      offset = area_bottom < 0 ? -area_bottom : 0;
      frame_off = GEN_INT (cfun_frame_layout.frame_size - offset);

      cfa = gen_rtx_SET (frame_pointer,
			 gen_rtx_PLUS (Pmode, frame_pointer, frame_off));
      if (DISP_IN_RANGE (INTVAL (frame_off)))
	{
	  rtx set;

	  set = gen_rtx_SET (frame_pointer,
			     gen_rtx_PLUS (Pmode, frame_pointer, frame_off));
	  insn = emit_insn (set);
	}
      else
	{
	  if (!CONST_OK_FOR_K (INTVAL (frame_off)))
	    frame_off = force_const_mem (Pmode, frame_off);

	  insn = emit_insn (gen_add2_insn (frame_pointer, frame_off));
	  annotate_constant_pool_refs (insn);
	}
      add_reg_note (insn, REG_CFA_ADJUST_CFA, cfa);
      RTX_FRAME_RELATED_P (insn) = 1;
    }

  /* Restore call saved fprs.  */

  if (TARGET_64BIT)
    {
      if (cfun_save_high_fprs_p)
	{
	  next_offset = cfun_frame_layout.f8_offset;
	  for (i = FPR8_REGNUM; i <= FPR15_REGNUM; i++)
	    {
	      if (cfun_fpr_save_p (i))
		{
		  restore_fpr (frame_pointer,
			       offset + next_offset, i);
		  cfa_restores
		    = alloc_reg_note (REG_CFA_RESTORE,
				      gen_rtx_REG (DFmode, i), cfa_restores);
		  next_offset += 8;
		}
	    }
	}

    }
  else
    {
      next_offset = cfun_frame_layout.f4_offset;
      /* f4, f6 */
      for (i = FPR4_REGNUM; i <= FPR4_REGNUM + 1; i++)
	{
	  if (cfun_fpr_save_p (i))
	    {
	      restore_fpr (frame_pointer,
			   offset + next_offset, i);
	      cfa_restores
		= alloc_reg_note (REG_CFA_RESTORE,
				  gen_rtx_REG (DFmode, i), cfa_restores);
	      next_offset += 8;
	    }
	  else if (!TARGET_PACKED_STACK)
	    next_offset += 8;
	}

    }

  /* Restore call saved gprs.  */

  if (cfun_frame_layout.first_restore_gpr != -1)
    {
      rtx insn, addr;
      int i;

      /* Check for global register and save them
	 to stack location from where they get restored.  */

      for (i = cfun_frame_layout.first_restore_gpr;
	   i <= cfun_frame_layout.last_restore_gpr;
	   i++)
	{
	  if (global_not_special_regno_p (i))
	    {
	      addr = plus_constant (Pmode, frame_pointer,
				    offset + cfun_frame_layout.gprs_offset
				    + (i - cfun_frame_layout.first_save_gpr_slot)
				    * UNITS_PER_LONG);
	      addr = gen_rtx_MEM (Pmode, addr);
	      set_mem_alias_set (addr, get_frame_alias_set ());
	      emit_move_insn (addr, gen_rtx_REG (Pmode, i));
	    }
	  else
	    cfa_restores
	      = alloc_reg_note (REG_CFA_RESTORE,
				gen_rtx_REG (Pmode, i), cfa_restores);
	}

      /* Fetch return address from stack before load multiple,
	 this will do good for scheduling.

	 Only do this if we already decided that r14 needs to be
	 saved to a stack slot. (And not just because r14 happens to
	 be in between two GPRs which need saving.)  Otherwise it
	 would be difficult to take that decision back in
	 s390_optimize_prologue.

	 This optimization is only helpful on in-order machines.  */
      if (! sibcall
	  && cfun_gpr_save_slot (RETURN_REGNUM) == SAVE_SLOT_STACK
	  && s390_tune <= PROCESSOR_2097_Z10)
	{
	  int return_regnum = find_unused_clobbered_reg();
	  if (!return_regnum
	      || (TARGET_INDIRECT_BRANCH_NOBP_RET_OPTION
		  && !TARGET_CPU_Z10
		  && return_regnum == INDIRECT_BRANCH_THUNK_REGNUM))
	    {
	      gcc_assert (INDIRECT_BRANCH_THUNK_REGNUM != 4);
	      return_regnum = 4;
	    }
	  return_reg = gen_rtx_REG (Pmode, return_regnum);

	  addr = plus_constant (Pmode, frame_pointer,
				offset + cfun_frame_layout.gprs_offset
				+ (RETURN_REGNUM
				   - cfun_frame_layout.first_save_gpr_slot)
				* UNITS_PER_LONG);
	  addr = gen_rtx_MEM (Pmode, addr);
	  set_mem_alias_set (addr, get_frame_alias_set ());
	  emit_move_insn (return_reg, addr);

	  /* Once we did that optimization we have to make sure
	     s390_optimize_prologue does not try to remove the store
	     of r14 since we will not be able to find the load issued
	     here.  */
	  cfun_frame_layout.save_return_addr_p = true;
	}

      insn = restore_gprs (frame_pointer,
			   offset + cfun_frame_layout.gprs_offset
			   + (cfun_frame_layout.first_restore_gpr
			      - cfun_frame_layout.first_save_gpr_slot)
			   * UNITS_PER_LONG,
			   cfun_frame_layout.first_restore_gpr,
			   cfun_frame_layout.last_restore_gpr);
      insn = emit_insn (insn);
      REG_NOTES (insn) = cfa_restores;
      add_reg_note (insn, REG_CFA_DEF_CFA,
		    plus_constant (Pmode, stack_pointer_rtx,
				   STACK_POINTER_OFFSET));
      RTX_FRAME_RELATED_P (insn) = 1;
    }

  s390_restore_gprs_from_fprs ();

  if (! sibcall)
    {
      if (!return_reg && !s390_can_use_return_insn ())
        /* We planned to emit (return), be we are not allowed to.  */
        return_reg = gen_rtx_REG (Pmode, RETURN_REGNUM);

      if (return_reg)
        /* Emit (return) and (use).  */
        emit_jump_insn (gen_return_use (return_reg));
      else
        /* The fact that RETURN_REGNUM is used is already reflected by
           EPILOGUE_USES.  Emit plain (return).  */
        emit_jump_insn (gen_return ());
    }
}

/* Implement TARGET_SET_UP_BY_PROLOGUE.  */

static void
s300_set_up_by_prologue (hard_reg_set_container *regs)
{
  if (cfun->machine->base_reg
      && !call_used_regs[REGNO (cfun->machine->base_reg)])
    SET_HARD_REG_BIT (regs->set, REGNO (cfun->machine->base_reg));
}

/* -fsplit-stack support.  */

/* A SYMBOL_REF for __morestack.  */
static GTY(()) rtx morestack_ref;

/* When using -fsplit-stack, the allocation routines set a field in
   the TCB to the bottom of the stack plus this much space, measured
   in bytes.  */

#define SPLIT_STACK_AVAILABLE 1024

/* Emit the parmblock for __morestack into .rodata section.  It
   consists of 3 pointer size entries:
   - frame size
   - size of stack arguments
   - offset between parm block and __morestack return label  */

void
s390_output_split_stack_data (rtx parm_block, rtx call_done,
			      rtx frame_size, rtx args_size)
{
  rtx ops[] = { parm_block, call_done };

  switch_to_section (targetm.asm_out.function_rodata_section
		     (current_function_decl, false));

  if (TARGET_64BIT)
    output_asm_insn (".align\t8", NULL);
  else
    output_asm_insn (".align\t4", NULL);

  (*targetm.asm_out.internal_label) (asm_out_file, "L",
				     CODE_LABEL_NUMBER (parm_block));
  if (TARGET_64BIT)
    {
      output_asm_insn (".quad\t%0", &frame_size);
      output_asm_insn (".quad\t%0", &args_size);
      output_asm_insn (".quad\t%1-%0", ops);
    }
  else
    {
      output_asm_insn (".long\t%0", &frame_size);
      output_asm_insn (".long\t%0", &args_size);
      output_asm_insn (".long\t%1-%0", ops);
    }

  switch_to_section (current_function_section ());
}

/* Emit -fsplit-stack prologue, which goes before the regular function
   prologue.  */

void
s390_expand_split_stack_prologue (void)
{
  rtx r1, guard, cc = NULL;
  rtx_insn *insn;
  /* Offset from thread pointer to __private_ss.  */
  int psso = TARGET_64BIT ? 0x38 : 0x20;
  /* Pointer size in bytes.  */
  /* Frame size and argument size - the two parameters to __morestack.  */
  HOST_WIDE_INT frame_size = cfun_frame_layout.frame_size;
  /* Align argument size to 8 bytes - simplifies __morestack code.  */
  HOST_WIDE_INT args_size = crtl->args.size >= 0
			    ? ((crtl->args.size + 7) & ~7)
			    : 0;
  /* Label to be called by __morestack.  */
  rtx_code_label *call_done = NULL;
  rtx_code_label *parm_base = NULL;
  rtx tmp;

  gcc_assert (flag_split_stack && reload_completed);

  r1 = gen_rtx_REG (Pmode, 1);

  /* If no stack frame will be allocated, don't do anything.  */
  if (!frame_size)
    {
      if (cfun->machine->split_stack_varargs_pointer != NULL_RTX)
	{
	  /* If va_start is used, just use r15.  */
	  emit_move_insn (r1,
			 gen_rtx_PLUS (Pmode, stack_pointer_rtx,
				       GEN_INT (STACK_POINTER_OFFSET)));

	}
      return;
    }

  if (morestack_ref == NULL_RTX)
    {
      morestack_ref = gen_rtx_SYMBOL_REF (Pmode, "__morestack");
      SYMBOL_REF_FLAGS (morestack_ref) |= (SYMBOL_FLAG_LOCAL
					   | SYMBOL_FLAG_FUNCTION);
    }

  if (CONST_OK_FOR_K (frame_size) || CONST_OK_FOR_Op (frame_size))
    {
      /* If frame_size will fit in an add instruction, do a stack space
	 check, and only call __morestack if there's not enough space.  */

      /* Get thread pointer.  r1 is the only register we can always destroy - r0
	 could contain a static chain (and cannot be used to address memory
	 anyway), r2-r6 can contain parameters, and r6-r15 are callee-saved.  */
      emit_insn (gen_get_thread_pointer (Pmode, r1));
      /* Aim at __private_ss.  */
      guard = gen_rtx_MEM (Pmode, plus_constant (Pmode, r1, psso));

      /* If less that 1kiB used, skip addition and compare directly with
	 __private_ss.  */
      if (frame_size > SPLIT_STACK_AVAILABLE)
	{
	  emit_move_insn (r1, guard);
	  if (TARGET_64BIT)
	    emit_insn (gen_adddi3 (r1, r1, GEN_INT (frame_size)));
	  else
	    emit_insn (gen_addsi3 (r1, r1, GEN_INT (frame_size)));
	  guard = r1;
	}

      /* Compare the (maybe adjusted) guard with the stack pointer.  */
      cc = s390_emit_compare (LT, stack_pointer_rtx, guard);
    }

  call_done = gen_label_rtx ();
  parm_base = gen_label_rtx ();
  LABEL_NUSES (parm_base)++;
  LABEL_NUSES (call_done)++;

  /* %r1 = litbase.  */
  insn = emit_move_insn (r1, gen_rtx_LABEL_REF (VOIDmode, parm_base));
  add_reg_note (insn, REG_LABEL_OPERAND, parm_base);
  LABEL_NUSES (parm_base)++;

  /* Now, we need to call __morestack.  It has very special calling
     conventions: it preserves param/return/static chain registers for
     calling main function body, and looks for its own parameters at %r1. */
  if (cc != NULL)
    tmp = gen_split_stack_cond_call (Pmode,
				     morestack_ref,
				     parm_base,
				     call_done,
				     GEN_INT (frame_size),
				     GEN_INT (args_size),
				     cc);
  else
    tmp = gen_split_stack_call (Pmode,
				morestack_ref,
				parm_base,
				call_done,
				GEN_INT (frame_size),
				GEN_INT (args_size));

  insn = emit_jump_insn (tmp);
  JUMP_LABEL (insn) = call_done;
  add_reg_note (insn, REG_LABEL_OPERAND, parm_base);
  add_reg_note (insn, REG_LABEL_OPERAND, call_done);

  if (cc != NULL)
    {
      /* Mark the jump as very unlikely to be taken.  */
      add_reg_br_prob_note (insn,
			    profile_probability::very_unlikely ());

      if (cfun->machine->split_stack_varargs_pointer != NULL_RTX)
	{
	  /* If va_start is used, and __morestack was not called, just use
	     r15.  */
	  emit_move_insn (r1,
			 gen_rtx_PLUS (Pmode, stack_pointer_rtx,
				       GEN_INT (STACK_POINTER_OFFSET)));
	}
    }
  else
    {
      emit_barrier ();
    }

  /* __morestack will call us here.  */

  emit_label (call_done);
}

/* We may have to tell the dataflow pass that the split stack prologue
   is initializing a register.  */

static void
s390_live_on_entry (bitmap regs)
{
  if (cfun->machine->split_stack_varargs_pointer != NULL_RTX)
    {
      gcc_assert (flag_split_stack);
      bitmap_set_bit (regs, 1);
    }
}

/* Return true if the function can use simple_return to return outside
   of a shrink-wrapped region.  At present shrink-wrapping is supported
   in all cases.  */

bool
s390_can_use_simple_return_insn (void)
{
  return true;
}

/* Return true if the epilogue is guaranteed to contain only a return
   instruction and if a direct return can therefore be used instead.
   One of the main advantages of using direct return instructions
   is that we can then use conditional returns.  */

bool
s390_can_use_return_insn (void)
{
  int i;

  if (!reload_completed)
    return false;

  if (crtl->profile)
    return false;

  if (TARGET_TPF_PROFILING)
    return false;

  for (i = 0; i < 16; i++)
    if (cfun_gpr_save_slot (i) != SAVE_SLOT_NONE)
      return false;

  /* For 31 bit this is not covered by the frame_size check below
     since f4, f6 are saved in the register save area without needing
     additional stack space.  */
  if (!TARGET_64BIT
      && (cfun_fpr_save_p (FPR4_REGNUM) || cfun_fpr_save_p (FPR6_REGNUM)))
    return false;

  if (cfun->machine->base_reg
      && !call_used_regs[REGNO (cfun->machine->base_reg)])
    return false;

  return cfun_frame_layout.frame_size == 0;
}

/* The VX ABI differs for vararg functions.  Therefore we need the
   prototype of the callee to be available when passing vector type
   values.  */
static const char *
s390_invalid_arg_for_unprototyped_fn (const_tree typelist, const_tree funcdecl, const_tree val)
{
  return ((TARGET_VX_ABI
	   && typelist == 0
	   && VECTOR_TYPE_P (TREE_TYPE (val))
	   && (funcdecl == NULL_TREE
	       || (TREE_CODE (funcdecl) == FUNCTION_DECL
		   && DECL_BUILT_IN_CLASS (funcdecl) != BUILT_IN_MD)))
	  ? N_("vector argument passed to unprototyped function")
	  : NULL);
}


/* Return the size in bytes of a function argument of
   type TYPE and/or mode MODE.  At least one of TYPE or
   MODE must be specified.  */

static int
s390_function_arg_size (machine_mode mode, const_tree type)
{
  if (type)
    return int_size_in_bytes (type);

  /* No type info available for some library calls ...  */
  if (mode != BLKmode)
    return GET_MODE_SIZE (mode);

  /* If we have neither type nor mode, abort */
  gcc_unreachable ();
}

/* Return true if a variable of TYPE should be passed as single value
   with type CODE. If STRICT_SIZE_CHECK_P is true the sizes of the
   record type and the field type must match.

   The ABI says that record types with a single member are treated
   just like that member would be.  This function is a helper to
   detect such cases.  The function also produces the proper
   diagnostics for cases where the outcome might be different
   depending on the GCC version.  */
static bool
s390_single_field_struct_p (enum tree_code code, const_tree type,
			    bool strict_size_check_p)
{
  int empty_base_seen = 0;
  bool zero_width_bf_skipped_p = false;
  const_tree orig_type = type;

  while (TREE_CODE (type) == RECORD_TYPE)
    {
      tree field, single_type = NULL_TREE;

      for (field = TYPE_FIELDS (type); field; field = DECL_CHAIN (field))
	{
	  if (TREE_CODE (field) != FIELD_DECL)
	    continue;

	  if (DECL_FIELD_ABI_IGNORED (field))
	    {
	      if (lookup_attribute ("no_unique_address",
				    DECL_ATTRIBUTES (field)))
		empty_base_seen |= 2;
	      else
		empty_base_seen |= 1;
	      continue;
	    }

	  if (DECL_FIELD_CXX_ZERO_WIDTH_BIT_FIELD (field))
	    {
	      zero_width_bf_skipped_p = true;
	      continue;
	    }

	  if (single_type == NULL_TREE)
	    single_type = TREE_TYPE (field);
	  else
	    return false;
	}

      if (single_type == NULL_TREE)
	return false;

      /* Reaching this point we have a struct with a single member and
	 zero or more zero-sized bit-fields which have been skipped in the
	 past.  */

      /* If ZERO_WIDTH_BF_SKIPPED_P then the struct will not be accepted.  In case
	 we are not supposed to emit a warning exit early.  */
      if (zero_width_bf_skipped_p && !warn_psabi)
	return false;

      /* If the field declaration adds extra bytes due to padding this
	 is not accepted with STRICT_SIZE_CHECK_P.  */
      if (strict_size_check_p
	  && (int_size_in_bytes (single_type) <= 0
	      || int_size_in_bytes (single_type) != int_size_in_bytes (type)))
	return false;

      type = single_type;
    }

  if (TREE_CODE (type) != code)
    return false;

  if (warn_psabi)
    {
      unsigned uid = TYPE_UID (TYPE_MAIN_VARIANT (orig_type));

      if (empty_base_seen)
	{
	  static unsigned last_reported_type_uid_empty_base;
	  if (uid != last_reported_type_uid_empty_base)
	    {
	      last_reported_type_uid_empty_base = uid;
	      const char *url = CHANGES_ROOT_URL "gcc-10/changes.html#empty_base";
	      if (empty_base_seen & 1)
		inform (input_location,
			"parameter passing for argument of type %qT when C++17 "
			"is enabled changed to match C++14 %{in GCC 10.1%}",
			orig_type, url);
	      else
		inform (input_location,
			"parameter passing for argument of type %qT with "
			"%<[[no_unique_address]]%> members changed "
			"%{in GCC 10.1%}", orig_type, url);
	    }
	}

      /* For C++ older GCCs ignored zero width bitfields and therefore
	 passed structs more often as single values than GCC 12 does.
	 So diagnostics are only required in cases where we do NOT
	 accept the struct to be passed as single value.  */
      if (zero_width_bf_skipped_p)
	{
	  static unsigned last_reported_type_uid_zero_width;
	  if (uid != last_reported_type_uid_zero_width)
	    {
	      last_reported_type_uid_zero_width = uid;
	      inform (input_location,
		      "parameter passing for argument of type %qT with "
		      "zero-width bit fields members changed in GCC 12",
		      orig_type);
	    }
	}
    }

  return !zero_width_bf_skipped_p;
}


/* Return true if a function argument of type TYPE and mode MODE
   is to be passed in a vector register, if available.  */

static bool
s390_function_arg_vector (machine_mode mode, const_tree type)
{
  if (!TARGET_VX_ABI)
    return false;

  if (s390_function_arg_size (mode, type) > 16)
    return false;

  /* No type info available for some library calls ...  */
  if (!type)
    return VECTOR_MODE_P (mode);

  if (!s390_single_field_struct_p (VECTOR_TYPE, type, true))
    return false;

  return true;
}

/* Return true if a function argument of type TYPE and mode MODE
   is to be passed in a floating-point register, if available.  */

static bool
s390_function_arg_float (machine_mode mode, const_tree type)
{
  if (s390_function_arg_size (mode, type) > 8)
    return false;

  /* Soft-float changes the ABI: no floating-point registers are used.  */
  if (TARGET_SOFT_FLOAT)
    return false;

  /* No type info available for some library calls ...  */
  if (!type)
    return mode == SFmode || mode == DFmode || mode == SDmode || mode == DDmode;

  if (!s390_single_field_struct_p (REAL_TYPE, type, false))
    return false;

  return true;
}

/* Return true if a function argument of type TYPE and mode MODE
   is to be passed in an integer register, or a pair of integer
   registers, if available.  */

static bool
s390_function_arg_integer (machine_mode mode, const_tree type)
{
  int size = s390_function_arg_size (mode, type);
  if (size > 8)
    return false;

  /* No type info available for some library calls ...  */
  if (!type)
    return GET_MODE_CLASS (mode) == MODE_INT
	   || (TARGET_SOFT_FLOAT &&  SCALAR_FLOAT_MODE_P (mode));

  /* We accept small integral (and similar) types.  */
  if (INTEGRAL_TYPE_P (type)
      || POINTER_TYPE_P (type)
      || TREE_CODE (type) == NULLPTR_TYPE
      || TREE_CODE (type) == OFFSET_TYPE
      || (TARGET_SOFT_FLOAT && TREE_CODE (type) == REAL_TYPE))
    return true;

  /* We also accept structs of size 1, 2, 4, 8 that are not
     passed in floating-point registers.  */
  if (AGGREGATE_TYPE_P (type)
      && exact_log2 (size) >= 0
      && !s390_function_arg_float (mode, type))
    return true;

  return false;
}

/* Return 1 if a function argument ARG is to be passed by reference.
   The ABI specifies that only structures of size 1, 2, 4, or 8 bytes
   are passed by value, all other structures (and complex numbers) are
   passed by reference.  */

static bool
s390_pass_by_reference (cumulative_args_t, const function_arg_info &arg)
{
  int size = s390_function_arg_size (arg.mode, arg.type);

  if (s390_function_arg_vector (arg.mode, arg.type))
    return false;

  if (size > 8)
    return true;

  if (tree type = arg.type)
    {
      if (AGGREGATE_TYPE_P (type) && exact_log2 (size) < 0)
	return true;

      if (TREE_CODE (type) == COMPLEX_TYPE
	  || TREE_CODE (type) == VECTOR_TYPE)
	return true;
    }

  return false;
}

/* Update the data in CUM to advance over argument ARG.  */

static void
s390_function_arg_advance (cumulative_args_t cum_v,
			   const function_arg_info &arg)
{
  CUMULATIVE_ARGS *cum = get_cumulative_args (cum_v);

  if (s390_function_arg_vector (arg.mode, arg.type))
    {
      /* We are called for unnamed vector stdarg arguments which are
	 passed on the stack.  In this case this hook does not have to
	 do anything since stack arguments are tracked by common
	 code.  */
      if (!arg.named)
	return;
      cum->vrs += 1;
    }
  else if (s390_function_arg_float (arg.mode, arg.type))
    {
      cum->fprs += 1;
    }
  else if (s390_function_arg_integer (arg.mode, arg.type))
    {
      int size = s390_function_arg_size (arg.mode, arg.type);
      cum->gprs += ((size + UNITS_PER_LONG - 1) / UNITS_PER_LONG);
    }
  else
    gcc_unreachable ();
}

/* Define where to put the arguments to a function.
   Value is zero to push the argument on the stack,
   or a hard register in which to store the argument.

   CUM is a variable of type CUMULATIVE_ARGS which gives info about
    the preceding args and about the function being called.
   ARG is a description of the argument.

   On S/390, we use general purpose registers 2 through 6 to
   pass integer, pointer, and certain structure arguments, and
   floating point registers 0 and 2 (0, 2, 4, and 6 on 64-bit)
   to pass floating point arguments.  All remaining arguments
   are pushed to the stack.  */

static rtx
s390_function_arg (cumulative_args_t cum_v, const function_arg_info &arg)
{
  CUMULATIVE_ARGS *cum = get_cumulative_args (cum_v);

  if (!arg.named)
    s390_check_type_for_vector_abi (arg.type, true, false);

  if (s390_function_arg_vector (arg.mode, arg.type))
    {
      /* Vector arguments being part of the ellipsis are passed on the
	 stack.  */
      if (!arg.named || (cum->vrs + 1 > VEC_ARG_NUM_REG))
	return NULL_RTX;

      return gen_rtx_REG (arg.mode, cum->vrs + FIRST_VEC_ARG_REGNO);
    }
  else if (s390_function_arg_float (arg.mode, arg.type))
    {
      if (cum->fprs + 1 > FP_ARG_NUM_REG)
	return NULL_RTX;
      else
	return gen_rtx_REG (arg.mode, cum->fprs + 16);
    }
  else if (s390_function_arg_integer (arg.mode, arg.type))
    {
      int size = s390_function_arg_size (arg.mode, arg.type);
      int n_gprs = (size + UNITS_PER_LONG - 1) / UNITS_PER_LONG;

      if (cum->gprs + n_gprs > GP_ARG_NUM_REG)
	return NULL_RTX;
      else if (n_gprs == 1 || UNITS_PER_WORD == UNITS_PER_LONG)
	return gen_rtx_REG (arg.mode, cum->gprs + 2);
      else if (n_gprs == 2)
	{
	  rtvec p = rtvec_alloc (2);

	  RTVEC_ELT (p, 0)
	    = gen_rtx_EXPR_LIST (SImode, gen_rtx_REG (SImode, cum->gprs + 2),
					 const0_rtx);
	  RTVEC_ELT (p, 1)
	    = gen_rtx_EXPR_LIST (SImode, gen_rtx_REG (SImode, cum->gprs + 3),
					 GEN_INT (4));

	  return gen_rtx_PARALLEL (arg.mode, p);
	}
    }

  /* After the real arguments, expand_call calls us once again with an
     end marker.  Whatever we return here is passed as operand 2 to the
     call expanders.

     We don't need this feature ...  */
  else if (arg.end_marker_p ())
    return const0_rtx;

  gcc_unreachable ();
}

/* Implement TARGET_FUNCTION_ARG_BOUNDARY.  Vector arguments are
   left-justified when placed on the stack during parameter passing.  */

static pad_direction
s390_function_arg_padding (machine_mode mode, const_tree type)
{
  if (s390_function_arg_vector (mode, type))
    return PAD_UPWARD;

  return default_function_arg_padding (mode, type);
}

/* Return true if return values of type TYPE should be returned
   in a memory buffer whose address is passed by the caller as
   hidden first argument.  */

static bool
s390_return_in_memory (const_tree type, const_tree fundecl ATTRIBUTE_UNUSED)
{
  /* We accept small integral (and similar) types.  */
  if (INTEGRAL_TYPE_P (type)
      || POINTER_TYPE_P (type)
      || TREE_CODE (type) == OFFSET_TYPE
      || TREE_CODE (type) == REAL_TYPE)
    return int_size_in_bytes (type) > 8;

  /* vector types which fit into a VR.  */
  if (TARGET_VX_ABI
      && VECTOR_TYPE_P (type)
      && int_size_in_bytes (type) <= 16)
    return false;

  /* Aggregates and similar constructs are always returned
     in memory.  */
  if (AGGREGATE_TYPE_P (type)
      || TREE_CODE (type) == COMPLEX_TYPE
      || VECTOR_TYPE_P (type))
    return true;

  /* ??? We get called on all sorts of random stuff from
     aggregate_value_p.  We can't abort, but it's not clear
     what's safe to return.  Pretend it's a struct I guess.  */
  return true;
}

/* Function arguments and return values are promoted to word size.  */

static machine_mode
s390_promote_function_mode (const_tree type, machine_mode mode,
			    int *punsignedp,
			    const_tree fntype ATTRIBUTE_UNUSED,
			    int for_return ATTRIBUTE_UNUSED)
{
  if (INTEGRAL_MODE_P (mode)
      && GET_MODE_SIZE (mode) < UNITS_PER_LONG)
    {
      if (type != NULL_TREE && POINTER_TYPE_P (type))
	*punsignedp = POINTERS_EXTEND_UNSIGNED;
      return Pmode;
    }

  return mode;
}

/* Define where to return a (scalar) value of type RET_TYPE.
   If RET_TYPE is null, define where to return a (scalar)
   value of mode MODE from a libcall.  */

static rtx
s390_function_and_libcall_value (machine_mode mode,
				 const_tree ret_type,
				 const_tree fntype_or_decl,
				 bool outgoing ATTRIBUTE_UNUSED)
{
  /* For vector return types it is important to use the RET_TYPE
     argument whenever available since the middle-end might have
     changed the mode to a scalar mode.  */
  bool vector_ret_type_p = ((ret_type && VECTOR_TYPE_P (ret_type))
			    || (!ret_type && VECTOR_MODE_P (mode)));

  /* For normal functions perform the promotion as
     promote_function_mode would do.  */
  if (ret_type)
    {
      int unsignedp = TYPE_UNSIGNED (ret_type);
      mode = promote_function_mode (ret_type, mode, &unsignedp,
				    fntype_or_decl, 1);
    }

  gcc_assert (GET_MODE_CLASS (mode) == MODE_INT
	      || SCALAR_FLOAT_MODE_P (mode)
	      || (TARGET_VX_ABI && vector_ret_type_p));
  gcc_assert (GET_MODE_SIZE (mode) <= (TARGET_VX_ABI ? 16 : 8));

  if (TARGET_VX_ABI && vector_ret_type_p)
    return gen_rtx_REG (mode, FIRST_VEC_ARG_REGNO);
  else if (TARGET_HARD_FLOAT && SCALAR_FLOAT_MODE_P (mode))
    return gen_rtx_REG (mode, 16);
  else if (GET_MODE_SIZE (mode) <= UNITS_PER_LONG
	   || UNITS_PER_LONG == UNITS_PER_WORD)
    return gen_rtx_REG (mode, 2);
  else if (GET_MODE_SIZE (mode) == 2 * UNITS_PER_LONG)
    {
      /* This case is triggered when returning a 64 bit value with
	 -m31 -mzarch.  Although the value would fit into a single
	 register it has to be forced into a 32 bit register pair in
	 order to match the ABI.  */
      rtvec p = rtvec_alloc (2);

      RTVEC_ELT (p, 0)
	= gen_rtx_EXPR_LIST (SImode, gen_rtx_REG (SImode, 2), const0_rtx);
      RTVEC_ELT (p, 1)
	= gen_rtx_EXPR_LIST (SImode, gen_rtx_REG (SImode, 3), GEN_INT (4));

      return gen_rtx_PARALLEL (mode, p);
    }

  gcc_unreachable ();
}

/* Define where to return a scalar return value of type RET_TYPE.  */

static rtx
s390_function_value (const_tree ret_type, const_tree fn_decl_or_type,
		     bool outgoing)
{
  return s390_function_and_libcall_value (TYPE_MODE (ret_type), ret_type,
					  fn_decl_or_type, outgoing);
}

/* Define where to return a scalar libcall return value of mode
   MODE.  */

static rtx
s390_libcall_value (machine_mode mode, const_rtx fun ATTRIBUTE_UNUSED)
{
  return s390_function_and_libcall_value (mode, NULL_TREE,
					  NULL_TREE, true);
}


/* Create and return the va_list datatype.

   On S/390, va_list is an array type equivalent to

      typedef struct __va_list_tag
	{
	    long __gpr;
	    long __fpr;
	    void *__overflow_arg_area;
	    void *__reg_save_area;
	} va_list[1];

   where __gpr and __fpr hold the number of general purpose
   or floating point arguments used up to now, respectively,
   __overflow_arg_area points to the stack location of the
   next argument passed on the stack, and __reg_save_area
   always points to the start of the register area in the
   call frame of the current function.  The function prologue
   saves all registers used for argument passing into this
   area if the function uses variable arguments.  */

static tree
s390_build_builtin_va_list (void)
{
  tree f_gpr, f_fpr, f_ovf, f_sav, record, type_decl;

  record = lang_hooks.types.make_type (RECORD_TYPE);

  type_decl =
    build_decl (BUILTINS_LOCATION,
		TYPE_DECL, get_identifier ("__va_list_tag"), record);

  f_gpr = build_decl (BUILTINS_LOCATION,
		      FIELD_DECL, get_identifier ("__gpr"),
		      long_integer_type_node);
  f_fpr = build_decl (BUILTINS_LOCATION,
		      FIELD_DECL, get_identifier ("__fpr"),
		      long_integer_type_node);
  f_ovf = build_decl (BUILTINS_LOCATION,
		      FIELD_DECL, get_identifier ("__overflow_arg_area"),
		      ptr_type_node);
  f_sav = build_decl (BUILTINS_LOCATION,
		      FIELD_DECL, get_identifier ("__reg_save_area"),
		      ptr_type_node);

  va_list_gpr_counter_field = f_gpr;
  va_list_fpr_counter_field = f_fpr;

  DECL_FIELD_CONTEXT (f_gpr) = record;
  DECL_FIELD_CONTEXT (f_fpr) = record;
  DECL_FIELD_CONTEXT (f_ovf) = record;
  DECL_FIELD_CONTEXT (f_sav) = record;

  TYPE_STUB_DECL (record) = type_decl;
  TYPE_NAME (record) = type_decl;
  TYPE_FIELDS (record) = f_gpr;
  DECL_CHAIN (f_gpr) = f_fpr;
  DECL_CHAIN (f_fpr) = f_ovf;
  DECL_CHAIN (f_ovf) = f_sav;

  layout_type (record);

  /* The correct type is an array type of one element.  */
  return build_array_type (record, build_index_type (size_zero_node));
}

/* Implement va_start by filling the va_list structure VALIST.
   STDARG_P is always true, and ignored.
   NEXTARG points to the first anonymous stack argument.

   The following global variables are used to initialize
   the va_list structure:

     crtl->args.info:
       holds number of gprs and fprs used for named arguments.
     crtl->args.arg_offset_rtx:
       holds the offset of the first anonymous stack argument
       (relative to the virtual arg pointer).  */

static void
s390_va_start (tree valist, rtx nextarg ATTRIBUTE_UNUSED)
{
  HOST_WIDE_INT n_gpr, n_fpr;
  int off;
  tree f_gpr, f_fpr, f_ovf, f_sav;
  tree gpr, fpr, ovf, sav, t;

  f_gpr = TYPE_FIELDS (TREE_TYPE (va_list_type_node));
  f_fpr = DECL_CHAIN (f_gpr);
  f_ovf = DECL_CHAIN (f_fpr);
  f_sav = DECL_CHAIN (f_ovf);

  valist = build_simple_mem_ref (valist);
  gpr = build3 (COMPONENT_REF, TREE_TYPE (f_gpr), valist, f_gpr, NULL_TREE);
  fpr = build3 (COMPONENT_REF, TREE_TYPE (f_fpr), valist, f_fpr, NULL_TREE);
  ovf = build3 (COMPONENT_REF, TREE_TYPE (f_ovf), valist, f_ovf, NULL_TREE);
  sav = build3 (COMPONENT_REF, TREE_TYPE (f_sav), valist, f_sav, NULL_TREE);

  /* Count number of gp and fp argument registers used.  */

  n_gpr = crtl->args.info.gprs;
  n_fpr = crtl->args.info.fprs;

  if (cfun->va_list_gpr_size)
    {
      t = build2 (MODIFY_EXPR, TREE_TYPE (gpr), gpr,
		  build_int_cst (NULL_TREE, n_gpr));
      TREE_SIDE_EFFECTS (t) = 1;
      expand_expr (t, const0_rtx, VOIDmode, EXPAND_NORMAL);
    }

  if (cfun->va_list_fpr_size)
    {
      t = build2 (MODIFY_EXPR, TREE_TYPE (fpr), fpr,
		  build_int_cst (NULL_TREE, n_fpr));
      TREE_SIDE_EFFECTS (t) = 1;
      expand_expr (t, const0_rtx, VOIDmode, EXPAND_NORMAL);
    }

  if (flag_split_stack
     && (lookup_attribute ("no_split_stack", DECL_ATTRIBUTES (cfun->decl))
	 == NULL)
     && cfun->machine->split_stack_varargs_pointer == NULL_RTX)
    {
      rtx reg;
      rtx_insn *seq;

      reg = gen_reg_rtx (Pmode);
      cfun->machine->split_stack_varargs_pointer = reg;

      start_sequence ();
      emit_move_insn (reg, gen_rtx_REG (Pmode, 1));
      seq = get_insns ();
      end_sequence ();

      push_topmost_sequence ();
      emit_insn_after (seq, entry_of_function ());
      pop_topmost_sequence ();
    }

  /* Find the overflow area.
     FIXME: This currently is too pessimistic when the vector ABI is
     enabled.  In that case we *always* set up the overflow area
     pointer.  */
  if (n_gpr + cfun->va_list_gpr_size > GP_ARG_NUM_REG
      || n_fpr + cfun->va_list_fpr_size > FP_ARG_NUM_REG
      || TARGET_VX_ABI)
    {
      if (cfun->machine->split_stack_varargs_pointer == NULL_RTX)
	t = make_tree (TREE_TYPE (ovf), virtual_incoming_args_rtx);
      else
	t = make_tree (TREE_TYPE (ovf), cfun->machine->split_stack_varargs_pointer);

      off = INTVAL (crtl->args.arg_offset_rtx);
      off = off < 0 ? 0 : off;
      if (TARGET_DEBUG_ARG)
	fprintf (stderr, "va_start: n_gpr = %d, n_fpr = %d off %d\n",
		 (int)n_gpr, (int)n_fpr, off);

      t = fold_build_pointer_plus_hwi (t, off);

      t = build2 (MODIFY_EXPR, TREE_TYPE (ovf), ovf, t);
      TREE_SIDE_EFFECTS (t) = 1;
      expand_expr (t, const0_rtx, VOIDmode, EXPAND_NORMAL);
    }

  /* Find the register save area.  */
  if ((cfun->va_list_gpr_size && n_gpr < GP_ARG_NUM_REG)
      || (cfun->va_list_fpr_size && n_fpr < FP_ARG_NUM_REG))
    {
      t = make_tree (TREE_TYPE (sav), return_address_pointer_rtx);
      t = fold_build_pointer_plus_hwi (t, -RETURN_REGNUM * UNITS_PER_LONG);

      t = build2 (MODIFY_EXPR, TREE_TYPE (sav), sav, t);
      TREE_SIDE_EFFECTS (t) = 1;
      expand_expr (t, const0_rtx, VOIDmode, EXPAND_NORMAL);
    }
}

/* Implement va_arg by updating the va_list structure
   VALIST as required to retrieve an argument of type
   TYPE, and returning that argument.

   Generates code equivalent to:

   if (integral value) {
     if (size  <= 4 && args.gpr < 5 ||
	 size  > 4 && args.gpr < 4 )
       ret = args.reg_save_area[args.gpr+8]
     else
       ret = *args.overflow_arg_area++;
   } else if (vector value) {
       ret = *args.overflow_arg_area;
       args.overflow_arg_area += size / 8;
   } else if (float value) {
     if (args.fgpr < 2)
       ret = args.reg_save_area[args.fpr+64]
     else
       ret = *args.overflow_arg_area++;
   } else if (aggregate value) {
     if (args.gpr < 5)
       ret = *args.reg_save_area[args.gpr]
     else
       ret = **args.overflow_arg_area++;
   } */

static tree
s390_gimplify_va_arg (tree valist, tree type, gimple_seq *pre_p,
		      gimple_seq *post_p ATTRIBUTE_UNUSED)
{
  tree f_gpr, f_fpr, f_ovf, f_sav;
  tree gpr, fpr, ovf, sav, reg, t, u;
  int indirect_p, size, n_reg, sav_ofs, sav_scale, max_reg;
  tree lab_false, lab_over = NULL_TREE;
  tree addr = create_tmp_var (ptr_type_node, "addr");
  bool left_align_p; /* How a value < UNITS_PER_LONG is aligned within
			a stack slot.  */

  f_gpr = TYPE_FIELDS (TREE_TYPE (va_list_type_node));
  f_fpr = DECL_CHAIN (f_gpr);
  f_ovf = DECL_CHAIN (f_fpr);
  f_sav = DECL_CHAIN (f_ovf);

  gpr = build3 (COMPONENT_REF, TREE_TYPE (f_gpr), valist, f_gpr, NULL_TREE);
  fpr = build3 (COMPONENT_REF, TREE_TYPE (f_fpr), valist, f_fpr, NULL_TREE);
  sav = build3 (COMPONENT_REF, TREE_TYPE (f_sav), valist, f_sav, NULL_TREE);

  /* The tree for args* cannot be shared between gpr/fpr and ovf since
     both appear on a lhs.  */
  valist = unshare_expr (valist);
  ovf = build3 (COMPONENT_REF, TREE_TYPE (f_ovf), valist, f_ovf, NULL_TREE);

  size = int_size_in_bytes (type);

  s390_check_type_for_vector_abi (type, true, false);

  if (pass_va_arg_by_reference (type))
    {
      if (TARGET_DEBUG_ARG)
	{
	  fprintf (stderr, "va_arg: aggregate type");
	  debug_tree (type);
	}

      /* Aggregates are passed by reference.  */
      indirect_p = 1;
      reg = gpr;
      n_reg = 1;

      /* kernel stack layout on 31 bit: It is assumed here that no padding
	 will be added by s390_frame_info because for va_args always an even
	 number of gprs has to be saved r15-r2 = 14 regs.  */
      sav_ofs = 2 * UNITS_PER_LONG;
      sav_scale = UNITS_PER_LONG;
      size = UNITS_PER_LONG;
      max_reg = GP_ARG_NUM_REG - n_reg;
      left_align_p = false;
    }
  else if (s390_function_arg_vector (TYPE_MODE (type), type))
    {
      if (TARGET_DEBUG_ARG)
	{
	  fprintf (stderr, "va_arg: vector type");
	  debug_tree (type);
	}

      indirect_p = 0;
      reg = NULL_TREE;
      n_reg = 0;
      sav_ofs = 0;
      sav_scale = 8;
      max_reg = 0;
      left_align_p = true;
    }
  else if (s390_function_arg_float (TYPE_MODE (type), type))
    {
      if (TARGET_DEBUG_ARG)
	{
	  fprintf (stderr, "va_arg: float type");
	  debug_tree (type);
	}

      /* FP args go in FP registers, if present.  */
      indirect_p = 0;
      reg = fpr;
      n_reg = 1;
      sav_ofs = 16 * UNITS_PER_LONG;
      sav_scale = 8;
      max_reg = FP_ARG_NUM_REG - n_reg;
      left_align_p = false;
    }
  else
    {
      if (TARGET_DEBUG_ARG)
	{
	  fprintf (stderr, "va_arg: other type");
	  debug_tree (type);
	}

      /* Otherwise into GP registers.  */
      indirect_p = 0;
      reg = gpr;
      n_reg = (size + UNITS_PER_LONG - 1) / UNITS_PER_LONG;

      /* kernel stack layout on 31 bit: It is assumed here that no padding
	 will be added by s390_frame_info because for va_args always an even
	 number of gprs has to be saved r15-r2 = 14 regs.  */
      sav_ofs = 2 * UNITS_PER_LONG;

      if (size < UNITS_PER_LONG)
	sav_ofs += UNITS_PER_LONG - size;

      sav_scale = UNITS_PER_LONG;
      max_reg = GP_ARG_NUM_REG - n_reg;
      left_align_p = false;
    }

  /* Pull the value out of the saved registers ...  */

  if (reg != NULL_TREE)
    {
      /*
	if (reg > ((typeof (reg))max_reg))
	  goto lab_false;

	addr = sav + sav_ofs + reg * save_scale;

	goto lab_over;

	lab_false:
      */

      lab_false = create_artificial_label (UNKNOWN_LOCATION);
      lab_over = create_artificial_label (UNKNOWN_LOCATION);

      t = fold_convert (TREE_TYPE (reg), size_int (max_reg));
      t = build2 (GT_EXPR, boolean_type_node, reg, t);
      u = build1 (GOTO_EXPR, void_type_node, lab_false);
      t = build3 (COND_EXPR, void_type_node, t, u, NULL_TREE);
      gimplify_and_add (t, pre_p);

      t = fold_build_pointer_plus_hwi (sav, sav_ofs);
      u = build2 (MULT_EXPR, TREE_TYPE (reg), reg,
		  fold_convert (TREE_TYPE (reg), size_int (sav_scale)));
      t = fold_build_pointer_plus (t, u);

      gimplify_assign (addr, t, pre_p);

      gimple_seq_add_stmt (pre_p, gimple_build_goto (lab_over));

      gimple_seq_add_stmt (pre_p, gimple_build_label (lab_false));
    }

  /* ... Otherwise out of the overflow area.  */

  t = ovf;
  if (size < UNITS_PER_LONG && !left_align_p)
    t = fold_build_pointer_plus_hwi (t, UNITS_PER_LONG - size);

  gimplify_expr (&t, pre_p, NULL, is_gimple_val, fb_rvalue);

  gimplify_assign (addr, t, pre_p);

  if (size < UNITS_PER_LONG && left_align_p)
    t = fold_build_pointer_plus_hwi (t, UNITS_PER_LONG);
  else
    t = fold_build_pointer_plus_hwi (t, size);

  gimplify_assign (ovf, t, pre_p);

  if (reg != NULL_TREE)
    gimple_seq_add_stmt (pre_p, gimple_build_label (lab_over));


  /* Increment register save count.  */

  if (n_reg > 0)
    {
      u = build2 (PREINCREMENT_EXPR, TREE_TYPE (reg), reg,
		  fold_convert (TREE_TYPE (reg), size_int (n_reg)));
      gimplify_and_add (u, pre_p);
    }

  if (indirect_p)
    {
      t = build_pointer_type_for_mode (build_pointer_type (type),
				       ptr_mode, true);
      addr = fold_convert (t, addr);
      addr = build_va_arg_indirect_ref (addr);
    }
  else
    {
      t = build_pointer_type_for_mode (type, ptr_mode, true);
      addr = fold_convert (t, addr);
    }

  return build_va_arg_indirect_ref (addr);
}

/* Emit rtl for the tbegin or tbegin_retry (RETRY != NULL_RTX)
   expanders.
   DEST  - Register location where CC will be stored.
   TDB   - Pointer to a 256 byte area where to store the transaction.
	   diagnostic block. NULL if TDB is not needed.
   RETRY - Retry count value.  If non-NULL a retry loop for CC2
	   is emitted
   CLOBBER_FPRS_P - If true clobbers for all FPRs are emitted as part
		    of the tbegin instruction pattern.  */

void
s390_expand_tbegin (rtx dest, rtx tdb, rtx retry, bool clobber_fprs_p)
{
  rtx retry_plus_two = gen_reg_rtx (SImode);
  rtx retry_reg = gen_reg_rtx (SImode);
  rtx_code_label *retry_label = NULL;

  if (retry != NULL_RTX)
    {
      emit_move_insn (retry_reg, retry);
      emit_insn (gen_addsi3 (retry_plus_two, retry_reg, const2_rtx));
      emit_insn (gen_addsi3 (retry_reg, retry_reg, const1_rtx));
      retry_label = gen_label_rtx ();
      emit_label (retry_label);
    }

  if (clobber_fprs_p)
    {
      if (TARGET_VX)
	emit_insn (gen_tbegin_1_z13 (gen_rtx_CONST_INT (VOIDmode, TBEGIN_MASK),
				     tdb));
      else
	emit_insn (gen_tbegin_1 (gen_rtx_CONST_INT (VOIDmode, TBEGIN_MASK),
				 tdb));
    }
  else
    emit_insn (gen_tbegin_nofloat_1 (gen_rtx_CONST_INT (VOIDmode, TBEGIN_MASK),
				     tdb));

  emit_move_insn (dest, gen_rtx_UNSPEC (SImode,
					gen_rtvec (1, gen_rtx_REG (CCRAWmode,
								   CC_REGNUM)),
					UNSPEC_CC_TO_INT));
  if (retry != NULL_RTX)
    {
      const int CC0 = 1 << 3;
      const int CC1 = 1 << 2;
      const int CC3 = 1 << 0;
      rtx jump;
      rtx count = gen_reg_rtx (SImode);
      rtx_code_label *leave_label = gen_label_rtx ();

      /* Exit for success and permanent failures.  */
      jump = s390_emit_jump (leave_label,
			     gen_rtx_EQ (VOIDmode,
			       gen_rtx_REG (CCRAWmode, CC_REGNUM),
			       gen_rtx_CONST_INT (VOIDmode, CC0 | CC1 | CC3)));
      LABEL_NUSES (leave_label) = 1;

      /* CC2 - transient failure. Perform retry with ppa.  */
      emit_move_insn (count, retry_plus_two);
      emit_insn (gen_subsi3 (count, count, retry_reg));
      emit_insn (gen_tx_assist (count));
      jump = emit_jump_insn (gen_doloop_si64 (retry_label,
					      retry_reg,
					      retry_reg));
      JUMP_LABEL (jump) = retry_label;
      LABEL_NUSES (retry_label) = 1;
      emit_label (leave_label);
    }
}


/* Return the decl for the target specific builtin with the function
   code FCODE.  */

static tree
s390_builtin_decl (unsigned fcode, bool initialized_p ATTRIBUTE_UNUSED)
{
  if (fcode >= S390_BUILTIN_MAX)
    return error_mark_node;

  return s390_builtin_decls[fcode];
}

/* We call mcount before the function prologue.  So a profiled leaf
   function should stay a leaf function.  */

static bool
s390_keep_leaf_when_profiled ()
{
  return true;
}

/* Output assembly code for the trampoline template to
   stdio stream FILE.

   On S/390, we use gpr 1 internally in the trampoline code;
   gpr 0 is used to hold the static chain.  */

static void
s390_asm_trampoline_template (FILE *file)
{
  rtx op[2];
  op[0] = gen_rtx_REG (Pmode, 0);
  op[1] = gen_rtx_REG (Pmode, 1);

  if (TARGET_64BIT)
    {
      output_asm_insn ("basr\t%1,0", op);         /* 2 byte */
      output_asm_insn ("lmg\t%0,%1,14(%1)", op);  /* 6 byte */
      output_asm_insn ("br\t%1", op);             /* 2 byte */
      ASM_OUTPUT_SKIP (file, (HOST_WIDE_INT)(TRAMPOLINE_SIZE - 10));
    }
  else
    {
      output_asm_insn ("basr\t%1,0", op);         /* 2 byte */
      output_asm_insn ("lm\t%0,%1,6(%1)", op);    /* 4 byte */
      output_asm_insn ("br\t%1", op);             /* 2 byte */
      ASM_OUTPUT_SKIP (file, (HOST_WIDE_INT)(TRAMPOLINE_SIZE - 8));
    }
}

/* Emit RTL insns to initialize the variable parts of a trampoline.
   FNADDR is an RTX for the address of the function's pure code.
   CXT is an RTX for the static chain value for the function.  */

static void
s390_trampoline_init (rtx m_tramp, tree fndecl, rtx cxt)
{
  rtx fnaddr = XEXP (DECL_RTL (fndecl), 0);
  rtx mem;

  emit_block_move (m_tramp, assemble_trampoline_template (),
		   GEN_INT (2 * UNITS_PER_LONG), BLOCK_OP_NORMAL);

  mem = adjust_address (m_tramp, Pmode, 2 * UNITS_PER_LONG);
  emit_move_insn (mem, cxt);
  mem = adjust_address (m_tramp, Pmode, 3 * UNITS_PER_LONG);
  emit_move_insn (mem, fnaddr);
}

static void
output_asm_nops (const char *user, int hw)
{
  asm_fprintf (asm_out_file, "\t# NOPs for %s (%d halfwords)\n", user, hw);
  while (hw > 0)
    {
      if (hw >= 3)
	{
	  output_asm_insn ("brcl\t0,0", NULL);
	  hw -= 3;
	}
      else if (hw >= 2)
	{
	  output_asm_insn ("bc\t0,0", NULL);
	  hw -= 2;
	}
      else
	{
	  output_asm_insn ("bcr\t0,0", NULL);
	  hw -= 1;
	}
    }
}

/* Output assembler code to FILE to call a profiler hook.  */

void
s390_function_profiler (FILE *file, int labelno ATTRIBUTE_UNUSED)
{
  rtx op[4];

  fprintf (file, "# function profiler \n");

  op[0] = gen_rtx_REG (Pmode, RETURN_REGNUM);
  op[1] = gen_rtx_REG (Pmode, STACK_POINTER_REGNUM);
  op[1] = gen_rtx_MEM (Pmode, plus_constant (Pmode, op[1], UNITS_PER_LONG));
  op[3] = GEN_INT (UNITS_PER_LONG);

  op[2] = gen_rtx_SYMBOL_REF (Pmode, flag_fentry ? "__fentry__" : "_mcount");
  SYMBOL_REF_FLAGS (op[2]) |= SYMBOL_FLAG_FUNCTION;
  if (flag_pic && !TARGET_64BIT)
    {
      op[2] = gen_rtx_UNSPEC (Pmode, gen_rtvec (1, op[2]), UNSPEC_PLT31);
      op[2] = gen_rtx_CONST (Pmode, op[2]);
    }

  if (flag_record_mcount)
    fprintf (file, "1:\n");

  if (flag_fentry)
    {
      if (flag_nop_mcount)
	output_asm_nops ("-mnop-mcount", /* brasl */ 3);
      else if (cfun->static_chain_decl)
	warning (OPT_Wcannot_profile, "nested functions cannot be profiled "
		 "with %<-mfentry%> on s390");
      else
	output_asm_insn ("brasl\t0,%2%K2", op);
    }
  else if (TARGET_64BIT)
    {
      if (flag_nop_mcount)
	output_asm_nops ("-mnop-mcount", /* stg */ 3 + /* brasl */ 3 +
			 /* lg */ 3);
      else
	{
	  output_asm_insn ("stg\t%0,%1", op);
	  if (flag_dwarf2_cfi_asm)
	    output_asm_insn (".cfi_rel_offset\t%0,%3", op);
	  output_asm_insn ("brasl\t%0,%2%K2", op);
	  output_asm_insn ("lg\t%0,%1", op);
	  if (flag_dwarf2_cfi_asm)
	    output_asm_insn (".cfi_restore\t%0", op);
	}
    }
  else
    {
      if (flag_nop_mcount)
	output_asm_nops ("-mnop-mcount", /* st */ 2 + /* brasl */ 3 +
			 /* l */ 2);
      else
	{
	  output_asm_insn ("st\t%0,%1", op);
	  if (flag_dwarf2_cfi_asm)
	    output_asm_insn (".cfi_rel_offset\t%0,%3", op);
	  output_asm_insn ("brasl\t%0,%2%K2", op);
	  output_asm_insn ("l\t%0,%1", op);
	  if (flag_dwarf2_cfi_asm)
	    output_asm_insn (".cfi_restore\t%0", op);
	}
    }

  if (flag_record_mcount)
    {
      fprintf (file, "\t.section __mcount_loc, \"a\",@progbits\n");
      fprintf (file, "\t.%s 1b\n", TARGET_64BIT ? "quad" : "long");
      fprintf (file, "\t.previous\n");
    }
}

/* Encode symbol attributes (local vs. global, tls model) of a SYMBOL_REF
   into its SYMBOL_REF_FLAGS.  */

static void
s390_encode_section_info (tree decl, rtx rtl, int first)
{
  default_encode_section_info (decl, rtl, first);

  if (TREE_CODE (decl) == VAR_DECL)
    {
      /* Store the alignment to be able to check if we can use
	 a larl/load-relative instruction.  We only handle the cases
	 that can go wrong (i.e. no FUNC_DECLs).  */
      if (DECL_ALIGN (decl) == 0 || DECL_ALIGN (decl) % 16)
	SYMBOL_FLAG_SET_NOTALIGN2 (XEXP (rtl, 0));
      else if (DECL_ALIGN (decl) % 32)
	SYMBOL_FLAG_SET_NOTALIGN4 (XEXP (rtl, 0));
      else if (DECL_ALIGN (decl) % 64)
	SYMBOL_FLAG_SET_NOTALIGN8 (XEXP (rtl, 0));
    }

  /* Literal pool references don't have a decl so they are handled
     differently here.  We rely on the information in the MEM_ALIGN
     entry to decide upon the alignment.  */
  if (MEM_P (rtl)
      && GET_CODE (XEXP (rtl, 0)) == SYMBOL_REF
      && TREE_CONSTANT_POOL_ADDRESS_P (XEXP (rtl, 0)))
    {
      if (MEM_ALIGN (rtl) == 0 || MEM_ALIGN (rtl) % 16)
	SYMBOL_FLAG_SET_NOTALIGN2 (XEXP (rtl, 0));
      else if (MEM_ALIGN (rtl) % 32)
	SYMBOL_FLAG_SET_NOTALIGN4 (XEXP (rtl, 0));
      else if (MEM_ALIGN (rtl) % 64)
	SYMBOL_FLAG_SET_NOTALIGN8 (XEXP (rtl, 0));
    }
}

/* Output thunk to FILE that implements a C++ virtual function call (with
   multiple inheritance) to FUNCTION.  The thunk adjusts the this pointer
   by DELTA, and unless VCALL_OFFSET is zero, applies an additional adjustment
   stored at VCALL_OFFSET in the vtable whose address is located at offset 0
   relative to the resulting this pointer.  */

static void
s390_output_mi_thunk (FILE *file, tree thunk ATTRIBUTE_UNUSED,
		      HOST_WIDE_INT delta, HOST_WIDE_INT vcall_offset,
		      tree function)
{
  const char *fnname = IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (thunk));
  rtx op[10];
  int nonlocal = 0;

  assemble_start_function (thunk, fnname);
  /* Make sure unwind info is emitted for the thunk if needed.  */
  final_start_function (emit_barrier (), file, 1);

  /* Operand 0 is the target function.  */
  op[0] = XEXP (DECL_RTL (function), 0);
  if (flag_pic && !SYMBOL_REF_LOCAL_P (op[0]))
    {
      nonlocal = 1;
      if (!TARGET_64BIT)
	{
	  op[0] = gen_rtx_UNSPEC (Pmode, gen_rtvec (1, op[0]), UNSPEC_GOT);
	  op[0] = gen_rtx_CONST (Pmode, op[0]);
	}
    }

  /* Operand 1 is the 'this' pointer.  */
  if (aggregate_value_p (TREE_TYPE (TREE_TYPE (function)), function))
    op[1] = gen_rtx_REG (Pmode, 3);
  else
    op[1] = gen_rtx_REG (Pmode, 2);

  /* Operand 2 is the delta.  */
  op[2] = GEN_INT (delta);

  /* Operand 3 is the vcall_offset.  */
  op[3] = GEN_INT (vcall_offset);

  /* Operand 4 is the temporary register.  */
  op[4] = gen_rtx_REG (Pmode, 1);

  /* Operands 5 to 8 can be used as labels.  */
  op[5] = NULL_RTX;
  op[6] = NULL_RTX;
  op[7] = NULL_RTX;
  op[8] = NULL_RTX;

  /* Operand 9 can be used for temporary register.  */
  op[9] = NULL_RTX;

  /* Generate code.  */
  if (TARGET_64BIT)
    {
      /* Setup literal pool pointer if required.  */
      if ((!DISP_IN_RANGE (delta)
	   && !CONST_OK_FOR_K (delta)
	   && !CONST_OK_FOR_Os (delta))
	  || (!DISP_IN_RANGE (vcall_offset)
	      && !CONST_OK_FOR_K (vcall_offset)
	      && !CONST_OK_FOR_Os (vcall_offset)))
	{
	  op[5] = gen_label_rtx ();
	  output_asm_insn ("larl\t%4,%5", op);
	}

      /* Add DELTA to this pointer.  */
      if (delta)
	{
	  if (CONST_OK_FOR_J (delta))
	    output_asm_insn ("la\t%1,%2(%1)", op);
	  else if (DISP_IN_RANGE (delta))
	    output_asm_insn ("lay\t%1,%2(%1)", op);
	  else if (CONST_OK_FOR_K (delta))
	    output_asm_insn ("aghi\t%1,%2", op);
	  else if (CONST_OK_FOR_Os (delta))
	    output_asm_insn ("agfi\t%1,%2", op);
	  else
	    {
	      op[6] = gen_label_rtx ();
	      output_asm_insn ("agf\t%1,%6-%5(%4)", op);
	    }
	}

      /* Perform vcall adjustment.  */
      if (vcall_offset)
	{
	  if (DISP_IN_RANGE (vcall_offset))
	    {
	      output_asm_insn ("lg\t%4,0(%1)", op);
	      output_asm_insn ("ag\t%1,%3(%4)", op);
	    }
	  else if (CONST_OK_FOR_K (vcall_offset))
	    {
	      output_asm_insn ("lghi\t%4,%3", op);
	      output_asm_insn ("ag\t%4,0(%1)", op);
	      output_asm_insn ("ag\t%1,0(%4)", op);
	    }
	  else if (CONST_OK_FOR_Os (vcall_offset))
	    {
	      output_asm_insn ("lgfi\t%4,%3", op);
	      output_asm_insn ("ag\t%4,0(%1)", op);
	      output_asm_insn ("ag\t%1,0(%4)", op);
	    }
	  else
	    {
	      op[7] = gen_label_rtx ();
	      output_asm_insn ("llgf\t%4,%7-%5(%4)", op);
	      output_asm_insn ("ag\t%4,0(%1)", op);
	      output_asm_insn ("ag\t%1,0(%4)", op);
	    }
	}

      /* Jump to target.  */
      output_asm_insn ("jg\t%0%K0", op);

      /* Output literal pool if required.  */
      if (op[5])
	{
	  output_asm_insn (".align\t4", op);
	  targetm.asm_out.internal_label (file, "L",
					  CODE_LABEL_NUMBER (op[5]));
	}
      if (op[6])
	{
	  targetm.asm_out.internal_label (file, "L",
					  CODE_LABEL_NUMBER (op[6]));
	  output_asm_insn (".long\t%2", op);
	}
      if (op[7])
	{
	  targetm.asm_out.internal_label (file, "L",
					  CODE_LABEL_NUMBER (op[7]));
	  output_asm_insn (".long\t%3", op);
	}
    }
  else
    {
      /* Setup base pointer if required.  */
      if (!vcall_offset
	  || (!DISP_IN_RANGE (delta)
	      && !CONST_OK_FOR_K (delta)
	      && !CONST_OK_FOR_Os (delta))
	  || (!DISP_IN_RANGE (delta)
	      && !CONST_OK_FOR_K (vcall_offset)
	      && !CONST_OK_FOR_Os (vcall_offset)))
	{
	  op[5] = gen_label_rtx ();
	  output_asm_insn ("basr\t%4,0", op);
	  targetm.asm_out.internal_label (file, "L",
					  CODE_LABEL_NUMBER (op[5]));
	}

      /* Add DELTA to this pointer.  */
      if (delta)
	{
	  if (CONST_OK_FOR_J (delta))
	    output_asm_insn ("la\t%1,%2(%1)", op);
	  else if (DISP_IN_RANGE (delta))
	    output_asm_insn ("lay\t%1,%2(%1)", op);
	  else if (CONST_OK_FOR_K (delta))
	    output_asm_insn ("ahi\t%1,%2", op);
	  else if (CONST_OK_FOR_Os (delta))
	    output_asm_insn ("afi\t%1,%2", op);
	  else
	    {
	      op[6] = gen_label_rtx ();
	      output_asm_insn ("a\t%1,%6-%5(%4)", op);
	    }
	}

      /* Perform vcall adjustment.  */
      if (vcall_offset)
	{
	  if (CONST_OK_FOR_J (vcall_offset))
	    {
	      output_asm_insn ("l\t%4,0(%1)", op);
	      output_asm_insn ("a\t%1,%3(%4)", op);
	    }
	  else if (DISP_IN_RANGE (vcall_offset))
	    {
	      output_asm_insn ("l\t%4,0(%1)", op);
	      output_asm_insn ("ay\t%1,%3(%4)", op);
	    }
	  else if (CONST_OK_FOR_K (vcall_offset))
	    {
	      output_asm_insn ("lhi\t%4,%3", op);
	      output_asm_insn ("a\t%4,0(%1)", op);
	      output_asm_insn ("a\t%1,0(%4)", op);
	    }
	  else if (CONST_OK_FOR_Os (vcall_offset))
	    {
	      output_asm_insn ("iilf\t%4,%3", op);
	      output_asm_insn ("a\t%4,0(%1)", op);
	      output_asm_insn ("a\t%1,0(%4)", op);
	    }
	  else
	    {
	      op[7] = gen_label_rtx ();
	      output_asm_insn ("l\t%4,%7-%5(%4)", op);
	      output_asm_insn ("a\t%4,0(%1)", op);
	      output_asm_insn ("a\t%1,0(%4)", op);
	    }

	  /* We had to clobber the base pointer register.
	     Re-setup the base pointer (with a different base).  */
	  op[5] = gen_label_rtx ();
	  output_asm_insn ("basr\t%4,0", op);
	  targetm.asm_out.internal_label (file, "L",
					  CODE_LABEL_NUMBER (op[5]));
	}

      /* Jump to target.  */
      op[8] = gen_label_rtx ();

      if (!flag_pic)
	output_asm_insn ("l\t%4,%8-%5(%4)", op);
      else if (!nonlocal)
	output_asm_insn ("a\t%4,%8-%5(%4)", op);
      /* We cannot call through .plt, since .plt requires %r12 loaded.  */
      else if (flag_pic == 1)
	{
	  output_asm_insn ("a\t%4,%8-%5(%4)", op);
	  output_asm_insn ("l\t%4,%0(%4)", op);
	}
      else if (flag_pic == 2)
	{
	  op[9] = gen_rtx_REG (Pmode, 0);
	  output_asm_insn ("l\t%9,%8-4-%5(%4)", op);
	  output_asm_insn ("a\t%4,%8-%5(%4)", op);
	  output_asm_insn ("ar\t%4,%9", op);
	  output_asm_insn ("l\t%4,0(%4)", op);
	}

      output_asm_insn ("br\t%4", op);

      /* Output literal pool.  */
      output_asm_insn (".align\t4", op);

      if (nonlocal && flag_pic == 2)
	output_asm_insn (".long\t%0", op);
      if (nonlocal)
	{
	  op[0] = gen_rtx_SYMBOL_REF (Pmode, "_GLOBAL_OFFSET_TABLE_");
	  SYMBOL_REF_FLAGS (op[0]) = SYMBOL_FLAG_LOCAL;
	}

      targetm.asm_out.internal_label (file, "L", CODE_LABEL_NUMBER (op[8]));
      if (!flag_pic)
	output_asm_insn (".long\t%0", op);
      else
	output_asm_insn (".long\t%0-%5", op);

      if (op[6])
	{
	  targetm.asm_out.internal_label (file, "L",
					  CODE_LABEL_NUMBER (op[6]));
	  output_asm_insn (".long\t%2", op);
	}
      if (op[7])
	{
	  targetm.asm_out.internal_label (file, "L",
					  CODE_LABEL_NUMBER (op[7]));
	  output_asm_insn (".long\t%3", op);
	}
    }
  final_end_function ();
  assemble_end_function (thunk, fnname);
}

/* Output either an indirect jump or an indirect call
   (RETURN_ADDR_REGNO != INVALID_REGNUM) with target register REGNO
   using a branch trampoline disabling branch target prediction.  */

void
s390_indirect_branch_via_thunk (unsigned int regno,
				unsigned int return_addr_regno,
				rtx comparison_operator,
				enum s390_indirect_branch_type type)
{
  enum s390_indirect_branch_option option;

  if (type == s390_indirect_branch_type_return)
    {
      if (s390_return_addr_from_memory ())
	option = s390_opt_function_return_mem;
      else
	option = s390_opt_function_return_reg;
    }
  else if (type == s390_indirect_branch_type_jump)
    option = s390_opt_indirect_branch_jump;
  else if (type == s390_indirect_branch_type_call)
    option = s390_opt_indirect_branch_call;
  else
    gcc_unreachable ();

  if (TARGET_INDIRECT_BRANCH_TABLE)
    {
      char label[32];

      ASM_GENERATE_INTERNAL_LABEL (label,
				   indirect_branch_table_label[option],
				   indirect_branch_table_label_no[option]++);
      ASM_OUTPUT_LABEL (asm_out_file, label);
    }

  if (return_addr_regno != INVALID_REGNUM)
    {
      gcc_assert (comparison_operator == NULL_RTX);
      fprintf (asm_out_file, " \tbrasl\t%%r%d,", return_addr_regno);
    }
  else
    {
      fputs (" \tjg", asm_out_file);
      if (comparison_operator != NULL_RTX)
	print_operand (asm_out_file, comparison_operator, 'C');

      fputs ("\t", asm_out_file);
    }

  if (TARGET_CPU_Z10)
    fprintf (asm_out_file,
	     TARGET_INDIRECT_BRANCH_THUNK_NAME_EXRL "\n",
	     regno);
  else
    fprintf (asm_out_file,
	     TARGET_INDIRECT_BRANCH_THUNK_NAME_EX "\n",
	     INDIRECT_BRANCH_THUNK_REGNUM, regno);

  if ((option == s390_opt_indirect_branch_jump
       && cfun->machine->indirect_branch_jump == indirect_branch_thunk)
      || (option == s390_opt_indirect_branch_call
	  && cfun->machine->indirect_branch_call == indirect_branch_thunk)
      || (option == s390_opt_function_return_reg
	  && cfun->machine->function_return_reg == indirect_branch_thunk)
      || (option == s390_opt_function_return_mem
	  && cfun->machine->function_return_mem == indirect_branch_thunk))
    {
      if (TARGET_CPU_Z10)
	indirect_branch_z10thunk_mask |= (1 << regno);
      else
	indirect_branch_prez10thunk_mask |= (1 << regno);
    }
}

/* Output an inline thunk for indirect jumps.  EXECUTE_TARGET can
   either be an address register or a label pointing to the location
   of the jump instruction.  */

void
s390_indirect_branch_via_inline_thunk (rtx execute_target)
{
  if (TARGET_INDIRECT_BRANCH_TABLE)
    {
      char label[32];

      ASM_GENERATE_INTERNAL_LABEL (label,
				   indirect_branch_table_label[s390_opt_indirect_branch_jump],
				   indirect_branch_table_label_no[s390_opt_indirect_branch_jump]++);
      ASM_OUTPUT_LABEL (asm_out_file, label);
    }

  if (!TARGET_ZARCH)
    fputs ("\t.machinemode zarch\n", asm_out_file);

  if (REG_P (execute_target))
    fprintf (asm_out_file, "\tex\t%%r0,0(%%r%d)\n", REGNO (execute_target));
  else
    output_asm_insn ("\texrl\t%%r0,%0", &execute_target);

  if (!TARGET_ZARCH)
    fputs ("\t.machinemode esa\n", asm_out_file);

  fputs ("0:\tj\t0b\n", asm_out_file);
}

static bool
s390_valid_pointer_mode (scalar_int_mode mode)
{
  return (mode == SImode || (TARGET_64BIT && mode == DImode));
}

/* Checks whether the given CALL_EXPR would use a caller
   saved register.  This is used to decide whether sibling call
   optimization could be performed on the respective function
   call.  */

static bool
s390_call_saved_register_used (tree call_expr)
{
  CUMULATIVE_ARGS cum_v;
  cumulative_args_t cum;
  tree parameter;
  rtx parm_rtx;
  int reg, i;

  INIT_CUMULATIVE_ARGS (cum_v, NULL, NULL, 0, 0);
  cum = pack_cumulative_args (&cum_v);

  for (i = 0; i < call_expr_nargs (call_expr); i++)
    {
      parameter = CALL_EXPR_ARG (call_expr, i);
      gcc_assert (parameter);

      /* For an undeclared variable passed as parameter we will get
	 an ERROR_MARK node here.  */
      if (TREE_CODE (parameter) == ERROR_MARK)
	return true;

      /* We assume that in the target function all parameters are
	 named.  This only has an impact on vector argument register
	 usage none of which is call-saved.  */
      function_arg_info arg (TREE_TYPE (parameter), /*named=*/true);
      apply_pass_by_reference_rules (&cum_v, arg);

      parm_rtx = s390_function_arg (cum, arg);

      s390_function_arg_advance (cum, arg);

      if (!parm_rtx)
	continue;

      if (REG_P (parm_rtx))
	{
	  int size = s390_function_arg_size (arg.mode, arg.type);
	  int nregs = (size + UNITS_PER_LONG - 1) / UNITS_PER_LONG;

	  for (reg = 0; reg < nregs; reg++)
	    if (!call_used_or_fixed_reg_p (reg + REGNO (parm_rtx)))
	      return true;
	}
      else if (GET_CODE (parm_rtx) == PARALLEL)
	{
	  int i;

	  for (i = 0; i < XVECLEN (parm_rtx, 0); i++)
	    {
	      rtx r = XEXP (XVECEXP (parm_rtx, 0, i), 0);

	      gcc_assert (REG_P (r));
	      gcc_assert (REG_NREGS (r) == 1);

	      if (!call_used_or_fixed_reg_p (REGNO (r)))
		return true;
	    }
	}
    }
  return false;
}

/* Return true if the given call expression can be
   turned into a sibling call.
   DECL holds the declaration of the function to be called whereas
   EXP is the call expression itself.  */

static bool
s390_function_ok_for_sibcall (tree decl, tree exp)
{
  /* The TPF epilogue uses register 1.  */
  if (TARGET_TPF_PROFILING)
    return false;

  /* The 31 bit PLT code uses register 12 (GOT pointer - caller saved)
     which would have to be restored before the sibcall.  */
  if (!TARGET_64BIT && flag_pic && decl && !targetm.binds_local_p (decl))
    return false;

  /* The thunks for indirect branches require r1 if no exrl is
     available.  r1 might not be available when doing a sibling
     call.  */
  if (TARGET_INDIRECT_BRANCH_NOBP_CALL
      && !TARGET_CPU_Z10
      && !decl)
    return false;

  /* Register 6 on s390 is available as an argument register but unfortunately
     "caller saved". This makes functions needing this register for arguments
     not suitable for sibcalls.  */
  return !s390_call_saved_register_used (exp);
}

/* Return the fixed registers used for condition codes.  */

static bool
s390_fixed_condition_code_regs (unsigned int *p1, unsigned int *p2)
{
  *p1 = CC_REGNUM;
  *p2 = INVALID_REGNUM;

  return true;
}

/* This function is used by the call expanders of the machine description.
   It emits the call insn itself together with the necessary operations
   to adjust the target address and returns the emitted insn.
   ADDR_LOCATION is the target address rtx
   TLS_CALL the location of the thread-local symbol
   RESULT_REG the register where the result of the call should be stored
   RETADDR_REG the register where the return address should be stored
	       If this parameter is NULL_RTX the call is considered
	       to be a sibling call.  */

rtx_insn *
s390_emit_call (rtx addr_location, rtx tls_call, rtx result_reg,
		rtx retaddr_reg)
{
  bool plt31_call_p = false;
  rtx_insn *insn;
  rtx vec[4] = { NULL_RTX };
  int elts = 0;
  rtx *call = &vec[0];
  rtx *clobber_ret_reg = &vec[1];
  rtx *use = &vec[2];
  rtx *clobber_thunk_reg = &vec[3];
  int i;

  /* Direct function calls need special treatment.  */
  if (GET_CODE (addr_location) == SYMBOL_REF)
    {
      /* When calling a global routine in PIC mode, we must
	 replace the symbol itself with the PLT stub.  */
      if (flag_pic && !SYMBOL_REF_LOCAL_P (addr_location) && !TARGET_64BIT)
	{
	  if (retaddr_reg != NULL_RTX)
	    {
	      addr_location = gen_rtx_UNSPEC (Pmode,
					      gen_rtvec (1, addr_location),
					      UNSPEC_PLT31);
	      addr_location = gen_rtx_CONST (Pmode, addr_location);
	      plt31_call_p = true;
	    }
	  else
	    /* For -fpic code the PLT entries might use r12 which is
	       call-saved.  Therefore we cannot do a sibcall when
	       calling directly using a symbol ref.  When reaching
	       this point we decided (in s390_function_ok_for_sibcall)
	       to do a sibcall for a function pointer but one of the
	       optimizers was able to get rid of the function pointer
	       by propagating the symbol ref into the call.  This
	       optimization is illegal for S/390 so we turn the direct
	       call into a indirect call again.  */
	    addr_location = force_reg (Pmode, addr_location);
	}
    }

  /* If it is already an indirect call or the code above moved the
     SYMBOL_REF to somewhere else make sure the address can be found in
     register 1.  */
  if (retaddr_reg == NULL_RTX
      && GET_CODE (addr_location) != SYMBOL_REF
      && !plt31_call_p)
    {
      emit_move_insn (gen_rtx_REG (Pmode, SIBCALL_REGNUM), addr_location);
      addr_location = gen_rtx_REG (Pmode, SIBCALL_REGNUM);
    }

  if (TARGET_INDIRECT_BRANCH_NOBP_CALL
      && GET_CODE (addr_location) != SYMBOL_REF
      && !plt31_call_p)
    {
      /* Indirect branch thunks require the target to be a single GPR.  */
      addr_location = force_reg (Pmode, addr_location);

      /* Without exrl the indirect branch thunks need an additional
	 register for larl;ex */
      if (!TARGET_CPU_Z10)
	{
	  *clobber_thunk_reg = gen_rtx_REG (Pmode, INDIRECT_BRANCH_THUNK_REGNUM);
	  *clobber_thunk_reg = gen_rtx_CLOBBER (VOIDmode, *clobber_thunk_reg);
	}
    }

  addr_location = gen_rtx_MEM (QImode, addr_location);
  *call = gen_rtx_CALL (VOIDmode, addr_location, const0_rtx);

  if (result_reg != NULL_RTX)
    *call = gen_rtx_SET (result_reg, *call);

  if (retaddr_reg != NULL_RTX)
    {
      *clobber_ret_reg = gen_rtx_CLOBBER (VOIDmode, retaddr_reg);

      if (tls_call != NULL_RTX)
	*use = gen_rtx_USE (VOIDmode, tls_call);
    }


  for (i = 0; i < 4; i++)
    if (vec[i] != NULL_RTX)
      elts++;

  if (elts > 1)
    {
      rtvec v;
      int e = 0;

      v = rtvec_alloc (elts);
      for (i = 0; i < 4; i++)
	if (vec[i] != NULL_RTX)
	  {
	    RTVEC_ELT (v, e) = vec[i];
	    e++;
	  }

      *call = gen_rtx_PARALLEL (VOIDmode, v);
    }

  insn = emit_call_insn (*call);

  /* 31-bit PLT stubs and tls calls use the GOT register implicitly.  */
  if (plt31_call_p || tls_call != NULL_RTX)
    {
      /* s390_function_ok_for_sibcall should
	 have denied sibcalls in this case.  */
      gcc_assert (retaddr_reg != NULL_RTX);
      use_reg (&CALL_INSN_FUNCTION_USAGE (insn), gen_rtx_REG (Pmode, 12));
    }
  return insn;
}

/* Implement TARGET_CONDITIONAL_REGISTER_USAGE.  */

static void
s390_conditional_register_usage (void)
{
  int i;

  if (flag_pic)
    fixed_regs[PIC_OFFSET_TABLE_REGNUM] = 1;
  fixed_regs[BASE_REGNUM] = 0;
  fixed_regs[RETURN_REGNUM] = 0;
  if (TARGET_64BIT)
    {
      for (i = FPR8_REGNUM; i <= FPR15_REGNUM; i++)
	call_used_regs[i] = 0;
    }
  else
    {
      call_used_regs[FPR4_REGNUM] = 0;
      call_used_regs[FPR6_REGNUM] = 0;
    }

  if (TARGET_SOFT_FLOAT)
    {
      for (i = FPR0_REGNUM; i <= FPR15_REGNUM; i++)
	fixed_regs[i] = 1;
    }

  /* Disable v16 - v31 for non-vector target.  */
  if (!TARGET_VX)
    {
      for (i = VR16_REGNUM; i <= VR31_REGNUM; i++)
	fixed_regs[i] = call_used_regs[i] = 1;
    }
}

/* Corresponding function to eh_return expander.  */

static GTY(()) rtx s390_tpf_eh_return_symbol;
void
s390_emit_tpf_eh_return (rtx target)
{
  rtx_insn *insn;
  rtx reg, orig_ra;

  if (!s390_tpf_eh_return_symbol)
    {
      s390_tpf_eh_return_symbol = gen_rtx_SYMBOL_REF (Pmode, "__tpf_eh_return");
      SYMBOL_REF_FLAGS (s390_tpf_eh_return_symbol) |= SYMBOL_FLAG_FUNCTION;
    }

  reg = gen_rtx_REG (Pmode, 2);
  orig_ra = gen_rtx_REG (Pmode, 3);

  emit_move_insn (reg, target);
  emit_move_insn (orig_ra, get_hard_reg_initial_val (Pmode, RETURN_REGNUM));
  insn = s390_emit_call (s390_tpf_eh_return_symbol, NULL_RTX, reg,
				     gen_rtx_REG (Pmode, RETURN_REGNUM));
  use_reg (&CALL_INSN_FUNCTION_USAGE (insn), reg);
  use_reg (&CALL_INSN_FUNCTION_USAGE (insn), orig_ra);

  emit_move_insn (EH_RETURN_HANDLER_RTX, reg);
}

/* Rework the prologue/epilogue to avoid saving/restoring
   registers unnecessarily.  */

static void
s390_optimize_prologue (void)
{
  rtx_insn *insn, *new_insn, *next_insn;

  /* Do a final recompute of the frame-related data.  */
  s390_optimize_register_info ();

  /* If all special registers are in fact used, there's nothing we
     can do, so no point in walking the insn list.  */

  if (cfun_frame_layout.first_save_gpr <= BASE_REGNUM
      && cfun_frame_layout.last_save_gpr >= BASE_REGNUM)
    return;

  /* Search for prologue/epilogue insns and replace them.  */
  for (insn = get_insns (); insn; insn = next_insn)
    {
      int first, last, off;
      rtx set, base, offset;
      rtx pat;

      next_insn = NEXT_INSN (insn);

      if (! NONJUMP_INSN_P (insn) || ! RTX_FRAME_RELATED_P (insn))
	continue;

      pat = PATTERN (insn);

      /* Remove ldgr/lgdr instructions used for saving and restore
	 GPRs if possible.  */
      if (TARGET_Z10)
	{
	  rtx tmp_pat = pat;

	  if (INSN_CODE (insn) == CODE_FOR_stack_restore_from_fpr)
	    tmp_pat = XVECEXP (pat, 0, 0);

	  if (GET_CODE (tmp_pat) == SET
	      && GET_MODE (SET_SRC (tmp_pat)) == DImode
	      && REG_P (SET_SRC (tmp_pat))
	      && REG_P (SET_DEST (tmp_pat)))
	    {
	      int src_regno = REGNO (SET_SRC (tmp_pat));
	      int dest_regno = REGNO (SET_DEST (tmp_pat));
	      int gpr_regno;
	      int fpr_regno;

	      if (!((GENERAL_REGNO_P (src_regno)
		     && FP_REGNO_P (dest_regno))
		    || (FP_REGNO_P (src_regno)
			&& GENERAL_REGNO_P (dest_regno))))
		continue;

	      gpr_regno = GENERAL_REGNO_P (src_regno) ? src_regno : dest_regno;
	      fpr_regno = FP_REGNO_P (src_regno) ? src_regno : dest_regno;

	      /* GPR must be call-saved, FPR must be call-clobbered.  */
	      if (!call_used_regs[fpr_regno]
		  || call_used_regs[gpr_regno])
		continue;

	      /* It must not happen that what we once saved in an FPR now
		 needs a stack slot.  */
	      gcc_assert (cfun_gpr_save_slot (gpr_regno) != SAVE_SLOT_STACK);

	      if (cfun_gpr_save_slot (gpr_regno) == SAVE_SLOT_NONE)
		{
		  remove_insn (insn);
		  continue;
		}
	    }
	}

      if (GET_CODE (pat) == PARALLEL
	  && store_multiple_operation (pat, VOIDmode))
	{
	  set = XVECEXP (pat, 0, 0);
	  first = REGNO (SET_SRC (set));
	  last = first + XVECLEN (pat, 0) - 1;
	  offset = const0_rtx;
	  base = eliminate_constant_term (XEXP (SET_DEST (set), 0), &offset);
	  off = INTVAL (offset);

	  if (GET_CODE (base) != REG || off < 0)
	    continue;
	  if (cfun_frame_layout.first_save_gpr != -1
	      && (cfun_frame_layout.first_save_gpr < first
		  || cfun_frame_layout.last_save_gpr > last))
	    continue;
	  if (REGNO (base) != STACK_POINTER_REGNUM
	      && REGNO (base) != HARD_FRAME_POINTER_REGNUM)
	    continue;
	  if (first > BASE_REGNUM || last < BASE_REGNUM)
	    continue;

	  if (cfun_frame_layout.first_save_gpr != -1)
	    {
	      rtx s_pat = save_gprs (base,
				     off + (cfun_frame_layout.first_save_gpr
					    - first) * UNITS_PER_LONG,
				     cfun_frame_layout.first_save_gpr,
				     cfun_frame_layout.last_save_gpr);
	      new_insn = emit_insn_before (s_pat, insn);
	      INSN_ADDRESSES_NEW (new_insn, -1);
	    }

	  remove_insn (insn);
	  continue;
	}

      if (cfun_frame_layout.first_save_gpr == -1
	  && GET_CODE (pat) == SET
	  && GENERAL_REG_P (SET_SRC (pat))
	  && GET_CODE (SET_DEST (pat)) == MEM)
	{
	  set = pat;
	  first = REGNO (SET_SRC (set));
	  offset = const0_rtx;
	  base = eliminate_constant_term (XEXP (SET_DEST (set), 0), &offset);
	  off = INTVAL (offset);

	  if (GET_CODE (base) != REG || off < 0)
	    continue;
	  if (REGNO (base) != STACK_POINTER_REGNUM
	      && REGNO (base) != HARD_FRAME_POINTER_REGNUM)
	    continue;

	  remove_insn (insn);
	  continue;
	}

      if (GET_CODE (pat) == PARALLEL
	  && load_multiple_operation (pat, VOIDmode))
	{
	  set = XVECEXP (pat, 0, 0);
	  first = REGNO (SET_DEST (set));
	  last = first + XVECLEN (pat, 0) - 1;
	  offset = const0_rtx;
	  base = eliminate_constant_term (XEXP (SET_SRC (set), 0), &offset);
	  off = INTVAL (offset);

	  if (GET_CODE (base) != REG || off < 0)
	    continue;

	  if (cfun_frame_layout.first_restore_gpr != -1
	      && (cfun_frame_layout.first_restore_gpr < first
		  || cfun_frame_layout.last_restore_gpr > last))
	    continue;
	  if (REGNO (base) != STACK_POINTER_REGNUM
	      && REGNO (base) != HARD_FRAME_POINTER_REGNUM)
	    continue;
	  if (first > BASE_REGNUM || last < BASE_REGNUM)
	    continue;

	  if (cfun_frame_layout.first_restore_gpr != -1)
	    {
	      rtx rpat = restore_gprs (base,
				       off + (cfun_frame_layout.first_restore_gpr
					      - first) * UNITS_PER_LONG,
				       cfun_frame_layout.first_restore_gpr,
				       cfun_frame_layout.last_restore_gpr);

	      /* Remove REG_CFA_RESTOREs for registers that we no
		 longer need to save.  */
	      REG_NOTES (rpat) = REG_NOTES (insn);
	      for (rtx *ptr = &REG_NOTES (rpat); *ptr; )
		if (REG_NOTE_KIND (*ptr) == REG_CFA_RESTORE
		    && ((int) REGNO (XEXP (*ptr, 0))
			< cfun_frame_layout.first_restore_gpr))
		  *ptr = XEXP (*ptr, 1);
		else
		  ptr = &XEXP (*ptr, 1);
	      new_insn = emit_insn_before (rpat, insn);
	      RTX_FRAME_RELATED_P (new_insn) = 1;
	      INSN_ADDRESSES_NEW (new_insn, -1);
	    }

	  remove_insn (insn);
	  continue;
	}

      if (cfun_frame_layout.first_restore_gpr == -1
	  && GET_CODE (pat) == SET
	  && GENERAL_REG_P (SET_DEST (pat))
	  && GET_CODE (SET_SRC (pat)) == MEM)
	{
	  set = pat;
	  first = REGNO (SET_DEST (set));
	  offset = const0_rtx;
	  base = eliminate_constant_term (XEXP (SET_SRC (set), 0), &offset);
	  off = INTVAL (offset);

	  if (GET_CODE (base) != REG || off < 0)
	    continue;

	  if (REGNO (base) != STACK_POINTER_REGNUM
	      && REGNO (base) != HARD_FRAME_POINTER_REGNUM)
	    continue;

	  remove_insn (insn);
	  continue;
	}
    }
}

/* On z10 and later the dynamic branch prediction must see the
   backward jump within a certain windows.  If not it falls back to
   the static prediction.  This function rearranges the loop backward
   branch in a way which makes the static prediction always correct.
   The function returns true if it added an instruction.  */
static bool
s390_fix_long_loop_prediction (rtx_insn *insn)
{
  rtx set = single_set (insn);
  rtx code_label, label_ref;
  rtx_insn *uncond_jump;
  rtx_insn *cur_insn;
  rtx tmp;
  int distance;

  /* This will exclude branch on count and branch on index patterns
     since these are correctly statically predicted.

     The additional check for a PARALLEL is required here since
     single_set might be != NULL for PARALLELs where the set of the
     iteration variable is dead.  */
  if (GET_CODE (PATTERN (insn)) == PARALLEL
      || !set
      || SET_DEST (set) != pc_rtx
      || GET_CODE (SET_SRC(set)) != IF_THEN_ELSE)
    return false;

  /* Skip conditional returns.  */
  if (ANY_RETURN_P (XEXP (SET_SRC (set), 1))
      && XEXP (SET_SRC (set), 2) == pc_rtx)
    return false;

  label_ref = (GET_CODE (XEXP (SET_SRC (set), 1)) == LABEL_REF ?
	       XEXP (SET_SRC (set), 1) : XEXP (SET_SRC (set), 2));

  gcc_assert (GET_CODE (label_ref) == LABEL_REF);

  code_label = XEXP (label_ref, 0);

  if (INSN_ADDRESSES (INSN_UID (code_label)) == -1
      || INSN_ADDRESSES (INSN_UID (insn)) == -1
      || (INSN_ADDRESSES (INSN_UID (insn))
	  - INSN_ADDRESSES (INSN_UID (code_label)) < PREDICT_DISTANCE))
    return false;

  for (distance = 0, cur_insn = PREV_INSN (insn);
       distance < PREDICT_DISTANCE - 6;
       distance += get_attr_length (cur_insn), cur_insn = PREV_INSN (cur_insn))
    if (!cur_insn || JUMP_P (cur_insn) || LABEL_P (cur_insn))
      return false;

  rtx_code_label *new_label = gen_label_rtx ();
  uncond_jump = emit_jump_insn_after (
		  gen_rtx_SET (pc_rtx,
			       gen_rtx_LABEL_REF (VOIDmode, code_label)),
		  insn);
  emit_label_after (new_label, uncond_jump);

  tmp = XEXP (SET_SRC (set), 1);
  XEXP (SET_SRC (set), 1) = XEXP (SET_SRC (set), 2);
  XEXP (SET_SRC (set), 2) = tmp;
  INSN_CODE (insn) = -1;

  XEXP (label_ref, 0) = new_label;
  JUMP_LABEL (insn) = new_label;
  JUMP_LABEL (uncond_jump) = code_label;

  return true;
}

/* Returns 1 if INSN reads the value of REG for purposes not related
   to addressing of memory, and 0 otherwise.  */
static int
s390_non_addr_reg_read_p (rtx reg, rtx_insn *insn)
{
  return reg_referenced_p (reg, PATTERN (insn))
    && !reg_used_in_mem_p (REGNO (reg), PATTERN (insn));
}

/* Starting from INSN find_cond_jump looks downwards in the insn
   stream for a single jump insn which is the last user of the
   condition code set in INSN.  */
static rtx_insn *
find_cond_jump (rtx_insn *insn)
{
  for (; insn; insn = NEXT_INSN (insn))
    {
      rtx ite, cc;

      if (LABEL_P (insn))
	break;

      if (!JUMP_P (insn))
	{
	  if (reg_mentioned_p (gen_rtx_REG (CCmode, CC_REGNUM), insn))
	    break;
	  continue;
	}

      /* This will be triggered by a return.  */
      if (GET_CODE (PATTERN (insn)) != SET)
	break;

      gcc_assert (SET_DEST (PATTERN (insn)) == pc_rtx);
      ite = SET_SRC (PATTERN (insn));

      if (GET_CODE (ite) != IF_THEN_ELSE)
	break;

      cc = XEXP (XEXP (ite, 0), 0);
      if (!REG_P (cc) || !CC_REGNO_P (REGNO (cc)))
	break;

      if (find_reg_note (insn, REG_DEAD, cc))
	return insn;
      break;
    }

  return NULL;
}

/* Swap the condition in COND and the operands in OP0 and OP1 so that
   the semantics does not change.  If NULL_RTX is passed as COND the
   function tries to find the conditional jump starting with INSN.  */
static void
s390_swap_cmp (rtx cond, rtx *op0, rtx *op1, rtx_insn *insn)
{
  rtx tmp = *op0;

  if (cond == NULL_RTX)
    {
      rtx_insn *jump = find_cond_jump (NEXT_INSN (insn));
      rtx set = jump ? single_set (jump) : NULL_RTX;

      if (set == NULL_RTX)
	return;

      cond = XEXP (SET_SRC (set), 0);
    }

  *op0 = *op1;
  *op1 = tmp;
  PUT_CODE (cond, swap_condition (GET_CODE (cond)));
}

/* On z10, instructions of the compare-and-branch family have the
   property to access the register occurring as second operand with
   its bits complemented.  If such a compare is grouped with a second
   instruction that accesses the same register non-complemented, and
   if that register's value is delivered via a bypass, then the
   pipeline recycles, thereby causing significant performance decline.
   This function locates such situations and exchanges the two
   operands of the compare.  The function return true whenever it
   added an insn.  */
static bool
s390_z10_optimize_cmp (rtx_insn *insn)
{
  rtx_insn *prev_insn, *next_insn;
  bool insn_added_p = false;
  rtx cond, *op0, *op1;

  if (GET_CODE (PATTERN (insn)) == PARALLEL)
    {
      /* Handle compare and branch and branch on count
	 instructions.  */
      rtx pattern = single_set (insn);

      if (!pattern
	  || SET_DEST (pattern) != pc_rtx
	  || GET_CODE (SET_SRC (pattern)) != IF_THEN_ELSE)
	return false;

      cond = XEXP (SET_SRC (pattern), 0);
      op0 = &XEXP (cond, 0);
      op1 = &XEXP (cond, 1);
    }
  else if (GET_CODE (PATTERN (insn)) == SET)
    {
      rtx src, dest;

      /* Handle normal compare instructions.  */
      src = SET_SRC (PATTERN (insn));
      dest = SET_DEST (PATTERN (insn));

      if (!REG_P (dest)
	  || !CC_REGNO_P (REGNO (dest))
	  || GET_CODE (src) != COMPARE)
	return false;

      /* s390_swap_cmp will try to find the conditional
	 jump when passing NULL_RTX as condition.  */
      cond = NULL_RTX;
      op0 = &XEXP (src, 0);
      op1 = &XEXP (src, 1);
    }
  else
    return false;

  if (!REG_P (*op0) || !REG_P (*op1))
    return false;

  if (GET_MODE_CLASS (GET_MODE (*op0)) != MODE_INT)
    return false;

  /* Swap the COMPARE arguments and its mask if there is a
     conflicting access in the previous insn.  */
  prev_insn = prev_active_insn (insn);
  if (prev_insn != NULL_RTX && INSN_P (prev_insn)
      && reg_referenced_p (*op1, PATTERN (prev_insn)))
    s390_swap_cmp (cond, op0, op1, insn);

  /* Check if there is a conflict with the next insn. If there
     was no conflict with the previous insn, then swap the
     COMPARE arguments and its mask.  If we already swapped
     the operands, or if swapping them would cause a conflict
     with the previous insn, issue a NOP after the COMPARE in
     order to separate the two instuctions.  */
  next_insn = next_active_insn (insn);
  if (next_insn != NULL_RTX && INSN_P (next_insn)
      && s390_non_addr_reg_read_p (*op1, next_insn))
    {
      if (prev_insn != NULL_RTX && INSN_P (prev_insn)
	  && s390_non_addr_reg_read_p (*op0, prev_insn))
	{
	  if (REGNO (*op1) == 0)
	    emit_insn_after (gen_nop_lr1 (), insn);
	  else
	    emit_insn_after (gen_nop_lr0 (), insn);
	  insn_added_p = true;
	}
      else
	s390_swap_cmp (cond, op0, op1, insn);
    }
  return insn_added_p;
}

/* Number of INSNs to be scanned backward in the last BB of the loop
   and forward in the first BB of the loop.  This usually should be a
   bit more than the number of INSNs which could go into one
   group.  */
#define S390_OSC_SCAN_INSN_NUM 5

/* Scan LOOP for static OSC collisions and return true if a osc_break
   should be issued for this loop.  */
static bool
s390_adjust_loop_scan_osc (struct loop* loop)

{
  HARD_REG_SET modregs, newregs;
  rtx_insn *insn, *store_insn = NULL;
  rtx set;
  struct s390_address addr_store, addr_load;
  subrtx_iterator::array_type array;
  int insn_count;

  CLEAR_HARD_REG_SET (modregs);

  insn_count = 0;
  FOR_BB_INSNS_REVERSE (loop->latch, insn)
    {
      if (!INSN_P (insn) || INSN_CODE (insn) <= 0)
	continue;

      insn_count++;
      if (insn_count > S390_OSC_SCAN_INSN_NUM)
	return false;

      find_all_hard_reg_sets (insn, &newregs, true);
      modregs |= newregs;

      set = single_set (insn);
      if (!set)
	continue;

      if (MEM_P (SET_DEST (set))
	  && s390_decompose_address (XEXP (SET_DEST (set), 0), &addr_store))
	{
	  store_insn = insn;
	  break;
	}
    }

  if (store_insn == NULL_RTX)
    return false;

  insn_count = 0;
  FOR_BB_INSNS (loop->header, insn)
    {
      if (!INSN_P (insn) || INSN_CODE (insn) <= 0)
	continue;

      if (insn == store_insn)
	return false;

      insn_count++;
      if (insn_count > S390_OSC_SCAN_INSN_NUM)
	return false;

      find_all_hard_reg_sets (insn, &newregs, true);
      modregs |= newregs;

      set = single_set (insn);
      if (!set)
	continue;

      /* An intermediate store disrupts static OSC checking
	 anyway.  */
      if (MEM_P (SET_DEST (set))
	  && s390_decompose_address (XEXP (SET_DEST (set), 0), NULL))
	return false;

      FOR_EACH_SUBRTX (iter, array, SET_SRC (set), NONCONST)
	if (MEM_P (*iter)
	    && s390_decompose_address (XEXP (*iter, 0), &addr_load)
	    && rtx_equal_p (addr_load.base, addr_store.base)
	    && rtx_equal_p (addr_load.indx, addr_store.indx)
	    && rtx_equal_p (addr_load.disp, addr_store.disp))
	  {
	    if ((addr_load.base != NULL_RTX
		 && TEST_HARD_REG_BIT (modregs, REGNO (addr_load.base)))
		|| (addr_load.indx != NULL_RTX
		    && TEST_HARD_REG_BIT (modregs, REGNO (addr_load.indx))))
	      return true;
	  }
    }
  return false;
}

/* Look for adjustments which can be done on simple innermost
   loops.  */
static void
s390_adjust_loops ()
{
  df_analyze ();
  compute_bb_for_insn ();

  /* Find the loops.  */
  loop_optimizer_init (AVOID_CFG_MODIFICATIONS);

  for (auto loop : loops_list (cfun, LI_ONLY_INNERMOST))
    {
      if (dump_file)
	{
	  flow_loop_dump (loop, dump_file, NULL, 0);
	  fprintf (dump_file, ";;  OSC loop scan Loop: ");
	}
      if (loop->latch == NULL
	  || pc_set (BB_END (loop->latch)) == NULL_RTX
	  || !s390_adjust_loop_scan_osc (loop))
	{
	  if (dump_file)
	    {
	      if (loop->latch == NULL)
		fprintf (dump_file, " muliple backward jumps\n");
	      else
		{
		  fprintf (dump_file, " header insn: %d latch insn: %d ",
			   INSN_UID (BB_HEAD (loop->header)),
			   INSN_UID (BB_END (loop->latch)));
		  if (pc_set (BB_END (loop->latch)) == NULL_RTX)
		    fprintf (dump_file, " loop does not end with jump\n");
		  else
		    fprintf (dump_file, " not instrumented\n");
		}
	    }
	}
      else
	{
	  rtx_insn *new_insn;

	  if (dump_file)
	    fprintf (dump_file, " adding OSC break insn: ");
	  new_insn = emit_insn_before (gen_osc_break (),
				       BB_END (loop->latch));
	  INSN_ADDRESSES_NEW (new_insn, -1);
	}
    }

  loop_optimizer_finalize ();

  df_finish_pass (false);
}

/* Perform machine-dependent processing.  */

static void
s390_reorg (void)
{
  struct constant_pool *pool;
  rtx_insn *insn;
  int hw_before, hw_after;

  if (s390_tune == PROCESSOR_2964_Z13)
    s390_adjust_loops ();

  /* Make sure all splits have been performed; splits after
     machine_dependent_reorg might confuse insn length counts.  */
  split_all_insns_noflow ();

  /* Install the main literal pool and the associated base
     register load insns.  The literal pool might be > 4096 bytes in
     size, so that some of its elements cannot be directly accessed.

     To fix this, we split the single literal pool into multiple
     pool chunks, reloading the pool base register at various
     points throughout the function to ensure it always points to
     the pool chunk the following code expects.  */

  /* Collect the literal pool.  */
  pool = s390_mainpool_start ();
  if (pool)
    {
      /* Finish up literal pool related changes.  */
      s390_mainpool_finish (pool);
    }
  else
    {
      /* If literal pool overflowed, chunkify it.  */
      pool = s390_chunkify_start ();
      s390_chunkify_finish (pool);
    }

  /* Generate out-of-pool execute target insns.  */
  for (insn = get_insns (); insn; insn = NEXT_INSN (insn))
    {
      rtx label;
      rtx_insn *target;

      label = s390_execute_label (insn);
      if (!label)
	continue;

      gcc_assert (label != const0_rtx);

      target = emit_label (XEXP (label, 0));
      INSN_ADDRESSES_NEW (target, -1);

      if (JUMP_P (insn))
	{
	  target = emit_jump_insn (s390_execute_target (insn));
	  /* This is important in order to keep a table jump
	     pointing at the jump table label.  Only this makes it
	     being recognized as table jump.  */
	  JUMP_LABEL (target) = JUMP_LABEL (insn);
	}
      else
	target = emit_insn (s390_execute_target (insn));
      INSN_ADDRESSES_NEW (target, -1);
    }

  /* Try to optimize prologue and epilogue further.  */
  s390_optimize_prologue ();

  /* Walk over the insns and do some >=z10 specific changes.  */
  if (s390_tune >= PROCESSOR_2097_Z10)
    {
      rtx_insn *insn;
      bool insn_added_p = false;

      /* The insn lengths and addresses have to be up to date for the
	 following manipulations.  */
      shorten_branches (get_insns ());

      for (insn = get_insns (); insn; insn = NEXT_INSN (insn))
	{
	  if (!INSN_P (insn) || INSN_CODE (insn) <= 0)
	    continue;

	  if (JUMP_P (insn))
	    insn_added_p |= s390_fix_long_loop_prediction (insn);

	  if ((GET_CODE (PATTERN (insn)) == PARALLEL
	       || GET_CODE (PATTERN (insn)) == SET)
	      && s390_tune == PROCESSOR_2097_Z10)
	    insn_added_p |= s390_z10_optimize_cmp (insn);
	}

      /* Adjust branches if we added new instructions.  */
      if (insn_added_p)
	shorten_branches (get_insns ());
    }

  s390_function_num_hotpatch_hw (current_function_decl, &hw_before, &hw_after);
  if (hw_after > 0)
    {
      rtx_insn *insn;

      /* Insert NOPs for hotpatching. */
      for (insn = get_insns (); insn; insn = NEXT_INSN (insn))
	/* Emit NOPs
	    1. inside the area covered by debug information to allow setting
	       breakpoints at the NOPs,
	    2. before any insn which results in an asm instruction,
	    3. before in-function labels to avoid jumping to the NOPs, for
	       example as part of a loop,
	    4. before any barrier in case the function is completely empty
	       (__builtin_unreachable ()) and has neither internal labels nor
	       active insns.
	*/
	if (active_insn_p (insn) || BARRIER_P (insn) || LABEL_P (insn))
	  break;
      /* Output a series of NOPs before the first active insn.  */
      while (insn && hw_after > 0)
	{
	  if (hw_after >= 3)
	    {
	      emit_insn_before (gen_nop_6_byte (), insn);
	      hw_after -= 3;
	    }
	  else if (hw_after >= 2)
	    {
	      emit_insn_before (gen_nop_4_byte (), insn);
	      hw_after -= 2;
	    }
	  else
	    {
	      emit_insn_before (gen_nop_2_byte (), insn);
	      hw_after -= 1;
	    }
	}
    }
}

/* Return true if INSN is a fp load insn writing register REGNO.  */
static inline bool
s390_fpload_toreg (rtx_insn *insn, unsigned int regno)
{
  rtx set;
  enum attr_type flag = s390_safe_attr_type (insn);

  if (flag != TYPE_FLOADSF && flag != TYPE_FLOADDF)
    return false;

  set = single_set (insn);

  if (set == NULL_RTX)
    return false;

  if (!REG_P (SET_DEST (set)) || !MEM_P (SET_SRC (set)))
    return false;

  if (REGNO (SET_DEST (set)) != regno)
    return false;

  return true;
}

/* This value describes the distance to be avoided between an
   arithmetic fp instruction and an fp load writing the same register.
   Z10_EARLYLOAD_DISTANCE - 1 as well as Z10_EARLYLOAD_DISTANCE + 1 is
   fine but the exact value has to be avoided. Otherwise the FP
   pipeline will throw an exception causing a major penalty.  */
#define Z10_EARLYLOAD_DISTANCE 7

/* Rearrange the ready list in order to avoid the situation described
   for Z10_EARLYLOAD_DISTANCE.  A problematic load instruction is
   moved to the very end of the ready list.  */
static void
s390_z10_prevent_earlyload_conflicts (rtx_insn **ready, int *nready_p)
{
  unsigned int regno;
  int nready = *nready_p;
  rtx_insn *tmp;
  int i;
  rtx_insn *insn;
  rtx set;
  enum attr_type flag;
  int distance;

  /* Skip DISTANCE - 1 active insns.  */
  for (insn = last_scheduled_insn, distance = Z10_EARLYLOAD_DISTANCE - 1;
       distance > 0 && insn != NULL_RTX;
       distance--, insn = prev_active_insn (insn))
    if (CALL_P (insn) || JUMP_P (insn))
      return;

  if (insn == NULL_RTX)
    return;

  set = single_set (insn);

  if (set == NULL_RTX || !REG_P (SET_DEST (set))
      || GET_MODE_CLASS (GET_MODE (SET_DEST (set))) != MODE_FLOAT)
    return;

  flag = s390_safe_attr_type (insn);

  if (flag == TYPE_FLOADSF || flag == TYPE_FLOADDF)
    return;

  regno = REGNO (SET_DEST (set));
  i = nready - 1;

  while (!s390_fpload_toreg (ready[i], regno) && i > 0)
    i--;

  if (!i)
    return;

  tmp = ready[i];
  memmove (&ready[1], &ready[0], sizeof (rtx_insn *) * i);
  ready[0] = tmp;
}

/* Returns TRUE if BB is entered via a fallthru edge and all other
   incoming edges are less than likely.  */
static bool
s390_bb_fallthru_entry_likely (basic_block bb)
{
  edge e, fallthru_edge;
  edge_iterator ei;

  if (!bb)
    return false;

  fallthru_edge = find_fallthru_edge (bb->preds);
  if (!fallthru_edge)
    return false;

  FOR_EACH_EDGE (e, ei, bb->preds)
    if (e != fallthru_edge
	&& e->probability >= profile_probability::likely ())
      return false;

  return true;
}

struct s390_sched_state
{
  /* Number of insns in the group.  */
  int group_state;
  /* Execution side of the group.  */
  int side;
  /* Group can only hold two insns.  */
  bool group_of_two;
} s390_sched_state;

static struct s390_sched_state sched_state = {0, 1, false};

#define S390_SCHED_ATTR_MASK_CRACKED    0x1
#define S390_SCHED_ATTR_MASK_EXPANDED   0x2
#define S390_SCHED_ATTR_MASK_ENDGROUP   0x4
#define S390_SCHED_ATTR_MASK_GROUPALONE 0x8
#define S390_SCHED_ATTR_MASK_GROUPOFTWO 0x10

static unsigned int
s390_get_sched_attrmask (rtx_insn *insn)
{
  unsigned int mask = 0;

  switch (s390_tune)
    {
    case PROCESSOR_2827_ZEC12:
      if (get_attr_zEC12_cracked (insn))
	mask |= S390_SCHED_ATTR_MASK_CRACKED;
      if (get_attr_zEC12_expanded (insn))
	mask |= S390_SCHED_ATTR_MASK_EXPANDED;
      if (get_attr_zEC12_endgroup (insn))
	mask |= S390_SCHED_ATTR_MASK_ENDGROUP;
      if (get_attr_zEC12_groupalone (insn))
	mask |= S390_SCHED_ATTR_MASK_GROUPALONE;
      break;
    case PROCESSOR_2964_Z13:
      if (get_attr_z13_cracked (insn))
	mask |= S390_SCHED_ATTR_MASK_CRACKED;
      if (get_attr_z13_expanded (insn))
	mask |= S390_SCHED_ATTR_MASK_EXPANDED;
      if (get_attr_z13_endgroup (insn))
	mask |= S390_SCHED_ATTR_MASK_ENDGROUP;
      if (get_attr_z13_groupalone (insn))
	mask |= S390_SCHED_ATTR_MASK_GROUPALONE;
      if (get_attr_z13_groupoftwo (insn))
	mask |= S390_SCHED_ATTR_MASK_GROUPOFTWO;
      break;
    case PROCESSOR_3906_Z14:
      if (get_attr_z14_cracked (insn))
	mask |= S390_SCHED_ATTR_MASK_CRACKED;
      if (get_attr_z14_expanded (insn))
	mask |= S390_SCHED_ATTR_MASK_EXPANDED;
      if (get_attr_z14_endgroup (insn))
	mask |= S390_SCHED_ATTR_MASK_ENDGROUP;
      if (get_attr_z14_groupalone (insn))
	mask |= S390_SCHED_ATTR_MASK_GROUPALONE;
      if (get_attr_z14_groupoftwo (insn))
	mask |= S390_SCHED_ATTR_MASK_GROUPOFTWO;
      break;
    case PROCESSOR_8561_Z15:
      if (get_attr_z15_cracked (insn))
	mask |= S390_SCHED_ATTR_MASK_CRACKED;
      if (get_attr_z15_expanded (insn))
	mask |= S390_SCHED_ATTR_MASK_EXPANDED;
      if (get_attr_z15_endgroup (insn))
	mask |= S390_SCHED_ATTR_MASK_ENDGROUP;
      if (get_attr_z15_groupalone (insn))
	mask |= S390_SCHED_ATTR_MASK_GROUPALONE;
      if (get_attr_z15_groupoftwo (insn))
	mask |= S390_SCHED_ATTR_MASK_GROUPOFTWO;
      break;
    case PROCESSOR_3931_Z16:
      if (get_attr_z16_cracked (insn))
	mask |= S390_SCHED_ATTR_MASK_CRACKED;
      if (get_attr_z16_expanded (insn))
	mask |= S390_SCHED_ATTR_MASK_EXPANDED;
      if (get_attr_z16_endgroup (insn))
	mask |= S390_SCHED_ATTR_MASK_ENDGROUP;
      if (get_attr_z16_groupalone (insn))
	mask |= S390_SCHED_ATTR_MASK_GROUPALONE;
      if (get_attr_z16_groupoftwo (insn))
	mask |= S390_SCHED_ATTR_MASK_GROUPOFTWO;
      break;
    default:
      gcc_unreachable ();
    }
  return mask;
}

static unsigned int
s390_get_unit_mask (rtx_insn *insn, int *units)
{
  unsigned int mask = 0;

  switch (s390_tune)
    {
    case PROCESSOR_2964_Z13:
      *units = 4;
      if (get_attr_z13_unit_lsu (insn))
	mask |= 1 << 0;
      if (get_attr_z13_unit_fxa (insn))
	mask |= 1 << 1;
      if (get_attr_z13_unit_fxb (insn))
	mask |= 1 << 2;
      if (get_attr_z13_unit_vfu (insn))
	mask |= 1 << 3;
      break;
    case PROCESSOR_3906_Z14:
      *units = 4;
      if (get_attr_z14_unit_lsu (insn))
	mask |= 1 << 0;
      if (get_attr_z14_unit_fxa (insn))
	mask |= 1 << 1;
      if (get_attr_z14_unit_fxb (insn))
	mask |= 1 << 2;
      if (get_attr_z14_unit_vfu (insn))
	mask |= 1 << 3;
      break;
    case PROCESSOR_8561_Z15:
      *units = 4;
      if (get_attr_z15_unit_lsu (insn))
	mask |= 1 << 0;
      if (get_attr_z15_unit_fxa (insn))
	mask |= 1 << 1;
      if (get_attr_z15_unit_fxb (insn))
	mask |= 1 << 2;
      if (get_attr_z15_unit_vfu (insn))
	mask |= 1 << 3;
      break;
    case PROCESSOR_3931_Z16:
      *units = 4;
      if (get_attr_z16_unit_lsu (insn))
	mask |= 1 << 0;
      if (get_attr_z16_unit_fxa (insn))
	mask |= 1 << 1;
      if (get_attr_z16_unit_fxb (insn))
	mask |= 1 << 2;
      if (get_attr_z16_unit_vfu (insn))
	mask |= 1 << 3;
      break;
    default:
      gcc_unreachable ();
    }
  return mask;
}

static bool
s390_is_fpd (rtx_insn *insn)
{
  if (insn == NULL_RTX)
    return false;

  return get_attr_z13_unit_fpd (insn) || get_attr_z14_unit_fpd (insn)
    || get_attr_z15_unit_fpd (insn) || get_attr_z16_unit_fpd (insn);
}

static bool
s390_is_fxd (rtx_insn *insn)
{
  if (insn == NULL_RTX)
    return false;

  return get_attr_z13_unit_fxd (insn) || get_attr_z14_unit_fxd (insn)
    || get_attr_z15_unit_fxd (insn) || get_attr_z16_unit_fxd (insn);
}

/* Returns TRUE if INSN is a long-running instruction.  */
static bool
s390_is_longrunning (rtx_insn *insn)
{
  if (insn == NULL_RTX)
    return false;

  return s390_is_fxd (insn) || s390_is_fpd (insn);
}


/* Return the scheduling score for INSN.  The higher the score the
   better.  The score is calculated from the OOO scheduling attributes
   of INSN and the scheduling state sched_state.  */
static int
s390_sched_score (rtx_insn *insn)
{
  unsigned int mask = s390_get_sched_attrmask (insn);
  int score = 0;

  switch (sched_state.group_state)
    {
    case 0:
      /* Try to put insns into the first slot which would otherwise
	 break a group.  */
      if ((mask & S390_SCHED_ATTR_MASK_CRACKED) != 0
	  || (mask & S390_SCHED_ATTR_MASK_EXPANDED) != 0)
	score += 5;
      if ((mask & S390_SCHED_ATTR_MASK_GROUPALONE) != 0)
	score += 10;
      break;
    case 1:
      /* Prefer not cracked insns while trying to put together a
	 group.  */
      if ((mask & S390_SCHED_ATTR_MASK_CRACKED) == 0
	  && (mask & S390_SCHED_ATTR_MASK_EXPANDED) == 0
	  && (mask & S390_SCHED_ATTR_MASK_GROUPALONE) == 0)
	score += 10;
      if ((mask & S390_SCHED_ATTR_MASK_ENDGROUP) == 0)
	score += 5;
      /* If we are in a group of two already, try to schedule another
	 group-of-two insn to avoid shortening another group.  */
      if (sched_state.group_of_two
	  && (mask & S390_SCHED_ATTR_MASK_GROUPOFTWO) != 0)
	score += 15;
      break;
    case 2:
      /* Prefer not cracked insns while trying to put together a
	 group.  */
      if ((mask & S390_SCHED_ATTR_MASK_CRACKED) == 0
	  && (mask & S390_SCHED_ATTR_MASK_EXPANDED) == 0
	  && (mask & S390_SCHED_ATTR_MASK_GROUPALONE) == 0)
	score += 10;
      /* Prefer endgroup insns in the last slot.  */
      if ((mask & S390_SCHED_ATTR_MASK_ENDGROUP) != 0)
	score += 10;
      /* Try to avoid group-of-two insns in the last slot as they will
	 shorten this group as well as the next one.  */
      if ((mask & S390_SCHED_ATTR_MASK_GROUPOFTWO) != 0)
	score = MAX (0, score - 15);
      break;
    }

  if (s390_tune >= PROCESSOR_2964_Z13)
    {
      int units, i;
      unsigned unit_mask, m = 1;

      unit_mask = s390_get_unit_mask (insn, &units);
      gcc_assert (units <= MAX_SCHED_UNITS);

      /* Add a score in range 0..MAX_SCHED_MIX_SCORE depending on how long
	 ago the last insn of this unit type got scheduled.  This is
	 supposed to help providing a proper instruction mix to the
	 CPU.  */
      for (i = 0; i < units; i++, m <<= 1)
	if (m & unit_mask)
	  score += (last_scheduled_unit_distance[i][sched_state.side]
	      * MAX_SCHED_MIX_SCORE / MAX_SCHED_MIX_DISTANCE);

      int other_side = 1 - sched_state.side;

      /* Try to delay long-running insns when side is busy.  */
      if (s390_is_longrunning (insn))
	{
	  if (s390_is_fxd (insn))
	    {
	      if (fxd_longrunning[sched_state.side]
		  && fxd_longrunning[other_side]
		  <= fxd_longrunning[sched_state.side])
		score = MAX (0, score - 10);

	      else if (fxd_longrunning[other_side]
		  >= fxd_longrunning[sched_state.side])
		score += 10;
	    }

	  if (s390_is_fpd (insn))
	    {
	      if (fpd_longrunning[sched_state.side]
		  && fpd_longrunning[other_side]
		  <= fpd_longrunning[sched_state.side])
		score = MAX (0, score - 10);

	      else if (fpd_longrunning[other_side]
		  >= fpd_longrunning[sched_state.side])
		score += 10;
	    }
	}
    }

  return score;
}

/* This function is called via hook TARGET_SCHED_REORDER before
   issuing one insn from list READY which contains *NREADYP entries.
   For target z10 it reorders load instructions to avoid early load
   conflicts in the floating point pipeline  */
static int
s390_sched_reorder (FILE *file, int verbose,
		    rtx_insn **ready, int *nreadyp, int clock ATTRIBUTE_UNUSED)
{
  if (s390_tune == PROCESSOR_2097_Z10
      && reload_completed
      && *nreadyp > 1)
    s390_z10_prevent_earlyload_conflicts (ready, nreadyp);

  if (s390_tune >= PROCESSOR_2827_ZEC12
      && reload_completed
      && *nreadyp > 1)
    {
      int i;
      int last_index = *nreadyp - 1;
      int max_index = -1;
      int max_score = -1;
      rtx_insn *tmp;

      /* Just move the insn with the highest score to the top (the
	 end) of the list.  A full sort is not needed since a conflict
	 in the hazard recognition cannot happen.  So the top insn in
	 the ready list will always be taken.  */
      for (i = last_index; i >= 0; i--)
	{
	  int score;

	  if (recog_memoized (ready[i]) < 0)
	    continue;

	  score = s390_sched_score (ready[i]);
	  if (score > max_score)
	    {
	      max_score = score;
	      max_index = i;
	    }
	}

      if (max_index != -1)
	{
	  if (max_index != last_index)
	    {
	      tmp = ready[max_index];
	      ready[max_index] = ready[last_index];
	      ready[last_index] = tmp;

	      if (verbose > 5)
		fprintf (file,
			 ";;\t\tBACKEND: move insn %d to the top of list\n",
			 INSN_UID (ready[last_index]));
	    }
	  else if (verbose > 5)
	    fprintf (file,
		     ";;\t\tBACKEND: best insn %d already on top\n",
		     INSN_UID (ready[last_index]));
	}

      if (verbose > 5)
	{
	  fprintf (file, "ready list ooo attributes - sched state: %d\n",
		   sched_state.group_state);

	  for (i = last_index; i >= 0; i--)
	    {
	      unsigned int sched_mask;
	      rtx_insn *insn = ready[i];

	      if (recog_memoized (insn) < 0)
		continue;

	      sched_mask = s390_get_sched_attrmask (insn);
	      fprintf (file, ";;\t\tBACKEND: insn %d score: %d: ",
		       INSN_UID (insn),
		       s390_sched_score (insn));
#define PRINT_SCHED_ATTR(M, ATTR) fprintf (file, "%s ",\
					   ((M) & sched_mask) ? #ATTR : "");
	      PRINT_SCHED_ATTR (S390_SCHED_ATTR_MASK_CRACKED, cracked);
	      PRINT_SCHED_ATTR (S390_SCHED_ATTR_MASK_EXPANDED, expanded);
	      PRINT_SCHED_ATTR (S390_SCHED_ATTR_MASK_ENDGROUP, endgroup);
	      PRINT_SCHED_ATTR (S390_SCHED_ATTR_MASK_GROUPALONE, groupalone);
#undef PRINT_SCHED_ATTR
	      if (s390_tune >= PROCESSOR_2964_Z13)
		{
		  unsigned int unit_mask, m = 1;
		  int units, j;

		  unit_mask  = s390_get_unit_mask (insn, &units);
		  fprintf (file, "(units:");
		  for (j = 0; j < units; j++, m <<= 1)
		    if (m & unit_mask)
		      fprintf (file, " u%d", j);
		  fprintf (file, ")");
		}
	      fprintf (file, "\n");
	    }
	}
    }

  return s390_issue_rate ();
}


/* This function is called via hook TARGET_SCHED_VARIABLE_ISSUE after
   the scheduler has issued INSN.  It stores the last issued insn into
   last_scheduled_insn in order to make it available for
   s390_sched_reorder.  */
static int
s390_sched_variable_issue (FILE *file, int verbose, rtx_insn *insn, int more)
{
  last_scheduled_insn = insn;

  bool ends_group = false;

  if (s390_tune >= PROCESSOR_2827_ZEC12
      && reload_completed
      && recog_memoized (insn) >= 0)
    {
      unsigned int mask = s390_get_sched_attrmask (insn);

      if ((mask & S390_SCHED_ATTR_MASK_GROUPOFTWO) != 0)
	sched_state.group_of_two = true;

      /* If this is a group-of-two insn, we actually ended the last group
	 and this insn is the first one of the new group.  */
      if (sched_state.group_state == 2 && sched_state.group_of_two)
	{
	  sched_state.side = sched_state.side ? 0 : 1;
	  sched_state.group_state = 0;
	}

      /* Longrunning and side bookkeeping.  */
      for (int i = 0; i < 2; i++)
	{
	  fxd_longrunning[i] = MAX (0, fxd_longrunning[i] - 1);
	  fpd_longrunning[i] = MAX (0, fpd_longrunning[i] - 1);
	}

      unsigned latency = insn_default_latency (insn);
      if (s390_is_longrunning (insn))
	{
	  if (s390_is_fxd (insn))
	    fxd_longrunning[sched_state.side] = latency;
	  else
	    fpd_longrunning[sched_state.side] = latency;
	}

      if (s390_tune >= PROCESSOR_2964_Z13)
	{
	  int units, i;
	  unsigned unit_mask, m = 1;

	  unit_mask = s390_get_unit_mask (insn, &units);
	  gcc_assert (units <= MAX_SCHED_UNITS);

	  for (i = 0; i < units; i++, m <<= 1)
	    if (m & unit_mask)
	      last_scheduled_unit_distance[i][sched_state.side] = 0;
	    else if (last_scheduled_unit_distance[i][sched_state.side]
		< MAX_SCHED_MIX_DISTANCE)
	      last_scheduled_unit_distance[i][sched_state.side]++;
	}

      if ((mask & S390_SCHED_ATTR_MASK_CRACKED) != 0
	  || (mask & S390_SCHED_ATTR_MASK_EXPANDED) != 0
	  || (mask & S390_SCHED_ATTR_MASK_GROUPALONE) != 0
	  || (mask & S390_SCHED_ATTR_MASK_ENDGROUP) != 0)
	{
	  sched_state.group_state = 0;
	  ends_group = true;
	}
      else
	{
	  switch (sched_state.group_state)
	    {
	    case 0:
	      sched_state.group_state++;
	      break;
	    case 1:
	      sched_state.group_state++;
	      if (sched_state.group_of_two)
		{
		  sched_state.group_state = 0;
		  ends_group = true;
		}
	      break;
	    case 2:
	      sched_state.group_state++;
	      ends_group = true;
	      break;
	    }
	}

      if (verbose > 5)
	{
	  unsigned int sched_mask;

	  sched_mask = s390_get_sched_attrmask (insn);

	  fprintf (file, ";;\t\tBACKEND: insn %d: ", INSN_UID (insn));
#define PRINT_SCHED_ATTR(M, ATTR) fprintf (file, "%s ", ((M) & sched_mask) ? #ATTR : "");
	  PRINT_SCHED_ATTR (S390_SCHED_ATTR_MASK_CRACKED, cracked);
	  PRINT_SCHED_ATTR (S390_SCHED_ATTR_MASK_EXPANDED, expanded);
	  PRINT_SCHED_ATTR (S390_SCHED_ATTR_MASK_ENDGROUP, endgroup);
	  PRINT_SCHED_ATTR (S390_SCHED_ATTR_MASK_GROUPALONE, groupalone);
#undef PRINT_SCHED_ATTR

	  if (s390_tune >= PROCESSOR_2964_Z13)
	    {
	      unsigned int unit_mask, m = 1;
	      int units, j;

	      unit_mask  = s390_get_unit_mask (insn, &units);
	      fprintf (file, "(units:");
	      for (j = 0; j < units; j++, m <<= 1)
		if (m & unit_mask)
		  fprintf (file, " %d", j);
	      fprintf (file, ")");
	    }
	  fprintf (file, " sched state: %d\n", sched_state.group_state);

	  if (s390_tune >= PROCESSOR_2964_Z13)
	    {
	      int units, j;

	      s390_get_unit_mask (insn, &units);

	      fprintf (file, ";;\t\tBACKEND: units on this side unused for: ");
	      for (j = 0; j < units; j++)
		fprintf (file, "%d:%d ", j,
		    last_scheduled_unit_distance[j][sched_state.side]);
	      fprintf (file, "\n");
	    }
	}

      /* If this insn ended a group, the next will be on the other side.  */
      if (ends_group)
	{
	  sched_state.group_state = 0;
	  sched_state.side = sched_state.side ? 0 : 1;
	  sched_state.group_of_two = false;
	}
    }

  if (GET_CODE (PATTERN (insn)) != USE
      && GET_CODE (PATTERN (insn)) != CLOBBER)
    return more - 1;
  else
    return more;
}

static void
s390_sched_init (FILE *file ATTRIBUTE_UNUSED,
		 int verbose ATTRIBUTE_UNUSED,
		 int max_ready ATTRIBUTE_UNUSED)
{
  /* If the next basic block is most likely entered via a fallthru edge
     we keep the last sched state.  Otherwise we start a new group.
     The scheduler traverses basic blocks in "instruction stream" ordering
     so if we see a fallthru edge here, sched_state will be of its
     source block.

     current_sched_info->prev_head is the insn before the first insn of the
     block of insns to be scheduled.
     */
  rtx_insn *insn = current_sched_info->prev_head
    ? NEXT_INSN (current_sched_info->prev_head) : NULL;
  basic_block bb = insn ? BLOCK_FOR_INSN (insn) : NULL;
  if (s390_tune < PROCESSOR_2964_Z13 || !s390_bb_fallthru_entry_likely (bb))
    {
      last_scheduled_insn = NULL;
      memset (last_scheduled_unit_distance, 0,
	  MAX_SCHED_UNITS * NUM_SIDES * sizeof (int));
      sched_state.group_state = 0;
      sched_state.group_of_two = false;
    }
}

/* This target hook implementation for TARGET_LOOP_UNROLL_ADJUST calculates
   a new number struct loop *loop should be unrolled if tuned for cpus with
   a built-in stride prefetcher.
   The loop is analyzed for memory accesses by calling check_dpu for
   each rtx of the loop. Depending on the loop_depth and the amount of
   memory accesses a new number <=nunroll is returned to improve the
   behavior of the hardware prefetch unit.  */
static unsigned
s390_loop_unroll_adjust (unsigned nunroll, struct loop *loop)
{
  basic_block *bbs;
  rtx_insn *insn;
  unsigned i;
  unsigned mem_count = 0;

  if (s390_tune < PROCESSOR_2097_Z10)
    return nunroll;

  if (unroll_only_small_loops)
    {
      /* Only unroll loops smaller than or equal to 12 insns.  */
      const unsigned int small_threshold = 12;

      if (loop->ninsns > small_threshold)
	return 0;

      /* ???: Make this dependent on the type of registers in
	 the loop.  Increase the limit for vector registers.  */
      const unsigned int max_insns = optimize >= 3 ? 36 : 24;

      nunroll = MIN (nunroll, max_insns / loop->ninsns);
    }

  /* Count the number of memory references within the loop body.  */
  bbs = get_loop_body (loop);
  subrtx_iterator::array_type array;
  for (i = 0; i < loop->num_nodes; i++)
    FOR_BB_INSNS (bbs[i], insn)
      if (INSN_P (insn) && INSN_CODE (insn) != -1)
	{
	  rtx set;

	  /* The runtime of small loops with memory block operations
	     will be determined by the memory operation.  Doing
	     unrolling doesn't help here.  Measurements to confirm
	     this where only done on recent CPU levels.  So better do
	     not change anything for older CPUs.  */
	  if (s390_tune >= PROCESSOR_2964_Z13
	      && loop->ninsns <= BLOCK_MEM_OPS_LOOP_INSNS
	      && ((set = single_set (insn)) != NULL_RTX)
	      && ((GET_MODE (SET_DEST (set)) == BLKmode
		   && (GET_MODE (SET_SRC (set)) == BLKmode
		       || SET_SRC (set) == const0_rtx))
		  || (GET_CODE (SET_SRC (set)) == COMPARE
		      && GET_MODE (XEXP (SET_SRC (set), 0)) == BLKmode
		      && GET_MODE (XEXP (SET_SRC (set), 1)) == BLKmode)))
	    {
	      free (bbs);
	      return 1;
	    }

	  FOR_EACH_SUBRTX (iter, array, PATTERN (insn), NONCONST)
	    if (MEM_P (*iter))
	      mem_count += 1;
	}
  free (bbs);

  /* Prevent division by zero, and we do not need to adjust nunroll in this case.  */
  if (mem_count == 0)
    return nunroll;

  switch (loop_depth(loop))
    {
    case 1:
      return MIN (nunroll, 28 / mem_count);
    case 2:
      return MIN (nunroll, 22 / mem_count);
    default:
      return MIN (nunroll, 16 / mem_count);
    }
}

/* Restore the current options.  This is a hook function and also called
   internally.  */

static void
s390_function_specific_restore (struct gcc_options *opts,
				struct gcc_options */* opts_set */,
				struct cl_target_option *ptr ATTRIBUTE_UNUSED)
{
  opts->x_s390_cost_pointer = (long)processor_table[opts->x_s390_tune].cost;
}

static void
s390_default_align (struct gcc_options *opts)
{
  /* Set the default function alignment to 16 in order to get rid of
     some unwanted performance effects. */
  if (opts->x_flag_align_functions && !opts->x_str_align_functions
      && opts->x_s390_tune >= PROCESSOR_2964_Z13)
    opts->x_str_align_functions = "16";
}

static void
s390_override_options_after_change (void)
{
  s390_default_align (&global_options);

  /* Explicit -funroll-loops turns -munroll-only-small-loops off.  */
  if ((OPTION_SET_P (flag_unroll_loops) && flag_unroll_loops)
       || (OPTION_SET_P (flag_unroll_all_loops)
	   && flag_unroll_all_loops))
    {
      if (!OPTION_SET_P (unroll_only_small_loops))
	unroll_only_small_loops = 0;
      if (!OPTION_SET_P (flag_cunroll_grow_size))
	flag_cunroll_grow_size = 1;
    }
  else if (!OPTION_SET_P (flag_cunroll_grow_size))
    flag_cunroll_grow_size = flag_peel_loops || optimize >= 3;
}

static void
s390_option_override_internal (struct gcc_options *opts,
			       struct gcc_options *opts_set)
{
  /* Architecture mode defaults according to ABI.  */
  if (!(opts_set->x_target_flags & MASK_ZARCH))
    {
      if (TARGET_64BIT)
	opts->x_target_flags |= MASK_ZARCH;
      else
	opts->x_target_flags &= ~MASK_ZARCH;
    }

  /* Set the march default in case it hasn't been specified on cmdline.  */
  if (!opts_set->x_s390_arch)
    opts->x_s390_arch = PROCESSOR_2064_Z900;

  opts->x_s390_arch_flags = processor_flags_table[(int) opts->x_s390_arch];

  /* Determine processor to tune for.  */
  if (!opts_set->x_s390_tune)
    opts->x_s390_tune = opts->x_s390_arch;

  opts->x_s390_tune_flags = processor_flags_table[opts->x_s390_tune];

  /* Sanity checks.  */
  if (opts->x_s390_arch == PROCESSOR_NATIVE
      || opts->x_s390_tune == PROCESSOR_NATIVE)
    gcc_unreachable ();
  if (TARGET_64BIT && !TARGET_ZARCH_P (opts->x_target_flags))
    error ("64-bit ABI not supported in ESA/390 mode");

  if (opts->x_s390_indirect_branch == indirect_branch_thunk_inline
      || opts->x_s390_indirect_branch_call == indirect_branch_thunk_inline
      || opts->x_s390_function_return == indirect_branch_thunk_inline
      || opts->x_s390_function_return_reg == indirect_branch_thunk_inline
      || opts->x_s390_function_return_mem == indirect_branch_thunk_inline)
    error ("thunk-inline is only supported with %<-mindirect-branch-jump%>");

  if (opts->x_s390_indirect_branch != indirect_branch_keep)
    {
      if (!opts_set->x_s390_indirect_branch_call)
	opts->x_s390_indirect_branch_call = opts->x_s390_indirect_branch;

      if (!opts_set->x_s390_indirect_branch_jump)
	opts->x_s390_indirect_branch_jump = opts->x_s390_indirect_branch;
    }

  if (opts->x_s390_function_return != indirect_branch_keep)
    {
      if (!opts_set->x_s390_function_return_reg)
	opts->x_s390_function_return_reg = opts->x_s390_function_return;

      if (!opts_set->x_s390_function_return_mem)
	opts->x_s390_function_return_mem = opts->x_s390_function_return;
    }

  /* Enable hardware transactions if available and not explicitly
     disabled by user.  E.g. with -m31 -march=zEC12 -mzarch */
  if (!TARGET_OPT_HTM_P (opts_set->x_target_flags))
    {
      if (TARGET_CPU_HTM_P (opts) && TARGET_ZARCH_P (opts->x_target_flags))
	opts->x_target_flags |= MASK_OPT_HTM;
      else
	opts->x_target_flags &= ~MASK_OPT_HTM;
    }

  if (TARGET_OPT_VX_P (opts_set->x_target_flags))
    {
      if (TARGET_OPT_VX_P (opts->x_target_flags))
	{
	  if (!TARGET_CPU_VX_P (opts))
	    error ("hardware vector support not available on %s",
		   processor_table[(int)opts->x_s390_arch].name);
	  if (TARGET_SOFT_FLOAT_P (opts->x_target_flags))
	    error ("hardware vector support not available with "
		   "%<-msoft-float%>");
	}
    }
  else
    {
      if (TARGET_CPU_VX_P (opts))
	/* Enable vector support if available and not explicitly disabled
	   by user.  E.g. with -m31 -march=z13 -mzarch */
	opts->x_target_flags |= MASK_OPT_VX;
      else
	opts->x_target_flags &= ~MASK_OPT_VX;
    }

  /* Use hardware DFP if available and not explicitly disabled by
     user. E.g. with -m31 -march=z10 -mzarch   */
  if (!TARGET_HARD_DFP_P (opts_set->x_target_flags))
    {
      if (TARGET_DFP_P (opts))
	opts->x_target_flags |= MASK_HARD_DFP;
      else
	opts->x_target_flags &= ~MASK_HARD_DFP;
    }

  if (TARGET_HARD_DFP_P (opts->x_target_flags) && !TARGET_DFP_P (opts))
    {
      if (TARGET_HARD_DFP_P (opts_set->x_target_flags))
	{
	  if (!TARGET_CPU_DFP_P (opts))
	    error ("hardware decimal floating-point instructions"
		   " not available on %s",
		   processor_table[(int)opts->x_s390_arch].name);
	  if (!TARGET_ZARCH_P (opts->x_target_flags))
	    error ("hardware decimal floating-point instructions"
		   " not available in ESA/390 mode");
	}
      else
	opts->x_target_flags &= ~MASK_HARD_DFP;
    }

  if (TARGET_SOFT_FLOAT_P (opts_set->x_target_flags)
      && TARGET_SOFT_FLOAT_P (opts->x_target_flags))
    {
      if (TARGET_HARD_DFP_P (opts_set->x_target_flags)
	  && TARGET_HARD_DFP_P (opts->x_target_flags))
	error ("%<-mhard-dfp%> cannot be used in conjunction with "
	       "%<-msoft-float%>");

      opts->x_target_flags &= ~MASK_HARD_DFP;
    }

  if (TARGET_BACKCHAIN_P (opts->x_target_flags)
      && TARGET_PACKED_STACK_P (opts->x_target_flags)
      && TARGET_HARD_FLOAT_P (opts->x_target_flags))
    error ("%<-mbackchain%> %<-mpacked-stack%> %<-mhard-float%> are not "
	   "supported in combination");

  if (opts->x_s390_stack_size)
    {
      if (opts->x_s390_stack_guard >= opts->x_s390_stack_size)
	error ("stack size must be greater than the stack guard value");
      else if (opts->x_s390_stack_size > 1 << 16)
	error ("stack size must not be greater than 64k");
    }
  else if (opts->x_s390_stack_guard)
    error ("%<-mstack-guard%> implies use of %<-mstack-size%>");

  /* Our implementation of the stack probe requires the probe interval
     to be used as displacement in an address operand.  The maximum
     probe interval currently is 64k.  This would exceed short
     displacements.  Trim that value down to 4k if that happens.  This
     might result in too many probes being generated only on the
     oldest supported machine level z900.  */
  if (!DISP_IN_RANGE ((1 << param_stack_clash_protection_probe_interval)))
    param_stack_clash_protection_probe_interval = 12;

#if TARGET_TPF != 0
  if (!CONST_OK_FOR_J (opts->x_s390_tpf_trace_hook_prologue_check))
    error ("%<-mtpf-trace-hook-prologue-check%> requires integer in range 0-4095");

  if (!CONST_OK_FOR_J (opts->x_s390_tpf_trace_hook_prologue_target))
    error ("%<-mtpf-trace-hook-prologue-target%> requires integer in range 0-4095");

  if (!CONST_OK_FOR_J (opts->x_s390_tpf_trace_hook_epilogue_check))
    error ("%<-mtpf-trace-hook-epilogue-check%> requires integer in range 0-4095");

  if (!CONST_OK_FOR_J (opts->x_s390_tpf_trace_hook_epilogue_target))
    error ("%<-mtpf-trace-hook-epilogue-target%> requires integer in range 0-4095");

  if (s390_tpf_trace_skip)
    {
      opts->x_s390_tpf_trace_hook_prologue_target = TPF_TRACE_PROLOGUE_SKIP_TARGET;
      opts->x_s390_tpf_trace_hook_epilogue_target = TPF_TRACE_EPILOGUE_SKIP_TARGET;
    }
#endif

#ifdef TARGET_DEFAULT_LONG_DOUBLE_128
  if (!TARGET_LONG_DOUBLE_128_P (opts_set->x_target_flags))
    opts->x_target_flags |= MASK_LONG_DOUBLE_128;
#endif

  if (opts->x_s390_tune >= PROCESSOR_2097_Z10)
    {
      SET_OPTION_IF_UNSET (opts, opts_set, param_max_unrolled_insns,
			   100);
      SET_OPTION_IF_UNSET (opts, opts_set, param_max_unroll_times, 32);
      SET_OPTION_IF_UNSET (opts, opts_set, param_max_completely_peeled_insns,
			   2000);
      SET_OPTION_IF_UNSET (opts, opts_set, param_max_completely_peel_times,
			   64);
    }

  SET_OPTION_IF_UNSET (opts, opts_set, param_max_pending_list_length,
		       256);
  /* values for loop prefetching */
  SET_OPTION_IF_UNSET (opts, opts_set, param_l1_cache_line_size, 256);
  SET_OPTION_IF_UNSET (opts, opts_set, param_l1_cache_size, 128);
  /* s390 has more than 2 levels and the size is much larger.  Since
     we are always running virtualized assume that we only get a small
     part of the caches above l1.  */
  SET_OPTION_IF_UNSET (opts, opts_set, param_l2_cache_size, 1500);
  SET_OPTION_IF_UNSET (opts, opts_set,
		       param_prefetch_min_insn_to_mem_ratio, 2);
  SET_OPTION_IF_UNSET (opts, opts_set, param_simultaneous_prefetches, 6);

  /* Use the alternative scheduling-pressure algorithm by default.  */
  SET_OPTION_IF_UNSET (opts, opts_set, param_sched_pressure_algorithm, 2);
  SET_OPTION_IF_UNSET (opts, opts_set, param_min_vect_loop_bound, 2);

  /* Set the default alignment.  */
  s390_default_align (opts);

  /* Set unroll options.  */
  s390_override_options_after_change ();

  /* Call target specific restore function to do post-init work.  At the moment,
     this just sets opts->x_s390_cost_pointer.  */
  s390_function_specific_restore (opts, opts_set, NULL);

  /* Check whether -mfentry is supported. It cannot be used in 31-bit mode,
     because 31-bit PLT stubs assume that %r12 contains GOT address, which is
     not the case when the code runs before the prolog. */
  if (opts->x_flag_fentry && !TARGET_64BIT)
    error ("%<-mfentry%> is supported only for 64-bit CPUs");
}

static void
s390_option_override (void)
{
  unsigned int i;
  cl_deferred_option *opt;
  vec<cl_deferred_option> *v =
    (vec<cl_deferred_option> *) s390_deferred_options;

  if (v)
    FOR_EACH_VEC_ELT (*v, i, opt)
      {
	switch (opt->opt_index)
	  {
	  case OPT_mhotpatch_:
	    {
	      int val1;
	      int val2;
	      char *s = strtok (ASTRDUP (opt->arg), ",");
	      char *t = strtok (NULL, "\0");

	      if (t != NULL)
		{
		  val1 = integral_argument (s);
		  val2 = integral_argument (t);
		}
	      else
		{
		  val1 = -1;
		  val2 = -1;
		}
	      if (val1 == -1 || val2 == -1)
		{
		  /* argument is not a plain number */
		  error ("arguments to %qs should be non-negative integers",
			 "-mhotpatch=n,m");
		  break;
		}
	      else if (val1 > s390_hotpatch_hw_max
		       || val2 > s390_hotpatch_hw_max)
		{
		  error ("argument to %qs is too large (max. %d)",
			 "-mhotpatch=n,m", s390_hotpatch_hw_max);
		  break;
		}
	      s390_hotpatch_hw_before_label = val1;
	      s390_hotpatch_hw_after_label = val2;
	      break;
	    }
	  default:
	    gcc_unreachable ();
	  }
      }

  /* Set up function hooks.  */
  init_machine_status = s390_init_machine_status;

  s390_option_override_internal (&global_options, &global_options_set);

  /* Save the initial options in case the user does function specific
     options.  */
  target_option_default_node
    = build_target_option_node (&global_options, &global_options_set);
  target_option_current_node = target_option_default_node;

  /* This cannot reside in s390_option_optimization_table since HAVE_prefetch
     requires the arch flags to be evaluated already.  Since prefetching
     is beneficial on s390, we enable it if available.  */
  if (flag_prefetch_loop_arrays < 0 && HAVE_prefetch && optimize >= 3)
    flag_prefetch_loop_arrays = 1;

  if (!s390_pic_data_is_text_relative && !flag_pic)
    error ("%<-mno-pic-data-is-text-relative%> cannot be used without "
	   "%<-fpic%>/%<-fPIC%>");

  if (TARGET_TPF)
    {
      /* Don't emit DWARF3/4 unless specifically selected.  The TPF
	 debuggers do not yet support DWARF 3/4.  */
      if (!OPTION_SET_P (dwarf_strict))
	dwarf_strict = 1;
      if (!OPTION_SET_P (dwarf_version))
	dwarf_version = 2;
    }
}

#if S390_USE_TARGET_ATTRIBUTE
/* Inner function to process the attribute((target(...))), take an argument and
   set the current options from the argument. If we have a list, recursively go
   over the list.  */

static bool
s390_valid_target_attribute_inner_p (tree args,
				     struct gcc_options *opts,
				     struct gcc_options *new_opts_set,
				     bool force_pragma)
{
  char *next_optstr;
  bool ret = true;

#define S390_ATTRIB(S,O,A)  { S, sizeof (S)-1, O, A, 0 }
#define S390_PRAGMA(S,O,A)  { S, sizeof (S)-1, O, A, 1 }
  static const struct
  {
    const char *string;
    size_t len;
    int opt;
    int has_arg;
    int only_as_pragma;
  } attrs[] = {
    /* enum options */
    S390_ATTRIB ("arch=", OPT_march_, 1),
    S390_ATTRIB ("tune=", OPT_mtune_, 1),
    /* uinteger options */
    S390_ATTRIB ("stack-guard=", OPT_mstack_guard_, 1),
    S390_ATTRIB ("stack-size=", OPT_mstack_size_, 1),
    S390_ATTRIB ("branch-cost=", OPT_mbranch_cost_, 1),
    S390_ATTRIB ("warn-framesize=", OPT_mwarn_framesize_, 1),
    /* flag options */
    S390_ATTRIB ("backchain", OPT_mbackchain, 0),
    S390_ATTRIB ("hard-dfp", OPT_mhard_dfp, 0),
    S390_ATTRIB ("hard-float", OPT_mhard_float, 0),
    S390_ATTRIB ("htm", OPT_mhtm, 0),
    S390_ATTRIB ("vx", OPT_mvx, 0),
    S390_ATTRIB ("packed-stack", OPT_mpacked_stack, 0),
    S390_ATTRIB ("small-exec", OPT_msmall_exec, 0),
    S390_ATTRIB ("soft-float", OPT_msoft_float, 0),
    S390_ATTRIB ("mvcle", OPT_mmvcle, 0),
    S390_PRAGMA ("zvector", OPT_mzvector, 0),
    /* boolean options */
    S390_ATTRIB ("warn-dynamicstack", OPT_mwarn_dynamicstack, 0),
  };
#undef S390_ATTRIB
#undef S390_PRAGMA

  /* If this is a list, recurse to get the options.  */
  if (TREE_CODE (args) == TREE_LIST)
    {
      bool ret = true;
      int num_pragma_values;
      int i;

      /* Note: attribs.cc:decl_attributes prepends the values from
	 current_target_pragma to the list of target attributes.  To determine
	 whether we're looking at a value of the attribute or the pragma we
	 assume that the first [list_length (current_target_pragma)] values in
	 the list are the values from the pragma.  */
      num_pragma_values = (!force_pragma && current_target_pragma != NULL)
	? list_length (current_target_pragma) : 0;
      for (i = 0; args; args = TREE_CHAIN (args), i++)
	{
	  bool is_pragma;

	  is_pragma = (force_pragma || i < num_pragma_values);
	  if (TREE_VALUE (args)
	      && !s390_valid_target_attribute_inner_p (TREE_VALUE (args),
						       opts, new_opts_set,
						       is_pragma))
	    {
	      ret = false;
	    }
	}
      return ret;
    }

  else if (TREE_CODE (args) != STRING_CST)
    {
      error ("attribute %<target%> argument not a string");
      return false;
    }

  /* Handle multiple arguments separated by commas.  */
  next_optstr = ASTRDUP (TREE_STRING_POINTER (args));

  while (next_optstr && *next_optstr != '\0')
    {
      char *p = next_optstr;
      char *orig_p = p;
      char *comma = strchr (next_optstr, ',');
      size_t len, opt_len;
      int opt;
      bool opt_set_p;
      char ch;
      unsigned i;
      int mask = 0;
      enum cl_var_type var_type;
      bool found;

      if (comma)
	{
	  *comma = '\0';
	  len = comma - next_optstr;
	  next_optstr = comma + 1;
	}
      else
	{
	  len = strlen (p);
	  next_optstr = NULL;
	}

      /* Recognize no-xxx.  */
      if (len > 3 && p[0] == 'n' && p[1] == 'o' && p[2] == '-')
	{
	  opt_set_p = false;
	  p += 3;
	  len -= 3;
	}
      else
	opt_set_p = true;

      /* Find the option.  */
      ch = *p;
      found = false;
      for (i = 0; i < ARRAY_SIZE (attrs); i++)
	{
	  opt_len = attrs[i].len;
	  if (ch == attrs[i].string[0]
	      && ((attrs[i].has_arg) ? len > opt_len : len == opt_len)
	      && memcmp (p, attrs[i].string, opt_len) == 0)
	    {
	      opt = attrs[i].opt;
	      if (!opt_set_p && cl_options[opt].cl_reject_negative)
		continue;
	      mask = cl_options[opt].var_value;
	      var_type = cl_options[opt].var_type;
	      found = true;
	      break;
	    }
	}

      /* Process the option.  */
      if (!found)
	{
	  error ("attribute %<target%> argument %qs is unknown", orig_p);
	  return false;
	}
      else if (attrs[i].only_as_pragma && !force_pragma)
	{
	  /* Value is not allowed for the target attribute.  */
	  error ("value %qs is not supported by attribute %<target%>",
		 attrs[i].string);
	  return false;
	}

      else if (var_type == CLVC_BIT_SET || var_type == CLVC_BIT_CLEAR)
	{
	  if (var_type == CLVC_BIT_CLEAR)
	    opt_set_p = !opt_set_p;

	  if (opt_set_p)
	    opts->x_target_flags |= mask;
	  else
	    opts->x_target_flags &= ~mask;
	  new_opts_set->x_target_flags |= mask;
	}

      else if (cl_options[opt].var_type == CLVC_INTEGER)
	{
	  int value;

	  if (cl_options[opt].cl_uinteger)
	    {
	      /* Unsigned integer argument.  Code based on the function
		 decode_cmdline_option () in opts-common.cc.  */
	      value = integral_argument (p + opt_len);
	    }
	  else
	    value = (opt_set_p) ? 1 : 0;

	  if (value != -1)
	    {
	      struct cl_decoded_option decoded;

	      /* Value range check; only implemented for numeric and boolean
		 options at the moment.  */
	      generate_option (opt, NULL, value, CL_TARGET, &decoded);
	      s390_handle_option (opts, new_opts_set, &decoded, input_location);
	      set_option (opts, new_opts_set, opt, value,
			  p + opt_len, DK_UNSPECIFIED, input_location,
			  global_dc);
	    }
	  else
	    {
	      error ("attribute %<target%> argument %qs is unknown", orig_p);
	      ret = false;
	    }
	}

      else if (cl_options[opt].var_type == CLVC_ENUM)
	{
	  bool arg_ok;
	  int value;

	  arg_ok = opt_enum_arg_to_value (opt, p + opt_len, &value, CL_TARGET);
	  if (arg_ok)
	    set_option (opts, new_opts_set, opt, value,
			p + opt_len, DK_UNSPECIFIED, input_location,
			global_dc);
	  else
	    {
	      error ("attribute %<target%> argument %qs is unknown", orig_p);
	      ret = false;
	    }
	}

      else
	gcc_unreachable ();
    }
  return ret;
}

/* Return a TARGET_OPTION_NODE tree of the target options listed or NULL.  */

tree
s390_valid_target_attribute_tree (tree args,
				  struct gcc_options *opts,
				  const struct gcc_options *opts_set,
				  bool force_pragma)
{
  tree t = NULL_TREE;
  struct gcc_options new_opts_set;

  memset (&new_opts_set, 0, sizeof (new_opts_set));

  /* Process each of the options on the chain.  */
  if (! s390_valid_target_attribute_inner_p (args, opts, &new_opts_set,
					     force_pragma))
    return error_mark_node;

  /* If some option was set (even if it has not changed), rerun
     s390_option_override_internal, and then save the options away.  */
  if (new_opts_set.x_target_flags
      || new_opts_set.x_s390_arch
      || new_opts_set.x_s390_tune
      || new_opts_set.x_s390_stack_guard
      || new_opts_set.x_s390_stack_size
      || new_opts_set.x_s390_branch_cost
      || new_opts_set.x_s390_warn_framesize
      || new_opts_set.x_s390_warn_dynamicstack_p)
    {
      const unsigned char *src = (const unsigned char *)opts_set;
      unsigned char *dest = (unsigned char *)&new_opts_set;
      unsigned int i;

      /* Merge the original option flags into the new ones.  */
      for (i = 0; i < sizeof(*opts_set); i++)
	dest[i] |= src[i];

      /* Do any overrides, such as arch=xxx, or tune=xxx support.  */
      s390_option_override_internal (opts, &new_opts_set);
      /* Save the current options unless we are validating options for
	 #pragma.  */
      t = build_target_option_node (opts, &new_opts_set);
    }
  return t;
}

/* Hook to validate attribute((target("string"))).  */

static bool
s390_valid_target_attribute_p (tree fndecl,
			       tree ARG_UNUSED (name),
			       tree args,
			       int ARG_UNUSED (flags))
{
  struct gcc_options func_options, func_options_set;
  tree new_target, new_optimize;
  bool ret = true;

  /* attribute((target("default"))) does nothing, beyond
     affecting multi-versioning.  */
  if (TREE_VALUE (args)
      && TREE_CODE (TREE_VALUE (args)) == STRING_CST
      && TREE_CHAIN (args) == NULL_TREE
      && strcmp (TREE_STRING_POINTER (TREE_VALUE (args)), "default") == 0)
    return true;

  tree old_optimize
    = build_optimization_node (&global_options, &global_options_set);

  /* Get the optimization options of the current function.  */
  tree func_optimize = DECL_FUNCTION_SPECIFIC_OPTIMIZATION (fndecl);

  if (!func_optimize)
    func_optimize = old_optimize;

  /* Init func_options.  */
  memset (&func_options, 0, sizeof (func_options));
  init_options_struct (&func_options, NULL);
  lang_hooks.init_options_struct (&func_options);
  memset (&func_options_set, 0, sizeof (func_options_set));

  cl_optimization_restore (&func_options, &func_options_set,
			   TREE_OPTIMIZATION (func_optimize));

  /* Initialize func_options to the default before its target options can
     be set.  */
  cl_target_option_restore (&func_options, &func_options_set,
			    TREE_TARGET_OPTION (target_option_default_node));

  new_target = s390_valid_target_attribute_tree (args, &func_options,
						 &global_options_set,
						 (args ==
						  current_target_pragma));
  new_optimize = build_optimization_node (&func_options, &func_options_set);
  if (new_target == error_mark_node)
    ret = false;
  else if (fndecl && new_target)
    {
      DECL_FUNCTION_SPECIFIC_TARGET (fndecl) = new_target;
      if (old_optimize != new_optimize)
	DECL_FUNCTION_SPECIFIC_OPTIMIZATION (fndecl) = new_optimize;
    }
  return ret;
}

/* Hook to determine if one function can safely inline another.  */

static bool
s390_can_inline_p (tree caller, tree callee)
{
  /* Flags which if present in the callee are required in the caller as well.  */
  const unsigned HOST_WIDE_INT caller_required_masks = MASK_OPT_HTM;

  /* Flags which affect the ABI and in general prevent inlining.  */
  unsigned HOST_WIDE_INT must_match_masks
    = (MASK_64BIT | MASK_ZARCH | MASK_HARD_DFP | MASK_SOFT_FLOAT
       | MASK_LONG_DOUBLE_128 | MASK_OPT_VX);

  /* Flags which we in general want to prevent inlining but accept for
     always_inline.  */
  const unsigned HOST_WIDE_INT always_inline_safe_masks
    = MASK_MVCLE | MASK_BACKCHAIN | MASK_SMALL_EXEC;

  const HOST_WIDE_INT all_masks
     = (caller_required_masks | must_match_masks | always_inline_safe_masks
	| MASK_DEBUG_ARG | MASK_PACKED_STACK | MASK_ZVECTOR);

  tree caller_tree = DECL_FUNCTION_SPECIFIC_TARGET (caller);
  tree callee_tree = DECL_FUNCTION_SPECIFIC_TARGET (callee);

  if (!callee_tree)
    callee_tree = target_option_default_node;
  if (!caller_tree)
    caller_tree = target_option_default_node;
  if (callee_tree == caller_tree)
    return true;

  struct cl_target_option *caller_opts = TREE_TARGET_OPTION (caller_tree);
  struct cl_target_option *callee_opts = TREE_TARGET_OPTION (callee_tree);

  /* If one of these triggers make sure to add proper handling of your
     new flag to this hook.  */
  gcc_assert (!(caller_opts->x_target_flags & ~all_masks));
  gcc_assert (!(callee_opts->x_target_flags & ~all_masks));

  bool always_inline
    = (DECL_DISREGARD_INLINE_LIMITS (callee)
       && lookup_attribute ("always_inline", DECL_ATTRIBUTES (callee)));

  if (!always_inline)
    must_match_masks |= always_inline_safe_masks;

  /* Inlining a hard float function into a soft float function is only
     allowed if the hard float function doesn't actually make use of
     floating point.

     We are called from FEs for multi-versioning call optimization, so
     beware of ipa_fn_summaries not available.  */
  if (always_inline && ipa_fn_summaries
      && !ipa_fn_summaries->get(cgraph_node::get (callee))->fp_expressions)
    must_match_masks &= ~(MASK_HARD_DFP | MASK_SOFT_FLOAT);

  if ((caller_opts->x_target_flags & must_match_masks)
      != (callee_opts->x_target_flags & must_match_masks))
    return false;

  if (~(caller_opts->x_target_flags & caller_required_masks)
      & (callee_opts->x_target_flags & caller_required_masks))
    return false;

  /* Don't inline functions to be compiled for a more recent arch into a
     function for an older arch.  */
  if (caller_opts->x_s390_arch < callee_opts->x_s390_arch)
    return false;

  if (!always_inline && caller_opts->x_s390_tune != callee_opts->x_s390_tune)
    return false;

  return true;
}
#endif

/* Set VAL to correct enum value according to the indirect-branch or
   function-return attribute in ATTR.  */

static inline void
s390_indirect_branch_attrvalue (tree attr, enum indirect_branch *val)
{
  const char *str = TREE_STRING_POINTER (TREE_VALUE (TREE_VALUE (attr)));
  if (strcmp (str, "keep") == 0)
    *val = indirect_branch_keep;
  else if (strcmp (str, "thunk") == 0)
    *val = indirect_branch_thunk;
  else if (strcmp (str, "thunk-inline") == 0)
    *val = indirect_branch_thunk_inline;
  else if (strcmp (str, "thunk-extern") == 0)
    *val = indirect_branch_thunk_extern;
}

/* Memorize the setting for -mindirect-branch* and -mfunction-return*
   from either the cmdline or the function attributes in
   cfun->machine.  */

static void
s390_indirect_branch_settings (tree fndecl)
{
  tree attr;

  if (!fndecl)
    return;

  /* Initialize with the cmdline options and let the attributes
     override it.  */
  cfun->machine->indirect_branch_jump = s390_indirect_branch_jump;
  cfun->machine->indirect_branch_call = s390_indirect_branch_call;

  cfun->machine->function_return_reg = s390_function_return_reg;
  cfun->machine->function_return_mem = s390_function_return_mem;

  if ((attr = lookup_attribute ("indirect_branch",
				DECL_ATTRIBUTES (fndecl))))
    {
      s390_indirect_branch_attrvalue (attr,
				      &cfun->machine->indirect_branch_jump);
      s390_indirect_branch_attrvalue (attr,
				      &cfun->machine->indirect_branch_call);
    }

  if ((attr = lookup_attribute ("indirect_branch_jump",
				DECL_ATTRIBUTES (fndecl))))
    s390_indirect_branch_attrvalue (attr, &cfun->machine->indirect_branch_jump);

  if ((attr = lookup_attribute ("indirect_branch_call",
				DECL_ATTRIBUTES (fndecl))))
    s390_indirect_branch_attrvalue (attr, &cfun->machine->indirect_branch_call);

  if ((attr = lookup_attribute ("function_return",
				DECL_ATTRIBUTES (fndecl))))
    {
      s390_indirect_branch_attrvalue (attr,
				      &cfun->machine->function_return_reg);
      s390_indirect_branch_attrvalue (attr,
				      &cfun->machine->function_return_mem);
    }

  if ((attr = lookup_attribute ("function_return_reg",
				DECL_ATTRIBUTES (fndecl))))
    s390_indirect_branch_attrvalue (attr, &cfun->machine->function_return_reg);

  if ((attr = lookup_attribute ("function_return_mem",
				DECL_ATTRIBUTES (fndecl))))
    s390_indirect_branch_attrvalue (attr, &cfun->machine->function_return_mem);
}

#if S390_USE_TARGET_ATTRIBUTE
/* Restore targets globals from NEW_TREE and invalidate s390_previous_fndecl
   cache.  */

void
s390_activate_target_options (tree new_tree)
{
  cl_target_option_restore (&global_options, &global_options_set,
			    TREE_TARGET_OPTION (new_tree));
  if (TREE_TARGET_GLOBALS (new_tree))
    restore_target_globals (TREE_TARGET_GLOBALS (new_tree));
  else if (new_tree == target_option_default_node)
    restore_target_globals (&default_target_globals);
  else
    TREE_TARGET_GLOBALS (new_tree) = save_target_globals_default_opts ();
  s390_previous_fndecl = NULL_TREE;
}
#endif

/* Establish appropriate back-end context for processing the function
   FNDECL.  The argument might be NULL to indicate processing at top
   level, outside of any function scope.  */
static void
s390_set_current_function (tree fndecl)
{
#if S390_USE_TARGET_ATTRIBUTE
  /* Only change the context if the function changes.  This hook is called
     several times in the course of compiling a function, and we don't want to
     slow things down too much or call target_reinit when it isn't safe.  */
  if (fndecl == s390_previous_fndecl)
    {
      s390_indirect_branch_settings (fndecl);
      return;
    }

  tree old_tree;
  if (s390_previous_fndecl == NULL_TREE)
    old_tree = target_option_current_node;
  else if (DECL_FUNCTION_SPECIFIC_TARGET (s390_previous_fndecl))
    old_tree = DECL_FUNCTION_SPECIFIC_TARGET (s390_previous_fndecl);
  else
    old_tree = target_option_default_node;

  if (fndecl == NULL_TREE)
    {
      if (old_tree != target_option_current_node)
	s390_activate_target_options (target_option_current_node);
      return;
    }

  tree new_tree = DECL_FUNCTION_SPECIFIC_TARGET (fndecl);
  if (new_tree == NULL_TREE)
    new_tree = target_option_default_node;

  if (old_tree != new_tree)
    s390_activate_target_options (new_tree);
  s390_previous_fndecl = fndecl;
#endif
  s390_indirect_branch_settings (fndecl);
}

/* Implement TARGET_USE_BY_PIECES_INFRASTRUCTURE_P.  */

static bool
s390_use_by_pieces_infrastructure_p (unsigned HOST_WIDE_INT size,
				     unsigned int align ATTRIBUTE_UNUSED,
				     enum by_pieces_operation op ATTRIBUTE_UNUSED,
				     bool speed_p ATTRIBUTE_UNUSED)
{
  return (size == 1 || size == 2
	  || size == 4 || (TARGET_ZARCH && size == 8));
}

/* Implement TARGET_ATOMIC_ASSIGN_EXPAND_FENV hook.  */

static void
s390_atomic_assign_expand_fenv (tree *hold, tree *clear, tree *update)
{
  tree sfpc = s390_builtin_decls[S390_BUILTIN_s390_sfpc];
  tree efpc = s390_builtin_decls[S390_BUILTIN_s390_efpc];
  tree call_efpc = build_call_expr (efpc, 0);
  tree fenv_var = create_tmp_var_raw (unsigned_type_node);

#define FPC_EXCEPTION_MASK	 HOST_WIDE_INT_UC (0xf8000000)
#define FPC_FLAGS_MASK		 HOST_WIDE_INT_UC (0x00f80000)
#define FPC_DXC_MASK		 HOST_WIDE_INT_UC (0x0000ff00)
#define FPC_EXCEPTION_MASK_SHIFT HOST_WIDE_INT_UC (24)
#define FPC_FLAGS_SHIFT		 HOST_WIDE_INT_UC (16)
#define FPC_DXC_SHIFT		 HOST_WIDE_INT_UC (8)

  /* Generates the equivalent of feholdexcept (&fenv_var)

     fenv_var = __builtin_s390_efpc ();
     __builtin_s390_sfpc (fenv_var & mask) */
  tree old_fpc = build4 (TARGET_EXPR, unsigned_type_node, fenv_var, call_efpc,
			 NULL_TREE, NULL_TREE);
  tree new_fpc
    = build2 (BIT_AND_EXPR, unsigned_type_node, fenv_var,
	      build_int_cst (unsigned_type_node,
			     ~(FPC_DXC_MASK | FPC_FLAGS_MASK
			       | FPC_EXCEPTION_MASK)));
  tree set_new_fpc = build_call_expr (sfpc, 1, new_fpc);
  *hold = build2 (COMPOUND_EXPR, void_type_node, old_fpc, set_new_fpc);

  /* Generates the equivalent of feclearexcept (FE_ALL_EXCEPT)

     __builtin_s390_sfpc (__builtin_s390_efpc () & mask) */
  new_fpc = build2 (BIT_AND_EXPR, unsigned_type_node, call_efpc,
		    build_int_cst (unsigned_type_node,
				   ~(FPC_DXC_MASK | FPC_FLAGS_MASK)));
  *clear = build_call_expr (sfpc, 1, new_fpc);

  /* Generates the equivalent of feupdateenv (fenv_var)

  old_fpc = __builtin_s390_efpc ();
  __builtin_s390_sfpc (fenv_var);
  __atomic_feraiseexcept ((old_fpc & FPC_FLAGS_MASK) >> FPC_FLAGS_SHIFT);  */

  old_fpc = create_tmp_var_raw (unsigned_type_node);
  tree store_old_fpc = build4 (TARGET_EXPR, void_type_node, old_fpc, call_efpc,
			       NULL_TREE, NULL_TREE);

  set_new_fpc = build_call_expr (sfpc, 1, fenv_var);

  tree raise_old_except = build2 (BIT_AND_EXPR, unsigned_type_node, old_fpc,
				  build_int_cst (unsigned_type_node,
						 FPC_FLAGS_MASK));
  raise_old_except = build2 (RSHIFT_EXPR, unsigned_type_node, raise_old_except,
			     build_int_cst (unsigned_type_node,
					    FPC_FLAGS_SHIFT));
  tree atomic_feraiseexcept
    = builtin_decl_implicit (BUILT_IN_ATOMIC_FERAISEEXCEPT);
  raise_old_except = build_call_expr (atomic_feraiseexcept,
				      1, raise_old_except);

  *update = build2 (COMPOUND_EXPR, void_type_node,
		    build2 (COMPOUND_EXPR, void_type_node,
			    store_old_fpc, set_new_fpc),
		    raise_old_except);

#undef FPC_EXCEPTION_MASK
#undef FPC_FLAGS_MASK
#undef FPC_DXC_MASK
#undef FPC_EXCEPTION_MASK_SHIFT
#undef FPC_FLAGS_SHIFT
#undef FPC_DXC_SHIFT
}

/* Return the vector mode to be used for inner mode MODE when doing
   vectorization.  */
static machine_mode
s390_preferred_simd_mode (scalar_mode mode)
{
  if (TARGET_VXE)
    switch (mode)
      {
      case E_SFmode:
	return V4SFmode;
      default:;
      }

  if (TARGET_VX)
    switch (mode)
      {
      case E_DFmode:
	return V2DFmode;
      case E_DImode:
	return V2DImode;
      case E_SImode:
	return V4SImode;
      case E_HImode:
	return V8HImode;
      case E_QImode:
	return V16QImode;
      default:;
      }
  return word_mode;
}

/* Our hardware does not require vectors to be strictly aligned.  */
static bool
s390_support_vector_misalignment (machine_mode mode ATTRIBUTE_UNUSED,
				  const_tree type ATTRIBUTE_UNUSED,
				  int misalignment ATTRIBUTE_UNUSED,
				  bool is_packed ATTRIBUTE_UNUSED)
{
  if (TARGET_VX)
    return true;

  return default_builtin_support_vector_misalignment (mode, type, misalignment,
						      is_packed);
}

/* The vector ABI requires vector types to be aligned on an 8 byte
   boundary (our stack alignment).  However, we allow this to be
   overriden by the user, while this definitely breaks the ABI.  */
static HOST_WIDE_INT
s390_vector_alignment (const_tree type)
{
  tree size = TYPE_SIZE (type);

  if (!TARGET_VX_ABI)
    return default_vector_alignment (type);

  if (TYPE_USER_ALIGN (type))
    return TYPE_ALIGN (type);

  if (tree_fits_uhwi_p (size)
      && tree_to_uhwi (size) < BIGGEST_ALIGNMENT)
    return tree_to_uhwi (size);

  return BIGGEST_ALIGNMENT;
}

/* Implement TARGET_CONSTANT_ALIGNMENT.  Alignment on even addresses for
   LARL instruction.  */

static HOST_WIDE_INT
s390_constant_alignment (const_tree, HOST_WIDE_INT align)
{
  return MAX (align, 16);
}

#ifdef HAVE_AS_MACHINE_MACHINEMODE
/* Implement TARGET_ASM_FILE_START.  */
static void
s390_asm_file_start (void)
{
  default_file_start ();
  s390_asm_output_machine_for_arch (asm_out_file);
}
#endif

/* Implement TARGET_ASM_FILE_END.  */
static void
s390_asm_file_end (void)
{
#ifdef HAVE_AS_GNU_ATTRIBUTE
  varpool_node *vnode;
  cgraph_node *cnode;

  FOR_EACH_VARIABLE (vnode)
    if (TREE_PUBLIC (vnode->decl))
      s390_check_type_for_vector_abi (TREE_TYPE (vnode->decl), false, false);

  FOR_EACH_FUNCTION (cnode)
    if (TREE_PUBLIC (cnode->decl))
      s390_check_type_for_vector_abi (TREE_TYPE (cnode->decl), false, false);


  if (s390_vector_abi != 0)
    fprintf (asm_out_file, "\t.gnu_attribute 8, %d\n",
	     s390_vector_abi);
#endif
  file_end_indicate_exec_stack ();

  if (flag_split_stack)
    file_end_indicate_split_stack ();
}

/* Return true if TYPE is a vector bool type.  */
static inline bool
s390_vector_bool_type_p (const_tree type)
{
  return TYPE_VECTOR_OPAQUE (type);
}

/* Return the diagnostic message string if the binary operation OP is
   not permitted on TYPE1 and TYPE2, NULL otherwise.  */
static const char*
s390_invalid_binary_op (int op ATTRIBUTE_UNUSED, const_tree type1, const_tree type2)
{
  bool bool1_p, bool2_p;
  bool plusminus_p;
  bool muldiv_p;
  bool compare_p;
  machine_mode mode1, mode2;

  if (!TARGET_ZVECTOR)
    return NULL;

  if (!VECTOR_TYPE_P (type1) || !VECTOR_TYPE_P (type2))
    return NULL;

  bool1_p = s390_vector_bool_type_p (type1);
  bool2_p = s390_vector_bool_type_p (type2);

  /* Mixing signed and unsigned types is forbidden for all
     operators.  */
  if (!bool1_p && !bool2_p
      && TYPE_UNSIGNED (type1) != TYPE_UNSIGNED (type2))
    return N_("types differ in signedness");

  plusminus_p = (op == PLUS_EXPR || op == MINUS_EXPR);
  muldiv_p = (op == MULT_EXPR || op == RDIV_EXPR || op == TRUNC_DIV_EXPR
	      || op == CEIL_DIV_EXPR || op == FLOOR_DIV_EXPR
	      || op == ROUND_DIV_EXPR);
  compare_p = (op == LT_EXPR || op == LE_EXPR || op == GT_EXPR || op == GE_EXPR
	       || op == EQ_EXPR || op == NE_EXPR);

  if (bool1_p && bool2_p && (plusminus_p || muldiv_p))
    return N_("binary operator does not support two vector bool operands");

  if (bool1_p != bool2_p && (muldiv_p || compare_p))
    return N_("binary operator does not support vector bool operand");

  mode1 = TYPE_MODE (type1);
  mode2 = TYPE_MODE (type2);

  if (bool1_p != bool2_p && plusminus_p
      && (GET_MODE_CLASS (mode1) == MODE_VECTOR_FLOAT
	  || GET_MODE_CLASS (mode2) == MODE_VECTOR_FLOAT))
    return N_("binary operator does not support mixing vector "
	      "bool with floating point vector operands");

  return NULL;
}

#if ENABLE_S390_EXCESS_FLOAT_PRECISION == 1
/* Implement TARGET_C_EXCESS_PRECISION to maintain historic behavior with older
   glibc versions

   For historical reasons, float_t and double_t had been typedef'ed to
   double on s390, causing operations on float_t to operate in a higher
   precision than is necessary.  However, it is not the case that SFmode
   operations have implicit excess precision, and we generate more optimal
   code if we let the compiler know no implicit extra precision is added.

   With a glibc with that "historic" definition, configure will enable this hook
   to set FLT_EVAL_METHOD to 1 for -fexcess-precision=standard (e.g., as implied
   by -std=cXY).  That means when we are compiling with -fexcess-precision=fast,
   the value we set for FLT_EVAL_METHOD will be out of line with the actual
   precision of float_t.

   Newer versions of glibc will be modified to derive the definition of float_t
   from FLT_EVAL_METHOD on s390x, as on many other architectures.  There,
   configure will disable this hook by default, so that we defer to the default
   of FLT_EVAL_METHOD_PROMOTE_TO_FLOAT and a resulting typedef of float_t to
   float.  Note that in that scenario, float_t and FLT_EVAL_METHOD will be in
   line independent of -fexcess-precision. */

static enum flt_eval_method
s390_excess_precision (enum excess_precision_type type)
{
  switch (type)
    {
      case EXCESS_PRECISION_TYPE_IMPLICIT:
      case EXCESS_PRECISION_TYPE_FAST:
	/* The fastest type to promote to will always be the native type,
	   whether that occurs with implicit excess precision or
	   otherwise.  */
	return FLT_EVAL_METHOD_PROMOTE_TO_FLOAT;
      case EXCESS_PRECISION_TYPE_STANDARD:
	/* Otherwise, when we are in a standards compliant mode, to
	   ensure consistency with the implementation in glibc, report that
	   float is evaluated to the range and precision of double.  */
	return FLT_EVAL_METHOD_PROMOTE_TO_DOUBLE;
      case EXCESS_PRECISION_TYPE_FLOAT16:
	error ("%<-fexcess-precision=16%> is not supported on this target");
	break;
      default:
	gcc_unreachable ();
    }
  return FLT_EVAL_METHOD_UNPREDICTABLE;
}
#endif

void
s390_rawmemchr (machine_mode elt_mode, rtx dst, rtx src, rtx pat)
{
  machine_mode vec_mode = mode_for_vector (as_a <scalar_int_mode> (elt_mode),
					   16 / GET_MODE_SIZE (elt_mode)).require();
  rtx lens = gen_reg_rtx (V16QImode);
  rtx pattern = gen_reg_rtx (vec_mode);
  rtx loop_start = gen_label_rtx ();
  rtx loop_end = gen_label_rtx ();
  rtx addr = gen_reg_rtx (Pmode);
  rtx offset = gen_reg_rtx (Pmode);
  rtx loadlen = gen_reg_rtx (SImode);
  rtx matchlen = gen_reg_rtx (SImode);
  rtx mem;

  pat = GEN_INT (trunc_int_for_mode (INTVAL (pat), elt_mode));
  emit_insn (gen_rtx_SET (pattern, gen_rtx_VEC_DUPLICATE (vec_mode, pat)));

  emit_move_insn (addr, XEXP (src, 0));

  // alignment
  emit_insn (gen_vlbb (lens, gen_rtx_MEM (BLKmode, addr), GEN_INT (6)));
  emit_insn (gen_lcbb (loadlen, addr, GEN_INT (6)));
  lens = convert_to_mode (vec_mode, lens, 1);
  emit_insn (gen_vec_vfees (vec_mode, lens, lens, pattern, GEN_INT (0)));
  lens = convert_to_mode (V4SImode, lens, 1);
  emit_insn (gen_vec_extractv4sisi (matchlen, lens, GEN_INT (1)));
  lens = convert_to_mode (vec_mode, lens, 1);
  emit_cmp_and_jump_insns (matchlen, loadlen, LT, NULL_RTX, SImode, 1, loop_end);
  force_expand_binop (Pmode, add_optab, addr, GEN_INT(16), addr, 1, OPTAB_DIRECT);
  force_expand_binop (Pmode, and_optab, addr, GEN_INT(~HOST_WIDE_INT_UC(0xf)), addr, 1, OPTAB_DIRECT);
  // now, addr is 16-byte aligned

  mem = gen_rtx_MEM (vec_mode, addr);
  set_mem_align (mem, 128);
  emit_move_insn (lens, mem);
  emit_insn (gen_vec_vfees (vec_mode, lens, lens, pattern, GEN_INT (VSTRING_FLAG_CS)));
  add_int_reg_note (s390_emit_ccraw_jump (4, EQ, loop_end),
		    REG_BR_PROB,
		    profile_probability::very_unlikely ().to_reg_br_prob_note ());

  emit_label (loop_start);
  LABEL_NUSES (loop_start) = 1;

  force_expand_binop (Pmode, add_optab, addr, GEN_INT (16), addr, 1, OPTAB_DIRECT);
  mem = gen_rtx_MEM (vec_mode, addr);
  set_mem_align (mem, 128);
  emit_move_insn (lens, mem);
  emit_insn (gen_vec_vfees (vec_mode, lens, lens, pattern, GEN_INT (VSTRING_FLAG_CS)));
  add_int_reg_note (s390_emit_ccraw_jump (4, NE, loop_start),
		    REG_BR_PROB,
		    profile_probability::very_likely ().to_reg_br_prob_note ());

  emit_label (loop_end);
  LABEL_NUSES (loop_end) = 1;

  if (TARGET_64BIT)
    {
      lens = convert_to_mode (V2DImode, lens, 1);
      emit_insn (gen_vec_extractv2didi (offset, lens, GEN_INT (0)));
    }
  else
    {
      lens = convert_to_mode (V4SImode, lens, 1);
      emit_insn (gen_vec_extractv4sisi (offset, lens, GEN_INT (1)));
    }
  force_expand_binop (Pmode, add_optab, addr, offset, dst, 1, OPTAB_DIRECT);
}

/* Implement the TARGET_ASAN_SHADOW_OFFSET hook.  */

static unsigned HOST_WIDE_INT
s390_asan_shadow_offset (void)
{
  return TARGET_64BIT ? HOST_WIDE_INT_1U << 52 : HOST_WIDE_INT_UC (0x20000000);
}

#ifdef HAVE_GAS_HIDDEN
# define USE_HIDDEN_LINKONCE 1
#else
# define USE_HIDDEN_LINKONCE 0
#endif

/* Output an indirect branch trampoline for target register REGNO.  */

static void
s390_output_indirect_thunk_function (unsigned int regno, bool z10_p)
{
  tree decl;
  char thunk_label[32];
  int i;

  if (z10_p)
    sprintf (thunk_label, TARGET_INDIRECT_BRANCH_THUNK_NAME_EXRL, regno);
  else
    sprintf (thunk_label, TARGET_INDIRECT_BRANCH_THUNK_NAME_EX,
	     INDIRECT_BRANCH_THUNK_REGNUM, regno);

  decl = build_decl (BUILTINS_LOCATION, FUNCTION_DECL,
		     get_identifier (thunk_label),
		     build_function_type_list (void_type_node, NULL_TREE));
  DECL_RESULT (decl) = build_decl (BUILTINS_LOCATION, RESULT_DECL,
				   NULL_TREE, void_type_node);
  TREE_PUBLIC (decl) = 1;
  TREE_STATIC (decl) = 1;
  DECL_IGNORED_P (decl) = 1;

  if (USE_HIDDEN_LINKONCE)
    {
      cgraph_node::create (decl)->set_comdat_group (DECL_ASSEMBLER_NAME (decl));

      targetm.asm_out.unique_section (decl, 0);
      switch_to_section (get_named_section (decl, NULL, 0));

      targetm.asm_out.globalize_label (asm_out_file, thunk_label);
      fputs ("\t.hidden\t", asm_out_file);
      assemble_name (asm_out_file, thunk_label);
      putc ('\n', asm_out_file);
      ASM_DECLARE_FUNCTION_NAME (asm_out_file, thunk_label, decl);
    }
  else
    {
      switch_to_section (text_section);
      ASM_OUTPUT_LABEL (asm_out_file, thunk_label);
    }

  DECL_INITIAL (decl) = make_node (BLOCK);
  current_function_decl = decl;
  allocate_struct_function (decl, false);
  init_function_start (decl);
  cfun->is_thunk = true;
  first_function_block_is_cold = false;
  final_start_function (emit_barrier (), asm_out_file, 1);

  /* This makes CFI at least usable for indirect jumps.

     Stopping in the thunk: backtrace will point to the thunk target
     is if it was interrupted by a signal.  For a call this means that
     the call chain will be: caller->callee->thunk   */
  if (flag_asynchronous_unwind_tables && flag_dwarf2_cfi_asm)
    {
      fputs ("\t.cfi_signal_frame\n", asm_out_file);
      fprintf (asm_out_file, "\t.cfi_return_column %d\n", regno);
      for (i = 0; i < FPR15_REGNUM; i++)
	fprintf (asm_out_file, "\t.cfi_same_value %s\n", reg_names[i]);
    }

  if (z10_p)
    {
      /* exrl  0,1f  */

      /* We generate a thunk for z10 compiled code although z10 is
	 currently not enabled.  Tell the assembler to accept the
	 instruction.  */
      if (!TARGET_CPU_Z10)
	{
	  fputs ("\t.machine push\n", asm_out_file);
	  fputs ("\t.machine z10\n", asm_out_file);
	}
      /* We use exrl even if -mzarch hasn't been specified on the
	 command line so we have to tell the assembler to accept
	 it.  */
      if (!TARGET_ZARCH)
	fputs ("\t.machinemode zarch\n", asm_out_file);

      fputs ("\texrl\t0,1f\n", asm_out_file);

      if (!TARGET_ZARCH)
	fputs ("\t.machinemode esa\n", asm_out_file);

      if (!TARGET_CPU_Z10)
	fputs ("\t.machine pop\n", asm_out_file);
    }
  else
    {
      /* larl %r1,1f  */
      fprintf (asm_out_file, "\tlarl\t%%r%d,1f\n",
	       INDIRECT_BRANCH_THUNK_REGNUM);

      /* ex 0,0(%r1)  */
      fprintf (asm_out_file, "\tex\t0,0(%%r%d)\n",
	       INDIRECT_BRANCH_THUNK_REGNUM);
    }

  /* 0:    j 0b  */
  fputs ("0:\tj\t0b\n", asm_out_file);

  /* 1:    br <regno>  */
  fprintf (asm_out_file, "1:\tbr\t%%r%d\n", regno);

  final_end_function ();
  init_insn_lengths ();
  free_after_compilation (cfun);
  set_cfun (NULL);
  current_function_decl = NULL;
}

/* Implement the asm.code_end target hook.  */

static void
s390_code_end (void)
{
  int i;

  for (i = 1; i < 16; i++)
    {
      if (indirect_branch_z10thunk_mask & (1 << i))
	s390_output_indirect_thunk_function (i, true);

      if (indirect_branch_prez10thunk_mask & (1 << i))
	s390_output_indirect_thunk_function (i, false);
    }

  if (TARGET_INDIRECT_BRANCH_TABLE)
    {
      int o;
      int i;

      for (o = 0; o < INDIRECT_BRANCH_NUM_OPTIONS; o++)
	{
	  if (indirect_branch_table_label_no[o] == 0)
	    continue;

	  switch_to_section (get_section (indirect_branch_table_name[o],
					  0,
					  NULL_TREE));
	  for (i = 0; i < indirect_branch_table_label_no[o]; i++)
	    {
	      char label_start[32];

	      ASM_GENERATE_INTERNAL_LABEL (label_start,
					   indirect_branch_table_label[o], i);

	      fputs ("\t.long\t", asm_out_file);
	      assemble_name_raw (asm_out_file, label_start);
	      fputs ("-.\n", asm_out_file);
	    }
	}
    }
}

/* Implement the TARGET_CASE_VALUES_THRESHOLD target hook.  */

unsigned int
s390_case_values_threshold (void)
{
  /* Disabling branch prediction for indirect jumps makes jump tables
     much more expensive.  */
  if (TARGET_INDIRECT_BRANCH_NOBP_JUMP)
    return 20;

  return default_case_values_threshold ();
}

/* Evaluate the insns between HEAD and TAIL and do back-end to install
   back-end specific dependencies.

   Establish an ANTI dependency between r11 and r15 restores from FPRs
   to prevent the instructions scheduler from reordering them since
   this would break CFI.  No further handling in the sched_reorder
   hook is required since the r11 and r15 restore will never appear in
   the same ready list with that change.  */
void
s390_sched_dependencies_evaluation (rtx_insn *head, rtx_insn *tail)
{
  if (!frame_pointer_needed || !epilogue_completed)
    return;

  while (head != tail && DEBUG_INSN_P (head))
    head = NEXT_INSN (head);

  rtx_insn *r15_restore = NULL, *r11_restore = NULL;

  for (rtx_insn *insn = tail; insn != head; insn = PREV_INSN (insn))
    {
      rtx set = single_set (insn);
      if (!INSN_P (insn)
	  || !RTX_FRAME_RELATED_P (insn)
	  || set == NULL_RTX
	  || !REG_P (SET_DEST (set))
	  || !FP_REG_P (SET_SRC (set)))
	continue;

      if (REGNO (SET_DEST (set)) == HARD_FRAME_POINTER_REGNUM)
	r11_restore = insn;

      if (REGNO (SET_DEST (set)) == STACK_POINTER_REGNUM)
	r15_restore = insn;
    }

  if (r11_restore == NULL || r15_restore == NULL)
    return;
  add_dependence (r11_restore, r15_restore, REG_DEP_ANTI);
}

/* Implement TARGET_SHIFT_TRUNCATION_MASK for integer shifts.  */

static unsigned HOST_WIDE_INT
s390_shift_truncation_mask (machine_mode mode)
{
  return mode == DImode || mode == SImode ? 63 : 0;
}

/* Return TRUE iff CONSTRAINT is an "f" constraint, possibly with additional
   modifiers.  */

static bool
f_constraint_p (const char *constraint)
{
  bool seen_f_p = false;
  bool seen_v_p = false;

  for (size_t i = 0, c_len = strlen (constraint); i < c_len;
       i += CONSTRAINT_LEN (constraint[i], constraint + i))
    {
      if (constraint[i] == 'f')
	seen_f_p = true;
      if (constraint[i] == 'v')
	seen_v_p = true;
    }

  /* Treat "fv" constraints as "v", because LRA will choose the widest register
   * class.  */
  return seen_f_p && !seen_v_p;
}

/* Return TRUE iff X is a hard floating-point (and not a vector) register.  */

static bool
s390_hard_fp_reg_p (rtx x)
{
  if (!(REG_P (x) && HARD_REGISTER_P (x) && REG_ATTRS (x)))
    return false;

  tree decl = REG_EXPR (x);
  if (!(HAS_DECL_ASSEMBLER_NAME_P (decl) && DECL_ASSEMBLER_NAME_SET_P (decl)))
    return false;

  const char *name = IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (decl));

  return name[0] == '*' && name[1] == 'f';
}

/* Implement TARGET_MD_ASM_ADJUST hook in order to fix up "f"
   constraints when long doubles are stored in vector registers.  */

static rtx_insn *
s390_md_asm_adjust (vec<rtx> &outputs, vec<rtx> &inputs,
		    vec<machine_mode> &input_modes,
		    vec<const char *> &constraints, vec<rtx> & /*clobbers*/,
		    HARD_REG_SET & /*clobbered_regs*/, location_t /*loc*/)
{
  if (!TARGET_VXE)
    /* Long doubles are stored in FPR pairs - nothing to do.  */
    return NULL;

  rtx_insn *after_md_seq = NULL, *after_md_end = NULL;

  unsigned ninputs = inputs.length ();
  unsigned noutputs = outputs.length ();
  for (unsigned i = 0; i < noutputs; i++)
    {
      if (GET_MODE (outputs[i]) != TFmode)
	/* Not a long double - nothing to do.  */
	continue;
      const char *constraint = constraints[i];
      bool allows_mem, allows_reg, is_inout;
      bool ok = parse_output_constraint (&constraint, i, ninputs, noutputs,
					 &allows_mem, &allows_reg, &is_inout);
      gcc_assert (ok);
      if (!f_constraint_p (constraint))
	/* Long double with a constraint other than "=f" - nothing to do.  */
	continue;
      gcc_assert (allows_reg);
      gcc_assert (!is_inout);
      /* Copy output value from a FPR pair into a vector register.  */
      rtx fprx2;
      push_to_sequence2 (after_md_seq, after_md_end);
      if (s390_hard_fp_reg_p (outputs[i]))
	{
	  fprx2 = gen_rtx_REG (FPRX2mode, REGNO (outputs[i]));
	  /* The first half is already at the correct location, copy only the
	   * second one.  Use the UNSPEC pattern instead of the SUBREG one,
	   * since s390_can_change_mode_class() rejects
	   * (subreg:DF (reg:TF %fN) 8) and thus subreg validation fails.  */
	  rtx v1 = gen_rtx_REG (V2DFmode, REGNO (outputs[i]));
	  rtx v3 = gen_rtx_REG (V2DFmode, REGNO (outputs[i]) + 1);
	  emit_insn (gen_vec_permiv2df (v1, v1, v3, const0_rtx));
	}
      else
	{
	  fprx2 = gen_reg_rtx (FPRX2mode);
	  emit_insn (gen_fprx2_to_tf (outputs[i], fprx2));
	}
      after_md_seq = get_insns ();
      after_md_end = get_last_insn ();
      end_sequence ();
      outputs[i] = fprx2;
    }

  for (unsigned i = 0; i < ninputs; i++)
    {
      if (GET_MODE (inputs[i]) != TFmode)
	/* Not a long double - nothing to do.  */
	continue;
      const char *constraint = constraints[noutputs + i];
      bool allows_mem, allows_reg;
      bool ok = parse_input_constraint (&constraint, i, ninputs, noutputs, 0,
					constraints.address (), &allows_mem,
					&allows_reg);
      gcc_assert (ok);
      if (!f_constraint_p (constraint))
	/* Long double with a constraint other than "f" (or "=f" for inout
	   operands) - nothing to do.  */
	continue;
      gcc_assert (allows_reg);
      /* Copy input value from a vector register into a FPR pair.  */
      rtx fprx2;
      if (s390_hard_fp_reg_p (inputs[i]))
	{
	  fprx2 = gen_rtx_REG (FPRX2mode, REGNO (inputs[i]));
	  /* Copy only the second half.  */
	  rtx v1 = gen_rtx_REG (V2DFmode, REGNO (inputs[i]) + 1);
	  rtx v2 = gen_rtx_REG (V2DFmode, REGNO (inputs[i]));
	  emit_insn (gen_vec_permiv2df (v1, v2, v1, GEN_INT (3)));
	}
      else
	{
	  fprx2 = gen_reg_rtx (FPRX2mode);
	  emit_insn (gen_tf_to_fprx2 (fprx2, inputs[i]));
	}
      inputs[i] = fprx2;
      input_modes[i] = FPRX2mode;
    }

  return after_md_seq;
}

#define MAX_VECT_LEN	16

struct expand_vec_perm_d
{
  rtx target, op0, op1;
  unsigned char perm[MAX_VECT_LEN];
  machine_mode vmode;
  unsigned char nelt;
  bool testing_p;
  bool only_op0;
  bool only_op1;
};

/* Try to expand the vector permute operation described by D using the
   vector merge instructions vml and vmh.  Return true if vector merge
   could be used.  */
static bool
expand_perm_with_merge (const struct expand_vec_perm_d &d)
{
  bool merge_lo_p = true;
  bool merge_hi_p = true;

  if (d.nelt % 2)
    return false;

  // For V4SI this checks for: { 0, 4, 1, 5 }
  for (int telt = 0; telt < d.nelt; telt++)
    if (d.perm[telt] != telt / 2 + (telt % 2) * d.nelt)
      {
	merge_hi_p = false;
	break;
      }

  if (!merge_hi_p)
    {
      // For V4SI this checks for: { 2, 6, 3, 7 }
      for (int telt = 0; telt < d.nelt; telt++)
	if (d.perm[telt] != (telt + d.nelt) / 2 + (telt % 2) * d.nelt)
	  {
	    merge_lo_p = false;
	    break;
	  }
    }
  else
    merge_lo_p = false;

  if (d.testing_p)
    return merge_lo_p || merge_hi_p;

  if (merge_lo_p || merge_hi_p)
    s390_expand_merge (d.target, d.op0, d.op1, merge_hi_p);

  return merge_lo_p || merge_hi_p;
}

/* Try to expand the vector permute operation described by D using the
   vector permute doubleword immediate instruction vpdi.  Return true
   if vpdi could be used.

   VPDI allows 4 different immediate values (0, 1, 4, 5). The 0 and 5
   cases are covered by vmrhg and vmrlg already.  So we only care
   about the 1, 4 cases here.
   1 - First element of src1 and second of src2
   4 - Second element of src1 and first of src2  */
static bool
expand_perm_with_vpdi (const struct expand_vec_perm_d &d)
{
  bool vpdi1_p = false;
  bool vpdi4_p = false;
  rtx op0_reg, op1_reg;

  // Only V2DI and V2DF are supported here.
  if (d.nelt != 2)
    return false;

  if (d.perm[0] == 0 && d.perm[1] == 3)
    vpdi1_p = true;

  if ((d.perm[0] == 1 && d.perm[1] == 2)
      || (d.perm[0] == 1 && d.perm[1] == 0)
      || (d.perm[0] == 3 && d.perm[1] == 2))
    vpdi4_p = true;

  if (!vpdi1_p && !vpdi4_p)
    return false;

  if (d.testing_p)
    return true;

  op0_reg = force_reg (GET_MODE (d.op0), d.op0);
  op1_reg = force_reg (GET_MODE (d.op1), d.op1);

  /* If we only reference either of the operands in
     the permute mask, just use one of them.  */
  if (d.only_op0)
    op1_reg = op0_reg;
  else if (d.only_op1)
    op0_reg = op1_reg;

  if (vpdi1_p)
    emit_insn (gen_vpdi1 (d.vmode, d.target, op0_reg, op1_reg));
  if (vpdi4_p)
    emit_insn (gen_vpdi4 (d.vmode, d.target, op0_reg, op1_reg));

  return true;
}

/* Helper that checks if a vector permutation mask D
   represents a reversal of the vector's elements.  */
static inline bool
is_reverse_perm_mask (const struct expand_vec_perm_d &d)
{
  for (int i = 0; i < d.nelt; i++)
    if (d.perm[i] != d.nelt - i - 1)
      return false;
  return true;
}

/* The case of reversing a four-element vector [0, 1, 2, 3]
   can be handled by first permuting the doublewords
   [2, 3, 0, 1] and subsequently rotating them by 32 bits.  */
static bool
expand_perm_with_rot (const struct expand_vec_perm_d &d)
{
  if (d.nelt != 4)
    return false;

  if (d.op0 == d.op1 && is_reverse_perm_mask (d))
    {
      if (d.testing_p)
	return true;

      rtx tmp = gen_reg_rtx (d.vmode);
      rtx op0_reg = force_reg (GET_MODE (d.op0), d.op0);

      emit_insn (gen_vpdi4_2 (d.vmode, tmp, op0_reg, op0_reg));
      if (d.vmode == V4SImode)
	emit_insn (gen_rotlv4si3_di (d.target, tmp));
      else if (d.vmode == V4SFmode)
	emit_insn (gen_rotlv4sf3_di (d.target, tmp));

      return true;
    }

  return false;
}

/* If we just reverse the elements, emit an eltswap if we have
   vler/vster.  */
static bool
expand_perm_with_vster (const struct expand_vec_perm_d &d)
{
  if (TARGET_VXE2 && d.op0 == d.op1 && is_reverse_perm_mask (d)
      && (d.vmode == V2DImode || d.vmode == V2DFmode
	  || d.vmode == V4SImode || d.vmode == V4SFmode
	  || d.vmode == V8HImode))
    {
      if (d.testing_p)
	return true;

      if (d.vmode == V2DImode)
	emit_insn (gen_eltswapv2di (d.target, d.op0));
      else if (d.vmode == V2DFmode)
	emit_insn (gen_eltswapv2df (d.target, d.op0));
      else if (d.vmode == V4SImode)
	emit_insn (gen_eltswapv4si (d.target, d.op0));
      else if (d.vmode == V4SFmode)
	emit_insn (gen_eltswapv4sf (d.target, d.op0));
      else if (d.vmode == V8HImode)
	emit_insn (gen_eltswapv8hi (d.target, d.op0));
      return true;
    }
  return false;
}

/* If we reverse a byte-vector this is the same as
   byte reversing it which can be done with vstbrq.  */
static bool
expand_perm_with_vstbrq (const struct expand_vec_perm_d &d)
{
  if (TARGET_VXE2 && d.op0 == d.op1 && is_reverse_perm_mask (d)
      && d.vmode == V16QImode)
    {
      if (d.testing_p)
	return true;

      emit_insn (gen_eltswapv16qi (d.target, d.op0));
      return true;
    }
  return false;
}


/* Try to find the best sequence for the vector permute operation
   described by D.  Return true if the operation could be
   expanded.  */
static bool
vectorize_vec_perm_const_1 (const struct expand_vec_perm_d &d)
{
  if (expand_perm_with_merge (d))
    return true;

  if (expand_perm_with_vster (d))
    return true;

  if (expand_perm_with_vstbrq (d))
    return true;

  if (expand_perm_with_vpdi (d))
    return true;

  if (expand_perm_with_rot (d))
    return true;

  return false;
}

/* Return true if we can emit instructions for the constant
   permutation vector in SEL.  If OUTPUT, IN0, IN1 are non-null the
   hook is supposed to emit the required INSNs.  */

bool
s390_vectorize_vec_perm_const (machine_mode vmode, machine_mode op_mode,
			       rtx target, rtx op0, rtx op1,
			       const vec_perm_indices &sel)
{
  if (vmode != op_mode)
    return false;

  struct expand_vec_perm_d d;
  unsigned int i, nelt;

  if (!s390_vector_mode_supported_p (vmode) || GET_MODE_SIZE (vmode) != 16)
    return false;

  d.target = target;
  d.op0 = op0;
  d.op1 = op1;

  d.vmode = vmode;
  gcc_assert (VECTOR_MODE_P (d.vmode));
  d.nelt = nelt = GET_MODE_NUNITS (d.vmode);
  d.testing_p = target == NULL_RTX;
  d.only_op0 = false;
  d.only_op1 = false;

  gcc_assert (target == NULL_RTX || REG_P (target));
  gcc_assert (sel.length () == nelt);

  unsigned int highest = 0, lowest = 2 * nelt - 1;
  for (i = 0; i < nelt; i++)
    {
      unsigned char e = sel[i];
      lowest = MIN (lowest, e);
      highest = MAX (highest, e);
      gcc_assert (e < 2 * nelt);
      d.perm[i] = e;
    }

  if (lowest < nelt && highest < nelt)
    d.only_op0 = true;
  else if (lowest >= nelt && highest >= nelt)
    d.only_op1 = true;

  return vectorize_vec_perm_const_1 (d);
}

/* Initialize GCC target structure.  */

#undef  TARGET_ASM_ALIGNED_HI_OP
#define TARGET_ASM_ALIGNED_HI_OP "\t.word\t"
#undef  TARGET_ASM_ALIGNED_DI_OP
#define TARGET_ASM_ALIGNED_DI_OP "\t.quad\t"
#undef  TARGET_ASM_INTEGER
#define TARGET_ASM_INTEGER s390_assemble_integer

#undef  TARGET_ASM_OPEN_PAREN
#define TARGET_ASM_OPEN_PAREN ""

#undef  TARGET_ASM_CLOSE_PAREN
#define TARGET_ASM_CLOSE_PAREN ""

#undef TARGET_OPTION_OVERRIDE
#define TARGET_OPTION_OVERRIDE s390_option_override

#ifdef TARGET_THREAD_SSP_OFFSET
#undef TARGET_STACK_PROTECT_GUARD
#define TARGET_STACK_PROTECT_GUARD hook_tree_void_null
#endif

#undef	TARGET_ENCODE_SECTION_INFO
#define TARGET_ENCODE_SECTION_INFO s390_encode_section_info

#undef TARGET_SCALAR_MODE_SUPPORTED_P
#define TARGET_SCALAR_MODE_SUPPORTED_P s390_scalar_mode_supported_p

#ifdef HAVE_AS_TLS
#undef TARGET_HAVE_TLS
#define TARGET_HAVE_TLS true
#endif
#undef TARGET_CANNOT_FORCE_CONST_MEM
#define TARGET_CANNOT_FORCE_CONST_MEM s390_cannot_force_const_mem

#undef TARGET_DELEGITIMIZE_ADDRESS
#define TARGET_DELEGITIMIZE_ADDRESS s390_delegitimize_address

#undef TARGET_LEGITIMIZE_ADDRESS
#define TARGET_LEGITIMIZE_ADDRESS s390_legitimize_address

#undef TARGET_RETURN_IN_MEMORY
#define TARGET_RETURN_IN_MEMORY s390_return_in_memory

#undef  TARGET_INIT_BUILTINS
#define TARGET_INIT_BUILTINS s390_init_builtins
#undef  TARGET_EXPAND_BUILTIN
#define TARGET_EXPAND_BUILTIN s390_expand_builtin
#undef  TARGET_BUILTIN_DECL
#define TARGET_BUILTIN_DECL s390_builtin_decl

#undef TARGET_ASM_OUTPUT_ADDR_CONST_EXTRA
#define TARGET_ASM_OUTPUT_ADDR_CONST_EXTRA s390_output_addr_const_extra

#undef TARGET_ASM_OUTPUT_MI_THUNK
#define TARGET_ASM_OUTPUT_MI_THUNK s390_output_mi_thunk
#undef TARGET_ASM_CAN_OUTPUT_MI_THUNK
#define TARGET_ASM_CAN_OUTPUT_MI_THUNK hook_bool_const_tree_hwi_hwi_const_tree_true

#if ENABLE_S390_EXCESS_FLOAT_PRECISION == 1
/* This hook is only needed to maintain the historic behavior with glibc
   versions that typedef float_t to double. */
#undef TARGET_C_EXCESS_PRECISION
#define TARGET_C_EXCESS_PRECISION s390_excess_precision
#endif

#undef  TARGET_SCHED_ADJUST_PRIORITY
#define TARGET_SCHED_ADJUST_PRIORITY s390_adjust_priority
#undef TARGET_SCHED_ISSUE_RATE
#define TARGET_SCHED_ISSUE_RATE s390_issue_rate
#undef TARGET_SCHED_FIRST_CYCLE_MULTIPASS_DFA_LOOKAHEAD
#define TARGET_SCHED_FIRST_CYCLE_MULTIPASS_DFA_LOOKAHEAD s390_first_cycle_multipass_dfa_lookahead

#undef TARGET_SCHED_VARIABLE_ISSUE
#define TARGET_SCHED_VARIABLE_ISSUE s390_sched_variable_issue
#undef TARGET_SCHED_REORDER
#define TARGET_SCHED_REORDER s390_sched_reorder
#undef TARGET_SCHED_INIT
#define TARGET_SCHED_INIT s390_sched_init

#undef TARGET_CANNOT_COPY_INSN_P
#define TARGET_CANNOT_COPY_INSN_P s390_cannot_copy_insn_p
#undef TARGET_RTX_COSTS
#define TARGET_RTX_COSTS s390_rtx_costs
#undef TARGET_ADDRESS_COST
#define TARGET_ADDRESS_COST s390_address_cost
#undef TARGET_REGISTER_MOVE_COST
#define TARGET_REGISTER_MOVE_COST s390_register_move_cost
#undef TARGET_MEMORY_MOVE_COST
#define TARGET_MEMORY_MOVE_COST s390_memory_move_cost
#undef TARGET_VECTORIZE_BUILTIN_VECTORIZATION_COST
#define TARGET_VECTORIZE_BUILTIN_VECTORIZATION_COST \
  s390_builtin_vectorization_cost

#undef TARGET_MACHINE_DEPENDENT_REORG
#define TARGET_MACHINE_DEPENDENT_REORG s390_reorg

#undef TARGET_VALID_POINTER_MODE
#define TARGET_VALID_POINTER_MODE s390_valid_pointer_mode

#undef TARGET_BUILD_BUILTIN_VA_LIST
#define TARGET_BUILD_BUILTIN_VA_LIST s390_build_builtin_va_list
#undef TARGET_EXPAND_BUILTIN_VA_START
#define TARGET_EXPAND_BUILTIN_VA_START s390_va_start
#undef TARGET_ASAN_SHADOW_OFFSET
#define TARGET_ASAN_SHADOW_OFFSET s390_asan_shadow_offset
#undef TARGET_GIMPLIFY_VA_ARG_EXPR
#define TARGET_GIMPLIFY_VA_ARG_EXPR s390_gimplify_va_arg

#undef TARGET_PROMOTE_FUNCTION_MODE
#define TARGET_PROMOTE_FUNCTION_MODE s390_promote_function_mode
#undef TARGET_PASS_BY_REFERENCE
#define TARGET_PASS_BY_REFERENCE s390_pass_by_reference

#undef  TARGET_OVERRIDE_OPTIONS_AFTER_CHANGE
#define TARGET_OVERRIDE_OPTIONS_AFTER_CHANGE s390_override_options_after_change

#undef TARGET_FUNCTION_OK_FOR_SIBCALL
#define TARGET_FUNCTION_OK_FOR_SIBCALL s390_function_ok_for_sibcall
#undef TARGET_FUNCTION_ARG
#define TARGET_FUNCTION_ARG s390_function_arg
#undef TARGET_FUNCTION_ARG_ADVANCE
#define TARGET_FUNCTION_ARG_ADVANCE s390_function_arg_advance
#undef TARGET_FUNCTION_ARG_PADDING
#define TARGET_FUNCTION_ARG_PADDING s390_function_arg_padding
#undef TARGET_FUNCTION_VALUE
#define TARGET_FUNCTION_VALUE s390_function_value
#undef TARGET_LIBCALL_VALUE
#define TARGET_LIBCALL_VALUE s390_libcall_value
#undef TARGET_STRICT_ARGUMENT_NAMING
#define TARGET_STRICT_ARGUMENT_NAMING hook_bool_CUMULATIVE_ARGS_true

#undef TARGET_KEEP_LEAF_WHEN_PROFILED
#define TARGET_KEEP_LEAF_WHEN_PROFILED s390_keep_leaf_when_profiled

#undef TARGET_FIXED_CONDITION_CODE_REGS
#define TARGET_FIXED_CONDITION_CODE_REGS s390_fixed_condition_code_regs

#undef TARGET_CC_MODES_COMPATIBLE
#define TARGET_CC_MODES_COMPATIBLE s390_cc_modes_compatible

#undef TARGET_INVALID_WITHIN_DOLOOP
#define TARGET_INVALID_WITHIN_DOLOOP hook_constcharptr_const_rtx_insn_null

#ifdef HAVE_AS_TLS
#undef TARGET_ASM_OUTPUT_DWARF_DTPREL
#define TARGET_ASM_OUTPUT_DWARF_DTPREL s390_output_dwarf_dtprel
#endif

#undef TARGET_DWARF_FRAME_REG_MODE
#define TARGET_DWARF_FRAME_REG_MODE s390_dwarf_frame_reg_mode

#ifdef TARGET_ALTERNATE_LONG_DOUBLE_MANGLING
#undef TARGET_MANGLE_TYPE
#define TARGET_MANGLE_TYPE s390_mangle_type
#endif

#undef TARGET_SCALAR_MODE_SUPPORTED_P
#define TARGET_SCALAR_MODE_SUPPORTED_P s390_scalar_mode_supported_p

#undef TARGET_VECTOR_MODE_SUPPORTED_P
#define TARGET_VECTOR_MODE_SUPPORTED_P s390_vector_mode_supported_p

#undef  TARGET_PREFERRED_RELOAD_CLASS
#define TARGET_PREFERRED_RELOAD_CLASS s390_preferred_reload_class

#undef TARGET_SECONDARY_RELOAD
#define TARGET_SECONDARY_RELOAD s390_secondary_reload
#undef TARGET_SECONDARY_MEMORY_NEEDED
#define TARGET_SECONDARY_MEMORY_NEEDED s390_secondary_memory_needed
#undef TARGET_SECONDARY_MEMORY_NEEDED_MODE
#define TARGET_SECONDARY_MEMORY_NEEDED_MODE s390_secondary_memory_needed_mode

#undef TARGET_LIBGCC_CMP_RETURN_MODE
#define TARGET_LIBGCC_CMP_RETURN_MODE s390_libgcc_cmp_return_mode

#undef TARGET_LIBGCC_SHIFT_COUNT_MODE
#define TARGET_LIBGCC_SHIFT_COUNT_MODE s390_libgcc_shift_count_mode

#undef TARGET_LEGITIMATE_ADDRESS_P
#define TARGET_LEGITIMATE_ADDRESS_P s390_legitimate_address_p

#undef TARGET_LEGITIMATE_CONSTANT_P
#define TARGET_LEGITIMATE_CONSTANT_P s390_legitimate_constant_p

#undef TARGET_LRA_P
#define TARGET_LRA_P s390_lra_p

#undef TARGET_CAN_ELIMINATE
#define TARGET_CAN_ELIMINATE s390_can_eliminate

#undef TARGET_CONDITIONAL_REGISTER_USAGE
#define TARGET_CONDITIONAL_REGISTER_USAGE s390_conditional_register_usage

#undef TARGET_LOOP_UNROLL_ADJUST
#define TARGET_LOOP_UNROLL_ADJUST s390_loop_unroll_adjust

#undef TARGET_ASM_TRAMPOLINE_TEMPLATE
#define TARGET_ASM_TRAMPOLINE_TEMPLATE s390_asm_trampoline_template
#undef TARGET_TRAMPOLINE_INIT
#define TARGET_TRAMPOLINE_INIT s390_trampoline_init

/* PR 79421 */
#undef TARGET_CUSTOM_FUNCTION_DESCRIPTORS
#define TARGET_CUSTOM_FUNCTION_DESCRIPTORS 1

#undef TARGET_UNWIND_WORD_MODE
#define TARGET_UNWIND_WORD_MODE s390_unwind_word_mode

#undef TARGET_CANONICALIZE_COMPARISON
#define TARGET_CANONICALIZE_COMPARISON s390_canonicalize_comparison

#undef TARGET_HARD_REGNO_SCRATCH_OK
#define TARGET_HARD_REGNO_SCRATCH_OK s390_hard_regno_scratch_ok

#undef TARGET_HARD_REGNO_NREGS
#define TARGET_HARD_REGNO_NREGS s390_hard_regno_nregs
#undef TARGET_HARD_REGNO_MODE_OK
#define TARGET_HARD_REGNO_MODE_OK s390_hard_regno_mode_ok
#undef TARGET_MODES_TIEABLE_P
#define TARGET_MODES_TIEABLE_P s390_modes_tieable_p

#undef TARGET_HARD_REGNO_CALL_PART_CLOBBERED
#define TARGET_HARD_REGNO_CALL_PART_CLOBBERED \
  s390_hard_regno_call_part_clobbered

#undef TARGET_ATTRIBUTE_TABLE
#define TARGET_ATTRIBUTE_TABLE s390_attribute_table

#undef TARGET_FUNCTION_ATTRIBUTE_INLINABLE_P
#define TARGET_FUNCTION_ATTRIBUTE_INLINABLE_P hook_bool_const_tree_true

#undef TARGET_SET_UP_BY_PROLOGUE
#define TARGET_SET_UP_BY_PROLOGUE s300_set_up_by_prologue

#undef TARGET_EXTRA_LIVE_ON_ENTRY
#define TARGET_EXTRA_LIVE_ON_ENTRY s390_live_on_entry

#undef TARGET_USE_BY_PIECES_INFRASTRUCTURE_P
#define TARGET_USE_BY_PIECES_INFRASTRUCTURE_P \
  s390_use_by_pieces_infrastructure_p

#undef TARGET_ATOMIC_ASSIGN_EXPAND_FENV
#define TARGET_ATOMIC_ASSIGN_EXPAND_FENV s390_atomic_assign_expand_fenv

#undef TARGET_INVALID_ARG_FOR_UNPROTOTYPED_FN
#define TARGET_INVALID_ARG_FOR_UNPROTOTYPED_FN s390_invalid_arg_for_unprototyped_fn

#undef TARGET_VECTORIZE_PREFERRED_SIMD_MODE
#define TARGET_VECTORIZE_PREFERRED_SIMD_MODE s390_preferred_simd_mode

#undef TARGET_VECTORIZE_SUPPORT_VECTOR_MISALIGNMENT
#define TARGET_VECTORIZE_SUPPORT_VECTOR_MISALIGNMENT s390_support_vector_misalignment

#undef TARGET_VECTOR_ALIGNMENT
#define TARGET_VECTOR_ALIGNMENT s390_vector_alignment

#undef TARGET_INVALID_BINARY_OP
#define TARGET_INVALID_BINARY_OP s390_invalid_binary_op

#ifdef HAVE_AS_MACHINE_MACHINEMODE
#undef TARGET_ASM_FILE_START
#define TARGET_ASM_FILE_START s390_asm_file_start
#endif

#undef TARGET_ASM_FILE_END
#define TARGET_ASM_FILE_END s390_asm_file_end

#undef TARGET_SET_CURRENT_FUNCTION
#define TARGET_SET_CURRENT_FUNCTION s390_set_current_function

#if S390_USE_TARGET_ATTRIBUTE
#undef TARGET_OPTION_VALID_ATTRIBUTE_P
#define TARGET_OPTION_VALID_ATTRIBUTE_P s390_valid_target_attribute_p

#undef TARGET_CAN_INLINE_P
#define TARGET_CAN_INLINE_P s390_can_inline_p
#endif

#undef TARGET_OPTION_RESTORE
#define TARGET_OPTION_RESTORE s390_function_specific_restore

#undef TARGET_CAN_CHANGE_MODE_CLASS
#define TARGET_CAN_CHANGE_MODE_CLASS s390_can_change_mode_class

#undef TARGET_CONSTANT_ALIGNMENT
#define TARGET_CONSTANT_ALIGNMENT s390_constant_alignment

#undef TARGET_ASM_CODE_END
#define TARGET_ASM_CODE_END s390_code_end

#undef TARGET_CASE_VALUES_THRESHOLD
#define TARGET_CASE_VALUES_THRESHOLD s390_case_values_threshold

#undef TARGET_SCHED_DEPENDENCIES_EVALUATION_HOOK
#define TARGET_SCHED_DEPENDENCIES_EVALUATION_HOOK \
  s390_sched_dependencies_evaluation

#undef TARGET_SHIFT_TRUNCATION_MASK
#define TARGET_SHIFT_TRUNCATION_MASK s390_shift_truncation_mask

/* Use only short displacement, since long displacement is not available for
   the floating point instructions.  */
#undef TARGET_MAX_ANCHOR_OFFSET
#define TARGET_MAX_ANCHOR_OFFSET 0xfff

#undef TARGET_MD_ASM_ADJUST
#define TARGET_MD_ASM_ADJUST s390_md_asm_adjust

#undef TARGET_VECTORIZE_VEC_PERM_CONST
#define TARGET_VECTORIZE_VEC_PERM_CONST s390_vectorize_vec_perm_const


struct gcc_target targetm = TARGET_INITIALIZER;

#include "gt-s390.h"
