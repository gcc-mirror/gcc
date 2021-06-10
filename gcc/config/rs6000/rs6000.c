/* Subroutines used for code generation on IBM RS/6000.
   Copyright (C) 1991-2020 Free Software Foundation, Inc.
   Contributed by Richard Kenner (kenner@vlsi1.ultra.nyu.edu)

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
#include "rtl.h"
#include "tree.h"
#include "memmodel.h"
#include "gimple.h"
#include "cfghooks.h"
#include "cfgloop.h"
#include "df.h"
#include "tm_p.h"
#include "stringpool.h"
#include "expmed.h"
#include "optabs.h"
#include "regs.h"
#include "ira.h"
#include "recog.h"
#include "cgraph.h"
#include "diagnostic-core.h"
#include "insn-attr.h"
#include "flags.h"
#include "alias.h"
#include "fold-const.h"
#include "attribs.h"
#include "stor-layout.h"
#include "calls.h"
#include "print-tree.h"
#include "varasm.h"
#include "explow.h"
#include "expr.h"
#include "output.h"
#include "common/common-target.h"
#include "langhooks.h"
#include "reload.h"
#include "sched-int.h"
#include "gimplify.h"
#include "gimple-fold.h"
#include "gimple-iterator.h"
#include "gimple-ssa.h"
#include "gimple-walk.h"
#include "intl.h"
#include "tm-constrs.h"
#include "tree-vectorizer.h"
#include "target-globals.h"
#include "builtins.h"
#include "tree-vector-builder.h"
#include "context.h"
#include "tree-pass.h"
#include "except.h"
#if TARGET_XCOFF
#include "xcoffout.h"  /* get declarations of xcoff_*_section_name */
#endif
#include "case-cfn-macros.h"
#include "ppc-auxv.h"
#include "tree-ssa-propagate.h"
#include "tree-vrp.h"
#include "tree-ssanames.h"
#include "rs6000-internal.h"
#include "opts.h"

/* This file should be included last.  */
#include "target-def.h"

  /* Set -mabi=ieeelongdouble on some old targets.  In the future, power server
     systems will also set long double to be IEEE 128-bit.  AIX and Darwin
     explicitly redefine TARGET_IEEEQUAD and TARGET_IEEEQUAD_DEFAULT to 0, so
     those systems will not pick up this default.  This needs to be after all
     of the include files, so that POWERPC_LINUX and POWERPC_FREEBSD are
     properly defined.  */
#ifndef TARGET_IEEEQUAD_DEFAULT
#if !defined (POWERPC_LINUX) && !defined (POWERPC_FREEBSD)
#define TARGET_IEEEQUAD_DEFAULT 1
#else
#define TARGET_IEEEQUAD_DEFAULT 0
#endif
#endif

/* Don't enable PC-relative addressing if the target does not support it.  */
#ifndef PCREL_SUPPORTED_BY_OS
#define PCREL_SUPPORTED_BY_OS	0
#endif

/* Support targetm.vectorize.builtin_mask_for_load.  */
tree altivec_builtin_mask_for_load;

#ifdef USING_ELFOS_H
/* Counter for labels which are to be placed in .fixup.  */
int fixuplabelno = 0;
#endif

/* Whether to use variant of AIX ABI for PowerPC64 Linux.  */
int dot_symbols;

/* Specify the machine mode that pointers have.  After generation of rtl, the
   compiler makes no further distinction between pointers and any other objects
   of this machine mode.  */
scalar_int_mode rs6000_pmode;

#if TARGET_ELF
/* Note whether IEEE 128-bit floating point was passed or returned, either as
   the __float128/_Float128 explicit type, or when long double is IEEE 128-bit
   floating point.  We changed the default C++ mangling for these types and we
   may want to generate a weak alias of the old mangling (U10__float128) to the
   new mangling (u9__ieee128).  */
bool rs6000_passes_ieee128 = false;
#endif

/* Generate the manged name (i.e. U10__float128) used in GCC 8.1, and not the
   name used in current releases (i.e. u9__ieee128).  */
static bool ieee128_mangling_gcc_8_1;

/* Width in bits of a pointer.  */
unsigned rs6000_pointer_size;

#ifdef HAVE_AS_GNU_ATTRIBUTE
# ifndef HAVE_LD_PPC_GNU_ATTR_LONG_DOUBLE
# define HAVE_LD_PPC_GNU_ATTR_LONG_DOUBLE 0
# endif
/* Flag whether floating point values have been passed/returned.
   Note that this doesn't say whether fprs are used, since the
   Tag_GNU_Power_ABI_FP .gnu.attributes value this flag controls
   should be set for soft-float values passed in gprs and ieee128
   values passed in vsx registers.  */
bool rs6000_passes_float = false;
bool rs6000_passes_long_double = false;
/* Flag whether vector values have been passed/returned.  */
bool rs6000_passes_vector = false;
/* Flag whether small (<= 8 byte) structures have been returned.  */
bool rs6000_returns_struct = false;
#endif

/* Value is TRUE if register/mode pair is acceptable.  */
static bool rs6000_hard_regno_mode_ok_p
  [NUM_MACHINE_MODES][FIRST_PSEUDO_REGISTER];

/* Maximum number of registers needed for a given register class and mode.  */
unsigned char rs6000_class_max_nregs[NUM_MACHINE_MODES][LIM_REG_CLASSES];

/* How many registers are needed for a given register and mode.  */
unsigned char rs6000_hard_regno_nregs[NUM_MACHINE_MODES][FIRST_PSEUDO_REGISTER];

/* Map register number to register class.  */
enum reg_class rs6000_regno_regclass[FIRST_PSEUDO_REGISTER];

static int dbg_cost_ctrl;

/* Built in types.  */
tree rs6000_builtin_types[RS6000_BTI_MAX];
tree rs6000_builtin_decls[RS6000_BUILTIN_COUNT];

/* Flag to say the TOC is initialized */
int toc_initialized, need_toc_init;
char toc_label_name[10];

/* Cached value of rs6000_variable_issue. This is cached in
   rs6000_variable_issue hook and returned from rs6000_sched_reorder2.  */
static short cached_can_issue_more;

static GTY(()) section *read_only_data_section;
static GTY(()) section *private_data_section;
static GTY(()) section *tls_data_section;
static GTY(()) section *tls_private_data_section;
static GTY(()) section *read_only_private_data_section;
static GTY(()) section *sdata2_section;

section *toc_section = 0;

/* Describe the vector unit used for modes.  */
enum rs6000_vector rs6000_vector_unit[NUM_MACHINE_MODES];
enum rs6000_vector rs6000_vector_mem[NUM_MACHINE_MODES];

/* Register classes for various constraints that are based on the target
   switches.  */
enum reg_class rs6000_constraints[RS6000_CONSTRAINT_MAX];

/* Describe the alignment of a vector.  */
int rs6000_vector_align[NUM_MACHINE_MODES];

/* Map selected modes to types for builtins.  */
tree builtin_mode_to_type[MAX_MACHINE_MODE][2];

/* What modes to automatically generate reciprocal divide estimate (fre) and
   reciprocal sqrt (frsqrte) for.  */
unsigned char rs6000_recip_bits[MAX_MACHINE_MODE];

/* Masks to determine which reciprocal esitmate instructions to generate
   automatically.  */
enum rs6000_recip_mask {
  RECIP_SF_DIV		= 0x001,	/* Use divide estimate */
  RECIP_DF_DIV		= 0x002,
  RECIP_V4SF_DIV	= 0x004,
  RECIP_V2DF_DIV	= 0x008,

  RECIP_SF_RSQRT	= 0x010,	/* Use reciprocal sqrt estimate.  */
  RECIP_DF_RSQRT	= 0x020,
  RECIP_V4SF_RSQRT	= 0x040,
  RECIP_V2DF_RSQRT	= 0x080,

  /* Various combination of flags for -mrecip=xxx.  */
  RECIP_NONE		= 0,
  RECIP_ALL		= (RECIP_SF_DIV | RECIP_DF_DIV | RECIP_V4SF_DIV
			   | RECIP_V2DF_DIV | RECIP_SF_RSQRT | RECIP_DF_RSQRT
			   | RECIP_V4SF_RSQRT | RECIP_V2DF_RSQRT),

  RECIP_HIGH_PRECISION	= RECIP_ALL,

  /* On low precision machines like the power5, don't enable double precision
     reciprocal square root estimate, since it isn't accurate enough.  */
  RECIP_LOW_PRECISION	= (RECIP_ALL & ~(RECIP_DF_RSQRT | RECIP_V2DF_RSQRT))
};

/* -mrecip options.  */
static struct
{
  const char *string;		/* option name */
  unsigned int mask;		/* mask bits to set */
} recip_options[] = {
  { "all",	 RECIP_ALL },
  { "none",	 RECIP_NONE },
  { "div",	 (RECIP_SF_DIV | RECIP_DF_DIV | RECIP_V4SF_DIV
		  | RECIP_V2DF_DIV) },
  { "divf",	 (RECIP_SF_DIV | RECIP_V4SF_DIV) },
  { "divd",	 (RECIP_DF_DIV | RECIP_V2DF_DIV) },
  { "rsqrt",	 (RECIP_SF_RSQRT | RECIP_DF_RSQRT | RECIP_V4SF_RSQRT
		  | RECIP_V2DF_RSQRT) },
  { "rsqrtf",	 (RECIP_SF_RSQRT | RECIP_V4SF_RSQRT) },
  { "rsqrtd",	 (RECIP_DF_RSQRT | RECIP_V2DF_RSQRT) },
};

/* On PowerPC, we have a limited number of target clones that we care about
   which means we can use an array to hold the options, rather than having more
   elaborate data structures to identify each possible variation.  Order the
   clones from the default to the highest ISA.  */
enum {
  CLONE_DEFAULT		= 0,		/* default clone.  */
  CLONE_ISA_2_05,			/* ISA 2.05 (power6).  */
  CLONE_ISA_2_06,			/* ISA 2.06 (power7).  */
  CLONE_ISA_2_07,			/* ISA 2.07 (power8).  */
  CLONE_ISA_3_00,			/* ISA 3.0 (power9).  */
  CLONE_ISA_3_1,			/* ISA 3.1 (power10).  */
  CLONE_MAX
};

/* Map compiler ISA bits into HWCAP names.  */
struct clone_map {
  HOST_WIDE_INT isa_mask;	/* rs6000_isa mask */
  const char *name;		/* name to use in __builtin_cpu_supports.  */
};

static const struct clone_map rs6000_clone_map[CLONE_MAX] = {
  { 0,				"" },		/* Default options.  */
  { OPTION_MASK_CMPB,		"arch_2_05" },	/* ISA 2.05 (power6).  */
  { OPTION_MASK_POPCNTD,	"arch_2_06" },	/* ISA 2.06 (power7).  */
  { OPTION_MASK_P8_VECTOR,	"arch_2_07" },	/* ISA 2.07 (power8).  */
  { OPTION_MASK_P9_VECTOR,	"arch_3_00" },	/* ISA 3.0 (power9).  */
  { OPTION_MASK_POWER10,	"arch_3_1" },	/* ISA 3.1 (power10).  */
};


/* Newer LIBCs explicitly export this symbol to declare that they provide
   the AT_PLATFORM and AT_HWCAP/AT_HWCAP2 values in the TCB.  We emit a
   reference to this symbol whenever we expand a CPU builtin, so that
   we never link against an old LIBC.  */
const char *tcb_verification_symbol = "__parse_hwcap_and_convert_at_platform";

/* True if we have expanded a CPU builtin.  */
bool cpu_builtin_p = false;

/* Pointer to function (in rs6000-c.c) that can define or undefine target
   macros that have changed.  Languages that don't support the preprocessor
   don't link in rs6000-c.c, so we can't call it directly.  */
void (*rs6000_target_modify_macros_ptr) (bool, HOST_WIDE_INT, HOST_WIDE_INT);

/* Simplfy register classes into simpler classifications.  We assume
   GPR_REG_TYPE - FPR_REG_TYPE are ordered so that we can use a simple range
   check for standard register classes (gpr/floating/altivec/vsx) and
   floating/vector classes (float/altivec/vsx).  */

enum rs6000_reg_type {
  NO_REG_TYPE,
  PSEUDO_REG_TYPE,
  GPR_REG_TYPE,
  VSX_REG_TYPE,
  ALTIVEC_REG_TYPE,
  FPR_REG_TYPE,
  SPR_REG_TYPE,
  CR_REG_TYPE
};

/* Map register class to register type.  */
static enum rs6000_reg_type reg_class_to_reg_type[N_REG_CLASSES];

/* First/last register type for the 'normal' register types (i.e. general
   purpose, floating point, altivec, and VSX registers).  */
#define IS_STD_REG_TYPE(RTYPE) IN_RANGE(RTYPE, GPR_REG_TYPE, FPR_REG_TYPE)

#define IS_FP_VECT_REG_TYPE(RTYPE) IN_RANGE(RTYPE, VSX_REG_TYPE, FPR_REG_TYPE)


/* Register classes we care about in secondary reload or go if legitimate
   address.  We only need to worry about GPR, FPR, and Altivec registers here,
   along an ANY field that is the OR of the 3 register classes.  */

enum rs6000_reload_reg_type {
  RELOAD_REG_GPR,			/* General purpose registers.  */
  RELOAD_REG_FPR,			/* Traditional floating point regs.  */
  RELOAD_REG_VMX,			/* Altivec (VMX) registers.  */
  RELOAD_REG_ANY,			/* OR of GPR, FPR, Altivec masks.  */
  N_RELOAD_REG
};

/* For setting up register classes, loop through the 3 register classes mapping
   into real registers, and skip the ANY class, which is just an OR of the
   bits.  */
#define FIRST_RELOAD_REG_CLASS	RELOAD_REG_GPR
#define LAST_RELOAD_REG_CLASS	RELOAD_REG_VMX

/* Map reload register type to a register in the register class.  */
struct reload_reg_map_type {
  const char *name;			/* Register class name.  */
  int reg;				/* Register in the register class.  */
};

static const struct reload_reg_map_type reload_reg_map[N_RELOAD_REG] = {
  { "Gpr",	FIRST_GPR_REGNO },	/* RELOAD_REG_GPR.  */
  { "Fpr",	FIRST_FPR_REGNO },	/* RELOAD_REG_FPR.  */
  { "VMX",	FIRST_ALTIVEC_REGNO },	/* RELOAD_REG_VMX.  */
  { "Any",	-1 },			/* RELOAD_REG_ANY.  */
};

/* Mask bits for each register class, indexed per mode.  Historically the
   compiler has been more restrictive which types can do PRE_MODIFY instead of
   PRE_INC and PRE_DEC, so keep track of sepaate bits for these two.  */
typedef unsigned char addr_mask_type;

#define RELOAD_REG_VALID	0x01	/* Mode valid in register..  */
#define RELOAD_REG_MULTIPLE	0x02	/* Mode takes multiple registers.  */
#define RELOAD_REG_INDEXED	0x04	/* Reg+reg addressing.  */
#define RELOAD_REG_OFFSET	0x08	/* Reg+offset addressing. */
#define RELOAD_REG_PRE_INCDEC	0x10	/* PRE_INC/PRE_DEC valid.  */
#define RELOAD_REG_PRE_MODIFY	0x20	/* PRE_MODIFY valid.  */
#define RELOAD_REG_AND_M16	0x40	/* AND -16 addressing.  */
#define RELOAD_REG_QUAD_OFFSET	0x80	/* quad offset is limited.  */

/* Register type masks based on the type, of valid addressing modes.  */
struct rs6000_reg_addr {
  enum insn_code reload_load;		/* INSN to reload for loading. */
  enum insn_code reload_store;		/* INSN to reload for storing.  */
  enum insn_code reload_fpr_gpr;	/* INSN to move from FPR to GPR.  */
  enum insn_code reload_gpr_vsx;	/* INSN to move from GPR to VSX.  */
  enum insn_code reload_vsx_gpr;	/* INSN to move from VSX to GPR.  */
  addr_mask_type addr_mask[(int)N_RELOAD_REG]; /* Valid address masks.  */
  bool scalar_in_vmx_p;			/* Scalar value can go in VMX.  */
};

static struct rs6000_reg_addr reg_addr[NUM_MACHINE_MODES];

/* Helper function to say whether a mode supports PRE_INC or PRE_DEC.  */
static inline bool
mode_supports_pre_incdec_p (machine_mode mode)
{
  return ((reg_addr[mode].addr_mask[RELOAD_REG_ANY] & RELOAD_REG_PRE_INCDEC)
	  != 0);
}

/* Helper function to say whether a mode supports PRE_MODIFY.  */
static inline bool
mode_supports_pre_modify_p (machine_mode mode)
{
  return ((reg_addr[mode].addr_mask[RELOAD_REG_ANY] & RELOAD_REG_PRE_MODIFY)
	  != 0);
}

/* Return true if we have D-form addressing in altivec registers.  */
static inline bool
mode_supports_vmx_dform (machine_mode mode)
{
  return ((reg_addr[mode].addr_mask[RELOAD_REG_VMX] & RELOAD_REG_OFFSET) != 0);
}

/* Return true if we have D-form addressing in VSX registers.  This addressing
   is more limited than normal d-form addressing in that the offset must be
   aligned on a 16-byte boundary.  */
static inline bool
mode_supports_dq_form (machine_mode mode)
{
  return ((reg_addr[mode].addr_mask[RELOAD_REG_ANY] & RELOAD_REG_QUAD_OFFSET)
	  != 0);
}

/* Given that there exists at least one variable that is set (produced)
   by OUT_INSN and read (consumed) by IN_INSN, return true iff
   IN_INSN represents one or more memory store operations and none of
   the variables set by OUT_INSN is used by IN_INSN as the address of a
   store operation.  If either IN_INSN or OUT_INSN does not represent
   a "single" RTL SET expression (as loosely defined by the
   implementation of the single_set function) or a PARALLEL with only
   SETs, CLOBBERs, and USEs inside, this function returns false.

   This rs6000-specific version of store_data_bypass_p checks for
   certain conditions that result in assertion failures (and internal
   compiler errors) in the generic store_data_bypass_p function and
   returns false rather than calling store_data_bypass_p if one of the
   problematic conditions is detected.  */

int
rs6000_store_data_bypass_p (rtx_insn *out_insn, rtx_insn *in_insn)
{
  rtx out_set, in_set;
  rtx out_pat, in_pat;
  rtx out_exp, in_exp;
  int i, j;

  in_set = single_set (in_insn);
  if (in_set)
    {
      if (MEM_P (SET_DEST (in_set)))
	{
	  out_set = single_set (out_insn);
	  if (!out_set)
	    {
	      out_pat = PATTERN (out_insn);
	      if (GET_CODE (out_pat) == PARALLEL)
		{
		  for (i = 0; i < XVECLEN (out_pat, 0); i++)
		    {
		      out_exp = XVECEXP (out_pat, 0, i);
		      if ((GET_CODE (out_exp) == CLOBBER)
			  || (GET_CODE (out_exp) == USE))
			continue;
		      else if (GET_CODE (out_exp) != SET)
			return false;
		    }
		}
	    }
	}
    }
  else
    {
      in_pat = PATTERN (in_insn);
      if (GET_CODE (in_pat) != PARALLEL)
	return false;

      for (i = 0; i < XVECLEN (in_pat, 0); i++)
	{
	  in_exp = XVECEXP (in_pat, 0, i);
	  if ((GET_CODE (in_exp) == CLOBBER) || (GET_CODE (in_exp) == USE))
	    continue;
	  else if (GET_CODE (in_exp) != SET)
	    return false;

	  if (MEM_P (SET_DEST (in_exp)))
	    {
	      out_set = single_set (out_insn);
	      if (!out_set)
		{
		  out_pat = PATTERN (out_insn);
		  if (GET_CODE (out_pat) != PARALLEL)
		    return false;
		  for (j = 0; j < XVECLEN (out_pat, 0); j++)
		    {
		      out_exp = XVECEXP (out_pat, 0, j);
		      if ((GET_CODE (out_exp) == CLOBBER)
			  || (GET_CODE (out_exp) == USE))
			continue;
		      else if (GET_CODE (out_exp) != SET)
			return false;
		    }
		}
	    }
	}
    }
  return store_data_bypass_p (out_insn, in_insn);
}


/* Processor costs (relative to an add) */

const struct processor_costs *rs6000_cost;

/* Instruction size costs on 32bit processors.  */
static const
struct processor_costs size32_cost = {
  COSTS_N_INSNS (1),    /* mulsi */
  COSTS_N_INSNS (1),    /* mulsi_const */
  COSTS_N_INSNS (1),    /* mulsi_const9 */
  COSTS_N_INSNS (1),    /* muldi */
  COSTS_N_INSNS (1),    /* divsi */
  COSTS_N_INSNS (1),    /* divdi */
  COSTS_N_INSNS (1),    /* fp */
  COSTS_N_INSNS (1),    /* dmul */
  COSTS_N_INSNS (1),    /* sdiv */
  COSTS_N_INSNS (1),    /* ddiv */
  32,			/* cache line size */
  0,			/* l1 cache */
  0,			/* l2 cache */
  0,			/* streams */
  0,			/* SF->DF convert */
};

/* Instruction size costs on 64bit processors.  */
static const
struct processor_costs size64_cost = {
  COSTS_N_INSNS (1),    /* mulsi */
  COSTS_N_INSNS (1),    /* mulsi_const */
  COSTS_N_INSNS (1),    /* mulsi_const9 */
  COSTS_N_INSNS (1),    /* muldi */
  COSTS_N_INSNS (1),    /* divsi */
  COSTS_N_INSNS (1),    /* divdi */
  COSTS_N_INSNS (1),    /* fp */
  COSTS_N_INSNS (1),    /* dmul */
  COSTS_N_INSNS (1),    /* sdiv */
  COSTS_N_INSNS (1),    /* ddiv */
  128,			/* cache line size */
  0,			/* l1 cache */
  0,			/* l2 cache */
  0,			/* streams */
  0,			/* SF->DF convert */
};

/* Instruction costs on RS64A processors.  */
static const
struct processor_costs rs64a_cost = {
  COSTS_N_INSNS (20),   /* mulsi */
  COSTS_N_INSNS (12),   /* mulsi_const */
  COSTS_N_INSNS (8),    /* mulsi_const9 */
  COSTS_N_INSNS (34),   /* muldi */
  COSTS_N_INSNS (65),   /* divsi */
  COSTS_N_INSNS (67),   /* divdi */
  COSTS_N_INSNS (4),    /* fp */
  COSTS_N_INSNS (4),    /* dmul */
  COSTS_N_INSNS (31),   /* sdiv */
  COSTS_N_INSNS (31),   /* ddiv */
  128,			/* cache line size */
  128,			/* l1 cache */
  2048,			/* l2 cache */
  1,			/* streams */
  0,			/* SF->DF convert */
};

/* Instruction costs on MPCCORE processors.  */
static const
struct processor_costs mpccore_cost = {
  COSTS_N_INSNS (2),    /* mulsi */
  COSTS_N_INSNS (2),    /* mulsi_const */
  COSTS_N_INSNS (2),    /* mulsi_const9 */
  COSTS_N_INSNS (2),    /* muldi */
  COSTS_N_INSNS (6),    /* divsi */
  COSTS_N_INSNS (6),    /* divdi */
  COSTS_N_INSNS (4),    /* fp */
  COSTS_N_INSNS (5),    /* dmul */
  COSTS_N_INSNS (10),   /* sdiv */
  COSTS_N_INSNS (17),   /* ddiv */
  32,			/* cache line size */
  4,			/* l1 cache */
  16,			/* l2 cache */
  1,			/* streams */
  0,			/* SF->DF convert */
};

/* Instruction costs on PPC403 processors.  */
static const
struct processor_costs ppc403_cost = {
  COSTS_N_INSNS (4),    /* mulsi */
  COSTS_N_INSNS (4),    /* mulsi_const */
  COSTS_N_INSNS (4),    /* mulsi_const9 */
  COSTS_N_INSNS (4),    /* muldi */
  COSTS_N_INSNS (33),   /* divsi */
  COSTS_N_INSNS (33),   /* divdi */
  COSTS_N_INSNS (11),   /* fp */
  COSTS_N_INSNS (11),   /* dmul */
  COSTS_N_INSNS (11),   /* sdiv */
  COSTS_N_INSNS (11),   /* ddiv */
  32,			/* cache line size */
  4,			/* l1 cache */
  16,			/* l2 cache */
  1,			/* streams */
  0,			/* SF->DF convert */
};

/* Instruction costs on PPC405 processors.  */
static const
struct processor_costs ppc405_cost = {
  COSTS_N_INSNS (5),    /* mulsi */
  COSTS_N_INSNS (4),    /* mulsi_const */
  COSTS_N_INSNS (3),    /* mulsi_const9 */
  COSTS_N_INSNS (5),    /* muldi */
  COSTS_N_INSNS (35),   /* divsi */
  COSTS_N_INSNS (35),   /* divdi */
  COSTS_N_INSNS (11),   /* fp */
  COSTS_N_INSNS (11),   /* dmul */
  COSTS_N_INSNS (11),   /* sdiv */
  COSTS_N_INSNS (11),   /* ddiv */
  32,			/* cache line size */
  16,			/* l1 cache */
  128,			/* l2 cache */
  1,			/* streams */
  0,			/* SF->DF convert */
};

/* Instruction costs on PPC440 processors.  */
static const
struct processor_costs ppc440_cost = {
  COSTS_N_INSNS (3),    /* mulsi */
  COSTS_N_INSNS (2),    /* mulsi_const */
  COSTS_N_INSNS (2),    /* mulsi_const9 */
  COSTS_N_INSNS (3),    /* muldi */
  COSTS_N_INSNS (34),   /* divsi */
  COSTS_N_INSNS (34),   /* divdi */
  COSTS_N_INSNS (5),    /* fp */
  COSTS_N_INSNS (5),    /* dmul */
  COSTS_N_INSNS (19),   /* sdiv */
  COSTS_N_INSNS (33),   /* ddiv */
  32,			/* cache line size */
  32,			/* l1 cache */
  256,			/* l2 cache */
  1,			/* streams */
  0,			/* SF->DF convert */
};

/* Instruction costs on PPC476 processors.  */
static const
struct processor_costs ppc476_cost = {
  COSTS_N_INSNS (4),    /* mulsi */
  COSTS_N_INSNS (4),    /* mulsi_const */
  COSTS_N_INSNS (4),    /* mulsi_const9 */
  COSTS_N_INSNS (4),    /* muldi */
  COSTS_N_INSNS (11),   /* divsi */
  COSTS_N_INSNS (11),   /* divdi */
  COSTS_N_INSNS (6),    /* fp */
  COSTS_N_INSNS (6),    /* dmul */
  COSTS_N_INSNS (19),   /* sdiv */
  COSTS_N_INSNS (33),   /* ddiv */
  32,			/* l1 cache line size */
  32,			/* l1 cache */
  512,			/* l2 cache */
  1,			/* streams */
  0,			/* SF->DF convert */
};

/* Instruction costs on PPC601 processors.  */
static const
struct processor_costs ppc601_cost = {
  COSTS_N_INSNS (5),    /* mulsi */
  COSTS_N_INSNS (5),    /* mulsi_const */
  COSTS_N_INSNS (5),    /* mulsi_const9 */
  COSTS_N_INSNS (5),    /* muldi */
  COSTS_N_INSNS (36),   /* divsi */
  COSTS_N_INSNS (36),   /* divdi */
  COSTS_N_INSNS (4),    /* fp */
  COSTS_N_INSNS (5),    /* dmul */
  COSTS_N_INSNS (17),   /* sdiv */
  COSTS_N_INSNS (31),   /* ddiv */
  32,			/* cache line size */
  32,			/* l1 cache */
  256,			/* l2 cache */
  1,			/* streams */
  0,			/* SF->DF convert */
};

/* Instruction costs on PPC603 processors.  */
static const
struct processor_costs ppc603_cost = {
  COSTS_N_INSNS (5),    /* mulsi */
  COSTS_N_INSNS (3),    /* mulsi_const */
  COSTS_N_INSNS (2),    /* mulsi_const9 */
  COSTS_N_INSNS (5),    /* muldi */
  COSTS_N_INSNS (37),   /* divsi */
  COSTS_N_INSNS (37),   /* divdi */
  COSTS_N_INSNS (3),    /* fp */
  COSTS_N_INSNS (4),    /* dmul */
  COSTS_N_INSNS (18),   /* sdiv */
  COSTS_N_INSNS (33),   /* ddiv */
  32,			/* cache line size */
  8,			/* l1 cache */
  64,			/* l2 cache */
  1,			/* streams */
  0,			/* SF->DF convert */
};

/* Instruction costs on PPC604 processors.  */
static const
struct processor_costs ppc604_cost = {
  COSTS_N_INSNS (4),    /* mulsi */
  COSTS_N_INSNS (4),    /* mulsi_const */
  COSTS_N_INSNS (4),    /* mulsi_const9 */
  COSTS_N_INSNS (4),    /* muldi */
  COSTS_N_INSNS (20),   /* divsi */
  COSTS_N_INSNS (20),   /* divdi */
  COSTS_N_INSNS (3),    /* fp */
  COSTS_N_INSNS (3),    /* dmul */
  COSTS_N_INSNS (18),   /* sdiv */
  COSTS_N_INSNS (32),   /* ddiv */
  32,			/* cache line size */
  16,			/* l1 cache */
  512,			/* l2 cache */
  1,			/* streams */
  0,			/* SF->DF convert */
};

/* Instruction costs on PPC604e processors.  */
static const
struct processor_costs ppc604e_cost = {
  COSTS_N_INSNS (2),    /* mulsi */
  COSTS_N_INSNS (2),    /* mulsi_const */
  COSTS_N_INSNS (2),    /* mulsi_const9 */
  COSTS_N_INSNS (2),    /* muldi */
  COSTS_N_INSNS (20),   /* divsi */
  COSTS_N_INSNS (20),   /* divdi */
  COSTS_N_INSNS (3),    /* fp */
  COSTS_N_INSNS (3),    /* dmul */
  COSTS_N_INSNS (18),   /* sdiv */
  COSTS_N_INSNS (32),   /* ddiv */
  32,			/* cache line size */
  32,			/* l1 cache */
  1024,			/* l2 cache */
  1,			/* streams */
  0,			/* SF->DF convert */
};

/* Instruction costs on PPC620 processors.  */
static const
struct processor_costs ppc620_cost = {
  COSTS_N_INSNS (5),    /* mulsi */
  COSTS_N_INSNS (4),    /* mulsi_const */
  COSTS_N_INSNS (3),    /* mulsi_const9 */
  COSTS_N_INSNS (7),    /* muldi */
  COSTS_N_INSNS (21),   /* divsi */
  COSTS_N_INSNS (37),   /* divdi */
  COSTS_N_INSNS (3),    /* fp */
  COSTS_N_INSNS (3),    /* dmul */
  COSTS_N_INSNS (18),   /* sdiv */
  COSTS_N_INSNS (32),   /* ddiv */
  128,			/* cache line size */
  32,			/* l1 cache */
  1024,			/* l2 cache */
  1,			/* streams */
  0,			/* SF->DF convert */
};

/* Instruction costs on PPC630 processors.  */
static const
struct processor_costs ppc630_cost = {
  COSTS_N_INSNS (5),    /* mulsi */
  COSTS_N_INSNS (4),    /* mulsi_const */
  COSTS_N_INSNS (3),    /* mulsi_const9 */
  COSTS_N_INSNS (7),    /* muldi */
  COSTS_N_INSNS (21),   /* divsi */
  COSTS_N_INSNS (37),   /* divdi */
  COSTS_N_INSNS (3),    /* fp */
  COSTS_N_INSNS (3),    /* dmul */
  COSTS_N_INSNS (17),   /* sdiv */
  COSTS_N_INSNS (21),   /* ddiv */
  128,			/* cache line size */
  64,			/* l1 cache */
  1024,			/* l2 cache */
  1,			/* streams */
  0,			/* SF->DF convert */
};

/* Instruction costs on Cell processor.  */
/* COSTS_N_INSNS (1) ~ one add.  */
static const
struct processor_costs ppccell_cost = {
  COSTS_N_INSNS (9/2)+2,    /* mulsi */
  COSTS_N_INSNS (6/2),    /* mulsi_const */
  COSTS_N_INSNS (6/2),    /* mulsi_const9 */
  COSTS_N_INSNS (15/2)+2,   /* muldi */
  COSTS_N_INSNS (38/2),   /* divsi */
  COSTS_N_INSNS (70/2),   /* divdi */
  COSTS_N_INSNS (10/2),   /* fp */
  COSTS_N_INSNS (10/2),   /* dmul */
  COSTS_N_INSNS (74/2),   /* sdiv */
  COSTS_N_INSNS (74/2),   /* ddiv */
  128,			/* cache line size */
  32,			/* l1 cache */
  512,			/* l2 cache */
  6,			/* streams */
  0,			/* SF->DF convert */
};

/* Instruction costs on PPC750 and PPC7400 processors.  */
static const
struct processor_costs ppc750_cost = {
  COSTS_N_INSNS (5),    /* mulsi */
  COSTS_N_INSNS (3),    /* mulsi_const */
  COSTS_N_INSNS (2),    /* mulsi_const9 */
  COSTS_N_INSNS (5),    /* muldi */
  COSTS_N_INSNS (17),   /* divsi */
  COSTS_N_INSNS (17),   /* divdi */
  COSTS_N_INSNS (3),    /* fp */
  COSTS_N_INSNS (3),    /* dmul */
  COSTS_N_INSNS (17),   /* sdiv */
  COSTS_N_INSNS (31),   /* ddiv */
  32,			/* cache line size */
  32,			/* l1 cache */
  512,			/* l2 cache */
  1,			/* streams */
  0,			/* SF->DF convert */
};

/* Instruction costs on PPC7450 processors.  */
static const
struct processor_costs ppc7450_cost = {
  COSTS_N_INSNS (4),    /* mulsi */
  COSTS_N_INSNS (3),    /* mulsi_const */
  COSTS_N_INSNS (3),    /* mulsi_const9 */
  COSTS_N_INSNS (4),    /* muldi */
  COSTS_N_INSNS (23),   /* divsi */
  COSTS_N_INSNS (23),   /* divdi */
  COSTS_N_INSNS (5),    /* fp */
  COSTS_N_INSNS (5),    /* dmul */
  COSTS_N_INSNS (21),   /* sdiv */
  COSTS_N_INSNS (35),   /* ddiv */
  32,			/* cache line size */
  32,			/* l1 cache */
  1024,			/* l2 cache */
  1,			/* streams */
  0,			/* SF->DF convert */
};

/* Instruction costs on PPC8540 processors.  */
static const
struct processor_costs ppc8540_cost = {
  COSTS_N_INSNS (4),    /* mulsi */
  COSTS_N_INSNS (4),    /* mulsi_const */
  COSTS_N_INSNS (4),    /* mulsi_const9 */
  COSTS_N_INSNS (4),    /* muldi */
  COSTS_N_INSNS (19),   /* divsi */
  COSTS_N_INSNS (19),   /* divdi */
  COSTS_N_INSNS (4),    /* fp */
  COSTS_N_INSNS (4),    /* dmul */
  COSTS_N_INSNS (29),   /* sdiv */
  COSTS_N_INSNS (29),   /* ddiv */
  32,			/* cache line size */
  32,			/* l1 cache */
  256,			/* l2 cache */
  1,			/* prefetch streams /*/
  0,			/* SF->DF convert */
};

/* Instruction costs on E300C2 and E300C3 cores.  */
static const
struct processor_costs ppce300c2c3_cost = {
  COSTS_N_INSNS (4),    /* mulsi */
  COSTS_N_INSNS (4),    /* mulsi_const */
  COSTS_N_INSNS (4),    /* mulsi_const9 */
  COSTS_N_INSNS (4),    /* muldi */
  COSTS_N_INSNS (19),   /* divsi */
  COSTS_N_INSNS (19),   /* divdi */
  COSTS_N_INSNS (3),    /* fp */
  COSTS_N_INSNS (4),    /* dmul */
  COSTS_N_INSNS (18),   /* sdiv */
  COSTS_N_INSNS (33),   /* ddiv */
  32,
  16,			/* l1 cache */
  16,			/* l2 cache */
  1,			/* prefetch streams /*/
  0,			/* SF->DF convert */
};

/* Instruction costs on PPCE500MC processors.  */
static const
struct processor_costs ppce500mc_cost = {
  COSTS_N_INSNS (4),    /* mulsi */
  COSTS_N_INSNS (4),    /* mulsi_const */
  COSTS_N_INSNS (4),    /* mulsi_const9 */
  COSTS_N_INSNS (4),    /* muldi */
  COSTS_N_INSNS (14),   /* divsi */
  COSTS_N_INSNS (14),   /* divdi */
  COSTS_N_INSNS (8),    /* fp */
  COSTS_N_INSNS (10),   /* dmul */
  COSTS_N_INSNS (36),   /* sdiv */
  COSTS_N_INSNS (66),   /* ddiv */
  64,			/* cache line size */
  32,			/* l1 cache */
  128,			/* l2 cache */
  1,			/* prefetch streams /*/
  0,			/* SF->DF convert */
};

/* Instruction costs on PPCE500MC64 processors.  */
static const
struct processor_costs ppce500mc64_cost = {
  COSTS_N_INSNS (4),    /* mulsi */
  COSTS_N_INSNS (4),    /* mulsi_const */
  COSTS_N_INSNS (4),    /* mulsi_const9 */
  COSTS_N_INSNS (4),    /* muldi */
  COSTS_N_INSNS (14),   /* divsi */
  COSTS_N_INSNS (14),   /* divdi */
  COSTS_N_INSNS (4),    /* fp */
  COSTS_N_INSNS (10),   /* dmul */
  COSTS_N_INSNS (36),   /* sdiv */
  COSTS_N_INSNS (66),   /* ddiv */
  64,			/* cache line size */
  32,			/* l1 cache */
  128,			/* l2 cache */
  1,			/* prefetch streams /*/
  0,			/* SF->DF convert */
};

/* Instruction costs on PPCE5500 processors.  */
static const
struct processor_costs ppce5500_cost = {
  COSTS_N_INSNS (5),    /* mulsi */
  COSTS_N_INSNS (5),    /* mulsi_const */
  COSTS_N_INSNS (4),    /* mulsi_const9 */
  COSTS_N_INSNS (5),    /* muldi */
  COSTS_N_INSNS (14),   /* divsi */
  COSTS_N_INSNS (14),   /* divdi */
  COSTS_N_INSNS (7),    /* fp */
  COSTS_N_INSNS (10),   /* dmul */
  COSTS_N_INSNS (36),   /* sdiv */
  COSTS_N_INSNS (66),   /* ddiv */
  64,			/* cache line size */
  32,			/* l1 cache */
  128,			/* l2 cache */
  1,			/* prefetch streams /*/
  0,			/* SF->DF convert */
};

/* Instruction costs on PPCE6500 processors.  */
static const
struct processor_costs ppce6500_cost = {
  COSTS_N_INSNS (5),    /* mulsi */
  COSTS_N_INSNS (5),    /* mulsi_const */
  COSTS_N_INSNS (4),    /* mulsi_const9 */
  COSTS_N_INSNS (5),    /* muldi */
  COSTS_N_INSNS (14),   /* divsi */
  COSTS_N_INSNS (14),   /* divdi */
  COSTS_N_INSNS (7),    /* fp */
  COSTS_N_INSNS (10),   /* dmul */
  COSTS_N_INSNS (36),   /* sdiv */
  COSTS_N_INSNS (66),   /* ddiv */
  64,			/* cache line size */
  32,			/* l1 cache */
  128,			/* l2 cache */
  1,			/* prefetch streams /*/
  0,			/* SF->DF convert */
};

/* Instruction costs on AppliedMicro Titan processors.  */
static const
struct processor_costs titan_cost = {
  COSTS_N_INSNS (5),    /* mulsi */
  COSTS_N_INSNS (5),    /* mulsi_const */
  COSTS_N_INSNS (5),    /* mulsi_const9 */
  COSTS_N_INSNS (5),    /* muldi */
  COSTS_N_INSNS (18),   /* divsi */
  COSTS_N_INSNS (18),   /* divdi */
  COSTS_N_INSNS (10),   /* fp */
  COSTS_N_INSNS (10),   /* dmul */
  COSTS_N_INSNS (46),   /* sdiv */
  COSTS_N_INSNS (72),   /* ddiv */
  32,			/* cache line size */
  32,			/* l1 cache */
  512,			/* l2 cache */
  1,			/* prefetch streams /*/
  0,			/* SF->DF convert */
};

/* Instruction costs on POWER4 and POWER5 processors.  */
static const
struct processor_costs power4_cost = {
  COSTS_N_INSNS (3),    /* mulsi */
  COSTS_N_INSNS (2),    /* mulsi_const */
  COSTS_N_INSNS (2),    /* mulsi_const9 */
  COSTS_N_INSNS (4),    /* muldi */
  COSTS_N_INSNS (18),   /* divsi */
  COSTS_N_INSNS (34),   /* divdi */
  COSTS_N_INSNS (3),    /* fp */
  COSTS_N_INSNS (3),    /* dmul */
  COSTS_N_INSNS (17),   /* sdiv */
  COSTS_N_INSNS (17),   /* ddiv */
  128,			/* cache line size */
  32,			/* l1 cache */
  1024,			/* l2 cache */
  8,			/* prefetch streams /*/
  0,			/* SF->DF convert */
};

/* Instruction costs on POWER6 processors.  */
static const
struct processor_costs power6_cost = {
  COSTS_N_INSNS (8),    /* mulsi */
  COSTS_N_INSNS (8),    /* mulsi_const */
  COSTS_N_INSNS (8),    /* mulsi_const9 */
  COSTS_N_INSNS (8),    /* muldi */
  COSTS_N_INSNS (22),   /* divsi */
  COSTS_N_INSNS (28),   /* divdi */
  COSTS_N_INSNS (3),    /* fp */
  COSTS_N_INSNS (3),    /* dmul */
  COSTS_N_INSNS (13),   /* sdiv */
  COSTS_N_INSNS (16),   /* ddiv */
  128,			/* cache line size */
  64,			/* l1 cache */
  2048,			/* l2 cache */
  16,			/* prefetch streams */
  0,			/* SF->DF convert */
};

/* Instruction costs on POWER7 processors.  */
static const
struct processor_costs power7_cost = {
  COSTS_N_INSNS (2),	/* mulsi */
  COSTS_N_INSNS (2),	/* mulsi_const */
  COSTS_N_INSNS (2),	/* mulsi_const9 */
  COSTS_N_INSNS (2),	/* muldi */
  COSTS_N_INSNS (18),	/* divsi */
  COSTS_N_INSNS (34),	/* divdi */
  COSTS_N_INSNS (3),	/* fp */
  COSTS_N_INSNS (3),	/* dmul */
  COSTS_N_INSNS (13),	/* sdiv */
  COSTS_N_INSNS (16),	/* ddiv */
  128,			/* cache line size */
  32,			/* l1 cache */
  256,			/* l2 cache */
  12,			/* prefetch streams */
  COSTS_N_INSNS (3),	/* SF->DF convert */
};

/* Instruction costs on POWER8 processors.  */
static const
struct processor_costs power8_cost = {
  COSTS_N_INSNS (3),	/* mulsi */
  COSTS_N_INSNS (3),	/* mulsi_const */
  COSTS_N_INSNS (3),	/* mulsi_const9 */
  COSTS_N_INSNS (3),	/* muldi */
  COSTS_N_INSNS (19),	/* divsi */
  COSTS_N_INSNS (35),	/* divdi */
  COSTS_N_INSNS (3),	/* fp */
  COSTS_N_INSNS (3),	/* dmul */
  COSTS_N_INSNS (14),	/* sdiv */
  COSTS_N_INSNS (17),	/* ddiv */
  128,			/* cache line size */
  32,			/* l1 cache */
  512,			/* l2 cache */
  12,			/* prefetch streams */
  COSTS_N_INSNS (3),	/* SF->DF convert */
};

/* Instruction costs on POWER9 processors.  */
static const
struct processor_costs power9_cost = {
  COSTS_N_INSNS (3),	/* mulsi */
  COSTS_N_INSNS (3),	/* mulsi_const */
  COSTS_N_INSNS (3),	/* mulsi_const9 */
  COSTS_N_INSNS (3),	/* muldi */
  COSTS_N_INSNS (8),	/* divsi */
  COSTS_N_INSNS (12),	/* divdi */
  COSTS_N_INSNS (3),	/* fp */
  COSTS_N_INSNS (3),	/* dmul */
  COSTS_N_INSNS (13),	/* sdiv */
  COSTS_N_INSNS (18),	/* ddiv */
  128,			/* cache line size */
  32,			/* l1 cache */
  512,			/* l2 cache */
  8,			/* prefetch streams */
  COSTS_N_INSNS (3),	/* SF->DF convert */
};

/* Instruction costs on POWER A2 processors.  */
static const
struct processor_costs ppca2_cost = {
  COSTS_N_INSNS (16),    /* mulsi */
  COSTS_N_INSNS (16),    /* mulsi_const */
  COSTS_N_INSNS (16),    /* mulsi_const9 */
  COSTS_N_INSNS (16),   /* muldi */
  COSTS_N_INSNS (22),   /* divsi */
  COSTS_N_INSNS (28),   /* divdi */
  COSTS_N_INSNS (3),    /* fp */
  COSTS_N_INSNS (3),    /* dmul */
  COSTS_N_INSNS (59),   /* sdiv */
  COSTS_N_INSNS (72),   /* ddiv */
  64,
  16,			/* l1 cache */
  2048,			/* l2 cache */
  16,			/* prefetch streams */
  0,			/* SF->DF convert */
};

/* Support for -mveclibabi=<xxx> to control which vector library to use.  */
static tree (*rs6000_veclib_handler) (combined_fn, tree, tree);


static bool rs6000_debug_legitimate_address_p (machine_mode, rtx, bool);
static tree rs6000_handle_longcall_attribute (tree *, tree, tree, int, bool *);
static tree rs6000_handle_altivec_attribute (tree *, tree, tree, int, bool *);
static tree rs6000_handle_struct_attribute (tree *, tree, tree, int, bool *);
static tree rs6000_builtin_vectorized_libmass (combined_fn, tree, tree);
static void rs6000_emit_set_long_const (rtx, HOST_WIDE_INT);
static int rs6000_memory_move_cost (machine_mode, reg_class_t, bool);
static bool rs6000_debug_rtx_costs (rtx, machine_mode, int, int, int *, bool);
static int rs6000_debug_address_cost (rtx, machine_mode, addr_space_t,
				      bool);
static int rs6000_debug_adjust_cost (rtx_insn *, int, rtx_insn *, int,
				     unsigned int);
static bool is_microcoded_insn (rtx_insn *);
static bool is_nonpipeline_insn (rtx_insn *);
static bool is_cracked_insn (rtx_insn *);
static bool is_load_insn (rtx, rtx *);
static bool is_store_insn (rtx, rtx *);
static bool set_to_load_agen (rtx_insn *,rtx_insn *);
static bool insn_terminates_group_p (rtx_insn *, enum group_termination);
static bool insn_must_be_first_in_group (rtx_insn *);
static bool insn_must_be_last_in_group (rtx_insn *);
int easy_vector_constant (rtx, machine_mode);
static rtx rs6000_debug_legitimize_address (rtx, rtx, machine_mode);
static rtx rs6000_legitimize_tls_address (rtx, enum tls_model);
#if TARGET_MACHO
static tree get_prev_label (tree);
#endif
static bool rs6000_mode_dependent_address (const_rtx);
static bool rs6000_debug_mode_dependent_address (const_rtx);
static bool rs6000_offsettable_memref_p (rtx, machine_mode, bool);
static enum reg_class rs6000_secondary_reload_class (enum reg_class,
						     machine_mode, rtx);
static enum reg_class rs6000_debug_secondary_reload_class (enum reg_class,
							   machine_mode,
							   rtx);
static enum reg_class rs6000_preferred_reload_class (rtx, enum reg_class);
static enum reg_class rs6000_debug_preferred_reload_class (rtx,
							   enum reg_class);
static bool rs6000_debug_secondary_memory_needed (machine_mode,
						  reg_class_t,
						  reg_class_t);
static bool rs6000_debug_can_change_mode_class (machine_mode,
						machine_mode,
						reg_class_t);

static bool (*rs6000_mode_dependent_address_ptr) (const_rtx)
  = rs6000_mode_dependent_address;

enum reg_class (*rs6000_secondary_reload_class_ptr) (enum reg_class,
						     machine_mode, rtx)
  = rs6000_secondary_reload_class;

enum reg_class (*rs6000_preferred_reload_class_ptr) (rtx, enum reg_class)
  = rs6000_preferred_reload_class;

const int INSN_NOT_AVAILABLE = -1;

static void rs6000_print_isa_options (FILE *, int, const char *,
				      HOST_WIDE_INT);
static void rs6000_print_builtin_options (FILE *, int, const char *,
					  HOST_WIDE_INT);
static HOST_WIDE_INT rs6000_disable_incompatible_switches (void);

static enum rs6000_reg_type register_to_reg_type (rtx, bool *);
static bool rs6000_secondary_reload_move (enum rs6000_reg_type,
					  enum rs6000_reg_type,
					  machine_mode,
					  secondary_reload_info *,
					  bool);
static enum non_prefixed_form reg_to_non_prefixed (rtx reg, machine_mode mode);
rtl_opt_pass *make_pass_analyze_swaps (gcc::context*);

/* Hash table stuff for keeping track of TOC entries.  */

struct GTY((for_user)) toc_hash_struct
{
  /* `key' will satisfy CONSTANT_P; in fact, it will satisfy
     ASM_OUTPUT_SPECIAL_POOL_ENTRY_P.  */
  rtx key;
  machine_mode key_mode;
  int labelno;
};

struct toc_hasher : ggc_ptr_hash<toc_hash_struct>
{
  static hashval_t hash (toc_hash_struct *);
  static bool equal (toc_hash_struct *, toc_hash_struct *);
};

static GTY (()) hash_table<toc_hasher> *toc_hash_table;



/* Default register names.  */
char rs6000_reg_names[][8] =
{
  /* GPRs */
      "0",  "1",  "2",  "3",  "4",  "5",  "6",  "7",
      "8",  "9", "10", "11", "12", "13", "14", "15",
     "16", "17", "18", "19", "20", "21", "22", "23",
     "24", "25", "26", "27", "28", "29", "30", "31",
  /* FPRs */
      "0",  "1",  "2",  "3",  "4",  "5",  "6",  "7",
      "8",  "9", "10", "11", "12", "13", "14", "15",
     "16", "17", "18", "19", "20", "21", "22", "23",
     "24", "25", "26", "27", "28", "29", "30", "31",
  /* VRs */
      "0",  "1",  "2",  "3",  "4",  "5",  "6",  "7",
      "8",  "9", "10", "11", "12", "13", "14", "15",
     "16", "17", "18", "19", "20", "21", "22", "23",
     "24", "25", "26", "27", "28", "29", "30", "31",
  /* lr ctr ca ap */
     "lr", "ctr", "ca", "ap",
  /* cr0..cr7 */
      "0",  "1",  "2",  "3",  "4",  "5",  "6",  "7",
  /* vrsave vscr sfp */
      "vrsave", "vscr", "sfp",
};

#ifdef TARGET_REGNAMES
static const char alt_reg_names[][8] =
{
  /* GPRs */
   "%r0",  "%r1",  "%r2",  "%r3",  "%r4",  "%r5",  "%r6",  "%r7",
   "%r8",  "%r9", "%r10", "%r11", "%r12", "%r13", "%r14", "%r15",
  "%r16", "%r17", "%r18", "%r19", "%r20", "%r21", "%r22", "%r23",
  "%r24", "%r25", "%r26", "%r27", "%r28", "%r29", "%r30", "%r31",
  /* FPRs */
   "%f0",  "%f1",  "%f2",  "%f3",  "%f4",  "%f5",  "%f6",  "%f7",
   "%f8",  "%f9", "%f10", "%f11", "%f12", "%f13", "%f14", "%f15",
  "%f16", "%f17", "%f18", "%f19", "%f20", "%f21", "%f22", "%f23",
  "%f24", "%f25", "%f26", "%f27", "%f28", "%f29", "%f30", "%f31",
  /* VRs */
   "%v0",  "%v1",  "%v2",  "%v3",  "%v4",  "%v5",  "%v6",  "%v7",
   "%v8",  "%v9", "%v10", "%v11", "%v12", "%v13", "%v14", "%v15",
  "%v16", "%v17", "%v18", "%v19", "%v20", "%v21", "%v22", "%v23",
  "%v24", "%v25", "%v26", "%v27", "%v28", "%v29", "%v30", "%v31",
  /* lr ctr ca ap */
    "lr",  "ctr",   "ca",   "ap",
  /* cr0..cr7 */
  "%cr0",  "%cr1", "%cr2", "%cr3", "%cr4", "%cr5", "%cr6", "%cr7",
  /* vrsave vscr sfp */
  "vrsave", "vscr", "sfp",
};
#endif

/* Table of valid machine attributes.  */

static const struct attribute_spec rs6000_attribute_table[] =
{
  /* { name, min_len, max_len, decl_req, type_req, fn_type_req,
       affects_type_identity, handler, exclude } */
  { "altivec",   1, 1, false, true,  false, false,
    rs6000_handle_altivec_attribute, NULL },
  { "longcall",  0, 0, false, true,  true,  false,
    rs6000_handle_longcall_attribute, NULL },
  { "shortcall", 0, 0, false, true,  true,  false,
    rs6000_handle_longcall_attribute, NULL },
  { "ms_struct", 0, 0, false, false, false, false,
    rs6000_handle_struct_attribute, NULL },
  { "gcc_struct", 0, 0, false, false, false, false,
    rs6000_handle_struct_attribute, NULL },
#ifdef SUBTARGET_ATTRIBUTE_TABLE
  SUBTARGET_ATTRIBUTE_TABLE,
#endif
  { NULL,        0, 0, false, false, false, false, NULL, NULL }
};

#ifndef TARGET_PROFILE_KERNEL
#define TARGET_PROFILE_KERNEL 0
#endif

/* Initialize the GCC target structure.  */
#undef TARGET_ATTRIBUTE_TABLE
#define TARGET_ATTRIBUTE_TABLE rs6000_attribute_table
#undef TARGET_SET_DEFAULT_TYPE_ATTRIBUTES
#define TARGET_SET_DEFAULT_TYPE_ATTRIBUTES rs6000_set_default_type_attributes
#undef TARGET_ATTRIBUTE_TAKES_IDENTIFIER_P
#define TARGET_ATTRIBUTE_TAKES_IDENTIFIER_P rs6000_attribute_takes_identifier_p

#undef TARGET_ASM_ALIGNED_DI_OP
#define TARGET_ASM_ALIGNED_DI_OP DOUBLE_INT_ASM_OP

/* Default unaligned ops are only provided for ELF.  Find the ops needed
   for non-ELF systems.  */
#ifndef OBJECT_FORMAT_ELF
#if TARGET_XCOFF
/* For XCOFF.  rs6000_assemble_integer will handle unaligned DIs on
   64-bit targets.  */
#undef TARGET_ASM_UNALIGNED_HI_OP
#define TARGET_ASM_UNALIGNED_HI_OP "\t.vbyte\t2,"
#undef TARGET_ASM_UNALIGNED_SI_OP
#define TARGET_ASM_UNALIGNED_SI_OP "\t.vbyte\t4,"
#undef TARGET_ASM_UNALIGNED_DI_OP
#define TARGET_ASM_UNALIGNED_DI_OP "\t.vbyte\t8,"
#else
/* For Darwin.  */
#undef TARGET_ASM_UNALIGNED_HI_OP
#define TARGET_ASM_UNALIGNED_HI_OP "\t.short\t"
#undef TARGET_ASM_UNALIGNED_SI_OP
#define TARGET_ASM_UNALIGNED_SI_OP "\t.long\t"
#undef TARGET_ASM_UNALIGNED_DI_OP
#define TARGET_ASM_UNALIGNED_DI_OP "\t.quad\t"
#undef TARGET_ASM_ALIGNED_DI_OP
#define TARGET_ASM_ALIGNED_DI_OP "\t.quad\t"
#endif
#endif

/* This hook deals with fixups for relocatable code and DI-mode objects
   in 64-bit code.  */
#undef TARGET_ASM_INTEGER
#define TARGET_ASM_INTEGER rs6000_assemble_integer

#if defined (HAVE_GAS_HIDDEN) && !TARGET_MACHO
#undef TARGET_ASM_ASSEMBLE_VISIBILITY
#define TARGET_ASM_ASSEMBLE_VISIBILITY rs6000_assemble_visibility
#endif

#undef TARGET_SET_UP_BY_PROLOGUE
#define TARGET_SET_UP_BY_PROLOGUE rs6000_set_up_by_prologue

#undef TARGET_SHRINK_WRAP_GET_SEPARATE_COMPONENTS
#define TARGET_SHRINK_WRAP_GET_SEPARATE_COMPONENTS rs6000_get_separate_components
#undef TARGET_SHRINK_WRAP_COMPONENTS_FOR_BB
#define TARGET_SHRINK_WRAP_COMPONENTS_FOR_BB rs6000_components_for_bb
#undef TARGET_SHRINK_WRAP_DISQUALIFY_COMPONENTS
#define TARGET_SHRINK_WRAP_DISQUALIFY_COMPONENTS rs6000_disqualify_components
#undef TARGET_SHRINK_WRAP_EMIT_PROLOGUE_COMPONENTS
#define TARGET_SHRINK_WRAP_EMIT_PROLOGUE_COMPONENTS rs6000_emit_prologue_components
#undef TARGET_SHRINK_WRAP_EMIT_EPILOGUE_COMPONENTS
#define TARGET_SHRINK_WRAP_EMIT_EPILOGUE_COMPONENTS rs6000_emit_epilogue_components
#undef TARGET_SHRINK_WRAP_SET_HANDLED_COMPONENTS
#define TARGET_SHRINK_WRAP_SET_HANDLED_COMPONENTS rs6000_set_handled_components

#undef TARGET_EXTRA_LIVE_ON_ENTRY
#define TARGET_EXTRA_LIVE_ON_ENTRY rs6000_live_on_entry

#undef TARGET_INTERNAL_ARG_POINTER
#define TARGET_INTERNAL_ARG_POINTER rs6000_internal_arg_pointer

#undef TARGET_HAVE_TLS
#define TARGET_HAVE_TLS HAVE_AS_TLS

#undef TARGET_CANNOT_FORCE_CONST_MEM
#define TARGET_CANNOT_FORCE_CONST_MEM rs6000_cannot_force_const_mem

#undef TARGET_DELEGITIMIZE_ADDRESS
#define TARGET_DELEGITIMIZE_ADDRESS rs6000_delegitimize_address

#undef TARGET_CONST_NOT_OK_FOR_DEBUG_P
#define TARGET_CONST_NOT_OK_FOR_DEBUG_P rs6000_const_not_ok_for_debug_p

#undef TARGET_LEGITIMATE_COMBINED_INSN
#define TARGET_LEGITIMATE_COMBINED_INSN rs6000_legitimate_combined_insn

#undef TARGET_ASM_FUNCTION_PROLOGUE
#define TARGET_ASM_FUNCTION_PROLOGUE rs6000_output_function_prologue
#undef TARGET_ASM_FUNCTION_EPILOGUE
#define TARGET_ASM_FUNCTION_EPILOGUE rs6000_output_function_epilogue

#undef TARGET_ASM_OUTPUT_ADDR_CONST_EXTRA
#define TARGET_ASM_OUTPUT_ADDR_CONST_EXTRA rs6000_output_addr_const_extra

#undef TARGET_LEGITIMIZE_ADDRESS
#define TARGET_LEGITIMIZE_ADDRESS rs6000_legitimize_address

#undef  TARGET_SCHED_VARIABLE_ISSUE
#define TARGET_SCHED_VARIABLE_ISSUE rs6000_variable_issue

#undef TARGET_SCHED_ISSUE_RATE
#define TARGET_SCHED_ISSUE_RATE rs6000_issue_rate
#undef TARGET_SCHED_ADJUST_COST
#define TARGET_SCHED_ADJUST_COST rs6000_adjust_cost
#undef TARGET_SCHED_ADJUST_PRIORITY
#define TARGET_SCHED_ADJUST_PRIORITY rs6000_adjust_priority
#undef TARGET_SCHED_IS_COSTLY_DEPENDENCE
#define TARGET_SCHED_IS_COSTLY_DEPENDENCE rs6000_is_costly_dependence
#undef TARGET_SCHED_INIT
#define TARGET_SCHED_INIT rs6000_sched_init
#undef TARGET_SCHED_FINISH
#define TARGET_SCHED_FINISH rs6000_sched_finish
#undef TARGET_SCHED_REORDER
#define TARGET_SCHED_REORDER rs6000_sched_reorder
#undef TARGET_SCHED_REORDER2
#define TARGET_SCHED_REORDER2 rs6000_sched_reorder2

#undef TARGET_SCHED_FIRST_CYCLE_MULTIPASS_DFA_LOOKAHEAD
#define TARGET_SCHED_FIRST_CYCLE_MULTIPASS_DFA_LOOKAHEAD rs6000_use_sched_lookahead

#undef TARGET_SCHED_FIRST_CYCLE_MULTIPASS_DFA_LOOKAHEAD_GUARD
#define TARGET_SCHED_FIRST_CYCLE_MULTIPASS_DFA_LOOKAHEAD_GUARD rs6000_use_sched_lookahead_guard

#undef TARGET_SCHED_ALLOC_SCHED_CONTEXT
#define TARGET_SCHED_ALLOC_SCHED_CONTEXT rs6000_alloc_sched_context
#undef TARGET_SCHED_INIT_SCHED_CONTEXT
#define TARGET_SCHED_INIT_SCHED_CONTEXT rs6000_init_sched_context
#undef TARGET_SCHED_SET_SCHED_CONTEXT
#define TARGET_SCHED_SET_SCHED_CONTEXT rs6000_set_sched_context
#undef TARGET_SCHED_FREE_SCHED_CONTEXT
#define TARGET_SCHED_FREE_SCHED_CONTEXT rs6000_free_sched_context

#undef TARGET_SCHED_CAN_SPECULATE_INSN
#define TARGET_SCHED_CAN_SPECULATE_INSN rs6000_sched_can_speculate_insn

#undef TARGET_VECTORIZE_BUILTIN_MASK_FOR_LOAD
#define TARGET_VECTORIZE_BUILTIN_MASK_FOR_LOAD rs6000_builtin_mask_for_load
#undef TARGET_VECTORIZE_SUPPORT_VECTOR_MISALIGNMENT
#define TARGET_VECTORIZE_SUPPORT_VECTOR_MISALIGNMENT		\
  rs6000_builtin_support_vector_misalignment
#undef TARGET_VECTORIZE_VECTOR_ALIGNMENT_REACHABLE
#define TARGET_VECTORIZE_VECTOR_ALIGNMENT_REACHABLE rs6000_vector_alignment_reachable
#undef TARGET_VECTORIZE_BUILTIN_VECTORIZATION_COST
#define TARGET_VECTORIZE_BUILTIN_VECTORIZATION_COST \
  rs6000_builtin_vectorization_cost
#undef TARGET_VECTORIZE_PREFERRED_SIMD_MODE
#define TARGET_VECTORIZE_PREFERRED_SIMD_MODE \
  rs6000_preferred_simd_mode
#undef TARGET_VECTORIZE_INIT_COST
#define TARGET_VECTORIZE_INIT_COST rs6000_init_cost
#undef TARGET_VECTORIZE_ADD_STMT_COST
#define TARGET_VECTORIZE_ADD_STMT_COST rs6000_add_stmt_cost
#undef TARGET_VECTORIZE_FINISH_COST
#define TARGET_VECTORIZE_FINISH_COST rs6000_finish_cost
#undef TARGET_VECTORIZE_DESTROY_COST_DATA
#define TARGET_VECTORIZE_DESTROY_COST_DATA rs6000_destroy_cost_data

#undef TARGET_LOOP_UNROLL_ADJUST
#define TARGET_LOOP_UNROLL_ADJUST rs6000_loop_unroll_adjust

#undef TARGET_INIT_BUILTINS
#define TARGET_INIT_BUILTINS rs6000_init_builtins
#undef TARGET_BUILTIN_DECL
#define TARGET_BUILTIN_DECL rs6000_builtin_decl

#undef TARGET_FOLD_BUILTIN
#define TARGET_FOLD_BUILTIN rs6000_fold_builtin
#undef TARGET_GIMPLE_FOLD_BUILTIN
#define TARGET_GIMPLE_FOLD_BUILTIN rs6000_gimple_fold_builtin

#undef TARGET_EXPAND_BUILTIN
#define TARGET_EXPAND_BUILTIN rs6000_expand_builtin

#undef TARGET_MANGLE_TYPE
#define TARGET_MANGLE_TYPE rs6000_mangle_type

#undef TARGET_INIT_LIBFUNCS
#define TARGET_INIT_LIBFUNCS rs6000_init_libfuncs

#if TARGET_MACHO
#undef TARGET_BINDS_LOCAL_P
#define TARGET_BINDS_LOCAL_P darwin_binds_local_p
#endif

#undef TARGET_MS_BITFIELD_LAYOUT_P
#define TARGET_MS_BITFIELD_LAYOUT_P rs6000_ms_bitfield_layout_p

#undef TARGET_ASM_OUTPUT_MI_THUNK
#define TARGET_ASM_OUTPUT_MI_THUNK rs6000_output_mi_thunk

#undef TARGET_ASM_CAN_OUTPUT_MI_THUNK
#define TARGET_ASM_CAN_OUTPUT_MI_THUNK hook_bool_const_tree_hwi_hwi_const_tree_true

#undef TARGET_FUNCTION_OK_FOR_SIBCALL
#define TARGET_FUNCTION_OK_FOR_SIBCALL rs6000_function_ok_for_sibcall

#undef TARGET_REGISTER_MOVE_COST
#define TARGET_REGISTER_MOVE_COST rs6000_register_move_cost
#undef TARGET_MEMORY_MOVE_COST
#define TARGET_MEMORY_MOVE_COST rs6000_memory_move_cost
#undef TARGET_IRA_CHANGE_PSEUDO_ALLOCNO_CLASS
#define TARGET_IRA_CHANGE_PSEUDO_ALLOCNO_CLASS \
  rs6000_ira_change_pseudo_allocno_class
#undef TARGET_CANNOT_COPY_INSN_P
#define TARGET_CANNOT_COPY_INSN_P rs6000_cannot_copy_insn_p
#undef TARGET_RTX_COSTS
#define TARGET_RTX_COSTS rs6000_rtx_costs
#undef TARGET_ADDRESS_COST
#define TARGET_ADDRESS_COST hook_int_rtx_mode_as_bool_0
#undef TARGET_INSN_COST
#define TARGET_INSN_COST rs6000_insn_cost

#undef TARGET_INIT_DWARF_REG_SIZES_EXTRA
#define TARGET_INIT_DWARF_REG_SIZES_EXTRA rs6000_init_dwarf_reg_sizes_extra

#undef TARGET_PROMOTE_FUNCTION_MODE
#define TARGET_PROMOTE_FUNCTION_MODE rs6000_promote_function_mode

#undef TARGET_OVERRIDE_OPTIONS_AFTER_CHANGE
#define TARGET_OVERRIDE_OPTIONS_AFTER_CHANGE rs6000_override_options_after_change

#undef TARGET_RETURN_IN_MEMORY
#define TARGET_RETURN_IN_MEMORY rs6000_return_in_memory

#undef TARGET_RETURN_IN_MSB
#define TARGET_RETURN_IN_MSB rs6000_return_in_msb

#undef TARGET_SETUP_INCOMING_VARARGS
#define TARGET_SETUP_INCOMING_VARARGS setup_incoming_varargs

/* Always strict argument naming on rs6000.  */
#undef TARGET_STRICT_ARGUMENT_NAMING
#define TARGET_STRICT_ARGUMENT_NAMING hook_bool_CUMULATIVE_ARGS_true
#undef TARGET_PRETEND_OUTGOING_VARARGS_NAMED
#define TARGET_PRETEND_OUTGOING_VARARGS_NAMED hook_bool_CUMULATIVE_ARGS_true
#undef TARGET_SPLIT_COMPLEX_ARG
#define TARGET_SPLIT_COMPLEX_ARG hook_bool_const_tree_true
#undef TARGET_MUST_PASS_IN_STACK
#define TARGET_MUST_PASS_IN_STACK rs6000_must_pass_in_stack
#undef TARGET_PASS_BY_REFERENCE
#define TARGET_PASS_BY_REFERENCE rs6000_pass_by_reference
#undef TARGET_ARG_PARTIAL_BYTES
#define TARGET_ARG_PARTIAL_BYTES rs6000_arg_partial_bytes
#undef TARGET_FUNCTION_ARG_ADVANCE
#define TARGET_FUNCTION_ARG_ADVANCE rs6000_function_arg_advance
#undef TARGET_FUNCTION_ARG
#define TARGET_FUNCTION_ARG rs6000_function_arg
#undef TARGET_FUNCTION_ARG_PADDING
#define TARGET_FUNCTION_ARG_PADDING rs6000_function_arg_padding
#undef TARGET_FUNCTION_ARG_BOUNDARY
#define TARGET_FUNCTION_ARG_BOUNDARY rs6000_function_arg_boundary

#undef TARGET_BUILD_BUILTIN_VA_LIST
#define TARGET_BUILD_BUILTIN_VA_LIST rs6000_build_builtin_va_list

#undef TARGET_EXPAND_BUILTIN_VA_START
#define TARGET_EXPAND_BUILTIN_VA_START rs6000_va_start

#undef TARGET_GIMPLIFY_VA_ARG_EXPR
#define TARGET_GIMPLIFY_VA_ARG_EXPR rs6000_gimplify_va_arg

#undef TARGET_EH_RETURN_FILTER_MODE
#define TARGET_EH_RETURN_FILTER_MODE rs6000_eh_return_filter_mode

#undef TARGET_TRANSLATE_MODE_ATTRIBUTE
#define TARGET_TRANSLATE_MODE_ATTRIBUTE rs6000_translate_mode_attribute

#undef TARGET_SCALAR_MODE_SUPPORTED_P
#define TARGET_SCALAR_MODE_SUPPORTED_P rs6000_scalar_mode_supported_p

#undef TARGET_VECTOR_MODE_SUPPORTED_P
#define TARGET_VECTOR_MODE_SUPPORTED_P rs6000_vector_mode_supported_p

#undef TARGET_FLOATN_MODE
#define TARGET_FLOATN_MODE rs6000_floatn_mode

#undef TARGET_INVALID_ARG_FOR_UNPROTOTYPED_FN
#define TARGET_INVALID_ARG_FOR_UNPROTOTYPED_FN invalid_arg_for_unprototyped_fn

#undef TARGET_MD_ASM_ADJUST
#define TARGET_MD_ASM_ADJUST rs6000_md_asm_adjust

#undef TARGET_OPTION_OVERRIDE
#define TARGET_OPTION_OVERRIDE rs6000_option_override

#undef TARGET_VECTORIZE_BUILTIN_VECTORIZED_FUNCTION
#define TARGET_VECTORIZE_BUILTIN_VECTORIZED_FUNCTION \
  rs6000_builtin_vectorized_function

#undef TARGET_VECTORIZE_BUILTIN_MD_VECTORIZED_FUNCTION
#define TARGET_VECTORIZE_BUILTIN_MD_VECTORIZED_FUNCTION \
  rs6000_builtin_md_vectorized_function

#undef TARGET_STACK_PROTECT_GUARD
#define TARGET_STACK_PROTECT_GUARD rs6000_init_stack_protect_guard

#if !TARGET_MACHO
#undef TARGET_STACK_PROTECT_FAIL
#define TARGET_STACK_PROTECT_FAIL rs6000_stack_protect_fail
#endif

#ifdef HAVE_AS_TLS
#undef TARGET_ASM_OUTPUT_DWARF_DTPREL
#define TARGET_ASM_OUTPUT_DWARF_DTPREL rs6000_output_dwarf_dtprel
#endif

/* Use a 32-bit anchor range.  This leads to sequences like:

	addis	tmp,anchor,high
	add	dest,tmp,low

   where tmp itself acts as an anchor, and can be shared between
   accesses to the same 64k page.  */
#undef TARGET_MIN_ANCHOR_OFFSET
#define TARGET_MIN_ANCHOR_OFFSET -0x7fffffff - 1
#undef TARGET_MAX_ANCHOR_OFFSET
#define TARGET_MAX_ANCHOR_OFFSET 0x7fffffff
#undef TARGET_USE_BLOCKS_FOR_CONSTANT_P
#define TARGET_USE_BLOCKS_FOR_CONSTANT_P rs6000_use_blocks_for_constant_p
#undef TARGET_USE_BLOCKS_FOR_DECL_P
#define TARGET_USE_BLOCKS_FOR_DECL_P rs6000_use_blocks_for_decl_p

#undef TARGET_BUILTIN_RECIPROCAL
#define TARGET_BUILTIN_RECIPROCAL rs6000_builtin_reciprocal

#undef TARGET_SECONDARY_RELOAD
#define TARGET_SECONDARY_RELOAD rs6000_secondary_reload
#undef TARGET_SECONDARY_MEMORY_NEEDED
#define TARGET_SECONDARY_MEMORY_NEEDED rs6000_secondary_memory_needed
#undef TARGET_SECONDARY_MEMORY_NEEDED_MODE
#define TARGET_SECONDARY_MEMORY_NEEDED_MODE rs6000_secondary_memory_needed_mode

#undef TARGET_LEGITIMATE_ADDRESS_P
#define TARGET_LEGITIMATE_ADDRESS_P rs6000_legitimate_address_p

#undef TARGET_MODE_DEPENDENT_ADDRESS_P
#define TARGET_MODE_DEPENDENT_ADDRESS_P rs6000_mode_dependent_address_p

#undef TARGET_COMPUTE_PRESSURE_CLASSES
#define TARGET_COMPUTE_PRESSURE_CLASSES rs6000_compute_pressure_classes

#undef TARGET_CAN_ELIMINATE
#define TARGET_CAN_ELIMINATE rs6000_can_eliminate

#undef TARGET_CONDITIONAL_REGISTER_USAGE
#define TARGET_CONDITIONAL_REGISTER_USAGE rs6000_conditional_register_usage

#undef TARGET_SCHED_REASSOCIATION_WIDTH
#define TARGET_SCHED_REASSOCIATION_WIDTH rs6000_reassociation_width

#undef TARGET_TRAMPOLINE_INIT
#define TARGET_TRAMPOLINE_INIT rs6000_trampoline_init

#undef TARGET_FUNCTION_VALUE
#define TARGET_FUNCTION_VALUE rs6000_function_value

#undef TARGET_OPTION_VALID_ATTRIBUTE_P
#define TARGET_OPTION_VALID_ATTRIBUTE_P rs6000_valid_attribute_p

#undef TARGET_OPTION_SAVE
#define TARGET_OPTION_SAVE rs6000_function_specific_save

#undef TARGET_OPTION_RESTORE
#define TARGET_OPTION_RESTORE rs6000_function_specific_restore

#undef TARGET_OPTION_PRINT
#define TARGET_OPTION_PRINT rs6000_function_specific_print

#undef TARGET_CAN_INLINE_P
#define TARGET_CAN_INLINE_P rs6000_can_inline_p

#undef TARGET_SET_CURRENT_FUNCTION
#define TARGET_SET_CURRENT_FUNCTION rs6000_set_current_function

#undef TARGET_LEGITIMATE_CONSTANT_P
#define TARGET_LEGITIMATE_CONSTANT_P rs6000_legitimate_constant_p

#undef TARGET_VECTORIZE_VEC_PERM_CONST
#define TARGET_VECTORIZE_VEC_PERM_CONST rs6000_vectorize_vec_perm_const

#undef TARGET_CAN_USE_DOLOOP_P
#define TARGET_CAN_USE_DOLOOP_P can_use_doloop_if_innermost

#undef TARGET_PREDICT_DOLOOP_P
#define TARGET_PREDICT_DOLOOP_P rs6000_predict_doloop_p

#undef TARGET_HAVE_COUNT_REG_DECR_P
#define TARGET_HAVE_COUNT_REG_DECR_P true

/* 1000000000 is infinite cost in IVOPTs.  */
#undef TARGET_DOLOOP_COST_FOR_GENERIC
#define TARGET_DOLOOP_COST_FOR_GENERIC 1000000000

#undef TARGET_DOLOOP_COST_FOR_ADDRESS
#define TARGET_DOLOOP_COST_FOR_ADDRESS 1000000000

#undef TARGET_ATOMIC_ASSIGN_EXPAND_FENV
#define TARGET_ATOMIC_ASSIGN_EXPAND_FENV rs6000_atomic_assign_expand_fenv

#undef TARGET_LIBGCC_CMP_RETURN_MODE
#define TARGET_LIBGCC_CMP_RETURN_MODE rs6000_abi_word_mode
#undef TARGET_LIBGCC_SHIFT_COUNT_MODE
#define TARGET_LIBGCC_SHIFT_COUNT_MODE rs6000_abi_word_mode
#undef TARGET_UNWIND_WORD_MODE
#define TARGET_UNWIND_WORD_MODE rs6000_abi_word_mode

#undef TARGET_OFFLOAD_OPTIONS
#define TARGET_OFFLOAD_OPTIONS rs6000_offload_options

#undef TARGET_C_MODE_FOR_SUFFIX
#define TARGET_C_MODE_FOR_SUFFIX rs6000_c_mode_for_suffix

#undef TARGET_INVALID_BINARY_OP
#define TARGET_INVALID_BINARY_OP rs6000_invalid_binary_op

#undef TARGET_OPTAB_SUPPORTED_P
#define TARGET_OPTAB_SUPPORTED_P rs6000_optab_supported_p

#undef TARGET_CUSTOM_FUNCTION_DESCRIPTORS
#define TARGET_CUSTOM_FUNCTION_DESCRIPTORS 1

#undef TARGET_COMPARE_VERSION_PRIORITY
#define TARGET_COMPARE_VERSION_PRIORITY rs6000_compare_version_priority

#undef TARGET_GENERATE_VERSION_DISPATCHER_BODY
#define TARGET_GENERATE_VERSION_DISPATCHER_BODY				\
  rs6000_generate_version_dispatcher_body

#undef TARGET_GET_FUNCTION_VERSIONS_DISPATCHER
#define TARGET_GET_FUNCTION_VERSIONS_DISPATCHER				\
  rs6000_get_function_versions_dispatcher

#undef TARGET_OPTION_FUNCTION_VERSIONS
#define TARGET_OPTION_FUNCTION_VERSIONS common_function_versions

#undef TARGET_HARD_REGNO_NREGS
#define TARGET_HARD_REGNO_NREGS rs6000_hard_regno_nregs_hook
#undef TARGET_HARD_REGNO_MODE_OK
#define TARGET_HARD_REGNO_MODE_OK rs6000_hard_regno_mode_ok

#undef TARGET_MODES_TIEABLE_P
#define TARGET_MODES_TIEABLE_P rs6000_modes_tieable_p

#undef TARGET_HARD_REGNO_CALL_PART_CLOBBERED
#define TARGET_HARD_REGNO_CALL_PART_CLOBBERED \
  rs6000_hard_regno_call_part_clobbered

#undef TARGET_SLOW_UNALIGNED_ACCESS
#define TARGET_SLOW_UNALIGNED_ACCESS rs6000_slow_unaligned_access

#undef TARGET_CAN_CHANGE_MODE_CLASS
#define TARGET_CAN_CHANGE_MODE_CLASS rs6000_can_change_mode_class

#undef TARGET_CONSTANT_ALIGNMENT
#define TARGET_CONSTANT_ALIGNMENT rs6000_constant_alignment

#undef TARGET_STARTING_FRAME_OFFSET
#define TARGET_STARTING_FRAME_OFFSET rs6000_starting_frame_offset

#if TARGET_ELF && RS6000_WEAK
#undef TARGET_ASM_GLOBALIZE_DECL_NAME
#define TARGET_ASM_GLOBALIZE_DECL_NAME rs6000_globalize_decl_name
#endif

#undef TARGET_SETJMP_PRESERVES_NONVOLATILE_REGS_P
#define TARGET_SETJMP_PRESERVES_NONVOLATILE_REGS_P hook_bool_void_true

#undef TARGET_MANGLE_DECL_ASSEMBLER_NAME
#define TARGET_MANGLE_DECL_ASSEMBLER_NAME rs6000_mangle_decl_assembler_name

#undef TARGET_CANNOT_SUBSTITUTE_MEM_EQUIV_P
#define TARGET_CANNOT_SUBSTITUTE_MEM_EQUIV_P \
  rs6000_cannot_substitute_mem_equiv_p

#undef TARGET_INVALID_CONVERSION
#define TARGET_INVALID_CONVERSION rs6000_invalid_conversion


/* Processor table.  */
struct rs6000_ptt
{
  const char *const name;		/* Canonical processor name.  */
  const enum processor_type processor;	/* Processor type enum value.  */
  const HOST_WIDE_INT target_enable;	/* Target flags to enable.  */
};

static struct rs6000_ptt const processor_target_table[] =
{
#define RS6000_CPU(NAME, CPU, FLAGS) { NAME, CPU, FLAGS },
#include "rs6000-cpus.def"
#undef RS6000_CPU
};

/* Look up a processor name for -mcpu=xxx and -mtune=xxx.  Return -1 if the
   name is invalid.  */

static int
rs6000_cpu_name_lookup (const char *name)
{
  size_t i;

  if (name != NULL)
    {
      for (i = 0; i < ARRAY_SIZE (processor_target_table); i++)
	if (! strcmp (name, processor_target_table[i].name))
	  return (int)i;
    }

  return -1;
}


/* Return number of consecutive hard regs needed starting at reg REGNO
   to hold something of mode MODE.
   This is ordinarily the length in words of a value of mode MODE
   but can be less for certain modes in special long registers.

   POWER and PowerPC GPRs hold 32 bits worth;
   PowerPC64 GPRs and FPRs point register holds 64 bits worth.  */

static int
rs6000_hard_regno_nregs_internal (int regno, machine_mode mode)
{
  unsigned HOST_WIDE_INT reg_size;

  /* 128-bit floating point usually takes 2 registers, unless it is IEEE
     128-bit floating point that can go in vector registers, which has VSX
     memory addressing.  */
  if (FP_REGNO_P (regno))
    reg_size = (VECTOR_MEM_VSX_P (mode) || VECTOR_ALIGNMENT_P (mode)
		? UNITS_PER_VSX_WORD
		: UNITS_PER_FP_WORD);

  else if (ALTIVEC_REGNO_P (regno))
    reg_size = UNITS_PER_ALTIVEC_WORD;

  else
    reg_size = UNITS_PER_WORD;

  return (GET_MODE_SIZE (mode) + reg_size - 1) / reg_size;
}

/* Value is 1 if hard register REGNO can hold a value of machine-mode
   MODE.  */
static int
rs6000_hard_regno_mode_ok_uncached (int regno, machine_mode mode)
{
  int last_regno = regno + rs6000_hard_regno_nregs[mode][regno] - 1;

  if (COMPLEX_MODE_P (mode))
    mode = GET_MODE_INNER (mode);

  /* Vector pair modes need even/odd VSX register pairs.  Only allow vector
     registers.  We need to allow OImode to have the same registers as POImode,
     even though we do not enable the move pattern for OImode.  */
  if (mode == POImode || mode == OImode)
    return (TARGET_MMA && VSX_REGNO_P (regno) && (regno & 1) == 0);

  /* MMA accumulator modes need FPR registers divisible by 4.  We need to allow
     XImode to have the same registers as PXImode, even though we do not enable
     the move pattern for XImode.  */
  if (mode == PXImode || mode == XImode)
    return (TARGET_MMA && FP_REGNO_P (regno) && (regno & 3) == 0);

  /* PTImode can only go in GPRs.  Quad word memory operations require even/odd
     register combinations, and use PTImode where we need to deal with quad
     word memory operations.  Don't allow quad words in the argument or frame
     pointer registers, just registers 0..31.  */
  if (mode == PTImode)
    return (IN_RANGE (regno, FIRST_GPR_REGNO, LAST_GPR_REGNO)
	    && IN_RANGE (last_regno, FIRST_GPR_REGNO, LAST_GPR_REGNO)
	    && ((regno & 1) == 0));

  /* VSX registers that overlap the FPR registers are larger than for non-VSX
     implementations.  Don't allow an item to be split between a FP register
     and an Altivec register.  Allow TImode in all VSX registers if the user
     asked for it.  */
  if (TARGET_VSX && VSX_REGNO_P (regno)
      && (VECTOR_MEM_VSX_P (mode)
	  || VECTOR_ALIGNMENT_P (mode)
	  || reg_addr[mode].scalar_in_vmx_p
	  || mode == TImode
	  || (TARGET_VADDUQM && mode == V1TImode)))
    {
      if (FP_REGNO_P (regno))
	return FP_REGNO_P (last_regno);

      if (ALTIVEC_REGNO_P (regno))
	{
	  if (GET_MODE_SIZE (mode) < 16 && !reg_addr[mode].scalar_in_vmx_p)
	    return 0;

	  return ALTIVEC_REGNO_P (last_regno);
	}
    }

  /* The GPRs can hold any mode, but values bigger than one register
     cannot go past R31.  */
  if (INT_REGNO_P (regno))
    return INT_REGNO_P (last_regno);

  /* The float registers (except for VSX vector modes) can only hold floating
     modes and DImode.  */
  if (FP_REGNO_P (regno))
    {
      if (VECTOR_ALIGNMENT_P (mode))
	return false;

      if (SCALAR_FLOAT_MODE_P (mode)
	  && (mode != TDmode || (regno % 2) == 0)
	  && FP_REGNO_P (last_regno))
	return 1;

      if (GET_MODE_CLASS (mode) == MODE_INT)
	{
	  if(GET_MODE_SIZE (mode) == UNITS_PER_FP_WORD)
	    return 1;

	  if (TARGET_P8_VECTOR && (mode == SImode))
	    return 1;

	  if (TARGET_P9_VECTOR && (mode == QImode || mode == HImode))
	    return 1;
	}

      return 0;
    }

  /* The CR register can only hold CC modes.  */
  if (CR_REGNO_P (regno))
    return GET_MODE_CLASS (mode) == MODE_CC;

  if (CA_REGNO_P (regno))
    return mode == Pmode || mode == SImode;

  /* AltiVec only in AldyVec registers.  */
  if (ALTIVEC_REGNO_P (regno))
    return (VECTOR_MEM_ALTIVEC_OR_VSX_P (mode)
	    || mode == V1TImode);

  /* We cannot put non-VSX TImode or PTImode anywhere except general register
     and it must be able to fit within the register set.  */

  return GET_MODE_SIZE (mode) <= UNITS_PER_WORD;
}

/* Implement TARGET_HARD_REGNO_NREGS.  */

static unsigned int
rs6000_hard_regno_nregs_hook (unsigned int regno, machine_mode mode)
{
  return rs6000_hard_regno_nregs[mode][regno];
}

/* Implement TARGET_HARD_REGNO_MODE_OK.  */

static bool
rs6000_hard_regno_mode_ok (unsigned int regno, machine_mode mode)
{
  return rs6000_hard_regno_mode_ok_p[mode][regno];
}

/* Implement TARGET_MODES_TIEABLE_P.

   PTImode cannot tie with other modes because PTImode is restricted to even
   GPR registers, and TImode can go in any GPR as well as VSX registers (PR
   57744).

   Similarly, don't allow POImode (vector pair, restricted to even VSX
   registers) or PXImode (vector quad, restricted to FPR registers divisible
   by 4) to tie with other modes.

   Altivec/VSX vector tests were moved ahead of scalar float mode, so that IEEE
   128-bit floating point on VSX systems ties with other vectors.  */

static bool
rs6000_modes_tieable_p (machine_mode mode1, machine_mode mode2)
{
  if (mode1 == PTImode || mode1 == POImode || mode1 == PXImode
      || mode2 == PTImode || mode2 == POImode || mode2 == PXImode)
    return mode1 == mode2;

  if (ALTIVEC_OR_VSX_VECTOR_MODE (mode1))
    return ALTIVEC_OR_VSX_VECTOR_MODE (mode2);
  if (ALTIVEC_OR_VSX_VECTOR_MODE (mode2))
    return false;

  if (SCALAR_FLOAT_MODE_P (mode1))
    return SCALAR_FLOAT_MODE_P (mode2);
  if (SCALAR_FLOAT_MODE_P (mode2))
    return false;

  if (GET_MODE_CLASS (mode1) == MODE_CC)
    return GET_MODE_CLASS (mode2) == MODE_CC;
  if (GET_MODE_CLASS (mode2) == MODE_CC)
    return false;

  return true;
}

/* Implement TARGET_HARD_REGNO_CALL_PART_CLOBBERED.  */

static bool
rs6000_hard_regno_call_part_clobbered (unsigned int, unsigned int regno,
				       machine_mode mode)
{
  if (TARGET_32BIT
      && TARGET_POWERPC64
      && GET_MODE_SIZE (mode) > 4
      && INT_REGNO_P (regno))
    return true;

  if (TARGET_VSX
      && FP_REGNO_P (regno)
      && GET_MODE_SIZE (mode) > 8
      && !FLOAT128_2REG_P (mode))
    return true;

  return false;
}

/* Print interesting facts about registers.  */
static void
rs6000_debug_reg_print (int first_regno, int last_regno, const char *reg_name)
{
  int r, m;

  for (r = first_regno; r <= last_regno; ++r)
    {
      const char *comma = "";
      int len;

      if (first_regno == last_regno)
	fprintf (stderr, "%s:\t", reg_name);
      else
	fprintf (stderr, "%s%d:\t", reg_name, r - first_regno);

      len = 8;
      for (m = 0; m < NUM_MACHINE_MODES; ++m)
	if (rs6000_hard_regno_mode_ok_p[m][r] && rs6000_hard_regno_nregs[m][r])
	  {
	    if (len > 70)
	      {
		fprintf (stderr, ",\n\t");
		len = 8;
		comma = "";
	      }

	    if (rs6000_hard_regno_nregs[m][r] > 1)
	      len += fprintf (stderr, "%s%s/%d", comma, GET_MODE_NAME (m),
			     rs6000_hard_regno_nregs[m][r]);
	    else
	      len += fprintf (stderr, "%s%s", comma, GET_MODE_NAME (m));

	    comma = ", ";
	  }

      if (call_used_or_fixed_reg_p (r))
	{
	  if (len > 70)
	    {
	      fprintf (stderr, ",\n\t");
	      len = 8;
	      comma = "";
	    }

	  len += fprintf (stderr, "%s%s", comma, "call-used");
	  comma = ", ";
	}

      if (fixed_regs[r])
	{
	  if (len > 70)
	    {
	      fprintf (stderr, ",\n\t");
	      len = 8;
	      comma = "";
	    }

	  len += fprintf (stderr, "%s%s", comma, "fixed");
	  comma = ", ";
	}

      if (len > 70)
	{
	  fprintf (stderr, ",\n\t");
	  comma = "";
	}

      len += fprintf (stderr, "%sreg-class = %s", comma,
		      reg_class_names[(int)rs6000_regno_regclass[r]]);
      comma = ", ";

      if (len > 70)
	{
	  fprintf (stderr, ",\n\t");
	  comma = "";
	}

      fprintf (stderr, "%sregno = %d\n", comma, r);
    }
}

static const char *
rs6000_debug_vector_unit (enum rs6000_vector v)
{
  const char *ret;

  switch (v)
    {
    case VECTOR_NONE:	   ret = "none";      break;
    case VECTOR_ALTIVEC:   ret = "altivec";   break;
    case VECTOR_VSX:	   ret = "vsx";       break;
    case VECTOR_P8_VECTOR: ret = "p8_vector"; break;
    default:		   ret = "unknown";   break;
    }

  return ret;
}

/* Inner function printing just the address mask for a particular reload
   register class.  */
DEBUG_FUNCTION char *
rs6000_debug_addr_mask (addr_mask_type mask, bool keep_spaces)
{
  static char ret[8];
  char *p = ret;

  if ((mask & RELOAD_REG_VALID) != 0)
    *p++ = 'v';
  else if (keep_spaces)
    *p++ = ' ';

  if ((mask & RELOAD_REG_MULTIPLE) != 0)
    *p++ = 'm';
  else if (keep_spaces)
    *p++ = ' ';

  if ((mask & RELOAD_REG_INDEXED) != 0)
    *p++ = 'i';
  else if (keep_spaces)
    *p++ = ' ';

  if ((mask & RELOAD_REG_QUAD_OFFSET) != 0)
    *p++ = 'O';
  else if ((mask & RELOAD_REG_OFFSET) != 0)
    *p++ = 'o';
  else if (keep_spaces)
    *p++ = ' ';

  if ((mask & RELOAD_REG_PRE_INCDEC) != 0)
    *p++ = '+';
  else if (keep_spaces)
    *p++ = ' ';

  if ((mask & RELOAD_REG_PRE_MODIFY) != 0)
    *p++ = '+';
  else if (keep_spaces)
    *p++ = ' ';

  if ((mask & RELOAD_REG_AND_M16) != 0)
    *p++ = '&';
  else if (keep_spaces)
    *p++ = ' ';

  *p = '\0';

  return ret;
}

/* Print the address masks in a human readble fashion.  */
DEBUG_FUNCTION void
rs6000_debug_print_mode (ssize_t m)
{
  ssize_t rc;
  int spaces = 0;

  fprintf (stderr, "Mode: %-5s", GET_MODE_NAME (m));
  for (rc = 0; rc < N_RELOAD_REG; rc++)
    fprintf (stderr, " %s: %s", reload_reg_map[rc].name,
	     rs6000_debug_addr_mask (reg_addr[m].addr_mask[rc], true));

  if ((reg_addr[m].reload_store != CODE_FOR_nothing)
      || (reg_addr[m].reload_load != CODE_FOR_nothing))
    {
      fprintf (stderr, "%*s  Reload=%c%c", spaces, "",
	       (reg_addr[m].reload_store != CODE_FOR_nothing) ? 's' : '*',
	       (reg_addr[m].reload_load != CODE_FOR_nothing) ? 'l' : '*');
      spaces = 0;
    }
  else
    spaces += strlen ("  Reload=sl");

  if (reg_addr[m].scalar_in_vmx_p)
    {
      fprintf (stderr, "%*s  Upper=y", spaces, "");
      spaces = 0;
    }
  else
    spaces += strlen ("  Upper=y");

  if (rs6000_vector_unit[m] != VECTOR_NONE
      || rs6000_vector_mem[m] != VECTOR_NONE)
    {
      fprintf (stderr, "%*s  vector: arith=%-10s mem=%s",
	       spaces, "",
	       rs6000_debug_vector_unit (rs6000_vector_unit[m]),
	       rs6000_debug_vector_unit (rs6000_vector_mem[m]));
    }

  fputs ("\n", stderr);
}

#define DEBUG_FMT_ID "%-32s= "
#define DEBUG_FMT_D   DEBUG_FMT_ID "%d\n"
#define DEBUG_FMT_WX  DEBUG_FMT_ID "%#.12" HOST_WIDE_INT_PRINT "x: "
#define DEBUG_FMT_S   DEBUG_FMT_ID "%s\n"

/* Print various interesting information with -mdebug=reg.  */
static void
rs6000_debug_reg_global (void)
{
  static const char *const tf[2] = { "false", "true" };
  const char *nl = (const char *)0;
  int m;
  size_t m1, m2, v;
  char costly_num[20];
  char nop_num[20];
  char flags_buffer[40];
  const char *costly_str;
  const char *nop_str;
  const char *trace_str;
  const char *abi_str;
  const char *cmodel_str;
  struct cl_target_option cl_opts;

  /* Modes we want tieable information on.  */
  static const machine_mode print_tieable_modes[] = {
    QImode,
    HImode,
    SImode,
    DImode,
    TImode,
    PTImode,
    SFmode,
    DFmode,
    TFmode,
    IFmode,
    KFmode,
    SDmode,
    DDmode,
    TDmode,
    V2SImode,
    V2SFmode,
    V16QImode,
    V8HImode,
    V4SImode,
    V2DImode,
    V1TImode,
    V32QImode,
    V16HImode,
    V8SImode,
    V4DImode,
    V2TImode,
    V4SFmode,
    V2DFmode,
    V8SFmode,
    V4DFmode,
    OImode,
    XImode,
    POImode,
    PXImode,
    CCmode,
    CCUNSmode,
    CCEQmode,
    CCFPmode,
  };

  /* Virtual regs we are interested in.  */
  const static struct {
    int regno;			/* register number.  */
    const char *name;		/* register name.  */
  } virtual_regs[] = {
    { STACK_POINTER_REGNUM,			"stack pointer:" },
    { TOC_REGNUM,				"toc:          " },
    { STATIC_CHAIN_REGNUM,			"static chain: " },
    { RS6000_PIC_OFFSET_TABLE_REGNUM,		"pic offset:   " },
    { HARD_FRAME_POINTER_REGNUM,		"hard frame:   " },
    { ARG_POINTER_REGNUM,			"arg pointer:  " },
    { FRAME_POINTER_REGNUM,			"frame pointer:" },
    { FIRST_PSEUDO_REGISTER,			"first pseudo: " },
    { FIRST_VIRTUAL_REGISTER,			"first virtual:" },
    { VIRTUAL_INCOMING_ARGS_REGNUM,		"incoming_args:" },
    { VIRTUAL_STACK_VARS_REGNUM,		"stack_vars:   " },
    { VIRTUAL_STACK_DYNAMIC_REGNUM,		"stack_dynamic:" },
    { VIRTUAL_OUTGOING_ARGS_REGNUM,		"outgoing_args:" },
    { VIRTUAL_CFA_REGNUM,			"cfa (frame):  " },
    { VIRTUAL_PREFERRED_STACK_BOUNDARY_REGNUM,	"stack boundry:" },
    { LAST_VIRTUAL_REGISTER,			"last virtual: " },
  };

  fputs ("\nHard register information:\n", stderr);
  rs6000_debug_reg_print (FIRST_GPR_REGNO, LAST_GPR_REGNO, "gr");
  rs6000_debug_reg_print (FIRST_FPR_REGNO, LAST_FPR_REGNO, "fp");
  rs6000_debug_reg_print (FIRST_ALTIVEC_REGNO,
			  LAST_ALTIVEC_REGNO,
			  "vs");
  rs6000_debug_reg_print (LR_REGNO, LR_REGNO, "lr");
  rs6000_debug_reg_print (CTR_REGNO, CTR_REGNO, "ctr");
  rs6000_debug_reg_print (CR0_REGNO, CR7_REGNO, "cr");
  rs6000_debug_reg_print (CA_REGNO, CA_REGNO, "ca");
  rs6000_debug_reg_print (VRSAVE_REGNO, VRSAVE_REGNO, "vrsave");
  rs6000_debug_reg_print (VSCR_REGNO, VSCR_REGNO, "vscr");

  fputs ("\nVirtual/stack/frame registers:\n", stderr);
  for (v = 0; v < ARRAY_SIZE (virtual_regs); v++)
    fprintf (stderr, "%s regno = %3d\n", virtual_regs[v].name, virtual_regs[v].regno);

  fprintf (stderr,
	   "\n"
	   "d  reg_class = %s\n"
	   "f  reg_class = %s\n"
	   "v  reg_class = %s\n"
	   "wa reg_class = %s\n"
	   "we reg_class = %s\n"
	   "wr reg_class = %s\n"
	   "wx reg_class = %s\n"
	   "wA reg_class = %s\n"
	   "\n",
	   reg_class_names[rs6000_constraints[RS6000_CONSTRAINT_d]],
	   reg_class_names[rs6000_constraints[RS6000_CONSTRAINT_f]],
	   reg_class_names[rs6000_constraints[RS6000_CONSTRAINT_v]],
	   reg_class_names[rs6000_constraints[RS6000_CONSTRAINT_wa]],
	   reg_class_names[rs6000_constraints[RS6000_CONSTRAINT_we]],
	   reg_class_names[rs6000_constraints[RS6000_CONSTRAINT_wr]],
	   reg_class_names[rs6000_constraints[RS6000_CONSTRAINT_wx]],
	   reg_class_names[rs6000_constraints[RS6000_CONSTRAINT_wA]]);

  nl = "\n";
  for (m = 0; m < NUM_MACHINE_MODES; ++m)
    rs6000_debug_print_mode (m);

  fputs ("\n", stderr);

  for (m1 = 0; m1 < ARRAY_SIZE (print_tieable_modes); m1++)
    {
      machine_mode mode1 = print_tieable_modes[m1];
      bool first_time = true;

      nl = (const char *)0;
      for (m2 = 0; m2 < ARRAY_SIZE (print_tieable_modes); m2++)
	{
	  machine_mode mode2 = print_tieable_modes[m2];
	  if (mode1 != mode2 && rs6000_modes_tieable_p (mode1, mode2))
	    {
	      if (first_time)
		{
		  fprintf (stderr, "Tieable modes %s:", GET_MODE_NAME (mode1));
		  nl = "\n";
		  first_time = false;
		}

	      fprintf (stderr, " %s", GET_MODE_NAME (mode2));
	    }
	}

      if (!first_time)
	fputs ("\n", stderr);
    }

  if (nl)
    fputs (nl, stderr);

  if (rs6000_recip_control)
    {
      fprintf (stderr, "\nReciprocal mask = 0x%x\n", rs6000_recip_control);

      for (m = 0; m < NUM_MACHINE_MODES; ++m)
	if (rs6000_recip_bits[m])
	  {
	    fprintf (stderr,
		     "Reciprocal estimate mode: %-5s divide: %s rsqrt: %s\n",
		     GET_MODE_NAME (m),
		     (RS6000_RECIP_AUTO_RE_P (m)
		      ? "auto"
		      : (RS6000_RECIP_HAVE_RE_P (m) ? "have" : "none")),
		     (RS6000_RECIP_AUTO_RSQRTE_P (m)
		      ? "auto"
		      : (RS6000_RECIP_HAVE_RSQRTE_P (m) ? "have" : "none")));
	  }

      fputs ("\n", stderr);
    }

  if (rs6000_cpu_index >= 0)
    {
      const char *name = processor_target_table[rs6000_cpu_index].name;
      HOST_WIDE_INT flags
	= processor_target_table[rs6000_cpu_index].target_enable;

      sprintf (flags_buffer, "-mcpu=%s flags", name);
      rs6000_print_isa_options (stderr, 0, flags_buffer, flags);
    }
  else
    fprintf (stderr, DEBUG_FMT_S, "cpu", "<none>");

  if (rs6000_tune_index >= 0)
    {
      const char *name = processor_target_table[rs6000_tune_index].name;
      HOST_WIDE_INT flags
	= processor_target_table[rs6000_tune_index].target_enable;

      sprintf (flags_buffer, "-mtune=%s flags", name);
      rs6000_print_isa_options (stderr, 0, flags_buffer, flags);
    }
  else
    fprintf (stderr, DEBUG_FMT_S, "tune", "<none>");

  cl_target_option_save (&cl_opts, &global_options);
  rs6000_print_isa_options (stderr, 0, "rs6000_isa_flags",
			    rs6000_isa_flags);

  rs6000_print_isa_options (stderr, 0, "rs6000_isa_flags_explicit",
			    rs6000_isa_flags_explicit);

  rs6000_print_builtin_options (stderr, 0, "rs6000_builtin_mask",
				rs6000_builtin_mask);

  rs6000_print_isa_options (stderr, 0, "TARGET_DEFAULT", TARGET_DEFAULT);

  fprintf (stderr, DEBUG_FMT_S, "--with-cpu default",
	   OPTION_TARGET_CPU_DEFAULT ? OPTION_TARGET_CPU_DEFAULT : "<none>");

  switch (rs6000_sched_costly_dep)
    {
    case max_dep_latency:
      costly_str = "max_dep_latency";
      break;

    case no_dep_costly:
      costly_str = "no_dep_costly";
      break;

    case all_deps_costly:
      costly_str = "all_deps_costly";
      break;

    case true_store_to_load_dep_costly:
      costly_str = "true_store_to_load_dep_costly";
      break;

    case store_to_load_dep_costly:
      costly_str = "store_to_load_dep_costly";
      break;

    default:
      costly_str = costly_num;
      sprintf (costly_num, "%d", (int)rs6000_sched_costly_dep);
      break;
    }

  fprintf (stderr, DEBUG_FMT_S, "sched_costly_dep", costly_str);

  switch (rs6000_sched_insert_nops)
    {
    case sched_finish_regroup_exact:
      nop_str = "sched_finish_regroup_exact";
      break;

    case sched_finish_pad_groups:
      nop_str = "sched_finish_pad_groups";
      break;

    case sched_finish_none:
      nop_str = "sched_finish_none";
      break;

    default:
      nop_str = nop_num;
      sprintf (nop_num, "%d", (int)rs6000_sched_insert_nops);
      break;
    }

  fprintf (stderr, DEBUG_FMT_S, "sched_insert_nops", nop_str);

  switch (rs6000_sdata)
    {
    default:
    case SDATA_NONE:
      break;

    case SDATA_DATA:
      fprintf (stderr, DEBUG_FMT_S, "sdata", "data");
      break;

    case SDATA_SYSV:
      fprintf (stderr, DEBUG_FMT_S, "sdata", "sysv");
      break;

    case SDATA_EABI:
      fprintf (stderr, DEBUG_FMT_S, "sdata", "eabi");
      break;

    }

  switch (rs6000_traceback)
    {
    case traceback_default:	trace_str = "default";	break;
    case traceback_none:	trace_str = "none";	break;
    case traceback_part:	trace_str = "part";	break;
    case traceback_full:	trace_str = "full";	break;
    default:			trace_str = "unknown";	break;
    }

  fprintf (stderr, DEBUG_FMT_S, "traceback", trace_str);

  switch (rs6000_current_cmodel)
    {
    case CMODEL_SMALL:	cmodel_str = "small";	break;
    case CMODEL_MEDIUM:	cmodel_str = "medium";	break;
    case CMODEL_LARGE:	cmodel_str = "large";	break;
    default:		cmodel_str = "unknown";	break;
    }

  fprintf (stderr, DEBUG_FMT_S, "cmodel", cmodel_str);

  switch (rs6000_current_abi)
    {
    case ABI_NONE:	abi_str = "none";	break;
    case ABI_AIX:	abi_str = "aix";	break;
    case ABI_ELFv2:	abi_str = "ELFv2";	break;
    case ABI_V4:	abi_str = "V4";		break;
    case ABI_DARWIN:	abi_str = "darwin";	break;
    default:		abi_str = "unknown";	break;
    }

  fprintf (stderr, DEBUG_FMT_S, "abi", abi_str);

  if (rs6000_altivec_abi)
    fprintf (stderr, DEBUG_FMT_S, "altivec_abi", "true");

  if (rs6000_aix_extabi)
    fprintf (stderr, DEBUG_FMT_S, "AIX vec-extabi", "true");

  if (rs6000_darwin64_abi)
    fprintf (stderr, DEBUG_FMT_S, "darwin64_abi", "true");

  fprintf (stderr, DEBUG_FMT_S, "soft_float",
	   (TARGET_SOFT_FLOAT ? "true" : "false"));

  if (TARGET_LINK_STACK)
    fprintf (stderr, DEBUG_FMT_S, "link_stack", "true");

  if (TARGET_P8_FUSION)
    {
      char options[80];

      strcpy (options, "power8");
      if (TARGET_P8_FUSION_SIGN)
	strcat (options, ", sign");

      fprintf (stderr, DEBUG_FMT_S, "fusion", options);
    }

  fprintf (stderr, DEBUG_FMT_S, "plt-format",
	   TARGET_SECURE_PLT ? "secure" : "bss");
  fprintf (stderr, DEBUG_FMT_S, "struct-return",
	   aix_struct_return ? "aix" : "sysv");
  fprintf (stderr, DEBUG_FMT_S, "always_hint", tf[!!rs6000_always_hint]);
  fprintf (stderr, DEBUG_FMT_S, "sched_groups", tf[!!rs6000_sched_groups]);
  fprintf (stderr, DEBUG_FMT_S, "align_branch",
	   tf[!!rs6000_align_branch_targets]);
  fprintf (stderr, DEBUG_FMT_D, "tls_size", rs6000_tls_size);
  fprintf (stderr, DEBUG_FMT_D, "long_double_size",
	   rs6000_long_double_type_size);
  if (rs6000_long_double_type_size > 64)
    {
      fprintf (stderr, DEBUG_FMT_S, "long double type",
	       TARGET_IEEEQUAD ? "IEEE" : "IBM");
      fprintf (stderr, DEBUG_FMT_S, "default long double type",
	       TARGET_IEEEQUAD_DEFAULT ? "IEEE" : "IBM");
    }
  fprintf (stderr, DEBUG_FMT_D, "sched_restricted_insns_priority",
	   (int)rs6000_sched_restricted_insns_priority);
  fprintf (stderr, DEBUG_FMT_D, "Number of standard builtins",
	   (int)END_BUILTINS);
  fprintf (stderr, DEBUG_FMT_D, "Number of rs6000 builtins",
	   (int)RS6000_BUILTIN_COUNT);

  fprintf (stderr, DEBUG_FMT_D, "Enable float128 on VSX",
	   (int)TARGET_FLOAT128_ENABLE_TYPE);

  if (TARGET_VSX)
    fprintf (stderr, DEBUG_FMT_D, "VSX easy 64-bit scalar element",
	     (int)VECTOR_ELEMENT_SCALAR_64BIT);

  if (TARGET_DIRECT_MOVE_128)
    fprintf (stderr, DEBUG_FMT_D, "VSX easy 64-bit mfvsrld element",
	     (int)VECTOR_ELEMENT_MFVSRLD_64BIT);
}


/* Update the addr mask bits in reg_addr to help secondary reload and go if
   legitimate address support to figure out the appropriate addressing to
   use.  */

static void
rs6000_setup_reg_addr_masks (void)
{
  ssize_t rc, reg, m, nregs;
  addr_mask_type any_addr_mask, addr_mask;

  for (m = 0; m < NUM_MACHINE_MODES; ++m)
    {
      machine_mode m2 = (machine_mode) m;
      bool complex_p = false;
      bool small_int_p = (m2 == QImode || m2 == HImode || m2 == SImode);
      size_t msize;

      if (COMPLEX_MODE_P (m2))
	{
	  complex_p = true;
	  m2 = GET_MODE_INNER (m2);
	}

      msize = GET_MODE_SIZE (m2);

      /* SDmode is special in that we want to access it only via REG+REG
	 addressing on power7 and above, since we want to use the LFIWZX and
	 STFIWZX instructions to load it.  */
      bool indexed_only_p = (m == SDmode && TARGET_NO_SDMODE_STACK);

      any_addr_mask = 0;
      for (rc = FIRST_RELOAD_REG_CLASS; rc <= LAST_RELOAD_REG_CLASS; rc++)
	{
	  addr_mask = 0;
	  reg = reload_reg_map[rc].reg;

	  /* Can mode values go in the GPR/FPR/Altivec registers?  */
	  if (reg >= 0 && rs6000_hard_regno_mode_ok_p[m][reg])
	    {
	      bool small_int_vsx_p = (small_int_p
				      && (rc == RELOAD_REG_FPR
					  || rc == RELOAD_REG_VMX));

	      nregs = rs6000_hard_regno_nregs[m][reg];
	      addr_mask |= RELOAD_REG_VALID;

	      /* Indicate if the mode takes more than 1 physical register.  If
		 it takes a single register, indicate it can do REG+REG
		 addressing.  Small integers in VSX registers can only do
		 REG+REG addressing.  */
	      if (small_int_vsx_p)
		addr_mask |= RELOAD_REG_INDEXED;
	      else if (nregs > 1 || m == BLKmode || complex_p)
		addr_mask |= RELOAD_REG_MULTIPLE;
	      else
		addr_mask |= RELOAD_REG_INDEXED;

	      /* Figure out if we can do PRE_INC, PRE_DEC, or PRE_MODIFY
		 addressing.  If we allow scalars into Altivec registers,
		 don't allow PRE_INC, PRE_DEC, or PRE_MODIFY.

		 For VSX systems, we don't allow update addressing for
		 DFmode/SFmode if those registers can go in both the
		 traditional floating point registers and Altivec registers.
		 The load/store instructions for the Altivec registers do not
		 have update forms.  If we allowed update addressing, it seems
		 to break IV-OPT code using floating point if the index type is
		 int instead of long (PR target/81550 and target/84042).  */

	      if (TARGET_UPDATE
		  && (rc == RELOAD_REG_GPR || rc == RELOAD_REG_FPR)
		  && msize <= 8
		  && !VECTOR_MODE_P (m2)
		  && !VECTOR_ALIGNMENT_P (m2)
		  && !complex_p
		  && (m != E_DFmode || !TARGET_VSX)
		  && (m != E_SFmode || !TARGET_P8_VECTOR)
		  && !small_int_vsx_p)
		{
		  addr_mask |= RELOAD_REG_PRE_INCDEC;

		  /* PRE_MODIFY is more restricted than PRE_INC/PRE_DEC in that
		     we don't allow PRE_MODIFY for some multi-register
		     operations.  */
		  switch (m)
		    {
		    default:
		      addr_mask |= RELOAD_REG_PRE_MODIFY;
		      break;

		    case E_DImode:
		      if (TARGET_POWERPC64)
			addr_mask |= RELOAD_REG_PRE_MODIFY;
		      break;

		    case E_DFmode:
		    case E_DDmode:
		      if (TARGET_HARD_FLOAT)
			addr_mask |= RELOAD_REG_PRE_MODIFY;
		      break;
		    }
		}
	    }

	  /* GPR and FPR registers can do REG+OFFSET addressing, except
	     possibly for SDmode.  ISA 3.0 (i.e. power9) adds D-form addressing
	     for 64-bit scalars and 32-bit SFmode to altivec registers.  */
	  if ((addr_mask != 0) && !indexed_only_p
	      && msize <= 8
	      && (rc == RELOAD_REG_GPR
		  || ((msize == 8 || m2 == SFmode)
		      && (rc == RELOAD_REG_FPR
			  || (rc == RELOAD_REG_VMX && TARGET_P9_VECTOR)))))
	    addr_mask |= RELOAD_REG_OFFSET;

	  /* VSX registers can do REG+OFFSET addresssing if ISA 3.0
	     instructions are enabled.  The offset for 128-bit VSX registers is
	     only 12-bits.  While GPRs can handle the full offset range, VSX
	     registers can only handle the restricted range.  */
	  else if ((addr_mask != 0) && !indexed_only_p
		   && msize == 16 && TARGET_P9_VECTOR
		   && (ALTIVEC_OR_VSX_VECTOR_MODE (m2)
		       || (m2 == TImode && TARGET_VSX)))
	    {
	      addr_mask |= RELOAD_REG_OFFSET;
	      if (rc == RELOAD_REG_FPR || rc == RELOAD_REG_VMX)
		addr_mask |= RELOAD_REG_QUAD_OFFSET;
	    }

	  /* Vector pairs can do both indexed and offset loads if the
	     instructions are enabled, otherwise they can only do offset loads
	     since it will be broken into two vector moves.  Vector quads can
	     only do offset loads.  */
	  else if ((addr_mask != 0) && TARGET_MMA
		   && (m2 == POImode || m2 == PXImode))
	    {
	      addr_mask |= RELOAD_REG_OFFSET;
	      if (rc == RELOAD_REG_FPR || rc == RELOAD_REG_VMX)
		{
		  addr_mask |= RELOAD_REG_QUAD_OFFSET;
		  if (m2 == POImode)
		    addr_mask |= RELOAD_REG_INDEXED;
		}
	    }

	  /* VMX registers can do (REG & -16) and ((REG+REG) & -16)
	     addressing on 128-bit types.  */
	  if (rc == RELOAD_REG_VMX && msize == 16
	      && (addr_mask & RELOAD_REG_VALID) != 0)
	    addr_mask |= RELOAD_REG_AND_M16;

	  reg_addr[m].addr_mask[rc] = addr_mask;
	  any_addr_mask |= addr_mask;
	}

      reg_addr[m].addr_mask[RELOAD_REG_ANY] = any_addr_mask;
    }
}


/* Initialize the various global tables that are based on register size.  */
static void
rs6000_init_hard_regno_mode_ok (bool global_init_p)
{
  ssize_t r, m, c;
  int align64;
  int align32;

  /* Precalculate REGNO_REG_CLASS.  */
  rs6000_regno_regclass[0] = GENERAL_REGS;
  for (r = 1; r < 32; ++r)
    rs6000_regno_regclass[r] = BASE_REGS;

  for (r = 32; r < 64; ++r)
    rs6000_regno_regclass[r] = FLOAT_REGS;

  for (r = 64; HARD_REGISTER_NUM_P (r); ++r)
    rs6000_regno_regclass[r] = NO_REGS;

  for (r = FIRST_ALTIVEC_REGNO; r <= LAST_ALTIVEC_REGNO; ++r)
    rs6000_regno_regclass[r] = ALTIVEC_REGS;

  rs6000_regno_regclass[CR0_REGNO] = CR0_REGS;
  for (r = CR1_REGNO; r <= CR7_REGNO; ++r)
    rs6000_regno_regclass[r] = CR_REGS;

  rs6000_regno_regclass[LR_REGNO] = LINK_REGS;
  rs6000_regno_regclass[CTR_REGNO] = CTR_REGS;
  rs6000_regno_regclass[CA_REGNO] = NO_REGS;
  rs6000_regno_regclass[VRSAVE_REGNO] = VRSAVE_REGS;
  rs6000_regno_regclass[VSCR_REGNO] = VRSAVE_REGS;
  rs6000_regno_regclass[ARG_POINTER_REGNUM] = BASE_REGS;
  rs6000_regno_regclass[FRAME_POINTER_REGNUM] = BASE_REGS;

  /* Precalculate register class to simpler reload register class.  We don't
     need all of the register classes that are combinations of different
     classes, just the simple ones that have constraint letters.  */
  for (c = 0; c < N_REG_CLASSES; c++)
    reg_class_to_reg_type[c] = NO_REG_TYPE;

  reg_class_to_reg_type[(int)GENERAL_REGS] = GPR_REG_TYPE;
  reg_class_to_reg_type[(int)BASE_REGS] = GPR_REG_TYPE;
  reg_class_to_reg_type[(int)VSX_REGS] = VSX_REG_TYPE;
  reg_class_to_reg_type[(int)VRSAVE_REGS] = SPR_REG_TYPE;
  reg_class_to_reg_type[(int)VSCR_REGS] = SPR_REG_TYPE;
  reg_class_to_reg_type[(int)LINK_REGS] = SPR_REG_TYPE;
  reg_class_to_reg_type[(int)CTR_REGS] = SPR_REG_TYPE;
  reg_class_to_reg_type[(int)LINK_OR_CTR_REGS] = SPR_REG_TYPE;
  reg_class_to_reg_type[(int)CR_REGS] = CR_REG_TYPE;
  reg_class_to_reg_type[(int)CR0_REGS] = CR_REG_TYPE;

  if (TARGET_VSX)
    {
      reg_class_to_reg_type[(int)FLOAT_REGS] = VSX_REG_TYPE;
      reg_class_to_reg_type[(int)ALTIVEC_REGS] = VSX_REG_TYPE;
    }
  else
    {
      reg_class_to_reg_type[(int)FLOAT_REGS] = FPR_REG_TYPE;
      reg_class_to_reg_type[(int)ALTIVEC_REGS] = ALTIVEC_REG_TYPE;
    }

  /* Precalculate the valid memory formats as well as the vector information,
     this must be set up before the rs6000_hard_regno_nregs_internal calls
     below.  */
  gcc_assert ((int)VECTOR_NONE == 0);
  memset ((void *) &rs6000_vector_unit[0], '\0', sizeof (rs6000_vector_unit));
  memset ((void *) &rs6000_vector_mem[0], '\0', sizeof (rs6000_vector_mem));

  gcc_assert ((int)CODE_FOR_nothing == 0);
  memset ((void *) &reg_addr[0], '\0', sizeof (reg_addr));

  gcc_assert ((int)NO_REGS == 0);
  memset ((void *) &rs6000_constraints[0], '\0', sizeof (rs6000_constraints));

  /* The VSX hardware allows native alignment for vectors, but control whether the compiler
     believes it can use native alignment or still uses 128-bit alignment.  */
  if (TARGET_VSX && !TARGET_VSX_ALIGN_128)
    {
      align64 = 64;
      align32 = 32;
    }
  else
    {
      align64 = 128;
      align32 = 128;
    }

  /* KF mode (IEEE 128-bit in VSX registers).  We do not have arithmetic, so
     only set the memory modes.  Include TFmode if -mabi=ieeelongdouble.  */
  if (TARGET_FLOAT128_TYPE)
    {
      rs6000_vector_mem[KFmode] = VECTOR_VSX;
      rs6000_vector_align[KFmode] = 128;

      if (FLOAT128_IEEE_P (TFmode))
	{
	  rs6000_vector_mem[TFmode] = VECTOR_VSX;
	  rs6000_vector_align[TFmode] = 128;
	}
    }

  /* V2DF mode, VSX only.  */
  if (TARGET_VSX)
    {
      rs6000_vector_unit[V2DFmode] = VECTOR_VSX;
      rs6000_vector_mem[V2DFmode] = VECTOR_VSX;
      rs6000_vector_align[V2DFmode] = align64;
    }

  /* V4SF mode, either VSX or Altivec.  */
  if (TARGET_VSX)
    {
      rs6000_vector_unit[V4SFmode] = VECTOR_VSX;
      rs6000_vector_mem[V4SFmode] = VECTOR_VSX;
      rs6000_vector_align[V4SFmode] = align32;
    }
  else if (TARGET_ALTIVEC)
    {
      rs6000_vector_unit[V4SFmode] = VECTOR_ALTIVEC;
      rs6000_vector_mem[V4SFmode] = VECTOR_ALTIVEC;
      rs6000_vector_align[V4SFmode] = align32;
    }

  /* V16QImode, V8HImode, V4SImode are Altivec only, but possibly do VSX loads
     and stores. */
  if (TARGET_ALTIVEC)
    {
      rs6000_vector_unit[V4SImode] = VECTOR_ALTIVEC;
      rs6000_vector_unit[V8HImode] = VECTOR_ALTIVEC;
      rs6000_vector_unit[V16QImode] = VECTOR_ALTIVEC;
      rs6000_vector_align[V4SImode] = align32;
      rs6000_vector_align[V8HImode] = align32;
      rs6000_vector_align[V16QImode] = align32;

      if (TARGET_VSX)
	{
	  rs6000_vector_mem[V4SImode] = VECTOR_VSX;
	  rs6000_vector_mem[V8HImode] = VECTOR_VSX;
	  rs6000_vector_mem[V16QImode] = VECTOR_VSX;
	}
      else
	{
	  rs6000_vector_mem[V4SImode] = VECTOR_ALTIVEC;
	  rs6000_vector_mem[V8HImode] = VECTOR_ALTIVEC;
	  rs6000_vector_mem[V16QImode] = VECTOR_ALTIVEC;
	}
    }

  /* V2DImode, full mode depends on ISA 2.07 vector mode.  Allow under VSX to
     do insert/splat/extract.  Altivec doesn't have 64-bit integer support.  */
  if (TARGET_VSX)
    {
      rs6000_vector_mem[V2DImode] = VECTOR_VSX;
      rs6000_vector_unit[V2DImode]
	= (TARGET_P8_VECTOR) ? VECTOR_P8_VECTOR : VECTOR_NONE;
      rs6000_vector_align[V2DImode] = align64;

      rs6000_vector_mem[V1TImode] = VECTOR_VSX;
      rs6000_vector_unit[V1TImode]
	= (TARGET_P8_VECTOR) ? VECTOR_P8_VECTOR : VECTOR_NONE;
      rs6000_vector_align[V1TImode] = 128;
    }

  /* DFmode, see if we want to use the VSX unit.  Memory is handled
     differently, so don't set rs6000_vector_mem.  */
  if (TARGET_VSX)
    {
      rs6000_vector_unit[DFmode] = VECTOR_VSX;
      rs6000_vector_align[DFmode] = 64;
    }

  /* SFmode, see if we want to use the VSX unit.  */
  if (TARGET_P8_VECTOR)
    {
      rs6000_vector_unit[SFmode] = VECTOR_VSX;
      rs6000_vector_align[SFmode] = 32;
    }

  /* Allow TImode in VSX register and set the VSX memory macros.  */
  if (TARGET_VSX)
    {
      rs6000_vector_mem[TImode] = VECTOR_VSX;
      rs6000_vector_align[TImode] = align64;
    }

  /* Add support for vector pairs and vector quad registers.  */
  if (TARGET_MMA)
    {
      rs6000_vector_unit[POImode] = VECTOR_NONE;
      rs6000_vector_mem[POImode] = VECTOR_VSX;
      rs6000_vector_align[POImode] = 256;

      rs6000_vector_unit[PXImode] = VECTOR_NONE;
      rs6000_vector_mem[PXImode] = VECTOR_VSX;
      rs6000_vector_align[PXImode] = 512;
    }

  /* Register class constraints for the constraints that depend on compile
     switches. When the VSX code was added, different constraints were added
     based on the type (DFmode, V2DFmode, V4SFmode).  For the vector types, all
     of the VSX registers are used.  The register classes for scalar floating
     point types is set, based on whether we allow that type into the upper
     (Altivec) registers.  GCC has register classes to target the Altivec
     registers for load/store operations, to select using a VSX memory
     operation instead of the traditional floating point operation.  The
     constraints are:

	d  - Register class to use with traditional DFmode instructions.
	f  - Register class to use with traditional SFmode instructions.
	v  - Altivec register.
	wa - Any VSX register.
	wc - Reserved to represent individual CR bits (used in LLVM).
	wn - always NO_REGS.
	wr - GPR if 64-bit mode is permitted.
	wx - Float register if we can do 32-bit int stores.  */

  if (TARGET_HARD_FLOAT)
    {
      rs6000_constraints[RS6000_CONSTRAINT_f] = FLOAT_REGS;	/* SFmode  */
      rs6000_constraints[RS6000_CONSTRAINT_d] = FLOAT_REGS;	/* DFmode  */
    }

  if (TARGET_VSX)
    rs6000_constraints[RS6000_CONSTRAINT_wa] = VSX_REGS;

  /* Add conditional constraints based on various options, to allow us to
     collapse multiple insn patterns.  */
  if (TARGET_ALTIVEC)
    rs6000_constraints[RS6000_CONSTRAINT_v] = ALTIVEC_REGS;

  if (TARGET_POWERPC64)
    {
      rs6000_constraints[RS6000_CONSTRAINT_wr] = GENERAL_REGS;
      rs6000_constraints[RS6000_CONSTRAINT_wA] = BASE_REGS;
    }

  if (TARGET_STFIWX)
    rs6000_constraints[RS6000_CONSTRAINT_wx] = FLOAT_REGS;	/* DImode  */

  /* Support for new direct moves (ISA 3.0 + 64bit).  */
  if (TARGET_DIRECT_MOVE_128)
    rs6000_constraints[RS6000_CONSTRAINT_we] = VSX_REGS;

  /* Set up the reload helper and direct move functions.  */
  if (TARGET_VSX || TARGET_ALTIVEC)
    {
      if (TARGET_64BIT)
	{
	  reg_addr[V16QImode].reload_store = CODE_FOR_reload_v16qi_di_store;
	  reg_addr[V16QImode].reload_load  = CODE_FOR_reload_v16qi_di_load;
	  reg_addr[V8HImode].reload_store  = CODE_FOR_reload_v8hi_di_store;
	  reg_addr[V8HImode].reload_load   = CODE_FOR_reload_v8hi_di_load;
	  reg_addr[V4SImode].reload_store  = CODE_FOR_reload_v4si_di_store;
	  reg_addr[V4SImode].reload_load   = CODE_FOR_reload_v4si_di_load;
	  reg_addr[V2DImode].reload_store  = CODE_FOR_reload_v2di_di_store;
	  reg_addr[V2DImode].reload_load   = CODE_FOR_reload_v2di_di_load;
	  reg_addr[V1TImode].reload_store  = CODE_FOR_reload_v1ti_di_store;
	  reg_addr[V1TImode].reload_load   = CODE_FOR_reload_v1ti_di_load;
	  reg_addr[V4SFmode].reload_store  = CODE_FOR_reload_v4sf_di_store;
	  reg_addr[V4SFmode].reload_load   = CODE_FOR_reload_v4sf_di_load;
	  reg_addr[V2DFmode].reload_store  = CODE_FOR_reload_v2df_di_store;
	  reg_addr[V2DFmode].reload_load   = CODE_FOR_reload_v2df_di_load;
	  reg_addr[DFmode].reload_store    = CODE_FOR_reload_df_di_store;
	  reg_addr[DFmode].reload_load     = CODE_FOR_reload_df_di_load;
	  reg_addr[DDmode].reload_store    = CODE_FOR_reload_dd_di_store;
	  reg_addr[DDmode].reload_load     = CODE_FOR_reload_dd_di_load;
	  reg_addr[SFmode].reload_store    = CODE_FOR_reload_sf_di_store;
	  reg_addr[SFmode].reload_load     = CODE_FOR_reload_sf_di_load;

	  if (FLOAT128_VECTOR_P (KFmode))
	    {
	      reg_addr[KFmode].reload_store = CODE_FOR_reload_kf_di_store;
	      reg_addr[KFmode].reload_load  = CODE_FOR_reload_kf_di_load;
	    }

	  if (FLOAT128_VECTOR_P (TFmode))
	    {
	      reg_addr[TFmode].reload_store = CODE_FOR_reload_tf_di_store;
	      reg_addr[TFmode].reload_load  = CODE_FOR_reload_tf_di_load;
	    }

	  /* Only provide a reload handler for SDmode if lfiwzx/stfiwx are
	     available.  */
	  if (TARGET_NO_SDMODE_STACK)
	    {
	      reg_addr[SDmode].reload_store = CODE_FOR_reload_sd_di_store;
	      reg_addr[SDmode].reload_load  = CODE_FOR_reload_sd_di_load;
	    }

	  if (TARGET_VSX)
	    {
	      reg_addr[TImode].reload_store  = CODE_FOR_reload_ti_di_store;
	      reg_addr[TImode].reload_load   = CODE_FOR_reload_ti_di_load;
	    }

	  if (TARGET_DIRECT_MOVE && !TARGET_DIRECT_MOVE_128)
	    {
	      reg_addr[TImode].reload_gpr_vsx    = CODE_FOR_reload_gpr_from_vsxti;
	      reg_addr[V1TImode].reload_gpr_vsx  = CODE_FOR_reload_gpr_from_vsxv1ti;
	      reg_addr[V2DFmode].reload_gpr_vsx  = CODE_FOR_reload_gpr_from_vsxv2df;
	      reg_addr[V2DImode].reload_gpr_vsx  = CODE_FOR_reload_gpr_from_vsxv2di;
	      reg_addr[V4SFmode].reload_gpr_vsx  = CODE_FOR_reload_gpr_from_vsxv4sf;
	      reg_addr[V4SImode].reload_gpr_vsx  = CODE_FOR_reload_gpr_from_vsxv4si;
	      reg_addr[V8HImode].reload_gpr_vsx  = CODE_FOR_reload_gpr_from_vsxv8hi;
	      reg_addr[V16QImode].reload_gpr_vsx = CODE_FOR_reload_gpr_from_vsxv16qi;
	      reg_addr[SFmode].reload_gpr_vsx    = CODE_FOR_reload_gpr_from_vsxsf;

	      reg_addr[TImode].reload_vsx_gpr    = CODE_FOR_reload_vsx_from_gprti;
	      reg_addr[V1TImode].reload_vsx_gpr  = CODE_FOR_reload_vsx_from_gprv1ti;
	      reg_addr[V2DFmode].reload_vsx_gpr  = CODE_FOR_reload_vsx_from_gprv2df;
	      reg_addr[V2DImode].reload_vsx_gpr  = CODE_FOR_reload_vsx_from_gprv2di;
	      reg_addr[V4SFmode].reload_vsx_gpr  = CODE_FOR_reload_vsx_from_gprv4sf;
	      reg_addr[V4SImode].reload_vsx_gpr  = CODE_FOR_reload_vsx_from_gprv4si;
	      reg_addr[V8HImode].reload_vsx_gpr  = CODE_FOR_reload_vsx_from_gprv8hi;
	      reg_addr[V16QImode].reload_vsx_gpr = CODE_FOR_reload_vsx_from_gprv16qi;
	      reg_addr[SFmode].reload_vsx_gpr    = CODE_FOR_reload_vsx_from_gprsf;

	      if (FLOAT128_VECTOR_P (KFmode))
		{
		  reg_addr[KFmode].reload_gpr_vsx = CODE_FOR_reload_gpr_from_vsxkf;
		  reg_addr[KFmode].reload_vsx_gpr = CODE_FOR_reload_vsx_from_gprkf;
		}

	      if (FLOAT128_VECTOR_P (TFmode))
		{
		  reg_addr[TFmode].reload_gpr_vsx = CODE_FOR_reload_gpr_from_vsxtf;
		  reg_addr[TFmode].reload_vsx_gpr = CODE_FOR_reload_vsx_from_gprtf;
		}

	      if (TARGET_MMA)
		{
		  reg_addr[POImode].reload_store = CODE_FOR_reload_poi_di_store;
		  reg_addr[POImode].reload_load = CODE_FOR_reload_poi_di_load;
		  reg_addr[PXImode].reload_store = CODE_FOR_reload_pxi_di_store;
		  reg_addr[PXImode].reload_load = CODE_FOR_reload_pxi_di_load;
		}
	    }
	}
      else
	{
	  reg_addr[V16QImode].reload_store = CODE_FOR_reload_v16qi_si_store;
	  reg_addr[V16QImode].reload_load  = CODE_FOR_reload_v16qi_si_load;
	  reg_addr[V8HImode].reload_store  = CODE_FOR_reload_v8hi_si_store;
	  reg_addr[V8HImode].reload_load   = CODE_FOR_reload_v8hi_si_load;
	  reg_addr[V4SImode].reload_store  = CODE_FOR_reload_v4si_si_store;
	  reg_addr[V4SImode].reload_load   = CODE_FOR_reload_v4si_si_load;
	  reg_addr[V2DImode].reload_store  = CODE_FOR_reload_v2di_si_store;
	  reg_addr[V2DImode].reload_load   = CODE_FOR_reload_v2di_si_load;
	  reg_addr[V1TImode].reload_store  = CODE_FOR_reload_v1ti_si_store;
	  reg_addr[V1TImode].reload_load   = CODE_FOR_reload_v1ti_si_load;
	  reg_addr[V4SFmode].reload_store  = CODE_FOR_reload_v4sf_si_store;
	  reg_addr[V4SFmode].reload_load   = CODE_FOR_reload_v4sf_si_load;
	  reg_addr[V2DFmode].reload_store  = CODE_FOR_reload_v2df_si_store;
	  reg_addr[V2DFmode].reload_load   = CODE_FOR_reload_v2df_si_load;
	  reg_addr[DFmode].reload_store    = CODE_FOR_reload_df_si_store;
	  reg_addr[DFmode].reload_load     = CODE_FOR_reload_df_si_load;
	  reg_addr[DDmode].reload_store    = CODE_FOR_reload_dd_si_store;
	  reg_addr[DDmode].reload_load     = CODE_FOR_reload_dd_si_load;
	  reg_addr[SFmode].reload_store    = CODE_FOR_reload_sf_si_store;
	  reg_addr[SFmode].reload_load     = CODE_FOR_reload_sf_si_load;

	  if (FLOAT128_VECTOR_P (KFmode))
	    {
	      reg_addr[KFmode].reload_store = CODE_FOR_reload_kf_si_store;
	      reg_addr[KFmode].reload_load  = CODE_FOR_reload_kf_si_load;
	    }

	  if (FLOAT128_IEEE_P (TFmode))
	    {
	      reg_addr[TFmode].reload_store = CODE_FOR_reload_tf_si_store;
	      reg_addr[TFmode].reload_load  = CODE_FOR_reload_tf_si_load;
	    }

	  /* Only provide a reload handler for SDmode if lfiwzx/stfiwx are
	     available.  */
	  if (TARGET_NO_SDMODE_STACK)
	    {
	      reg_addr[SDmode].reload_store = CODE_FOR_reload_sd_si_store;
	      reg_addr[SDmode].reload_load  = CODE_FOR_reload_sd_si_load;
	    }

	  if (TARGET_VSX)
	    {
	      reg_addr[TImode].reload_store  = CODE_FOR_reload_ti_si_store;
	      reg_addr[TImode].reload_load   = CODE_FOR_reload_ti_si_load;
	    }

	  if (TARGET_DIRECT_MOVE)
	    {
	      reg_addr[DImode].reload_fpr_gpr = CODE_FOR_reload_fpr_from_gprdi;
	      reg_addr[DDmode].reload_fpr_gpr = CODE_FOR_reload_fpr_from_gprdd;
	      reg_addr[DFmode].reload_fpr_gpr = CODE_FOR_reload_fpr_from_gprdf;
	    }
	}

      reg_addr[DFmode].scalar_in_vmx_p = true;
      reg_addr[DImode].scalar_in_vmx_p = true;

      if (TARGET_P8_VECTOR)
	{
	  reg_addr[SFmode].scalar_in_vmx_p = true;
	  reg_addr[SImode].scalar_in_vmx_p = true;

	  if (TARGET_P9_VECTOR)
	    {
	      reg_addr[HImode].scalar_in_vmx_p = true;
	      reg_addr[QImode].scalar_in_vmx_p = true;
	    }
	}
    }

  /* Precalculate HARD_REGNO_NREGS.  */
  for (r = 0; HARD_REGISTER_NUM_P (r); ++r)
    for (m = 0; m < NUM_MACHINE_MODES; ++m)
      rs6000_hard_regno_nregs[m][r]
	= rs6000_hard_regno_nregs_internal (r, (machine_mode) m);

  /* Precalculate TARGET_HARD_REGNO_MODE_OK.  */
  for (r = 0; HARD_REGISTER_NUM_P (r); ++r)
    for (m = 0; m < NUM_MACHINE_MODES; ++m)
      rs6000_hard_regno_mode_ok_p[m][r]
	= rs6000_hard_regno_mode_ok_uncached (r, (machine_mode) m);

  /* Precalculate CLASS_MAX_NREGS sizes.  */
  for (c = 0; c < LIM_REG_CLASSES; ++c)
    {
      int reg_size;

      if (TARGET_VSX && VSX_REG_CLASS_P (c))
	reg_size = UNITS_PER_VSX_WORD;

      else if (c == ALTIVEC_REGS)
	reg_size = UNITS_PER_ALTIVEC_WORD;

      else if (c == FLOAT_REGS)
	reg_size = UNITS_PER_FP_WORD;

      else
	reg_size = UNITS_PER_WORD;

      for (m = 0; m < NUM_MACHINE_MODES; ++m)
	{
	  machine_mode m2 = (machine_mode)m;
	  int reg_size2 = reg_size;

	  /* TDmode & IBM 128-bit floating point always takes 2 registers, even
	     in VSX.  */
	  if (TARGET_VSX && VSX_REG_CLASS_P (c) && FLOAT128_2REG_P (m))
	    reg_size2 = UNITS_PER_FP_WORD;

	  rs6000_class_max_nregs[m][c]
	    = (GET_MODE_SIZE (m2) + reg_size2 - 1) / reg_size2;
	}
    }

  /* Calculate which modes to automatically generate code to use a the
     reciprocal divide and square root instructions.  In the future, possibly
     automatically generate the instructions even if the user did not specify
     -mrecip.  The older machines double precision reciprocal sqrt estimate is
     not accurate enough.  */
  memset (rs6000_recip_bits, 0, sizeof (rs6000_recip_bits));
  if (TARGET_FRES)
    rs6000_recip_bits[SFmode] = RS6000_RECIP_MASK_HAVE_RE;
  if (TARGET_FRE)
    rs6000_recip_bits[DFmode] = RS6000_RECIP_MASK_HAVE_RE;
  if (VECTOR_UNIT_ALTIVEC_OR_VSX_P (V4SFmode))
    rs6000_recip_bits[V4SFmode] = RS6000_RECIP_MASK_HAVE_RE;
  if (VECTOR_UNIT_VSX_P (V2DFmode))
    rs6000_recip_bits[V2DFmode] = RS6000_RECIP_MASK_HAVE_RE;

  if (TARGET_FRSQRTES)
    rs6000_recip_bits[SFmode] |= RS6000_RECIP_MASK_HAVE_RSQRTE;
  if (TARGET_FRSQRTE)
    rs6000_recip_bits[DFmode] |= RS6000_RECIP_MASK_HAVE_RSQRTE;
  if (VECTOR_UNIT_ALTIVEC_OR_VSX_P (V4SFmode))
    rs6000_recip_bits[V4SFmode] |= RS6000_RECIP_MASK_HAVE_RSQRTE;
  if (VECTOR_UNIT_VSX_P (V2DFmode))
    rs6000_recip_bits[V2DFmode] |= RS6000_RECIP_MASK_HAVE_RSQRTE;

  if (rs6000_recip_control)
    {
      if (!flag_finite_math_only)
	warning (0, "%qs requires %qs or %qs", "-mrecip", "-ffinite-math",
		 "-ffast-math");
      if (flag_trapping_math)
	warning (0, "%qs requires %qs or %qs", "-mrecip",
		 "-fno-trapping-math", "-ffast-math");
      if (!flag_reciprocal_math)
	warning (0, "%qs requires %qs or %qs", "-mrecip", "-freciprocal-math",
		 "-ffast-math");
      if (flag_finite_math_only && !flag_trapping_math && flag_reciprocal_math)
	{
	  if (RS6000_RECIP_HAVE_RE_P (SFmode)
	      && (rs6000_recip_control & RECIP_SF_DIV) != 0)
	    rs6000_recip_bits[SFmode] |= RS6000_RECIP_MASK_AUTO_RE;

	  if (RS6000_RECIP_HAVE_RE_P (DFmode)
	      && (rs6000_recip_control & RECIP_DF_DIV) != 0)
	    rs6000_recip_bits[DFmode] |= RS6000_RECIP_MASK_AUTO_RE;

	  if (RS6000_RECIP_HAVE_RE_P (V4SFmode)
	      && (rs6000_recip_control & RECIP_V4SF_DIV) != 0)
	    rs6000_recip_bits[V4SFmode] |= RS6000_RECIP_MASK_AUTO_RE;

	  if (RS6000_RECIP_HAVE_RE_P (V2DFmode)
	      && (rs6000_recip_control & RECIP_V2DF_DIV) != 0)
	    rs6000_recip_bits[V2DFmode] |= RS6000_RECIP_MASK_AUTO_RE;

	  if (RS6000_RECIP_HAVE_RSQRTE_P (SFmode)
	      && (rs6000_recip_control & RECIP_SF_RSQRT) != 0)
	    rs6000_recip_bits[SFmode] |= RS6000_RECIP_MASK_AUTO_RSQRTE;

	  if (RS6000_RECIP_HAVE_RSQRTE_P (DFmode)
	      && (rs6000_recip_control & RECIP_DF_RSQRT) != 0)
	    rs6000_recip_bits[DFmode] |= RS6000_RECIP_MASK_AUTO_RSQRTE;

	  if (RS6000_RECIP_HAVE_RSQRTE_P (V4SFmode)
	      && (rs6000_recip_control & RECIP_V4SF_RSQRT) != 0)
	    rs6000_recip_bits[V4SFmode] |= RS6000_RECIP_MASK_AUTO_RSQRTE;

	  if (RS6000_RECIP_HAVE_RSQRTE_P (V2DFmode)
	      && (rs6000_recip_control & RECIP_V2DF_RSQRT) != 0)
	    rs6000_recip_bits[V2DFmode] |= RS6000_RECIP_MASK_AUTO_RSQRTE;
	}
    }

  /* Update the addr mask bits in reg_addr to help secondary reload and go if
     legitimate address support to figure out the appropriate addressing to
     use.  */
  rs6000_setup_reg_addr_masks ();

  if (global_init_p || TARGET_DEBUG_TARGET)
    {
      if (TARGET_DEBUG_REG)
	rs6000_debug_reg_global ();

      if (TARGET_DEBUG_COST || TARGET_DEBUG_REG)
	fprintf (stderr,
		 "SImode variable mult cost       = %d\n"
		 "SImode constant mult cost       = %d\n"
		 "SImode short constant mult cost = %d\n"
		 "DImode multipliciation cost     = %d\n"
		 "SImode division cost            = %d\n"
		 "DImode division cost            = %d\n"
		 "Simple fp operation cost        = %d\n"
		 "DFmode multiplication cost      = %d\n"
		 "SFmode division cost            = %d\n"
		 "DFmode division cost            = %d\n"
		 "cache line size                 = %d\n"
		 "l1 cache size                   = %d\n"
		 "l2 cache size                   = %d\n"
		 "simultaneous prefetches         = %d\n"
		 "\n",
		 rs6000_cost->mulsi,
		 rs6000_cost->mulsi_const,
		 rs6000_cost->mulsi_const9,
		 rs6000_cost->muldi,
		 rs6000_cost->divsi,
		 rs6000_cost->divdi,
		 rs6000_cost->fp,
		 rs6000_cost->dmul,
		 rs6000_cost->sdiv,
		 rs6000_cost->ddiv,
		 rs6000_cost->cache_line_size,
		 rs6000_cost->l1_cache_size,
		 rs6000_cost->l2_cache_size,
		 rs6000_cost->simultaneous_prefetches);
    }
}

#if TARGET_MACHO
/* The Darwin version of SUBTARGET_OVERRIDE_OPTIONS.  */

static void
darwin_rs6000_override_options (void)
{
  /* The Darwin ABI always includes AltiVec, can't be (validly) turned
     off.  */
  rs6000_altivec_abi = 1;
  TARGET_ALTIVEC_VRSAVE = 1;
  rs6000_current_abi = ABI_DARWIN;

  if (DEFAULT_ABI == ABI_DARWIN
      && TARGET_64BIT)
      darwin_one_byte_bool = 1;

  if (TARGET_64BIT && ! TARGET_POWERPC64)
    {
      rs6000_isa_flags |= OPTION_MASK_POWERPC64;
      warning (0, "%qs requires PowerPC64 architecture, enabling", "-m64");
    }

  /* The linkers [ld64] that support 64Bit do not need the JBSR longcall
     optimisation, and will not work with the most generic case (where the
     symbol is undefined external, but there is no symbl stub).  */
  if (TARGET_64BIT)
    rs6000_default_long_calls = 0;

  /* ld_classic is (so far) still used for kernel (static) code, and supports
     the JBSR longcall / branch islands.  */
  if (flag_mkernel)
    {
      rs6000_default_long_calls = 1;

      /* Allow a kext author to do -mkernel -mhard-float.  */
      if (! (rs6000_isa_flags_explicit & OPTION_MASK_SOFT_FLOAT))
        rs6000_isa_flags |= OPTION_MASK_SOFT_FLOAT;
    }

  /* Make -m64 imply -maltivec.  Darwin's 64-bit ABI includes
     Altivec.  */
  if (!flag_mkernel && !flag_apple_kext
      && TARGET_64BIT
      && ! (rs6000_isa_flags_explicit & OPTION_MASK_ALTIVEC))
    rs6000_isa_flags |= OPTION_MASK_ALTIVEC;

  /* Unless the user (not the configurer) has explicitly overridden
     it with -mcpu=G3 or -mno-altivec, then 10.5+ targets default to
     G4 unless targeting the kernel.  */
  if (!flag_mkernel
      && !flag_apple_kext
      && strverscmp (darwin_macosx_version_min, "10.5") >= 0
      && ! (rs6000_isa_flags_explicit & OPTION_MASK_ALTIVEC)
      && ! global_options_set.x_rs6000_cpu_index)
    {
      rs6000_isa_flags |= OPTION_MASK_ALTIVEC;
    }
}
#endif

/* If not otherwise specified by a target, make 'long double' equivalent to
   'double'.  */

#ifndef RS6000_DEFAULT_LONG_DOUBLE_SIZE
#define RS6000_DEFAULT_LONG_DOUBLE_SIZE 64
#endif

/* Return the builtin mask of the various options used that could affect which
   builtins were used.  In the past we used target_flags, but we've run out of
   bits, and some options are no longer in target_flags.  */

HOST_WIDE_INT
rs6000_builtin_mask_calculate (void)
{
  return (((TARGET_ALTIVEC)		    ? RS6000_BTM_ALTIVEC   : 0)
	  | ((TARGET_CMPB)		    ? RS6000_BTM_CMPB	   : 0)
	  | ((TARGET_VSX)		    ? RS6000_BTM_VSX	   : 0)
	  | ((TARGET_FRE)		    ? RS6000_BTM_FRE	   : 0)
	  | ((TARGET_FRES)		    ? RS6000_BTM_FRES	   : 0)
	  | ((TARGET_FRSQRTE)		    ? RS6000_BTM_FRSQRTE   : 0)
	  | ((TARGET_FRSQRTES)		    ? RS6000_BTM_FRSQRTES  : 0)
	  | ((TARGET_POPCNTD)		    ? RS6000_BTM_POPCNTD   : 0)
	  | ((rs6000_cpu == PROCESSOR_CELL) ? RS6000_BTM_CELL      : 0)
	  | ((TARGET_P8_VECTOR)		    ? RS6000_BTM_P8_VECTOR : 0)
	  | ((TARGET_P9_VECTOR)		    ? RS6000_BTM_P9_VECTOR : 0)
	  | ((TARGET_P9_MISC)		    ? RS6000_BTM_P9_MISC   : 0)
	  | ((TARGET_MODULO)		    ? RS6000_BTM_MODULO    : 0)
	  | ((TARGET_64BIT)		    ? RS6000_BTM_64BIT     : 0)
	  | ((TARGET_POWERPC64)		    ? RS6000_BTM_POWERPC64 : 0)
	  | ((TARGET_CRYPTO)		    ? RS6000_BTM_CRYPTO	   : 0)
	  | ((TARGET_HTM)		    ? RS6000_BTM_HTM	   : 0)
	  | ((TARGET_DFP)		    ? RS6000_BTM_DFP	   : 0)
	  | ((TARGET_HARD_FLOAT)	    ? RS6000_BTM_HARD_FLOAT : 0)
	  | ((TARGET_LONG_DOUBLE_128
	      && TARGET_HARD_FLOAT
	      && !TARGET_IEEEQUAD)	    ? RS6000_BTM_LDBL128   : 0)
	  | ((TARGET_FLOAT128_TYPE)	    ? RS6000_BTM_FLOAT128  : 0)
	  | ((TARGET_FLOAT128_HW)	    ? RS6000_BTM_FLOAT128_HW : 0)
	  | ((TARGET_MMA)		    ? RS6000_BTM_MMA	   : 0)
	  | ((TARGET_POWER10)               ? RS6000_BTM_P10       : 0));
}

/* Implement TARGET_MD_ASM_ADJUST.  All asm statements are considered
   to clobber the XER[CA] bit because clobbering that bit without telling
   the compiler worked just fine with versions of GCC before GCC 5, and
   breaking a lot of older code in ways that are hard to track down is
   not such a great idea.  */

static rtx_insn *
rs6000_md_asm_adjust (vec<rtx> &/*outputs*/, vec<rtx> &/*inputs*/,
		      vec<const char *> &/*constraints*/,
		      vec<rtx> &clobbers, HARD_REG_SET &clobbered_regs)
{
  clobbers.safe_push (gen_rtx_REG (SImode, CA_REGNO));
  SET_HARD_REG_BIT (clobbered_regs, CA_REGNO);
  return NULL;
}

/* This target function is similar to the hook TARGET_OPTION_OVERRIDE
   but is called when the optimize level is changed via an attribute or
   pragma or when it is reset at the end of the code affected by the
   attribute or pragma.  It is not called at the beginning of compilation
   when TARGET_OPTION_OVERRIDE is called so if you want to perform these
   actions then, you should have TARGET_OPTION_OVERRIDE call
   TARGET_OVERRIDE_OPTIONS_AFTER_CHANGE.  */

static void
rs6000_override_options_after_change (void)
{
  /* Explicit -funroll-loops turns -munroll-only-small-loops off, and
     turns -frename-registers on.  */
  if ((global_options_set.x_flag_unroll_loops && flag_unroll_loops)
       || (global_options_set.x_flag_unroll_all_loops
	   && flag_unroll_all_loops))
    {
      if (!global_options_set.x_unroll_only_small_loops)
	unroll_only_small_loops = 0;
      if (!global_options_set.x_flag_rename_registers)
	flag_rename_registers = 1;
      if (!global_options_set.x_flag_cunroll_grow_size)
	flag_cunroll_grow_size = 1;
    }
  else if (!global_options_set.x_flag_cunroll_grow_size)
    flag_cunroll_grow_size = flag_peel_loops || optimize >= 3;
}

/* Override command line options.

   Combine build-specific configuration information with options
   specified on the command line to set various state variables which
   influence code generation, optimization, and expansion of built-in
   functions.  Assure that command-line configuration preferences are
   compatible with each other and with the build configuration; issue
   warnings while adjusting configuration or error messages while
   rejecting configuration.

   Upon entry to this function:

     This function is called once at the beginning of
     compilation, and then again at the start and end of compiling
     each section of code that has a different configuration, as
     indicated, for example, by adding the

       __attribute__((__target__("cpu=power9")))

     qualifier to a function definition or, for example, by bracketing
     code between

       #pragma GCC target("altivec")

     and

       #pragma GCC reset_options

     directives.  Parameter global_init_p is true for the initial
     invocation, which initializes global variables, and false for all
     subsequent invocations.


     Various global state information is assumed to be valid.  This
     includes OPTION_TARGET_CPU_DEFAULT, representing the name of the
     default CPU specified at build configure time, TARGET_DEFAULT,
     representing the default set of option flags for the default
     target, and global_options_set.x_rs6000_isa_flags, representing
     which options were requested on the command line.

   Upon return from this function:

     rs6000_isa_flags_explicit has a non-zero bit for each flag that
     was set by name on the command line.  Additionally, if certain
     attributes are automatically enabled or disabled by this function
     in order to assure compatibility between options and
     configuration, the flags associated with those attributes are
     also set.  By setting these "explicit bits", we avoid the risk
     that other code might accidentally overwrite these particular
     attributes with "default values".

     The various bits of rs6000_isa_flags are set to indicate the
     target options that have been selected for the most current
     compilation efforts.  This has the effect of also turning on the
     associated TARGET_XXX values since these are macros which are
     generally defined to test the corresponding bit of the
     rs6000_isa_flags variable.

     The variable rs6000_builtin_mask is set to represent the target
     options for the most current compilation efforts, consistent with
     the current contents of rs6000_isa_flags.  This variable controls
     expansion of built-in functions.

     Various other global variables and fields of global structures
     (over 50 in all) are initialized to reflect the desired options
     for the most current compilation efforts.  */

static bool
rs6000_option_override_internal (bool global_init_p)
{
  bool ret = true;

  HOST_WIDE_INT set_masks;
  HOST_WIDE_INT ignore_masks;
  int cpu_index = -1;
  int tune_index;
  struct cl_target_option *main_target_opt
    = ((global_init_p || target_option_default_node == NULL)
       ? NULL : TREE_TARGET_OPTION (target_option_default_node));

  /* Print defaults.  */
  if ((TARGET_DEBUG_REG || TARGET_DEBUG_TARGET) && global_init_p)
    rs6000_print_isa_options (stderr, 0, "TARGET_DEFAULT", TARGET_DEFAULT);

  /* Remember the explicit arguments.  */
  if (global_init_p)
    rs6000_isa_flags_explicit = global_options_set.x_rs6000_isa_flags;

  /* On 64-bit Darwin, power alignment is ABI-incompatible with some C
     library functions, so warn about it. The flag may be useful for
     performance studies from time to time though, so don't disable it
     entirely.  */
  if (global_options_set.x_rs6000_alignment_flags
      && rs6000_alignment_flags == MASK_ALIGN_POWER
      && DEFAULT_ABI == ABI_DARWIN
      && TARGET_64BIT)
    warning (0, "%qs is not supported for 64-bit Darwin;"
	     " it is incompatible with the installed C and C++ libraries",
	     "-malign-power");

  /* Numerous experiment shows that IRA based loop pressure
     calculation works better for RTL loop invariant motion on targets
     with enough (>= 32) registers.  It is an expensive optimization.
     So it is on only for peak performance.  */
  if (optimize >= 3 && global_init_p
      && !global_options_set.x_flag_ira_loop_pressure)
    flag_ira_loop_pressure = 1;

  /* -fsanitize=address needs to turn on -fasynchronous-unwind-tables in order
     for tracebacks to be complete but not if any -fasynchronous-unwind-tables
     options were already specified.  */
  if (flag_sanitize & SANITIZE_USER_ADDRESS
      && !global_options_set.x_flag_asynchronous_unwind_tables)
    flag_asynchronous_unwind_tables = 1;

  /* -fvariable-expansion-in-unroller is a win for POWER whenever the
     loop unroller is active.  It is only checked during unrolling, so
     we can just set it on by default.  */
  if (!global_options_set.x_flag_variable_expansion_in_unroller)
    flag_variable_expansion_in_unroller = 1;

  /* Set the pointer size.  */
  if (TARGET_64BIT)
    {
      rs6000_pmode = DImode;
      rs6000_pointer_size = 64;
    }
  else
    {
      rs6000_pmode = SImode;
      rs6000_pointer_size = 32;
    }

  /* Some OSs don't support saving the high part of 64-bit registers on context
     switch.  Other OSs don't support saving Altivec registers.  On those OSs,
     we don't touch the OPTION_MASK_POWERPC64 or OPTION_MASK_ALTIVEC settings;
     if the user wants either, the user must explicitly specify them and we
     won't interfere with the user's specification.  */

  set_masks = POWERPC_MASKS;
#ifdef OS_MISSING_POWERPC64
  if (OS_MISSING_POWERPC64)
    set_masks &= ~OPTION_MASK_POWERPC64;
#endif
#ifdef OS_MISSING_ALTIVEC
  if (OS_MISSING_ALTIVEC)
    set_masks &= ~(OPTION_MASK_ALTIVEC | OPTION_MASK_VSX
		   | OTHER_VSX_VECTOR_MASKS);
#endif

  /* Don't override by the processor default if given explicitly.  */
  set_masks &= ~rs6000_isa_flags_explicit;

  /* Process the -mcpu=<xxx> and -mtune=<xxx> argument.  If the user changed
     the cpu in a target attribute or pragma, but did not specify a tuning
     option, use the cpu for the tuning option rather than the option specified
     with -mtune on the command line.  Process a '--with-cpu' configuration
     request as an implicit --cpu.  */
  if (rs6000_cpu_index >= 0)
    cpu_index = rs6000_cpu_index;
  else if (main_target_opt != NULL && main_target_opt->x_rs6000_cpu_index >= 0)
    cpu_index = main_target_opt->x_rs6000_cpu_index;
  else if (OPTION_TARGET_CPU_DEFAULT)
    cpu_index = rs6000_cpu_name_lookup (OPTION_TARGET_CPU_DEFAULT);

  /* If we have a cpu, either through an explicit -mcpu=<xxx> or if the
     compiler was configured with --with-cpu=<xxx>, replace all of the ISA bits
     with those from the cpu, except for options that were explicitly set.  If
     we don't have a cpu, do not override the target bits set in
     TARGET_DEFAULT.  */
  if (cpu_index >= 0)
    {
      rs6000_cpu_index = cpu_index;
      rs6000_isa_flags &= ~set_masks;
      rs6000_isa_flags |= (processor_target_table[cpu_index].target_enable
			   & set_masks);
    }
  else
    {
      /* If no -mcpu=<xxx>, inherit any default options that were cleared via
	 POWERPC_MASKS.  Originally, TARGET_DEFAULT was used to initialize
	 target_flags via the TARGET_DEFAULT_TARGET_FLAGS hook.  When we switched
	 to using rs6000_isa_flags, we need to do the initialization here.

	 If there is a TARGET_DEFAULT, use that.  Otherwise fall back to using
	 -mcpu=powerpc, -mcpu=powerpc64, or -mcpu=powerpc64le defaults.  */
      HOST_WIDE_INT flags;
      if (TARGET_DEFAULT)
	flags = TARGET_DEFAULT;
      else
	{
	  /* PowerPC 64-bit LE requires at least ISA 2.07.  */
	  const char *default_cpu = (!TARGET_POWERPC64
				     ? "powerpc"
				     : (BYTES_BIG_ENDIAN
					? "powerpc64"
					: "powerpc64le"));
	  int default_cpu_index = rs6000_cpu_name_lookup (default_cpu);
	  flags = processor_target_table[default_cpu_index].target_enable;
	}
      rs6000_isa_flags |= (flags & ~rs6000_isa_flags_explicit);
    }

  if (rs6000_tune_index >= 0)
    tune_index = rs6000_tune_index;
  else if (cpu_index >= 0)
    rs6000_tune_index = tune_index = cpu_index;
  else
    {
      size_t i;
      enum processor_type tune_proc
	= (TARGET_POWERPC64 ? PROCESSOR_DEFAULT64 : PROCESSOR_DEFAULT);

      tune_index = -1;
      for (i = 0; i < ARRAY_SIZE (processor_target_table); i++)
	if (processor_target_table[i].processor == tune_proc)
	  {
	    tune_index = i;
	    break;
	  }
    }

  if (cpu_index >= 0)
    rs6000_cpu = processor_target_table[cpu_index].processor;
  else
    rs6000_cpu = TARGET_POWERPC64 ? PROCESSOR_DEFAULT64 : PROCESSOR_DEFAULT;

  gcc_assert (tune_index >= 0);
  rs6000_tune = processor_target_table[tune_index].processor;

  if (rs6000_cpu == PROCESSOR_PPCE300C2 || rs6000_cpu == PROCESSOR_PPCE300C3
      || rs6000_cpu == PROCESSOR_PPCE500MC || rs6000_cpu == PROCESSOR_PPCE500MC64
      || rs6000_cpu == PROCESSOR_PPCE5500)
    {
      if (TARGET_ALTIVEC)
	error ("AltiVec not supported in this target");
    }

  /* If we are optimizing big endian systems for space, use the load/store
     multiple instructions.  */
  if (BYTES_BIG_ENDIAN && optimize_size)
    rs6000_isa_flags |= ~rs6000_isa_flags_explicit & OPTION_MASK_MULTIPLE;

  /* Don't allow -mmultiple on little endian systems unless the cpu is a 750,
     because the hardware doesn't support the instructions used in little
     endian mode, and causes an alignment trap.  The 750 does not cause an
     alignment trap (except when the target is unaligned).  */

  if (!BYTES_BIG_ENDIAN && rs6000_cpu != PROCESSOR_PPC750 && TARGET_MULTIPLE)
    {
      rs6000_isa_flags &= ~OPTION_MASK_MULTIPLE;
      if ((rs6000_isa_flags_explicit & OPTION_MASK_MULTIPLE) != 0)
	warning (0, "%qs is not supported on little endian systems",
		 "-mmultiple");
    }

  /* If little-endian, default to -mstrict-align on older processors.
     Testing for htm matches power8 and later.  */
  if (!BYTES_BIG_ENDIAN
      && !(processor_target_table[tune_index].target_enable & OPTION_MASK_HTM))
    rs6000_isa_flags |= ~rs6000_isa_flags_explicit & OPTION_MASK_STRICT_ALIGN;

  if (!rs6000_fold_gimple)
     fprintf (stderr,
	      "gimple folding of rs6000 builtins has been disabled.\n");

  /* Add some warnings for VSX.  */
  if (TARGET_VSX)
    {
      const char *msg = NULL;
      if (!TARGET_HARD_FLOAT)
	{
	  if (rs6000_isa_flags_explicit & OPTION_MASK_VSX)
	    msg = N_("%<-mvsx%> requires hardware floating point");
	  else
	    {
	      rs6000_isa_flags &= ~ OPTION_MASK_VSX;
	      rs6000_isa_flags_explicit |= OPTION_MASK_VSX;
	    }
	}
      else if (TARGET_AVOID_XFORM > 0)
	msg = N_("%<-mvsx%> needs indexed addressing");
      else if (!TARGET_ALTIVEC && (rs6000_isa_flags_explicit
				   & OPTION_MASK_ALTIVEC))
        {
	  if (rs6000_isa_flags_explicit & OPTION_MASK_VSX)
	    msg = N_("%<-mvsx%> and %<-mno-altivec%> are incompatible");
	  else
	    msg = N_("%<-mno-altivec%> disables vsx");
        }

      if (msg)
	{
	  warning (0, msg);
	  rs6000_isa_flags &= ~ OPTION_MASK_VSX;
	  rs6000_isa_flags_explicit |= OPTION_MASK_VSX;
	}
    }

  /* If hard-float/altivec/vsx were explicitly turned off then don't allow
     the -mcpu setting to enable options that conflict. */
  if ((!TARGET_HARD_FLOAT || !TARGET_ALTIVEC || !TARGET_VSX)
      && (rs6000_isa_flags_explicit & (OPTION_MASK_SOFT_FLOAT
				       | OPTION_MASK_ALTIVEC
				       | OPTION_MASK_VSX)) != 0)
    rs6000_isa_flags &= ~((OPTION_MASK_P8_VECTOR | OPTION_MASK_CRYPTO
			   | OPTION_MASK_DIRECT_MOVE)
		         & ~rs6000_isa_flags_explicit);

  if (TARGET_DEBUG_REG || TARGET_DEBUG_TARGET)
    rs6000_print_isa_options (stderr, 0, "before defaults", rs6000_isa_flags);

  /* Handle explicit -mno-{altivec,vsx,power8-vector,power9-vector} and turn
     off all of the options that depend on those flags.  */
  ignore_masks = rs6000_disable_incompatible_switches ();

  /* For the newer switches (vsx, dfp, etc.) set some of the older options,
     unless the user explicitly used the -mno-<option> to disable the code.  */
  if (TARGET_P9_VECTOR || TARGET_MODULO || TARGET_P9_MISC)
    rs6000_isa_flags |= (ISA_3_0_MASKS_SERVER & ~ignore_masks);
  else if (TARGET_P9_MINMAX)
    {
      if (cpu_index >= 0)
	{
	  if (cpu_index == PROCESSOR_POWER9)
	    {
	      /* legacy behavior: allow -mcpu=power9 with certain
		 capabilities explicitly disabled.  */
	      rs6000_isa_flags |= (ISA_3_0_MASKS_SERVER & ~ignore_masks);
	    }
	  else
	    error ("power9 target option is incompatible with %<%s=<xxx>%> "
		   "for <xxx> less than power9", "-mcpu");
	}
      else if ((ISA_3_0_MASKS_SERVER & rs6000_isa_flags_explicit)
	       != (ISA_3_0_MASKS_SERVER & rs6000_isa_flags
		   & rs6000_isa_flags_explicit))
	/* Enforce that none of the ISA_3_0_MASKS_SERVER flags
	   were explicitly cleared.  */
	error ("%qs incompatible with explicitly disabled options",
	       "-mpower9-minmax");
      else
	rs6000_isa_flags |= ISA_3_0_MASKS_SERVER;
    }
  else if (TARGET_P8_VECTOR || TARGET_DIRECT_MOVE || TARGET_CRYPTO)
    rs6000_isa_flags |= (ISA_2_7_MASKS_SERVER & ~ignore_masks);
  else if (TARGET_VSX)
    rs6000_isa_flags |= (ISA_2_6_MASKS_SERVER & ~ignore_masks);
  else if (TARGET_POPCNTD)
    rs6000_isa_flags |= (ISA_2_6_MASKS_EMBEDDED & ~ignore_masks);
  else if (TARGET_DFP)
    rs6000_isa_flags |= (ISA_2_5_MASKS_SERVER & ~ignore_masks);
  else if (TARGET_CMPB)
    rs6000_isa_flags |= (ISA_2_5_MASKS_EMBEDDED & ~ignore_masks);
  else if (TARGET_FPRND)
    rs6000_isa_flags |= (ISA_2_4_MASKS & ~ignore_masks);
  else if (TARGET_POPCNTB)
    rs6000_isa_flags |= (ISA_2_2_MASKS & ~ignore_masks);
  else if (TARGET_ALTIVEC)
    rs6000_isa_flags |= (OPTION_MASK_PPC_GFXOPT & ~ignore_masks);

  if (TARGET_CRYPTO && !TARGET_ALTIVEC)
    {
      if (rs6000_isa_flags_explicit & OPTION_MASK_CRYPTO)
	error ("%qs requires %qs", "-mcrypto", "-maltivec");
      rs6000_isa_flags &= ~OPTION_MASK_CRYPTO;
    }

  if (!TARGET_FPRND && TARGET_VSX)
    {
      if (rs6000_isa_flags_explicit & OPTION_MASK_FPRND)
	/* TARGET_VSX = 1 implies Power 7 and newer */
	error ("%qs requires %qs", "-mvsx", "-mfprnd");
      rs6000_isa_flags &= ~OPTION_MASK_FPRND;
    }

  if (TARGET_DIRECT_MOVE && !TARGET_VSX)
    {
      if (rs6000_isa_flags_explicit & OPTION_MASK_DIRECT_MOVE)
	error ("%qs requires %qs", "-mdirect-move", "-mvsx");
      rs6000_isa_flags &= ~OPTION_MASK_DIRECT_MOVE;
    }

  if (TARGET_P8_VECTOR && !TARGET_ALTIVEC)
    {
      if (rs6000_isa_flags_explicit & OPTION_MASK_P8_VECTOR)
	error ("%qs requires %qs", "-mpower8-vector", "-maltivec");
      rs6000_isa_flags &= ~OPTION_MASK_P8_VECTOR;
    }

  if (TARGET_P8_VECTOR && !TARGET_VSX)
    {
      if ((rs6000_isa_flags_explicit & OPTION_MASK_P8_VECTOR)
	  && (rs6000_isa_flags_explicit & OPTION_MASK_VSX))
	error ("%qs requires %qs", "-mpower8-vector", "-mvsx");
      else if ((rs6000_isa_flags_explicit & OPTION_MASK_P8_VECTOR) == 0)
	{
	  rs6000_isa_flags &= ~OPTION_MASK_P8_VECTOR;
	  if (rs6000_isa_flags_explicit & OPTION_MASK_VSX)
	    rs6000_isa_flags_explicit |= OPTION_MASK_P8_VECTOR;
	}
      else
	{
	  /* OPTION_MASK_P8_VECTOR is explicit, and OPTION_MASK_VSX is
	     not explicit.  */
	  rs6000_isa_flags |= OPTION_MASK_VSX;
	  rs6000_isa_flags_explicit |= OPTION_MASK_VSX;
	}
    }

  if (TARGET_DFP && !TARGET_HARD_FLOAT)
    {
      if (rs6000_isa_flags_explicit & OPTION_MASK_DFP)
	error ("%qs requires %qs", "-mhard-dfp", "-mhard-float");
      rs6000_isa_flags &= ~OPTION_MASK_DFP;
    }

  /* The quad memory instructions only works in 64-bit mode. In 32-bit mode,
     silently turn off quad memory mode.  */
  if ((TARGET_QUAD_MEMORY || TARGET_QUAD_MEMORY_ATOMIC) && !TARGET_POWERPC64)
    {
      if ((rs6000_isa_flags_explicit & OPTION_MASK_QUAD_MEMORY) != 0)
	warning (0, N_("%<-mquad-memory%> requires 64-bit mode"));

      if ((rs6000_isa_flags_explicit & OPTION_MASK_QUAD_MEMORY_ATOMIC) != 0)
	warning (0, N_("%<-mquad-memory-atomic%> requires 64-bit mode"));

      rs6000_isa_flags &= ~(OPTION_MASK_QUAD_MEMORY
			    | OPTION_MASK_QUAD_MEMORY_ATOMIC);
    }

  /* Non-atomic quad memory load/store are disabled for little endian, since
     the words are reversed, but atomic operations can still be done by
     swapping the words.  */
  if (TARGET_QUAD_MEMORY && !WORDS_BIG_ENDIAN)
    {
      if ((rs6000_isa_flags_explicit & OPTION_MASK_QUAD_MEMORY) != 0)
	warning (0, N_("%<-mquad-memory%> is not available in little endian "
		       "mode"));

      rs6000_isa_flags &= ~OPTION_MASK_QUAD_MEMORY;
    }

  /* Assume if the user asked for normal quad memory instructions, they want
     the atomic versions as well, unless they explicity told us not to use quad
     word atomic instructions.  */
  if (TARGET_QUAD_MEMORY
      && !TARGET_QUAD_MEMORY_ATOMIC
      && ((rs6000_isa_flags_explicit & OPTION_MASK_QUAD_MEMORY_ATOMIC) == 0))
    rs6000_isa_flags |= OPTION_MASK_QUAD_MEMORY_ATOMIC;

  /* If we can shrink-wrap the TOC register save separately, then use
     -msave-toc-indirect unless explicitly disabled.  */
  if ((rs6000_isa_flags_explicit & OPTION_MASK_SAVE_TOC_INDIRECT) == 0
      && flag_shrink_wrap_separate
      && optimize_function_for_speed_p (cfun))
    rs6000_isa_flags |= OPTION_MASK_SAVE_TOC_INDIRECT;

  /* Enable power8 fusion if we are tuning for power8, even if we aren't
     generating power8 instructions.  Power9 does not optimize power8 fusion
     cases.  */
  if (!(rs6000_isa_flags_explicit & OPTION_MASK_P8_FUSION))
    {
      if (processor_target_table[tune_index].processor == PROCESSOR_POWER8)
	rs6000_isa_flags |= OPTION_MASK_P8_FUSION;
      else
	rs6000_isa_flags &= ~OPTION_MASK_P8_FUSION;
    }

  /* Setting additional fusion flags turns on base fusion.  */
  if (!TARGET_P8_FUSION && TARGET_P8_FUSION_SIGN)
    {
      if (rs6000_isa_flags_explicit & OPTION_MASK_P8_FUSION)
	{
	  if (TARGET_P8_FUSION_SIGN)
	    error ("%qs requires %qs", "-mpower8-fusion-sign",
		   "-mpower8-fusion");

	  rs6000_isa_flags &= ~OPTION_MASK_P8_FUSION;
	}
      else
	rs6000_isa_flags |= OPTION_MASK_P8_FUSION;
    }

  /* Power8 does not fuse sign extended loads with the addis.  If we are
     optimizing at high levels for speed, convert a sign extended load into a
     zero extending load, and an explicit sign extension.  */
  if (TARGET_P8_FUSION
      && !(rs6000_isa_flags_explicit & OPTION_MASK_P8_FUSION_SIGN)
      && optimize_function_for_speed_p (cfun)
      && optimize >= 3)
    rs6000_isa_flags |= OPTION_MASK_P8_FUSION_SIGN;

  /* ISA 3.0 vector instructions include ISA 2.07.  */
  if (TARGET_P9_VECTOR && !TARGET_P8_VECTOR)
    {
      /* We prefer to not mention undocumented options in
	 error messages.  However, if users have managed to select
	 power9-vector without selecting power8-vector, they
	 already know about undocumented flags.  */
      if ((rs6000_isa_flags_explicit & OPTION_MASK_P9_VECTOR) &&
	  (rs6000_isa_flags_explicit & OPTION_MASK_P8_VECTOR))
	error ("%qs requires %qs", "-mpower9-vector", "-mpower8-vector");
      else if ((rs6000_isa_flags_explicit & OPTION_MASK_P9_VECTOR) == 0)
	{
	  rs6000_isa_flags &= ~OPTION_MASK_P9_VECTOR;
	  if (rs6000_isa_flags_explicit & OPTION_MASK_P8_VECTOR)
	    rs6000_isa_flags_explicit |= OPTION_MASK_P9_VECTOR;
	}
      else
	{
	  /* OPTION_MASK_P9_VECTOR is explicit and
	     OPTION_MASK_P8_VECTOR is not explicit.  */
	  rs6000_isa_flags |= OPTION_MASK_P8_VECTOR;
	  rs6000_isa_flags_explicit |= OPTION_MASK_P8_VECTOR;
	}
    }

  /* Set -mallow-movmisalign to explicitly on if we have full ISA 2.07
     support. If we only have ISA 2.06 support, and the user did not specify
     the switch, leave it set to -1 so the movmisalign patterns are enabled,
     but we don't enable the full vectorization support  */
  if (TARGET_ALLOW_MOVMISALIGN == -1 && TARGET_P8_VECTOR && TARGET_DIRECT_MOVE)
    TARGET_ALLOW_MOVMISALIGN = 1;

  else if (TARGET_ALLOW_MOVMISALIGN && !TARGET_VSX)
    {
      if (TARGET_ALLOW_MOVMISALIGN > 0
	  && global_options_set.x_TARGET_ALLOW_MOVMISALIGN)
	error ("%qs requires %qs", "-mallow-movmisalign", "-mvsx");

      TARGET_ALLOW_MOVMISALIGN = 0;
    }

  /* Determine when unaligned vector accesses are permitted, and when
     they are preferred over masked Altivec loads.  Note that if
     TARGET_ALLOW_MOVMISALIGN has been disabled by the user, then
     TARGET_EFFICIENT_UNALIGNED_VSX must be as well.  The converse is
     not true.  */
  if (TARGET_EFFICIENT_UNALIGNED_VSX)
    {
      if (!TARGET_VSX)
	{
	  if (rs6000_isa_flags_explicit & OPTION_MASK_EFFICIENT_UNALIGNED_VSX)
	    error ("%qs requires %qs", "-mefficient-unaligned-vsx", "-mvsx");

	  rs6000_isa_flags &= ~OPTION_MASK_EFFICIENT_UNALIGNED_VSX;
	}

      else if (!TARGET_ALLOW_MOVMISALIGN)
	{
	  if (rs6000_isa_flags_explicit & OPTION_MASK_EFFICIENT_UNALIGNED_VSX)
	    error ("%qs requires %qs", "-munefficient-unaligned-vsx",
		   "-mallow-movmisalign");

	  rs6000_isa_flags &= ~OPTION_MASK_EFFICIENT_UNALIGNED_VSX;
	}
    }

  /* Use long double size to select the appropriate long double.  We use
     TYPE_PRECISION to differentiate the 3 different long double types.  We map
     128 into the precision used for TFmode.  */
  int default_long_double_size = (RS6000_DEFAULT_LONG_DOUBLE_SIZE == 64
				  ? 64
				  : FLOAT_PRECISION_TFmode);

  /* Set long double size before the IEEE 128-bit tests.  */
  if (!global_options_set.x_rs6000_long_double_type_size)
    {
      if (main_target_opt != NULL
	  && (main_target_opt->x_rs6000_long_double_type_size
	      != default_long_double_size))
	error ("target attribute or pragma changes %<long double%> size");
      else
	rs6000_long_double_type_size = default_long_double_size;
    }
  else if (rs6000_long_double_type_size == 128)
    rs6000_long_double_type_size = FLOAT_PRECISION_TFmode;
  else if (global_options_set.x_rs6000_ieeequad)
    {
      if (global_options.x_rs6000_ieeequad)
	error ("%qs requires %qs", "-mabi=ieeelongdouble", "-mlong-double-128");
      else
	error ("%qs requires %qs", "-mabi=ibmlongdouble", "-mlong-double-128");
    }

  /* Set -mabi=ieeelongdouble on some old targets.  In the future, power server
     systems will also set long double to be IEEE 128-bit.  AIX and Darwin
     explicitly redefine TARGET_IEEEQUAD and TARGET_IEEEQUAD_DEFAULT to 0, so
     those systems will not pick up this default.  Warn if the user changes the
     default unless -Wno-psabi.  */
  if (!global_options_set.x_rs6000_ieeequad)
    rs6000_ieeequad = TARGET_IEEEQUAD_DEFAULT;

  else
    {
      if (global_options.x_rs6000_ieeequad
	  && (!TARGET_POPCNTD || !TARGET_VSX))
	error ("%qs requires full ISA 2.06 support", "-mabi=ieeelongdouble");

      if (rs6000_ieeequad != TARGET_IEEEQUAD_DEFAULT && TARGET_LONG_DOUBLE_128)
	{
	  static bool warned_change_long_double;
	  if (!warned_change_long_double)
	    {
	      warned_change_long_double = true;
	      if (TARGET_IEEEQUAD)
		warning (OPT_Wpsabi, "Using IEEE extended precision "
			 "%<long double%>");
	      else
		warning (OPT_Wpsabi, "Using IBM extended precision "
			 "%<long double%>");
	    }
	}
    }

  /* Enable the default support for IEEE 128-bit floating point on Linux VSX
     sytems.  In GCC 7, we would enable the IEEE 128-bit floating point
     infrastructure (-mfloat128-type) but not enable the actual __float128 type
     unless the user used the explicit -mfloat128.  In GCC 8, we enable both
     the keyword as well as the type.  */
  TARGET_FLOAT128_TYPE = TARGET_FLOAT128_ENABLE_TYPE && TARGET_VSX;

  /* IEEE 128-bit floating point requires VSX support.  */
  if (TARGET_FLOAT128_KEYWORD)
    {
      if (!TARGET_VSX)
	{
	  if ((rs6000_isa_flags_explicit & OPTION_MASK_FLOAT128_KEYWORD) != 0)
	    error ("%qs requires VSX support", "-mfloat128");

	  TARGET_FLOAT128_TYPE = 0;
	  rs6000_isa_flags &= ~(OPTION_MASK_FLOAT128_KEYWORD
				| OPTION_MASK_FLOAT128_HW);
	}
      else if (!TARGET_FLOAT128_TYPE)
	{
	  TARGET_FLOAT128_TYPE = 1;
	  warning (0, "The %<-mfloat128%> option may not be fully supported");
	}
    }

  /* Enable the __float128 keyword under Linux by default.  */
  if (TARGET_FLOAT128_TYPE && !TARGET_FLOAT128_KEYWORD
      && (rs6000_isa_flags_explicit & OPTION_MASK_FLOAT128_KEYWORD) == 0)
    rs6000_isa_flags |= OPTION_MASK_FLOAT128_KEYWORD;

  /* If we have are supporting the float128 type and full ISA 3.0 support,
     enable -mfloat128-hardware by default.  However, don't enable the
     __float128 keyword if it was explicitly turned off.  64-bit mode is needed
     because sometimes the compiler wants to put things in an integer
     container, and if we don't have __int128 support, it is impossible.  */
  if (TARGET_FLOAT128_TYPE && !TARGET_FLOAT128_HW && TARGET_64BIT
      && (rs6000_isa_flags & ISA_3_0_MASKS_IEEE) == ISA_3_0_MASKS_IEEE
      && !(rs6000_isa_flags_explicit & OPTION_MASK_FLOAT128_HW))
    rs6000_isa_flags |= OPTION_MASK_FLOAT128_HW;

  if (TARGET_FLOAT128_HW
      && (rs6000_isa_flags & ISA_3_0_MASKS_IEEE) != ISA_3_0_MASKS_IEEE)
    {
      if ((rs6000_isa_flags_explicit & OPTION_MASK_FLOAT128_HW) != 0)
	error ("%qs requires full ISA 3.0 support", "%<-mfloat128-hardware%>");

      rs6000_isa_flags &= ~OPTION_MASK_FLOAT128_HW;
    }

  if (TARGET_FLOAT128_HW && !TARGET_64BIT)
    {
      if ((rs6000_isa_flags_explicit & OPTION_MASK_FLOAT128_HW) != 0)
	error ("%qs requires %qs", "%<-mfloat128-hardware%>", "-m64");

      rs6000_isa_flags &= ~OPTION_MASK_FLOAT128_HW;
    }

  /* Enable -mprefixed by default on power10 systems.  */
  if (TARGET_POWER10 && (rs6000_isa_flags_explicit & OPTION_MASK_PREFIXED) == 0)
    rs6000_isa_flags |= OPTION_MASK_PREFIXED;

  /* -mprefixed requires -mcpu=power10 (or later).  */
  else if (TARGET_PREFIXED && !TARGET_POWER10)
    {
      if ((rs6000_isa_flags_explicit & OPTION_MASK_PREFIXED) != 0)
	error ("%qs requires %qs", "-mprefixed", "-mcpu=power10");

      rs6000_isa_flags &= ~OPTION_MASK_PREFIXED;
    }

  /* -mpcrel requires prefixed load/store addressing.  */
  if (TARGET_PCREL && !TARGET_PREFIXED)
    {
      if ((rs6000_isa_flags_explicit & OPTION_MASK_PCREL) != 0)
	error ("%qs requires %qs", "-mpcrel", "-mprefixed");

      rs6000_isa_flags &= ~OPTION_MASK_PCREL;
    }

  /* Print the options after updating the defaults.  */
  if (TARGET_DEBUG_REG || TARGET_DEBUG_TARGET)
    rs6000_print_isa_options (stderr, 0, "after defaults", rs6000_isa_flags);

  /* E500mc does "better" if we inline more aggressively.  Respect the
     user's opinion, though.  */
  if (rs6000_block_move_inline_limit == 0
      && (rs6000_tune == PROCESSOR_PPCE500MC
	  || rs6000_tune == PROCESSOR_PPCE500MC64
	  || rs6000_tune == PROCESSOR_PPCE5500
	  || rs6000_tune == PROCESSOR_PPCE6500))
    rs6000_block_move_inline_limit = 128;

  /* store_one_arg depends on expand_block_move to handle at least the
     size of reg_parm_stack_space.  */
  if (rs6000_block_move_inline_limit < (TARGET_POWERPC64 ? 64 : 32))
    rs6000_block_move_inline_limit = (TARGET_POWERPC64 ? 64 : 32);

  if (global_init_p)
    {
      /* If the appropriate debug option is enabled, replace the target hooks
	 with debug versions that call the real version and then prints
	 debugging information.  */
      if (TARGET_DEBUG_COST)
	{
	  targetm.rtx_costs = rs6000_debug_rtx_costs;
	  targetm.address_cost = rs6000_debug_address_cost;
	  targetm.sched.adjust_cost = rs6000_debug_adjust_cost;
	}

      if (TARGET_DEBUG_ADDR)
	{
	  targetm.legitimate_address_p = rs6000_debug_legitimate_address_p;
	  targetm.legitimize_address = rs6000_debug_legitimize_address;
	  rs6000_secondary_reload_class_ptr
	    = rs6000_debug_secondary_reload_class;
	  targetm.secondary_memory_needed
	    = rs6000_debug_secondary_memory_needed;
	  targetm.can_change_mode_class
	    = rs6000_debug_can_change_mode_class;
	  rs6000_preferred_reload_class_ptr
	    = rs6000_debug_preferred_reload_class;
	  rs6000_mode_dependent_address_ptr
	    = rs6000_debug_mode_dependent_address;
	}

      if (rs6000_veclibabi_name)
	{
	  if (strcmp (rs6000_veclibabi_name, "mass") == 0)
	    rs6000_veclib_handler = rs6000_builtin_vectorized_libmass;
	  else
	    {
	      error ("unknown vectorization library ABI type (%qs) for "
		     "%qs switch", rs6000_veclibabi_name, "-mveclibabi=");
	      ret = false;
	    }
	}
    }

  /* Disable VSX and Altivec silently if the user switched cpus to power7 in a
     target attribute or pragma which automatically enables both options,
     unless the altivec ABI was set.  This is set by default for 64-bit, but
     not for 32-bit.  */
  if (main_target_opt != NULL && !main_target_opt->x_rs6000_altivec_abi)
    {
      TARGET_FLOAT128_TYPE = 0;
      rs6000_isa_flags &= ~((OPTION_MASK_VSX | OPTION_MASK_ALTIVEC
			     | OPTION_MASK_FLOAT128_KEYWORD)
			    & ~rs6000_isa_flags_explicit);
    }

  /* Enable Altivec ABI for AIX -maltivec.  */
  if (TARGET_XCOFF && (TARGET_ALTIVEC || TARGET_VSX))
    {
      if (main_target_opt != NULL && !main_target_opt->x_rs6000_altivec_abi)
	error ("target attribute or pragma changes AltiVec ABI");
      else
	rs6000_altivec_abi = 1;
    }

  /* The AltiVec ABI is the default for PowerPC-64 GNU/Linux.  For
     PowerPC-32 GNU/Linux, -maltivec implies the AltiVec ABI.  It can
     be explicitly overridden in either case.  */
  if (TARGET_ELF)
    {
      if (!global_options_set.x_rs6000_altivec_abi
	  && (TARGET_64BIT || TARGET_ALTIVEC || TARGET_VSX))
	{
	  if (main_target_opt != NULL &&
	      !main_target_opt->x_rs6000_altivec_abi)
	    error ("target attribute or pragma changes AltiVec ABI");
	  else
	    rs6000_altivec_abi = 1;
	}
    }

  /* Set the Darwin64 ABI as default for 64-bit Darwin.  
     So far, the only darwin64 targets are also MACH-O.  */
  if (TARGET_MACHO
      && DEFAULT_ABI == ABI_DARWIN 
      && TARGET_64BIT)
    {
      if (main_target_opt != NULL && !main_target_opt->x_rs6000_darwin64_abi)
	error ("target attribute or pragma changes darwin64 ABI");
      else
	{
	  rs6000_darwin64_abi = 1;
	  /* Default to natural alignment, for better performance.  */
	  rs6000_alignment_flags = MASK_ALIGN_NATURAL;
	}
    }

  /* Place FP constants in the constant pool instead of TOC
     if section anchors enabled.  */
  if (flag_section_anchors
      && !global_options_set.x_TARGET_NO_FP_IN_TOC)
    TARGET_NO_FP_IN_TOC = 1;

  if (TARGET_DEBUG_REG || TARGET_DEBUG_TARGET)
    rs6000_print_isa_options (stderr, 0, "before subtarget", rs6000_isa_flags);

#ifdef SUBTARGET_OVERRIDE_OPTIONS
  SUBTARGET_OVERRIDE_OPTIONS;
#endif
#ifdef SUBSUBTARGET_OVERRIDE_OPTIONS
  SUBSUBTARGET_OVERRIDE_OPTIONS;
#endif
#ifdef SUB3TARGET_OVERRIDE_OPTIONS
  SUB3TARGET_OVERRIDE_OPTIONS;
#endif

  /* If the ABI has support for PC-relative relocations, enable it by default.
     This test depends on the sub-target tests above setting the code model to
     medium for ELF v2 systems.  */
  if (PCREL_SUPPORTED_BY_OS
      && (rs6000_isa_flags_explicit & OPTION_MASK_PCREL) == 0)
    rs6000_isa_flags |= OPTION_MASK_PCREL;

  /* -mpcrel requires -mcmodel=medium, but we can't check TARGET_CMODEL until
      after the subtarget override options are done.  */
  else if (TARGET_PCREL && TARGET_CMODEL != CMODEL_MEDIUM)
    {
      if ((rs6000_isa_flags_explicit & OPTION_MASK_PCREL) != 0)
	error ("%qs requires %qs", "-mpcrel", "-mcmodel=medium");

      rs6000_isa_flags &= ~OPTION_MASK_PCREL;
    }

  /* Enable -mmma by default on power10 systems.  */
  if (TARGET_POWER10 && (rs6000_isa_flags_explicit & OPTION_MASK_MMA) == 0)
    rs6000_isa_flags |= OPTION_MASK_MMA;

  /* Turn off vector pair/mma options on non-power10 systems.  */
  else if (!TARGET_POWER10 && TARGET_MMA)
    {
      if ((rs6000_isa_flags_explicit & OPTION_MASK_MMA) != 0)
	error ("%qs requires %qs", "-mmma", "-mcpu=power10");

      rs6000_isa_flags &= ~OPTION_MASK_MMA;
    }

  if (TARGET_DEBUG_REG || TARGET_DEBUG_TARGET)
    rs6000_print_isa_options (stderr, 0, "after subtarget", rs6000_isa_flags);

  rs6000_always_hint = (rs6000_tune != PROCESSOR_POWER4
			&& rs6000_tune != PROCESSOR_POWER5
			&& rs6000_tune != PROCESSOR_POWER6
			&& rs6000_tune != PROCESSOR_POWER7
			&& rs6000_tune != PROCESSOR_POWER8
			&& rs6000_tune != PROCESSOR_POWER9
			&& rs6000_tune != PROCESSOR_POWER10
			&& rs6000_tune != PROCESSOR_PPCA2
			&& rs6000_tune != PROCESSOR_CELL
			&& rs6000_tune != PROCESSOR_PPC476);
  rs6000_sched_groups = (rs6000_tune == PROCESSOR_POWER4
			 || rs6000_tune == PROCESSOR_POWER5
			 || rs6000_tune == PROCESSOR_POWER7
			 || rs6000_tune == PROCESSOR_POWER8);
  rs6000_align_branch_targets = (rs6000_tune == PROCESSOR_POWER4
				 || rs6000_tune == PROCESSOR_POWER5
				 || rs6000_tune == PROCESSOR_POWER6
				 || rs6000_tune == PROCESSOR_POWER7
				 || rs6000_tune == PROCESSOR_POWER8
				 || rs6000_tune == PROCESSOR_POWER9
				 || rs6000_tune == PROCESSOR_POWER10
				 || rs6000_tune == PROCESSOR_PPCE500MC
				 || rs6000_tune == PROCESSOR_PPCE500MC64
				 || rs6000_tune == PROCESSOR_PPCE5500
				 || rs6000_tune == PROCESSOR_PPCE6500);

  /* Allow debug switches to override the above settings.  These are set to -1
     in rs6000.opt to indicate the user hasn't directly set the switch.  */
  if (TARGET_ALWAYS_HINT >= 0)
    rs6000_always_hint = TARGET_ALWAYS_HINT;

  if (TARGET_SCHED_GROUPS >= 0)
    rs6000_sched_groups = TARGET_SCHED_GROUPS;

  if (TARGET_ALIGN_BRANCH_TARGETS >= 0)
    rs6000_align_branch_targets = TARGET_ALIGN_BRANCH_TARGETS;

  rs6000_sched_restricted_insns_priority
    = (rs6000_sched_groups ? 1 : 0);

  /* Handle -msched-costly-dep option.  */
  rs6000_sched_costly_dep
    = (rs6000_sched_groups ? true_store_to_load_dep_costly : no_dep_costly);

  if (rs6000_sched_costly_dep_str)
    {
      if (! strcmp (rs6000_sched_costly_dep_str, "no"))
	rs6000_sched_costly_dep = no_dep_costly;
      else if (! strcmp (rs6000_sched_costly_dep_str, "all"))
	rs6000_sched_costly_dep = all_deps_costly;
      else if (! strcmp (rs6000_sched_costly_dep_str, "true_store_to_load"))
	rs6000_sched_costly_dep = true_store_to_load_dep_costly;
      else if (! strcmp (rs6000_sched_costly_dep_str, "store_to_load"))
	rs6000_sched_costly_dep = store_to_load_dep_costly;
      else
	rs6000_sched_costly_dep = ((enum rs6000_dependence_cost)
				   atoi (rs6000_sched_costly_dep_str));
    }

  /* Handle -minsert-sched-nops option.  */
  rs6000_sched_insert_nops
    = (rs6000_sched_groups ? sched_finish_regroup_exact : sched_finish_none);

  if (rs6000_sched_insert_nops_str)
    {
      if (! strcmp (rs6000_sched_insert_nops_str, "no"))
	rs6000_sched_insert_nops = sched_finish_none;
      else if (! strcmp (rs6000_sched_insert_nops_str, "pad"))
	rs6000_sched_insert_nops = sched_finish_pad_groups;
      else if (! strcmp (rs6000_sched_insert_nops_str, "regroup_exact"))
	rs6000_sched_insert_nops = sched_finish_regroup_exact;
      else
	rs6000_sched_insert_nops = ((enum rs6000_nop_insertion)
				    atoi (rs6000_sched_insert_nops_str));
    }

  /* Handle stack protector */
  if (!global_options_set.x_rs6000_stack_protector_guard)
#ifdef TARGET_THREAD_SSP_OFFSET
    rs6000_stack_protector_guard = SSP_TLS;
#else
    rs6000_stack_protector_guard = SSP_GLOBAL;
#endif

#ifdef TARGET_THREAD_SSP_OFFSET
  rs6000_stack_protector_guard_offset = TARGET_THREAD_SSP_OFFSET;
  rs6000_stack_protector_guard_reg = TARGET_64BIT ? 13 : 2;
#endif

  if (global_options_set.x_rs6000_stack_protector_guard_offset_str)
    {
      char *endp;
      const char *str = rs6000_stack_protector_guard_offset_str;

      errno = 0;
      long offset = strtol (str, &endp, 0);
      if (!*str || *endp || errno)
	error ("%qs is not a valid number in %qs", str,
	       "-mstack-protector-guard-offset=");

      if (!IN_RANGE (offset, -0x8000, 0x7fff)
	  || (TARGET_64BIT && (offset & 3)))
	error ("%qs is not a valid offset in %qs", str,
	       "-mstack-protector-guard-offset=");

      rs6000_stack_protector_guard_offset = offset;
    }

  if (global_options_set.x_rs6000_stack_protector_guard_reg_str)
    {
      const char *str = rs6000_stack_protector_guard_reg_str;
      int reg = decode_reg_name (str);

      if (!IN_RANGE (reg, 1, 31))
	error ("%qs is not a valid base register in %qs", str,
	       "-mstack-protector-guard-reg=");

      rs6000_stack_protector_guard_reg = reg;
    }

  if (rs6000_stack_protector_guard == SSP_TLS
      && !IN_RANGE (rs6000_stack_protector_guard_reg, 1, 31))
    error ("%qs needs a valid base register", "-mstack-protector-guard=tls");

  if (global_init_p)
    {
#ifdef TARGET_REGNAMES
      /* If the user desires alternate register names, copy in the
	 alternate names now.  */
      if (TARGET_REGNAMES)
	memcpy (rs6000_reg_names, alt_reg_names, sizeof (rs6000_reg_names));
#endif

      /* Set aix_struct_return last, after the ABI is determined.
	 If -maix-struct-return or -msvr4-struct-return was explicitly
	 used, don't override with the ABI default.  */
      if (!global_options_set.x_aix_struct_return)
	aix_struct_return = (DEFAULT_ABI != ABI_V4 || DRAFT_V4_STRUCT_RET);

#if 0
      /* IBM XL compiler defaults to unsigned bitfields.  */
      if (TARGET_XL_COMPAT)
	flag_signed_bitfields = 0;
#endif

      if (TARGET_LONG_DOUBLE_128 && !TARGET_IEEEQUAD)
	REAL_MODE_FORMAT (TFmode) = &ibm_extended_format;

      ASM_GENERATE_INTERNAL_LABEL (toc_label_name, "LCTOC", 1);

      /* We can only guarantee the availability of DI pseudo-ops when
	 assembling for 64-bit targets.  */
      if (!TARGET_64BIT)
	{
	  targetm.asm_out.aligned_op.di = NULL;
	  targetm.asm_out.unaligned_op.di = NULL;
	}


      /* Set branch target alignment, if not optimizing for size.  */
      if (!optimize_size)
	{
	  /* Cell wants to be aligned 8byte for dual issue.  Titan wants to be
	     aligned 8byte to avoid misprediction by the branch predictor.  */
	  if (rs6000_tune == PROCESSOR_TITAN
	      || rs6000_tune == PROCESSOR_CELL)
	    {
	      if (flag_align_functions && !str_align_functions)
		str_align_functions = "8";
	      if (flag_align_jumps && !str_align_jumps)
		str_align_jumps = "8";
	      if (flag_align_loops && !str_align_loops)
		str_align_loops = "8";
	    }
	  if (rs6000_align_branch_targets)
	    {
	      if (flag_align_functions && !str_align_functions)
		str_align_functions = "16";
	      if (flag_align_jumps && !str_align_jumps)
		str_align_jumps = "16";
	      if (flag_align_loops && !str_align_loops)
		{
		  can_override_loop_align = 1;
		  str_align_loops = "16";
		}
	    }
	}

      /* Arrange to save and restore machine status around nested functions.  */
      init_machine_status = rs6000_init_machine_status;

      /* We should always be splitting complex arguments, but we can't break
	 Linux and Darwin ABIs at the moment.  For now, only AIX is fixed.  */
      if (DEFAULT_ABI == ABI_V4 || DEFAULT_ABI == ABI_DARWIN)
	targetm.calls.split_complex_arg = NULL;

      /* The AIX and ELFv1 ABIs define standard function descriptors.  */
      if (DEFAULT_ABI == ABI_AIX)
	targetm.calls.custom_function_descriptors = 0;
    }

  /* Initialize rs6000_cost with the appropriate target costs.  */
  if (optimize_size)
    rs6000_cost = TARGET_POWERPC64 ? &size64_cost : &size32_cost;
  else
    switch (rs6000_tune)
      {
      case PROCESSOR_RS64A:
	rs6000_cost = &rs64a_cost;
	break;

      case PROCESSOR_MPCCORE:
	rs6000_cost = &mpccore_cost;
	break;

      case PROCESSOR_PPC403:
	rs6000_cost = &ppc403_cost;
	break;

      case PROCESSOR_PPC405:
	rs6000_cost = &ppc405_cost;
	break;

      case PROCESSOR_PPC440:
	rs6000_cost = &ppc440_cost;
	break;

      case PROCESSOR_PPC476:
	rs6000_cost = &ppc476_cost;
	break;

      case PROCESSOR_PPC601:
	rs6000_cost = &ppc601_cost;
	break;

      case PROCESSOR_PPC603:
	rs6000_cost = &ppc603_cost;
	break;

      case PROCESSOR_PPC604:
	rs6000_cost = &ppc604_cost;
	break;

      case PROCESSOR_PPC604e:
	rs6000_cost = &ppc604e_cost;
	break;

      case PROCESSOR_PPC620:
	rs6000_cost = &ppc620_cost;
	break;

      case PROCESSOR_PPC630:
	rs6000_cost = &ppc630_cost;
	break;

      case PROCESSOR_CELL:
	rs6000_cost = &ppccell_cost;
	break;

      case PROCESSOR_PPC750:
      case PROCESSOR_PPC7400:
	rs6000_cost = &ppc750_cost;
	break;

      case PROCESSOR_PPC7450:
	rs6000_cost = &ppc7450_cost;
	break;

      case PROCESSOR_PPC8540:
      case PROCESSOR_PPC8548:
	rs6000_cost = &ppc8540_cost;
	break;

      case PROCESSOR_PPCE300C2:
      case PROCESSOR_PPCE300C3:
	rs6000_cost = &ppce300c2c3_cost;
	break;

      case PROCESSOR_PPCE500MC:
	rs6000_cost = &ppce500mc_cost;
	break;

      case PROCESSOR_PPCE500MC64:
	rs6000_cost = &ppce500mc64_cost;
	break;

      case PROCESSOR_PPCE5500:
	rs6000_cost = &ppce5500_cost;
	break;

      case PROCESSOR_PPCE6500:
	rs6000_cost = &ppce6500_cost;
	break;

      case PROCESSOR_TITAN:
	rs6000_cost = &titan_cost;
	break;

      case PROCESSOR_POWER4:
      case PROCESSOR_POWER5:
	rs6000_cost = &power4_cost;
	break;

      case PROCESSOR_POWER6:
	rs6000_cost = &power6_cost;
	break;

      case PROCESSOR_POWER7:
	rs6000_cost = &power7_cost;
	break;

      case PROCESSOR_POWER8:
	rs6000_cost = &power8_cost;
	break;

      case PROCESSOR_POWER9:
      case PROCESSOR_POWER10:
	rs6000_cost = &power9_cost;
	break;

      case PROCESSOR_PPCA2:
	rs6000_cost = &ppca2_cost;
	break;

      default:
	gcc_unreachable ();
      }

  if (global_init_p)
    {
      SET_OPTION_IF_UNSET (&global_options, &global_options_set,
			   param_simultaneous_prefetches,
			   rs6000_cost->simultaneous_prefetches);
      SET_OPTION_IF_UNSET (&global_options, &global_options_set,
			   param_l1_cache_size,
			   rs6000_cost->l1_cache_size);
      SET_OPTION_IF_UNSET (&global_options, &global_options_set,
			   param_l1_cache_line_size,
			   rs6000_cost->cache_line_size);
      SET_OPTION_IF_UNSET (&global_options, &global_options_set,
			   param_l2_cache_size,
			   rs6000_cost->l2_cache_size);

      /* Increase loop peeling limits based on performance analysis. */
      SET_OPTION_IF_UNSET (&global_options, &global_options_set,
			   param_max_peeled_insns, 400);
      SET_OPTION_IF_UNSET (&global_options, &global_options_set,
			   param_max_completely_peeled_insns, 400);

      /* Use the 'model' -fsched-pressure algorithm by default.  */
      SET_OPTION_IF_UNSET (&global_options, &global_options_set,
			   param_sched_pressure_algorithm,
			   SCHED_PRESSURE_MODEL);

      /* If using typedef char *va_list, signal that
	 __builtin_va_start (&ap, 0) can be optimized to
	 ap = __builtin_next_arg (0).  */
      if (DEFAULT_ABI != ABI_V4)
	targetm.expand_builtin_va_start = NULL;
    }

  rs6000_override_options_after_change ();

  /* If not explicitly specified via option, decide whether to generate indexed
     load/store instructions.  A value of -1 indicates that the
     initial value of this variable has not been overwritten. During
     compilation, TARGET_AVOID_XFORM is either 0 or 1. */
  if (TARGET_AVOID_XFORM == -1)
    /* Avoid indexed addressing when targeting Power6 in order to avoid the
     DERAT mispredict penalty.  However the LVE and STVE altivec instructions
     need indexed accesses and the type used is the scalar type of the element
     being loaded or stored.  */
    TARGET_AVOID_XFORM = (rs6000_tune == PROCESSOR_POWER6 && TARGET_CMPB
			  && !TARGET_ALTIVEC);

  /* Set the -mrecip options.  */
  if (rs6000_recip_name)
    {
      char *p = ASTRDUP (rs6000_recip_name);
      char *q;
      unsigned int mask, i;
      bool invert;

      while ((q = strtok (p, ",")) != NULL)
	{
	  p = NULL;
	  if (*q == '!')
	    {
	      invert = true;
	      q++;
	    }
	  else
	    invert = false;

	  if (!strcmp (q, "default"))
	    mask = ((TARGET_RECIP_PRECISION)
		    ? RECIP_HIGH_PRECISION : RECIP_LOW_PRECISION);
	  else
	    {
	      for (i = 0; i < ARRAY_SIZE (recip_options); i++)
		if (!strcmp (q, recip_options[i].string))
		  {
		    mask = recip_options[i].mask;
		    break;
		  }

	      if (i == ARRAY_SIZE (recip_options))
		{
		  error ("unknown option for %<%s=%s%>", "-mrecip", q);
		  invert = false;
		  mask = 0;
		  ret = false;
		}
	    }

	  if (invert)
	    rs6000_recip_control &= ~mask;
	  else
	    rs6000_recip_control |= mask;
	}
    }

  /* Set the builtin mask of the various options used that could affect which
     builtins were used.  In the past we used target_flags, but we've run out
     of bits, and some options are no longer in target_flags.  */
  rs6000_builtin_mask = rs6000_builtin_mask_calculate ();
  if (TARGET_DEBUG_BUILTIN || TARGET_DEBUG_TARGET)
    rs6000_print_builtin_options (stderr, 0, "builtin mask",
				  rs6000_builtin_mask);

  /* Initialize all of the registers.  */
  rs6000_init_hard_regno_mode_ok (global_init_p);

  /* Save the initial options in case the user does function specific options */
  if (global_init_p)
    target_option_default_node = target_option_current_node
      = build_target_option_node (&global_options);

  /* If not explicitly specified via option, decide whether to generate the
     extra blr's required to preserve the link stack on some cpus (eg, 476).  */
  if (TARGET_LINK_STACK == -1)
    SET_TARGET_LINK_STACK (rs6000_tune == PROCESSOR_PPC476 && flag_pic);

  /* Deprecate use of -mno-speculate-indirect-jumps.  */
  if (!rs6000_speculate_indirect_jumps)
    warning (0, "%qs is deprecated and not recommended in any circumstances",
	     "-mno-speculate-indirect-jumps");

  return ret;
}

/* Implement TARGET_OPTION_OVERRIDE.  On the RS/6000 this is used to
   define the target cpu type.  */

static void
rs6000_option_override (void)
{
  (void) rs6000_option_override_internal (true);
}


/* Implement targetm.vectorize.builtin_mask_for_load.  */
static tree
rs6000_builtin_mask_for_load (void)
{
  /* Don't use lvsl/vperm for P8 and similarly efficient machines.  */
  if ((TARGET_ALTIVEC && !TARGET_VSX)
      || (TARGET_VSX && !TARGET_EFFICIENT_UNALIGNED_VSX))
    return altivec_builtin_mask_for_load;
  else
    return 0;
}

/* Implement LOOP_ALIGN. */
align_flags
rs6000_loop_align (rtx label)
{
  basic_block bb;
  int ninsns;

  /* Don't override loop alignment if -falign-loops was specified. */
  if (!can_override_loop_align)
    return align_loops;

  bb = BLOCK_FOR_INSN (label);
  ninsns = num_loop_insns(bb->loop_father);

  /* Align small loops to 32 bytes to fit in an icache sector, otherwise return default. */
  if (ninsns > 4 && ninsns <= 8
      && (rs6000_tune == PROCESSOR_POWER4
	  || rs6000_tune == PROCESSOR_POWER5
	  || rs6000_tune == PROCESSOR_POWER6
	  || rs6000_tune == PROCESSOR_POWER7
	  || rs6000_tune == PROCESSOR_POWER8))
    return align_flags (5);
  else
    return align_loops;
}

/* Return true iff, data reference of TYPE can reach vector alignment (16)
   after applying N number of iterations.  This routine does not determine
   how may iterations are required to reach desired alignment.  */

static bool
rs6000_vector_alignment_reachable (const_tree type ATTRIBUTE_UNUSED, bool is_packed)
{
  if (is_packed)
    return false;

  if (TARGET_32BIT)
    {
      if (rs6000_alignment_flags == MASK_ALIGN_NATURAL)
        return true;

      if (rs6000_alignment_flags ==  MASK_ALIGN_POWER)
        return true;

      return false;
    }
  else
    {
      if (TARGET_MACHO)
        return false;

      /* Assuming that all other types are naturally aligned. CHECKME!  */
      return true;
    }
}

/* Return true if the vector misalignment factor is supported by the
   target.  */ 
static bool
rs6000_builtin_support_vector_misalignment (machine_mode mode,
					    const_tree type,
					    int misalignment,
					    bool is_packed)
{
  if (TARGET_VSX)
    {
      if (TARGET_EFFICIENT_UNALIGNED_VSX)
	return true;

      /* Return if movmisalign pattern is not supported for this mode.  */
      if (optab_handler (movmisalign_optab, mode) == CODE_FOR_nothing)
        return false;

      if (misalignment == -1)
	{
	  /* Misalignment factor is unknown at compile time but we know
	     it's word aligned.  */
	  if (rs6000_vector_alignment_reachable (type, is_packed))
            {
              int element_size = TREE_INT_CST_LOW (TYPE_SIZE (type));

              if (element_size == 64 || element_size == 32)
               return true;
            }

	  return false;
	}

      /* VSX supports word-aligned vector.  */
      if (misalignment % 4 == 0)
	return true;
    }
  return false;
}

/* Implement targetm.vectorize.builtin_vectorization_cost.  */
static int
rs6000_builtin_vectorization_cost (enum vect_cost_for_stmt type_of_cost,
                                   tree vectype, int misalign)
{
  unsigned elements;
  tree elem_type;

  switch (type_of_cost)
    {
      case scalar_stmt:
      case scalar_store:
      case vector_stmt:
      case vector_store:
      case vec_to_scalar:
      case scalar_to_vec:
      case cond_branch_not_taken:
        return 1;
      case scalar_load:
      case vector_load:
	/* Like rs6000_insn_cost, make load insns cost a bit more.  */
	  return 2;

      case vec_perm:
	/* Power7 has only one permute unit, make it a bit expensive.  */
	if (TARGET_VSX && rs6000_tune == PROCESSOR_POWER7)
	  return 3;
	else
	  return 1;

      case vec_promote_demote:
	/* Power7 has only one permute/pack unit, make it a bit expensive.  */
	if (TARGET_VSX && rs6000_tune == PROCESSOR_POWER7)
	  return 4;
	else
	  return 1;

      case cond_branch_taken:
        return 3;

      case unaligned_load:
      case vector_gather_load:
	/* Like rs6000_insn_cost, make load insns cost a bit more.  */
	if (TARGET_EFFICIENT_UNALIGNED_VSX)
	  return 2;

	if (TARGET_VSX && TARGET_ALLOW_MOVMISALIGN)
	  {
	    elements = TYPE_VECTOR_SUBPARTS (vectype);
	    if (elements == 2)
	      /* Double word aligned.  */
	      return 4;

	    if (elements == 4)
	      {
		switch (misalign)
		  {
		  case 8:
		    /* Double word aligned.  */
		    return 4;

		  case -1:
		    /* Unknown misalignment.  */
		  case 4:
		  case 12:
		    /* Word aligned.  */
		    return 33;

		  default:
		    gcc_unreachable ();
		  }
	      }
	  }

	if (TARGET_ALTIVEC)
	  /* Misaligned loads are not supported.  */
	  gcc_unreachable ();

	/* Like rs6000_insn_cost, make load insns cost a bit more.  */
	return 4;

      case unaligned_store:
      case vector_scatter_store:
	if (TARGET_EFFICIENT_UNALIGNED_VSX)
	  return 1;

        if (TARGET_VSX && TARGET_ALLOW_MOVMISALIGN)
          {
            elements = TYPE_VECTOR_SUBPARTS (vectype);
            if (elements == 2)
              /* Double word aligned.  */
              return 2;

            if (elements == 4)
              {
                switch (misalign)
                  {
                    case 8:
                      /* Double word aligned.  */
                      return 2;

                    case -1:
                      /* Unknown misalignment.  */
                    case 4:
                    case 12:
                      /* Word aligned.  */
                      return 23;

                    default:
                      gcc_unreachable ();
                  }
              }
          }

        if (TARGET_ALTIVEC)
          /* Misaligned stores are not supported.  */
          gcc_unreachable ();

        return 2;

      case vec_construct:
	/* This is a rough approximation assuming non-constant elements
	   constructed into a vector via element insertion.  FIXME:
	   vec_construct is not granular enough for uniformly good
	   decisions.  If the initialization is a splat, this is
	   cheaper than we estimate.  Improve this someday.  */
	elem_type = TREE_TYPE (vectype);
	/* 32-bit vectors loaded into registers are stored as double
	   precision, so we need 2 permutes, 2 converts, and 1 merge
	   to construct a vector of short floats from them.  */
	if (SCALAR_FLOAT_TYPE_P (elem_type)
	    && TYPE_PRECISION (elem_type) == 32)
	  return 5;
	/* On POWER9, integer vector types are built up in GPRs and then
	   use a direct move (2 cycles).  For POWER8 this is even worse,
	   as we need two direct moves and a merge, and the direct moves
	   are five cycles.  */
	else if (INTEGRAL_TYPE_P (elem_type))
	  {
	    if (TARGET_P9_VECTOR)
	      return TYPE_VECTOR_SUBPARTS (vectype) - 1 + 2;
	    else
	      return TYPE_VECTOR_SUBPARTS (vectype) - 1 + 5;
	  }
	else
	  /* V2DFmode doesn't need a direct move.  */
	  return 2;

      default:
        gcc_unreachable ();
    }
}

/* Implement targetm.vectorize.preferred_simd_mode.  */

static machine_mode
rs6000_preferred_simd_mode (scalar_mode mode)
{
  opt_machine_mode vmode = mode_for_vector (mode, 16 / GET_MODE_SIZE (mode));

  if (vmode.exists () && !VECTOR_MEM_NONE_P (vmode.require ()))
    return vmode.require ();

  return word_mode;
}

typedef struct _rs6000_cost_data
{
  struct loop *loop_info;
  unsigned cost[3];
} rs6000_cost_data;

/* Test for likely overcommitment of vector hardware resources.  If a
   loop iteration is relatively large, and too large a percentage of
   instructions in the loop are vectorized, the cost model may not
   adequately reflect delays from unavailable vector resources.
   Penalize the loop body cost for this case.  */

static void
rs6000_density_test (rs6000_cost_data *data)
{
  const int DENSITY_PCT_THRESHOLD = 85;
  const int DENSITY_SIZE_THRESHOLD = 70;
  const int DENSITY_PENALTY = 10;
  struct loop *loop = data->loop_info;
  basic_block *bbs = get_loop_body (loop);
  int nbbs = loop->num_nodes;
  loop_vec_info loop_vinfo = loop_vec_info_for_loop (data->loop_info);
  int vec_cost = data->cost[vect_body], not_vec_cost = 0;
  int i, density_pct;

  for (i = 0; i < nbbs; i++)
    {
      basic_block bb = bbs[i];
      gimple_stmt_iterator gsi;

      for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
	{
	  gimple *stmt = gsi_stmt (gsi);
	  stmt_vec_info stmt_info = loop_vinfo->lookup_stmt (stmt);

	  if (!STMT_VINFO_RELEVANT_P (stmt_info)
	      && !STMT_VINFO_IN_PATTERN_P (stmt_info))
	    not_vec_cost++;
	}
    }

  free (bbs);
  density_pct = (vec_cost * 100) / (vec_cost + not_vec_cost);

  if (density_pct > DENSITY_PCT_THRESHOLD
      && vec_cost + not_vec_cost > DENSITY_SIZE_THRESHOLD)
    {
      data->cost[vect_body] = vec_cost * (100 + DENSITY_PENALTY) / 100;
      if (dump_enabled_p ())
	dump_printf_loc (MSG_NOTE, vect_location,
			 "density %d%%, cost %d exceeds threshold, penalizing "
			 "loop body cost by %d%%", density_pct,
			 vec_cost + not_vec_cost, DENSITY_PENALTY);
    }
}

/* Implement targetm.vectorize.init_cost.  */

/* For each vectorized loop, this var holds TRUE iff a non-memory vector
   instruction is needed by the vectorization.  */
static bool rs6000_vect_nonmem;

static void *
rs6000_init_cost (struct loop *loop_info)
{
  rs6000_cost_data *data = XNEW (struct _rs6000_cost_data);
  data->loop_info = loop_info;
  data->cost[vect_prologue] = 0;
  data->cost[vect_body]     = 0;
  data->cost[vect_epilogue] = 0;
  rs6000_vect_nonmem = false;
  return data;
}

/* Adjust vectorization cost after calling rs6000_builtin_vectorization_cost.
   For some statement, we would like to further fine-grain tweak the cost on
   top of rs6000_builtin_vectorization_cost handling which doesn't have any
   information on statement operation codes etc.  One typical case here is
   COND_EXPR, it takes the same cost to simple FXU instruction when evaluating
   for scalar cost, but it should be priced more whatever transformed to either
   compare + branch or compare + isel instructions.  */

static unsigned
adjust_vectorization_cost (enum vect_cost_for_stmt kind,
			   struct _stmt_vec_info *stmt_info)
{
  if (kind == scalar_stmt && stmt_info && stmt_info->stmt
      && gimple_code (stmt_info->stmt) == GIMPLE_ASSIGN)
    {
      tree_code subcode = gimple_assign_rhs_code (stmt_info->stmt);
      if (subcode == COND_EXPR)
	return 2;
    }

  return 0;
}

/* Implement targetm.vectorize.add_stmt_cost.  */

static unsigned
rs6000_add_stmt_cost (void *data, int count, enum vect_cost_for_stmt kind,
		      struct _stmt_vec_info *stmt_info, int misalign,
		      enum vect_cost_model_location where)
{
  rs6000_cost_data *cost_data = (rs6000_cost_data*) data;
  unsigned retval = 0;

  if (flag_vect_cost_model)
    {
      tree vectype = stmt_info ? stmt_vectype (stmt_info) : NULL_TREE;
      int stmt_cost = rs6000_builtin_vectorization_cost (kind, vectype,
							 misalign);
      stmt_cost += adjust_vectorization_cost (kind, stmt_info);
      /* Statements in an inner loop relative to the loop being
	 vectorized are weighted more heavily.  The value here is
	 arbitrary and could potentially be improved with analysis.  */
      if (where == vect_body && stmt_info && stmt_in_inner_loop_p (stmt_info))
	count *= 50;  /* FIXME.  */

      retval = (unsigned) (count * stmt_cost);
      cost_data->cost[where] += retval;

      /* Check whether we're doing something other than just a copy loop.
	 Not all such loops may be profitably vectorized; see
	 rs6000_finish_cost.  */
      if ((kind == vec_to_scalar || kind == vec_perm
	   || kind == vec_promote_demote || kind == vec_construct
	   || kind == scalar_to_vec)
	  || (where == vect_body && kind == vector_stmt))
	rs6000_vect_nonmem = true;
    }

  return retval;
}

/* Implement targetm.vectorize.finish_cost.  */

static void
rs6000_finish_cost (void *data, unsigned *prologue_cost,
		    unsigned *body_cost, unsigned *epilogue_cost)
{
  rs6000_cost_data *cost_data = (rs6000_cost_data*) data;

  if (cost_data->loop_info)
    rs6000_density_test (cost_data);

  /* Don't vectorize minimum-vectorization-factor, simple copy loops
     that require versioning for any reason.  The vectorization is at
     best a wash inside the loop, and the versioning checks make
     profitability highly unlikely and potentially quite harmful.  */
  if (cost_data->loop_info)
    {
      loop_vec_info vec_info = loop_vec_info_for_loop (cost_data->loop_info);
      if (!rs6000_vect_nonmem
	  && LOOP_VINFO_VECT_FACTOR (vec_info) == 2
	  && LOOP_REQUIRES_VERSIONING (vec_info))
	cost_data->cost[vect_body] += 10000;
    }

  *prologue_cost = cost_data->cost[vect_prologue];
  *body_cost     = cost_data->cost[vect_body];
  *epilogue_cost = cost_data->cost[vect_epilogue];
}

/* Implement targetm.vectorize.destroy_cost_data.  */

static void
rs6000_destroy_cost_data (void *data)
{
  free (data);
}

/* Implement targetm.loop_unroll_adjust.  */

static unsigned
rs6000_loop_unroll_adjust (unsigned nunroll, struct loop *loop)
{
   if (unroll_only_small_loops)
    {
      /* TODO: This is hardcoded to 10 right now.  It can be refined, for
	 example we may want to unroll very small loops more times (4 perhaps).
	 We also should use a PARAM for this.  */
      if (loop->ninsns <= 10)
	return MIN (2, nunroll);
      else
	return 0;
    }

  return nunroll;
}

/* Handler for the Mathematical Acceleration Subsystem (mass) interface to a
   library with vectorized intrinsics.  */

static tree
rs6000_builtin_vectorized_libmass (combined_fn fn, tree type_out,
				   tree type_in)
{
  char name[32];
  const char *suffix = NULL;
  tree fntype, new_fndecl, bdecl = NULL_TREE;
  int n_args = 1;
  const char *bname;
  machine_mode el_mode, in_mode;
  int n, in_n;

  /* Libmass is suitable for unsafe math only as it does not correctly support
     parts of IEEE with the required precision such as denormals.  Only support
     it if we have VSX to use the simd d2 or f4 functions.
     XXX: Add variable length support.  */
  if (!flag_unsafe_math_optimizations || !TARGET_VSX)
    return NULL_TREE;

  el_mode = TYPE_MODE (TREE_TYPE (type_out));
  n = TYPE_VECTOR_SUBPARTS (type_out);
  in_mode = TYPE_MODE (TREE_TYPE (type_in));
  in_n = TYPE_VECTOR_SUBPARTS (type_in);
  if (el_mode != in_mode
      || n != in_n)
    return NULL_TREE;

  switch (fn)
    {
    CASE_CFN_ATAN2:
    CASE_CFN_HYPOT:
    CASE_CFN_POW:
      n_args = 2;
      gcc_fallthrough ();

    CASE_CFN_ACOS:
    CASE_CFN_ACOSH:
    CASE_CFN_ASIN:
    CASE_CFN_ASINH:
    CASE_CFN_ATAN:
    CASE_CFN_ATANH:
    CASE_CFN_CBRT:
    CASE_CFN_COS:
    CASE_CFN_COSH:
    CASE_CFN_ERF:
    CASE_CFN_ERFC:
    CASE_CFN_EXP2:
    CASE_CFN_EXP:
    CASE_CFN_EXPM1:
    CASE_CFN_LGAMMA:
    CASE_CFN_LOG10:
    CASE_CFN_LOG1P:
    CASE_CFN_LOG2:
    CASE_CFN_LOG:
    CASE_CFN_SIN:
    CASE_CFN_SINH:
    CASE_CFN_SQRT:
    CASE_CFN_TAN:
    CASE_CFN_TANH:
      if (el_mode == DFmode && n == 2)
	{
	  bdecl = mathfn_built_in (double_type_node, fn);
	  suffix = "d2";				/* pow -> powd2 */
	}
      else if (el_mode == SFmode && n == 4)
	{
	  bdecl = mathfn_built_in (float_type_node, fn);
	  suffix = "4";					/* powf -> powf4 */
	}
      else
	return NULL_TREE;
      if (!bdecl)
	return NULL_TREE;
      break;

    default:
      return NULL_TREE;
    }

  gcc_assert (suffix != NULL);
  bname = IDENTIFIER_POINTER (DECL_NAME (bdecl));
  if (!bname)
    return NULL_TREE;

  strcpy (name, bname + strlen ("__builtin_"));
  strcat (name, suffix);

  if (n_args == 1)
    fntype = build_function_type_list (type_out, type_in, NULL);
  else if (n_args == 2)
    fntype = build_function_type_list (type_out, type_in, type_in, NULL);
  else
    gcc_unreachable ();

  /* Build a function declaration for the vectorized function.  */
  new_fndecl = build_decl (BUILTINS_LOCATION,
			   FUNCTION_DECL, get_identifier (name), fntype);
  TREE_PUBLIC (new_fndecl) = 1;
  DECL_EXTERNAL (new_fndecl) = 1;
  DECL_IS_NOVOPS (new_fndecl) = 1;
  TREE_READONLY (new_fndecl) = 1;

  return new_fndecl;
}

/* Returns a function decl for a vectorized version of the builtin function
   with builtin function code FN and the result vector type TYPE, or NULL_TREE
   if it is not available.  */

static tree
rs6000_builtin_vectorized_function (unsigned int fn, tree type_out,
				    tree type_in)
{
  machine_mode in_mode, out_mode;
  int in_n, out_n;

  if (TARGET_DEBUG_BUILTIN)
    fprintf (stderr, "rs6000_builtin_vectorized_function (%s, %s, %s)\n",
	     combined_fn_name (combined_fn (fn)),
	     GET_MODE_NAME (TYPE_MODE (type_out)),
	     GET_MODE_NAME (TYPE_MODE (type_in)));

  if (TREE_CODE (type_out) != VECTOR_TYPE
      || TREE_CODE (type_in) != VECTOR_TYPE)
    return NULL_TREE;

  out_mode = TYPE_MODE (TREE_TYPE (type_out));
  out_n = TYPE_VECTOR_SUBPARTS (type_out);
  in_mode = TYPE_MODE (TREE_TYPE (type_in));
  in_n = TYPE_VECTOR_SUBPARTS (type_in);

  switch (fn)
    {
    CASE_CFN_COPYSIGN:
      if (VECTOR_UNIT_VSX_P (V2DFmode)
	  && out_mode == DFmode && out_n == 2
	  && in_mode == DFmode && in_n == 2)
	return rs6000_builtin_decls[VSX_BUILTIN_CPSGNDP];
      if (VECTOR_UNIT_VSX_P (V4SFmode)
	  && out_mode == SFmode && out_n == 4
	  && in_mode == SFmode && in_n == 4)
	return rs6000_builtin_decls[VSX_BUILTIN_CPSGNSP];
      if (VECTOR_UNIT_ALTIVEC_P (V4SFmode)
	  && out_mode == SFmode && out_n == 4
	  && in_mode == SFmode && in_n == 4)
	return rs6000_builtin_decls[ALTIVEC_BUILTIN_COPYSIGN_V4SF];
      break;
    CASE_CFN_CEIL:
      if (VECTOR_UNIT_VSX_P (V2DFmode)
	  && out_mode == DFmode && out_n == 2
	  && in_mode == DFmode && in_n == 2)
	return rs6000_builtin_decls[VSX_BUILTIN_XVRDPIP];
      if (VECTOR_UNIT_VSX_P (V4SFmode)
	  && out_mode == SFmode && out_n == 4
	  && in_mode == SFmode && in_n == 4)
	return rs6000_builtin_decls[VSX_BUILTIN_XVRSPIP];
      if (VECTOR_UNIT_ALTIVEC_P (V4SFmode)
	  && out_mode == SFmode && out_n == 4
	  && in_mode == SFmode && in_n == 4)
	return rs6000_builtin_decls[ALTIVEC_BUILTIN_VRFIP];
      break;
    CASE_CFN_FLOOR:
      if (VECTOR_UNIT_VSX_P (V2DFmode)
	  && out_mode == DFmode && out_n == 2
	  && in_mode == DFmode && in_n == 2)
	return rs6000_builtin_decls[VSX_BUILTIN_XVRDPIM];
      if (VECTOR_UNIT_VSX_P (V4SFmode)
	  && out_mode == SFmode && out_n == 4
	  && in_mode == SFmode && in_n == 4)
	return rs6000_builtin_decls[VSX_BUILTIN_XVRSPIM];
      if (VECTOR_UNIT_ALTIVEC_P (V4SFmode)
	  && out_mode == SFmode && out_n == 4
	  && in_mode == SFmode && in_n == 4)
	return rs6000_builtin_decls[ALTIVEC_BUILTIN_VRFIM];
      break;
    CASE_CFN_FMA:
      if (VECTOR_UNIT_VSX_P (V2DFmode)
	  && out_mode == DFmode && out_n == 2
	  && in_mode == DFmode && in_n == 2)
	return rs6000_builtin_decls[VSX_BUILTIN_XVMADDDP];
      if (VECTOR_UNIT_VSX_P (V4SFmode)
	  && out_mode == SFmode && out_n == 4
	  && in_mode == SFmode && in_n == 4)
	return rs6000_builtin_decls[VSX_BUILTIN_XVMADDSP];
      if (VECTOR_UNIT_ALTIVEC_P (V4SFmode)
	  && out_mode == SFmode && out_n == 4
	  && in_mode == SFmode && in_n == 4)
	return rs6000_builtin_decls[ALTIVEC_BUILTIN_VMADDFP];
      break;
    CASE_CFN_TRUNC:
      if (VECTOR_UNIT_VSX_P (V2DFmode)
	  && out_mode == DFmode && out_n == 2
	  && in_mode == DFmode && in_n == 2)
	return rs6000_builtin_decls[VSX_BUILTIN_XVRDPIZ];
      if (VECTOR_UNIT_VSX_P (V4SFmode)
	  && out_mode == SFmode && out_n == 4
	  && in_mode == SFmode && in_n == 4)
	return rs6000_builtin_decls[VSX_BUILTIN_XVRSPIZ];
      if (VECTOR_UNIT_ALTIVEC_P (V4SFmode)
	  && out_mode == SFmode && out_n == 4
	  && in_mode == SFmode && in_n == 4)
	return rs6000_builtin_decls[ALTIVEC_BUILTIN_VRFIZ];
      break;
    CASE_CFN_NEARBYINT:
      if (VECTOR_UNIT_VSX_P (V2DFmode)
	  && flag_unsafe_math_optimizations
	  && out_mode == DFmode && out_n == 2
	  && in_mode == DFmode && in_n == 2)
	return rs6000_builtin_decls[VSX_BUILTIN_XVRDPI];
      if (VECTOR_UNIT_VSX_P (V4SFmode)
	  && flag_unsafe_math_optimizations
	  && out_mode == SFmode && out_n == 4
	  && in_mode == SFmode && in_n == 4)
	return rs6000_builtin_decls[VSX_BUILTIN_XVRSPI];
      break;
    CASE_CFN_RINT:
      if (VECTOR_UNIT_VSX_P (V2DFmode)
	  && !flag_trapping_math
	  && out_mode == DFmode && out_n == 2
	  && in_mode == DFmode && in_n == 2)
	return rs6000_builtin_decls[VSX_BUILTIN_XVRDPIC];
      if (VECTOR_UNIT_VSX_P (V4SFmode)
	  && !flag_trapping_math
	  && out_mode == SFmode && out_n == 4
	  && in_mode == SFmode && in_n == 4)
	return rs6000_builtin_decls[VSX_BUILTIN_XVRSPIC];
      break;
    default:
      break;
    }

  /* Generate calls to libmass if appropriate.  */
  if (rs6000_veclib_handler)
    return rs6000_veclib_handler (combined_fn (fn), type_out, type_in);

  return NULL_TREE;
}

/* Implement TARGET_VECTORIZE_BUILTIN_MD_VECTORIZED_FUNCTION.  */

static tree
rs6000_builtin_md_vectorized_function (tree fndecl, tree type_out,
				       tree type_in)
{
  machine_mode in_mode, out_mode;
  int in_n, out_n;

  if (TARGET_DEBUG_BUILTIN)
    fprintf (stderr, "rs6000_builtin_md_vectorized_function (%s, %s, %s)\n",
	     IDENTIFIER_POINTER (DECL_NAME (fndecl)),
	     GET_MODE_NAME (TYPE_MODE (type_out)),
	     GET_MODE_NAME (TYPE_MODE (type_in)));

  if (TREE_CODE (type_out) != VECTOR_TYPE
      || TREE_CODE (type_in) != VECTOR_TYPE)
    return NULL_TREE;

  out_mode = TYPE_MODE (TREE_TYPE (type_out));
  out_n = TYPE_VECTOR_SUBPARTS (type_out);
  in_mode = TYPE_MODE (TREE_TYPE (type_in));
  in_n = TYPE_VECTOR_SUBPARTS (type_in);

  enum rs6000_builtins fn
    = (enum rs6000_builtins) DECL_MD_FUNCTION_CODE (fndecl);
  switch (fn)
    {
    case RS6000_BUILTIN_RSQRTF:
      if (VECTOR_UNIT_ALTIVEC_OR_VSX_P (V4SFmode)
	  && out_mode == SFmode && out_n == 4
	  && in_mode == SFmode && in_n == 4)
	return rs6000_builtin_decls[ALTIVEC_BUILTIN_VRSQRTFP];
      break;
    case RS6000_BUILTIN_RSQRT:
      if (VECTOR_UNIT_VSX_P (V2DFmode)
	  && out_mode == DFmode && out_n == 2
	  && in_mode == DFmode && in_n == 2)
	return rs6000_builtin_decls[VSX_BUILTIN_RSQRT_2DF];
      break;
    case RS6000_BUILTIN_RECIPF:
      if (VECTOR_UNIT_ALTIVEC_OR_VSX_P (V4SFmode)
	  && out_mode == SFmode && out_n == 4
	  && in_mode == SFmode && in_n == 4)
	return rs6000_builtin_decls[ALTIVEC_BUILTIN_VRECIPFP];
      break;
    case RS6000_BUILTIN_RECIP:
      if (VECTOR_UNIT_VSX_P (V2DFmode)
	  && out_mode == DFmode && out_n == 2
	  && in_mode == DFmode && in_n == 2)
	return rs6000_builtin_decls[VSX_BUILTIN_RECIP_V2DF];
      break;
    default:
      break;
    }
  return NULL_TREE;
}

/* Default CPU string for rs6000*_file_start functions.  */
static const char *rs6000_default_cpu;

#ifdef USING_ELFOS_H
const char *rs6000_machine;

const char *
rs6000_machine_from_flags (void)
{
  HOST_WIDE_INT flags = rs6000_isa_flags;

  /* Disable the flags that should never influence the .machine selection.  */
  flags &= ~(OPTION_MASK_PPC_GFXOPT | OPTION_MASK_PPC_GPOPT | OPTION_MASK_ISEL);

  if ((flags & (ISA_3_1_MASKS_SERVER & ~ISA_3_0_MASKS_SERVER)) != 0)
    return "power10";
  if ((flags & (ISA_3_0_MASKS_SERVER & ~ISA_2_7_MASKS_SERVER)) != 0)
    return "power9";
  if ((flags & (ISA_2_7_MASKS_SERVER & ~ISA_2_6_MASKS_SERVER)) != 0)
    return "power8";
  if ((flags & (ISA_2_6_MASKS_SERVER & ~ISA_2_5_MASKS_SERVER)) != 0)
    return "power7";
  if ((flags & (ISA_2_5_MASKS_SERVER & ~ISA_2_4_MASKS)) != 0)
    return "power6";
  if ((flags & (ISA_2_4_MASKS & ~ISA_2_1_MASKS)) != 0)
    return "power5";
  if ((flags & ISA_2_1_MASKS) != 0)
    return "power4";
  if ((flags & OPTION_MASK_POWERPC64) != 0)
    return "ppc64";
  return "ppc";
}

void
emit_asm_machine (void)
{
  fprintf (asm_out_file, "\t.machine %s\n", rs6000_machine);
}
#endif

/* Do anything needed at the start of the asm file.  */

static void
rs6000_file_start (void)
{
  char buffer[80];
  const char *start = buffer;
  FILE *file = asm_out_file;

  rs6000_default_cpu = TARGET_CPU_DEFAULT;

  default_file_start ();

  if (flag_verbose_asm)
    {
      sprintf (buffer, "\n%s rs6000/powerpc options:", ASM_COMMENT_START);

      if (rs6000_default_cpu != 0 && rs6000_default_cpu[0] != '\0')
	{
	  fprintf (file, "%s --with-cpu=%s", start, rs6000_default_cpu);
	  start = "";
	}

      if (global_options_set.x_rs6000_cpu_index)
	{
	  fprintf (file, "%s -mcpu=%s", start,
		   processor_target_table[rs6000_cpu_index].name);
	  start = "";
	}

      if (global_options_set.x_rs6000_tune_index)
	{
	  fprintf (file, "%s -mtune=%s", start,
		   processor_target_table[rs6000_tune_index].name);
	  start = "";
	}

      if (PPC405_ERRATUM77)
	{
	  fprintf (file, "%s PPC405CR_ERRATUM77", start);
	  start = "";
	}

#ifdef USING_ELFOS_H
      switch (rs6000_sdata)
	{
	case SDATA_NONE: fprintf (file, "%s -msdata=none", start); start = ""; break;
	case SDATA_DATA: fprintf (file, "%s -msdata=data", start); start = ""; break;
	case SDATA_SYSV: fprintf (file, "%s -msdata=sysv", start); start = ""; break;
	case SDATA_EABI: fprintf (file, "%s -msdata=eabi", start); start = ""; break;
	}

      if (rs6000_sdata && g_switch_value)
	{
	  fprintf (file, "%s -G %d", start,
		   g_switch_value);
	  start = "";
	}
#endif

      if (*start == '\0')
	putc ('\n', file);
    }

#ifdef USING_ELFOS_H
  rs6000_machine = rs6000_machine_from_flags ();
  emit_asm_machine ();
#endif

  if (DEFAULT_ABI == ABI_ELFv2)
    fprintf (file, "\t.abiversion 2\n");
}


/* Return nonzero if this function is known to have a null epilogue.  */

int
direct_return (void)
{
  if (reload_completed)
    {
      rs6000_stack_t *info = rs6000_stack_info ();

      if (info->first_gp_reg_save == 32
	  && info->first_fp_reg_save == 64
	  && info->first_altivec_reg_save == LAST_ALTIVEC_REGNO + 1
	  && ! info->lr_save_p
	  && ! info->cr_save_p
	  && info->vrsave_size == 0
	  && ! info->push_p)
	return 1;
    }

  return 0;
}

/* Helper for num_insns_constant.  Calculate number of instructions to
   load VALUE to a single gpr using combinations of addi, addis, ori,
   oris and sldi instructions.  */

static int
num_insns_constant_gpr (HOST_WIDE_INT value)
{
  /* signed constant loadable with addi */
  if (SIGNED_INTEGER_16BIT_P (value))
    return 1;

  /* constant loadable with addis */
  else if ((value & 0xffff) == 0
	   && (value >> 31 == -1 || value >> 31 == 0))
    return 1;

  /* PADDI can support up to 34 bit signed integers.  */
  else if (TARGET_PREFIXED && SIGNED_INTEGER_34BIT_P (value))
    return 1;

  else if (TARGET_POWERPC64)
    {
      HOST_WIDE_INT low  = ((value & 0xffffffff) ^ 0x80000000) - 0x80000000;
      HOST_WIDE_INT high = value >> 31;

      if (high == 0 || high == -1)
	return 2;

      high >>= 1;

      if (low == 0)
	return num_insns_constant_gpr (high) + 1;
      else if (high == 0)
	return num_insns_constant_gpr (low) + 1;
      else
	return (num_insns_constant_gpr (high)
		+ num_insns_constant_gpr (low) + 1);
    }

  else
    return 2;
}

/* Helper for num_insns_constant.  Allow constants formed by the
   num_insns_constant_gpr sequences, plus li -1, rldicl/rldicr/rlwinm,
   and handle modes that require multiple gprs.  */

static int
num_insns_constant_multi (HOST_WIDE_INT value, machine_mode mode)
{
  int nregs = (GET_MODE_SIZE (mode) + UNITS_PER_WORD - 1) / UNITS_PER_WORD;
  int total = 0;
  while (nregs-- > 0)
    {
      HOST_WIDE_INT low = sext_hwi (value, BITS_PER_WORD);
      int insns = num_insns_constant_gpr (low);
      if (insns > 2
	  /* We won't get more than 2 from num_insns_constant_gpr
	     except when TARGET_POWERPC64 and mode is DImode or
	     wider, so the register mode must be DImode.  */
	  && rs6000_is_valid_and_mask (GEN_INT (low), DImode))
	insns = 2;
      total += insns;
      /* If BITS_PER_WORD is the number of bits in HOST_WIDE_INT, doing
	 it all at once would be UB. */
      value >>= (BITS_PER_WORD - 1);
      value >>= 1;
    }
  return total;
}

/* Return the number of instructions it takes to form a constant in as
   many gprs are needed for MODE.  */

int
num_insns_constant (rtx op, machine_mode mode)
{
  HOST_WIDE_INT val;

  switch (GET_CODE (op))
    {
    case CONST_INT:
      val = INTVAL (op);
      break;

    case CONST_WIDE_INT:
      {
	int insns = 0;
	for (int i = 0; i < CONST_WIDE_INT_NUNITS (op); i++)
	  insns += num_insns_constant_multi (CONST_WIDE_INT_ELT (op, i),
					     DImode);
	return insns;
      }

    case CONST_DOUBLE:
      {
	const struct real_value *rv = CONST_DOUBLE_REAL_VALUE (op);

	if (mode == SFmode || mode == SDmode)
	  {
	    long l;

	    if (mode == SDmode)
	      REAL_VALUE_TO_TARGET_DECIMAL32 (*rv, l);
	    else
	      REAL_VALUE_TO_TARGET_SINGLE (*rv, l);
	    /* See the first define_split in rs6000.md handling a
	       const_double_operand.  */
	    val = l;
	    mode = SImode;
	  }
	else if (mode == DFmode || mode == DDmode)
	  {
	    long l[2];

	    if (mode == DDmode)
	      REAL_VALUE_TO_TARGET_DECIMAL64 (*rv, l);
	    else
	      REAL_VALUE_TO_TARGET_DOUBLE (*rv, l);

	    /* See the second (32-bit) and third (64-bit) define_split
	       in rs6000.md handling a const_double_operand.  */
	    val = (unsigned HOST_WIDE_INT) l[WORDS_BIG_ENDIAN ? 0 : 1] << 32;
	    val |= l[WORDS_BIG_ENDIAN ? 1 : 0] & 0xffffffffUL;
	    mode = DImode;
	  }
	else if (mode == TFmode || mode == TDmode
		 || mode == KFmode || mode == IFmode)
	  {
	    long l[4];
	    int insns;

	    if (mode == TDmode)
	      REAL_VALUE_TO_TARGET_DECIMAL128 (*rv, l);
	    else
	      REAL_VALUE_TO_TARGET_LONG_DOUBLE (*rv, l);

	    val = (unsigned HOST_WIDE_INT) l[WORDS_BIG_ENDIAN ? 0 : 3] << 32;
	    val |= l[WORDS_BIG_ENDIAN ? 1 : 2] & 0xffffffffUL;
	    insns = num_insns_constant_multi (val, DImode);
	    val = (unsigned HOST_WIDE_INT) l[WORDS_BIG_ENDIAN ? 2 : 1] << 32;
	    val |= l[WORDS_BIG_ENDIAN ? 3 : 0] & 0xffffffffUL;
	    insns += num_insns_constant_multi (val, DImode);
	    return insns;
	  }
	else
	  gcc_unreachable ();
      }
      break;

    default:
      gcc_unreachable ();
    }

  return num_insns_constant_multi (val, mode);
}

/* Interpret element ELT of the CONST_VECTOR OP as an integer value.
   If the mode of OP is MODE_VECTOR_INT, this simply returns the
   corresponding element of the vector, but for V4SFmode, the
   corresponding "float" is interpreted as an SImode integer.  */

HOST_WIDE_INT
const_vector_elt_as_int (rtx op, unsigned int elt)
{
  rtx tmp;

  /* We can't handle V2DImode and V2DFmode vector constants here yet.  */
  gcc_assert (GET_MODE (op) != V2DImode
	      && GET_MODE (op) != V2DFmode);

  tmp = CONST_VECTOR_ELT (op, elt);
  if (GET_MODE (op) == V4SFmode)
    tmp = gen_lowpart (SImode, tmp);
  return INTVAL (tmp);
}

/* Return true if OP can be synthesized with a particular vspltisb, vspltish
   or vspltisw instruction.  OP is a CONST_VECTOR.  Which instruction is used
   depends on STEP and COPIES, one of which will be 1.  If COPIES > 1,
   all items are set to the same value and contain COPIES replicas of the
   vsplt's operand; if STEP > 1, one in STEP elements is set to the vsplt's
   operand and the others are set to the value of the operand's msb.  */

static bool
vspltis_constant (rtx op, unsigned step, unsigned copies)
{
  machine_mode mode = GET_MODE (op);
  machine_mode inner = GET_MODE_INNER (mode);

  unsigned i;
  unsigned nunits;
  unsigned bitsize;
  unsigned mask;

  HOST_WIDE_INT val;
  HOST_WIDE_INT splat_val;
  HOST_WIDE_INT msb_val;

  if (mode == V2DImode || mode == V2DFmode || mode == V1TImode)
    return false;

  nunits = GET_MODE_NUNITS (mode);
  bitsize = GET_MODE_BITSIZE (inner);
  mask = GET_MODE_MASK (inner);

  val = const_vector_elt_as_int (op, BYTES_BIG_ENDIAN ? nunits - 1 : 0);
  splat_val = val;
  msb_val = val >= 0 ? 0 : -1;

  /* Construct the value to be splatted, if possible.  If not, return 0.  */
  for (i = 2; i <= copies; i *= 2)
    {
      HOST_WIDE_INT small_val;
      bitsize /= 2;
      small_val = splat_val >> bitsize;
      mask >>= bitsize;
      if (splat_val != ((HOST_WIDE_INT)
          ((unsigned HOST_WIDE_INT) small_val << bitsize)
          | (small_val & mask)))
	return false;
      splat_val = small_val;
    }

  /* Check if SPLAT_VAL can really be the operand of a vspltis[bhw].  */
  if (EASY_VECTOR_15 (splat_val))
    ;

  /* Also check if we can splat, and then add the result to itself.  Do so if
     the value is positive, of if the splat instruction is using OP's mode;
     for splat_val < 0, the splat and the add should use the same mode.  */
  else if (EASY_VECTOR_15_ADD_SELF (splat_val)
           && (splat_val >= 0 || (step == 1 && copies == 1)))
    ;

  /* Also check if are loading up the most significant bit which can be done by
     loading up -1 and shifting the value left by -1.  */
  else if (EASY_VECTOR_MSB (splat_val, inner))
    ;

  else
    return false;

  /* Check if VAL is present in every STEP-th element, and the
     other elements are filled with its most significant bit.  */
  for (i = 1; i < nunits; ++i)
    {
      HOST_WIDE_INT desired_val;
      unsigned elt = BYTES_BIG_ENDIAN ? nunits - 1 - i : i;
      if ((i & (step - 1)) == 0)
	desired_val = val;
      else
	desired_val = msb_val;

      if (desired_val != const_vector_elt_as_int (op, elt))
	return false;
    }

  return true;
}

/* Like vsplitis_constant, but allow the value to be shifted left with a VSLDOI
   instruction, filling in the bottom elements with 0 or -1.

   Return 0 if the constant cannot be generated with VSLDOI.  Return positive
   for the number of zeroes to shift in, or negative for the number of 0xff
   bytes to shift in.

   OP is a CONST_VECTOR.  */

int
vspltis_shifted (rtx op)
{
  machine_mode mode = GET_MODE (op);
  machine_mode inner = GET_MODE_INNER (mode);

  unsigned i, j;
  unsigned nunits;
  unsigned mask;

  HOST_WIDE_INT val;

  if (mode != V16QImode && mode != V8HImode && mode != V4SImode)
    return false;

  /* We need to create pseudo registers to do the shift, so don't recognize
     shift vector constants after reload.  */
  if (!can_create_pseudo_p ())
    return false;

  nunits = GET_MODE_NUNITS (mode);
  mask = GET_MODE_MASK (inner);

  val = const_vector_elt_as_int (op, BYTES_BIG_ENDIAN ? 0 : nunits - 1);

  /* Check if the value can really be the operand of a vspltis[bhw].  */
  if (EASY_VECTOR_15 (val))
    ;

  /* Also check if we are loading up the most significant bit which can be done
     by loading up -1 and shifting the value left by -1.  */
  else if (EASY_VECTOR_MSB (val, inner))
    ;

  else
    return 0;

  /* Check if VAL is present in every STEP-th element until we find elements
     that are 0 or all 1 bits.  */
  for (i = 1; i < nunits; ++i)
    {
      unsigned elt = BYTES_BIG_ENDIAN ? i : nunits - 1 - i;
      HOST_WIDE_INT elt_val = const_vector_elt_as_int (op, elt);

      /* If the value isn't the splat value, check for the remaining elements
	 being 0/-1.  */
      if (val != elt_val)
	{
	  if (elt_val == 0)
	    {
	      for (j = i+1; j < nunits; ++j)
		{
		  unsigned elt2 = BYTES_BIG_ENDIAN ? j : nunits - 1 - j;
		  if (const_vector_elt_as_int (op, elt2) != 0)
		    return 0;
		}

	      return (nunits - i) * GET_MODE_SIZE (inner);
	    }

	  else if ((elt_val & mask) == mask)
	    {
	      for (j = i+1; j < nunits; ++j)
		{
		  unsigned elt2 = BYTES_BIG_ENDIAN ? j : nunits - 1 - j;
		  if ((const_vector_elt_as_int (op, elt2) & mask) != mask)
		    return 0;
		}

	      return -((nunits - i) * GET_MODE_SIZE (inner));
	    }

	  else
	    return 0;
	}
    }

  /* If all elements are equal, we don't need to do VLSDOI.  */
  return 0;
}


/* Return true if OP is of the given MODE and can be synthesized
   with a vspltisb, vspltish or vspltisw.  */

bool
easy_altivec_constant (rtx op, machine_mode mode)
{
  unsigned step, copies;

  if (mode == VOIDmode)
    mode = GET_MODE (op);
  else if (mode != GET_MODE (op))
    return false;

  /* V2DI/V2DF was added with VSX.  Only allow 0 and all 1's as easy
     constants.  */
  if (mode == V2DFmode)
    return zero_constant (op, mode);

  else if (mode == V2DImode)
    {
      if (!CONST_INT_P (CONST_VECTOR_ELT (op, 0))
	  || !CONST_INT_P (CONST_VECTOR_ELT (op, 1)))
	return false;

      if (zero_constant (op, mode))
	return true;

      if (INTVAL (CONST_VECTOR_ELT (op, 0)) == -1
	  && INTVAL (CONST_VECTOR_ELT (op, 1)) == -1)
	return true;

      return false;
    }

  /* V1TImode is a special container for TImode.  Ignore for now.  */
  else if (mode == V1TImode)
    return false;

  /* Start with a vspltisw.  */
  step = GET_MODE_NUNITS (mode) / 4;
  copies = 1;

  if (vspltis_constant (op, step, copies))
    return true;

  /* Then try with a vspltish.  */
  if (step == 1)
    copies <<= 1;
  else
    step >>= 1;

  if (vspltis_constant (op, step, copies))
    return true;

  /* And finally a vspltisb.  */
  if (step == 1)
    copies <<= 1;
  else
    step >>= 1;

  if (vspltis_constant (op, step, copies))
    return true;

  if (vspltis_shifted (op) != 0)
    return true;

  return false;
}

/* Generate a VEC_DUPLICATE representing a vspltis[bhw] instruction whose
   result is OP.  Abort if it is not possible.  */

rtx
gen_easy_altivec_constant (rtx op)
{
  machine_mode mode = GET_MODE (op);
  int nunits = GET_MODE_NUNITS (mode);
  rtx val = CONST_VECTOR_ELT (op, BYTES_BIG_ENDIAN ? nunits - 1 : 0);
  unsigned step = nunits / 4;
  unsigned copies = 1;

  /* Start with a vspltisw.  */
  if (vspltis_constant (op, step, copies))
    return gen_rtx_VEC_DUPLICATE (V4SImode, gen_lowpart (SImode, val));

  /* Then try with a vspltish.  */
  if (step == 1)
    copies <<= 1;
  else
    step >>= 1;

  if (vspltis_constant (op, step, copies))
    return gen_rtx_VEC_DUPLICATE (V8HImode, gen_lowpart (HImode, val));

  /* And finally a vspltisb.  */
  if (step == 1)
    copies <<= 1;
  else
    step >>= 1;

  if (vspltis_constant (op, step, copies))
    return gen_rtx_VEC_DUPLICATE (V16QImode, gen_lowpart (QImode, val));

  gcc_unreachable ();
}

/* Return true if OP is of the given MODE and can be synthesized with ISA 3.0
   instructions (xxspltib, vupkhsb/vextsb2w/vextb2d).

   Return the number of instructions needed (1 or 2) into the address pointed
   via NUM_INSNS_PTR.

   Return the constant that is being split via CONSTANT_PTR.  */

bool
xxspltib_constant_p (rtx op,
		     machine_mode mode,
		     int *num_insns_ptr,
		     int *constant_ptr)
{
  size_t nunits = GET_MODE_NUNITS (mode);
  size_t i;
  HOST_WIDE_INT value;
  rtx element;

  /* Set the returned values to out of bound values.  */
  *num_insns_ptr = -1;
  *constant_ptr = 256;

  if (!TARGET_P9_VECTOR)
    return false;

  if (mode == VOIDmode)
    mode = GET_MODE (op);

  else if (mode != GET_MODE (op) && GET_MODE (op) != VOIDmode)
    return false;

  /* Handle (vec_duplicate <constant>).  */
  if (GET_CODE (op) == VEC_DUPLICATE)
    {
      if (mode != V16QImode && mode != V8HImode && mode != V4SImode
	  && mode != V2DImode)
	return false;

      element = XEXP (op, 0);
      if (!CONST_INT_P (element))
	return false;

      value = INTVAL (element);
      if (!IN_RANGE (value, -128, 127))
	return false;
    }

  /* Handle (const_vector [...]).  */
  else if (GET_CODE (op) == CONST_VECTOR)
    {
      if (mode != V16QImode && mode != V8HImode && mode != V4SImode
	  && mode != V2DImode)
	return false;

      element = CONST_VECTOR_ELT (op, 0);
      if (!CONST_INT_P (element))
	return false;

      value = INTVAL (element);
      if (!IN_RANGE (value, -128, 127))
	return false;

      for (i = 1; i < nunits; i++)
	{
	  element = CONST_VECTOR_ELT (op, i);
	  if (!CONST_INT_P (element))
	    return false;

	  if (value != INTVAL (element))
	    return false;
	}
    }

  /* Handle integer constants being loaded into the upper part of the VSX
     register as a scalar.  If the value isn't 0/-1, only allow it if the mode
     can go in Altivec registers.  Prefer VSPLTISW/VUPKHSW over XXSPLITIB.  */
  else if (CONST_INT_P (op))
    {
      if (!SCALAR_INT_MODE_P (mode))
	return false;

      value = INTVAL (op);
      if (!IN_RANGE (value, -128, 127))
	return false;

      if (!IN_RANGE (value, -1, 0))
	{
	  if (!(reg_addr[mode].addr_mask[RELOAD_REG_VMX] & RELOAD_REG_VALID))
	    return false;

	  if (EASY_VECTOR_15 (value))
	    return false;
	}
    }

  else
    return false;

  /* See if we could generate vspltisw/vspltish directly instead of xxspltib +
     sign extend.  Special case 0/-1 to allow getting any VSX register instead
     of an Altivec register.  */
  if ((mode == V4SImode || mode == V8HImode) && !IN_RANGE (value, -1, 0)
      && EASY_VECTOR_15 (value))
    return false;

  /* Return # of instructions and the constant byte for XXSPLTIB.  */
  if (mode == V16QImode)
    *num_insns_ptr = 1;

  else if (IN_RANGE (value, -1, 0))
    *num_insns_ptr = 1;

  else
    *num_insns_ptr = 2;

  *constant_ptr = (int) value;
  return true;
}

const char *
output_vec_const_move (rtx *operands)
{
  int shift;
  machine_mode mode;
  rtx dest, vec;

  dest = operands[0];
  vec = operands[1];
  mode = GET_MODE (dest);

  if (TARGET_VSX)
    {
      bool dest_vmx_p = ALTIVEC_REGNO_P (REGNO (dest));
      int xxspltib_value = 256;
      int num_insns = -1;

      if (zero_constant (vec, mode))
	{
	  if (TARGET_P9_VECTOR)
	    return "xxspltib %x0,0";

	  else if (dest_vmx_p)
	    return "vspltisw %0,0";

	  else
	    return "xxlxor %x0,%x0,%x0";
	}

      if (all_ones_constant (vec, mode))
	{
	  if (TARGET_P9_VECTOR)
	    return "xxspltib %x0,255";

	  else if (dest_vmx_p)
	    return "vspltisw %0,-1";

	  else if (TARGET_P8_VECTOR)
	    return "xxlorc %x0,%x0,%x0";

	  else
	    gcc_unreachable ();
	}

      if (TARGET_P9_VECTOR
	  && xxspltib_constant_p (vec, mode, &num_insns, &xxspltib_value))
	{
	  if (num_insns == 1)
	    {
	      operands[2] = GEN_INT (xxspltib_value & 0xff);
	      return "xxspltib %x0,%2";
	    }

	  return "#";
	}
    }

  if (TARGET_ALTIVEC)
    {
      rtx splat_vec;

      gcc_assert (ALTIVEC_REGNO_P (REGNO (dest)));
      if (zero_constant (vec, mode))
	return "vspltisw %0,0";

      if (all_ones_constant (vec, mode))
	return "vspltisw %0,-1";

      /* Do we need to construct a value using VSLDOI?  */
      shift = vspltis_shifted (vec);
      if (shift != 0)
	return "#";

      splat_vec = gen_easy_altivec_constant (vec);
      gcc_assert (GET_CODE (splat_vec) == VEC_DUPLICATE);
      operands[1] = XEXP (splat_vec, 0);
      if (!EASY_VECTOR_15 (INTVAL (operands[1])))
	return "#";

      switch (GET_MODE (splat_vec))
	{
	case E_V4SImode:
	  return "vspltisw %0,%1";

	case E_V8HImode:
	  return "vspltish %0,%1";

	case E_V16QImode:
	  return "vspltisb %0,%1";

	default:
	  gcc_unreachable ();
	}
    }

  gcc_unreachable ();
}

/* Initialize vector TARGET to VALS.  */

void
rs6000_expand_vector_init (rtx target, rtx vals)
{
  machine_mode mode = GET_MODE (target);
  machine_mode inner_mode = GET_MODE_INNER (mode);
  int n_elts = GET_MODE_NUNITS (mode);
  int n_var = 0, one_var = -1;
  bool all_same = true, all_const_zero = true;
  rtx x, mem;
  int i;

  for (i = 0; i < n_elts; ++i)
    {
      x = XVECEXP (vals, 0, i);
      if (!(CONST_SCALAR_INT_P (x) || CONST_DOUBLE_P (x) || CONST_FIXED_P (x)))
	++n_var, one_var = i;
      else if (x != CONST0_RTX (inner_mode))
	all_const_zero = false;

      if (i > 0 && !rtx_equal_p (x, XVECEXP (vals, 0, 0)))
	all_same = false;
    }

  if (n_var == 0)
    {
      rtx const_vec = gen_rtx_CONST_VECTOR (mode, XVEC (vals, 0));
      bool int_vector_p = (GET_MODE_CLASS (mode) == MODE_VECTOR_INT);
      if ((int_vector_p || TARGET_VSX) && all_const_zero)
	{
	  /* Zero register.  */
	  emit_move_insn (target, CONST0_RTX (mode));
	  return;
	}
      else if (int_vector_p && easy_vector_constant (const_vec, mode))
	{
	  /* Splat immediate.  */
	  emit_insn (gen_rtx_SET (target, const_vec));
	  return;
	}
      else
	{
	  /* Load from constant pool.  */
	  emit_move_insn (target, const_vec);
	  return;
	}
    }

  /* Double word values on VSX can use xxpermdi or lxvdsx.  */
  if (VECTOR_MEM_VSX_P (mode) && (mode == V2DFmode || mode == V2DImode))
    {
      rtx op[2];
      size_t i;
      size_t num_elements = all_same ? 1 : 2;
      for (i = 0; i < num_elements; i++)
	{
	  op[i] = XVECEXP (vals, 0, i);
	  /* Just in case there is a SUBREG with a smaller mode, do a
	     conversion.  */
	  if (GET_MODE (op[i]) != inner_mode)
	    {
	      rtx tmp = gen_reg_rtx (inner_mode);
	      convert_move (tmp, op[i], 0);
	      op[i] = tmp;
	    }
	  /* Allow load with splat double word.  */
	  else if (MEM_P (op[i]))
	    {
	      if (!all_same)
		op[i] = force_reg (inner_mode, op[i]);
	    }
	  else if (!REG_P (op[i]))
	    op[i] = force_reg (inner_mode, op[i]);
	}

      if (all_same)
	{
	  if (mode == V2DFmode)
	    emit_insn (gen_vsx_splat_v2df (target, op[0]));
	  else
	    emit_insn (gen_vsx_splat_v2di (target, op[0]));
	}
      else
	{
	  if (mode == V2DFmode)
	    emit_insn (gen_vsx_concat_v2df (target, op[0], op[1]));
	  else
	    emit_insn (gen_vsx_concat_v2di (target, op[0], op[1]));
	}
      return;
    }

  /* Special case initializing vector int if we are on 64-bit systems with
     direct move or we have the ISA 3.0 instructions.  */
  if (mode == V4SImode  && VECTOR_MEM_VSX_P (V4SImode)
      && TARGET_DIRECT_MOVE_64BIT)
    {
      if (all_same)
	{
	  rtx element0 = XVECEXP (vals, 0, 0);
	  if (MEM_P (element0))
	    element0 = rs6000_force_indexed_or_indirect_mem (element0);
	  else
	    element0 = force_reg (SImode, element0);

	  if (TARGET_P9_VECTOR)
	    emit_insn (gen_vsx_splat_v4si (target, element0));
	  else
	    {
	      rtx tmp = gen_reg_rtx (DImode);
	      emit_insn (gen_zero_extendsidi2 (tmp, element0));
	      emit_insn (gen_vsx_splat_v4si_di (target, tmp));
	    }
	  return;
	}
      else
	{
	  rtx elements[4];
	  size_t i;

	  for (i = 0; i < 4; i++)
	    elements[i] = force_reg (SImode, XVECEXP (vals, 0, i));

	  emit_insn (gen_vsx_init_v4si (target, elements[0], elements[1],
					elements[2], elements[3]));
	  return;
	}
    }

  /* With single precision floating point on VSX, know that internally single
     precision is actually represented as a double, and either make 2 V2DF
     vectors, and convert these vectors to single precision, or do one
     conversion, and splat the result to the other elements.  */
  if (mode == V4SFmode && VECTOR_MEM_VSX_P (V4SFmode))
    {
      if (all_same)
	{
	  rtx element0 = XVECEXP (vals, 0, 0);

	  if (TARGET_P9_VECTOR)
	    {
	      if (MEM_P (element0))
		element0 = rs6000_force_indexed_or_indirect_mem (element0);

	      emit_insn (gen_vsx_splat_v4sf (target, element0));
	    }

	  else
	    {
	      rtx freg = gen_reg_rtx (V4SFmode);
	      rtx sreg = force_reg (SFmode, element0);
	      rtx cvt  = (TARGET_XSCVDPSPN
			  ? gen_vsx_xscvdpspn_scalar (freg, sreg)
			  : gen_vsx_xscvdpsp_scalar (freg, sreg));

	      emit_insn (cvt);
	      emit_insn (gen_vsx_xxspltw_v4sf_direct (target, freg,
						      const0_rtx));
	    }
	}
      else
	{
	  rtx dbl_even = gen_reg_rtx (V2DFmode);
	  rtx dbl_odd  = gen_reg_rtx (V2DFmode);
	  rtx flt_even = gen_reg_rtx (V4SFmode);
	  rtx flt_odd  = gen_reg_rtx (V4SFmode);
	  rtx op0 = force_reg (SFmode, XVECEXP (vals, 0, 0));
	  rtx op1 = force_reg (SFmode, XVECEXP (vals, 0, 1));
	  rtx op2 = force_reg (SFmode, XVECEXP (vals, 0, 2));
	  rtx op3 = force_reg (SFmode, XVECEXP (vals, 0, 3));

	  /* Use VMRGEW if we can instead of doing a permute.  */
	  if (TARGET_P8_VECTOR)
	    {
	      emit_insn (gen_vsx_concat_v2sf (dbl_even, op0, op2));
	      emit_insn (gen_vsx_concat_v2sf (dbl_odd, op1, op3));
	      emit_insn (gen_vsx_xvcvdpsp (flt_even, dbl_even));
	      emit_insn (gen_vsx_xvcvdpsp (flt_odd, dbl_odd));
	      if (BYTES_BIG_ENDIAN)
		emit_insn (gen_p8_vmrgew_v4sf_direct (target, flt_even, flt_odd));
	      else
		emit_insn (gen_p8_vmrgew_v4sf_direct (target, flt_odd, flt_even));
	    }
	  else
	    {
	      emit_insn (gen_vsx_concat_v2sf (dbl_even, op0, op1));
	      emit_insn (gen_vsx_concat_v2sf (dbl_odd, op2, op3));
	      emit_insn (gen_vsx_xvcvdpsp (flt_even, dbl_even));
	      emit_insn (gen_vsx_xvcvdpsp (flt_odd, dbl_odd));
	      rs6000_expand_extract_even (target, flt_even, flt_odd);
	    }
	}
      return;
    }

  /* Special case initializing vector short/char that are splats if we are on
     64-bit systems with direct move.  */
  if (all_same && TARGET_DIRECT_MOVE_64BIT
      && (mode == V16QImode || mode == V8HImode))
    {
      rtx op0 = XVECEXP (vals, 0, 0);
      rtx di_tmp = gen_reg_rtx (DImode);

      if (!REG_P (op0))
	op0 = force_reg (GET_MODE_INNER (mode), op0);

      if (mode == V16QImode)
	{
	  emit_insn (gen_zero_extendqidi2 (di_tmp, op0));
	  emit_insn (gen_vsx_vspltb_di (target, di_tmp));
	  return;
	}

      if (mode == V8HImode)
	{
	  emit_insn (gen_zero_extendhidi2 (di_tmp, op0));
	  emit_insn (gen_vsx_vsplth_di (target, di_tmp));
	  return;
	}
    }

  /* Store value to stack temp.  Load vector element.  Splat.  However, splat
     of 64-bit items is not supported on Altivec.  */
  if (all_same && GET_MODE_SIZE (inner_mode) <= 4)
    {
      mem = assign_stack_temp (mode, GET_MODE_SIZE (inner_mode));
      emit_move_insn (adjust_address_nv (mem, inner_mode, 0),
		      XVECEXP (vals, 0, 0));
      x = gen_rtx_UNSPEC (VOIDmode,
			  gen_rtvec (1, const0_rtx), UNSPEC_LVE);
      emit_insn (gen_rtx_PARALLEL (VOIDmode,
				   gen_rtvec (2,
					      gen_rtx_SET (target, mem),
					      x)));
      x = gen_rtx_VEC_SELECT (inner_mode, target,
			      gen_rtx_PARALLEL (VOIDmode,
						gen_rtvec (1, const0_rtx)));
      emit_insn (gen_rtx_SET (target, gen_rtx_VEC_DUPLICATE (mode, x)));
      return;
    }

  /* One field is non-constant.  Load constant then overwrite
     varying field.  */
  if (n_var == 1)
    {
      rtx copy = copy_rtx (vals);

      /* Load constant part of vector, substitute neighboring value for
	 varying element.  */
      XVECEXP (copy, 0, one_var) = XVECEXP (vals, 0, (one_var + 1) % n_elts);
      rs6000_expand_vector_init (target, copy);

      /* Insert variable.  */
      rs6000_expand_vector_set (target, XVECEXP (vals, 0, one_var), one_var);
      return;
    }

  /* Construct the vector in memory one field at a time
     and load the whole vector.  */
  mem = assign_stack_temp (mode, GET_MODE_SIZE (mode));
  for (i = 0; i < n_elts; i++)
    emit_move_insn (adjust_address_nv (mem, inner_mode,
				    i * GET_MODE_SIZE (inner_mode)),
		    XVECEXP (vals, 0, i));
  emit_move_insn (target, mem);
}

/* Set field ELT of TARGET to VAL.  */

void
rs6000_expand_vector_set (rtx target, rtx val, int elt)
{
  machine_mode mode = GET_MODE (target);
  machine_mode inner_mode = GET_MODE_INNER (mode);
  rtx reg = gen_reg_rtx (mode);
  rtx mask, mem, x;
  int width = GET_MODE_SIZE (inner_mode);
  int i;

  val = force_reg (GET_MODE (val), val);

  if (VECTOR_MEM_VSX_P (mode))
    {
      rtx insn = NULL_RTX;
      rtx elt_rtx = GEN_INT (elt);

      if (mode == V2DFmode)
	insn = gen_vsx_set_v2df (target, target, val, elt_rtx);

      else if (mode == V2DImode)
	insn = gen_vsx_set_v2di (target, target, val, elt_rtx);

      else if (TARGET_P9_VECTOR && TARGET_POWERPC64)
	{
	  if (mode == V4SImode)
	    insn = gen_vsx_set_v4si_p9 (target, target, val, elt_rtx);
	  else if (mode == V8HImode)
	    insn = gen_vsx_set_v8hi_p9 (target, target, val, elt_rtx);
	  else if (mode == V16QImode)
	    insn = gen_vsx_set_v16qi_p9 (target, target, val, elt_rtx);
	  else if (mode == V4SFmode)
	    insn = gen_vsx_set_v4sf_p9 (target, target, val, elt_rtx);
	}

      if (insn)
	{
	  emit_insn (insn);
	  return;
	}
    }

  /* Simplify setting single element vectors like V1TImode.  */
  if (GET_MODE_SIZE (mode) == GET_MODE_SIZE (inner_mode) && elt == 0)
    {
      emit_move_insn (target, gen_lowpart (mode, val));
      return;
    }

  /* Load single variable value.  */
  mem = assign_stack_temp (mode, GET_MODE_SIZE (inner_mode));
  emit_move_insn (adjust_address_nv (mem, inner_mode, 0), val);
  x = gen_rtx_UNSPEC (VOIDmode,
		      gen_rtvec (1, const0_rtx), UNSPEC_LVE);
  emit_insn (gen_rtx_PARALLEL (VOIDmode,
			       gen_rtvec (2,
					  gen_rtx_SET (reg, mem),
					  x)));

  /* Linear sequence.  */
  mask = gen_rtx_PARALLEL (V16QImode, rtvec_alloc (16));
  for (i = 0; i < 16; ++i)
    XVECEXP (mask, 0, i) = GEN_INT (i);

  /* Set permute mask to insert element into target.  */
  for (i = 0; i < width; ++i)
    XVECEXP (mask, 0, elt*width + i)
      = GEN_INT (i + 0x10);
  x = gen_rtx_CONST_VECTOR (V16QImode, XVEC (mask, 0));

  if (BYTES_BIG_ENDIAN)
    x = gen_rtx_UNSPEC (mode,
			gen_rtvec (3, target, reg,
				   force_reg (V16QImode, x)),
			UNSPEC_VPERM);
  else
    {
      if (TARGET_P9_VECTOR)
	x = gen_rtx_UNSPEC (mode,
			    gen_rtvec (3, reg, target,
				       force_reg (V16QImode, x)),
			    UNSPEC_VPERMR);
      else
	{
	  /* Invert selector.  We prefer to generate VNAND on P8 so
	     that future fusion opportunities can kick in, but must
	     generate VNOR elsewhere.  */
	  rtx notx = gen_rtx_NOT (V16QImode, force_reg (V16QImode, x));
	  rtx iorx = (TARGET_P8_VECTOR
		      ? gen_rtx_IOR (V16QImode, notx, notx)
		      : gen_rtx_AND (V16QImode, notx, notx));
	  rtx tmp = gen_reg_rtx (V16QImode);
	  emit_insn (gen_rtx_SET (tmp, iorx));

	  /* Permute with operands reversed and adjusted selector.  */
	  x = gen_rtx_UNSPEC (mode, gen_rtvec (3, reg, target, tmp),
			      UNSPEC_VPERM);
	}
    }

  emit_insn (gen_rtx_SET (target, x));
}

/* Extract field ELT from VEC into TARGET.  */

void
rs6000_expand_vector_extract (rtx target, rtx vec, rtx elt)
{
  machine_mode mode = GET_MODE (vec);
  machine_mode inner_mode = GET_MODE_INNER (mode);
  rtx mem;

  if (VECTOR_MEM_VSX_P (mode) && CONST_INT_P (elt))
    {
      switch (mode)
	{
	default:
	  break;
	case E_V1TImode:
	  emit_move_insn (target, gen_lowpart (TImode, vec));
	  break;
	case E_V2DFmode:
	  emit_insn (gen_vsx_extract_v2df (target, vec, elt));
	  return;
	case E_V2DImode:
	  emit_insn (gen_vsx_extract_v2di (target, vec, elt));
	  return;
	case E_V4SFmode:
	  emit_insn (gen_vsx_extract_v4sf (target, vec, elt));
	  return;
	case E_V16QImode:
	  if (TARGET_DIRECT_MOVE_64BIT)
	    {
	      emit_insn (gen_vsx_extract_v16qi (target, vec, elt));
	      return;
	    }
	  else
	    break;
	case E_V8HImode:
	  if (TARGET_DIRECT_MOVE_64BIT)
	    {
	      emit_insn (gen_vsx_extract_v8hi (target, vec, elt));
	      return;
	    }
	  else
	    break;
	case E_V4SImode:
	  if (TARGET_DIRECT_MOVE_64BIT)
	    {
	      emit_insn (gen_vsx_extract_v4si (target, vec, elt));
	      return;
	    }
	  break;
	}
    }
  else if (VECTOR_MEM_VSX_P (mode) && !CONST_INT_P (elt)
	   && TARGET_DIRECT_MOVE_64BIT)
    {
      if (GET_MODE (elt) != DImode)
	{
	  rtx tmp = gen_reg_rtx (DImode);
	  convert_move (tmp, elt, 0);
	  elt = tmp;
	}
      else if (!REG_P (elt))
	elt = force_reg (DImode, elt);

      switch (mode)
	{
	case E_V1TImode:
	  emit_move_insn (target, gen_lowpart (TImode, vec));
	  return;

	case E_V2DFmode:
	  emit_insn (gen_vsx_extract_v2df_var (target, vec, elt));
	  return;

	case E_V2DImode:
	  emit_insn (gen_vsx_extract_v2di_var (target, vec, elt));
	  return;

	case E_V4SFmode:
	  emit_insn (gen_vsx_extract_v4sf_var (target, vec, elt));
	  return;

	case E_V4SImode:
	  emit_insn (gen_vsx_extract_v4si_var (target, vec, elt));
	  return;

	case E_V8HImode:
	  emit_insn (gen_vsx_extract_v8hi_var (target, vec, elt));
	  return;

	case E_V16QImode:
	  emit_insn (gen_vsx_extract_v16qi_var (target, vec, elt));
	  return;

	default:
	  gcc_unreachable ();
	}
    }

  /* Allocate mode-sized buffer.  */
  mem = assign_stack_temp (mode, GET_MODE_SIZE (mode));

  emit_move_insn (mem, vec);
  if (CONST_INT_P (elt))
    {
      int modulo_elt = INTVAL (elt) % GET_MODE_NUNITS (mode);

      /* Add offset to field within buffer matching vector element.  */
      mem = adjust_address_nv (mem, inner_mode,
			       modulo_elt * GET_MODE_SIZE (inner_mode));
      emit_move_insn (target, adjust_address_nv (mem, inner_mode, 0));
    }
  else
    {
      unsigned int ele_size = GET_MODE_SIZE (inner_mode);
      rtx num_ele_m1 = GEN_INT (GET_MODE_NUNITS (mode) - 1);
      rtx new_addr = gen_reg_rtx (Pmode);

      elt = gen_rtx_AND (Pmode, elt, num_ele_m1);
      if (ele_size > 1)
	elt = gen_rtx_MULT (Pmode, elt, GEN_INT (ele_size));
      new_addr = gen_rtx_PLUS (Pmode, XEXP (mem, 0), elt);
      new_addr = change_address (mem, inner_mode, new_addr);
      emit_move_insn (target, new_addr);
    }
}

/* Return the offset within a memory object (MEM) of a vector type to a given
   element within the vector (ELEMENT) with an element size (SCALAR_SIZE).  If
   the element is constant, we return a constant integer.

   Otherwise, we use a base register temporary to calculate the offset after
   masking it to fit within the bounds of the vector and scaling it.  The
   masking is required by the 64-bit ELF version 2 ABI for the vec_extract
   built-in function.  */

static rtx
get_vector_offset (rtx mem, rtx element, rtx base_tmp, unsigned scalar_size)
{
  if (CONST_INT_P (element))
    return GEN_INT (INTVAL (element) * scalar_size);

  /* All insns should use the 'Q' constraint (address is a single register) if
     the element number is not a constant.  */
  gcc_assert (satisfies_constraint_Q (mem));

  /* Mask the element to make sure the element number is between 0 and the
     maximum number of elements - 1 so that we don't generate an address
     outside the vector.  */
  rtx num_ele_m1 = GEN_INT (GET_MODE_NUNITS (GET_MODE (mem)) - 1);
  rtx and_op = gen_rtx_AND (Pmode, element, num_ele_m1);
  emit_insn (gen_rtx_SET (base_tmp, and_op));

  /* Shift the element to get the byte offset from the element number.  */
  int shift = exact_log2 (scalar_size);
  gcc_assert (shift >= 0);

  if (shift > 0)
    {
      rtx shift_op = gen_rtx_ASHIFT (Pmode, base_tmp, GEN_INT (shift));
      emit_insn (gen_rtx_SET (base_tmp, shift_op));
    }

  return base_tmp;
}

/* Helper function update PC-relative addresses when we are adjusting a memory
   address (ADDR) to a vector to point to a scalar field within the vector with
   a constant offset (ELEMENT_OFFSET).  If the address is not valid, we can
   use the base register temporary (BASE_TMP) to form the address.  */

static rtx
adjust_vec_address_pcrel (rtx addr, rtx element_offset, rtx base_tmp)
{
  rtx new_addr = NULL;

  gcc_assert (CONST_INT_P (element_offset));

  if (GET_CODE (addr) == CONST)
    addr = XEXP (addr, 0);

  if (GET_CODE (addr) == PLUS)
    {
      rtx op0 = XEXP (addr, 0);
      rtx op1 = XEXP (addr, 1);

      if (CONST_INT_P (op1))
	{
	  HOST_WIDE_INT offset
	    = INTVAL (XEXP (addr, 1)) + INTVAL (element_offset);

	  if (offset == 0)
	    new_addr = op0;

	  else
	    {
	      rtx plus = gen_rtx_PLUS (Pmode, op0, GEN_INT (offset));
	      new_addr = gen_rtx_CONST (Pmode, plus);
	    }
	}

      else
	{
	  emit_move_insn (base_tmp, addr);
	  new_addr = gen_rtx_PLUS (Pmode, base_tmp, element_offset);
	}
    }

  else if (SYMBOL_REF_P (addr) || LABEL_REF_P (addr))
    {
      rtx plus = gen_rtx_PLUS (Pmode, addr, element_offset);
      new_addr = gen_rtx_CONST (Pmode, plus);
    }

  else
    gcc_unreachable ();

  return new_addr;
}

/* Adjust a memory address (MEM) of a vector type to point to a scalar field
   within the vector (ELEMENT) with a mode (SCALAR_MODE).  Use a base register
   temporary (BASE_TMP) to fixup the address.  Return the new memory address
   that is valid for reads or writes to a given register (SCALAR_REG).

   This function is expected to be called after reload is completed when we are
   splitting insns.  The temporary BASE_TMP might be set multiple times with
   this code.  */

rtx
rs6000_adjust_vec_address (rtx scalar_reg,
			   rtx mem,
			   rtx element,
			   rtx base_tmp,
			   machine_mode scalar_mode)
{
  unsigned scalar_size = GET_MODE_SIZE (scalar_mode);
  rtx addr = XEXP (mem, 0);
  rtx new_addr;

  gcc_assert (!reg_mentioned_p (base_tmp, addr));
  gcc_assert (!reg_mentioned_p (base_tmp, element));

  /* Vector addresses should not have PRE_INC, PRE_DEC, or PRE_MODIFY.  */
  gcc_assert (GET_RTX_CLASS (GET_CODE (addr)) != RTX_AUTOINC);

  /* Calculate what we need to add to the address to get the element
     address.  */
  rtx element_offset = get_vector_offset (mem, element, base_tmp, scalar_size);

  /* Create the new address pointing to the element within the vector.  If we
     are adding 0, we don't have to change the address.  */
  if (element_offset == const0_rtx)
    new_addr = addr;

  /* A simple indirect address can be converted into a reg + offset
     address.  */
  else if (REG_P (addr) || SUBREG_P (addr))
    new_addr = gen_rtx_PLUS (Pmode, addr, element_offset);

  /* For references to local static variables, fold a constant offset into the
     address.  */
  else if (pcrel_local_address (addr, Pmode) && CONST_INT_P (element_offset))
    new_addr = adjust_vec_address_pcrel (addr, element_offset, base_tmp);

  /* Optimize D-FORM addresses with constant offset with a constant element, to
     include the element offset in the address directly.  */
  else if (GET_CODE (addr) == PLUS)
    {
      rtx op0 = XEXP (addr, 0);
      rtx op1 = XEXP (addr, 1);

      gcc_assert (REG_P (op0) || SUBREG_P (op0));
      if (CONST_INT_P (op1) && CONST_INT_P (element_offset))
	{
	  /* op0 should never be r0, because r0+offset is not valid.  But it
	     doesn't hurt to make sure it is not r0.  */
	  gcc_assert (reg_or_subregno (op0) != 0);

	  /* D-FORM address with constant element number.  */
	  HOST_WIDE_INT offset = INTVAL (op1) + INTVAL (element_offset);
	  rtx offset_rtx = GEN_INT (offset);
	  new_addr = gen_rtx_PLUS (Pmode, op0, offset_rtx);
	}
      else
	{
	  /* If we don't have a D-FORM address with a constant element number,
	     add the two elements in the current address.  Then add the offset.

	     Previously, we tried to add the offset to OP1 and change the
	     address to an X-FORM format adding OP0 and BASE_TMP, but it became
	     complicated because we had to verify that op1 was not GPR0 and we
	     had a constant element offset (due to the way ADDI is defined).
	     By doing the add of OP0 and OP1 first, and then adding in the
	     offset, it has the benefit that if D-FORM instructions are
	     allowed, the offset is part of the memory access to the vector
	     element. */
	  emit_insn (gen_rtx_SET (base_tmp, gen_rtx_PLUS (Pmode, op0, op1)));
	  new_addr = gen_rtx_PLUS (Pmode, base_tmp, element_offset);
	}
    }

  else
    {
      emit_move_insn (base_tmp, addr);
      new_addr = gen_rtx_PLUS (Pmode, base_tmp, element_offset);
    }

    /* If the address isn't valid, move the address into the temporary base
       register.  Some reasons it could not be valid include:

       The address offset overflowed the 16 or 34 bit offset size;
       We need to use a DS-FORM load, and the bottom 2 bits are non-zero;
       We need to use a DQ-FORM load, and the bottom 4 bits are non-zero;
       Only X_FORM loads can be done, and the address is D_FORM.  */

  enum insn_form iform
    = address_to_insn_form (new_addr, scalar_mode,
			    reg_to_non_prefixed (scalar_reg, scalar_mode));

  if (iform == INSN_FORM_BAD)
    {
      emit_move_insn (base_tmp, new_addr);
      new_addr = base_tmp;
    }

  return change_address (mem, scalar_mode, new_addr);
}

/* Split a variable vec_extract operation into the component instructions.  */

void
rs6000_split_vec_extract_var (rtx dest, rtx src, rtx element, rtx tmp_gpr,
			      rtx tmp_altivec)
{
  machine_mode mode = GET_MODE (src);
  machine_mode scalar_mode = GET_MODE_INNER (GET_MODE (src));
  unsigned scalar_size = GET_MODE_SIZE (scalar_mode);
  int byte_shift = exact_log2 (scalar_size);

  gcc_assert (byte_shift >= 0);

  /* If we are given a memory address, optimize to load just the element.  We
     don't have to adjust the vector element number on little endian
     systems.  */
  if (MEM_P (src))
    {
      emit_move_insn (dest,
		      rs6000_adjust_vec_address (dest, src, element, tmp_gpr,
						 scalar_mode));
      return;
    }

  else if (REG_P (src) || SUBREG_P (src))
    {
      int num_elements = GET_MODE_NUNITS (mode);
      int bits_in_element = mode_to_bits (GET_MODE_INNER (mode));
      int bit_shift = 7 - exact_log2 (num_elements);
      rtx element2;
      unsigned int dest_regno = reg_or_subregno (dest);
      unsigned int src_regno = reg_or_subregno (src);
      unsigned int element_regno = reg_or_subregno (element);

      gcc_assert (REG_P (tmp_gpr));

      /* See if we want to generate VEXTU{B,H,W}{L,R}X if the destination is in
	 a general purpose register.  */
      if (TARGET_P9_VECTOR
	  && (mode == V16QImode || mode == V8HImode || mode == V4SImode)
	  && INT_REGNO_P (dest_regno)
	  && ALTIVEC_REGNO_P (src_regno)
	  && INT_REGNO_P (element_regno))
	{
	  rtx dest_si = gen_rtx_REG (SImode, dest_regno);
	  rtx element_si = gen_rtx_REG (SImode, element_regno);

	  if (mode == V16QImode)
	    emit_insn (BYTES_BIG_ENDIAN
		       ? gen_vextublx (dest_si, element_si, src)
		       : gen_vextubrx (dest_si, element_si, src));

	  else if (mode == V8HImode)
	    {
	      rtx tmp_gpr_si = gen_rtx_REG (SImode, REGNO (tmp_gpr));
	      emit_insn (gen_ashlsi3 (tmp_gpr_si, element_si, const1_rtx));
	      emit_insn (BYTES_BIG_ENDIAN
			 ? gen_vextuhlx (dest_si, tmp_gpr_si, src)
			 : gen_vextuhrx (dest_si, tmp_gpr_si, src));
	    }


	  else
	    {
	      rtx tmp_gpr_si = gen_rtx_REG (SImode, REGNO (tmp_gpr));
	      emit_insn (gen_ashlsi3 (tmp_gpr_si, element_si, const2_rtx));
	      emit_insn (BYTES_BIG_ENDIAN
			 ? gen_vextuwlx (dest_si, tmp_gpr_si, src)
			 : gen_vextuwrx (dest_si, tmp_gpr_si, src));
	    }

	  return;
	}


      gcc_assert (REG_P (tmp_altivec));

      /* For little endian, adjust element ordering.  For V2DI/V2DF, we can use
	 an XOR, otherwise we need to subtract.  The shift amount is so VSLO
	 will shift the element into the upper position (adding 3 to convert a
	 byte shift into a bit shift).  */
      if (scalar_size == 8)
	{
	  if (!BYTES_BIG_ENDIAN)
	    {
	      emit_insn (gen_xordi3 (tmp_gpr, element, const1_rtx));
	      element2 = tmp_gpr;
	    }
	  else
	    element2 = element;

	  /* Generate RLDIC directly to shift left 6 bits and retrieve 1
	     bit.  */
	  emit_insn (gen_rtx_SET (tmp_gpr,
				  gen_rtx_AND (DImode,
					       gen_rtx_ASHIFT (DImode,
							       element2,
							       GEN_INT (6)),
					       GEN_INT (64))));
	}
      else
	{
	  if (!BYTES_BIG_ENDIAN)
	    {
	      rtx num_ele_m1 = GEN_INT (num_elements - 1);

	      emit_insn (gen_anddi3 (tmp_gpr, element, num_ele_m1));
	      emit_insn (gen_subdi3 (tmp_gpr, num_ele_m1, tmp_gpr));
	      element2 = tmp_gpr;
	    }
	  else
	    element2 = element;

	  emit_insn (gen_ashldi3 (tmp_gpr, element2, GEN_INT (bit_shift)));
	}

      /* Get the value into the lower byte of the Altivec register where VSLO
	 expects it.  */
      if (TARGET_P9_VECTOR)
	emit_insn (gen_vsx_splat_v2di (tmp_altivec, tmp_gpr));
      else if (can_create_pseudo_p ())
	emit_insn (gen_vsx_concat_v2di (tmp_altivec, tmp_gpr, tmp_gpr));
      else
	{
	  rtx tmp_di = gen_rtx_REG (DImode, REGNO (tmp_altivec));
	  emit_move_insn (tmp_di, tmp_gpr);
	  emit_insn (gen_vsx_concat_v2di (tmp_altivec, tmp_di, tmp_di));
	}

      /* Do the VSLO to get the value into the final location.  */
      switch (mode)
	{
	case E_V2DFmode:
	  emit_insn (gen_vsx_vslo_v2df (dest, src, tmp_altivec));
	  return;

	case E_V2DImode:
	  emit_insn (gen_vsx_vslo_v2di (dest, src, tmp_altivec));
	  return;

	case E_V4SFmode:
	  {
	    rtx tmp_altivec_di = gen_rtx_REG (DImode, REGNO (tmp_altivec));
	    rtx tmp_altivec_v4sf = gen_rtx_REG (V4SFmode, REGNO (tmp_altivec));
	    rtx src_v2di = gen_rtx_REG (V2DImode, REGNO (src));
	    emit_insn (gen_vsx_vslo_v2di (tmp_altivec_di, src_v2di,
					  tmp_altivec));

	    emit_insn (gen_vsx_xscvspdp_scalar2 (dest, tmp_altivec_v4sf));
	    return;
	  }

	case E_V4SImode:
	case E_V8HImode:
	case E_V16QImode:
	  {
	    rtx tmp_altivec_di = gen_rtx_REG (DImode, REGNO (tmp_altivec));
	    rtx src_v2di = gen_rtx_REG (V2DImode, REGNO (src));
	    rtx tmp_gpr_di = gen_rtx_REG (DImode, REGNO (dest));
	    emit_insn (gen_vsx_vslo_v2di (tmp_altivec_di, src_v2di,
					  tmp_altivec));
	    emit_move_insn (tmp_gpr_di, tmp_altivec_di);
	    emit_insn (gen_lshrdi3 (tmp_gpr_di, tmp_gpr_di,
				    GEN_INT (64 - bits_in_element)));
	    return;
	  }

	default:
	  gcc_unreachable ();
	}

      return;
    }
  else
    gcc_unreachable ();
 }

/* Return alignment of TYPE.  Existing alignment is ALIGN.  HOW
   selects whether the alignment is abi mandated, optional, or
   both abi and optional alignment.  */
   
unsigned int
rs6000_data_alignment (tree type, unsigned int align, enum data_align how)
{
  if (how != align_opt)
    {
      if (TREE_CODE (type) == VECTOR_TYPE && align < 128)
	align = 128;
    }

  if (how != align_abi)
    {
      if (TREE_CODE (type) == ARRAY_TYPE
	  && TYPE_MODE (TREE_TYPE (type)) == QImode)
	{
	  if (align < BITS_PER_WORD)
	    align = BITS_PER_WORD;
	}
    }

  return align;
}

/* Implement TARGET_SLOW_UNALIGNED_ACCESS.  Altivec vector memory
   instructions simply ignore the low bits; VSX memory instructions
   are aligned to 4 or 8 bytes.  */

static bool
rs6000_slow_unaligned_access (machine_mode mode, unsigned int align)
{
  return (STRICT_ALIGNMENT
	  || (!TARGET_EFFICIENT_UNALIGNED_VSX
	      && ((SCALAR_FLOAT_MODE_NOT_VECTOR_P (mode) && align < 32)
		  || ((VECTOR_MODE_P (mode) || VECTOR_ALIGNMENT_P (mode))
		      && (int) align < VECTOR_ALIGN (mode)))));
}

/* Previous GCC releases forced all vector types to have 16-byte alignment.  */

bool
rs6000_special_adjust_field_align_p (tree type, unsigned int computed)
{
  if (TARGET_ALTIVEC && TREE_CODE (type) == VECTOR_TYPE)
    {
      if (computed != 128)
	{
	  static bool warned;
	  if (!warned && warn_psabi)
	    {
	      warned = true;
	      inform (input_location,
		      "the layout of aggregates containing vectors with"
		      " %d-byte alignment has changed in GCC 5",
		      computed / BITS_PER_UNIT);
	    }
	}
      /* In current GCC there is no special case.  */
      return false;
    }

  return false;
}

/* AIX increases natural record alignment to doubleword if the first
   field is an FP double while the FP fields remain word aligned.  */

unsigned int
rs6000_special_round_type_align (tree type, unsigned int computed,
				 unsigned int specified)
{
  unsigned int align = MAX (computed, specified);
  tree field = TYPE_FIELDS (type);

  /* Skip all non field decls */
  while (field != NULL
	 && (TREE_CODE (field) != FIELD_DECL
	     || DECL_FIELD_ABI_IGNORED (field)))
    field = DECL_CHAIN (field);

  if (field != NULL && field != type)
    {
      type = TREE_TYPE (field);
      while (TREE_CODE (type) == ARRAY_TYPE)
	type = TREE_TYPE (type);

      if (type != error_mark_node && TYPE_MODE (type) == DFmode)
	align = MAX (align, 64);
    }

  return align;
}

/* Darwin increases record alignment to the natural alignment of
   the first field.  */

unsigned int
darwin_rs6000_special_round_type_align (tree type, unsigned int computed,
					unsigned int specified)
{
  unsigned int align = MAX (computed, specified);

  if (TYPE_PACKED (type))
    return align;

  /* Find the first field, looking down into aggregates.  */
  do {
    tree field = TYPE_FIELDS (type);
    /* Skip all non field decls */
    while (field != NULL
	   && (TREE_CODE (field) != FIELD_DECL
	       || DECL_FIELD_ABI_IGNORED (field)))
      field = DECL_CHAIN (field);
    if (! field)
      break;
    /* A packed field does not contribute any extra alignment.  */
    if (DECL_PACKED (field))
      return align;
    type = TREE_TYPE (field);
    while (TREE_CODE (type) == ARRAY_TYPE)
      type = TREE_TYPE (type);
  } while (AGGREGATE_TYPE_P (type));

  if (! AGGREGATE_TYPE_P (type) && type != error_mark_node)
    align = MAX (align, TYPE_ALIGN (type));

  return align;
}

/* Return 1 for an operand in small memory on V.4/eabi.  */

int
small_data_operand (rtx op ATTRIBUTE_UNUSED,
		    machine_mode mode ATTRIBUTE_UNUSED)
{
#if TARGET_ELF
  rtx sym_ref;

  if (rs6000_sdata == SDATA_NONE || rs6000_sdata == SDATA_DATA)
    return 0;

  if (DEFAULT_ABI != ABI_V4)
    return 0;

  if (SYMBOL_REF_P (op))
    sym_ref = op;

  else if (GET_CODE (op) != CONST
	   || GET_CODE (XEXP (op, 0)) != PLUS
	   || !SYMBOL_REF_P (XEXP (XEXP (op, 0), 0))
	   || !CONST_INT_P (XEXP (XEXP (op, 0), 1)))
    return 0;

  else
    {
      rtx sum = XEXP (op, 0);
      HOST_WIDE_INT summand;

      /* We have to be careful here, because it is the referenced address
	 that must be 32k from _SDA_BASE_, not just the symbol.  */
      summand = INTVAL (XEXP (sum, 1));
      if (summand < 0 || summand > g_switch_value)
	return 0;

      sym_ref = XEXP (sum, 0);
    }

  return SYMBOL_REF_SMALL_P (sym_ref);
#else
  return 0;
#endif
}

/* Return true if either operand is a general purpose register.  */

bool
gpr_or_gpr_p (rtx op0, rtx op1)
{
  return ((REG_P (op0) && INT_REGNO_P (REGNO (op0)))
	  || (REG_P (op1) && INT_REGNO_P (REGNO (op1))));
}

/* Return true if this is a move direct operation between GPR registers and
   floating point/VSX registers.  */

bool
direct_move_p (rtx op0, rtx op1)
{
  if (!REG_P (op0) || !REG_P (op1))
    return false;

  if (!TARGET_DIRECT_MOVE)
    return false;

  int regno0 = REGNO (op0);
  int regno1 = REGNO (op1);
  if (!HARD_REGISTER_NUM_P (regno0) || !HARD_REGISTER_NUM_P (regno1))
    return false;

  if (INT_REGNO_P (regno0) && VSX_REGNO_P (regno1))
    return true;

  if (VSX_REGNO_P (regno0) && INT_REGNO_P (regno1))
    return true;

  return false;
}

/* Return true if the ADDR is an acceptable address for a quad memory
   operation of mode MODE (either LQ/STQ for general purpose registers, or
   LXV/STXV for vector registers under ISA 3.0.  GPR_P is true if this address
   is intended for LQ/STQ.  If it is false, the address is intended for the ISA
   3.0 LXV/STXV instruction.  */

bool
quad_address_p (rtx addr, machine_mode mode, bool strict)
{
  rtx op0, op1;

  if (GET_MODE_SIZE (mode) < 16)
    return false;

  if (legitimate_indirect_address_p (addr, strict))
    return true;

  if (VECTOR_MODE_P (mode) && !mode_supports_dq_form (mode))
    return false;

  /* Is this a valid prefixed address?  If the bottom four bits of the offset
     are non-zero, we could use a prefixed instruction (which does not have the
     DQ-form constraint that the traditional instruction had) instead of
     forcing the unaligned offset to a GPR.  */
  if (address_is_prefixed (addr, mode, NON_PREFIXED_DQ))
    return true;

  if (GET_CODE (addr) != PLUS)
    return false;

  op0 = XEXP (addr, 0);
  if (!REG_P (op0) || !INT_REG_OK_FOR_BASE_P (op0, strict))
    return false;

  op1 = XEXP (addr, 1);
  if (!CONST_INT_P (op1))
    return false;

  return quad_address_offset_p (INTVAL (op1));
}

/* Return true if this is a load or store quad operation.  This function does
   not handle the atomic quad memory instructions.  */

bool
quad_load_store_p (rtx op0, rtx op1)
{
  bool ret;

  if (!TARGET_QUAD_MEMORY)
    ret = false;

  else if (REG_P (op0) && MEM_P (op1))
    ret = (quad_int_reg_operand (op0, GET_MODE (op0))
	   && quad_memory_operand (op1, GET_MODE (op1))
	   && !reg_overlap_mentioned_p (op0, op1));

  else if (MEM_P (op0) && REG_P (op1))
    ret = (quad_memory_operand (op0, GET_MODE (op0))
	   && quad_int_reg_operand (op1, GET_MODE (op1)));

  else
    ret = false;

  if (TARGET_DEBUG_ADDR)
    {
      fprintf (stderr, "\n========== quad_load_store, return %s\n",
	       ret ? "true" : "false");
      debug_rtx (gen_rtx_SET (op0, op1));
    }

  return ret;
}

/* Given an address, return a constant offset term if one exists.  */

static rtx
address_offset (rtx op)
{
  if (GET_CODE (op) == PRE_INC
      || GET_CODE (op) == PRE_DEC)
    op = XEXP (op, 0);
  else if (GET_CODE (op) == PRE_MODIFY
	   || GET_CODE (op) == LO_SUM)
    op = XEXP (op, 1);

  if (GET_CODE (op) == CONST)
    op = XEXP (op, 0);

  if (GET_CODE (op) == PLUS)
    op = XEXP (op, 1);

  if (CONST_INT_P (op))
    return op;

  return NULL_RTX;
}

/* This tests that a lo_sum {constant, symbol, symbol+offset} is valid for
   the mode.  If we can't find (or don't know) the alignment of the symbol
   we assume (optimistically) that it's sufficiently aligned [??? maybe we
   should be pessimistic].  Offsets are validated in the same way as for
   reg + offset.  */
static bool
darwin_rs6000_legitimate_lo_sum_const_p (rtx x, machine_mode mode)
{
  /* We should not get here with this.  */
  gcc_checking_assert (! mode_supports_dq_form (mode));

  if (GET_CODE (x) == CONST)
    x = XEXP (x, 0);

  if (GET_CODE (x) == UNSPEC && XINT (x, 1) == UNSPEC_MACHOPIC_OFFSET)
    x =  XVECEXP (x, 0, 0);

  rtx sym = NULL_RTX;
  unsigned HOST_WIDE_INT offset = 0;

  if (GET_CODE (x) == PLUS)
    {
      sym = XEXP (x, 0);
      if (! SYMBOL_REF_P (sym))
	return false;
      if (!CONST_INT_P (XEXP (x, 1)))
	return false;
      offset = INTVAL (XEXP (x, 1));
    }
  else if (SYMBOL_REF_P (x))
    sym = x;
  else if (CONST_INT_P (x))
    offset = INTVAL (x);
  else if (GET_CODE (x) == LABEL_REF)
    offset = 0; // We assume code labels are Pmode aligned
  else
    return false; // not sure what we have here.

  /* If we don't know the alignment of the thing to which the symbol refers,
     we assume optimistically it is "enough".
     ??? maybe we should be pessimistic instead.  */
  unsigned align = 0;

  if (sym)
    {
      tree decl = SYMBOL_REF_DECL (sym);
#if TARGET_MACHO
      if (MACHO_SYMBOL_INDIRECTION_P (sym))
      /* The decl in an indirection symbol is the original one, which might
	 be less aligned than the indirection.  Our indirections are always
	 pointer-aligned.  */
	;
      else
#endif
      if (decl && DECL_ALIGN (decl))
	align = DECL_ALIGN_UNIT (decl);
   }

  unsigned int extra = 0;
  switch (mode)
    {
    case E_DFmode:
    case E_DDmode:
    case E_DImode:
      /* If we are using VSX scalar loads, restrict ourselves to reg+reg
	 addressing.  */
      if (VECTOR_MEM_VSX_P (mode))
	return false;

      if (!TARGET_POWERPC64)
	extra = 4;
      else if ((offset & 3) || (align & 3))
	return false;
      break;

    case E_TFmode:
    case E_IFmode:
    case E_KFmode:
    case E_TDmode:
    case E_TImode:
    case E_PTImode:
      extra = 8;
      if (!TARGET_POWERPC64)
	extra = 12;
      else if ((offset & 3) || (align & 3))
	return false;
      break;

    default:
      break;
    }

  /* We only care if the access(es) would cause a change to the high part.  */
  offset = ((offset & 0xffff) ^ 0x8000) - 0x8000;
  return SIGNED_16BIT_OFFSET_EXTRA_P (offset, extra);
}

/* Return true if the MEM operand is a memory operand suitable for use
   with a (full width, possibly multiple) gpr load/store.  On
   powerpc64 this means the offset must be divisible by 4.
   Implements 'Y' constraint.

   Accept direct, indexed, offset, lo_sum and tocref.  Since this is
   a constraint function we know the operand has satisfied a suitable
   memory predicate.

   Offsetting a lo_sum should not be allowed, except where we know by
   alignment that a 32k boundary is not crossed.  Note that by
   "offsetting" here we mean a further offset to access parts of the
   MEM.  It's fine to have a lo_sum where the inner address is offset
   from a sym, since the same sym+offset will appear in the high part
   of the address calculation.  */

bool
mem_operand_gpr (rtx op, machine_mode mode)
{
  unsigned HOST_WIDE_INT offset;
  int extra;
  rtx addr = XEXP (op, 0);

  /* PR85755: Allow PRE_INC and PRE_DEC addresses.  */
  if (TARGET_UPDATE
      && (GET_CODE (addr) == PRE_INC || GET_CODE (addr) == PRE_DEC)
      && mode_supports_pre_incdec_p (mode)
      && legitimate_indirect_address_p (XEXP (addr, 0), false))
    return true;

  /* Allow prefixed instructions if supported.  If the bottom two bits of the
     offset are non-zero, we could use a prefixed instruction (which does not
     have the DS-form constraint that the traditional instruction had) instead
     of forcing the unaligned offset to a GPR.  */
  if (address_is_prefixed (addr, mode, NON_PREFIXED_DS))
    return true;

  /* We need to look through Mach-O PIC unspecs to determine if a lo_sum is
     really OK.  Doing this early avoids teaching all the other machinery
     about them.  */
  if (TARGET_MACHO && GET_CODE (addr) == LO_SUM)
    return darwin_rs6000_legitimate_lo_sum_const_p (XEXP (addr, 1), mode);

  /* Only allow offsettable addresses.  See PRs 83969 and 84279.  */
  if (!rs6000_offsettable_memref_p (op, mode, false))
    return false;

  op = address_offset (addr);
  if (op == NULL_RTX)
    return true;

  offset = INTVAL (op);
  if (TARGET_POWERPC64 && (offset & 3) != 0)
    return false;

  extra = GET_MODE_SIZE (mode) - UNITS_PER_WORD;
  if (extra < 0)
    extra = 0;

  if (GET_CODE (addr) == LO_SUM)
    /* For lo_sum addresses, we must allow any offset except one that
       causes a wrap, so test only the low 16 bits.  */
    offset = ((offset & 0xffff) ^ 0x8000) - 0x8000;

  return SIGNED_16BIT_OFFSET_EXTRA_P (offset, extra);
}

/* As above, but for DS-FORM VSX insns.  Unlike mem_operand_gpr,
   enforce an offset divisible by 4 even for 32-bit.  */

bool
mem_operand_ds_form (rtx op, machine_mode mode)
{
  unsigned HOST_WIDE_INT offset;
  int extra;
  rtx addr = XEXP (op, 0);

  /* Allow prefixed instructions if supported.  If the bottom two bits of the
     offset are non-zero, we could use a prefixed instruction (which does not
     have the DS-form constraint that the traditional instruction had) instead
     of forcing the unaligned offset to a GPR.  */
  if (address_is_prefixed (addr, mode, NON_PREFIXED_DS))
    return true;

  if (!offsettable_address_p (false, mode, addr))
    return false;

  op = address_offset (addr);
  if (op == NULL_RTX)
    return true;

  offset = INTVAL (op);
  if ((offset & 3) != 0)
    return false;

  extra = GET_MODE_SIZE (mode) - UNITS_PER_WORD;
  if (extra < 0)
    extra = 0;

  if (GET_CODE (addr) == LO_SUM)
    /* For lo_sum addresses, we must allow any offset except one that
       causes a wrap, so test only the low 16 bits.  */
    offset = ((offset & 0xffff) ^ 0x8000) - 0x8000;

  return SIGNED_16BIT_OFFSET_EXTRA_P (offset, extra);
}

/* Subroutines of rs6000_legitimize_address and rs6000_legitimate_address_p.  */

static bool
reg_offset_addressing_ok_p (machine_mode mode)
{
  switch (mode)
    {
    case E_V16QImode:
    case E_V8HImode:
    case E_V4SFmode:
    case E_V4SImode:
    case E_V2DFmode:
    case E_V2DImode:
    case E_V1TImode:
    case E_TImode:
    case E_TFmode:
    case E_KFmode:
      /* AltiVec/VSX vector modes.  Only reg+reg addressing was valid until the
	 ISA 3.0 vector d-form addressing mode was added.  While TImode is not
	 a vector mode, if we want to use the VSX registers to move it around,
	 we need to restrict ourselves to reg+reg addressing.  Similarly for
	 IEEE 128-bit floating point that is passed in a single vector
	 register.  */
      if (VECTOR_MEM_ALTIVEC_OR_VSX_P (mode))
	return mode_supports_dq_form (mode);
      break;

      /* The vector pair/quad types support offset addressing if the
	 underlying vectors support offset addressing.  */
    case E_POImode:
    case E_PXImode:
      return TARGET_MMA;

    case E_SDmode:
      /* If we can do direct load/stores of SDmode, restrict it to reg+reg
	 addressing for the LFIWZX and STFIWX instructions.  */
      if (TARGET_NO_SDMODE_STACK)
	return false;
      break;

    default:
      break;
    }

  return true;
}

static bool
virtual_stack_registers_memory_p (rtx op)
{
  int regnum;

  if (REG_P (op))
    regnum = REGNO (op);

  else if (GET_CODE (op) == PLUS
	   && REG_P (XEXP (op, 0))
	   && CONST_INT_P (XEXP (op, 1)))
    regnum = REGNO (XEXP (op, 0));

  else
    return false;

  return (regnum >= FIRST_VIRTUAL_REGISTER
	  && regnum <= LAST_VIRTUAL_POINTER_REGISTER);
}

/* Return true if a MODE sized memory accesses to OP plus OFFSET
   is known to not straddle a 32k boundary.  This function is used
   to determine whether -mcmodel=medium code can use TOC pointer
   relative addressing for OP.  This means the alignment of the TOC
   pointer must also be taken into account, and unfortunately that is
   only 8 bytes.  */ 

#ifndef POWERPC64_TOC_POINTER_ALIGNMENT
#define POWERPC64_TOC_POINTER_ALIGNMENT 8
#endif

static bool
offsettable_ok_by_alignment (rtx op, HOST_WIDE_INT offset,
			     machine_mode mode)
{
  tree decl;
  unsigned HOST_WIDE_INT dsize, dalign, lsb, mask;

  if (!SYMBOL_REF_P (op))
    return false;

  /* ISA 3.0 vector d-form addressing is restricted, don't allow
     SYMBOL_REF.  */
  if (mode_supports_dq_form (mode))
    return false;

  dsize = GET_MODE_SIZE (mode);
  decl = SYMBOL_REF_DECL (op);
  if (!decl)
    {
      if (dsize == 0)
	return false;

      /* -fsection-anchors loses the original SYMBOL_REF_DECL when
	 replacing memory addresses with an anchor plus offset.  We
	 could find the decl by rummaging around in the block->objects
	 VEC for the given offset but that seems like too much work.  */
      dalign = BITS_PER_UNIT;
      if (SYMBOL_REF_HAS_BLOCK_INFO_P (op)
	  && SYMBOL_REF_ANCHOR_P (op)
	  && SYMBOL_REF_BLOCK (op) != NULL)
	{
	  struct object_block *block = SYMBOL_REF_BLOCK (op);

	  dalign = block->alignment;
	  offset += SYMBOL_REF_BLOCK_OFFSET (op);
	}
      else if (CONSTANT_POOL_ADDRESS_P (op))
	{
	  /* It would be nice to have get_pool_align()..  */
	  machine_mode cmode = get_pool_mode (op);

	  dalign = GET_MODE_ALIGNMENT (cmode);
	}
    }
  else if (DECL_P (decl))
    {
      dalign = DECL_ALIGN (decl);

      if (dsize == 0)
	{
	  /* Allow BLKmode when the entire object is known to not
	     cross a 32k boundary.  */
	  if (!DECL_SIZE_UNIT (decl))
	    return false;

	  if (!tree_fits_uhwi_p (DECL_SIZE_UNIT (decl)))
	    return false;

	  dsize = tree_to_uhwi (DECL_SIZE_UNIT (decl));
	  if (dsize > 32768)
	    return false;

	  dalign /= BITS_PER_UNIT;
	  if (dalign > POWERPC64_TOC_POINTER_ALIGNMENT)
	    dalign = POWERPC64_TOC_POINTER_ALIGNMENT;
	  return dalign >= dsize;
	}
    }
  else
    gcc_unreachable ();

  /* Find how many bits of the alignment we know for this access.  */
  dalign /= BITS_PER_UNIT;
  if (dalign > POWERPC64_TOC_POINTER_ALIGNMENT)
    dalign = POWERPC64_TOC_POINTER_ALIGNMENT;
  mask = dalign - 1;
  lsb = offset & -offset;
  mask &= lsb - 1;
  dalign = mask + 1;

  return dalign >= dsize;
}

static bool
constant_pool_expr_p (rtx op)
{
  rtx base, offset;

  split_const (op, &base, &offset);
  return (SYMBOL_REF_P (base)
	  && CONSTANT_POOL_ADDRESS_P (base)
	  && ASM_OUTPUT_SPECIAL_POOL_ENTRY_P (get_pool_constant (base), Pmode));
}

/* Create a TOC reference for symbol_ref SYMBOL.  If LARGETOC_REG is non-null,
   use that as the register to put the HIGH value into if register allocation
   is already done.  */

rtx
create_TOC_reference (rtx symbol, rtx largetoc_reg)
{
  rtx tocrel, tocreg, hi;

  gcc_assert (TARGET_TOC);

  if (TARGET_DEBUG_ADDR)
    {
      if (SYMBOL_REF_P (symbol))
	fprintf (stderr, "\ncreate_TOC_reference, (symbol_ref %s)\n",
		 XSTR (symbol, 0));
      else
	{
	  fprintf (stderr, "\ncreate_TOC_reference, code %s:\n",
		   GET_RTX_NAME (GET_CODE (symbol)));
	  debug_rtx (symbol);
	}
    }

  if (!can_create_pseudo_p ())
    df_set_regs_ever_live (TOC_REGISTER, true);

  tocreg = gen_rtx_REG (Pmode, TOC_REGISTER);
  tocrel = gen_rtx_UNSPEC (Pmode, gen_rtvec (2, symbol, tocreg), UNSPEC_TOCREL);
  if (TARGET_CMODEL == CMODEL_SMALL || can_create_pseudo_p ())
    return tocrel;

  hi = gen_rtx_HIGH (Pmode, copy_rtx (tocrel));
  if (largetoc_reg != NULL)
    {
      emit_move_insn (largetoc_reg, hi);
      hi = largetoc_reg;
    }
  return gen_rtx_LO_SUM (Pmode, hi, tocrel);
}

/* These are only used to pass through from print_operand/print_operand_address
   to rs6000_output_addr_const_extra over the intervening function
   output_addr_const which is not target code.  */
static const_rtx tocrel_base_oac, tocrel_offset_oac;

/* Return true if OP is a toc pointer relative address (the output
   of create_TOC_reference).  If STRICT, do not match non-split
   -mcmodel=large/medium toc pointer relative addresses.  If the pointers 
   are non-NULL, place base and offset pieces in TOCREL_BASE_RET and 
   TOCREL_OFFSET_RET respectively.  */

bool
toc_relative_expr_p (const_rtx op, bool strict, const_rtx *tocrel_base_ret,
		     const_rtx *tocrel_offset_ret)
{
  if (!TARGET_TOC)
    return false;

  if (TARGET_CMODEL != CMODEL_SMALL)
    {
      /* When strict ensure we have everything tidy.  */
      if (strict
	  && !(GET_CODE (op) == LO_SUM
	       && REG_P (XEXP (op, 0))
	       && INT_REG_OK_FOR_BASE_P (XEXP (op, 0), strict)))
	return false;

      /* When not strict, allow non-split TOC addresses and also allow
	 (lo_sum (high ..)) TOC addresses created during reload.  */
      if (GET_CODE (op) == LO_SUM)
	op = XEXP (op, 1);
    }

  const_rtx tocrel_base = op;
  const_rtx tocrel_offset = const0_rtx;

  if (GET_CODE (op) == PLUS && add_cint_operand (XEXP (op, 1), GET_MODE (op)))
    {
      tocrel_base = XEXP (op, 0);
      tocrel_offset = XEXP (op, 1);
    }

  if (tocrel_base_ret)
    *tocrel_base_ret = tocrel_base;
  if (tocrel_offset_ret)
    *tocrel_offset_ret = tocrel_offset;

  return (GET_CODE (tocrel_base) == UNSPEC
	  && XINT (tocrel_base, 1) == UNSPEC_TOCREL
	  && REG_P (XVECEXP (tocrel_base, 0, 1))
	  && REGNO (XVECEXP (tocrel_base, 0, 1)) == TOC_REGISTER);
}

/* Return true if X is a constant pool address, and also for cmodel=medium
   if X is a toc-relative address known to be offsettable within MODE.  */

bool
legitimate_constant_pool_address_p (const_rtx x, machine_mode mode,
				    bool strict)
{
  const_rtx tocrel_base, tocrel_offset;
  return (toc_relative_expr_p (x, strict, &tocrel_base, &tocrel_offset)
	  && (TARGET_CMODEL != CMODEL_MEDIUM
	      || constant_pool_expr_p (XVECEXP (tocrel_base, 0, 0))
	      || mode == QImode
	      || offsettable_ok_by_alignment (XVECEXP (tocrel_base, 0, 0),
					      INTVAL (tocrel_offset), mode)));
}

static bool
legitimate_small_data_p (machine_mode mode, rtx x)
{
  return (DEFAULT_ABI == ABI_V4
	  && !flag_pic && !TARGET_TOC
	  && (SYMBOL_REF_P (x) || GET_CODE (x) == CONST)
	  && small_data_operand (x, mode));
}

bool
rs6000_legitimate_offset_address_p (machine_mode mode, rtx x,
				    bool strict, bool worst_case)
{
  unsigned HOST_WIDE_INT offset;
  unsigned int extra;

  if (GET_CODE (x) != PLUS)
    return false;
  if (!REG_P (XEXP (x, 0)))
    return false;
  if (!INT_REG_OK_FOR_BASE_P (XEXP (x, 0), strict))
    return false;
  if (mode_supports_dq_form (mode))
    return quad_address_p (x, mode, strict);
  if (!reg_offset_addressing_ok_p (mode))
    return virtual_stack_registers_memory_p (x);
  if (legitimate_constant_pool_address_p (x, mode, strict || lra_in_progress))
    return true;
  if (!CONST_INT_P (XEXP (x, 1)))
    return false;

  offset = INTVAL (XEXP (x, 1));
  extra = 0;
  switch (mode)
    {
    case E_DFmode:
    case E_DDmode:
    case E_DImode:
      /* If we are using VSX scalar loads, restrict ourselves to reg+reg
	 addressing.  */
      if (VECTOR_MEM_VSX_P (mode))
	return false;

      if (!worst_case)
	break;
      if (!TARGET_POWERPC64)
	extra = 4;
      else if (offset & 3)
	return false;
      break;

    case E_TFmode:
    case E_IFmode:
    case E_KFmode:
    case E_TDmode:
    case E_TImode:
    case E_PTImode:
      extra = 8;
      if (!worst_case)
	break;
      if (!TARGET_POWERPC64)
	extra = 12;
      else if (offset & 3)
	return false;
      break;

    default:
      break;
    }

  if (TARGET_PREFIXED)
    return SIGNED_34BIT_OFFSET_EXTRA_P (offset, extra);
  else
    return SIGNED_16BIT_OFFSET_EXTRA_P (offset, extra);
}

bool
legitimate_indexed_address_p (rtx x, int strict)
{
  rtx op0, op1;

  if (GET_CODE (x) != PLUS)
    return false;

  op0 = XEXP (x, 0);
  op1 = XEXP (x, 1);

  return (REG_P (op0) && REG_P (op1)
	  && ((INT_REG_OK_FOR_BASE_P (op0, strict)
	       && INT_REG_OK_FOR_INDEX_P (op1, strict))
	      || (INT_REG_OK_FOR_BASE_P (op1, strict)
		  && INT_REG_OK_FOR_INDEX_P (op0, strict))));
}

bool
avoiding_indexed_address_p (machine_mode mode)
{
  unsigned int msize = GET_MODE_SIZE (mode);

  /* Avoid indexed addressing for modes that have non-indexed load/store
     instruction forms.  On power10, vector pairs have an indexed
     form, but vector quads don't.  */
  if (msize > 16)
    return msize != 32;

  return (TARGET_AVOID_XFORM && VECTOR_MEM_NONE_P (mode));
}

bool
legitimate_indirect_address_p (rtx x, int strict)
{
  return REG_P (x) && INT_REG_OK_FOR_BASE_P (x, strict);
}

bool
macho_lo_sum_memory_operand (rtx x, machine_mode mode)
{
  if (!TARGET_MACHO || !flag_pic
      || mode != SImode || !MEM_P (x))
    return false;
  x = XEXP (x, 0);

  if (GET_CODE (x) != LO_SUM)
    return false;
  if (!REG_P (XEXP (x, 0)))
    return false;
  if (!INT_REG_OK_FOR_BASE_P (XEXP (x, 0), 0))
    return false;
  x = XEXP (x, 1);

  return CONSTANT_P (x);
}

static bool
legitimate_lo_sum_address_p (machine_mode mode, rtx x, int strict)
{
  if (GET_CODE (x) != LO_SUM)
    return false;
  if (!REG_P (XEXP (x, 0)))
    return false;
  if (!INT_REG_OK_FOR_BASE_P (XEXP (x, 0), strict))
    return false;
  /* quad word addresses are restricted, and we can't use LO_SUM.  */
  if (mode_supports_dq_form (mode))
    return false;
  x = XEXP (x, 1);

  if (TARGET_ELF || TARGET_MACHO)
    {
      bool large_toc_ok;

      if (DEFAULT_ABI == ABI_V4 && flag_pic)
	return false;
      /* LRA doesn't use LEGITIMIZE_RELOAD_ADDRESS as it usually calls
	 push_reload from reload pass code.  LEGITIMIZE_RELOAD_ADDRESS
	 recognizes some LO_SUM addresses as valid although this
	 function says opposite.  In most cases, LRA through different
	 transformations can generate correct code for address reloads.
	 It cannot manage only some LO_SUM cases.  So we need to add
	 code here saying that some addresses are still valid.  */
      large_toc_ok = (lra_in_progress && TARGET_CMODEL != CMODEL_SMALL
		      && small_toc_ref (x, VOIDmode));
      if (TARGET_TOC && ! large_toc_ok)
	return false;
      if (GET_MODE_NUNITS (mode) != 1)
	return false;
      if (GET_MODE_SIZE (mode) > UNITS_PER_WORD
	  && !(/* ??? Assume floating point reg based on mode?  */
	       TARGET_HARD_FLOAT && (mode == DFmode || mode == DDmode)))
	return false;

      return CONSTANT_P (x) || large_toc_ok;
    }

  return false;
}


/* Try machine-dependent ways of modifying an illegitimate address
   to be legitimate.  If we find one, return the new, valid address.
   This is used from only one place: `memory_address' in explow.c.

   OLDX is the address as it was before break_out_memory_refs was
   called.  In some cases it is useful to look at this to decide what
   needs to be done.

   It is always safe for this function to do nothing.  It exists to
   recognize opportunities to optimize the output.

   On RS/6000, first check for the sum of a register with a constant
   integer that is out of range.  If so, generate code to add the
   constant with the low-order 16 bits masked to the register and force
   this result into another register (this can be done with `cau').
   Then generate an address of REG+(CONST&0xffff), allowing for the
   possibility of bit 16 being a one.

   Then check for the sum of a register and something not constant, try to
   load the other things into a register and return the sum.  */

static rtx
rs6000_legitimize_address (rtx x, rtx oldx ATTRIBUTE_UNUSED,
			   machine_mode mode)
{
  unsigned int extra;

  if (!reg_offset_addressing_ok_p (mode)
      || mode_supports_dq_form (mode))
    {
      if (virtual_stack_registers_memory_p (x))
	return x;

      /* In theory we should not be seeing addresses of the form reg+0,
	 but just in case it is generated, optimize it away.  */
      if (GET_CODE (x) == PLUS && XEXP (x, 1) == const0_rtx)
	return force_reg (Pmode, XEXP (x, 0));

      /* For TImode with load/store quad, restrict addresses to just a single
	 pointer, so it works with both GPRs and VSX registers.  */
      /* Make sure both operands are registers.  */
      else if (GET_CODE (x) == PLUS
	       && (mode != TImode || !TARGET_VSX))
	return gen_rtx_PLUS (Pmode,
			     force_reg (Pmode, XEXP (x, 0)),
			     force_reg (Pmode, XEXP (x, 1)));
      else
	return force_reg (Pmode, x);
    }
  if (SYMBOL_REF_P (x))
    {
      enum tls_model model = SYMBOL_REF_TLS_MODEL (x);
      if (model != 0)
	return rs6000_legitimize_tls_address (x, model);
    }

  extra = 0;
  switch (mode)
    {
    case E_TFmode:
    case E_TDmode:
    case E_TImode:
    case E_PTImode:
    case E_IFmode:
    case E_KFmode:
      /* As in legitimate_offset_address_p we do not assume
	 worst-case.  The mode here is just a hint as to the registers
	 used.  A TImode is usually in gprs, but may actually be in
	 fprs.  Leave worst-case scenario for reload to handle via
	 insn constraints.  PTImode is only GPRs.  */
      extra = 8;
      break;
    default:
      break;
    }

  if (GET_CODE (x) == PLUS
      && REG_P (XEXP (x, 0))
      && CONST_INT_P (XEXP (x, 1))
      && ((unsigned HOST_WIDE_INT) (INTVAL (XEXP (x, 1)) + 0x8000)
	  >= 0x10000 - extra))
    {
      HOST_WIDE_INT high_int, low_int;
      rtx sum;
      low_int = ((INTVAL (XEXP (x, 1)) & 0xffff) ^ 0x8000) - 0x8000;
      if (low_int >= 0x8000 - extra)
	low_int = 0;
      high_int = INTVAL (XEXP (x, 1)) - low_int;
      sum = force_operand (gen_rtx_PLUS (Pmode, XEXP (x, 0),
					 gen_int_mode (high_int, Pmode)), 0);
      return plus_constant (Pmode, sum, low_int);
    }
  else if (GET_CODE (x) == PLUS
	   && REG_P (XEXP (x, 0))
	   && !CONST_INT_P (XEXP (x, 1))
	   && GET_MODE_NUNITS (mode) == 1
	   && (GET_MODE_SIZE (mode) <= UNITS_PER_WORD
	       || (/* ??? Assume floating point reg based on mode?  */
		   TARGET_HARD_FLOAT && (mode == DFmode || mode == DDmode)))
	   && !avoiding_indexed_address_p (mode))
    {
      return gen_rtx_PLUS (Pmode, XEXP (x, 0),
			   force_reg (Pmode, force_operand (XEXP (x, 1), 0)));
    }
  else if ((TARGET_ELF
#if TARGET_MACHO
	    || !MACHO_DYNAMIC_NO_PIC_P
#endif
	    )
	   && TARGET_32BIT
	   && TARGET_NO_TOC_OR_PCREL
	   && !flag_pic
	   && !CONST_INT_P (x)
	   && !CONST_WIDE_INT_P (x)
	   && !CONST_DOUBLE_P (x)
	   && CONSTANT_P (x)
	   && GET_MODE_NUNITS (mode) == 1
	   && (GET_MODE_SIZE (mode) <= UNITS_PER_WORD
	       || (/* ??? Assume floating point reg based on mode?  */
		   TARGET_HARD_FLOAT && (mode == DFmode || mode == DDmode))))
    {
      rtx reg = gen_reg_rtx (Pmode);
      if (TARGET_ELF)
	emit_insn (gen_elf_high (reg, x));
      else
	emit_insn (gen_macho_high (Pmode, reg, x));
      return gen_rtx_LO_SUM (Pmode, reg, x);
    }
  else if (TARGET_TOC
	   && SYMBOL_REF_P (x)
	   && constant_pool_expr_p (x)
	   && ASM_OUTPUT_SPECIAL_POOL_ENTRY_P (get_pool_constant (x), Pmode))
    return create_TOC_reference (x, NULL_RTX);
  else
    return x;
}

/* Debug version of rs6000_legitimize_address.  */
static rtx
rs6000_debug_legitimize_address (rtx x, rtx oldx, machine_mode mode)
{
  rtx ret;
  rtx_insn *insns;

  start_sequence ();
  ret = rs6000_legitimize_address (x, oldx, mode);
  insns = get_insns ();
  end_sequence ();

  if (ret != x)
    {
      fprintf (stderr,
	       "\nrs6000_legitimize_address: mode %s, old code %s, "
	       "new code %s, modified\n",
	       GET_MODE_NAME (mode), GET_RTX_NAME (GET_CODE (x)),
	       GET_RTX_NAME (GET_CODE (ret)));

      fprintf (stderr, "Original address:\n");
      debug_rtx (x);

      fprintf (stderr, "oldx:\n");
      debug_rtx (oldx);

      fprintf (stderr, "New address:\n");
      debug_rtx (ret);

      if (insns)
	{
	  fprintf (stderr, "Insns added:\n");
	  debug_rtx_list (insns, 20);
	}
    }
  else
    {
      fprintf (stderr,
	       "\nrs6000_legitimize_address: mode %s, code %s, no change:\n",
	       GET_MODE_NAME (mode), GET_RTX_NAME (GET_CODE (x)));

      debug_rtx (x);
    }

  if (insns)
    emit_insn (insns);

  return ret;
}

/* This is called from dwarf2out.c via TARGET_ASM_OUTPUT_DWARF_DTPREL.
   We need to emit DTP-relative relocations.  */

static void rs6000_output_dwarf_dtprel (FILE *, int, rtx) ATTRIBUTE_UNUSED;
static void
rs6000_output_dwarf_dtprel (FILE *file, int size, rtx x)
{
  switch (size)
    {
    case 4:
      fputs ("\t.long\t", file);
      break;
    case 8:
      fputs (DOUBLE_INT_ASM_OP, file);
      break;
    default:
      gcc_unreachable ();
    }
  output_addr_const (file, x);
  if (TARGET_ELF)
    fputs ("@dtprel+0x8000", file);
}

/* Return true if X is a symbol that refers to real (rather than emulated)
   TLS.  */

static bool
rs6000_real_tls_symbol_ref_p (rtx x)
{
  return (SYMBOL_REF_P (x)
	  && SYMBOL_REF_TLS_MODEL (x) >= TLS_MODEL_REAL);
}

/* In the name of slightly smaller debug output, and to cater to
   general assembler lossage, recognize various UNSPEC sequences
   and turn them back into a direct symbol reference.  */

static rtx
rs6000_delegitimize_address (rtx orig_x)
{
  rtx x, y, offset;

  if (GET_CODE (orig_x) == UNSPEC && XINT (orig_x, 1) == UNSPEC_FUSION_GPR)
    orig_x = XVECEXP (orig_x, 0, 0);

  orig_x = delegitimize_mem_from_attrs (orig_x);

  x = orig_x;
  if (MEM_P (x))
    x = XEXP (x, 0);

  y = x;
  if (TARGET_CMODEL != CMODEL_SMALL && GET_CODE (y) == LO_SUM)
    y = XEXP (y, 1);

  offset = NULL_RTX;
  if (GET_CODE (y) == PLUS
      && GET_MODE (y) == Pmode
      && CONST_INT_P (XEXP (y, 1)))
    {
      offset = XEXP (y, 1);
      y = XEXP (y, 0);
    }

  if (GET_CODE (y) == UNSPEC && XINT (y, 1) == UNSPEC_TOCREL)
    {
      y = XVECEXP (y, 0, 0);

#ifdef HAVE_AS_TLS
      /* Do not associate thread-local symbols with the original
	 constant pool symbol.  */
      if (TARGET_XCOFF
	  && SYMBOL_REF_P (y)
	  && CONSTANT_POOL_ADDRESS_P (y)
	  && rs6000_real_tls_symbol_ref_p (get_pool_constant (y)))
	return orig_x;
#endif

      if (offset != NULL_RTX)
	y = gen_rtx_PLUS (Pmode, y, offset);
      if (!MEM_P (orig_x))
	return y;
      else
	return replace_equiv_address_nv (orig_x, y);
    }

  if (TARGET_MACHO
      && GET_CODE (orig_x) == LO_SUM
      && GET_CODE (XEXP (orig_x, 1)) == CONST)
    {
      y = XEXP (XEXP (orig_x, 1), 0);
      if (GET_CODE (y) == UNSPEC && XINT (y, 1) == UNSPEC_MACHOPIC_OFFSET)
	return XVECEXP (y, 0, 0);
    }

  return orig_x;
}

/* Return true if X shouldn't be emitted into the debug info.
   The linker doesn't like .toc section references from
   .debug_* sections, so reject .toc section symbols.  */

static bool
rs6000_const_not_ok_for_debug_p (rtx x)
{
  if (GET_CODE (x) == UNSPEC)
    return true;
  if (SYMBOL_REF_P (x)
      && CONSTANT_POOL_ADDRESS_P (x))
    {
      rtx c = get_pool_constant (x);
      machine_mode cmode = get_pool_mode (x);
      if (ASM_OUTPUT_SPECIAL_POOL_ENTRY_P (c, cmode))
	return true;
    }

  return false;
}

/* Implement the TARGET_LEGITIMATE_COMBINED_INSN hook.  */

static bool
rs6000_legitimate_combined_insn (rtx_insn *insn)
{
  int icode = INSN_CODE (insn);

  /* Reject creating doloop insns.  Combine should not be allowed
     to create these for a number of reasons:
     1) In a nested loop, if combine creates one of these in an
     outer loop and the register allocator happens to allocate ctr
     to the outer loop insn, then the inner loop can't use ctr.
     Inner loops ought to be more highly optimized.
     2) Combine often wants to create one of these from what was
     originally a three insn sequence, first combining the three
     insns to two, then to ctrsi/ctrdi.  When ctrsi/ctrdi is not
     allocated ctr, the splitter takes use back to the three insn
     sequence.  It's better to stop combine at the two insn
     sequence.
     3) Faced with not being able to allocate ctr for ctrsi/crtdi
     insns, the register allocator sometimes uses floating point
     or vector registers for the pseudo.  Since ctrsi/ctrdi is a
     jump insn and output reloads are not implemented for jumps,
     the ctrsi/ctrdi splitters need to handle all possible cases.
     That's a pain, and it gets to be seriously difficult when a
     splitter that runs after reload needs memory to transfer from
     a gpr to fpr.  See PR70098 and PR71763 which are not fixed
     for the difficult case.  It's better to not create problems
     in the first place.  */
  if (icode != CODE_FOR_nothing
      && (icode == CODE_FOR_bdz_si
	  || icode == CODE_FOR_bdz_di
	  || icode == CODE_FOR_bdnz_si
	  || icode == CODE_FOR_bdnz_di
	  || icode == CODE_FOR_bdztf_si
	  || icode == CODE_FOR_bdztf_di
	  || icode == CODE_FOR_bdnztf_si
	  || icode == CODE_FOR_bdnztf_di))
    return false;

  return true;
}

/* Construct the SYMBOL_REF for the tls_get_addr function.  */

static GTY(()) rtx rs6000_tls_symbol;
static rtx
rs6000_tls_get_addr (void)
{
  if (!rs6000_tls_symbol)
    rs6000_tls_symbol = init_one_libfunc ("__tls_get_addr");

  return rs6000_tls_symbol;
}

/* Construct the SYMBOL_REF for TLS GOT references.  */

static GTY(()) rtx rs6000_got_symbol;
rtx
rs6000_got_sym (void)
{
  if (!rs6000_got_symbol)
    {
      rs6000_got_symbol = gen_rtx_SYMBOL_REF (Pmode, "_GLOBAL_OFFSET_TABLE_");
      SYMBOL_REF_FLAGS (rs6000_got_symbol) |= SYMBOL_FLAG_LOCAL;
      SYMBOL_REF_FLAGS (rs6000_got_symbol) |= SYMBOL_FLAG_EXTERNAL;
    }

  return rs6000_got_symbol;
}

/* AIX Thread-Local Address support.  */

static rtx
rs6000_legitimize_tls_address_aix (rtx addr, enum tls_model model)
{
  rtx sym, mem, tocref, tlsreg, tmpreg, dest, tlsaddr;
  const char *name;
  char *tlsname;

  name = XSTR (addr, 0);
  /* Append TLS CSECT qualifier, unless the symbol already is qualified
     or the symbol will be in TLS private data section.  */
  if (name[strlen (name) - 1] != ']'
      && (TREE_PUBLIC (SYMBOL_REF_DECL (addr))
	  || bss_initializer_p (SYMBOL_REF_DECL (addr))))
    {
      tlsname = XALLOCAVEC (char, strlen (name) + 4);
      strcpy (tlsname, name);
      strcat (tlsname,
	      bss_initializer_p (SYMBOL_REF_DECL (addr)) ? "[UL]" : "[TL]");
      tlsaddr = copy_rtx (addr);
      XSTR (tlsaddr, 0) = ggc_strdup (tlsname);
    }
  else
    tlsaddr = addr;

  /* Place addr into TOC constant pool.  */
  sym = force_const_mem (GET_MODE (tlsaddr), tlsaddr);

  /* Output the TOC entry and create the MEM referencing the value.  */
  if (constant_pool_expr_p (XEXP (sym, 0))
      && ASM_OUTPUT_SPECIAL_POOL_ENTRY_P (get_pool_constant (XEXP (sym, 0)), Pmode))
    {
      tocref = create_TOC_reference (XEXP (sym, 0), NULL_RTX);
      mem = gen_const_mem (Pmode, tocref);
      set_mem_alias_set (mem, get_TOC_alias_set ());
    }
  else
    return sym;

  /* Use global-dynamic for local-dynamic.  */
  if (model == TLS_MODEL_GLOBAL_DYNAMIC
      || model == TLS_MODEL_LOCAL_DYNAMIC)
    {
      /* Create new TOC reference for @m symbol.  */
      name = XSTR (XVECEXP (XEXP (mem, 0), 0, 0), 0);
      tlsname = XALLOCAVEC (char, strlen (name) + 1);
      strcpy (tlsname, "*LCM");
      strcat (tlsname, name + 3);
      rtx modaddr = gen_rtx_SYMBOL_REF (Pmode, ggc_strdup (tlsname));
      SYMBOL_REF_FLAGS (modaddr) |= SYMBOL_FLAG_LOCAL;
      tocref = create_TOC_reference (modaddr, NULL_RTX);
      rtx modmem = gen_const_mem (Pmode, tocref);
      set_mem_alias_set (modmem, get_TOC_alias_set ());
      
      rtx modreg = gen_reg_rtx (Pmode);
      emit_insn (gen_rtx_SET (modreg, modmem));

      tmpreg = gen_reg_rtx (Pmode);
      emit_insn (gen_rtx_SET (tmpreg, mem));

      dest = gen_reg_rtx (Pmode);
      if (TARGET_32BIT)
	emit_insn (gen_tls_get_addrsi (dest, modreg, tmpreg));
      else
	emit_insn (gen_tls_get_addrdi (dest, modreg, tmpreg));
      return dest;
    }
  /* Obtain TLS pointer: 32 bit call or 64 bit GPR 13.  */
  else if (TARGET_32BIT)
    {
      tlsreg = gen_reg_rtx (SImode);
      emit_insn (gen_tls_get_tpointer (tlsreg));
    }
  else
    tlsreg = gen_rtx_REG (DImode, 13);

  /* Load the TOC value into temporary register.  */
  tmpreg = gen_reg_rtx (Pmode);
  emit_insn (gen_rtx_SET (tmpreg, mem));
  set_unique_reg_note (get_last_insn (), REG_EQUAL,
		       gen_rtx_MINUS (Pmode, addr, tlsreg));

  /* Add TOC symbol value to TLS pointer.  */
  dest = force_reg (Pmode, gen_rtx_PLUS (Pmode, tmpreg, tlsreg));

  return dest;
}

/* Passes the tls arg value for global dynamic and local dynamic
   emit_library_call_value in rs6000_legitimize_tls_address to
   rs6000_call_aix and rs6000_call_sysv.  This is used to emit the
   marker relocs put on __tls_get_addr calls.  */
static rtx global_tlsarg;

/* ADDR contains a thread-local SYMBOL_REF.  Generate code to compute
   this (thread-local) address.  */

static rtx
rs6000_legitimize_tls_address (rtx addr, enum tls_model model)
{
  rtx dest, insn;

  if (TARGET_XCOFF)
    return rs6000_legitimize_tls_address_aix (addr, model);

  dest = gen_reg_rtx (Pmode);
  if (model == TLS_MODEL_LOCAL_EXEC
      && (rs6000_tls_size == 16 || rs6000_pcrel_p (cfun)))
    {
      rtx tlsreg;

      if (TARGET_64BIT)
	{
	  tlsreg = gen_rtx_REG (Pmode, 13);
	  insn = gen_tls_tprel_64 (dest, tlsreg, addr);
	}
      else
	{
	  tlsreg = gen_rtx_REG (Pmode, 2);
	  insn = gen_tls_tprel_32 (dest, tlsreg, addr);
	}
      emit_insn (insn);
    }
  else if (model == TLS_MODEL_LOCAL_EXEC && rs6000_tls_size == 32)
    {
      rtx tlsreg, tmp;

      tmp = gen_reg_rtx (Pmode);
      if (TARGET_64BIT)
	{
	  tlsreg = gen_rtx_REG (Pmode, 13);
	  insn = gen_tls_tprel_ha_64 (tmp, tlsreg, addr);
	}
      else
	{
	  tlsreg = gen_rtx_REG (Pmode, 2);
	  insn = gen_tls_tprel_ha_32 (tmp, tlsreg, addr);
	}
      emit_insn (insn);
      if (TARGET_64BIT)
	insn = gen_tls_tprel_lo_64 (dest, tmp, addr);
      else
	insn = gen_tls_tprel_lo_32 (dest, tmp, addr);
      emit_insn (insn);
    }
  else
    {
      rtx got, tga, tmp1, tmp2;

      /* We currently use relocations like @got@tlsgd for tls, which
	 means the linker will handle allocation of tls entries, placing
	 them in the .got section.  So use a pointer to the .got section,
	 not one to secondary TOC sections used by 64-bit -mminimal-toc,
	 or to secondary GOT sections used by 32-bit -fPIC.  */
      if (rs6000_pcrel_p (cfun))
	got = const0_rtx;
      else if (TARGET_64BIT)
	got = gen_rtx_REG (Pmode, 2);
      else
	{
	  if (flag_pic == 1)
	    got = gen_rtx_REG (Pmode, RS6000_PIC_OFFSET_TABLE_REGNUM);
	  else
	    {
	      rtx gsym = rs6000_got_sym ();
	      got = gen_reg_rtx (Pmode);
	      if (flag_pic == 0)
		rs6000_emit_move (got, gsym, Pmode);
	      else
		{
		  rtx mem, lab;

		  tmp1 = gen_reg_rtx (Pmode);
		  tmp2 = gen_reg_rtx (Pmode);
		  mem = gen_const_mem (Pmode, tmp1);
		  lab = gen_label_rtx ();
		  emit_insn (gen_load_toc_v4_PIC_1b (gsym, lab));
		  emit_move_insn (tmp1, gen_rtx_REG (Pmode, LR_REGNO));
		  if (TARGET_LINK_STACK)
		    emit_insn (gen_addsi3 (tmp1, tmp1, GEN_INT (4)));
		  emit_move_insn (tmp2, mem);
		  rtx_insn *last = emit_insn (gen_addsi3 (got, tmp1, tmp2));
		  set_unique_reg_note (last, REG_EQUAL, gsym);
		}
	    }
	}

      if (model == TLS_MODEL_GLOBAL_DYNAMIC)
	{
	  rtx arg = gen_rtx_UNSPEC (Pmode, gen_rtvec (2, addr, got),
				    UNSPEC_TLSGD);
	  tga = rs6000_tls_get_addr ();
	  rtx argreg = gen_rtx_REG (Pmode, 3);
	  emit_insn (gen_rtx_SET (argreg, arg));
	  global_tlsarg = arg;
	  emit_library_call_value (tga, dest, LCT_CONST, Pmode, argreg, Pmode);
	  global_tlsarg = NULL_RTX;

	  /* Make a note so that the result of this call can be CSEd.  */
	  rtvec vec = gen_rtvec (1, copy_rtx (arg));
	  rtx uns = gen_rtx_UNSPEC (Pmode, vec, UNSPEC_TLS_GET_ADDR);
	  set_unique_reg_note (get_last_insn (), REG_EQUAL, uns);
	}
      else if (model == TLS_MODEL_LOCAL_DYNAMIC)
	{
	  rtx arg = gen_rtx_UNSPEC (Pmode, gen_rtvec (1, got), UNSPEC_TLSLD);
	  tga = rs6000_tls_get_addr ();
	  tmp1 = gen_reg_rtx (Pmode);
	  rtx argreg = gen_rtx_REG (Pmode, 3);
	  emit_insn (gen_rtx_SET (argreg, arg));
	  global_tlsarg = arg;
	  emit_library_call_value (tga, tmp1, LCT_CONST, Pmode, argreg, Pmode);
	  global_tlsarg = NULL_RTX;

	  /* Make a note so that the result of this call can be CSEd.  */
	  rtvec vec = gen_rtvec (1, copy_rtx (arg));
	  rtx uns = gen_rtx_UNSPEC (Pmode, vec, UNSPEC_TLS_GET_ADDR);
	  set_unique_reg_note (get_last_insn (), REG_EQUAL, uns);

	  if (rs6000_tls_size == 16 || rs6000_pcrel_p (cfun))
	    {
	      if (TARGET_64BIT)
		insn = gen_tls_dtprel_64 (dest, tmp1, addr);
	      else
		insn = gen_tls_dtprel_32 (dest, tmp1, addr);
	    }
	  else if (rs6000_tls_size == 32)
	    {
	      tmp2 = gen_reg_rtx (Pmode);
	      if (TARGET_64BIT)
		insn = gen_tls_dtprel_ha_64 (tmp2, tmp1, addr);
	      else
		insn = gen_tls_dtprel_ha_32 (tmp2, tmp1, addr);
	      emit_insn (insn);
	      if (TARGET_64BIT)
		insn = gen_tls_dtprel_lo_64 (dest, tmp2, addr);
	      else
		insn = gen_tls_dtprel_lo_32 (dest, tmp2, addr);
	    }
	  else
	    {
	      tmp2 = gen_reg_rtx (Pmode);
	      if (TARGET_64BIT)
		insn = gen_tls_got_dtprel_64 (tmp2, got, addr);
	      else
		insn = gen_tls_got_dtprel_32 (tmp2, got, addr);
	      emit_insn (insn);
	      insn = gen_rtx_SET (dest, gen_rtx_PLUS (Pmode, tmp2, tmp1));
	    }
	  emit_insn (insn);
	}
      else
	{
	  /* IE, or 64-bit offset LE.  */
	  tmp2 = gen_reg_rtx (Pmode);
	  if (TARGET_64BIT)
	    insn = gen_tls_got_tprel_64 (tmp2, got, addr);
	  else
	    insn = gen_tls_got_tprel_32 (tmp2, got, addr);
	  emit_insn (insn);
	  if (rs6000_pcrel_p (cfun))
	    {
	      if (TARGET_64BIT)
		insn = gen_tls_tls_pcrel_64 (dest, tmp2, addr);
	      else
		insn = gen_tls_tls_pcrel_32 (dest, tmp2, addr);
	    }
	  else if (TARGET_64BIT)
	    insn = gen_tls_tls_64 (dest, tmp2, addr);
	  else
	    insn = gen_tls_tls_32 (dest, tmp2, addr);
	  emit_insn (insn);
	}
    }

  return dest;
}

/* Only create the global variable for the stack protect guard if we are using
   the global flavor of that guard.  */
static tree
rs6000_init_stack_protect_guard (void)
{
  if (rs6000_stack_protector_guard == SSP_GLOBAL)
    return default_stack_protect_guard ();

  return NULL_TREE;
}

/* Implement TARGET_CANNOT_FORCE_CONST_MEM.  */

static bool
rs6000_cannot_force_const_mem (machine_mode mode ATTRIBUTE_UNUSED, rtx x)
{
  if (GET_CODE (x) == HIGH
      && GET_CODE (XEXP (x, 0)) == UNSPEC)
    return true;

  /* A TLS symbol in the TOC cannot contain a sum.  */
  if (GET_CODE (x) == CONST
      && GET_CODE (XEXP (x, 0)) == PLUS
      && SYMBOL_REF_P (XEXP (XEXP (x, 0), 0))
      && SYMBOL_REF_TLS_MODEL (XEXP (XEXP (x, 0), 0)) != 0)
    return true;

  /* Do not place an ELF TLS symbol in the constant pool.  */
  return TARGET_ELF && tls_referenced_p (x);
}

/* Return true iff the given SYMBOL_REF refers to a constant pool entry
   that we have put in the TOC, or for cmodel=medium, if the SYMBOL_REF
   can be addressed relative to the toc pointer.  */

static bool
use_toc_relative_ref (rtx sym, machine_mode mode)
{
  return ((constant_pool_expr_p (sym)
	   && ASM_OUTPUT_SPECIAL_POOL_ENTRY_P (get_pool_constant (sym),
					       get_pool_mode (sym)))
	  || (TARGET_CMODEL == CMODEL_MEDIUM
	      && SYMBOL_REF_LOCAL_P (sym)
	      && GET_MODE_SIZE (mode) <= POWERPC64_TOC_POINTER_ALIGNMENT));
}

/* TARGET_LEGITIMATE_ADDRESS_P recognizes an RTL expression
   that is a valid memory address for an instruction.
   The MODE argument is the machine mode for the MEM expression
   that wants to use this address.

   On the RS/6000, there are four valid address: a SYMBOL_REF that
   refers to a constant pool entry of an address (or the sum of it
   plus a constant), a short (16-bit signed) constant plus a register,
   the sum of two registers, or a register indirect, possibly with an
   auto-increment.  For DFmode, DDmode and DImode with a constant plus
   register, we must ensure that both words are addressable or PowerPC64
   with offset word aligned.

   For modes spanning multiple registers (DFmode and DDmode in 32-bit GPRs,
   32-bit DImode, TImode, TFmode, TDmode), indexed addressing cannot be used
   because adjacent memory cells are accessed by adding word-sized offsets
   during assembly output.  */
static bool
rs6000_legitimate_address_p (machine_mode mode, rtx x, bool reg_ok_strict)
{
  bool reg_offset_p = reg_offset_addressing_ok_p (mode);
  bool quad_offset_p = mode_supports_dq_form (mode);

  /* If this is an unaligned stvx/ldvx type address, discard the outer AND.  */
  if (VECTOR_MEM_ALTIVEC_OR_VSX_P (mode)
      && GET_CODE (x) == AND
      && CONST_INT_P (XEXP (x, 1))
      && INTVAL (XEXP (x, 1)) == -16)
    x = XEXP (x, 0);

  if (TARGET_ELF && RS6000_SYMBOL_REF_TLS_P (x))
    return 0;
  if (legitimate_indirect_address_p (x, reg_ok_strict))
    return 1;
  if (TARGET_UPDATE
      && (GET_CODE (x) == PRE_INC || GET_CODE (x) == PRE_DEC)
      && mode_supports_pre_incdec_p (mode)
      && legitimate_indirect_address_p (XEXP (x, 0), reg_ok_strict))
    return 1;

  /* Handle prefixed addresses (PC-relative or 34-bit offset).  */
  if (address_is_prefixed (x, mode, NON_PREFIXED_DEFAULT))
    return 1;

  /* Handle restricted vector d-form offsets in ISA 3.0.  */
  if (quad_offset_p)
    {
      if (quad_address_p (x, mode, reg_ok_strict))
	return 1;
    }
  else if (virtual_stack_registers_memory_p (x))
    return 1;

  else if (reg_offset_p)
    {
      if (legitimate_small_data_p (mode, x))
	return 1;
      if (legitimate_constant_pool_address_p (x, mode,
					     reg_ok_strict || lra_in_progress))
	return 1;
    }

  /* For TImode, if we have TImode in VSX registers, only allow register
     indirect addresses.  This will allow the values to go in either GPRs
     or VSX registers without reloading.  The vector types would tend to
     go into VSX registers, so we allow REG+REG, while TImode seems
     somewhat split, in that some uses are GPR based, and some VSX based.  */
  /* FIXME: We could loosen this by changing the following to
       if (mode == TImode && TARGET_QUAD_MEMORY && TARGET_VSX)
     but currently we cannot allow REG+REG addressing for TImode.  See
     PR72827 for complete details on how this ends up hoodwinking DSE.  */
  if (mode == TImode && TARGET_VSX)
    return 0;
  /* If not REG_OK_STRICT (before reload) let pass any stack offset.  */
  if (! reg_ok_strict
      && reg_offset_p
      && GET_CODE (x) == PLUS
      && REG_P (XEXP (x, 0))
      && (XEXP (x, 0) == virtual_stack_vars_rtx
	  || XEXP (x, 0) == arg_pointer_rtx)
      && CONST_INT_P (XEXP (x, 1)))
    return 1;
  if (rs6000_legitimate_offset_address_p (mode, x, reg_ok_strict, false))
    return 1;
  if (!FLOAT128_2REG_P (mode)
      && (TARGET_HARD_FLOAT
	  || TARGET_POWERPC64
	  || (mode != DFmode && mode != DDmode))
      && (TARGET_POWERPC64 || mode != DImode)
      && (mode != TImode || VECTOR_MEM_VSX_P (TImode))
      && mode != PTImode
      && !avoiding_indexed_address_p (mode)
      && legitimate_indexed_address_p (x, reg_ok_strict))
    return 1;
  if (TARGET_UPDATE && GET_CODE (x) == PRE_MODIFY
      && mode_supports_pre_modify_p (mode)
      && legitimate_indirect_address_p (XEXP (x, 0), reg_ok_strict)
      && (rs6000_legitimate_offset_address_p (mode, XEXP (x, 1),
					      reg_ok_strict, false)
	  || (!avoiding_indexed_address_p (mode)
	      && legitimate_indexed_address_p (XEXP (x, 1), reg_ok_strict)))
      && rtx_equal_p (XEXP (XEXP (x, 1), 0), XEXP (x, 0)))
    {
      /* There is no prefixed version of the load/store with update.  */
      rtx addr = XEXP (x, 1);
      return !address_is_prefixed (addr, mode, NON_PREFIXED_DEFAULT);
    }
  if (reg_offset_p && !quad_offset_p
      && legitimate_lo_sum_address_p (mode, x, reg_ok_strict))
    return 1;
  return 0;
}

/* Debug version of rs6000_legitimate_address_p.  */
static bool
rs6000_debug_legitimate_address_p (machine_mode mode, rtx x,
				   bool reg_ok_strict)
{
  bool ret = rs6000_legitimate_address_p (mode, x, reg_ok_strict);
  fprintf (stderr,
	   "\nrs6000_legitimate_address_p: return = %s, mode = %s, "
	   "strict = %d, reload = %s, code = %s\n",
	   ret ? "true" : "false",
	   GET_MODE_NAME (mode),
	   reg_ok_strict,
	   (reload_completed ? "after" : "before"),
	   GET_RTX_NAME (GET_CODE (x)));
  debug_rtx (x);

  return ret;
}

/* Implement TARGET_MODE_DEPENDENT_ADDRESS_P.  */

static bool
rs6000_mode_dependent_address_p (const_rtx addr,
				 addr_space_t as ATTRIBUTE_UNUSED)
{
  return rs6000_mode_dependent_address_ptr (addr);
}

/* Go to LABEL if ADDR (a legitimate address expression)
   has an effect that depends on the machine mode it is used for.

   On the RS/6000 this is true of all integral offsets (since AltiVec
   and VSX modes don't allow them) or is a pre-increment or decrement.

   ??? Except that due to conceptual problems in offsettable_address_p
   we can't really report the problems of integral offsets.  So leave
   this assuming that the adjustable offset must be valid for the
   sub-words of a TFmode operand, which is what we had before.  */

static bool
rs6000_mode_dependent_address (const_rtx addr)
{
  switch (GET_CODE (addr))
    {
    case PLUS:
      /* Any offset from virtual_stack_vars_rtx and arg_pointer_rtx
	 is considered a legitimate address before reload, so there
	 are no offset restrictions in that case.  Note that this
	 condition is safe in strict mode because any address involving
	 virtual_stack_vars_rtx or arg_pointer_rtx would already have
	 been rejected as illegitimate.  */
      if (XEXP (addr, 0) != virtual_stack_vars_rtx
	  && XEXP (addr, 0) != arg_pointer_rtx
	  && CONST_INT_P (XEXP (addr, 1)))
	{
	  HOST_WIDE_INT val = INTVAL (XEXP (addr, 1));
	  HOST_WIDE_INT extra = TARGET_POWERPC64 ? 8 : 12;
	  if (TARGET_PREFIXED)
	    return !SIGNED_34BIT_OFFSET_EXTRA_P (val, extra);
	  else
	    return !SIGNED_16BIT_OFFSET_EXTRA_P (val, extra);
	}
      break;

    case LO_SUM:
      /* Anything in the constant pool is sufficiently aligned that
	 all bytes have the same high part address.  */
      return !legitimate_constant_pool_address_p (addr, QImode, false);

    /* Auto-increment cases are now treated generically in recog.c.  */
    case PRE_MODIFY:
      return TARGET_UPDATE;

    /* AND is only allowed in Altivec loads.  */
    case AND:
      return true;

    default:
      break;
    }

  return false;
}

/* Debug version of rs6000_mode_dependent_address.  */
static bool
rs6000_debug_mode_dependent_address (const_rtx addr)
{
  bool ret = rs6000_mode_dependent_address (addr);

  fprintf (stderr, "\nrs6000_mode_dependent_address: ret = %s\n",
	   ret ? "true" : "false");
  debug_rtx (addr);

  return ret;
}

/* Implement FIND_BASE_TERM.  */

rtx
rs6000_find_base_term (rtx op)
{
  rtx base;

  base = op;
  if (GET_CODE (base) == CONST)
    base = XEXP (base, 0);
  if (GET_CODE (base) == PLUS)
    base = XEXP (base, 0);
  if (GET_CODE (base) == UNSPEC)
    switch (XINT (base, 1))
      {
      case UNSPEC_TOCREL:
      case UNSPEC_MACHOPIC_OFFSET:
	/* OP represents SYM [+ OFFSET] - ANCHOR.  SYM is the base term
	   for aliasing purposes.  */
	return XVECEXP (base, 0, 0);
      }

  return op;
}

/* More elaborate version of recog's offsettable_memref_p predicate
   that works around the ??? note of rs6000_mode_dependent_address.
   In particular it accepts

     (mem:DI (plus:SI (reg/f:SI 31 31) (const_int 32760 [0x7ff8])))

   in 32-bit mode, that the recog predicate rejects.  */

static bool
rs6000_offsettable_memref_p (rtx op, machine_mode reg_mode, bool strict)
{
  bool worst_case;

  if (!MEM_P (op))
    return false;

  /* First mimic offsettable_memref_p.  */
  if (offsettable_address_p (strict, GET_MODE (op), XEXP (op, 0)))
    return true;

  /* offsettable_address_p invokes rs6000_mode_dependent_address, but
     the latter predicate knows nothing about the mode of the memory
     reference and, therefore, assumes that it is the largest supported
     mode (TFmode).  As a consequence, legitimate offsettable memory
     references are rejected.  rs6000_legitimate_offset_address_p contains
     the correct logic for the PLUS case of rs6000_mode_dependent_address,
     at least with a little bit of help here given that we know the
     actual registers used.  */
  worst_case = ((TARGET_POWERPC64 && GET_MODE_CLASS (reg_mode) == MODE_INT)
		|| GET_MODE_SIZE (reg_mode) == 4);
  return rs6000_legitimate_offset_address_p (GET_MODE (op), XEXP (op, 0),
					     strict, worst_case);
}

/* Determine the reassociation width to be used in reassociate_bb.
   This takes into account how many parallel operations we
   can actually do of a given type, and also the latency.
   P8:
     int add/sub 6/cycle     
         mul 2/cycle
     vect add/sub/mul 2/cycle
     fp   add/sub/mul 2/cycle
     dfp  1/cycle
*/
 
static int
rs6000_reassociation_width (unsigned int opc ATTRIBUTE_UNUSED,
                            machine_mode mode)
{
  switch (rs6000_tune)
    {
    case PROCESSOR_POWER8:
    case PROCESSOR_POWER9:
    case PROCESSOR_POWER10:
      if (DECIMAL_FLOAT_MODE_P (mode))
	return 1;
      if (VECTOR_MODE_P (mode))
	return 4;
      if (INTEGRAL_MODE_P (mode)) 
	return 1;
      if (FLOAT_MODE_P (mode))
	return 4;
      break;
    default:
      break;
    }
  return 1;
}

/* Change register usage conditional on target flags.  */
static void
rs6000_conditional_register_usage (void)
{
  int i;

  if (TARGET_DEBUG_TARGET)
    fprintf (stderr, "rs6000_conditional_register_usage called\n");

  /* 64-bit AIX and Linux reserve GPR13 for thread-private data.  */
  if (TARGET_64BIT)
    fixed_regs[13] = call_used_regs[13] = 1;

  /* Conditionally disable FPRs.  */
  if (TARGET_SOFT_FLOAT)
    for (i = 32; i < 64; i++)
      fixed_regs[i] = call_used_regs[i] = 1;

  /* The TOC register is not killed across calls in a way that is
     visible to the compiler.  */
  if (DEFAULT_ABI == ABI_AIX || DEFAULT_ABI == ABI_ELFv2)
    call_used_regs[2] = 0;

  if (DEFAULT_ABI == ABI_V4 && flag_pic == 2)
    fixed_regs[RS6000_PIC_OFFSET_TABLE_REGNUM] = 1;

  if (DEFAULT_ABI == ABI_V4 && flag_pic == 1)
    fixed_regs[RS6000_PIC_OFFSET_TABLE_REGNUM]
      = call_used_regs[RS6000_PIC_OFFSET_TABLE_REGNUM] = 1;

  if (DEFAULT_ABI == ABI_DARWIN && flag_pic)
    fixed_regs[RS6000_PIC_OFFSET_TABLE_REGNUM]
      = call_used_regs[RS6000_PIC_OFFSET_TABLE_REGNUM] = 1;

  if (TARGET_TOC && TARGET_MINIMAL_TOC)
    fixed_regs[RS6000_PIC_OFFSET_TABLE_REGNUM] = 1;

  if (!TARGET_ALTIVEC && !TARGET_VSX)
    {
      for (i = FIRST_ALTIVEC_REGNO; i <= LAST_ALTIVEC_REGNO; ++i)
	fixed_regs[i] = call_used_regs[i] = 1;
      call_used_regs[VRSAVE_REGNO] = 1;
    }

  if (TARGET_ALTIVEC || TARGET_VSX)
    global_regs[VSCR_REGNO] = 1;

  if (TARGET_ALTIVEC_ABI)
    {
      for (i = FIRST_ALTIVEC_REGNO; i < FIRST_ALTIVEC_REGNO + 20; ++i)
	call_used_regs[i] = 1;

      /* AIX reserves VR20:31 in non-extended ABI mode.  */
      if (TARGET_XCOFF && !rs6000_aix_extabi)
	for (i = FIRST_ALTIVEC_REGNO + 20; i < FIRST_ALTIVEC_REGNO + 32; ++i)
	  fixed_regs[i] = call_used_regs[i] = 1;
    }
}


/* Output insns to set DEST equal to the constant SOURCE as a series of
   lis, ori and shl instructions and return TRUE.  */

bool
rs6000_emit_set_const (rtx dest, rtx source)
{
  machine_mode mode = GET_MODE (dest);
  rtx temp, set;
  rtx_insn *insn;
  HOST_WIDE_INT c;

  gcc_checking_assert (CONST_INT_P (source));
  c = INTVAL (source);
  switch (mode)
    {
    case E_QImode:
    case E_HImode:
      emit_insn (gen_rtx_SET (dest, source));
      return true;

    case E_SImode:
      temp = !can_create_pseudo_p () ? dest : gen_reg_rtx (SImode);

      emit_insn (gen_rtx_SET (copy_rtx (temp),
			      GEN_INT (c & ~(HOST_WIDE_INT) 0xffff)));
      emit_insn (gen_rtx_SET (dest,
			      gen_rtx_IOR (SImode, copy_rtx (temp),
					   GEN_INT (c & 0xffff))));
      break;

    case E_DImode:
      if (!TARGET_POWERPC64)
	{
	  rtx hi, lo;

	  hi = operand_subword_force (copy_rtx (dest), WORDS_BIG_ENDIAN == 0,
				      DImode);
	  lo = operand_subword_force (dest, WORDS_BIG_ENDIAN != 0,
				      DImode);
	  emit_move_insn (hi, GEN_INT (c >> 32));
	  c = ((c & 0xffffffff) ^ 0x80000000) - 0x80000000;
	  emit_move_insn (lo, GEN_INT (c));
	}
      else
	rs6000_emit_set_long_const (dest, c);
      break;

    default:
      gcc_unreachable ();
    }

  insn = get_last_insn ();
  set = single_set (insn);
  if (! CONSTANT_P (SET_SRC (set)))
    set_unique_reg_note (insn, REG_EQUAL, GEN_INT (c));

  return true;
}

/* Subroutine of rs6000_emit_set_const, handling PowerPC64 DImode.
   Output insns to set DEST equal to the constant C as a series of
   lis, ori and shl instructions.  */

static void
rs6000_emit_set_long_const (rtx dest, HOST_WIDE_INT c)
{
  rtx temp;
  HOST_WIDE_INT ud1, ud2, ud3, ud4;

  ud1 = c & 0xffff;
  c = c >> 16;
  ud2 = c & 0xffff;
  c = c >> 16;
  ud3 = c & 0xffff;
  c = c >> 16;
  ud4 = c & 0xffff;

  if ((ud4 == 0xffff && ud3 == 0xffff && ud2 == 0xffff && (ud1 & 0x8000))
      || (ud4 == 0 && ud3 == 0 && ud2 == 0 && ! (ud1 & 0x8000)))
    emit_move_insn (dest, GEN_INT ((ud1 ^ 0x8000) - 0x8000));

  else if ((ud4 == 0xffff && ud3 == 0xffff && (ud2 & 0x8000))
	   || (ud4 == 0 && ud3 == 0 && ! (ud2 & 0x8000)))
    {
      temp = !can_create_pseudo_p () ? dest : gen_reg_rtx (DImode);

      emit_move_insn (ud1 != 0 ? copy_rtx (temp) : dest,
		      GEN_INT (((ud2 << 16) ^ 0x80000000) - 0x80000000));
      if (ud1 != 0)
	emit_move_insn (dest,
			gen_rtx_IOR (DImode, copy_rtx (temp),
				     GEN_INT (ud1)));
    }
  else if (ud3 == 0 && ud4 == 0)
    {
      temp = !can_create_pseudo_p () ? dest : gen_reg_rtx (DImode);

      gcc_assert (ud2 & 0x8000);
      emit_move_insn (copy_rtx (temp),
		      GEN_INT (((ud2 << 16) ^ 0x80000000) - 0x80000000));
      if (ud1 != 0)
	emit_move_insn (copy_rtx (temp),
			gen_rtx_IOR (DImode, copy_rtx (temp),
				     GEN_INT (ud1)));
      emit_move_insn (dest,
		      gen_rtx_ZERO_EXTEND (DImode,
					   gen_lowpart (SImode,
							copy_rtx (temp))));
    }
  else if (ud1 == ud3 && ud2 == ud4)
    {
      temp = !can_create_pseudo_p () ? dest : gen_reg_rtx (DImode);
      HOST_WIDE_INT num = (ud2 << 16) | ud1;
      rs6000_emit_set_long_const (temp, (num ^ 0x80000000) - 0x80000000);
      rtx one = gen_rtx_AND (DImode, temp, GEN_INT (0xffffffff));
      rtx two = gen_rtx_ASHIFT (DImode, temp, GEN_INT (32));
      emit_move_insn (dest, gen_rtx_IOR (DImode, one, two));
    }
  else if ((ud4 == 0xffff && (ud3 & 0x8000))
	   || (ud4 == 0 && ! (ud3 & 0x8000)))
    {
      temp = !can_create_pseudo_p () ? dest : gen_reg_rtx (DImode);

      emit_move_insn (copy_rtx (temp),
		      GEN_INT (((ud3 << 16) ^ 0x80000000) - 0x80000000));
      if (ud2 != 0)
	emit_move_insn (copy_rtx (temp),
			gen_rtx_IOR (DImode, copy_rtx (temp),
				     GEN_INT (ud2)));
      emit_move_insn (ud1 != 0 ? copy_rtx (temp) : dest,
		      gen_rtx_ASHIFT (DImode, copy_rtx (temp),
				      GEN_INT (16)));
      if (ud1 != 0)
	emit_move_insn (dest,
			gen_rtx_IOR (DImode, copy_rtx (temp),
				     GEN_INT (ud1)));
    }
  else
    {
      temp = !can_create_pseudo_p () ? dest : gen_reg_rtx (DImode);

      emit_move_insn (copy_rtx (temp),
		      GEN_INT (((ud4 << 16) ^ 0x80000000) - 0x80000000));
      if (ud3 != 0)
	emit_move_insn (copy_rtx (temp),
			gen_rtx_IOR (DImode, copy_rtx (temp),
				     GEN_INT (ud3)));

      emit_move_insn (ud2 != 0 || ud1 != 0 ? copy_rtx (temp) : dest,
		      gen_rtx_ASHIFT (DImode, copy_rtx (temp),
				      GEN_INT (32)));
      if (ud2 != 0)
	emit_move_insn (ud1 != 0 ? copy_rtx (temp) : dest,
			gen_rtx_IOR (DImode, copy_rtx (temp),
				     GEN_INT (ud2 << 16)));
      if (ud1 != 0)
	emit_move_insn (dest,
			gen_rtx_IOR (DImode, copy_rtx (temp),
				     GEN_INT (ud1)));
    }
}

/* Helper for the following.  Get rid of [r+r] memory refs
   in cases where it won't work (TImode, TFmode, TDmode, PTImode).  */

static void
rs6000_eliminate_indexed_memrefs (rtx operands[2])
{
  if (MEM_P (operands[0])
      && !REG_P (XEXP (operands[0], 0))
      && ! legitimate_constant_pool_address_p (XEXP (operands[0], 0),
					       GET_MODE (operands[0]), false))
    operands[0]
      = replace_equiv_address (operands[0],
			       copy_addr_to_reg (XEXP (operands[0], 0)));

  if (MEM_P (operands[1])
      && !REG_P (XEXP (operands[1], 0))
      && ! legitimate_constant_pool_address_p (XEXP (operands[1], 0),
					       GET_MODE (operands[1]), false))
    operands[1]
      = replace_equiv_address (operands[1],
			       copy_addr_to_reg (XEXP (operands[1], 0)));
}

/* Generate a vector of constants to permute MODE for a little-endian
   storage operation by swapping the two halves of a vector.  */
static rtvec
rs6000_const_vec (machine_mode mode)
{
  int i, subparts;
  rtvec v;

  switch (mode)
    {
    case E_V1TImode:
      subparts = 1;
      break;
    case E_V2DFmode:
    case E_V2DImode:
      subparts = 2;
      break;
    case E_V4SFmode:
    case E_V4SImode:
      subparts = 4;
      break;
    case E_V8HImode:
      subparts = 8;
      break;
    case E_V16QImode:
      subparts = 16;
      break;
    default:
      gcc_unreachable();
    }

  v = rtvec_alloc (subparts);

  for (i = 0; i < subparts / 2; ++i)
    RTVEC_ELT (v, i) = gen_rtx_CONST_INT (DImode, i + subparts / 2);
  for (i = subparts / 2; i < subparts; ++i)
    RTVEC_ELT (v, i) = gen_rtx_CONST_INT (DImode, i - subparts / 2);

  return v;
}

/* Emit an lxvd2x, stxvd2x, or xxpermdi instruction for a VSX load or
   store operation.  */
void
rs6000_emit_le_vsx_permute (rtx dest, rtx source, machine_mode mode)
{
  gcc_assert (!altivec_indexed_or_indirect_operand (dest, mode));
  gcc_assert (!altivec_indexed_or_indirect_operand (source, mode));

  /* Scalar permutations are easier to express in integer modes rather than
     floating-point modes, so cast them here.  We use V1TImode instead
     of TImode to ensure that the values don't go through GPRs.  */
  if (FLOAT128_VECTOR_P (mode))
    {
      dest = gen_lowpart (V1TImode, dest);
      source = gen_lowpart (V1TImode, source);
      mode = V1TImode;
    }

  /* Use ROTATE instead of VEC_SELECT if the mode contains only a single
     scalar.  */
  if (mode == TImode || mode == V1TImode)
    emit_insn (gen_rtx_SET (dest, gen_rtx_ROTATE (mode, source,
						  GEN_INT (64))));
  else
    {
      rtx par = gen_rtx_PARALLEL (VOIDmode, rs6000_const_vec (mode));
      emit_insn (gen_rtx_SET (dest, gen_rtx_VEC_SELECT (mode, source, par)));
    }
}

/* Emit a little-endian load from vector memory location SOURCE to VSX
   register DEST in mode MODE.  The load is done with two permuting
   insn's that represent an lxvd2x and xxpermdi.  */
void
rs6000_emit_le_vsx_load (rtx dest, rtx source, machine_mode mode)
{
  /* Use V2DImode to do swaps of types with 128-bit scalare parts (TImode,
     V1TImode).  */
  if (mode == TImode || mode == V1TImode)
    {
      mode = V2DImode;
      dest = gen_lowpart (V2DImode, dest);
      source = adjust_address (source, V2DImode, 0);
    }

  rtx tmp = can_create_pseudo_p () ? gen_reg_rtx_and_attrs (dest) : dest;
  rs6000_emit_le_vsx_permute (tmp, source, mode);
  rs6000_emit_le_vsx_permute (dest, tmp, mode);
}

/* Emit a little-endian store to vector memory location DEST from VSX
   register SOURCE in mode MODE.  The store is done with two permuting
   insn's that represent an xxpermdi and an stxvd2x.  */
void
rs6000_emit_le_vsx_store (rtx dest, rtx source, machine_mode mode)
{
  /* This should never be called during or after LRA, because it does
     not re-permute the source register.  It is intended only for use
     during expand.  */
  gcc_assert (!lra_in_progress && !reload_completed);

  /* Use V2DImode to do swaps of types with 128-bit scalar parts (TImode,
     V1TImode).  */
  if (mode == TImode || mode == V1TImode)
    {
      mode = V2DImode;
      dest = adjust_address (dest, V2DImode, 0);
      source = gen_lowpart (V2DImode, source);
    }

  rtx tmp = can_create_pseudo_p () ? gen_reg_rtx_and_attrs (source) : source;
  rs6000_emit_le_vsx_permute (tmp, source, mode);
  rs6000_emit_le_vsx_permute (dest, tmp, mode);
}

/* Emit a sequence representing a little-endian VSX load or store,
   moving data from SOURCE to DEST in mode MODE.  This is done
   separately from rs6000_emit_move to ensure it is called only
   during expand.  LE VSX loads and stores introduced later are
   handled with a split.  The expand-time RTL generation allows
   us to optimize away redundant pairs of register-permutes.  */
void
rs6000_emit_le_vsx_move (rtx dest, rtx source, machine_mode mode)
{
  gcc_assert (!BYTES_BIG_ENDIAN
	      && VECTOR_MEM_VSX_P (mode)
	      && !TARGET_P9_VECTOR
	      && !gpr_or_gpr_p (dest, source)
	      && (MEM_P (source) ^ MEM_P (dest)));

  if (MEM_P (source))
    {
      gcc_assert (REG_P (dest) || SUBREG_P (dest));
      rs6000_emit_le_vsx_load (dest, source, mode);
    }
  else
    {
      if (!REG_P (source))
	source = force_reg (mode, source);
      rs6000_emit_le_vsx_store (dest, source, mode);
    }
}

/* Return whether a SFmode or SImode move can be done without converting one
   mode to another.  This arrises when we have:

	(SUBREG:SF (REG:SI ...))
	(SUBREG:SI (REG:SF ...))

   and one of the values is in a floating point/vector register, where SFmode
   scalars are stored in DFmode format.  */

bool
valid_sf_si_move (rtx dest, rtx src, machine_mode mode)
{
  if (TARGET_ALLOW_SF_SUBREG)
    return true;

  if (mode != SFmode && GET_MODE_CLASS (mode) != MODE_INT)
    return true;

  if (!SUBREG_P (src) || !sf_subreg_operand (src, mode))
    return true;

  /*.  Allow (set (SUBREG:SI (REG:SF)) (SUBREG:SI (REG:SF))).  */
  if (SUBREG_P (dest))
    {
      rtx dest_subreg = SUBREG_REG (dest);
      rtx src_subreg = SUBREG_REG (src);
      return GET_MODE (dest_subreg) == GET_MODE (src_subreg);
    }

  return false;
}


/* Helper function to change moves with:

	(SUBREG:SF (REG:SI)) and
	(SUBREG:SI (REG:SF))

   into separate UNSPEC insns.  In the PowerPC architecture, scalar SFmode
   values are stored as DFmode values in the VSX registers.  We need to convert
   the bits before we can use a direct move or operate on the bits in the
   vector register as an integer type.

   Skip things like (set (SUBREG:SI (...) (SUBREG:SI (...)).  */

static bool
rs6000_emit_move_si_sf_subreg (rtx dest, rtx source, machine_mode mode)
{
  if (TARGET_DIRECT_MOVE_64BIT && !reload_completed
      && (!SUBREG_P (dest) || !sf_subreg_operand (dest, mode))
      && SUBREG_P (source) && sf_subreg_operand (source, mode))
    {
      rtx inner_source = SUBREG_REG (source);
      machine_mode inner_mode = GET_MODE (inner_source);

      if (mode == SImode && inner_mode == SFmode)
	{
	  emit_insn (gen_movsi_from_sf (dest, inner_source));
	  return true;
	}

      if (mode == SFmode && inner_mode == SImode)
	{
	  emit_insn (gen_movsf_from_si (dest, inner_source));
	  return true;
	}
    }

  return false;
}

/* Emit a move from SOURCE to DEST in mode MODE.  */
void
rs6000_emit_move (rtx dest, rtx source, machine_mode mode)
{
  rtx operands[2];
  operands[0] = dest;
  operands[1] = source;

  if (TARGET_DEBUG_ADDR)
    {
      fprintf (stderr,
	       "\nrs6000_emit_move: mode = %s, lra_in_progress = %d, "
	       "reload_completed = %d, can_create_pseudos = %d.\ndest:\n",
	       GET_MODE_NAME (mode),
	       lra_in_progress,
	       reload_completed,
	       can_create_pseudo_p ());
      debug_rtx (dest);
      fprintf (stderr, "source:\n");
      debug_rtx (source);
    }

  /* Check that we get CONST_WIDE_INT only when we should.  */
  if (CONST_WIDE_INT_P (operands[1])
      && GET_MODE_BITSIZE (mode) <= HOST_BITS_PER_WIDE_INT)
    gcc_unreachable ();

#ifdef HAVE_AS_GNU_ATTRIBUTE
  /* If we use a long double type, set the flags in .gnu_attribute that say
     what the long double type is.  This is to allow the linker's warning
     message for the wrong long double to be useful, even if the function does
     not do a call (for example, doing a 128-bit add on power9 if the long
     double type is IEEE 128-bit.  Do not set this if __ibm128 or __floa128 are
     used if they aren't the default long dobule type.  */
  if (rs6000_gnu_attr && (HAVE_LD_PPC_GNU_ATTR_LONG_DOUBLE || TARGET_64BIT))
    {
      if (TARGET_LONG_DOUBLE_128 && (mode == TFmode || mode == TCmode))
	rs6000_passes_float = rs6000_passes_long_double = true;

      else if (!TARGET_LONG_DOUBLE_128 && (mode == DFmode || mode == DCmode))
	rs6000_passes_float = rs6000_passes_long_double = true;
    }
#endif

  /* See if we need to special case SImode/SFmode SUBREG moves.  */
  if ((mode == SImode || mode == SFmode) && SUBREG_P (source)
      && rs6000_emit_move_si_sf_subreg (dest, source, mode))
    return;

  /* Check if GCC is setting up a block move that will end up using FP
     registers as temporaries.  We must make sure this is acceptable.  */
  if (MEM_P (operands[0])
      && MEM_P (operands[1])
      && mode == DImode
      && (rs6000_slow_unaligned_access (DImode, MEM_ALIGN (operands[0]))
	  || rs6000_slow_unaligned_access (DImode, MEM_ALIGN (operands[1])))
      && ! (rs6000_slow_unaligned_access (SImode,
					  (MEM_ALIGN (operands[0]) > 32
					   ? 32 : MEM_ALIGN (operands[0])))
	    || rs6000_slow_unaligned_access (SImode,
					     (MEM_ALIGN (operands[1]) > 32
					      ? 32 : MEM_ALIGN (operands[1]))))
      && ! MEM_VOLATILE_P (operands [0])
      && ! MEM_VOLATILE_P (operands [1]))
    {
      emit_move_insn (adjust_address (operands[0], SImode, 0),
		      adjust_address (operands[1], SImode, 0));
      emit_move_insn (adjust_address (copy_rtx (operands[0]), SImode, 4),
		      adjust_address (copy_rtx (operands[1]), SImode, 4));
      return;
    }

  if (can_create_pseudo_p () && MEM_P (operands[0])
      && !gpc_reg_operand (operands[1], mode))
    operands[1] = force_reg (mode, operands[1]);

  /* Recognize the case where operand[1] is a reference to thread-local
     data and load its address to a register.  */
  if (tls_referenced_p (operands[1]))
    {
      enum tls_model model;
      rtx tmp = operands[1];
      rtx addend = NULL;

      if (GET_CODE (tmp) == CONST && GET_CODE (XEXP (tmp, 0)) == PLUS)
	{
          addend = XEXP (XEXP (tmp, 0), 1);
	  tmp = XEXP (XEXP (tmp, 0), 0);
	}

      gcc_assert (SYMBOL_REF_P (tmp));
      model = SYMBOL_REF_TLS_MODEL (tmp);
      gcc_assert (model != 0);

      tmp = rs6000_legitimize_tls_address (tmp, model);
      if (addend)
	{
	  tmp = gen_rtx_PLUS (mode, tmp, addend);
	  tmp = force_operand (tmp, operands[0]);
	}
      operands[1] = tmp;
    }

  /* 128-bit constant floating-point values on Darwin should really be loaded
     as two parts.  However, this premature splitting is a problem when DFmode
     values can go into Altivec registers.  */
  if (TARGET_MACHO && CONST_DOUBLE_P (operands[1]) && FLOAT128_IBM_P (mode)
      && !reg_addr[DFmode].scalar_in_vmx_p)
    {
      rs6000_emit_move (simplify_gen_subreg (DFmode, operands[0], mode, 0),
			simplify_gen_subreg (DFmode, operands[1], mode, 0),
			DFmode);
      rs6000_emit_move (simplify_gen_subreg (DFmode, operands[0], mode,
					     GET_MODE_SIZE (DFmode)),
			simplify_gen_subreg (DFmode, operands[1], mode,
					     GET_MODE_SIZE (DFmode)),
			DFmode);
      return;
    }

  /* Transform (p0:DD, (SUBREG:DD p1:SD)) to ((SUBREG:SD p0:DD),
     p1:SD) if p1 is not of floating point class and p0 is spilled as
     we can have no analogous movsd_store for this.  */
  if (lra_in_progress && mode == DDmode
      && REG_P (operands[0]) && !HARD_REGISTER_P (operands[0])
      && reg_preferred_class (REGNO (operands[0])) == NO_REGS
      && SUBREG_P (operands[1]) && REG_P (SUBREG_REG (operands[1]))
      && GET_MODE (SUBREG_REG (operands[1])) == SDmode)
    {
      enum reg_class cl;
      int regno = REGNO (SUBREG_REG (operands[1]));

      if (!HARD_REGISTER_NUM_P (regno))
	{
	  cl = reg_preferred_class (regno);
	  regno = reg_renumber[regno];
	  if (regno < 0)
	    regno = cl == NO_REGS ? -1 : ira_class_hard_regs[cl][1];
	}
      if (regno >= 0 && ! FP_REGNO_P (regno))
	{
	  mode = SDmode;
	  operands[0] = gen_lowpart_SUBREG (SDmode, operands[0]);
	  operands[1] = SUBREG_REG (operands[1]);
	}
    }
  if (lra_in_progress
      && mode == SDmode
      && REG_P (operands[0]) && !HARD_REGISTER_P (operands[0])
      && reg_preferred_class (REGNO (operands[0])) == NO_REGS
      && (REG_P (operands[1])
	  || (SUBREG_P (operands[1]) && REG_P (SUBREG_REG (operands[1])))))
    {
      int regno = reg_or_subregno (operands[1]);
      enum reg_class cl;

      if (!HARD_REGISTER_NUM_P (regno))
	{
	  cl = reg_preferred_class (regno);
	  gcc_assert (cl != NO_REGS);
	  regno = reg_renumber[regno];
	  if (regno < 0)
	    regno = ira_class_hard_regs[cl][0];
	}
      if (FP_REGNO_P (regno))
	{
	  if (GET_MODE (operands[0]) != DDmode)
	    operands[0] = gen_rtx_SUBREG (DDmode, operands[0], 0);
	  emit_insn (gen_movsd_store (operands[0], operands[1]));
	}
      else if (INT_REGNO_P (regno))
	emit_insn (gen_movsd_hardfloat (operands[0], operands[1]));
      else
	gcc_unreachable();
      return;
    }
  /* Transform ((SUBREG:DD p0:SD), p1:DD) to (p0:SD, (SUBREG:SD
     p:DD)) if p0 is not of floating point class and p1 is spilled as
     we can have no analogous movsd_load for this.  */
  if (lra_in_progress && mode == DDmode
      && SUBREG_P (operands[0]) && REG_P (SUBREG_REG (operands[0]))
      && GET_MODE (SUBREG_REG (operands[0])) == SDmode
      && REG_P (operands[1]) && !HARD_REGISTER_P (operands[1])
      && reg_preferred_class (REGNO (operands[1])) == NO_REGS)
    {
      enum reg_class cl;
      int regno = REGNO (SUBREG_REG (operands[0]));

      if (!HARD_REGISTER_NUM_P (regno))
	{
	  cl = reg_preferred_class (regno);
	  regno = reg_renumber[regno];
	  if (regno < 0)
	    regno = cl == NO_REGS ? -1 : ira_class_hard_regs[cl][0];
	}
      if (regno >= 0 && ! FP_REGNO_P (regno))
	{
	  mode = SDmode;
	  operands[0] = SUBREG_REG (operands[0]);
	  operands[1] = gen_lowpart_SUBREG (SDmode, operands[1]);
	}
    }
  if (lra_in_progress
      && mode == SDmode
      && (REG_P (operands[0])
	  || (SUBREG_P (operands[0]) && REG_P (SUBREG_REG (operands[0]))))
      && REG_P (operands[1]) && !HARD_REGISTER_P (operands[1])
      && reg_preferred_class (REGNO (operands[1])) == NO_REGS)
    {
      int regno = reg_or_subregno (operands[0]);
      enum reg_class cl;

      if (!HARD_REGISTER_NUM_P (regno))
	{
	  cl = reg_preferred_class (regno);
	  gcc_assert (cl != NO_REGS);
	  regno = reg_renumber[regno];
	  if (regno < 0)
	    regno = ira_class_hard_regs[cl][0];
	}
      if (FP_REGNO_P (regno))
	{
	  if (GET_MODE (operands[1]) != DDmode)
	    operands[1] = gen_rtx_SUBREG (DDmode, operands[1], 0);
	  emit_insn (gen_movsd_load (operands[0], operands[1]));
	}
      else if (INT_REGNO_P (regno))
	emit_insn (gen_movsd_hardfloat (operands[0], operands[1]));
      else
	gcc_unreachable();
      return;
    }

  /* FIXME:  In the long term, this switch statement should go away
     and be replaced by a sequence of tests based on things like
     mode == Pmode.  */
  switch (mode)
    {
    case E_HImode:
    case E_QImode:
      if (CONSTANT_P (operands[1])
	  && !CONST_INT_P (operands[1]))
	operands[1] = force_const_mem (mode, operands[1]);
      break;

    case E_TFmode:
    case E_TDmode:
    case E_IFmode:
    case E_KFmode:
      if (FLOAT128_2REG_P (mode))
	rs6000_eliminate_indexed_memrefs (operands);
      /* fall through */

    case E_DFmode:
    case E_DDmode:
    case E_SFmode:
    case E_SDmode:
      if (CONSTANT_P (operands[1])
	  && ! easy_fp_constant (operands[1], mode))
	operands[1] = force_const_mem (mode, operands[1]);
      break;

    case E_V16QImode:
    case E_V8HImode:
    case E_V4SFmode:
    case E_V4SImode:
    case E_V2DFmode:
    case E_V2DImode:
    case E_V1TImode:
      if (CONSTANT_P (operands[1])
	  && !easy_vector_constant (operands[1], mode))
	operands[1] = force_const_mem (mode, operands[1]);
      break;

    case E_POImode:
    case E_PXImode:
      if (CONST_INT_P (operands[1]) && INTVAL (operands[1]) != 0)
	error ("%qs is an opaque type, and you can't set it to other values.",
	       (mode == POImode) ? "__vector_pair" : "__vector_quad");
      break;

    case E_SImode:
    case E_DImode:
      /* Use default pattern for address of ELF small data */
      if (TARGET_ELF
	  && mode == Pmode
	  && DEFAULT_ABI == ABI_V4
	  && (SYMBOL_REF_P (operands[1])
	      || GET_CODE (operands[1]) == CONST)
	  && small_data_operand (operands[1], mode))
	{
	  emit_insn (gen_rtx_SET (operands[0], operands[1]));
	  return;
	}

      /* Use the default pattern for loading up PC-relative addresses.  */
      if (TARGET_PCREL && mode == Pmode
	  && pcrel_local_or_external_address (operands[1], Pmode))
	{
	  emit_insn (gen_rtx_SET (operands[0], operands[1]));
	  return;
	}

      if (DEFAULT_ABI == ABI_V4
	  && mode == Pmode && mode == SImode
	  && flag_pic == 1 && got_operand (operands[1], mode))
	{
	  emit_insn (gen_movsi_got (operands[0], operands[1]));
	  return;
	}

      if ((TARGET_ELF || DEFAULT_ABI == ABI_DARWIN)
	  && TARGET_NO_TOC_OR_PCREL
	  && ! flag_pic
	  && mode == Pmode
	  && CONSTANT_P (operands[1])
	  && GET_CODE (operands[1]) != HIGH
	  && !CONST_INT_P (operands[1]))
	{
	  rtx target = (!can_create_pseudo_p ()
			? operands[0]
			: gen_reg_rtx (mode));

	  /* If this is a function address on -mcall-aixdesc,
	     convert it to the address of the descriptor.  */
	  if (DEFAULT_ABI == ABI_AIX
	      && SYMBOL_REF_P (operands[1])
	      && XSTR (operands[1], 0)[0] == '.')
	    {
	      const char *name = XSTR (operands[1], 0);
	      rtx new_ref;
	      while (*name == '.')
		name++;
	      new_ref = gen_rtx_SYMBOL_REF (Pmode, name);
	      CONSTANT_POOL_ADDRESS_P (new_ref)
		= CONSTANT_POOL_ADDRESS_P (operands[1]);
	      SYMBOL_REF_FLAGS (new_ref) = SYMBOL_REF_FLAGS (operands[1]);
	      SYMBOL_REF_USED (new_ref) = SYMBOL_REF_USED (operands[1]);
	      SYMBOL_REF_DATA (new_ref) = SYMBOL_REF_DATA (operands[1]);
	      operands[1] = new_ref;
	    }

	  if (DEFAULT_ABI == ABI_DARWIN)
	    {
#if TARGET_MACHO
	      /* This is not PIC code, but could require the subset of
		 indirections used by mdynamic-no-pic.  */
	      if (MACHO_DYNAMIC_NO_PIC_P)
		{
		  /* Take care of any required data indirection.  */
		  operands[1] = rs6000_machopic_legitimize_pic_address (
				  operands[1], mode, operands[0]);
		  if (operands[0] != operands[1])
		    emit_insn (gen_rtx_SET (operands[0], operands[1]));
		  return;
		}
#endif
	      emit_insn (gen_macho_high (Pmode, target, operands[1]));
	      emit_insn (gen_macho_low (Pmode, operands[0],
					target, operands[1]));
	      return;
	    }

	  emit_insn (gen_elf_high (target, operands[1]));
	  emit_insn (gen_elf_low (operands[0], target, operands[1]));
	  return;
	}

      /* If this is a SYMBOL_REF that refers to a constant pool entry,
	 and we have put it in the TOC, we just need to make a TOC-relative
	 reference to it.  */
      if (TARGET_TOC
	  && SYMBOL_REF_P (operands[1])
	  && use_toc_relative_ref (operands[1], mode))
	operands[1] = create_TOC_reference (operands[1], operands[0]);
      else if (mode == Pmode
	       && CONSTANT_P (operands[1])
	       && GET_CODE (operands[1]) != HIGH
	       && ((REG_P (operands[0])
		    && FP_REGNO_P (REGNO (operands[0])))
		   || !CONST_INT_P (operands[1])
		   || (num_insns_constant (operands[1], mode)
		       > (TARGET_CMODEL != CMODEL_SMALL ? 3 : 2)))
	       && !toc_relative_expr_p (operands[1], false, NULL, NULL)
	       && (TARGET_CMODEL == CMODEL_SMALL
		   || can_create_pseudo_p ()
		   || (REG_P (operands[0])
		       && INT_REG_OK_FOR_BASE_P (operands[0], true))))
	{

#if TARGET_MACHO
	  /* Darwin uses a special PIC legitimizer.  */
	  if (DEFAULT_ABI == ABI_DARWIN && MACHOPIC_INDIRECT)
	    {
	      operands[1] =
		rs6000_machopic_legitimize_pic_address (operands[1], mode,
							operands[0]);
	      if (operands[0] != operands[1])
		emit_insn (gen_rtx_SET (operands[0], operands[1]));
	      return;
	    }
#endif

	  /* If we are to limit the number of things we put in the TOC and
	     this is a symbol plus a constant we can add in one insn,
	     just put the symbol in the TOC and add the constant.  */
	  if (GET_CODE (operands[1]) == CONST
	      && TARGET_NO_SUM_IN_TOC
	      && GET_CODE (XEXP (operands[1], 0)) == PLUS
	      && add_operand (XEXP (XEXP (operands[1], 0), 1), mode)
	      && (GET_CODE (XEXP (XEXP (operands[1], 0), 0)) == LABEL_REF
		  || SYMBOL_REF_P (XEXP (XEXP (operands[1], 0), 0)))
	      && ! side_effects_p (operands[0]))
	    {
	      rtx sym =
		force_const_mem (mode, XEXP (XEXP (operands[1], 0), 0));
	      rtx other = XEXP (XEXP (operands[1], 0), 1);

	      sym = force_reg (mode, sym);
	      emit_insn (gen_add3_insn (operands[0], sym, other));
	      return;
	    }

	  operands[1] = force_const_mem (mode, operands[1]);

	  if (TARGET_TOC
	      && SYMBOL_REF_P (XEXP (operands[1], 0))
	      && use_toc_relative_ref (XEXP (operands[1], 0), mode))
	    {
	      rtx tocref = create_TOC_reference (XEXP (operands[1], 0),
						 operands[0]);
	      operands[1] = gen_const_mem (mode, tocref);
	      set_mem_alias_set (operands[1], get_TOC_alias_set ());
	    }
	}
      break;

    case E_TImode:
      if (!VECTOR_MEM_VSX_P (TImode))
	rs6000_eliminate_indexed_memrefs (operands);
      break;

    case E_PTImode:
      rs6000_eliminate_indexed_memrefs (operands);
      break;

    default:
      fatal_insn ("bad move", gen_rtx_SET (dest, source));
    }

  /* Above, we may have called force_const_mem which may have returned
     an invalid address.  If we can, fix this up; otherwise, reload will
     have to deal with it.  */
  if (MEM_P (operands[1]))
    operands[1] = validize_mem (operands[1]);

  emit_insn (gen_rtx_SET (operands[0], operands[1]));
}


/* Set up AIX/Darwin/64-bit Linux quad floating point routines.  */
static void
init_float128_ibm (machine_mode mode)
{
  if (!TARGET_XL_COMPAT)
    {
      set_optab_libfunc (add_optab, mode, "__gcc_qadd");
      set_optab_libfunc (sub_optab, mode, "__gcc_qsub");
      set_optab_libfunc (smul_optab, mode, "__gcc_qmul");
      set_optab_libfunc (sdiv_optab, mode, "__gcc_qdiv");

      if (!TARGET_HARD_FLOAT)
	{
	  set_optab_libfunc (neg_optab, mode, "__gcc_qneg");
	  set_optab_libfunc (eq_optab, mode, "__gcc_qeq");
	  set_optab_libfunc (ne_optab, mode, "__gcc_qne");
	  set_optab_libfunc (gt_optab, mode, "__gcc_qgt");
	  set_optab_libfunc (ge_optab, mode, "__gcc_qge");
	  set_optab_libfunc (lt_optab, mode, "__gcc_qlt");
	  set_optab_libfunc (le_optab, mode, "__gcc_qle");
	  set_optab_libfunc (unord_optab, mode, "__gcc_qunord");

	  set_conv_libfunc (sext_optab, mode, SFmode, "__gcc_stoq");
	  set_conv_libfunc (sext_optab, mode, DFmode, "__gcc_dtoq");
	  set_conv_libfunc (trunc_optab, SFmode, mode, "__gcc_qtos");
	  set_conv_libfunc (trunc_optab, DFmode, mode, "__gcc_qtod");
	  set_conv_libfunc (sfix_optab, SImode, mode, "__gcc_qtoi");
	  set_conv_libfunc (ufix_optab, SImode, mode, "__gcc_qtou");
	  set_conv_libfunc (sfloat_optab, mode, SImode, "__gcc_itoq");
	  set_conv_libfunc (ufloat_optab, mode, SImode, "__gcc_utoq");
	}
    }
  else
    {
      set_optab_libfunc (add_optab, mode, "_xlqadd");
      set_optab_libfunc (sub_optab, mode, "_xlqsub");
      set_optab_libfunc (smul_optab, mode, "_xlqmul");
      set_optab_libfunc (sdiv_optab, mode, "_xlqdiv");
    }

  /* Add various conversions for IFmode to use the traditional TFmode
     names.  */
  if (mode == IFmode)
    {
      set_conv_libfunc (sext_optab, mode, SDmode, "__dpd_extendsdtf");
      set_conv_libfunc (sext_optab, mode, DDmode, "__dpd_extendddtf");
      set_conv_libfunc (trunc_optab, mode, TDmode, "__dpd_trunctdtf");
      set_conv_libfunc (trunc_optab, SDmode, mode, "__dpd_trunctfsd");
      set_conv_libfunc (trunc_optab, DDmode, mode, "__dpd_trunctfdd");
      set_conv_libfunc (sext_optab, TDmode, mode, "__dpd_extendtftd");

      if (TARGET_POWERPC64)
	{
	  set_conv_libfunc (sfix_optab, TImode, mode, "__fixtfti");
	  set_conv_libfunc (ufix_optab, TImode, mode, "__fixunstfti");
	  set_conv_libfunc (sfloat_optab, mode, TImode, "__floattitf");
	  set_conv_libfunc (ufloat_optab, mode, TImode, "__floatuntitf");
	}
    }
}

/* Create a decl for either complex long double multiply or complex long double
   divide when long double is IEEE 128-bit floating point.  We can't use
   __multc3 and __divtc3 because the original long double using IBM extended
   double used those names.  The complex multiply/divide functions are encoded
   as builtin functions with a complex result and 4 scalar inputs.  */

static void
create_complex_muldiv (const char *name, built_in_function fncode, tree fntype)
{
  tree fndecl = add_builtin_function (name, fntype, fncode, BUILT_IN_NORMAL,
				      name, NULL_TREE);

  set_builtin_decl (fncode, fndecl, true);

  if (TARGET_DEBUG_BUILTIN)
    fprintf (stderr, "create complex %s, fncode: %d\n", name, (int) fncode);

  return;
}

/* Set up IEEE 128-bit floating point routines.  Use different names if the
   arguments can be passed in a vector register.  The historical PowerPC
   implementation of IEEE 128-bit floating point used _q_<op> for the names, so
   continue to use that if we aren't using vector registers to pass IEEE
   128-bit floating point.  */

static void
init_float128_ieee (machine_mode mode)
{
  if (FLOAT128_VECTOR_P (mode))
    {
      static bool complex_muldiv_init_p = false;

      /* Set up to call __mulkc3 and __divkc3 under -mabi=ieeelongdouble.  If
	 we have clone or target attributes, this will be called a second
	 time.  We want to create the built-in function only once.  */
     if (mode == TFmode && TARGET_IEEEQUAD && !complex_muldiv_init_p)
       {
	 complex_muldiv_init_p = true;
	 built_in_function fncode_mul =
	   (built_in_function) (BUILT_IN_COMPLEX_MUL_MIN + TCmode
				- MIN_MODE_COMPLEX_FLOAT);
	 built_in_function fncode_div =
	   (built_in_function) (BUILT_IN_COMPLEX_DIV_MIN + TCmode
				- MIN_MODE_COMPLEX_FLOAT);

	 tree fntype = build_function_type_list (complex_long_double_type_node,
						 long_double_type_node,
						 long_double_type_node,
						 long_double_type_node,
						 long_double_type_node,
						 NULL_TREE);

	 create_complex_muldiv ("__mulkc3", fncode_mul, fntype);
	 create_complex_muldiv ("__divkc3", fncode_div, fntype);
       }

      set_optab_libfunc (add_optab, mode, "__addkf3");
      set_optab_libfunc (sub_optab, mode, "__subkf3");
      set_optab_libfunc (neg_optab, mode, "__negkf2");
      set_optab_libfunc (smul_optab, mode, "__mulkf3");
      set_optab_libfunc (sdiv_optab, mode, "__divkf3");
      set_optab_libfunc (sqrt_optab, mode, "__sqrtkf2");
      set_optab_libfunc (abs_optab, mode, "__abskf2");
      set_optab_libfunc (powi_optab, mode, "__powikf2");

      set_optab_libfunc (eq_optab, mode, "__eqkf2");
      set_optab_libfunc (ne_optab, mode, "__nekf2");
      set_optab_libfunc (gt_optab, mode, "__gtkf2");
      set_optab_libfunc (ge_optab, mode, "__gekf2");
      set_optab_libfunc (lt_optab, mode, "__ltkf2");
      set_optab_libfunc (le_optab, mode, "__lekf2");
      set_optab_libfunc (unord_optab, mode, "__unordkf2");

      set_conv_libfunc (sext_optab, mode, SFmode, "__extendsfkf2");
      set_conv_libfunc (sext_optab, mode, DFmode, "__extenddfkf2");
      set_conv_libfunc (trunc_optab, SFmode, mode, "__trunckfsf2");
      set_conv_libfunc (trunc_optab, DFmode, mode, "__trunckfdf2");

      set_conv_libfunc (sext_optab, mode, IFmode, "__trunctfkf2");
      if (mode != TFmode && FLOAT128_IBM_P (TFmode))
	set_conv_libfunc (sext_optab, mode, TFmode, "__trunctfkf2");

      set_conv_libfunc (trunc_optab, IFmode, mode, "__extendkftf2");
      if (mode != TFmode && FLOAT128_IBM_P (TFmode))
	set_conv_libfunc (trunc_optab, TFmode, mode, "__extendkftf2");

      set_conv_libfunc (sext_optab, mode, SDmode, "__dpd_extendsdkf");
      set_conv_libfunc (sext_optab, mode, DDmode, "__dpd_extendddkf");
      set_conv_libfunc (trunc_optab, mode, TDmode, "__dpd_trunctdkf");
      set_conv_libfunc (trunc_optab, SDmode, mode, "__dpd_trunckfsd");
      set_conv_libfunc (trunc_optab, DDmode, mode, "__dpd_trunckfdd");
      set_conv_libfunc (sext_optab, TDmode, mode, "__dpd_extendkftd");

      set_conv_libfunc (sfix_optab, SImode, mode, "__fixkfsi");
      set_conv_libfunc (ufix_optab, SImode, mode, "__fixunskfsi");
      set_conv_libfunc (sfix_optab, DImode, mode, "__fixkfdi");
      set_conv_libfunc (ufix_optab, DImode, mode, "__fixunskfdi");

      set_conv_libfunc (sfloat_optab, mode, SImode, "__floatsikf");
      set_conv_libfunc (ufloat_optab, mode, SImode, "__floatunsikf");
      set_conv_libfunc (sfloat_optab, mode, DImode, "__floatdikf");
      set_conv_libfunc (ufloat_optab, mode, DImode, "__floatundikf");

      if (TARGET_POWERPC64)
	{
	  set_conv_libfunc (sfix_optab, TImode, mode, "__fixkfti");
	  set_conv_libfunc (ufix_optab, TImode, mode, "__fixunskfti");
	  set_conv_libfunc (sfloat_optab, mode, TImode, "__floattikf");
	  set_conv_libfunc (ufloat_optab, mode, TImode, "__floatuntikf");
	}
    }

  else
    {
      set_optab_libfunc (add_optab, mode, "_q_add");
      set_optab_libfunc (sub_optab, mode, "_q_sub");
      set_optab_libfunc (neg_optab, mode, "_q_neg");
      set_optab_libfunc (smul_optab, mode, "_q_mul");
      set_optab_libfunc (sdiv_optab, mode, "_q_div");
      if (TARGET_PPC_GPOPT)
	set_optab_libfunc (sqrt_optab, mode, "_q_sqrt");

      set_optab_libfunc (eq_optab, mode, "_q_feq");
      set_optab_libfunc (ne_optab, mode, "_q_fne");
      set_optab_libfunc (gt_optab, mode, "_q_fgt");
      set_optab_libfunc (ge_optab, mode, "_q_fge");
      set_optab_libfunc (lt_optab, mode, "_q_flt");
      set_optab_libfunc (le_optab, mode, "_q_fle");

      set_conv_libfunc (sext_optab, mode, SFmode, "_q_stoq");
      set_conv_libfunc (sext_optab, mode, DFmode, "_q_dtoq");
      set_conv_libfunc (trunc_optab, SFmode, mode, "_q_qtos");
      set_conv_libfunc (trunc_optab, DFmode, mode, "_q_qtod");
      set_conv_libfunc (sfix_optab, SImode, mode, "_q_qtoi");
      set_conv_libfunc (ufix_optab, SImode, mode, "_q_qtou");
      set_conv_libfunc (sfloat_optab, mode, SImode, "_q_itoq");
      set_conv_libfunc (ufloat_optab, mode, SImode, "_q_utoq");
    }
}

static void
rs6000_init_libfuncs (void)
{
  /* __float128 support.  */
  if (TARGET_FLOAT128_TYPE)
    {
      init_float128_ibm (IFmode);
      init_float128_ieee (KFmode);
    }

  /* AIX/Darwin/64-bit Linux quad floating point routines.  */
  if (TARGET_LONG_DOUBLE_128)
    {
      if (!TARGET_IEEEQUAD)
	init_float128_ibm (TFmode);

      /* IEEE 128-bit including 32-bit SVR4 quad floating point routines.  */
      else
	init_float128_ieee (TFmode);
    }
}

/* Emit a potentially record-form instruction, setting DST from SRC.
   If DOT is 0, that is all; otherwise, set CCREG to the result of the
   signed comparison of DST with zero.  If DOT is 1, the generated RTL
   doesn't care about the DST result; if DOT is 2, it does.  If CCREG
   is CR0 do a single dot insn (as a PARALLEL); otherwise, do a SET and
   a separate COMPARE.  */

void
rs6000_emit_dot_insn (rtx dst, rtx src, int dot, rtx ccreg)
{
  if (dot == 0)
    {
      emit_move_insn (dst, src);
      return;
    }

  if (cc_reg_not_cr0_operand (ccreg, CCmode))
    {
      emit_move_insn (dst, src);
      emit_move_insn (ccreg, gen_rtx_COMPARE (CCmode, dst, const0_rtx));
      return;
    }

  rtx ccset = gen_rtx_SET (ccreg, gen_rtx_COMPARE (CCmode, src, const0_rtx));
  if (dot == 1)
    {
      rtx clobber = gen_rtx_CLOBBER (VOIDmode, dst);
      emit_insn (gen_rtx_PARALLEL (VOIDmode, gen_rtvec (2, ccset, clobber)));
    }
  else
    {
      rtx set = gen_rtx_SET (dst, src);
      emit_insn (gen_rtx_PARALLEL (VOIDmode, gen_rtvec (2, ccset, set)));
    }
}


/* A validation routine: say whether CODE, a condition code, and MODE
   match.  The other alternatives either don't make sense or should
   never be generated.  */

void
validate_condition_mode (enum rtx_code code, machine_mode mode)
{
  gcc_assert ((GET_RTX_CLASS (code) == RTX_COMPARE
	       || GET_RTX_CLASS (code) == RTX_COMM_COMPARE)
	      && GET_MODE_CLASS (mode) == MODE_CC);

  /* These don't make sense.  */
  gcc_assert ((code != GT && code != LT && code != GE && code != LE)
	      || mode != CCUNSmode);

  gcc_assert ((code != GTU && code != LTU && code != GEU && code != LEU)
	      || mode == CCUNSmode);

  gcc_assert (mode == CCFPmode
	      || (code != ORDERED && code != UNORDERED
		  && code != UNEQ && code != LTGT
		  && code != UNGT && code != UNLT
		  && code != UNGE && code != UNLE));

  /* These are invalid; the information is not there.  */
  gcc_assert (mode != CCEQmode || code == EQ || code == NE);
}


/* Return whether MASK (a CONST_INT) is a valid mask for any rlwinm,
   rldicl, rldicr, or rldic instruction in mode MODE.  If so, if E is
   not zero, store there the bit offset (counted from the right) where
   the single stretch of 1 bits begins; and similarly for B, the bit
   offset where it ends.  */

bool
rs6000_is_valid_mask (rtx mask, int *b, int *e, machine_mode mode)
{
  unsigned HOST_WIDE_INT val = INTVAL (mask);
  unsigned HOST_WIDE_INT bit;
  int nb, ne;
  int n = GET_MODE_PRECISION (mode);

  if (mode != DImode && mode != SImode)
    return false;

  if (INTVAL (mask) >= 0)
    {
      bit = val & -val;
      ne = exact_log2 (bit);
      nb = exact_log2 (val + bit);
    }
  else if (val + 1 == 0)
    {
      nb = n;
      ne = 0;
    }
  else if (val & 1)
    {
      val = ~val;
      bit = val & -val;
      nb = exact_log2 (bit);
      ne = exact_log2 (val + bit);
    }
  else
    {
      bit = val & -val;
      ne = exact_log2 (bit);
      if (val + bit == 0)
	nb = n;
      else
	nb = 0;
    }

  nb--;

  if (nb < 0 || ne < 0 || nb >= n || ne >= n)
    return false;

  if (b)
    *b = nb;
  if (e)
    *e = ne;

  return true;
}

/* Return whether MASK (a CONST_INT) is a valid mask for any rlwinm, rldicl,
   or rldicr instruction, to implement an AND with it in mode MODE.  */

bool
rs6000_is_valid_and_mask (rtx mask, machine_mode mode)
{
  int nb, ne;

  if (!rs6000_is_valid_mask (mask, &nb, &ne, mode))
    return false;

  /* For DImode, we need a rldicl, rldicr, or a rlwinm with mask that
     does not wrap.  */
  if (mode == DImode)
    return (ne == 0 || nb == 63 || (nb < 32 && ne <= nb));

  /* For SImode, rlwinm can do everything.  */
  if (mode == SImode)
    return (nb < 32 && ne < 32);

  return false;
}

/* Return the instruction template for an AND with mask in mode MODE, with
   operands OPERANDS.  If DOT is true, make it a record-form instruction.  */

const char *
rs6000_insn_for_and_mask (machine_mode mode, rtx *operands, bool dot)
{
  int nb, ne;

  if (!rs6000_is_valid_mask (operands[2], &nb, &ne, mode))
    gcc_unreachable ();

  if (mode == DImode && ne == 0)
    {
      operands[3] = GEN_INT (63 - nb);
      if (dot)
	return "rldicl. %0,%1,0,%3";
      return "rldicl %0,%1,0,%3";
    }

  if (mode == DImode && nb == 63)
    {
      operands[3] = GEN_INT (63 - ne);
      if (dot)
	return "rldicr. %0,%1,0,%3";
      return "rldicr %0,%1,0,%3";
    }

  if (nb < 32 && ne < 32)
    {
      operands[3] = GEN_INT (31 - nb);
      operands[4] = GEN_INT (31 - ne);
      if (dot)
	return "rlwinm. %0,%1,0,%3,%4";
      return "rlwinm %0,%1,0,%3,%4";
    }

  gcc_unreachable ();
}

/* Return whether MASK (a CONST_INT) is a valid mask for any rlw[i]nm,
   rld[i]cl, rld[i]cr, or rld[i]c instruction, to implement an AND with
   shift SHIFT (a ROTATE, ASHIFT, or LSHIFTRT) in mode MODE.  */

bool
rs6000_is_valid_shift_mask (rtx mask, rtx shift, machine_mode mode)
{
  int nb, ne;

  if (!rs6000_is_valid_mask (mask, &nb, &ne, mode))
    return false;

  int n = GET_MODE_PRECISION (mode);
  int sh = -1;

  if (CONST_INT_P (XEXP (shift, 1)))
    {
      sh = INTVAL (XEXP (shift, 1));
      if (sh < 0 || sh >= n)
	return false;
    }

  rtx_code code = GET_CODE (shift);

  /* Convert any shift by 0 to a rotate, to simplify below code.  */
  if (sh == 0)
    code = ROTATE;

  /* Convert rotate to simple shift if we can, to make analysis simpler.  */
  if (code == ROTATE && sh >= 0 && nb >= ne && ne >= sh)
    code = ASHIFT;
  if (code == ROTATE && sh >= 0 && nb >= ne && nb < sh)
    {
      code = LSHIFTRT;
      sh = n - sh;
    }

  /* DImode rotates need rld*.  */
  if (mode == DImode && code == ROTATE)
    return (nb == 63 || ne == 0 || ne == sh);

  /* SImode rotates need rlw*.  */
  if (mode == SImode && code == ROTATE)
    return (nb < 32 && ne < 32 && sh < 32);

  /* Wrap-around masks are only okay for rotates.  */
  if (ne > nb)
    return false;

  /* Variable shifts are only okay for rotates.  */
  if (sh < 0)
    return false;

  /* Don't allow ASHIFT if the mask is wrong for that.  */
  if (code == ASHIFT && ne < sh)
    return false;

  /* If we can do it with an rlw*, we can do it.  Don't allow LSHIFTRT
     if the mask is wrong for that.  */
  if (nb < 32 && ne < 32 && sh < 32
      && !(code == LSHIFTRT && nb >= 32 - sh))
    return true;

  /* If we can do it with an rld*, we can do it.  Don't allow LSHIFTRT
     if the mask is wrong for that.  */
  if (code == LSHIFTRT)
    sh = 64 - sh;
  if (nb == 63 || ne == 0 || ne == sh)
    return !(code == LSHIFTRT && nb >= sh);

  return false;
}

/* Return the instruction template for a shift with mask in mode MODE, with
   operands OPERANDS.  If DOT is true, make it a record-form instruction.  */

const char *
rs6000_insn_for_shift_mask (machine_mode mode, rtx *operands, bool dot)
{
  int nb, ne;

  if (!rs6000_is_valid_mask (operands[3], &nb, &ne, mode))
    gcc_unreachable ();

  if (mode == DImode && ne == 0)
    {
      if (GET_CODE (operands[4]) == LSHIFTRT && INTVAL (operands[2]))
	operands[2] = GEN_INT (64 - INTVAL (operands[2]));
      operands[3] = GEN_INT (63 - nb);
      if (dot)
	return "rld%I2cl. %0,%1,%2,%3";
      return "rld%I2cl %0,%1,%2,%3";
    }

  if (mode == DImode && nb == 63)
    {
      operands[3] = GEN_INT (63 - ne);
      if (dot)
	return "rld%I2cr. %0,%1,%2,%3";
      return "rld%I2cr %0,%1,%2,%3";
    }

  if (mode == DImode
      && GET_CODE (operands[4]) != LSHIFTRT
      && CONST_INT_P (operands[2])
      && ne == INTVAL (operands[2]))
    {
      operands[3] = GEN_INT (63 - nb);
      if (dot)
	return "rld%I2c. %0,%1,%2,%3";
      return "rld%I2c %0,%1,%2,%3";
    }

  if (nb < 32 && ne < 32)
    {
      if (GET_CODE (operands[4]) == LSHIFTRT && INTVAL (operands[2]))
	operands[2] = GEN_INT (32 - INTVAL (operands[2]));
      operands[3] = GEN_INT (31 - nb);
      operands[4] = GEN_INT (31 - ne);
      /* This insn can also be a 64-bit rotate with mask that really makes
	 it just a shift right (with mask); the %h below are to adjust for
	 that situation (shift count is >= 32 in that case).  */
      if (dot)
	return "rlw%I2nm. %0,%1,%h2,%3,%4";
      return "rlw%I2nm %0,%1,%h2,%3,%4";
    }

  gcc_unreachable ();
}

/* Return whether MASK (a CONST_INT) is a valid mask for any rlwimi or
   rldimi instruction, to implement an insert with shift SHIFT (a ROTATE,
   ASHIFT, or LSHIFTRT) in mode MODE.  */

bool
rs6000_is_valid_insert_mask (rtx mask, rtx shift, machine_mode mode)
{
  int nb, ne;

  if (!rs6000_is_valid_mask (mask, &nb, &ne, mode))
    return false;

  int n = GET_MODE_PRECISION (mode);

  int sh = INTVAL (XEXP (shift, 1));
  if (sh < 0 || sh >= n)
    return false;

  rtx_code code = GET_CODE (shift);

  /* Convert any shift by 0 to a rotate, to simplify below code.  */
  if (sh == 0)
    code = ROTATE;

  /* Convert rotate to simple shift if we can, to make analysis simpler.  */
  if (code == ROTATE && sh >= 0 && nb >= ne && ne >= sh)
    code = ASHIFT;
  if (code == ROTATE && sh >= 0 && nb >= ne && nb < sh)
    {
      code = LSHIFTRT;
      sh = n - sh;
    }

  /* DImode rotates need rldimi.  */
  if (mode == DImode && code == ROTATE)
    return (ne == sh);

  /* SImode rotates need rlwimi.  */
  if (mode == SImode && code == ROTATE)
    return (nb < 32 && ne < 32 && sh < 32);

  /* Wrap-around masks are only okay for rotates.  */
  if (ne > nb)
    return false;

  /* Don't allow ASHIFT if the mask is wrong for that.  */
  if (code == ASHIFT && ne < sh)
    return false;

  /* If we can do it with an rlwimi, we can do it.  Don't allow LSHIFTRT
     if the mask is wrong for that.  */
  if (nb < 32 && ne < 32 && sh < 32
      && !(code == LSHIFTRT && nb >= 32 - sh))
    return true;

  /* If we can do it with an rldimi, we can do it.  Don't allow LSHIFTRT
     if the mask is wrong for that.  */
  if (code == LSHIFTRT)
    sh = 64 - sh;
  if (ne == sh)
    return !(code == LSHIFTRT && nb >= sh);

  return false;
}

/* Return the instruction template for an insert with mask in mode MODE, with
   operands OPERANDS.  If DOT is true, make it a record-form instruction.  */

const char *
rs6000_insn_for_insert_mask (machine_mode mode, rtx *operands, bool dot)
{
  int nb, ne;

  if (!rs6000_is_valid_mask (operands[3], &nb, &ne, mode))
    gcc_unreachable ();

  /* Prefer rldimi because rlwimi is cracked.  */
  if (TARGET_POWERPC64
      && (!dot || mode == DImode)
      && GET_CODE (operands[4]) != LSHIFTRT
      && ne == INTVAL (operands[2]))
    {
      operands[3] = GEN_INT (63 - nb);
      if (dot)
	return "rldimi. %0,%1,%2,%3";
      return "rldimi %0,%1,%2,%3";
    }

  if (nb < 32 && ne < 32)
    {
      if (GET_CODE (operands[4]) == LSHIFTRT && INTVAL (operands[2]))
	operands[2] = GEN_INT (32 - INTVAL (operands[2]));
      operands[3] = GEN_INT (31 - nb);
      operands[4] = GEN_INT (31 - ne);
      if (dot)
	return "rlwimi. %0,%1,%2,%3,%4";
      return "rlwimi %0,%1,%2,%3,%4";
    }

  gcc_unreachable ();
}

/* Return whether an AND with C (a CONST_INT) in mode MODE can be done
   using two machine instructions.  */

bool
rs6000_is_valid_2insn_and (rtx c, machine_mode mode)
{
  /* There are two kinds of AND we can handle with two insns:
     1) those we can do with two rl* insn;
     2) ori[s];xori[s].

     We do not handle that last case yet.  */

  /* If there is just one stretch of ones, we can do it.  */
  if (rs6000_is_valid_mask (c, NULL, NULL, mode))
    return true;

  /* Otherwise, fill in the lowest "hole"; if we can do the result with
     one insn, we can do the whole thing with two.  */
  unsigned HOST_WIDE_INT val = INTVAL (c);
  unsigned HOST_WIDE_INT bit1 = val & -val;
  unsigned HOST_WIDE_INT bit2 = (val + bit1) & ~val;
  unsigned HOST_WIDE_INT val1 = (val + bit1) & val;
  unsigned HOST_WIDE_INT bit3 = val1 & -val1;
  return rs6000_is_valid_and_mask (GEN_INT (val + bit3 - bit2), mode);
}

/* Emit the two insns to do an AND in mode MODE, with operands OPERANDS.
   If EXPAND is true, split rotate-and-mask instructions we generate to
   their constituent parts as well (this is used during expand); if DOT
   is 1, make the last insn a record-form instruction clobbering the
   destination GPR and setting the CC reg (from operands[3]); if 2, set
   that GPR as well as the CC reg.  */

void
rs6000_emit_2insn_and (machine_mode mode, rtx *operands, bool expand, int dot)
{
  gcc_assert (!(expand && dot));

  unsigned HOST_WIDE_INT val = INTVAL (operands[2]);

  /* If it is one stretch of ones, it is DImode; shift left, mask, then
     shift right.  This generates better code than doing the masks without
     shifts, or shifting first right and then left.  */
  int nb, ne;
  if (rs6000_is_valid_mask (operands[2], &nb, &ne, mode) && nb >= ne)
    {
      gcc_assert (mode == DImode);

      int shift = 63 - nb;
      if (expand)
	{
	  rtx tmp1 = gen_reg_rtx (DImode);
	  rtx tmp2 = gen_reg_rtx (DImode);
	  emit_insn (gen_ashldi3 (tmp1, operands[1], GEN_INT (shift)));
	  emit_insn (gen_anddi3 (tmp2, tmp1, GEN_INT (val << shift)));
	  emit_insn (gen_lshrdi3 (operands[0], tmp2, GEN_INT (shift)));
	}
      else
	{
	  rtx tmp = gen_rtx_ASHIFT (mode, operands[1], GEN_INT (shift));
	  tmp = gen_rtx_AND (mode, tmp, GEN_INT (val << shift));
	  emit_move_insn (operands[0], tmp);
	  tmp = gen_rtx_LSHIFTRT (mode, operands[0], GEN_INT (shift));
	  rs6000_emit_dot_insn (operands[0], tmp, dot, dot ? operands[3] : 0);
	}
      return;
    }

  /* Otherwise, make a mask2 that cuts out the lowest "hole", and a mask1
     that does the rest.  */
  unsigned HOST_WIDE_INT bit1 = val & -val;
  unsigned HOST_WIDE_INT bit2 = (val + bit1) & ~val;
  unsigned HOST_WIDE_INT val1 = (val + bit1) & val;
  unsigned HOST_WIDE_INT bit3 = val1 & -val1;

  unsigned HOST_WIDE_INT mask1 = -bit3 + bit2 - 1;
  unsigned HOST_WIDE_INT mask2 = val + bit3 - bit2;

  gcc_assert (rs6000_is_valid_and_mask (GEN_INT (mask2), mode));

  /* Two "no-rotate"-and-mask instructions, for SImode.  */
  if (rs6000_is_valid_and_mask (GEN_INT (mask1), mode))
    {
      gcc_assert (mode == SImode);

      rtx reg = expand ? gen_reg_rtx (mode) : operands[0];
      rtx tmp = gen_rtx_AND (mode, operands[1], GEN_INT (mask1));
      emit_move_insn (reg, tmp);
      tmp = gen_rtx_AND (mode, reg, GEN_INT (mask2));
      rs6000_emit_dot_insn (operands[0], tmp, dot, dot ? operands[3] : 0);
      return;
    }

  gcc_assert (mode == DImode);

  /* Two "no-rotate"-and-mask instructions, for DImode: both are rlwinm
     insns; we have to do the first in SImode, because it wraps.  */
  if (mask2 <= 0xffffffff
      && rs6000_is_valid_and_mask (GEN_INT (mask1), SImode))
    {
      rtx reg = expand ? gen_reg_rtx (mode) : operands[0];
      rtx tmp = gen_rtx_AND (SImode, gen_lowpart (SImode, operands[1]),
			     GEN_INT (mask1));
      rtx reg_low = gen_lowpart (SImode, reg);
      emit_move_insn (reg_low, tmp);
      tmp = gen_rtx_AND (mode, reg, GEN_INT (mask2));
      rs6000_emit_dot_insn (operands[0], tmp, dot, dot ? operands[3] : 0);
      return;
    }

  /* Two rld* insns: rotate, clear the hole in the middle (which now is
     at the top end), rotate back and clear the other hole.  */
  int right = exact_log2 (bit3);
  int left = 64 - right;

  /* Rotate the mask too.  */
  mask1 = (mask1 >> right) | ((bit2 - 1) << left);

  if (expand)
    {
      rtx tmp1 = gen_reg_rtx (DImode);
      rtx tmp2 = gen_reg_rtx (DImode);
      rtx tmp3 = gen_reg_rtx (DImode);
      emit_insn (gen_rotldi3 (tmp1, operands[1], GEN_INT (left)));
      emit_insn (gen_anddi3 (tmp2, tmp1, GEN_INT (mask1)));
      emit_insn (gen_rotldi3 (tmp3, tmp2, GEN_INT (right)));
      emit_insn (gen_anddi3 (operands[0], tmp3, GEN_INT (mask2)));
    }
  else
    {
      rtx tmp = gen_rtx_ROTATE (mode, operands[1], GEN_INT (left));
      tmp = gen_rtx_AND (mode, tmp, GEN_INT (mask1));
      emit_move_insn (operands[0], tmp);
      tmp = gen_rtx_ROTATE (mode, operands[0], GEN_INT (right));
      tmp = gen_rtx_AND (mode, tmp, GEN_INT (mask2));
      rs6000_emit_dot_insn (operands[0], tmp, dot, dot ? operands[3] : 0);
    }
}

/* Return 1 if REGNO (reg1) == REGNO (reg2) - 1 making them candidates
   for lfq and stfq insns iff the registers are hard registers.   */

int
registers_ok_for_quad_peep (rtx reg1, rtx reg2)
{
  /* We might have been passed a SUBREG.  */
  if (!REG_P (reg1) || !REG_P (reg2))
    return 0;

  /* We might have been passed non floating point registers.  */
  if (!FP_REGNO_P (REGNO (reg1))
      || !FP_REGNO_P (REGNO (reg2)))
    return 0;

  return (REGNO (reg1) == REGNO (reg2) - 1);
}

/* Return 1 if addr1 and addr2 are suitable for lfq or stfq insn.
   addr1 and addr2 must be in consecutive memory locations
   (addr2 == addr1 + 8).  */

int
mems_ok_for_quad_peep (rtx mem1, rtx mem2)
{
  rtx addr1, addr2;
  unsigned int reg1, reg2;
  int offset1, offset2;

  /* The mems cannot be volatile.  */
  if (MEM_VOLATILE_P (mem1) || MEM_VOLATILE_P (mem2))
    return 0;

  addr1 = XEXP (mem1, 0);
  addr2 = XEXP (mem2, 0);

  /* Extract an offset (if used) from the first addr.  */
  if (GET_CODE (addr1) == PLUS)
    {
      /* If not a REG, return zero.  */
      if (!REG_P (XEXP (addr1, 0)))
	return 0;
      else
	{
	  reg1 = REGNO (XEXP (addr1, 0));
	  /* The offset must be constant!  */
	  if (!CONST_INT_P (XEXP (addr1, 1)))
	    return 0;
	  offset1 = INTVAL (XEXP (addr1, 1));
	}
    }
  else if (!REG_P (addr1))
    return 0;
  else
    {
      reg1 = REGNO (addr1);
      /* This was a simple (mem (reg)) expression.  Offset is 0.  */
      offset1 = 0;
    }

  /* And now for the second addr.  */
  if (GET_CODE (addr2) == PLUS)
    {
      /* If not a REG, return zero.  */
      if (!REG_P (XEXP (addr2, 0)))
	return 0;
      else
	{
	  reg2 = REGNO (XEXP (addr2, 0));
	  /* The offset must be constant. */
	  if (!CONST_INT_P (XEXP (addr2, 1)))
	    return 0;
	  offset2 = INTVAL (XEXP (addr2, 1));
	}
    }
  else if (!REG_P (addr2))
    return 0;
  else
    {
      reg2 = REGNO (addr2);
      /* This was a simple (mem (reg)) expression.  Offset is 0.  */
      offset2 = 0;
    }

  /* Both of these must have the same base register.  */
  if (reg1 != reg2)
    return 0;

  /* The offset for the second addr must be 8 more than the first addr.  */
  if (offset2 != offset1 + 8)
    return 0;

  /* All the tests passed.  addr1 and addr2 are valid for lfq or stfq
     instructions.  */
  return 1;
}

/* Implement TARGET_SECONDARY_RELOAD_NEEDED_MODE.  For SDmode values we
   need to use DDmode, in all other cases we can use the same mode.  */
static machine_mode
rs6000_secondary_memory_needed_mode (machine_mode mode)
{
  if (lra_in_progress && mode == SDmode)
    return DDmode;
  return mode;
}

/* Classify a register type.  Because the FMRGOW/FMRGEW instructions only work
   on traditional floating point registers, and the VMRGOW/VMRGEW instructions
   only work on the traditional altivec registers, note if an altivec register
   was chosen.  */

static enum rs6000_reg_type
register_to_reg_type (rtx reg, bool *is_altivec)
{
  HOST_WIDE_INT regno;
  enum reg_class rclass;

  if (SUBREG_P (reg))
    reg = SUBREG_REG (reg);

  if (!REG_P (reg))
    return NO_REG_TYPE;

  regno = REGNO (reg);
  if (!HARD_REGISTER_NUM_P (regno))
    {
      if (!lra_in_progress && !reload_completed)
	return PSEUDO_REG_TYPE;

      regno = true_regnum (reg);
      if (regno < 0 || !HARD_REGISTER_NUM_P (regno))
	return PSEUDO_REG_TYPE;
    }

  gcc_assert (regno >= 0);

  if (is_altivec && ALTIVEC_REGNO_P (regno))
    *is_altivec = true;

  rclass = rs6000_regno_regclass[regno];
  return reg_class_to_reg_type[(int)rclass];
}

/* Helper function to return the cost of adding a TOC entry address.  */

static inline int
rs6000_secondary_reload_toc_costs (addr_mask_type addr_mask)
{
  int ret;

  if (TARGET_CMODEL != CMODEL_SMALL)
    ret = ((addr_mask & RELOAD_REG_OFFSET) == 0) ? 1 : 2;

  else
    ret = (TARGET_MINIMAL_TOC) ? 6 : 3;

  return ret;
}

/* Helper function for rs6000_secondary_reload to determine whether the memory
   address (ADDR) with a given register class (RCLASS) and machine mode (MODE)
   needs reloading.  Return negative if the memory is not handled by the memory
   helper functions and to try a different reload method, 0 if no additional
   instructions are need, and positive to give the extra cost for the
   memory.  */

static int
rs6000_secondary_reload_memory (rtx addr,
				enum reg_class rclass,
				machine_mode mode)
{
  int extra_cost = 0;
  rtx reg, and_arg, plus_arg0, plus_arg1;
  addr_mask_type addr_mask;
  const char *type = NULL;
  const char *fail_msg = NULL;

  if (GPR_REG_CLASS_P (rclass))
    addr_mask = reg_addr[mode].addr_mask[RELOAD_REG_GPR];

  else if (rclass == FLOAT_REGS)
    addr_mask = reg_addr[mode].addr_mask[RELOAD_REG_FPR];

  else if (rclass == ALTIVEC_REGS)
    addr_mask = reg_addr[mode].addr_mask[RELOAD_REG_VMX];

  /* For the combined VSX_REGS, turn off Altivec AND -16.  */
  else if (rclass == VSX_REGS)
    addr_mask = (reg_addr[mode].addr_mask[RELOAD_REG_VMX]
		 & ~RELOAD_REG_AND_M16);

  /* If the register allocator hasn't made up its mind yet on the register
     class to use, settle on defaults to use.  */
  else if (rclass == NO_REGS)
    {
      addr_mask = (reg_addr[mode].addr_mask[RELOAD_REG_ANY]
		   & ~RELOAD_REG_AND_M16);

      if ((addr_mask & RELOAD_REG_MULTIPLE) != 0)
	addr_mask &= ~(RELOAD_REG_INDEXED
		       | RELOAD_REG_PRE_INCDEC
		       | RELOAD_REG_PRE_MODIFY);
    }

  else
    addr_mask = 0;

  /* If the register isn't valid in this register class, just return now.  */
  if ((addr_mask & RELOAD_REG_VALID) == 0)
    {
      if (TARGET_DEBUG_ADDR)
	{
	  fprintf (stderr,
		   "rs6000_secondary_reload_memory: mode = %s, class = %s, "
		   "not valid in class\n",
		   GET_MODE_NAME (mode), reg_class_names[rclass]);
	  debug_rtx (addr);
	}

      return -1;
    }

  switch (GET_CODE (addr))
    {
      /* Does the register class supports auto update forms for this mode?  We
	 don't need a scratch register, since the powerpc only supports
	 PRE_INC, PRE_DEC, and PRE_MODIFY.  */
    case PRE_INC:
    case PRE_DEC:
      reg = XEXP (addr, 0);
      if (!base_reg_operand (addr, GET_MODE (reg)))
	{
	  fail_msg = "no base register #1";
	  extra_cost = -1;
	}

      else if ((addr_mask & RELOAD_REG_PRE_INCDEC) == 0)
	{
	  extra_cost = 1;
	  type = "update";
	}
      break;

    case PRE_MODIFY:
      reg = XEXP (addr, 0);
      plus_arg1 = XEXP (addr, 1);
      if (!base_reg_operand (reg, GET_MODE (reg))
	  || GET_CODE (plus_arg1) != PLUS
	  || !rtx_equal_p (reg, XEXP (plus_arg1, 0)))
	{
	  fail_msg = "bad PRE_MODIFY";
	  extra_cost = -1;
	}

      else if ((addr_mask & RELOAD_REG_PRE_MODIFY) == 0)
	{
	  extra_cost = 1;
	  type = "update";
	}
      break;

      /* Do we need to simulate AND -16 to clear the bottom address bits used
	 in VMX load/stores?  Only allow the AND for vector sizes.  */
    case AND:
      and_arg = XEXP (addr, 0);
      if (GET_MODE_SIZE (mode) != 16
	  || !CONST_INT_P (XEXP (addr, 1))
	  || INTVAL (XEXP (addr, 1)) != -16)
	{
	  fail_msg = "bad Altivec AND #1";
	  extra_cost = -1;
	}

      if (rclass != ALTIVEC_REGS)
	{
	  if (legitimate_indirect_address_p (and_arg, false))
	    extra_cost = 1;

	  else if (legitimate_indexed_address_p (and_arg, false))
	    extra_cost = 2;

	  else
	    {
	      fail_msg = "bad Altivec AND #2";
	      extra_cost = -1;
	    }

	  type = "and";
	}
      break;

      /* If this is an indirect address, make sure it is a base register.  */
    case REG:
    case SUBREG:
      if (!legitimate_indirect_address_p (addr, false))
	{
	  extra_cost = 1;
	  type = "move";
	}
      break;

      /* If this is an indexed address, make sure the register class can handle
	 indexed addresses for this mode.  */
    case PLUS:
      plus_arg0 = XEXP (addr, 0);
      plus_arg1 = XEXP (addr, 1);

      /* (plus (plus (reg) (constant)) (constant)) is generated during
	 push_reload processing, so handle it now.  */
      if (GET_CODE (plus_arg0) == PLUS && CONST_INT_P (plus_arg1))
	{
	  if ((addr_mask & RELOAD_REG_OFFSET) == 0)
	    {
	      extra_cost = 1;
	      type = "offset";
	    }
	}

      /* (plus (plus (reg) (constant)) (reg)) is also generated during
	 push_reload processing, so handle it now.  */
      else if (GET_CODE (plus_arg0) == PLUS && REG_P (plus_arg1))
	{
	  if ((addr_mask & RELOAD_REG_INDEXED) == 0)
	    {
	      extra_cost = 1;
	      type = "indexed #2";
	    }
	}

      else if (!base_reg_operand (plus_arg0, GET_MODE (plus_arg0)))
	{
	  fail_msg = "no base register #2";
	  extra_cost = -1;
	}

      else if (int_reg_operand (plus_arg1, GET_MODE (plus_arg1)))
	{
	  if ((addr_mask & RELOAD_REG_INDEXED) == 0
	      || !legitimate_indexed_address_p (addr, false))
	    {
	      extra_cost = 1;
	      type = "indexed";
	    }
	}

      else if ((addr_mask & RELOAD_REG_QUAD_OFFSET) != 0
	       && CONST_INT_P (plus_arg1))
	{
	  if (!quad_address_offset_p (INTVAL (plus_arg1)))
	    {
	      extra_cost = 1;
	      type = "vector d-form offset";
	    }
	}

      /* Make sure the register class can handle offset addresses.  */
      else if (rs6000_legitimate_offset_address_p (mode, addr, false, true))
	{
	  if ((addr_mask & RELOAD_REG_OFFSET) == 0)
	    {
	      extra_cost = 1;
	      type = "offset #2";
	    }
	}

      else
	{
	  fail_msg = "bad PLUS";
	  extra_cost = -1;
	}

      break;

    case LO_SUM:
      /* Quad offsets are restricted and can't handle normal addresses.  */
      if ((addr_mask & RELOAD_REG_QUAD_OFFSET) != 0)
	{
	  extra_cost = -1;
	  type = "vector d-form lo_sum";
	}

      else if (!legitimate_lo_sum_address_p (mode, addr, false))
	{
	  fail_msg = "bad LO_SUM";
	  extra_cost = -1;
	}

      if ((addr_mask & RELOAD_REG_OFFSET) == 0)
	{
	  extra_cost = 1;
	  type = "lo_sum";
	}
      break;

      /* Static addresses need to create a TOC entry.  */
    case CONST:
    case SYMBOL_REF:
    case LABEL_REF:
      if ((addr_mask & RELOAD_REG_QUAD_OFFSET) != 0)
	{
	  extra_cost = -1;
	  type = "vector d-form lo_sum #2";
	}

      else
	{
	  type = "address";
	  extra_cost = rs6000_secondary_reload_toc_costs (addr_mask);
	}
      break;

      /* TOC references look like offsetable memory.  */
    case UNSPEC:
      if (TARGET_CMODEL == CMODEL_SMALL || XINT (addr, 1) != UNSPEC_TOCREL)
	{
	  fail_msg = "bad UNSPEC";
	  extra_cost = -1;
	}

      else if ((addr_mask & RELOAD_REG_QUAD_OFFSET) != 0)
	{
	  extra_cost = -1;
	  type = "vector d-form lo_sum #3";
	}

      else if ((addr_mask & RELOAD_REG_OFFSET) == 0)
	{
	  extra_cost = 1;
	  type = "toc reference";
	}
      break;

    default:
	{
	  fail_msg = "bad address";
	  extra_cost = -1;
	}
    }

  if (TARGET_DEBUG_ADDR /* && extra_cost != 0 */)
    {
      if (extra_cost < 0)
	fprintf (stderr,
		 "rs6000_secondary_reload_memory error: mode = %s, "
		 "class = %s, addr_mask = '%s', %s\n",
		 GET_MODE_NAME (mode),
		 reg_class_names[rclass],
		 rs6000_debug_addr_mask (addr_mask, false),
		 (fail_msg != NULL) ? fail_msg : "<bad address>");

      else
	fprintf (stderr,
		 "rs6000_secondary_reload_memory: mode = %s, class = %s, "
		 "addr_mask = '%s', extra cost = %d, %s\n",
		 GET_MODE_NAME (mode),
		 reg_class_names[rclass],
		 rs6000_debug_addr_mask (addr_mask, false),
		 extra_cost,
		 (type) ? type : "<none>");

      debug_rtx (addr);
    }

  return extra_cost;
}

/* Helper function for rs6000_secondary_reload to return true if a move to a
   different register classe is really a simple move.  */

static bool
rs6000_secondary_reload_simple_move (enum rs6000_reg_type to_type,
				     enum rs6000_reg_type from_type,
				     machine_mode mode)
{
  int size = GET_MODE_SIZE (mode);

  /* Add support for various direct moves available.  In this function, we only
     look at cases where we don't need any extra registers, and one or more
     simple move insns are issued.  Originally small integers are not allowed
     in FPR/VSX registers.  Single precision binary floating is not a simple
     move because we need to convert to the single precision memory layout.
     The 4-byte SDmode can be moved.  TDmode values are disallowed since they
     need special direct move handling, which we do not support yet.  */
  if (TARGET_DIRECT_MOVE
      && ((to_type == GPR_REG_TYPE && from_type == VSX_REG_TYPE)
	  || (to_type == VSX_REG_TYPE && from_type == GPR_REG_TYPE)))
    {
      if (TARGET_POWERPC64)
	{
	  /* ISA 2.07: MTVSRD or MVFVSRD.  */
	  if (size == 8)
	    return true;

	  /* ISA 3.0: MTVSRDD or MFVSRD + MFVSRLD.  */
	  if (size == 16 && TARGET_P9_VECTOR && mode != TDmode)
	    return true;
	}

      /* ISA 2.07: MTVSRWZ or  MFVSRWZ.  */
      if (TARGET_P8_VECTOR)
	{
	  if (mode == SImode)
	    return true;

	  if (TARGET_P9_VECTOR && (mode == HImode || mode == QImode))
	    return true;
	}

      /* ISA 2.07: MTVSRWZ or  MFVSRWZ.  */
      if (mode == SDmode)
	return true;
    }

  /* Move to/from SPR.  */
  else if ((size == 4 || (TARGET_POWERPC64 && size == 8))
	   && ((to_type == GPR_REG_TYPE && from_type == SPR_REG_TYPE)
	       || (to_type == SPR_REG_TYPE && from_type == GPR_REG_TYPE)))
    return true;

  return false;
}

/* Direct move helper function for rs6000_secondary_reload, handle all of the
   special direct moves that involve allocating an extra register, return the
   insn code of the helper function if there is such a function or
   CODE_FOR_nothing if not.  */

static bool
rs6000_secondary_reload_direct_move (enum rs6000_reg_type to_type,
				     enum rs6000_reg_type from_type,
				     machine_mode mode,
				     secondary_reload_info *sri,
				     bool altivec_p)
{
  bool ret = false;
  enum insn_code icode = CODE_FOR_nothing;
  int cost = 0;
  int size = GET_MODE_SIZE (mode);

  if (TARGET_POWERPC64 && size == 16)
    {
      /* Handle moving 128-bit values from GPRs to VSX point registers on
	 ISA 2.07 (power8, power9) when running in 64-bit mode using
	 XXPERMDI to glue the two 64-bit values back together.  */
      if (to_type == VSX_REG_TYPE && from_type == GPR_REG_TYPE)
	{
	  cost = 3;			/* 2 mtvsrd's, 1 xxpermdi.  */
	  icode = reg_addr[mode].reload_vsx_gpr;
	}

      /* Handle moving 128-bit values from VSX point registers to GPRs on
	 ISA 2.07 when running in 64-bit mode using XXPERMDI to get access to the
	 bottom 64-bit value.  */
      else if (to_type == GPR_REG_TYPE && from_type == VSX_REG_TYPE)
	{
	  cost = 3;			/* 2 mfvsrd's, 1 xxpermdi.  */
	  icode = reg_addr[mode].reload_gpr_vsx;
	}
    }

  else if (TARGET_POWERPC64 && mode == SFmode)
    {
      if (to_type == GPR_REG_TYPE && from_type == VSX_REG_TYPE)
	{
	  cost = 3;			/* xscvdpspn, mfvsrd, and.  */
	  icode = reg_addr[mode].reload_gpr_vsx;
	}

      else if (to_type == VSX_REG_TYPE && from_type == GPR_REG_TYPE)
	{
	  cost = 2;			/* mtvsrz, xscvspdpn.  */
	  icode = reg_addr[mode].reload_vsx_gpr;
	}
    }

  else if (!TARGET_POWERPC64 && size == 8)
    {
      /* Handle moving 64-bit values from GPRs to floating point registers on
	 ISA 2.07 when running in 32-bit mode using FMRGOW to glue the two
	 32-bit values back together.  Altivec register classes must be handled
	 specially since a different instruction is used, and the secondary
	 reload support requires a single instruction class in the scratch
	 register constraint.  However, right now TFmode is not allowed in
	 Altivec registers, so the pattern will never match.  */
      if (to_type == VSX_REG_TYPE && from_type == GPR_REG_TYPE && !altivec_p)
	{
	  cost = 3;			/* 2 mtvsrwz's, 1 fmrgow.  */
	  icode = reg_addr[mode].reload_fpr_gpr;
	}
    }

  if (icode != CODE_FOR_nothing)
    {
      ret = true;
      if (sri)
	{
	  sri->icode = icode;
	  sri->extra_cost = cost;
	}
    }

  return ret;
}

/* Return whether a move between two register classes can be done either
   directly (simple move) or via a pattern that uses a single extra temporary
   (using ISA 2.07's direct move in this case.  */

static bool
rs6000_secondary_reload_move (enum rs6000_reg_type to_type,
			      enum rs6000_reg_type from_type,
			      machine_mode mode,
			      secondary_reload_info *sri,
			      bool altivec_p)
{
  /* Fall back to load/store reloads if either type is not a register.  */
  if (to_type == NO_REG_TYPE || from_type == NO_REG_TYPE)
    return false;

  /* If we haven't allocated registers yet, assume the move can be done for the
     standard register types.  */
  if ((to_type == PSEUDO_REG_TYPE && from_type == PSEUDO_REG_TYPE)
      || (to_type == PSEUDO_REG_TYPE && IS_STD_REG_TYPE (from_type))
      || (from_type == PSEUDO_REG_TYPE && IS_STD_REG_TYPE (to_type)))
    return true;

  /* Moves to the same set of registers is a simple move for non-specialized
     registers.  */
  if (to_type == from_type && IS_STD_REG_TYPE (to_type))
    return true;

  /* Check whether a simple move can be done directly.  */
  if (rs6000_secondary_reload_simple_move (to_type, from_type, mode))
    {
      if (sri)
	{
	  sri->icode = CODE_FOR_nothing;
	  sri->extra_cost = 0;
	}
      return true;
    }

  /* Now check if we can do it in a few steps.  */
  return rs6000_secondary_reload_direct_move (to_type, from_type, mode, sri,
					      altivec_p);
}

/* Inform reload about cases where moving X with a mode MODE to a register in
   RCLASS requires an extra scratch or immediate register.  Return the class
   needed for the immediate register.

   For VSX and Altivec, we may need a register to convert sp+offset into
   reg+sp.

   For misaligned 64-bit gpr loads and stores we need a register to
   convert an offset address to indirect.  */

static reg_class_t
rs6000_secondary_reload (bool in_p,
			 rtx x,
			 reg_class_t rclass_i,
			 machine_mode mode,
			 secondary_reload_info *sri)
{
  enum reg_class rclass = (enum reg_class) rclass_i;
  reg_class_t ret = ALL_REGS;
  enum insn_code icode;
  bool default_p = false;
  bool done_p = false;

  /* Allow subreg of memory before/during reload.  */
  bool memory_p = (MEM_P (x)
		   || (!reload_completed && SUBREG_P (x)
		       && MEM_P (SUBREG_REG (x))));

  sri->icode = CODE_FOR_nothing;
  sri->t_icode = CODE_FOR_nothing;
  sri->extra_cost = 0;
  icode = ((in_p)
	   ? reg_addr[mode].reload_load
	   : reg_addr[mode].reload_store);

  if (REG_P (x) || register_operand (x, mode))
    {
      enum rs6000_reg_type to_type = reg_class_to_reg_type[(int)rclass];
      bool altivec_p = (rclass == ALTIVEC_REGS);
      enum rs6000_reg_type from_type = register_to_reg_type (x, &altivec_p);

      if (!in_p)
	std::swap (to_type, from_type);

      /* Can we do a direct move of some sort?  */
      if (rs6000_secondary_reload_move (to_type, from_type, mode, sri,
					altivec_p))
	{
	  icode = (enum insn_code)sri->icode;
	  default_p = false;
	  done_p = true;
	  ret = NO_REGS;
	}
    }

  /* Make sure 0.0 is not reloaded or forced into memory.  */
  if (x == CONST0_RTX (mode) && VSX_REG_CLASS_P (rclass))
    {
      ret = NO_REGS;
      default_p = false;
      done_p = true;
    }

  /* If this is a scalar floating point value and we want to load it into the
     traditional Altivec registers, do it via a move via a traditional floating
     point register, unless we have D-form addressing.  Also make sure that
     non-zero constants use a FPR.  */
  if (!done_p && reg_addr[mode].scalar_in_vmx_p
      && !mode_supports_vmx_dform (mode)
      && (rclass == VSX_REGS || rclass == ALTIVEC_REGS)
      && (memory_p || CONST_DOUBLE_P (x)))
    {
      ret = FLOAT_REGS;
      default_p = false;
      done_p = true;
    }

  /* Handle reload of load/stores if we have reload helper functions.  */
  if (!done_p && icode != CODE_FOR_nothing && memory_p)
    {
      int extra_cost = rs6000_secondary_reload_memory (XEXP (x, 0), rclass,
						       mode);

      if (extra_cost >= 0)
	{
	  done_p = true;
	  ret = NO_REGS;
	  if (extra_cost > 0)
	    {
	      sri->extra_cost = extra_cost;
	      sri->icode = icode;
	    }
	}
    }

  /* Handle unaligned loads and stores of integer registers.  */
  if (!done_p && TARGET_POWERPC64
      && reg_class_to_reg_type[(int)rclass] == GPR_REG_TYPE
      && memory_p
      && GET_MODE_SIZE (GET_MODE (x)) >= UNITS_PER_WORD)
    {
      rtx addr = XEXP (x, 0);
      rtx off = address_offset (addr);

      if (off != NULL_RTX)
	{
	  unsigned int extra = GET_MODE_SIZE (GET_MODE (x)) - UNITS_PER_WORD;
	  unsigned HOST_WIDE_INT offset = INTVAL (off);

	  /* We need a secondary reload when our legitimate_address_p
	     says the address is good (as otherwise the entire address
	     will be reloaded), and the offset is not a multiple of
	     four or we have an address wrap.  Address wrap will only
	     occur for LO_SUMs since legitimate_offset_address_p
	     rejects addresses for 16-byte mems that will wrap.  */
	  if (GET_CODE (addr) == LO_SUM
	      ? (1 /* legitimate_address_p allows any offset for lo_sum */
		 && ((offset & 3) != 0
		     || ((offset & 0xffff) ^ 0x8000) >= 0x10000 - extra))
	      : (offset + 0x8000 < 0x10000 - extra /* legitimate_address_p */
		 && (offset & 3) != 0))
	    {
	      /* -m32 -mpowerpc64 needs to use a 32-bit scratch register.  */
	      if (in_p)
		sri->icode = ((TARGET_32BIT) ? CODE_FOR_reload_si_load
			      : CODE_FOR_reload_di_load);
	      else
		sri->icode = ((TARGET_32BIT) ? CODE_FOR_reload_si_store
			      : CODE_FOR_reload_di_store);
	      sri->extra_cost = 2;
	      ret = NO_REGS;
	      done_p = true;
	    }
	  else
	    default_p = true;
	}
      else
	default_p = true;
    }

  if (!done_p && !TARGET_POWERPC64
      && reg_class_to_reg_type[(int)rclass] == GPR_REG_TYPE
      && memory_p
      && GET_MODE_SIZE (GET_MODE (x)) > UNITS_PER_WORD)
    {
      rtx addr = XEXP (x, 0);
      rtx off = address_offset (addr);

      if (off != NULL_RTX)
	{
	  unsigned int extra = GET_MODE_SIZE (GET_MODE (x)) - UNITS_PER_WORD;
	  unsigned HOST_WIDE_INT offset = INTVAL (off);

	  /* We need a secondary reload when our legitimate_address_p
	     says the address is good (as otherwise the entire address
	     will be reloaded), and we have a wrap.

	     legitimate_lo_sum_address_p allows LO_SUM addresses to
	     have any offset so test for wrap in the low 16 bits.

	     legitimate_offset_address_p checks for the range
	     [-0x8000,0x7fff] for mode size of 8 and [-0x8000,0x7ff7]
	     for mode size of 16.  We wrap at [0x7ffc,0x7fff] and
	     [0x7ff4,0x7fff] respectively, so test for the
	     intersection of these ranges, [0x7ffc,0x7fff] and
	     [0x7ff4,0x7ff7] respectively.

	     Note that the address we see here may have been
	     manipulated by legitimize_reload_address.  */
	  if (GET_CODE (addr) == LO_SUM
	      ? ((offset & 0xffff) ^ 0x8000) >= 0x10000 - extra
	      : offset - (0x8000 - extra) < UNITS_PER_WORD)
	    {
	      if (in_p)
		sri->icode = CODE_FOR_reload_si_load;
	      else
		sri->icode = CODE_FOR_reload_si_store;
	      sri->extra_cost = 2;
	      ret = NO_REGS;
	      done_p = true;
	    }
	  else
	    default_p = true;
	}
      else
	default_p = true;
    }

  if (!done_p)
    default_p = true;

  if (default_p)
    ret = default_secondary_reload (in_p, x, rclass, mode, sri);

  gcc_assert (ret != ALL_REGS);

  if (TARGET_DEBUG_ADDR)
    {
      fprintf (stderr,
	       "\nrs6000_secondary_reload, return %s, in_p = %s, rclass = %s, "
	       "mode = %s",
	       reg_class_names[ret],
	       in_p ? "true" : "false",
	       reg_class_names[rclass],
	       GET_MODE_NAME (mode));

      if (reload_completed)
	fputs (", after reload", stderr);

      if (!done_p)
	fputs (", done_p not set", stderr);

      if (default_p)
	fputs (", default secondary reload", stderr);

      if (sri->icode != CODE_FOR_nothing)
	fprintf (stderr, ", reload func = %s, extra cost = %d",
		 insn_data[sri->icode].name, sri->extra_cost);

      else if (sri->extra_cost > 0)
	fprintf (stderr, ", extra cost = %d", sri->extra_cost);

      fputs ("\n", stderr);
      debug_rtx (x);
    }

  return ret;
}

/* Better tracing for rs6000_secondary_reload_inner.  */

static void
rs6000_secondary_reload_trace (int line, rtx reg, rtx mem, rtx scratch,
			       bool store_p)
{
  rtx set, clobber;

  gcc_assert (reg != NULL_RTX && mem != NULL_RTX && scratch != NULL_RTX);

  fprintf (stderr, "rs6000_secondary_reload_inner:%d, type = %s\n", line,
	   store_p ? "store" : "load");

  if (store_p)
    set = gen_rtx_SET (mem, reg);
  else
    set = gen_rtx_SET (reg, mem);

  clobber = gen_rtx_CLOBBER (VOIDmode, scratch);
  debug_rtx (gen_rtx_PARALLEL (VOIDmode, gen_rtvec (2, set, clobber)));
}

static void rs6000_secondary_reload_fail (int, rtx, rtx, rtx, bool)
  ATTRIBUTE_NORETURN;

static void
rs6000_secondary_reload_fail (int line, rtx reg, rtx mem, rtx scratch,
			      bool store_p)
{
  rs6000_secondary_reload_trace (line, reg, mem, scratch, store_p);
  gcc_unreachable ();
}

/* Fixup reload addresses for values in GPR, FPR, and VMX registers that have
   reload helper functions.  These were identified in
   rs6000_secondary_reload_memory, and if reload decided to use the secondary
   reload, it calls the insns:
	reload_<RELOAD:mode>_<P:mptrsize>_store
	reload_<RELOAD:mode>_<P:mptrsize>_load

   which in turn calls this function, to do whatever is necessary to create
   valid addresses.  */

void
rs6000_secondary_reload_inner (rtx reg, rtx mem, rtx scratch, bool store_p)
{
  int regno = true_regnum (reg);
  machine_mode mode = GET_MODE (reg);
  addr_mask_type addr_mask;
  rtx addr;
  rtx new_addr;
  rtx op_reg, op0, op1;
  rtx and_op;
  rtx cc_clobber;
  rtvec rv;

  if (regno < 0 || !HARD_REGISTER_NUM_P (regno) || !MEM_P (mem)
      || !base_reg_operand (scratch, GET_MODE (scratch)))
    rs6000_secondary_reload_fail (__LINE__, reg, mem, scratch, store_p);

  if (IN_RANGE (regno, FIRST_GPR_REGNO, LAST_GPR_REGNO))
    addr_mask = reg_addr[mode].addr_mask[RELOAD_REG_GPR];

  else if (IN_RANGE (regno, FIRST_FPR_REGNO, LAST_FPR_REGNO))
    addr_mask = reg_addr[mode].addr_mask[RELOAD_REG_FPR];

  else if (IN_RANGE (regno, FIRST_ALTIVEC_REGNO, LAST_ALTIVEC_REGNO))
    addr_mask = reg_addr[mode].addr_mask[RELOAD_REG_VMX];

  else
    rs6000_secondary_reload_fail (__LINE__, reg, mem, scratch, store_p);

  /* Make sure the mode is valid in this register class.  */
  if ((addr_mask & RELOAD_REG_VALID) == 0)
    rs6000_secondary_reload_fail (__LINE__, reg, mem, scratch, store_p);

  if (TARGET_DEBUG_ADDR)
    rs6000_secondary_reload_trace (__LINE__, reg, mem, scratch, store_p);

  new_addr = addr = XEXP (mem, 0);
  switch (GET_CODE (addr))
    {
      /* Does the register class support auto update forms for this mode?  If
	 not, do the update now.  We don't need a scratch register, since the
	 powerpc only supports PRE_INC, PRE_DEC, and PRE_MODIFY.  */
    case PRE_INC:
    case PRE_DEC:
      op_reg = XEXP (addr, 0);
      if (!base_reg_operand (op_reg, Pmode))
	rs6000_secondary_reload_fail (__LINE__, reg, mem, scratch, store_p);

      if ((addr_mask & RELOAD_REG_PRE_INCDEC) == 0)
	{
	  int delta = GET_MODE_SIZE (mode);
	  if (GET_CODE (addr) == PRE_DEC)
	    delta = -delta;
	  emit_insn (gen_add2_insn (op_reg, GEN_INT (delta)));
	  new_addr = op_reg;
	}
      break;

    case PRE_MODIFY:
      op0 = XEXP (addr, 0);
      op1 = XEXP (addr, 1);
      if (!base_reg_operand (op0, Pmode)
	  || GET_CODE (op1) != PLUS
	  || !rtx_equal_p (op0, XEXP (op1, 0)))
	rs6000_secondary_reload_fail (__LINE__, reg, mem, scratch, store_p);

      if ((addr_mask & RELOAD_REG_PRE_MODIFY) == 0)
	{
	  emit_insn (gen_rtx_SET (op0, op1));
	  new_addr = reg;
	}
      break;

      /* Do we need to simulate AND -16 to clear the bottom address bits used
	 in VMX load/stores?  */
    case AND:
      op0 = XEXP (addr, 0);
      op1 = XEXP (addr, 1);
      if ((addr_mask & RELOAD_REG_AND_M16) == 0)
	{
	  if (REG_P (op0) || SUBREG_P (op0))
	    op_reg = op0;

	  else if (GET_CODE (op1) == PLUS)
	    {
	      emit_insn (gen_rtx_SET (scratch, op1));
	      op_reg = scratch;
	    }

	  else
	    rs6000_secondary_reload_fail (__LINE__, reg, mem, scratch, store_p);

	  and_op = gen_rtx_AND (GET_MODE (scratch), op_reg, op1);
	  cc_clobber = gen_rtx_CLOBBER (VOIDmode, gen_rtx_SCRATCH (CCmode));
	  rv = gen_rtvec (2, gen_rtx_SET (scratch, and_op), cc_clobber);
	  emit_insn (gen_rtx_PARALLEL (VOIDmode, rv));
	  new_addr = scratch;
	}
      break;

      /* If this is an indirect address, make sure it is a base register.  */
    case REG:
    case SUBREG:
      if (!base_reg_operand (addr, GET_MODE (addr)))
	{
	  emit_insn (gen_rtx_SET (scratch, addr));
	  new_addr = scratch;
	}
      break;

      /* If this is an indexed address, make sure the register class can handle
	 indexed addresses for this mode.  */
    case PLUS:
      op0 = XEXP (addr, 0);
      op1 = XEXP (addr, 1);
      if (!base_reg_operand (op0, Pmode))
	rs6000_secondary_reload_fail (__LINE__, reg, mem, scratch, store_p);

      else if (int_reg_operand (op1, Pmode))
	{
	  if ((addr_mask & RELOAD_REG_INDEXED) == 0)
	    {
	      emit_insn (gen_rtx_SET (scratch, addr));
	      new_addr = scratch;
	    }
	}

      else if (mode_supports_dq_form (mode) && CONST_INT_P (op1))
	{
	  if (((addr_mask & RELOAD_REG_QUAD_OFFSET) == 0)
	      || !quad_address_p (addr, mode, false))
	    {
	      emit_insn (gen_rtx_SET (scratch, addr));
	      new_addr = scratch;
	    }
	}

      /* Make sure the register class can handle offset addresses.  */
      else if (rs6000_legitimate_offset_address_p (mode, addr, false, true))
	{
	  if ((addr_mask & RELOAD_REG_OFFSET) == 0)
	    {
	      emit_insn (gen_rtx_SET (scratch, addr));
	      new_addr = scratch;
	    }
	}

      else
	rs6000_secondary_reload_fail (__LINE__, reg, mem, scratch, store_p);

      break;

    case LO_SUM:
      op0 = XEXP (addr, 0);
      op1 = XEXP (addr, 1);
      if (!base_reg_operand (op0, Pmode))
	rs6000_secondary_reload_fail (__LINE__, reg, mem, scratch, store_p);

      else if (int_reg_operand (op1, Pmode))
	{
	  if ((addr_mask & RELOAD_REG_INDEXED) == 0)
	    {
	      emit_insn (gen_rtx_SET (scratch, addr));
	      new_addr = scratch;
	    }
	}

      /* Quad offsets are restricted and can't handle normal addresses.  */
      else if (mode_supports_dq_form (mode))
	{
	  emit_insn (gen_rtx_SET (scratch, addr));
	  new_addr = scratch;
	}

      /* Make sure the register class can handle offset addresses.  */
      else if (legitimate_lo_sum_address_p (mode, addr, false))
	{
	  if ((addr_mask & RELOAD_REG_OFFSET) == 0)
	    {
	      emit_insn (gen_rtx_SET (scratch, addr));
	      new_addr = scratch;
	    }
	}

      else
	rs6000_secondary_reload_fail (__LINE__, reg, mem, scratch, store_p);

      break;

    case SYMBOL_REF:
    case CONST:
    case LABEL_REF:
      rs6000_emit_move (scratch, addr, Pmode);
      new_addr = scratch;
      break;

    default:
      rs6000_secondary_reload_fail (__LINE__, reg, mem, scratch, store_p);
    }

  /* Adjust the address if it changed.  */
  if (addr != new_addr)
    {
      mem = replace_equiv_address_nv (mem, new_addr);
      if (TARGET_DEBUG_ADDR)
	fprintf (stderr, "\nrs6000_secondary_reload_inner, mem adjusted.\n");
    }

  /* Now create the move.  */
  if (store_p)
    emit_insn (gen_rtx_SET (mem, reg));
  else
    emit_insn (gen_rtx_SET (reg, mem));

  return;
}

/* Convert reloads involving 64-bit gprs and misaligned offset
   addressing, or multiple 32-bit gprs and offsets that are too large,
   to use indirect addressing.  */

void
rs6000_secondary_reload_gpr (rtx reg, rtx mem, rtx scratch, bool store_p)
{
  int regno = true_regnum (reg);
  enum reg_class rclass;
  rtx addr;
  rtx scratch_or_premodify = scratch;

  if (TARGET_DEBUG_ADDR)
    {
      fprintf (stderr, "\nrs6000_secondary_reload_gpr, type = %s\n",
	       store_p ? "store" : "load");
      fprintf (stderr, "reg:\n");
      debug_rtx (reg);
      fprintf (stderr, "mem:\n");
      debug_rtx (mem);
      fprintf (stderr, "scratch:\n");
      debug_rtx (scratch);
    }

  gcc_assert (regno >= 0 && HARD_REGISTER_NUM_P (regno));
  gcc_assert (MEM_P (mem));
  rclass = REGNO_REG_CLASS (regno);
  gcc_assert (rclass == GENERAL_REGS || rclass == BASE_REGS);
  addr = XEXP (mem, 0);

  if (GET_CODE (addr) == PRE_MODIFY)
    {
      gcc_assert (REG_P (XEXP (addr, 0))
		  && GET_CODE (XEXP (addr, 1)) == PLUS
		  && XEXP (XEXP (addr, 1), 0) == XEXP (addr, 0));
      scratch_or_premodify = XEXP (addr, 0);
      addr = XEXP (addr, 1);
    }
  gcc_assert (GET_CODE (addr) == PLUS || GET_CODE (addr) == LO_SUM);

  rs6000_emit_move (scratch_or_premodify, addr, Pmode);

  mem = replace_equiv_address_nv (mem, scratch_or_premodify);

  /* Now create the move.  */
  if (store_p)
    emit_insn (gen_rtx_SET (mem, reg));
  else
    emit_insn (gen_rtx_SET (reg, mem));

  return;
}

/* Given an rtx X being reloaded into a reg required to be
   in class CLASS, return the class of reg to actually use.
   In general this is just CLASS; but on some machines
   in some cases it is preferable to use a more restrictive class.

   On the RS/6000, we have to return NO_REGS when we want to reload a
   floating-point CONST_DOUBLE to force it to be copied to memory.

   We also don't want to reload integer values into floating-point
   registers if we can at all help it.  In fact, this can
   cause reload to die, if it tries to generate a reload of CTR
   into a FP register and discovers it doesn't have the memory location
   required.

   ??? Would it be a good idea to have reload do the converse, that is
   try to reload floating modes into FP registers if possible?
 */

static enum reg_class
rs6000_preferred_reload_class (rtx x, enum reg_class rclass)
{
  machine_mode mode = GET_MODE (x);
  bool is_constant = CONSTANT_P (x);

  /* If a mode can't go in FPR/ALTIVEC/VSX registers, don't return a preferred
     reload class for it.  */
  if ((rclass == ALTIVEC_REGS || rclass == VSX_REGS)
      && (reg_addr[mode].addr_mask[RELOAD_REG_VMX] & RELOAD_REG_VALID) == 0)
    return NO_REGS;

  if ((rclass == FLOAT_REGS || rclass == VSX_REGS)
      && (reg_addr[mode].addr_mask[RELOAD_REG_FPR] & RELOAD_REG_VALID) == 0)
    return NO_REGS;

  /* For VSX, see if we should prefer FLOAT_REGS or ALTIVEC_REGS.  Do not allow
     the reloading of address expressions using PLUS into floating point
     registers.  */
  if (TARGET_VSX && VSX_REG_CLASS_P (rclass) && GET_CODE (x) != PLUS)
    {
      if (is_constant)
	{
	  /* Zero is always allowed in all VSX registers.  */
	  if (x == CONST0_RTX (mode))
	    return rclass;

	  /* If this is a vector constant that can be formed with a few Altivec
	     instructions, we want altivec registers.  */
	  if (GET_CODE (x) == CONST_VECTOR && easy_vector_constant (x, mode))
	    return ALTIVEC_REGS;

	  /* If this is an integer constant that can easily be loaded into
	     vector registers, allow it.  */
	  if (CONST_INT_P (x))
	    {
	      HOST_WIDE_INT value = INTVAL (x);

	      /* ISA 2.07 can generate -1 in all registers with XXLORC.  ISA
		 2.06 can generate it in the Altivec registers with
		 VSPLTI<x>.  */
	      if (value == -1)
		{
		  if (TARGET_P8_VECTOR)
		    return rclass;
		  else if (rclass == ALTIVEC_REGS || rclass == VSX_REGS)
		    return ALTIVEC_REGS;
		  else
		    return NO_REGS;
		}

	      /* ISA 3.0 can load -128..127 using the XXSPLTIB instruction and
		 a sign extend in the Altivec registers.  */
	      if (IN_RANGE (value, -128, 127) && TARGET_P9_VECTOR
		  && (rclass == ALTIVEC_REGS || rclass == VSX_REGS))
		return ALTIVEC_REGS;
	    }

	  /* Force constant to memory.  */
	  return NO_REGS;
	}

      /* D-form addressing can easily reload the value.  */
      if (mode_supports_vmx_dform (mode)
	  || mode_supports_dq_form (mode))
	return rclass;

      /* If this is a scalar floating point value and we don't have D-form
	 addressing, prefer the traditional floating point registers so that we
	 can use D-form (register+offset) addressing.  */
      if (rclass == VSX_REGS
	  && (mode == SFmode || GET_MODE_SIZE (mode) == 8))
	return FLOAT_REGS;

      /* Prefer the Altivec registers if Altivec is handling the vector
	 operations (i.e. V16QI, V8HI, and V4SI), or if we prefer Altivec
	 loads.  */
      if (VECTOR_UNIT_ALTIVEC_P (mode) || VECTOR_MEM_ALTIVEC_P (mode)
	  || mode == V1TImode)
	return ALTIVEC_REGS;

      return rclass;
    }

  if (is_constant || GET_CODE (x) == PLUS)
    {
      if (reg_class_subset_p (GENERAL_REGS, rclass))
	return GENERAL_REGS;
      if (reg_class_subset_p (BASE_REGS, rclass))
	return BASE_REGS;
      return NO_REGS;
    }

  /* For the vector pair and vector quad modes, prefer their natural register
     (VSX or FPR) rather than GPR registers.  For other integer types, prefer
     the GPR registers.  */
  if (rclass == GEN_OR_FLOAT_REGS)
    {
      if (mode == POImode)
	return VSX_REGS;

      if (mode == PXImode)
	return FLOAT_REGS;

      if (GET_MODE_CLASS (mode) == MODE_INT)
	return GENERAL_REGS;
    }

  return rclass;
}

/* Debug version of rs6000_preferred_reload_class.  */
static enum reg_class
rs6000_debug_preferred_reload_class (rtx x, enum reg_class rclass)
{
  enum reg_class ret = rs6000_preferred_reload_class (x, rclass);

  fprintf (stderr,
	   "\nrs6000_preferred_reload_class, return %s, rclass = %s, "
	   "mode = %s, x:\n",
	   reg_class_names[ret], reg_class_names[rclass],
	   GET_MODE_NAME (GET_MODE (x)));
  debug_rtx (x);

  return ret;
}

/* If we are copying between FP or AltiVec registers and anything else, we need
   a memory location.  The exception is when we are targeting ppc64 and the
   move to/from fpr to gpr instructions are available.  Also, under VSX, you
   can copy vector registers from the FP register set to the Altivec register
   set and vice versa.  */

static bool
rs6000_secondary_memory_needed (machine_mode mode,
				reg_class_t from_class,
				reg_class_t to_class)
{
  enum rs6000_reg_type from_type, to_type;
  bool altivec_p = ((from_class == ALTIVEC_REGS)
		    || (to_class == ALTIVEC_REGS));

  /* If a simple/direct move is available, we don't need secondary memory  */
  from_type = reg_class_to_reg_type[(int)from_class];
  to_type = reg_class_to_reg_type[(int)to_class];

  if (rs6000_secondary_reload_move (to_type, from_type, mode,
				    (secondary_reload_info *)0, altivec_p))
    return false;

  /* If we have a floating point or vector register class, we need to use
     memory to transfer the data.  */
  if (IS_FP_VECT_REG_TYPE (from_type) || IS_FP_VECT_REG_TYPE (to_type))
    return true;

  return false;
}

/* Debug version of rs6000_secondary_memory_needed.  */
static bool
rs6000_debug_secondary_memory_needed (machine_mode mode,
				      reg_class_t from_class,
				      reg_class_t to_class)
{
  bool ret = rs6000_secondary_memory_needed (mode, from_class, to_class);

  fprintf (stderr,
	   "rs6000_secondary_memory_needed, return: %s, from_class = %s, "
	   "to_class = %s, mode = %s\n",
	   ret ? "true" : "false",
	   reg_class_names[from_class],
	   reg_class_names[to_class],
	   GET_MODE_NAME (mode));

  return ret;
}

/* Return the register class of a scratch register needed to copy IN into
   or out of a register in RCLASS in MODE.  If it can be done directly,
   NO_REGS is returned.  */

static enum reg_class
rs6000_secondary_reload_class (enum reg_class rclass, machine_mode mode,
			       rtx in)
{
  int regno;

  if (TARGET_ELF || (DEFAULT_ABI == ABI_DARWIN
#if TARGET_MACHO
		     && MACHOPIC_INDIRECT
#endif
		     ))
    {
      /* We cannot copy a symbolic operand directly into anything
	 other than BASE_REGS for TARGET_ELF.  So indicate that a
	 register from BASE_REGS is needed as an intermediate
	 register.

	 On Darwin, pic addresses require a load from memory, which
	 needs a base register.  */
      if (rclass != BASE_REGS
	  && (SYMBOL_REF_P (in)
	      || GET_CODE (in) == HIGH
	      || GET_CODE (in) == LABEL_REF
	      || GET_CODE (in) == CONST))
	return BASE_REGS;
    }

  if (REG_P (in))
    {
      regno = REGNO (in);
      if (!HARD_REGISTER_NUM_P (regno))
	{
	  regno = true_regnum (in);
	  if (!HARD_REGISTER_NUM_P (regno))
	    regno = -1;
	}
    }
  else if (SUBREG_P (in))
    {
      regno = true_regnum (in);
      if (!HARD_REGISTER_NUM_P (regno))
	regno = -1;
    }
  else
    regno = -1;

  /* If we have VSX register moves, prefer moving scalar values between
     Altivec registers and GPR by going via an FPR (and then via memory)
     instead of reloading the secondary memory address for Altivec moves.  */
  if (TARGET_VSX
      && GET_MODE_SIZE (mode) < 16
      && !mode_supports_vmx_dform (mode)
      && (((rclass == GENERAL_REGS || rclass == BASE_REGS)
           && (regno >= 0 && ALTIVEC_REGNO_P (regno)))
          || ((rclass == VSX_REGS || rclass == ALTIVEC_REGS)
              && (regno >= 0 && INT_REGNO_P (regno)))))
    return FLOAT_REGS;

  /* We can place anything into GENERAL_REGS and can put GENERAL_REGS
     into anything.  */
  if (rclass == GENERAL_REGS || rclass == BASE_REGS
      || (regno >= 0 && INT_REGNO_P (regno)))
    return NO_REGS;

  /* Constants, memory, and VSX registers can go into VSX registers (both the
     traditional floating point and the altivec registers).  */
  if (rclass == VSX_REGS
      && (regno == -1 || VSX_REGNO_P (regno)))
    return NO_REGS;

  /* Constants, memory, and FP registers can go into FP registers.  */
  if ((regno == -1 || FP_REGNO_P (regno))
      && (rclass == FLOAT_REGS || rclass == GEN_OR_FLOAT_REGS))
    return (mode != SDmode || lra_in_progress) ? NO_REGS : GENERAL_REGS;

  /* Memory, and AltiVec registers can go into AltiVec registers.  */
  if ((regno == -1 || ALTIVEC_REGNO_P (regno))
      && rclass == ALTIVEC_REGS)
    return NO_REGS;

  /* We can copy among the CR registers.  */
  if ((rclass == CR_REGS || rclass == CR0_REGS)
      && regno >= 0 && CR_REGNO_P (regno))
    return NO_REGS;

  /* Otherwise, we need GENERAL_REGS.  */
  return GENERAL_REGS;
}

/* Debug version of rs6000_secondary_reload_class.  */
static enum reg_class
rs6000_debug_secondary_reload_class (enum reg_class rclass,
				     machine_mode mode, rtx in)
{
  enum reg_class ret = rs6000_secondary_reload_class (rclass, mode, in);
  fprintf (stderr,
	   "\nrs6000_secondary_reload_class, return %s, rclass = %s, "
	   "mode = %s, input rtx:\n",
	   reg_class_names[ret], reg_class_names[rclass],
	   GET_MODE_NAME (mode));
  debug_rtx (in);

  return ret;
}

/* Implement TARGET_CAN_CHANGE_MODE_CLASS.  */

static bool
rs6000_can_change_mode_class (machine_mode from,
			      machine_mode to,
			      reg_class_t rclass)
{
  unsigned from_size = GET_MODE_SIZE (from);
  unsigned to_size = GET_MODE_SIZE (to);

  if (from_size != to_size)
    {
      enum reg_class xclass = (TARGET_VSX) ? VSX_REGS : FLOAT_REGS;

      if (reg_classes_intersect_p (xclass, rclass))
	{
	  unsigned to_nregs = hard_regno_nregs (FIRST_FPR_REGNO, to);
	  unsigned from_nregs = hard_regno_nregs (FIRST_FPR_REGNO, from);
	  bool to_float128_vector_p = FLOAT128_VECTOR_P (to);
	  bool from_float128_vector_p = FLOAT128_VECTOR_P (from);

	  /* Don't allow 64-bit types to overlap with 128-bit types that take a
	     single register under VSX because the scalar part of the register
	     is in the upper 64-bits, and not the lower 64-bits.  Types like
	     TFmode/TDmode that take 2 scalar register can overlap.  128-bit
	     IEEE floating point can't overlap, and neither can small
	     values.  */

	  if (to_float128_vector_p && from_float128_vector_p)
	    return true;

	  else if (to_float128_vector_p || from_float128_vector_p)
	    return false;

	  /* TDmode in floating-mode registers must always go into a register
	     pair with the most significant word in the even-numbered register
	     to match ISA requirements.  In little-endian mode, this does not
	     match subreg numbering, so we cannot allow subregs.  */
	  if (!BYTES_BIG_ENDIAN && (to == TDmode || from == TDmode))
	    return false;

	  /* Allow SD<->DD changes, since SDmode values are stored in
	     the low half of the DDmode, just like target-independent
	     code expects.  We need to allow at least SD->DD since
	     rs6000_secondary_memory_needed_mode asks for that change
	     to be made for SD reloads.  */
	  if ((to == DDmode && from == SDmode)
	      || (to == SDmode && from == DDmode))
	    return true;

	  if (from_size < 8 || to_size < 8)
	    return false;

	  if (from_size == 8 && (8 * to_nregs) != to_size)
	    return false;

	  if (to_size == 8 && (8 * from_nregs) != from_size)
	    return false;

	  return true;
	}
      else
	return true;
    }

  /* Since the VSX register set includes traditional floating point registers
     and altivec registers, just check for the size being different instead of
     trying to check whether the modes are vector modes.  Otherwise it won't
     allow say DF and DI to change classes.  For types like TFmode and TDmode
     that take 2 64-bit registers, rather than a single 128-bit register, don't
     allow subregs of those types to other 128 bit types.  */
  if (TARGET_VSX && VSX_REG_CLASS_P (rclass))
    {
      unsigned num_regs = (from_size + 15) / 16;
      if (hard_regno_nregs (FIRST_FPR_REGNO, to) > num_regs
	  || hard_regno_nregs (FIRST_FPR_REGNO, from) > num_regs)
	return false;

      return (from_size == 8 || from_size == 16);
    }

  if (TARGET_ALTIVEC && rclass == ALTIVEC_REGS
      && (ALTIVEC_VECTOR_MODE (from) + ALTIVEC_VECTOR_MODE (to)) == 1)
    return false;

  return true;
}

/* Debug version of rs6000_can_change_mode_class.  */
static bool
rs6000_debug_can_change_mode_class (machine_mode from,
				    machine_mode to,
				    reg_class_t rclass)
{
  bool ret = rs6000_can_change_mode_class (from, to, rclass);

  fprintf (stderr,
	   "rs6000_can_change_mode_class, return %s, from = %s, "
	   "to = %s, rclass = %s\n",
	   ret ? "true" : "false",
	   GET_MODE_NAME (from), GET_MODE_NAME (to),
	   reg_class_names[rclass]);

  return ret;
}

/* Return a string to do a move operation of 128 bits of data.  */

const char *
rs6000_output_move_128bit (rtx operands[])
{
  rtx dest = operands[0];
  rtx src = operands[1];
  machine_mode mode = GET_MODE (dest);
  int dest_regno;
  int src_regno;
  bool dest_gpr_p, dest_fp_p, dest_vmx_p, dest_vsx_p;
  bool src_gpr_p, src_fp_p, src_vmx_p, src_vsx_p;

  if (REG_P (dest))
    {
      dest_regno = REGNO (dest);
      dest_gpr_p = INT_REGNO_P (dest_regno);
      dest_fp_p = FP_REGNO_P (dest_regno);
      dest_vmx_p = ALTIVEC_REGNO_P (dest_regno);
      dest_vsx_p = dest_fp_p | dest_vmx_p;
    }
  else
    {
      dest_regno = -1;
      dest_gpr_p = dest_fp_p = dest_vmx_p = dest_vsx_p = false;
    }

  if (REG_P (src))
    {
      src_regno = REGNO (src);
      src_gpr_p = INT_REGNO_P (src_regno);
      src_fp_p = FP_REGNO_P (src_regno);
      src_vmx_p = ALTIVEC_REGNO_P (src_regno);
      src_vsx_p = src_fp_p | src_vmx_p;
    }
  else
    {
      src_regno = -1;
      src_gpr_p = src_fp_p = src_vmx_p = src_vsx_p = false;
    }

  /* Register moves.  */
  if (dest_regno >= 0 && src_regno >= 0)
    {
      if (dest_gpr_p)
	{
	  if (src_gpr_p)
	    return "#";

	  if (TARGET_DIRECT_MOVE_128 && src_vsx_p)
	    return (WORDS_BIG_ENDIAN
		    ? "mfvsrd %0,%x1\n\tmfvsrld %L0,%x1"
		    : "mfvsrd %L0,%x1\n\tmfvsrld %0,%x1");

	  else if (TARGET_VSX && TARGET_DIRECT_MOVE && src_vsx_p)
	    return "#";
	}

      else if (TARGET_VSX && dest_vsx_p)
	{
	  if (src_vsx_p)
	    return "xxlor %x0,%x1,%x1";

	  else if (TARGET_DIRECT_MOVE_128 && src_gpr_p)
	    return (WORDS_BIG_ENDIAN
		    ? "mtvsrdd %x0,%1,%L1"
		    : "mtvsrdd %x0,%L1,%1");

	  else if (TARGET_DIRECT_MOVE && src_gpr_p)
	    return "#";
	}

      else if (TARGET_ALTIVEC && dest_vmx_p && src_vmx_p)
	return "vor %0,%1,%1";

      else if (dest_fp_p && src_fp_p)
	return "#";
    }

  /* Loads.  */
  else if (dest_regno >= 0 && MEM_P (src))
    {
      if (dest_gpr_p)
	{
	  if (TARGET_QUAD_MEMORY && quad_load_store_p (dest, src))
	    return "lq %0,%1";
	  else
	    return "#";
	}

      else if (TARGET_ALTIVEC && dest_vmx_p
	       && altivec_indexed_or_indirect_operand (src, mode))
	return "lvx %0,%y1";

      else if (TARGET_VSX && dest_vsx_p)
	{
	  if (mode_supports_dq_form (mode)
	      && quad_address_p (XEXP (src, 0), mode, true))
	    return "lxv %x0,%1";

	  else if (TARGET_P9_VECTOR)
	    return "lxvx %x0,%y1";

	  else if (mode == V16QImode || mode == V8HImode || mode == V4SImode)
	    return "lxvw4x %x0,%y1";

	  else
	    return "lxvd2x %x0,%y1";
	}

      else if (TARGET_ALTIVEC && dest_vmx_p)
	return "lvx %0,%y1";

      else if (dest_fp_p)
	return "#";
    }

  /* Stores.  */
  else if (src_regno >= 0 && MEM_P (dest))
    {
      if (src_gpr_p)
	{
 	  if (TARGET_QUAD_MEMORY && quad_load_store_p (dest, src))
	    return "stq %1,%0";
	  else
	    return "#";
	}

      else if (TARGET_ALTIVEC && src_vmx_p
	       && altivec_indexed_or_indirect_operand (dest, mode))
	return "stvx %1,%y0";

      else if (TARGET_VSX && src_vsx_p)
	{
	  if (mode_supports_dq_form (mode)
	      && quad_address_p (XEXP (dest, 0), mode, true))
	    return "stxv %x1,%0";

	  else if (TARGET_P9_VECTOR)
	    return "stxvx %x1,%y0";

	  else if (mode == V16QImode || mode == V8HImode || mode == V4SImode)
	    return "stxvw4x %x1,%y0";

	  else
	    return "stxvd2x %x1,%y0";
	}

      else if (TARGET_ALTIVEC && src_vmx_p)
	return "stvx %1,%y0";

      else if (src_fp_p)
	return "#";
    }

  /* Constants.  */
  else if (dest_regno >= 0
	   && (CONST_INT_P (src)
	       || CONST_WIDE_INT_P (src)
	       || CONST_DOUBLE_P (src)
	       || GET_CODE (src) == CONST_VECTOR))
    {
      if (dest_gpr_p)
	return "#";

      else if ((dest_vmx_p && TARGET_ALTIVEC)
	       || (dest_vsx_p && TARGET_VSX))
	return output_vec_const_move (operands);
    }

  fatal_insn ("Bad 128-bit move", gen_rtx_SET (dest, src));
}

/* Validate a 128-bit move.  */
bool
rs6000_move_128bit_ok_p (rtx operands[])
{
  machine_mode mode = GET_MODE (operands[0]);
  return (gpc_reg_operand (operands[0], mode)
	  || gpc_reg_operand (operands[1], mode));
}

/* Return true if a 128-bit move needs to be split.  */
bool
rs6000_split_128bit_ok_p (rtx operands[])
{
  if (!reload_completed)
    return false;

  if (!gpr_or_gpr_p (operands[0], operands[1]))
    return false;

  if (quad_load_store_p (operands[0], operands[1]))
    return false;

  return true;
}


/* Given a comparison operation, return the bit number in CCR to test.  We
   know this is a valid comparison.

   SCC_P is 1 if this is for an scc.  That means that %D will have been
   used instead of %C, so the bits will be in different places.

   Return -1 if OP isn't a valid comparison for some reason.  */

int
ccr_bit (rtx op, int scc_p)
{
  enum rtx_code code = GET_CODE (op);
  machine_mode cc_mode;
  int cc_regnum;
  int base_bit;
  rtx reg;

  if (!COMPARISON_P (op))
    return -1;

  reg = XEXP (op, 0);

  if (!REG_P (reg) || !CR_REGNO_P (REGNO (reg)))
    return -1;

  cc_mode = GET_MODE (reg);
  cc_regnum = REGNO (reg);
  base_bit = 4 * (cc_regnum - CR0_REGNO);

  validate_condition_mode (code, cc_mode);

  /* When generating a sCOND operation, only positive conditions are
     allowed.  */
  if (scc_p)
    switch (code)
      {
      case EQ:
      case GT:
      case LT:
      case UNORDERED:
      case GTU:
      case LTU:
	break;
      default:
	return -1;
      }

  switch (code)
    {
    case NE:
      return scc_p ? base_bit + 3 : base_bit + 2;
    case EQ:
      return base_bit + 2;
    case GT:  case GTU:  case UNLE:
      return base_bit + 1;
    case LT:  case LTU:  case UNGE:
      return base_bit;
    case ORDERED:  case UNORDERED:
      return base_bit + 3;

    case GE:  case GEU:
      /* If scc, we will have done a cror to put the bit in the
	 unordered position.  So test that bit.  For integer, this is ! LT
	 unless this is an scc insn.  */
      return scc_p ? base_bit + 3 : base_bit;

    case LE:  case LEU:
      return scc_p ? base_bit + 3 : base_bit + 1;

    default:
      return -1;
    }
}

/* Return the GOT register.  */

rtx
rs6000_got_register (rtx value ATTRIBUTE_UNUSED)
{
  /* The second flow pass currently (June 1999) can't update
     regs_ever_live without disturbing other parts of the compiler, so
     update it here to make the prolog/epilogue code happy.  */
  if (!can_create_pseudo_p ()
      && !df_regs_ever_live_p (RS6000_PIC_OFFSET_TABLE_REGNUM))
    df_set_regs_ever_live (RS6000_PIC_OFFSET_TABLE_REGNUM, true);

  crtl->uses_pic_offset_table = 1;

  return pic_offset_table_rtx;
}

#define INT_P(X) (CONST_INT_P (X) && GET_MODE (X) == VOIDmode)

/* Write out a function code label.  */

void
rs6000_output_function_entry (FILE *file, const char *fname)
{
  if (fname[0] != '.')
    {
      switch (DEFAULT_ABI)
	{
	default:
	  gcc_unreachable ();

	case ABI_AIX:
	  if (DOT_SYMBOLS)
	    putc ('.', file);
	  else
	    ASM_OUTPUT_INTERNAL_LABEL_PREFIX (file, "L.");
	  break;

	case ABI_ELFv2:
	case ABI_V4:
	case ABI_DARWIN:
	  break;
	}
    }

  RS6000_OUTPUT_BASENAME (file, fname);
}

/* Print an operand.  Recognize special options, documented below.  */

#if TARGET_ELF
/* Access to .sdata2 through r2 (see -msdata=eabi in invoke.texi) is
   only introduced by the linker, when applying the sda21
   relocation.  */
#define SMALL_DATA_RELOC ((rs6000_sdata == SDATA_EABI) ? "sda21" : "sdarel")
#define SMALL_DATA_REG ((rs6000_sdata == SDATA_EABI) ? 0 : 13)
#else
#define SMALL_DATA_RELOC "sda21"
#define SMALL_DATA_REG 0
#endif

void
print_operand (FILE *file, rtx x, int code)
{
  int i;
  unsigned HOST_WIDE_INT uval;

  switch (code)
    {
      /* %a is output_address.  */

      /* %c is output_addr_const if a CONSTANT_ADDRESS_P, otherwise
	 output_operand.  */

    case 'A':
      /* Write the MMA accumulator number associated with VSX register X.  */
      if (!REG_P (x) || !FP_REGNO_P (REGNO (x)) || (REGNO (x) % 4) != 0)
	output_operand_lossage ("invalid %%A value");
      else
	fprintf (file, "%d", (REGNO (x) - FIRST_FPR_REGNO) / 4);
      return;

    case 'D':
      /* Like 'J' but get to the GT bit only.  */
      if (!REG_P (x) || !CR_REGNO_P (REGNO (x)))
	{
	  output_operand_lossage ("invalid %%D value");
	  return;
	}

      /* Bit 1 is GT bit.  */
      i = 4 * (REGNO (x) - CR0_REGNO) + 1;

      /* Add one for shift count in rlinm for scc.  */
      fprintf (file, "%d", i + 1);
      return;

    case 'e':
      /* If the low 16 bits are 0, but some other bit is set, write 's'.  */
      if (! INT_P (x))
	{
	  output_operand_lossage ("invalid %%e value");
	  return;
	}

      uval = INTVAL (x);
      if ((uval & 0xffff) == 0 && uval != 0)
	putc ('s', file);
      return;

    case 'E':
      /* X is a CR register.  Print the number of the EQ bit of the CR */
      if (!REG_P (x) || !CR_REGNO_P (REGNO (x)))
	output_operand_lossage ("invalid %%E value");
      else
	fprintf (file, "%d", 4 * (REGNO (x) - CR0_REGNO) + 2);
      return;

    case 'f':
      /* X is a CR register.  Print the shift count needed to move it
	 to the high-order four bits.  */
      if (!REG_P (x) || !CR_REGNO_P (REGNO (x)))
	output_operand_lossage ("invalid %%f value");
      else
	fprintf (file, "%d", 4 * (REGNO (x) - CR0_REGNO));
      return;

    case 'F':
      /* Similar, but print the count for the rotate in the opposite
	 direction.  */
      if (!REG_P (x) || !CR_REGNO_P (REGNO (x)))
	output_operand_lossage ("invalid %%F value");
      else
	fprintf (file, "%d", 32 - 4 * (REGNO (x) - CR0_REGNO));
      return;

    case 'G':
      /* X is a constant integer.  If it is negative, print "m",
	 otherwise print "z".  This is to make an aze or ame insn.  */
      if (!CONST_INT_P (x))
	output_operand_lossage ("invalid %%G value");
      else if (INTVAL (x) >= 0)
	putc ('z', file);
      else
	putc ('m', file);
      return;

    case 'h':
      /* If constant, output low-order five bits.  Otherwise, write
	 normally.  */
      if (INT_P (x))
	fprintf (file, HOST_WIDE_INT_PRINT_DEC, INTVAL (x) & 31);
      else
	print_operand (file, x, 0);
      return;

    case 'H':
      /* If constant, output low-order six bits.  Otherwise, write
	 normally.  */
      if (INT_P (x))
	fprintf (file, HOST_WIDE_INT_PRINT_DEC, INTVAL (x) & 63);
      else
	print_operand (file, x, 0);
      return;

    case 'I':
      /* Print `i' if this is a constant, else nothing.  */
      if (INT_P (x))
	putc ('i', file);
      return;

    case 'j':
      /* Write the bit number in CCR for jump.  */
      i = ccr_bit (x, 0);
      if (i == -1)
	output_operand_lossage ("invalid %%j code");
      else
	fprintf (file, "%d", i);
      return;

    case 'J':
      /* Similar, but add one for shift count in rlinm for scc and pass
	 scc flag to `ccr_bit'.  */
      i = ccr_bit (x, 1);
      if (i == -1)
	output_operand_lossage ("invalid %%J code");
      else
	/* If we want bit 31, write a shift count of zero, not 32.  */
	fprintf (file, "%d", i == 31 ? 0 : i + 1);
      return;

    case 'k':
      /* X must be a constant.  Write the 1's complement of the
	 constant.  */
      if (! INT_P (x))
	output_operand_lossage ("invalid %%k value");
      else
	fprintf (file, HOST_WIDE_INT_PRINT_DEC, ~ INTVAL (x));
      return;

    case 'K':
      /* X must be a symbolic constant on ELF.  Write an
	 expression suitable for an 'addi' that adds in the low 16
	 bits of the MEM.  */
      if (GET_CODE (x) == CONST)
	{
	  if (GET_CODE (XEXP (x, 0)) != PLUS
	      || (!SYMBOL_REF_P (XEXP (XEXP (x, 0), 0))
		  && GET_CODE (XEXP (XEXP (x, 0), 0)) != LABEL_REF)
	      || !CONST_INT_P (XEXP (XEXP (x, 0), 1)))
	    output_operand_lossage ("invalid %%K value");
	}
      print_operand_address (file, x);
      fputs ("@l", file);
      return;

      /* %l is output_asm_label.  */

    case 'L':
      /* Write second word of DImode or DFmode reference.  Works on register
	 or non-indexed memory only.  */
      if (REG_P (x))
	fputs (reg_names[REGNO (x) + 1], file);
      else if (MEM_P (x))
	{
	  machine_mode mode = GET_MODE (x);
	  /* Handle possible auto-increment.  Since it is pre-increment and
	     we have already done it, we can just use an offset of word.  */
	  if (GET_CODE (XEXP (x, 0)) == PRE_INC
	      || GET_CODE (XEXP (x, 0)) == PRE_DEC)
	    output_address (mode, plus_constant (Pmode, XEXP (XEXP (x, 0), 0),
						 UNITS_PER_WORD));
	  else if (GET_CODE (XEXP (x, 0)) == PRE_MODIFY)
	    output_address (mode, plus_constant (Pmode, XEXP (XEXP (x, 0), 0),
						 UNITS_PER_WORD));
	  else
	    output_address (mode, XEXP (adjust_address_nv (x, SImode,
							   UNITS_PER_WORD),
				  0));

	  if (small_data_operand (x, GET_MODE (x)))
	    fprintf (file, "@%s(%s)", SMALL_DATA_RELOC,
		     reg_names[SMALL_DATA_REG]);
	}
      return;

    case 'N': /* Unused */
      /* Write the number of elements in the vector times 4.  */
      if (GET_CODE (x) != PARALLEL)
	output_operand_lossage ("invalid %%N value");
      else
	fprintf (file, "%d", XVECLEN (x, 0) * 4);
      return;

    case 'O': /* Unused */
      /* Similar, but subtract 1 first.  */
      if (GET_CODE (x) != PARALLEL)
	output_operand_lossage ("invalid %%O value");
      else
	fprintf (file, "%d", (XVECLEN (x, 0) - 1) * 4);
      return;

    case 'p':
      /* X is a CONST_INT that is a power of two.  Output the logarithm.  */
      if (! INT_P (x)
	  || INTVAL (x) < 0
	  || (i = exact_log2 (INTVAL (x))) < 0)
	output_operand_lossage ("invalid %%p value");
      else
	fprintf (file, "%d", i);
      return;

    case 'P':
      /* The operand must be an indirect memory reference.  The result
	 is the register name.  */
      if (!MEM_P (x) || !REG_P (XEXP (x, 0))
	  || REGNO (XEXP (x, 0)) >= 32)
	output_operand_lossage ("invalid %%P value");
      else
	fputs (reg_names[REGNO (XEXP (x, 0))], file);
      return;

    case 'q':
      /* This outputs the logical code corresponding to a boolean
	 expression.  The expression may have one or both operands
	 negated (if one, only the first one).  For condition register
	 logical operations, it will also treat the negated
	 CR codes as NOTs, but not handle NOTs of them.  */
      {
	const char *const *t = 0;
	const char *s;
	enum rtx_code code = GET_CODE (x);
	static const char * const tbl[3][3] = {
	  { "and", "andc", "nor" },
	  { "or", "orc", "nand" },
	  { "xor", "eqv", "xor" } };

	if (code == AND)
	  t = tbl[0];
	else if (code == IOR)
	  t = tbl[1];
	else if (code == XOR)
	  t = tbl[2];
	else
	  output_operand_lossage ("invalid %%q value");

	if (GET_CODE (XEXP (x, 0)) != NOT)
	  s = t[0];
	else
	  {
	    if (GET_CODE (XEXP (x, 1)) == NOT)
	      s = t[2];
	    else
	      s = t[1];
	  }

	fputs (s, file);
      }
      return;

    case 'Q':
      if (! TARGET_MFCRF)
	return;
      fputc (',', file);
      /* FALLTHRU */

    case 'R':
      /* X is a CR register.  Print the mask for `mtcrf'.  */
      if (!REG_P (x) || !CR_REGNO_P (REGNO (x)))
	output_operand_lossage ("invalid %%R value");
      else
	fprintf (file, "%d", 128 >> (REGNO (x) - CR0_REGNO));
      return;

    case 's':
      /* Low 5 bits of 32 - value */
      if (! INT_P (x))
	output_operand_lossage ("invalid %%s value");
      else
	fprintf (file, HOST_WIDE_INT_PRINT_DEC, (32 - INTVAL (x)) & 31);
      return;

    case 't':
      /* Like 'J' but get to the OVERFLOW/UNORDERED bit.  */
      if (!REG_P (x) || !CR_REGNO_P (REGNO (x)))
	{
	  output_operand_lossage ("invalid %%t value");
	  return;
	}

      /* Bit 3 is OV bit.  */
      i = 4 * (REGNO (x) - CR0_REGNO) + 3;

      /* If we want bit 31, write a shift count of zero, not 32.  */
      fprintf (file, "%d", i == 31 ? 0 : i + 1);
      return;

    case 'T':
      /* Print the symbolic name of a branch target register.  */
      if (GET_CODE (x) == UNSPEC && XINT (x, 1) == UNSPEC_PLTSEQ)
	x = XVECEXP (x, 0, 0);
      if (!REG_P (x) || (REGNO (x) != LR_REGNO
			 && REGNO (x) != CTR_REGNO))
	output_operand_lossage ("invalid %%T value");
      else if (REGNO (x) == LR_REGNO)
	fputs ("lr", file);
      else
	fputs ("ctr", file);
      return;

    case 'u':
      /* High-order or low-order 16 bits of constant, whichever is non-zero,
	 for use in unsigned operand.  */
      if (! INT_P (x))
	{
	  output_operand_lossage ("invalid %%u value");
	  return;
	}

      uval = INTVAL (x);
      if ((uval & 0xffff) == 0)
	uval >>= 16;

      fprintf (file, HOST_WIDE_INT_PRINT_HEX, uval & 0xffff);
      return;

    case 'v':
      /* High-order 16 bits of constant for use in signed operand.  */
      if (! INT_P (x))
	output_operand_lossage ("invalid %%v value");
      else
	fprintf (file, HOST_WIDE_INT_PRINT_HEX,
		 (INTVAL (x) >> 16) & 0xffff);
      return;

    case 'U':
      /* Print `u' if this has an auto-increment or auto-decrement.  */
      if (MEM_P (x)
	  && (GET_CODE (XEXP (x, 0)) == PRE_INC
	      || GET_CODE (XEXP (x, 0)) == PRE_DEC
	      || GET_CODE (XEXP (x, 0)) == PRE_MODIFY))
	putc ('u', file);
      return;

    case 'V':
      /* Print the trap code for this operand.  */
      switch (GET_CODE (x))
	{
	case EQ:
	  fputs ("eq", file);   /* 4 */
	  break;
	case NE:
	  fputs ("ne", file);   /* 24 */
	  break;
	case LT:
	  fputs ("lt", file);   /* 16 */
	  break;
	case LE:
	  fputs ("le", file);   /* 20 */
	  break;
	case GT:
	  fputs ("gt", file);   /* 8 */
	  break;
	case GE:
	  fputs ("ge", file);   /* 12 */
	  break;
	case LTU:
	  fputs ("llt", file);  /* 2 */
	  break;
	case LEU:
	  fputs ("lle", file);  /* 6 */
	  break;
	case GTU:
	  fputs ("lgt", file);  /* 1 */
	  break;
	case GEU:
	  fputs ("lge", file);  /* 5 */
	  break;
	default:
	  output_operand_lossage ("invalid %%V value");
	}
      break;

    case 'w':
      /* If constant, low-order 16 bits of constant, signed.  Otherwise, write
	 normally.  */
      if (INT_P (x))
	fprintf (file, HOST_WIDE_INT_PRINT_DEC,
		 ((INTVAL (x) & 0xffff) ^ 0x8000) - 0x8000);
      else
	print_operand (file, x, 0);
      return;

    case 'x':
      /* X is a FPR or Altivec register used in a VSX context.  */
      if (!REG_P (x) || !VSX_REGNO_P (REGNO (x)))
	output_operand_lossage ("invalid %%x value");
      else
	{
	  int reg = REGNO (x);
	  int vsx_reg = (FP_REGNO_P (reg)
			 ? reg - 32
			 : reg - FIRST_ALTIVEC_REGNO + 32);

#ifdef TARGET_REGNAMES      
	  if (TARGET_REGNAMES)
	    fprintf (file, "%%vs%d", vsx_reg);
	  else
#endif
	    fprintf (file, "%d", vsx_reg);
	}
      return;

    case 'X':
      if (MEM_P (x)
	  && (legitimate_indexed_address_p (XEXP (x, 0), 0)
	      || (GET_CODE (XEXP (x, 0)) == PRE_MODIFY
		  && legitimate_indexed_address_p (XEXP (XEXP (x, 0), 1), 0))))
	putc ('x', file);
      return;

    case 'Y':
      /* Like 'L', for third word of TImode/PTImode  */
      if (REG_P (x))
	fputs (reg_names[REGNO (x) + 2], file);
      else if (MEM_P (x))
	{
	  machine_mode mode = GET_MODE (x);
	  if (GET_CODE (XEXP (x, 0)) == PRE_INC
	      || GET_CODE (XEXP (x, 0)) == PRE_DEC)
	    output_address (mode, plus_constant (Pmode,
						 XEXP (XEXP (x, 0), 0), 8));
	  else if (GET_CODE (XEXP (x, 0)) == PRE_MODIFY)
	    output_address (mode, plus_constant (Pmode,
						 XEXP (XEXP (x, 0), 0), 8));
	  else
	    output_address (mode, XEXP (adjust_address_nv (x, SImode, 8), 0));
	  if (small_data_operand (x, GET_MODE (x)))
	    fprintf (file, "@%s(%s)", SMALL_DATA_RELOC,
		     reg_names[SMALL_DATA_REG]);
	}
      return;

    case 'z':
      if (GET_CODE (x) == UNSPEC && XINT (x, 1) == UNSPEC_PLTSEQ)
	x = XVECEXP (x, 0, 1);
      /* X is a SYMBOL_REF.  Write out the name preceded by a
	 period and without any trailing data in brackets.  Used for function
	 names.  If we are configured for System V (or the embedded ABI) on
	 the PowerPC, do not emit the period, since those systems do not use
	 TOCs and the like.  */
      if (!SYMBOL_REF_P (x))
	{
	  output_operand_lossage ("invalid %%z value");
	  return;
	}

      /* For macho, check to see if we need a stub.  */
      if (TARGET_MACHO)
	{
	  const char *name = XSTR (x, 0);
#if TARGET_MACHO
	  if (darwin_symbol_stubs
	      && MACHOPIC_INDIRECT
	      && machopic_classify_symbol (x) == MACHOPIC_UNDEFINED_FUNCTION)
	    name = machopic_indirection_name (x, /*stub_p=*/true);
#endif
	  assemble_name (file, name);
	}
      else if (!DOT_SYMBOLS)
	assemble_name (file, XSTR (x, 0));
      else
	rs6000_output_function_entry (file, XSTR (x, 0));
      return;

    case 'Z':
      /* Like 'L', for last word of TImode/PTImode.  */
      if (REG_P (x))
	fputs (reg_names[REGNO (x) + 3], file);
      else if (MEM_P (x))
	{
	  machine_mode mode = GET_MODE (x);
	  if (GET_CODE (XEXP (x, 0)) == PRE_INC
	      || GET_CODE (XEXP (x, 0)) == PRE_DEC)
	    output_address (mode, plus_constant (Pmode,
						 XEXP (XEXP (x, 0), 0), 12));
	  else if (GET_CODE (XEXP (x, 0)) == PRE_MODIFY)
	    output_address (mode, plus_constant (Pmode,
						 XEXP (XEXP (x, 0), 0), 12));
	  else
	    output_address (mode, XEXP (adjust_address_nv (x, SImode, 12), 0));
	  if (small_data_operand (x, GET_MODE (x)))
	    fprintf (file, "@%s(%s)", SMALL_DATA_RELOC,
		     reg_names[SMALL_DATA_REG]);
	}
      return;

      /* Print AltiVec memory operand.  */
    case 'y':
      {
	rtx tmp;

	gcc_assert (MEM_P (x));

	tmp = XEXP (x, 0);

	if (VECTOR_MEM_ALTIVEC_OR_VSX_P (GET_MODE (x))
	    && GET_CODE (tmp) == AND
	    && CONST_INT_P (XEXP (tmp, 1))
	    && INTVAL (XEXP (tmp, 1)) == -16)
	  tmp = XEXP (tmp, 0);
	else if (VECTOR_MEM_VSX_P (GET_MODE (x))
		 && GET_CODE (tmp) == PRE_MODIFY)
	  tmp = XEXP (tmp, 1);
	if (REG_P (tmp))
	  fprintf (file, "0,%s", reg_names[REGNO (tmp)]);
	else
	  {
	    if (GET_CODE (tmp) != PLUS
		|| !REG_P (XEXP (tmp, 0))
		|| !REG_P (XEXP (tmp, 1)))
	      {
		output_operand_lossage ("invalid %%y value, try using the 'Z' constraint");
		break;
	      }

	    if (REGNO (XEXP (tmp, 0)) == 0)
	      fprintf (file, "%s,%s", reg_names[ REGNO (XEXP (tmp, 1)) ],
		       reg_names[ REGNO (XEXP (tmp, 0)) ]);
	    else
	      fprintf (file, "%s,%s", reg_names[ REGNO (XEXP (tmp, 0)) ],
		       reg_names[ REGNO (XEXP (tmp, 1)) ]);
	  }
	break;
      }

    case 0:
      if (REG_P (x))
	fprintf (file, "%s", reg_names[REGNO (x)]);
      else if (MEM_P (x))
	{
	  /* We need to handle PRE_INC and PRE_DEC here, since we need to
	     know the width from the mode.  */
	  if (GET_CODE (XEXP (x, 0)) == PRE_INC)
	    fprintf (file, "%d(%s)", GET_MODE_SIZE (GET_MODE (x)),
		     reg_names[REGNO (XEXP (XEXP (x, 0), 0))]);
	  else if (GET_CODE (XEXP (x, 0)) == PRE_DEC)
	    fprintf (file, "%d(%s)", - GET_MODE_SIZE (GET_MODE (x)),
		     reg_names[REGNO (XEXP (XEXP (x, 0), 0))]);
	  else if (GET_CODE (XEXP (x, 0)) == PRE_MODIFY)
	    output_address (GET_MODE (x), XEXP (XEXP (x, 0), 1));
	  else
	    output_address (GET_MODE (x), XEXP (x, 0));
	}
      else if (toc_relative_expr_p (x, false,
				    &tocrel_base_oac, &tocrel_offset_oac))
	/* This hack along with a corresponding hack in
	   rs6000_output_addr_const_extra arranges to output addends
	   where the assembler expects to find them.  eg.
	   (plus (unspec [(symbol_ref ("x")) (reg 2)] tocrel) 4)
	   without this hack would be output as "x@toc+4".  We
	   want "x+4@toc".  */
	output_addr_const (file, CONST_CAST_RTX (tocrel_base_oac));
      else if (GET_CODE (x) == UNSPEC && XINT (x, 1) == UNSPEC_TLSGD)
	output_addr_const (file, XVECEXP (x, 0, 0));
      else if (GET_CODE (x) == UNSPEC && XINT (x, 1) == UNSPEC_PLTSEQ)
	output_addr_const (file, XVECEXP (x, 0, 1));
      else
	output_addr_const (file, x);
      return;

    case '&':
      if (const char *name = get_some_local_dynamic_name ())
	assemble_name (file, name);
      else
	output_operand_lossage ("'%%&' used without any "
				"local dynamic TLS references");
      return;

    default:
      output_operand_lossage ("invalid %%xn code");
    }
}

/* Print the address of an operand.  */

void
print_operand_address (FILE *file, rtx x)
{
  if (REG_P (x))
    fprintf (file, "0(%s)", reg_names[ REGNO (x) ]);

  /* Is it a PC-relative address?  */
  else if (TARGET_PCREL && pcrel_local_or_external_address (x, VOIDmode))
    {
      HOST_WIDE_INT offset;

      if (GET_CODE (x) == CONST)
	x = XEXP (x, 0);

      if (GET_CODE (x) == PLUS)
	{
	  offset = INTVAL (XEXP (x, 1));
	  x = XEXP (x, 0);
	}
      else
	offset = 0;

      output_addr_const (file, x);

      if (offset)
	fprintf (file, "%+" PRId64, offset);

      if (SYMBOL_REF_P (x) && !SYMBOL_REF_LOCAL_P (x))
	fprintf (file, "@got");

      fprintf (file, "@pcrel");
    }
  else if (SYMBOL_REF_P (x) || GET_CODE (x) == CONST
	   || GET_CODE (x) == LABEL_REF)
    {
      output_addr_const (file, x);
      if (small_data_operand (x, GET_MODE (x)))
	fprintf (file, "@%s(%s)", SMALL_DATA_RELOC,
		 reg_names[SMALL_DATA_REG]);
      else
	gcc_assert (!TARGET_TOC);
    }
  else if (GET_CODE (x) == PLUS && REG_P (XEXP (x, 0))
	   && REG_P (XEXP (x, 1)))
    {
      if (REGNO (XEXP (x, 0)) == 0)
	fprintf (file, "%s,%s", reg_names[ REGNO (XEXP (x, 1)) ],
		 reg_names[ REGNO (XEXP (x, 0)) ]);
      else
	fprintf (file, "%s,%s", reg_names[ REGNO (XEXP (x, 0)) ],
		 reg_names[ REGNO (XEXP (x, 1)) ]);
    }
  else if (GET_CODE (x) == PLUS && REG_P (XEXP (x, 0))
	   && CONST_INT_P (XEXP (x, 1)))
    fprintf (file, HOST_WIDE_INT_PRINT_DEC "(%s)",
	     INTVAL (XEXP (x, 1)), reg_names[ REGNO (XEXP (x, 0)) ]);
#if TARGET_MACHO
  else if (GET_CODE (x) == LO_SUM && REG_P (XEXP (x, 0))
	   && CONSTANT_P (XEXP (x, 1)))
    {
      fprintf (file, "lo16(");
      output_addr_const (file, XEXP (x, 1));
      fprintf (file, ")(%s)", reg_names[ REGNO (XEXP (x, 0)) ]);
    }
#endif
#if TARGET_ELF
  else if (GET_CODE (x) == LO_SUM && REG_P (XEXP (x, 0))
	   && CONSTANT_P (XEXP (x, 1)))
    {
      output_addr_const (file, XEXP (x, 1));
      fprintf (file, "@l(%s)", reg_names[ REGNO (XEXP (x, 0)) ]);
    }
#endif
  else if (toc_relative_expr_p (x, false, &tocrel_base_oac, &tocrel_offset_oac))
    {
      /* This hack along with a corresponding hack in
	 rs6000_output_addr_const_extra arranges to output addends
	 where the assembler expects to find them.  eg.
	 (lo_sum (reg 9)
	 .       (plus (unspec [(symbol_ref ("x")) (reg 2)] tocrel) 8))
	 without this hack would be output as "x@toc+8@l(9)".  We
	 want "x+8@toc@l(9)".  */
      output_addr_const (file, CONST_CAST_RTX (tocrel_base_oac));
      if (GET_CODE (x) == LO_SUM)
	fprintf (file, "@l(%s)", reg_names[REGNO (XEXP (x, 0))]);
      else
	fprintf (file, "(%s)", reg_names[REGNO (XVECEXP (tocrel_base_oac, 0, 1))]);
    }
  else
    output_addr_const (file, x);
}

/* Implement TARGET_ASM_OUTPUT_ADDR_CONST_EXTRA.  */

bool
rs6000_output_addr_const_extra (FILE *file, rtx x)
{
  if (GET_CODE (x) == UNSPEC)
    switch (XINT (x, 1))
      {
      case UNSPEC_TOCREL:
	gcc_checking_assert (SYMBOL_REF_P (XVECEXP (x, 0, 0))
			     && REG_P (XVECEXP (x, 0, 1))
			     && REGNO (XVECEXP (x, 0, 1)) == TOC_REGISTER);
	output_addr_const (file, XVECEXP (x, 0, 0));
	if (x == tocrel_base_oac && tocrel_offset_oac != const0_rtx)
	  {
	    if (INTVAL (tocrel_offset_oac) >= 0)
	      fprintf (file, "+");
	    output_addr_const (file, CONST_CAST_RTX (tocrel_offset_oac));
	  }
	if (!TARGET_AIX || (TARGET_ELF && TARGET_MINIMAL_TOC))
	  {
	    putc ('-', file);
	    assemble_name (file, toc_label_name);
	    need_toc_init = 1;
	  }
	else if (TARGET_ELF)
	  fputs ("@toc", file);
	return true;

#if TARGET_MACHO
      case UNSPEC_MACHOPIC_OFFSET:
	output_addr_const (file, XVECEXP (x, 0, 0));
	putc ('-', file);
	machopic_output_function_base_name (file);
	return true;
#endif
      }
  return false;
}

/* Target hook for assembling integer objects.  The PowerPC version has
   to handle fixup entries for relocatable code if RELOCATABLE_NEEDS_FIXUP
   is defined.  It also needs to handle DI-mode objects on 64-bit
   targets.  */

static bool
rs6000_assemble_integer (rtx x, unsigned int size, int aligned_p)
{
#ifdef RELOCATABLE_NEEDS_FIXUP
  /* Special handling for SI values.  */
  if (RELOCATABLE_NEEDS_FIXUP && size == 4 && aligned_p)
    {
      static int recurse = 0;

      /* For -mrelocatable, we mark all addresses that need to be fixed up in
	 the .fixup section.  Since the TOC section is already relocated, we
	 don't need to mark it here.  We used to skip the text section, but it
	 should never be valid for relocated addresses to be placed in the text
	 section.  */
      if (DEFAULT_ABI == ABI_V4
	  && (TARGET_RELOCATABLE || flag_pic > 1)
	  && in_section != toc_section
	  && !recurse
	  && !CONST_SCALAR_INT_P (x)
	  && CONSTANT_P (x))
	{
	  char buf[256];

	  recurse = 1;
	  ASM_GENERATE_INTERNAL_LABEL (buf, "LCP", fixuplabelno);
	  fixuplabelno++;
	  ASM_OUTPUT_LABEL (asm_out_file, buf);
	  fprintf (asm_out_file, "\t.long\t(");
	  output_addr_const (asm_out_file, x);
	  fprintf (asm_out_file, ")@fixup\n");
	  fprintf (asm_out_file, "\t.section\t\".fixup\",\"aw\"\n");
	  ASM_OUTPUT_ALIGN (asm_out_file, 2);
	  fprintf (asm_out_file, "\t.long\t");
	  assemble_name (asm_out_file, buf);
	  fprintf (asm_out_file, "\n\t.previous\n");
	  recurse = 0;
	  return true;
	}
      /* Remove initial .'s to turn a -mcall-aixdesc function
	 address into the address of the descriptor, not the function
	 itself.  */
      else if (SYMBOL_REF_P (x)
	       && XSTR (x, 0)[0] == '.'
	       && DEFAULT_ABI == ABI_AIX)
	{
	  const char *name = XSTR (x, 0);
	  while (*name == '.')
	    name++;

	  fprintf (asm_out_file, "\t.long\t%s\n", name);
	  return true;
	}
    }
#endif /* RELOCATABLE_NEEDS_FIXUP */
  return default_assemble_integer (x, size, aligned_p);
}

/* Return a template string for assembly to emit when making an
   external call.  FUNOP is the call mem argument operand number.  */

static const char *
rs6000_call_template_1 (rtx *operands, unsigned int funop, bool sibcall)
{
  /* -Wformat-overflow workaround, without which gcc thinks that %u
      might produce 10 digits.  */
  gcc_assert (funop <= MAX_RECOG_OPERANDS);

  char arg[12];
  arg[0] = 0;
  if (GET_CODE (operands[funop + 1]) == UNSPEC)
    {
      if (XINT (operands[funop + 1], 1) == UNSPEC_TLSGD)
	sprintf (arg, "(%%%u@tlsgd)", funop + 1);
      else if (XINT (operands[funop + 1], 1) == UNSPEC_TLSLD)
	sprintf (arg, "(%%&@tlsld)");
    }

  /* The magic 32768 offset here corresponds to the offset of
     r30 in .got2, as given by LCTOC1.  See sysv4.h:toc_section.  */
  char z[11];
  sprintf (z, "%%z%u%s", funop,
	   (DEFAULT_ABI == ABI_V4 && TARGET_SECURE_PLT && flag_pic == 2
	    ? "+32768" : ""));

  static char str[32];  /* 1 spare */
  if (rs6000_pcrel_p (cfun))
    sprintf (str, "b%s %s@notoc%s", sibcall ? "" : "l", z, arg);
  else if (DEFAULT_ABI == ABI_AIX || DEFAULT_ABI == ABI_ELFv2)
    sprintf (str, "b%s %s%s%s", sibcall ? "" : "l", z, arg,
	     sibcall ? "" : "\n\tnop");
  else if (DEFAULT_ABI == ABI_V4)
    sprintf (str, "b%s %s%s%s", sibcall ? "" : "l", z, arg,
	     flag_pic ? "@plt" : "");
#if TARGET_MACHO
  /* If/when we remove the mlongcall opt, we can share the AIX/ELGv2 case. */
   else if (DEFAULT_ABI == ABI_DARWIN)
    {
      /* The cookie is in operand func+2.  */
      gcc_checking_assert (GET_CODE (operands[funop + 2]) == CONST_INT);
      int cookie = INTVAL (operands[funop + 2]);
      if (cookie & CALL_LONG)
	{
	  tree funname = get_identifier (XSTR (operands[funop], 0));
	  tree labelname = get_prev_label (funname);
	  gcc_checking_assert (labelname && !sibcall);

	  /* "jbsr foo, L42" is Mach-O for "Link as 'bl foo' if a 'bl'
	     instruction will reach 'foo', otherwise link as 'bl L42'".
	     "L42" should be a 'branch island', that will do a far jump to
	     'foo'.  Branch islands are generated in
	     macho_branch_islands().  */
	  sprintf (str, "jbsr %%z%u,%.10s", funop,
		   IDENTIFIER_POINTER (labelname));
	}
      else
        /* Same as AIX or ELFv2, except to keep backwards compat, no nop
	   after the call.  */
	sprintf (str, "b%s %s%s", sibcall ? "" : "l", z, arg);
    }
#endif
  else
    gcc_unreachable ();
  return str;
}

const char *
rs6000_call_template (rtx *operands, unsigned int funop)
{
  return rs6000_call_template_1 (operands, funop, false);
}

const char *
rs6000_sibcall_template (rtx *operands, unsigned int funop)
{
  return rs6000_call_template_1 (operands, funop, true);
}

/* As above, for indirect calls.  */

static const char *
rs6000_indirect_call_template_1 (rtx *operands, unsigned int funop,
				 bool sibcall)
{
  /* -Wformat-overflow workaround, without which gcc thinks that %u
     might produce 10 digits.  Note that -Wformat-overflow will not
     currently warn here for str[], so do not rely on a warning to
     ensure str[] is correctly sized.  */
  gcc_assert (funop <= MAX_RECOG_OPERANDS);

  /* Currently, funop is either 0 or 1.  The maximum string is always
     a !speculate 64-bit __tls_get_addr call.

     ABI_ELFv2, pcrel:
     . 27	.reloc .,R_PPC64_TLSGD,%2\n\t
     . 35	.reloc .,R_PPC64_PLTSEQ_NOTOC,%z1\n\t
     .  9	crset 2\n\t
     . 27	.reloc .,R_PPC64_TLSGD,%2\n\t
     . 36	.reloc .,R_PPC64_PLTCALL_NOTOC,%z1\n\t
     .  8	beq%T1l-
     .---
     .142

     ABI_AIX:
     .  9	ld 2,%3\n\t
     . 27	.reloc .,R_PPC64_TLSGD,%2\n\t
     . 29	.reloc .,R_PPC64_PLTSEQ,%z1\n\t
     .  9	crset 2\n\t
     . 27	.reloc .,R_PPC64_TLSGD,%2\n\t
     . 30	.reloc .,R_PPC64_PLTCALL,%z1\n\t
     . 10	beq%T1l-\n\t
     . 10	ld 2,%4(1)
     .---
     .151

     ABI_ELFv2:
     . 27	.reloc .,R_PPC64_TLSGD,%2\n\t
     . 29	.reloc .,R_PPC64_PLTSEQ,%z1\n\t
     .  9	crset 2\n\t
     . 27	.reloc .,R_PPC64_TLSGD,%2\n\t
     . 30	.reloc .,R_PPC64_PLTCALL,%z1\n\t
     . 10	beq%T1l-\n\t
     . 10	ld 2,%3(1)
     .---
     .142

     ABI_V4:
     . 27	.reloc .,R_PPC64_TLSGD,%2\n\t
     . 35	.reloc .,R_PPC64_PLTSEQ,%z1+32768\n\t
     .  9	crset 2\n\t
     . 27	.reloc .,R_PPC64_TLSGD,%2\n\t
     . 36	.reloc .,R_PPC64_PLTCALL,%z1+32768\n\t
     .  8	beq%T1l-
     .---
     .141  */
  static char str[160];  /* 8 spare */
  char *s = str;
  const char *ptrload = TARGET_64BIT ? "d" : "wz";

  if (DEFAULT_ABI == ABI_AIX)
    s += sprintf (s,
		  "l%s 2,%%%u\n\t",
		  ptrload, funop + 3);

  /* We don't need the extra code to stop indirect call speculation if
     calling via LR.  */
  bool speculate = (TARGET_MACHO
		    || rs6000_speculate_indirect_jumps
		    || (REG_P (operands[funop])
			&& REGNO (operands[funop]) == LR_REGNO));

  if (TARGET_PLTSEQ && GET_CODE (operands[funop]) == UNSPEC)
    {
      const char *rel64 = TARGET_64BIT ? "64" : "";
      char tls[29];
      tls[0] = 0;
      if (GET_CODE (operands[funop + 1]) == UNSPEC)
	{
	  if (XINT (operands[funop + 1], 1) == UNSPEC_TLSGD)
	    sprintf (tls, ".reloc .,R_PPC%s_TLSGD,%%%u\n\t",
		     rel64, funop + 1);
	  else if (XINT (operands[funop + 1], 1) == UNSPEC_TLSLD)
	    sprintf (tls, ".reloc .,R_PPC%s_TLSLD,%%&\n\t",
		     rel64);
	}

      const char *notoc = rs6000_pcrel_p (cfun) ? "_NOTOC" : "";
      const char *addend = (DEFAULT_ABI == ABI_V4 && TARGET_SECURE_PLT
			    && flag_pic == 2 ? "+32768" : "");
      if (!speculate)
	{
	  s += sprintf (s,
			"%s.reloc .,R_PPC%s_PLTSEQ%s,%%z%u%s\n\t",
			tls, rel64, notoc, funop, addend);
	  s += sprintf (s, "crset 2\n\t");
	}
      s += sprintf (s,
		    "%s.reloc .,R_PPC%s_PLTCALL%s,%%z%u%s\n\t",
		    tls, rel64, notoc, funop, addend);
    }
  else if (!speculate)
    s += sprintf (s, "crset 2\n\t");

  if (rs6000_pcrel_p (cfun))
    {
      if (speculate)
	sprintf (s, "b%%T%ul", funop);
      else
	sprintf (s, "beq%%T%ul-", funop);
    }
  else if (DEFAULT_ABI == ABI_AIX)
    {
      if (speculate)
	sprintf (s,
		 "b%%T%ul\n\t"
		 "l%s 2,%%%u(1)",
		 funop, ptrload, funop + 4);
      else
	sprintf (s,
		 "beq%%T%ul-\n\t"
		 "l%s 2,%%%u(1)",
		 funop, ptrload, funop + 4);
    }
  else if (DEFAULT_ABI == ABI_ELFv2)
    {
      if (speculate)
	sprintf (s,
		 "b%%T%ul\n\t"
		 "l%s 2,%%%u(1)",
		 funop, ptrload, funop + 3);
      else
	sprintf (s,
		 "beq%%T%ul-\n\t"
		 "l%s 2,%%%u(1)",
		 funop, ptrload, funop + 3);
    }
  else
    {
      if (speculate)
	sprintf (s,
		 "b%%T%u%s",
		 funop, sibcall ? "" : "l");
      else
	sprintf (s,
		 "beq%%T%u%s-%s",
		 funop, sibcall ? "" : "l", sibcall ? "\n\tb $" : "");
    }
  return str;
}

const char *
rs6000_indirect_call_template (rtx *operands, unsigned int funop)
{
  return rs6000_indirect_call_template_1 (operands, funop, false);
}

const char *
rs6000_indirect_sibcall_template (rtx *operands, unsigned int funop)
{
  return rs6000_indirect_call_template_1 (operands, funop, true);
}

#if HAVE_AS_PLTSEQ
/* Output indirect call insns.  WHICH identifies the type of sequence.  */
const char *
rs6000_pltseq_template (rtx *operands, int which)
{
  const char *rel64 = TARGET_64BIT ? "64" : "";
  char tls[30];
  tls[0] = 0;
  if (GET_CODE (operands[3]) == UNSPEC)
    {
      char off = which == RS6000_PLTSEQ_PLT_PCREL34 ? '8' : '4';
      if (XINT (operands[3], 1) == UNSPEC_TLSGD)
	sprintf (tls, ".reloc .-%c,R_PPC%s_TLSGD,%%3\n\t",
		 off, rel64);
      else if (XINT (operands[3], 1) == UNSPEC_TLSLD)
	sprintf (tls, ".reloc .-%c,R_PPC%s_TLSLD,%%&\n\t",
		 off, rel64);
    }

  gcc_assert (DEFAULT_ABI == ABI_ELFv2 || DEFAULT_ABI == ABI_V4);
  static char str[96];  /* 10 spare */
  char off = WORDS_BIG_ENDIAN ? '2' : '4';
  const char *addend = (DEFAULT_ABI == ABI_V4 && TARGET_SECURE_PLT
			&& flag_pic == 2 ? "+32768" : "");
  switch (which)
    {
    case RS6000_PLTSEQ_TOCSAVE:
      sprintf (str,
	       "st%s\n\t"
	       "%s.reloc .-4,R_PPC%s_PLTSEQ,%%z2",
	       TARGET_64BIT ? "d 2,24(1)" : "w 2,12(1)",
	       tls, rel64);
      break;
    case RS6000_PLTSEQ_PLT16_HA:
      if (DEFAULT_ABI == ABI_V4 && !flag_pic)
	sprintf (str,
		 "lis %%0,0\n\t"
		 "%s.reloc .-%c,R_PPC%s_PLT16_HA,%%z2",
		 tls, off, rel64);
      else
	sprintf (str,
		 "addis %%0,%%1,0\n\t"
		 "%s.reloc .-%c,R_PPC%s_PLT16_HA,%%z2%s",
		 tls, off, rel64, addend);
      break;
    case RS6000_PLTSEQ_PLT16_LO:
      sprintf (str,
	       "l%s %%0,0(%%1)\n\t"
	       "%s.reloc .-%c,R_PPC%s_PLT16_LO%s,%%z2%s",
	       TARGET_64BIT ? "d" : "wz",
	       tls, off, rel64, TARGET_64BIT ? "_DS" : "", addend);
      break;
    case RS6000_PLTSEQ_MTCTR:
      sprintf (str,
	       "mtctr %%1\n\t"
	       "%s.reloc .-4,R_PPC%s_PLTSEQ,%%z2%s",
	       tls, rel64, addend);
      break;
    case RS6000_PLTSEQ_PLT_PCREL34:
      sprintf (str,
	       "pl%s %%0,0(0),1\n\t"
	       "%s.reloc .-8,R_PPC%s_PLT_PCREL34_NOTOC,%%z2",
	       TARGET_64BIT ? "d" : "wz",
	       tls, rel64);
      break;
    default:
      gcc_unreachable ();
    }
  return str;
}
#endif

#if defined (HAVE_GAS_HIDDEN) && !TARGET_MACHO
/* Emit an assembler directive to set symbol visibility for DECL to
   VISIBILITY_TYPE.  */

static void
rs6000_assemble_visibility (tree decl, int vis)
{
  if (TARGET_XCOFF)
    return;

  /* Functions need to have their entry point symbol visibility set as
     well as their descriptor symbol visibility.  */
  if (DEFAULT_ABI == ABI_AIX
      && DOT_SYMBOLS
      && TREE_CODE (decl) == FUNCTION_DECL)
    {
      static const char * const visibility_types[] = {
	NULL, "protected", "hidden", "internal"
      };

      const char *name, *type;

      name = ((* targetm.strip_name_encoding)
	      (IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (decl))));
      type = visibility_types[vis];

      fprintf (asm_out_file, "\t.%s\t%s\n", type, name);
      fprintf (asm_out_file, "\t.%s\t.%s\n", type, name);
    }
  else
    default_assemble_visibility (decl, vis);
}
#endif

enum rtx_code
rs6000_reverse_condition (machine_mode mode, enum rtx_code code)
{
  /* Reversal of FP compares takes care -- an ordered compare
     becomes an unordered compare and vice versa.  */
  if (mode == CCFPmode
      && (!flag_finite_math_only
	  || code == UNLT || code == UNLE || code == UNGT || code == UNGE
	  || code == UNEQ || code == LTGT))
    return reverse_condition_maybe_unordered (code);
  else
    return reverse_condition (code);
}

/* Generate a compare for CODE.  Return a brand-new rtx that
   represents the result of the compare.  */

static rtx
rs6000_generate_compare (rtx cmp, machine_mode mode)
{
  machine_mode comp_mode;
  rtx compare_result;
  enum rtx_code code = GET_CODE (cmp);
  rtx op0 = XEXP (cmp, 0);
  rtx op1 = XEXP (cmp, 1);

  if (!TARGET_FLOAT128_HW && FLOAT128_VECTOR_P (mode))
    comp_mode = CCmode;
  else if (FLOAT_MODE_P (mode))
    comp_mode = CCFPmode;
  else if (code == GTU || code == LTU
	   || code == GEU || code == LEU)
    comp_mode = CCUNSmode;
  else if ((code == EQ || code == NE)
	   && unsigned_reg_p (op0)
	   && (unsigned_reg_p (op1)
	       || (CONST_INT_P (op1) && INTVAL (op1) != 0)))
    /* These are unsigned values, perhaps there will be a later
       ordering compare that can be shared with this one.  */
    comp_mode = CCUNSmode;
  else
    comp_mode = CCmode;

  /* If we have an unsigned compare, make sure we don't have a signed value as
     an immediate.  */
  if (comp_mode == CCUNSmode && CONST_INT_P (op1)
      && INTVAL (op1) < 0)
    {
      op0 = copy_rtx_if_shared (op0);
      op1 = force_reg (GET_MODE (op0), op1);
      cmp = gen_rtx_fmt_ee (code, GET_MODE (cmp), op0, op1);
    }

  /* First, the compare.  */
  compare_result = gen_reg_rtx (comp_mode);

  /* IEEE 128-bit support in VSX registers when we do not have hardware
     support.  */
  if (!TARGET_FLOAT128_HW && FLOAT128_VECTOR_P (mode))
    {
      rtx libfunc = NULL_RTX;
      bool check_nan = false;
      rtx dest;

      switch (code)
	{
	case EQ:
	case NE:
	  libfunc = optab_libfunc (eq_optab, mode);
	  break;

	case GT:
	case GE:
	  libfunc = optab_libfunc (ge_optab, mode);
	  break;

	case LT:
	case LE:
	  libfunc = optab_libfunc (le_optab, mode);
	  break;

	case UNORDERED:
	case ORDERED:
	  libfunc = optab_libfunc (unord_optab, mode);
	  code = (code == UNORDERED) ? NE : EQ;
	  break;

	case UNGE:
	case UNGT:
	  check_nan = true;
	  libfunc = optab_libfunc (ge_optab, mode);
	  code = (code == UNGE) ? GE : GT;
	  break;

	case UNLE:
	case UNLT:
	  check_nan = true;
	  libfunc = optab_libfunc (le_optab, mode);
	  code = (code == UNLE) ? LE : LT;
	  break;

	case UNEQ:
	case LTGT:
	  check_nan = true;
	  libfunc = optab_libfunc (eq_optab, mode);
	  code = (code = UNEQ) ? EQ : NE;
	  break;

	default:
	  gcc_unreachable ();
	}

      gcc_assert (libfunc);

      if (!check_nan)
	dest = emit_library_call_value (libfunc, NULL_RTX, LCT_CONST,
					SImode, op0, mode, op1, mode);

      /* The library signals an exception for signalling NaNs, so we need to
	 handle isgreater, etc. by first checking isordered.  */
      else
	{
	  rtx ne_rtx, normal_dest, unord_dest;
	  rtx unord_func = optab_libfunc (unord_optab, mode);
	  rtx join_label = gen_label_rtx ();
	  rtx join_ref = gen_rtx_LABEL_REF (VOIDmode, join_label);
	  rtx unord_cmp = gen_reg_rtx (comp_mode);


	  /* Test for either value being a NaN.  */
	  gcc_assert (unord_func);
	  unord_dest = emit_library_call_value (unord_func, NULL_RTX, LCT_CONST,
						SImode, op0, mode, op1, mode);

	  /* Set value (0) if either value is a NaN, and jump to the join
	     label.  */
	  dest = gen_reg_rtx (SImode);
	  emit_move_insn (dest, const1_rtx);
	  emit_insn (gen_rtx_SET (unord_cmp,
				  gen_rtx_COMPARE (comp_mode, unord_dest,
						   const0_rtx)));

	  ne_rtx = gen_rtx_NE (comp_mode, unord_cmp, const0_rtx);
	  emit_jump_insn (gen_rtx_SET (pc_rtx,
				       gen_rtx_IF_THEN_ELSE (VOIDmode, ne_rtx,
							     join_ref,
							     pc_rtx)));

	  /* Do the normal comparison, knowing that the values are not
	     NaNs.  */
	  normal_dest = emit_library_call_value (libfunc, NULL_RTX, LCT_CONST,
						 SImode, op0, mode, op1, mode);

	  emit_insn (gen_cstoresi4 (dest,
				    gen_rtx_fmt_ee (code, SImode, normal_dest,
						    const0_rtx),
				    normal_dest, const0_rtx));

	  /* Join NaN and non-Nan paths.  Compare dest against 0.  */
	  emit_label (join_label);
	  code = NE;
	}

      emit_insn (gen_rtx_SET (compare_result,
			      gen_rtx_COMPARE (comp_mode, dest, const0_rtx)));
    }

  else
    {
      /* Generate XLC-compatible TFmode compare as PARALLEL with extra
	 CLOBBERs to match cmptf_internal2 pattern.  */
      if (comp_mode == CCFPmode && TARGET_XL_COMPAT
	  && FLOAT128_IBM_P (GET_MODE (op0))
	  && TARGET_HARD_FLOAT)
	emit_insn (gen_rtx_PARALLEL (VOIDmode,
	  gen_rtvec (10,
		     gen_rtx_SET (compare_result,
				  gen_rtx_COMPARE (comp_mode, op0, op1)),
		     gen_rtx_CLOBBER (VOIDmode, gen_rtx_SCRATCH (DFmode)),
		     gen_rtx_CLOBBER (VOIDmode, gen_rtx_SCRATCH (DFmode)),
		     gen_rtx_CLOBBER (VOIDmode, gen_rtx_SCRATCH (DFmode)),
		     gen_rtx_CLOBBER (VOIDmode, gen_rtx_SCRATCH (DFmode)),
		     gen_rtx_CLOBBER (VOIDmode, gen_rtx_SCRATCH (DFmode)),
		     gen_rtx_CLOBBER (VOIDmode, gen_rtx_SCRATCH (DFmode)),
		     gen_rtx_CLOBBER (VOIDmode, gen_rtx_SCRATCH (DFmode)),
		     gen_rtx_CLOBBER (VOIDmode, gen_rtx_SCRATCH (DFmode)),
		     gen_rtx_CLOBBER (VOIDmode, gen_rtx_SCRATCH (Pmode)))));
      else if (GET_CODE (op1) == UNSPEC
	       && XINT (op1, 1) == UNSPEC_SP_TEST)
	{
	  rtx op1b = XVECEXP (op1, 0, 0);
	  comp_mode = CCEQmode;
	  compare_result = gen_reg_rtx (CCEQmode);
	  if (TARGET_64BIT)
	    emit_insn (gen_stack_protect_testdi (compare_result, op0, op1b));
	  else
	    emit_insn (gen_stack_protect_testsi (compare_result, op0, op1b));
	}
      else
	emit_insn (gen_rtx_SET (compare_result,
				gen_rtx_COMPARE (comp_mode, op0, op1)));
    }

  validate_condition_mode (code, GET_MODE (compare_result));

  return gen_rtx_fmt_ee (code, VOIDmode, compare_result, const0_rtx);
}


/* Return the diagnostic message string if the binary operation OP is
   not permitted on TYPE1 and TYPE2, NULL otherwise.  */

static const char*
rs6000_invalid_binary_op (int op ATTRIBUTE_UNUSED,
			  const_tree type1,
			  const_tree type2)
{
  machine_mode mode1 = TYPE_MODE (type1);
  machine_mode mode2 = TYPE_MODE (type2);

  /* For complex modes, use the inner type.  */
  if (COMPLEX_MODE_P (mode1))
    mode1 = GET_MODE_INNER (mode1);

  if (COMPLEX_MODE_P (mode2))
    mode2 = GET_MODE_INNER (mode2);

  /* Don't allow IEEE 754R 128-bit binary floating point and IBM extended
     double to intermix unless -mfloat128-convert.  */
  if (mode1 == mode2)
    return NULL;

  if (!TARGET_FLOAT128_CVT)
    {
      if ((mode1 == KFmode && mode2 == IFmode)
	  || (mode1 == IFmode && mode2 == KFmode))
	return N_("__float128 and __ibm128 cannot be used in the same "
		  "expression");

      if (TARGET_IEEEQUAD
	  && ((mode1 == IFmode && mode2 == TFmode)
	      || (mode1 == TFmode && mode2 == IFmode)))
	return N_("__ibm128 and long double cannot be used in the same "
		  "expression");

      if (!TARGET_IEEEQUAD
	  && ((mode1 == KFmode && mode2 == TFmode)
	      || (mode1 == TFmode && mode2 == KFmode)))
	return N_("__float128 and long double cannot be used in the same "
		  "expression");
    }

  return NULL;
}


/* Expand floating point conversion to/from __float128 and __ibm128.  */

void
rs6000_expand_float128_convert (rtx dest, rtx src, bool unsigned_p)
{
  machine_mode dest_mode = GET_MODE (dest);
  machine_mode src_mode = GET_MODE (src);
  convert_optab cvt = unknown_optab;
  bool do_move = false;
  rtx libfunc = NULL_RTX;
  rtx dest2;
  typedef rtx (*rtx_2func_t) (rtx, rtx);
  rtx_2func_t hw_convert = (rtx_2func_t)0;
  size_t kf_or_tf;

  struct hw_conv_t {
    rtx_2func_t	from_df;
    rtx_2func_t from_sf;
    rtx_2func_t from_si_sign;
    rtx_2func_t from_si_uns;
    rtx_2func_t from_di_sign;
    rtx_2func_t from_di_uns;
    rtx_2func_t to_df;
    rtx_2func_t to_sf;
    rtx_2func_t to_si_sign;
    rtx_2func_t to_si_uns;
    rtx_2func_t to_di_sign;
    rtx_2func_t to_di_uns;
  } hw_conversions[2] = {
    /* convertions to/from KFmode */
    {
      gen_extenddfkf2_hw,		/* KFmode <- DFmode.  */
      gen_extendsfkf2_hw,		/* KFmode <- SFmode.  */
      gen_float_kfsi2_hw,		/* KFmode <- SImode (signed).  */
      gen_floatuns_kfsi2_hw,		/* KFmode <- SImode (unsigned).  */
      gen_float_kfdi2_hw,		/* KFmode <- DImode (signed).  */
      gen_floatuns_kfdi2_hw,		/* KFmode <- DImode (unsigned).  */
      gen_trunckfdf2_hw,		/* DFmode <- KFmode.  */
      gen_trunckfsf2_hw,		/* SFmode <- KFmode.  */
      gen_fix_kfsi2_hw,			/* SImode <- KFmode (signed).  */
      gen_fixuns_kfsi2_hw,		/* SImode <- KFmode (unsigned).  */
      gen_fix_kfdi2_hw,			/* DImode <- KFmode (signed).  */
      gen_fixuns_kfdi2_hw,		/* DImode <- KFmode (unsigned).  */
    },

    /* convertions to/from TFmode */
    {
      gen_extenddftf2_hw,		/* TFmode <- DFmode.  */
      gen_extendsftf2_hw,		/* TFmode <- SFmode.  */
      gen_float_tfsi2_hw,		/* TFmode <- SImode (signed).  */
      gen_floatuns_tfsi2_hw,		/* TFmode <- SImode (unsigned).  */
      gen_float_tfdi2_hw,		/* TFmode <- DImode (signed).  */
      gen_floatuns_tfdi2_hw,		/* TFmode <- DImode (unsigned).  */
      gen_trunctfdf2_hw,		/* DFmode <- TFmode.  */
      gen_trunctfsf2_hw,		/* SFmode <- TFmode.  */
      gen_fix_tfsi2_hw,			/* SImode <- TFmode (signed).  */
      gen_fixuns_tfsi2_hw,		/* SImode <- TFmode (unsigned).  */
      gen_fix_tfdi2_hw,			/* DImode <- TFmode (signed).  */
      gen_fixuns_tfdi2_hw,		/* DImode <- TFmode (unsigned).  */
    },
  };

  if (dest_mode == src_mode)
    gcc_unreachable ();

  /* Eliminate memory operations.  */
  if (MEM_P (src))
    src = force_reg (src_mode, src);

  if (MEM_P (dest))
    {
      rtx tmp = gen_reg_rtx (dest_mode);
      rs6000_expand_float128_convert (tmp, src, unsigned_p);
      rs6000_emit_move (dest, tmp, dest_mode);
      return;
    }

  /* Convert to IEEE 128-bit floating point.  */
  if (FLOAT128_IEEE_P (dest_mode))
    {
      if (dest_mode == KFmode)
	kf_or_tf = 0;
      else if (dest_mode == TFmode)
	kf_or_tf = 1;
      else
	gcc_unreachable ();

      switch (src_mode)
	{
	case E_DFmode:
	  cvt = sext_optab;
	  hw_convert = hw_conversions[kf_or_tf].from_df;
	  break;

	case E_SFmode:
	  cvt = sext_optab;
	  hw_convert = hw_conversions[kf_or_tf].from_sf;
	  break;

	case E_KFmode:
	case E_IFmode:
	case E_TFmode:
	  if (FLOAT128_IBM_P (src_mode))
	    cvt = sext_optab;
	  else
	    do_move = true;
	  break;

	case E_SImode:
	  if (unsigned_p)
	    {
	      cvt = ufloat_optab;
	      hw_convert = hw_conversions[kf_or_tf].from_si_uns;
	    }
	  else
	    {
	      cvt = sfloat_optab;
	      hw_convert = hw_conversions[kf_or_tf].from_si_sign;
	    }
	  break;

	case E_DImode:
	  if (unsigned_p)
	    {
	      cvt = ufloat_optab;
	      hw_convert = hw_conversions[kf_or_tf].from_di_uns;
	    }
	  else
	    {
	      cvt = sfloat_optab;
	      hw_convert = hw_conversions[kf_or_tf].from_di_sign;
	    }
	  break;

	default:
	  gcc_unreachable ();
	}
    }

  /* Convert from IEEE 128-bit floating point.  */
  else if (FLOAT128_IEEE_P (src_mode))
    {
      if (src_mode == KFmode)
	kf_or_tf = 0;
      else if (src_mode == TFmode)
	kf_or_tf = 1;
      else
	gcc_unreachable ();

      switch (dest_mode)
	{
	case E_DFmode:
	  cvt = trunc_optab;
	  hw_convert = hw_conversions[kf_or_tf].to_df;
	  break;

	case E_SFmode:
	  cvt = trunc_optab;
	  hw_convert = hw_conversions[kf_or_tf].to_sf;
	  break;

	case E_KFmode:
	case E_IFmode:
	case E_TFmode:
	  if (FLOAT128_IBM_P (dest_mode))
	    cvt = trunc_optab;
	  else
	    do_move = true;
	  break;

	case E_SImode:
	  if (unsigned_p)
	    {
	      cvt = ufix_optab;
	      hw_convert = hw_conversions[kf_or_tf].to_si_uns;
	    }
	  else
	    {
	      cvt = sfix_optab;
	      hw_convert = hw_conversions[kf_or_tf].to_si_sign;
	    }
	  break;

	case E_DImode:
	  if (unsigned_p)
	    {
	      cvt = ufix_optab;
	      hw_convert = hw_conversions[kf_or_tf].to_di_uns;
	    }
	  else
	    {
	      cvt = sfix_optab;
	      hw_convert = hw_conversions[kf_or_tf].to_di_sign;
	    }
	  break;

	default:
	  gcc_unreachable ();
	}
    }

  /* Both IBM format.  */
  else if (FLOAT128_IBM_P (dest_mode) && FLOAT128_IBM_P (src_mode))
    do_move = true;

  else
    gcc_unreachable ();

  /* Handle conversion between TFmode/KFmode/IFmode.  */
  if (do_move)
    emit_insn (gen_rtx_SET (dest, gen_rtx_FLOAT_EXTEND (dest_mode, src)));

  /* Handle conversion if we have hardware support.  */
  else if (TARGET_FLOAT128_HW && hw_convert)
    emit_insn ((hw_convert) (dest, src));

  /* Call an external function to do the conversion.  */
  else if (cvt != unknown_optab)
    {
      libfunc = convert_optab_libfunc (cvt, dest_mode, src_mode);
      gcc_assert (libfunc != NULL_RTX);

      dest2 = emit_library_call_value (libfunc, dest, LCT_CONST, dest_mode,
				       src, src_mode);

      gcc_assert (dest2 != NULL_RTX);
      if (!rtx_equal_p (dest, dest2))
	emit_move_insn (dest, dest2);
    }

  else
    gcc_unreachable ();

  return;
}


/* Emit RTL that sets a register to zero if OP1 and OP2 are equal.  SCRATCH
   can be used as that dest register.  Return the dest register.  */

rtx
rs6000_emit_eqne (machine_mode mode, rtx op1, rtx op2, rtx scratch)
{
  if (op2 == const0_rtx)
    return op1;

  if (GET_CODE (scratch) == SCRATCH)
    scratch = gen_reg_rtx (mode);

  if (logical_operand (op2, mode))
    emit_insn (gen_rtx_SET (scratch, gen_rtx_XOR (mode, op1, op2)));
  else
    emit_insn (gen_rtx_SET (scratch,
			    gen_rtx_PLUS (mode, op1, negate_rtx (mode, op2))));

  return scratch;
}

/* Emit code doing a cror of two CR bits, for FP comparisons with a CODE that
   requires this.  The result is mode MODE.  */
rtx
rs6000_emit_fp_cror (rtx_code code, machine_mode mode, rtx x)
{
  rtx cond[2];
  int n = 0;
  if (code == LTGT || code == LE || code == UNLT)
    cond[n++] = gen_rtx_fmt_ee (LT, mode, x, const0_rtx);
  if (code == LTGT || code == GE || code == UNGT)
    cond[n++] = gen_rtx_fmt_ee (GT, mode, x, const0_rtx);
  if (code == LE || code == GE || code == UNEQ)
    cond[n++] = gen_rtx_fmt_ee (EQ, mode, x, const0_rtx);
  if (code == UNLT || code == UNGT || code == UNEQ)
    cond[n++] = gen_rtx_fmt_ee (UNORDERED, mode, x, const0_rtx);

  gcc_assert (n == 2);

  rtx cc = gen_reg_rtx (CCEQmode);
  rtx logical = gen_rtx_IOR (mode, cond[0], cond[1]);
  emit_insn (gen_cceq_ior_compare (mode, cc, logical, cond[0], x, cond[1], x));

  return cc;
}

void
rs6000_emit_sCOND (machine_mode mode, rtx operands[])
{
  rtx condition_rtx = rs6000_generate_compare (operands[1], mode);
  rtx_code cond_code = GET_CODE (condition_rtx);

  if (FLOAT_MODE_P (mode) && HONOR_NANS (mode)
      && !(FLOAT128_VECTOR_P (mode) && !TARGET_FLOAT128_HW))
    ;
  else if (cond_code == NE
	   || cond_code == GE || cond_code == LE
	   || cond_code == GEU || cond_code == LEU
	   || cond_code == ORDERED || cond_code == UNGE || cond_code == UNLE)
    {
      rtx not_result = gen_reg_rtx (CCEQmode);
      rtx not_op, rev_cond_rtx;
      machine_mode cc_mode;

      cc_mode = GET_MODE (XEXP (condition_rtx, 0));

      rev_cond_rtx = gen_rtx_fmt_ee (rs6000_reverse_condition (cc_mode, cond_code),
				     SImode, XEXP (condition_rtx, 0), const0_rtx);
      not_op = gen_rtx_COMPARE (CCEQmode, rev_cond_rtx, const0_rtx);
      emit_insn (gen_rtx_SET (not_result, not_op));
      condition_rtx = gen_rtx_EQ (VOIDmode, not_result, const0_rtx);
    }

  machine_mode op_mode = GET_MODE (XEXP (operands[1], 0));
  if (op_mode == VOIDmode)
    op_mode = GET_MODE (XEXP (operands[1], 1));

  if (TARGET_POWERPC64 && (op_mode == DImode || FLOAT_MODE_P (mode)))
    {
      PUT_MODE (condition_rtx, DImode);
      convert_move (operands[0], condition_rtx, 0);
    }
  else
    {
      PUT_MODE (condition_rtx, SImode);
      emit_insn (gen_rtx_SET (operands[0], condition_rtx));
    }
}

/* Emit a branch of kind CODE to location LOC.  */

void
rs6000_emit_cbranch (machine_mode mode, rtx operands[])
{
  rtx condition_rtx = rs6000_generate_compare (operands[0], mode);
  rtx loc_ref = gen_rtx_LABEL_REF (VOIDmode, operands[3]);
  rtx ite = gen_rtx_IF_THEN_ELSE (VOIDmode, condition_rtx, loc_ref, pc_rtx);
  emit_jump_insn (gen_rtx_SET (pc_rtx, ite));
}

/* Return the string to output a conditional branch to LABEL, which is
   the operand template of the label, or NULL if the branch is really a
   conditional return.

   OP is the conditional expression.  XEXP (OP, 0) is assumed to be a
   condition code register and its mode specifies what kind of
   comparison we made.

   REVERSED is nonzero if we should reverse the sense of the comparison.

   INSN is the insn.  */

char *
output_cbranch (rtx op, const char *label, int reversed, rtx_insn *insn)
{
  static char string[64];
  enum rtx_code code = GET_CODE (op);
  rtx cc_reg = XEXP (op, 0);
  machine_mode mode = GET_MODE (cc_reg);
  int cc_regno = REGNO (cc_reg) - CR0_REGNO;
  int need_longbranch = label != NULL && get_attr_length (insn) == 8;
  int really_reversed = reversed ^ need_longbranch;
  char *s = string;
  const char *ccode;
  const char *pred;
  rtx note;

  validate_condition_mode (code, mode);

  /* Work out which way this really branches.  We could use
     reverse_condition_maybe_unordered here always but this
     makes the resulting assembler clearer.  */
  if (really_reversed)
    {
      /* Reversal of FP compares takes care -- an ordered compare
	 becomes an unordered compare and vice versa.  */
      if (mode == CCFPmode)
	code = reverse_condition_maybe_unordered (code);
      else
	code = reverse_condition (code);
    }

  switch (code)
    {
      /* Not all of these are actually distinct opcodes, but
	 we distinguish them for clarity of the resulting assembler.  */
    case NE: case LTGT:
      ccode = "ne"; break;
    case EQ: case UNEQ:
      ccode = "eq"; break;
    case GE: case GEU:
      ccode = "ge"; break;
    case GT: case GTU: case UNGT:
      ccode = "gt"; break;
    case LE: case LEU:
      ccode = "le"; break;
    case LT: case LTU: case UNLT:
      ccode = "lt"; break;
    case UNORDERED: ccode = "un"; break;
    case ORDERED: ccode = "nu"; break;
    case UNGE: ccode = "nl"; break;
    case UNLE: ccode = "ng"; break;
    default:
      gcc_unreachable ();
    }

  /* Maybe we have a guess as to how likely the branch is.  */
  pred = "";
  note = find_reg_note (insn, REG_BR_PROB, NULL_RTX);
  if (note != NULL_RTX)
    {
      /* PROB is the difference from 50%.  */
      int prob = profile_probability::from_reg_br_prob_note (XINT (note, 0))
		   .to_reg_br_prob_base () - REG_BR_PROB_BASE / 2;

      /* Only hint for highly probable/improbable branches on newer cpus when
	 we have real profile data, as static prediction overrides processor
	 dynamic prediction.  For older cpus we may as well always hint, but
	 assume not taken for branches that are very close to 50% as a
	 mispredicted taken branch is more expensive than a
	 mispredicted not-taken branch.  */
      if (rs6000_always_hint
	  || (abs (prob) > REG_BR_PROB_BASE / 100 * 48
	      && (profile_status_for_fn (cfun) != PROFILE_GUESSED)
	      && br_prob_note_reliable_p (note)))
	{
	  if (abs (prob) > REG_BR_PROB_BASE / 20
	      && ((prob > 0) ^ need_longbranch))
	    pred = "+";
	  else
	    pred = "-";
	}
    }

  if (label == NULL)
    s += sprintf (s, "b%slr%s ", ccode, pred);
  else
    s += sprintf (s, "b%s%s ", ccode, pred);

  /* We need to escape any '%' characters in the reg_names string.
     Assume they'd only be the first character....  */
  if (reg_names[cc_regno + CR0_REGNO][0] == '%')
    *s++ = '%';
  s += sprintf (s, "%s", reg_names[cc_regno + CR0_REGNO]);

  if (label != NULL)
    {
      /* If the branch distance was too far, we may have to use an
	 unconditional branch to go the distance.  */
      if (need_longbranch)
	s += sprintf (s, ",$+8\n\tb %s", label);
      else
	s += sprintf (s, ",%s", label);
    }

  return string;
}

/* Return insn for VSX or Altivec comparisons.  */

static rtx
rs6000_emit_vector_compare_inner (enum rtx_code code, rtx op0, rtx op1)
{
  rtx mask;
  machine_mode mode = GET_MODE (op0);

  switch (code)
    {
    default:
      break;

    case GE:
      if (GET_MODE_CLASS (mode) == MODE_VECTOR_INT)
	return NULL_RTX;
      /* FALLTHRU */

    case EQ:
    case GT:
    case GTU:
    case ORDERED:
    case UNORDERED:
    case UNEQ:
    case LTGT:
      mask = gen_reg_rtx (mode);
      emit_insn (gen_rtx_SET (mask, gen_rtx_fmt_ee (code, mode, op0, op1)));
      return mask;
    }

  return NULL_RTX;
}

/* Emit vector compare for operands OP0 and OP1 using code RCODE.
   DMODE is expected destination mode. This is a recursive function.  */

static rtx
rs6000_emit_vector_compare (enum rtx_code rcode,
			    rtx op0, rtx op1,
			    machine_mode dmode)
{
  rtx mask;
  bool swap_operands = false;
  bool try_again = false;

  gcc_assert (VECTOR_UNIT_ALTIVEC_OR_VSX_P (dmode));
  gcc_assert (GET_MODE (op0) == GET_MODE (op1));

  /* See if the comparison works as is.  */
  mask = rs6000_emit_vector_compare_inner (rcode, op0, op1);
  if (mask)
    return mask;

  switch (rcode)
    {
    case LT:
      rcode = GT;
      swap_operands = true;
      try_again = true;
      break;
    case LTU:
      rcode = GTU;
      swap_operands = true;
      try_again = true;
      break;
    case NE:
    case UNLE:
    case UNLT:
    case UNGE:
    case UNGT:
      /* Invert condition and try again.
	 e.g., A != B becomes ~(A==B).  */
      {
	enum rtx_code rev_code;
	enum insn_code nor_code;
	rtx mask2;

	rev_code = reverse_condition_maybe_unordered (rcode);
	if (rev_code == UNKNOWN)
	  return NULL_RTX;

	nor_code = optab_handler (one_cmpl_optab, dmode);
	if (nor_code == CODE_FOR_nothing)
	  return NULL_RTX;

	mask2 = rs6000_emit_vector_compare (rev_code, op0, op1, dmode);
	if (!mask2)
	  return NULL_RTX;

	mask = gen_reg_rtx (dmode);
	emit_insn (GEN_FCN (nor_code) (mask, mask2));
	return mask;
      }
      break;
    case GE:
    case GEU:
    case LE:
    case LEU:
      /* Try GT/GTU/LT/LTU OR EQ */
      {
	rtx c_rtx, eq_rtx;
	enum insn_code ior_code;
	enum rtx_code new_code;

	switch (rcode)
	  {
	  case  GE:
	    new_code = GT;
	    break;

	  case GEU:
	    new_code = GTU;
	    break;

	  case LE:
	    new_code = LT;
	    break;

	  case LEU:
	    new_code = LTU;
	    break;

	  default:
	    gcc_unreachable ();
	  }

	ior_code = optab_handler (ior_optab, dmode);
	if (ior_code == CODE_FOR_nothing)
	  return NULL_RTX;

	c_rtx = rs6000_emit_vector_compare (new_code, op0, op1, dmode);
	if (!c_rtx)
	  return NULL_RTX;

	eq_rtx = rs6000_emit_vector_compare (EQ, op0, op1, dmode);
	if (!eq_rtx)
	  return NULL_RTX;

	mask = gen_reg_rtx (dmode);
	emit_insn (GEN_FCN (ior_code) (mask, c_rtx, eq_rtx));
	return mask;
      }
      break;
    default:
      return NULL_RTX;
    }

  if (try_again)
    {
      if (swap_operands)
	std::swap (op0, op1);

      mask = rs6000_emit_vector_compare_inner (rcode, op0, op1);
      if (mask)
	return mask;
    }

  /* You only get two chances.  */
  return NULL_RTX;
}

/* Emit vector conditional expression.  DEST is destination. OP_TRUE and
   OP_FALSE are two VEC_COND_EXPR operands.  CC_OP0 and CC_OP1 are the two
   operands for the relation operation COND.  */

int
rs6000_emit_vector_cond_expr (rtx dest, rtx op_true, rtx op_false,
			      rtx cond, rtx cc_op0, rtx cc_op1)
{
  machine_mode dest_mode = GET_MODE (dest);
  machine_mode mask_mode = GET_MODE (cc_op0);
  enum rtx_code rcode = GET_CODE (cond);
  machine_mode cc_mode = CCmode;
  rtx mask;
  rtx cond2;
  bool invert_move = false;

  if (VECTOR_UNIT_NONE_P (dest_mode))
    return 0;

  gcc_assert (GET_MODE_SIZE (dest_mode) == GET_MODE_SIZE (mask_mode)
	      && GET_MODE_NUNITS (dest_mode) == GET_MODE_NUNITS (mask_mode));

  switch (rcode)
    {
      /* Swap operands if we can, and fall back to doing the operation as
	 specified, and doing a NOR to invert the test.  */
    case NE:
    case UNLE:
    case UNLT:
    case UNGE:
    case UNGT:
      /* Invert condition and try again.
	 e.g., A  = (B != C) ? D : E becomes A = (B == C) ? E : D.  */
      invert_move = true;
      rcode = reverse_condition_maybe_unordered (rcode);
      if (rcode == UNKNOWN)
	return 0;
      break;

    case GE:
    case LE:
      if (GET_MODE_CLASS (mask_mode) == MODE_VECTOR_INT)
	{
	  /* Invert condition to avoid compound test.  */
	  invert_move = true;
	  rcode = reverse_condition (rcode);
	}
      break;

    case GTU:
    case GEU:
    case LTU:
    case LEU:
      /* Mark unsigned tests with CCUNSmode.  */
      cc_mode = CCUNSmode;

      /* Invert condition to avoid compound test if necessary.  */
      if (rcode == GEU || rcode == LEU)
	{
	  invert_move = true;
	  rcode = reverse_condition (rcode);
	}
      break;

    default:
      break;
    }

  /* Get the vector mask for the given relational operations.  */
  mask = rs6000_emit_vector_compare (rcode, cc_op0, cc_op1, mask_mode);

  if (!mask)
    return 0;

  if (invert_move)
    std::swap (op_true, op_false);

  /* Optimize vec1 == vec2, to know the mask generates -1/0.  */
  if (GET_MODE_CLASS (dest_mode) == MODE_VECTOR_INT
      && (GET_CODE (op_true) == CONST_VECTOR
	  || GET_CODE (op_false) == CONST_VECTOR))
    {
      rtx constant_0 = CONST0_RTX (dest_mode);
      rtx constant_m1 = CONSTM1_RTX (dest_mode);

      if (op_true == constant_m1 && op_false == constant_0)
	{
	  emit_move_insn (dest, mask);
	  return 1;
	}

      else if (op_true == constant_0 && op_false == constant_m1)
	{
	  emit_insn (gen_rtx_SET (dest, gen_rtx_NOT (dest_mode, mask)));
	  return 1;
	}

      /* If we can't use the vector comparison directly, perhaps we can use
	 the mask for the true or false fields, instead of loading up a
	 constant.  */
      if (op_true == constant_m1)
	op_true = mask;

      if (op_false == constant_0)
	op_false = mask;
    }

  if (!REG_P (op_true) && !SUBREG_P (op_true))
    op_true = force_reg (dest_mode, op_true);

  if (!REG_P (op_false) && !SUBREG_P (op_false))
    op_false = force_reg (dest_mode, op_false);

  cond2 = gen_rtx_fmt_ee (NE, cc_mode, gen_lowpart (dest_mode, mask),
			  CONST0_RTX (dest_mode));
  emit_insn (gen_rtx_SET (dest,
			  gen_rtx_IF_THEN_ELSE (dest_mode,
						cond2,
						op_true,
						op_false)));
  return 1;
}

/* ISA 3.0 (power9) minmax subcase to emit a XSMAXCDP or XSMINCDP instruction
   for SF/DF scalars.  Move TRUE_COND to DEST if OP of the operands of the last
   comparison is nonzero/true, FALSE_COND if it is zero/false.  Return 0 if the
   hardware has no such operation.  */

static int
rs6000_emit_p9_fp_minmax (rtx dest, rtx op, rtx true_cond, rtx false_cond)
{
  enum rtx_code code = GET_CODE (op);
  rtx op0 = XEXP (op, 0);
  rtx op1 = XEXP (op, 1);
  machine_mode compare_mode = GET_MODE (op0);
  machine_mode result_mode = GET_MODE (dest);
  bool max_p = false;

  if (result_mode != compare_mode)
    return 0;

  if (code == GE || code == GT)
    max_p = true;
  else if (code == LE || code == LT)
    max_p = false;
  else
    return 0;

  if (rtx_equal_p (op0, true_cond) && rtx_equal_p (op1, false_cond))
    ;

  /* Only when NaNs and signed-zeros are not in effect, smax could be
     used for `op0 < op1 ? op1 : op0`, and smin could be used for
     `op0 > op1 ? op1 : op0`.  */
  else if (rtx_equal_p (op1, true_cond) && rtx_equal_p (op0, false_cond)
	   && !HONOR_NANS (compare_mode) && !HONOR_SIGNED_ZEROS (compare_mode))
    max_p = !max_p;

  else
    return 0;

  rs6000_emit_minmax (dest, max_p ? SMAX : SMIN, op0, op1);
  return 1;
}

/* ISA 3.0 (power9) conditional move subcase to emit XSCMP{EQ,GE,GT,NE}DP and
   XXSEL instructions for SF/DF scalars.  Move TRUE_COND to DEST if OP of the
   operands of the last comparison is nonzero/true, FALSE_COND if it is
   zero/false.  Return 0 if the hardware has no such operation.  */

static int
rs6000_emit_p9_fp_cmove (rtx dest, rtx op, rtx true_cond, rtx false_cond)
{
  enum rtx_code code = GET_CODE (op);
  rtx op0 = XEXP (op, 0);
  rtx op1 = XEXP (op, 1);
  machine_mode result_mode = GET_MODE (dest);
  rtx compare_rtx;
  rtx cmove_rtx;
  rtx clobber_rtx;

  if (!can_create_pseudo_p ())
    return 0;

  switch (code)
    {
    case EQ:
    case GE:
    case GT:
      break;

    case NE:
    case LT:
    case LE:
      code = swap_condition (code);
      std::swap (op0, op1);
      break;

    default:
      return 0;
    }

  /* Generate:	[(parallel [(set (dest)
				 (if_then_else (op (cmp1) (cmp2))
					       (true)
					       (false)))
			    (clobber (scratch))])].  */

  compare_rtx = gen_rtx_fmt_ee (code, CCFPmode, op0, op1);
  cmove_rtx = gen_rtx_SET (dest,
			   gen_rtx_IF_THEN_ELSE (result_mode,
						 compare_rtx,
						 true_cond,
						 false_cond));

  clobber_rtx = gen_rtx_CLOBBER (VOIDmode, gen_rtx_SCRATCH (V2DImode));
  emit_insn (gen_rtx_PARALLEL (VOIDmode,
			       gen_rtvec (2, cmove_rtx, clobber_rtx)));

  return 1;
}

/* Emit a conditional move: move TRUE_COND to DEST if OP of the
   operands of the last comparison is nonzero/true, FALSE_COND if it
   is zero/false.  Return 0 if the hardware has no such operation.  */

int
rs6000_emit_cmove (rtx dest, rtx op, rtx true_cond, rtx false_cond)
{
  enum rtx_code code = GET_CODE (op);
  rtx op0 = XEXP (op, 0);
  rtx op1 = XEXP (op, 1);
  machine_mode compare_mode = GET_MODE (op0);
  machine_mode result_mode = GET_MODE (dest);
  rtx temp;
  bool is_against_zero;

  /* These modes should always match.  */
  if (GET_MODE (op1) != compare_mode
      /* In the isel case however, we can use a compare immediate, so
	 op1 may be a small constant.  */
      && (!TARGET_ISEL || !short_cint_operand (op1, VOIDmode)))
    return 0;
  if (GET_MODE (true_cond) != result_mode)
    return 0;
  if (GET_MODE (false_cond) != result_mode)
    return 0;

  /* See if we can use the ISA 3.0 (power9) min/max/compare functions.  */
  if (TARGET_P9_MINMAX
      && (compare_mode == SFmode || compare_mode == DFmode)
      && (result_mode == SFmode || result_mode == DFmode))
    {
      if (rs6000_emit_p9_fp_minmax (dest, op, true_cond, false_cond))
	return 1;

      if (rs6000_emit_p9_fp_cmove (dest, op, true_cond, false_cond))
	return 1;
    }

  /* Don't allow using floating point comparisons for integer results for
     now.  */
  if (FLOAT_MODE_P (compare_mode) && !FLOAT_MODE_P (result_mode))
    return 0;

  /* First, work out if the hardware can do this at all, or
     if it's too slow....  */
  if (!FLOAT_MODE_P (compare_mode))
    {
      if (TARGET_ISEL)
	return rs6000_emit_int_cmove (dest, op, true_cond, false_cond);
      return 0;
    }

  is_against_zero = op1 == CONST0_RTX (compare_mode);

  /* A floating-point subtract might overflow, underflow, or produce
     an inexact result, thus changing the floating-point flags, so it
     can't be generated if we care about that.  It's safe if one side
     of the construct is zero, since then no subtract will be
     generated.  */
  if (SCALAR_FLOAT_MODE_P (compare_mode)
      && flag_trapping_math && ! is_against_zero)
    return 0;

  /* Eliminate half of the comparisons by switching operands, this
     makes the remaining code simpler.  */
  if (code == UNLT || code == UNGT || code == UNORDERED || code == NE
      || code == LTGT || code == LT || code == UNLE)
    {
      code = reverse_condition_maybe_unordered (code);
      temp = true_cond;
      true_cond = false_cond;
      false_cond = temp;
    }

  /* UNEQ and LTGT take four instructions for a comparison with zero,
     it'll probably be faster to use a branch here too.  */
  if (code == UNEQ && HONOR_NANS (compare_mode))
    return 0;

  /* We're going to try to implement comparisons by performing
     a subtract, then comparing against zero.  Unfortunately,
     Inf - Inf is NaN which is not zero, and so if we don't
     know that the operand is finite and the comparison
     would treat EQ different to UNORDERED, we can't do it.  */
  if (HONOR_INFINITIES (compare_mode)
      && code != GT && code != UNGE
      && (!CONST_DOUBLE_P (op1)
	  || real_isinf (CONST_DOUBLE_REAL_VALUE (op1)))
      /* Constructs of the form (a OP b ? a : b) are safe.  */
      && ((! rtx_equal_p (op0, false_cond) && ! rtx_equal_p (op1, false_cond))
	  || (! rtx_equal_p (op0, true_cond)
	      && ! rtx_equal_p (op1, true_cond))))
    return 0;

  /* At this point we know we can use fsel.  */

  /* Don't allow compare_mode other than SFmode or DFmode, for others there
     is no fsel instruction.  */
  if (compare_mode != SFmode && compare_mode != DFmode)
    return 0;

  /* Reduce the comparison to a comparison against zero.  */
  if (! is_against_zero)
    {
      temp = gen_reg_rtx (compare_mode);
      emit_insn (gen_rtx_SET (temp, gen_rtx_MINUS (compare_mode, op0, op1)));
      op0 = temp;
      op1 = CONST0_RTX (compare_mode);
    }

  /* If we don't care about NaNs we can reduce some of the comparisons
     down to faster ones.  */
  if (! HONOR_NANS (compare_mode))
    switch (code)
      {
      case GT:
	code = LE;
	temp = true_cond;
	true_cond = false_cond;
	false_cond = temp;
	break;
      case UNGE:
	code = GE;
	break;
      case UNEQ:
	code = EQ;
	break;
      default:
	break;
      }

  /* Now, reduce everything down to a GE.  */
  switch (code)
    {
    case GE:
      break;

    case LE:
      temp = gen_reg_rtx (compare_mode);
      emit_insn (gen_rtx_SET (temp, gen_rtx_NEG (compare_mode, op0)));
      op0 = temp;
      break;

    case ORDERED:
      temp = gen_reg_rtx (compare_mode);
      emit_insn (gen_rtx_SET (temp, gen_rtx_ABS (compare_mode, op0)));
      op0 = temp;
      break;

    case EQ:
      temp = gen_reg_rtx (compare_mode);
      emit_insn (gen_rtx_SET (temp,
			      gen_rtx_NEG (compare_mode,
					   gen_rtx_ABS (compare_mode, op0))));
      op0 = temp;
      break;

    case UNGE:
      /* a UNGE 0 <-> (a GE 0 || -a UNLT 0) */
      temp = gen_reg_rtx (result_mode);
      emit_insn (gen_rtx_SET (temp,
			      gen_rtx_IF_THEN_ELSE (result_mode,
						    gen_rtx_GE (VOIDmode,
								op0, op1),
						    true_cond, false_cond)));
      false_cond = true_cond;
      true_cond = temp;

      temp = gen_reg_rtx (compare_mode);
      emit_insn (gen_rtx_SET (temp, gen_rtx_NEG (compare_mode, op0)));
      op0 = temp;
      break;

    case GT:
      /* a GT 0 <-> (a GE 0 && -a UNLT 0) */
      temp = gen_reg_rtx (result_mode);
      emit_insn (gen_rtx_SET (temp,
			      gen_rtx_IF_THEN_ELSE (result_mode,
						    gen_rtx_GE (VOIDmode,
								op0, op1),
						    true_cond, false_cond)));
      true_cond = false_cond;
      false_cond = temp;

      temp = gen_reg_rtx (compare_mode);
      emit_insn (gen_rtx_SET (temp, gen_rtx_NEG (compare_mode, op0)));
      op0 = temp;
      break;

    default:
      gcc_unreachable ();
    }

  emit_insn (gen_rtx_SET (dest,
			  gen_rtx_IF_THEN_ELSE (result_mode,
						gen_rtx_GE (VOIDmode,
							    op0, op1),
						true_cond, false_cond)));
  return 1;
}

/* Same as above, but for ints (isel).  */

int
rs6000_emit_int_cmove (rtx dest, rtx op, rtx true_cond, rtx false_cond)
{
  rtx condition_rtx, cr;
  machine_mode mode = GET_MODE (dest);
  enum rtx_code cond_code;
  rtx (*isel_func) (rtx, rtx, rtx, rtx, rtx);
  bool signedp;

  if (mode != SImode && (!TARGET_POWERPC64 || mode != DImode))
    return 0;

  /* We still have to do the compare, because isel doesn't do a
     compare, it just looks at the CRx bits set by a previous compare
     instruction.  */
  condition_rtx = rs6000_generate_compare (op, mode);
  cond_code = GET_CODE (condition_rtx);
  cr = XEXP (condition_rtx, 0);
  signedp = GET_MODE (cr) == CCmode;

  isel_func = (mode == SImode
	       ? (signedp ? gen_isel_signed_si : gen_isel_unsigned_si)
	       : (signedp ? gen_isel_signed_di : gen_isel_unsigned_di));

  switch (cond_code)
    {
    case LT: case GT: case LTU: case GTU: case EQ:
      /* isel handles these directly.  */
      break;

    default:
      /* We need to swap the sense of the comparison.  */
      {
	std::swap (false_cond, true_cond);
	PUT_CODE (condition_rtx, reverse_condition (cond_code));
      }
      break;
    }

  false_cond = force_reg (mode, false_cond);
  if (true_cond != const0_rtx)
    true_cond = force_reg (mode, true_cond);

  emit_insn (isel_func (dest, condition_rtx, true_cond, false_cond, cr));

  return 1;
}

void
rs6000_emit_minmax (rtx dest, enum rtx_code code, rtx op0, rtx op1)
{
  machine_mode mode = GET_MODE (op0);
  enum rtx_code c;
  rtx target;

  /* VSX/altivec have direct min/max insns.  */
  if ((code == SMAX || code == SMIN)
      && (VECTOR_UNIT_ALTIVEC_OR_VSX_P (mode)
	  || (mode == SFmode && VECTOR_UNIT_VSX_P (DFmode))))
    {
      emit_insn (gen_rtx_SET (dest, gen_rtx_fmt_ee (code, mode, op0, op1)));
      return;
    }

  if (code == SMAX || code == SMIN)
    c = GE;
  else
    c = GEU;

  if (code == SMAX || code == UMAX)
    target = emit_conditional_move (dest, c, op0, op1, mode,
				    op0, op1, mode, 0);
  else
    target = emit_conditional_move (dest, c, op0, op1, mode,
				    op1, op0, mode, 0);
  gcc_assert (target);
  if (target != dest)
    emit_move_insn (dest, target);
}

/* A subroutine of the atomic operation splitters.  Jump to LABEL if
   COND is true.  Mark the jump as unlikely to be taken.  */

static void
emit_unlikely_jump (rtx cond, rtx label)
{
  rtx x = gen_rtx_IF_THEN_ELSE (VOIDmode, cond, label, pc_rtx);
  rtx_insn *insn = emit_jump_insn (gen_rtx_SET (pc_rtx, x));
  add_reg_br_prob_note (insn, profile_probability::very_unlikely ());
}

/* A subroutine of the atomic operation splitters.  Emit a load-locked
   instruction in MODE.  For QI/HImode, possibly use a pattern than includes
   the zero_extend operation.  */

static void
emit_load_locked (machine_mode mode, rtx reg, rtx mem)
{
  rtx (*fn) (rtx, rtx) = NULL;

  switch (mode)
    {
    case E_QImode:
      fn = gen_load_lockedqi;
      break;
    case E_HImode:
      fn = gen_load_lockedhi;
      break;
    case E_SImode:
      if (GET_MODE (mem) == QImode)
	fn = gen_load_lockedqi_si;
      else if (GET_MODE (mem) == HImode)
	fn = gen_load_lockedhi_si;
      else
	fn = gen_load_lockedsi;
      break;
    case E_DImode:
      fn = gen_load_lockeddi;
      break;
    case E_TImode:
      fn = gen_load_lockedti;
      break;
    default:
      gcc_unreachable ();
    }
  emit_insn (fn (reg, mem));
}

/* A subroutine of the atomic operation splitters.  Emit a store-conditional
   instruction in MODE.  */

static void
emit_store_conditional (machine_mode mode, rtx res, rtx mem, rtx val)
{
  rtx (*fn) (rtx, rtx, rtx) = NULL;

  switch (mode)
    {
    case E_QImode:
      fn = gen_store_conditionalqi;
      break;
    case E_HImode:
      fn = gen_store_conditionalhi;
      break;
    case E_SImode:
      fn = gen_store_conditionalsi;
      break;
    case E_DImode:
      fn = gen_store_conditionaldi;
      break;
    case E_TImode:
      fn = gen_store_conditionalti;
      break;
    default:
      gcc_unreachable ();
    }

  /* Emit sync before stwcx. to address PPC405 Erratum.  */
  if (PPC405_ERRATUM77)
    emit_insn (gen_hwsync ());

  emit_insn (fn (res, mem, val));
}

/* Expand barriers before and after a load_locked/store_cond sequence.  */

static rtx
rs6000_pre_atomic_barrier (rtx mem, enum memmodel model)
{
  rtx addr = XEXP (mem, 0);

  if (!legitimate_indirect_address_p (addr, reload_completed)
      && !legitimate_indexed_address_p (addr, reload_completed))
    {
      addr = force_reg (Pmode, addr);
      mem = replace_equiv_address_nv (mem, addr);
    }

  switch (model)
    {
    case MEMMODEL_RELAXED:
    case MEMMODEL_CONSUME:
    case MEMMODEL_ACQUIRE:
      break;
    case MEMMODEL_RELEASE:
    case MEMMODEL_ACQ_REL:
      emit_insn (gen_lwsync ());
      break;
    case MEMMODEL_SEQ_CST:
      emit_insn (gen_hwsync ());
      break;
    default:
      gcc_unreachable ();
    }
  return mem;
}

static void
rs6000_post_atomic_barrier (enum memmodel model)
{
  switch (model)
    {
    case MEMMODEL_RELAXED:
    case MEMMODEL_CONSUME:
    case MEMMODEL_RELEASE:
      break;
    case MEMMODEL_ACQUIRE:
    case MEMMODEL_ACQ_REL:
    case MEMMODEL_SEQ_CST:
      emit_insn (gen_isync ());
      break;
    default:
      gcc_unreachable ();
    }
}

/* A subroutine of the various atomic expanders.  For sub-word operations,
   we must adjust things to operate on SImode.  Given the original MEM,
   return a new aligned memory.  Also build and return the quantities by
   which to shift and mask.  */

static rtx
rs6000_adjust_atomic_subword (rtx orig_mem, rtx *pshift, rtx *pmask)
{
  rtx addr, align, shift, mask, mem;
  HOST_WIDE_INT shift_mask;
  machine_mode mode = GET_MODE (orig_mem);

  /* For smaller modes, we have to implement this via SImode.  */
  shift_mask = (mode == QImode ? 0x18 : 0x10);

  addr = XEXP (orig_mem, 0);
  addr = force_reg (GET_MODE (addr), addr);

  /* Aligned memory containing subword.  Generate a new memory.  We
     do not want any of the existing MEM_ATTR data, as we're now
     accessing memory outside the original object.  */
  align = expand_simple_binop (Pmode, AND, addr, GEN_INT (-4),
			       NULL_RTX, 1, OPTAB_LIB_WIDEN);
  mem = gen_rtx_MEM (SImode, align);
  MEM_VOLATILE_P (mem) = MEM_VOLATILE_P (orig_mem);
  if (MEM_ALIAS_SET (orig_mem) == ALIAS_SET_MEMORY_BARRIER)
    set_mem_alias_set (mem, ALIAS_SET_MEMORY_BARRIER);

  /* Shift amount for subword relative to aligned word.  */
  shift = gen_reg_rtx (SImode);
  addr = gen_lowpart (SImode, addr);
  rtx tmp = gen_reg_rtx (SImode);
  emit_insn (gen_ashlsi3 (tmp, addr, GEN_INT (3)));
  emit_insn (gen_andsi3 (shift, tmp, GEN_INT (shift_mask)));
  if (BYTES_BIG_ENDIAN)
    shift = expand_simple_binop (SImode, XOR, shift, GEN_INT (shift_mask),
			         shift, 1, OPTAB_LIB_WIDEN);
  *pshift = shift;

  /* Mask for insertion.  */
  mask = expand_simple_binop (SImode, ASHIFT, GEN_INT (GET_MODE_MASK (mode)),
			      shift, NULL_RTX, 1, OPTAB_LIB_WIDEN);
  *pmask = mask;

  return mem;
}

/* A subroutine of the various atomic expanders.  For sub-word operands,
   combine OLDVAL and NEWVAL via MASK.  Returns a new pseduo.  */

static rtx
rs6000_mask_atomic_subword (rtx oldval, rtx newval, rtx mask)
{
  rtx x;

  x = gen_reg_rtx (SImode);
  emit_insn (gen_rtx_SET (x, gen_rtx_AND (SImode,
					  gen_rtx_NOT (SImode, mask),
					  oldval)));

  x = expand_simple_binop (SImode, IOR, newval, x, x, 1, OPTAB_LIB_WIDEN);

  return x;
}

/* A subroutine of the various atomic expanders.  For sub-word operands,
   extract WIDE to NARROW via SHIFT.  */

static void
rs6000_finish_atomic_subword (rtx narrow, rtx wide, rtx shift)
{
  wide = expand_simple_binop (SImode, LSHIFTRT, wide, shift,
			      wide, 1, OPTAB_LIB_WIDEN);
  emit_move_insn (narrow, gen_lowpart (GET_MODE (narrow), wide));
}

/* Expand an atomic compare and swap operation.  */

void
rs6000_expand_atomic_compare_and_swap (rtx operands[])
{
  rtx boolval, retval, mem, oldval, newval, cond;
  rtx label1, label2, x, mask, shift;
  machine_mode mode, orig_mode;
  enum memmodel mod_s, mod_f;
  bool is_weak;

  boolval = operands[0];
  retval = operands[1];
  mem = operands[2];
  oldval = operands[3];
  newval = operands[4];
  is_weak = (INTVAL (operands[5]) != 0);
  mod_s = memmodel_base (INTVAL (operands[6]));
  mod_f = memmodel_base (INTVAL (operands[7]));
  orig_mode = mode = GET_MODE (mem);

  mask = shift = NULL_RTX;
  if (mode == QImode || mode == HImode)
    {
      /* Before power8, we didn't have access to lbarx/lharx, so generate a
	 lwarx and shift/mask operations.  With power8, we need to do the
	 comparison in SImode, but the store is still done in QI/HImode.  */
      oldval = convert_modes (SImode, mode, oldval, 1);

      if (!TARGET_SYNC_HI_QI)
	{
	  mem = rs6000_adjust_atomic_subword (mem, &shift, &mask);

	  /* Shift and mask OLDVAL into position with the word.  */
	  oldval = expand_simple_binop (SImode, ASHIFT, oldval, shift,
					NULL_RTX, 1, OPTAB_LIB_WIDEN);

	  /* Shift and mask NEWVAL into position within the word.  */
	  newval = convert_modes (SImode, mode, newval, 1);
	  newval = expand_simple_binop (SImode, ASHIFT, newval, shift,
					NULL_RTX, 1, OPTAB_LIB_WIDEN);
	}

      /* Prepare to adjust the return value.  */
      retval = gen_reg_rtx (SImode);
      mode = SImode;
    }
  else if (reg_overlap_mentioned_p (retval, oldval))
    oldval = copy_to_reg (oldval);

  if (mode != TImode && !reg_or_short_operand (oldval, mode))
    oldval = copy_to_mode_reg (mode, oldval);

  if (reg_overlap_mentioned_p (retval, newval))
    newval = copy_to_reg (newval);

  mem = rs6000_pre_atomic_barrier (mem, mod_s);

  label1 = NULL_RTX;
  if (!is_weak)
    {
      label1 = gen_rtx_LABEL_REF (VOIDmode, gen_label_rtx ());
      emit_label (XEXP (label1, 0));
    }
  label2 = gen_rtx_LABEL_REF (VOIDmode, gen_label_rtx ());

  emit_load_locked (mode, retval, mem);

  x = retval;
  if (mask)
    x = expand_simple_binop (SImode, AND, retval, mask,
			     NULL_RTX, 1, OPTAB_LIB_WIDEN);

  cond = gen_reg_rtx (CCmode);
  /* If we have TImode, synthesize a comparison.  */
  if (mode != TImode)
    x = gen_rtx_COMPARE (CCmode, x, oldval);
  else
    {
      rtx xor1_result = gen_reg_rtx (DImode);
      rtx xor2_result = gen_reg_rtx (DImode);
      rtx or_result = gen_reg_rtx (DImode);
      rtx new_word0 = simplify_gen_subreg (DImode, x, TImode, 0);
      rtx new_word1 = simplify_gen_subreg (DImode, x, TImode, 8);
      rtx old_word0 = simplify_gen_subreg (DImode, oldval, TImode, 0);
      rtx old_word1 = simplify_gen_subreg (DImode, oldval, TImode, 8);

      emit_insn (gen_xordi3 (xor1_result, new_word0, old_word0));
      emit_insn (gen_xordi3 (xor2_result, new_word1, old_word1));
      emit_insn (gen_iordi3 (or_result, xor1_result, xor2_result));
      x = gen_rtx_COMPARE (CCmode, or_result, const0_rtx);
    }

  emit_insn (gen_rtx_SET (cond, x));

  x = gen_rtx_NE (VOIDmode, cond, const0_rtx);
  emit_unlikely_jump (x, label2);

  x = newval;
  if (mask)
    x = rs6000_mask_atomic_subword (retval, newval, mask);

  emit_store_conditional (orig_mode, cond, mem, x);

  if (!is_weak)
    {
      x = gen_rtx_NE (VOIDmode, cond, const0_rtx);
      emit_unlikely_jump (x, label1);
    }

  if (!is_mm_relaxed (mod_f))
    emit_label (XEXP (label2, 0));

  rs6000_post_atomic_barrier (mod_s);

  if (is_mm_relaxed (mod_f))
    emit_label (XEXP (label2, 0));

  if (shift)
    rs6000_finish_atomic_subword (operands[1], retval, shift);
  else if (mode != GET_MODE (operands[1]))
    convert_move (operands[1], retval, 1);

  /* In all cases, CR0 contains EQ on success, and NE on failure.  */
  x = gen_rtx_EQ (SImode, cond, const0_rtx);
  emit_insn (gen_rtx_SET (boolval, x));
}

/* Expand an atomic exchange operation.  */

void
rs6000_expand_atomic_exchange (rtx operands[])
{
  rtx retval, mem, val, cond;
  machine_mode mode;
  enum memmodel model;
  rtx label, x, mask, shift;

  retval = operands[0];
  mem = operands[1];
  val = operands[2];
  model = memmodel_base (INTVAL (operands[3]));
  mode = GET_MODE (mem);

  mask = shift = NULL_RTX;
  if (!TARGET_SYNC_HI_QI && (mode == QImode || mode == HImode))
    {
      mem = rs6000_adjust_atomic_subword (mem, &shift, &mask);

      /* Shift and mask VAL into position with the word.  */
      val = convert_modes (SImode, mode, val, 1);
      val = expand_simple_binop (SImode, ASHIFT, val, shift,
				 NULL_RTX, 1, OPTAB_LIB_WIDEN);

      /* Prepare to adjust the return value.  */
      retval = gen_reg_rtx (SImode);
      mode = SImode;
    }

  mem = rs6000_pre_atomic_barrier (mem, model);

  label = gen_rtx_LABEL_REF (VOIDmode, gen_label_rtx ());
  emit_label (XEXP (label, 0));

  emit_load_locked (mode, retval, mem);

  x = val;
  if (mask)
    x = rs6000_mask_atomic_subword (retval, val, mask);

  cond = gen_reg_rtx (CCmode);
  emit_store_conditional (mode, cond, mem, x);

  x = gen_rtx_NE (VOIDmode, cond, const0_rtx);
  emit_unlikely_jump (x, label);

  rs6000_post_atomic_barrier (model);

  if (shift)
    rs6000_finish_atomic_subword (operands[0], retval, shift);
}

/* Expand an atomic fetch-and-operate pattern.  CODE is the binary operation
   to perform.  MEM is the memory on which to operate.  VAL is the second
   operand of the binary operator.  BEFORE and AFTER are optional locations to
   return the value of MEM either before of after the operation.  MODEL_RTX
   is a CONST_INT containing the memory model to use.  */

void
rs6000_expand_atomic_op (enum rtx_code code, rtx mem, rtx val,
			 rtx orig_before, rtx orig_after, rtx model_rtx)
{
  enum memmodel model = memmodel_base (INTVAL (model_rtx));
  machine_mode mode = GET_MODE (mem);
  machine_mode store_mode = mode;
  rtx label, x, cond, mask, shift;
  rtx before = orig_before, after = orig_after;

  mask = shift = NULL_RTX;
  /* On power8, we want to use SImode for the operation.  On previous systems,
     use the operation in a subword and shift/mask to get the proper byte or
     halfword.  */
  if (mode == QImode || mode == HImode)
    {
      if (TARGET_SYNC_HI_QI)
	{
	  val = convert_modes (SImode, mode, val, 1);

	  /* Prepare to adjust the return value.  */
	  before = gen_reg_rtx (SImode);
	  if (after)
	    after = gen_reg_rtx (SImode);
	  mode = SImode;
	}
      else
	{
	  mem = rs6000_adjust_atomic_subword (mem, &shift, &mask);

	  /* Shift and mask VAL into position with the word.  */
	  val = convert_modes (SImode, mode, val, 1);
	  val = expand_simple_binop (SImode, ASHIFT, val, shift,
				     NULL_RTX, 1, OPTAB_LIB_WIDEN);

	  switch (code)
	    {
	    case IOR:
	    case XOR:
	      /* We've already zero-extended VAL.  That is sufficient to
		 make certain that it does not affect other bits.  */
	      mask = NULL;
	      break;

	    case AND:
	      /* If we make certain that all of the other bits in VAL are
		 set, that will be sufficient to not affect other bits.  */
	      x = gen_rtx_NOT (SImode, mask);
	      x = gen_rtx_IOR (SImode, x, val);
	      emit_insn (gen_rtx_SET (val, x));
	      mask = NULL;
	      break;

	    case NOT:
	    case PLUS:
	    case MINUS:
	      /* These will all affect bits outside the field and need
		 adjustment via MASK within the loop.  */
	      break;

	    default:
	      gcc_unreachable ();
	    }

	  /* Prepare to adjust the return value.  */
	  before = gen_reg_rtx (SImode);
	  if (after)
	    after = gen_reg_rtx (SImode);
	  store_mode = mode = SImode;
	}
    }

  mem = rs6000_pre_atomic_barrier (mem, model);

  label = gen_label_rtx ();
  emit_label (label);
  label = gen_rtx_LABEL_REF (VOIDmode, label);

  if (before == NULL_RTX)
    before = gen_reg_rtx (mode);

  emit_load_locked (mode, before, mem);

  if (code == NOT)
    {
      x = expand_simple_binop (mode, AND, before, val,
			       NULL_RTX, 1, OPTAB_LIB_WIDEN);
      after = expand_simple_unop (mode, NOT, x, after, 1);
    }
  else
    {
      after = expand_simple_binop (mode, code, before, val,
				   after, 1, OPTAB_LIB_WIDEN);
    }

  x = after;
  if (mask)
    {
      x = expand_simple_binop (SImode, AND, after, mask,
			       NULL_RTX, 1, OPTAB_LIB_WIDEN);
      x = rs6000_mask_atomic_subword (before, x, mask);
    }
  else if (store_mode != mode)
    x = convert_modes (store_mode, mode, x, 1);

  cond = gen_reg_rtx (CCmode);
  emit_store_conditional (store_mode, cond, mem, x);

  x = gen_rtx_NE (VOIDmode, cond, const0_rtx);
  emit_unlikely_jump (x, label);

  rs6000_post_atomic_barrier (model);

  if (shift)
    {
      /* QImode/HImode on machines without lbarx/lharx where we do a lwarx and
	 then do the calcuations in a SImode register.  */
      if (orig_before)
	rs6000_finish_atomic_subword (orig_before, before, shift);
      if (orig_after)
	rs6000_finish_atomic_subword (orig_after, after, shift);
    }
  else if (store_mode != mode)
    {
      /* QImode/HImode on machines with lbarx/lharx where we do the native
	 operation and then do the calcuations in a SImode register.  */
      if (orig_before)
	convert_move (orig_before, before, 1);
      if (orig_after)
	convert_move (orig_after, after, 1);
    }
  else if (orig_after && after != orig_after)
    emit_move_insn (orig_after, after);
}

/* Emit instructions to move SRC to DST.  Called by splitters for
   multi-register moves.  It will emit at most one instruction for
   each register that is accessed; that is, it won't emit li/lis pairs
   (or equivalent for 64-bit code).  One of SRC or DST must be a hard
   register.  */

void
rs6000_split_multireg_move (rtx dst, rtx src)
{
  /* The register number of the first register being moved.  */
  int reg;
  /* The mode that is to be moved.  */
  machine_mode mode;
  /* The mode that the move is being done in, and its size.  */
  machine_mode reg_mode;
  int reg_mode_size;
  /* The number of registers that will be moved.  */
  int nregs;

  reg = REG_P (dst) ? REGNO (dst) : REGNO (src);
  mode = GET_MODE (dst);
  nregs = hard_regno_nregs (reg, mode);

  /* If we have a vector quad register for MMA, and this is a load or store,
     see if we can use vector paired load/stores.  */
  if (mode == PXImode && TARGET_MMA
      && (MEM_P (dst) || MEM_P (src)))
    {
      reg_mode = POImode;
      nregs /= 2;
    }
  /* If we have a vector pair/quad mode, split it into two/four separate
     vectors.  */
  else if (mode == POImode || mode == PXImode)
    reg_mode = V1TImode;
  else if (FP_REGNO_P (reg))
    reg_mode = DECIMAL_FLOAT_MODE_P (mode) ? DDmode :
	(TARGET_HARD_FLOAT ? DFmode : SFmode);
  else if (ALTIVEC_REGNO_P (reg))
    reg_mode = V16QImode;
  else
    reg_mode = word_mode;
  reg_mode_size = GET_MODE_SIZE (reg_mode);

  gcc_assert (reg_mode_size * nregs == GET_MODE_SIZE (mode));

  /* TDmode residing in FP registers is special, since the ISA requires that
     the lower-numbered word of a register pair is always the most significant
     word, even in little-endian mode.  This does not match the usual subreg
     semantics, so we cannnot use simplify_gen_subreg in those cases.  Access
     the appropriate constituent registers "by hand" in little-endian mode.

     Note we do not need to check for destructive overlap here since TDmode
     can only reside in even/odd register pairs.  */
  if (FP_REGNO_P (reg) && DECIMAL_FLOAT_MODE_P (mode) && !BYTES_BIG_ENDIAN)
    {
      rtx p_src, p_dst;
      int i;

      for (i = 0; i < nregs; i++)
	{
	  if (REG_P (src) && FP_REGNO_P (REGNO (src)))
	    p_src = gen_rtx_REG (reg_mode, REGNO (src) + nregs - 1 - i);
	  else
	    p_src = simplify_gen_subreg (reg_mode, src, mode,
					 i * reg_mode_size);

	  if (REG_P (dst) && FP_REGNO_P (REGNO (dst)))
	    p_dst = gen_rtx_REG (reg_mode, REGNO (dst) + nregs - 1 - i);
	  else
	    p_dst = simplify_gen_subreg (reg_mode, dst, mode,
					 i * reg_mode_size);

	  emit_insn (gen_rtx_SET (p_dst, p_src));
	}

      return;
    }

  /* The __vector_pair and __vector_quad modes are multi-register modes,
     so if have to load or store the registers, we have to be careful to
     properly swap them if we're in little endian mode below.  This means
     the last register gets the first memory location.  */
  if (mode == POImode || mode == PXImode)
    {
      if (MEM_P (dst))
	{
	  unsigned offset = 0;
	  unsigned size = GET_MODE_SIZE (reg_mode);

	  /* If we are reading an accumulator register, we have to
	     deprime it before we can access it.  */
	  if (TARGET_MMA
	      && GET_MODE (src) == PXImode && FP_REGNO_P (REGNO (src)))
	    emit_insn (gen_mma_xxmfacc (src, src));

	  for (int i = 0; i < nregs; i++)
	    {
	      unsigned subreg = (WORDS_BIG_ENDIAN)
				  ? i * size : (nregs - 1 - i) * size;
	      rtx dst2 = adjust_address (dst, reg_mode, offset);
	      rtx src2 = simplify_gen_subreg (reg_mode, src, mode, subreg);
	      offset += size;
	      emit_insn (gen_rtx_SET (dst2, src2));
	    }

	  return;
	}

      if (MEM_P (src))
	{
	  unsigned offset = 0;
	  unsigned size = GET_MODE_SIZE (reg_mode);

	  for (int i = 0; i < nregs; i++)
	    {
	      unsigned subreg = (WORDS_BIG_ENDIAN)
				  ? i * size : (nregs - 1 - i) * size;
	      rtx dst2 = simplify_gen_subreg (reg_mode, dst, mode, subreg);
	      rtx src2 = adjust_address (src, reg_mode, offset);
	      offset += size;
	      emit_insn (gen_rtx_SET (dst2, src2));
	    }

	  /* If we are writing an accumulator register, we have to
	     prime it after we've written it.  */
	  if (TARGET_MMA
	      && GET_MODE (dst) == PXImode && FP_REGNO_P (REGNO (dst)))
	    emit_insn (gen_mma_xxmtacc (dst, dst));

	  return;
	}

      if (GET_CODE (src) == UNSPEC)
	{
	  gcc_assert (REG_P (dst)
		      && FP_REGNO_P (REGNO (dst))
		      && XINT (src, 1) == UNSPEC_MMA_ASSEMBLE_ACC);

	  reg_mode = GET_MODE (XVECEXP (src, 0, 0));
	  int nvecs = XVECLEN (src, 0);
	  for (int i = 0; i < nvecs; i++)
	    {
	      int index = WORDS_BIG_ENDIAN ? i : nvecs - 1 - i;
	      rtx dst_i = gen_rtx_REG (reg_mode, reg + index);
	      emit_insn (gen_rtx_SET (dst_i, XVECEXP (src, 0, i)));
	    }

	  /* We are writing an accumulator register, so we have to
	     prime it after we've written it.  */
	  emit_insn (gen_mma_xxmtacc (dst, dst));

	  return;
	}

      /* Register -> register moves can use common code.  */
    }

  if (REG_P (src) && REG_P (dst) && (REGNO (src) < REGNO (dst)))
    {
      /* If we are reading an accumulator register, we have to
	 deprime it before we can access it.  */
      if (TARGET_MMA
	  && GET_MODE (src) == PXImode && FP_REGNO_P (REGNO (src)))
	emit_insn (gen_mma_xxmfacc (src, src));

      /* Move register range backwards, if we might have destructive
	 overlap.  */
      int i;
      for (i = nregs - 1; i >= 0; i--)
	emit_insn (gen_rtx_SET (simplify_gen_subreg (reg_mode, dst, mode,
						     i * reg_mode_size),
				simplify_gen_subreg (reg_mode, src, mode,
						     i * reg_mode_size)));

      /* If we are writing an accumulator register, we have to
	 prime it after we've written it.  */
      if (TARGET_MMA
	  && GET_MODE (dst) == PXImode && FP_REGNO_P (REGNO (dst)))
	emit_insn (gen_mma_xxmtacc (dst, dst));
    }
  else
    {
      int i;
      int j = -1;
      bool used_update = false;
      rtx restore_basereg = NULL_RTX;

      if (MEM_P (src) && INT_REGNO_P (reg))
	{
	  rtx breg;

	  if (GET_CODE (XEXP (src, 0)) == PRE_INC
	      || GET_CODE (XEXP (src, 0)) == PRE_DEC)
	    {
	      rtx delta_rtx;
	      breg = XEXP (XEXP (src, 0), 0);
	      delta_rtx = (GET_CODE (XEXP (src, 0)) == PRE_INC
			   ? GEN_INT (GET_MODE_SIZE (GET_MODE (src)))
			   : GEN_INT (-GET_MODE_SIZE (GET_MODE (src))));
	      emit_insn (gen_add3_insn (breg, breg, delta_rtx));
	      src = replace_equiv_address (src, breg);
	    }
	  else if (! rs6000_offsettable_memref_p (src, reg_mode, true))
	    {
	      if (GET_CODE (XEXP (src, 0)) == PRE_MODIFY)
		{
		  rtx basereg = XEXP (XEXP (src, 0), 0);
		  if (TARGET_UPDATE)
		    {
		      rtx ndst = simplify_gen_subreg (reg_mode, dst, mode, 0);
		      emit_insn (gen_rtx_SET (ndst,
					      gen_rtx_MEM (reg_mode,
							   XEXP (src, 0))));
		      used_update = true;
		    }
		  else
		    emit_insn (gen_rtx_SET (basereg,
					    XEXP (XEXP (src, 0), 1)));
		  src = replace_equiv_address (src, basereg);
		}
	      else
		{
		  rtx basereg = gen_rtx_REG (Pmode, reg);
		  emit_insn (gen_rtx_SET (basereg, XEXP (src, 0)));
		  src = replace_equiv_address (src, basereg);
		}
	    }

	  breg = XEXP (src, 0);
	  if (GET_CODE (breg) == PLUS || GET_CODE (breg) == LO_SUM)
	    breg = XEXP (breg, 0);

	  /* If the base register we are using to address memory is
	     also a destination reg, then change that register last.  */
	  if (REG_P (breg)
	      && REGNO (breg) >= REGNO (dst)
	      && REGNO (breg) < REGNO (dst) + nregs)
	    j = REGNO (breg) - REGNO (dst);
	}
      else if (MEM_P (dst) && INT_REGNO_P (reg))
	{
	  rtx breg;

	  if (GET_CODE (XEXP (dst, 0)) == PRE_INC
	      || GET_CODE (XEXP (dst, 0)) == PRE_DEC)
	    {
	      rtx delta_rtx;
	      breg = XEXP (XEXP (dst, 0), 0);
	      delta_rtx = (GET_CODE (XEXP (dst, 0)) == PRE_INC
			   ? GEN_INT (GET_MODE_SIZE (GET_MODE (dst)))
			   : GEN_INT (-GET_MODE_SIZE (GET_MODE (dst))));

	      /* We have to update the breg before doing the store.
		 Use store with update, if available.  */

	      if (TARGET_UPDATE)
		{
		  rtx nsrc = simplify_gen_subreg (reg_mode, src, mode, 0);
		  emit_insn (TARGET_32BIT
			     ? (TARGET_POWERPC64
				? gen_movdi_si_update (breg, breg, delta_rtx, nsrc)
				: gen_movsi_si_update (breg, breg, delta_rtx, nsrc))
			     : gen_movdi_di_update (breg, breg, delta_rtx, nsrc));
		  used_update = true;
		}
	      else
		emit_insn (gen_add3_insn (breg, breg, delta_rtx));
	      dst = replace_equiv_address (dst, breg);
	    }
	  else if (!rs6000_offsettable_memref_p (dst, reg_mode, true)
		   && GET_CODE (XEXP (dst, 0)) != LO_SUM)
	    {
	      if (GET_CODE (XEXP (dst, 0)) == PRE_MODIFY)
		{
		  rtx basereg = XEXP (XEXP (dst, 0), 0);
		  if (TARGET_UPDATE)
		    {
		      rtx nsrc = simplify_gen_subreg (reg_mode, src, mode, 0);
		      emit_insn (gen_rtx_SET (gen_rtx_MEM (reg_mode,
							   XEXP (dst, 0)),
					      nsrc));
		      used_update = true;
		    }
		  else
		    emit_insn (gen_rtx_SET (basereg,
					    XEXP (XEXP (dst, 0), 1)));
		  dst = replace_equiv_address (dst, basereg);
		}
	      else
		{
		  rtx basereg = XEXP (XEXP (dst, 0), 0);
		  rtx offsetreg = XEXP (XEXP (dst, 0), 1);
		  gcc_assert (GET_CODE (XEXP (dst, 0)) == PLUS
			      && REG_P (basereg)
			      && REG_P (offsetreg)
			      && REGNO (basereg) != REGNO (offsetreg));
		  if (REGNO (basereg) == 0)
		    {
		      rtx tmp = offsetreg;
		      offsetreg = basereg;
		      basereg = tmp;
		    }
		  emit_insn (gen_add3_insn (basereg, basereg, offsetreg));
		  restore_basereg = gen_sub3_insn (basereg, basereg, offsetreg);
		  dst = replace_equiv_address (dst, basereg);
		}
	    }
	  else if (GET_CODE (XEXP (dst, 0)) != LO_SUM)
	    gcc_assert (rs6000_offsettable_memref_p (dst, reg_mode, true));
	}

      /* If we are reading an accumulator register, we have to
	 deprime it before we can access it.  */
      if (TARGET_MMA && REG_P (src)
	  && GET_MODE (src) == PXImode && FP_REGNO_P (REGNO (src)))
	emit_insn (gen_mma_xxmfacc (src, src));

      for (i = 0; i < nregs; i++)
	{
	  /* Calculate index to next subword.  */
	  ++j;
	  if (j == nregs)
	    j = 0;

	  /* If compiler already emitted move of first word by
	     store with update, no need to do anything.  */
	  if (j == 0 && used_update)
	    continue;

	  emit_insn (gen_rtx_SET (simplify_gen_subreg (reg_mode, dst, mode,
						       j * reg_mode_size),
				  simplify_gen_subreg (reg_mode, src, mode,
						       j * reg_mode_size)));
	}

      /* If we are writing an accumulator register, we have to
	 prime it after we've written it.  */
      if (TARGET_MMA && REG_P (dst)
	  && GET_MODE (dst) == PXImode && FP_REGNO_P (REGNO (dst)))
	emit_insn (gen_mma_xxmtacc (dst, dst));

      if (restore_basereg != NULL_RTX)
	emit_insn (restore_basereg);
    }
}

static GTY(()) alias_set_type TOC_alias_set = -1;

alias_set_type
get_TOC_alias_set (void)
{
  if (TOC_alias_set == -1)
    TOC_alias_set = new_alias_set ();
  return TOC_alias_set;
}

/* The mode the ABI uses for a word.  This is not the same as word_mode
   for -m32 -mpowerpc64.  This is used to implement various target hooks.  */

static scalar_int_mode
rs6000_abi_word_mode (void)
{
  return TARGET_32BIT ? SImode : DImode;
}

/* Implement the TARGET_OFFLOAD_OPTIONS hook.  */
static char *
rs6000_offload_options (void)
{
  if (TARGET_64BIT)
    return xstrdup ("-foffload-abi=lp64");
  else
    return xstrdup ("-foffload-abi=ilp32");
}


/* A quick summary of the various types of 'constant-pool tables'
   under PowerPC:

   Target	Flags		Name		One table per
   AIX		(none)		AIX TOC		object file
   AIX		-mfull-toc	AIX TOC		object file
   AIX		-mminimal-toc	AIX minimal TOC	translation unit
   SVR4/EABI	(none)		SVR4 SDATA	object file
   SVR4/EABI	-fpic		SVR4 pic	object file
   SVR4/EABI	-fPIC		SVR4 PIC	translation unit
   SVR4/EABI	-mrelocatable	EABI TOC	function
   SVR4/EABI	-maix		AIX TOC		object file
   SVR4/EABI	-maix -mminimal-toc
				AIX minimal TOC	translation unit

   Name			Reg.	Set by	entries	      contains:
					made by	 addrs?	fp?	sum?

   AIX TOC		2	crt0	as	 Y	option	option
   AIX minimal TOC	30	prolog	gcc	 Y	Y	option
   SVR4 SDATA		13	crt0	gcc	 N	Y	N
   SVR4 pic		30	prolog	ld	 Y	not yet	N
   SVR4 PIC		30	prolog	gcc	 Y	option	option
   EABI TOC		30	prolog	gcc	 Y	option	option

*/

/* Hash functions for the hash table.  */

static unsigned
rs6000_hash_constant (rtx k)
{
  enum rtx_code code = GET_CODE (k);
  machine_mode mode = GET_MODE (k);
  unsigned result = (code << 3) ^ mode;
  const char *format;
  int flen, fidx;

  format = GET_RTX_FORMAT (code);
  flen = strlen (format);
  fidx = 0;

  switch (code)
    {
    case LABEL_REF:
      return result * 1231 + (unsigned) INSN_UID (XEXP (k, 0));

    case CONST_WIDE_INT:
      {
	int i;
	flen = CONST_WIDE_INT_NUNITS (k);
	for (i = 0; i < flen; i++)
	  result = result * 613 + CONST_WIDE_INT_ELT (k, i);
	return result;
      }

    case CONST_DOUBLE:
      return real_hash (CONST_DOUBLE_REAL_VALUE (k)) * result;

    case CODE_LABEL:
      fidx = 3;
      break;

    default:
      break;
    }

  for (; fidx < flen; fidx++)
    switch (format[fidx])
      {
      case 's':
	{
	  unsigned i, len;
	  const char *str = XSTR (k, fidx);
	  len = strlen (str);
	  result = result * 613 + len;
	  for (i = 0; i < len; i++)
	    result = result * 613 + (unsigned) str[i];
	  break;
	}
      case 'u':
      case 'e':
	result = result * 1231 + rs6000_hash_constant (XEXP (k, fidx));
	break;
      case 'i':
      case 'n':
	result = result * 613 + (unsigned) XINT (k, fidx);
	break;
      case 'w':
	if (sizeof (unsigned) >= sizeof (HOST_WIDE_INT))
	  result = result * 613 + (unsigned) XWINT (k, fidx);
	else
	  {
	    size_t i;
	    for (i = 0; i < sizeof (HOST_WIDE_INT) / sizeof (unsigned); i++)
	      result = result * 613 + (unsigned) (XWINT (k, fidx)
						  >> CHAR_BIT * i);
	  }
	break;
      case '0':
	break;
      default:
	gcc_unreachable ();
      }

  return result;
}

hashval_t
toc_hasher::hash (toc_hash_struct *thc)
{
  return rs6000_hash_constant (thc->key) ^ thc->key_mode;
}

/* Compare H1 and H2 for equivalence.  */

bool
toc_hasher::equal (toc_hash_struct *h1, toc_hash_struct *h2)
{
  rtx r1 = h1->key;
  rtx r2 = h2->key;

  if (h1->key_mode != h2->key_mode)
    return 0;

  return rtx_equal_p (r1, r2);
}

/* These are the names given by the C++ front-end to vtables, and
   vtable-like objects.  Ideally, this logic should not be here;
   instead, there should be some programmatic way of inquiring as
   to whether or not an object is a vtable.  */

#define VTABLE_NAME_P(NAME)				\
  (strncmp ("_vt.", name, strlen ("_vt.")) == 0		\
  || strncmp ("_ZTV", name, strlen ("_ZTV")) == 0	\
  || strncmp ("_ZTT", name, strlen ("_ZTT")) == 0	\
  || strncmp ("_ZTI", name, strlen ("_ZTI")) == 0	\
  || strncmp ("_ZTC", name, strlen ("_ZTC")) == 0)

#ifdef NO_DOLLAR_IN_LABEL
/* Return a GGC-allocated character string translating dollar signs in
   input NAME to underscores.  Used by XCOFF ASM_OUTPUT_LABELREF.  */

const char *
rs6000_xcoff_strip_dollar (const char *name)
{
  char *strip, *p;
  const char *q;
  size_t len;

  q = (const char *) strchr (name, '$');

  if (q == 0 || q == name)
    return name;

  len = strlen (name);
  strip = XALLOCAVEC (char, len + 1);
  strcpy (strip, name);
  p = strip + (q - name);
  while (p)
    {
      *p = '_';
      p = strchr (p + 1, '$');
    }

  return ggc_alloc_string (strip, len);
}
#endif

void
rs6000_output_symbol_ref (FILE *file, rtx x)
{
  const char *name = XSTR (x, 0);

  /* Currently C++ toc references to vtables can be emitted before it
     is decided whether the vtable is public or private.  If this is
     the case, then the linker will eventually complain that there is
     a reference to an unknown section.  Thus, for vtables only,
     we emit the TOC reference to reference the identifier and not the
     symbol.  */
  if (VTABLE_NAME_P (name))
    {
      RS6000_OUTPUT_BASENAME (file, name);
    }
  else
    assemble_name (file, name);
}

/* Output a TOC entry.  We derive the entry name from what is being
   written.  */

void
output_toc (FILE *file, rtx x, int labelno, machine_mode mode)
{
  char buf[256];
  const char *name = buf;
  rtx base = x;
  HOST_WIDE_INT offset = 0;

  gcc_assert (!TARGET_NO_TOC_OR_PCREL);

  /* When the linker won't eliminate them, don't output duplicate
     TOC entries (this happens on AIX if there is any kind of TOC,
     and on SVR4 under -fPIC or -mrelocatable).  Don't do this for
     CODE_LABELs.  */
  if (TARGET_TOC && GET_CODE (x) != LABEL_REF)
    {
      struct toc_hash_struct *h;

      /* Create toc_hash_table.  This can't be done at TARGET_OPTION_OVERRIDE
	 time because GGC is not initialized at that point.  */
      if (toc_hash_table == NULL)
	toc_hash_table = hash_table<toc_hasher>::create_ggc (1021);

      h = ggc_alloc<toc_hash_struct> ();
      h->key = x;
      h->key_mode = mode;
      h->labelno = labelno;

      toc_hash_struct **found = toc_hash_table->find_slot (h, INSERT);
      if (*found == NULL)
	*found = h;
      else  /* This is indeed a duplicate.
	       Set this label equal to that label.  */
	{
	  fputs ("\t.set ", file);
	  ASM_OUTPUT_INTERNAL_LABEL_PREFIX (file, "LC");
	  fprintf (file, "%d,", labelno);
	  ASM_OUTPUT_INTERNAL_LABEL_PREFIX (file, "LC");
	  fprintf (file, "%d\n", ((*found)->labelno));

#ifdef HAVE_AS_TLS
	  if (TARGET_XCOFF && SYMBOL_REF_P (x)
	      && (SYMBOL_REF_TLS_MODEL (x) == TLS_MODEL_GLOBAL_DYNAMIC
		  || SYMBOL_REF_TLS_MODEL (x) == TLS_MODEL_LOCAL_DYNAMIC))
	    {
	      fputs ("\t.set ", file);
	      ASM_OUTPUT_INTERNAL_LABEL_PREFIX (file, "LCM");
	      fprintf (file, "%d,", labelno);
	      ASM_OUTPUT_INTERNAL_LABEL_PREFIX (file, "LCM");
	      fprintf (file, "%d\n", ((*found)->labelno));
	    }
#endif
	  return;
	}
    }

  /* If we're going to put a double constant in the TOC, make sure it's
     aligned properly when strict alignment is on.  */
  if ((CONST_DOUBLE_P (x) || CONST_WIDE_INT_P (x))
      && STRICT_ALIGNMENT
      && GET_MODE_BITSIZE (mode) >= 64
      && ! (TARGET_NO_FP_IN_TOC && ! TARGET_MINIMAL_TOC)) {
    ASM_OUTPUT_ALIGN (file, 3);
  }

  (*targetm.asm_out.internal_label) (file, "LC", labelno);

  /* Handle FP constants specially.  Note that if we have a minimal
     TOC, things we put here aren't actually in the TOC, so we can allow
     FP constants.  */
  if (CONST_DOUBLE_P (x)
      && (GET_MODE (x) == TFmode || GET_MODE (x) == TDmode
	  || GET_MODE (x) == IFmode || GET_MODE (x) == KFmode))
    {
      long k[4];

      if (DECIMAL_FLOAT_MODE_P (GET_MODE (x)))
	REAL_VALUE_TO_TARGET_DECIMAL128 (*CONST_DOUBLE_REAL_VALUE (x), k);
      else
	REAL_VALUE_TO_TARGET_LONG_DOUBLE (*CONST_DOUBLE_REAL_VALUE (x), k);

      if (TARGET_64BIT)
	{
	  if (TARGET_ELF || TARGET_MINIMAL_TOC)
	    fputs (DOUBLE_INT_ASM_OP, file);
	  else
	    fprintf (file, "\t.tc FT_%lx_%lx_%lx_%lx[TC],",
		     k[0] & 0xffffffff, k[1] & 0xffffffff,
		     k[2] & 0xffffffff, k[3] & 0xffffffff);
	  fprintf (file, "0x%lx%08lx,0x%lx%08lx\n",
		   k[WORDS_BIG_ENDIAN ? 0 : 1] & 0xffffffff,
		   k[WORDS_BIG_ENDIAN ? 1 : 0] & 0xffffffff,
		   k[WORDS_BIG_ENDIAN ? 2 : 3] & 0xffffffff,
		   k[WORDS_BIG_ENDIAN ? 3 : 2] & 0xffffffff);
	  return;
	}
      else
	{
	  if (TARGET_ELF || TARGET_MINIMAL_TOC)
	    fputs ("\t.long ", file);
	  else
	    fprintf (file, "\t.tc FT_%lx_%lx_%lx_%lx[TC],",
		     k[0] & 0xffffffff, k[1] & 0xffffffff,
		     k[2] & 0xffffffff, k[3] & 0xffffffff);
	  fprintf (file, "0x%lx,0x%lx,0x%lx,0x%lx\n",
		   k[0] & 0xffffffff, k[1] & 0xffffffff,
		   k[2] & 0xffffffff, k[3] & 0xffffffff);
	  return;
	}
    }
  else if (CONST_DOUBLE_P (x)
	   && (GET_MODE (x) == DFmode || GET_MODE (x) == DDmode))
    {
      long k[2];

      if (DECIMAL_FLOAT_MODE_P (GET_MODE (x)))
	REAL_VALUE_TO_TARGET_DECIMAL64 (*CONST_DOUBLE_REAL_VALUE (x), k);
      else
	REAL_VALUE_TO_TARGET_DOUBLE (*CONST_DOUBLE_REAL_VALUE (x), k);

      if (TARGET_64BIT)
	{
	  if (TARGET_ELF || TARGET_MINIMAL_TOC)
	    fputs (DOUBLE_INT_ASM_OP, file);
	  else
	    fprintf (file, "\t.tc FD_%lx_%lx[TC],",
		     k[0] & 0xffffffff, k[1] & 0xffffffff);
	  fprintf (file, "0x%lx%08lx\n",
		   k[WORDS_BIG_ENDIAN ? 0 : 1] & 0xffffffff,
		   k[WORDS_BIG_ENDIAN ? 1 : 0] & 0xffffffff);
	  return;
	}
      else
	{
	  if (TARGET_ELF || TARGET_MINIMAL_TOC)
	    fputs ("\t.long ", file);
	  else
	    fprintf (file, "\t.tc FD_%lx_%lx[TC],",
		     k[0] & 0xffffffff, k[1] & 0xffffffff);
	  fprintf (file, "0x%lx,0x%lx\n",
		   k[0] & 0xffffffff, k[1] & 0xffffffff);
	  return;
	}
    }
  else if (CONST_DOUBLE_P (x)
	   && (GET_MODE (x) == SFmode || GET_MODE (x) == SDmode))
    {
      long l;

      if (DECIMAL_FLOAT_MODE_P (GET_MODE (x)))
	REAL_VALUE_TO_TARGET_DECIMAL32 (*CONST_DOUBLE_REAL_VALUE (x), l);
      else
	REAL_VALUE_TO_TARGET_SINGLE (*CONST_DOUBLE_REAL_VALUE (x), l);

      if (TARGET_64BIT)
	{
	  if (TARGET_ELF || TARGET_MINIMAL_TOC)
	    fputs (DOUBLE_INT_ASM_OP, file);
	  else
	    fprintf (file, "\t.tc FS_%lx[TC],", l & 0xffffffff);
	  if (WORDS_BIG_ENDIAN)
	    fprintf (file, "0x%lx00000000\n", l & 0xffffffff);
	  else
	    fprintf (file, "0x%lx\n", l & 0xffffffff);
	  return;
	}
      else
	{
	  if (TARGET_ELF || TARGET_MINIMAL_TOC)
	    fputs ("\t.long ", file);
	  else
	    fprintf (file, "\t.tc FS_%lx[TC],", l & 0xffffffff);
	  fprintf (file, "0x%lx\n", l & 0xffffffff);
	  return;
	}
    }
  else if (GET_MODE (x) == VOIDmode && CONST_INT_P (x))
    {
      unsigned HOST_WIDE_INT low;
      HOST_WIDE_INT high;

      low = INTVAL (x) & 0xffffffff;
      high = (HOST_WIDE_INT) INTVAL (x) >> 32;

      /* TOC entries are always Pmode-sized, so when big-endian
	 smaller integer constants in the TOC need to be padded.
	 (This is still a win over putting the constants in
	 a separate constant pool, because then we'd have
	 to have both a TOC entry _and_ the actual constant.)

	 For a 32-bit target, CONST_INT values are loaded and shifted
	 entirely within `low' and can be stored in one TOC entry.  */

      /* It would be easy to make this work, but it doesn't now.  */
      gcc_assert (!TARGET_64BIT || POINTER_SIZE >= GET_MODE_BITSIZE (mode));

      if (WORDS_BIG_ENDIAN && POINTER_SIZE > GET_MODE_BITSIZE (mode))
	{
	  low |= high << 32;
	  low <<= POINTER_SIZE - GET_MODE_BITSIZE (mode);
	  high = (HOST_WIDE_INT) low >> 32;
	  low &= 0xffffffff;
	}

      if (TARGET_64BIT)
	{
	  if (TARGET_ELF || TARGET_MINIMAL_TOC)
	    fputs (DOUBLE_INT_ASM_OP, file);
	  else
	    fprintf (file, "\t.tc ID_%lx_%lx[TC],",
		     (long) high & 0xffffffff, (long) low & 0xffffffff);
	  fprintf (file, "0x%lx%08lx\n",
		   (long) high & 0xffffffff, (long) low & 0xffffffff);
	  return;
	}
      else
	{
	  if (POINTER_SIZE < GET_MODE_BITSIZE (mode))
	    {
	      if (TARGET_ELF || TARGET_MINIMAL_TOC)
		fputs ("\t.long ", file);
	      else
		fprintf (file, "\t.tc ID_%lx_%lx[TC],",
			 (long) high & 0xffffffff, (long) low & 0xffffffff);
	      fprintf (file, "0x%lx,0x%lx\n",
		       (long) high & 0xffffffff, (long) low & 0xffffffff);
	    }
	  else
	    {
	      if (TARGET_ELF || TARGET_MINIMAL_TOC)
		fputs ("\t.long ", file);
	      else
		fprintf (file, "\t.tc IS_%lx[TC],", (long) low & 0xffffffff);
	      fprintf (file, "0x%lx\n", (long) low & 0xffffffff);
	    }
	  return;
	}
    }

  if (GET_CODE (x) == CONST)
    {
      gcc_assert (GET_CODE (XEXP (x, 0)) == PLUS
		  && CONST_INT_P (XEXP (XEXP (x, 0), 1)));

      base = XEXP (XEXP (x, 0), 0);
      offset = INTVAL (XEXP (XEXP (x, 0), 1));
    }

  switch (GET_CODE (base))
    {
    case SYMBOL_REF:
      name = XSTR (base, 0);
      break;

    case LABEL_REF:
      ASM_GENERATE_INTERNAL_LABEL (buf, "L",
				   CODE_LABEL_NUMBER (XEXP (base, 0)));
      break;

    case CODE_LABEL:
      ASM_GENERATE_INTERNAL_LABEL (buf, "L", CODE_LABEL_NUMBER (base));
      break;

    default:
      gcc_unreachable ();
    }

  if (TARGET_ELF || TARGET_MINIMAL_TOC)
    fputs (TARGET_32BIT ? "\t.long " : DOUBLE_INT_ASM_OP, file);
  else
    {
      fputs ("\t.tc ", file);
      RS6000_OUTPUT_BASENAME (file, name);

      if (offset < 0)
	fprintf (file, ".N" HOST_WIDE_INT_PRINT_UNSIGNED, - offset);
      else if (offset)
	fprintf (file, ".P" HOST_WIDE_INT_PRINT_UNSIGNED, offset);

      /* Mark large TOC symbols on AIX with [TE] so they are mapped
	 after other TOC symbols, reducing overflow of small TOC access
	 to [TC] symbols.  */
      fputs (TARGET_XCOFF && TARGET_CMODEL != CMODEL_SMALL
	     ? "[TE]," : "[TC],", file);
    }

  /* Currently C++ toc references to vtables can be emitted before it
     is decided whether the vtable is public or private.  If this is
     the case, then the linker will eventually complain that there is
     a TOC reference to an unknown section.  Thus, for vtables only,
     we emit the TOC reference to reference the symbol and not the
     section.  */
  if (VTABLE_NAME_P (name))
    {
      RS6000_OUTPUT_BASENAME (file, name);
      if (offset < 0)
	fprintf (file, HOST_WIDE_INT_PRINT_DEC, offset);
      else if (offset > 0)
	fprintf (file, "+" HOST_WIDE_INT_PRINT_DEC, offset);
    }
  else
    output_addr_const (file, x);

#if HAVE_AS_TLS
  if (TARGET_XCOFF && SYMBOL_REF_P (base))
    {
      switch (SYMBOL_REF_TLS_MODEL (base))
	{
	case 0:
	  break;
	case TLS_MODEL_LOCAL_EXEC:
	  fputs ("@le", file);
	  break;
	case TLS_MODEL_INITIAL_EXEC:
	  fputs ("@ie", file);
	  break;
	/* Use global-dynamic for local-dynamic.  */
	case TLS_MODEL_GLOBAL_DYNAMIC:
	case TLS_MODEL_LOCAL_DYNAMIC:
	  putc ('\n', file);
	  (*targetm.asm_out.internal_label) (file, "LCM", labelno);
	  fputs ("\t.tc .", file);
	  RS6000_OUTPUT_BASENAME (file, name);
	  fputs ("[TC],", file);
	  output_addr_const (file, x);
	  fputs ("@m", file);
	  break;
	default:
	  gcc_unreachable ();
	}
    }
#endif

  putc ('\n', file);
}

/* Output an assembler pseudo-op to write an ASCII string of N characters
   starting at P to FILE.

   On the RS/6000, we have to do this using the .byte operation and
   write out special characters outside the quoted string.
   Also, the assembler is broken; very long strings are truncated,
   so we must artificially break them up early.  */

void
output_ascii (FILE *file, const char *p, int n)
{
  char c;
  int i, count_string;
  const char *for_string = "\t.byte \"";
  const char *for_decimal = "\t.byte ";
  const char *to_close = NULL;

  count_string = 0;
  for (i = 0; i < n; i++)
    {
      c = *p++;
      if (c >= ' ' && c < 0177)
	{
	  if (for_string)
	    fputs (for_string, file);
	  putc (c, file);

	  /* Write two quotes to get one.  */
	  if (c == '"')
	    {
	      putc (c, file);
	      ++count_string;
	    }

	  for_string = NULL;
	  for_decimal = "\"\n\t.byte ";
	  to_close = "\"\n";
	  ++count_string;

	  if (count_string >= 512)
	    {
	      fputs (to_close, file);

	      for_string = "\t.byte \"";
	      for_decimal = "\t.byte ";
	      to_close = NULL;
	      count_string = 0;
	    }
	}
      else
	{
	  if (for_decimal)
	    fputs (for_decimal, file);
	  fprintf (file, "%d", c);

	  for_string = "\n\t.byte \"";
	  for_decimal = ", ";
	  to_close = "\n";
	  count_string = 0;
	}
    }

  /* Now close the string if we have written one.  Then end the line.  */
  if (to_close)
    fputs (to_close, file);
}

/* Generate a unique section name for FILENAME for a section type
   represented by SECTION_DESC.  Output goes into BUF.

   SECTION_DESC can be any string, as long as it is different for each
   possible section type.

   We name the section in the same manner as xlc.  The name begins with an
   underscore followed by the filename (after stripping any leading directory
   names) with the last period replaced by the string SECTION_DESC.  If
   FILENAME does not contain a period, SECTION_DESC is appended to the end of
   the name.  */

void
rs6000_gen_section_name (char **buf, const char *filename,
			 const char *section_desc)
{
  const char *q, *after_last_slash, *last_period = 0;
  char *p;
  int len;

  after_last_slash = filename;
  for (q = filename; *q; q++)
    {
      if (*q == '/')
	after_last_slash = q + 1;
      else if (*q == '.')
	last_period = q;
    }

  len = strlen (after_last_slash) + strlen (section_desc) + 2;
  *buf = (char *) xmalloc (len);

  p = *buf;
  *p++ = '_';

  for (q = after_last_slash; *q; q++)
    {
      if (q == last_period)
	{
	  strcpy (p, section_desc);
	  p += strlen (section_desc);
	  break;
	}

      else if (ISALNUM (*q))
	*p++ = *q;
    }

  if (last_period == 0)
    strcpy (p, section_desc);
  else
    *p = '\0';
}

/* Emit profile function.  */

void
output_profile_hook (int labelno ATTRIBUTE_UNUSED)
{
  /* Non-standard profiling for kernels, which just saves LR then calls
     _mcount without worrying about arg saves.  The idea is to change
     the function prologue as little as possible as it isn't easy to
     account for arg save/restore code added just for _mcount.  */
  if (TARGET_PROFILE_KERNEL)
    return;

  if (DEFAULT_ABI == ABI_AIX || DEFAULT_ABI == ABI_ELFv2)
    {
#ifndef NO_PROFILE_COUNTERS
# define NO_PROFILE_COUNTERS 0
#endif
      if (NO_PROFILE_COUNTERS)
	emit_library_call (init_one_libfunc (RS6000_MCOUNT),
			   LCT_NORMAL, VOIDmode);
      else
	{
	  char buf[30];
	  const char *label_name;
	  rtx fun;

	  ASM_GENERATE_INTERNAL_LABEL (buf, "LP", labelno);
	  label_name = ggc_strdup ((*targetm.strip_name_encoding) (buf));
	  fun = gen_rtx_SYMBOL_REF (Pmode, label_name);

	  emit_library_call (init_one_libfunc (RS6000_MCOUNT),
			     LCT_NORMAL, VOIDmode, fun, Pmode);
	}
    }
  else if (DEFAULT_ABI == ABI_DARWIN)
    {
      const char *mcount_name = RS6000_MCOUNT;
      int caller_addr_regno = LR_REGNO;

      /* Be conservative and always set this, at least for now.  */
      crtl->uses_pic_offset_table = 1;

#if TARGET_MACHO
      /* For PIC code, set up a stub and collect the caller's address
	 from r0, which is where the prologue puts it.  */
      if (MACHOPIC_INDIRECT
	  && crtl->uses_pic_offset_table)
	caller_addr_regno = 0;
#endif
      emit_library_call (gen_rtx_SYMBOL_REF (Pmode, mcount_name),
			 LCT_NORMAL, VOIDmode,
			 gen_rtx_REG (Pmode, caller_addr_regno), Pmode);
    }
}

/* Write function profiler code.  */

void
output_function_profiler (FILE *file, int labelno)
{
  char buf[100];

  switch (DEFAULT_ABI)
    {
    default:
      gcc_unreachable ();

    case ABI_V4:
      if (!TARGET_32BIT)
	{
	  warning (0, "no profiling of 64-bit code for this ABI");
	  return;
	}
      ASM_GENERATE_INTERNAL_LABEL (buf, "LP", labelno);
      fprintf (file, "\tmflr %s\n", reg_names[0]);
      if (NO_PROFILE_COUNTERS)
	{
	  asm_fprintf (file, "\tstw %s,4(%s)\n",
		       reg_names[0], reg_names[1]);
	}
      else if (TARGET_SECURE_PLT && flag_pic)
	{
	  if (TARGET_LINK_STACK)
	    {
	      char name[32];
	      get_ppc476_thunk_name (name);
	      asm_fprintf (file, "\tbl %s\n", name);
	    }
	  else
	    asm_fprintf (file, "\tbcl 20,31,1f\n1:\n");
	  asm_fprintf (file, "\tstw %s,4(%s)\n",
		       reg_names[0], reg_names[1]);
	  asm_fprintf (file, "\tmflr %s\n", reg_names[12]);
	  asm_fprintf (file, "\taddis %s,%s,",
		       reg_names[12], reg_names[12]);
	  assemble_name (file, buf);
	  asm_fprintf (file, "-1b@ha\n\tla %s,", reg_names[0]);
	  assemble_name (file, buf);
	  asm_fprintf (file, "-1b@l(%s)\n", reg_names[12]);
	}
      else if (flag_pic == 1)
	{
	  fputs ("\tbl _GLOBAL_OFFSET_TABLE_@local-4\n", file);
	  asm_fprintf (file, "\tstw %s,4(%s)\n",
		       reg_names[0], reg_names[1]);
	  asm_fprintf (file, "\tmflr %s\n", reg_names[12]);
	  asm_fprintf (file, "\tlwz %s,", reg_names[0]);
	  assemble_name (file, buf);
	  asm_fprintf (file, "@got(%s)\n", reg_names[12]);
	}
      else if (flag_pic > 1)
	{
	  asm_fprintf (file, "\tstw %s,4(%s)\n",
		       reg_names[0], reg_names[1]);
	  /* Now, we need to get the address of the label.  */
	  if (TARGET_LINK_STACK)
	    {
	      char name[32];
	      get_ppc476_thunk_name (name);
	      asm_fprintf (file, "\tbl %s\n\tb 1f\n\t.long ", name);
	      assemble_name (file, buf);
	      fputs ("-.\n1:", file);
	      asm_fprintf (file, "\tmflr %s\n", reg_names[11]);
	      asm_fprintf (file, "\taddi %s,%s,4\n",
			   reg_names[11], reg_names[11]);
	    }
	  else
	    {
	      fputs ("\tbcl 20,31,1f\n\t.long ", file);
	      assemble_name (file, buf);
	      fputs ("-.\n1:", file);
	      asm_fprintf (file, "\tmflr %s\n", reg_names[11]);
	    }
	  asm_fprintf (file, "\tlwz %s,0(%s)\n",
		       reg_names[0], reg_names[11]);
	  asm_fprintf (file, "\tadd %s,%s,%s\n",
		       reg_names[0], reg_names[0], reg_names[11]);
	}
      else
	{
	  asm_fprintf (file, "\tlis %s,", reg_names[12]);
	  assemble_name (file, buf);
	  fputs ("@ha\n", file);
	  asm_fprintf (file, "\tstw %s,4(%s)\n",
		       reg_names[0], reg_names[1]);
	  asm_fprintf (file, "\tla %s,", reg_names[0]);
	  assemble_name (file, buf);
	  asm_fprintf (file, "@l(%s)\n", reg_names[12]);
	}

      /* ABI_V4 saves the static chain reg with ASM_OUTPUT_REG_PUSH.  */
      fprintf (file, "\tbl %s%s\n",
	       RS6000_MCOUNT, flag_pic ? "@plt" : "");
      break;

    case ABI_AIX:
    case ABI_ELFv2:
    case ABI_DARWIN:
      /* Don't do anything, done in output_profile_hook ().  */
      break;
    }
}



/* The following variable value is the last issued insn.  */

static rtx_insn *last_scheduled_insn;

/* The following variable helps to balance issuing of load and
   store instructions */

static int load_store_pendulum;

/* The following variable helps pair divide insns during scheduling.  */
static int divide_cnt;
/* The following variable helps pair and alternate vector and vector load
   insns during scheduling.  */
static int vec_pairing;


/* Power4 load update and store update instructions are cracked into a
   load or store and an integer insn which are executed in the same cycle.
   Branches have their own dispatch slot which does not count against the
   GCC issue rate, but it changes the program flow so there are no other
   instructions to issue in this cycle.  */

static int
rs6000_variable_issue_1 (rtx_insn *insn, int more)
{
  last_scheduled_insn = insn;
  if (GET_CODE (PATTERN (insn)) == USE
      || GET_CODE (PATTERN (insn)) == CLOBBER)
    {
      cached_can_issue_more = more;
      return cached_can_issue_more;
    }

  if (insn_terminates_group_p (insn, current_group))
    {
      cached_can_issue_more = 0;
      return cached_can_issue_more;
    }

  /* If no reservation, but reach here */
  if (recog_memoized (insn) < 0)
    return more;

  if (rs6000_sched_groups)
    {
      if (is_microcoded_insn (insn))
        cached_can_issue_more = 0;
      else if (is_cracked_insn (insn))
        cached_can_issue_more = more > 2 ? more - 2 : 0;
      else
        cached_can_issue_more = more - 1;

      return cached_can_issue_more;
    }

  if (rs6000_tune == PROCESSOR_CELL && is_nonpipeline_insn (insn))
    return 0;

  cached_can_issue_more = more - 1;
  return cached_can_issue_more;
}

static int
rs6000_variable_issue (FILE *stream, int verbose, rtx_insn *insn, int more)
{
  int r = rs6000_variable_issue_1 (insn, more);
  if (verbose)
    fprintf (stream, "// rs6000_variable_issue (more = %d) = %d\n", more, r);
  return r;
}

/* Adjust the cost of a scheduling dependency.  Return the new cost of
   a dependency LINK or INSN on DEP_INSN.  COST is the current cost.  */

static int
rs6000_adjust_cost (rtx_insn *insn, int dep_type, rtx_insn *dep_insn, int cost,
		    unsigned int)
{
  enum attr_type attr_type;

  if (recog_memoized (insn) < 0 || recog_memoized (dep_insn) < 0)
    return cost;

  switch (dep_type)
    {
    case REG_DEP_TRUE:
      {
        /* Data dependency; DEP_INSN writes a register that INSN reads
	   some cycles later.  */

	/* Separate a load from a narrower, dependent store.  */
	if ((rs6000_sched_groups || rs6000_tune == PROCESSOR_POWER9
	     || rs6000_tune == PROCESSOR_POWER10)
	    && GET_CODE (PATTERN (insn)) == SET
	    && GET_CODE (PATTERN (dep_insn)) == SET
	    && MEM_P (XEXP (PATTERN (insn), 1))
	    && MEM_P (XEXP (PATTERN (dep_insn), 0))
	    && (GET_MODE_SIZE (GET_MODE (XEXP (PATTERN (insn), 1)))
		> GET_MODE_SIZE (GET_MODE (XEXP (PATTERN (dep_insn), 0)))))
	  return cost + 14;

        attr_type = get_attr_type (insn);

        switch (attr_type)
          {
          case TYPE_JMPREG:
            /* Tell the first scheduling pass about the latency between
               a mtctr and bctr (and mtlr and br/blr).  The first
               scheduling pass will not know about this latency since
               the mtctr instruction, which has the latency associated
               to it, will be generated by reload.  */
            return 4;
          case TYPE_BRANCH:
            /* Leave some extra cycles between a compare and its
               dependent branch, to inhibit expensive mispredicts.  */
            if ((rs6000_tune == PROCESSOR_PPC603
                 || rs6000_tune == PROCESSOR_PPC604
                 || rs6000_tune == PROCESSOR_PPC604e
                 || rs6000_tune == PROCESSOR_PPC620
                 || rs6000_tune == PROCESSOR_PPC630
                 || rs6000_tune == PROCESSOR_PPC750
                 || rs6000_tune == PROCESSOR_PPC7400
                 || rs6000_tune == PROCESSOR_PPC7450
                 || rs6000_tune == PROCESSOR_PPCE5500
                 || rs6000_tune == PROCESSOR_PPCE6500
                 || rs6000_tune == PROCESSOR_POWER4
                 || rs6000_tune == PROCESSOR_POWER5
		 || rs6000_tune == PROCESSOR_POWER7
		 || rs6000_tune == PROCESSOR_POWER8
		 || rs6000_tune == PROCESSOR_POWER9
		 || rs6000_tune == PROCESSOR_POWER10
                 || rs6000_tune == PROCESSOR_CELL)
                && recog_memoized (dep_insn)
                && (INSN_CODE (dep_insn) >= 0))

              switch (get_attr_type (dep_insn))
                {
                case TYPE_CMP:
                case TYPE_FPCOMPARE:
                case TYPE_CR_LOGICAL:
		  return cost + 2;
                case TYPE_EXTS:
                case TYPE_MUL:
		  if (get_attr_dot (dep_insn) == DOT_YES)
		    return cost + 2;
		  else
		    break;
                case TYPE_SHIFT:
		  if (get_attr_dot (dep_insn) == DOT_YES
		      && get_attr_var_shift (dep_insn) == VAR_SHIFT_NO)
		    return cost + 2;
		  else
		    break;
		default:
		  break;
		}
            break;

          case TYPE_STORE:
          case TYPE_FPSTORE:
            if ((rs6000_tune == PROCESSOR_POWER6)
                && recog_memoized (dep_insn)
                && (INSN_CODE (dep_insn) >= 0))
              {

                if (GET_CODE (PATTERN (insn)) != SET)
                  /* If this happens, we have to extend this to schedule
                     optimally.  Return default for now.  */
                  return cost;

                /* Adjust the cost for the case where the value written
                   by a fixed point operation is used as the address
                   gen value on a store. */
                switch (get_attr_type (dep_insn))
                  {
                  case TYPE_LOAD:
                  case TYPE_CNTLZ:
                    {
                      if (! rs6000_store_data_bypass_p (dep_insn, insn))
                        return get_attr_sign_extend (dep_insn)
                               == SIGN_EXTEND_YES ? 6 : 4;
                      break;
                    }
                  case TYPE_SHIFT:
                    {
                      if (! rs6000_store_data_bypass_p (dep_insn, insn))
                        return get_attr_var_shift (dep_insn) == VAR_SHIFT_YES ?
                               6 : 3;
                      break;
		    }
                  case TYPE_INTEGER:
                  case TYPE_ADD:
                  case TYPE_LOGICAL:
                  case TYPE_EXTS:
                  case TYPE_INSERT:
                    {
                      if (! rs6000_store_data_bypass_p (dep_insn, insn))
                        return 3;
                      break;
                    }
                  case TYPE_STORE:
                  case TYPE_FPLOAD:
                  case TYPE_FPSTORE:
                    {
                      if (get_attr_update (dep_insn) == UPDATE_YES
                          && ! rs6000_store_data_bypass_p (dep_insn, insn))
                        return 3;
                      break;
                    }
                  case TYPE_MUL:
                    {
                      if (! rs6000_store_data_bypass_p (dep_insn, insn))
                        return 17;
                      break;
                    }
                  case TYPE_DIV:
                    {
                      if (! rs6000_store_data_bypass_p (dep_insn, insn))
                        return get_attr_size (dep_insn) == SIZE_32 ? 45 : 57;
                      break;
                    }
                  default:
                    break;
                  }
              }
	    break;

          case TYPE_LOAD:
            if ((rs6000_tune == PROCESSOR_POWER6)
                && recog_memoized (dep_insn)
                && (INSN_CODE (dep_insn) >= 0))
              {

                /* Adjust the cost for the case where the value written
                   by a fixed point instruction is used within the address
                   gen portion of a subsequent load(u)(x) */
                switch (get_attr_type (dep_insn))
                  {
                  case TYPE_LOAD:
                  case TYPE_CNTLZ:
                    {
                      if (set_to_load_agen (dep_insn, insn))
                        return get_attr_sign_extend (dep_insn)
                               == SIGN_EXTEND_YES ? 6 : 4;
                      break;
                    }
                  case TYPE_SHIFT:
                    {
                      if (set_to_load_agen (dep_insn, insn))
                        return get_attr_var_shift (dep_insn) == VAR_SHIFT_YES ?
                               6 : 3;
                      break;
		    }
                  case TYPE_INTEGER:
                  case TYPE_ADD:
                  case TYPE_LOGICAL:
                  case TYPE_EXTS:
                  case TYPE_INSERT:
                    {
                      if (set_to_load_agen (dep_insn, insn))
                        return 3;
                      break;
                    }
                  case TYPE_STORE:
                  case TYPE_FPLOAD:
                  case TYPE_FPSTORE:
                    {
                      if (get_attr_update (dep_insn) == UPDATE_YES
                          && set_to_load_agen (dep_insn, insn))
                        return 3;
                      break;
                    }
                  case TYPE_MUL:
                    {
                      if (set_to_load_agen (dep_insn, insn))
                        return 17;
                      break;
                    }
                  case TYPE_DIV:
                    {
                      if (set_to_load_agen (dep_insn, insn))
                        return get_attr_size (dep_insn) == SIZE_32 ? 45 : 57;
                      break;
                    }
                  default:
                    break;
                  }
              }
            break;

          case TYPE_FPLOAD:
            if ((rs6000_tune == PROCESSOR_POWER6)
                && get_attr_update (insn) == UPDATE_NO
                && recog_memoized (dep_insn)
                && (INSN_CODE (dep_insn) >= 0)
                && (get_attr_type (dep_insn) == TYPE_MFFGPR))
              return 2;

          default:
            break;
          }

	/* Fall out to return default cost.  */
      }
      break;

    case REG_DEP_OUTPUT:
      /* Output dependency; DEP_INSN writes a register that INSN writes some
	 cycles later.  */
      if ((rs6000_tune == PROCESSOR_POWER6)
          && recog_memoized (dep_insn)
          && (INSN_CODE (dep_insn) >= 0))
        {
          attr_type = get_attr_type (insn);

          switch (attr_type)
            {
            case TYPE_FP:
            case TYPE_FPSIMPLE:
              if (get_attr_type (dep_insn) == TYPE_FP
		  || get_attr_type (dep_insn) == TYPE_FPSIMPLE)
                return 1;
              break;
            case TYPE_FPLOAD:
              if (get_attr_update (insn) == UPDATE_NO
                  && get_attr_type (dep_insn) == TYPE_MFFGPR)
                return 2;
              break;
            default:
              break;
            }
        }
      /* Fall through, no cost for output dependency.  */
      /* FALLTHRU */

    case REG_DEP_ANTI:
      /* Anti dependency; DEP_INSN reads a register that INSN writes some
	 cycles later.  */
      return 0;

    default:
      gcc_unreachable ();
    }

  return cost;
}

/* Debug version of rs6000_adjust_cost.  */

static int
rs6000_debug_adjust_cost (rtx_insn *insn, int dep_type, rtx_insn *dep_insn,
			  int cost, unsigned int dw)
{
  int ret = rs6000_adjust_cost (insn, dep_type, dep_insn, cost, dw);

  if (ret != cost)
    {
      const char *dep;

      switch (dep_type)
	{
	default:	     dep = "unknown depencency"; break;
	case REG_DEP_TRUE:   dep = "data dependency";	 break;
	case REG_DEP_OUTPUT: dep = "output dependency";  break;
	case REG_DEP_ANTI:   dep = "anti depencency";	 break;
	}

      fprintf (stderr,
	       "\nrs6000_adjust_cost, final cost = %d, orig cost = %d, "
	       "%s, insn:\n", ret, cost, dep);

      debug_rtx (insn);
    }

  return ret;
}

/* The function returns a true if INSN is microcoded.
   Return false otherwise.  */

static bool
is_microcoded_insn (rtx_insn *insn)
{
  if (!insn || !NONDEBUG_INSN_P (insn)
      || GET_CODE (PATTERN (insn)) == USE
      || GET_CODE (PATTERN (insn)) == CLOBBER)
    return false;

  if (rs6000_tune == PROCESSOR_CELL)
    return get_attr_cell_micro (insn) == CELL_MICRO_ALWAYS;

  if (rs6000_sched_groups
      && (rs6000_tune == PROCESSOR_POWER4 || rs6000_tune == PROCESSOR_POWER5))
    {
      enum attr_type type = get_attr_type (insn);
      if ((type == TYPE_LOAD
	   && get_attr_update (insn) == UPDATE_YES
	   && get_attr_sign_extend (insn) == SIGN_EXTEND_YES)
	  || ((type == TYPE_LOAD || type == TYPE_STORE)
	      && get_attr_update (insn) == UPDATE_YES
	      && get_attr_indexed (insn) == INDEXED_YES)
	  || type == TYPE_MFCR)
	return true;
    }

  return false;
}

/* The function returns true if INSN is cracked into 2 instructions
   by the processor (and therefore occupies 2 issue slots).  */

static bool
is_cracked_insn (rtx_insn *insn)
{
  if (!insn || !NONDEBUG_INSN_P (insn)
      || GET_CODE (PATTERN (insn)) == USE
      || GET_CODE (PATTERN (insn)) == CLOBBER)
    return false;

  if (rs6000_sched_groups
      && (rs6000_tune == PROCESSOR_POWER4 || rs6000_tune == PROCESSOR_POWER5))
    {
      enum attr_type type = get_attr_type (insn);
      if ((type == TYPE_LOAD
	   && get_attr_sign_extend (insn) == SIGN_EXTEND_YES
	   && get_attr_update (insn) == UPDATE_NO)
	  || (type == TYPE_LOAD
	      && get_attr_sign_extend (insn) == SIGN_EXTEND_NO
	      && get_attr_update (insn) == UPDATE_YES
	      && get_attr_indexed (insn) == INDEXED_NO)
	  || (type == TYPE_STORE
	      && get_attr_update (insn) == UPDATE_YES
	      && get_attr_indexed (insn) == INDEXED_NO)
	  || ((type == TYPE_FPLOAD || type == TYPE_FPSTORE)
	      && get_attr_update (insn) == UPDATE_YES)
	  || (type == TYPE_CR_LOGICAL
	      && get_attr_cr_logical_3op (insn) == CR_LOGICAL_3OP_YES)
	  || (type == TYPE_EXTS
	      && get_attr_dot (insn) == DOT_YES)
	  || (type == TYPE_SHIFT
	      && get_attr_dot (insn) == DOT_YES
	      && get_attr_var_shift (insn) == VAR_SHIFT_NO)
	  || (type == TYPE_MUL
	      && get_attr_dot (insn) == DOT_YES)
	  || type == TYPE_DIV
	  || (type == TYPE_INSERT
	      && get_attr_size (insn) == SIZE_32))
	return true;
    }

  return false;
}

/* The function returns true if INSN can be issued only from
   the branch slot.  */

static bool
is_branch_slot_insn (rtx_insn *insn)
{
  if (!insn || !NONDEBUG_INSN_P (insn)
      || GET_CODE (PATTERN (insn)) == USE
      || GET_CODE (PATTERN (insn)) == CLOBBER)
    return false;

  if (rs6000_sched_groups)
    {
      enum attr_type type = get_attr_type (insn);
      if (type == TYPE_BRANCH || type == TYPE_JMPREG)
	return true;
      return false;
    }

  return false;
}

/* The function returns true if out_inst sets a value that is
   used in the address generation computation of in_insn */
static bool
set_to_load_agen (rtx_insn *out_insn, rtx_insn *in_insn)
{
  rtx out_set, in_set;

  /* For performance reasons, only handle the simple case where
     both loads are a single_set. */
  out_set = single_set (out_insn);
  if (out_set)
    {
      in_set = single_set (in_insn);
      if (in_set)
        return reg_mentioned_p (SET_DEST (out_set), SET_SRC (in_set));
    }

  return false;
}

/* Try to determine base/offset/size parts of the given MEM.
   Return true if successful, false if all the values couldn't
   be determined.

   This function only looks for REG or REG+CONST address forms.
   REG+REG address form will return false. */

static bool
get_memref_parts (rtx mem, rtx *base, HOST_WIDE_INT *offset,
		  HOST_WIDE_INT *size)
{
  rtx addr_rtx;
  if MEM_SIZE_KNOWN_P (mem)
    *size = MEM_SIZE (mem);
  else
    return false;

  addr_rtx = (XEXP (mem, 0));
  if (GET_CODE (addr_rtx) == PRE_MODIFY)
    addr_rtx = XEXP (addr_rtx, 1);

  *offset = 0;
  while (GET_CODE (addr_rtx) == PLUS
	 && CONST_INT_P (XEXP (addr_rtx, 1)))
    {
      *offset += INTVAL (XEXP (addr_rtx, 1));
      addr_rtx = XEXP (addr_rtx, 0);
    }
  if (!REG_P (addr_rtx))
    return false;

  *base = addr_rtx;
  return true;
}

/* The function returns true if the target storage location of
   mem1 is adjacent to the target storage location of mem2 */
/* Return 1 if memory locations are adjacent.  */

static bool
adjacent_mem_locations (rtx mem1, rtx mem2)
{
  rtx reg1, reg2;
  HOST_WIDE_INT off1, size1, off2, size2;

  if (get_memref_parts (mem1, &reg1, &off1, &size1)
      && get_memref_parts (mem2, &reg2, &off2, &size2))
    return ((REGNO (reg1) == REGNO (reg2))
	    && ((off1 + size1 == off2)
		|| (off2 + size2 == off1)));

  return false;
}

/* This function returns true if it can be determined that the two MEM
   locations overlap by at least 1 byte based on base reg/offset/size. */

static bool
mem_locations_overlap (rtx mem1, rtx mem2)
{
  rtx reg1, reg2;
  HOST_WIDE_INT off1, size1, off2, size2;

  if (get_memref_parts (mem1, &reg1, &off1, &size1)
      && get_memref_parts (mem2, &reg2, &off2, &size2))
    return ((REGNO (reg1) == REGNO (reg2))
	    && (((off1 <= off2) && (off1 + size1 > off2))
		|| ((off2 <= off1) && (off2 + size2 > off1))));

  return false;
}

/* A C statement (sans semicolon) to update the integer scheduling
   priority INSN_PRIORITY (INSN). Increase the priority to execute the
   INSN earlier, reduce the priority to execute INSN later.  Do not
   define this macro if you do not need to adjust the scheduling
   priorities of insns.  */

static int
rs6000_adjust_priority (rtx_insn *insn ATTRIBUTE_UNUSED, int priority)
{
  rtx load_mem, str_mem;
  /* On machines (like the 750) which have asymmetric integer units,
     where one integer unit can do multiply and divides and the other
     can't, reduce the priority of multiply/divide so it is scheduled
     before other integer operations.  */

#if 0
  if (! INSN_P (insn))
    return priority;

  if (GET_CODE (PATTERN (insn)) == USE)
    return priority;

  switch (rs6000_tune) {
  case PROCESSOR_PPC750:
    switch (get_attr_type (insn))
      {
      default:
	break;

      case TYPE_MUL:
      case TYPE_DIV:
	fprintf (stderr, "priority was %#x (%d) before adjustment\n",
		 priority, priority);
	if (priority >= 0 && priority < 0x01000000)
	  priority >>= 3;
	break;
      }
  }
#endif

  if (insn_must_be_first_in_group (insn)
      && reload_completed
      && current_sched_info->sched_max_insns_priority
      && rs6000_sched_restricted_insns_priority)
    {

      /* Prioritize insns that can be dispatched only in the first
	 dispatch slot.  */
      if (rs6000_sched_restricted_insns_priority == 1)
	/* Attach highest priority to insn. This means that in
	   haifa-sched.c:ready_sort(), dispatch-slot restriction considerations
	   precede 'priority' (critical path) considerations.  */
	return current_sched_info->sched_max_insns_priority;
      else if (rs6000_sched_restricted_insns_priority == 2)
	/* Increase priority of insn by a minimal amount. This means that in
	   haifa-sched.c:ready_sort(), only 'priority' (critical path)
	   considerations precede dispatch-slot restriction considerations.  */
	return (priority + 1);
    }

  if (rs6000_tune == PROCESSOR_POWER6
      && ((load_store_pendulum == -2 && is_load_insn (insn, &load_mem))
          || (load_store_pendulum == 2 && is_store_insn (insn, &str_mem))))
    /* Attach highest priority to insn if the scheduler has just issued two
       stores and this instruction is a load, or two loads and this instruction
       is a store. Power6 wants loads and stores scheduled alternately
       when possible */
    return current_sched_info->sched_max_insns_priority;

  return priority;
}

/* Return true if the instruction is nonpipelined on the Cell. */
static bool
is_nonpipeline_insn (rtx_insn *insn)
{
  enum attr_type type;
  if (!insn || !NONDEBUG_INSN_P (insn)
      || GET_CODE (PATTERN (insn)) == USE
      || GET_CODE (PATTERN (insn)) == CLOBBER)
    return false;

  type = get_attr_type (insn);
  if (type == TYPE_MUL
      || type == TYPE_DIV
      || type == TYPE_SDIV
      || type == TYPE_DDIV
      || type == TYPE_SSQRT
      || type == TYPE_DSQRT
      || type == TYPE_MFCR
      || type == TYPE_MFCRF
      || type == TYPE_MFJMPR)
    {
      return true;
    }
  return false;
}


/* Return how many instructions the machine can issue per cycle.  */

static int
rs6000_issue_rate (void)
{
  /* Unless scheduling for register pressure, use issue rate of 1 for
     first scheduling pass to decrease degradation.  */
  if (!reload_completed && !flag_sched_pressure)
    return 1;

  switch (rs6000_tune) {
  case PROCESSOR_RS64A:
  case PROCESSOR_PPC601: /* ? */
  case PROCESSOR_PPC7450:
    return 3;
  case PROCESSOR_PPC440:
  case PROCESSOR_PPC603:
  case PROCESSOR_PPC750:
  case PROCESSOR_PPC7400:
  case PROCESSOR_PPC8540:
  case PROCESSOR_PPC8548:
  case PROCESSOR_CELL:
  case PROCESSOR_PPCE300C2:
  case PROCESSOR_PPCE300C3:
  case PROCESSOR_PPCE500MC:
  case PROCESSOR_PPCE500MC64:
  case PROCESSOR_PPCE5500:
  case PROCESSOR_PPCE6500:
  case PROCESSOR_TITAN:
    return 2;
  case PROCESSOR_PPC476:
  case PROCESSOR_PPC604:
  case PROCESSOR_PPC604e:
  case PROCESSOR_PPC620:
  case PROCESSOR_PPC630:
    return 4;
  case PROCESSOR_POWER4:
  case PROCESSOR_POWER5:
  case PROCESSOR_POWER6:
  case PROCESSOR_POWER7:
    return 5;
  case PROCESSOR_POWER8:
    return 7;
  case PROCESSOR_POWER9:
  case PROCESSOR_POWER10:
    return 6;
  default:
    return 1;
  }
}

/* Return how many instructions to look ahead for better insn
   scheduling.  */

static int
rs6000_use_sched_lookahead (void)
{
  switch (rs6000_tune)
    {
    case PROCESSOR_PPC8540:
    case PROCESSOR_PPC8548:
      return 4;

    case PROCESSOR_CELL:
      return (reload_completed ? 8 : 0);

    default:
      return 0;
    }
}

/* We are choosing insn from the ready queue.  Return zero if INSN can be
   chosen.  */
static int
rs6000_use_sched_lookahead_guard (rtx_insn *insn, int ready_index)
{
  if (ready_index == 0)
    return 0;

  if (rs6000_tune != PROCESSOR_CELL)
    return 0;

  gcc_assert (insn != NULL_RTX && INSN_P (insn));

  if (!reload_completed
      || is_nonpipeline_insn (insn)
      || is_microcoded_insn (insn))
    return 1;

  return 0;
}

/* Determine if PAT refers to memory. If so, set MEM_REF to the MEM rtx
   and return true.  */

static bool
find_mem_ref (rtx pat, rtx *mem_ref)
{
  const char * fmt;
  int i, j;

  /* stack_tie does not produce any real memory traffic.  */
  if (tie_operand (pat, VOIDmode))
    return false;

  if (MEM_P (pat))
    {
      *mem_ref = pat;
      return true;
    }

  /* Recursively process the pattern.  */
  fmt = GET_RTX_FORMAT (GET_CODE (pat));

  for (i = GET_RTX_LENGTH (GET_CODE (pat)) - 1; i >= 0; i--)
    {
      if (fmt[i] == 'e')
	{
	  if (find_mem_ref (XEXP (pat, i), mem_ref))
	    return true;
	}
      else if (fmt[i] == 'E')
	for (j = XVECLEN (pat, i) - 1; j >= 0; j--)
	  {
	    if (find_mem_ref (XVECEXP (pat, i, j), mem_ref))
	      return true;
	  }
    }

  return false;
}

/* Determine if PAT is a PATTERN of a load insn.  */

static bool
is_load_insn1 (rtx pat, rtx *load_mem)
{
  if (!pat || pat == NULL_RTX)
    return false;

  if (GET_CODE (pat) == SET)
    return find_mem_ref (SET_SRC (pat), load_mem);

  if (GET_CODE (pat) == PARALLEL)
    {
      int i;

      for (i = 0; i < XVECLEN (pat, 0); i++)
	if (is_load_insn1 (XVECEXP (pat, 0, i), load_mem))
	  return true;
    }

  return false;
}

/* Determine if INSN loads from memory.  */

static bool
is_load_insn (rtx insn, rtx *load_mem)
{
  if (!insn || !INSN_P (insn))
    return false;

  if (CALL_P (insn))
    return false;

  return is_load_insn1 (PATTERN (insn), load_mem);
}

/* Determine if PAT is a PATTERN of a store insn.  */

static bool
is_store_insn1 (rtx pat, rtx *str_mem)
{
  if (!pat || pat == NULL_RTX)
    return false;

  if (GET_CODE (pat) == SET)
    return find_mem_ref (SET_DEST (pat), str_mem);

  if (GET_CODE (pat) == PARALLEL)
    {
      int i;

      for (i = 0; i < XVECLEN (pat, 0); i++)
	if (is_store_insn1 (XVECEXP (pat, 0, i), str_mem))
	  return true;
    }

  return false;
}

/* Determine if INSN stores to memory.  */

static bool
is_store_insn (rtx insn, rtx *str_mem)
{
  if (!insn || !INSN_P (insn))
    return false;

  return is_store_insn1 (PATTERN (insn), str_mem);
}

/* Return whether TYPE is a Power9 pairable vector instruction type.  */

static bool
is_power9_pairable_vec_type (enum attr_type type)
{
  switch (type)
    {
      case TYPE_VECSIMPLE:
      case TYPE_VECCOMPLEX:
      case TYPE_VECDIV:
      case TYPE_VECCMP:
      case TYPE_VECPERM:
      case TYPE_VECFLOAT:
      case TYPE_VECFDIV:
      case TYPE_VECDOUBLE:
	return true;
      default:
	break;
    }
  return false;
}

/* Returns whether the dependence between INSN and NEXT is considered
   costly by the given target.  */

static bool
rs6000_is_costly_dependence (dep_t dep, int cost, int distance)
{
  rtx insn;
  rtx next;
  rtx load_mem, str_mem;

  /* If the flag is not enabled - no dependence is considered costly;
     allow all dependent insns in the same group.
     This is the most aggressive option.  */
  if (rs6000_sched_costly_dep == no_dep_costly)
    return false;

  /* If the flag is set to 1 - a dependence is always considered costly;
     do not allow dependent instructions in the same group.
     This is the most conservative option.  */
  if (rs6000_sched_costly_dep == all_deps_costly)
    return true;

  insn = DEP_PRO (dep);
  next = DEP_CON (dep);

  if (rs6000_sched_costly_dep == store_to_load_dep_costly
      && is_load_insn (next, &load_mem)
      && is_store_insn (insn, &str_mem))
    /* Prevent load after store in the same group.  */
    return true;

  if (rs6000_sched_costly_dep == true_store_to_load_dep_costly
      && is_load_insn (next, &load_mem)
      && is_store_insn (insn, &str_mem)
      && DEP_TYPE (dep) == REG_DEP_TRUE
      && mem_locations_overlap(str_mem, load_mem))
     /* Prevent load after store in the same group if it is a true
	dependence.  */
     return true;

  /* The flag is set to X; dependences with latency >= X are considered costly,
     and will not be scheduled in the same group.  */
  if (rs6000_sched_costly_dep <= max_dep_latency
      && ((cost - distance) >= (int)rs6000_sched_costly_dep))
    return true;

  return false;
}

/* Return the next insn after INSN that is found before TAIL is reached,
   skipping any "non-active" insns - insns that will not actually occupy
   an issue slot.  Return NULL_RTX if such an insn is not found.  */

static rtx_insn *
get_next_active_insn (rtx_insn *insn, rtx_insn *tail)
{
  if (insn == NULL_RTX || insn == tail)
    return NULL;

  while (1)
    {
      insn = NEXT_INSN (insn);
      if (insn == NULL_RTX || insn == tail)
	return NULL;

      if (CALL_P (insn)
	  || JUMP_P (insn) || JUMP_TABLE_DATA_P (insn)
	  || (NONJUMP_INSN_P (insn)
	      && GET_CODE (PATTERN (insn)) != USE
	      && GET_CODE (PATTERN (insn)) != CLOBBER
	      && INSN_CODE (insn) != CODE_FOR_stack_tie))
	break;
    }
  return insn;
}

/* Move instruction at POS to the end of the READY list.  */

static void
move_to_end_of_ready (rtx_insn **ready, int pos, int lastpos)
{
  rtx_insn *tmp;
  int i;

  tmp = ready[pos];
  for (i = pos; i < lastpos; i++)
    ready[i] = ready[i + 1];
  ready[lastpos] = tmp;
}

/* Do Power6 specific sched_reorder2 reordering of ready list.  */

static int
power6_sched_reorder2 (rtx_insn **ready, int lastpos)
{
  /* For Power6, we need to handle some special cases to try and keep the
     store queue from overflowing and triggering expensive flushes.

     This code monitors how load and store instructions are being issued
     and skews the ready list one way or the other to increase the likelihood
     that a desired instruction is issued at the proper time.

     A couple of things are done.  First, we maintain a "load_store_pendulum"
     to track the current state of load/store issue.

       - If the pendulum is at zero, then no loads or stores have been
	 issued in the current cycle so we do nothing.

       - If the pendulum is 1, then a single load has been issued in this
	 cycle and we attempt to locate another load in the ready list to
	 issue with it.

       - If the pendulum is -2, then two stores have already been
	 issued in this cycle, so we increase the priority of the first load
	 in the ready list to increase it's likelihood of being chosen first
	 in the next cycle.

       - If the pendulum is -1, then a single store has been issued in this
	 cycle and we attempt to locate another store in the ready list to
	 issue with it, preferring a store to an adjacent memory location to
	 facilitate store pairing in the store queue.

       - If the pendulum is 2, then two loads have already been
	 issued in this cycle, so we increase the priority of the first store
	 in the ready list to increase it's likelihood of being chosen first
	 in the next cycle.

       - If the pendulum < -2 or > 2, then do nothing.

       Note: This code covers the most common scenarios.  There exist non
	     load/store instructions which make use of the LSU and which
	     would need to be accounted for to strictly model the behavior
	     of the machine.  Those instructions are currently unaccounted
	     for to help minimize compile time overhead of this code.
   */
  int pos;
  rtx load_mem, str_mem;

  if (is_store_insn (last_scheduled_insn, &str_mem))
    /* Issuing a store, swing the load_store_pendulum to the left */
    load_store_pendulum--;
  else if (is_load_insn (last_scheduled_insn, &load_mem))
    /* Issuing a load, swing the load_store_pendulum to the right */
    load_store_pendulum++;
  else
    return cached_can_issue_more;

  /* If the pendulum is balanced, or there is only one instruction on
     the ready list, then all is well, so return. */
  if ((load_store_pendulum == 0) || (lastpos <= 0))
    return cached_can_issue_more;

  if (load_store_pendulum == 1)
    {
      /* A load has been issued in this cycle.  Scan the ready list
	 for another load to issue with it */
      pos = lastpos;

      while (pos >= 0)
	{
	  if (is_load_insn (ready[pos], &load_mem))
	    {
	      /* Found a load.  Move it to the head of the ready list,
		 and adjust it's priority so that it is more likely to
		 stay there */
	      move_to_end_of_ready (ready, pos, lastpos);

	      if (!sel_sched_p ()
		  && INSN_PRIORITY_KNOWN (ready[lastpos]))
		INSN_PRIORITY (ready[lastpos])++;
	      break;
	    }
	  pos--;
	}
    }
  else if (load_store_pendulum == -2)
    {
      /* Two stores have been issued in this cycle.  Increase the
	 priority of the first load in the ready list to favor it for
	 issuing in the next cycle. */
      pos = lastpos;

      while (pos >= 0)
	{
	  if (is_load_insn (ready[pos], &load_mem)
	      && !sel_sched_p ()
	      && INSN_PRIORITY_KNOWN (ready[pos]))
	    {
	      INSN_PRIORITY (ready[pos])++;

	      /* Adjust the pendulum to account for the fact that a load
		 was found and increased in priority.  This is to prevent
		 increasing the priority of multiple loads */
	      load_store_pendulum--;

	      break;
	    }
	  pos--;
	}
    }
  else if (load_store_pendulum == -1)
    {
      /* A store has been issued in this cycle.  Scan the ready list for
	 another store to issue with it, preferring a store to an adjacent
	 memory location */
      int first_store_pos = -1;

      pos = lastpos;

      while (pos >= 0)
	{
	  if (is_store_insn (ready[pos], &str_mem))
	    {
	      rtx str_mem2;
	      /* Maintain the index of the first store found on the
		 list */
	      if (first_store_pos == -1)
		first_store_pos = pos;

	      if (is_store_insn (last_scheduled_insn, &str_mem2)
		  && adjacent_mem_locations (str_mem, str_mem2))
		{
		  /* Found an adjacent store.  Move it to the head of the
		     ready list, and adjust it's priority so that it is
		     more likely to stay there */
		  move_to_end_of_ready (ready, pos, lastpos);

		  if (!sel_sched_p ()
		      && INSN_PRIORITY_KNOWN (ready[lastpos]))
		    INSN_PRIORITY (ready[lastpos])++;

		  first_store_pos = -1;

		  break;
		};
	    }
	  pos--;
	}

      if (first_store_pos >= 0)
	{
	  /* An adjacent store wasn't found, but a non-adjacent store was,
	     so move the non-adjacent store to the front of the ready
	     list, and adjust its priority so that it is more likely to
	     stay there. */
	  move_to_end_of_ready (ready, first_store_pos, lastpos);
	  if (!sel_sched_p ()
	      && INSN_PRIORITY_KNOWN (ready[lastpos]))
	    INSN_PRIORITY (ready[lastpos])++;
	}
    }
  else if (load_store_pendulum == 2)
    {
      /* Two loads have been issued in this cycle.  Increase the priority
	 of the first store in the ready list to favor it for issuing in
	 the next cycle. */
      pos = lastpos;

      while (pos >= 0)
	{
	  if (is_store_insn (ready[pos], &str_mem)
	      && !sel_sched_p ()
	      && INSN_PRIORITY_KNOWN (ready[pos]))
	    {
	      INSN_PRIORITY (ready[pos])++;

	      /* Adjust the pendulum to account for the fact that a store
		 was found and increased in priority.  This is to prevent
		 increasing the priority of multiple stores */
	      load_store_pendulum++;

	      break;
	    }
	  pos--;
	}
    }

  return cached_can_issue_more;
}

/* Do Power9 specific sched_reorder2 reordering of ready list.  */

static int
power9_sched_reorder2 (rtx_insn **ready, int lastpos)
{
  int pos;
  enum attr_type type, type2;

  type = get_attr_type (last_scheduled_insn);

  /* Try to issue fixed point divides back-to-back in pairs so they will be
     routed to separate execution units and execute in parallel.  */
  if (type == TYPE_DIV && divide_cnt == 0)
    {
      /* First divide has been scheduled.  */
      divide_cnt = 1;

      /* Scan the ready list looking for another divide, if found move it
	 to the end of the list so it is chosen next.  */
      pos = lastpos;
      while (pos >= 0)
	{
	  if (recog_memoized (ready[pos]) >= 0
	      && get_attr_type (ready[pos]) == TYPE_DIV)
	    {
	      move_to_end_of_ready (ready, pos, lastpos);
	      break;
	    }
	  pos--;
	}
    }
  else
    {
      /* Last insn was the 2nd divide or not a divide, reset the counter.  */
      divide_cnt = 0;

      /* The best dispatch throughput for vector and vector load insns can be
	 achieved by interleaving a vector and vector load such that they'll
	 dispatch to the same superslice. If this pairing cannot be achieved
	 then it is best to pair vector insns together and vector load insns
	 together.

	 To aid in this pairing, vec_pairing maintains the current state with
	 the following values:

	     0  : Initial state, no vecload/vector pairing has been started.

	     1  : A vecload or vector insn has been issued and a candidate for
		  pairing has been found and moved to the end of the ready
		  list.  */
      if (type == TYPE_VECLOAD)
	{
	  /* Issued a vecload.  */
	  if (vec_pairing == 0)
	    {
	      int vecload_pos = -1;
	      /* We issued a single vecload, look for a vector insn to pair it
		 with.  If one isn't found, try to pair another vecload.  */
	      pos = lastpos;
	      while (pos >= 0)
		{
		  if (recog_memoized (ready[pos]) >= 0)
		    {
		      type2 = get_attr_type (ready[pos]);
		      if (is_power9_pairable_vec_type (type2))
			{
			  /* Found a vector insn to pair with, move it to the
			     end of the ready list so it is scheduled next.  */
			  move_to_end_of_ready (ready, pos, lastpos);
			  vec_pairing = 1;
			  return cached_can_issue_more;
			}
		      else if (type2 == TYPE_VECLOAD && vecload_pos == -1)
			/* Remember position of first vecload seen.  */
			vecload_pos = pos;
		    }
		  pos--;
		}
	      if (vecload_pos >= 0)
		{
		  /* Didn't find a vector to pair with but did find a vecload,
		     move it to the end of the ready list.  */
		  move_to_end_of_ready (ready, vecload_pos, lastpos);
		  vec_pairing = 1;
		  return cached_can_issue_more;
		}
	    }
	}
      else if (is_power9_pairable_vec_type (type))
	{
	  /* Issued a vector operation.  */
	  if (vec_pairing == 0)
	    {
	      int vec_pos = -1;
	      /* We issued a single vector insn, look for a vecload to pair it
		 with.  If one isn't found, try to pair another vector.  */
	      pos = lastpos;
	      while (pos >= 0)
		{
		  if (recog_memoized (ready[pos]) >= 0)
		    {
		      type2 = get_attr_type (ready[pos]);
		      if (type2 == TYPE_VECLOAD)
			{
			  /* Found a vecload insn to pair with, move it to the
			     end of the ready list so it is scheduled next.  */
			  move_to_end_of_ready (ready, pos, lastpos);
			  vec_pairing = 1;
			  return cached_can_issue_more;
			}
		      else if (is_power9_pairable_vec_type (type2)
			       && vec_pos == -1)
			/* Remember position of first vector insn seen.  */
			vec_pos = pos;
		    }
		  pos--;
		}
	      if (vec_pos >= 0)
		{
		  /* Didn't find a vecload to pair with but did find a vector
		     insn, move it to the end of the ready list.  */
		  move_to_end_of_ready (ready, vec_pos, lastpos);
		  vec_pairing = 1;
		  return cached_can_issue_more;
		}
	    }
	}

      /* We've either finished a vec/vecload pair, couldn't find an insn to
	 continue the current pair, or the last insn had nothing to do with
	 with pairing.  In any case, reset the state.  */
      vec_pairing = 0;
    }

  return cached_can_issue_more;
}

/* We are about to begin issuing insns for this clock cycle. */

static int
rs6000_sched_reorder (FILE *dump ATTRIBUTE_UNUSED, int sched_verbose,
                        rtx_insn **ready ATTRIBUTE_UNUSED,
                        int *pn_ready ATTRIBUTE_UNUSED,
		        int clock_var ATTRIBUTE_UNUSED)
{
  int n_ready = *pn_ready;

  if (sched_verbose)
    fprintf (dump, "// rs6000_sched_reorder :\n");

  /* Reorder the ready list, if the second to last ready insn
     is a nonepipeline insn.  */
  if (rs6000_tune == PROCESSOR_CELL && n_ready > 1)
  {
    if (is_nonpipeline_insn (ready[n_ready - 1])
        && (recog_memoized (ready[n_ready - 2]) > 0))
      /* Simply swap first two insns.  */
      std::swap (ready[n_ready - 1], ready[n_ready - 2]);
  }

  if (rs6000_tune == PROCESSOR_POWER6)
    load_store_pendulum = 0;

  return rs6000_issue_rate ();
}

/* Like rs6000_sched_reorder, but called after issuing each insn.  */

static int
rs6000_sched_reorder2 (FILE *dump, int sched_verbose, rtx_insn **ready,
		         int *pn_ready, int clock_var ATTRIBUTE_UNUSED)
{
  if (sched_verbose)
    fprintf (dump, "// rs6000_sched_reorder2 :\n");

  /* Do Power6 dependent reordering if necessary.  */
  if (rs6000_tune == PROCESSOR_POWER6 && last_scheduled_insn)
    return power6_sched_reorder2 (ready, *pn_ready - 1);

  /* Do Power9 dependent reordering if necessary.  */
  if (rs6000_tune == PROCESSOR_POWER9 && last_scheduled_insn
      && recog_memoized (last_scheduled_insn) >= 0)
    return power9_sched_reorder2 (ready, *pn_ready - 1);

  return cached_can_issue_more;
}

/* Return whether the presence of INSN causes a dispatch group termination
   of group WHICH_GROUP.

   If WHICH_GROUP == current_group, this function will return true if INSN
   causes the termination of the current group (i.e, the dispatch group to
   which INSN belongs). This means that INSN will be the last insn in the
   group it belongs to.

   If WHICH_GROUP == previous_group, this function will return true if INSN
   causes the termination of the previous group (i.e, the dispatch group that
   precedes the group to which INSN belongs).  This means that INSN will be
   the first insn in the group it belongs to).  */

static bool
insn_terminates_group_p (rtx_insn *insn, enum group_termination which_group)
{
  bool first, last;

  if (! insn)
    return false;

  first = insn_must_be_first_in_group (insn);
  last = insn_must_be_last_in_group (insn);

  if (first && last)
    return true;

  if (which_group == current_group)
    return last;
  else if (which_group == previous_group)
    return first;

  return false;
}


static bool
insn_must_be_first_in_group (rtx_insn *insn)
{
  enum attr_type type;

  if (!insn
      || NOTE_P (insn)
      || DEBUG_INSN_P (insn)
      || GET_CODE (PATTERN (insn)) == USE
      || GET_CODE (PATTERN (insn)) == CLOBBER)
    return false;

  switch (rs6000_tune)
    {
    case PROCESSOR_POWER5:
      if (is_cracked_insn (insn))
        return true;
      /* FALLTHRU */
    case PROCESSOR_POWER4:
      if (is_microcoded_insn (insn))
        return true;

      if (!rs6000_sched_groups)
        return false;

      type = get_attr_type (insn);

      switch (type)
        {
        case TYPE_MFCR:
        case TYPE_MFCRF:
        case TYPE_MTCR:
        case TYPE_CR_LOGICAL:
        case TYPE_MTJMPR:
        case TYPE_MFJMPR:
        case TYPE_DIV:
        case TYPE_LOAD_L:
        case TYPE_STORE_C:
        case TYPE_ISYNC:
        case TYPE_SYNC:
          return true;
        default:
          break;
        }
      break;
    case PROCESSOR_POWER6:
      type = get_attr_type (insn);

      switch (type)
        {
        case TYPE_EXTS:
        case TYPE_CNTLZ:
        case TYPE_TRAP:
        case TYPE_MUL:
        case TYPE_INSERT:
        case TYPE_FPCOMPARE:
        case TYPE_MFCR:
        case TYPE_MTCR:
        case TYPE_MFJMPR:
        case TYPE_MTJMPR:
        case TYPE_ISYNC:
        case TYPE_SYNC:
        case TYPE_LOAD_L:
        case TYPE_STORE_C:
          return true;
        case TYPE_SHIFT:
          if (get_attr_dot (insn) == DOT_NO
              || get_attr_var_shift (insn) == VAR_SHIFT_NO)
            return true;
          else
            break;
        case TYPE_DIV:
          if (get_attr_size (insn) == SIZE_32)
            return true;
          else
            break;
        case TYPE_LOAD:
        case TYPE_STORE:
        case TYPE_FPLOAD:
        case TYPE_FPSTORE:
          if (get_attr_update (insn) == UPDATE_YES)
            return true;
          else
            break;
        default:
          break;
        }
      break;
    case PROCESSOR_POWER7:
      type = get_attr_type (insn);

      switch (type)
        {
        case TYPE_CR_LOGICAL:
        case TYPE_MFCR:
        case TYPE_MFCRF:
        case TYPE_MTCR:
        case TYPE_DIV:
        case TYPE_ISYNC:
        case TYPE_LOAD_L:
        case TYPE_STORE_C:
        case TYPE_MFJMPR:
        case TYPE_MTJMPR:
          return true;
        case TYPE_MUL:
        case TYPE_SHIFT:
        case TYPE_EXTS:
          if (get_attr_dot (insn) == DOT_YES)
            return true;
          else
            break;
        case TYPE_LOAD:
          if (get_attr_sign_extend (insn) == SIGN_EXTEND_YES
              || get_attr_update (insn) == UPDATE_YES)
            return true;
          else
            break;
        case TYPE_STORE:
        case TYPE_FPLOAD:
        case TYPE_FPSTORE:
          if (get_attr_update (insn) == UPDATE_YES)
            return true;
          else
            break;
        default:
          break;
        }
      break;
    case PROCESSOR_POWER8:
      type = get_attr_type (insn);

      switch (type)
        {
        case TYPE_CR_LOGICAL:
        case TYPE_MFCR:
        case TYPE_MFCRF:
        case TYPE_MTCR:
        case TYPE_SYNC:
        case TYPE_ISYNC:
        case TYPE_LOAD_L:
        case TYPE_STORE_C:
        case TYPE_VECSTORE:
        case TYPE_MFJMPR:
        case TYPE_MTJMPR:
          return true;
        case TYPE_SHIFT:
        case TYPE_EXTS:
        case TYPE_MUL:
          if (get_attr_dot (insn) == DOT_YES)
            return true;
          else
            break;
        case TYPE_LOAD:
          if (get_attr_sign_extend (insn) == SIGN_EXTEND_YES
              || get_attr_update (insn) == UPDATE_YES)
            return true;
          else
            break;
        case TYPE_STORE:
          if (get_attr_update (insn) == UPDATE_YES
              && get_attr_indexed (insn) == INDEXED_YES)
            return true;
          else
            break;
        default:
          break;
        }
      break;
    default:
      break;
    }

  return false;
}

static bool
insn_must_be_last_in_group (rtx_insn *insn)
{
  enum attr_type type;

  if (!insn
      || NOTE_P (insn)
      || DEBUG_INSN_P (insn)
      || GET_CODE (PATTERN (insn)) == USE
      || GET_CODE (PATTERN (insn)) == CLOBBER)
    return false;

  switch (rs6000_tune) {
  case PROCESSOR_POWER4:
  case PROCESSOR_POWER5:
    if (is_microcoded_insn (insn))
      return true;

    if (is_branch_slot_insn (insn))
      return true;

    break;
  case PROCESSOR_POWER6:
    type = get_attr_type (insn);

    switch (type)
      {
      case TYPE_EXTS:
      case TYPE_CNTLZ:
      case TYPE_TRAP:
      case TYPE_MUL:
      case TYPE_FPCOMPARE:
      case TYPE_MFCR:
      case TYPE_MTCR:
      case TYPE_MFJMPR:
      case TYPE_MTJMPR:
      case TYPE_ISYNC:
      case TYPE_SYNC:
      case TYPE_LOAD_L:
      case TYPE_STORE_C:
        return true;
      case TYPE_SHIFT:
        if (get_attr_dot (insn) == DOT_NO
            || get_attr_var_shift (insn) == VAR_SHIFT_NO)
          return true;
        else
          break;
      case TYPE_DIV:
        if (get_attr_size (insn) == SIZE_32)
          return true;
        else
          break;
      default:
        break;
    }
    break;
  case PROCESSOR_POWER7:
    type = get_attr_type (insn);

    switch (type)
      {
      case TYPE_ISYNC:
      case TYPE_SYNC:
      case TYPE_LOAD_L:
      case TYPE_STORE_C:
        return true;
      case TYPE_LOAD:
        if (get_attr_sign_extend (insn) == SIGN_EXTEND_YES
            && get_attr_update (insn) == UPDATE_YES)
          return true;
        else
          break;
      case TYPE_STORE:
        if (get_attr_update (insn) == UPDATE_YES
            && get_attr_indexed (insn) == INDEXED_YES)
          return true;
        else
          break;
      default:
        break;
    }
    break;
  case PROCESSOR_POWER8:
    type = get_attr_type (insn);

    switch (type)
      {
      case TYPE_MFCR:
      case TYPE_MTCR:
      case TYPE_ISYNC:
      case TYPE_SYNC:
      case TYPE_LOAD_L:
      case TYPE_STORE_C:
        return true;
      case TYPE_LOAD:
        if (get_attr_sign_extend (insn) == SIGN_EXTEND_YES
            && get_attr_update (insn) == UPDATE_YES)
          return true;
        else
          break;
      case TYPE_STORE:
        if (get_attr_update (insn) == UPDATE_YES
            && get_attr_indexed (insn) == INDEXED_YES)
          return true;
        else
          break;
      default:
        break;
    }
    break;
  default:
    break;
  }

  return false;
}

/* Return true if it is recommended to keep NEXT_INSN "far" (in a separate
   dispatch group) from the insns in GROUP_INSNS.  Return false otherwise.  */

static bool
is_costly_group (rtx *group_insns, rtx next_insn)
{
  int i;
  int issue_rate = rs6000_issue_rate ();

  for (i = 0; i < issue_rate; i++)
    {
      sd_iterator_def sd_it;
      dep_t dep;
      rtx insn = group_insns[i];

      if (!insn)
	continue;

      FOR_EACH_DEP (insn, SD_LIST_RES_FORW, sd_it, dep)
	{
	  rtx next = DEP_CON (dep);

	  if (next == next_insn
	      && rs6000_is_costly_dependence (dep, dep_cost (dep), 0))
	    return true;
	}
    }

  return false;
}

/* Utility of the function redefine_groups.
   Check if it is too costly to schedule NEXT_INSN together with GROUP_INSNS
   in the same dispatch group.  If so, insert nops before NEXT_INSN, in order
   to keep it "far" (in a separate group) from GROUP_INSNS, following
   one of the following schemes, depending on the value of the flag
   -minsert_sched_nops = X:
   (1) X == sched_finish_regroup_exact: insert exactly as many nops as needed
       in order to force NEXT_INSN into a separate group.
   (2) X < sched_finish_regroup_exact: insert exactly X nops.
   GROUP_END, CAN_ISSUE_MORE and GROUP_COUNT record the state after nop
   insertion (has a group just ended, how many vacant issue slots remain in the
   last group, and how many dispatch groups were encountered so far).  */

static int
force_new_group (int sched_verbose, FILE *dump, rtx *group_insns,
		 rtx_insn *next_insn, bool *group_end, int can_issue_more,
		 int *group_count)
{
  rtx nop;
  bool force;
  int issue_rate = rs6000_issue_rate ();
  bool end = *group_end;
  int i;

  if (next_insn == NULL_RTX || DEBUG_INSN_P (next_insn))
    return can_issue_more;

  if (rs6000_sched_insert_nops > sched_finish_regroup_exact)
    return can_issue_more;

  force = is_costly_group (group_insns, next_insn);
  if (!force)
    return can_issue_more;

  if (sched_verbose > 6)
    fprintf (dump,"force: group count = %d, can_issue_more = %d\n",
	     *group_count ,can_issue_more);

  if (rs6000_sched_insert_nops == sched_finish_regroup_exact)
    {
      if (*group_end)
	can_issue_more = 0;

      /* Since only a branch can be issued in the last issue_slot, it is
	 sufficient to insert 'can_issue_more - 1' nops if next_insn is not
	 a branch. If next_insn is a branch, we insert 'can_issue_more' nops;
	 in this case the last nop will start a new group and the branch
	 will be forced to the new group.  */
      if (can_issue_more && !is_branch_slot_insn (next_insn))
	can_issue_more--;

      /* Do we have a special group ending nop? */
      if (rs6000_tune == PROCESSOR_POWER6 || rs6000_tune == PROCESSOR_POWER7
	  || rs6000_tune == PROCESSOR_POWER8)
	{
	  nop = gen_group_ending_nop ();
	  emit_insn_before (nop, next_insn);
	  can_issue_more = 0;
	}
      else
	while (can_issue_more > 0)
	  {
	    nop = gen_nop ();
	    emit_insn_before (nop, next_insn);
	    can_issue_more--;
	  }

      *group_end = true;
      return 0;
    }

  if (rs6000_sched_insert_nops < sched_finish_regroup_exact)
    {
      int n_nops = rs6000_sched_insert_nops;

      /* Nops can't be issued from the branch slot, so the effective
	 issue_rate for nops is 'issue_rate - 1'.  */
      if (can_issue_more == 0)
	can_issue_more = issue_rate;
      can_issue_more--;
      if (can_issue_more == 0)
	{
	  can_issue_more = issue_rate - 1;
	  (*group_count)++;
	  end = true;
	  for (i = 0; i < issue_rate; i++)
	    {
	      group_insns[i] = 0;
	    }
	}

      while (n_nops > 0)
	{
	  nop = gen_nop ();
	  emit_insn_before (nop, next_insn);
	  if (can_issue_more == issue_rate - 1) /* new group begins */
	    end = false;
	  can_issue_more--;
	  if (can_issue_more == 0)
	    {
	      can_issue_more = issue_rate - 1;
	      (*group_count)++;
	      end = true;
	      for (i = 0; i < issue_rate; i++)
		{
		  group_insns[i] = 0;
		}
	    }
	  n_nops--;
	}

      /* Scale back relative to 'issue_rate' (instead of 'issue_rate - 1').  */
      can_issue_more++;

      /* Is next_insn going to start a new group?  */
      *group_end
	= (end
	   || (can_issue_more == 1 && !is_branch_slot_insn (next_insn))
	   || (can_issue_more <= 2 && is_cracked_insn (next_insn))
	   || (can_issue_more < issue_rate &&
	       insn_terminates_group_p (next_insn, previous_group)));
      if (*group_end && end)
	(*group_count)--;

      if (sched_verbose > 6)
	fprintf (dump, "done force: group count = %d, can_issue_more = %d\n",
		 *group_count, can_issue_more);
      return can_issue_more;
    }

  return can_issue_more;
}

/* This function tries to synch the dispatch groups that the compiler "sees"
   with the dispatch groups that the processor dispatcher is expected to
   form in practice.  It tries to achieve this synchronization by forcing the
   estimated processor grouping on the compiler (as opposed to the function
   'pad_goups' which tries to force the scheduler's grouping on the processor).

   The function scans the insn sequence between PREV_HEAD_INSN and TAIL and
   examines the (estimated) dispatch groups that will be formed by the processor
   dispatcher.  It marks these group boundaries to reflect the estimated
   processor grouping, overriding the grouping that the scheduler had marked.
   Depending on the value of the flag '-minsert-sched-nops' this function can
   force certain insns into separate groups or force a certain distance between
   them by inserting nops, for example, if there exists a "costly dependence"
   between the insns.

   The function estimates the group boundaries that the processor will form as
   follows:  It keeps track of how many vacant issue slots are available after
   each insn.  A subsequent insn will start a new group if one of the following
   4 cases applies:
   - no more vacant issue slots remain in the current dispatch group.
   - only the last issue slot, which is the branch slot, is vacant, but the next
     insn is not a branch.
   - only the last 2 or less issue slots, including the branch slot, are vacant,
     which means that a cracked insn (which occupies two issue slots) can't be
     issued in this group.
   - less than 'issue_rate' slots are vacant, and the next insn always needs to
     start a new group.  */

static int
redefine_groups (FILE *dump, int sched_verbose, rtx_insn *prev_head_insn,
		 rtx_insn *tail)
{
  rtx_insn *insn, *next_insn;
  int issue_rate;
  int can_issue_more;
  int slot, i;
  bool group_end;
  int group_count = 0;
  rtx *group_insns;

  /* Initialize.  */
  issue_rate = rs6000_issue_rate ();
  group_insns = XALLOCAVEC (rtx, issue_rate);
  for (i = 0; i < issue_rate; i++)
    {
      group_insns[i] = 0;
    }
  can_issue_more = issue_rate;
  slot = 0;
  insn = get_next_active_insn (prev_head_insn, tail);
  group_end = false;

  while (insn != NULL_RTX)
    {
      slot = (issue_rate - can_issue_more);
      group_insns[slot] = insn;
      can_issue_more =
	rs6000_variable_issue (dump, sched_verbose, insn, can_issue_more);
      if (insn_terminates_group_p (insn, current_group))
	can_issue_more = 0;

      next_insn = get_next_active_insn (insn, tail);
      if (next_insn == NULL_RTX)
	return group_count + 1;

      /* Is next_insn going to start a new group?  */
      group_end
	= (can_issue_more == 0
	   || (can_issue_more == 1 && !is_branch_slot_insn (next_insn))
	   || (can_issue_more <= 2 && is_cracked_insn (next_insn))
	   || (can_issue_more < issue_rate &&
	       insn_terminates_group_p (next_insn, previous_group)));

      can_issue_more = force_new_group (sched_verbose, dump, group_insns,
					next_insn, &group_end, can_issue_more,
					&group_count);

      if (group_end)
	{
	  group_count++;
	  can_issue_more = 0;
	  for (i = 0; i < issue_rate; i++)
	    {
	      group_insns[i] = 0;
	    }
	}

      if (GET_MODE (next_insn) == TImode && can_issue_more)
	PUT_MODE (next_insn, VOIDmode);
      else if (!can_issue_more && GET_MODE (next_insn) != TImode)
	PUT_MODE (next_insn, TImode);

      insn = next_insn;
      if (can_issue_more == 0)
	can_issue_more = issue_rate;
    } /* while */

  return group_count;
}

/* Scan the insn sequence between PREV_HEAD_INSN and TAIL and examine the
   dispatch group boundaries that the scheduler had marked.  Pad with nops
   any dispatch groups which have vacant issue slots, in order to force the
   scheduler's grouping on the processor dispatcher.  The function
   returns the number of dispatch groups found.  */

static int
pad_groups (FILE *dump, int sched_verbose, rtx_insn *prev_head_insn,
	    rtx_insn *tail)
{
  rtx_insn *insn, *next_insn;
  rtx nop;
  int issue_rate;
  int can_issue_more;
  int group_end;
  int group_count = 0;

  /* Initialize issue_rate.  */
  issue_rate = rs6000_issue_rate ();
  can_issue_more = issue_rate;

  insn = get_next_active_insn (prev_head_insn, tail);
  next_insn = get_next_active_insn (insn, tail);

  while (insn != NULL_RTX)
    {
      can_issue_more =
      	rs6000_variable_issue (dump, sched_verbose, insn, can_issue_more);

      group_end = (next_insn == NULL_RTX || GET_MODE (next_insn) == TImode);

      if (next_insn == NULL_RTX)
	break;

      if (group_end)
	{
	  /* If the scheduler had marked group termination at this location
	     (between insn and next_insn), and neither insn nor next_insn will
	     force group termination, pad the group with nops to force group
	     termination.  */
	  if (can_issue_more
	      && (rs6000_sched_insert_nops == sched_finish_pad_groups)
	      && !insn_terminates_group_p (insn, current_group)
	      && !insn_terminates_group_p (next_insn, previous_group))
	    {
	      if (!is_branch_slot_insn (next_insn))
		can_issue_more--;

	      while (can_issue_more)
		{
		  nop = gen_nop ();
		  emit_insn_before (nop, next_insn);
		  can_issue_more--;
		}
	    }

	  can_issue_more = issue_rate;
	  group_count++;
	}

      insn = next_insn;
      next_insn = get_next_active_insn (insn, tail);
    }

  return group_count;
}

/* We're beginning a new block.  Initialize data structures as necessary.  */

static void
rs6000_sched_init (FILE *dump ATTRIBUTE_UNUSED,
		     int sched_verbose ATTRIBUTE_UNUSED,
		     int max_ready ATTRIBUTE_UNUSED)
{
  last_scheduled_insn = NULL;
  load_store_pendulum = 0;
  divide_cnt = 0;
  vec_pairing = 0;
}

/* The following function is called at the end of scheduling BB.
   After reload, it inserts nops at insn group bundling.  */

static void
rs6000_sched_finish (FILE *dump, int sched_verbose)
{
  int n_groups;

  if (sched_verbose)
    fprintf (dump, "=== Finishing schedule.\n");

  if (reload_completed && rs6000_sched_groups)
    {
      /* Do not run sched_finish hook when selective scheduling enabled.  */
      if (sel_sched_p ())
	return;

      if (rs6000_sched_insert_nops == sched_finish_none)
	return;

      if (rs6000_sched_insert_nops == sched_finish_pad_groups)
	n_groups = pad_groups (dump, sched_verbose,
			       current_sched_info->prev_head,
			       current_sched_info->next_tail);
      else
	n_groups = redefine_groups (dump, sched_verbose,
				    current_sched_info->prev_head,
				    current_sched_info->next_tail);

      if (sched_verbose >= 6)
	{
    	  fprintf (dump, "ngroups = %d\n", n_groups);
	  print_rtl (dump, current_sched_info->prev_head);
	  fprintf (dump, "Done finish_sched\n");
	}
    }
}

struct rs6000_sched_context
{
  short cached_can_issue_more;
  rtx_insn *last_scheduled_insn;
  int load_store_pendulum;
  int divide_cnt;
  int vec_pairing;
};

typedef struct rs6000_sched_context rs6000_sched_context_def;
typedef rs6000_sched_context_def *rs6000_sched_context_t;

/* Allocate store for new scheduling context.  */
static void *
rs6000_alloc_sched_context (void)
{
  return xmalloc (sizeof (rs6000_sched_context_def));
}

/* If CLEAN_P is true then initializes _SC with clean data,
   and from the global context otherwise.  */
static void
rs6000_init_sched_context (void *_sc, bool clean_p)
{
  rs6000_sched_context_t sc = (rs6000_sched_context_t) _sc;

  if (clean_p)
    {
      sc->cached_can_issue_more = 0;
      sc->last_scheduled_insn = NULL;
      sc->load_store_pendulum = 0;
      sc->divide_cnt = 0;
      sc->vec_pairing = 0;
    }
  else
    {
      sc->cached_can_issue_more = cached_can_issue_more;
      sc->last_scheduled_insn = last_scheduled_insn;
      sc->load_store_pendulum = load_store_pendulum;
      sc->divide_cnt = divide_cnt;
      sc->vec_pairing = vec_pairing;
    }
}

/* Sets the global scheduling context to the one pointed to by _SC.  */
static void
rs6000_set_sched_context (void *_sc)
{
  rs6000_sched_context_t sc = (rs6000_sched_context_t) _sc;

  gcc_assert (sc != NULL);

  cached_can_issue_more = sc->cached_can_issue_more;
  last_scheduled_insn = sc->last_scheduled_insn;
  load_store_pendulum = sc->load_store_pendulum;
  divide_cnt = sc->divide_cnt;
  vec_pairing = sc->vec_pairing;
}

/* Free _SC.  */
static void
rs6000_free_sched_context (void *_sc)
{
  gcc_assert (_sc != NULL);

  free (_sc);
}

static bool
rs6000_sched_can_speculate_insn (rtx_insn *insn)
{
  switch (get_attr_type (insn))
    {
    case TYPE_DIV:
    case TYPE_SDIV:
    case TYPE_DDIV:
    case TYPE_VECDIV:
    case TYPE_SSQRT:
    case TYPE_DSQRT:
      return false;

    default:
      return true;
  }
}

/* Length in units of the trampoline for entering a nested function.  */

int
rs6000_trampoline_size (void)
{
  int ret = 0;

  switch (DEFAULT_ABI)
    {
    default:
      gcc_unreachable ();

    case ABI_AIX:
      ret = (TARGET_32BIT) ? 12 : 24;
      break;

    case ABI_ELFv2:
      gcc_assert (!TARGET_32BIT);
      ret = 32;
      break;

    case ABI_DARWIN:
    case ABI_V4:
      ret = (TARGET_32BIT) ? 40 : 48;
      break;
    }

  return ret;
}

/* Emit RTL insns to initialize the variable parts of a trampoline.
   FNADDR is an RTX for the address of the function's pure code.
   CXT is an RTX for the static chain value for the function.  */

static void
rs6000_trampoline_init (rtx m_tramp, tree fndecl, rtx cxt)
{
  int regsize = (TARGET_32BIT) ? 4 : 8;
  rtx fnaddr = XEXP (DECL_RTL (fndecl), 0);
  rtx ctx_reg = force_reg (Pmode, cxt);
  rtx addr = force_reg (Pmode, XEXP (m_tramp, 0));

  switch (DEFAULT_ABI)
    {
    default:
      gcc_unreachable ();

    /* Under AIX, just build the 3 word function descriptor */
    case ABI_AIX:
      {
	rtx fnmem, fn_reg, toc_reg;

	if (!TARGET_POINTERS_TO_NESTED_FUNCTIONS)
	  error ("you cannot take the address of a nested function if you use "
		 "the %qs option", "-mno-pointers-to-nested-functions");

	fnmem = gen_const_mem (Pmode, force_reg (Pmode, fnaddr));
	fn_reg = gen_reg_rtx (Pmode);
	toc_reg = gen_reg_rtx (Pmode);

  /* Macro to shorten the code expansions below.  */
# define MEM_PLUS(MEM, OFFSET) adjust_address (MEM, Pmode, OFFSET)

	m_tramp = replace_equiv_address (m_tramp, addr);

	emit_move_insn (fn_reg, MEM_PLUS (fnmem, 0));
	emit_move_insn (toc_reg, MEM_PLUS (fnmem, regsize));
	emit_move_insn (MEM_PLUS (m_tramp, 0), fn_reg);
	emit_move_insn (MEM_PLUS (m_tramp, regsize), toc_reg);
	emit_move_insn (MEM_PLUS (m_tramp, 2*regsize), ctx_reg);

# undef MEM_PLUS
      }
      break;

    /* Under V.4/eabi/darwin, __trampoline_setup does the real work.  */
    case ABI_ELFv2:
    case ABI_DARWIN:
    case ABI_V4:
      emit_library_call (gen_rtx_SYMBOL_REF (Pmode, "__trampoline_setup"),
			 LCT_NORMAL, VOIDmode,
			 addr, Pmode,
			 GEN_INT (rs6000_trampoline_size ()), SImode,
			 fnaddr, Pmode,
			 ctx_reg, Pmode);
      break;
    }
}


/* Returns TRUE iff the target attribute indicated by ATTR_ID takes a plain
   identifier as an argument, so the front end shouldn't look it up.  */

static bool
rs6000_attribute_takes_identifier_p (const_tree attr_id)
{
  return is_attribute_p ("altivec", attr_id);
}

/* Handle the "altivec" attribute.  The attribute may have
   arguments as follows:

	__attribute__((altivec(vector__)))
	__attribute__((altivec(pixel__)))	(always followed by 'unsigned short')
	__attribute__((altivec(bool__)))	(always followed by 'unsigned')

  and may appear more than once (e.g., 'vector bool char') in a
  given declaration.  */

static tree
rs6000_handle_altivec_attribute (tree *node,
				 tree name ATTRIBUTE_UNUSED,
				 tree args,
				 int flags ATTRIBUTE_UNUSED,
				 bool *no_add_attrs)
{
  tree type = *node, result = NULL_TREE;
  machine_mode mode;
  int unsigned_p;
  char altivec_type
    = ((args && TREE_CODE (args) == TREE_LIST && TREE_VALUE (args)
	&& TREE_CODE (TREE_VALUE (args)) == IDENTIFIER_NODE)
       ? *IDENTIFIER_POINTER (TREE_VALUE (args))
       : '?');

  while (POINTER_TYPE_P (type)
	 || TREE_CODE (type) == FUNCTION_TYPE
	 || TREE_CODE (type) == METHOD_TYPE
	 || TREE_CODE (type) == ARRAY_TYPE)
    type = TREE_TYPE (type);

  mode = TYPE_MODE (type);

  /* Check for invalid AltiVec type qualifiers.  */
  if (type == long_double_type_node)
    error ("use of %<long double%> in AltiVec types is invalid");
  else if (type == boolean_type_node)
    error ("use of boolean types in AltiVec types is invalid");
  else if (TREE_CODE (type) == COMPLEX_TYPE)
    error ("use of %<complex%> in AltiVec types is invalid");
  else if (DECIMAL_FLOAT_MODE_P (mode))
    error ("use of decimal floating point types in AltiVec types is invalid");
  else if (!TARGET_VSX)
    {
      if (type == long_unsigned_type_node || type == long_integer_type_node)
	{
	  if (TARGET_64BIT)
	    error ("use of %<long%> in AltiVec types is invalid for "
		   "64-bit code without %qs", "-mvsx");
	  else if (rs6000_warn_altivec_long)
	    warning (0, "use of %<long%> in AltiVec types is deprecated; "
		     "use %<int%>");
	}
      else if (type == long_long_unsigned_type_node
	       || type == long_long_integer_type_node)
	error ("use of %<long long%> in AltiVec types is invalid without %qs",
	       "-mvsx");
      else if (type == double_type_node)
	error ("use of %<double%> in AltiVec types is invalid without %qs",
	       "-mvsx");
    }

  switch (altivec_type)
    {
    case 'v':
      unsigned_p = TYPE_UNSIGNED (type);
      switch (mode)
	{
	case E_TImode:
	  result = (unsigned_p ? unsigned_V1TI_type_node : V1TI_type_node);
	  break;
	case E_DImode:
	  result = (unsigned_p ? unsigned_V2DI_type_node : V2DI_type_node);
	  break;
	case E_SImode:
	  result = (unsigned_p ? unsigned_V4SI_type_node : V4SI_type_node);
	  break;
	case E_HImode:
	  result = (unsigned_p ? unsigned_V8HI_type_node : V8HI_type_node);
	  break;
	case E_QImode:
	  result = (unsigned_p ? unsigned_V16QI_type_node : V16QI_type_node);
	  break;
	case E_SFmode: result = V4SF_type_node; break;
	case E_DFmode: result = V2DF_type_node; break;
	  /* If the user says 'vector int bool', we may be handed the 'bool'
	     attribute _before_ the 'vector' attribute, and so select the
	     proper type in the 'b' case below.  */
	case E_V4SImode: case E_V8HImode: case E_V16QImode: case E_V4SFmode:
	case E_V2DImode: case E_V2DFmode:
	  result = type;
	default: break;
	}
      break;
    case 'b':
      switch (mode)
	{
	case E_DImode: case E_V2DImode: result = bool_V2DI_type_node; break;
	case E_SImode: case E_V4SImode: result = bool_V4SI_type_node; break;
	case E_HImode: case E_V8HImode: result = bool_V8HI_type_node; break;
	case E_QImode: case E_V16QImode: result = bool_V16QI_type_node;
	default: break;
	}
      break;
    case 'p':
      switch (mode)
	{
	case E_V8HImode: result = pixel_V8HI_type_node;
	default: break;
	}
    default: break;
    }

  /* Propagate qualifiers attached to the element type
     onto the vector type.  */
  if (result && result != type && TYPE_QUALS (type))
    result = build_qualified_type (result, TYPE_QUALS (type));

  *no_add_attrs = true;  /* No need to hang on to the attribute.  */

  if (result)
    *node = lang_hooks.types.reconstruct_complex_type (*node, result);

  return NULL_TREE;
}

/* AltiVec defines five built-in scalar types that serve as vector
   elements; we must teach the compiler how to mangle them.  The 128-bit
   floating point mangling is target-specific as well.  MMA defines
   two built-in types to be used as opaque vector types.  */

static const char *
rs6000_mangle_type (const_tree type)
{
  type = TYPE_MAIN_VARIANT (type);

  if (TREE_CODE (type) != VOID_TYPE && TREE_CODE (type) != BOOLEAN_TYPE
      && TREE_CODE (type) != INTEGER_TYPE && TREE_CODE (type) != REAL_TYPE)
    return NULL;

  if (type == bool_char_type_node) return "U6__boolc";
  if (type == bool_short_type_node) return "U6__bools";
  if (type == pixel_type_node) return "u7__pixel";
  if (type == bool_int_type_node) return "U6__booli";
  if (type == bool_long_long_type_node) return "U6__boolx";

  if (SCALAR_FLOAT_TYPE_P (type) && FLOAT128_IBM_P (TYPE_MODE (type)))
    return "g";
  if (SCALAR_FLOAT_TYPE_P (type) && FLOAT128_IEEE_P (TYPE_MODE (type)))
    return ieee128_mangling_gcc_8_1 ? "U10__float128" : "u9__ieee128";

  if (type == vector_pair_type_node)
    return "u13__vector_pair";
  if (type == vector_quad_type_node)
    return "u13__vector_quad";

  /* For all other types, use the default mangling.  */
  return NULL;
}

/* Handle a "longcall" or "shortcall" attribute; arguments as in
   struct attribute_spec.handler.  */

static tree
rs6000_handle_longcall_attribute (tree *node, tree name,
				  tree args ATTRIBUTE_UNUSED,
				  int flags ATTRIBUTE_UNUSED,
				  bool *no_add_attrs)
{
  if (TREE_CODE (*node) != FUNCTION_TYPE
      && TREE_CODE (*node) != FIELD_DECL
      && TREE_CODE (*node) != TYPE_DECL)
    {
      warning (OPT_Wattributes, "%qE attribute only applies to functions",
	       name);
      *no_add_attrs = true;
    }

  return NULL_TREE;
}

/* Set longcall attributes on all functions declared when
   rs6000_default_long_calls is true.  */
static void
rs6000_set_default_type_attributes (tree type)
{
  if (rs6000_default_long_calls
      && (TREE_CODE (type) == FUNCTION_TYPE
	  || TREE_CODE (type) == METHOD_TYPE))
    TYPE_ATTRIBUTES (type) = tree_cons (get_identifier ("longcall"),
					NULL_TREE,
					TYPE_ATTRIBUTES (type));

#if TARGET_MACHO
  darwin_set_default_type_attributes (type);
#endif
}

/* Return a reference suitable for calling a function with the
   longcall attribute.  */

static rtx
rs6000_longcall_ref (rtx call_ref, rtx arg)
{
  /* System V adds '.' to the internal name, so skip them.  */
  const char *call_name = XSTR (call_ref, 0);
  if (*call_name == '.')
    {
      while (*call_name == '.')
	call_name++;

      tree node = get_identifier (call_name);
      call_ref = gen_rtx_SYMBOL_REF (VOIDmode, IDENTIFIER_POINTER (node));
    }

  if (TARGET_PLTSEQ)
    {
      rtx base = const0_rtx;
      int regno = 12;
      if (rs6000_pcrel_p (cfun))
	{
	  rtx reg = gen_rtx_REG (Pmode, regno);
	  rtx u = gen_rtx_UNSPEC_VOLATILE (Pmode,
					   gen_rtvec (3, base, call_ref, arg),
					   UNSPECV_PLT_PCREL);
	  emit_insn (gen_rtx_SET (reg, u));
	  return reg;
	}

      if (DEFAULT_ABI == ABI_ELFv2)
	base = gen_rtx_REG (Pmode, TOC_REGISTER);
      else
	{
	  if (flag_pic)
	    base = gen_rtx_REG (Pmode, RS6000_PIC_OFFSET_TABLE_REGNUM);
	  regno = 11;
	}
      /* Reg must match that used by linker PLT stubs.  For ELFv2, r12
	 may be used by a function global entry point.  For SysV4, r11
	 is used by __glink_PLTresolve lazy resolver entry.  */
      rtx reg = gen_rtx_REG (Pmode, regno);
      rtx hi = gen_rtx_UNSPEC (Pmode, gen_rtvec (3, base, call_ref, arg),
			       UNSPEC_PLT16_HA);
      rtx lo = gen_rtx_UNSPEC_VOLATILE (Pmode,
					gen_rtvec (3, reg, call_ref, arg),
					UNSPECV_PLT16_LO);
      emit_insn (gen_rtx_SET (reg, hi));
      emit_insn (gen_rtx_SET (reg, lo));
      return reg;
    }

  return force_reg (Pmode, call_ref);
}

#ifndef TARGET_USE_MS_BITFIELD_LAYOUT
#define TARGET_USE_MS_BITFIELD_LAYOUT 0
#endif

/* Handle a "ms_struct" or "gcc_struct" attribute; arguments as in
   struct attribute_spec.handler.  */
static tree
rs6000_handle_struct_attribute (tree *node, tree name,
				tree args ATTRIBUTE_UNUSED,
				int flags ATTRIBUTE_UNUSED, bool *no_add_attrs)
{
  tree *type = NULL;
  if (DECL_P (*node))
    {
      if (TREE_CODE (*node) == TYPE_DECL)
        type = &TREE_TYPE (*node);
    }
  else
    type = node;

  if (!(type && (TREE_CODE (*type) == RECORD_TYPE
                 || TREE_CODE (*type) == UNION_TYPE)))
    {
      warning (OPT_Wattributes, "%qE attribute ignored", name);
      *no_add_attrs = true;
    }

  else if ((is_attribute_p ("ms_struct", name)
            && lookup_attribute ("gcc_struct", TYPE_ATTRIBUTES (*type)))
           || ((is_attribute_p ("gcc_struct", name)
                && lookup_attribute ("ms_struct", TYPE_ATTRIBUTES (*type)))))
    {
      warning (OPT_Wattributes, "%qE incompatible attribute ignored",
               name);
      *no_add_attrs = true;
    }

  return NULL_TREE;
}

static bool
rs6000_ms_bitfield_layout_p (const_tree record_type)
{
  return (TARGET_USE_MS_BITFIELD_LAYOUT &&
          !lookup_attribute ("gcc_struct", TYPE_ATTRIBUTES (record_type)))
    || lookup_attribute ("ms_struct", TYPE_ATTRIBUTES (record_type));
}

#ifdef USING_ELFOS_H

/* A get_unnamed_section callback, used for switching to toc_section.  */

static void
rs6000_elf_output_toc_section_asm_op (const void *data ATTRIBUTE_UNUSED)
{
  if ((DEFAULT_ABI == ABI_AIX || DEFAULT_ABI == ABI_ELFv2)
      && TARGET_MINIMAL_TOC)
    {
      if (!toc_initialized)
	{
	  fprintf (asm_out_file, "%s\n", TOC_SECTION_ASM_OP);
	  ASM_OUTPUT_ALIGN (asm_out_file, TARGET_64BIT ? 3 : 2);
	  (*targetm.asm_out.internal_label) (asm_out_file, "LCTOC", 0);
	  fprintf (asm_out_file, "\t.tc ");
	  ASM_OUTPUT_INTERNAL_LABEL_PREFIX (asm_out_file, "LCTOC1[TC],");
	  ASM_OUTPUT_INTERNAL_LABEL_PREFIX (asm_out_file, "LCTOC1");
	  fprintf (asm_out_file, "\n");

	  fprintf (asm_out_file, "%s\n", MINIMAL_TOC_SECTION_ASM_OP);
	  ASM_OUTPUT_ALIGN (asm_out_file, TARGET_64BIT ? 3 : 2);
	  ASM_OUTPUT_INTERNAL_LABEL_PREFIX (asm_out_file, "LCTOC1");
	  fprintf (asm_out_file, " = .+32768\n");
	  toc_initialized = 1;
	}
      else
	fprintf (asm_out_file, "%s\n", MINIMAL_TOC_SECTION_ASM_OP);
    }
  else if (DEFAULT_ABI == ABI_AIX || DEFAULT_ABI == ABI_ELFv2)
    {
      fprintf (asm_out_file, "%s\n", TOC_SECTION_ASM_OP);
      if (!toc_initialized)
	{
	  ASM_OUTPUT_ALIGN (asm_out_file, TARGET_64BIT ? 3 : 2);
	  toc_initialized = 1;
	}
    }
  else
    {
      fprintf (asm_out_file, "%s\n", MINIMAL_TOC_SECTION_ASM_OP);
      if (!toc_initialized)
	{
	  ASM_OUTPUT_ALIGN (asm_out_file, TARGET_64BIT ? 3 : 2);
	  ASM_OUTPUT_INTERNAL_LABEL_PREFIX (asm_out_file, "LCTOC1");
	  fprintf (asm_out_file, " = .+32768\n");
	  toc_initialized = 1;
	}
    }
}

/* Implement TARGET_ASM_INIT_SECTIONS.  */

static void
rs6000_elf_asm_init_sections (void)
{
  toc_section
    = get_unnamed_section (0, rs6000_elf_output_toc_section_asm_op, NULL);

  sdata2_section
    = get_unnamed_section (SECTION_WRITE, output_section_asm_op,
			   SDATA2_SECTION_ASM_OP);
}

/* Implement TARGET_SELECT_RTX_SECTION.  */

static section *
rs6000_elf_select_rtx_section (machine_mode mode, rtx x,
			       unsigned HOST_WIDE_INT align)
{
  if (ASM_OUTPUT_SPECIAL_POOL_ENTRY_P (x, mode))
    return toc_section;
  else
    return default_elf_select_rtx_section (mode, x, align);
}

/* For a SYMBOL_REF, set generic flags and then perform some
   target-specific processing.

   When the AIX ABI is requested on a non-AIX system, replace the
   function name with the real name (with a leading .) rather than the
   function descriptor name.  This saves a lot of overriding code to
   read the prefixes.  */

static void rs6000_elf_encode_section_info (tree, rtx, int) ATTRIBUTE_UNUSED;
static void
rs6000_elf_encode_section_info (tree decl, rtx rtl, int first)
{
  default_encode_section_info (decl, rtl, first);

  if (first
      && TREE_CODE (decl) == FUNCTION_DECL
      && !TARGET_AIX
      && DEFAULT_ABI == ABI_AIX)
    {
      rtx sym_ref = XEXP (rtl, 0);
      size_t len = strlen (XSTR (sym_ref, 0));
      char *str = XALLOCAVEC (char, len + 2);
      str[0] = '.';
      memcpy (str + 1, XSTR (sym_ref, 0), len + 1);
      XSTR (sym_ref, 0) = ggc_alloc_string (str, len + 1);
    }
}

static inline bool
compare_section_name (const char *section, const char *templ)
{
  int len;

  len = strlen (templ);
  return (strncmp (section, templ, len) == 0
	  && (section[len] == 0 || section[len] == '.'));
}

bool
rs6000_elf_in_small_data_p (const_tree decl)
{
  if (rs6000_sdata == SDATA_NONE)
    return false;

  /* We want to merge strings, so we never consider them small data.  */
  if (TREE_CODE (decl) == STRING_CST)
    return false;

  /* Functions are never in the small data area.  */
  if (TREE_CODE (decl) == FUNCTION_DECL)
    return false;

  if (TREE_CODE (decl) == VAR_DECL && DECL_SECTION_NAME (decl))
    {
      const char *section = DECL_SECTION_NAME (decl);
      if (compare_section_name (section, ".sdata")
	  || compare_section_name (section, ".sdata2")
	  || compare_section_name (section, ".gnu.linkonce.s")
	  || compare_section_name (section, ".sbss")
	  || compare_section_name (section, ".sbss2")
	  || compare_section_name (section, ".gnu.linkonce.sb")
	  || strcmp (section, ".PPC.EMB.sdata0") == 0
	  || strcmp (section, ".PPC.EMB.sbss0") == 0)
	return true;
    }
  else
    {
      /* If we are told not to put readonly data in sdata, then don't.  */
      if (TREE_READONLY (decl) && rs6000_sdata != SDATA_EABI
	  && !rs6000_readonly_in_sdata)
	return false;

      HOST_WIDE_INT size = int_size_in_bytes (TREE_TYPE (decl));

      if (size > 0
	  && size <= g_switch_value
	  /* If it's not public, and we're not going to reference it there,
	     there's no need to put it in the small data section.  */
	  && (rs6000_sdata != SDATA_DATA || TREE_PUBLIC (decl)))
	return true;
    }

  return false;
}

#endif /* USING_ELFOS_H */

/* Implement TARGET_USE_BLOCKS_FOR_CONSTANT_P.  */

static bool
rs6000_use_blocks_for_constant_p (machine_mode mode, const_rtx x)
{
  return !ASM_OUTPUT_SPECIAL_POOL_ENTRY_P (x, mode);
}

/* Do not place thread-local symbols refs in the object blocks.  */

static bool
rs6000_use_blocks_for_decl_p (const_tree decl)
{
  return !DECL_THREAD_LOCAL_P (decl);
}

/* Return a REG that occurs in ADDR with coefficient 1.
   ADDR can be effectively incremented by incrementing REG.

   r0 is special and we must not select it as an address
   register by this routine since our caller will try to
   increment the returned register via an "la" instruction.  */

rtx
find_addr_reg (rtx addr)
{
  while (GET_CODE (addr) == PLUS)
    {
      if (REG_P (XEXP (addr, 0))
	  && REGNO (XEXP (addr, 0)) != 0)
	addr = XEXP (addr, 0);
      else if (REG_P (XEXP (addr, 1))
	       && REGNO (XEXP (addr, 1)) != 0)
	addr = XEXP (addr, 1);
      else if (CONSTANT_P (XEXP (addr, 0)))
	addr = XEXP (addr, 1);
      else if (CONSTANT_P (XEXP (addr, 1)))
	addr = XEXP (addr, 0);
      else
	gcc_unreachable ();
    }
  gcc_assert (REG_P (addr) && REGNO (addr) != 0);
  return addr;
}

void
rs6000_fatal_bad_address (rtx op)
{
  fatal_insn ("bad address", op);
}

#if TARGET_MACHO

vec<branch_island, va_gc> *branch_islands;

/* Remember to generate a branch island for far calls to the given
   function.  */

static void
add_compiler_branch_island (tree label_name, tree function_name,
			    int line_number)
{
  branch_island bi = {function_name, label_name, line_number};
  vec_safe_push (branch_islands, bi);
}

/* NO_PREVIOUS_DEF checks in the link list whether the function name is
   already there or not.  */

static int
no_previous_def (tree function_name)
{
  branch_island *bi;
  unsigned ix;

  FOR_EACH_VEC_SAFE_ELT (branch_islands, ix, bi)
    if (function_name == bi->function_name)
      return 0;
  return 1;
}

/* GET_PREV_LABEL gets the label name from the previous definition of
   the function.  */

static tree
get_prev_label (tree function_name)
{
  branch_island *bi;
  unsigned ix;

  FOR_EACH_VEC_SAFE_ELT (branch_islands, ix, bi)
    if (function_name == bi->function_name)
      return bi->label_name;
  return NULL_TREE;
}

/* Generate external symbol indirection stubs (PIC and non-PIC).  */

void
machopic_output_stub (FILE *file, const char *symb, const char *stub)
{
  unsigned int length;
  char *symbol_name, *lazy_ptr_name;
  char *local_label_0;
  static unsigned label = 0;

  /* Lose our funky encoding stuff so it doesn't contaminate the stub.  */
  symb = (*targetm.strip_name_encoding) (symb);

  length = strlen (symb);
  symbol_name = XALLOCAVEC (char, length + 32);
  GEN_SYMBOL_NAME_FOR_SYMBOL (symbol_name, symb, length);

  lazy_ptr_name = XALLOCAVEC (char, length + 32);
  GEN_LAZY_PTR_NAME_FOR_SYMBOL (lazy_ptr_name, symb, length);

  if (MACHOPIC_PURE)
    {
      switch_to_section (darwin_sections[machopic_picsymbol_stub1_section]);
      fprintf (file, "\t.align 5\n");

      fprintf (file, "%s:\n", stub);
      fprintf (file, "\t.indirect_symbol %s\n", symbol_name);

      label++;
      local_label_0 = XALLOCAVEC (char, 16);
      sprintf (local_label_0, "L%u$spb", label);

      fprintf (file, "\tmflr r0\n");
      fprintf (file, "\tbcl 20,31,%s\n", local_label_0);
      fprintf (file, "%s:\n\tmflr r11\n", local_label_0);
      fprintf (file, "\taddis r11,r11,ha16(%s-%s)\n",
	       lazy_ptr_name, local_label_0);
      fprintf (file, "\tmtlr r0\n");
      fprintf (file, "\t%s r12,lo16(%s-%s)(r11)\n",
	       (TARGET_64BIT ? "ldu" : "lwzu"),
	       lazy_ptr_name, local_label_0);
      fprintf (file, "\tmtctr r12\n");
      fprintf (file, "\tbctr\n");
    }
  else /* mdynamic-no-pic or mkernel.  */
    {
      switch_to_section (darwin_sections[machopic_symbol_stub1_section]);
      fprintf (file, "\t.align 4\n");

      fprintf (file, "%s:\n", stub);
      fprintf (file, "\t.indirect_symbol %s\n", symbol_name);

      fprintf (file, "\tlis r11,ha16(%s)\n", lazy_ptr_name);
      fprintf (file, "\t%s r12,lo16(%s)(r11)\n",
	       (TARGET_64BIT ? "ldu" : "lwzu"),
	       lazy_ptr_name);
      fprintf (file, "\tmtctr r12\n");
      fprintf (file, "\tbctr\n");
    }

  switch_to_section (darwin_sections[machopic_lazy_symbol_ptr_section]);
  fprintf (file, "%s:\n", lazy_ptr_name);
  fprintf (file, "\t.indirect_symbol %s\n", symbol_name);
  fprintf (file, "%sdyld_stub_binding_helper\n",
	   (TARGET_64BIT ? DOUBLE_INT_ASM_OP : "\t.long\t"));
}

/* Legitimize PIC addresses.  If the address is already
   position-independent, we return ORIG.  Newly generated
   position-independent addresses go into a reg.  This is REG if non
   zero, otherwise we allocate register(s) as necessary.  */

#define SMALL_INT(X) ((UINTVAL (X) + 0x8000) < 0x10000)

rtx
rs6000_machopic_legitimize_pic_address (rtx orig, machine_mode mode,
					rtx reg)
{
  rtx base, offset;

  if (reg == NULL && !reload_completed)
    reg = gen_reg_rtx (Pmode);

  if (GET_CODE (orig) == CONST)
    {
      rtx reg_temp;

      if (GET_CODE (XEXP (orig, 0)) == PLUS
	  && XEXP (XEXP (orig, 0), 0) == pic_offset_table_rtx)
	return orig;

      gcc_assert (GET_CODE (XEXP (orig, 0)) == PLUS);

      /* Use a different reg for the intermediate value, as
	 it will be marked UNCHANGING.  */
      reg_temp = !can_create_pseudo_p () ? reg : gen_reg_rtx (Pmode);
      base = rs6000_machopic_legitimize_pic_address (XEXP (XEXP (orig, 0), 0),
						     Pmode, reg_temp);
      offset =
	rs6000_machopic_legitimize_pic_address (XEXP (XEXP (orig, 0), 1),
						Pmode, reg);

      if (CONST_INT_P (offset))
	{
	  if (SMALL_INT (offset))
	    return plus_constant (Pmode, base, INTVAL (offset));
	  else if (!reload_completed)
	    offset = force_reg (Pmode, offset);
	  else
	    {
 	      rtx mem = force_const_mem (Pmode, orig);
	      return machopic_legitimize_pic_address (mem, Pmode, reg);
	    }
	}
      return gen_rtx_PLUS (Pmode, base, offset);
    }

  /* Fall back on generic machopic code.  */
  return machopic_legitimize_pic_address (orig, mode, reg);
}

/* Output a .machine directive for the Darwin assembler, and call
   the generic start_file routine.  */

static void
rs6000_darwin_file_start (void)
{
  static const struct
  {
    const char *arg;
    const char *name;
    HOST_WIDE_INT if_set;
  } mapping[] = {
    { "ppc64", "ppc64", MASK_64BIT },
    { "970", "ppc970", MASK_PPC_GPOPT | MASK_MFCRF | MASK_POWERPC64 },
    { "power4", "ppc970", 0 },
    { "G5", "ppc970", 0 },
    { "7450", "ppc7450", 0 },
    { "7400", "ppc7400", MASK_ALTIVEC },
    { "G4", "ppc7400", 0 },
    { "750", "ppc750", 0 },
    { "740", "ppc750", 0 },
    { "G3", "ppc750", 0 },
    { "604e", "ppc604e", 0 },
    { "604", "ppc604", 0 },
    { "603e", "ppc603", 0 },
    { "603", "ppc603", 0 },
    { "601", "ppc601", 0 },
    { NULL, "ppc", 0 } };
  const char *cpu_id = "";
  size_t i;

  rs6000_file_start ();
  darwin_file_start ();

  /* Determine the argument to -mcpu=.  Default to G3 if not specified.  */
  
  if (rs6000_default_cpu != 0 && rs6000_default_cpu[0] != '\0')
    cpu_id = rs6000_default_cpu;

  if (global_options_set.x_rs6000_cpu_index)
    cpu_id = processor_target_table[rs6000_cpu_index].name;

  /* Look through the mapping array.  Pick the first name that either
     matches the argument, has a bit set in IF_SET that is also set
     in the target flags, or has a NULL name.  */

  i = 0;
  while (mapping[i].arg != NULL
	 && strcmp (mapping[i].arg, cpu_id) != 0
	 && (mapping[i].if_set & rs6000_isa_flags) == 0)
    i++;

  fprintf (asm_out_file, "\t.machine %s\n", mapping[i].name);
}

#endif /* TARGET_MACHO */

#if TARGET_ELF
static int
rs6000_elf_reloc_rw_mask (void)
{
  if (flag_pic)
    return 3;
  else if (DEFAULT_ABI == ABI_AIX || DEFAULT_ABI == ABI_ELFv2)
    return 2;
  else
    return 0;
}

/* Record an element in the table of global constructors.  SYMBOL is
   a SYMBOL_REF of the function to be called; PRIORITY is a number
   between 0 and MAX_INIT_PRIORITY.

   This differs from default_named_section_asm_out_constructor in
   that we have special handling for -mrelocatable.  */

static void rs6000_elf_asm_out_constructor (rtx, int) ATTRIBUTE_UNUSED;
static void
rs6000_elf_asm_out_constructor (rtx symbol, int priority)
{
  const char *section = ".ctors";
  char buf[18];

  if (priority != DEFAULT_INIT_PRIORITY)
    {
      sprintf (buf, ".ctors.%.5u",
	       /* Invert the numbering so the linker puts us in the proper
		  order; constructors are run from right to left, and the
		  linker sorts in increasing order.  */
	       MAX_INIT_PRIORITY - priority);
      section = buf;
    }

  switch_to_section (get_section (section, SECTION_WRITE, NULL));
  assemble_align (POINTER_SIZE);

  if (DEFAULT_ABI == ABI_V4
      && (TARGET_RELOCATABLE || flag_pic > 1))
    {
      fputs ("\t.long (", asm_out_file);
      output_addr_const (asm_out_file, symbol);
      fputs (")@fixup\n", asm_out_file);
    }
  else
    assemble_integer (symbol, POINTER_SIZE / BITS_PER_UNIT, POINTER_SIZE, 1);
}

static void rs6000_elf_asm_out_destructor (rtx, int) ATTRIBUTE_UNUSED;
static void
rs6000_elf_asm_out_destructor (rtx symbol, int priority)
{
  const char *section = ".dtors";
  char buf[18];

  if (priority != DEFAULT_INIT_PRIORITY)
    {
      sprintf (buf, ".dtors.%.5u",
	       /* Invert the numbering so the linker puts us in the proper
		  order; constructors are run from right to left, and the
		  linker sorts in increasing order.  */
	       MAX_INIT_PRIORITY - priority);
      section = buf;
    }

  switch_to_section (get_section (section, SECTION_WRITE, NULL));
  assemble_align (POINTER_SIZE);

  if (DEFAULT_ABI == ABI_V4
      && (TARGET_RELOCATABLE || flag_pic > 1))
    {
      fputs ("\t.long (", asm_out_file);
      output_addr_const (asm_out_file, symbol);
      fputs (")@fixup\n", asm_out_file);
    }
  else
    assemble_integer (symbol, POINTER_SIZE / BITS_PER_UNIT, POINTER_SIZE, 1);
}

void
rs6000_elf_declare_function_name (FILE *file, const char *name, tree decl)
{
  if (TARGET_64BIT && DEFAULT_ABI != ABI_ELFv2)
    {
      fputs ("\t.section\t\".opd\",\"aw\"\n\t.align 3\n", file);
      ASM_OUTPUT_LABEL (file, name);
      fputs (DOUBLE_INT_ASM_OP, file);
      rs6000_output_function_entry (file, name);
      fputs (",.TOC.@tocbase,0\n\t.previous\n", file);
      if (DOT_SYMBOLS)
	{
	  fputs ("\t.size\t", file);
	  assemble_name (file, name);
	  fputs (",24\n\t.type\t.", file);
	  assemble_name (file, name);
	  fputs (",@function\n", file);
	  if (TREE_PUBLIC (decl) && ! DECL_WEAK (decl))
	    {
	      fputs ("\t.globl\t.", file);
	      assemble_name (file, name);
	      putc ('\n', file);
	    }
	}
      else
	ASM_OUTPUT_TYPE_DIRECTIVE (file, name, "function");
      ASM_DECLARE_RESULT (file, DECL_RESULT (decl));
      rs6000_output_function_entry (file, name);
      fputs (":\n", file);
      return;
    }

  int uses_toc;
  if (DEFAULT_ABI == ABI_V4
      && (TARGET_RELOCATABLE || flag_pic > 1)
      && !TARGET_SECURE_PLT
      && (!constant_pool_empty_p () || crtl->profile)
      && (uses_toc = uses_TOC ()))
    {
      char buf[256];

      if (uses_toc == 2)
	switch_to_other_text_partition ();
      (*targetm.asm_out.internal_label) (file, "LCL", rs6000_pic_labelno);

      fprintf (file, "\t.long ");
      assemble_name (file, toc_label_name);
      need_toc_init = 1;
      putc ('-', file);
      ASM_GENERATE_INTERNAL_LABEL (buf, "LCF", rs6000_pic_labelno);
      assemble_name (file, buf);
      putc ('\n', file);
      if (uses_toc == 2)
	switch_to_other_text_partition ();
    }

  ASM_OUTPUT_TYPE_DIRECTIVE (file, name, "function");
  ASM_DECLARE_RESULT (file, DECL_RESULT (decl));

  if (TARGET_CMODEL == CMODEL_LARGE
      && rs6000_global_entry_point_prologue_needed_p ())
    {
      char buf[256];

      (*targetm.asm_out.internal_label) (file, "LCL", rs6000_pic_labelno);

      fprintf (file, "\t.quad .TOC.-");
      ASM_GENERATE_INTERNAL_LABEL (buf, "LCF", rs6000_pic_labelno);
      assemble_name (file, buf);
      putc ('\n', file);
    }

  if (DEFAULT_ABI == ABI_AIX)
    {
      const char *desc_name, *orig_name;

      orig_name = (*targetm.strip_name_encoding) (name);
      desc_name = orig_name;
      while (*desc_name == '.')
	desc_name++;

      if (TREE_PUBLIC (decl))
	fprintf (file, "\t.globl %s\n", desc_name);

      fprintf (file, "%s\n", MINIMAL_TOC_SECTION_ASM_OP);
      fprintf (file, "%s:\n", desc_name);
      fprintf (file, "\t.long %s\n", orig_name);
      fputs ("\t.long _GLOBAL_OFFSET_TABLE_\n", file);
      fputs ("\t.long 0\n", file);
      fprintf (file, "\t.previous\n");
    }
  ASM_OUTPUT_LABEL (file, name);
}

static void rs6000_elf_file_end (void) ATTRIBUTE_UNUSED;
static void
rs6000_elf_file_end (void)
{
#ifdef HAVE_AS_GNU_ATTRIBUTE
  /* ??? The value emitted depends on options active at file end.
     Assume anyone using #pragma or attributes that might change
     options knows what they are doing.  */
  if ((TARGET_64BIT || DEFAULT_ABI == ABI_V4)
      && rs6000_passes_float)
    {
      int fp;

      if (TARGET_HARD_FLOAT)
	fp = 1;
      else
	fp = 2;
      if (rs6000_passes_long_double)
	{
	  if (!TARGET_LONG_DOUBLE_128)
	    fp |= 2 * 4;
	  else if (TARGET_IEEEQUAD)
	    fp |= 3 * 4;
	  else
	    fp |= 1 * 4;
	}
      fprintf (asm_out_file, "\t.gnu_attribute 4, %d\n", fp);
    }
  if (TARGET_32BIT && DEFAULT_ABI == ABI_V4)
    {
      if (rs6000_passes_vector)
	fprintf (asm_out_file, "\t.gnu_attribute 8, %d\n",
		 (TARGET_ALTIVEC_ABI ? 2 : 1));
      if (rs6000_returns_struct)
	fprintf (asm_out_file, "\t.gnu_attribute 12, %d\n",
		 aix_struct_return ? 2 : 1);
    }
#endif
#if defined (POWERPC_LINUX) || defined (POWERPC_FREEBSD)
  if (TARGET_32BIT || DEFAULT_ABI == ABI_ELFv2)
    file_end_indicate_exec_stack ();
#endif

  if (flag_split_stack)
    file_end_indicate_split_stack ();

  if (cpu_builtin_p)
    {
      /* We have expanded a CPU builtin, so we need to emit a reference to
	 the special symbol that LIBC uses to declare it supports the
	 AT_PLATFORM and AT_HWCAP/AT_HWCAP2 in the TCB feature.  */
      switch_to_section (data_section);
      fprintf (asm_out_file, "\t.align %u\n", TARGET_32BIT ? 2 : 3);
      fprintf (asm_out_file, "\t%s %s\n",
	       TARGET_32BIT ? ".long" : ".quad", tcb_verification_symbol);
    }
}
#endif

#if TARGET_XCOFF

#ifndef HAVE_XCOFF_DWARF_EXTRAS
#define HAVE_XCOFF_DWARF_EXTRAS 0
#endif

static enum unwind_info_type
rs6000_xcoff_debug_unwind_info (void)
{
  return UI_NONE;
}

static void
rs6000_xcoff_asm_output_anchor (rtx symbol)
{
  char buffer[100];

  sprintf (buffer, "$ + " HOST_WIDE_INT_PRINT_DEC,
	   SYMBOL_REF_BLOCK_OFFSET (symbol));
  fprintf (asm_out_file, "%s", SET_ASM_OP);
  RS6000_OUTPUT_BASENAME (asm_out_file, XSTR (symbol, 0));
  fprintf (asm_out_file, ",");
  RS6000_OUTPUT_BASENAME (asm_out_file, buffer);
  fprintf (asm_out_file, "\n");
}

static void
rs6000_xcoff_asm_globalize_label (FILE *stream, const char *name)
{
  fputs (GLOBAL_ASM_OP, stream);
  RS6000_OUTPUT_BASENAME (stream, name);
  putc ('\n', stream);
}

/* A get_unnamed_decl callback, used for read-only sections.  PTR
   points to the section string variable.  */

static void
rs6000_xcoff_output_readonly_section_asm_op (const void *directive)
{
  fprintf (asm_out_file, "\t.csect %s[RO],%s\n",
	   *(const char *const *) directive,
	   XCOFF_CSECT_DEFAULT_ALIGNMENT_STR);
}

/* Likewise for read-write sections.  */

static void
rs6000_xcoff_output_readwrite_section_asm_op (const void *directive)
{
  fprintf (asm_out_file, "\t.csect %s[RW],%s\n",
	   *(const char *const *) directive,
	   XCOFF_CSECT_DEFAULT_ALIGNMENT_STR);
}

static void
rs6000_xcoff_output_tls_section_asm_op (const void *directive)
{
  fprintf (asm_out_file, "\t.csect %s[TL],%s\n",
	   *(const char *const *) directive,
	   XCOFF_CSECT_DEFAULT_ALIGNMENT_STR);
}

/* A get_unnamed_section callback, used for switching to toc_section.  */

static void
rs6000_xcoff_output_toc_section_asm_op (const void *data ATTRIBUTE_UNUSED)
{
  if (TARGET_MINIMAL_TOC)
    {
      /* toc_section is always selected at least once from
	 rs6000_xcoff_file_start, so this is guaranteed to
	 always be defined once and only once in each file.  */
      if (!toc_initialized)
	{
	  fputs ("\t.toc\nLCTOC..1:\n", asm_out_file);
	  fputs ("\t.tc toc_table[TC],toc_table[RW]\n", asm_out_file);
	  toc_initialized = 1;
	}
      fprintf (asm_out_file, "\t.csect toc_table[RW]%s\n",
	       (TARGET_32BIT ? "" : ",3"));
    }
  else
    fputs ("\t.toc\n", asm_out_file);
}

/* Implement TARGET_ASM_INIT_SECTIONS.  */

static void
rs6000_xcoff_asm_init_sections (void)
{
  read_only_data_section
    = get_unnamed_section (0, rs6000_xcoff_output_readonly_section_asm_op,
			   &xcoff_read_only_section_name);

  private_data_section
    = get_unnamed_section (SECTION_WRITE,
			   rs6000_xcoff_output_readwrite_section_asm_op,
			   &xcoff_private_data_section_name);

  read_only_private_data_section
    = get_unnamed_section (0, rs6000_xcoff_output_readonly_section_asm_op,
			   &xcoff_private_rodata_section_name);

  tls_data_section
    = get_unnamed_section (SECTION_TLS,
			   rs6000_xcoff_output_tls_section_asm_op,
			   &xcoff_tls_data_section_name);

  tls_private_data_section
    = get_unnamed_section (SECTION_TLS,
			   rs6000_xcoff_output_tls_section_asm_op,
			   &xcoff_private_data_section_name);

  toc_section
    = get_unnamed_section (0, rs6000_xcoff_output_toc_section_asm_op, NULL);

  readonly_data_section = read_only_data_section;
}

static int
rs6000_xcoff_reloc_rw_mask (void)
{
  return 3;
}

static void
rs6000_xcoff_asm_named_section (const char *name, unsigned int flags,
				tree decl ATTRIBUTE_UNUSED)
{
  int smclass;
  static const char * const suffix[5] = { "PR", "RO", "RW", "TL", "XO" };

  if (flags & SECTION_EXCLUDE)
    smclass = 4;
  else if (flags & SECTION_DEBUG)
    {
      fprintf (asm_out_file, "\t.dwsect %s\n", name);
      return;
    }
  else if (flags & SECTION_CODE)
    smclass = 0;
  else if (flags & SECTION_TLS)
    smclass = 3;
  else if (flags & SECTION_WRITE)
    smclass = 2;
  else
    smclass = 1;

  fprintf (asm_out_file, "\t.csect %s%s[%s],%u\n",
	   (flags & SECTION_CODE) ? "." : "",
	   name, suffix[smclass], flags & SECTION_ENTSIZE);
}

#define IN_NAMED_SECTION(DECL) \
  ((TREE_CODE (DECL) == FUNCTION_DECL || TREE_CODE (DECL) == VAR_DECL) \
   && DECL_SECTION_NAME (DECL) != NULL)

static section *
rs6000_xcoff_select_section (tree decl, int reloc,
			     unsigned HOST_WIDE_INT align)
{
  /* Place variables with alignment stricter than BIGGEST_ALIGNMENT into
     named section.  */
  if (align > BIGGEST_ALIGNMENT)
    {
      resolve_unique_section (decl, reloc, true);
      if (IN_NAMED_SECTION (decl))
	return get_named_section (decl, NULL, reloc);
    }

  if (decl_readonly_section (decl, reloc))
    {
      if (TREE_PUBLIC (decl))
	return read_only_data_section;
      else
	return read_only_private_data_section;
    }
  else
    {
#if HAVE_AS_TLS
      if (TREE_CODE (decl) == VAR_DECL && DECL_THREAD_LOCAL_P (decl))
	{
	  if (TREE_PUBLIC (decl))
	    return tls_data_section;
	  else if (bss_initializer_p (decl))
	    {
	      /* Convert to COMMON to emit in BSS.  */
	      DECL_COMMON (decl) = 1;
	      return tls_comm_section;
	    }
	  else
	    return tls_private_data_section;
	}
      else
#endif
	if (TREE_PUBLIC (decl))
	return data_section;
      else
	return private_data_section;
    }
}

static void
rs6000_xcoff_unique_section (tree decl, int reloc ATTRIBUTE_UNUSED)
{
  const char *name;

  /* Use select_section for private data and uninitialized data with
     alignment <= BIGGEST_ALIGNMENT.  */
  if (!TREE_PUBLIC (decl)
      || DECL_COMMON (decl)
      || (DECL_INITIAL (decl) == NULL_TREE
	  && DECL_ALIGN (decl) <= BIGGEST_ALIGNMENT)
      || DECL_INITIAL (decl) == error_mark_node
      || (flag_zero_initialized_in_bss
	  && initializer_zerop (DECL_INITIAL (decl))))
    return;

  name = IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (decl));
  name = (*targetm.strip_name_encoding) (name);
  set_decl_section_name (decl, name);
}

/* Select section for constant in constant pool.

   On RS/6000, all constants are in the private read-only data area.
   However, if this is being placed in the TOC it must be output as a
   toc entry.  */

static section *
rs6000_xcoff_select_rtx_section (machine_mode mode, rtx x,
				 unsigned HOST_WIDE_INT align ATTRIBUTE_UNUSED)
{
  if (ASM_OUTPUT_SPECIAL_POOL_ENTRY_P (x, mode))
    return toc_section;
  else
    return read_only_private_data_section;
}

/* Remove any trailing [DS] or the like from the symbol name.  */

static const char *
rs6000_xcoff_strip_name_encoding (const char *name)
{
  size_t len;
  if (*name == '*')
    name++;
  len = strlen (name);
  if (name[len - 1] == ']')
    return ggc_alloc_string (name, len - 4);
  else
    return name;
}

/* Section attributes.  AIX is always PIC.  */

static unsigned int
rs6000_xcoff_section_type_flags (tree decl, const char *name, int reloc)
{
  unsigned int align;
  unsigned int flags = default_section_type_flags (decl, name, reloc);

  /* Align to at least UNIT size.  */
  if ((flags & SECTION_CODE) != 0 || !decl || !DECL_P (decl))
    align = MIN_UNITS_PER_WORD;
  else
    /* Increase alignment of large objects if not already stricter.  */
    align = MAX ((DECL_ALIGN (decl) / BITS_PER_UNIT),
		 int_size_in_bytes (TREE_TYPE (decl)) > MIN_UNITS_PER_WORD
		 ? UNITS_PER_FP_WORD : MIN_UNITS_PER_WORD);

  return flags | (exact_log2 (align) & SECTION_ENTSIZE);
}

/* Output at beginning of assembler file.

   Initialize the section names for the RS/6000 at this point.

   Specify filename, including full path, to assembler.

   We want to go into the TOC section so at least one .toc will be emitted.
   Also, in order to output proper .bs/.es pairs, we need at least one static
   [RW] section emitted.

   Finally, declare mcount when profiling to make the assembler happy.  */

static void
rs6000_xcoff_file_start (void)
{
  rs6000_gen_section_name (&xcoff_bss_section_name,
			   main_input_filename, ".bss_");
  rs6000_gen_section_name (&xcoff_private_data_section_name,
			   main_input_filename, ".rw_");
  rs6000_gen_section_name (&xcoff_private_rodata_section_name,
			   main_input_filename, ".rop_");
  rs6000_gen_section_name (&xcoff_read_only_section_name,
			   main_input_filename, ".ro_");
  rs6000_gen_section_name (&xcoff_tls_data_section_name,
			   main_input_filename, ".tls_");
  rs6000_gen_section_name (&xcoff_tbss_section_name,
			   main_input_filename, ".tbss_[UL]");

  fputs ("\t.file\t", asm_out_file);
  output_quoted_string (asm_out_file, main_input_filename);
  fputc ('\n', asm_out_file);
  if (write_symbols != NO_DEBUG)
    switch_to_section (private_data_section);
  switch_to_section (toc_section);
  switch_to_section (text_section);
  if (profile_flag)
    fprintf (asm_out_file, "\t.extern %s\n", RS6000_MCOUNT);
  rs6000_file_start ();
}

/* Output at end of assembler file.
   On the RS/6000, referencing data should automatically pull in text.  */

static void
rs6000_xcoff_file_end (void)
{
  switch_to_section (text_section);
  fputs ("_section_.text:\n", asm_out_file);
  switch_to_section (data_section);
  fputs (TARGET_32BIT
	 ? "\t.long _section_.text\n" : "\t.llong _section_.text\n",
	 asm_out_file);
}

struct declare_alias_data
{
  FILE *file;
  bool function_descriptor;
};

/* Declare alias N.  A helper function for for_node_and_aliases.  */

static bool
rs6000_declare_alias (struct symtab_node *n, void *d)
{
  struct declare_alias_data *data = (struct declare_alias_data *)d;
  /* Main symbol is output specially, because varasm machinery does part of
     the job for us - we do not need to declare .globl/lglobs and such.  */
  if (!n->alias || n->weakref)
    return false;

  if (lookup_attribute ("ifunc", DECL_ATTRIBUTES (n->decl)))
    return false;

  /* Prevent assemble_alias from trying to use .set pseudo operation
     that does not behave as expected by the middle-end.  */
  TREE_ASM_WRITTEN (n->decl) = true;

  const char *name = IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (n->decl));
  char *buffer = (char *) alloca (strlen (name) + 2);
  char *p;
  int dollar_inside = 0;

  strcpy (buffer, name);
  p = strchr (buffer, '$');
  while (p) {
    *p = '_';
    dollar_inside++;
    p = strchr (p + 1, '$');
  }
  if (TREE_PUBLIC (n->decl))
    {
      if (!RS6000_WEAK || !DECL_WEAK (n->decl))
	{
          if (dollar_inside) {
	      if (data->function_descriptor)
                fprintf(data->file, "\t.rename .%s,\".%s\"\n", buffer, name);
	      fprintf(data->file, "\t.rename %s,\"%s\"\n", buffer, name);
	    }
	  if (data->function_descriptor)
	    {
	      fputs ("\t.globl .", data->file);
	      RS6000_OUTPUT_BASENAME (data->file, buffer);
	      putc ('\n', data->file);
	    }
	  fputs ("\t.globl ", data->file);
	  RS6000_OUTPUT_BASENAME (data->file, buffer);
	  putc ('\n', data->file);
	}
#ifdef ASM_WEAKEN_DECL
      else if (DECL_WEAK (n->decl) && !data->function_descriptor)
	ASM_WEAKEN_DECL (data->file, n->decl, name, NULL);
#endif
    }
  else
    {
      if (dollar_inside)
	{
	  if (data->function_descriptor)
            fprintf(data->file, "\t.rename .%s,\".%s\"\n", buffer, name);
	  fprintf(data->file, "\t.rename %s,\"%s\"\n", buffer, name);
	}
      if (data->function_descriptor)
	{
	  fputs ("\t.lglobl .", data->file);
	  RS6000_OUTPUT_BASENAME (data->file, buffer);
	  putc ('\n', data->file);
	}
      fputs ("\t.lglobl ", data->file);
      RS6000_OUTPUT_BASENAME (data->file, buffer);
      putc ('\n', data->file);
    }
  if (data->function_descriptor)
    fputs (".", data->file);
  RS6000_OUTPUT_BASENAME (data->file, buffer);
  fputs (":\n", data->file);
  return false;
}


#ifdef HAVE_GAS_HIDDEN
/* Helper function to calculate visibility of a DECL
   and return the value as a const string.  */

static const char *
rs6000_xcoff_visibility (tree decl)
{
  static const char * const visibility_types[] = {
    "", ",protected", ",hidden", ",internal"
  };

  enum symbol_visibility vis = DECL_VISIBILITY (decl);
  return visibility_types[vis];
}
#endif


/* This macro produces the initial definition of a function name.
   On the RS/6000, we need to place an extra '.' in the function name and
   output the function descriptor.
   Dollar signs are converted to underscores.

   The csect for the function will have already been created when
   text_section was selected.  We do have to go back to that csect, however.

   The third and fourth parameters to the .function pseudo-op (16 and 044)
   are placeholders which no longer have any use.

   Because AIX assembler's .set command has unexpected semantics, we output
   all aliases as alternative labels in front of the definition.  */

void
rs6000_xcoff_declare_function_name (FILE *file, const char *name, tree decl)
{
  char *buffer = (char *) alloca (strlen (name) + 1);
  char *p;
  int dollar_inside = 0;
  struct declare_alias_data data = {file, false};

  strcpy (buffer, name);
  p = strchr (buffer, '$');
  while (p) {
    *p = '_';
    dollar_inside++;
    p = strchr (p + 1, '$');
  }
  if (TREE_PUBLIC (decl))
    {
      if (!RS6000_WEAK || !DECL_WEAK (decl))
	{
          if (dollar_inside) {
              fprintf(file, "\t.rename .%s,\".%s\"\n", buffer, name);
              fprintf(file, "\t.rename %s,\"%s\"\n", buffer, name);
	    }
	  fputs ("\t.globl .", file);
	  RS6000_OUTPUT_BASENAME (file, buffer);
#ifdef HAVE_GAS_HIDDEN
	  fputs (rs6000_xcoff_visibility (decl), file);
#endif
	  putc ('\n', file);
	}
    }
  else
    {
      if (dollar_inside) {
          fprintf(file, "\t.rename .%s,\".%s\"\n", buffer, name);
          fprintf(file, "\t.rename %s,\"%s\"\n", buffer, name);
	}
      fputs ("\t.lglobl .", file);
      RS6000_OUTPUT_BASENAME (file, buffer);
      putc ('\n', file);
    }
  fputs ("\t.csect ", file);
  RS6000_OUTPUT_BASENAME (file, buffer);
  fputs (TARGET_32BIT ? "[DS]\n" : "[DS],3\n", file);
  RS6000_OUTPUT_BASENAME (file, buffer);
  fputs (":\n", file);
  symtab_node::get (decl)->call_for_symbol_and_aliases (rs6000_declare_alias,
							&data, true);
  fputs (TARGET_32BIT ? "\t.long ." : "\t.llong .", file);
  RS6000_OUTPUT_BASENAME (file, buffer);
  fputs (", TOC[tc0], 0\n", file);
  in_section = NULL;
  switch_to_section (function_section (decl));
  putc ('.', file);
  RS6000_OUTPUT_BASENAME (file, buffer);
  fputs (":\n", file);
  data.function_descriptor = true;
  symtab_node::get (decl)->call_for_symbol_and_aliases (rs6000_declare_alias,
							&data, true);
  if (!DECL_IGNORED_P (decl))
    {
      if (write_symbols == DBX_DEBUG || write_symbols == XCOFF_DEBUG)
	xcoffout_declare_function (file, decl, buffer);
      else if (write_symbols == DWARF2_DEBUG)
	{
	  name = (*targetm.strip_name_encoding) (name);
	  fprintf (file, "\t.function .%s,.%s,2,0\n", name, name);
	}
    }
  return;
}


/* Output assembly language to globalize a symbol from a DECL,
   possibly with visibility.  */

void
rs6000_xcoff_asm_globalize_decl_name (FILE *stream, tree decl)
{
  const char *name = XSTR (XEXP (DECL_RTL (decl), 0), 0);
  fputs (GLOBAL_ASM_OP, stream);
  RS6000_OUTPUT_BASENAME (stream, name);
#ifdef HAVE_GAS_HIDDEN
  fputs (rs6000_xcoff_visibility (decl), stream);
#endif
  putc ('\n', stream);
}

/* Output assembly language to define a symbol as COMMON from a DECL,
   possibly with visibility.  */

void
rs6000_xcoff_asm_output_aligned_decl_common (FILE *stream,
					     tree decl ATTRIBUTE_UNUSED,
					     const char *name,
					     unsigned HOST_WIDE_INT size,
					     unsigned HOST_WIDE_INT align)
{
  unsigned HOST_WIDE_INT align2 = 2;

  if (align > 32)
    align2 = floor_log2 (align / BITS_PER_UNIT);
  else if (size > 4)
    align2 = 3;

  fputs (COMMON_ASM_OP, stream);
  RS6000_OUTPUT_BASENAME (stream, name);

  fprintf (stream,
	   "," HOST_WIDE_INT_PRINT_UNSIGNED "," HOST_WIDE_INT_PRINT_UNSIGNED,
	   size, align2);

#ifdef HAVE_GAS_HIDDEN
  if (decl != NULL)
    fputs (rs6000_xcoff_visibility (decl), stream);
#endif
  putc ('\n', stream);
}

/* This macro produces the initial definition of a object (variable) name.
   Because AIX assembler's .set command has unexpected semantics, we output
   all aliases as alternative labels in front of the definition.  */

void
rs6000_xcoff_declare_object_name (FILE *file, const char *name, tree decl)
{
  struct declare_alias_data data = {file, false};
  RS6000_OUTPUT_BASENAME (file, name);
  fputs (":\n", file);
  symtab_node::get_create (decl)->call_for_symbol_and_aliases (rs6000_declare_alias,
							       &data, true);
}

/* Overide the default 'SYMBOL-.' syntax with AIX compatible 'SYMBOL-$'. */

void
rs6000_asm_output_dwarf_pcrel (FILE *file, int size, const char *label)
{
  fputs (integer_asm_op (size, FALSE), file);
  assemble_name (file, label);
  fputs ("-$", file);
}

/* Output a symbol offset relative to the dbase for the current object.
   We use __gcc_unwind_dbase as an arbitrary base for dbase and assume
   signed offsets.

   __gcc_unwind_dbase is embedded in all executables/libraries through
   libgcc/config/rs6000/crtdbase.S.  */

void
rs6000_asm_output_dwarf_datarel (FILE *file, int size, const char *label)
{
  fputs (integer_asm_op (size, FALSE), file);
  assemble_name (file, label);
  fputs("-__gcc_unwind_dbase", file);
}

#ifdef HAVE_AS_TLS
static void
rs6000_xcoff_encode_section_info (tree decl, rtx rtl, int first)
{
  rtx symbol;
  int flags;
  const char *symname;

  default_encode_section_info (decl, rtl, first);

  /* Careful not to prod global register variables.  */
  if (!MEM_P (rtl))
    return;
  symbol = XEXP (rtl, 0);
  if (!SYMBOL_REF_P (symbol))
    return;

  flags = SYMBOL_REF_FLAGS (symbol);

  if (TREE_CODE (decl) == VAR_DECL && DECL_THREAD_LOCAL_P (decl))
    flags &= ~SYMBOL_FLAG_HAS_BLOCK_INFO;

  SYMBOL_REF_FLAGS (symbol) = flags;

  /* Append mapping class to extern decls.  */
  symname = XSTR (symbol, 0);
  if (decl /* sync condition with assemble_external () */
      && DECL_P (decl) && DECL_EXTERNAL (decl) && TREE_PUBLIC (decl)
      && ((TREE_CODE (decl) == VAR_DECL && !DECL_THREAD_LOCAL_P (decl))
	  || TREE_CODE (decl) == FUNCTION_DECL)
      && symname[strlen (symname) - 1] != ']')
    {
      char *newname = (char *) alloca (strlen (symname) + 5);
      strcpy (newname, symname);
      strcat (newname, (TREE_CODE (decl) == FUNCTION_DECL
			? "[DS]" : "[UA]"));
      XSTR (symbol, 0) = ggc_strdup (newname);
    }
}
#endif /* HAVE_AS_TLS */
#endif /* TARGET_XCOFF */

void
rs6000_asm_weaken_decl (FILE *stream, tree decl,
			const char *name, const char *val)
{
  fputs ("\t.weak\t", stream);
  RS6000_OUTPUT_BASENAME (stream, name);
  if (decl && TREE_CODE (decl) == FUNCTION_DECL
      && DEFAULT_ABI == ABI_AIX && DOT_SYMBOLS)
    {
      if (TARGET_XCOFF)						
	fputs ("[DS]", stream);
#if TARGET_XCOFF && HAVE_GAS_HIDDEN
      if (TARGET_XCOFF)
	fputs (rs6000_xcoff_visibility (decl), stream);
#endif
      fputs ("\n\t.weak\t.", stream);
      RS6000_OUTPUT_BASENAME (stream, name);
    }
#if TARGET_XCOFF && HAVE_GAS_HIDDEN
  if (TARGET_XCOFF)
    fputs (rs6000_xcoff_visibility (decl), stream);
#endif
  fputc ('\n', stream);
  if (val)
    {
#ifdef ASM_OUTPUT_DEF
      ASM_OUTPUT_DEF (stream, name, val);
#endif
      if (decl && TREE_CODE (decl) == FUNCTION_DECL
	  && DEFAULT_ABI == ABI_AIX && DOT_SYMBOLS)
	{
	  fputs ("\t.set\t.", stream);
	  RS6000_OUTPUT_BASENAME (stream, name);
	  fputs (",.", stream);
	  RS6000_OUTPUT_BASENAME (stream, val);
	  fputc ('\n', stream);
	}
    }
}


/* Return true if INSN should not be copied.  */

static bool
rs6000_cannot_copy_insn_p (rtx_insn *insn)
{
  return recog_memoized (insn) >= 0
	 && get_attr_cannot_copy (insn);
}

/* Compute a (partial) cost for rtx X.  Return true if the complete
   cost has been computed, and false if subexpressions should be
   scanned.  In either case, *TOTAL contains the cost result.  */

static bool
rs6000_rtx_costs (rtx x, machine_mode mode, int outer_code,
		  int opno ATTRIBUTE_UNUSED, int *total, bool speed)
{
  int code = GET_CODE (x);

  switch (code)
    {
      /* On the RS/6000, if it is valid in the insn, it is free.  */
    case CONST_INT:
      if (((outer_code == SET
	    || outer_code == PLUS
	    || outer_code == MINUS)
	   && (satisfies_constraint_I (x)
	       || satisfies_constraint_L (x)))
	  || (outer_code == AND
	      && (satisfies_constraint_K (x)
		  || (mode == SImode
		      ? satisfies_constraint_L (x)
		      : satisfies_constraint_J (x))))
	  || ((outer_code == IOR || outer_code == XOR)
	      && (satisfies_constraint_K (x)
		  || (mode == SImode
		      ? satisfies_constraint_L (x)
		      : satisfies_constraint_J (x))))
	  || outer_code == ASHIFT
	  || outer_code == ASHIFTRT
	  || outer_code == LSHIFTRT
	  || outer_code == ROTATE
	  || outer_code == ROTATERT
	  || outer_code == ZERO_EXTRACT
	  || (outer_code == MULT
	      && satisfies_constraint_I (x))
	  || ((outer_code == DIV || outer_code == UDIV
	       || outer_code == MOD || outer_code == UMOD)
	      && exact_log2 (INTVAL (x)) >= 0)
	  || (outer_code == COMPARE
	      && (satisfies_constraint_I (x)
		  || satisfies_constraint_K (x)))
	  || ((outer_code == EQ || outer_code == NE)
	      && (satisfies_constraint_I (x)
		  || satisfies_constraint_K (x)
		  || (mode == SImode
		      ? satisfies_constraint_L (x)
		      : satisfies_constraint_J (x))))
	  || (outer_code == GTU
	      && satisfies_constraint_I (x))
	  || (outer_code == LTU
	      && satisfies_constraint_P (x)))
	{
	  *total = 0;
	  return true;
	}
      else if ((outer_code == PLUS
		&& reg_or_add_cint_operand (x, VOIDmode))
	       || (outer_code == MINUS
		   && reg_or_sub_cint_operand (x, VOIDmode))
	       || ((outer_code == SET
		    || outer_code == IOR
		    || outer_code == XOR)
		   && (INTVAL (x)
		       & ~ (unsigned HOST_WIDE_INT) 0xffffffff) == 0))
	{
	  *total = COSTS_N_INSNS (1);
	  return true;
	}
      /* FALLTHRU */

    case CONST_DOUBLE:
    case CONST_WIDE_INT:
    case CONST:
    case HIGH:
    case SYMBOL_REF:
      *total = !speed ? COSTS_N_INSNS (1) + 1 : COSTS_N_INSNS (2);
      return true;

    case MEM:
      /* When optimizing for size, MEM should be slightly more expensive
	 than generating address, e.g., (plus (reg) (const)).
	 L1 cache latency is about two instructions.  */
      *total = !speed ? COSTS_N_INSNS (1) + 1 : COSTS_N_INSNS (2);
      if (rs6000_slow_unaligned_access (mode, MEM_ALIGN (x)))
	*total += COSTS_N_INSNS (100);
      return true;

    case LABEL_REF:
      *total = 0;
      return true;

    case PLUS:
    case MINUS:
      if (FLOAT_MODE_P (mode))
	*total = rs6000_cost->fp;
      else
	*total = COSTS_N_INSNS (1);
      return false;

    case MULT:
      if (CONST_INT_P (XEXP (x, 1))
	  && satisfies_constraint_I (XEXP (x, 1)))
	{
	  if (INTVAL (XEXP (x, 1)) >= -256
	      && INTVAL (XEXP (x, 1)) <= 255)
	    *total = rs6000_cost->mulsi_const9;
	  else
	    *total = rs6000_cost->mulsi_const;
	}
      else if (mode == SFmode)
	*total = rs6000_cost->fp;
      else if (FLOAT_MODE_P (mode))
	*total = rs6000_cost->dmul;
      else if (mode == DImode)
	*total = rs6000_cost->muldi;
      else
	*total = rs6000_cost->mulsi;
      return false;

    case FMA:
      if (mode == SFmode)
	*total = rs6000_cost->fp;
      else
	*total = rs6000_cost->dmul;
      break;

    case DIV:
    case MOD:
      if (FLOAT_MODE_P (mode))
	{
	  *total = mode == DFmode ? rs6000_cost->ddiv
				  : rs6000_cost->sdiv;
	  return false;
	}
      /* FALLTHRU */

    case UDIV:
    case UMOD:
      if (CONST_INT_P (XEXP (x, 1))
	  && exact_log2 (INTVAL (XEXP (x, 1))) >= 0)
	{
	  if (code == DIV || code == MOD)
	    /* Shift, addze */
	    *total = COSTS_N_INSNS (2);
	  else
	    /* Shift */
	    *total = COSTS_N_INSNS (1);
	}
      else
	{
	  if (GET_MODE (XEXP (x, 1)) == DImode)
	    *total = rs6000_cost->divdi;
	  else
	    *total = rs6000_cost->divsi;
	}
      /* Add in shift and subtract for MOD unless we have a mod instruction. */
      if (!TARGET_MODULO && (code == MOD || code == UMOD))
	*total += COSTS_N_INSNS (2);
      return false;

    case CTZ:
      *total = COSTS_N_INSNS (TARGET_CTZ ? 1 : 4);
      return false;

    case FFS:
      *total = COSTS_N_INSNS (4);
      return false;

    case POPCOUNT:
      *total = COSTS_N_INSNS (TARGET_POPCNTD ? 1 : 6);
      return false;

    case PARITY:
      *total = COSTS_N_INSNS (TARGET_CMPB ? 2 : 6);
      return false;

    case NOT:
      if (outer_code == AND || outer_code == IOR || outer_code == XOR)
	*total = 0;
      else
	*total = COSTS_N_INSNS (1);
      return false;

    case AND:
      if (CONST_INT_P (XEXP (x, 1)))
	{
	  rtx left = XEXP (x, 0);
	  rtx_code left_code = GET_CODE (left);

	  /* rotate-and-mask: 1 insn.  */
	  if ((left_code == ROTATE
	       || left_code == ASHIFT
	       || left_code == LSHIFTRT)
	      && rs6000_is_valid_shift_mask (XEXP (x, 1), left, mode))
	    {
	      *total = rtx_cost (XEXP (left, 0), mode, left_code, 0, speed);
	      if (!CONST_INT_P (XEXP (left, 1)))
		*total += rtx_cost (XEXP (left, 1), SImode, left_code, 1, speed);
	      *total += COSTS_N_INSNS (1);
	      return true;
	    }

	  /* rotate-and-mask (no rotate), andi., andis.: 1 insn.  */
	  HOST_WIDE_INT val = INTVAL (XEXP (x, 1));
	  if (rs6000_is_valid_and_mask (XEXP (x, 1), mode)
	      || (val & 0xffff) == val
	      || (val & 0xffff0000) == val
	      || ((val & 0xffff) == 0 && mode == SImode))
	    {
	      *total = rtx_cost (left, mode, AND, 0, speed);
	      *total += COSTS_N_INSNS (1);
	      return true;
	    }

	  /* 2 insns.  */
	  if (rs6000_is_valid_2insn_and (XEXP (x, 1), mode))
	    {
	      *total = rtx_cost (left, mode, AND, 0, speed);
	      *total += COSTS_N_INSNS (2);
	      return true;
	    }
	}

      *total = COSTS_N_INSNS (1);
      return false;

    case IOR:
      /* FIXME */
      *total = COSTS_N_INSNS (1);
      return true;

    case CLZ:
    case XOR:
    case ZERO_EXTRACT:
      *total = COSTS_N_INSNS (1);
      return false;

    case ASHIFT:
      /* The EXTSWSLI instruction is a combined instruction.  Don't count both
	 the sign extend and shift separately within the insn.  */
      if (TARGET_EXTSWSLI && mode == DImode
	  && GET_CODE (XEXP (x, 0)) == SIGN_EXTEND
	  && GET_MODE (XEXP (XEXP (x, 0), 0)) == SImode)
	{
	  *total = 0;
	  return false;
	}
      /* fall through */
	  
    case ASHIFTRT:
    case LSHIFTRT:
    case ROTATE:
    case ROTATERT:
      /* Handle mul_highpart.  */
      if (outer_code == TRUNCATE
	  && GET_CODE (XEXP (x, 0)) == MULT)
	{
	  if (mode == DImode)
	    *total = rs6000_cost->muldi;
	  else
	    *total = rs6000_cost->mulsi;
	  return true;
	}
      else if (outer_code == AND)
	*total = 0;
      else
	*total = COSTS_N_INSNS (1);
      return false;

    case SIGN_EXTEND:
    case ZERO_EXTEND:
      if (MEM_P (XEXP (x, 0)))
	*total = 0;
      else
	*total = COSTS_N_INSNS (1);
      return false;

    case COMPARE:
    case NEG:
    case ABS:
      if (!FLOAT_MODE_P (mode))
	{
	  *total = COSTS_N_INSNS (1);
	  return false;
	}
      /* FALLTHRU */

    case FLOAT:
    case UNSIGNED_FLOAT:
    case FIX:
    case UNSIGNED_FIX:
    case FLOAT_TRUNCATE:
      *total = rs6000_cost->fp;
      return false;

    case FLOAT_EXTEND:
      if (mode == DFmode)
	*total = rs6000_cost->sfdf_convert;
      else
	*total = rs6000_cost->fp;
      return false;

    case CALL:
    case IF_THEN_ELSE:
      if (!speed)
	{
	  *total = COSTS_N_INSNS (1);
	  return true;
	}
      else if (FLOAT_MODE_P (mode) && TARGET_PPC_GFXOPT && TARGET_HARD_FLOAT)
	{
	  *total = rs6000_cost->fp;
	  return false;
	}
      break;

    case NE:
    case EQ:
    case GTU:
    case LTU:
      /* Carry bit requires mode == Pmode.
	 NEG or PLUS already counted so only add one.  */
      if (mode == Pmode
	  && (outer_code == NEG || outer_code == PLUS))
	{
	  *total = COSTS_N_INSNS (1);
	  return true;
	}
      /* FALLTHRU */

    case GT:
    case LT:
    case UNORDERED:
      if (outer_code == SET)
	{
	  if (XEXP (x, 1) == const0_rtx)
	    {
	      *total = COSTS_N_INSNS (2);
	      return true;
	    }
	  else
	    {
	      *total = COSTS_N_INSNS (3);
	      return false;
	    }
	}
      /* CC COMPARE.  */
      if (outer_code == COMPARE)
	{
	  *total = 0;
	  return true;
	}
      break;

    default:
      break;
    }

  return false;
}

/* Debug form of r6000_rtx_costs that is selected if -mdebug=cost.  */

static bool
rs6000_debug_rtx_costs (rtx x, machine_mode mode, int outer_code,
			int opno, int *total, bool speed)
{
  bool ret = rs6000_rtx_costs (x, mode, outer_code, opno, total, speed);

  fprintf (stderr,
	   "\nrs6000_rtx_costs, return = %s, mode = %s, outer_code = %s, "
	   "opno = %d, total = %d, speed = %s, x:\n",
	   ret ? "complete" : "scan inner",
	   GET_MODE_NAME (mode),
	   GET_RTX_NAME (outer_code),
	   opno,
	   *total,
	   speed ? "true" : "false");

  debug_rtx (x);

  return ret;
}

static int
rs6000_insn_cost (rtx_insn *insn, bool speed)
{
  if (recog_memoized (insn) < 0)
    return 0;

  /* If we are optimizing for size, just use the length.  */
  if (!speed)
    return get_attr_length (insn);

  /* Use the cost if provided.  */
  int cost = get_attr_cost (insn);
  if (cost > 0)
    return cost;

  /* If the insn tells us how many insns there are, use that.  Otherwise use
     the length/4.  Adjust the insn length to remove the extra size that
     prefixed instructions take.  */
  int n = get_attr_num_insns (insn);
  if (n == 0)
    {
      int length = get_attr_length (insn);
      if (get_attr_prefixed (insn) == PREFIXED_YES)
	{
	  int adjust = 0;
	  ADJUST_INSN_LENGTH (insn, adjust);
	  length -= adjust;
	}

      n = length / 4;
    }

  enum attr_type type = get_attr_type (insn);

  switch (type)
    {
    case TYPE_LOAD:
    case TYPE_FPLOAD:
    case TYPE_VECLOAD:
      cost = COSTS_N_INSNS (n + 1);
      break;

    case TYPE_MUL:
      switch (get_attr_size (insn))
	{
	case SIZE_8:
	  cost = COSTS_N_INSNS (n - 1) + rs6000_cost->mulsi_const9;
	  break;
	case SIZE_16:
	  cost = COSTS_N_INSNS (n - 1) + rs6000_cost->mulsi_const;
	  break;
	case SIZE_32:
	  cost = COSTS_N_INSNS (n - 1) + rs6000_cost->mulsi;
	  break;
	case SIZE_64:
	  cost = COSTS_N_INSNS (n - 1) + rs6000_cost->muldi;
	  break;
	default:
	  gcc_unreachable ();
	}
      break;
    case TYPE_DIV:
      switch (get_attr_size (insn))
	{
	case SIZE_32:
	  cost = COSTS_N_INSNS (n - 1) + rs6000_cost->divsi;
	  break;
	case SIZE_64:
	  cost = COSTS_N_INSNS (n - 1) + rs6000_cost->divdi;
	  break;
	default:
	  gcc_unreachable ();
	}
      break;

    case TYPE_FP:
      cost = n * rs6000_cost->fp;
      break;
    case TYPE_DMUL:
      cost = n * rs6000_cost->dmul;
      break;
    case TYPE_SDIV:
      cost = n * rs6000_cost->sdiv;
      break;
    case TYPE_DDIV:
      cost = n * rs6000_cost->ddiv;
      break;

    case TYPE_SYNC:
    case TYPE_LOAD_L:
    case TYPE_MFCR:
    case TYPE_MFCRF:
      cost = COSTS_N_INSNS (n + 2);
      break;

    default:
      cost = COSTS_N_INSNS (n);
    }

  return cost;
}

/* Debug form of ADDRESS_COST that is selected if -mdebug=cost.  */

static int
rs6000_debug_address_cost (rtx x, machine_mode mode,
			   addr_space_t as, bool speed)
{
  int ret = TARGET_ADDRESS_COST (x, mode, as, speed);

  fprintf (stderr, "\nrs6000_address_cost, return = %d, speed = %s, x:\n",
	   ret, speed ? "true" : "false");
  debug_rtx (x);

  return ret;
}


/* A C expression returning the cost of moving data from a register of class
   CLASS1 to one of CLASS2.  */

static int
rs6000_register_move_cost (machine_mode mode,
			   reg_class_t from, reg_class_t to)
{
  int ret;
  reg_class_t rclass;

  if (TARGET_DEBUG_COST)
    dbg_cost_ctrl++;

  /* If we have VSX, we can easily move between FPR or Altivec registers,
     otherwise we can only easily move within classes.
     Do this first so we give best-case answers for union classes
     containing both gprs and vsx regs.  */
  HARD_REG_SET to_vsx, from_vsx;
  to_vsx = reg_class_contents[to] & reg_class_contents[VSX_REGS];
  from_vsx = reg_class_contents[from] & reg_class_contents[VSX_REGS];
  if (!hard_reg_set_empty_p (to_vsx)
      && !hard_reg_set_empty_p (from_vsx)
      && (TARGET_VSX
	  || hard_reg_set_intersect_p (to_vsx, from_vsx)))
    {
      int reg = FIRST_FPR_REGNO;
      if (TARGET_VSX
	  || (TEST_HARD_REG_BIT (to_vsx, FIRST_ALTIVEC_REGNO)
	      && TEST_HARD_REG_BIT (from_vsx, FIRST_ALTIVEC_REGNO)))
	reg = FIRST_ALTIVEC_REGNO;
      ret = 2 * hard_regno_nregs (reg, mode);
    }

  /*  Moves from/to GENERAL_REGS.  */
  else if ((rclass = from, reg_classes_intersect_p (to, GENERAL_REGS))
	   || (rclass = to, reg_classes_intersect_p (from, GENERAL_REGS)))
    {
      if (rclass == FLOAT_REGS || rclass == ALTIVEC_REGS || rclass == VSX_REGS)
	{
	  if (TARGET_DIRECT_MOVE)
	    {
	      /* Keep the cost for direct moves above that for within
		 a register class even if the actual processor cost is
		 comparable.  We do this because a direct move insn
		 can't be a nop, whereas with ideal register
		 allocation a move within the same class might turn
		 out to be a nop.  */
	      if (rs6000_tune == PROCESSOR_POWER9
		  || rs6000_tune == PROCESSOR_POWER10)
		ret = 3 * hard_regno_nregs (FIRST_GPR_REGNO, mode);
	      else
		ret = 4 * hard_regno_nregs (FIRST_GPR_REGNO, mode);
	      /* SFmode requires a conversion when moving between gprs
		 and vsx.  */
	      if (mode == SFmode)
		ret += 2;
	    }
	  else
	    ret = (rs6000_memory_move_cost (mode, rclass, false)
		   + rs6000_memory_move_cost (mode, GENERAL_REGS, false));
	}

      /* It's more expensive to move CR_REGS than CR0_REGS because of the
	 shift.  */
      else if (rclass == CR_REGS)
	ret = 4;

      /* For those processors that have slow LR/CTR moves, make them more
         expensive than memory in order to bias spills to memory .*/
      else if ((rs6000_tune == PROCESSOR_POWER6
		|| rs6000_tune == PROCESSOR_POWER7
		|| rs6000_tune == PROCESSOR_POWER8
		|| rs6000_tune == PROCESSOR_POWER9)
	       && reg_class_subset_p (rclass, SPECIAL_REGS))
        ret = 6 * hard_regno_nregs (FIRST_GPR_REGNO, mode);

      else
	/* A move will cost one instruction per GPR moved.  */
	ret = 2 * hard_regno_nregs (FIRST_GPR_REGNO, mode);
    }

  /* Everything else has to go through GENERAL_REGS.  */
  else
    ret = (rs6000_register_move_cost (mode, GENERAL_REGS, to)
	   + rs6000_register_move_cost (mode, from, GENERAL_REGS));

  if (TARGET_DEBUG_COST)
    {
      if (dbg_cost_ctrl == 1)
	fprintf (stderr,
		 "rs6000_register_move_cost: ret=%d, mode=%s, from=%s, to=%s\n",
		 ret, GET_MODE_NAME (mode), reg_class_names[from],
		 reg_class_names[to]);
      dbg_cost_ctrl--;
    }

  return ret;
}

/* A C expressions returning the cost of moving data of MODE from a register to
   or from memory.  */

static int
rs6000_memory_move_cost (machine_mode mode, reg_class_t rclass,
			 bool in ATTRIBUTE_UNUSED)
{
  int ret;

  if (TARGET_DEBUG_COST)
    dbg_cost_ctrl++;

  if (reg_classes_intersect_p (rclass, GENERAL_REGS))
    ret = 4 * hard_regno_nregs (0, mode);
  else if ((reg_classes_intersect_p (rclass, FLOAT_REGS)
	    || reg_classes_intersect_p (rclass, VSX_REGS)))
    ret = 4 * hard_regno_nregs (32, mode);
  else if (reg_classes_intersect_p (rclass, ALTIVEC_REGS))
    ret = 4 * hard_regno_nregs (FIRST_ALTIVEC_REGNO, mode);
  else
    ret = 4 + rs6000_register_move_cost (mode, rclass, GENERAL_REGS);

  if (TARGET_DEBUG_COST)
    {
      if (dbg_cost_ctrl == 1)
	fprintf (stderr,
		 "rs6000_memory_move_cost: ret=%d, mode=%s, rclass=%s, in=%d\n",
		 ret, GET_MODE_NAME (mode), reg_class_names[rclass], in);
      dbg_cost_ctrl--;
    }

  return ret;
}

/* Implement TARGET_IRA_CHANGE_PSEUDO_ALLOCNO_CLASS.

   The register allocator chooses GEN_OR_VSX_REGS for the allocno
   class if GENERAL_REGS and VSX_REGS cost is lower than the memory
   cost.  This happens a lot when TARGET_DIRECT_MOVE makes the register
   move cost between GENERAL_REGS and VSX_REGS low.

   It might seem reasonable to use a union class.  After all, if usage
   of vsr is low and gpr high, it might make sense to spill gpr to vsr
   rather than memory.  However, in cases where register pressure of
   both is high, like the cactus_adm spec test, allowing
   GEN_OR_VSX_REGS as the allocno class results in bad decisions in
   the first scheduling pass.  This is partly due to an allocno of
   GEN_OR_VSX_REGS wrongly contributing to the GENERAL_REGS pressure
   class, which gives too high a pressure for GENERAL_REGS and too low
   for VSX_REGS.  So, force a choice of the subclass here.

   The best class is also the union if GENERAL_REGS and VSX_REGS have
   the same cost.  In that case we do use GEN_OR_VSX_REGS as the
   allocno class, since trying to narrow down the class by regno mode
   is prone to error.  For example, SImode is allowed in VSX regs and
   in some cases (eg. gcc.target/powerpc/p9-xxbr-3.c do_bswap32_vect)
   it would be wrong to choose an allocno of GENERAL_REGS based on
   SImode.  */

static reg_class_t
rs6000_ira_change_pseudo_allocno_class (int regno ATTRIBUTE_UNUSED,
					reg_class_t allocno_class,
					reg_class_t best_class)
{
  switch (allocno_class)
    {
    case GEN_OR_VSX_REGS:
      /* best_class must be a subset of allocno_class.  */
      gcc_checking_assert (best_class == GEN_OR_VSX_REGS
			   || best_class == GEN_OR_FLOAT_REGS
			   || best_class == VSX_REGS
			   || best_class == ALTIVEC_REGS
			   || best_class == FLOAT_REGS
			   || best_class == GENERAL_REGS
			   || best_class == BASE_REGS);
      /* Use best_class but choose wider classes when copying from the
	 wider class to best_class is cheap.  This mimics IRA choice
	 of allocno class.  */
      if (best_class == BASE_REGS)
	return GENERAL_REGS;
      if (TARGET_VSX
	  && (best_class == FLOAT_REGS || best_class == ALTIVEC_REGS))
	return VSX_REGS;
      return best_class;

    default:
      break;
    }

  return allocno_class;
}

/* Returns a code for a target-specific builtin that implements
   reciprocal of the function, or NULL_TREE if not available.  */

static tree
rs6000_builtin_reciprocal (tree fndecl)
{
  switch (DECL_MD_FUNCTION_CODE (fndecl))
    {
    case VSX_BUILTIN_XVSQRTDP:
      if (!RS6000_RECIP_AUTO_RSQRTE_P (V2DFmode))
	return NULL_TREE;

      return rs6000_builtin_decls[VSX_BUILTIN_RSQRT_2DF];

    case VSX_BUILTIN_XVSQRTSP:
      if (!RS6000_RECIP_AUTO_RSQRTE_P (V4SFmode))
	return NULL_TREE;

      return rs6000_builtin_decls[VSX_BUILTIN_RSQRT_4SF];

    default:
      return NULL_TREE;
    }
}

/* Load up a constant.  If the mode is a vector mode, splat the value across
   all of the vector elements.  */

static rtx
rs6000_load_constant_and_splat (machine_mode mode, REAL_VALUE_TYPE dconst)
{
  rtx reg;

  if (mode == SFmode || mode == DFmode)
    {
      rtx d = const_double_from_real_value (dconst, mode);
      reg = force_reg (mode, d);
    }
  else if (mode == V4SFmode)
    {
      rtx d = const_double_from_real_value (dconst, SFmode);
      rtvec v = gen_rtvec (4, d, d, d, d);
      reg = gen_reg_rtx (mode);
      rs6000_expand_vector_init (reg, gen_rtx_PARALLEL (mode, v));
    }
  else if (mode == V2DFmode)
    {
      rtx d = const_double_from_real_value (dconst, DFmode);
      rtvec v = gen_rtvec (2, d, d);
      reg = gen_reg_rtx (mode);
      rs6000_expand_vector_init (reg, gen_rtx_PARALLEL (mode, v));
    }
  else
    gcc_unreachable ();

  return reg;
}

/* Generate an FMA instruction.  */

static void
rs6000_emit_madd (rtx target, rtx m1, rtx m2, rtx a)
{
  machine_mode mode = GET_MODE (target);
  rtx dst;

  dst = expand_ternary_op (mode, fma_optab, m1, m2, a, target, 0);
  gcc_assert (dst != NULL);

  if (dst != target)
    emit_move_insn (target, dst);
}

/* Generate a FNMSUB instruction: dst = -fma(m1, m2, -a).  */

static void
rs6000_emit_nmsub (rtx dst, rtx m1, rtx m2, rtx a)
{
  machine_mode mode = GET_MODE (dst);
  rtx r;

  /* This is a tad more complicated, since the fnma_optab is for
     a different expression: fma(-m1, m2, a), which is the same
     thing except in the case of signed zeros.

     Fortunately we know that if FMA is supported that FNMSUB is
     also supported in the ISA.  Just expand it directly.  */

  gcc_assert (optab_handler (fma_optab, mode) != CODE_FOR_nothing);

  r = gen_rtx_NEG (mode, a);
  r = gen_rtx_FMA (mode, m1, m2, r);
  r = gen_rtx_NEG (mode, r);
  emit_insn (gen_rtx_SET (dst, r));
}

/* Newton-Raphson approximation of floating point divide DST = N/D.  If NOTE_P,
   add a reg_note saying that this was a division.  Support both scalar and
   vector divide.  Assumes no trapping math and finite arguments.  */

void
rs6000_emit_swdiv (rtx dst, rtx n, rtx d, bool note_p)
{
  machine_mode mode = GET_MODE (dst);
  rtx one, x0, e0, x1, xprev, eprev, xnext, enext, u, v;
  int i;

  /* Low precision estimates guarantee 5 bits of accuracy.  High
     precision estimates guarantee 14 bits of accuracy.  SFmode
     requires 23 bits of accuracy.  DFmode requires 52 bits of
     accuracy.  Each pass at least doubles the accuracy, leading
     to the following.  */
  int passes = (TARGET_RECIP_PRECISION) ? 1 : 3;
  if (mode == DFmode || mode == V2DFmode)
    passes++;

  enum insn_code code = optab_handler (smul_optab, mode);
  insn_gen_fn gen_mul = GEN_FCN (code);

  gcc_assert (code != CODE_FOR_nothing);

  one = rs6000_load_constant_and_splat (mode, dconst1);

  /* x0 = 1./d estimate */
  x0 = gen_reg_rtx (mode);
  emit_insn (gen_rtx_SET (x0, gen_rtx_UNSPEC (mode, gen_rtvec (1, d),
					      UNSPEC_FRES)));

  /* Each iteration but the last calculates x_(i+1) = x_i * (2 - d * x_i).  */
  if (passes > 1) {

    /* e0 = 1. - d * x0  */
    e0 = gen_reg_rtx (mode);
    rs6000_emit_nmsub (e0, d, x0, one);

    /* x1 = x0 + e0 * x0  */
    x1 = gen_reg_rtx (mode);
    rs6000_emit_madd (x1, e0, x0, x0);

    for (i = 0, xprev = x1, eprev = e0; i < passes - 2;
	 ++i, xprev = xnext, eprev = enext) {
      
      /* enext = eprev * eprev  */
      enext = gen_reg_rtx (mode);
      emit_insn (gen_mul (enext, eprev, eprev));

      /* xnext = xprev + enext * xprev  */
      xnext = gen_reg_rtx (mode);
      rs6000_emit_madd (xnext, enext, xprev, xprev);
    }

  } else
    xprev = x0;

  /* The last iteration calculates x_(i+1) = n * x_i * (2 - d * x_i).  */

  /* u = n * xprev  */
  u = gen_reg_rtx (mode);
  emit_insn (gen_mul (u, n, xprev));

  /* v = n - (d * u)  */
  v = gen_reg_rtx (mode);
  rs6000_emit_nmsub (v, d, u, n);

  /* dst = (v * xprev) + u  */
  rs6000_emit_madd (dst, v, xprev, u);

  if (note_p)
    add_reg_note (get_last_insn (), REG_EQUAL, gen_rtx_DIV (mode, n, d));
}

/* Goldschmidt's Algorithm for single/double-precision floating point
   sqrt and rsqrt.  Assumes no trapping math and finite arguments.  */

void
rs6000_emit_swsqrt (rtx dst, rtx src, bool recip)
{
  machine_mode mode = GET_MODE (src);
  rtx e = gen_reg_rtx (mode);
  rtx g = gen_reg_rtx (mode);
  rtx h = gen_reg_rtx (mode);

  /* Low precision estimates guarantee 5 bits of accuracy.  High
     precision estimates guarantee 14 bits of accuracy.  SFmode
     requires 23 bits of accuracy.  DFmode requires 52 bits of
     accuracy.  Each pass at least doubles the accuracy, leading
     to the following.  */
  int passes = (TARGET_RECIP_PRECISION) ? 1 : 3;
  if (mode == DFmode || mode == V2DFmode)
    passes++;

  int i;
  rtx mhalf;
  enum insn_code code = optab_handler (smul_optab, mode);
  insn_gen_fn gen_mul = GEN_FCN (code);

  gcc_assert (code != CODE_FOR_nothing);

  mhalf = rs6000_load_constant_and_splat (mode, dconsthalf);

  /* e = rsqrt estimate */
  emit_insn (gen_rtx_SET (e, gen_rtx_UNSPEC (mode, gen_rtvec (1, src),
					     UNSPEC_RSQRT)));

  /* If (src == 0.0) filter infinity to prevent NaN for sqrt(0.0).  */
  if (!recip)
    {
      rtx zero = force_reg (mode, CONST0_RTX (mode));

      if (mode == SFmode)
	{
	  rtx target = emit_conditional_move (e, GT, src, zero, mode,
					      e, zero, mode, 0);
	  if (target != e)
	    emit_move_insn (e, target);
	}
      else
	{
	  rtx cond = gen_rtx_GT (VOIDmode, e, zero);
	  rs6000_emit_vector_cond_expr (e, e, zero, cond, src, zero);
	}
    }

  /* g = sqrt estimate.  */
  emit_insn (gen_mul (g, e, src));
  /* h = 1/(2*sqrt) estimate.  */
  emit_insn (gen_mul (h, e, mhalf));

  if (recip)
    {
      if (passes == 1)
	{
	  rtx t = gen_reg_rtx (mode);
	  rs6000_emit_nmsub (t, g, h, mhalf);
	  /* Apply correction directly to 1/rsqrt estimate.  */
	  rs6000_emit_madd (dst, e, t, e);
	}
      else
	{
	  for (i = 0; i < passes; i++)
	    {
	      rtx t1 = gen_reg_rtx (mode);
	      rtx g1 = gen_reg_rtx (mode);
	      rtx h1 = gen_reg_rtx (mode);

	      rs6000_emit_nmsub (t1, g, h, mhalf);
	      rs6000_emit_madd (g1, g, t1, g);
	      rs6000_emit_madd (h1, h, t1, h);

	      g = g1;
	      h = h1;
	    }
	  /* Multiply by 2 for 1/rsqrt.  */
	  emit_insn (gen_add3_insn (dst, h, h));
	}
    }
  else
    {
      rtx t = gen_reg_rtx (mode);
      rs6000_emit_nmsub (t, g, h, mhalf);
      rs6000_emit_madd (dst, g, t, g);
    }

  return;
}

/* Emit popcount intrinsic on TARGET_POPCNTB (Power5) and TARGET_POPCNTD
   (Power7) targets.  DST is the target, and SRC is the argument operand.  */

void
rs6000_emit_popcount (rtx dst, rtx src)
{
  machine_mode mode = GET_MODE (dst);
  rtx tmp1, tmp2;

  /* Use the PPC ISA 2.06 popcnt{w,d} instruction if we can.  */
  if (TARGET_POPCNTD)
    {
      if (mode == SImode)
	emit_insn (gen_popcntdsi2 (dst, src));
      else
	emit_insn (gen_popcntddi2 (dst, src));
      return;
    }

  tmp1 = gen_reg_rtx (mode);

  if (mode == SImode)
    {
      emit_insn (gen_popcntbsi2 (tmp1, src));
      tmp2 = expand_mult (SImode, tmp1, GEN_INT (0x01010101),
			   NULL_RTX, 0);
      tmp2 = force_reg (SImode, tmp2);
      emit_insn (gen_lshrsi3 (dst, tmp2, GEN_INT (24)));
    }
  else
    {
      emit_insn (gen_popcntbdi2 (tmp1, src));
      tmp2 = expand_mult (DImode, tmp1,
			  GEN_INT ((HOST_WIDE_INT)
				   0x01010101 << 32 | 0x01010101),
			  NULL_RTX, 0);
      tmp2 = force_reg (DImode, tmp2);
      emit_insn (gen_lshrdi3 (dst, tmp2, GEN_INT (56)));
    }
}


/* Emit parity intrinsic on TARGET_POPCNTB targets.  DST is the
   target, and SRC is the argument operand.  */

void
rs6000_emit_parity (rtx dst, rtx src)
{
  machine_mode mode = GET_MODE (dst);
  rtx tmp;

  tmp = gen_reg_rtx (mode);

  /* Use the PPC ISA 2.05 prtyw/prtyd instruction if we can.  */
  if (TARGET_CMPB)
    {
      if (mode == SImode)
	{
	  emit_insn (gen_popcntbsi2 (tmp, src));
	  emit_insn (gen_paritysi2_cmpb (dst, tmp));
	}
      else
	{
	  emit_insn (gen_popcntbdi2 (tmp, src));
	  emit_insn (gen_paritydi2_cmpb (dst, tmp));
	}
      return;
    }

  if (mode == SImode)
    {
      /* Is mult+shift >= shift+xor+shift+xor?  */
      if (rs6000_cost->mulsi_const >= COSTS_N_INSNS (3))
	{
	  rtx tmp1, tmp2, tmp3, tmp4;

	  tmp1 = gen_reg_rtx (SImode);
	  emit_insn (gen_popcntbsi2 (tmp1, src));

	  tmp2 = gen_reg_rtx (SImode);
	  emit_insn (gen_lshrsi3 (tmp2, tmp1, GEN_INT (16)));
	  tmp3 = gen_reg_rtx (SImode);
	  emit_insn (gen_xorsi3 (tmp3, tmp1, tmp2));

	  tmp4 = gen_reg_rtx (SImode);
	  emit_insn (gen_lshrsi3 (tmp4, tmp3, GEN_INT (8)));
	  emit_insn (gen_xorsi3 (tmp, tmp3, tmp4));
	}
      else
	rs6000_emit_popcount (tmp, src);
      emit_insn (gen_andsi3 (dst, tmp, const1_rtx));
    }
  else
    {
      /* Is mult+shift >= shift+xor+shift+xor+shift+xor?  */
      if (rs6000_cost->muldi >= COSTS_N_INSNS (5))
	{
	  rtx tmp1, tmp2, tmp3, tmp4, tmp5, tmp6;

	  tmp1 = gen_reg_rtx (DImode);
	  emit_insn (gen_popcntbdi2 (tmp1, src));

	  tmp2 = gen_reg_rtx (DImode);
	  emit_insn (gen_lshrdi3 (tmp2, tmp1, GEN_INT (32)));
	  tmp3 = gen_reg_rtx (DImode);
	  emit_insn (gen_xordi3 (tmp3, tmp1, tmp2));

	  tmp4 = gen_reg_rtx (DImode);
	  emit_insn (gen_lshrdi3 (tmp4, tmp3, GEN_INT (16)));
	  tmp5 = gen_reg_rtx (DImode);
	  emit_insn (gen_xordi3 (tmp5, tmp3, tmp4));

	  tmp6 = gen_reg_rtx (DImode);
	  emit_insn (gen_lshrdi3 (tmp6, tmp5, GEN_INT (8)));
	  emit_insn (gen_xordi3 (tmp, tmp5, tmp6));
	}
      else
        rs6000_emit_popcount (tmp, src);
      emit_insn (gen_anddi3 (dst, tmp, const1_rtx));
    }
}

/* Expand an Altivec constant permutation for little endian mode.
   OP0 and OP1 are the input vectors and TARGET is the output vector.
   SEL specifies the constant permutation vector.

   There are two issues: First, the two input operands must be
   swapped so that together they form a double-wide array in LE
   order.  Second, the vperm instruction has surprising behavior
   in LE mode:  it interprets the elements of the source vectors
   in BE mode ("left to right") and interprets the elements of
   the destination vector in LE mode ("right to left").  To
   correct for this, we must subtract each element of the permute
   control vector from 31.

   For example, suppose we want to concatenate vr10 = {0, 1, 2, 3}
   with vr11 = {4, 5, 6, 7} and extract {0, 2, 4, 6} using a vperm.
   We place {0,1,2,3,8,9,10,11,16,17,18,19,24,25,26,27} in vr12 to
   serve as the permute control vector.  Then, in BE mode,

     vperm 9,10,11,12

   places the desired result in vr9.  However, in LE mode the 
   vector contents will be

     vr10 = 00000003 00000002 00000001 00000000
     vr11 = 00000007 00000006 00000005 00000004

   The result of the vperm using the same permute control vector is

     vr9  = 05000000 07000000 01000000 03000000

   That is, the leftmost 4 bytes of vr10 are interpreted as the
   source for the rightmost 4 bytes of vr9, and so on.

   If we change the permute control vector to

     vr12 = {31,20,29,28,23,22,21,20,15,14,13,12,7,6,5,4}

   and issue

     vperm 9,11,10,12

   we get the desired

   vr9  = 00000006 00000004 00000002 00000000.  */

static void
altivec_expand_vec_perm_const_le (rtx target, rtx op0, rtx op1,
				  const vec_perm_indices &sel)
{
  unsigned int i;
  rtx perm[16];
  rtx constv, unspec;

  /* Unpack and adjust the constant selector.  */
  for (i = 0; i < 16; ++i)
    {
      unsigned int elt = 31 - (sel[i] & 31);
      perm[i] = GEN_INT (elt);
    }

  /* Expand to a permute, swapping the inputs and using the
     adjusted selector.  */
  if (!REG_P (op0))
    op0 = force_reg (V16QImode, op0);
  if (!REG_P (op1))
    op1 = force_reg (V16QImode, op1);

  constv = gen_rtx_CONST_VECTOR (V16QImode, gen_rtvec_v (16, perm));
  constv = force_reg (V16QImode, constv);
  unspec = gen_rtx_UNSPEC (V16QImode, gen_rtvec (3, op1, op0, constv),
			   UNSPEC_VPERM);
  if (!REG_P (target))
    {
      rtx tmp = gen_reg_rtx (V16QImode);
      emit_move_insn (tmp, unspec);
      unspec = tmp;
    }

  emit_move_insn (target, unspec);
}

/* Similarly to altivec_expand_vec_perm_const_le, we must adjust the
   permute control vector.  But here it's not a constant, so we must
   generate a vector NAND or NOR to do the adjustment.  */

void
altivec_expand_vec_perm_le (rtx operands[4])
{
  rtx notx, iorx, unspec;
  rtx target = operands[0];
  rtx op0 = operands[1];
  rtx op1 = operands[2];
  rtx sel = operands[3];
  rtx tmp = target;
  rtx norreg = gen_reg_rtx (V16QImode);
  machine_mode mode = GET_MODE (target);

  /* Get everything in regs so the pattern matches.  */
  if (!REG_P (op0))
    op0 = force_reg (mode, op0);
  if (!REG_P (op1))
    op1 = force_reg (mode, op1);
  if (!REG_P (sel))
    sel = force_reg (V16QImode, sel);
  if (!REG_P (target))
    tmp = gen_reg_rtx (mode);

  if (TARGET_P9_VECTOR)
    {
      unspec = gen_rtx_UNSPEC (mode, gen_rtvec (3, op1, op0, sel),
			       UNSPEC_VPERMR);
    }
  else
    {
      /* Invert the selector with a VNAND if available, else a VNOR.
	 The VNAND is preferred for future fusion opportunities.  */
      notx = gen_rtx_NOT (V16QImode, sel);
      iorx = (TARGET_P8_VECTOR
	      ? gen_rtx_IOR (V16QImode, notx, notx)
	      : gen_rtx_AND (V16QImode, notx, notx));
      emit_insn (gen_rtx_SET (norreg, iorx));

      /* Permute with operands reversed and adjusted selector.  */
      unspec = gen_rtx_UNSPEC (mode, gen_rtvec (3, op1, op0, norreg),
			       UNSPEC_VPERM);
    }

  /* Copy into target, possibly by way of a register.  */
  if (!REG_P (target))
    {
      emit_move_insn (tmp, unspec);
      unspec = tmp;
    }

  emit_move_insn (target, unspec);
}

/* Expand an Altivec constant permutation.  Return true if we match
   an efficient implementation; false to fall back to VPERM.

   OP0 and OP1 are the input vectors and TARGET is the output vector.
   SEL specifies the constant permutation vector.  */

static bool
altivec_expand_vec_perm_const (rtx target, rtx op0, rtx op1,
			       const vec_perm_indices &sel)
{
  struct altivec_perm_insn {
    HOST_WIDE_INT mask;
    enum insn_code impl;
    unsigned char perm[16];
  };
  static const struct altivec_perm_insn patterns[] = {
    { OPTION_MASK_ALTIVEC, CODE_FOR_altivec_vpkuhum_direct,
      {  1,  3,  5,  7,  9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29, 31 } },
    { OPTION_MASK_ALTIVEC, CODE_FOR_altivec_vpkuwum_direct,
      {  2,  3,  6,  7, 10, 11, 14, 15, 18, 19, 22, 23, 26, 27, 30, 31 } },
    { OPTION_MASK_ALTIVEC, 
      (BYTES_BIG_ENDIAN ? CODE_FOR_altivec_vmrghb_direct
       : CODE_FOR_altivec_vmrglb_direct),
      {  0, 16,  1, 17,  2, 18,  3, 19,  4, 20,  5, 21,  6, 22,  7, 23 } },
    { OPTION_MASK_ALTIVEC,
      (BYTES_BIG_ENDIAN ? CODE_FOR_altivec_vmrghh_direct
       : CODE_FOR_altivec_vmrglh_direct),
      {  0,  1, 16, 17,  2,  3, 18, 19,  4,  5, 20, 21,  6,  7, 22, 23 } },
    { OPTION_MASK_ALTIVEC,
      (BYTES_BIG_ENDIAN ? CODE_FOR_altivec_vmrghw_direct
       : CODE_FOR_altivec_vmrglw_direct),
      {  0,  1,  2,  3, 16, 17, 18, 19,  4,  5,  6,  7, 20, 21, 22, 23 } },
    { OPTION_MASK_ALTIVEC,
      (BYTES_BIG_ENDIAN ? CODE_FOR_altivec_vmrglb_direct
       : CODE_FOR_altivec_vmrghb_direct),
      {  8, 24,  9, 25, 10, 26, 11, 27, 12, 28, 13, 29, 14, 30, 15, 31 } },
    { OPTION_MASK_ALTIVEC,
      (BYTES_BIG_ENDIAN ? CODE_FOR_altivec_vmrglh_direct
       : CODE_FOR_altivec_vmrghh_direct),
      {  8,  9, 24, 25, 10, 11, 26, 27, 12, 13, 28, 29, 14, 15, 30, 31 } },
    { OPTION_MASK_ALTIVEC,
      (BYTES_BIG_ENDIAN ? CODE_FOR_altivec_vmrglw_direct
       : CODE_FOR_altivec_vmrghw_direct),
      {  8,  9, 10, 11, 24, 25, 26, 27, 12, 13, 14, 15, 28, 29, 30, 31 } },
    { OPTION_MASK_P8_VECTOR,
      (BYTES_BIG_ENDIAN ? CODE_FOR_p8_vmrgew_v4sf_direct
       : CODE_FOR_p8_vmrgow_v4sf_direct),
      {  0,  1,  2,  3, 16, 17, 18, 19,  8,  9, 10, 11, 24, 25, 26, 27 } },
    { OPTION_MASK_P8_VECTOR,
      (BYTES_BIG_ENDIAN ? CODE_FOR_p8_vmrgow_v4sf_direct
       : CODE_FOR_p8_vmrgew_v4sf_direct),
      {  4,  5,  6,  7, 20, 21, 22, 23, 12, 13, 14, 15, 28, 29, 30, 31 } }
  };

  unsigned int i, j, elt, which;
  unsigned char perm[16];
  rtx x;
  bool one_vec;

  /* Unpack the constant selector.  */
  for (i = which = 0; i < 16; ++i)
    {
      elt = sel[i] & 31;
      which |= (elt < 16 ? 1 : 2);
      perm[i] = elt;
    }

  /* Simplify the constant selector based on operands.  */
  switch (which)
    {
    default:
      gcc_unreachable ();

    case 3:
      one_vec = false;
      if (!rtx_equal_p (op0, op1))
	break;
      /* FALLTHRU */

    case 2:
      for (i = 0; i < 16; ++i)
	perm[i] &= 15;
      op0 = op1;
      one_vec = true;
      break;

    case 1:
      op1 = op0;
      one_vec = true;
      break;
    }
 
  /* Look for splat patterns.  */
  if (one_vec)
    {
      elt = perm[0];

      for (i = 0; i < 16; ++i)
	if (perm[i] != elt)
	  break;
      if (i == 16)
	{
          if (!BYTES_BIG_ENDIAN)
            elt = 15 - elt;
	  emit_insn (gen_altivec_vspltb_direct (target, op0, GEN_INT (elt)));
	  return true;
	}

      if (elt % 2 == 0)
	{
	  for (i = 0; i < 16; i += 2)
	    if (perm[i] != elt || perm[i + 1] != elt + 1)
	      break;
	  if (i == 16)
	    {
	      int field = BYTES_BIG_ENDIAN ? elt / 2 : 7 - elt / 2;
	      x = gen_reg_rtx (V8HImode);
	      emit_insn (gen_altivec_vsplth_direct (x, gen_lowpart (V8HImode, op0),
						    GEN_INT (field)));
	      emit_move_insn (target, gen_lowpart (V16QImode, x));
	      return true;
	    }
	}

      if (elt % 4 == 0)
	{
	  for (i = 0; i < 16; i += 4)
	    if (perm[i] != elt
		|| perm[i + 1] != elt + 1
		|| perm[i + 2] != elt + 2
		|| perm[i + 3] != elt + 3)
	      break;
	  if (i == 16)
	    {
	      int field = BYTES_BIG_ENDIAN ? elt / 4 : 3 - elt / 4;
	      x = gen_reg_rtx (V4SImode);
	      emit_insn (gen_altivec_vspltw_direct (x, gen_lowpart (V4SImode, op0),
						    GEN_INT (field)));
	      emit_move_insn (target, gen_lowpart (V16QImode, x));
	      return true;
	    }
	}
    }

  /* Look for merge and pack patterns.  */
  for (j = 0; j < ARRAY_SIZE (patterns); ++j)
    {
      bool swapped;

      if ((patterns[j].mask & rs6000_isa_flags) == 0)
	continue;

      elt = patterns[j].perm[0];
      if (perm[0] == elt)
	swapped = false;
      else if (perm[0] == elt + 16)
	swapped = true;
      else
	continue;
      for (i = 1; i < 16; ++i)
	{
	  elt = patterns[j].perm[i];
	  if (swapped)
	    elt = (elt >= 16 ? elt - 16 : elt + 16);
	  else if (one_vec && elt >= 16)
	    elt -= 16;
	  if (perm[i] != elt)
	    break;
	}
      if (i == 16)
	{
	  enum insn_code icode = patterns[j].impl;
	  machine_mode omode = insn_data[icode].operand[0].mode;
	  machine_mode imode = insn_data[icode].operand[1].mode;

	  /* For little-endian, don't use vpkuwum and vpkuhum if the
	     underlying vector type is not V4SI and V8HI, respectively.
	     For example, using vpkuwum with a V8HI picks up the even
	     halfwords (BE numbering) when the even halfwords (LE
	     numbering) are what we need.  */
	  if (!BYTES_BIG_ENDIAN
	      && icode == CODE_FOR_altivec_vpkuwum_direct
	      && ((REG_P (op0)
		   && GET_MODE (op0) != V4SImode)
		  || (SUBREG_P (op0)
		      && GET_MODE (XEXP (op0, 0)) != V4SImode)))
	    continue;
	  if (!BYTES_BIG_ENDIAN
	      && icode == CODE_FOR_altivec_vpkuhum_direct
	      && ((REG_P (op0)
		   && GET_MODE (op0) != V8HImode)
		  || (SUBREG_P (op0)
		      && GET_MODE (XEXP (op0, 0)) != V8HImode)))
	    continue;

          /* For little-endian, the two input operands must be swapped
             (or swapped back) to ensure proper right-to-left numbering
             from 0 to 2N-1.  */
	  if (swapped ^ !BYTES_BIG_ENDIAN)
	    std::swap (op0, op1);
	  if (imode != V16QImode)
	    {
	      op0 = gen_lowpart (imode, op0);
	      op1 = gen_lowpart (imode, op1);
	    }
	  if (omode == V16QImode)
	    x = target;
	  else
	    x = gen_reg_rtx (omode);
	  emit_insn (GEN_FCN (icode) (x, op0, op1));
	  if (omode != V16QImode)
	    emit_move_insn (target, gen_lowpart (V16QImode, x));
	  return true;
	}
    }

  if (!BYTES_BIG_ENDIAN)
    {
      altivec_expand_vec_perm_const_le (target, op0, op1, sel);
      return true;
    }

  return false;
}

/* Expand a VSX Permute Doubleword constant permutation.
   Return true if we match an efficient implementation.  */

static bool
rs6000_expand_vec_perm_const_1 (rtx target, rtx op0, rtx op1,
				unsigned char perm0, unsigned char perm1)
{
  rtx x;

  /* If both selectors come from the same operand, fold to single op.  */
  if ((perm0 & 2) == (perm1 & 2))
    {
      if (perm0 & 2)
	op0 = op1;
      else
	op1 = op0;
    }
  /* If both operands are equal, fold to simpler permutation.  */
  if (rtx_equal_p (op0, op1))
    {
      perm0 = perm0 & 1;
      perm1 = (perm1 & 1) + 2;
    }
  /* If the first selector comes from the second operand, swap.  */
  else if (perm0 & 2)
    {
      if (perm1 & 2)
	return false;
      perm0 -= 2;
      perm1 += 2;
      std::swap (op0, op1);
    }
  /* If the second selector does not come from the second operand, fail.  */
  else if ((perm1 & 2) == 0)
    return false;

  /* Success! */
  if (target != NULL)
    {
      machine_mode vmode, dmode;
      rtvec v;

      vmode = GET_MODE (target);
      gcc_assert (GET_MODE_NUNITS (vmode) == 2);
      dmode = mode_for_vector (GET_MODE_INNER (vmode), 4).require ();
      x = gen_rtx_VEC_CONCAT (dmode, op0, op1);
      v = gen_rtvec (2, GEN_INT (perm0), GEN_INT (perm1));
      x = gen_rtx_VEC_SELECT (vmode, x, gen_rtx_PARALLEL (VOIDmode, v));
      emit_insn (gen_rtx_SET (target, x));
    }
  return true;
}

/* Implement TARGET_VECTORIZE_VEC_PERM_CONST.  */

static bool
rs6000_vectorize_vec_perm_const (machine_mode vmode, rtx target, rtx op0,
				 rtx op1, const vec_perm_indices &sel)
{
  bool testing_p = !target;

  /* AltiVec (and thus VSX) can handle arbitrary permutations.  */
  if (TARGET_ALTIVEC && testing_p)
    return true;

  /* Check for ps_merge* or xxpermdi insns.  */
  if ((vmode == V2DFmode || vmode == V2DImode) && VECTOR_MEM_VSX_P (vmode))
    {
      if (testing_p)
	{
	  op0 = gen_raw_REG (vmode, LAST_VIRTUAL_REGISTER + 1);
	  op1 = gen_raw_REG (vmode, LAST_VIRTUAL_REGISTER + 2);
	}
      if (rs6000_expand_vec_perm_const_1 (target, op0, op1, sel[0], sel[1]))
	return true;
    }

  if (TARGET_ALTIVEC)
    {
      /* Force the target-independent code to lower to V16QImode.  */
      if (vmode != V16QImode)
	return false;
      if (altivec_expand_vec_perm_const (target, op0, op1, sel))
	return true;
    }

  return false;
}

/* A subroutine for rs6000_expand_extract_even & rs6000_expand_interleave.
   OP0 and OP1 are the input vectors and TARGET is the output vector.
   PERM specifies the constant permutation vector.  */

static void
rs6000_do_expand_vec_perm (rtx target, rtx op0, rtx op1,
			   machine_mode vmode, const vec_perm_builder &perm)
{
  rtx x = expand_vec_perm_const (vmode, op0, op1, perm, BLKmode, target);
  if (x != target)
    emit_move_insn (target, x);
}

/* Expand an extract even operation.  */

void
rs6000_expand_extract_even (rtx target, rtx op0, rtx op1)
{
  machine_mode vmode = GET_MODE (target);
  unsigned i, nelt = GET_MODE_NUNITS (vmode);
  vec_perm_builder perm (nelt, nelt, 1);

  for (i = 0; i < nelt; i++)
    perm.quick_push (i * 2);

  rs6000_do_expand_vec_perm (target, op0, op1, vmode, perm);
}

/* Expand a vector interleave operation.  */

void
rs6000_expand_interleave (rtx target, rtx op0, rtx op1, bool highp)
{
  machine_mode vmode = GET_MODE (target);
  unsigned i, high, nelt = GET_MODE_NUNITS (vmode);
  vec_perm_builder perm (nelt, nelt, 1);

  high = (highp ? 0 : nelt / 2);
  for (i = 0; i < nelt / 2; i++)
    {
      perm.quick_push (i + high);
      perm.quick_push (i + nelt + high);
    }

  rs6000_do_expand_vec_perm (target, op0, op1, vmode, perm);
}

/* Scale a V2DF vector SRC by two to the SCALE and place in TGT.  */
void
rs6000_scale_v2df (rtx tgt, rtx src, int scale)
{
  HOST_WIDE_INT hwi_scale (scale);
  REAL_VALUE_TYPE r_pow;
  rtvec v = rtvec_alloc (2);
  rtx elt;
  rtx scale_vec = gen_reg_rtx (V2DFmode);
  (void)real_powi (&r_pow, DFmode, &dconst2, hwi_scale);
  elt = const_double_from_real_value (r_pow, DFmode);
  RTVEC_ELT (v, 0) = elt;
  RTVEC_ELT (v, 1) = elt;
  rs6000_expand_vector_init (scale_vec, gen_rtx_PARALLEL (V2DFmode, v));
  emit_insn (gen_mulv2df3 (tgt, src, scale_vec));
}

/* Return an RTX representing where to find the function value of a
   function returning MODE.  */
static rtx
rs6000_complex_function_value (machine_mode mode)
{
  unsigned int regno;
  rtx r1, r2;
  machine_mode inner = GET_MODE_INNER (mode);
  unsigned int inner_bytes = GET_MODE_UNIT_SIZE (mode);

  if (TARGET_FLOAT128_TYPE
      && (mode == KCmode
	  || (mode == TCmode && TARGET_IEEEQUAD)))
    regno = ALTIVEC_ARG_RETURN;

  else if (FLOAT_MODE_P (mode) && TARGET_HARD_FLOAT)
    regno = FP_ARG_RETURN;

  else
    {
      regno = GP_ARG_RETURN;

      /* 32-bit is OK since it'll go in r3/r4.  */
      if (TARGET_32BIT && inner_bytes >= 4)
	return gen_rtx_REG (mode, regno);
    }

  if (inner_bytes >= 8)
    return gen_rtx_REG (mode, regno);

  r1 = gen_rtx_EXPR_LIST (inner, gen_rtx_REG (inner, regno),
			  const0_rtx);
  r2 = gen_rtx_EXPR_LIST (inner, gen_rtx_REG (inner, regno + 1),
			  GEN_INT (inner_bytes));
  return gen_rtx_PARALLEL (mode, gen_rtvec (2, r1, r2));
}

/* Return an rtx describing a return value of MODE as a PARALLEL
   in N_ELTS registers, each of mode ELT_MODE, starting at REGNO,
   stride REG_STRIDE.  */

static rtx
rs6000_parallel_return (machine_mode mode,
			int n_elts, machine_mode elt_mode,
			unsigned int regno, unsigned int reg_stride)
{
  rtx par = gen_rtx_PARALLEL (mode, rtvec_alloc (n_elts));

  int i;
  for (i = 0; i < n_elts; i++)
    {
      rtx r = gen_rtx_REG (elt_mode, regno);
      rtx off = GEN_INT (i * GET_MODE_SIZE (elt_mode));
      XVECEXP (par, 0, i) = gen_rtx_EXPR_LIST (VOIDmode, r, off);
      regno += reg_stride;
    }

  return par;
}

/* Target hook for TARGET_FUNCTION_VALUE.

   An integer value is in r3 and a floating-point value is in fp1,
   unless -msoft-float.  */

static rtx
rs6000_function_value (const_tree valtype,
		       const_tree fn_decl_or_type ATTRIBUTE_UNUSED,
		       bool outgoing ATTRIBUTE_UNUSED)
{
  machine_mode mode;
  unsigned int regno;
  machine_mode elt_mode;
  int n_elts;

  /* Special handling for structs in darwin64.  */
  if (TARGET_MACHO 
      && rs6000_darwin64_struct_check_p (TYPE_MODE (valtype), valtype))
    {
      CUMULATIVE_ARGS valcum;
      rtx valret;

      valcum.words = 0;
      valcum.fregno = FP_ARG_MIN_REG;
      valcum.vregno = ALTIVEC_ARG_MIN_REG;
      /* Do a trial code generation as if this were going to be passed as
	 an argument; if any part goes in memory, we return NULL.  */
      valret = rs6000_darwin64_record_arg (&valcum, valtype, true, /* retval= */ true);
      if (valret)
	return valret;
      /* Otherwise fall through to standard ABI rules.  */
    }

  mode = TYPE_MODE (valtype);

  /* The ELFv2 ABI returns homogeneous VFP aggregates in registers.  */
  if (rs6000_discover_homogeneous_aggregate (mode, valtype, &elt_mode, &n_elts))
    {
      int first_reg, n_regs;

      if (SCALAR_FLOAT_MODE_NOT_VECTOR_P (elt_mode))
	{
	  /* _Decimal128 must use even/odd register pairs.  */
	  first_reg = (elt_mode == TDmode) ? FP_ARG_RETURN + 1 : FP_ARG_RETURN;
	  n_regs = (GET_MODE_SIZE (elt_mode) + 7) >> 3;
	}
      else
	{
	  first_reg = ALTIVEC_ARG_RETURN;
	  n_regs = 1;
	}

      return rs6000_parallel_return (mode, n_elts, elt_mode, first_reg, n_regs);
    }

  /* Some return value types need be split in -mpowerpc64, 32bit ABI.  */
  if (TARGET_32BIT && TARGET_POWERPC64)
    switch (mode)
      {
      default:
	break;
      case E_DImode:
      case E_SCmode:
      case E_DCmode:
      case E_TCmode:
	int count = GET_MODE_SIZE (mode) / 4;
	return rs6000_parallel_return (mode, count, SImode, GP_ARG_RETURN, 1);
      }

  if ((INTEGRAL_TYPE_P (valtype)
       && GET_MODE_BITSIZE (mode) < (TARGET_32BIT ? 32 : 64))
      || POINTER_TYPE_P (valtype))
    mode = TARGET_32BIT ? SImode : DImode;

  if (DECIMAL_FLOAT_MODE_P (mode) && TARGET_HARD_FLOAT)
    /* _Decimal128 must use an even/odd register pair.  */
    regno = (mode == TDmode) ? FP_ARG_RETURN + 1 : FP_ARG_RETURN;
  else if (SCALAR_FLOAT_TYPE_P (valtype) && TARGET_HARD_FLOAT
	   && !FLOAT128_VECTOR_P (mode))
    regno = FP_ARG_RETURN;
  else if (TREE_CODE (valtype) == COMPLEX_TYPE
	   && targetm.calls.split_complex_arg)
    return rs6000_complex_function_value (mode);
  /* VSX is a superset of Altivec and adds V2DImode/V2DFmode.  Since the same
     return register is used in both cases, and we won't see V2DImode/V2DFmode
     for pure altivec, combine the two cases.  */
  else if ((TREE_CODE (valtype) == VECTOR_TYPE || VECTOR_ALIGNMENT_P (mode))
	   && TARGET_ALTIVEC && TARGET_ALTIVEC_ABI
	   && ALTIVEC_OR_VSX_VECTOR_MODE (mode))
    regno = ALTIVEC_ARG_RETURN;
  else
    regno = GP_ARG_RETURN;

  return gen_rtx_REG (mode, regno);
}

/* Define how to find the value returned by a library function
   assuming the value has mode MODE.  */
rtx
rs6000_libcall_value (machine_mode mode)
{
  unsigned int regno;

  /* Long long return value need be split in -mpowerpc64, 32bit ABI.  */
  if (TARGET_32BIT && TARGET_POWERPC64 && mode == DImode)
    return rs6000_parallel_return (mode, 2, SImode, GP_ARG_RETURN, 1);

  if (DECIMAL_FLOAT_MODE_P (mode) && TARGET_HARD_FLOAT)
    /* _Decimal128 must use an even/odd register pair.  */
    regno = (mode == TDmode) ? FP_ARG_RETURN + 1 : FP_ARG_RETURN;
  else if (SCALAR_FLOAT_MODE_NOT_VECTOR_P (mode) && TARGET_HARD_FLOAT)
    regno = FP_ARG_RETURN;
  /* VSX is a superset of Altivec and adds V2DImode/V2DFmode.  Since the same
     return register is used in both cases, and we won't see V2DImode/V2DFmode
     for pure altivec, combine the two cases.  */
  else if (ALTIVEC_OR_VSX_VECTOR_MODE (mode)
	   && TARGET_ALTIVEC && TARGET_ALTIVEC_ABI)
    regno = ALTIVEC_ARG_RETURN;
  else if (COMPLEX_MODE_P (mode) && targetm.calls.split_complex_arg)
    return rs6000_complex_function_value (mode);
  else
    regno = GP_ARG_RETURN;

  return gen_rtx_REG (mode, regno);
}

/* Compute register pressure classes.  We implement the target hook to avoid
   IRA picking something like GEN_OR_FLOAT_REGS as a pressure class, which can
   lead to incorrect estimates of number of available registers and therefor
   increased register pressure/spill.   */
static int
rs6000_compute_pressure_classes (enum reg_class *pressure_classes)
{
  int n;

  n = 0;
  pressure_classes[n++] = GENERAL_REGS;
  if (TARGET_VSX)
    pressure_classes[n++] = VSX_REGS;
  else
    {
      if (TARGET_ALTIVEC)
	pressure_classes[n++] = ALTIVEC_REGS;
      if (TARGET_HARD_FLOAT)
	pressure_classes[n++] = FLOAT_REGS;
    }
  pressure_classes[n++] = CR_REGS;
  pressure_classes[n++] = SPECIAL_REGS;

  return n;
}

/* Given FROM and TO register numbers, say whether this elimination is allowed.
   Frame pointer elimination is automatically handled.

   For the RS/6000, if frame pointer elimination is being done, we would like
   to convert ap into fp, not sp.

   We need r30 if -mminimal-toc was specified, and there are constant pool
   references.  */

static bool
rs6000_can_eliminate (const int from, const int to)
{
  return (from == ARG_POINTER_REGNUM && to == STACK_POINTER_REGNUM
	  ? ! frame_pointer_needed
	  : from == RS6000_PIC_OFFSET_TABLE_REGNUM
	    ? ! TARGET_MINIMAL_TOC || TARGET_NO_TOC_OR_PCREL
		|| constant_pool_empty_p ()
	    : true);
}

/* Define the offset between two registers, FROM to be eliminated and its
   replacement TO, at the start of a routine.  */
HOST_WIDE_INT
rs6000_initial_elimination_offset (int from, int to)
{
  rs6000_stack_t *info = rs6000_stack_info ();
  HOST_WIDE_INT offset;

  if (from == HARD_FRAME_POINTER_REGNUM && to == STACK_POINTER_REGNUM)
    offset = info->push_p ? 0 : -info->total_size;
  else if (from == FRAME_POINTER_REGNUM && to == STACK_POINTER_REGNUM)
    {
      offset = info->push_p ? 0 : -info->total_size;
      if (FRAME_GROWS_DOWNWARD)
	offset += info->fixed_size + info->vars_size + info->parm_size;
    }
  else if (from == FRAME_POINTER_REGNUM && to == HARD_FRAME_POINTER_REGNUM)
    offset = FRAME_GROWS_DOWNWARD
	     ? info->fixed_size + info->vars_size + info->parm_size
	     : 0;
  else if (from == ARG_POINTER_REGNUM && to == HARD_FRAME_POINTER_REGNUM)
    offset = info->total_size;
  else if (from == ARG_POINTER_REGNUM && to == STACK_POINTER_REGNUM)
    offset = info->push_p ? info->total_size : 0;
  else if (from == RS6000_PIC_OFFSET_TABLE_REGNUM)
    offset = 0;
  else
    gcc_unreachable ();

  return offset;
}

/* Fill in sizes of registers used by unwinder.  */

static void
rs6000_init_dwarf_reg_sizes_extra (tree address)
{
  if (TARGET_MACHO && ! TARGET_ALTIVEC)
    {
      int i;
      machine_mode mode = TYPE_MODE (char_type_node);
      rtx addr = expand_expr (address, NULL_RTX, VOIDmode, EXPAND_NORMAL);
      rtx mem = gen_rtx_MEM (BLKmode, addr);
      rtx value = gen_int_mode (16, mode);

      /* On Darwin, libgcc may be built to run on both G3 and G4/5.
	 The unwinder still needs to know the size of Altivec registers.  */

      for (i = FIRST_ALTIVEC_REGNO; i < LAST_ALTIVEC_REGNO+1; i++)
	{
	  int column = DWARF_REG_TO_UNWIND_COLUMN
		(DWARF2_FRAME_REG_OUT (DWARF_FRAME_REGNUM (i), true));
	  HOST_WIDE_INT offset = column * GET_MODE_SIZE (mode);

	  emit_move_insn (adjust_address (mem, mode, offset), value);
	}
    }
}

/* Map internal gcc register numbers to debug format register numbers.
   FORMAT specifies the type of debug register number to use:
     0 -- debug information, except for frame-related sections
     1 -- DWARF .debug_frame section
     2 -- DWARF .eh_frame section  */

unsigned int
rs6000_dbx_register_number (unsigned int regno, unsigned int format)
{
  /* On some platforms, we use the standard DWARF register
     numbering for .debug_info and .debug_frame.  */
  if ((format == 0 && write_symbols == DWARF2_DEBUG) || format == 1)
    {
#ifdef RS6000_USE_DWARF_NUMBERING
      if (regno <= 31)
	return regno;
      if (FP_REGNO_P (regno))
	return regno - FIRST_FPR_REGNO + 32;
      if (ALTIVEC_REGNO_P (regno))
	return regno - FIRST_ALTIVEC_REGNO + 1124;
      if (regno == LR_REGNO)
	return 108;
      if (regno == CTR_REGNO)
	return 109;
      if (regno == CA_REGNO)
	return 101;  /* XER */
      /* Special handling for CR for .debug_frame: rs6000_emit_prologue has
	 translated any combination of CR2, CR3, CR4 saves to a save of CR2.
	 The actual code emitted saves the whole of CR, so we map CR2_REGNO
	 to the DWARF reg for CR.  */
      if (format == 1 && regno == CR2_REGNO)
	return 64;
      if (CR_REGNO_P (regno))
	return regno - CR0_REGNO + 86;
      if (regno == VRSAVE_REGNO)
	return 356;
      if (regno == VSCR_REGNO)
	return 67;

      /* These do not make much sense.  */
      if (regno == FRAME_POINTER_REGNUM)
	return 111;
      if (regno == ARG_POINTER_REGNUM)
	return 67;
      if (regno == 64)
	return 100;

      gcc_unreachable ();
#endif
    }

  /* We use the GCC 7 (and before) internal number for non-DWARF debug
     information, and also for .eh_frame.  */
  /* Translate the regnos to their numbers in GCC 7 (and before).  */
  if (regno <= 31)
    return regno;
  if (FP_REGNO_P (regno))
    return regno - FIRST_FPR_REGNO + 32;
  if (ALTIVEC_REGNO_P (regno))
    return regno - FIRST_ALTIVEC_REGNO + 77;
  if (regno == LR_REGNO)
    return 65;
  if (regno == CTR_REGNO)
    return 66;
  if (regno == CA_REGNO)
    return 76;  /* XER */
  if (CR_REGNO_P (regno))
    return regno - CR0_REGNO + 68;
  if (regno == VRSAVE_REGNO)
    return 109;
  if (regno == VSCR_REGNO)
    return 110;

  if (regno == FRAME_POINTER_REGNUM)
    return 111;
  if (regno == ARG_POINTER_REGNUM)
    return 67;
  if (regno == 64)
    return 64;

  gcc_unreachable ();
}

/* target hook eh_return_filter_mode */
static scalar_int_mode
rs6000_eh_return_filter_mode (void)
{
  return TARGET_32BIT ? SImode : word_mode;
}

/* Target hook for translate_mode_attribute.  */
static machine_mode
rs6000_translate_mode_attribute (machine_mode mode)
{
  if ((FLOAT128_IEEE_P (mode)
       && ieee128_float_type_node == long_double_type_node)
      || (FLOAT128_IBM_P (mode)
	  && ibm128_float_type_node == long_double_type_node))
    return COMPLEX_MODE_P (mode) ? E_TCmode : E_TFmode;
  return mode;
}

/* Target hook for scalar_mode_supported_p.  */
static bool
rs6000_scalar_mode_supported_p (scalar_mode mode)
{
  /* -m32 does not support TImode.  This is the default, from
     default_scalar_mode_supported_p.  For -m32 -mpowerpc64 we want the
     same ABI as for -m32.  But default_scalar_mode_supported_p allows
     integer modes of precision 2 * BITS_PER_WORD, which matches TImode
     for -mpowerpc64.  */
  if (TARGET_32BIT && mode == TImode)
    return false;

  if (DECIMAL_FLOAT_MODE_P (mode))
    return default_decimal_float_supported_p ();
  else if (TARGET_FLOAT128_TYPE && (mode == KFmode || mode == IFmode))
    return true;
  else
    return default_scalar_mode_supported_p (mode);
}

/* Target hook for vector_mode_supported_p.  */
static bool
rs6000_vector_mode_supported_p (machine_mode mode)
{
  /* There is no vector form for IEEE 128-bit.  If we return true for IEEE
     128-bit, the compiler might try to widen IEEE 128-bit to IBM
     double-double.  */
  if (VECTOR_MEM_ALTIVEC_OR_VSX_P (mode) && !FLOAT128_IEEE_P (mode))
    return true;

  else
    return false;
}

/* Target hook for floatn_mode.  */
static opt_scalar_float_mode
rs6000_floatn_mode (int n, bool extended)
{
  if (extended)
    {
      switch (n)
	{
	case 32:
	  return DFmode;

	case 64:
	  if (TARGET_FLOAT128_TYPE)
	    return (FLOAT128_IEEE_P (TFmode)) ? TFmode : KFmode;
	  else
	    return opt_scalar_float_mode ();

	case 128:
	  return opt_scalar_float_mode ();

	default:
	  /* Those are the only valid _FloatNx types.  */
	  gcc_unreachable ();
	}
    }
  else
    {
      switch (n)
	{
	case 32:
	  return SFmode;

	case 64:
	  return DFmode;

	case 128:
	  if (TARGET_FLOAT128_TYPE)
	    return (FLOAT128_IEEE_P (TFmode)) ? TFmode : KFmode;
	  else
	    return opt_scalar_float_mode ();

	default:
	  return opt_scalar_float_mode ();
	}
    }

}

/* Target hook for c_mode_for_suffix.  */
static machine_mode
rs6000_c_mode_for_suffix (char suffix)
{
  if (TARGET_FLOAT128_TYPE)
    {
      if (suffix == 'q' || suffix == 'Q')
	return (FLOAT128_IEEE_P (TFmode)) ? TFmode : KFmode;

      /* At the moment, we are not defining a suffix for IBM extended double.
	 If/when the default for -mabi=ieeelongdouble is changed, and we want
	 to support __ibm128 constants in legacy library code, we may need to
	 re-evalaute this decision.  Currently, c-lex.c only supports 'w' and
	 'q' as machine dependent suffixes.  The x86_64 port uses 'w' for
	 __float80 constants.  */
    }

  return VOIDmode;
}

/* Target hook for invalid_arg_for_unprototyped_fn. */
static const char *
invalid_arg_for_unprototyped_fn (const_tree typelist, const_tree funcdecl, const_tree val)
{
  return (!rs6000_darwin64_abi
	  && typelist == 0
          && TREE_CODE (TREE_TYPE (val)) == VECTOR_TYPE
          && (funcdecl == NULL_TREE
              || (TREE_CODE (funcdecl) == FUNCTION_DECL
                  && DECL_BUILT_IN_CLASS (funcdecl) != BUILT_IN_MD)))
	  ? N_("AltiVec argument passed to unprototyped function")
	  : NULL;
}

/* For TARGET_SECURE_PLT 32-bit PIC code we can save PIC register
   setup by using __stack_chk_fail_local hidden function instead of
   calling __stack_chk_fail directly.  Otherwise it is better to call
   __stack_chk_fail directly.  */

static tree ATTRIBUTE_UNUSED
rs6000_stack_protect_fail (void)
{
  return (DEFAULT_ABI == ABI_V4 && TARGET_SECURE_PLT && flag_pic)
	 ? default_hidden_stack_protect_fail ()
	 : default_external_stack_protect_fail ();
}

/* Implement the TARGET_ASAN_SHADOW_OFFSET hook.  */

#if TARGET_ELF
static unsigned HOST_WIDE_INT
rs6000_asan_shadow_offset (void)
{
  return (unsigned HOST_WIDE_INT) 1 << (TARGET_64BIT ? 41 : 29);
}
#endif

/* Mask options that we want to support inside of attribute((target)) and
   #pragma GCC target operations.  Note, we do not include things like
   64/32-bit, endianness, hard/soft floating point, etc. that would have
   different calling sequences.  */

struct rs6000_opt_mask {
  const char *name;		/* option name */
  HOST_WIDE_INT mask;		/* mask to set */
  bool invert;			/* invert sense of mask */
  bool valid_target;		/* option is a target option */
};

static struct rs6000_opt_mask const rs6000_opt_masks[] =
{
  { "altivec",			OPTION_MASK_ALTIVEC,		false, true  },
  { "cmpb",			OPTION_MASK_CMPB,		false, true  },
  { "crypto",			OPTION_MASK_CRYPTO,		false, true  },
  { "direct-move",		OPTION_MASK_DIRECT_MOVE,	false, true  },
  { "dlmzb",			OPTION_MASK_DLMZB,		false, true  },
  { "efficient-unaligned-vsx",	OPTION_MASK_EFFICIENT_UNALIGNED_VSX,
								false, true  },
  { "float128",			OPTION_MASK_FLOAT128_KEYWORD,	false, true  },
  { "float128-hardware",	OPTION_MASK_FLOAT128_HW,	false, true  },
  { "fprnd",			OPTION_MASK_FPRND,		false, true  },
  { "power10",			OPTION_MASK_POWER10,		false, true  },
  { "hard-dfp",			OPTION_MASK_DFP,		false, true  },
  { "htm",			OPTION_MASK_HTM,		false, true  },
  { "isel",			OPTION_MASK_ISEL,		false, true  },
  { "mfcrf",			OPTION_MASK_MFCRF,		false, true  },
  { "mfpgpr",			0,				false, true  },
  { "mma",			OPTION_MASK_MMA,		false, true  },
  { "modulo",			OPTION_MASK_MODULO,		false, true  },
  { "mulhw",			OPTION_MASK_MULHW,		false, true  },
  { "multiple",			OPTION_MASK_MULTIPLE,		false, true  },
  { "pcrel",			OPTION_MASK_PCREL,		false, true  },
  { "popcntb",			OPTION_MASK_POPCNTB,		false, true  },
  { "popcntd",			OPTION_MASK_POPCNTD,		false, true  },
  { "power8-fusion",		OPTION_MASK_P8_FUSION,		false, true  },
  { "power8-fusion-sign",	OPTION_MASK_P8_FUSION_SIGN,	false, true  },
  { "power8-vector",		OPTION_MASK_P8_VECTOR,		false, true  },
  { "power9-minmax",		OPTION_MASK_P9_MINMAX,		false, true  },
  { "power9-misc",		OPTION_MASK_P9_MISC,		false, true  },
  { "power9-vector",		OPTION_MASK_P9_VECTOR,		false, true  },
  { "powerpc-gfxopt",		OPTION_MASK_PPC_GFXOPT,		false, true  },
  { "powerpc-gpopt",		OPTION_MASK_PPC_GPOPT,		false, true  },
  { "prefixed",			OPTION_MASK_PREFIXED,		false, true  },
  { "quad-memory",		OPTION_MASK_QUAD_MEMORY,	false, true  },
  { "quad-memory-atomic",	OPTION_MASK_QUAD_MEMORY_ATOMIC,	false, true  },
  { "recip-precision",		OPTION_MASK_RECIP_PRECISION,	false, true  },
  { "save-toc-indirect",	OPTION_MASK_SAVE_TOC_INDIRECT,	false, true  },
  { "string",			0,				false, true  },
  { "update",			OPTION_MASK_NO_UPDATE,		true , true  },
  { "vsx",			OPTION_MASK_VSX,		false, true  },
#ifdef OPTION_MASK_64BIT
#if TARGET_AIX_OS
  { "aix64",			OPTION_MASK_64BIT,		false, false },
  { "aix32",			OPTION_MASK_64BIT,		true,  false },
#else
  { "64",			OPTION_MASK_64BIT,		false, false },
  { "32",			OPTION_MASK_64BIT,		true,  false },
#endif
#endif
#ifdef OPTION_MASK_EABI
  { "eabi",			OPTION_MASK_EABI,		false, false },
#endif
#ifdef OPTION_MASK_LITTLE_ENDIAN
  { "little",			OPTION_MASK_LITTLE_ENDIAN,	false, false },
  { "big",			OPTION_MASK_LITTLE_ENDIAN,	true,  false },
#endif
#ifdef OPTION_MASK_RELOCATABLE
  { "relocatable",		OPTION_MASK_RELOCATABLE,	false, false },
#endif
#ifdef OPTION_MASK_STRICT_ALIGN
  { "strict-align",		OPTION_MASK_STRICT_ALIGN,	false, false },
#endif
  { "soft-float",		OPTION_MASK_SOFT_FLOAT,		false, false },
  { "string",			0,				false, false },
};

/* Builtin mask mapping for printing the flags.  */
static struct rs6000_opt_mask const rs6000_builtin_mask_names[] =
{
  { "altivec",		 RS6000_BTM_ALTIVEC,	false, false },
  { "vsx",		 RS6000_BTM_VSX,	false, false },
  { "fre",		 RS6000_BTM_FRE,	false, false },
  { "fres",		 RS6000_BTM_FRES,	false, false },
  { "frsqrte",		 RS6000_BTM_FRSQRTE,	false, false },
  { "frsqrtes",		 RS6000_BTM_FRSQRTES,	false, false },
  { "popcntd",		 RS6000_BTM_POPCNTD,	false, false },
  { "cell",		 RS6000_BTM_CELL,	false, false },
  { "power8-vector",	 RS6000_BTM_P8_VECTOR,	false, false },
  { "power9-vector",	 RS6000_BTM_P9_VECTOR,	false, false },
  { "power9-misc",	 RS6000_BTM_P9_MISC,	false, false },
  { "crypto",		 RS6000_BTM_CRYPTO,	false, false },
  { "htm",		 RS6000_BTM_HTM,	false, false },
  { "hard-dfp",		 RS6000_BTM_DFP,	false, false },
  { "hard-float",	 RS6000_BTM_HARD_FLOAT,	false, false },
  { "long-double-128",	 RS6000_BTM_LDBL128,	false, false },
  { "powerpc64",	 RS6000_BTM_POWERPC64,  false, false },
  { "float128",		 RS6000_BTM_FLOAT128,   false, false },
  { "float128-hw",	 RS6000_BTM_FLOAT128_HW,false, false },
  { "mma",		 RS6000_BTM_MMA,	false, false },
  { "power10",		 RS6000_BTM_P10,	false, false },
};

/* Option variables that we want to support inside attribute((target)) and
   #pragma GCC target operations.  */

struct rs6000_opt_var {
  const char *name;		/* option name */
  size_t global_offset;		/* offset of the option in global_options.  */
  size_t target_offset;		/* offset of the option in target options.  */
};

static struct rs6000_opt_var const rs6000_opt_vars[] =
{
  { "friz",
    offsetof (struct gcc_options, x_TARGET_FRIZ),
    offsetof (struct cl_target_option, x_TARGET_FRIZ), },
  { "avoid-indexed-addresses",
    offsetof (struct gcc_options, x_TARGET_AVOID_XFORM),
    offsetof (struct cl_target_option, x_TARGET_AVOID_XFORM) },
  { "longcall",
    offsetof (struct gcc_options, x_rs6000_default_long_calls),
    offsetof (struct cl_target_option, x_rs6000_default_long_calls), },
  { "optimize-swaps",
    offsetof (struct gcc_options, x_rs6000_optimize_swaps),
    offsetof (struct cl_target_option, x_rs6000_optimize_swaps), },
  { "allow-movmisalign",
    offsetof (struct gcc_options, x_TARGET_ALLOW_MOVMISALIGN),
    offsetof (struct cl_target_option, x_TARGET_ALLOW_MOVMISALIGN), },
  { "sched-groups",
    offsetof (struct gcc_options, x_TARGET_SCHED_GROUPS),
    offsetof (struct cl_target_option, x_TARGET_SCHED_GROUPS), },
  { "always-hint",
    offsetof (struct gcc_options, x_TARGET_ALWAYS_HINT),
    offsetof (struct cl_target_option, x_TARGET_ALWAYS_HINT), },
  { "align-branch-targets",
    offsetof (struct gcc_options, x_TARGET_ALIGN_BRANCH_TARGETS),
    offsetof (struct cl_target_option, x_TARGET_ALIGN_BRANCH_TARGETS), },
  { "sched-prolog",
    offsetof (struct gcc_options, x_TARGET_SCHED_PROLOG),
    offsetof (struct cl_target_option, x_TARGET_SCHED_PROLOG), },
  { "sched-epilog",
    offsetof (struct gcc_options, x_TARGET_SCHED_PROLOG),
    offsetof (struct cl_target_option, x_TARGET_SCHED_PROLOG), },
  { "speculate-indirect-jumps",
    offsetof (struct gcc_options, x_rs6000_speculate_indirect_jumps),
    offsetof (struct cl_target_option, x_rs6000_speculate_indirect_jumps), },
};

/* Inner function to handle attribute((target("..."))) and #pragma GCC target
   parsing.  Return true if there were no errors.  */

static bool
rs6000_inner_target_options (tree args, bool attr_p)
{
  bool ret = true;

  if (args == NULL_TREE)
    ;

  else if (TREE_CODE (args) == STRING_CST)
    {
      char *p = ASTRDUP (TREE_STRING_POINTER (args));
      char *q;

      while ((q = strtok (p, ",")) != NULL)
	{
	  bool error_p = false;
	  bool not_valid_p = false;
	  const char *cpu_opt = NULL;

	  p = NULL;
	  if (strncmp (q, "cpu=", 4) == 0)
	    {
	      int cpu_index = rs6000_cpu_name_lookup (q+4);
	      if (cpu_index >= 0)
		rs6000_cpu_index = cpu_index;
	      else
		{
		  error_p = true;
		  cpu_opt = q+4;
		}
	    }
	  else if (strncmp (q, "tune=", 5) == 0)
	    {
	      int tune_index = rs6000_cpu_name_lookup (q+5);
	      if (tune_index >= 0)
		rs6000_tune_index = tune_index;
	      else
		{
		  error_p = true;
		  cpu_opt = q+5;
		}
	    }
	  else
	    {
	      size_t i;
	      bool invert = false;
	      char *r = q;

	      error_p = true;
	      if (strncmp (r, "no-", 3) == 0)
		{
		  invert = true;
		  r += 3;
		}

	      for (i = 0; i < ARRAY_SIZE (rs6000_opt_masks); i++)
		if (strcmp (r, rs6000_opt_masks[i].name) == 0)
		  {
		    HOST_WIDE_INT mask = rs6000_opt_masks[i].mask;

		    if (!rs6000_opt_masks[i].valid_target)
		      not_valid_p = true;
		    else
		      {
			error_p = false;
			rs6000_isa_flags_explicit |= mask;

			/* VSX needs altivec, so -mvsx automagically sets
			   altivec and disables -mavoid-indexed-addresses.  */
			if (!invert)
			  {
			    if (mask == OPTION_MASK_VSX)
			      {
				mask |= OPTION_MASK_ALTIVEC;
				TARGET_AVOID_XFORM = 0;
			      }
			  }

			if (rs6000_opt_masks[i].invert)
			  invert = !invert;

			if (invert)
			  rs6000_isa_flags &= ~mask;
			else
			  rs6000_isa_flags |= mask;
		      }
		    break;
		  }

	      if (error_p && !not_valid_p)
		{
		  for (i = 0; i < ARRAY_SIZE (rs6000_opt_vars); i++)
		    if (strcmp (r, rs6000_opt_vars[i].name) == 0)
		      {
			size_t j = rs6000_opt_vars[i].global_offset;
			*((int *) ((char *)&global_options + j)) = !invert;
			error_p = false;
			not_valid_p = false;
			break;
		      }
		}
	    }

	  if (error_p)
	    {
	      const char *eprefix, *esuffix;

	      ret = false;
	      if (attr_p)
		{
		  eprefix = "__attribute__((__target__(";
		  esuffix = ")))";
		}
	      else
		{
		  eprefix = "#pragma GCC target ";
		  esuffix = "";
		}

	      if (cpu_opt)
		error ("invalid cpu %qs for %s%qs%s", cpu_opt, eprefix,
		       q, esuffix);
	      else if (not_valid_p)
		error ("%s%qs%s is not allowed", eprefix, q, esuffix);
	      else
		error ("%s%qs%s is invalid", eprefix, q, esuffix);
	    }
	}
    }

  else if (TREE_CODE (args) == TREE_LIST)
    {
      do
	{
	  tree value = TREE_VALUE (args);
	  if (value)
	    {
	      bool ret2 = rs6000_inner_target_options (value, attr_p);
	      if (!ret2)
		ret = false;
	    }
	  args = TREE_CHAIN (args);
	}
      while (args != NULL_TREE);
    }

  else
    {
      error ("attribute %<target%> argument not a string");
      return false;
    }

  return ret;
}

/* Print out the target options as a list for -mdebug=target.  */

static void
rs6000_debug_target_options (tree args, const char *prefix)
{
  if (args == NULL_TREE)
    fprintf (stderr, "%s<NULL>", prefix);

  else if (TREE_CODE (args) == STRING_CST)
    {
      char *p = ASTRDUP (TREE_STRING_POINTER (args));
      char *q;

      while ((q = strtok (p, ",")) != NULL)
	{
	  p = NULL;
	  fprintf (stderr, "%s\"%s\"", prefix, q);
	  prefix = ", ";
	}
    }

  else if (TREE_CODE (args) == TREE_LIST)
    {
      do
	{
	  tree value = TREE_VALUE (args);
	  if (value)
	    {
	      rs6000_debug_target_options (value, prefix);
	      prefix = ", ";
	    }
	  args = TREE_CHAIN (args);
	}
      while (args != NULL_TREE);
    }

  else
    gcc_unreachable ();

  return;
}


/* Hook to validate attribute((target("..."))).  */

static bool
rs6000_valid_attribute_p (tree fndecl,
			  tree ARG_UNUSED (name),
			  tree args,
			  int flags)
{
  struct cl_target_option cur_target;
  bool ret;
  tree old_optimize;
  tree new_target, new_optimize;
  tree func_optimize;

  gcc_assert ((fndecl != NULL_TREE) && (args != NULL_TREE));

  if (TARGET_DEBUG_TARGET)
    {
      tree tname = DECL_NAME (fndecl);
      fprintf (stderr, "\n==================== rs6000_valid_attribute_p:\n");
      if (tname)
	fprintf (stderr, "function: %.*s\n",
		 (int) IDENTIFIER_LENGTH (tname),
		 IDENTIFIER_POINTER (tname));
      else
	fprintf (stderr, "function: unknown\n");
  
      fprintf (stderr, "args:");
      rs6000_debug_target_options (args, " ");
      fprintf (stderr, "\n");

      if (flags)
	fprintf (stderr, "flags: 0x%x\n", flags);

      fprintf (stderr, "--------------------\n");
    }

  /* attribute((target("default"))) does nothing, beyond
     affecting multi-versioning.  */
  if (TREE_VALUE (args)
      && TREE_CODE (TREE_VALUE (args)) == STRING_CST
      && TREE_CHAIN (args) == NULL_TREE
      && strcmp (TREE_STRING_POINTER (TREE_VALUE (args)), "default") == 0)
    return true;

  old_optimize = build_optimization_node (&global_options);
  func_optimize = DECL_FUNCTION_SPECIFIC_OPTIMIZATION (fndecl);

  /* If the function changed the optimization levels as well as setting target
     options, start with the optimizations specified.  */
  if (func_optimize && func_optimize != old_optimize)
    cl_optimization_restore (&global_options,
			     TREE_OPTIMIZATION (func_optimize));

  /* The target attributes may also change some optimization flags, so update
     the optimization options if necessary.  */
  cl_target_option_save (&cur_target, &global_options);
  rs6000_cpu_index = rs6000_tune_index = -1;
  ret = rs6000_inner_target_options (args, true);

  /* Set up any additional state.  */
  if (ret)
    {
      ret = rs6000_option_override_internal (false);
      new_target = build_target_option_node (&global_options);
    }
  else
    new_target = NULL;

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


/* Hook to validate the current #pragma GCC target and set the state, and
   update the macros based on what was changed.  If ARGS is NULL, then
   POP_TARGET is used to reset the options.  */

bool
rs6000_pragma_target_parse (tree args, tree pop_target)
{
  tree prev_tree = build_target_option_node (&global_options);
  tree cur_tree;
  struct cl_target_option *prev_opt, *cur_opt;
  HOST_WIDE_INT prev_flags, cur_flags, diff_flags;
  HOST_WIDE_INT prev_bumask, cur_bumask, diff_bumask;

  if (TARGET_DEBUG_TARGET)
    {
      fprintf (stderr, "\n==================== rs6000_pragma_target_parse\n");
      fprintf (stderr, "args:");
      rs6000_debug_target_options (args, " ");
      fprintf (stderr, "\n");

      if (pop_target)
	{
	  fprintf (stderr, "pop_target:\n");
	  debug_tree (pop_target);
	}
      else
	fprintf (stderr, "pop_target: <NULL>\n");

      fprintf (stderr, "--------------------\n");
    }

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
      rs6000_cpu_index = rs6000_tune_index = -1;
      if (!rs6000_inner_target_options (args, false)
	  || !rs6000_option_override_internal (false)
	  || (cur_tree = build_target_option_node (&global_options))
	     == NULL_TREE)
	{
	  if (TARGET_DEBUG_BUILTIN || TARGET_DEBUG_TARGET)
	    fprintf (stderr, "invalid pragma\n");

	  return false;
	}
    }

  target_option_current_node = cur_tree;
  rs6000_activate_target_options (target_option_current_node);

  /* If we have the preprocessor linked in (i.e. C or C++ languages), possibly
     change the macros that are defined.  */
  if (rs6000_target_modify_macros_ptr)
    {
      prev_opt    = TREE_TARGET_OPTION (prev_tree);
      prev_bumask = prev_opt->x_rs6000_builtin_mask;
      prev_flags  = prev_opt->x_rs6000_isa_flags;

      cur_opt     = TREE_TARGET_OPTION (cur_tree);
      cur_flags   = cur_opt->x_rs6000_isa_flags;
      cur_bumask  = cur_opt->x_rs6000_builtin_mask;

      diff_bumask = (prev_bumask ^ cur_bumask);
      diff_flags  = (prev_flags ^ cur_flags);

      if ((diff_flags != 0) || (diff_bumask != 0))
	{
	  /* Delete old macros.  */
	  rs6000_target_modify_macros_ptr (false,
					   prev_flags & diff_flags,
					   prev_bumask & diff_bumask);

	  /* Define new macros.  */
	  rs6000_target_modify_macros_ptr (true,
					   cur_flags & diff_flags,
					   cur_bumask & diff_bumask);
	}
    }

  return true;
}


/* Remember the last target of rs6000_set_current_function.  */
static GTY(()) tree rs6000_previous_fndecl;

/* Restore target's globals from NEW_TREE and invalidate the
   rs6000_previous_fndecl cache.  */

void
rs6000_activate_target_options (tree new_tree)
{
  cl_target_option_restore (&global_options, TREE_TARGET_OPTION (new_tree));
  if (TREE_TARGET_GLOBALS (new_tree))
    restore_target_globals (TREE_TARGET_GLOBALS (new_tree));
  else if (new_tree == target_option_default_node)
    restore_target_globals (&default_target_globals);
  else
    TREE_TARGET_GLOBALS (new_tree) = save_target_globals_default_opts ();
  rs6000_previous_fndecl = NULL_TREE;
}

/* Establish appropriate back-end context for processing the function
   FNDECL.  The argument might be NULL to indicate processing at top
   level, outside of any function scope.  */
static void
rs6000_set_current_function (tree fndecl)
{
  if (TARGET_DEBUG_TARGET)
    {
      fprintf (stderr, "\n==================== rs6000_set_current_function");

      if (fndecl)
	fprintf (stderr, ", fndecl %s (%p)",
		 (DECL_NAME (fndecl)
		  ? IDENTIFIER_POINTER (DECL_NAME (fndecl))
		  : "<unknown>"), (void *)fndecl);

      if (rs6000_previous_fndecl)
	fprintf (stderr, ", prev_fndecl (%p)", (void *)rs6000_previous_fndecl);

      fprintf (stderr, "\n");
    }

  /* Only change the context if the function changes.  This hook is called
     several times in the course of compiling a function, and we don't want to
     slow things down too much or call target_reinit when it isn't safe.  */
  if (fndecl == rs6000_previous_fndecl)
    return;

  tree old_tree;
  if (rs6000_previous_fndecl == NULL_TREE)
    old_tree = target_option_current_node;
  else if (DECL_FUNCTION_SPECIFIC_TARGET (rs6000_previous_fndecl))
    old_tree = DECL_FUNCTION_SPECIFIC_TARGET (rs6000_previous_fndecl);
  else
    old_tree = target_option_default_node;

  tree new_tree;
  if (fndecl == NULL_TREE)
    {
      if (old_tree != target_option_current_node)
	new_tree = target_option_current_node;
      else
	new_tree = NULL_TREE;
    }
  else
    {
      new_tree = DECL_FUNCTION_SPECIFIC_TARGET (fndecl);
      if (new_tree == NULL_TREE)
	new_tree = target_option_default_node;
    }

  if (TARGET_DEBUG_TARGET)
    {
      if (new_tree)
	{
	  fprintf (stderr, "\nnew fndecl target specific options:\n");
	  debug_tree (new_tree);
	}

      if (old_tree)
	{
	  fprintf (stderr, "\nold fndecl target specific options:\n");
	  debug_tree (old_tree);
	}

      if (old_tree != NULL_TREE || new_tree != NULL_TREE)
	fprintf (stderr, "--------------------\n");
    }

  if (new_tree && old_tree != new_tree)
    rs6000_activate_target_options (new_tree);

  if (fndecl)
    rs6000_previous_fndecl = fndecl;
}


/* Save the current options */

static void
rs6000_function_specific_save (struct cl_target_option *ptr,
			       struct gcc_options *opts)
{
  ptr->x_rs6000_isa_flags = opts->x_rs6000_isa_flags;
  ptr->x_rs6000_isa_flags_explicit = opts->x_rs6000_isa_flags_explicit;
}

/* Restore the current options */

static void
rs6000_function_specific_restore (struct gcc_options *opts,
				  struct cl_target_option *ptr)
				  
{
  opts->x_rs6000_isa_flags = ptr->x_rs6000_isa_flags;
  opts->x_rs6000_isa_flags_explicit = ptr->x_rs6000_isa_flags_explicit;
  (void) rs6000_option_override_internal (false);
}

/* Print the current options */

static void
rs6000_function_specific_print (FILE *file, int indent,
				struct cl_target_option *ptr)
{
  rs6000_print_isa_options (file, indent, "Isa options set",
			    ptr->x_rs6000_isa_flags);

  rs6000_print_isa_options (file, indent, "Isa options explicit",
			    ptr->x_rs6000_isa_flags_explicit);
}

/* Helper function to print the current isa or misc options on a line.  */

static void
rs6000_print_options_internal (FILE *file,
			       int indent,
			       const char *string,
			       HOST_WIDE_INT flags,
			       const char *prefix,
			       const struct rs6000_opt_mask *opts,
			       size_t num_elements)
{
  size_t i;
  size_t start_column = 0;
  size_t cur_column;
  size_t max_column = 120;
  size_t prefix_len = strlen (prefix);
  size_t comma_len = 0;
  const char *comma = "";

  if (indent)
    start_column += fprintf (file, "%*s", indent, "");

  if (!flags)
    {
      fprintf (stderr, DEBUG_FMT_S, string, "<none>");
      return;
    }

  start_column += fprintf (stderr, DEBUG_FMT_WX, string, flags);

  /* Print the various mask options.  */
  cur_column = start_column;
  for (i = 0; i < num_elements; i++)
    {
      bool invert = opts[i].invert;
      const char *name = opts[i].name;
      const char *no_str = "";
      HOST_WIDE_INT mask = opts[i].mask;
      size_t len = comma_len + prefix_len + strlen (name);

      if (!invert)
	{
	  if ((flags & mask) == 0)
	    {
	      no_str = "no-";
	      len += strlen ("no-");
	    }

	  flags &= ~mask;
	}

      else
	{
	  if ((flags & mask) != 0)
	    {
	      no_str = "no-";
	      len += strlen ("no-");
	    }

	  flags |= mask;
	}

      cur_column += len;
      if (cur_column > max_column)
	{
	  fprintf (stderr, ", \\\n%*s", (int)start_column, "");
	  cur_column = start_column + len;
	  comma = "";
	}

      fprintf (file, "%s%s%s%s", comma, prefix, no_str, name);
      comma = ", ";
      comma_len = strlen (", ");
    }

  fputs ("\n", file);
}

/* Helper function to print the current isa options on a line.  */

static void
rs6000_print_isa_options (FILE *file, int indent, const char *string,
			  HOST_WIDE_INT flags)
{
  rs6000_print_options_internal (file, indent, string, flags, "-m",
				 &rs6000_opt_masks[0],
				 ARRAY_SIZE (rs6000_opt_masks));
}

static void
rs6000_print_builtin_options (FILE *file, int indent, const char *string,
			      HOST_WIDE_INT flags)
{
  rs6000_print_options_internal (file, indent, string, flags, "",
				 &rs6000_builtin_mask_names[0],
				 ARRAY_SIZE (rs6000_builtin_mask_names));
}

/* If the user used -mno-vsx, we need turn off all of the implicit ISA 2.06,
   2.07, and 3.0 options that relate to the vector unit (-mdirect-move,
   -mupper-regs-df, etc.).

   If the user used -mno-power8-vector, we need to turn off all of the implicit
   ISA 2.07 and 3.0 options that relate to the vector unit.

   If the user used -mno-power9-vector, we need to turn off all of the implicit
   ISA 3.0 options that relate to the vector unit.

   This function does not handle explicit options such as the user specifying
   -mdirect-move.  These are handled in rs6000_option_override_internal, and
   the appropriate error is given if needed.

   We return a mask of all of the implicit options that should not be enabled
   by default.  */

static HOST_WIDE_INT
rs6000_disable_incompatible_switches (void)
{
  HOST_WIDE_INT ignore_masks = rs6000_isa_flags_explicit;
  size_t i, j;

  static const struct {
    const HOST_WIDE_INT no_flag;	/* flag explicitly turned off.  */
    const HOST_WIDE_INT dep_flags;	/* flags that depend on this option.  */
    const char *const name;		/* name of the switch.  */
  } flags[] = {
    { OPTION_MASK_POWER10,	OTHER_POWER10_MASKS,	"power10"	},
    { OPTION_MASK_P9_VECTOR,	OTHER_P9_VECTOR_MASKS,	"power9-vector"	},
    { OPTION_MASK_P8_VECTOR,	OTHER_P8_VECTOR_MASKS,	"power8-vector"	},
    { OPTION_MASK_VSX,		OTHER_VSX_VECTOR_MASKS,	"vsx"		},
    { OPTION_MASK_ALTIVEC,	OTHER_ALTIVEC_MASKS,	"altivec"	},
  };

  for (i = 0; i < ARRAY_SIZE (flags); i++)
    {
      HOST_WIDE_INT no_flag = flags[i].no_flag;

      if ((rs6000_isa_flags & no_flag) == 0
	  && (rs6000_isa_flags_explicit & no_flag) != 0)
	{
	  HOST_WIDE_INT dep_flags = flags[i].dep_flags;
	  HOST_WIDE_INT set_flags = (rs6000_isa_flags_explicit
				     & rs6000_isa_flags
				     & dep_flags);

	  if (set_flags)
	    {
	      for (j = 0; j < ARRAY_SIZE (rs6000_opt_masks); j++)
		if ((set_flags & rs6000_opt_masks[j].mask) != 0)
		  {
		    set_flags &= ~rs6000_opt_masks[j].mask;
		    error ("%<-mno-%s%> turns off %<-m%s%>",
			   flags[i].name,
			   rs6000_opt_masks[j].name);
		  }

	      gcc_assert (!set_flags);
	    }

	  rs6000_isa_flags &= ~dep_flags;
	  ignore_masks |= no_flag | dep_flags;
	}
    }

  return ignore_masks;
}


/* Helper function for printing the function name when debugging.  */

static const char *
get_decl_name (tree fn)
{
  tree name;

  if (!fn)
    return "<null>";

  name = DECL_NAME (fn);
  if (!name)
    return "<no-name>";

  return IDENTIFIER_POINTER (name);
}

/* Return the clone id of the target we are compiling code for in a target
   clone.  The clone id is ordered from 0 (default) to CLONE_MAX-1 and gives
   the priority list for the target clones (ordered from lowest to
   highest).  */

static int
rs6000_clone_priority (tree fndecl)
{
  tree fn_opts = DECL_FUNCTION_SPECIFIC_TARGET (fndecl);
  HOST_WIDE_INT isa_masks;
  int ret = CLONE_DEFAULT;
  tree attrs = lookup_attribute ("target", DECL_ATTRIBUTES (fndecl));
  const char *attrs_str = NULL;

  attrs = TREE_VALUE (TREE_VALUE (attrs));
  attrs_str = TREE_STRING_POINTER (attrs);

  /* Return priority zero for default function.  Return the ISA needed for the
     function if it is not the default.  */
  if (strcmp (attrs_str, "default") != 0)
    {
      if (fn_opts == NULL_TREE)
	fn_opts = target_option_default_node;

      if (!fn_opts || !TREE_TARGET_OPTION (fn_opts))
	isa_masks = rs6000_isa_flags;
      else
	isa_masks = TREE_TARGET_OPTION (fn_opts)->x_rs6000_isa_flags;

      for (ret = CLONE_MAX - 1; ret != 0; ret--)
	if ((rs6000_clone_map[ret].isa_mask & isa_masks) != 0)
	  break;
    }

  if (TARGET_DEBUG_TARGET)
    fprintf (stderr, "rs6000_get_function_version_priority (%s) => %d\n",
	     get_decl_name (fndecl), ret);

  return ret;
}

/* This compares the priority of target features in function DECL1 and DECL2.
   It returns positive value if DECL1 is higher priority, negative value if
   DECL2 is higher priority and 0 if they are the same.  Note, priorities are
   ordered from lowest (CLONE_DEFAULT) to highest (currently CLONE_ISA_3_0).  */

static int
rs6000_compare_version_priority (tree decl1, tree decl2)
{
  int priority1 = rs6000_clone_priority (decl1);
  int priority2 = rs6000_clone_priority (decl2);
  int ret = priority1 - priority2;

  if (TARGET_DEBUG_TARGET)
    fprintf (stderr, "rs6000_compare_version_priority (%s, %s) => %d\n",
	     get_decl_name (decl1), get_decl_name (decl2), ret);

  return ret;
}

/* Make a dispatcher declaration for the multi-versioned function DECL.
   Calls to DECL function will be replaced with calls to the dispatcher
   by the front-end.  Returns the decl of the dispatcher function.  */

static tree
rs6000_get_function_versions_dispatcher (void *decl)
{
  tree fn = (tree) decl;
  struct cgraph_node *node = NULL;
  struct cgraph_node *default_node = NULL;
  struct cgraph_function_version_info *node_v = NULL;
  struct cgraph_function_version_info *first_v = NULL;

  tree dispatch_decl = NULL;

  struct cgraph_function_version_info *default_version_info = NULL;
  gcc_assert (fn != NULL && DECL_FUNCTION_VERSIONED (fn));

  if (TARGET_DEBUG_TARGET)
    fprintf (stderr, "rs6000_get_function_versions_dispatcher (%s)\n",
	     get_decl_name (fn));

  node = cgraph_node::get (fn);
  gcc_assert (node != NULL);

  node_v = node->function_version ();
  gcc_assert (node_v != NULL);

  if (node_v->dispatcher_resolver != NULL)
    return node_v->dispatcher_resolver;

  /* Find the default version and make it the first node.  */
  first_v = node_v;
  /* Go to the beginning of the chain.  */
  while (first_v->prev != NULL)
    first_v = first_v->prev;

  default_version_info = first_v;
  while (default_version_info != NULL)
    {
      const tree decl2 = default_version_info->this_node->decl;
      if (is_function_default_version (decl2))
        break;
      default_version_info = default_version_info->next;
    }

  /* If there is no default node, just return NULL.  */
  if (default_version_info == NULL)
    return NULL;

  /* Make default info the first node.  */
  if (first_v != default_version_info)
    {
      default_version_info->prev->next = default_version_info->next;
      if (default_version_info->next)
        default_version_info->next->prev = default_version_info->prev;
      first_v->prev = default_version_info;
      default_version_info->next = first_v;
      default_version_info->prev = NULL;
    }

  default_node = default_version_info->this_node;

#ifndef TARGET_LIBC_PROVIDES_HWCAP_IN_TCB
  error_at (DECL_SOURCE_LOCATION (default_node->decl),
	    "%<target_clones%> attribute needs GLIBC (2.23 and newer) that "
	    "exports hardware capability bits");
#else

  if (targetm.has_ifunc_p ())
    {
      struct cgraph_function_version_info *it_v = NULL;
      struct cgraph_node *dispatcher_node = NULL;
      struct cgraph_function_version_info *dispatcher_version_info = NULL;

      /* Right now, the dispatching is done via ifunc.  */
      dispatch_decl = make_dispatcher_decl (default_node->decl);

      dispatcher_node = cgraph_node::get_create (dispatch_decl);
      gcc_assert (dispatcher_node != NULL);
      dispatcher_node->dispatcher_function = 1;
      dispatcher_version_info
	= dispatcher_node->insert_new_function_version ();
      dispatcher_version_info->next = default_version_info;
      dispatcher_node->definition = 1;

      /* Set the dispatcher for all the versions.  */
      it_v = default_version_info;
      while (it_v != NULL)
	{
	  it_v->dispatcher_resolver = dispatch_decl;
	  it_v = it_v->next;
	}
    }
  else
    {
      error_at (DECL_SOURCE_LOCATION (default_node->decl),
		"multiversioning needs ifunc which is not supported "
		"on this target");
    }
#endif

  return dispatch_decl;
}

/* Make the resolver function decl to dispatch the versions of a multi-
   versioned function, DEFAULT_DECL.  Create an empty basic block in the
   resolver and store the pointer in EMPTY_BB.  Return the decl of the resolver
   function.  */

static tree
make_resolver_func (const tree default_decl,
		    const tree dispatch_decl,
		    basic_block *empty_bb)
{
  /* Make the resolver function static.  The resolver function returns
     void *.  */
  tree decl_name = clone_function_name (default_decl, "resolver");
  const char *resolver_name = IDENTIFIER_POINTER (decl_name);
  tree type = build_function_type_list (ptr_type_node, NULL_TREE);
  tree decl = build_fn_decl (resolver_name, type);
  SET_DECL_ASSEMBLER_NAME (decl, decl_name);

  DECL_NAME (decl) = decl_name;
  TREE_USED (decl) = 1;
  DECL_ARTIFICIAL (decl) = 1;
  DECL_IGNORED_P (decl) = 0;
  TREE_PUBLIC (decl) = 0;
  DECL_UNINLINABLE (decl) = 1;

  /* Resolver is not external, body is generated.  */
  DECL_EXTERNAL (decl) = 0;
  DECL_EXTERNAL (dispatch_decl) = 0;

  DECL_CONTEXT (decl) = NULL_TREE;
  DECL_INITIAL (decl) = make_node (BLOCK);
  DECL_STATIC_CONSTRUCTOR (decl) = 0;

  if (DECL_COMDAT_GROUP (default_decl)
      || TREE_PUBLIC (default_decl))
    {
      /* In this case, each translation unit with a call to this
	 versioned function will put out a resolver.  Ensure it
	 is comdat to keep just one copy.  */
      DECL_COMDAT (decl) = 1;
      make_decl_one_only (decl, DECL_ASSEMBLER_NAME (decl));
    }
  else
    TREE_PUBLIC (dispatch_decl) = 0;

  /* Build result decl and add to function_decl.  */
  tree t = build_decl (UNKNOWN_LOCATION, RESULT_DECL, NULL_TREE, ptr_type_node);
  DECL_CONTEXT (t) = decl;
  DECL_ARTIFICIAL (t) = 1;
  DECL_IGNORED_P (t) = 1;
  DECL_RESULT (decl) = t;

  gimplify_function_tree (decl);
  push_cfun (DECL_STRUCT_FUNCTION (decl));
  *empty_bb = init_lowered_empty_function (decl, false,
					   profile_count::uninitialized ());

  cgraph_node::add_new_function (decl, true);
  symtab->call_cgraph_insertion_hooks (cgraph_node::get_create (decl));

  pop_cfun ();

  /* Mark dispatch_decl as "ifunc" with resolver as resolver_name.  */
  DECL_ATTRIBUTES (dispatch_decl)
    = make_attribute ("ifunc", resolver_name, DECL_ATTRIBUTES (dispatch_decl));

  cgraph_node::create_same_body_alias (dispatch_decl, decl);

  return decl;
}

/* This adds a condition to the basic_block NEW_BB in function FUNCTION_DECL to
   return a pointer to VERSION_DECL if we are running on a machine that
   supports the index CLONE_ISA hardware architecture bits.  This function will
   be called during version dispatch to decide which function version to
   execute.  It returns the basic block at the end, to which more conditions
   can be added.  */

static basic_block
add_condition_to_bb (tree function_decl, tree version_decl,
		     int clone_isa, basic_block new_bb)
{
  push_cfun (DECL_STRUCT_FUNCTION (function_decl));

  gcc_assert (new_bb != NULL);
  gimple_seq gseq = bb_seq (new_bb);


  tree convert_expr = build1 (CONVERT_EXPR, ptr_type_node,
			      build_fold_addr_expr (version_decl));
  tree result_var = create_tmp_var (ptr_type_node);
  gimple *convert_stmt = gimple_build_assign (result_var, convert_expr);
  gimple *return_stmt = gimple_build_return (result_var);

  if (clone_isa == CLONE_DEFAULT)
    {
      gimple_seq_add_stmt (&gseq, convert_stmt);
      gimple_seq_add_stmt (&gseq, return_stmt);
      set_bb_seq (new_bb, gseq);
      gimple_set_bb (convert_stmt, new_bb);
      gimple_set_bb (return_stmt, new_bb);
      pop_cfun ();
      return new_bb;
    }

  tree bool_zero = build_int_cst (bool_int_type_node, 0);
  tree cond_var = create_tmp_var (bool_int_type_node);
  tree predicate_decl = rs6000_builtin_decls [(int) RS6000_BUILTIN_CPU_SUPPORTS];
  const char *arg_str = rs6000_clone_map[clone_isa].name;
  tree predicate_arg = build_string_literal (strlen (arg_str) + 1, arg_str);
  gimple *call_cond_stmt = gimple_build_call (predicate_decl, 1, predicate_arg);
  gimple_call_set_lhs (call_cond_stmt, cond_var);

  gimple_set_block (call_cond_stmt, DECL_INITIAL (function_decl));
  gimple_set_bb (call_cond_stmt, new_bb);
  gimple_seq_add_stmt (&gseq, call_cond_stmt);

  gimple *if_else_stmt = gimple_build_cond (NE_EXPR, cond_var, bool_zero,
					    NULL_TREE, NULL_TREE);
  gimple_set_block (if_else_stmt, DECL_INITIAL (function_decl));
  gimple_set_bb (if_else_stmt, new_bb);
  gimple_seq_add_stmt (&gseq, if_else_stmt);

  gimple_seq_add_stmt (&gseq, convert_stmt);
  gimple_seq_add_stmt (&gseq, return_stmt);
  set_bb_seq (new_bb, gseq);

  basic_block bb1 = new_bb;
  edge e12 = split_block (bb1, if_else_stmt);
  basic_block bb2 = e12->dest;
  e12->flags &= ~EDGE_FALLTHRU;
  e12->flags |= EDGE_TRUE_VALUE;

  edge e23 = split_block (bb2, return_stmt);
  gimple_set_bb (convert_stmt, bb2);
  gimple_set_bb (return_stmt, bb2);

  basic_block bb3 = e23->dest;
  make_edge (bb1, bb3, EDGE_FALSE_VALUE);

  remove_edge (e23);
  make_edge (bb2, EXIT_BLOCK_PTR_FOR_FN (cfun), 0);

  pop_cfun ();
  return bb3;
}

/* This function generates the dispatch function for multi-versioned functions.
   DISPATCH_DECL is the function which will contain the dispatch logic.
   FNDECLS are the function choices for dispatch, and is a tree chain.
   EMPTY_BB is the basic block pointer in DISPATCH_DECL in which the dispatch
   code is generated.  */

static int
dispatch_function_versions (tree dispatch_decl,
			    void *fndecls_p,
			    basic_block *empty_bb)
{
  int ix;
  tree ele;
  vec<tree> *fndecls;
  tree clones[CLONE_MAX];

  if (TARGET_DEBUG_TARGET)
    fputs ("dispatch_function_versions, top\n", stderr);

  gcc_assert (dispatch_decl != NULL
	      && fndecls_p != NULL
	      && empty_bb != NULL);

  /* fndecls_p is actually a vector.  */
  fndecls = static_cast<vec<tree> *> (fndecls_p);

  /* At least one more version other than the default.  */
  gcc_assert (fndecls->length () >= 2);

  /* The first version in the vector is the default decl.  */
  memset ((void *) clones, '\0', sizeof (clones));
  clones[CLONE_DEFAULT] = (*fndecls)[0];

  /* On the PowerPC, we do not need to call __builtin_cpu_init, which is a NOP
     on the PowerPC (on the x86_64, it is not a NOP).  The builtin function
     __builtin_cpu_support ensures that the TOC fields are setup by requiring a
     recent glibc.  If we ever need to call __builtin_cpu_init, we would need
     to insert the code here to do the call.  */

  for (ix = 1; fndecls->iterate (ix, &ele); ++ix)
    {
      int priority = rs6000_clone_priority (ele);
      if (!clones[priority])
	clones[priority] = ele;
    }

  for (ix = CLONE_MAX - 1; ix >= 0; ix--)
    if (clones[ix])
      {
	if (TARGET_DEBUG_TARGET)
	  fprintf (stderr, "dispatch_function_versions, clone %d, %s\n",
		   ix, get_decl_name (clones[ix]));

	*empty_bb = add_condition_to_bb (dispatch_decl, clones[ix], ix,
					 *empty_bb);
      }

  return 0;
}

/* Generate the dispatching code body to dispatch multi-versioned function
   DECL.  The target hook is called to process the "target" attributes and
   provide the code to dispatch the right function at run-time.  NODE points
   to the dispatcher decl whose body will be created.  */

static tree
rs6000_generate_version_dispatcher_body (void *node_p)
{
  tree resolver;
  basic_block empty_bb;
  struct cgraph_node *node = (cgraph_node *) node_p;
  struct cgraph_function_version_info *ninfo = node->function_version ();

  if (ninfo->dispatcher_resolver)
    return ninfo->dispatcher_resolver;

  /* node is going to be an alias, so remove the finalized bit.  */
  node->definition = false;

  /* The first version in the chain corresponds to the default version.  */
  ninfo->dispatcher_resolver = resolver
    = make_resolver_func (ninfo->next->this_node->decl, node->decl, &empty_bb);

  if (TARGET_DEBUG_TARGET)
    fprintf (stderr, "rs6000_get_function_versions_dispatcher, %s\n",
	     get_decl_name (resolver));

  push_cfun (DECL_STRUCT_FUNCTION (resolver));
  auto_vec<tree, 2> fn_ver_vec;

  for (struct cgraph_function_version_info *vinfo = ninfo->next;
       vinfo;
       vinfo = vinfo->next)
    {
      struct cgraph_node *version = vinfo->this_node;
      /* Check for virtual functions here again, as by this time it should
	 have been determined if this function needs a vtable index or
	 not.  This happens for methods in derived classes that override
	 virtual methods in base classes but are not explicitly marked as
	 virtual.  */
      if (DECL_VINDEX (version->decl))
	sorry ("Virtual function multiversioning not supported");

      fn_ver_vec.safe_push (version->decl);
    }

  dispatch_function_versions (resolver, &fn_ver_vec, &empty_bb);
  cgraph_edge::rebuild_edges ();
  pop_cfun ();
  return resolver;
}


/* Hook to determine if one function can safely inline another.  */

static bool
rs6000_can_inline_p (tree caller, tree callee)
{
  bool ret = false;
  tree caller_tree = DECL_FUNCTION_SPECIFIC_TARGET (caller);
  tree callee_tree = DECL_FUNCTION_SPECIFIC_TARGET (callee);

  /* If the callee has no option attributes, then it is ok to inline.  */
  if (!callee_tree)
    ret = true;

  else
    {
      HOST_WIDE_INT caller_isa;
      struct cl_target_option *callee_opts = TREE_TARGET_OPTION (callee_tree);
      HOST_WIDE_INT callee_isa = callee_opts->x_rs6000_isa_flags;
      HOST_WIDE_INT explicit_isa = callee_opts->x_rs6000_isa_flags_explicit;

      /* If the caller has option attributes, then use them.
	 Otherwise, use the command line options.  */
      if (caller_tree)
	caller_isa = TREE_TARGET_OPTION (caller_tree)->x_rs6000_isa_flags;
      else
	caller_isa = rs6000_isa_flags;

      /* The callee's options must be a subset of the caller's options, i.e.
	 a vsx function may inline an altivec function, but a no-vsx function
	 must not inline a vsx function.  However, for those options that the
	 callee has explicitly enabled or disabled, then we must enforce that
	 the callee's and caller's options match exactly; see PR70010.  */
      if (((caller_isa & callee_isa) == callee_isa)
	  && (caller_isa & explicit_isa) == (callee_isa & explicit_isa))
	ret = true;
    }

  if (TARGET_DEBUG_TARGET)
    fprintf (stderr, "rs6000_can_inline_p:, caller %s, callee %s, %s inline\n",
	     get_decl_name (caller), get_decl_name (callee),
	     (ret ? "can" : "cannot"));

  return ret;
}

/* Allocate a stack temp and fixup the address so it meets the particular
   memory requirements (either offetable or REG+REG addressing).  */

rtx
rs6000_allocate_stack_temp (machine_mode mode,
			    bool offsettable_p,
			    bool reg_reg_p)
{
  rtx stack = assign_stack_temp (mode, GET_MODE_SIZE (mode));
  rtx addr = XEXP (stack, 0);
  int strict_p = reload_completed;

  if (!legitimate_indirect_address_p (addr, strict_p))
    {
      if (offsettable_p
	  && !rs6000_legitimate_offset_address_p (mode, addr, strict_p, true))
	stack = replace_equiv_address (stack, copy_addr_to_reg (addr));

      else if (reg_reg_p && !legitimate_indexed_address_p (addr, strict_p))
	stack = replace_equiv_address (stack, copy_addr_to_reg (addr));
    }

  return stack;
}

/* Given a memory reference, if it is not a reg or reg+reg addressing,
   convert to such a form to deal with memory reference instructions
   like STFIWX and LDBRX that only take reg+reg addressing.  */

rtx
rs6000_force_indexed_or_indirect_mem (rtx x)
{
  machine_mode mode = GET_MODE (x);

  gcc_assert (MEM_P (x));
  if (can_create_pseudo_p () && !indexed_or_indirect_operand (x, mode))
    {
      rtx addr = XEXP (x, 0);
      if (GET_CODE (addr) == PRE_INC || GET_CODE (addr) == PRE_DEC)
	{
	  rtx reg = XEXP (addr, 0);
	  HOST_WIDE_INT size = GET_MODE_SIZE (GET_MODE (x));
	  rtx size_rtx = GEN_INT ((GET_CODE (addr) == PRE_DEC) ? -size : size);
	  gcc_assert (REG_P (reg));
	  emit_insn (gen_add3_insn (reg, reg, size_rtx));
	  addr = reg;
	}
      else if (GET_CODE (addr) == PRE_MODIFY)
	{
	  rtx reg = XEXP (addr, 0);
	  rtx expr = XEXP (addr, 1);
	  gcc_assert (REG_P (reg));
	  gcc_assert (GET_CODE (expr) == PLUS);
	  emit_insn (gen_add3_insn (reg, XEXP (expr, 0), XEXP (expr, 1)));
	  addr = reg;
	}

      if (GET_CODE (addr) == PLUS)
	{
	  rtx op0 = XEXP (addr, 0);
	  rtx op1 = XEXP (addr, 1);
	  op0 = force_reg (Pmode, op0);
	  op1 = force_reg (Pmode, op1);
	  x = replace_equiv_address (x, gen_rtx_PLUS (Pmode, op0, op1));
	}
      else
	x = replace_equiv_address (x, force_reg (Pmode, addr));
    }

  return x;
}

/* Implement TARGET_LEGITIMATE_CONSTANT_P.

   On the RS/6000, all integer constants are acceptable, most won't be valid
   for particular insns, though.  Only easy FP constants are acceptable.  */

static bool
rs6000_legitimate_constant_p (machine_mode mode, rtx x)
{
  if (TARGET_ELF && tls_referenced_p (x))
    return false;

  if (CONST_DOUBLE_P (x))
    return easy_fp_constant (x, mode);

  if (GET_CODE (x) == CONST_VECTOR)
    return easy_vector_constant (x, mode);

  return true;
}


/* Return TRUE iff the sequence ending in LAST sets the static chain.  */

static bool
chain_already_loaded (rtx_insn *last)
{
  for (; last != NULL; last = PREV_INSN (last))
    {
      if (NONJUMP_INSN_P (last))
	{
	  rtx patt = PATTERN (last);

	  if (GET_CODE (patt) == SET)
	    {
	      rtx lhs = XEXP (patt, 0);

	      if (REG_P (lhs) && REGNO (lhs) == STATIC_CHAIN_REGNUM)
		return true;
	    }
	}
    }
  return false;
}

/* Expand code to perform a call under the AIX or ELFv2 ABI.  */

void
rs6000_call_aix (rtx value, rtx func_desc, rtx tlsarg, rtx cookie)
{
  rtx func = func_desc;
  rtx toc_reg = gen_rtx_REG (Pmode, TOC_REGNUM);
  rtx toc_load = NULL_RTX;
  rtx toc_restore = NULL_RTX;
  rtx func_addr;
  rtx abi_reg = NULL_RTX;
  rtx call[5];
  int n_call;
  rtx insn;
  bool is_pltseq_longcall;

  if (global_tlsarg)
    tlsarg = global_tlsarg;

  /* Handle longcall attributes.  */
  is_pltseq_longcall = false;
  if ((INTVAL (cookie) & CALL_LONG) != 0
      && GET_CODE (func_desc) == SYMBOL_REF)
    {
      func = rs6000_longcall_ref (func_desc, tlsarg);
      if (TARGET_PLTSEQ)
	is_pltseq_longcall = true;
    }

  /* Handle indirect calls.  */
  if (!SYMBOL_REF_P (func)
      || (DEFAULT_ABI == ABI_AIX && !SYMBOL_REF_FUNCTION_P (func)))
    {
      if (!rs6000_pcrel_p (cfun))
	{
	  /* Save the TOC into its reserved slot before the call,
	     and prepare to restore it after the call.  */
	  rtx stack_toc_offset = GEN_INT (RS6000_TOC_SAVE_SLOT);
	  rtx stack_toc_unspec = gen_rtx_UNSPEC (Pmode,
						 gen_rtvec (1, stack_toc_offset),
						 UNSPEC_TOCSLOT);
	  toc_restore = gen_rtx_SET (toc_reg, stack_toc_unspec);

	  /* Can we optimize saving the TOC in the prologue or
	     do we need to do it at every call?  */
	  if (TARGET_SAVE_TOC_INDIRECT && !cfun->calls_alloca)
	    cfun->machine->save_toc_in_prologue = true;
	  else
	    {
	      rtx stack_ptr = gen_rtx_REG (Pmode, STACK_POINTER_REGNUM);
	      rtx stack_toc_mem = gen_frame_mem (Pmode,
						 gen_rtx_PLUS (Pmode, stack_ptr,
							       stack_toc_offset));
	      MEM_VOLATILE_P (stack_toc_mem) = 1;
	      if (is_pltseq_longcall)
		{
		  rtvec v = gen_rtvec (3, toc_reg, func_desc, tlsarg);
		  rtx mark_toc_reg = gen_rtx_UNSPEC (Pmode, v, UNSPEC_PLTSEQ);
		  emit_insn (gen_rtx_SET (stack_toc_mem, mark_toc_reg));
		}
	      else
		emit_move_insn (stack_toc_mem, toc_reg);
	    }
	}

      if (DEFAULT_ABI == ABI_ELFv2)
	{
	  /* A function pointer in the ELFv2 ABI is just a plain address, but
	     the ABI requires it to be loaded into r12 before the call.  */
	  func_addr = gen_rtx_REG (Pmode, 12);
	  if (!rtx_equal_p (func_addr, func))
	    emit_move_insn (func_addr, func);
	  abi_reg = func_addr;
	  /* Indirect calls via CTR are strongly preferred over indirect
	     calls via LR, so move the address there.  Needed to mark
	     this insn for linker plt sequence editing too.  */
	  func_addr = gen_rtx_REG (Pmode, CTR_REGNO);
	  if (is_pltseq_longcall)
	    {
	      rtvec v = gen_rtvec (3, abi_reg, func_desc, tlsarg);
	      rtx mark_func = gen_rtx_UNSPEC (Pmode, v, UNSPEC_PLTSEQ);
	      emit_insn (gen_rtx_SET (func_addr, mark_func));
	      v = gen_rtvec (2, func_addr, func_desc);
	      func_addr = gen_rtx_UNSPEC (Pmode, v, UNSPEC_PLTSEQ);
	    }
	  else
	    emit_move_insn (func_addr, abi_reg);
	}
      else
	{
	  /* A function pointer under AIX is a pointer to a data area whose
	     first word contains the actual address of the function, whose
	     second word contains a pointer to its TOC, and whose third word
	     contains a value to place in the static chain register (r11).
	     Note that if we load the static chain, our "trampoline" need
	     not have any executable code.  */

	  /* Load up address of the actual function.  */
	  func = force_reg (Pmode, func);
	  func_addr = gen_reg_rtx (Pmode);
	  emit_move_insn (func_addr, gen_rtx_MEM (Pmode, func));

	  /* Indirect calls via CTR are strongly preferred over indirect
	     calls via LR, so move the address there.  */
	  rtx ctr_reg = gen_rtx_REG (Pmode, CTR_REGNO);
	  emit_move_insn (ctr_reg, func_addr);
	  func_addr = ctr_reg;

	  /* Prepare to load the TOC of the called function.  Note that the
	     TOC load must happen immediately before the actual call so
	     that unwinding the TOC registers works correctly.  See the
	     comment in frob_update_context.  */
	  rtx func_toc_offset = GEN_INT (GET_MODE_SIZE (Pmode));
	  rtx func_toc_mem = gen_rtx_MEM (Pmode,
					  gen_rtx_PLUS (Pmode, func,
							func_toc_offset));
	  toc_load = gen_rtx_USE (VOIDmode, func_toc_mem);

	  /* If we have a static chain, load it up.  But, if the call was
	     originally direct, the 3rd word has not been written since no
	     trampoline has been built, so we ought not to load it, lest we
	     override a static chain value.  */
	  if (!(GET_CODE (func_desc) == SYMBOL_REF
		&& SYMBOL_REF_FUNCTION_P (func_desc))
	      && TARGET_POINTERS_TO_NESTED_FUNCTIONS
	      && !chain_already_loaded (get_current_sequence ()->next->last))
	    {
	      rtx sc_reg = gen_rtx_REG (Pmode, STATIC_CHAIN_REGNUM);
	      rtx func_sc_offset = GEN_INT (2 * GET_MODE_SIZE (Pmode));
	      rtx func_sc_mem = gen_rtx_MEM (Pmode,
					     gen_rtx_PLUS (Pmode, func,
							   func_sc_offset));
	      emit_move_insn (sc_reg, func_sc_mem);
	      abi_reg = sc_reg;
	    }
	}
    }
  else
    {
      /* No TOC register needed for calls from PC-relative callers.  */
      if (!rs6000_pcrel_p (cfun))
	/* Direct calls use the TOC: for local calls, the callee will
	   assume the TOC register is set; for non-local calls, the
	   PLT stub needs the TOC register.  */
	abi_reg = toc_reg;
      func_addr = func;
    }

  /* Create the call.  */
  call[0] = gen_rtx_CALL (VOIDmode, gen_rtx_MEM (SImode, func_addr), tlsarg);
  if (value != NULL_RTX)
    call[0] = gen_rtx_SET (value, call[0]);
  call[1] = gen_rtx_USE (VOIDmode, cookie);
  n_call = 2;

  if (toc_load)
    call[n_call++] = toc_load;
  if (toc_restore)
    call[n_call++] = toc_restore;

  call[n_call++] = gen_hard_reg_clobber (Pmode, LR_REGNO);

  insn = gen_rtx_PARALLEL (VOIDmode, gen_rtvec_v (n_call, call));
  insn = emit_call_insn (insn);

  /* Mention all registers defined by the ABI to hold information
     as uses in CALL_INSN_FUNCTION_USAGE.  */
  if (abi_reg)
    use_reg (&CALL_INSN_FUNCTION_USAGE (insn), abi_reg);
}

/* Expand code to perform a sibling call under the AIX or ELFv2 ABI.  */

void
rs6000_sibcall_aix (rtx value, rtx func_desc, rtx tlsarg, rtx cookie)
{
  rtx call[2];
  rtx insn;

  gcc_assert (INTVAL (cookie) == 0);

  if (global_tlsarg)
    tlsarg = global_tlsarg;

  /* Create the call.  */
  call[0] = gen_rtx_CALL (VOIDmode, gen_rtx_MEM (SImode, func_desc), tlsarg);
  if (value != NULL_RTX)
    call[0] = gen_rtx_SET (value, call[0]);

  call[1] = simple_return_rtx;

  insn = gen_rtx_PARALLEL (VOIDmode, gen_rtvec_v (2, call));
  insn = emit_call_insn (insn);

  /* Note use of the TOC register.  */
  if (!rs6000_pcrel_p (cfun))
    use_reg (&CALL_INSN_FUNCTION_USAGE (insn),
	     gen_rtx_REG (Pmode, TOC_REGNUM));
}

/* Expand code to perform a call under the SYSV4 ABI.  */

void
rs6000_call_sysv (rtx value, rtx func_desc, rtx tlsarg, rtx cookie)
{
  rtx func = func_desc;
  rtx func_addr;
  rtx call[4];
  rtx insn;
  rtx abi_reg = NULL_RTX;
  int n;

  if (global_tlsarg)
    tlsarg = global_tlsarg;

  /* Handle longcall attributes.  */
  if ((INTVAL (cookie) & CALL_LONG) != 0
      && GET_CODE (func_desc) == SYMBOL_REF)
    {
      func = rs6000_longcall_ref (func_desc, tlsarg);
      /* If the longcall was implemented as an inline PLT call using
	 PLT unspecs then func will be REG:r11.  If not, func will be
	 a pseudo reg.  The inline PLT call sequence supports lazy
	 linking (and longcalls to functions in dlopen'd libraries).
	 The other style of longcalls don't.  The lazy linking entry
	 to the dynamic symbol resolver requires r11 be the function
	 address (as it is for linker generated PLT stubs).  Ensure
	 r11 stays valid to the bctrl by marking r11 used by the call.  */
      if (TARGET_PLTSEQ)
	abi_reg = func;
    }

  /* Handle indirect calls.  */
  if (GET_CODE (func) != SYMBOL_REF)
    {
      func = force_reg (Pmode, func);

      /* Indirect calls via CTR are strongly preferred over indirect
	 calls via LR, so move the address there.  That can't be left
	 to reload because we want to mark every instruction in an
	 inline PLT call sequence with a reloc, enabling the linker to
	 edit the sequence back to a direct call when that makes sense.  */
      func_addr = gen_rtx_REG (Pmode, CTR_REGNO);
      if (abi_reg)
	{
	  rtvec v = gen_rtvec (3, func, func_desc, tlsarg);
	  rtx mark_func = gen_rtx_UNSPEC (Pmode, v, UNSPEC_PLTSEQ);
	  emit_insn (gen_rtx_SET (func_addr, mark_func));
	  v = gen_rtvec (2, func_addr, func_desc);
	  func_addr = gen_rtx_UNSPEC (Pmode, v, UNSPEC_PLTSEQ);
	}
      else
	emit_move_insn (func_addr, func);
    }
  else
    func_addr = func;

  /* Create the call.  */
  call[0] = gen_rtx_CALL (VOIDmode, gen_rtx_MEM (SImode, func_addr), tlsarg);
  if (value != NULL_RTX)
    call[0] = gen_rtx_SET (value, call[0]);

  call[1] = gen_rtx_USE (VOIDmode, cookie);
  n = 2;
  if (TARGET_SECURE_PLT
      && flag_pic
      && GET_CODE (func_addr) == SYMBOL_REF
      && !SYMBOL_REF_LOCAL_P (func_addr))
    call[n++] = gen_rtx_USE (VOIDmode, pic_offset_table_rtx);

  call[n++] = gen_hard_reg_clobber (Pmode, LR_REGNO);

  insn = gen_rtx_PARALLEL (VOIDmode, gen_rtvec_v (n, call));
  insn = emit_call_insn (insn);
  if (abi_reg)
    use_reg (&CALL_INSN_FUNCTION_USAGE (insn), abi_reg);
}

/* Expand code to perform a sibling call under the SysV4 ABI.  */

void
rs6000_sibcall_sysv (rtx value, rtx func_desc, rtx tlsarg, rtx cookie)
{
  rtx func = func_desc;
  rtx func_addr;
  rtx call[3];
  rtx insn;
  rtx abi_reg = NULL_RTX;

  if (global_tlsarg)
    tlsarg = global_tlsarg;

  /* Handle longcall attributes.  */
  if ((INTVAL (cookie) & CALL_LONG) != 0
      && GET_CODE (func_desc) == SYMBOL_REF)
    {
      func = rs6000_longcall_ref (func_desc, tlsarg);
      /* If the longcall was implemented as an inline PLT call using
	 PLT unspecs then func will be REG:r11.  If not, func will be
	 a pseudo reg.  The inline PLT call sequence supports lazy
	 linking (and longcalls to functions in dlopen'd libraries).
	 The other style of longcalls don't.  The lazy linking entry
	 to the dynamic symbol resolver requires r11 be the function
	 address (as it is for linker generated PLT stubs).  Ensure
	 r11 stays valid to the bctr by marking r11 used by the call.  */
      if (TARGET_PLTSEQ)
	abi_reg = func;
    }

  /* Handle indirect calls.  */
  if (GET_CODE (func) != SYMBOL_REF)
    {
      func = force_reg (Pmode, func);

      /* Indirect sibcalls must go via CTR.  That can't be left to
	 reload because we want to mark every instruction in an inline
	 PLT call sequence with a reloc, enabling the linker to edit
	 the sequence back to a direct call when that makes sense.  */
      func_addr = gen_rtx_REG (Pmode, CTR_REGNO);
      if (abi_reg)
	{
	  rtvec v = gen_rtvec (3, func, func_desc, tlsarg);
	  rtx mark_func = gen_rtx_UNSPEC (Pmode, v, UNSPEC_PLTSEQ);
	  emit_insn (gen_rtx_SET (func_addr, mark_func));
	  v = gen_rtvec (2, func_addr, func_desc);
	  func_addr = gen_rtx_UNSPEC (Pmode, v, UNSPEC_PLTSEQ);
	}
      else
	emit_move_insn (func_addr, func);
    }
  else
    func_addr = func;

  /* Create the call.  */
  call[0] = gen_rtx_CALL (VOIDmode, gen_rtx_MEM (SImode, func_addr), tlsarg);
  if (value != NULL_RTX)
    call[0] = gen_rtx_SET (value, call[0]);

  call[1] = gen_rtx_USE (VOIDmode, cookie);
  call[2] = simple_return_rtx;

  insn = gen_rtx_PARALLEL (VOIDmode, gen_rtvec_v (3, call));
  insn = emit_call_insn (insn);
  if (abi_reg)
    use_reg (&CALL_INSN_FUNCTION_USAGE (insn), abi_reg);
}

#if TARGET_MACHO

/* Expand code to perform a call under the Darwin ABI.
   Modulo handling of mlongcall, this is much the same as sysv.
   if/when the longcall optimisation is removed, we could drop this
   code and use the sysv case (taking care to avoid the tls stuff).

   We can use this for sibcalls too, if needed.  */

void
rs6000_call_darwin_1 (rtx value, rtx func_desc, rtx tlsarg,
		      rtx cookie, bool sibcall)
{
  rtx func = func_desc;
  rtx func_addr;
  rtx call[3];
  rtx insn;
  int cookie_val = INTVAL (cookie);
  bool make_island = false;

  /* Handle longcall attributes, there are two cases for Darwin:
     1) Newer linkers are capable of synthesising any branch islands needed.
     2) We need a helper branch island synthesised by the compiler.
     The second case has mostly been retired and we don't use it for m64.
     In fact, it's is an optimisation, we could just indirect as sysv does..
     ... however, backwards compatibility for now.
     If we're going to use this, then we need to keep the CALL_LONG bit set,
     so that we can pick up the special insn form later.  */
  if ((cookie_val & CALL_LONG) != 0
      && GET_CODE (func_desc) == SYMBOL_REF)
    {
      /* FIXME: the longcall opt should not hang off this flag, it is most
	 likely incorrect for kernel-mode code-generation.  */
      if (darwin_symbol_stubs && TARGET_32BIT)
	make_island = true; /* Do nothing yet, retain the CALL_LONG flag.  */
      else
	{
	  /* The linker is capable of doing this, but the user explicitly
	     asked for -mlongcall, so we'll do the 'normal' version.  */
	  func = rs6000_longcall_ref (func_desc, NULL_RTX);
	  cookie_val &= ~CALL_LONG; /* Handled, zap it.  */
	}
    }

  /* Handle indirect calls.  */
  if (GET_CODE (func) != SYMBOL_REF)
    {
      func = force_reg (Pmode, func);

      /* Indirect calls via CTR are strongly preferred over indirect
	 calls via LR, and are required for indirect sibcalls, so move
	 the address there.   */
      func_addr = gen_rtx_REG (Pmode, CTR_REGNO);
      emit_move_insn (func_addr, func);
    }
  else
    func_addr = func;

  /* Create the call.  */
  call[0] = gen_rtx_CALL (VOIDmode, gen_rtx_MEM (SImode, func_addr), tlsarg);
  if (value != NULL_RTX)
    call[0] = gen_rtx_SET (value, call[0]);

  call[1] = gen_rtx_USE (VOIDmode, GEN_INT (cookie_val));

  if (sibcall)
    call[2] = simple_return_rtx;
  else
    call[2] = gen_hard_reg_clobber (Pmode, LR_REGNO);

  insn = gen_rtx_PARALLEL (VOIDmode, gen_rtvec_v (3, call));
  insn = emit_call_insn (insn);
  /* Now we have the debug info in the insn, we can set up the branch island
     if we're using one.  */
  if (make_island)
    {
      tree funname = get_identifier (XSTR (func_desc, 0));

      if (no_previous_def (funname))
	{
	  rtx label_rtx = gen_label_rtx ();
	  char *label_buf, temp_buf[256];
	  ASM_GENERATE_INTERNAL_LABEL (temp_buf, "L",
				       CODE_LABEL_NUMBER (label_rtx));
	  label_buf = temp_buf[0] == '*' ? temp_buf + 1 : temp_buf;
	  tree labelname = get_identifier (label_buf);
	  add_compiler_branch_island (labelname, funname,
				     insn_line ((const rtx_insn*)insn));
	}
     }
}
#endif

void
rs6000_call_darwin (rtx value ATTRIBUTE_UNUSED, rtx func_desc ATTRIBUTE_UNUSED,
		    rtx tlsarg ATTRIBUTE_UNUSED, rtx cookie ATTRIBUTE_UNUSED)
{
#if TARGET_MACHO
  rs6000_call_darwin_1 (value, func_desc, tlsarg, cookie, false);
#else
  gcc_unreachable();
#endif
}


void
rs6000_sibcall_darwin (rtx value ATTRIBUTE_UNUSED, rtx func_desc ATTRIBUTE_UNUSED,
		       rtx tlsarg ATTRIBUTE_UNUSED, rtx cookie ATTRIBUTE_UNUSED)
{
#if TARGET_MACHO
  rs6000_call_darwin_1 (value, func_desc, tlsarg, cookie, true);
#else
  gcc_unreachable();
#endif
}

/* Return whether we should generate PC-relative code for FNDECL.  */
bool
rs6000_fndecl_pcrel_p (const_tree fndecl)
{
  if (DEFAULT_ABI != ABI_ELFv2)
    return false;

  struct cl_target_option *opts = target_opts_for_fn (fndecl);

  return ((opts->x_rs6000_isa_flags & OPTION_MASK_PCREL) != 0
	  && TARGET_CMODEL == CMODEL_MEDIUM);
}

/* Return whether we should generate PC-relative code for *FN.  */
bool
rs6000_pcrel_p (struct function *fn)
{
  if (DEFAULT_ABI != ABI_ELFv2)
    return false;

  /* Optimize usual case.  */
  if (fn == cfun)
    return ((rs6000_isa_flags & OPTION_MASK_PCREL) != 0
	    && TARGET_CMODEL == CMODEL_MEDIUM);

  return rs6000_fndecl_pcrel_p (fn->decl);
}


/* Given an address (ADDR), a mode (MODE), and what the format of the
   non-prefixed address (NON_PREFIXED_FORMAT) is, return the instruction format
   for the address.  */

enum insn_form
address_to_insn_form (rtx addr,
		      machine_mode mode,
		      enum non_prefixed_form non_prefixed_format)
{
  /* Single register is easy.  */
  if (REG_P (addr) || SUBREG_P (addr))
    return INSN_FORM_BASE_REG;

  /* If the non prefixed instruction format doesn't support offset addressing,
     make sure only indexed addressing is allowed.

     We special case SDmode so that the register allocator does not try to move
     SDmode through GPR registers, but instead uses the 32-bit integer load and
     store instructions for the floating point registers.  */
  if (non_prefixed_format == NON_PREFIXED_X || (mode == SDmode && TARGET_DFP))
    {
      if (GET_CODE (addr) != PLUS)
	return INSN_FORM_BAD;

      rtx op0 = XEXP (addr, 0);
      rtx op1 = XEXP (addr, 1);
      if (!REG_P (op0) && !SUBREG_P (op0))
	return INSN_FORM_BAD;

      if (!REG_P (op1) && !SUBREG_P (op1))
	return INSN_FORM_BAD;

      return INSN_FORM_X;
    }

  /* Deal with update forms.  */
  if (GET_RTX_CLASS (GET_CODE (addr)) == RTX_AUTOINC)
    return INSN_FORM_UPDATE;

  /* Handle PC-relative symbols and labels.  Check for both local and
     external symbols.  Assume labels are always local.  TLS symbols
     are not PC-relative for rs6000.  */
  if (TARGET_PCREL)
    {
      if (LABEL_REF_P (addr))
	return INSN_FORM_PCREL_LOCAL;

      if (SYMBOL_REF_P (addr) && !SYMBOL_REF_TLS_MODEL (addr))
	{
	  if (!SYMBOL_REF_LOCAL_P (addr))
	    return INSN_FORM_PCREL_EXTERNAL;
	  else
	    return INSN_FORM_PCREL_LOCAL;
	}
    }

  if (GET_CODE (addr) == CONST)
    addr = XEXP (addr, 0);

  /* Recognize LO_SUM addresses used with TOC and 32-bit addressing.  */
  if (GET_CODE (addr) == LO_SUM)
    return INSN_FORM_LO_SUM;

  /* Everything below must be an offset address of some form.  */
  if (GET_CODE (addr) != PLUS)
    return INSN_FORM_BAD;

  rtx op0 = XEXP (addr, 0);
  rtx op1 = XEXP (addr, 1);

  /* Check for indexed addresses.  */
  if (REG_P (op1) || SUBREG_P (op1))
    {
      if (REG_P (op0) || SUBREG_P (op0))
	return INSN_FORM_X;

      return INSN_FORM_BAD;
    }

  if (!CONST_INT_P (op1))
    return INSN_FORM_BAD;

  HOST_WIDE_INT offset = INTVAL (op1);
  if (!SIGNED_INTEGER_34BIT_P (offset))
    return INSN_FORM_BAD;

  /* Check for local and external PC-relative addresses.  Labels are always
     local.  TLS symbols are not PC-relative for rs6000.  */
  if (TARGET_PCREL)
    {
      if (LABEL_REF_P (op0))
	return INSN_FORM_PCREL_LOCAL;

      if (SYMBOL_REF_P (op0) && !SYMBOL_REF_TLS_MODEL (op0))
	{
	  if (!SYMBOL_REF_LOCAL_P (op0))
	    return INSN_FORM_PCREL_EXTERNAL;
	  else
	    return INSN_FORM_PCREL_LOCAL;
	}
    }

  /* If it isn't PC-relative, the address must use a base register.  */
  if (!REG_P (op0) && !SUBREG_P (op0))
    return INSN_FORM_BAD;

  /* Large offsets must be prefixed.  */
  if (!SIGNED_INTEGER_16BIT_P (offset))
    {
      if (TARGET_PREFIXED)
	return INSN_FORM_PREFIXED_NUMERIC;

      return INSN_FORM_BAD;
    }

  /* We have a 16-bit offset, see what default instruction format to use.  */
  if (non_prefixed_format == NON_PREFIXED_DEFAULT)
    {
      unsigned size = GET_MODE_SIZE (mode);

      /* On 64-bit systems, assume 64-bit integers need to use DS form
	 addresses (for LD/STD).  VSX vectors need to use DQ form addresses
	 (for LXV and STXV).  TImode is problematical in that its normal usage
	 is expected to be GPRs where it wants a DS instruction format, but if
	 it goes into the vector registers, it wants a DQ instruction
	 format.  */
      if (TARGET_POWERPC64 && size >= 8 && GET_MODE_CLASS (mode) == MODE_INT)
	non_prefixed_format = NON_PREFIXED_DS;

      else if (TARGET_VSX && size >= 16
	       && (VECTOR_MODE_P (mode) || VECTOR_ALIGNMENT_P (mode)))
	non_prefixed_format = NON_PREFIXED_DQ;

      else
	non_prefixed_format = NON_PREFIXED_D;
    }

  /* Classify the D/DS/DQ-form addresses.  */
  switch (non_prefixed_format)
    {
      /* Instruction format D, all 16 bits are valid.  */
    case NON_PREFIXED_D:
      return INSN_FORM_D;

      /* Instruction format DS, bottom 2 bits must be 0.  */
    case NON_PREFIXED_DS:
      if ((offset & 3) == 0)
	return INSN_FORM_DS;

      else if (TARGET_PREFIXED)
	return INSN_FORM_PREFIXED_NUMERIC;

      else
	return INSN_FORM_BAD;

      /* Instruction format DQ, bottom 4 bits must be 0.  */
    case NON_PREFIXED_DQ:
      if ((offset & 15) == 0)
	return INSN_FORM_DQ;

      else if (TARGET_PREFIXED)
	return INSN_FORM_PREFIXED_NUMERIC;

      else
	return INSN_FORM_BAD;

    default:
      break;
    }

  return INSN_FORM_BAD;
}

/* Helper function to see if we're potentially looking at lfs/stfs.
   - PARALLEL containing a SET and a CLOBBER
   - stfs:
    - SET is from UNSPEC_SI_FROM_SF to MEM:SI
    - CLOBBER is a V4SF
   - lfs:
    - SET is from UNSPEC_SF_FROM_SI to REG:SF
    - CLOBBER is a DI
 */

static bool
is_lfs_stfs_insn (rtx_insn *insn)
{
  rtx pattern = PATTERN (insn);
  if (GET_CODE (pattern) != PARALLEL)
    return false;

  /* This should be a parallel with exactly one set and one clobber.  */
  if (XVECLEN (pattern, 0) != 2)
    return false;

  rtx set = XVECEXP (pattern, 0, 0);
  if (GET_CODE (set) != SET)
    return false;

  rtx clobber = XVECEXP (pattern, 0, 1);
  if (GET_CODE (clobber) != CLOBBER)
    return false;

  /* All we care is that the destination of the SET is a mem:SI,
     the source should be an UNSPEC_SI_FROM_SF, and the clobber
     should be a scratch:V4SF.  */

  rtx dest = SET_DEST (set);
  rtx src = SET_SRC (set);
  rtx scratch = SET_DEST (clobber);

  if (GET_CODE (src) != UNSPEC)
    return false;

  /* stfs case.  */
  if (XINT (src, 1) == UNSPEC_SI_FROM_SF
      && GET_CODE (dest) == MEM && GET_MODE (dest) == SImode
      && GET_CODE (scratch) == SCRATCH && GET_MODE (scratch) == V4SFmode)
    return true;

  /* lfs case.  */
  if (XINT (src, 1) == UNSPEC_SF_FROM_SI
      && GET_CODE (dest) == REG && GET_MODE (dest) == SFmode
      && GET_CODE (scratch) == SCRATCH && GET_MODE (scratch) == DImode)
    return true;

  return false;
}

/* Helper function to take a REG and a MODE and turn it into the non-prefixed
   instruction format (D/DS/DQ) used for offset memory.  */

static enum non_prefixed_form
reg_to_non_prefixed (rtx reg, machine_mode mode)
{
  /* If it isn't a register, use the defaults.  */
  if (!REG_P (reg) && !SUBREG_P (reg))
    return NON_PREFIXED_DEFAULT;

  unsigned int r = reg_or_subregno (reg);

  /* If we have a pseudo, use the default instruction format.  */
  if (!HARD_REGISTER_NUM_P (r))
    return NON_PREFIXED_DEFAULT;

  unsigned size = GET_MODE_SIZE (mode);

  /* FPR registers use D-mode for scalars, and DQ-mode for vectors, IEEE
     128-bit floating point, and 128-bit integers.  Before power9, only indexed
     addressing was available for vectors.  */
  if (FP_REGNO_P (r))
    {
      if (mode == SFmode || size == 8 || FLOAT128_2REG_P (mode))
	return NON_PREFIXED_D;

      else if (size < 8)
	return NON_PREFIXED_X;

      else if (TARGET_VSX && size >= 16
	       && (VECTOR_MODE_P (mode)
		   || VECTOR_ALIGNMENT_P (mode)
		   || mode == TImode || mode == CTImode))
	return (TARGET_P9_VECTOR) ? NON_PREFIXED_DQ : NON_PREFIXED_X;

      else
	return NON_PREFIXED_DEFAULT;
    }

  /* Altivec registers use DS-mode for scalars, and DQ-mode for vectors, IEEE
     128-bit floating point, and 128-bit integers.  Before power9, only indexed
     addressing was available.  */
  else if (ALTIVEC_REGNO_P (r))
    {
      if (!TARGET_P9_VECTOR)
	return NON_PREFIXED_X;

      if (mode == SFmode || size == 8 || FLOAT128_2REG_P (mode))
	return NON_PREFIXED_DS;

      else if (size < 8)
	return NON_PREFIXED_X;

      else if (TARGET_VSX && size >= 16
	       && (VECTOR_MODE_P (mode)
		   || VECTOR_ALIGNMENT_P (mode)
		   || mode == TImode || mode == CTImode))
	return NON_PREFIXED_DQ;

      else
	return NON_PREFIXED_DEFAULT;
    }

  /* GPR registers use DS-mode for 64-bit items on 64-bit systems, and D-mode
     otherwise.  Assume that any other register, such as LR, CRs, etc. will go
     through the GPR registers for memory operations.  */
  else if (TARGET_POWERPC64 && size >= 8)
    return NON_PREFIXED_DS;

  return NON_PREFIXED_D;
}


/* Whether a load instruction is a prefixed instruction.  This is called from
   the prefixed attribute processing.  */

bool
prefixed_load_p (rtx_insn *insn)
{
  /* Validate the insn to make sure it is a normal load insn.  */
  extract_insn_cached (insn);
  if (recog_data.n_operands < 2)
    return false;

  rtx reg = recog_data.operand[0];
  rtx mem = recog_data.operand[1];

  if (!REG_P (reg) && !SUBREG_P (reg))
    return false;

  if (!MEM_P (mem))
    return false;

  /* Prefixed load instructions do not support update or indexed forms.  */
  if (get_attr_indexed (insn) == INDEXED_YES
      || get_attr_update (insn) == UPDATE_YES)
    return false;

  /* LWA uses the DS format instead of the D format that LWZ uses.  */
  enum non_prefixed_form non_prefixed;
  machine_mode reg_mode = GET_MODE (reg);
  machine_mode mem_mode = GET_MODE (mem);

  if (mem_mode == SImode && reg_mode == DImode
      && get_attr_sign_extend (insn) == SIGN_EXTEND_YES)
    non_prefixed = NON_PREFIXED_DS;

  else
    non_prefixed = reg_to_non_prefixed (reg, mem_mode);

  if (non_prefixed == NON_PREFIXED_X && is_lfs_stfs_insn (insn))
    return address_is_prefixed (XEXP (mem, 0), mem_mode, NON_PREFIXED_DEFAULT);
  else
    return address_is_prefixed (XEXP (mem, 0), mem_mode, non_prefixed);
}

/* Whether a store instruction is a prefixed instruction.  This is called from
   the prefixed attribute processing.  */

bool
prefixed_store_p (rtx_insn *insn)
{
  /* Validate the insn to make sure it is a normal store insn.  */
  extract_insn_cached (insn);
  if (recog_data.n_operands < 2)
    return false;

  rtx mem = recog_data.operand[0];
  rtx reg = recog_data.operand[1];

  if (!REG_P (reg) && !SUBREG_P (reg))
    return false;

  if (!MEM_P (mem))
    return false;

  /* Prefixed store instructions do not support update or indexed forms.  */
  if (get_attr_indexed (insn) == INDEXED_YES
      || get_attr_update (insn) == UPDATE_YES)
    return false;

  machine_mode mem_mode = GET_MODE (mem);
  rtx addr = XEXP (mem, 0);
  enum non_prefixed_form non_prefixed = reg_to_non_prefixed (reg, mem_mode);

  /* Need to make sure we aren't looking at a stfs which doesn't look
     like the other things reg_to_non_prefixed/address_is_prefixed
     looks for.  */
  if (non_prefixed == NON_PREFIXED_X && is_lfs_stfs_insn (insn))
    return address_is_prefixed (addr, mem_mode, NON_PREFIXED_DEFAULT);
  else
    return address_is_prefixed (addr, mem_mode, non_prefixed);
}

/* Whether a load immediate or add instruction is a prefixed instruction.  This
   is called from the prefixed attribute processing.  */

bool
prefixed_paddi_p (rtx_insn *insn)
{
  rtx set = single_set (insn);
  if (!set)
    return false;

  rtx dest = SET_DEST (set);
  rtx src = SET_SRC (set);

  if (!REG_P (dest) && !SUBREG_P (dest))
    return false;

  /* Is this a load immediate that can't be done with a simple ADDI or
     ADDIS?  */
  if (CONST_INT_P (src))
    return (satisfies_constraint_eI (src)
	    && !satisfies_constraint_I (src)
	    && !satisfies_constraint_L (src));

  /* Is this a PADDI instruction that can't be done with a simple ADDI or
     ADDIS?  */
  if (GET_CODE (src) == PLUS)
    {
      rtx op1 = XEXP (src, 1);

      return (CONST_INT_P (op1)
	      && satisfies_constraint_eI (op1)
	      && !satisfies_constraint_I (op1)
	      && !satisfies_constraint_L (op1));
    }

  /* If not, is it a load of a PC-relative address?  */
  if (!TARGET_PCREL || GET_MODE (dest) != Pmode)
    return false;

  if (!SYMBOL_REF_P (src) && !LABEL_REF_P (src) && GET_CODE (src) != CONST)
    return false;

  enum insn_form iform = address_to_insn_form (src, Pmode,
					       NON_PREFIXED_DEFAULT);

  return (iform == INSN_FORM_PCREL_EXTERNAL || iform == INSN_FORM_PCREL_LOCAL);
}

/* Whether the next instruction needs a 'p' prefix issued before the
   instruction is printed out.  */
static bool next_insn_prefixed_p;

/* Define FINAL_PRESCAN_INSN if some processing needs to be done before
   outputting the assembler code.  On the PowerPC, we remember if the current
   insn is a prefixed insn where we need to emit a 'p' before the insn.

   In addition, if the insn is part of a PC-relative reference to an external
   label optimization, this is recorded also.  */
void
rs6000_final_prescan_insn (rtx_insn *insn, rtx [], int)
{
  next_insn_prefixed_p = (get_attr_prefixed (insn) != PREFIXED_NO);
  return;
}

/* Define ASM_OUTPUT_OPCODE to do anything special before emitting an opcode.
   We use it to emit a 'p' for prefixed insns that is set in
   FINAL_PRESCAN_INSN.  */
void
rs6000_asm_output_opcode (FILE *stream)
{
  if (next_insn_prefixed_p)
    fprintf (stream, "p");

  return;
}

/* Adjust the length of an INSN.  LENGTH is the currently-computed length and
   should be adjusted to reflect any required changes.  This macro is used when
   there is some systematic length adjustment required that would be difficult
   to express in the length attribute.

   In the PowerPC, we use this to adjust the length of an instruction if one or
   more prefixed instructions are generated, using the attribute
   num_prefixed_insns.  A prefixed instruction is 8 bytes instead of 4, but the
   hardware requires that a prefied instruciton does not cross a 64-byte
   boundary.  This means the compiler has to assume the length of the first
   prefixed instruction is 12 bytes instead of 8 bytes.  Since the length is
   already set for the non-prefixed instruction, we just need to udpate for the
   difference.  */

int
rs6000_adjust_insn_length (rtx_insn *insn, int length)
{
  if (TARGET_PREFIXED && NONJUMP_INSN_P (insn))
    {
      rtx pattern = PATTERN (insn);
      if (GET_CODE (pattern) != USE && GET_CODE (pattern) != CLOBBER
	  && get_attr_prefixed (insn) == PREFIXED_YES)
	{
	  int num_prefixed = get_attr_max_prefixed_insns (insn);
	  length += 4 * (num_prefixed + 1);
	}
    }

  return length;
}


#ifdef HAVE_GAS_HIDDEN
# define USE_HIDDEN_LINKONCE 1
#else
# define USE_HIDDEN_LINKONCE 0
#endif

/* Fills in the label name that should be used for a 476 link stack thunk.  */

void
get_ppc476_thunk_name (char name[32])
{
  gcc_assert (TARGET_LINK_STACK);

  if (USE_HIDDEN_LINKONCE)
    sprintf (name, "__ppc476.get_thunk");
  else
    ASM_GENERATE_INTERNAL_LABEL (name, "LPPC476_", 0);
}

/* This function emits the simple thunk routine that is used to preserve
   the link stack on the 476 cpu.  */

static void rs6000_code_end (void) ATTRIBUTE_UNUSED;
static void
rs6000_code_end (void)
{
  char name[32];
  tree decl;

  if (!TARGET_LINK_STACK)
    return;

  get_ppc476_thunk_name (name);

  decl = build_decl (BUILTINS_LOCATION, FUNCTION_DECL, get_identifier (name),
		     build_function_type_list (void_type_node, NULL_TREE));
  DECL_RESULT (decl) = build_decl (BUILTINS_LOCATION, RESULT_DECL,
				   NULL_TREE, void_type_node);
  TREE_PUBLIC (decl) = 1;
  TREE_STATIC (decl) = 1;

#if RS6000_WEAK
  if (USE_HIDDEN_LINKONCE && !TARGET_XCOFF)
    {
      cgraph_node::create (decl)->set_comdat_group (DECL_ASSEMBLER_NAME (decl));
      targetm.asm_out.unique_section (decl, 0);
      switch_to_section (get_named_section (decl, NULL, 0));
      DECL_WEAK (decl) = 1;
      ASM_WEAKEN_DECL (asm_out_file, decl, name, 0);
      targetm.asm_out.globalize_label (asm_out_file, name);
      targetm.asm_out.assemble_visibility (decl, VISIBILITY_HIDDEN);
      ASM_DECLARE_FUNCTION_NAME (asm_out_file, name, decl);
    }
  else
#endif
    {
      switch_to_section (text_section);
      ASM_OUTPUT_LABEL (asm_out_file, name);
    }

  DECL_INITIAL (decl) = make_node (BLOCK);
  current_function_decl = decl;
  allocate_struct_function (decl, false);
  init_function_start (decl);
  first_function_block_is_cold = false;
  /* Make sure unwind info is emitted for the thunk if needed.  */
  final_start_function (emit_barrier (), asm_out_file, 1);

  fputs ("\tblr\n", asm_out_file);

  final_end_function ();
  init_insn_lengths ();
  free_after_compilation (cfun);
  set_cfun (NULL);
  current_function_decl = NULL;
}

/* Add r30 to hard reg set if the prologue sets it up and it is not
   pic_offset_table_rtx.  */

static void
rs6000_set_up_by_prologue (struct hard_reg_set_container *set)
{
  if (!TARGET_SINGLE_PIC_BASE
      && TARGET_TOC
      && TARGET_MINIMAL_TOC
      && !constant_pool_empty_p ())
    add_to_hard_reg_set (&set->set, Pmode, RS6000_PIC_OFFSET_TABLE_REGNUM);
  if (cfun->machine->split_stack_argp_used)
    add_to_hard_reg_set (&set->set, Pmode, 12);

  /* Make sure the hard reg set doesn't include r2, which was possibly added
     via PIC_OFFSET_TABLE_REGNUM.  */
  if (TARGET_TOC)
    remove_from_hard_reg_set (&set->set, Pmode, TOC_REGNUM);
}


/* Helper function for rs6000_split_logical to emit a logical instruction after
   spliting the operation to single GPR registers.

   DEST is the destination register.
   OP1 and OP2 are the input source registers.
   CODE is the base operation (AND, IOR, XOR, NOT).
   MODE is the machine mode.
   If COMPLEMENT_FINAL_P is true, wrap the whole operation with NOT.
   If COMPLEMENT_OP1_P is true, wrap operand1 with NOT.
   If COMPLEMENT_OP2_P is true, wrap operand2 with NOT.  */

static void
rs6000_split_logical_inner (rtx dest,
			    rtx op1,
			    rtx op2,
			    enum rtx_code code,
			    machine_mode mode,
			    bool complement_final_p,
			    bool complement_op1_p,
			    bool complement_op2_p)
{
  rtx bool_rtx;

  /* Optimize AND of 0/0xffffffff and IOR/XOR of 0.  */
  if (op2 && CONST_INT_P (op2)
      && (mode == SImode || (mode == DImode && TARGET_POWERPC64))
      && !complement_final_p && !complement_op1_p && !complement_op2_p)
    {
      HOST_WIDE_INT mask = GET_MODE_MASK (mode);
      HOST_WIDE_INT value = INTVAL (op2) & mask;

      /* Optimize AND of 0 to just set 0.  Optimize AND of -1 to be a move.  */
      if (code == AND)
	{
	  if (value == 0)
	    {
	      emit_insn (gen_rtx_SET (dest, const0_rtx));
	      return;
	    }

	  else if (value == mask)
	    {
	      if (!rtx_equal_p (dest, op1))
		emit_insn (gen_rtx_SET (dest, op1));
	      return;
	    }
	}

      /* Optimize IOR/XOR of 0 to be a simple move.  Split large operations
	 into separate ORI/ORIS or XORI/XORIS instrucitons.  */
      else if (code == IOR || code == XOR)
	{
	  if (value == 0)
	    {
	      if (!rtx_equal_p (dest, op1))
		emit_insn (gen_rtx_SET (dest, op1));
	      return;
	    }
	}
    }

  if (code == AND && mode == SImode
      && !complement_final_p && !complement_op1_p && !complement_op2_p)
    {
      emit_insn (gen_andsi3 (dest, op1, op2));
      return;
    }

  if (complement_op1_p)
    op1 = gen_rtx_NOT (mode, op1);

  if (complement_op2_p)
    op2 = gen_rtx_NOT (mode, op2);

  /* For canonical RTL, if only one arm is inverted it is the first.  */
  if (!complement_op1_p && complement_op2_p)
    std::swap (op1, op2);

  bool_rtx = ((code == NOT)
	      ? gen_rtx_NOT (mode, op1)
	      : gen_rtx_fmt_ee (code, mode, op1, op2));

  if (complement_final_p)
    bool_rtx = gen_rtx_NOT (mode, bool_rtx);

  emit_insn (gen_rtx_SET (dest, bool_rtx));
}

/* Split a DImode AND/IOR/XOR with a constant on a 32-bit system.  These
   operations are split immediately during RTL generation to allow for more
   optimizations of the AND/IOR/XOR.

   OPERANDS is an array containing the destination and two input operands.
   CODE is the base operation (AND, IOR, XOR, NOT).
   MODE is the machine mode.
   If COMPLEMENT_FINAL_P is true, wrap the whole operation with NOT.
   If COMPLEMENT_OP1_P is true, wrap operand1 with NOT.
   If COMPLEMENT_OP2_P is true, wrap operand2 with NOT.
   CLOBBER_REG is either NULL or a scratch register of type CC to allow
   formation of the AND instructions.  */

static void
rs6000_split_logical_di (rtx operands[3],
			 enum rtx_code code,
			 bool complement_final_p,
			 bool complement_op1_p,
			 bool complement_op2_p)
{
  const HOST_WIDE_INT lower_32bits = HOST_WIDE_INT_C(0xffffffff);
  const HOST_WIDE_INT upper_32bits = ~ lower_32bits;
  const HOST_WIDE_INT sign_bit = HOST_WIDE_INT_C(0x80000000);
  enum hi_lo { hi = 0, lo = 1 };
  rtx op0_hi_lo[2], op1_hi_lo[2], op2_hi_lo[2];
  size_t i;

  op0_hi_lo[hi] = gen_highpart (SImode, operands[0]);
  op1_hi_lo[hi] = gen_highpart (SImode, operands[1]);
  op0_hi_lo[lo] = gen_lowpart (SImode, operands[0]);
  op1_hi_lo[lo] = gen_lowpart (SImode, operands[1]);

  if (code == NOT)
    op2_hi_lo[hi] = op2_hi_lo[lo] = NULL_RTX;
  else
    {
      if (!CONST_INT_P (operands[2]))
	{
	  op2_hi_lo[hi] = gen_highpart_mode (SImode, DImode, operands[2]);
	  op2_hi_lo[lo] = gen_lowpart (SImode, operands[2]);
	}
      else
	{
	  HOST_WIDE_INT value = INTVAL (operands[2]);
	  HOST_WIDE_INT value_hi_lo[2];

	  gcc_assert (!complement_final_p);
	  gcc_assert (!complement_op1_p);
	  gcc_assert (!complement_op2_p);

	  value_hi_lo[hi] = value >> 32;
	  value_hi_lo[lo] = value & lower_32bits;

	  for (i = 0; i < 2; i++)
	    {
	      HOST_WIDE_INT sub_value = value_hi_lo[i];

	      if (sub_value & sign_bit)
		sub_value |= upper_32bits;

	      op2_hi_lo[i] = GEN_INT (sub_value);

	      /* If this is an AND instruction, check to see if we need to load
		 the value in a register.  */
	      if (code == AND && sub_value != -1 && sub_value != 0
		  && !and_operand (op2_hi_lo[i], SImode))
		op2_hi_lo[i] = force_reg (SImode, op2_hi_lo[i]);
	    }
	}
    }

  for (i = 0; i < 2; i++)
    {
      /* Split large IOR/XOR operations.  */
      if ((code == IOR || code == XOR)
	  && CONST_INT_P (op2_hi_lo[i])
	  && !complement_final_p
	  && !complement_op1_p
	  && !complement_op2_p
	  && !logical_const_operand (op2_hi_lo[i], SImode))
	{
	  HOST_WIDE_INT value = INTVAL (op2_hi_lo[i]);
	  HOST_WIDE_INT hi_16bits = value & HOST_WIDE_INT_C(0xffff0000);
	  HOST_WIDE_INT lo_16bits = value & HOST_WIDE_INT_C(0x0000ffff);
	  rtx tmp = gen_reg_rtx (SImode);

	  /* Make sure the constant is sign extended.  */
	  if ((hi_16bits & sign_bit) != 0)
	    hi_16bits |= upper_32bits;

	  rs6000_split_logical_inner (tmp, op1_hi_lo[i], GEN_INT (hi_16bits),
				      code, SImode, false, false, false);

	  rs6000_split_logical_inner (op0_hi_lo[i], tmp, GEN_INT (lo_16bits),
				      code, SImode, false, false, false);
	}
      else
	rs6000_split_logical_inner (op0_hi_lo[i], op1_hi_lo[i], op2_hi_lo[i],
				    code, SImode, complement_final_p,
				    complement_op1_p, complement_op2_p);
    }

  return;
}

/* Split the insns that make up boolean operations operating on multiple GPR
   registers.  The boolean MD patterns ensure that the inputs either are
   exactly the same as the output registers, or there is no overlap.

   OPERANDS is an array containing the destination and two input operands.
   CODE is the base operation (AND, IOR, XOR, NOT).
   If COMPLEMENT_FINAL_P is true, wrap the whole operation with NOT.
   If COMPLEMENT_OP1_P is true, wrap operand1 with NOT.
   If COMPLEMENT_OP2_P is true, wrap operand2 with NOT.  */

void
rs6000_split_logical (rtx operands[3],
		      enum rtx_code code,
		      bool complement_final_p,
		      bool complement_op1_p,
		      bool complement_op2_p)
{
  machine_mode mode = GET_MODE (operands[0]);
  machine_mode sub_mode;
  rtx op0, op1, op2;
  int sub_size, regno0, regno1, nregs, i;

  /* If this is DImode, use the specialized version that can run before
     register allocation.  */
  if (mode == DImode && !TARGET_POWERPC64)
    {
      rs6000_split_logical_di (operands, code, complement_final_p,
			       complement_op1_p, complement_op2_p);
      return;
    }

  op0 = operands[0];
  op1 = operands[1];
  op2 = (code == NOT) ? NULL_RTX : operands[2];
  sub_mode = (TARGET_POWERPC64) ? DImode : SImode;
  sub_size = GET_MODE_SIZE (sub_mode);
  regno0 = REGNO (op0);
  regno1 = REGNO (op1);

  gcc_assert (reload_completed);
  gcc_assert (IN_RANGE (regno0, FIRST_GPR_REGNO, LAST_GPR_REGNO));
  gcc_assert (IN_RANGE (regno1, FIRST_GPR_REGNO, LAST_GPR_REGNO));

  nregs = rs6000_hard_regno_nregs[(int)mode][regno0];
  gcc_assert (nregs > 1);

  if (op2 && REG_P (op2))
    gcc_assert (IN_RANGE (REGNO (op2), FIRST_GPR_REGNO, LAST_GPR_REGNO));

  for (i = 0; i < nregs; i++)
    {
      int offset = i * sub_size;
      rtx sub_op0 = simplify_subreg (sub_mode, op0, mode, offset);
      rtx sub_op1 = simplify_subreg (sub_mode, op1, mode, offset);
      rtx sub_op2 = ((code == NOT)
		     ? NULL_RTX
		     : simplify_subreg (sub_mode, op2, mode, offset));

      rs6000_split_logical_inner (sub_op0, sub_op1, sub_op2, code, sub_mode,
				  complement_final_p, complement_op1_p,
				  complement_op2_p);
    }

  return;
}


/* Return true if the peephole2 can combine a load involving a combination of
   an addis instruction and a load with an offset that can be fused together on
   a power8.  */

bool
fusion_gpr_load_p (rtx addis_reg,	/* register set via addis.  */
		   rtx addis_value,	/* addis value.  */
		   rtx target,		/* target register that is loaded.  */
		   rtx mem)		/* bottom part of the memory addr.  */
{
  rtx addr;
  rtx base_reg;

  /* Validate arguments.  */
  if (!base_reg_operand (addis_reg, GET_MODE (addis_reg)))
    return false;

  if (!base_reg_operand (target, GET_MODE (target)))
    return false;

  if (!fusion_gpr_addis (addis_value, GET_MODE (addis_value)))
    return false;

  /* Allow sign/zero extension.  */
  if (GET_CODE (mem) == ZERO_EXTEND
      || (GET_CODE (mem) == SIGN_EXTEND && TARGET_P8_FUSION_SIGN))
    mem = XEXP (mem, 0);

  if (!MEM_P (mem))
    return false;

  if (!fusion_gpr_mem_load (mem, GET_MODE (mem)))
    return false;

  addr = XEXP (mem, 0);			/* either PLUS or LO_SUM.  */
  if (GET_CODE (addr) != PLUS && GET_CODE (addr) != LO_SUM)
    return false;

  /* Validate that the register used to load the high value is either the
     register being loaded, or we can safely replace its use.

     This function is only called from the peephole2 pass and we assume that
     there are 2 instructions in the peephole (addis and load), so we want to
     check if the target register was not used in the memory address and the
     register to hold the addis result is dead after the peephole.  */
  if (REGNO (addis_reg) != REGNO (target))
    {
      if (reg_mentioned_p (target, mem))
	return false;

      if (!peep2_reg_dead_p (2, addis_reg))
	return false;

      /* If the target register being loaded is the stack pointer, we must
         avoid loading any other value into it, even temporarily.  */
      if (REG_P (target) && REGNO (target) == STACK_POINTER_REGNUM)
	return false;
    }

  base_reg = XEXP (addr, 0);
  return REGNO (addis_reg) == REGNO (base_reg);
}

/* During the peephole2 pass, adjust and expand the insns for a load fusion
   sequence.  We adjust the addis register to use the target register.  If the
   load sign extends, we adjust the code to do the zero extending load, and an
   explicit sign extension later since the fusion only covers zero extending
   loads.

   The operands are:
	operands[0]	register set with addis (to be replaced with target)
	operands[1]	value set via addis
	operands[2]	target register being loaded
	operands[3]	D-form memory reference using operands[0].  */

void
expand_fusion_gpr_load (rtx *operands)
{
  rtx addis_value = operands[1];
  rtx target = operands[2];
  rtx orig_mem = operands[3];
  rtx  new_addr, new_mem, orig_addr, offset;
  enum rtx_code plus_or_lo_sum;
  machine_mode target_mode = GET_MODE (target);
  machine_mode extend_mode = target_mode;
  machine_mode ptr_mode = Pmode;
  enum rtx_code extend = UNKNOWN;

  if (GET_CODE (orig_mem) == ZERO_EXTEND
      || (TARGET_P8_FUSION_SIGN && GET_CODE (orig_mem) == SIGN_EXTEND))
    {
      extend = GET_CODE (orig_mem);
      orig_mem = XEXP (orig_mem, 0);
      target_mode = GET_MODE (orig_mem);
    }

  gcc_assert (MEM_P (orig_mem));

  orig_addr = XEXP (orig_mem, 0);
  plus_or_lo_sum = GET_CODE (orig_addr);
  gcc_assert (plus_or_lo_sum == PLUS || plus_or_lo_sum == LO_SUM);

  offset = XEXP (orig_addr, 1);
  new_addr = gen_rtx_fmt_ee (plus_or_lo_sum, ptr_mode, addis_value, offset);
  new_mem = replace_equiv_address_nv (orig_mem, new_addr, false);

  if (extend != UNKNOWN)
    new_mem = gen_rtx_fmt_e (ZERO_EXTEND, extend_mode, new_mem);

  new_mem = gen_rtx_UNSPEC (extend_mode, gen_rtvec (1, new_mem),
			    UNSPEC_FUSION_GPR);
  emit_insn (gen_rtx_SET (target, new_mem));

  if (extend == SIGN_EXTEND)
    {
      int sub_off = ((BYTES_BIG_ENDIAN)
		     ? GET_MODE_SIZE (extend_mode) - GET_MODE_SIZE (target_mode)
		     : 0);
      rtx sign_reg
	= simplify_subreg (target_mode, target, extend_mode, sub_off);

      emit_insn (gen_rtx_SET (target,
			      gen_rtx_SIGN_EXTEND (extend_mode, sign_reg)));
    }

  return;
}

/* Emit the addis instruction that will be part of a fused instruction
   sequence.  */

void
emit_fusion_addis (rtx target, rtx addis_value)
{
  rtx fuse_ops[10];
  const char *addis_str = NULL;

  /* Emit the addis instruction.  */
  fuse_ops[0] = target;
  if (satisfies_constraint_L (addis_value))
    {
      fuse_ops[1] = addis_value;
      addis_str = "lis %0,%v1";
    }

  else if (GET_CODE (addis_value) == PLUS)
    {
      rtx op0 = XEXP (addis_value, 0);
      rtx op1 = XEXP (addis_value, 1);

      if (REG_P (op0) && CONST_INT_P (op1)
	  && satisfies_constraint_L (op1))
	{
	  fuse_ops[1] = op0;
	  fuse_ops[2] = op1;
	  addis_str = "addis %0,%1,%v2";
	}
    }

  else if (GET_CODE (addis_value) == HIGH)
    {
      rtx value = XEXP (addis_value, 0);
      if (GET_CODE (value) == UNSPEC && XINT (value, 1) == UNSPEC_TOCREL)
	{
	  fuse_ops[1] = XVECEXP (value, 0, 0);		/* symbol ref.  */
	  fuse_ops[2] = XVECEXP (value, 0, 1);		/* TOC register.  */
	  if (TARGET_ELF)
	    addis_str = "addis %0,%2,%1@toc@ha";

	  else if (TARGET_XCOFF)
	    addis_str = "addis %0,%1@u(%2)";

	  else
	    gcc_unreachable ();
	}

      else if (GET_CODE (value) == PLUS)
	{
	  rtx op0 = XEXP (value, 0);
	  rtx op1 = XEXP (value, 1);

	  if (GET_CODE (op0) == UNSPEC
	      && XINT (op0, 1) == UNSPEC_TOCREL
	      && CONST_INT_P (op1))
	    {
	      fuse_ops[1] = XVECEXP (op0, 0, 0);	/* symbol ref.  */
	      fuse_ops[2] = XVECEXP (op0, 0, 1);	/* TOC register.  */
	      fuse_ops[3] = op1;
	      if (TARGET_ELF)
		addis_str = "addis %0,%2,%1+%3@toc@ha";

	      else if (TARGET_XCOFF)
		addis_str = "addis %0,%1+%3@u(%2)";

	      else
		gcc_unreachable ();
	    }
	}

      else if (satisfies_constraint_L (value))
	{
	  fuse_ops[1] = value;
	  addis_str = "lis %0,%v1";
	}

      else if (TARGET_ELF && !TARGET_POWERPC64 && CONSTANT_P (value))
	{
	  fuse_ops[1] = value;
	  addis_str = "lis %0,%1@ha";
	}
    }

  if (!addis_str)
    fatal_insn ("Could not generate addis value for fusion", addis_value);

  output_asm_insn (addis_str, fuse_ops);
}

/* Emit a D-form load or store instruction that is the second instruction
   of a fusion sequence.  */

static void
emit_fusion_load (rtx load_reg, rtx addis_reg, rtx offset, const char *insn_str)
{
  rtx fuse_ops[10];
  char insn_template[80];

  fuse_ops[0] = load_reg;
  fuse_ops[1] = addis_reg;

  if (CONST_INT_P (offset) && satisfies_constraint_I (offset))
    {
      sprintf (insn_template, "%s %%0,%%2(%%1)", insn_str);
      fuse_ops[2] = offset;
      output_asm_insn (insn_template, fuse_ops);
    }

  else if (GET_CODE (offset) == UNSPEC
	   && XINT (offset, 1) == UNSPEC_TOCREL)
    {
      if (TARGET_ELF)
	sprintf (insn_template, "%s %%0,%%2@toc@l(%%1)", insn_str);

      else if (TARGET_XCOFF)
	sprintf (insn_template, "%s %%0,%%2@l(%%1)", insn_str);

      else
	gcc_unreachable ();

      fuse_ops[2] = XVECEXP (offset, 0, 0);
      output_asm_insn (insn_template, fuse_ops);
    }

  else if (GET_CODE (offset) == PLUS
	   && GET_CODE (XEXP (offset, 0)) == UNSPEC
	   && XINT (XEXP (offset, 0), 1) == UNSPEC_TOCREL
	   && CONST_INT_P (XEXP (offset, 1)))
    {
      rtx tocrel_unspec = XEXP (offset, 0);
      if (TARGET_ELF)
	sprintf (insn_template, "%s %%0,%%2+%%3@toc@l(%%1)", insn_str);

      else if (TARGET_XCOFF)
	sprintf (insn_template, "%s %%0,%%2+%%3@l(%%1)", insn_str);

      else
	gcc_unreachable ();

      fuse_ops[2] = XVECEXP (tocrel_unspec, 0, 0);
      fuse_ops[3] = XEXP (offset, 1);
      output_asm_insn (insn_template, fuse_ops);
    }

  else if (TARGET_ELF && !TARGET_POWERPC64 && CONSTANT_P (offset))
    {
      sprintf (insn_template, "%s %%0,%%2@l(%%1)", insn_str);

      fuse_ops[2] = offset;
      output_asm_insn (insn_template, fuse_ops);
    }

  else
    fatal_insn ("Unable to generate load/store offset for fusion", offset);

  return;
}

/* Given an address, convert it into the addis and load offset parts.  Addresses
   created during the peephole2 process look like:
	(lo_sum (high (unspec [(sym)] UNSPEC_TOCREL))
		(unspec [(...)] UNSPEC_TOCREL))  */

static void
fusion_split_address (rtx addr, rtx *p_hi, rtx *p_lo)
{
  rtx hi, lo;

  if (GET_CODE (addr) == PLUS || GET_CODE (addr) == LO_SUM)
    {
      hi = XEXP (addr, 0);
      lo = XEXP (addr, 1);
    }
  else
    gcc_unreachable ();

  *p_hi = hi;
  *p_lo = lo;
}

/* Return a string to fuse an addis instruction with a gpr load to the same
   register that we loaded up the addis instruction.  The address that is used
   is the logical address that was formed during peephole2:
	(lo_sum (high) (low-part))

   The code is complicated, so we call output_asm_insn directly, and just
   return "".  */

const char *
emit_fusion_gpr_load (rtx target, rtx mem)
{
  rtx addis_value;
  rtx addr;
  rtx load_offset;
  const char *load_str = NULL;
  machine_mode mode;

  if (GET_CODE (mem) == ZERO_EXTEND)
    mem = XEXP (mem, 0);

  gcc_assert (REG_P (target) && MEM_P (mem));

  addr = XEXP (mem, 0);
  fusion_split_address (addr, &addis_value, &load_offset);

  /* Now emit the load instruction to the same register.  */
  mode = GET_MODE (mem);
  switch (mode)
    {
    case E_QImode:
      load_str = "lbz";
      break;

    case E_HImode:
      load_str = "lhz";
      break;

    case E_SImode:
    case E_SFmode:
      load_str = "lwz";
      break;

    case E_DImode:
    case E_DFmode:
      gcc_assert (TARGET_POWERPC64);
      load_str = "ld";
      break;

    default:
      fatal_insn ("Bad GPR fusion", gen_rtx_SET (target, mem));
    }

  /* Emit the addis instruction.  */
  emit_fusion_addis (target, addis_value);

  /* Emit the D-form load instruction.  */
  emit_fusion_load (target, target, load_offset, load_str);

  return "";
}


#ifdef RS6000_GLIBC_ATOMIC_FENV
/* Function declarations for rs6000_atomic_assign_expand_fenv.  */
static tree atomic_hold_decl, atomic_clear_decl, atomic_update_decl;
#endif

/* Implement TARGET_ATOMIC_ASSIGN_EXPAND_FENV hook.  */

static void
rs6000_atomic_assign_expand_fenv (tree *hold, tree *clear, tree *update)
{
  if (!TARGET_HARD_FLOAT)
    {
#ifdef RS6000_GLIBC_ATOMIC_FENV
      if (atomic_hold_decl == NULL_TREE)
	{
	  atomic_hold_decl
	    = build_decl (BUILTINS_LOCATION, FUNCTION_DECL,
			  get_identifier ("__atomic_feholdexcept"),
			  build_function_type_list (void_type_node,
						    double_ptr_type_node,
						    NULL_TREE));
	  TREE_PUBLIC (atomic_hold_decl) = 1;
	  DECL_EXTERNAL (atomic_hold_decl) = 1;
	}

      if (atomic_clear_decl == NULL_TREE)
	{
	  atomic_clear_decl
	    = build_decl (BUILTINS_LOCATION, FUNCTION_DECL,
			  get_identifier ("__atomic_feclearexcept"),
			  build_function_type_list (void_type_node,
						    NULL_TREE));
	  TREE_PUBLIC (atomic_clear_decl) = 1;
	  DECL_EXTERNAL (atomic_clear_decl) = 1;
	}

      tree const_double = build_qualified_type (double_type_node,
						TYPE_QUAL_CONST);
      tree const_double_ptr = build_pointer_type (const_double);
      if (atomic_update_decl == NULL_TREE)
	{
	  atomic_update_decl
	    = build_decl (BUILTINS_LOCATION, FUNCTION_DECL,
			  get_identifier ("__atomic_feupdateenv"),
			  build_function_type_list (void_type_node,
						    const_double_ptr,
						    NULL_TREE));
	  TREE_PUBLIC (atomic_update_decl) = 1;
	  DECL_EXTERNAL (atomic_update_decl) = 1;
	}

      tree fenv_var = create_tmp_var_raw (double_type_node);
      TREE_ADDRESSABLE (fenv_var) = 1;
      tree fenv_addr = build1 (ADDR_EXPR, double_ptr_type_node,
			       build4 (TARGET_EXPR, double_type_node, fenv_var,
				       void_node, NULL_TREE, NULL_TREE));

      *hold = build_call_expr (atomic_hold_decl, 1, fenv_addr);
      *clear = build_call_expr (atomic_clear_decl, 0);
      *update = build_call_expr (atomic_update_decl, 1,
				 fold_convert (const_double_ptr, fenv_addr));
#endif
      return;
    }

  tree mffs = rs6000_builtin_decls[RS6000_BUILTIN_MFFS];
  tree mtfsf = rs6000_builtin_decls[RS6000_BUILTIN_MTFSF];
  tree call_mffs = build_call_expr (mffs, 0);

  /* Generates the equivalent of feholdexcept (&fenv_var)

     *fenv_var = __builtin_mffs ();
     double fenv_hold;
     *(uint64_t*)&fenv_hold = *(uint64_t*)fenv_var & 0xffffffff00000007LL;
     __builtin_mtfsf (0xff, fenv_hold);  */

  /* Mask to clear everything except for the rounding modes and non-IEEE
     arithmetic flag.  */
  const unsigned HOST_WIDE_INT hold_exception_mask
    = HOST_WIDE_INT_C (0xffffffff00000007);

  tree fenv_var = create_tmp_var_raw (double_type_node);

  tree hold_mffs = build4 (TARGET_EXPR, double_type_node, fenv_var, call_mffs,
			   NULL_TREE, NULL_TREE);

  tree fenv_llu = build1 (VIEW_CONVERT_EXPR, uint64_type_node, fenv_var);
  tree fenv_llu_and = build2 (BIT_AND_EXPR, uint64_type_node, fenv_llu,
			      build_int_cst (uint64_type_node,
					     hold_exception_mask));

  tree fenv_hold_mtfsf = build1 (VIEW_CONVERT_EXPR, double_type_node,
				 fenv_llu_and);

  tree hold_mtfsf = build_call_expr (mtfsf, 2,
				     build_int_cst (unsigned_type_node, 0xff),
				     fenv_hold_mtfsf);

  *hold = build2 (COMPOUND_EXPR, void_type_node, hold_mffs, hold_mtfsf);

  /* Generates the equivalent of feclearexcept (FE_ALL_EXCEPT):

     double fenv_clear = __builtin_mffs ();
     *(uint64_t)&fenv_clear &= 0xffffffff00000000LL;
     __builtin_mtfsf (0xff, fenv_clear);  */

  /* Mask to clear everything except for the rounding modes and non-IEEE
     arithmetic flag.  */
  const unsigned HOST_WIDE_INT clear_exception_mask
    = HOST_WIDE_INT_C (0xffffffff00000000);

  tree fenv_clear = create_tmp_var_raw (double_type_node);

  tree clear_mffs = build4 (TARGET_EXPR, double_type_node, fenv_clear,
			    call_mffs, NULL_TREE, NULL_TREE);

  tree fenv_clean_llu = build1 (VIEW_CONVERT_EXPR, uint64_type_node, fenv_clear);
  tree fenv_clear_llu_and = build2 (BIT_AND_EXPR, uint64_type_node,
				    fenv_clean_llu,
				    build_int_cst (uint64_type_node,
						   clear_exception_mask));

  tree fenv_clear_mtfsf = build1 (VIEW_CONVERT_EXPR, double_type_node,
				  fenv_clear_llu_and);

  tree clear_mtfsf = build_call_expr (mtfsf, 2,
				      build_int_cst (unsigned_type_node, 0xff),
				      fenv_clear_mtfsf);

  *clear = build2 (COMPOUND_EXPR, void_type_node, clear_mffs, clear_mtfsf);

  /* Generates the equivalent of feupdateenv (&fenv_var)

     double old_fenv = __builtin_mffs ();
     double fenv_update;
     *(uint64_t*)&fenv_update = (*(uint64_t*)&old & 0xffffffff1fffff00LL) |
                                (*(uint64_t*)fenv_var 0x1ff80fff);
     __builtin_mtfsf (0xff, fenv_update);  */

  const unsigned HOST_WIDE_INT update_exception_mask
    = HOST_WIDE_INT_C (0xffffffff1fffff00);
  const unsigned HOST_WIDE_INT new_exception_mask
    = HOST_WIDE_INT_C (0x1ff80fff);

  tree old_fenv = create_tmp_var_raw (double_type_node);
  tree update_mffs = build4 (TARGET_EXPR, double_type_node, old_fenv,
			     call_mffs, NULL_TREE, NULL_TREE);

  tree old_llu = build1 (VIEW_CONVERT_EXPR, uint64_type_node, old_fenv);
  tree old_llu_and = build2 (BIT_AND_EXPR, uint64_type_node, old_llu,
			     build_int_cst (uint64_type_node,
					    update_exception_mask));

  tree new_llu_and = build2 (BIT_AND_EXPR, uint64_type_node, fenv_llu,
			     build_int_cst (uint64_type_node,
					    new_exception_mask));

  tree new_llu_mask = build2 (BIT_IOR_EXPR, uint64_type_node,
			      old_llu_and, new_llu_and);

  tree fenv_update_mtfsf = build1 (VIEW_CONVERT_EXPR, double_type_node,
				   new_llu_mask);

  tree update_mtfsf = build_call_expr (mtfsf, 2,
				       build_int_cst (unsigned_type_node, 0xff),
				       fenv_update_mtfsf);

  *update = build2 (COMPOUND_EXPR, void_type_node, update_mffs, update_mtfsf);
}

void
rs6000_generate_float2_double_code (rtx dst, rtx src1, rtx src2)
{
  rtx rtx_tmp0, rtx_tmp1, rtx_tmp2, rtx_tmp3;

  rtx_tmp0 = gen_reg_rtx (V2DFmode);
  rtx_tmp1 = gen_reg_rtx (V2DFmode);

  /* The destination of the vmrgew instruction layout is:
     rtx_tmp2[0] rtx_tmp3[0] rtx_tmp2[1] rtx_tmp3[0].
     Setup rtx_tmp0 and rtx_tmp1 to ensure the order of the elements after the
     vmrgew instruction will be correct.  */
  if (BYTES_BIG_ENDIAN)
    {
       emit_insn (gen_vsx_xxpermdi_v2df_be (rtx_tmp0, src1, src2,
					    GEN_INT (0)));
       emit_insn (gen_vsx_xxpermdi_v2df_be (rtx_tmp1, src1, src2,
					    GEN_INT (3)));
    }
  else
    {
       emit_insn (gen_vsx_xxpermdi_v2df (rtx_tmp0, src1, src2, GEN_INT (3)));
       emit_insn (gen_vsx_xxpermdi_v2df (rtx_tmp1, src1, src2, GEN_INT (0)));
    }

  rtx_tmp2 = gen_reg_rtx (V4SFmode);
  rtx_tmp3 = gen_reg_rtx (V4SFmode);

  emit_insn (gen_vsx_xvcvdpsp (rtx_tmp2, rtx_tmp0));
  emit_insn (gen_vsx_xvcvdpsp (rtx_tmp3, rtx_tmp1));

  if (BYTES_BIG_ENDIAN)
    emit_insn (gen_p8_vmrgew_v4sf (dst, rtx_tmp2, rtx_tmp3));
  else
    emit_insn (gen_p8_vmrgew_v4sf (dst, rtx_tmp3, rtx_tmp2));
}

void
rs6000_generate_float2_code (bool signed_convert, rtx dst, rtx src1, rtx src2)
{
  rtx rtx_tmp0, rtx_tmp1, rtx_tmp2, rtx_tmp3;

  rtx_tmp0 = gen_reg_rtx (V2DImode);
  rtx_tmp1 = gen_reg_rtx (V2DImode);

  /* The destination of the vmrgew instruction layout is:
     rtx_tmp2[0] rtx_tmp3[0] rtx_tmp2[1] rtx_tmp3[0].
     Setup rtx_tmp0 and rtx_tmp1 to ensure the order of the elements after the
     vmrgew instruction will be correct.  */
  if (BYTES_BIG_ENDIAN)
    {
      emit_insn (gen_vsx_xxpermdi_v2di_be (rtx_tmp0, src1, src2, GEN_INT (0)));
      emit_insn (gen_vsx_xxpermdi_v2di_be (rtx_tmp1, src1, src2, GEN_INT (3)));
    }
  else
    {
      emit_insn (gen_vsx_xxpermdi_v2di (rtx_tmp0, src1, src2, GEN_INT (3)));
      emit_insn (gen_vsx_xxpermdi_v2di (rtx_tmp1, src1, src2, GEN_INT (0)));
    }

  rtx_tmp2 = gen_reg_rtx (V4SFmode);
  rtx_tmp3 = gen_reg_rtx (V4SFmode);

  if (signed_convert)
    {
      emit_insn (gen_vsx_xvcvsxdsp (rtx_tmp2, rtx_tmp0));
      emit_insn (gen_vsx_xvcvsxdsp (rtx_tmp3, rtx_tmp1));
    }
  else
    {
       emit_insn (gen_vsx_xvcvuxdsp (rtx_tmp2, rtx_tmp0));
       emit_insn (gen_vsx_xvcvuxdsp (rtx_tmp3, rtx_tmp1));
    }

  if (BYTES_BIG_ENDIAN)
    emit_insn (gen_p8_vmrgew_v4sf (dst, rtx_tmp2, rtx_tmp3));
  else
    emit_insn (gen_p8_vmrgew_v4sf (dst, rtx_tmp3, rtx_tmp2));
}

void
rs6000_generate_vsigned2_code (bool signed_convert, rtx dst, rtx src1,
			       rtx src2)
{
  rtx rtx_tmp0, rtx_tmp1, rtx_tmp2, rtx_tmp3;

  rtx_tmp0 = gen_reg_rtx (V2DFmode);
  rtx_tmp1 = gen_reg_rtx (V2DFmode);

  emit_insn (gen_vsx_xxpermdi_v2df (rtx_tmp0, src1, src2, GEN_INT (0)));
  emit_insn (gen_vsx_xxpermdi_v2df (rtx_tmp1, src1, src2, GEN_INT (3)));

  rtx_tmp2 = gen_reg_rtx (V4SImode);
  rtx_tmp3 = gen_reg_rtx (V4SImode);

  if (signed_convert)
    {
      emit_insn (gen_vsx_xvcvdpsxws (rtx_tmp2, rtx_tmp0));
      emit_insn (gen_vsx_xvcvdpsxws (rtx_tmp3, rtx_tmp1));
    }
  else
    {
      emit_insn (gen_vsx_xvcvdpuxws (rtx_tmp2, rtx_tmp0));
      emit_insn (gen_vsx_xvcvdpuxws (rtx_tmp3, rtx_tmp1));
    }

  emit_insn (gen_p8_vmrgew_v4si (dst, rtx_tmp2, rtx_tmp3));
}

/* Implement the TARGET_OPTAB_SUPPORTED_P hook.  */

static bool
rs6000_optab_supported_p (int op, machine_mode mode1, machine_mode,
			  optimization_type opt_type)
{
  switch (op)
    {
    case rsqrt_optab:
      return (opt_type == OPTIMIZE_FOR_SPEED
	      && RS6000_RECIP_AUTO_RSQRTE_P (mode1));

    default:
      return true;
    }
}

/* Implement TARGET_CONSTANT_ALIGNMENT.  */

static HOST_WIDE_INT
rs6000_constant_alignment (const_tree exp, HOST_WIDE_INT align)
{
  if (TREE_CODE (exp) == STRING_CST
      && (STRICT_ALIGNMENT || !optimize_size))
    return MAX (align, BITS_PER_WORD);
  return align;
}

/* Implement TARGET_STARTING_FRAME_OFFSET.  */

static HOST_WIDE_INT
rs6000_starting_frame_offset (void)
{
  if (FRAME_GROWS_DOWNWARD)
    return 0;
  return RS6000_STARTING_FRAME_OFFSET;
}


/* Create an alias for a mangled name where we have changed the mangling (in
   GCC 8.1, we used U10__float128, and now we use u9__ieee128).  This is called
   via the target hook TARGET_ASM_GLOBALIZE_DECL_NAME.  */

#if TARGET_ELF && RS6000_WEAK
static void
rs6000_globalize_decl_name (FILE * stream, tree decl)
{
  const char *name = XSTR (XEXP (DECL_RTL (decl), 0), 0);

  targetm.asm_out.globalize_label (stream, name);

  if (rs6000_passes_ieee128 && name[0] == '_' && name[1] == 'Z')
    {
      tree save_asm_name = DECL_ASSEMBLER_NAME (decl);
      const char *old_name;

      ieee128_mangling_gcc_8_1 = true;
      lang_hooks.set_decl_assembler_name (decl);
      old_name = IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (decl));
      SET_DECL_ASSEMBLER_NAME (decl, save_asm_name);
      ieee128_mangling_gcc_8_1 = false;

      if (strcmp (name, old_name) != 0)
	{
	  fprintf (stream, "\t.weak %s\n", old_name);
	  fprintf (stream, "\t.set %s,%s\n", old_name, name);
	}
    }
}
#endif


/* On 64-bit Linux and Freebsd systems, possibly switch the long double library
   function names from <foo>l to <foo>f128 if the default long double type is
   IEEE 128-bit.  Typically, with the C and C++ languages, the standard math.h
   include file switches the names on systems that support long double as IEEE
   128-bit, but that doesn't work if the user uses __builtin_<foo>l directly.
   In the future, glibc will export names like __ieee128_sinf128 and we can
   switch to using those instead of using sinf128, which pollutes the user's
   namespace.

   This will switch the names for Fortran math functions as well (which doesn't
   use math.h).  However, Fortran needs other changes to the compiler and
   library before you can switch the real*16 type at compile time.

   We use the TARGET_MANGLE_DECL_ASSEMBLER_NAME hook to change this name.  We
   only do this if the default is that long double is IBM extended double, and
   the user asked for IEEE 128-bit.  */

static tree
rs6000_mangle_decl_assembler_name (tree decl, tree id)
{
  if (!TARGET_IEEEQUAD_DEFAULT && TARGET_IEEEQUAD && TARGET_LONG_DOUBLE_128
      && TREE_CODE (decl) == FUNCTION_DECL && DECL_IS_BUILTIN (decl) )
    {
      size_t len = IDENTIFIER_LENGTH (id);
      const char *name = IDENTIFIER_POINTER (id);

      if (name[len - 1] == 'l')
	{
	  bool uses_ieee128_p = false;
	  tree type = TREE_TYPE (decl);
	  machine_mode ret_mode = TYPE_MODE (type);

	  /* See if the function returns a IEEE 128-bit floating point type or
	     complex type.  */
	  if (ret_mode == TFmode || ret_mode == TCmode)
	    uses_ieee128_p = true;
	  else
	    {
	      function_args_iterator args_iter;
	      tree arg;

	      /* See if the function passes a IEEE 128-bit floating point type
		 or complex type.  */
	      FOREACH_FUNCTION_ARGS (type, arg, args_iter)
		{
		  machine_mode arg_mode = TYPE_MODE (arg);
		  if (arg_mode == TFmode || arg_mode == TCmode)
		    {
		      uses_ieee128_p = true;
		      break;
		    }
		}
	    }

	  /* If we passed or returned an IEEE 128-bit floating point type,
	     change the name.  */
	  if (uses_ieee128_p)
	    {
	      char *name2 = (char *) alloca (len + 4);
	      memcpy (name2, name, len - 1);
	      strcpy (name2 + len - 1, "f128");
	      id = get_identifier (name2);
	    }
	}
    }

  return id;
}

/* Predict whether the given loop in gimple will be transformed in the RTL
   doloop_optimize pass.  */

static bool
rs6000_predict_doloop_p (struct loop *loop)
{
  gcc_assert (loop);

  /* On rs6000, targetm.can_use_doloop_p is actually
     can_use_doloop_if_innermost.  Just ensure the loop is innermost.  */
  if (loop->inner != NULL)
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "Predict doloop failure due to"
			    " loop nesting.\n");
      return false;
    }

  return true;
}

/* Implement TARGET_CANNOT_SUBSTITUTE_MEM_EQUIV_P.  */

static bool
rs6000_cannot_substitute_mem_equiv_p (rtx mem)
{
  gcc_assert (MEM_P (mem));

  /* curr_insn_transform()'s handling of subregs cannot handle altivec AND:
     type addresses, so don't allow MEMs with those address types to be
     substituted as an equivalent expression.  See PR93974 for details.  */
  if (GET_CODE (XEXP (mem, 0)) == AND)
    return true;

  return false;
}

/* Implement TARGET_INVALID_CONVERSION.  */

static const char *
rs6000_invalid_conversion (const_tree fromtype, const_tree totype)
{
  /* Make sure we're working with the canonical types.  */
  if (TYPE_CANONICAL (fromtype) != NULL_TREE)
    fromtype = TYPE_CANONICAL (fromtype);
  if (TYPE_CANONICAL (totype) != NULL_TREE)
    totype = TYPE_CANONICAL (totype);

  machine_mode frommode = TYPE_MODE (fromtype);
  machine_mode tomode = TYPE_MODE (totype);

  if (frommode != tomode)
    {
      /* Do not allow conversions to/from PXImode and POImode types.  */
      if (frommode == PXImode)
	return N_("invalid conversion from type %<__vector_quad%>");
      if (tomode == PXImode)
	return N_("invalid conversion to type %<__vector_quad%>");
      if (frommode == POImode)
	return N_("invalid conversion from type %<__vector_pair%>");
      if (tomode == POImode)
	return N_("invalid conversion to type %<__vector_pair%>");
    }
  else if (POINTER_TYPE_P (fromtype) && POINTER_TYPE_P (totype))
    {
      /* We really care about the modes of the base types.  */
      frommode = TYPE_MODE (TREE_TYPE (fromtype));
      tomode = TYPE_MODE (TREE_TYPE (totype));

      /* Do not allow conversions to/from PXImode and POImode pointer
	 types, except to/from void pointers.  */
      if (frommode != tomode
	  && frommode != VOIDmode
	  && tomode != VOIDmode)
	{
	  if (frommode == PXImode)
	    return N_("invalid conversion from type %<* __vector_quad%>");
	  if (tomode == PXImode)
	    return N_("invalid conversion to type %<* __vector_quad%>");
	  if (frommode == POImode)
	    return N_("invalid conversion from type %<* __vector_pair%>");
	  if (tomode == POImode)
	    return N_("invalid conversion to type %<* __vector_pair%>");
	}
    }

  /* Conversion allowed.  */
  return NULL;
}

struct gcc_target targetm = TARGET_INITIALIZER;

#include "gt-rs6000.h"
