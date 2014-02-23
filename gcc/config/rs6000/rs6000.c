/* Subroutines used for code generation on IBM RS/6000.
   Copyright (C) 1991-2014 Free Software Foundation, Inc.
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

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "rtl.h"
#include "regs.h"
#include "hard-reg-set.h"
#include "insn-config.h"
#include "conditions.h"
#include "insn-attr.h"
#include "flags.h"
#include "recog.h"
#include "obstack.h"
#include "tree.h"
#include "stringpool.h"
#include "stor-layout.h"
#include "calls.h"
#include "print-tree.h"
#include "varasm.h"
#include "expr.h"
#include "optabs.h"
#include "except.h"
#include "function.h"
#include "output.h"
#include "dbxout.h"
#include "basic-block.h"
#include "diagnostic-core.h"
#include "toplev.h"
#include "ggc.h"
#include "hashtab.h"
#include "tm_p.h"
#include "target.h"
#include "target-def.h"
#include "common/common-target.h"
#include "langhooks.h"
#include "reload.h"
#include "cfgloop.h"
#include "sched-int.h"
#include "pointer-set.h"
#include "hash-table.h"
#include "vec.h"
#include "basic-block.h"
#include "tree-ssa-alias.h"
#include "internal-fn.h"
#include "gimple-fold.h"
#include "tree-eh.h"
#include "gimple-expr.h"
#include "is-a.h"
#include "gimple.h"
#include "gimplify.h"
#include "gimple-iterator.h"
#include "gimple-walk.h"
#include "intl.h"
#include "params.h"
#include "tm-constrs.h"
#include "ira.h"
#include "opts.h"
#include "tree-vectorizer.h"
#include "dumpfile.h"
#include "cgraph.h"
#include "target-globals.h"
#if TARGET_XCOFF
#include "xcoffout.h"  /* get declarations of xcoff_*_section_name */
#endif
#if TARGET_MACHO
#include "gstab.h"  /* for N_SLINE */
#endif

#ifndef TARGET_NO_PROTOTYPE
#define TARGET_NO_PROTOTYPE 0
#endif

#define min(A,B)	((A) < (B) ? (A) : (B))
#define max(A,B)	((A) > (B) ? (A) : (B))

/* Structure used to define the rs6000 stack */
typedef struct rs6000_stack {
  int reload_completed;		/* stack info won't change from here on */
  int first_gp_reg_save;	/* first callee saved GP register used */
  int first_fp_reg_save;	/* first callee saved FP register used */
  int first_altivec_reg_save;	/* first callee saved AltiVec register used */
  int lr_save_p;		/* true if the link reg needs to be saved */
  int cr_save_p;		/* true if the CR reg needs to be saved */
  unsigned int vrsave_mask;	/* mask of vec registers to save */
  int push_p;			/* true if we need to allocate stack space */
  int calls_p;			/* true if the function makes any calls */
  int world_save_p;		/* true if we're saving *everything*:
				   r13-r31, cr, f14-f31, vrsave, v20-v31  */
  enum rs6000_abi abi;		/* which ABI to use */
  int gp_save_offset;		/* offset to save GP regs from initial SP */
  int fp_save_offset;		/* offset to save FP regs from initial SP */
  int altivec_save_offset;	/* offset to save AltiVec regs from initial SP */
  int lr_save_offset;		/* offset to save LR from initial SP */
  int cr_save_offset;		/* offset to save CR from initial SP */
  int vrsave_save_offset;	/* offset to save VRSAVE from initial SP */
  int spe_gp_save_offset;	/* offset to save spe 64-bit gprs  */
  int varargs_save_offset;	/* offset to save the varargs registers */
  int ehrd_offset;		/* offset to EH return data */
  int ehcr_offset;		/* offset to EH CR field data */
  int reg_size;			/* register size (4 or 8) */
  HOST_WIDE_INT vars_size;	/* variable save area size */
  int parm_size;		/* outgoing parameter size */
  int save_size;		/* save area size */
  int fixed_size;		/* fixed size of stack frame */
  int gp_size;			/* size of saved GP registers */
  int fp_size;			/* size of saved FP registers */
  int altivec_size;		/* size of saved AltiVec registers */
  int cr_size;			/* size to hold CR if not in save_size */
  int vrsave_size;		/* size to hold VRSAVE if not in save_size */
  int altivec_padding_size;	/* size of altivec alignment padding if
				   not in save_size */
  int spe_gp_size;		/* size of 64-bit GPR save size for SPE */
  int spe_padding_size;
  HOST_WIDE_INT total_size;	/* total bytes allocated for stack */
  int spe_64bit_regs_used;
  int savres_strategy;
} rs6000_stack_t;

/* A C structure for machine-specific, per-function data.
   This is added to the cfun structure.  */
typedef struct GTY(()) machine_function
{
  /* Some local-dynamic symbol.  */
  const char *some_ld_name;
  /* Whether the instruction chain has been scanned already.  */
  int insn_chain_scanned_p;
  /* Flags if __builtin_return_address (n) with n >= 1 was used.  */
  int ra_needs_full_frame;
  /* Flags if __builtin_return_address (0) was used.  */
  int ra_need_lr;
  /* Cache lr_save_p after expansion of builtin_eh_return.  */
  int lr_save_state;
  /* Whether we need to save the TOC to the reserved stack location in the
     function prologue.  */
  bool save_toc_in_prologue;
  /* Offset from virtual_stack_vars_rtx to the start of the ABI_V4
     varargs save area.  */
  HOST_WIDE_INT varargs_save_offset;
  /* Temporary stack slot to use for SDmode copies.  This slot is
     64-bits wide and is allocated early enough so that the offset
     does not overflow the 16-bit load/store offset field.  */
  rtx sdmode_stack_slot;
  /* Flag if r2 setup is needed with ELFv2 ABI.  */
  bool r2_setup_needed;
} machine_function;

/* Support targetm.vectorize.builtin_mask_for_load.  */
static GTY(()) tree altivec_builtin_mask_for_load;

/* Set to nonzero once AIX common-mode calls have been defined.  */
static GTY(()) int common_mode_defined;

/* Label number of label created for -mrelocatable, to call to so we can
   get the address of the GOT section */
static int rs6000_pic_labelno;

#ifdef USING_ELFOS_H
/* Counter for labels which are to be placed in .fixup.  */
int fixuplabelno = 0;
#endif

/* Whether to use variant of AIX ABI for PowerPC64 Linux.  */
int dot_symbols;

/* Specify the machine mode that pointers have.  After generation of rtl, the
   compiler makes no further distinction between pointers and any other objects
   of this machine mode.  The type is unsigned since not all things that
   include rs6000.h also include machmode.h.  */
unsigned rs6000_pmode;

/* Width in bits of a pointer.  */
unsigned rs6000_pointer_size;

#ifdef HAVE_AS_GNU_ATTRIBUTE
/* Flag whether floating point values have been passed/returned.  */
static bool rs6000_passes_float;
/* Flag whether vector values have been passed/returned.  */
static bool rs6000_passes_vector;
/* Flag whether small (<= 8 byte) structures have been returned.  */
static bool rs6000_returns_struct;
#endif

/* Value is TRUE if register/mode pair is acceptable.  */
bool rs6000_hard_regno_mode_ok_p[NUM_MACHINE_MODES][FIRST_PSEUDO_REGISTER];

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
int toc_initialized;
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
static GTY(()) section *toc_section;

struct builtin_description
{
  const HOST_WIDE_INT mask;
  const enum insn_code icode;
  const char *const name;
  const enum rs6000_builtins code;
};

/* Describe the vector unit used for modes.  */
enum rs6000_vector rs6000_vector_unit[NUM_MACHINE_MODES];
enum rs6000_vector rs6000_vector_mem[NUM_MACHINE_MODES];

/* Register classes for various constraints that are based on the target
   switches.  */
enum reg_class rs6000_constraints[RS6000_CONSTRAINT_MAX];

/* Describe the alignment of a vector.  */
int rs6000_vector_align[NUM_MACHINE_MODES];

/* Map selected modes to types for builtins.  */
static GTY(()) tree builtin_mode_to_type[MAX_MACHINE_MODE][2];

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
  CR_REG_TYPE,
  SPE_ACC_TYPE,
  SPEFSCR_REG_TYPE
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

/* Register type masks based on the type, of valid addressing modes.  */
struct rs6000_reg_addr {
  enum insn_code reload_load;		/* INSN to reload for loading. */
  enum insn_code reload_store;		/* INSN to reload for storing.  */
  enum insn_code reload_fpr_gpr;	/* INSN to move from FPR to GPR.  */
  enum insn_code reload_gpr_vsx;	/* INSN to move from GPR to VSX.  */
  enum insn_code reload_vsx_gpr;	/* INSN to move from VSX to GPR.  */
  addr_mask_type addr_mask[(int)N_RELOAD_REG]; /* Valid address masks.  */
};

static struct rs6000_reg_addr reg_addr[NUM_MACHINE_MODES];

/* Helper function to say whether a mode supports PRE_INC or PRE_DEC.  */
static inline bool
mode_supports_pre_incdec_p (enum machine_mode mode)
{
  return ((reg_addr[mode].addr_mask[RELOAD_REG_ANY] & RELOAD_REG_PRE_INCDEC)
	  != 0);
}

/* Helper function to say whether a mode supports PRE_MODIFY.  */
static inline bool
mode_supports_pre_modify_p (enum machine_mode mode)
{
  return ((reg_addr[mode].addr_mask[RELOAD_REG_ANY] & RELOAD_REG_PRE_MODIFY)
	  != 0);
}


/* Target cpu costs.  */

struct processor_costs {
  const int mulsi;	  /* cost of SImode multiplication.  */
  const int mulsi_const;  /* cost of SImode multiplication by constant.  */
  const int mulsi_const9; /* cost of SImode mult by short constant.  */
  const int muldi;	  /* cost of DImode multiplication.  */
  const int divsi;	  /* cost of SImode division.  */
  const int divdi;	  /* cost of DImode division.  */
  const int fp;		  /* cost of simple SFmode and DFmode insns.  */
  const int dmul;	  /* cost of DFmode multiplication (and fmadd).  */
  const int sdiv;	  /* cost of SFmode division (fdivs).  */
  const int ddiv;	  /* cost of DFmode division (fdiv).  */
  const int cache_line_size;    /* cache line size in bytes. */
  const int l1_cache_size;	/* size of l1 cache, in kilobytes.  */
  const int l2_cache_size;	/* size of l2 cache, in kilobytes.  */
  const int simultaneous_prefetches; /* number of parallel prefetch
					operations.  */
};

const struct processor_costs *rs6000_cost;

/* Processor costs (relative to an add) */

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
  32,
  0,
  0,
  0,
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
  128,
  0,
  0,
  0,
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
  256,			/* l2 cache */
  12,			/* prefetch streams */
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
};


/* Table that classifies rs6000 builtin functions (pure, const, etc.).  */
#undef RS6000_BUILTIN_1
#undef RS6000_BUILTIN_2
#undef RS6000_BUILTIN_3
#undef RS6000_BUILTIN_A
#undef RS6000_BUILTIN_D
#undef RS6000_BUILTIN_E
#undef RS6000_BUILTIN_H
#undef RS6000_BUILTIN_P
#undef RS6000_BUILTIN_Q
#undef RS6000_BUILTIN_S
#undef RS6000_BUILTIN_X

#define RS6000_BUILTIN_1(ENUM, NAME, MASK, ATTR, ICODE) \
  { NAME, ICODE, MASK, ATTR },

#define RS6000_BUILTIN_2(ENUM, NAME, MASK, ATTR, ICODE)  \
  { NAME, ICODE, MASK, ATTR },

#define RS6000_BUILTIN_3(ENUM, NAME, MASK, ATTR, ICODE)  \
  { NAME, ICODE, MASK, ATTR },

#define RS6000_BUILTIN_A(ENUM, NAME, MASK, ATTR, ICODE)  \
  { NAME, ICODE, MASK, ATTR },

#define RS6000_BUILTIN_D(ENUM, NAME, MASK, ATTR, ICODE)  \
  { NAME, ICODE, MASK, ATTR },

#define RS6000_BUILTIN_E(ENUM, NAME, MASK, ATTR, ICODE)  \
  { NAME, ICODE, MASK, ATTR },

#define RS6000_BUILTIN_H(ENUM, NAME, MASK, ATTR, ICODE)  \
  { NAME, ICODE, MASK, ATTR },

#define RS6000_BUILTIN_P(ENUM, NAME, MASK, ATTR, ICODE)  \
  { NAME, ICODE, MASK, ATTR },

#define RS6000_BUILTIN_Q(ENUM, NAME, MASK, ATTR, ICODE)  \
  { NAME, ICODE, MASK, ATTR },

#define RS6000_BUILTIN_S(ENUM, NAME, MASK, ATTR, ICODE)  \
  { NAME, ICODE, MASK, ATTR },

#define RS6000_BUILTIN_X(ENUM, NAME, MASK, ATTR, ICODE)  \
  { NAME, ICODE, MASK, ATTR },

struct rs6000_builtin_info_type {
  const char *name;
  const enum insn_code icode;
  const HOST_WIDE_INT mask;
  const unsigned attr;
};

static const struct rs6000_builtin_info_type rs6000_builtin_info[] =
{
#include "rs6000-builtin.def"
};

#undef RS6000_BUILTIN_1
#undef RS6000_BUILTIN_2
#undef RS6000_BUILTIN_3
#undef RS6000_BUILTIN_A
#undef RS6000_BUILTIN_D
#undef RS6000_BUILTIN_E
#undef RS6000_BUILTIN_H
#undef RS6000_BUILTIN_P
#undef RS6000_BUILTIN_Q
#undef RS6000_BUILTIN_S
#undef RS6000_BUILTIN_X

/* Support for -mveclibabi=<xxx> to control which vector library to use.  */
static tree (*rs6000_veclib_handler) (tree, tree, tree);


static bool rs6000_debug_legitimate_address_p (enum machine_mode, rtx, bool);
static bool spe_func_has_64bit_regs_p (void);
static struct machine_function * rs6000_init_machine_status (void);
static int rs6000_ra_ever_killed (void);
static tree rs6000_handle_longcall_attribute (tree *, tree, tree, int, bool *);
static tree rs6000_handle_altivec_attribute (tree *, tree, tree, int, bool *);
static tree rs6000_handle_struct_attribute (tree *, tree, tree, int, bool *);
static tree rs6000_builtin_vectorized_libmass (tree, tree, tree);
static rtx rs6000_emit_set_long_const (rtx, HOST_WIDE_INT, HOST_WIDE_INT);
static int rs6000_memory_move_cost (enum machine_mode, reg_class_t, bool);
static bool rs6000_debug_rtx_costs (rtx, int, int, int, int *, bool);
static int rs6000_debug_address_cost (rtx, enum machine_mode, addr_space_t,
				      bool);
static int rs6000_debug_adjust_cost (rtx, rtx, rtx, int);
static bool is_microcoded_insn (rtx);
static bool is_nonpipeline_insn (rtx);
static bool is_cracked_insn (rtx);
static bool is_load_insn (rtx, rtx *);
static bool is_store_insn (rtx, rtx *);
static bool set_to_load_agen (rtx,rtx);
static bool insn_terminates_group_p (rtx , enum group_termination);
static bool insn_must_be_first_in_group (rtx);
static bool insn_must_be_last_in_group (rtx);
static void altivec_init_builtins (void);
static tree builtin_function_type (enum machine_mode, enum machine_mode,
				   enum machine_mode, enum machine_mode,
				   enum rs6000_builtins, const char *name);
static void rs6000_common_init_builtins (void);
static void paired_init_builtins (void);
static rtx paired_expand_predicate_builtin (enum insn_code, tree, rtx);
static void spe_init_builtins (void);
static void htm_init_builtins (void);
static rtx spe_expand_predicate_builtin (enum insn_code, tree, rtx);
static rtx spe_expand_evsel_builtin (enum insn_code, tree, rtx);
static int rs6000_emit_int_cmove (rtx, rtx, rtx, rtx);
static rs6000_stack_t *rs6000_stack_info (void);
static void is_altivec_return_reg (rtx, void *);
int easy_vector_constant (rtx, enum machine_mode);
static rtx rs6000_debug_legitimize_address (rtx, rtx, enum machine_mode);
static rtx rs6000_legitimize_tls_address (rtx, enum tls_model);
static int rs6000_tls_symbol_ref_1 (rtx *, void *);
static int rs6000_get_some_local_dynamic_name_1 (rtx *, void *);
static rtx rs6000_darwin64_record_arg (CUMULATIVE_ARGS *, const_tree,
				       bool, bool);
#if TARGET_MACHO
static void macho_branch_islands (void);
#endif
static rtx rs6000_legitimize_reload_address (rtx, enum machine_mode, int, int,
					     int, int *);
static rtx rs6000_debug_legitimize_reload_address (rtx, enum machine_mode, int,
						   int, int, int *);
static bool rs6000_mode_dependent_address (const_rtx);
static bool rs6000_debug_mode_dependent_address (const_rtx);
static enum reg_class rs6000_secondary_reload_class (enum reg_class,
						     enum machine_mode, rtx);
static enum reg_class rs6000_debug_secondary_reload_class (enum reg_class,
							   enum machine_mode,
							   rtx);
static enum reg_class rs6000_preferred_reload_class (rtx, enum reg_class);
static enum reg_class rs6000_debug_preferred_reload_class (rtx,
							   enum reg_class);
static bool rs6000_secondary_memory_needed (enum reg_class, enum reg_class,
					    enum machine_mode);
static bool rs6000_debug_secondary_memory_needed (enum reg_class,
						  enum reg_class,
						  enum machine_mode);
static bool rs6000_cannot_change_mode_class (enum machine_mode,
					     enum machine_mode,
					     enum reg_class);
static bool rs6000_debug_cannot_change_mode_class (enum machine_mode,
						   enum machine_mode,
						   enum reg_class);
static bool rs6000_save_toc_in_prologue_p (void);

rtx (*rs6000_legitimize_reload_address_ptr) (rtx, enum machine_mode, int, int,
					     int, int *)
  = rs6000_legitimize_reload_address;

static bool (*rs6000_mode_dependent_address_ptr) (const_rtx)
  = rs6000_mode_dependent_address;

enum reg_class (*rs6000_secondary_reload_class_ptr) (enum reg_class,
						     enum machine_mode, rtx)
  = rs6000_secondary_reload_class;

enum reg_class (*rs6000_preferred_reload_class_ptr) (rtx, enum reg_class)
  = rs6000_preferred_reload_class;

bool (*rs6000_secondary_memory_needed_ptr) (enum reg_class, enum reg_class,
					    enum machine_mode)
  = rs6000_secondary_memory_needed;

bool (*rs6000_cannot_change_mode_class_ptr) (enum machine_mode,
					     enum machine_mode,
					     enum reg_class)
  = rs6000_cannot_change_mode_class;

const int INSN_NOT_AVAILABLE = -1;

static void rs6000_print_isa_options (FILE *, int, const char *,
				      HOST_WIDE_INT);
static void rs6000_print_builtin_options (FILE *, int, const char *,
					  HOST_WIDE_INT);

static enum rs6000_reg_type register_to_reg_type (rtx, bool *);
static bool rs6000_secondary_reload_move (enum rs6000_reg_type,
					  enum rs6000_reg_type,
					  enum machine_mode,
					  secondary_reload_info *,
					  bool);

/* Hash table stuff for keeping track of TOC entries.  */

struct GTY(()) toc_hash_struct
{
  /* `key' will satisfy CONSTANT_P; in fact, it will satisfy
     ASM_OUTPUT_SPECIAL_POOL_ENTRY_P.  */
  rtx key;
  enum machine_mode key_mode;
  int labelno;
};

static GTY ((param_is (struct toc_hash_struct))) htab_t toc_hash_table;

/* Hash table to keep track of the argument types for builtin functions.  */

struct GTY(()) builtin_hash_struct
{
  tree type;
  enum machine_mode mode[4];	/* return value + 3 arguments.  */
  unsigned char uns_p[4];	/* and whether the types are unsigned.  */
};

static GTY ((param_is (struct builtin_hash_struct))) htab_t builtin_hash_table;


/* Default register names.  */
char rs6000_reg_names[][8] =
{
      "0",  "1",  "2",  "3",  "4",  "5",  "6",  "7",
      "8",  "9", "10", "11", "12", "13", "14", "15",
     "16", "17", "18", "19", "20", "21", "22", "23",
     "24", "25", "26", "27", "28", "29", "30", "31",
      "0",  "1",  "2",  "3",  "4",  "5",  "6",  "7",
      "8",  "9", "10", "11", "12", "13", "14", "15",
     "16", "17", "18", "19", "20", "21", "22", "23",
     "24", "25", "26", "27", "28", "29", "30", "31",
     "mq", "lr", "ctr","ap",
      "0",  "1",  "2",  "3",  "4",  "5",  "6",  "7",
      "ca",
      /* AltiVec registers.  */
      "0",  "1",  "2",  "3",  "4",  "5",  "6", "7",
      "8",  "9",  "10", "11", "12", "13", "14", "15",
      "16", "17", "18", "19", "20", "21", "22", "23",
      "24", "25", "26", "27", "28", "29", "30", "31",
      "vrsave", "vscr",
      /* SPE registers.  */
      "spe_acc", "spefscr",
      /* Soft frame pointer.  */
      "sfp",
      /* HTM SPR registers.  */
      "tfhar", "tfiar", "texasr"
};

#ifdef TARGET_REGNAMES
static const char alt_reg_names[][8] =
{
   "%r0",   "%r1",  "%r2",  "%r3",  "%r4",  "%r5",  "%r6",  "%r7",
   "%r8",   "%r9", "%r10", "%r11", "%r12", "%r13", "%r14", "%r15",
  "%r16",  "%r17", "%r18", "%r19", "%r20", "%r21", "%r22", "%r23",
  "%r24",  "%r25", "%r26", "%r27", "%r28", "%r29", "%r30", "%r31",
   "%f0",   "%f1",  "%f2",  "%f3",  "%f4",  "%f5",  "%f6",  "%f7",
   "%f8",   "%f9", "%f10", "%f11", "%f12", "%f13", "%f14", "%f15",
  "%f16",  "%f17", "%f18", "%f19", "%f20", "%f21", "%f22", "%f23",
  "%f24",  "%f25", "%f26", "%f27", "%f28", "%f29", "%f30", "%f31",
    "mq",    "lr",  "ctr",   "ap",
  "%cr0",  "%cr1", "%cr2", "%cr3", "%cr4", "%cr5", "%cr6", "%cr7",
   "ca",
  /* AltiVec registers.  */
   "%v0",  "%v1",  "%v2",  "%v3",  "%v4",  "%v5",  "%v6", "%v7",
   "%v8",  "%v9", "%v10", "%v11", "%v12", "%v13", "%v14", "%v15",
  "%v16", "%v17", "%v18", "%v19", "%v20", "%v21", "%v22", "%v23",
  "%v24", "%v25", "%v26", "%v27", "%v28", "%v29", "%v30", "%v31",
  "vrsave", "vscr",
  /* SPE registers.  */
  "spe_acc", "spefscr",
  /* Soft frame pointer.  */
  "sfp",
  /* HTM SPR registers.  */
  "tfhar", "tfiar", "texasr"
};
#endif

/* Table of valid machine attributes.  */

static const struct attribute_spec rs6000_attribute_table[] =
{
  /* { name, min_len, max_len, decl_req, type_req, fn_type_req, handler,
       affects_type_identity } */
  { "altivec",   1, 1, false, true,  false, rs6000_handle_altivec_attribute,
    false },
  { "longcall",  0, 0, false, true,  true,  rs6000_handle_longcall_attribute,
    false },
  { "shortcall", 0, 0, false, true,  true,  rs6000_handle_longcall_attribute,
    false },
  { "ms_struct", 0, 0, false, false, false, rs6000_handle_struct_attribute,
    false },
  { "gcc_struct", 0, 0, false, false, false, rs6000_handle_struct_attribute,
    false },
#ifdef SUBTARGET_ATTRIBUTE_TABLE
  SUBTARGET_ATTRIBUTE_TABLE,
#endif
  { NULL,        0, 0, false, false, false, NULL, false }
};

#ifndef TARGET_PROFILE_KERNEL
#define TARGET_PROFILE_KERNEL 0
#endif

/* The VRSAVE bitmask puts bit %v0 as the most significant bit.  */
#define ALTIVEC_REG_BIT(REGNO) (0x80000000 >> ((REGNO) - FIRST_ALTIVEC_REGNO))

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

#undef TARGET_HAVE_TLS
#define TARGET_HAVE_TLS HAVE_AS_TLS

#undef TARGET_CANNOT_FORCE_CONST_MEM
#define TARGET_CANNOT_FORCE_CONST_MEM rs6000_cannot_force_const_mem

#undef TARGET_DELEGITIMIZE_ADDRESS
#define TARGET_DELEGITIMIZE_ADDRESS rs6000_delegitimize_address

#undef TARGET_CONST_NOT_OK_FOR_DEBUG_P
#define TARGET_CONST_NOT_OK_FOR_DEBUG_P rs6000_const_not_ok_for_debug_p

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

#undef TARGET_INIT_BUILTINS
#define TARGET_INIT_BUILTINS rs6000_init_builtins
#undef TARGET_BUILTIN_DECL
#define TARGET_BUILTIN_DECL rs6000_builtin_decl

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
#undef TARGET_RTX_COSTS
#define TARGET_RTX_COSTS rs6000_rtx_costs
#undef TARGET_ADDRESS_COST
#define TARGET_ADDRESS_COST hook_int_rtx_mode_as_bool_0

#undef TARGET_DWARF_REGISTER_SPAN
#define TARGET_DWARF_REGISTER_SPAN rs6000_dwarf_register_span

#undef TARGET_INIT_DWARF_REG_SIZES_EXTRA
#define TARGET_INIT_DWARF_REG_SIZES_EXTRA rs6000_init_dwarf_reg_sizes_extra

#undef TARGET_MEMBER_TYPE_FORCES_BLK
#define TARGET_MEMBER_TYPE_FORCES_BLK rs6000_member_type_forces_blk

/* On rs6000, function arguments are promoted, as are function return
   values.  */
#undef TARGET_PROMOTE_FUNCTION_MODE
#define TARGET_PROMOTE_FUNCTION_MODE default_promote_function_mode_always_promote

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

#undef TARGET_SCALAR_MODE_SUPPORTED_P
#define TARGET_SCALAR_MODE_SUPPORTED_P rs6000_scalar_mode_supported_p

#undef TARGET_VECTOR_MODE_SUPPORTED_P
#define TARGET_VECTOR_MODE_SUPPORTED_P rs6000_vector_mode_supported_p

#undef TARGET_INVALID_ARG_FOR_UNPROTOTYPED_FN
#define TARGET_INVALID_ARG_FOR_UNPROTOTYPED_FN invalid_arg_for_unprototyped_fn

#undef TARGET_ASM_LOOP_ALIGN_MAX_SKIP
#define TARGET_ASM_LOOP_ALIGN_MAX_SKIP rs6000_loop_align_max_skip

#undef TARGET_OPTION_OVERRIDE
#define TARGET_OPTION_OVERRIDE rs6000_option_override

#undef TARGET_VECTORIZE_BUILTIN_VECTORIZED_FUNCTION
#define TARGET_VECTORIZE_BUILTIN_VECTORIZED_FUNCTION \
  rs6000_builtin_vectorized_function

#if !TARGET_MACHO
#undef TARGET_STACK_PROTECT_FAIL
#define TARGET_STACK_PROTECT_FAIL rs6000_stack_protect_fail
#endif

/* MPC604EUM 3.5.2 Weak Consistency between Multiple Processors
   The PowerPC architecture requires only weak consistency among
   processors--that is, memory accesses between processors need not be
   sequentially consistent and memory accesses among processors can occur
   in any order. The ability to order memory accesses weakly provides
   opportunities for more efficient use of the system bus. Unless a
   dependency exists, the 604e allows read operations to precede store
   operations.  */
#undef TARGET_RELAXED_ORDERING
#define TARGET_RELAXED_ORDERING true

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

#undef TARGET_EXPAND_TO_RTL_HOOK
#define TARGET_EXPAND_TO_RTL_HOOK rs6000_alloc_sdmode_stack_slot

#undef TARGET_INSTANTIATE_DECLS
#define TARGET_INSTANTIATE_DECLS rs6000_instantiate_decls

#undef TARGET_SECONDARY_RELOAD
#define TARGET_SECONDARY_RELOAD rs6000_secondary_reload

#undef TARGET_LEGITIMATE_ADDRESS_P
#define TARGET_LEGITIMATE_ADDRESS_P rs6000_legitimate_address_p

#undef TARGET_MODE_DEPENDENT_ADDRESS_P
#define TARGET_MODE_DEPENDENT_ADDRESS_P rs6000_mode_dependent_address_p

#undef TARGET_LRA_P
#define TARGET_LRA_P rs6000_lra_p

#undef TARGET_CAN_ELIMINATE
#define TARGET_CAN_ELIMINATE rs6000_can_eliminate

#undef TARGET_CONDITIONAL_REGISTER_USAGE
#define TARGET_CONDITIONAL_REGISTER_USAGE rs6000_conditional_register_usage

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

#undef TARGET_VECTORIZE_VEC_PERM_CONST_OK
#define TARGET_VECTORIZE_VEC_PERM_CONST_OK rs6000_vectorize_vec_perm_const_ok

#undef TARGET_CAN_USE_DOLOOP_P
#define TARGET_CAN_USE_DOLOOP_P can_use_doloop_if_innermost


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

   For the SPE, GPRs are 64 bits but only 32 bits are visible in
   scalar instructions.  The upper 32 bits are only available to the
   SIMD instructions.

   POWER and PowerPC GPRs hold 32 bits worth;
   PowerPC64 GPRs and FPRs point register holds 64 bits worth.  */

static int
rs6000_hard_regno_nregs_internal (int regno, enum machine_mode mode)
{
  unsigned HOST_WIDE_INT reg_size;

  /* TF/TD modes are special in that they always take 2 registers.  */
  if (FP_REGNO_P (regno))
    reg_size = ((VECTOR_MEM_VSX_P (mode) && mode != TDmode && mode != TFmode)
		? UNITS_PER_VSX_WORD
		: UNITS_PER_FP_WORD);

  else if (SPE_SIMD_REGNO_P (regno) && TARGET_SPE && SPE_VECTOR_MODE (mode))
    reg_size = UNITS_PER_SPE_WORD;

  else if (ALTIVEC_REGNO_P (regno))
    reg_size = UNITS_PER_ALTIVEC_WORD;

  /* The value returned for SCmode in the E500 double case is 2 for
     ABI compatibility; storing an SCmode value in a single register
     would require function_arg and rs6000_spe_function_arg to handle
     SCmode so as to pass the value correctly in a pair of
     registers.  */
  else if (TARGET_E500_DOUBLE && FLOAT_MODE_P (mode) && mode != SCmode
	   && !DECIMAL_FLOAT_MODE_P (mode))
    reg_size = UNITS_PER_FP_WORD;

  else
    reg_size = UNITS_PER_WORD;

  return (GET_MODE_SIZE (mode) + reg_size - 1) / reg_size;
}

/* Value is 1 if hard register REGNO can hold a value of machine-mode
   MODE.  */
static int
rs6000_hard_regno_mode_ok (int regno, enum machine_mode mode)
{
  int last_regno = regno + rs6000_hard_regno_nregs[mode][regno] - 1;

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
	  || (TARGET_VSX_SCALAR_FLOAT && mode == SFmode)
	  || (TARGET_VSX_SCALAR_DOUBLE && (mode == DFmode || mode == DImode))
	  || (TARGET_VSX_TIMODE && mode == TImode)))
    {
      if (FP_REGNO_P (regno))
	return FP_REGNO_P (last_regno);

      if (ALTIVEC_REGNO_P (regno))
	{
	  if (mode == SFmode && !TARGET_UPPER_REGS_SF)
	    return 0;

	  if ((mode == DFmode || mode == DImode) && !TARGET_UPPER_REGS_DF)
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
      if (SCALAR_FLOAT_MODE_P (mode)
	  && (mode != TDmode || (regno % 2) == 0)
	  && FP_REGNO_P (last_regno))
	return 1;

      if (GET_MODE_CLASS (mode) == MODE_INT
	  && GET_MODE_SIZE (mode) == UNITS_PER_FP_WORD)
	return 1;

      if (PAIRED_SIMD_REGNO_P (regno) && TARGET_PAIRED_FLOAT
	  && PAIRED_VECTOR_MODE (mode))
	return 1;

      return 0;
    }

  /* The CR register can only hold CC modes.  */
  if (CR_REGNO_P (regno))
    return GET_MODE_CLASS (mode) == MODE_CC;

  if (CA_REGNO_P (regno))
    return mode == BImode;

  /* AltiVec only in AldyVec registers.  */
  if (ALTIVEC_REGNO_P (regno))
    return VECTOR_MEM_ALTIVEC_OR_VSX_P (mode);

  /* ...but GPRs can hold SIMD data on the SPE in one register.  */
  if (SPE_SIMD_REGNO_P (regno) && TARGET_SPE && SPE_VECTOR_MODE (mode))
    return 1;

  /* We cannot put non-VSX TImode or PTImode anywhere except general register
     and it must be able to fit within the register set.  */

  return GET_MODE_SIZE (mode) <= UNITS_PER_WORD;
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

      if (call_used_regs[r])
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
    case VECTOR_PAIRED:	   ret = "paired";    break;
    case VECTOR_SPE:	   ret = "spe";       break;
    case VECTOR_OTHER:	   ret = "other";     break;
    default:		   ret = "unknown";   break;
    }

  return ret;
}

/* Print the address masks in a human readble fashion.  */
DEBUG_FUNCTION void
rs6000_debug_print_mode (ssize_t m)
{
  ssize_t rc;

  fprintf (stderr, "Mode: %-5s", GET_MODE_NAME (m));
  for (rc = 0; rc < N_RELOAD_REG; rc++)
    {
      addr_mask_type mask = reg_addr[m].addr_mask[rc];
      fprintf (stderr,
	       "  %s: %c%c%c%c%c%c",
	       reload_reg_map[rc].name,
	       (mask & RELOAD_REG_VALID)      != 0 ? 'v' : ' ',
	       (mask & RELOAD_REG_MULTIPLE)   != 0 ? 'm' : ' ',
	       (mask & RELOAD_REG_INDEXED)    != 0 ? 'i' : ' ',
	       (mask & RELOAD_REG_OFFSET)     != 0 ? 'o' : ' ',
	       (mask & RELOAD_REG_PRE_INCDEC) != 0 ? '+' : ' ',
	       (mask & RELOAD_REG_PRE_MODIFY) != 0 ? '+' : ' ');
    }

  if (rs6000_vector_unit[m] != VECTOR_NONE
      || rs6000_vector_mem[m] != VECTOR_NONE
      || (reg_addr[m].reload_store != CODE_FOR_nothing)
      || (reg_addr[m].reload_load != CODE_FOR_nothing))
    {
      fprintf (stderr,
	       "  Vector-arith=%-10s Vector-mem=%-10s Reload=%c%c",
	       rs6000_debug_vector_unit (rs6000_vector_unit[m]),
	       rs6000_debug_vector_unit (rs6000_vector_mem[m]),
	       (reg_addr[m].reload_store != CODE_FOR_nothing) ? 's' : '*',
	       (reg_addr[m].reload_load != CODE_FOR_nothing) ? 'l' : '*');
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
  static const enum machine_mode print_tieable_modes[] = {
    QImode,
    HImode,
    SImode,
    DImode,
    TImode,
    PTImode,
    SFmode,
    DFmode,
    TFmode,
    SDmode,
    DDmode,
    TDmode,
    V8QImode,
    V4HImode,
    V2SImode,
    V16QImode,
    V8HImode,
    V4SImode,
    V2DImode,
    V32QImode,
    V16HImode,
    V8SImode,
    V4DImode,
    V2SFmode,
    V4SFmode,
    V2DFmode,
    V8SFmode,
    V4DFmode,
    CCmode,
    CCUNSmode,
    CCEQmode,
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
  rs6000_debug_reg_print (SPE_ACC_REGNO, SPE_ACC_REGNO, "spe_a");
  rs6000_debug_reg_print (SPEFSCR_REGNO, SPEFSCR_REGNO, "spe_f");

  fputs ("\nVirtual/stack/frame registers:\n", stderr);
  for (v = 0; v < ARRAY_SIZE (virtual_regs); v++)
    fprintf (stderr, "%s regno = %3d\n", virtual_regs[v].name, virtual_regs[v].regno);

  fprintf (stderr,
	   "\n"
	   "d  reg_class = %s\n"
	   "f  reg_class = %s\n"
	   "v  reg_class = %s\n"
	   "wa reg_class = %s\n"
	   "wd reg_class = %s\n"
	   "wf reg_class = %s\n"
	   "wg reg_class = %s\n"
	   "wl reg_class = %s\n"
	   "wm reg_class = %s\n"
	   "wr reg_class = %s\n"
	   "ws reg_class = %s\n"
	   "wt reg_class = %s\n"
	   "wu reg_class = %s\n"
	   "wv reg_class = %s\n"
	   "ww reg_class = %s\n"
	   "wx reg_class = %s\n"
	   "wy reg_class = %s\n"
	   "wz reg_class = %s\n"
	   "\n",
	   reg_class_names[rs6000_constraints[RS6000_CONSTRAINT_d]],
	   reg_class_names[rs6000_constraints[RS6000_CONSTRAINT_f]],
	   reg_class_names[rs6000_constraints[RS6000_CONSTRAINT_v]],
	   reg_class_names[rs6000_constraints[RS6000_CONSTRAINT_wa]],
	   reg_class_names[rs6000_constraints[RS6000_CONSTRAINT_wd]],
	   reg_class_names[rs6000_constraints[RS6000_CONSTRAINT_wf]],
	   reg_class_names[rs6000_constraints[RS6000_CONSTRAINT_wg]],
	   reg_class_names[rs6000_constraints[RS6000_CONSTRAINT_wl]],
	   reg_class_names[rs6000_constraints[RS6000_CONSTRAINT_wm]],
	   reg_class_names[rs6000_constraints[RS6000_CONSTRAINT_wr]],
	   reg_class_names[rs6000_constraints[RS6000_CONSTRAINT_ws]],
	   reg_class_names[rs6000_constraints[RS6000_CONSTRAINT_wt]],
	   reg_class_names[rs6000_constraints[RS6000_CONSTRAINT_wu]],
	   reg_class_names[rs6000_constraints[RS6000_CONSTRAINT_wv]],
	   reg_class_names[rs6000_constraints[RS6000_CONSTRAINT_ww]],
	   reg_class_names[rs6000_constraints[RS6000_CONSTRAINT_wx]],
	   reg_class_names[rs6000_constraints[RS6000_CONSTRAINT_wy]],
	   reg_class_names[rs6000_constraints[RS6000_CONSTRAINT_wz]]);

  nl = "\n";
  for (m = 0; m < NUM_MACHINE_MODES; ++m)
    rs6000_debug_print_mode (m);

  fputs ("\n", stderr);

  for (m1 = 0; m1 < ARRAY_SIZE (print_tieable_modes); m1++)
    {
      enum machine_mode mode1 = print_tieable_modes[m1];
      bool first_time = true;

      nl = (const char *)0;
      for (m2 = 0; m2 < ARRAY_SIZE (print_tieable_modes); m2++)
	{
	  enum machine_mode mode2 = print_tieable_modes[m2];
	  if (mode1 != mode2 && MODES_TIEABLE_P (mode1, mode2))
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

  if (rs6000_spe_abi)
    fprintf (stderr, DEBUG_FMT_S, "spe_abi", "true");

  if (rs6000_darwin64_abi)
    fprintf (stderr, DEBUG_FMT_S, "darwin64_abi", "true");

  if (rs6000_float_gprs)
    fprintf (stderr, DEBUG_FMT_S, "float_gprs", "true");

  if (TARGET_LINK_STACK)
    fprintf (stderr, DEBUG_FMT_S, "link_stack", "true");

  if (targetm.lra_p ())
    fprintf (stderr, DEBUG_FMT_S, "lra", "true");

  if (TARGET_P8_FUSION)
    fprintf (stderr, DEBUG_FMT_S, "p8 fusion",
	     (TARGET_P8_FUSION_SIGN) ? "zero+sign" : "zero");

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
  fprintf (stderr, DEBUG_FMT_D, "sched_restricted_insns_priority",
	   (int)rs6000_sched_restricted_insns_priority);
  fprintf (stderr, DEBUG_FMT_D, "Number of standard builtins",
	   (int)END_BUILTINS);
  fprintf (stderr, DEBUG_FMT_D, "Number of rs6000 builtins",
	   (int)RS6000_BUILTIN_COUNT);
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
	      nregs = rs6000_hard_regno_nregs[m][reg];
	      addr_mask |= RELOAD_REG_VALID;

	      /* Indicate if the mode takes more than 1 physical register.  If
		 it takes a single register, indicate it can do REG+REG
		 addressing.  */
	      if (nregs > 1 || m == BLKmode)
		addr_mask |= RELOAD_REG_MULTIPLE;
	      else
		addr_mask |= RELOAD_REG_INDEXED;

	      /* Figure out if we can do PRE_INC, PRE_DEC, or PRE_MODIFY
		 addressing.  Restrict addressing on SPE for 64-bit types
		 because of the SUBREG hackery used to address 64-bit floats in
		 '32-bit' GPRs.  To simplify secondary reload, don't allow
		 update forms on scalar floating point types that can go in the
		 upper registers.  */

	      if (TARGET_UPDATE
		  && (rc == RELOAD_REG_GPR || rc == RELOAD_REG_FPR)
		  && GET_MODE_SIZE (m) <= 8
		  && !VECTOR_MODE_P (m)
		  && !COMPLEX_MODE_P (m)
		  && !indexed_only_p
		  && !(TARGET_E500_DOUBLE && GET_MODE_SIZE (m) == 8)
		  && !(m == DFmode && TARGET_UPPER_REGS_DF)
		  && !(m == SFmode && TARGET_UPPER_REGS_SF))
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

		    case DImode:
		      if (TARGET_POWERPC64)
			addr_mask |= RELOAD_REG_PRE_MODIFY;
		      break;

		    case DFmode:
		    case DDmode:
		      if (TARGET_DF_INSN)
			addr_mask |= RELOAD_REG_PRE_MODIFY;
		      break;
		    }
		}
	    }

	  /* GPR and FPR registers can do REG+OFFSET addressing, except
	     possibly for SDmode.  */
	  if ((addr_mask != 0) && !indexed_only_p
	      && (rc == RELOAD_REG_GPR || rc == RELOAD_REG_FPR))
	    addr_mask |= RELOAD_REG_OFFSET;

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

  for (r = 64; r < FIRST_PSEUDO_REGISTER; ++r)
    rs6000_regno_regclass[r] = NO_REGS;

  for (r = FIRST_ALTIVEC_REGNO; r <= LAST_ALTIVEC_REGNO; ++r)
    rs6000_regno_regclass[r] = ALTIVEC_REGS;

  rs6000_regno_regclass[CR0_REGNO] = CR0_REGS;
  for (r = CR1_REGNO; r <= CR7_REGNO; ++r)
    rs6000_regno_regclass[r] = CR_REGS;

  rs6000_regno_regclass[LR_REGNO] = LINK_REGS;
  rs6000_regno_regclass[CTR_REGNO] = CTR_REGS;
  rs6000_regno_regclass[CA_REGNO] = CA_REGS;
  rs6000_regno_regclass[VRSAVE_REGNO] = VRSAVE_REGS;
  rs6000_regno_regclass[VSCR_REGNO] = VRSAVE_REGS;
  rs6000_regno_regclass[SPE_ACC_REGNO] = SPE_ACC_REGS;
  rs6000_regno_regclass[SPEFSCR_REGNO] = SPEFSCR_REGS;
  rs6000_regno_regclass[TFHAR_REGNO] = SPR_REGS;
  rs6000_regno_regclass[TFIAR_REGNO] = SPR_REGS;
  rs6000_regno_regclass[TEXASR_REGNO] = SPR_REGS;
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
  reg_class_to_reg_type[(int)SPE_ACC_REGS] = SPE_ACC_TYPE;
  reg_class_to_reg_type[(int)SPEFSCR_REGS] = SPEFSCR_REG_TYPE;

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
  memset ((void *) &rs6000_vector_mem[0], '\0', sizeof (rs6000_vector_unit));

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
    }

  /* DFmode, see if we want to use the VSX unit.  */
  if (TARGET_VSX && TARGET_VSX_SCALAR_DOUBLE)
    {
      rs6000_vector_unit[DFmode] = VECTOR_VSX;
      rs6000_vector_mem[DFmode]
	= (TARGET_UPPER_REGS_DF ? VECTOR_VSX : VECTOR_NONE);
      rs6000_vector_align[DFmode] = align64;
    }

  /* Allow TImode in VSX register and set the VSX memory macros.  */
  if (TARGET_VSX && TARGET_VSX_TIMODE)
    {
      rs6000_vector_mem[TImode] = VECTOR_VSX;
      rs6000_vector_align[TImode] = align64;
    }

  /* TODO add SPE and paired floating point vector support.  */

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
	wd - Preferred register class for V2DFmode.
	wf - Preferred register class for V4SFmode.
	wg - Float register for power6x move insns.
	wl - Float register if we can do 32-bit signed int loads.
	wm - VSX register for ISA 2.07 direct move operations.
	wr - GPR if 64-bit mode is permitted.
	ws - Register class to do ISA 2.06 DF operations.
	wu - Altivec register for ISA 2.07 VSX SF/SI load/stores.
	wv - Altivec register for ISA 2.06 VSX DF/DI load/stores.
	wt - VSX register for TImode in VSX registers.
	ww - Register class to do SF conversions in with VSX operations.
	wx - Float register if we can do 32-bit int stores.
	wy - Register class to do ISA 2.07 SF operations.
	wz - Float register if we can do 32-bit unsigned int loads.  */

  if (TARGET_HARD_FLOAT && TARGET_FPRS)
    rs6000_constraints[RS6000_CONSTRAINT_f] = FLOAT_REGS;

  if (TARGET_HARD_FLOAT && TARGET_FPRS && TARGET_DOUBLE_FLOAT)
    rs6000_constraints[RS6000_CONSTRAINT_d] = FLOAT_REGS;

  if (TARGET_VSX)
    {
      rs6000_constraints[RS6000_CONSTRAINT_wa] = VSX_REGS;
      rs6000_constraints[RS6000_CONSTRAINT_wd] = VSX_REGS;
      rs6000_constraints[RS6000_CONSTRAINT_wf] = VSX_REGS;

      if (TARGET_VSX_TIMODE)
	rs6000_constraints[RS6000_CONSTRAINT_wt] = VSX_REGS;

      if (TARGET_UPPER_REGS_DF)
	{
	  rs6000_constraints[RS6000_CONSTRAINT_ws] = VSX_REGS;
	  rs6000_constraints[RS6000_CONSTRAINT_wv] = ALTIVEC_REGS;
	}
      else
	rs6000_constraints[RS6000_CONSTRAINT_ws] = FLOAT_REGS;
    }

  /* Add conditional constraints based on various options, to allow us to
     collapse multiple insn patterns.  */
  if (TARGET_ALTIVEC)
    rs6000_constraints[RS6000_CONSTRAINT_v] = ALTIVEC_REGS;

  if (TARGET_MFPGPR)
    rs6000_constraints[RS6000_CONSTRAINT_wg] = FLOAT_REGS;

  if (TARGET_LFIWAX)
    rs6000_constraints[RS6000_CONSTRAINT_wl] = FLOAT_REGS;

  if (TARGET_DIRECT_MOVE)
    rs6000_constraints[RS6000_CONSTRAINT_wm] = VSX_REGS;

  if (TARGET_POWERPC64)
    rs6000_constraints[RS6000_CONSTRAINT_wr] = GENERAL_REGS;

  if (TARGET_P8_VECTOR && TARGET_UPPER_REGS_SF)
    {
      rs6000_constraints[RS6000_CONSTRAINT_wu] = ALTIVEC_REGS;
      rs6000_constraints[RS6000_CONSTRAINT_wy] = VSX_REGS;
      rs6000_constraints[RS6000_CONSTRAINT_ww] = VSX_REGS;
    }
  else if (TARGET_P8_VECTOR)
    {
      rs6000_constraints[RS6000_CONSTRAINT_wy] = FLOAT_REGS;
      rs6000_constraints[RS6000_CONSTRAINT_ww] = FLOAT_REGS;
    }
  else if (TARGET_VSX)
    rs6000_constraints[RS6000_CONSTRAINT_ww] = FLOAT_REGS;

  if (TARGET_STFIWX)
    rs6000_constraints[RS6000_CONSTRAINT_wx] = FLOAT_REGS;

  if (TARGET_LFIWZX)
    rs6000_constraints[RS6000_CONSTRAINT_wz] = FLOAT_REGS;

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
	  reg_addr[V4SFmode].reload_store  = CODE_FOR_reload_v4sf_di_store;
	  reg_addr[V4SFmode].reload_load   = CODE_FOR_reload_v4sf_di_load;
	  reg_addr[V2DFmode].reload_store  = CODE_FOR_reload_v2df_di_store;
	  reg_addr[V2DFmode].reload_load   = CODE_FOR_reload_v2df_di_load;
	  if (TARGET_VSX && TARGET_UPPER_REGS_DF)
	    {
	      reg_addr[DFmode].reload_store  = CODE_FOR_reload_df_di_store;
	      reg_addr[DFmode].reload_load   = CODE_FOR_reload_df_di_load;
	      reg_addr[DDmode].reload_store  = CODE_FOR_reload_dd_di_store;
	      reg_addr[DDmode].reload_load   = CODE_FOR_reload_dd_di_load;
	    }
	  if (TARGET_P8_VECTOR)
	    {
	      reg_addr[SFmode].reload_store  = CODE_FOR_reload_sf_di_store;
	      reg_addr[SFmode].reload_load   = CODE_FOR_reload_sf_di_load;
	      reg_addr[SDmode].reload_store  = CODE_FOR_reload_sd_di_store;
	      reg_addr[SDmode].reload_load   = CODE_FOR_reload_sd_di_load;
	    }
	  if (TARGET_VSX_TIMODE)
	    {
	      reg_addr[TImode].reload_store  = CODE_FOR_reload_ti_di_store;
	      reg_addr[TImode].reload_load   = CODE_FOR_reload_ti_di_load;
	    }
	  if (TARGET_DIRECT_MOVE)
	    {
	      if (TARGET_POWERPC64)
		{
		  reg_addr[TImode].reload_gpr_vsx    = CODE_FOR_reload_gpr_from_vsxti;
		  reg_addr[V2DFmode].reload_gpr_vsx  = CODE_FOR_reload_gpr_from_vsxv2df;
		  reg_addr[V2DImode].reload_gpr_vsx  = CODE_FOR_reload_gpr_from_vsxv2di;
		  reg_addr[V4SFmode].reload_gpr_vsx  = CODE_FOR_reload_gpr_from_vsxv4sf;
		  reg_addr[V4SImode].reload_gpr_vsx  = CODE_FOR_reload_gpr_from_vsxv4si;
		  reg_addr[V8HImode].reload_gpr_vsx  = CODE_FOR_reload_gpr_from_vsxv8hi;
		  reg_addr[V16QImode].reload_gpr_vsx = CODE_FOR_reload_gpr_from_vsxv16qi;
		  reg_addr[SFmode].reload_gpr_vsx    = CODE_FOR_reload_gpr_from_vsxsf;

		  reg_addr[TImode].reload_vsx_gpr    = CODE_FOR_reload_vsx_from_gprti;
		  reg_addr[V2DFmode].reload_vsx_gpr  = CODE_FOR_reload_vsx_from_gprv2df;
		  reg_addr[V2DImode].reload_vsx_gpr  = CODE_FOR_reload_vsx_from_gprv2di;
		  reg_addr[V4SFmode].reload_vsx_gpr  = CODE_FOR_reload_vsx_from_gprv4sf;
		  reg_addr[V4SImode].reload_vsx_gpr  = CODE_FOR_reload_vsx_from_gprv4si;
		  reg_addr[V8HImode].reload_vsx_gpr  = CODE_FOR_reload_vsx_from_gprv8hi;
		  reg_addr[V16QImode].reload_vsx_gpr = CODE_FOR_reload_vsx_from_gprv16qi;
		  reg_addr[SFmode].reload_vsx_gpr    = CODE_FOR_reload_vsx_from_gprsf;
		}
	      else
		{
		  reg_addr[DImode].reload_fpr_gpr = CODE_FOR_reload_fpr_from_gprdi;
		  reg_addr[DDmode].reload_fpr_gpr = CODE_FOR_reload_fpr_from_gprdd;
		  reg_addr[DFmode].reload_fpr_gpr = CODE_FOR_reload_fpr_from_gprdf;
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
	  reg_addr[V4SFmode].reload_store  = CODE_FOR_reload_v4sf_si_store;
	  reg_addr[V4SFmode].reload_load   = CODE_FOR_reload_v4sf_si_load;
	  reg_addr[V2DFmode].reload_store  = CODE_FOR_reload_v2df_si_store;
	  reg_addr[V2DFmode].reload_load   = CODE_FOR_reload_v2df_si_load;
	  if (TARGET_VSX && TARGET_UPPER_REGS_DF)
	    {
	      reg_addr[DFmode].reload_store  = CODE_FOR_reload_df_si_store;
	      reg_addr[DFmode].reload_load   = CODE_FOR_reload_df_si_load;
	      reg_addr[DDmode].reload_store  = CODE_FOR_reload_dd_si_store;
	      reg_addr[DDmode].reload_load   = CODE_FOR_reload_dd_si_load;
	    }
	  if (TARGET_P8_VECTOR)
	    {
	      reg_addr[SFmode].reload_store  = CODE_FOR_reload_sf_si_store;
	      reg_addr[SFmode].reload_load   = CODE_FOR_reload_sf_si_load;
	      reg_addr[SDmode].reload_store  = CODE_FOR_reload_sd_si_store;
	      reg_addr[SDmode].reload_load   = CODE_FOR_reload_sd_si_load;
	    }
	  if (TARGET_VSX_TIMODE)
	    {
	      reg_addr[TImode].reload_store  = CODE_FOR_reload_ti_si_store;
	      reg_addr[TImode].reload_load   = CODE_FOR_reload_ti_si_load;
	    }
	}
    }

  /* Precalculate HARD_REGNO_NREGS.  */
  for (r = 0; r < FIRST_PSEUDO_REGISTER; ++r)
    for (m = 0; m < NUM_MACHINE_MODES; ++m)
      rs6000_hard_regno_nregs[m][r]
	= rs6000_hard_regno_nregs_internal (r, (enum machine_mode)m);

  /* Precalculate HARD_REGNO_MODE_OK.  */
  for (r = 0; r < FIRST_PSEUDO_REGISTER; ++r)
    for (m = 0; m < NUM_MACHINE_MODES; ++m)
      if (rs6000_hard_regno_mode_ok (r, (enum machine_mode)m))
	rs6000_hard_regno_mode_ok_p[m][r] = true;

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
	  int reg_size2 = reg_size;

	  /* TFmode/TDmode always takes 2 registers, even in VSX.  */
	  if (TARGET_VSX && VSX_REG_CLASS_P (c)
	      && (m == TDmode || m == TFmode))
	    reg_size2 = UNITS_PER_FP_WORD;

	  rs6000_class_max_nregs[m][c]
	    = (GET_MODE_SIZE (m) + reg_size2 - 1) / reg_size2;
	}
    }

  if (TARGET_E500_DOUBLE)
    rs6000_class_max_nregs[DFmode][GENERAL_REGS] = 1;

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
	warning (0, "-mrecip requires -ffinite-math or -ffast-math");
      if (flag_trapping_math)
	warning (0, "-mrecip requires -fno-trapping-math or -ffast-math");
      if (!flag_reciprocal_math)
	warning (0, "-mrecip requires -freciprocal-math or -ffast-math");
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
      warning (0, "-m64 requires PowerPC64 architecture, enabling");
    }
  if (flag_mkernel)
    {
      rs6000_default_long_calls = 1;
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
   bits, and some options like SPE and PAIRED are no longer in
   target_flags.  */

HOST_WIDE_INT
rs6000_builtin_mask_calculate (void)
{
  return (((TARGET_ALTIVEC)		    ? RS6000_BTM_ALTIVEC   : 0)
	  | ((TARGET_VSX)		    ? RS6000_BTM_VSX	   : 0)
	  | ((TARGET_SPE)		    ? RS6000_BTM_SPE	   : 0)
	  | ((TARGET_PAIRED_FLOAT)	    ? RS6000_BTM_PAIRED	   : 0)
	  | ((TARGET_FRE)		    ? RS6000_BTM_FRE	   : 0)
	  | ((TARGET_FRES)		    ? RS6000_BTM_FRES	   : 0)
	  | ((TARGET_FRSQRTE)		    ? RS6000_BTM_FRSQRTE   : 0)
	  | ((TARGET_FRSQRTES)		    ? RS6000_BTM_FRSQRTES  : 0)
	  | ((TARGET_POPCNTD)		    ? RS6000_BTM_POPCNTD   : 0)
	  | ((rs6000_cpu == PROCESSOR_CELL) ? RS6000_BTM_CELL      : 0)
	  | ((TARGET_P8_VECTOR)		    ? RS6000_BTM_P8_VECTOR : 0)
	  | ((TARGET_CRYPTO)		    ? RS6000_BTM_CRYPTO	   : 0)
	  | ((TARGET_HTM)		    ? RS6000_BTM_HTM	   : 0));
}

/* Override command line options.  Mostly we process the processor type and
   sometimes adjust other TARGET_ options.  */

static bool
rs6000_option_override_internal (bool global_init_p)
{
  bool ret = true;
  bool have_cpu = false;

  /* The default cpu requested at configure time, if any.  */
  const char *implicit_cpu = OPTION_TARGET_CPU_DEFAULT;

  HOST_WIDE_INT set_masks;
  int cpu_index;
  int tune_index;
  struct cl_target_option *main_target_opt
    = ((global_init_p || target_option_default_node == NULL)
       ? NULL : TREE_TARGET_OPTION (target_option_default_node));

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
    warning (0, "-malign-power is not supported for 64-bit Darwin;"
	     " it is incompatible with the installed C and C++ libraries");

  /* Numerous experiment shows that IRA based loop pressure
     calculation works better for RTL loop invariant motion on targets
     with enough (>= 32) registers.  It is an expensive optimization.
     So it is on only for peak performance.  */
  if (optimize >= 3 && global_init_p
      && !global_options_set.x_flag_ira_loop_pressure)
    flag_ira_loop_pressure = 1;

  /* Set the pointer size.  */
  if (TARGET_64BIT)
    {
      rs6000_pmode = (int)DImode;
      rs6000_pointer_size = 64;
    }
  else
    {
      rs6000_pmode = (int)SImode;
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
    set_masks &= ~(OPTION_MASK_ALTIVEC | OPTION_MASK_VSX);
#endif

  /* Don't override by the processor default if given explicitly.  */
  set_masks &= ~rs6000_isa_flags_explicit;

  /* Process the -mcpu=<xxx> and -mtune=<xxx> argument.  If the user changed
     the cpu in a target attribute or pragma, but did not specify a tuning
     option, use the cpu for the tuning option rather than the option specified
     with -mtune on the command line.  Process a '--with-cpu' configuration
     request as an implicit --cpu.  */
  if (rs6000_cpu_index >= 0)
    {
      cpu_index = rs6000_cpu_index;
      have_cpu = true;
    }
  else if (main_target_opt != NULL && main_target_opt->x_rs6000_cpu_index >= 0)
    {
      rs6000_cpu_index = cpu_index = main_target_opt->x_rs6000_cpu_index;
      have_cpu = true;
    }
  else if (implicit_cpu)
    {
      rs6000_cpu_index = cpu_index = rs6000_cpu_name_lookup (implicit_cpu);
      have_cpu = true;
    }
  else
    {
      const char *default_cpu = (TARGET_POWERPC64 ? "powerpc64" : "powerpc");
      rs6000_cpu_index = cpu_index = rs6000_cpu_name_lookup (default_cpu);
      have_cpu = false;
    }

  gcc_assert (cpu_index >= 0);

  /* If we have a cpu, either through an explicit -mcpu=<xxx> or if the
     compiler was configured with --with-cpu=<xxx>, replace all of the ISA bits
     with those from the cpu, except for options that were explicitly set.  If
     we don't have a cpu, do not override the target bits set in
     TARGET_DEFAULT.  */
  if (have_cpu)
    {
      rs6000_isa_flags &= ~set_masks;
      rs6000_isa_flags |= (processor_target_table[cpu_index].target_enable
			   & set_masks);
    }
  else
    rs6000_isa_flags |= (processor_target_table[cpu_index].target_enable
			 & ~rs6000_isa_flags_explicit);

  /* If no -mcpu=<xxx>, inherit any default options that were cleared via
     POWERPC_MASKS.  Originally, TARGET_DEFAULT was used to initialize
     target_flags via the TARGET_DEFAULT_TARGET_FLAGS hook.  When we switched
     to using rs6000_isa_flags, we need to do the initialization here.  */
  if (!have_cpu)
    rs6000_isa_flags |= (TARGET_DEFAULT & ~rs6000_isa_flags_explicit);

  if (rs6000_tune_index >= 0)
    tune_index = rs6000_tune_index;
  else if (have_cpu)
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
	    rs6000_tune_index = tune_index = i;
	    break;
	  }
    }

  gcc_assert (tune_index >= 0);
  rs6000_cpu = processor_target_table[tune_index].processor;

  /* Pick defaults for SPE related control flags.  Do this early to make sure
     that the TARGET_ macros are representative ASAP.  */
  {
    int spe_capable_cpu =
      (rs6000_cpu == PROCESSOR_PPC8540
       || rs6000_cpu == PROCESSOR_PPC8548);

    if (!global_options_set.x_rs6000_spe_abi)
      rs6000_spe_abi = spe_capable_cpu;

    if (!global_options_set.x_rs6000_spe)
      rs6000_spe = spe_capable_cpu;

    if (!global_options_set.x_rs6000_float_gprs)
      rs6000_float_gprs =
        (rs6000_cpu == PROCESSOR_PPC8540 ? 1
         : rs6000_cpu == PROCESSOR_PPC8548 ? 2
         : 0);
  }

  if (global_options_set.x_rs6000_spe_abi
      && rs6000_spe_abi
      && !TARGET_SPE_ABI)
    error ("not configured for SPE ABI");

  if (global_options_set.x_rs6000_spe
      && rs6000_spe
      && !TARGET_SPE)
    error ("not configured for SPE instruction set");

  if (main_target_opt != NULL
      && ((main_target_opt->x_rs6000_spe_abi != rs6000_spe_abi)
          || (main_target_opt->x_rs6000_spe != rs6000_spe)
          || (main_target_opt->x_rs6000_float_gprs != rs6000_float_gprs)))
    error ("target attribute or pragma changes SPE ABI");

  if (rs6000_cpu == PROCESSOR_PPCE300C2 || rs6000_cpu == PROCESSOR_PPCE300C3
      || rs6000_cpu == PROCESSOR_PPCE500MC || rs6000_cpu == PROCESSOR_PPCE500MC64
      || rs6000_cpu == PROCESSOR_PPCE5500)
    {
      if (TARGET_ALTIVEC)
	error ("AltiVec not supported in this target");
      if (TARGET_SPE)
	error ("SPE not supported in this target");
    }
  if (rs6000_cpu == PROCESSOR_PPCE6500)
    {
      if (TARGET_SPE)
	error ("SPE not supported in this target");
    }

  /* Disable Cell microcode if we are optimizing for the Cell
     and not optimizing for size.  */
  if (rs6000_gen_cell_microcode == -1)
    rs6000_gen_cell_microcode = !(rs6000_cpu == PROCESSOR_CELL
                                  && !optimize_size);

  /* If we are optimizing big endian systems for space and it's OK to
     use instructions that would be microcoded on the Cell, use the
     load/store multiple and string instructions.  */
  if (BYTES_BIG_ENDIAN && optimize_size && rs6000_gen_cell_microcode)
    rs6000_isa_flags |= ~rs6000_isa_flags_explicit & (OPTION_MASK_MULTIPLE
						      | OPTION_MASK_STRING);

  /* Don't allow -mmultiple or -mstring on little endian systems
     unless the cpu is a 750, because the hardware doesn't support the
     instructions used in little endian mode, and causes an alignment
     trap.  The 750 does not cause an alignment trap (except when the
     target is unaligned).  */

  if (!BYTES_BIG_ENDIAN && rs6000_cpu != PROCESSOR_PPC750)
    {
      if (TARGET_MULTIPLE)
	{
	  rs6000_isa_flags &= ~OPTION_MASK_MULTIPLE;
	  if ((rs6000_isa_flags_explicit & OPTION_MASK_MULTIPLE) != 0)
	    warning (0, "-mmultiple is not supported on little endian systems");
	}

      if (TARGET_STRING)
	{
	  rs6000_isa_flags &= ~OPTION_MASK_STRING;
	  if ((rs6000_isa_flags_explicit & OPTION_MASK_STRING) != 0)
	    warning (0, "-mstring is not supported on little endian systems");
	}
    }

  /* If little-endian, default to -mstrict-align on older processors.
     Testing for htm matches power8 and later.  */
  if (!BYTES_BIG_ENDIAN
      && !(processor_target_table[tune_index].target_enable & OPTION_MASK_HTM))
    rs6000_isa_flags |= ~rs6000_isa_flags_explicit & OPTION_MASK_STRICT_ALIGN;

  /* -maltivec={le,be} implies -maltivec.  */
  if (rs6000_altivec_element_order != 0)
    rs6000_isa_flags |= OPTION_MASK_ALTIVEC;

  /* Disallow -maltivec=le in big endian mode for now.  This is not
     known to be useful for anyone.  */
  if (BYTES_BIG_ENDIAN && rs6000_altivec_element_order == 1)
    {
      warning (0, N_("-maltivec=le not allowed for big-endian targets"));
      rs6000_altivec_element_order = 0;
    }

  /* Add some warnings for VSX.  */
  if (TARGET_VSX)
    {
      const char *msg = NULL;
      if (!TARGET_HARD_FLOAT || !TARGET_FPRS
	  || !TARGET_SINGLE_FLOAT || !TARGET_DOUBLE_FLOAT)
	{
	  if (rs6000_isa_flags_explicit & OPTION_MASK_VSX)
	    msg = N_("-mvsx requires hardware floating point");
	  else
	    {
	      rs6000_isa_flags &= ~ OPTION_MASK_VSX;
	      rs6000_isa_flags_explicit |= OPTION_MASK_VSX;
	    }
	}
      else if (TARGET_PAIRED_FLOAT)
	msg = N_("-mvsx and -mpaired are incompatible");
      else if (TARGET_AVOID_XFORM > 0)
	msg = N_("-mvsx needs indexed addressing");
      else if (!TARGET_ALTIVEC && (rs6000_isa_flags_explicit
				   & OPTION_MASK_ALTIVEC))
        {
	  if (rs6000_isa_flags_explicit & OPTION_MASK_VSX)
	    msg = N_("-mvsx and -mno-altivec are incompatible");
	  else
	    msg = N_("-mno-altivec disables vsx");
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

  /* For the newer switches (vsx, dfp, etc.) set some of the older options,
     unless the user explicitly used the -mno-<option> to disable the code.  */
  if (TARGET_P8_VECTOR || TARGET_DIRECT_MOVE || TARGET_CRYPTO)
    rs6000_isa_flags |= (ISA_2_7_MASKS_SERVER & ~rs6000_isa_flags_explicit);
  else if (TARGET_VSX)
    rs6000_isa_flags |= (ISA_2_6_MASKS_SERVER & ~rs6000_isa_flags_explicit);
  else if (TARGET_POPCNTD)
    rs6000_isa_flags |= (ISA_2_6_MASKS_EMBEDDED & ~rs6000_isa_flags_explicit);
  else if (TARGET_DFP)
    rs6000_isa_flags |= (ISA_2_5_MASKS_SERVER & ~rs6000_isa_flags_explicit);
  else if (TARGET_CMPB)
    rs6000_isa_flags |= (ISA_2_5_MASKS_EMBEDDED & ~rs6000_isa_flags_explicit);
  else if (TARGET_FPRND)
    rs6000_isa_flags |= (ISA_2_4_MASKS & ~rs6000_isa_flags_explicit);
  else if (TARGET_POPCNTB)
    rs6000_isa_flags |= (ISA_2_2_MASKS & ~rs6000_isa_flags_explicit);
  else if (TARGET_ALTIVEC)
    rs6000_isa_flags |= (OPTION_MASK_PPC_GFXOPT & ~rs6000_isa_flags_explicit);

  if (TARGET_CRYPTO && !TARGET_ALTIVEC)
    {
      if (rs6000_isa_flags_explicit & OPTION_MASK_CRYPTO)
	error ("-mcrypto requires -maltivec");
      rs6000_isa_flags &= ~OPTION_MASK_CRYPTO;
    }

  if (TARGET_DIRECT_MOVE && !TARGET_VSX)
    {
      if (rs6000_isa_flags_explicit & OPTION_MASK_DIRECT_MOVE)
	error ("-mdirect-move requires -mvsx");
      rs6000_isa_flags &= ~OPTION_MASK_DIRECT_MOVE;
    }

  if (TARGET_P8_VECTOR && !TARGET_ALTIVEC)
    {
      if (rs6000_isa_flags_explicit & OPTION_MASK_P8_VECTOR)
	error ("-mpower8-vector requires -maltivec");
      rs6000_isa_flags &= ~OPTION_MASK_P8_VECTOR;
    }

  if (TARGET_P8_VECTOR && !TARGET_VSX)
    {
      if (rs6000_isa_flags_explicit & OPTION_MASK_P8_VECTOR)
	error ("-mpower8-vector requires -mvsx");
      rs6000_isa_flags &= ~OPTION_MASK_P8_VECTOR;
    }

  if (TARGET_VSX_TIMODE && !TARGET_VSX)
    {
      if (rs6000_isa_flags_explicit & OPTION_MASK_VSX_TIMODE)
	error ("-mvsx-timode requires -mvsx");
      rs6000_isa_flags &= ~OPTION_MASK_VSX_TIMODE;
    }

  /* The quad memory instructions only works in 64-bit mode. In 32-bit mode,
     silently turn off quad memory mode.  */
  if ((TARGET_QUAD_MEMORY || TARGET_QUAD_MEMORY_ATOMIC) && !TARGET_POWERPC64)
    {
      if ((rs6000_isa_flags_explicit & OPTION_MASK_QUAD_MEMORY) != 0)
	warning (0, N_("-mquad-memory requires 64-bit mode"));

      if ((rs6000_isa_flags_explicit & OPTION_MASK_QUAD_MEMORY_ATOMIC) != 0)
	warning (0, N_("-mquad-memory-atomic requires 64-bit mode"));

      rs6000_isa_flags &= ~(OPTION_MASK_QUAD_MEMORY
			    | OPTION_MASK_QUAD_MEMORY_ATOMIC);
    }

  /* Non-atomic quad memory load/store are disabled for little endian, since
     the words are reversed, but atomic operations can still be done by
     swapping the words.  */
  if (TARGET_QUAD_MEMORY && !WORDS_BIG_ENDIAN)
    {
      if ((rs6000_isa_flags_explicit & OPTION_MASK_QUAD_MEMORY) != 0)
	warning (0, N_("-mquad-memory is not available in little endian mode"));

      rs6000_isa_flags &= ~OPTION_MASK_QUAD_MEMORY;
    }

  /* Assume if the user asked for normal quad memory instructions, they want
     the atomic versions as well, unless they explicity told us not to use quad
     word atomic instructions.  */
  if (TARGET_QUAD_MEMORY
      && !TARGET_QUAD_MEMORY_ATOMIC
      && ((rs6000_isa_flags_explicit & OPTION_MASK_QUAD_MEMORY_ATOMIC) == 0))
    rs6000_isa_flags |= OPTION_MASK_QUAD_MEMORY_ATOMIC;

  /* Enable power8 fusion if we are tuning for power8, even if we aren't
     generating power8 instructions.  */
  if (!(rs6000_isa_flags_explicit & OPTION_MASK_P8_FUSION))
    rs6000_isa_flags |= (processor_target_table[tune_index].target_enable
			 & OPTION_MASK_P8_FUSION);

  /* Power8 does not fuse sign extended loads with the addis.  If we are
     optimizing at high levels for speed, convert a sign extended load into a
     zero extending load, and an explicit sign extension.  */
  if (TARGET_P8_FUSION
      && !(rs6000_isa_flags_explicit & OPTION_MASK_P8_FUSION_SIGN)
      && optimize_function_for_speed_p (cfun)
      && optimize >= 3)
    rs6000_isa_flags |= OPTION_MASK_P8_FUSION_SIGN;

  if (TARGET_DEBUG_REG || TARGET_DEBUG_TARGET)
    rs6000_print_isa_options (stderr, 0, "after defaults", rs6000_isa_flags);

  /* E500mc does "better" if we inline more aggressively.  Respect the
     user's opinion, though.  */
  if (rs6000_block_move_inline_limit == 0
      && (rs6000_cpu == PROCESSOR_PPCE500MC
	  || rs6000_cpu == PROCESSOR_PPCE500MC64
	  || rs6000_cpu == PROCESSOR_PPCE5500
	  || rs6000_cpu == PROCESSOR_PPCE6500))
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
	  rs6000_secondary_memory_needed_ptr
	    = rs6000_debug_secondary_memory_needed;
	  rs6000_cannot_change_mode_class_ptr
	    = rs6000_debug_cannot_change_mode_class;
	  rs6000_preferred_reload_class_ptr
	    = rs6000_debug_preferred_reload_class;
	  rs6000_legitimize_reload_address_ptr
	    = rs6000_debug_legitimize_reload_address;
	  rs6000_mode_dependent_address_ptr
	    = rs6000_debug_mode_dependent_address;
	}

      if (rs6000_veclibabi_name)
	{
	  if (strcmp (rs6000_veclibabi_name, "mass") == 0)
	    rs6000_veclib_handler = rs6000_builtin_vectorized_libmass;
	  else
	    {
	      error ("unknown vectorization library ABI type (%s) for "
		     "-mveclibabi= switch", rs6000_veclibabi_name);
	      ret = false;
	    }
	}
    }

  if (!global_options_set.x_rs6000_long_double_type_size)
    {
      if (main_target_opt != NULL
	  && (main_target_opt->x_rs6000_long_double_type_size
	      != RS6000_DEFAULT_LONG_DOUBLE_SIZE))
	error ("target attribute or pragma changes long double size");
      else
	rs6000_long_double_type_size = RS6000_DEFAULT_LONG_DOUBLE_SIZE;
    }

#if !defined (POWERPC_LINUX) && !defined (POWERPC_FREEBSD)
  if (!global_options_set.x_rs6000_ieeequad)
    rs6000_ieeequad = 1;
#endif

  /* Disable VSX and Altivec silently if the user switched cpus to power7 in a
     target attribute or pragma which automatically enables both options,
     unless the altivec ABI was set.  This is set by default for 64-bit, but
     not for 32-bit.  */
  if (main_target_opt != NULL && !main_target_opt->x_rs6000_altivec_abi)
    rs6000_isa_flags &= ~((OPTION_MASK_VSX | OPTION_MASK_ALTIVEC)
			  & ~rs6000_isa_flags_explicit);

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

  if (TARGET_DEBUG_REG || TARGET_DEBUG_TARGET)
    rs6000_print_isa_options (stderr, 0, "after subtarget", rs6000_isa_flags);

  /* For the E500 family of cores, reset the single/double FP flags to let us
     check that they remain constant across attributes or pragmas.  Also,
     clear a possible request for string instructions, not supported and which
     we might have silently queried above for -Os. 

     For other families, clear ISEL in case it was set implicitly.
  */

  switch (rs6000_cpu)
    {
    case PROCESSOR_PPC8540:
    case PROCESSOR_PPC8548:
    case PROCESSOR_PPCE500MC:
    case PROCESSOR_PPCE500MC64:
    case PROCESSOR_PPCE5500:
    case PROCESSOR_PPCE6500:

      rs6000_single_float = TARGET_E500_SINGLE || TARGET_E500_DOUBLE;
      rs6000_double_float = TARGET_E500_DOUBLE;

      rs6000_isa_flags &= ~OPTION_MASK_STRING;

      break;

    default:

      if (have_cpu && !(rs6000_isa_flags_explicit & OPTION_MASK_ISEL))
	rs6000_isa_flags &= ~OPTION_MASK_ISEL;

      break;
    }

  if (main_target_opt)
    {
      if (main_target_opt->x_rs6000_single_float != rs6000_single_float)
	error ("target attribute or pragma changes single precision floating "
	       "point");
      if (main_target_opt->x_rs6000_double_float != rs6000_double_float)
	error ("target attribute or pragma changes double precision floating "
	       "point");
    }

  /* Detect invalid option combinations with E500.  */
  CHECK_E500_OPTIONS;

  rs6000_always_hint = (rs6000_cpu != PROCESSOR_POWER4
			&& rs6000_cpu != PROCESSOR_POWER5
			&& rs6000_cpu != PROCESSOR_POWER6
			&& rs6000_cpu != PROCESSOR_POWER7
			&& rs6000_cpu != PROCESSOR_POWER8
			&& rs6000_cpu != PROCESSOR_PPCA2
			&& rs6000_cpu != PROCESSOR_CELL
			&& rs6000_cpu != PROCESSOR_PPC476);
  rs6000_sched_groups = (rs6000_cpu == PROCESSOR_POWER4
			 || rs6000_cpu == PROCESSOR_POWER5
			 || rs6000_cpu == PROCESSOR_POWER7
			 || rs6000_cpu == PROCESSOR_POWER8);
  rs6000_align_branch_targets = (rs6000_cpu == PROCESSOR_POWER4
				 || rs6000_cpu == PROCESSOR_POWER5
				 || rs6000_cpu == PROCESSOR_POWER6
				 || rs6000_cpu == PROCESSOR_POWER7
				 || rs6000_cpu == PROCESSOR_POWER8
				 || rs6000_cpu == PROCESSOR_PPCE500MC
				 || rs6000_cpu == PROCESSOR_PPCE500MC64
				 || rs6000_cpu == PROCESSOR_PPCE5500
				 || rs6000_cpu == PROCESSOR_PPCE6500);

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

      if (TARGET_TOC)
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
	  if (rs6000_cpu == PROCESSOR_TITAN
	      || rs6000_cpu == PROCESSOR_CELL)
	    {
	      if (align_functions <= 0)
		align_functions = 8;
	      if (align_jumps <= 0)
		align_jumps = 8;
	      if (align_loops <= 0)
		align_loops = 8;
	    }
	  if (rs6000_align_branch_targets)
	    {
	      if (align_functions <= 0)
		align_functions = 16;
	      if (align_jumps <= 0)
		align_jumps = 16;
	      if (align_loops <= 0)
		{
		  can_override_loop_align = 1;
		  align_loops = 16;
		}
	    }
	  if (align_jumps_max_skip <= 0)
	    align_jumps_max_skip = 15;
	  if (align_loops_max_skip <= 0)
	    align_loops_max_skip = 15;
	}

      /* Arrange to save and restore machine status around nested functions.  */
      init_machine_status = rs6000_init_machine_status;

      /* We should always be splitting complex arguments, but we can't break
	 Linux and Darwin ABIs at the moment.  For now, only AIX is fixed.  */
      if (DEFAULT_ABI == ABI_V4 || DEFAULT_ABI == ABI_DARWIN)
	targetm.calls.split_complex_arg = NULL;
    }

  /* Initialize rs6000_cost with the appropriate target costs.  */
  if (optimize_size)
    rs6000_cost = TARGET_POWERPC64 ? &size64_cost : &size32_cost;
  else
    switch (rs6000_cpu)
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

      case PROCESSOR_PPCA2:
	rs6000_cost = &ppca2_cost;
	break;

      default:
	gcc_unreachable ();
      }

  if (global_init_p)
    {
      maybe_set_param_value (PARAM_SIMULTANEOUS_PREFETCHES,
			     rs6000_cost->simultaneous_prefetches,
			     global_options.x_param_values,
			     global_options_set.x_param_values);
      maybe_set_param_value (PARAM_L1_CACHE_SIZE, rs6000_cost->l1_cache_size,
			     global_options.x_param_values,
			     global_options_set.x_param_values);
      maybe_set_param_value (PARAM_L1_CACHE_LINE_SIZE,
			     rs6000_cost->cache_line_size,
			     global_options.x_param_values,
			     global_options_set.x_param_values);
      maybe_set_param_value (PARAM_L2_CACHE_SIZE, rs6000_cost->l2_cache_size,
			     global_options.x_param_values,
			     global_options_set.x_param_values);

      /* Increase loop peeling limits based on performance analysis. */
      maybe_set_param_value (PARAM_MAX_PEELED_INSNS, 400,
			     global_options.x_param_values,
			     global_options_set.x_param_values);
      maybe_set_param_value (PARAM_MAX_COMPLETELY_PEELED_INSNS, 400,
			     global_options.x_param_values,
			     global_options_set.x_param_values);

      /* If using typedef char *va_list, signal that
	 __builtin_va_start (&ap, 0) can be optimized to
	 ap = __builtin_next_arg (0).  */
      if (DEFAULT_ABI != ABI_V4)
	targetm.expand_builtin_va_start = NULL;
    }

  /* Set up single/double float flags.  
     If TARGET_HARD_FLOAT is set, but neither single or double is set, 
     then set both flags. */
  if (TARGET_HARD_FLOAT && TARGET_FPRS 
      && rs6000_single_float == 0 && rs6000_double_float == 0)
    rs6000_single_float = rs6000_double_float = 1;

  /* If not explicitly specified via option, decide whether to generate indexed
     load/store instructions.  */
  if (TARGET_AVOID_XFORM == -1)
    /* Avoid indexed addressing when targeting Power6 in order to avoid the
     DERAT mispredict penalty.  However the LVE and STVE altivec instructions
     need indexed accesses and the type used is the scalar type of the element
     being loaded or stored.  */
    TARGET_AVOID_XFORM = (rs6000_cpu == PROCESSOR_POWER6 && TARGET_CMPB
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
		  error ("unknown option for -mrecip=%s", q);
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
     of bits, and some options like SPE and PAIRED are no longer in
     target_flags.  */
  rs6000_builtin_mask = rs6000_builtin_mask_calculate ();
  if (TARGET_DEBUG_BUILTIN || TARGET_DEBUG_TARGET)
    {
      fprintf (stderr,
	       "new builtin mask = " HOST_WIDE_INT_PRINT_HEX ", ",
	       rs6000_builtin_mask);
      rs6000_print_builtin_options (stderr, 0, NULL, rs6000_builtin_mask);
    }

  /* Initialize all of the registers.  */
  rs6000_init_hard_regno_mode_ok (global_init_p);

  /* Save the initial options in case the user does function specific options */
  if (global_init_p)
    target_option_default_node = target_option_current_node
      = build_target_option_node (&global_options);

  /* If not explicitly specified via option, decide whether to generate the
     extra blr's required to preserve the link stack on some cpus (eg, 476).  */
  if (TARGET_LINK_STACK == -1)
    SET_TARGET_LINK_STACK (rs6000_cpu == PROCESSOR_PPC476 && flag_pic);

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
  if (TARGET_ALTIVEC || TARGET_VSX)
    return altivec_builtin_mask_for_load;
  else
    return 0;
}

/* Implement LOOP_ALIGN. */
int
rs6000_loop_align (rtx label)
{
  basic_block bb;
  int ninsns;

  /* Don't override loop alignment if -falign-loops was specified. */
  if (!can_override_loop_align)
    return align_loops_log;

  bb = BLOCK_FOR_INSN (label);
  ninsns = num_loop_insns(bb->loop_father);

  /* Align small loops to 32 bytes to fit in an icache sector, otherwise return default. */
  if (ninsns > 4 && ninsns <= 8
      && (rs6000_cpu == PROCESSOR_POWER4
	  || rs6000_cpu == PROCESSOR_POWER5
	  || rs6000_cpu == PROCESSOR_POWER6
	  || rs6000_cpu == PROCESSOR_POWER7
	  || rs6000_cpu == PROCESSOR_POWER8))
    return 5;
  else
    return align_loops_log;
}

/* Implement TARGET_LOOP_ALIGN_MAX_SKIP. */
static int
rs6000_loop_align_max_skip (rtx label)
{
  return (1 << rs6000_loop_align (label)) - 1;
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
rs6000_builtin_support_vector_misalignment (enum machine_mode mode,
					    const_tree type,
					    int misalignment,
					    bool is_packed)
{
  if (TARGET_VSX)
    {
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
      case scalar_load:
      case scalar_store:
      case vector_stmt:
      case vector_load:
      case vector_store:
      case vec_to_scalar:
      case scalar_to_vec:
      case cond_branch_not_taken:
        return 1;

      case vec_perm:
	if (TARGET_VSX)
	  return 3;
	else
	  return 1;

      case vec_promote_demote:
        if (TARGET_VSX)
          return 4;
        else
          return 1;

      case cond_branch_taken:
        return 3;

      case unaligned_load:
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
                      return 22;

                    default:
                      gcc_unreachable ();
                  }
              }
          }

        if (TARGET_ALTIVEC)
          /* Misaligned loads are not supported.  */
          gcc_unreachable ();

        return 2;

      case unaligned_store:
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
	elements = TYPE_VECTOR_SUBPARTS (vectype);
	elem_type = TREE_TYPE (vectype);
	/* 32-bit vectors loaded into registers are stored as double
	   precision, so we need n/2 converts in addition to the usual
	   n/2 merges to construct a vector of short floats from them.  */
	if (SCALAR_FLOAT_TYPE_P (elem_type)
	    && TYPE_PRECISION (elem_type) == 32)
	  return elements + 1;
	else
	  return elements / 2 + 1;

      default:
        gcc_unreachable ();
    }
}

/* Implement targetm.vectorize.preferred_simd_mode.  */

static enum machine_mode
rs6000_preferred_simd_mode (enum machine_mode mode)
{
  if (TARGET_VSX)
    switch (mode)
      {
      case DFmode:
	return V2DFmode;
      default:;
      }
  if (TARGET_ALTIVEC || TARGET_VSX)
    switch (mode)
      {
      case SFmode:
	return V4SFmode;
      case DImode:
	return V2DImode;
      case SImode:
	return V4SImode;
      case HImode:
	return V8HImode;
      case QImode:
	return V16QImode;
      default:;
      }
  if (TARGET_SPE)
    switch (mode)
      {
      case SFmode:
	return V2SFmode;
      case SImode:
	return V2SImode;
      default:;
      }
  if (TARGET_PAIRED_FLOAT
      && mode == SFmode)
    return V2SFmode;
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
  int vec_cost = data->cost[vect_body], not_vec_cost = 0;
  int i, density_pct;

  for (i = 0; i < nbbs; i++)
    {
      basic_block bb = bbs[i];
      gimple_stmt_iterator gsi;

      for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
	{
	  gimple stmt = gsi_stmt (gsi);
	  stmt_vec_info stmt_info = vinfo_for_stmt (stmt);

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

static void *
rs6000_init_cost (struct loop *loop_info)
{
  rs6000_cost_data *data = XNEW (struct _rs6000_cost_data);
  data->loop_info = loop_info;
  data->cost[vect_prologue] = 0;
  data->cost[vect_body]     = 0;
  data->cost[vect_epilogue] = 0;
  return data;
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
      /* Statements in an inner loop relative to the loop being
	 vectorized are weighted more heavily.  The value here is
	 arbitrary and could potentially be improved with analysis.  */
      if (where == vect_body && stmt_info && stmt_in_inner_loop_p (stmt_info))
	count *= 50;  /* FIXME.  */

      retval = (unsigned) (count * stmt_cost);
      cost_data->cost[where] += retval;
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

/* Handler for the Mathematical Acceleration Subsystem (mass) interface to a
   library with vectorized intrinsics.  */

static tree
rs6000_builtin_vectorized_libmass (tree fndecl, tree type_out, tree type_in)
{
  char name[32];
  const char *suffix = NULL;
  tree fntype, new_fndecl, bdecl = NULL_TREE;
  int n_args = 1;
  const char *bname;
  enum machine_mode el_mode, in_mode;
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

  if (DECL_BUILT_IN_CLASS (fndecl) == BUILT_IN_NORMAL)
    {
      enum built_in_function fn = DECL_FUNCTION_CODE (fndecl);
      switch (fn)
	{
	case BUILT_IN_ATAN2:
	case BUILT_IN_HYPOT:
	case BUILT_IN_POW:
	  n_args = 2;
	  /* fall through */

	case BUILT_IN_ACOS:
	case BUILT_IN_ACOSH:
	case BUILT_IN_ASIN:
	case BUILT_IN_ASINH:
	case BUILT_IN_ATAN:
	case BUILT_IN_ATANH:
	case BUILT_IN_CBRT:
	case BUILT_IN_COS:
	case BUILT_IN_COSH:
	case BUILT_IN_ERF:
	case BUILT_IN_ERFC:
	case BUILT_IN_EXP2:
	case BUILT_IN_EXP:
	case BUILT_IN_EXPM1:
	case BUILT_IN_LGAMMA:
	case BUILT_IN_LOG10:
	case BUILT_IN_LOG1P:
	case BUILT_IN_LOG2:
	case BUILT_IN_LOG:
	case BUILT_IN_SIN:
	case BUILT_IN_SINH:
	case BUILT_IN_SQRT:
	case BUILT_IN_TAN:
	case BUILT_IN_TANH:
	  bdecl = builtin_decl_implicit (fn);
	  suffix = "d2";				/* pow -> powd2 */
	  if (el_mode != DFmode
	      || n != 2
	      || !bdecl)
	    return NULL_TREE;
	  break;

	case BUILT_IN_ATAN2F:
	case BUILT_IN_HYPOTF:
	case BUILT_IN_POWF:
	  n_args = 2;
	  /* fall through */

	case BUILT_IN_ACOSF:
	case BUILT_IN_ACOSHF:
	case BUILT_IN_ASINF:
	case BUILT_IN_ASINHF:
	case BUILT_IN_ATANF:
	case BUILT_IN_ATANHF:
	case BUILT_IN_CBRTF:
	case BUILT_IN_COSF:
	case BUILT_IN_COSHF:
	case BUILT_IN_ERFF:
	case BUILT_IN_ERFCF:
	case BUILT_IN_EXP2F:
	case BUILT_IN_EXPF:
	case BUILT_IN_EXPM1F:
	case BUILT_IN_LGAMMAF:
	case BUILT_IN_LOG10F:
	case BUILT_IN_LOG1PF:
	case BUILT_IN_LOG2F:
	case BUILT_IN_LOGF:
	case BUILT_IN_SINF:
	case BUILT_IN_SINHF:
	case BUILT_IN_SQRTF:
	case BUILT_IN_TANF:
	case BUILT_IN_TANHF:
	  bdecl = builtin_decl_implicit (fn);
	  suffix = "4";					/* powf -> powf4 */
	  if (el_mode != SFmode
	      || n != 4
	      || !bdecl)
	    return NULL_TREE;
	  break;

	default:
	  return NULL_TREE;
	}
    }
  else
    return NULL_TREE;

  gcc_assert (suffix != NULL);
  bname = IDENTIFIER_POINTER (DECL_NAME (bdecl));
  if (!bname)
    return NULL_TREE;

  strcpy (name, bname + sizeof ("__builtin_") - 1);
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
rs6000_builtin_vectorized_function (tree fndecl, tree type_out,
				    tree type_in)
{
  enum machine_mode in_mode, out_mode;
  int in_n, out_n;

  if (TARGET_DEBUG_BUILTIN)
    fprintf (stderr, "rs6000_builtin_vectorized_function (%s, %s, %s)\n",
	     IDENTIFIER_POINTER (DECL_NAME (fndecl)),
	     GET_MODE_NAME (TYPE_MODE (type_out)),
	     GET_MODE_NAME (TYPE_MODE (type_in)));

  if (TREE_CODE (type_out) != VECTOR_TYPE
      || TREE_CODE (type_in) != VECTOR_TYPE
      || !TARGET_VECTORIZE_BUILTINS)
    return NULL_TREE;

  out_mode = TYPE_MODE (TREE_TYPE (type_out));
  out_n = TYPE_VECTOR_SUBPARTS (type_out);
  in_mode = TYPE_MODE (TREE_TYPE (type_in));
  in_n = TYPE_VECTOR_SUBPARTS (type_in);

  if (DECL_BUILT_IN_CLASS (fndecl) == BUILT_IN_NORMAL)
    {
      enum built_in_function fn = DECL_FUNCTION_CODE (fndecl);
      switch (fn)
	{
	case BUILT_IN_CLZIMAX:
	case BUILT_IN_CLZLL:
	case BUILT_IN_CLZL:
	case BUILT_IN_CLZ:
	  if (TARGET_P8_VECTOR && in_mode == out_mode && out_n == in_n)
	    {
	      if (out_mode == QImode && out_n == 16)
		return rs6000_builtin_decls[P8V_BUILTIN_VCLZB];
	      else if (out_mode == HImode && out_n == 8)
		return rs6000_builtin_decls[P8V_BUILTIN_VCLZH];
	      else if (out_mode == SImode && out_n == 4)
		return rs6000_builtin_decls[P8V_BUILTIN_VCLZW];
	      else if (out_mode == DImode && out_n == 2)
		return rs6000_builtin_decls[P8V_BUILTIN_VCLZD];
	    }
	  break;
	case BUILT_IN_COPYSIGN:
	  if (VECTOR_UNIT_VSX_P (V2DFmode)
	      && out_mode == DFmode && out_n == 2
	      && in_mode == DFmode && in_n == 2)
	    return rs6000_builtin_decls[VSX_BUILTIN_CPSGNDP];
	  break;
	case BUILT_IN_COPYSIGNF:
	  if (out_mode != SFmode || out_n != 4
	      || in_mode != SFmode || in_n != 4)
	    break;
	  if (VECTOR_UNIT_VSX_P (V4SFmode))
	    return rs6000_builtin_decls[VSX_BUILTIN_CPSGNSP];
	  if (VECTOR_UNIT_ALTIVEC_P (V4SFmode))
	    return rs6000_builtin_decls[ALTIVEC_BUILTIN_COPYSIGN_V4SF];
	  break;
	case BUILT_IN_POPCOUNTIMAX:
	case BUILT_IN_POPCOUNTLL:
	case BUILT_IN_POPCOUNTL:
	case BUILT_IN_POPCOUNT:
	  if (TARGET_P8_VECTOR && in_mode == out_mode && out_n == in_n)
	    {
	      if (out_mode == QImode && out_n == 16)
		return rs6000_builtin_decls[P8V_BUILTIN_VPOPCNTB];
	      else if (out_mode == HImode && out_n == 8)
		return rs6000_builtin_decls[P8V_BUILTIN_VPOPCNTH];
	      else if (out_mode == SImode && out_n == 4)
		return rs6000_builtin_decls[P8V_BUILTIN_VPOPCNTW];
	      else if (out_mode == DImode && out_n == 2)
		return rs6000_builtin_decls[P8V_BUILTIN_VPOPCNTD];
	    }
	  break;
	case BUILT_IN_SQRT:
	  if (VECTOR_UNIT_VSX_P (V2DFmode)
	      && out_mode == DFmode && out_n == 2
	      && in_mode == DFmode && in_n == 2)
	    return rs6000_builtin_decls[VSX_BUILTIN_XVSQRTDP];
	  break;
	case BUILT_IN_SQRTF:
	  if (VECTOR_UNIT_VSX_P (V4SFmode)
	      && out_mode == SFmode && out_n == 4
	      && in_mode == SFmode && in_n == 4)
	    return rs6000_builtin_decls[VSX_BUILTIN_XVSQRTSP];
	  break;
	case BUILT_IN_CEIL:
	  if (VECTOR_UNIT_VSX_P (V2DFmode)
	      && out_mode == DFmode && out_n == 2
	      && in_mode == DFmode && in_n == 2)
	    return rs6000_builtin_decls[VSX_BUILTIN_XVRDPIP];
	  break;
	case BUILT_IN_CEILF:
	  if (out_mode != SFmode || out_n != 4
	      || in_mode != SFmode || in_n != 4)
	    break;
	  if (VECTOR_UNIT_VSX_P (V4SFmode))
	    return rs6000_builtin_decls[VSX_BUILTIN_XVRSPIP];
	  if (VECTOR_UNIT_ALTIVEC_P (V4SFmode))
	    return rs6000_builtin_decls[ALTIVEC_BUILTIN_VRFIP];
	  break;
	case BUILT_IN_FLOOR:
	  if (VECTOR_UNIT_VSX_P (V2DFmode)
	      && out_mode == DFmode && out_n == 2
	      && in_mode == DFmode && in_n == 2)
	    return rs6000_builtin_decls[VSX_BUILTIN_XVRDPIM];
	  break;
	case BUILT_IN_FLOORF:
	  if (out_mode != SFmode || out_n != 4
	      || in_mode != SFmode || in_n != 4)
	    break;
	  if (VECTOR_UNIT_VSX_P (V4SFmode))
	    return rs6000_builtin_decls[VSX_BUILTIN_XVRSPIM];
	  if (VECTOR_UNIT_ALTIVEC_P (V4SFmode))
	    return rs6000_builtin_decls[ALTIVEC_BUILTIN_VRFIM];
	  break;
	case BUILT_IN_FMA:
	  if (VECTOR_UNIT_VSX_P (V2DFmode)
	      && out_mode == DFmode && out_n == 2
	      && in_mode == DFmode && in_n == 2)
	    return rs6000_builtin_decls[VSX_BUILTIN_XVMADDDP];
	  break;
	case BUILT_IN_FMAF:
	  if (VECTOR_UNIT_VSX_P (V4SFmode)
	      && out_mode == SFmode && out_n == 4
	      && in_mode == SFmode && in_n == 4)
	    return rs6000_builtin_decls[VSX_BUILTIN_XVMADDSP];
	  else if (VECTOR_UNIT_ALTIVEC_P (V4SFmode)
	      && out_mode == SFmode && out_n == 4
	      && in_mode == SFmode && in_n == 4)
	    return rs6000_builtin_decls[ALTIVEC_BUILTIN_VMADDFP];
	  break;
	case BUILT_IN_TRUNC:
	  if (VECTOR_UNIT_VSX_P (V2DFmode)
	      && out_mode == DFmode && out_n == 2
	      && in_mode == DFmode && in_n == 2)
	    return rs6000_builtin_decls[VSX_BUILTIN_XVRDPIZ];
	  break;
	case BUILT_IN_TRUNCF:
	  if (out_mode != SFmode || out_n != 4
	      || in_mode != SFmode || in_n != 4)
	    break;
	  if (VECTOR_UNIT_VSX_P (V4SFmode))
	    return rs6000_builtin_decls[VSX_BUILTIN_XVRSPIZ];
	  if (VECTOR_UNIT_ALTIVEC_P (V4SFmode))
	    return rs6000_builtin_decls[ALTIVEC_BUILTIN_VRFIZ];
	  break;
	case BUILT_IN_NEARBYINT:
	  if (VECTOR_UNIT_VSX_P (V2DFmode)
	      && flag_unsafe_math_optimizations
	      && out_mode == DFmode && out_n == 2
	      && in_mode == DFmode && in_n == 2)
	    return rs6000_builtin_decls[VSX_BUILTIN_XVRDPI];
	  break;
	case BUILT_IN_NEARBYINTF:
	  if (VECTOR_UNIT_VSX_P (V4SFmode)
	      && flag_unsafe_math_optimizations
	      && out_mode == SFmode && out_n == 4
	      && in_mode == SFmode && in_n == 4)
	    return rs6000_builtin_decls[VSX_BUILTIN_XVRSPI];
	  break;
	case BUILT_IN_RINT:
	  if (VECTOR_UNIT_VSX_P (V2DFmode)
	      && !flag_trapping_math
	      && out_mode == DFmode && out_n == 2
	      && in_mode == DFmode && in_n == 2)
	    return rs6000_builtin_decls[VSX_BUILTIN_XVRDPIC];
	  break;
	case BUILT_IN_RINTF:
	  if (VECTOR_UNIT_VSX_P (V4SFmode)
	      && !flag_trapping_math
	      && out_mode == SFmode && out_n == 4
	      && in_mode == SFmode && in_n == 4)
	    return rs6000_builtin_decls[VSX_BUILTIN_XVRSPIC];
	  break;
	default:
	  break;
	}
    }

  else if (DECL_BUILT_IN_CLASS (fndecl) == BUILT_IN_MD)
    {
      enum rs6000_builtins fn
	= (enum rs6000_builtins)DECL_FUNCTION_CODE (fndecl);
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
    }

  /* Generate calls to libmass if appropriate.  */
  if (rs6000_veclib_handler)
    return rs6000_veclib_handler (fndecl, type_out, type_in);

  return NULL_TREE;
}

/* Default CPU string for rs6000*_file_start functions.  */
static const char *rs6000_default_cpu;

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

  if (DEFAULT_ABI == ABI_ELFv2)
    fprintf (file, "\t.abiversion 2\n");

  if (DEFAULT_ABI == ABI_AIX || DEFAULT_ABI == ABI_ELFv2
      || (TARGET_ELF && flag_pic == 2))
    {
      switch_to_section (toc_section);
      switch_to_section (text_section);
    }
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
	  && info->vrsave_mask == 0
	  && ! info->push_p)
	return 1;
    }

  return 0;
}

/* Return the number of instructions it takes to form a constant in an
   integer register.  */

int
num_insns_constant_wide (HOST_WIDE_INT value)
{
  /* signed constant loadable with addi */
  if ((unsigned HOST_WIDE_INT) (value + 0x8000) < 0x10000)
    return 1;

  /* constant loadable with addis */
  else if ((value & 0xffff) == 0
	   && (value >> 31 == -1 || value >> 31 == 0))
    return 1;

  else if (TARGET_POWERPC64)
    {
      HOST_WIDE_INT low  = ((value & 0xffffffff) ^ 0x80000000) - 0x80000000;
      HOST_WIDE_INT high = value >> 31;

      if (high == 0 || high == -1)
	return 2;

      high >>= 1;

      if (low == 0)
	return num_insns_constant_wide (high) + 1;
      else if (high == 0)
	return num_insns_constant_wide (low) + 1;
      else
	return (num_insns_constant_wide (high)
		+ num_insns_constant_wide (low) + 1);
    }

  else
    return 2;
}

int
num_insns_constant (rtx op, enum machine_mode mode)
{
  HOST_WIDE_INT low, high;

  switch (GET_CODE (op))
    {
    case CONST_INT:
      if ((INTVAL (op) >> 31) != 0 && (INTVAL (op) >> 31) != -1
	  && mask64_operand (op, mode))
	return 2;
      else
	return num_insns_constant_wide (INTVAL (op));

      case CONST_DOUBLE:
	if (mode == SFmode || mode == SDmode)
	  {
	    long l;
	    REAL_VALUE_TYPE rv;

	    REAL_VALUE_FROM_CONST_DOUBLE (rv, op);
	    if (DECIMAL_FLOAT_MODE_P (mode))
	      REAL_VALUE_TO_TARGET_DECIMAL32 (rv, l);
	    else
	      REAL_VALUE_TO_TARGET_SINGLE (rv, l);
	    return num_insns_constant_wide ((HOST_WIDE_INT) l);
	  }

	long l[2];
	REAL_VALUE_TYPE rv;

	REAL_VALUE_FROM_CONST_DOUBLE (rv, op);
	if (DECIMAL_FLOAT_MODE_P (mode))
	  REAL_VALUE_TO_TARGET_DECIMAL64 (rv, l);
	else
	  REAL_VALUE_TO_TARGET_DOUBLE (rv, l);
	high = l[WORDS_BIG_ENDIAN == 0];
	low  = l[WORDS_BIG_ENDIAN != 0];

	if (TARGET_32BIT)
	  return (num_insns_constant_wide (low)
		  + num_insns_constant_wide (high));
	else
	  {
	    if ((high == 0 && low >= 0)
		|| (high == -1 && low < 0))
	      return num_insns_constant_wide (low);

	    else if (mask64_operand (op, mode))
	      return 2;

	    else if (low == 0)
	      return num_insns_constant_wide (high) + 1;

	    else
	      return (num_insns_constant_wide (high)
		      + num_insns_constant_wide (low) + 1);
	  }

    default:
      gcc_unreachable ();
    }
}

/* Interpret element ELT of the CONST_VECTOR OP as an integer value.
   If the mode of OP is MODE_VECTOR_INT, this simply returns the
   corresponding element of the vector, but for V4SFmode and V2SFmode,
   the corresponding "float" is interpreted as an SImode integer.  */

HOST_WIDE_INT
const_vector_elt_as_int (rtx op, unsigned int elt)
{
  rtx tmp;

  /* We can't handle V2DImode and V2DFmode vector constants here yet.  */
  gcc_assert (GET_MODE (op) != V2DImode
	      && GET_MODE (op) != V2DFmode);

  tmp = CONST_VECTOR_ELT (op, elt);
  if (GET_MODE (op) == V4SFmode
      || GET_MODE (op) == V2SFmode)
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
  enum machine_mode mode = GET_MODE (op);
  enum machine_mode inner = GET_MODE_INNER (mode);

  unsigned i;
  unsigned nunits;
  unsigned bitsize;
  unsigned mask;

  HOST_WIDE_INT val;
  HOST_WIDE_INT splat_val;
  HOST_WIDE_INT msb_val;

  if (mode == V2DImode || mode == V2DFmode)
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
      if (splat_val != ((small_val << bitsize) | (small_val & mask)))
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


/* Return true if OP is of the given MODE and can be synthesized
   with a vspltisb, vspltish or vspltisw.  */

bool
easy_altivec_constant (rtx op, enum machine_mode mode)
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

  if (mode == V2DImode)
    {
      /* In case the compiler is built 32-bit, CONST_DOUBLE constants are not
	 easy.  */
      if (GET_CODE (CONST_VECTOR_ELT (op, 0)) != CONST_INT
	  || GET_CODE (CONST_VECTOR_ELT (op, 1)) != CONST_INT)
	return false;

      if (zero_constant (op, mode))
	return true;

      if (INTVAL (CONST_VECTOR_ELT (op, 0)) == -1
	  && INTVAL (CONST_VECTOR_ELT (op, 1)) == -1)
	return true;

      return false;
    }

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

  return false;
}

/* Generate a VEC_DUPLICATE representing a vspltis[bhw] instruction whose
   result is OP.  Abort if it is not possible.  */

rtx
gen_easy_altivec_constant (rtx op)
{
  enum machine_mode mode = GET_MODE (op);
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

const char *
output_vec_const_move (rtx *operands)
{
  int cst, cst2;
  enum machine_mode mode;
  rtx dest, vec;

  dest = operands[0];
  vec = operands[1];
  mode = GET_MODE (dest);

  if (TARGET_VSX)
    {
      if (zero_constant (vec, mode))
	return "xxlxor %x0,%x0,%x0";

      if (mode == V2DImode
	  && INTVAL (CONST_VECTOR_ELT (vec, 0)) == -1
	  && INTVAL (CONST_VECTOR_ELT (vec, 1)) == -1)
	return "vspltisw %0,-1";
    }

  if (TARGET_ALTIVEC)
    {
      rtx splat_vec;
      if (zero_constant (vec, mode))
	return "vxor %0,%0,%0";

      splat_vec = gen_easy_altivec_constant (vec);
      gcc_assert (GET_CODE (splat_vec) == VEC_DUPLICATE);
      operands[1] = XEXP (splat_vec, 0);
      if (!EASY_VECTOR_15 (INTVAL (operands[1])))
	return "#";

      switch (GET_MODE (splat_vec))
	{
	case V4SImode:
	  return "vspltisw %0,%1";

	case V8HImode:
	  return "vspltish %0,%1";

	case V16QImode:
	  return "vspltisb %0,%1";

	default:
	  gcc_unreachable ();
	}
    }

  gcc_assert (TARGET_SPE);

  /* Vector constant 0 is handled as a splitter of V2SI, and in the
     pattern of V1DI, V4HI, and V2SF.

     FIXME: We should probably return # and add post reload
     splitters for these, but this way is so easy ;-).  */
  cst = INTVAL (CONST_VECTOR_ELT (vec, 0));
  cst2 = INTVAL (CONST_VECTOR_ELT (vec, 1));
  operands[1] = CONST_VECTOR_ELT (vec, 0);
  operands[2] = CONST_VECTOR_ELT (vec, 1);
  if (cst == cst2)
    return "li %0,%1\n\tevmergelo %0,%0,%0";
  else
    return "li %0,%1\n\tevmergelo %0,%0,%0\n\tli %0,%2";
}

/* Initialize TARGET of vector PAIRED to VALS.  */

void
paired_expand_vector_init (rtx target, rtx vals)
{
  enum machine_mode mode = GET_MODE (target);
  int n_elts = GET_MODE_NUNITS (mode);
  int n_var = 0;
  rtx x, new_rtx, tmp, constant_op, op1, op2;
  int i;

  for (i = 0; i < n_elts; ++i)
    {
      x = XVECEXP (vals, 0, i);
      if (!(CONST_INT_P (x)
	    || GET_CODE (x) == CONST_DOUBLE
	    || GET_CODE (x) == CONST_FIXED))
	++n_var;
    }
  if (n_var == 0)
    {
      /* Load from constant pool.  */
      emit_move_insn (target, gen_rtx_CONST_VECTOR (mode, XVEC (vals, 0)));
      return;
    }

  if (n_var == 2)
    {
      /* The vector is initialized only with non-constants.  */
      new_rtx = gen_rtx_VEC_CONCAT (V2SFmode, XVECEXP (vals, 0, 0),
				XVECEXP (vals, 0, 1));

      emit_move_insn (target, new_rtx);
      return;
    }
  
  /* One field is non-constant and the other one is a constant.  Load the
     constant from the constant pool and use ps_merge instruction to
     construct the whole vector.  */
  op1 = XVECEXP (vals, 0, 0);
  op2 = XVECEXP (vals, 0, 1);

  constant_op = (CONSTANT_P (op1)) ? op1 : op2;

  tmp = gen_reg_rtx (GET_MODE (constant_op));
  emit_move_insn (tmp, constant_op);

  if (CONSTANT_P (op1))
    new_rtx = gen_rtx_VEC_CONCAT (V2SFmode, tmp, op2);
  else
    new_rtx = gen_rtx_VEC_CONCAT (V2SFmode, op1, tmp);

  emit_move_insn (target, new_rtx);
}

void
paired_expand_vector_move (rtx operands[])
{
  rtx op0 = operands[0], op1 = operands[1];

  emit_move_insn (op0, op1);
}

/* Emit vector compare for code RCODE.  DEST is destination, OP1 and
   OP2 are two VEC_COND_EXPR operands, CC_OP0 and CC_OP1 are the two
   operands for the relation operation COND.  This is a recursive
   function.  */

static void
paired_emit_vector_compare (enum rtx_code rcode,
                            rtx dest, rtx op0, rtx op1,
                            rtx cc_op0, rtx cc_op1)
{
  rtx tmp = gen_reg_rtx (V2SFmode);
  rtx tmp1, max, min;

  gcc_assert (TARGET_PAIRED_FLOAT);
  gcc_assert (GET_MODE (op0) == GET_MODE (op1));

  switch (rcode)
    {
    case LT:
    case LTU:
      paired_emit_vector_compare (GE, dest, op1, op0, cc_op0, cc_op1);
      return;
    case GE:
    case GEU:
      emit_insn (gen_subv2sf3 (tmp, cc_op0, cc_op1));
      emit_insn (gen_selv2sf4 (dest, tmp, op0, op1, CONST0_RTX (SFmode)));
      return;
    case LE:
    case LEU:
      paired_emit_vector_compare (GE, dest, op0, op1, cc_op1, cc_op0);
      return;
    case GT:
      paired_emit_vector_compare (LE, dest, op1, op0, cc_op0, cc_op1);
      return;
    case EQ:
      tmp1 = gen_reg_rtx (V2SFmode);
      max = gen_reg_rtx (V2SFmode);
      min = gen_reg_rtx (V2SFmode);
      gen_reg_rtx (V2SFmode);
      
      emit_insn (gen_subv2sf3 (tmp, cc_op0, cc_op1));
      emit_insn (gen_selv2sf4
                 (max, tmp, cc_op0, cc_op1, CONST0_RTX (SFmode)));
      emit_insn (gen_subv2sf3 (tmp, cc_op1, cc_op0));
      emit_insn (gen_selv2sf4
                 (min, tmp, cc_op0, cc_op1, CONST0_RTX (SFmode)));
      emit_insn (gen_subv2sf3 (tmp1, min, max));
      emit_insn (gen_selv2sf4 (dest, tmp1, op0, op1, CONST0_RTX (SFmode)));
      return;
    case NE:
      paired_emit_vector_compare (EQ, dest, op1, op0, cc_op0, cc_op1);
      return;
    case UNLE:
      paired_emit_vector_compare (LE, dest, op1, op0, cc_op0, cc_op1);
      return;
    case UNLT:
      paired_emit_vector_compare (LT, dest, op1, op0, cc_op0, cc_op1);
      return;
    case UNGE:
      paired_emit_vector_compare (GE, dest, op1, op0, cc_op0, cc_op1);
      return;
    case UNGT:
      paired_emit_vector_compare (GT, dest, op1, op0, cc_op0, cc_op1);
      return;
    default:
      gcc_unreachable ();
    }

  return;
}

/* Emit vector conditional expression.
   DEST is destination. OP1 and OP2 are two VEC_COND_EXPR operands.
   CC_OP0 and CC_OP1 are the two operands for the relation operation COND.  */

int
paired_emit_vector_cond_expr (rtx dest, rtx op1, rtx op2,
			      rtx cond, rtx cc_op0, rtx cc_op1)
{
  enum rtx_code rcode = GET_CODE (cond);

  if (!TARGET_PAIRED_FLOAT)
    return 0;

  paired_emit_vector_compare (rcode, dest, op1, op2, cc_op0, cc_op1);

  return 1;
}

/* Initialize vector TARGET to VALS.  */

void
rs6000_expand_vector_init (rtx target, rtx vals)
{
  enum machine_mode mode = GET_MODE (target);
  enum machine_mode inner_mode = GET_MODE_INNER (mode);
  int n_elts = GET_MODE_NUNITS (mode);
  int n_var = 0, one_var = -1;
  bool all_same = true, all_const_zero = true;
  rtx x, mem;
  int i;

  for (i = 0; i < n_elts; ++i)
    {
      x = XVECEXP (vals, 0, i);
      if (!(CONST_INT_P (x)
	    || GET_CODE (x) == CONST_DOUBLE
	    || GET_CODE (x) == CONST_FIXED))
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
	  emit_insn (gen_rtx_SET (VOIDmode, target,
				  gen_rtx_XOR (mode, target, target)));
	  return;
	}
      else if (int_vector_p && easy_vector_constant (const_vec, mode))
	{
	  /* Splat immediate.  */
	  emit_insn (gen_rtx_SET (VOIDmode, target, const_vec));
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
      rtx op0 = XVECEXP (vals, 0, 0);
      rtx op1 = XVECEXP (vals, 0, 1);
      if (all_same)
	{
	  if (!MEM_P (op0) && !REG_P (op0))
	    op0 = force_reg (inner_mode, op0);
	  if (mode == V2DFmode)
	    emit_insn (gen_vsx_splat_v2df (target, op0));
	  else
	    emit_insn (gen_vsx_splat_v2di (target, op0));
	}
      else
	{
	  op0 = force_reg (inner_mode, op0);
	  op1 = force_reg (inner_mode, op1);
	  if (mode == V2DFmode)
	    emit_insn (gen_vsx_concat_v2df (target, op0, op1));
	  else
	    emit_insn (gen_vsx_concat_v2di (target, op0, op1));
	}
      return;
    }

  /* With single precision floating point on VSX, know that internally single
     precision is actually represented as a double, and either make 2 V2DF
     vectors, and convert these vectors to single precision, or do one
     conversion, and splat the result to the other elements.  */
  if (mode == V4SFmode && VECTOR_MEM_VSX_P (mode))
    {
      if (all_same)
	{
	  rtx freg = gen_reg_rtx (V4SFmode);
	  rtx sreg = force_reg (SFmode, XVECEXP (vals, 0, 0));
	  rtx cvt  = ((TARGET_XSCVDPSPN)
		      ? gen_vsx_xscvdpspn_scalar (freg, sreg)
		      : gen_vsx_xscvdpsp_scalar (freg, sreg));

	  emit_insn (cvt);
	  emit_insn (gen_vsx_xxspltw_v4sf_direct (target, freg, const0_rtx));
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

	  emit_insn (gen_vsx_concat_v2sf (dbl_even, op0, op1));
	  emit_insn (gen_vsx_concat_v2sf (dbl_odd, op2, op3));
	  emit_insn (gen_vsx_xvcvdpsp (flt_even, dbl_even));
	  emit_insn (gen_vsx_xvcvdpsp (flt_odd, dbl_odd));
	  rs6000_expand_extract_even (target, flt_even, flt_odd);
	}
      return;
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
					      gen_rtx_SET (VOIDmode,
							   target, mem),
					      x)));
      x = gen_rtx_VEC_SELECT (inner_mode, target,
			      gen_rtx_PARALLEL (VOIDmode,
						gen_rtvec (1, const0_rtx)));
      emit_insn (gen_rtx_SET (VOIDmode, target,
			      gen_rtx_VEC_DUPLICATE (mode, x)));
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
  enum machine_mode mode = GET_MODE (target);
  enum machine_mode inner_mode = GET_MODE_INNER (mode);
  rtx reg = gen_reg_rtx (mode);
  rtx mask, mem, x;
  int width = GET_MODE_SIZE (inner_mode);
  int i;

  if (VECTOR_MEM_VSX_P (mode) && (mode == V2DFmode || mode == V2DImode))
    {
      rtx (*set_func) (rtx, rtx, rtx, rtx)
	= ((mode == V2DFmode) ? gen_vsx_set_v2df : gen_vsx_set_v2di);
      emit_insn (set_func (target, target, val, GEN_INT (elt)));
      return;
    }

  /* Load single variable value.  */
  mem = assign_stack_temp (mode, GET_MODE_SIZE (inner_mode));
  emit_move_insn (adjust_address_nv (mem, inner_mode, 0), val);
  x = gen_rtx_UNSPEC (VOIDmode,
		      gen_rtvec (1, const0_rtx), UNSPEC_LVE);
  emit_insn (gen_rtx_PARALLEL (VOIDmode,
			       gen_rtvec (2,
					  gen_rtx_SET (VOIDmode,
						       reg, mem),
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
      /* Invert selector.  */
      rtx splat = gen_rtx_VEC_DUPLICATE (V16QImode,
					 gen_rtx_CONST_INT (QImode, -1));
      rtx tmp = gen_reg_rtx (V16QImode);
      emit_move_insn (tmp, splat);
      x = gen_rtx_MINUS (V16QImode, tmp, force_reg (V16QImode, x));
      emit_move_insn (tmp, x);

      /* Permute with operands reversed and adjusted selector.  */
      x = gen_rtx_UNSPEC (mode, gen_rtvec (3, reg, target, tmp),
			  UNSPEC_VPERM);
    }

  emit_insn (gen_rtx_SET (VOIDmode, target, x));
}

/* Extract field ELT from VEC into TARGET.  */

void
rs6000_expand_vector_extract (rtx target, rtx vec, int elt)
{
  enum machine_mode mode = GET_MODE (vec);
  enum machine_mode inner_mode = GET_MODE_INNER (mode);
  rtx mem;

  if (VECTOR_MEM_VSX_P (mode))
    {
      switch (mode)
	{
	default:
	  break;
	case V2DFmode:
	  emit_insn (gen_vsx_extract_v2df (target, vec, GEN_INT (elt)));
	  return;
	case V2DImode:
	  emit_insn (gen_vsx_extract_v2di (target, vec, GEN_INT (elt)));
	  return;
	case V4SFmode:
	  emit_insn (gen_vsx_extract_v4sf (target, vec, GEN_INT (elt)));
	  return;
	}
    }

  /* Allocate mode-sized buffer.  */
  mem = assign_stack_temp (mode, GET_MODE_SIZE (mode));

  emit_move_insn (mem, vec);

  /* Add offset to field within buffer matching vector element.  */
  mem = adjust_address_nv (mem, inner_mode, elt * GET_MODE_SIZE (inner_mode));

  emit_move_insn (target, adjust_address_nv (mem, inner_mode, 0));
}

/* Generates shifts and masks for a pair of rldicl or rldicr insns to
   implement ANDing by the mask IN.  */
void
build_mask64_2_operands (rtx in, rtx *out)
{
  unsigned HOST_WIDE_INT c, lsb, m1, m2;
  int shift;

  gcc_assert (GET_CODE (in) == CONST_INT);

  c = INTVAL (in);
  if (c & 1)
    {
      /* Assume c initially something like 0x00fff000000fffff.  The idea
	 is to rotate the word so that the middle ^^^^^^ group of zeros
	 is at the MS end and can be cleared with an rldicl mask.  We then
	 rotate back and clear off the MS    ^^ group of zeros with a
	 second rldicl.  */
      c = ~c;			/*   c == 0xff000ffffff00000 */
      lsb = c & -c;		/* lsb == 0x0000000000100000 */
      m1 = -lsb;		/*  m1 == 0xfffffffffff00000 */
      c = ~c;			/*   c == 0x00fff000000fffff */
      c &= -lsb;		/*   c == 0x00fff00000000000 */
      lsb = c & -c;		/* lsb == 0x0000100000000000 */
      c = ~c;			/*   c == 0xff000fffffffffff */
      c &= -lsb;		/*   c == 0xff00000000000000 */
      shift = 0;
      while ((lsb >>= 1) != 0)
	shift++;		/* shift == 44 on exit from loop */
      m1 <<= 64 - shift;	/*  m1 == 0xffffff0000000000 */
      m1 = ~m1;			/*  m1 == 0x000000ffffffffff */
      m2 = ~c;			/*  m2 == 0x00ffffffffffffff */
    }
  else
    {
      /* Assume c initially something like 0xff000f0000000000.  The idea
	 is to rotate the word so that the     ^^^  middle group of zeros
	 is at the LS end and can be cleared with an rldicr mask.  We then
	 rotate back and clear off the LS group of ^^^^^^^^^^ zeros with
	 a second rldicr.  */
      lsb = c & -c;		/* lsb == 0x0000010000000000 */
      m2 = -lsb;		/*  m2 == 0xffffff0000000000 */
      c = ~c;			/*   c == 0x00fff0ffffffffff */
      c &= -lsb;		/*   c == 0x00fff00000000000 */
      lsb = c & -c;		/* lsb == 0x0000100000000000 */
      c = ~c;			/*   c == 0xff000fffffffffff */
      c &= -lsb;		/*   c == 0xff00000000000000 */
      shift = 0;
      while ((lsb >>= 1) != 0)
	shift++;		/* shift == 44 on exit from loop */
      m1 = ~c;			/*  m1 == 0x00ffffffffffffff */
      m1 >>= shift;		/*  m1 == 0x0000000000000fff */
      m1 = ~m1;			/*  m1 == 0xfffffffffffff000 */
    }

  /* Note that when we only have two 0->1 and 1->0 transitions, one of the
     masks will be all 1's.  We are guaranteed more than one transition.  */
  out[0] = GEN_INT (64 - shift);
  out[1] = GEN_INT (m1);
  out[2] = GEN_INT (shift);
  out[3] = GEN_INT (m2);
}

/* Return TRUE if OP is an invalid SUBREG operation on the e500.  */

bool
invalid_e500_subreg (rtx op, enum machine_mode mode)
{
  if (TARGET_E500_DOUBLE)
    {
      /* Reject (subreg:SI (reg:DF)); likewise with subreg:DI or
	 subreg:TI and reg:TF.  Decimal float modes are like integer
	 modes (only low part of each register used) for this
	 purpose.  */
      if (GET_CODE (op) == SUBREG
	  && (mode == SImode || mode == DImode || mode == TImode
	      || mode == DDmode || mode == TDmode || mode == PTImode)
	  && REG_P (SUBREG_REG (op))
	  && (GET_MODE (SUBREG_REG (op)) == DFmode
	      || GET_MODE (SUBREG_REG (op)) == TFmode))
	return true;

      /* Reject (subreg:DF (reg:DI)); likewise with subreg:TF and
	 reg:TI.  */
      if (GET_CODE (op) == SUBREG
	  && (mode == DFmode || mode == TFmode)
	  && REG_P (SUBREG_REG (op))
	  && (GET_MODE (SUBREG_REG (op)) == DImode
	      || GET_MODE (SUBREG_REG (op)) == TImode
	      || GET_MODE (SUBREG_REG (op)) == PTImode
	      || GET_MODE (SUBREG_REG (op)) == DDmode
	      || GET_MODE (SUBREG_REG (op)) == TDmode))
	return true;
    }

  if (TARGET_SPE
      && GET_CODE (op) == SUBREG
      && mode == SImode
      && REG_P (SUBREG_REG (op))
      && SPE_VECTOR_MODE (GET_MODE (SUBREG_REG (op))))
    return true;

  return false;
}

/* Return alignment of TYPE.  Existing alignment is ALIGN.  HOW
   selects whether the alignment is abi mandated, optional, or
   both abi and optional alignment.  */
   
unsigned int
rs6000_data_alignment (tree type, unsigned int align, enum data_align how)
{
  if (how != align_opt)
    {
      if (TREE_CODE (type) == VECTOR_TYPE)
	{
	  if ((TARGET_SPE && SPE_VECTOR_MODE (TYPE_MODE (type)))
	      || (TARGET_PAIRED_FLOAT && PAIRED_VECTOR_MODE (TYPE_MODE (type))))
	    {
	      if (align < 64)
		align = 64;
	    }
	  else if (align < 128)
	    align = 128;
	}
      else if (TARGET_E500_DOUBLE
	       && TREE_CODE (type) == REAL_TYPE
	       && TYPE_MODE (type) == DFmode)
	{
	  if (align < 64)
	    align = 64;
	}
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

/* AIX increases natural record alignment to doubleword if the first
   field is an FP double while the FP fields remain word aligned.  */

unsigned int
rs6000_special_round_type_align (tree type, unsigned int computed,
				 unsigned int specified)
{
  unsigned int align = MAX (computed, specified);
  tree field = TYPE_FIELDS (type);

  /* Skip all non field decls */
  while (field != NULL && TREE_CODE (field) != FIELD_DECL)
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
    while (field != NULL && TREE_CODE (field) != FIELD_DECL)
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
		    enum machine_mode mode ATTRIBUTE_UNUSED)
{
#if TARGET_ELF
  rtx sym_ref;

  if (rs6000_sdata == SDATA_NONE || rs6000_sdata == SDATA_DATA)
    return 0;

  if (DEFAULT_ABI != ABI_V4)
    return 0;

  /* Vector and float memory instructions have a limited offset on the
     SPE, so using a vector or float variable directly as an operand is
     not useful.  */
  if (TARGET_SPE
      && (SPE_VECTOR_MODE (mode) || FLOAT_MODE_P (mode)))
    return 0;

  if (GET_CODE (op) == SYMBOL_REF)
    sym_ref = op;

  else if (GET_CODE (op) != CONST
	   || GET_CODE (XEXP (op, 0)) != PLUS
	   || GET_CODE (XEXP (XEXP (op, 0), 0)) != SYMBOL_REF
	   || GET_CODE (XEXP (XEXP (op, 0), 1)) != CONST_INT)
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
  int regno0, regno1;

  if (!REG_P (op0) || !REG_P (op1))
    return false;

  if (!TARGET_DIRECT_MOVE && !TARGET_MFPGPR)
    return false;

  regno0 = REGNO (op0);
  regno1 = REGNO (op1);
  if (regno0 >= FIRST_PSEUDO_REGISTER || regno1 >= FIRST_PSEUDO_REGISTER)
    return false;

  if (INT_REGNO_P (regno0))
    return (TARGET_DIRECT_MOVE) ? VSX_REGNO_P (regno1) : FP_REGNO_P (regno1);

  else if (INT_REGNO_P (regno1))
    {
      if (TARGET_MFPGPR && FP_REGNO_P (regno0))
	return true;

      else if (TARGET_DIRECT_MOVE && VSX_REGNO_P (regno0))
	return true;
    }

  return false;
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
      debug_rtx (gen_rtx_SET (VOIDmode, op0, op1));
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

/* Return true if the MEM operand is a memory operand suitable for use
   with a (full width, possibly multiple) gpr load/store.  On
   powerpc64 this means the offset must be divisible by 4.
   Implements 'Y' constraint.

   Accept direct, indexed, offset, lo_sum and tocref.  Since this is
   a constraint function we know the operand has satisfied a suitable
   memory predicate.  Also accept some odd rtl generated by reload
   (see rs6000_legitimize_reload_address for various forms).  It is
   important that reload rtl be accepted by appropriate constraints
   but not by the operand predicate.

   Offsetting a lo_sum should not be allowed, except where we know by
   alignment that a 32k boundary is not crossed, but see the ???
   comment in rs6000_legitimize_reload_address.  Note that by
   "offsetting" here we mean a further offset to access parts of the
   MEM.  It's fine to have a lo_sum where the inner address is offset
   from a sym, since the same sym+offset will appear in the high part
   of the address calculation.  */

bool
mem_operand_gpr (rtx op, enum machine_mode mode)
{
  unsigned HOST_WIDE_INT offset;
  int extra;
  rtx addr = XEXP (op, 0);

  op = address_offset (addr);
  if (op == NULL_RTX)
    return true;

  offset = INTVAL (op);
  if (TARGET_POWERPC64 && (offset & 3) != 0)
    return false;

  extra = GET_MODE_SIZE (mode) - UNITS_PER_WORD;
  gcc_assert (extra >= 0);

  if (GET_CODE (addr) == LO_SUM)
    /* For lo_sum addresses, we must allow any offset except one that
       causes a wrap, so test only the low 16 bits.  */
    offset = ((offset & 0xffff) ^ 0x8000) - 0x8000;

  return offset + 0x8000 < 0x10000u - extra;
}

/* Subroutines of rs6000_legitimize_address and rs6000_legitimate_address_p.  */

static bool
reg_offset_addressing_ok_p (enum machine_mode mode)
{
  switch (mode)
    {
    case V16QImode:
    case V8HImode:
    case V4SFmode:
    case V4SImode:
    case V2DFmode:
    case V2DImode:
    case TImode:
      /* AltiVec/VSX vector modes.  Only reg+reg addressing is valid.  While
	 TImode is not a vector mode, if we want to use the VSX registers to
	 move it around, we need to restrict ourselves to reg+reg
	 addressing.  */
      if (VECTOR_MEM_ALTIVEC_OR_VSX_P (mode))
	return false;
      break;

    case V4HImode:
    case V2SImode:
    case V1DImode:
    case V2SFmode:
       /* Paired vector modes.  Only reg+reg addressing is valid.  */
      if (TARGET_PAIRED_FLOAT)
        return false;
      break;

    case SDmode:
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

  if (GET_CODE (op) == REG)
    regnum = REGNO (op);

  else if (GET_CODE (op) == PLUS
	   && GET_CODE (XEXP (op, 0)) == REG
	   && GET_CODE (XEXP (op, 1)) == CONST_INT)
    regnum = REGNO (XEXP (op, 0));

  else
    return false;

  return (regnum >= FIRST_VIRTUAL_REGISTER
	  && regnum <= LAST_VIRTUAL_POINTER_REGISTER);
}

/* Return true if a MODE sized memory accesses to OP plus OFFSET
   is known to not straddle a 32k boundary.  */

static bool
offsettable_ok_by_alignment (rtx op, HOST_WIDE_INT offset,
			     enum machine_mode mode)
{
  tree decl, type;
  unsigned HOST_WIDE_INT dsize, dalign, lsb, mask;

  if (GET_CODE (op) != SYMBOL_REF)
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
	  enum machine_mode cmode = get_pool_mode (op);

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

	  return dalign / BITS_PER_UNIT >= dsize;
	}
    }
  else
    {
      type = TREE_TYPE (decl);

      dalign = TYPE_ALIGN (type);
      if (CONSTANT_CLASS_P (decl))
	dalign = CONSTANT_ALIGNMENT (decl, dalign);
      else
	dalign = DATA_ALIGNMENT (decl, dalign);

      if (dsize == 0)
	{
	  /* BLKmode, check the entire object.  */
	  if (TREE_CODE (decl) == STRING_CST)
	    dsize = TREE_STRING_LENGTH (decl);
	  else if (TYPE_SIZE_UNIT (type)
		   && tree_fits_uhwi_p (TYPE_SIZE_UNIT (type)))
	    dsize = tree_to_uhwi (TYPE_SIZE_UNIT (type));
	  else
	    return false;
	  if (dsize > 32768)
	    return false;

	  return dalign / BITS_PER_UNIT >= dsize;
	}
    }

  /* Find how many bits of the alignment we know for this access.  */
  mask = dalign / BITS_PER_UNIT - 1;
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
  return (GET_CODE (base) == SYMBOL_REF
	  && CONSTANT_POOL_ADDRESS_P (base)
	  && ASM_OUTPUT_SPECIAL_POOL_ENTRY_P (get_pool_constant (base), Pmode));
}

static const_rtx tocrel_base, tocrel_offset;

/* Return true if OP is a toc pointer relative address (the output
   of create_TOC_reference).  If STRICT, do not match high part or
   non-split -mcmodel=large/medium toc pointer relative addresses.  */

bool
toc_relative_expr_p (const_rtx op, bool strict)
{
  if (!TARGET_TOC)
    return false;

  if (TARGET_CMODEL != CMODEL_SMALL)
    {
      /* Only match the low part.  */
      if (GET_CODE (op) == LO_SUM
	  && REG_P (XEXP (op, 0))
	  && INT_REG_OK_FOR_BASE_P (XEXP (op, 0), strict))
	op = XEXP (op, 1);
      else if (strict)
	return false;
    }

  tocrel_base = op;
  tocrel_offset = const0_rtx;
  if (GET_CODE (op) == PLUS && add_cint_operand (XEXP (op, 1), GET_MODE (op)))
    {
      tocrel_base = XEXP (op, 0);
      tocrel_offset = XEXP (op, 1);
    }

  return (GET_CODE (tocrel_base) == UNSPEC
	  && XINT (tocrel_base, 1) == UNSPEC_TOCREL);
}

/* Return true if X is a constant pool address, and also for cmodel=medium
   if X is a toc-relative address known to be offsettable within MODE.  */

bool
legitimate_constant_pool_address_p (const_rtx x, enum machine_mode mode,
				    bool strict)
{
  return (toc_relative_expr_p (x, strict)
	  && (TARGET_CMODEL != CMODEL_MEDIUM
	      || constant_pool_expr_p (XVECEXP (tocrel_base, 0, 0))
	      || mode == QImode
	      || offsettable_ok_by_alignment (XVECEXP (tocrel_base, 0, 0),
					      INTVAL (tocrel_offset), mode)));
}

static bool
legitimate_small_data_p (enum machine_mode mode, rtx x)
{
  return (DEFAULT_ABI == ABI_V4
	  && !flag_pic && !TARGET_TOC
	  && (GET_CODE (x) == SYMBOL_REF || GET_CODE (x) == CONST)
	  && small_data_operand (x, mode));
}

/* SPE offset addressing is limited to 5-bits worth of double words.  */
#define SPE_CONST_OFFSET_OK(x) (((x) & ~0xf8) == 0)

bool
rs6000_legitimate_offset_address_p (enum machine_mode mode, rtx x,
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
  if (!reg_offset_addressing_ok_p (mode))
    return virtual_stack_registers_memory_p (x);
  if (legitimate_constant_pool_address_p (x, mode, strict || lra_in_progress))
    return true;
  if (GET_CODE (XEXP (x, 1)) != CONST_INT)
    return false;

  offset = INTVAL (XEXP (x, 1));
  extra = 0;
  switch (mode)
    {
    case V4HImode:
    case V2SImode:
    case V1DImode:
    case V2SFmode:
      /* SPE vector modes.  */
      return SPE_CONST_OFFSET_OK (offset);

    case DFmode:
    case DDmode:
    case DImode:
      /* On e500v2, we may have:

	   (subreg:DF (mem:DI (plus (reg) (const_int))) 0).

         Which gets addressed with evldd instructions.  */
      if (TARGET_E500_DOUBLE)
	return SPE_CONST_OFFSET_OK (offset);

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

    case TFmode:
      if (TARGET_E500_DOUBLE)
	return (SPE_CONST_OFFSET_OK (offset)
		&& SPE_CONST_OFFSET_OK (offset + 8));
      /* fall through */

    case TDmode:
    case TImode:
    case PTImode:
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

  offset += 0x8000;
  return offset < 0x10000 - extra;
}

bool
legitimate_indexed_address_p (rtx x, int strict)
{
  rtx op0, op1;

  if (GET_CODE (x) != PLUS)
    return false;

  op0 = XEXP (x, 0);
  op1 = XEXP (x, 1);

  /* Recognize the rtl generated by reload which we know will later be
     replaced with proper base and index regs.  */
  if (!strict
      && reload_in_progress
      && (REG_P (op0) || GET_CODE (op0) == PLUS)
      && REG_P (op1))
    return true;

  return (REG_P (op0) && REG_P (op1)
	  && ((INT_REG_OK_FOR_BASE_P (op0, strict)
	       && INT_REG_OK_FOR_INDEX_P (op1, strict))
	      || (INT_REG_OK_FOR_BASE_P (op1, strict)
		  && INT_REG_OK_FOR_INDEX_P (op0, strict))));
}

bool
avoiding_indexed_address_p (enum machine_mode mode)
{
  /* Avoid indexed addressing for modes that have non-indexed
     load/store instruction forms.  */
  return (TARGET_AVOID_XFORM && VECTOR_MEM_NONE_P (mode));
}

bool
legitimate_indirect_address_p (rtx x, int strict)
{
  return GET_CODE (x) == REG && INT_REG_OK_FOR_BASE_P (x, strict);
}

bool
macho_lo_sum_memory_operand (rtx x, enum machine_mode mode)
{
  if (!TARGET_MACHO || !flag_pic
      || mode != SImode || GET_CODE (x) != MEM)
    return false;
  x = XEXP (x, 0);

  if (GET_CODE (x) != LO_SUM)
    return false;
  if (GET_CODE (XEXP (x, 0)) != REG)
    return false;
  if (!INT_REG_OK_FOR_BASE_P (XEXP (x, 0), 0))
    return false;
  x = XEXP (x, 1);

  return CONSTANT_P (x);
}

static bool
legitimate_lo_sum_address_p (enum machine_mode mode, rtx x, int strict)
{
  if (GET_CODE (x) != LO_SUM)
    return false;
  if (GET_CODE (XEXP (x, 0)) != REG)
    return false;
  if (!INT_REG_OK_FOR_BASE_P (XEXP (x, 0), strict))
    return false;
  /* Restrict addressing for DI because of our SUBREG hackery.  */
  if (TARGET_E500_DOUBLE && GET_MODE_SIZE (mode) > UNITS_PER_WORD)
    return false;
  x = XEXP (x, 1);

  if (TARGET_ELF || TARGET_MACHO)
    {
      bool large_toc_ok;

      if (DEFAULT_ABI == ABI_V4 && flag_pic)
	return false;
      /* LRA don't use LEGITIMIZE_RELOAD_ADDRESS as it usually calls
	 push_reload from reload pass code.  LEGITIMIZE_RELOAD_ADDRESS
	 recognizes some LO_SUM addresses as valid although this
	 function says opposite.  In most cases, LRA through different
	 transformations can generate correct code for address reloads.
	 It can not manage only some LO_SUM cases.  So we need to add
	 code analogous to one in rs6000_legitimize_reload_address for
	 LOW_SUM here saying that some addresses are still valid.  */
      large_toc_ok = (lra_in_progress && TARGET_CMODEL != CMODEL_SMALL
		      && small_toc_ref (x, VOIDmode));
      if (TARGET_TOC && ! large_toc_ok)
	return false;
      if (GET_MODE_NUNITS (mode) != 1)
	return false;
      if (GET_MODE_SIZE (mode) > UNITS_PER_WORD
	  && !(/* ??? Assume floating point reg based on mode?  */
	       TARGET_HARD_FLOAT && TARGET_FPRS && TARGET_DOUBLE_FLOAT
	       && (mode == DFmode || mode == DDmode)))
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
			   enum machine_mode mode)
{
  unsigned int extra;

  if (!reg_offset_addressing_ok_p (mode))
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
	       && (mode != TImode || !TARGET_QUAD_MEMORY))
	return gen_rtx_PLUS (Pmode,
			     force_reg (Pmode, XEXP (x, 0)),
			     force_reg (Pmode, XEXP (x, 1)));
      else
	return force_reg (Pmode, x);
    }
  if (GET_CODE (x) == SYMBOL_REF)
    {
      enum tls_model model = SYMBOL_REF_TLS_MODEL (x);
      if (model != 0)
	return rs6000_legitimize_tls_address (x, model);
    }

  extra = 0;
  switch (mode)
    {
    case TFmode:
    case TDmode:
    case TImode:
    case PTImode:
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
      && GET_CODE (XEXP (x, 0)) == REG
      && GET_CODE (XEXP (x, 1)) == CONST_INT
      && ((unsigned HOST_WIDE_INT) (INTVAL (XEXP (x, 1)) + 0x8000)
	  >= 0x10000 - extra)
      && !(SPE_VECTOR_MODE (mode)
	   || (TARGET_E500_DOUBLE && GET_MODE_SIZE (mode) > UNITS_PER_WORD)))
    {
      HOST_WIDE_INT high_int, low_int;
      rtx sum;
      low_int = ((INTVAL (XEXP (x, 1)) & 0xffff) ^ 0x8000) - 0x8000;
      if (low_int >= 0x8000 - extra)
	low_int = 0;
      high_int = INTVAL (XEXP (x, 1)) - low_int;
      sum = force_operand (gen_rtx_PLUS (Pmode, XEXP (x, 0),
					 GEN_INT (high_int)), 0);
      return plus_constant (Pmode, sum, low_int);
    }
  else if (GET_CODE (x) == PLUS
	   && GET_CODE (XEXP (x, 0)) == REG
	   && GET_CODE (XEXP (x, 1)) != CONST_INT
	   && GET_MODE_NUNITS (mode) == 1
	   && (GET_MODE_SIZE (mode) <= UNITS_PER_WORD
	       || (/* ??? Assume floating point reg based on mode?  */
		   (TARGET_HARD_FLOAT && TARGET_FPRS && TARGET_DOUBLE_FLOAT)
		   && (mode == DFmode || mode == DDmode)))
	   && !avoiding_indexed_address_p (mode))
    {
      return gen_rtx_PLUS (Pmode, XEXP (x, 0),
			   force_reg (Pmode, force_operand (XEXP (x, 1), 0)));
    }
  else if (SPE_VECTOR_MODE (mode)
	   || (TARGET_E500_DOUBLE && GET_MODE_SIZE (mode) > UNITS_PER_WORD))
    {
      if (mode == DImode)
	return x;
      /* We accept [reg + reg] and [reg + OFFSET].  */

      if (GET_CODE (x) == PLUS)
       {
         rtx op1 = XEXP (x, 0);
         rtx op2 = XEXP (x, 1);
         rtx y;

         op1 = force_reg (Pmode, op1);

         if (GET_CODE (op2) != REG
             && (GET_CODE (op2) != CONST_INT
                 || !SPE_CONST_OFFSET_OK (INTVAL (op2))
                 || (GET_MODE_SIZE (mode) > 8
                     && !SPE_CONST_OFFSET_OK (INTVAL (op2) + 8))))
           op2 = force_reg (Pmode, op2);

         /* We can't always do [reg + reg] for these, because [reg +
            reg + offset] is not a legitimate addressing mode.  */
         y = gen_rtx_PLUS (Pmode, op1, op2);

         if ((GET_MODE_SIZE (mode) > 8 || mode == DDmode) && REG_P (op2))
           return force_reg (Pmode, y);
         else
           return y;
       }

      return force_reg (Pmode, x);
    }
  else if ((TARGET_ELF
#if TARGET_MACHO
	    || !MACHO_DYNAMIC_NO_PIC_P
#endif
	    )
	   && TARGET_32BIT
	   && TARGET_NO_TOC
	   && ! flag_pic
	   && GET_CODE (x) != CONST_INT
	   && GET_CODE (x) != CONST_DOUBLE
	   && CONSTANT_P (x)
	   && GET_MODE_NUNITS (mode) == 1
	   && (GET_MODE_SIZE (mode) <= UNITS_PER_WORD
	       || (/* ??? Assume floating point reg based on mode?  */
		   (TARGET_HARD_FLOAT && TARGET_FPRS && TARGET_DOUBLE_FLOAT)
		   && (mode == DFmode || mode == DDmode))))
    {
      rtx reg = gen_reg_rtx (Pmode);
      if (TARGET_ELF)
	emit_insn (gen_elf_high (reg, x));
      else
	emit_insn (gen_macho_high (reg, x));
      return gen_rtx_LO_SUM (Pmode, reg, x);
    }
  else if (TARGET_TOC
	   && GET_CODE (x) == SYMBOL_REF
	   && constant_pool_expr_p (x)
	   && ASM_OUTPUT_SPECIAL_POOL_ENTRY_P (get_pool_constant (x), Pmode))
    return create_TOC_reference (x, NULL_RTX);
  else
    return x;
}

/* Debug version of rs6000_legitimize_address.  */
static rtx
rs6000_debug_legitimize_address (rtx x, rtx oldx, enum machine_mode mode)
{
  rtx ret;
  rtx insns;

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
  fputs ("@dtprel+0x8000", file);
}

/* In the name of slightly smaller debug output, and to cater to
   general assembler lossage, recognize various UNSPEC sequences
   and turn them back into a direct symbol reference.  */

static rtx
rs6000_delegitimize_address (rtx orig_x)
{
  rtx x, y, offset;

  orig_x = delegitimize_mem_from_attrs (orig_x);
  x = orig_x;
  if (MEM_P (x))
    x = XEXP (x, 0);

  y = x;
  if (TARGET_CMODEL != CMODEL_SMALL
      && GET_CODE (y) == LO_SUM)
    y = XEXP (y, 1);

  offset = NULL_RTX;
  if (GET_CODE (y) == PLUS
      && GET_MODE (y) == Pmode
      && CONST_INT_P (XEXP (y, 1)))
    {
      offset = XEXP (y, 1);
      y = XEXP (y, 0);
    }

  if (GET_CODE (y) == UNSPEC
      && XINT (y, 1) == UNSPEC_TOCREL)
    {
#ifdef ENABLE_CHECKING
      if (REG_P (XVECEXP (y, 0, 1))
	  && REGNO (XVECEXP (y, 0, 1)) == TOC_REGISTER)
	{
	  /* All good.  */
	}
      else if (GET_CODE (XVECEXP (y, 0, 1)) == DEBUG_EXPR)
	{
	  /* Weirdness alert.  df_note_compute can replace r2 with a
	     debug_expr when this unspec is in a debug_insn.
	     Seen in gcc.dg/pr51957-1.c  */
	}
      else
	{
	  debug_rtx (orig_x);
	  abort ();
	}
#endif
      y = XVECEXP (y, 0, 0);

#ifdef HAVE_AS_TLS
      /* Do not associate thread-local symbols with the original
	 constant pool symbol.  */
      if (TARGET_XCOFF
	  && GET_CODE (y) == SYMBOL_REF
	  && CONSTANT_POOL_ADDRESS_P (y)
	  && SYMBOL_REF_TLS_MODEL (get_pool_constant (y)) >= TLS_MODEL_REAL)
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
      if (GET_CODE (y) == UNSPEC
	  && XINT (y, 1) == UNSPEC_MACHOPIC_OFFSET)
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
  if (GET_CODE (x) == SYMBOL_REF
      && CONSTANT_POOL_ADDRESS_P (x))
    {
      rtx c = get_pool_constant (x);
      enum machine_mode cmode = get_pool_mode (x);
      if (ASM_OUTPUT_SPECIAL_POOL_ENTRY_P (c, cmode))
	return true;
    }

  return false;
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
static rtx
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
      emit_insn (gen_rtx_SET (VOIDmode, modreg, modmem));

      tmpreg = gen_reg_rtx (Pmode);
      emit_insn (gen_rtx_SET (VOIDmode, tmpreg, mem));

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
  emit_insn (gen_rtx_SET (VOIDmode, tmpreg, mem));
  set_unique_reg_note (get_last_insn (), REG_EQUAL,
		       gen_rtx_MINUS (Pmode, addr, tlsreg));

  /* Add TOC symbol value to TLS pointer.  */
  dest = force_reg (Pmode, gen_rtx_PLUS (Pmode, tmpreg, tlsreg));

  return dest;
}

/* ADDR contains a thread-local SYMBOL_REF.  Generate code to compute
   this (thread-local) address.  */

static rtx
rs6000_legitimize_tls_address (rtx addr, enum tls_model model)
{
  rtx dest, insn;

  if (TARGET_XCOFF)
    return rs6000_legitimize_tls_address_aix (addr, model);

  dest = gen_reg_rtx (Pmode);
  if (model == TLS_MODEL_LOCAL_EXEC && rs6000_tls_size == 16)
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
      rtx r3, got, tga, tmp1, tmp2, call_insn;

      /* We currently use relocations like @got@tlsgd for tls, which
	 means the linker will handle allocation of tls entries, placing
	 them in the .got section.  So use a pointer to the .got section,
	 not one to secondary TOC sections used by 64-bit -mminimal-toc,
	 or to secondary GOT sections used by 32-bit -fPIC.  */
      if (TARGET_64BIT)
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
		  rtx mem, lab, last;

		  tmp1 = gen_reg_rtx (Pmode);
		  tmp2 = gen_reg_rtx (Pmode);
		  mem = gen_const_mem (Pmode, tmp1);
		  lab = gen_label_rtx ();
		  emit_insn (gen_load_toc_v4_PIC_1b (gsym, lab));
		  emit_move_insn (tmp1, gen_rtx_REG (Pmode, LR_REGNO));
		  if (TARGET_LINK_STACK)
		    emit_insn (gen_addsi3 (tmp1, tmp1, GEN_INT (4)));
		  emit_move_insn (tmp2, mem);
		  last = emit_insn (gen_addsi3 (got, tmp1, tmp2));
		  set_unique_reg_note (last, REG_EQUAL, gsym);
		}
	    }
	}

      if (model == TLS_MODEL_GLOBAL_DYNAMIC)
	{
	  tga = rs6000_tls_get_addr ();
	  emit_library_call_value (tga, dest, LCT_CONST, Pmode,
				   1, const0_rtx, Pmode);

	  r3 = gen_rtx_REG (Pmode, 3);
	  if (DEFAULT_ABI == ABI_AIX || DEFAULT_ABI == ABI_ELFv2)
	    {
	      if (TARGET_64BIT)
		insn = gen_tls_gd_aix64 (r3, got, addr, tga, const0_rtx);
	      else
		insn = gen_tls_gd_aix32 (r3, got, addr, tga, const0_rtx);
	    }
	  else if (DEFAULT_ABI == ABI_V4)
	    insn = gen_tls_gd_sysvsi (r3, got, addr, tga, const0_rtx);
	  else
	    gcc_unreachable ();
	  call_insn = last_call_insn ();
	  PATTERN (call_insn) = insn;
	  if (DEFAULT_ABI == ABI_V4 && TARGET_SECURE_PLT && flag_pic)
	    use_reg (&CALL_INSN_FUNCTION_USAGE (call_insn),
		     pic_offset_table_rtx);
	}
      else if (model == TLS_MODEL_LOCAL_DYNAMIC)
	{
	  tga = rs6000_tls_get_addr ();
	  tmp1 = gen_reg_rtx (Pmode);
	  emit_library_call_value (tga, tmp1, LCT_CONST, Pmode,
				   1, const0_rtx, Pmode);

	  r3 = gen_rtx_REG (Pmode, 3);
	  if (DEFAULT_ABI == ABI_AIX || DEFAULT_ABI == ABI_ELFv2)
	    {
	      if (TARGET_64BIT)
		insn = gen_tls_ld_aix64 (r3, got, tga, const0_rtx);
	      else
		insn = gen_tls_ld_aix32 (r3, got, tga, const0_rtx);
	    }
	  else if (DEFAULT_ABI == ABI_V4)
	    insn = gen_tls_ld_sysvsi (r3, got, tga, const0_rtx);
	  else
	    gcc_unreachable ();
	  call_insn = last_call_insn ();
	  PATTERN (call_insn) = insn;
	  if (DEFAULT_ABI == ABI_V4 && TARGET_SECURE_PLT && flag_pic)
	    use_reg (&CALL_INSN_FUNCTION_USAGE (call_insn),
		     pic_offset_table_rtx);

	  if (rs6000_tls_size == 16)
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
	      insn = gen_rtx_SET (Pmode, dest,
				  gen_rtx_PLUS (Pmode, tmp2, tmp1));
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
	  if (TARGET_64BIT)
	    insn = gen_tls_tls_64 (dest, tmp2, addr);
	  else
	    insn = gen_tls_tls_32 (dest, tmp2, addr);
	  emit_insn (insn);
	}
    }

  return dest;
}

/* Return 1 if X contains a thread-local symbol.  */

static bool
rs6000_tls_referenced_p (rtx x)
{
  if (! TARGET_HAVE_TLS)
    return false;

  return for_each_rtx (&x, &rs6000_tls_symbol_ref_1, 0);
}

/* Implement TARGET_CANNOT_FORCE_CONST_MEM.  */

static bool
rs6000_cannot_force_const_mem (enum machine_mode mode ATTRIBUTE_UNUSED, rtx x)
{
  if (GET_CODE (x) == HIGH
      && GET_CODE (XEXP (x, 0)) == UNSPEC)
    return true;

  /* A TLS symbol in the TOC cannot contain a sum.  */
  if (GET_CODE (x) == CONST
      && GET_CODE (XEXP (x, 0)) == PLUS
      && GET_CODE (XEXP (XEXP (x, 0), 0)) == SYMBOL_REF
      && SYMBOL_REF_TLS_MODEL (XEXP (XEXP (x, 0), 0)) != 0)
    return true;

  /* Do not place an ELF TLS symbol in the constant pool.  */
  return TARGET_ELF && rs6000_tls_referenced_p (x);
}

/* Return 1 if *X is a thread-local symbol.  This is the same as
   rs6000_tls_symbol_ref except for the type of the unused argument.  */

static int
rs6000_tls_symbol_ref_1 (rtx *x, void *data ATTRIBUTE_UNUSED)
{
  return RS6000_SYMBOL_REF_TLS_P (*x);
}

/* Return true iff the given SYMBOL_REF refers to a constant pool entry
   that we have put in the TOC, or for cmodel=medium, if the SYMBOL_REF
   can be addressed relative to the toc pointer.  */

static bool
use_toc_relative_ref (rtx sym)
{
  return ((constant_pool_expr_p (sym)
	   && ASM_OUTPUT_SPECIAL_POOL_ENTRY_P (get_pool_constant (sym),
					       get_pool_mode (sym)))
	  || (TARGET_CMODEL == CMODEL_MEDIUM
	      && SYMBOL_REF_LOCAL_P (sym)));
}

/* Our implementation of LEGITIMIZE_RELOAD_ADDRESS.  Returns a value to
   replace the input X, or the original X if no replacement is called for.
   The output parameter *WIN is 1 if the calling macro should goto WIN,
   0 if it should not.

   For RS/6000, we wish to handle large displacements off a base
   register by splitting the addend across an addiu/addis and the mem insn.
   This cuts number of extra insns needed from 3 to 1.

   On Darwin, we use this to generate code for floating point constants.
   A movsf_low is generated so we wind up with 2 instructions rather than 3.
   The Darwin code is inside #if TARGET_MACHO because only then are the
   machopic_* functions defined.  */
static rtx
rs6000_legitimize_reload_address (rtx x, enum machine_mode mode,
				  int opnum, int type,
				  int ind_levels ATTRIBUTE_UNUSED, int *win)
{
  bool reg_offset_p = reg_offset_addressing_ok_p (mode);

  /* Nasty hack for vsx_splat_V2DF/V2DI load from mem, which takes a
     DFmode/DImode MEM.  */
  if (reg_offset_p
      && opnum == 1
      && ((mode == DFmode && recog_data.operand_mode[0] == V2DFmode)
	  || (mode == DImode && recog_data.operand_mode[0] == V2DImode)))
    reg_offset_p = false;

  /* We must recognize output that we have already generated ourselves.  */
  if (GET_CODE (x) == PLUS
      && GET_CODE (XEXP (x, 0)) == PLUS
      && GET_CODE (XEXP (XEXP (x, 0), 0)) == REG
      && GET_CODE (XEXP (XEXP (x, 0), 1)) == CONST_INT
      && GET_CODE (XEXP (x, 1)) == CONST_INT)
    {
      push_reload (XEXP (x, 0), NULL_RTX, &XEXP (x, 0), NULL,
		   BASE_REG_CLASS, GET_MODE (x), VOIDmode, 0, 0,
		   opnum, (enum reload_type) type);
      *win = 1;
      return x;
    }

  /* Likewise for (lo_sum (high ...) ...) output we have generated.  */
  if (GET_CODE (x) == LO_SUM
      && GET_CODE (XEXP (x, 0)) == HIGH)
    {
      push_reload (XEXP (x, 0), NULL_RTX, &XEXP (x, 0), NULL,
		   BASE_REG_CLASS, Pmode, VOIDmode, 0, 0,
		   opnum, (enum reload_type) type);
      *win = 1;
      return x;
    }

#if TARGET_MACHO
  if (DEFAULT_ABI == ABI_DARWIN && flag_pic
      && GET_CODE (x) == LO_SUM
      && GET_CODE (XEXP (x, 0)) == PLUS
      && XEXP (XEXP (x, 0), 0) == pic_offset_table_rtx
      && GET_CODE (XEXP (XEXP (x, 0), 1)) == HIGH
      && XEXP (XEXP (XEXP (x, 0), 1), 0) == XEXP (x, 1)
      && machopic_operand_p (XEXP (x, 1)))
    {
      /* Result of previous invocation of this function on Darwin
	 floating point constant.  */
      push_reload (XEXP (x, 0), NULL_RTX, &XEXP (x, 0), NULL,
		   BASE_REG_CLASS, Pmode, VOIDmode, 0, 0,
		   opnum, (enum reload_type) type);
      *win = 1;
      return x;
    }
#endif

  if (TARGET_CMODEL != CMODEL_SMALL
      && reg_offset_p
      && small_toc_ref (x, VOIDmode))
    {
      rtx hi = gen_rtx_HIGH (Pmode, copy_rtx (x));
      x = gen_rtx_LO_SUM (Pmode, hi, x);
      push_reload (XEXP (x, 0), NULL_RTX, &XEXP (x, 0), NULL,
		   BASE_REG_CLASS, Pmode, VOIDmode, 0, 0,
		   opnum, (enum reload_type) type);
      *win = 1;
      return x;
    }

  if (GET_CODE (x) == PLUS
      && GET_CODE (XEXP (x, 0)) == REG
      && REGNO (XEXP (x, 0)) < FIRST_PSEUDO_REGISTER
      && INT_REG_OK_FOR_BASE_P (XEXP (x, 0), 1)
      && GET_CODE (XEXP (x, 1)) == CONST_INT
      && reg_offset_p
      && !SPE_VECTOR_MODE (mode)
      && !(TARGET_E500_DOUBLE && GET_MODE_SIZE (mode) > UNITS_PER_WORD)
      && (!VECTOR_MODE_P (mode) || VECTOR_MEM_NONE_P (mode)))
    {
      HOST_WIDE_INT val = INTVAL (XEXP (x, 1));
      HOST_WIDE_INT low = ((val & 0xffff) ^ 0x8000) - 0x8000;
      HOST_WIDE_INT high
	= (((val - low) & 0xffffffff) ^ 0x80000000) - 0x80000000;

      /* Check for 32-bit overflow.  */
      if (high + low != val)
	{
	  *win = 0;
	  return x;
	}

      /* Reload the high part into a base reg; leave the low part
	 in the mem directly.  */

      x = gen_rtx_PLUS (GET_MODE (x),
			gen_rtx_PLUS (GET_MODE (x), XEXP (x, 0),
				      GEN_INT (high)),
			GEN_INT (low));

      push_reload (XEXP (x, 0), NULL_RTX, &XEXP (x, 0), NULL,
		   BASE_REG_CLASS, GET_MODE (x), VOIDmode, 0, 0,
		   opnum, (enum reload_type) type);
      *win = 1;
      return x;
    }

  if (GET_CODE (x) == SYMBOL_REF
      && reg_offset_p
      && (!VECTOR_MODE_P (mode) || VECTOR_MEM_NONE_P (mode))
      && !SPE_VECTOR_MODE (mode)
#if TARGET_MACHO
      && DEFAULT_ABI == ABI_DARWIN
      && (flag_pic || MACHO_DYNAMIC_NO_PIC_P)
      && machopic_symbol_defined_p (x)
#else
      && DEFAULT_ABI == ABI_V4
      && !flag_pic
#endif
      /* Don't do this for TFmode or TDmode, since the result isn't offsettable.
	 The same goes for DImode without 64-bit gprs and DFmode and DDmode
	 without fprs.
	 ??? Assume floating point reg based on mode?  This assumption is
	 violated by eg. powerpc-linux -m32 compile of gcc.dg/pr28796-2.c
	 where reload ends up doing a DFmode load of a constant from
	 mem using two gprs.  Unfortunately, at this point reload
	 hasn't yet selected regs so poking around in reload data
	 won't help and even if we could figure out the regs reliably,
	 we'd still want to allow this transformation when the mem is
	 naturally aligned.  Since we say the address is good here, we
	 can't disable offsets from LO_SUMs in mem_operand_gpr.
	 FIXME: Allow offset from lo_sum for other modes too, when
	 mem is sufficiently aligned.  */
      && mode != TFmode
      && mode != TDmode
      && (mode != TImode || !TARGET_VSX_TIMODE)
      && mode != PTImode
      && (mode != DImode || TARGET_POWERPC64)
      && ((mode != DFmode && mode != DDmode) || TARGET_POWERPC64
	  || (TARGET_HARD_FLOAT && TARGET_FPRS && TARGET_DOUBLE_FLOAT)))
    {
#if TARGET_MACHO
      if (flag_pic)
	{
	  rtx offset = machopic_gen_offset (x);
	  x = gen_rtx_LO_SUM (GET_MODE (x),
		gen_rtx_PLUS (Pmode, pic_offset_table_rtx,
		  gen_rtx_HIGH (Pmode, offset)), offset);
	}
      else
#endif
	x = gen_rtx_LO_SUM (GET_MODE (x),
	      gen_rtx_HIGH (Pmode, x), x);

      push_reload (XEXP (x, 0), NULL_RTX, &XEXP (x, 0), NULL,
		   BASE_REG_CLASS, Pmode, VOIDmode, 0, 0,
		   opnum, (enum reload_type) type);
      *win = 1;
      return x;
    }

  /* Reload an offset address wrapped by an AND that represents the
     masking of the lower bits.  Strip the outer AND and let reload
     convert the offset address into an indirect address.  For VSX,
     force reload to create the address with an AND in a separate
     register, because we can't guarantee an altivec register will
     be used.  */
  if (VECTOR_MEM_ALTIVEC_P (mode)
      && GET_CODE (x) == AND
      && GET_CODE (XEXP (x, 0)) == PLUS
      && GET_CODE (XEXP (XEXP (x, 0), 0)) == REG
      && GET_CODE (XEXP (XEXP (x, 0), 1)) == CONST_INT
      && GET_CODE (XEXP (x, 1)) == CONST_INT
      && INTVAL (XEXP (x, 1)) == -16)
    {
      x = XEXP (x, 0);
      *win = 1;
      return x;
    }

  if (TARGET_TOC
      && reg_offset_p
      && GET_CODE (x) == SYMBOL_REF
      && use_toc_relative_ref (x))
    {
      x = create_TOC_reference (x, NULL_RTX);
      if (TARGET_CMODEL != CMODEL_SMALL)
	push_reload (XEXP (x, 0), NULL_RTX, &XEXP (x, 0), NULL,
		     BASE_REG_CLASS, Pmode, VOIDmode, 0, 0,
		     opnum, (enum reload_type) type);
      *win = 1;
      return x;
    }
  *win = 0;
  return x;
}

/* Debug version of rs6000_legitimize_reload_address.  */
static rtx
rs6000_debug_legitimize_reload_address (rtx x, enum machine_mode mode,
					int opnum, int type,
					int ind_levels, int *win)
{
  rtx ret = rs6000_legitimize_reload_address (x, mode, opnum, type,
					      ind_levels, win);
  fprintf (stderr,
	   "\nrs6000_legitimize_reload_address: mode = %s, opnum = %d, "
	   "type = %d, ind_levels = %d, win = %d, original addr:\n",
	   GET_MODE_NAME (mode), opnum, type, ind_levels, *win);
  debug_rtx (x);

  if (x == ret)
    fprintf (stderr, "Same address returned\n");
  else if (!ret)
    fprintf (stderr, "NULL returned\n");
  else
    {
      fprintf (stderr, "New address:\n");
      debug_rtx (ret);
    }

  return ret;
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
rs6000_legitimate_address_p (enum machine_mode mode, rtx x, bool reg_ok_strict)
{
  bool reg_offset_p = reg_offset_addressing_ok_p (mode);

  /* If this is an unaligned stvx/ldvx type address, discard the outer AND.  */
  if (VECTOR_MEM_ALTIVEC_P (mode)
      && GET_CODE (x) == AND
      && GET_CODE (XEXP (x, 1)) == CONST_INT
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
  if (virtual_stack_registers_memory_p (x))
    return 1;
  if (reg_offset_p && legitimate_small_data_p (mode, x))
    return 1;
  if (reg_offset_p
      && legitimate_constant_pool_address_p (x, mode,
					     reg_ok_strict || lra_in_progress))
    return 1;
  /* For TImode, if we have load/store quad and TImode in VSX registers, only
     allow register indirect addresses.  This will allow the values to go in
     either GPRs or VSX registers without reloading.  The vector types would
     tend to go into VSX registers, so we allow REG+REG, while TImode seems
     somewhat split, in that some uses are GPR based, and some VSX based.  */
  if (mode == TImode && TARGET_QUAD_MEMORY && TARGET_VSX_TIMODE)
    return 0;
  /* If not REG_OK_STRICT (before reload) let pass any stack offset.  */
  if (! reg_ok_strict
      && reg_offset_p
      && GET_CODE (x) == PLUS
      && GET_CODE (XEXP (x, 0)) == REG
      && (XEXP (x, 0) == virtual_stack_vars_rtx
	  || XEXP (x, 0) == arg_pointer_rtx)
      && GET_CODE (XEXP (x, 1)) == CONST_INT)
    return 1;
  if (rs6000_legitimate_offset_address_p (mode, x, reg_ok_strict, false))
    return 1;
  if (mode != TFmode
      && mode != TDmode
      && ((TARGET_HARD_FLOAT && TARGET_FPRS && TARGET_DOUBLE_FLOAT)
	  || TARGET_POWERPC64
	  || (mode != DFmode && mode != DDmode)
	  || (TARGET_E500_DOUBLE && mode != DDmode))
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
    return 1;
  if (reg_offset_p && legitimate_lo_sum_address_p (mode, x, reg_ok_strict))
    return 1;
  return 0;
}

/* Debug version of rs6000_legitimate_address_p.  */
static bool
rs6000_debug_legitimate_address_p (enum machine_mode mode, rtx x,
				   bool reg_ok_strict)
{
  bool ret = rs6000_legitimate_address_p (mode, x, reg_ok_strict);
  fprintf (stderr,
	   "\nrs6000_legitimate_address_p: return = %s, mode = %s, "
	   "strict = %d, reload = %s, code = %s\n",
	   ret ? "true" : "false",
	   GET_MODE_NAME (mode),
	   reg_ok_strict,
	   (reload_completed
	    ? "after"
	    : (reload_in_progress ? "progress" : "before")),
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
	  && GET_CODE (XEXP (addr, 1)) == CONST_INT)
	{
	  unsigned HOST_WIDE_INT val = INTVAL (XEXP (addr, 1));
	  return val + 0x8000 >= 0x10000 - (TARGET_POWERPC64 ? 8 : 12);
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
rs6000_offsettable_memref_p (rtx op, enum machine_mode reg_mode)
{
  bool worst_case;

  if (!MEM_P (op))
    return false;

  /* First mimic offsettable_memref_p.  */
  if (offsettable_address_p (true, GET_MODE (op), XEXP (op, 0)))
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
					     true, worst_case);
}

/* Change register usage conditional on target flags.  */
static void
rs6000_conditional_register_usage (void)
{
  int i;

  if (TARGET_DEBUG_TARGET)
    fprintf (stderr, "rs6000_conditional_register_usage called\n");

  /* Set MQ register fixed (already call_used) so that it will not be
     allocated.  */
  fixed_regs[64] = 1;

  /* 64-bit AIX and Linux reserve GPR13 for thread-private data.  */
  if (TARGET_64BIT)
    fixed_regs[13] = call_used_regs[13]
      = call_really_used_regs[13] = 1;

  /* Conditionally disable FPRs.  */
  if (TARGET_SOFT_FLOAT || !TARGET_FPRS)
    for (i = 32; i < 64; i++)
      fixed_regs[i] = call_used_regs[i]
	= call_really_used_regs[i] = 1;

  /* The TOC register is not killed across calls in a way that is
     visible to the compiler.  */
  if (DEFAULT_ABI == ABI_AIX || DEFAULT_ABI == ABI_ELFv2)
    call_really_used_regs[2] = 0;

  if (DEFAULT_ABI == ABI_V4
      && PIC_OFFSET_TABLE_REGNUM != INVALID_REGNUM
      && flag_pic == 2)
    fixed_regs[RS6000_PIC_OFFSET_TABLE_REGNUM] = 1;

  if (DEFAULT_ABI == ABI_V4
      && PIC_OFFSET_TABLE_REGNUM != INVALID_REGNUM
      && flag_pic == 1)
    fixed_regs[RS6000_PIC_OFFSET_TABLE_REGNUM]
      = call_used_regs[RS6000_PIC_OFFSET_TABLE_REGNUM]
      = call_really_used_regs[RS6000_PIC_OFFSET_TABLE_REGNUM] = 1;

  if (DEFAULT_ABI == ABI_DARWIN
      && PIC_OFFSET_TABLE_REGNUM != INVALID_REGNUM)
      fixed_regs[RS6000_PIC_OFFSET_TABLE_REGNUM]
      = call_used_regs[RS6000_PIC_OFFSET_TABLE_REGNUM]
      = call_really_used_regs[RS6000_PIC_OFFSET_TABLE_REGNUM] = 1;

  if (TARGET_TOC && TARGET_MINIMAL_TOC)
    fixed_regs[RS6000_PIC_OFFSET_TABLE_REGNUM]
      = call_used_regs[RS6000_PIC_OFFSET_TABLE_REGNUM] = 1;

  if (TARGET_SPE)
    {
      global_regs[SPEFSCR_REGNO] = 1;
      /* We used to use r14 as FIXED_SCRATCH to address SPE 64-bit
         registers in prologues and epilogues.  We no longer use r14
         for FIXED_SCRATCH, but we're keeping r14 out of the allocation
         pool for link-compatibility with older versions of GCC.  Once
         "old" code has died out, we can return r14 to the allocation
         pool.  */
      fixed_regs[14]
	= call_used_regs[14]
	= call_really_used_regs[14] = 1;
    }

  if (!TARGET_ALTIVEC && !TARGET_VSX)
    {
      for (i = FIRST_ALTIVEC_REGNO; i <= LAST_ALTIVEC_REGNO; ++i)
	fixed_regs[i] = call_used_regs[i] = call_really_used_regs[i] = 1;
      call_really_used_regs[VRSAVE_REGNO] = 1;
    }

  if (TARGET_ALTIVEC || TARGET_VSX)
    global_regs[VSCR_REGNO] = 1;

  if (TARGET_ALTIVEC_ABI)
    {
      for (i = FIRST_ALTIVEC_REGNO; i < FIRST_ALTIVEC_REGNO + 20; ++i)
	call_used_regs[i] = call_really_used_regs[i] = 1;

      /* AIX reserves VR20:31 in non-extended ABI mode.  */
      if (TARGET_XCOFF)
	for (i = FIRST_ALTIVEC_REGNO + 20; i < FIRST_ALTIVEC_REGNO + 32; ++i)
	  fixed_regs[i] = call_used_regs[i] = call_really_used_regs[i] = 1;
    }
}


/* Try to output insns to set TARGET equal to the constant C if it can
   be done in less than N insns.  Do all computations in MODE.
   Returns the place where the output has been placed if it can be
   done and the insns have been emitted.  If it would take more than N
   insns, zero is returned and no insns and emitted.  */

rtx
rs6000_emit_set_const (rtx dest, enum machine_mode mode,
		       rtx source, int n ATTRIBUTE_UNUSED)
{
  rtx result, insn, set;
  HOST_WIDE_INT c0, c1;

  switch (mode)
    {
    case  QImode:
    case HImode:
      if (dest == NULL)
	dest = gen_reg_rtx (mode);
      emit_insn (gen_rtx_SET (VOIDmode, dest, source));
      return dest;

    case SImode:
      result = !can_create_pseudo_p () ? dest : gen_reg_rtx (SImode);

      emit_insn (gen_rtx_SET (VOIDmode, copy_rtx (result),
			      GEN_INT (INTVAL (source)
				       & (~ (HOST_WIDE_INT) 0xffff))));
      emit_insn (gen_rtx_SET (VOIDmode, dest,
			      gen_rtx_IOR (SImode, copy_rtx (result),
					   GEN_INT (INTVAL (source) & 0xffff))));
      result = dest;
      break;

    case DImode:
      switch (GET_CODE (source))
	{
	case CONST_INT:
	  c0 = INTVAL (source);
	  c1 = -(c0 < 0);
	  break;

	default:
	  gcc_unreachable ();
	}

      result = rs6000_emit_set_long_const (dest, c0, c1);
      break;

    default:
      gcc_unreachable ();
    }

  insn = get_last_insn ();
  set = single_set (insn);
  if (! CONSTANT_P (SET_SRC (set)))
    set_unique_reg_note (insn, REG_EQUAL, source);

  return result;
}

/* Having failed to find a 3 insn sequence in rs6000_emit_set_const,
   fall back to a straight forward decomposition.  We do this to avoid
   exponential run times encountered when looking for longer sequences
   with rs6000_emit_set_const.  */
static rtx
rs6000_emit_set_long_const (rtx dest, HOST_WIDE_INT c1, HOST_WIDE_INT c2)
{
  if (!TARGET_POWERPC64)
    {
      rtx operand1, operand2;

      operand1 = operand_subword_force (dest, WORDS_BIG_ENDIAN == 0,
					DImode);
      operand2 = operand_subword_force (copy_rtx (dest), WORDS_BIG_ENDIAN != 0,
					DImode);
      emit_move_insn (operand1, GEN_INT (c1));
      emit_move_insn (operand2, GEN_INT (c2));
    }
  else
    {
      HOST_WIDE_INT ud1, ud2, ud3, ud4;

      ud1 = c1 & 0xffff;
      ud2 = (c1 & 0xffff0000) >> 16;
      c2 = c1 >> 32;
      ud3 = c2 & 0xffff;
      ud4 = (c2 & 0xffff0000) >> 16;

      if ((ud4 == 0xffff && ud3 == 0xffff && ud2 == 0xffff && (ud1 & 0x8000))
	  || (ud4 == 0 && ud3 == 0 && ud2 == 0 && ! (ud1 & 0x8000)))
	emit_move_insn (dest, GEN_INT ((ud1 ^ 0x8000) - 0x8000));

      else if ((ud4 == 0xffff && ud3 == 0xffff && (ud2 & 0x8000))
	       || (ud4 == 0 && ud3 == 0 && ! (ud2 & 0x8000)))
	{
	  emit_move_insn (dest, GEN_INT (((ud2 << 16) ^ 0x80000000)
					 - 0x80000000));
	  if (ud1 != 0)
	    emit_move_insn (copy_rtx (dest),
			    gen_rtx_IOR (DImode, copy_rtx (dest),
					 GEN_INT (ud1)));
	}
      else if (ud3 == 0 && ud4 == 0)
	{
	  gcc_assert (ud2 & 0x8000);
	  emit_move_insn (dest, GEN_INT (((ud2 << 16) ^ 0x80000000)
					 - 0x80000000));
	  if (ud1 != 0)
	    emit_move_insn (copy_rtx (dest),
			    gen_rtx_IOR (DImode, copy_rtx (dest),
					 GEN_INT (ud1)));
	  emit_move_insn (copy_rtx (dest),
			  gen_rtx_ZERO_EXTEND (DImode,
					       gen_lowpart (SImode,
							    copy_rtx (dest))));
	}
      else if ((ud4 == 0xffff && (ud3 & 0x8000))
	       || (ud4 == 0 && ! (ud3 & 0x8000)))
	{
	  emit_move_insn (dest, GEN_INT (((ud3 << 16) ^ 0x80000000)
					 - 0x80000000));
	  if (ud2 != 0)
	    emit_move_insn (copy_rtx (dest),
			    gen_rtx_IOR (DImode, copy_rtx (dest),
					 GEN_INT (ud2)));
	  emit_move_insn (copy_rtx (dest),
			  gen_rtx_ASHIFT (DImode, copy_rtx (dest),
					  GEN_INT (16)));
	  if (ud1 != 0)
	    emit_move_insn (copy_rtx (dest),
			    gen_rtx_IOR (DImode, copy_rtx (dest),
					 GEN_INT (ud1)));
	}
      else
	{
	  emit_move_insn (dest, GEN_INT (((ud4 << 16) ^ 0x80000000)
					 - 0x80000000));
	  if (ud3 != 0)
	    emit_move_insn (copy_rtx (dest),
			    gen_rtx_IOR (DImode, copy_rtx (dest),
					 GEN_INT (ud3)));

	  emit_move_insn (copy_rtx (dest),
			  gen_rtx_ASHIFT (DImode, copy_rtx (dest),
					  GEN_INT (32)));
	  if (ud2 != 0)
	    emit_move_insn (copy_rtx (dest),
			    gen_rtx_IOR (DImode, copy_rtx (dest),
					 GEN_INT (ud2 << 16)));
	  if (ud1 != 0)
	    emit_move_insn (copy_rtx (dest),
			    gen_rtx_IOR (DImode, copy_rtx (dest),
					 GEN_INT (ud1)));
	}
    }
  return dest;
}

/* Helper for the following.  Get rid of [r+r] memory refs
   in cases where it won't work (TImode, TFmode, TDmode, PTImode).  */

static void
rs6000_eliminate_indexed_memrefs (rtx operands[2])
{
  if (reload_in_progress)
    return;

  if (GET_CODE (operands[0]) == MEM
      && GET_CODE (XEXP (operands[0], 0)) != REG
      && ! legitimate_constant_pool_address_p (XEXP (operands[0], 0),
					       GET_MODE (operands[0]), false))
    operands[0]
      = replace_equiv_address (operands[0],
			       copy_addr_to_reg (XEXP (operands[0], 0)));

  if (GET_CODE (operands[1]) == MEM
      && GET_CODE (XEXP (operands[1], 0)) != REG
      && ! legitimate_constant_pool_address_p (XEXP (operands[1], 0),
					       GET_MODE (operands[1]), false))
    operands[1]
      = replace_equiv_address (operands[1],
			       copy_addr_to_reg (XEXP (operands[1], 0)));
}

/* Generate a vector of constants to permute MODE for a little-endian
   storage operation by swapping the two halves of a vector.  */
static rtvec
rs6000_const_vec (enum machine_mode mode)
{
  int i, subparts;
  rtvec v;

  switch (mode)
    {
    case V2DFmode:
    case V2DImode:
      subparts = 2;
      break;
    case V4SFmode:
    case V4SImode:
      subparts = 4;
      break;
    case V8HImode:
      subparts = 8;
      break;
    case V16QImode:
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

/* Generate a permute rtx that represents an lxvd2x, stxvd2x, or xxpermdi
   for a VSX load or store operation.  */
rtx
rs6000_gen_le_vsx_permute (rtx source, enum machine_mode mode)
{
  rtx par = gen_rtx_PARALLEL (VOIDmode, rs6000_const_vec (mode));
  return gen_rtx_VEC_SELECT (mode, source, par);
}

/* Emit a little-endian load from vector memory location SOURCE to VSX
   register DEST in mode MODE.  The load is done with two permuting
   insn's that represent an lxvd2x and xxpermdi.  */
void
rs6000_emit_le_vsx_load (rtx dest, rtx source, enum machine_mode mode)
{
  rtx tmp = can_create_pseudo_p () ? gen_reg_rtx_and_attrs (dest) : dest;
  rtx permute_mem = rs6000_gen_le_vsx_permute (source, mode);
  rtx permute_reg = rs6000_gen_le_vsx_permute (tmp, mode);
  emit_insn (gen_rtx_SET (VOIDmode, tmp, permute_mem));
  emit_insn (gen_rtx_SET (VOIDmode, dest, permute_reg));
}

/* Emit a little-endian store to vector memory location DEST from VSX
   register SOURCE in mode MODE.  The store is done with two permuting
   insn's that represent an xxpermdi and an stxvd2x.  */
void
rs6000_emit_le_vsx_store (rtx dest, rtx source, enum machine_mode mode)
{
  rtx tmp = can_create_pseudo_p () ? gen_reg_rtx_and_attrs (source) : source;
  rtx permute_src = rs6000_gen_le_vsx_permute (source, mode);
  rtx permute_tmp = rs6000_gen_le_vsx_permute (tmp, mode);
  emit_insn (gen_rtx_SET (VOIDmode, tmp, permute_src));
  emit_insn (gen_rtx_SET (VOIDmode, dest, permute_tmp));
}

/* Emit a sequence representing a little-endian VSX load or store,
   moving data from SOURCE to DEST in mode MODE.  This is done
   separately from rs6000_emit_move to ensure it is called only
   during expand.  LE VSX loads and stores introduced later are
   handled with a split.  The expand-time RTL generation allows
   us to optimize away redundant pairs of register-permutes.  */
void
rs6000_emit_le_vsx_move (rtx dest, rtx source, enum machine_mode mode)
{
  gcc_assert (!BYTES_BIG_ENDIAN
	      && VECTOR_MEM_VSX_P (mode)
	      && mode != TImode
	      && !gpr_or_gpr_p (dest, source)
	      && (MEM_P (source) ^ MEM_P (dest)));

  if (MEM_P (source))
    {
      gcc_assert (REG_P (dest));
      rs6000_emit_le_vsx_load (dest, source, mode);
    }
  else
    {
      if (!REG_P (source))
	source = force_reg (mode, source);
      rs6000_emit_le_vsx_store (dest, source, mode);
    }
}

/* Emit a move from SOURCE to DEST in mode MODE.  */
void
rs6000_emit_move (rtx dest, rtx source, enum machine_mode mode)
{
  rtx operands[2];
  operands[0] = dest;
  operands[1] = source;

  if (TARGET_DEBUG_ADDR)
    {
      fprintf (stderr,
	       "\nrs6000_emit_move: mode = %s, reload_in_progress = %d, "
	       "reload_completed = %d, can_create_pseudos = %d.\ndest:\n",
	       GET_MODE_NAME (mode),
	       reload_in_progress,
	       reload_completed,
	       can_create_pseudo_p ());
      debug_rtx (dest);
      fprintf (stderr, "source:\n");
      debug_rtx (source);
    }

  /* Sanity checks.  Check that we get CONST_DOUBLE only when we should.  */
  if (GET_CODE (operands[1]) == CONST_DOUBLE
      && ! FLOAT_MODE_P (mode)
      && GET_MODE_BITSIZE (mode) <= HOST_BITS_PER_WIDE_INT)
    {
      /* FIXME.  This should never happen.  */
      /* Since it seems that it does, do the safe thing and convert
	 to a CONST_INT.  */
      operands[1] = gen_int_mode (CONST_DOUBLE_LOW (operands[1]), mode);
    }
  gcc_assert (GET_CODE (operands[1]) != CONST_DOUBLE
	      || FLOAT_MODE_P (mode)
	      || ((CONST_DOUBLE_HIGH (operands[1]) != 0
		   || CONST_DOUBLE_LOW (operands[1]) < 0)
		  && (CONST_DOUBLE_HIGH (operands[1]) != -1
		      || CONST_DOUBLE_LOW (operands[1]) >= 0)));

  /* Check if GCC is setting up a block move that will end up using FP
     registers as temporaries.  We must make sure this is acceptable.  */
  if (GET_CODE (operands[0]) == MEM
      && GET_CODE (operands[1]) == MEM
      && mode == DImode
      && (SLOW_UNALIGNED_ACCESS (DImode, MEM_ALIGN (operands[0]))
	  || SLOW_UNALIGNED_ACCESS (DImode, MEM_ALIGN (operands[1])))
      && ! (SLOW_UNALIGNED_ACCESS (SImode, (MEM_ALIGN (operands[0]) > 32
					    ? 32 : MEM_ALIGN (operands[0])))
	    || SLOW_UNALIGNED_ACCESS (SImode, (MEM_ALIGN (operands[1]) > 32
					       ? 32
					       : MEM_ALIGN (operands[1]))))
      && ! MEM_VOLATILE_P (operands [0])
      && ! MEM_VOLATILE_P (operands [1]))
    {
      emit_move_insn (adjust_address (operands[0], SImode, 0),
		      adjust_address (operands[1], SImode, 0));
      emit_move_insn (adjust_address (copy_rtx (operands[0]), SImode, 4),
		      adjust_address (copy_rtx (operands[1]), SImode, 4));
      return;
    }

  if (can_create_pseudo_p () && GET_CODE (operands[0]) == MEM
      && !gpc_reg_operand (operands[1], mode))
    operands[1] = force_reg (mode, operands[1]);

  /* Recognize the case where operand[1] is a reference to thread-local
     data and load its address to a register.  */
  if (rs6000_tls_referenced_p (operands[1]))
    {
      enum tls_model model;
      rtx tmp = operands[1];
      rtx addend = NULL;

      if (GET_CODE (tmp) == CONST && GET_CODE (XEXP (tmp, 0)) == PLUS)
	{
          addend = XEXP (XEXP (tmp, 0), 1);
	  tmp = XEXP (XEXP (tmp, 0), 0);
	}

      gcc_assert (GET_CODE (tmp) == SYMBOL_REF);
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

  /* Handle the case where reload calls us with an invalid address.  */
  if (reload_in_progress && mode == Pmode
      && (! general_operand (operands[1], mode)
	  || ! nonimmediate_operand (operands[0], mode)))
    goto emit_set;

  /* 128-bit constant floating-point values on Darwin should really be
     loaded as two parts.  */
  if (!TARGET_IEEEQUAD && TARGET_LONG_DOUBLE_128
      && mode == TFmode && GET_CODE (operands[1]) == CONST_DOUBLE)
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

  if (reload_in_progress && cfun->machine->sdmode_stack_slot != NULL_RTX)
    cfun->machine->sdmode_stack_slot =
      eliminate_regs (cfun->machine->sdmode_stack_slot, VOIDmode, NULL_RTX);


  if (lra_in_progress
      && mode == SDmode
      && REG_P (operands[0]) && REGNO (operands[0]) >= FIRST_PSEUDO_REGISTER
      && reg_preferred_class (REGNO (operands[0])) == NO_REGS
      && (REG_P (operands[1])
	  || (GET_CODE (operands[1]) == SUBREG
	      && REG_P (SUBREG_REG (operands[1])))))
    {
      int regno = REGNO (GET_CODE (operands[1]) == SUBREG
			 ? SUBREG_REG (operands[1]) : operands[1]);
      enum reg_class cl;

      if (regno >= FIRST_PSEUDO_REGISTER)
	{
	  cl = reg_preferred_class (regno);
	  gcc_assert (cl != NO_REGS);
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
  if (lra_in_progress
      && mode == SDmode
      && (REG_P (operands[0])
	  || (GET_CODE (operands[0]) == SUBREG
	      && REG_P (SUBREG_REG (operands[0]))))
      && REG_P (operands[1]) && REGNO (operands[1]) >= FIRST_PSEUDO_REGISTER
      && reg_preferred_class (REGNO (operands[1])) == NO_REGS)
    {
      int regno = REGNO (GET_CODE (operands[0]) == SUBREG
			 ? SUBREG_REG (operands[0]) : operands[0]);
      enum reg_class cl;

      if (regno >= FIRST_PSEUDO_REGISTER)
	{
	  cl = reg_preferred_class (regno);
	  gcc_assert (cl != NO_REGS);
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

  if (reload_in_progress
      && mode == SDmode
      && cfun->machine->sdmode_stack_slot != NULL_RTX
      && MEM_P (operands[0])
      && rtx_equal_p (operands[0], cfun->machine->sdmode_stack_slot)
      && REG_P (operands[1]))
    {
      if (FP_REGNO_P (REGNO (operands[1])))
	{
	  rtx mem = adjust_address_nv (operands[0], DDmode, 0);
	  mem = eliminate_regs (mem, VOIDmode, NULL_RTX);
	  emit_insn (gen_movsd_store (mem, operands[1]));
	}
      else if (INT_REGNO_P (REGNO (operands[1])))
	{
	  rtx mem = operands[0];
	  if (BYTES_BIG_ENDIAN)
	    mem = adjust_address_nv (mem, mode, 4);
	  mem = eliminate_regs (mem, VOIDmode, NULL_RTX);
	  emit_insn (gen_movsd_hardfloat (mem, operands[1]));
	}
      else
	gcc_unreachable();
      return;
    }
  if (reload_in_progress
      && mode == SDmode
      && REG_P (operands[0])
      && MEM_P (operands[1])
      && cfun->machine->sdmode_stack_slot != NULL_RTX
      && rtx_equal_p (operands[1], cfun->machine->sdmode_stack_slot))
    {
      if (FP_REGNO_P (REGNO (operands[0])))
	{
	  rtx mem = adjust_address_nv (operands[1], DDmode, 0);
	  mem = eliminate_regs (mem, VOIDmode, NULL_RTX);
	  emit_insn (gen_movsd_load (operands[0], mem));
	}
      else if (INT_REGNO_P (REGNO (operands[0])))
	{
	  rtx mem = operands[1];
	  if (BYTES_BIG_ENDIAN)
	    mem = adjust_address_nv (mem, mode, 4);
	  mem = eliminate_regs (mem, VOIDmode, NULL_RTX);
	  emit_insn (gen_movsd_hardfloat (operands[0], mem));
	}
      else
	gcc_unreachable();
      return;
    }

  /* FIXME:  In the long term, this switch statement should go away
     and be replaced by a sequence of tests based on things like
     mode == Pmode.  */
  switch (mode)
    {
    case HImode:
    case QImode:
      if (CONSTANT_P (operands[1])
	  && GET_CODE (operands[1]) != CONST_INT)
	operands[1] = force_const_mem (mode, operands[1]);
      break;

    case TFmode:
    case TDmode:
      rs6000_eliminate_indexed_memrefs (operands);
      /* fall through */

    case DFmode:
    case DDmode:
    case SFmode:
    case SDmode:
      if (CONSTANT_P (operands[1])
	  && ! easy_fp_constant (operands[1], mode))
	operands[1] = force_const_mem (mode, operands[1]);
      break;

    case V16QImode:
    case V8HImode:
    case V4SFmode:
    case V4SImode:
    case V4HImode:
    case V2SFmode:
    case V2SImode:
    case V1DImode:
    case V2DFmode:
    case V2DImode:
      if (CONSTANT_P (operands[1])
	  && !easy_vector_constant (operands[1], mode))
	operands[1] = force_const_mem (mode, operands[1]);
      break;

    case SImode:
    case DImode:
      /* Use default pattern for address of ELF small data */
      if (TARGET_ELF
	  && mode == Pmode
	  && DEFAULT_ABI == ABI_V4
	  && (GET_CODE (operands[1]) == SYMBOL_REF
	      || GET_CODE (operands[1]) == CONST)
	  && small_data_operand (operands[1], mode))
	{
	  emit_insn (gen_rtx_SET (VOIDmode, operands[0], operands[1]));
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
	  && TARGET_NO_TOC
	  && ! flag_pic
	  && mode == Pmode
	  && CONSTANT_P (operands[1])
	  && GET_CODE (operands[1]) != HIGH
	  && GET_CODE (operands[1]) != CONST_INT)
	{
	  rtx target = (!can_create_pseudo_p ()
			? operands[0]
			: gen_reg_rtx (mode));

	  /* If this is a function address on -mcall-aixdesc,
	     convert it to the address of the descriptor.  */
	  if (DEFAULT_ABI == ABI_AIX
	      && GET_CODE (operands[1]) == SYMBOL_REF
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
	      if (MACHO_DYNAMIC_NO_PIC_P)
		{
		  /* Take care of any required data indirection.  */
		  operands[1] = rs6000_machopic_legitimize_pic_address (
				  operands[1], mode, operands[0]);
		  if (operands[0] != operands[1])
		    emit_insn (gen_rtx_SET (VOIDmode,
					    operands[0], operands[1]));
		  return;
		}
#endif
	      emit_insn (gen_macho_high (target, operands[1]));
	      emit_insn (gen_macho_low (operands[0], target, operands[1]));
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
	  && GET_CODE (operands[1]) == SYMBOL_REF
	  && use_toc_relative_ref (operands[1]))
	operands[1] = create_TOC_reference (operands[1], operands[0]);
      else if (mode == Pmode
	       && CONSTANT_P (operands[1])
	       && GET_CODE (operands[1]) != HIGH
	       && ((GET_CODE (operands[1]) != CONST_INT
		    && ! easy_fp_constant (operands[1], mode))
		   || (GET_CODE (operands[1]) == CONST_INT
		       && (num_insns_constant (operands[1], mode)
			   > (TARGET_CMODEL != CMODEL_SMALL ? 3 : 2)))
		   || (GET_CODE (operands[0]) == REG
		       && FP_REGNO_P (REGNO (operands[0]))))
	       && !toc_relative_expr_p (operands[1], false)
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
		emit_insn (gen_rtx_SET (VOIDmode, operands[0], operands[1]));
	      return;
	    }
#endif

	  /* If we are to limit the number of things we put in the TOC and
	     this is a symbol plus a constant we can add in one insn,
	     just put the symbol in the TOC and add the constant.  Don't do
	     this if reload is in progress.  */
	  if (GET_CODE (operands[1]) == CONST
	      && TARGET_NO_SUM_IN_TOC && ! reload_in_progress
	      && GET_CODE (XEXP (operands[1], 0)) == PLUS
	      && add_operand (XEXP (XEXP (operands[1], 0), 1), mode)
	      && (GET_CODE (XEXP (XEXP (operands[1], 0), 0)) == LABEL_REF
		  || GET_CODE (XEXP (XEXP (operands[1], 0), 0)) == SYMBOL_REF)
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
	      && GET_CODE (XEXP (operands[1], 0)) == SYMBOL_REF
	      && constant_pool_expr_p (XEXP (operands[1], 0))
	      && ASM_OUTPUT_SPECIAL_POOL_ENTRY_P (
			get_pool_constant (XEXP (operands[1], 0)),
			get_pool_mode (XEXP (operands[1], 0))))
	    {
	      rtx tocref = create_TOC_reference (XEXP (operands[1], 0),
						 operands[0]);
	      operands[1] = gen_const_mem (mode, tocref);
	      set_mem_alias_set (operands[1], get_TOC_alias_set ());
	    }
	}
      break;

    case TImode:
      if (!VECTOR_MEM_VSX_P (TImode))
	rs6000_eliminate_indexed_memrefs (operands);
      break;

    case PTImode:
      rs6000_eliminate_indexed_memrefs (operands);
      break;

    default:
      fatal_insn ("bad move", gen_rtx_SET (VOIDmode, dest, source));
    }

  /* Above, we may have called force_const_mem which may have returned
     an invalid address.  If we can, fix this up; otherwise, reload will
     have to deal with it.  */
  if (GET_CODE (operands[1]) == MEM && ! reload_in_progress)
    operands[1] = validize_mem (operands[1]);

 emit_set:
  emit_insn (gen_rtx_SET (VOIDmode, operands[0], operands[1]));
}

/* Return true if a structure, union or array containing FIELD should be
   accessed using `BLKMODE'.

   For the SPE, simd types are V2SI, and gcc can be tempted to put the
   entire thing in a DI and use subregs to access the internals.
   store_bit_field() will force (subreg:DI (reg:V2SI x))'s to the
   back-end.  Because a single GPR can hold a V2SI, but not a DI, the
   best thing to do is set structs to BLKmode and avoid Severe Tire
   Damage.

   On e500 v2, DF and DI modes suffer from the same anomaly.  DF can
   fit into 1, whereas DI still needs two.  */

static bool
rs6000_member_type_forces_blk (const_tree field, enum machine_mode mode)
{
  return ((TARGET_SPE && TREE_CODE (TREE_TYPE (field)) == VECTOR_TYPE)
	  || (TARGET_E500_DOUBLE && mode == DFmode));
}

/* Nonzero if we can use a floating-point register to pass this arg.  */
#define USE_FP_FOR_ARG_P(CUM,MODE)		\
  (SCALAR_FLOAT_MODE_P (MODE)			\
   && (CUM)->fregno <= FP_ARG_MAX_REG		\
   && TARGET_HARD_FLOAT && TARGET_FPRS)

/* Nonzero if we can use an AltiVec register to pass this arg.  */
#define USE_ALTIVEC_FOR_ARG_P(CUM,MODE,NAMED)			\
  (ALTIVEC_OR_VSX_VECTOR_MODE (MODE)				\
   && (CUM)->vregno <= ALTIVEC_ARG_MAX_REG			\
   && TARGET_ALTIVEC_ABI					\
   && (NAMED))

/* Walk down the type tree of TYPE counting consecutive base elements.
   If *MODEP is VOIDmode, then set it to the first valid floating point
   or vector type.  If a non-floating point or vector type is found, or
   if a floating point or vector type that doesn't match a non-VOIDmode
   *MODEP is found, then return -1, otherwise return the count in the
   sub-tree.  */

static int
rs6000_aggregate_candidate (const_tree type, enum machine_mode *modep)
{
  enum machine_mode mode;
  HOST_WIDE_INT size;

  switch (TREE_CODE (type))
    {
    case REAL_TYPE:
      mode = TYPE_MODE (type);
      if (!SCALAR_FLOAT_MODE_P (mode))
	return -1;

      if (*modep == VOIDmode)
	*modep = mode;

      if (*modep == mode)
	return 1;

      break;

    case COMPLEX_TYPE:
      mode = TYPE_MODE (TREE_TYPE (type));
      if (!SCALAR_FLOAT_MODE_P (mode))
	return -1;

      if (*modep == VOIDmode)
	*modep = mode;

      if (*modep == mode)
	return 2;

      break;

    case VECTOR_TYPE:
      if (!TARGET_ALTIVEC_ABI || !TARGET_ALTIVEC)
	return -1;

      /* Use V4SImode as representative of all 128-bit vector types.  */
      size = int_size_in_bytes (type);
      switch (size)
	{
	case 16:
	  mode = V4SImode;
	  break;
	default:
	  return -1;
	}

      if (*modep == VOIDmode)
	*modep = mode;

      /* Vector modes are considered to be opaque: two vectors are
	 equivalent for the purposes of being homogeneous aggregates
	 if they are the same size.  */
      if (*modep == mode)
	return 1;

      break;

    case ARRAY_TYPE:
      {
	int count;
	tree index = TYPE_DOMAIN (type);

	/* Can't handle incomplete types.  */
	if (!COMPLETE_TYPE_P (type))
	  return -1;

	count = rs6000_aggregate_candidate (TREE_TYPE (type), modep);
	if (count == -1
	    || !index
	    || !TYPE_MAX_VALUE (index)
	    || !tree_fits_uhwi_p (TYPE_MAX_VALUE (index))
	    || !TYPE_MIN_VALUE (index)
	    || !tree_fits_uhwi_p (TYPE_MIN_VALUE (index))
	    || count < 0)
	  return -1;

	count *= (1 + tree_to_uhwi (TYPE_MAX_VALUE (index))
		      - tree_to_uhwi (TYPE_MIN_VALUE (index)));

	/* There must be no padding.  */
	if (!tree_fits_uhwi_p (TYPE_SIZE (type))
	    || ((HOST_WIDE_INT) tree_to_uhwi (TYPE_SIZE (type))
		!= count * GET_MODE_BITSIZE (*modep)))
	  return -1;

	return count;
      }

    case RECORD_TYPE:
      {
	int count = 0;
	int sub_count;
	tree field;

	/* Can't handle incomplete types.  */
	if (!COMPLETE_TYPE_P (type))
	  return -1;

	for (field = TYPE_FIELDS (type); field; field = TREE_CHAIN (field))
	  {
	    if (TREE_CODE (field) != FIELD_DECL)
	      continue;

	    sub_count = rs6000_aggregate_candidate (TREE_TYPE (field), modep);
	    if (sub_count < 0)
	      return -1;
	    count += sub_count;
	  }

	/* There must be no padding.  */
	if (!tree_fits_uhwi_p (TYPE_SIZE (type))
	    || ((HOST_WIDE_INT) tree_to_uhwi (TYPE_SIZE (type))
		!= count * GET_MODE_BITSIZE (*modep)))
	  return -1;

	return count;
      }

    case UNION_TYPE:
    case QUAL_UNION_TYPE:
      {
	/* These aren't very interesting except in a degenerate case.  */
	int count = 0;
	int sub_count;
	tree field;

	/* Can't handle incomplete types.  */
	if (!COMPLETE_TYPE_P (type))
	  return -1;

	for (field = TYPE_FIELDS (type); field; field = TREE_CHAIN (field))
	  {
	    if (TREE_CODE (field) != FIELD_DECL)
	      continue;

	    sub_count = rs6000_aggregate_candidate (TREE_TYPE (field), modep);
	    if (sub_count < 0)
	      return -1;
	    count = count > sub_count ? count : sub_count;
	  }

	/* There must be no padding.  */
	if (!tree_fits_uhwi_p (TYPE_SIZE (type))
	    || ((HOST_WIDE_INT) tree_to_uhwi (TYPE_SIZE (type))
		!= count * GET_MODE_BITSIZE (*modep)))
	  return -1;

	return count;
      }

    default:
      break;
    }

  return -1;
}

/* If an argument, whose type is described by TYPE and MODE, is a homogeneous
   float or vector aggregate that shall be passed in FP/vector registers
   according to the ELFv2 ABI, return the homogeneous element mode in
   *ELT_MODE and the number of elements in *N_ELTS, and return TRUE.

   Otherwise, set *ELT_MODE to MODE and *N_ELTS to 1, and return FALSE.  */

static bool
rs6000_discover_homogeneous_aggregate (enum machine_mode mode, const_tree type,
				       enum machine_mode *elt_mode,
				       int *n_elts)
{
  /* Note that we do not accept complex types at the top level as
     homogeneous aggregates; these types are handled via the
     targetm.calls.split_complex_arg mechanism.  Complex types
     can be elements of homogeneous aggregates, however.  */
  if (DEFAULT_ABI == ABI_ELFv2 && type && AGGREGATE_TYPE_P (type))
    {
      enum machine_mode field_mode = VOIDmode;
      int field_count = rs6000_aggregate_candidate (type, &field_mode);

      if (field_count > 0)
	{
	  int n_regs = (SCALAR_FLOAT_MODE_P (field_mode)?
			(GET_MODE_SIZE (field_mode) + 7) >> 3 : 1);

	  /* The ELFv2 ABI allows homogeneous aggregates to occupy
	     up to AGGR_ARG_NUM_REG registers.  */
	  if (field_count * n_regs <= AGGR_ARG_NUM_REG)
	    {
	      if (elt_mode)
		*elt_mode = field_mode;
	      if (n_elts)
		*n_elts = field_count;
	      return true;
	    }
	}
    }

  if (elt_mode)
    *elt_mode = mode;
  if (n_elts)
    *n_elts = 1;
  return false;
}

/* Return a nonzero value to say to return the function value in
   memory, just as large structures are always returned.  TYPE will be
   the data type of the value, and FNTYPE will be the type of the
   function doing the returning, or @code{NULL} for libcalls.

   The AIX ABI for the RS/6000 specifies that all structures are
   returned in memory.  The Darwin ABI does the same.
   
   For the Darwin 64 Bit ABI, a function result can be returned in
   registers or in memory, depending on the size of the return data
   type.  If it is returned in registers, the value occupies the same
   registers as it would if it were the first and only function
   argument.  Otherwise, the function places its result in memory at
   the location pointed to by GPR3.
   
   The SVR4 ABI specifies that structures <= 8 bytes are returned in r3/r4, 
   but a draft put them in memory, and GCC used to implement the draft
   instead of the final standard.  Therefore, aix_struct_return
   controls this instead of DEFAULT_ABI; V.4 targets needing backward
   compatibility can change DRAFT_V4_STRUCT_RET to override the
   default, and -m switches get the final word.  See
   rs6000_option_override_internal for more details.

   The PPC32 SVR4 ABI uses IEEE double extended for long double, if 128-bit
   long double support is enabled.  These values are returned in memory.

   int_size_in_bytes returns -1 for variable size objects, which go in
   memory always.  The cast to unsigned makes -1 > 8.  */

static bool
rs6000_return_in_memory (const_tree type, const_tree fntype ATTRIBUTE_UNUSED)
{
  /* For the Darwin64 ABI, test if we can fit the return value in regs.  */
  if (TARGET_MACHO
      && rs6000_darwin64_abi
      && TREE_CODE (type) == RECORD_TYPE
      && int_size_in_bytes (type) > 0)
    {
      CUMULATIVE_ARGS valcum;
      rtx valret;

      valcum.words = 0;
      valcum.fregno = FP_ARG_MIN_REG;
      valcum.vregno = ALTIVEC_ARG_MIN_REG;
      /* Do a trial code generation as if this were going to be passed
	 as an argument; if any part goes in memory, we return NULL.  */
      valret = rs6000_darwin64_record_arg (&valcum, type, true, true);
      if (valret)
	return false;
      /* Otherwise fall through to more conventional ABI rules.  */
    }

  /* The ELFv2 ABI returns homogeneous VFP aggregates in registers */
  if (rs6000_discover_homogeneous_aggregate (TYPE_MODE (type), type,
					     NULL, NULL))
    return false;

  /* The ELFv2 ABI returns aggregates up to 16B in registers */
  if (DEFAULT_ABI == ABI_ELFv2 && AGGREGATE_TYPE_P (type)
      && (unsigned HOST_WIDE_INT) int_size_in_bytes (type) <= 16)
    return false;

  if (AGGREGATE_TYPE_P (type)
      && (aix_struct_return
	  || (unsigned HOST_WIDE_INT) int_size_in_bytes (type) > 8))
    return true;

  /* Allow -maltivec -mabi=no-altivec without warning.  Altivec vector
     modes only exist for GCC vector types if -maltivec.  */
  if (TARGET_32BIT && !TARGET_ALTIVEC_ABI
      && ALTIVEC_VECTOR_MODE (TYPE_MODE (type)))
    return false;

  /* Return synthetic vectors in memory.  */
  if (TREE_CODE (type) == VECTOR_TYPE
      && int_size_in_bytes (type) > (TARGET_ALTIVEC_ABI ? 16 : 8))
    {
      static bool warned_for_return_big_vectors = false;
      if (!warned_for_return_big_vectors)
	{
	  warning (0, "GCC vector returned by reference: "
		   "non-standard ABI extension with no compatibility guarantee");
	  warned_for_return_big_vectors = true;
	}
      return true;
    }

  if (DEFAULT_ABI == ABI_V4 && TARGET_IEEEQUAD && TYPE_MODE (type) == TFmode)
    return true;

  return false;
}

/* Specify whether values returned in registers should be at the most
   significant end of a register.  We want aggregates returned by
   value to match the way aggregates are passed to functions.  */

static bool
rs6000_return_in_msb (const_tree valtype)
{
  return (DEFAULT_ABI == ABI_ELFv2
	  && BYTES_BIG_ENDIAN
	  && AGGREGATE_TYPE_P (valtype)
	  && FUNCTION_ARG_PADDING (TYPE_MODE (valtype), valtype) == upward);
}

#ifdef HAVE_AS_GNU_ATTRIBUTE
/* Return TRUE if a call to function FNDECL may be one that
   potentially affects the function calling ABI of the object file.  */

static bool
call_ABI_of_interest (tree fndecl)
{
  if (cgraph_state == CGRAPH_STATE_EXPANSION)
    {
      struct cgraph_node *c_node;

      /* Libcalls are always interesting.  */
      if (fndecl == NULL_TREE)
	return true;

      /* Any call to an external function is interesting.  */
      if (DECL_EXTERNAL (fndecl))
	return true;

      /* Interesting functions that we are emitting in this object file.  */
      c_node = cgraph_get_node (fndecl);
      c_node = cgraph_function_or_thunk_node (c_node, NULL);
      return !cgraph_only_called_directly_p (c_node);
    }
  return false;
}
#endif

/* Initialize a variable CUM of type CUMULATIVE_ARGS
   for a call to a function whose data type is FNTYPE.
   For a library call, FNTYPE is 0 and RETURN_MODE the return value mode.

   For incoming args we set the number of arguments in the prototype large
   so we never return a PARALLEL.  */

void
init_cumulative_args (CUMULATIVE_ARGS *cum, tree fntype,
		      rtx libname ATTRIBUTE_UNUSED, int incoming,
		      int libcall, int n_named_args,
		      tree fndecl ATTRIBUTE_UNUSED,
		      enum machine_mode return_mode ATTRIBUTE_UNUSED)
{
  static CUMULATIVE_ARGS zero_cumulative;

  *cum = zero_cumulative;
  cum->words = 0;
  cum->fregno = FP_ARG_MIN_REG;
  cum->vregno = ALTIVEC_ARG_MIN_REG;
  cum->prototype = (fntype && prototype_p (fntype));
  cum->call_cookie = ((DEFAULT_ABI == ABI_V4 && libcall)
		      ? CALL_LIBCALL : CALL_NORMAL);
  cum->sysv_gregno = GP_ARG_MIN_REG;
  cum->stdarg = stdarg_p (fntype);

  cum->nargs_prototype = 0;
  if (incoming || cum->prototype)
    cum->nargs_prototype = n_named_args;

  /* Check for a longcall attribute.  */
  if ((!fntype && rs6000_default_long_calls)
      || (fntype
	  && lookup_attribute ("longcall", TYPE_ATTRIBUTES (fntype))
	  && !lookup_attribute ("shortcall", TYPE_ATTRIBUTES (fntype))))
    cum->call_cookie |= CALL_LONG;

  if (TARGET_DEBUG_ARG)
    {
      fprintf (stderr, "\ninit_cumulative_args:");
      if (fntype)
	{
	  tree ret_type = TREE_TYPE (fntype);
	  fprintf (stderr, " ret code = %s,",
		   get_tree_code_name (TREE_CODE (ret_type)));
	}

      if (cum->call_cookie & CALL_LONG)
	fprintf (stderr, " longcall,");

      fprintf (stderr, " proto = %d, nargs = %d\n",
	       cum->prototype, cum->nargs_prototype);
    }

#ifdef HAVE_AS_GNU_ATTRIBUTE
  if (DEFAULT_ABI == ABI_V4)
    {
      cum->escapes = call_ABI_of_interest (fndecl);
      if (cum->escapes)
	{
	  tree return_type;

	  if (fntype)
	    {
	      return_type = TREE_TYPE (fntype);
	      return_mode = TYPE_MODE (return_type);
	    }
	  else
	    return_type = lang_hooks.types.type_for_mode (return_mode, 0);

	  if (return_type != NULL)
	    {
	      if (TREE_CODE (return_type) == RECORD_TYPE
		  && TYPE_TRANSPARENT_AGGR (return_type))
		{
		  return_type = TREE_TYPE (first_field (return_type));
		  return_mode = TYPE_MODE (return_type);
		}
	      if (AGGREGATE_TYPE_P (return_type)
		  && ((unsigned HOST_WIDE_INT) int_size_in_bytes (return_type)
		      <= 8))
		rs6000_returns_struct = true;
	    }
	  if (SCALAR_FLOAT_MODE_P (return_mode))
	    rs6000_passes_float = true;
	  else if (ALTIVEC_OR_VSX_VECTOR_MODE (return_mode)
		   || SPE_VECTOR_MODE (return_mode))
	    rs6000_passes_vector = true;
	}
    }
#endif

  if (fntype
      && !TARGET_ALTIVEC
      && TARGET_ALTIVEC_ABI
      && ALTIVEC_VECTOR_MODE (TYPE_MODE (TREE_TYPE (fntype))))
    {
      error ("cannot return value in vector register because"
	     " altivec instructions are disabled, use -maltivec"
	     " to enable them");
    }
}

/* Return true if TYPE must be passed on the stack and not in registers.  */

static bool
rs6000_must_pass_in_stack (enum machine_mode mode, const_tree type)
{
  if (DEFAULT_ABI == ABI_AIX || DEFAULT_ABI == ABI_ELFv2 || TARGET_64BIT)
    return must_pass_in_stack_var_size (mode, type);
  else
    return must_pass_in_stack_var_size_or_pad (mode, type);
}

/* If defined, a C expression which determines whether, and in which
   direction, to pad out an argument with extra space.  The value
   should be of type `enum direction': either `upward' to pad above
   the argument, `downward' to pad below, or `none' to inhibit
   padding.

   For the AIX ABI structs are always stored left shifted in their
   argument slot.  */

enum direction
function_arg_padding (enum machine_mode mode, const_tree type)
{
#ifndef AGGREGATE_PADDING_FIXED
#define AGGREGATE_PADDING_FIXED 0
#endif
#ifndef AGGREGATES_PAD_UPWARD_ALWAYS
#define AGGREGATES_PAD_UPWARD_ALWAYS 0
#endif

  if (!AGGREGATE_PADDING_FIXED)
    {
      /* GCC used to pass structures of the same size as integer types as
	 if they were in fact integers, ignoring FUNCTION_ARG_PADDING.
	 i.e. Structures of size 1 or 2 (or 4 when TARGET_64BIT) were
	 passed padded downward, except that -mstrict-align further
	 muddied the water in that multi-component structures of 2 and 4
	 bytes in size were passed padded upward.

	 The following arranges for best compatibility with previous
	 versions of gcc, but removes the -mstrict-align dependency.  */
      if (BYTES_BIG_ENDIAN)
	{
	  HOST_WIDE_INT size = 0;

	  if (mode == BLKmode)
	    {
	      if (type && TREE_CODE (TYPE_SIZE (type)) == INTEGER_CST)
		size = int_size_in_bytes (type);
	    }
	  else
	    size = GET_MODE_SIZE (mode);

	  if (size == 1 || size == 2 || size == 4)
	    return downward;
	}
      return upward;
    }

  if (AGGREGATES_PAD_UPWARD_ALWAYS)
    {
      if (type != 0 && AGGREGATE_TYPE_P (type))
	return upward;
    }

  /* Fall back to the default.  */
  return DEFAULT_FUNCTION_ARG_PADDING (mode, type);
}

/* If defined, a C expression that gives the alignment boundary, in bits,
   of an argument with the specified mode and type.  If it is not defined,
   PARM_BOUNDARY is used for all arguments.

   V.4 wants long longs and doubles to be double word aligned.  Just
   testing the mode size is a boneheaded way to do this as it means
   that other types such as complex int are also double word aligned.
   However, we're stuck with this because changing the ABI might break
   existing library interfaces.

   Doubleword align SPE vectors.
   Quadword align Altivec/VSX vectors.
   Quadword align large synthetic vector types.   */

static unsigned int
rs6000_function_arg_boundary (enum machine_mode mode, const_tree type)
{
  enum machine_mode elt_mode;
  int n_elts;

  rs6000_discover_homogeneous_aggregate (mode, type, &elt_mode, &n_elts);

  if (DEFAULT_ABI == ABI_V4
      && (GET_MODE_SIZE (mode) == 8
	  || (TARGET_HARD_FLOAT
	      && TARGET_FPRS
	      && (mode == TFmode || mode == TDmode))))
    return 64;
  else if (SPE_VECTOR_MODE (mode)
	   || (type && TREE_CODE (type) == VECTOR_TYPE
	       && int_size_in_bytes (type) >= 8
	       && int_size_in_bytes (type) < 16))
    return 64;
  else if (ALTIVEC_OR_VSX_VECTOR_MODE (elt_mode)
	   || (type && TREE_CODE (type) == VECTOR_TYPE
	       && int_size_in_bytes (type) >= 16))
    return 128;
  else if (((TARGET_MACHO && rs6000_darwin64_abi)
	    || DEFAULT_ABI == ABI_ELFv2
            || (DEFAULT_ABI == ABI_AIX && !rs6000_compat_align_parm))
 	   && mode == BLKmode
	   && type && TYPE_ALIGN (type) > 64)
    return 128;
  else
    return PARM_BOUNDARY;
}

/* The offset in words to the start of the parameter save area.  */

static unsigned int
rs6000_parm_offset (void)
{
  return (DEFAULT_ABI == ABI_V4 ? 2
	  : DEFAULT_ABI == ABI_ELFv2 ? 4
	  : 6);
}

/* For a function parm of MODE and TYPE, return the starting word in
   the parameter area.  NWORDS of the parameter area are already used.  */

static unsigned int
rs6000_parm_start (enum machine_mode mode, const_tree type,
		   unsigned int nwords)
{
  unsigned int align;

  align = rs6000_function_arg_boundary (mode, type) / PARM_BOUNDARY - 1;
  return nwords + (-(rs6000_parm_offset () + nwords) & align);
}

/* Compute the size (in words) of a function argument.  */

static unsigned long
rs6000_arg_size (enum machine_mode mode, const_tree type)
{
  unsigned long size;

  if (mode != BLKmode)
    size = GET_MODE_SIZE (mode);
  else
    size = int_size_in_bytes (type);

  if (TARGET_32BIT)
    return (size + 3) >> 2;
  else
    return (size + 7) >> 3;
}

/* Use this to flush pending int fields.  */

static void
rs6000_darwin64_record_arg_advance_flush (CUMULATIVE_ARGS *cum,
					  HOST_WIDE_INT bitpos, int final)
{
  unsigned int startbit, endbit;
  int intregs, intoffset;
  enum machine_mode mode;

  /* Handle the situations where a float is taking up the first half
     of the GPR, and the other half is empty (typically due to
     alignment restrictions). We can detect this by a 8-byte-aligned
     int field, or by seeing that this is the final flush for this
     argument. Count the word and continue on.  */
  if (cum->floats_in_gpr == 1
      && (cum->intoffset % 64 == 0
	  || (cum->intoffset == -1 && final)))
    {
      cum->words++;
      cum->floats_in_gpr = 0;
    }

  if (cum->intoffset == -1)
    return;

  intoffset = cum->intoffset;
  cum->intoffset = -1;
  cum->floats_in_gpr = 0;

  if (intoffset % BITS_PER_WORD != 0)
    {
      mode = mode_for_size (BITS_PER_WORD - intoffset % BITS_PER_WORD,
			    MODE_INT, 0);
      if (mode == BLKmode)
	{
	  /* We couldn't find an appropriate mode, which happens,
	     e.g., in packed structs when there are 3 bytes to load.
	     Back intoffset back to the beginning of the word in this
	     case.  */
	  intoffset = intoffset & -BITS_PER_WORD;
	}
    }

  startbit = intoffset & -BITS_PER_WORD;
  endbit = (bitpos + BITS_PER_WORD - 1) & -BITS_PER_WORD;
  intregs = (endbit - startbit) / BITS_PER_WORD;
  cum->words += intregs;
  /* words should be unsigned. */
  if ((unsigned)cum->words < (endbit/BITS_PER_WORD))
    {
      int pad = (endbit/BITS_PER_WORD) - cum->words;
      cum->words += pad;
    }
}

/* The darwin64 ABI calls for us to recurse down through structs,
   looking for elements passed in registers.  Unfortunately, we have
   to track int register count here also because of misalignments
   in powerpc alignment mode.  */

static void
rs6000_darwin64_record_arg_advance_recurse (CUMULATIVE_ARGS *cum,
					    const_tree type,
					    HOST_WIDE_INT startbitpos)
{
  tree f;

  for (f = TYPE_FIELDS (type); f ; f = DECL_CHAIN (f))
    if (TREE_CODE (f) == FIELD_DECL)
      {
	HOST_WIDE_INT bitpos = startbitpos;
	tree ftype = TREE_TYPE (f);
	enum machine_mode mode;
	if (ftype == error_mark_node)
	  continue;
	mode = TYPE_MODE (ftype);

	if (DECL_SIZE (f) != 0
	    && tree_fits_uhwi_p (bit_position (f)))
	  bitpos += int_bit_position (f);

	/* ??? FIXME: else assume zero offset.  */

	if (TREE_CODE (ftype) == RECORD_TYPE)
	  rs6000_darwin64_record_arg_advance_recurse (cum, ftype, bitpos);
	else if (USE_FP_FOR_ARG_P (cum, mode))
	  {
	    unsigned n_fpregs = (GET_MODE_SIZE (mode) + 7) >> 3;
	    rs6000_darwin64_record_arg_advance_flush (cum, bitpos, 0);
	    cum->fregno += n_fpregs;
	    /* Single-precision floats present a special problem for
	       us, because they are smaller than an 8-byte GPR, and so
	       the structure-packing rules combined with the standard
	       varargs behavior mean that we want to pack float/float
	       and float/int combinations into a single register's
	       space. This is complicated by the arg advance flushing,
	       which works on arbitrarily large groups of int-type
	       fields.  */
	    if (mode == SFmode)
	      {
		if (cum->floats_in_gpr == 1)
		  {
		    /* Two floats in a word; count the word and reset
		       the float count.  */
		    cum->words++;
		    cum->floats_in_gpr = 0;
		  }
		else if (bitpos % 64 == 0)
		  {
		    /* A float at the beginning of an 8-byte word;
		       count it and put off adjusting cum->words until
		       we see if a arg advance flush is going to do it
		       for us.  */
		    cum->floats_in_gpr++;
		  }
		else
		  {
		    /* The float is at the end of a word, preceded
		       by integer fields, so the arg advance flush
		       just above has already set cum->words and
		       everything is taken care of.  */
		  }
	      }
	    else
	      cum->words += n_fpregs;
	  }
	else if (USE_ALTIVEC_FOR_ARG_P (cum, mode, 1))
	  {
	    rs6000_darwin64_record_arg_advance_flush (cum, bitpos, 0);
	    cum->vregno++;
	    cum->words += 2;
	  }
	else if (cum->intoffset == -1)
	  cum->intoffset = bitpos;
      }
}

/* Check for an item that needs to be considered specially under the darwin 64
   bit ABI.  These are record types where the mode is BLK or the structure is
   8 bytes in size.  */
static int
rs6000_darwin64_struct_check_p (enum machine_mode mode, const_tree type)
{
  return rs6000_darwin64_abi
	 && ((mode == BLKmode 
	      && TREE_CODE (type) == RECORD_TYPE 
	      && int_size_in_bytes (type) > 0)
	  || (type && TREE_CODE (type) == RECORD_TYPE 
	      && int_size_in_bytes (type) == 8)) ? 1 : 0;
}

/* Update the data in CUM to advance over an argument
   of mode MODE and data type TYPE.
   (TYPE is null for libcalls where that information may not be available.)

   Note that for args passed by reference, function_arg will be called
   with MODE and TYPE set to that of the pointer to the arg, not the arg
   itself.  */

static void
rs6000_function_arg_advance_1 (CUMULATIVE_ARGS *cum, enum machine_mode mode,
			       const_tree type, bool named, int depth)
{
  enum machine_mode elt_mode;
  int n_elts;

  rs6000_discover_homogeneous_aggregate (mode, type, &elt_mode, &n_elts);

  /* Only tick off an argument if we're not recursing.  */
  if (depth == 0)
    cum->nargs_prototype--;

#ifdef HAVE_AS_GNU_ATTRIBUTE
  if (DEFAULT_ABI == ABI_V4
      && cum->escapes)
    {
      if (SCALAR_FLOAT_MODE_P (mode))
	rs6000_passes_float = true;
      else if (named && ALTIVEC_OR_VSX_VECTOR_MODE (mode))
	rs6000_passes_vector = true;
      else if (SPE_VECTOR_MODE (mode)
	       && !cum->stdarg
	       && cum->sysv_gregno <= GP_ARG_MAX_REG)
	rs6000_passes_vector = true;
    }
#endif

  if (TARGET_ALTIVEC_ABI
      && (ALTIVEC_OR_VSX_VECTOR_MODE (elt_mode)
	  || (type && TREE_CODE (type) == VECTOR_TYPE
	      && int_size_in_bytes (type) == 16)))
    {
      bool stack = false;

      if (USE_ALTIVEC_FOR_ARG_P (cum, elt_mode, named))
	{
	  cum->vregno += n_elts;

	  if (!TARGET_ALTIVEC)
	    error ("cannot pass argument in vector register because"
		   " altivec instructions are disabled, use -maltivec"
		   " to enable them");

	  /* PowerPC64 Linux and AIX allocate GPRs for a vector argument
	     even if it is going to be passed in a vector register.
	     Darwin does the same for variable-argument functions.  */
	  if (((DEFAULT_ABI == ABI_AIX || DEFAULT_ABI == ABI_ELFv2)
	       && TARGET_64BIT)
	      || (cum->stdarg && DEFAULT_ABI != ABI_V4))
	    stack = true;
	}
      else
	stack = true;

      if (stack)
	{
	  int align;

	  /* Vector parameters must be 16-byte aligned.  In 32-bit
	     mode this means we need to take into account the offset
	     to the parameter save area.  In 64-bit mode, they just
	     have to start on an even word, since the parameter save
	     area is 16-byte aligned.  */
	  if (TARGET_32BIT)
	    align = -(rs6000_parm_offset () + cum->words) & 3;
	  else
	    align = cum->words & 1;
	  cum->words += align + rs6000_arg_size (mode, type);

	  if (TARGET_DEBUG_ARG)
	    {
	      fprintf (stderr, "function_adv: words = %2d, align=%d, ",
		       cum->words, align);
	      fprintf (stderr, "nargs = %4d, proto = %d, mode = %4s\n",
		       cum->nargs_prototype, cum->prototype,
		       GET_MODE_NAME (mode));
	    }
	}
    }
  else if (TARGET_SPE_ABI && TARGET_SPE && SPE_VECTOR_MODE (mode)
	   && !cum->stdarg
	   && cum->sysv_gregno <= GP_ARG_MAX_REG)
    cum->sysv_gregno++;

  else if (TARGET_MACHO && rs6000_darwin64_struct_check_p (mode, type))
    {
      int size = int_size_in_bytes (type);
      /* Variable sized types have size == -1 and are
	 treated as if consisting entirely of ints.
	 Pad to 16 byte boundary if needed.  */
      if (TYPE_ALIGN (type) >= 2 * BITS_PER_WORD
	  && (cum->words % 2) != 0)
	cum->words++;
      /* For varargs, we can just go up by the size of the struct. */
      if (!named)
	cum->words += (size + 7) / 8;
      else
	{
	  /* It is tempting to say int register count just goes up by
	     sizeof(type)/8, but this is wrong in a case such as
	     { int; double; int; } [powerpc alignment].  We have to
	     grovel through the fields for these too.  */
	  cum->intoffset = 0;
	  cum->floats_in_gpr = 0;
	  rs6000_darwin64_record_arg_advance_recurse (cum, type, 0);
	  rs6000_darwin64_record_arg_advance_flush (cum,
						    size * BITS_PER_UNIT, 1);
	}
	  if (TARGET_DEBUG_ARG)
	    {
	      fprintf (stderr, "function_adv: words = %2d, align=%d, size=%d",
		       cum->words, TYPE_ALIGN (type), size);
	      fprintf (stderr, 
	           "nargs = %4d, proto = %d, mode = %4s (darwin64 abi)\n",
		       cum->nargs_prototype, cum->prototype,
		       GET_MODE_NAME (mode));
	    }
    }
  else if (DEFAULT_ABI == ABI_V4)
    {
      if (TARGET_HARD_FLOAT && TARGET_FPRS
	  && ((TARGET_SINGLE_FLOAT && mode == SFmode)
	      || (TARGET_DOUBLE_FLOAT && mode == DFmode)
	      || (mode == TFmode && !TARGET_IEEEQUAD)
	      || mode == SDmode || mode == DDmode || mode == TDmode))
	{
	  /* _Decimal128 must use an even/odd register pair.  This assumes
	     that the register number is odd when fregno is odd.  */
	  if (mode == TDmode && (cum->fregno % 2) == 1)
	    cum->fregno++;

	  if (cum->fregno + (mode == TFmode || mode == TDmode ? 1 : 0)
	      <= FP_ARG_V4_MAX_REG)
	    cum->fregno += (GET_MODE_SIZE (mode) + 7) >> 3;
	  else
	    {
	      cum->fregno = FP_ARG_V4_MAX_REG + 1;
	      if (mode == DFmode || mode == TFmode
		  || mode == DDmode || mode == TDmode)
		cum->words += cum->words & 1;
	      cum->words += rs6000_arg_size (mode, type);
	    }
	}
      else
	{
	  int n_words = rs6000_arg_size (mode, type);
	  int gregno = cum->sysv_gregno;

	  /* Long long and SPE vectors are put in (r3,r4), (r5,r6),
	     (r7,r8) or (r9,r10).  As does any other 2 word item such
	     as complex int due to a historical mistake.  */
	  if (n_words == 2)
	    gregno += (1 - gregno) & 1;

	  /* Multi-reg args are not split between registers and stack.  */
	  if (gregno + n_words - 1 > GP_ARG_MAX_REG)
	    {
	      /* Long long and SPE vectors are aligned on the stack.
		 So are other 2 word items such as complex int due to
		 a historical mistake.  */
	      if (n_words == 2)
		cum->words += cum->words & 1;
	      cum->words += n_words;
	    }

	  /* Note: continuing to accumulate gregno past when we've started
	     spilling to the stack indicates the fact that we've started
	     spilling to the stack to expand_builtin_saveregs.  */
	  cum->sysv_gregno = gregno + n_words;
	}

      if (TARGET_DEBUG_ARG)
	{
	  fprintf (stderr, "function_adv: words = %2d, fregno = %2d, ",
		   cum->words, cum->fregno);
	  fprintf (stderr, "gregno = %2d, nargs = %4d, proto = %d, ",
		   cum->sysv_gregno, cum->nargs_prototype, cum->prototype);
	  fprintf (stderr, "mode = %4s, named = %d\n",
		   GET_MODE_NAME (mode), named);
	}
    }
  else
    {
      int n_words = rs6000_arg_size (mode, type);
      int start_words = cum->words;
      int align_words = rs6000_parm_start (mode, type, start_words);

      cum->words = align_words + n_words;

      if (SCALAR_FLOAT_MODE_P (elt_mode)
	  && TARGET_HARD_FLOAT && TARGET_FPRS)
	{
	  /* _Decimal128 must be passed in an even/odd float register pair.
	     This assumes that the register number is odd when fregno is
	     odd.  */
	  if (elt_mode == TDmode && (cum->fregno % 2) == 1)
	    cum->fregno++;
	  cum->fregno += n_elts * ((GET_MODE_SIZE (elt_mode) + 7) >> 3);
	}

      if (TARGET_DEBUG_ARG)
	{
	  fprintf (stderr, "function_adv: words = %2d, fregno = %2d, ",
		   cum->words, cum->fregno);
	  fprintf (stderr, "nargs = %4d, proto = %d, mode = %4s, ",
		   cum->nargs_prototype, cum->prototype, GET_MODE_NAME (mode));
	  fprintf (stderr, "named = %d, align = %d, depth = %d\n",
		   named, align_words - start_words, depth);
	}
    }
}

static void
rs6000_function_arg_advance (cumulative_args_t cum, enum machine_mode mode,
			     const_tree type, bool named)
{
  rs6000_function_arg_advance_1 (get_cumulative_args (cum), mode, type, named,
				 0);
}

static rtx
spe_build_register_parallel (enum machine_mode mode, int gregno)
{
  rtx r1, r3, r5, r7;

  switch (mode)
    {
    case DFmode:
      r1 = gen_rtx_REG (DImode, gregno);
      r1 = gen_rtx_EXPR_LIST (VOIDmode, r1, const0_rtx);
      return gen_rtx_PARALLEL (mode, gen_rtvec (1, r1));

    case DCmode:
    case TFmode:
      r1 = gen_rtx_REG (DImode, gregno);
      r1 = gen_rtx_EXPR_LIST (VOIDmode, r1, const0_rtx);
      r3 = gen_rtx_REG (DImode, gregno + 2);
      r3 = gen_rtx_EXPR_LIST (VOIDmode, r3, GEN_INT (8));
      return gen_rtx_PARALLEL (mode, gen_rtvec (2, r1, r3));

    case TCmode:
      r1 = gen_rtx_REG (DImode, gregno);
      r1 = gen_rtx_EXPR_LIST (VOIDmode, r1, const0_rtx);
      r3 = gen_rtx_REG (DImode, gregno + 2);
      r3 = gen_rtx_EXPR_LIST (VOIDmode, r3, GEN_INT (8));
      r5 = gen_rtx_REG (DImode, gregno + 4);
      r5 = gen_rtx_EXPR_LIST (VOIDmode, r5, GEN_INT (16));
      r7 = gen_rtx_REG (DImode, gregno + 6);
      r7 = gen_rtx_EXPR_LIST (VOIDmode, r7, GEN_INT (24));
      return gen_rtx_PARALLEL (mode, gen_rtvec (4, r1, r3, r5, r7));

    default:
      gcc_unreachable ();
    }
}

/* Determine where to put a SIMD argument on the SPE.  */
static rtx
rs6000_spe_function_arg (const CUMULATIVE_ARGS *cum, enum machine_mode mode,
			 const_tree type)
{
  int gregno = cum->sysv_gregno;

  /* On E500 v2, double arithmetic is done on the full 64-bit GPR, but
     are passed and returned in a pair of GPRs for ABI compatibility.  */
  if (TARGET_E500_DOUBLE && (mode == DFmode || mode == TFmode
			     || mode == DCmode || mode == TCmode))
    {
      int n_words = rs6000_arg_size (mode, type);

      /* Doubles go in an odd/even register pair (r5/r6, etc).  */
      if (mode == DFmode)
	gregno += (1 - gregno) & 1;

      /* Multi-reg args are not split between registers and stack.  */
      if (gregno + n_words - 1 > GP_ARG_MAX_REG)
	return NULL_RTX;

      return spe_build_register_parallel (mode, gregno);
    }
  if (cum->stdarg)
    {
      int n_words = rs6000_arg_size (mode, type);

      /* SPE vectors are put in odd registers.  */
      if (n_words == 2 && (gregno & 1) == 0)
	gregno += 1;

      if (gregno + n_words - 1 <= GP_ARG_MAX_REG)
	{
	  rtx r1, r2;
	  enum machine_mode m = SImode;

	  r1 = gen_rtx_REG (m, gregno);
	  r1 = gen_rtx_EXPR_LIST (m, r1, const0_rtx);
	  r2 = gen_rtx_REG (m, gregno + 1);
	  r2 = gen_rtx_EXPR_LIST (m, r2, GEN_INT (4));
	  return gen_rtx_PARALLEL (mode, gen_rtvec (2, r1, r2));
	}
      else
	return NULL_RTX;
    }
  else
    {
      if (gregno <= GP_ARG_MAX_REG)
	return gen_rtx_REG (mode, gregno);
      else
	return NULL_RTX;
    }
}

/* A subroutine of rs6000_darwin64_record_arg.  Assign the bits of the
   structure between cum->intoffset and bitpos to integer registers.  */

static void
rs6000_darwin64_record_arg_flush (CUMULATIVE_ARGS *cum,
				  HOST_WIDE_INT bitpos, rtx rvec[], int *k)
{
  enum machine_mode mode;
  unsigned int regno;
  unsigned int startbit, endbit;
  int this_regno, intregs, intoffset;
  rtx reg;

  if (cum->intoffset == -1)
    return;

  intoffset = cum->intoffset;
  cum->intoffset = -1;

  /* If this is the trailing part of a word, try to only load that
     much into the register.  Otherwise load the whole register.  Note
     that in the latter case we may pick up unwanted bits.  It's not a
     problem at the moment but may wish to revisit.  */

  if (intoffset % BITS_PER_WORD != 0)
    {
      mode = mode_for_size (BITS_PER_WORD - intoffset % BITS_PER_WORD,
			  MODE_INT, 0);
      if (mode == BLKmode)
	{
	  /* We couldn't find an appropriate mode, which happens,
	     e.g., in packed structs when there are 3 bytes to load.
	     Back intoffset back to the beginning of the word in this
	     case.  */
	 intoffset = intoffset & -BITS_PER_WORD;
	 mode = word_mode;
	}
    }
  else
    mode = word_mode;

  startbit = intoffset & -BITS_PER_WORD;
  endbit = (bitpos + BITS_PER_WORD - 1) & -BITS_PER_WORD;
  intregs = (endbit - startbit) / BITS_PER_WORD;
  this_regno = cum->words + intoffset / BITS_PER_WORD;

  if (intregs > 0 && intregs > GP_ARG_NUM_REG - this_regno)
    cum->use_stack = 1;

  intregs = MIN (intregs, GP_ARG_NUM_REG - this_regno);
  if (intregs <= 0)
    return;

  intoffset /= BITS_PER_UNIT;
  do
    {
      regno = GP_ARG_MIN_REG + this_regno;
      reg = gen_rtx_REG (mode, regno);
      rvec[(*k)++] =
	gen_rtx_EXPR_LIST (VOIDmode, reg, GEN_INT (intoffset));

      this_regno += 1;
      intoffset = (intoffset | (UNITS_PER_WORD-1)) + 1;
      mode = word_mode;
      intregs -= 1;
    }
  while (intregs > 0);
}

/* Recursive workhorse for the following.  */

static void
rs6000_darwin64_record_arg_recurse (CUMULATIVE_ARGS *cum, const_tree type,
				    HOST_WIDE_INT startbitpos, rtx rvec[],
				    int *k)
{
  tree f;

  for (f = TYPE_FIELDS (type); f ; f = DECL_CHAIN (f))
    if (TREE_CODE (f) == FIELD_DECL)
      {
	HOST_WIDE_INT bitpos = startbitpos;
	tree ftype = TREE_TYPE (f);
	enum machine_mode mode;
	if (ftype == error_mark_node)
	  continue;
	mode = TYPE_MODE (ftype);

	if (DECL_SIZE (f) != 0
	    && tree_fits_uhwi_p (bit_position (f)))
	  bitpos += int_bit_position (f);

	/* ??? FIXME: else assume zero offset.  */

	if (TREE_CODE (ftype) == RECORD_TYPE)
	  rs6000_darwin64_record_arg_recurse (cum, ftype, bitpos, rvec, k);
	else if (cum->named && USE_FP_FOR_ARG_P (cum, mode))
	  {
	    unsigned n_fpreg = (GET_MODE_SIZE (mode) + 7) >> 3;
#if 0
	    switch (mode)
	      {
	      case SCmode: mode = SFmode; break;
	      case DCmode: mode = DFmode; break;
	      case TCmode: mode = TFmode; break;
	      default: break;
	      }
#endif
	    rs6000_darwin64_record_arg_flush (cum, bitpos, rvec, k);
	    if (cum->fregno + n_fpreg > FP_ARG_MAX_REG + 1)
	      {
		gcc_assert (cum->fregno == FP_ARG_MAX_REG
			    && (mode == TFmode || mode == TDmode));
		/* Long double or _Decimal128 split over regs and memory.  */
		mode = DECIMAL_FLOAT_MODE_P (mode) ? DDmode : DFmode;
		cum->use_stack=1;
	      }
	    rvec[(*k)++]
	      = gen_rtx_EXPR_LIST (VOIDmode,
				   gen_rtx_REG (mode, cum->fregno++),
				   GEN_INT (bitpos / BITS_PER_UNIT));
	    if (mode == TFmode || mode == TDmode)
	      cum->fregno++;
	  }
	else if (cum->named && USE_ALTIVEC_FOR_ARG_P (cum, mode, 1))
	  {
	    rs6000_darwin64_record_arg_flush (cum, bitpos, rvec, k);
	    rvec[(*k)++]
	      = gen_rtx_EXPR_LIST (VOIDmode,
				   gen_rtx_REG (mode, cum->vregno++),
				   GEN_INT (bitpos / BITS_PER_UNIT));
	  }
	else if (cum->intoffset == -1)
	  cum->intoffset = bitpos;
      }
}

/* For the darwin64 ABI, we want to construct a PARALLEL consisting of
   the register(s) to be used for each field and subfield of a struct
   being passed by value, along with the offset of where the
   register's value may be found in the block.  FP fields go in FP
   register, vector fields go in vector registers, and everything
   else goes in int registers, packed as in memory.

   This code is also used for function return values.  RETVAL indicates
   whether this is the case.

   Much of this is taken from the SPARC V9 port, which has a similar
   calling convention.  */

static rtx
rs6000_darwin64_record_arg (CUMULATIVE_ARGS *orig_cum, const_tree type,
			    bool named, bool retval)
{
  rtx rvec[FIRST_PSEUDO_REGISTER];
  int k = 1, kbase = 1;
  HOST_WIDE_INT typesize = int_size_in_bytes (type);
  /* This is a copy; modifications are not visible to our caller.  */
  CUMULATIVE_ARGS copy_cum = *orig_cum;
  CUMULATIVE_ARGS *cum = &copy_cum;

  /* Pad to 16 byte boundary if needed.  */
  if (!retval && TYPE_ALIGN (type) >= 2 * BITS_PER_WORD
      && (cum->words % 2) != 0)
    cum->words++;

  cum->intoffset = 0;
  cum->use_stack = 0;
  cum->named = named;

  /* Put entries into rvec[] for individual FP and vector fields, and
     for the chunks of memory that go in int regs.  Note we start at
     element 1; 0 is reserved for an indication of using memory, and
     may or may not be filled in below. */
  rs6000_darwin64_record_arg_recurse (cum, type, /* startbit pos= */ 0, rvec, &k);
  rs6000_darwin64_record_arg_flush (cum, typesize * BITS_PER_UNIT, rvec, &k);

  /* If any part of the struct went on the stack put all of it there.
     This hack is because the generic code for
     FUNCTION_ARG_PARTIAL_NREGS cannot handle cases where the register
     parts of the struct are not at the beginning.  */
  if (cum->use_stack)
    {
      if (retval)
	return NULL_RTX;    /* doesn't go in registers at all */
      kbase = 0;
      rvec[0] = gen_rtx_EXPR_LIST (VOIDmode, NULL_RTX, const0_rtx);
    }
  if (k > 1 || cum->use_stack)
    return gen_rtx_PARALLEL (BLKmode, gen_rtvec_v (k - kbase, &rvec[kbase]));
  else
    return NULL_RTX;
}

/* Determine where to place an argument in 64-bit mode with 32-bit ABI.  */

static rtx
rs6000_mixed_function_arg (enum machine_mode mode, const_tree type,
			   int align_words)
{
  int n_units;
  int i, k;
  rtx rvec[GP_ARG_NUM_REG + 1];

  if (align_words >= GP_ARG_NUM_REG)
    return NULL_RTX;

  n_units = rs6000_arg_size (mode, type);

  /* Optimize the simple case where the arg fits in one gpr, except in
     the case of BLKmode due to assign_parms assuming that registers are
     BITS_PER_WORD wide.  */
  if (n_units == 0
      || (n_units == 1 && mode != BLKmode))
    return gen_rtx_REG (mode, GP_ARG_MIN_REG + align_words);

  k = 0;
  if (align_words + n_units > GP_ARG_NUM_REG)
    /* Not all of the arg fits in gprs.  Say that it goes in memory too,
       using a magic NULL_RTX component.
       This is not strictly correct.  Only some of the arg belongs in
       memory, not all of it.  However, the normal scheme using
       function_arg_partial_nregs can result in unusual subregs, eg.
       (subreg:SI (reg:DF) 4), which are not handled well.  The code to
       store the whole arg to memory is often more efficient than code
       to store pieces, and we know that space is available in the right
       place for the whole arg.  */
    rvec[k++] = gen_rtx_EXPR_LIST (VOIDmode, NULL_RTX, const0_rtx);

  i = 0;
  do
    {
      rtx r = gen_rtx_REG (SImode, GP_ARG_MIN_REG + align_words);
      rtx off = GEN_INT (i++ * 4);
      rvec[k++] = gen_rtx_EXPR_LIST (VOIDmode, r, off);
    }
  while (++align_words < GP_ARG_NUM_REG && --n_units != 0);

  return gen_rtx_PARALLEL (mode, gen_rtvec_v (k, rvec));
}

/* We have an argument of MODE and TYPE that goes into FPRs or VRs,
   but must also be copied into the parameter save area starting at
   offset ALIGN_WORDS.  Fill in RVEC with the elements corresponding
   to the GPRs and/or memory.  Return the number of elements used.  */

static int
rs6000_psave_function_arg (enum machine_mode mode, const_tree type,
			   int align_words, rtx *rvec)
{
  int k = 0;

  if (align_words < GP_ARG_NUM_REG)
    {
      int n_words = rs6000_arg_size (mode, type);

      if (align_words + n_words > GP_ARG_NUM_REG
	  || mode == BLKmode
	  || (TARGET_32BIT && TARGET_POWERPC64))
	{
	  /* If this is partially on the stack, then we only
	     include the portion actually in registers here.  */
	  enum machine_mode rmode = TARGET_32BIT ? SImode : DImode;
	  int i = 0;

	  if (align_words + n_words > GP_ARG_NUM_REG)
	    {
	      /* Not all of the arg fits in gprs.  Say that it goes in memory
		 too, using a magic NULL_RTX component.  Also see comment in
		 rs6000_mixed_function_arg for why the normal
		 function_arg_partial_nregs scheme doesn't work in this case. */
	      rvec[k++] = gen_rtx_EXPR_LIST (VOIDmode, NULL_RTX, const0_rtx);
	    }

	  do
	    {
	      rtx r = gen_rtx_REG (rmode, GP_ARG_MIN_REG + align_words);
	      rtx off = GEN_INT (i++ * GET_MODE_SIZE (rmode));
	      rvec[k++] = gen_rtx_EXPR_LIST (VOIDmode, r, off);
	    }
	  while (++align_words < GP_ARG_NUM_REG && --n_words != 0);
	}
      else
	{
	  /* The whole arg fits in gprs.  */
	  rtx r = gen_rtx_REG (mode, GP_ARG_MIN_REG + align_words);
	  rvec[k++] = gen_rtx_EXPR_LIST (VOIDmode, r, const0_rtx);
	}
    }
  else
    {
      /* It's entirely in memory.  */
      rvec[k++] = gen_rtx_EXPR_LIST (VOIDmode, NULL_RTX, const0_rtx);
    }

  return k;
}

/* RVEC is a vector of K components of an argument of mode MODE.
   Construct the final function_arg return value from it.  */

static rtx
rs6000_finish_function_arg (enum machine_mode mode, rtx *rvec, int k)
{
  gcc_assert (k >= 1);

  /* Avoid returning a PARALLEL in the trivial cases.  */
  if (k == 1)
    {
      if (XEXP (rvec[0], 0) == NULL_RTX)
	return NULL_RTX;

      if (GET_MODE (XEXP (rvec[0], 0)) == mode)
	return XEXP (rvec[0], 0);
    }

  return gen_rtx_PARALLEL (mode, gen_rtvec_v (k, rvec));
}

/* Determine where to put an argument to a function.
   Value is zero to push the argument on the stack,
   or a hard register in which to store the argument.

   MODE is the argument's machine mode.
   TYPE is the data type of the argument (as a tree).
    This is null for libcalls where that information may
    not be available.
   CUM is a variable of type CUMULATIVE_ARGS which gives info about
    the preceding args and about the function being called.  It is
    not modified in this routine.
   NAMED is nonzero if this argument is a named parameter
    (otherwise it is an extra parameter matching an ellipsis).

   On RS/6000 the first eight words of non-FP are normally in registers
   and the rest are pushed.  Under AIX, the first 13 FP args are in registers.
   Under V.4, the first 8 FP args are in registers.

   If this is floating-point and no prototype is specified, we use
   both an FP and integer register (or possibly FP reg and stack).  Library
   functions (when CALL_LIBCALL is set) always have the proper types for args,
   so we can pass the FP value just in one register.  emit_library_function
   doesn't support PARALLEL anyway.

   Note that for args passed by reference, function_arg will be called
   with MODE and TYPE set to that of the pointer to the arg, not the arg
   itself.  */

static rtx
rs6000_function_arg (cumulative_args_t cum_v, enum machine_mode mode,
		     const_tree type, bool named)
{
  CUMULATIVE_ARGS *cum = get_cumulative_args (cum_v);
  enum rs6000_abi abi = DEFAULT_ABI;
  enum machine_mode elt_mode;
  int n_elts;

  /* Return a marker to indicate whether CR1 needs to set or clear the
     bit that V.4 uses to say fp args were passed in registers.
     Assume that we don't need the marker for software floating point,
     or compiler generated library calls.  */
  if (mode == VOIDmode)
    {
      if (abi == ABI_V4
	  && (cum->call_cookie & CALL_LIBCALL) == 0
	  && (cum->stdarg
	      || (cum->nargs_prototype < 0
		  && (cum->prototype || TARGET_NO_PROTOTYPE))))
	{
	  /* For the SPE, we need to crxor CR6 always.  */
	  if (TARGET_SPE_ABI)
	    return GEN_INT (cum->call_cookie | CALL_V4_SET_FP_ARGS);
	  else if (TARGET_HARD_FLOAT && TARGET_FPRS)
	    return GEN_INT (cum->call_cookie
			    | ((cum->fregno == FP_ARG_MIN_REG)
			       ? CALL_V4_SET_FP_ARGS
			       : CALL_V4_CLEAR_FP_ARGS));
	}

      return GEN_INT (cum->call_cookie & ~CALL_LIBCALL);
    }

  rs6000_discover_homogeneous_aggregate (mode, type, &elt_mode, &n_elts);

  if (TARGET_MACHO && rs6000_darwin64_struct_check_p (mode, type))
    {
      rtx rslt = rs6000_darwin64_record_arg (cum, type, named, /*retval= */false);
      if (rslt != NULL_RTX)
	return rslt;
      /* Else fall through to usual handling.  */
    }

  if (USE_ALTIVEC_FOR_ARG_P (cum, elt_mode, named))
    {
      rtx rvec[GP_ARG_NUM_REG + AGGR_ARG_NUM_REG + 1];
      rtx r, off;
      int i, k = 0;

      /* Do we also need to pass this argument in the parameter
	 save area?  */
      if (TARGET_64BIT && ! cum->prototype)
	{
	  int align_words = (cum->words + 1) & ~1;
	  k = rs6000_psave_function_arg (mode, type, align_words, rvec);
	}

      /* Describe where this argument goes in the vector registers.  */
      for (i = 0; i < n_elts && cum->vregno + i <= ALTIVEC_ARG_MAX_REG; i++)
	{
	  r = gen_rtx_REG (elt_mode, cum->vregno + i);
	  off = GEN_INT (i * GET_MODE_SIZE (elt_mode));
	  rvec[k++] =  gen_rtx_EXPR_LIST (VOIDmode, r, off);
	}

      return rs6000_finish_function_arg (mode, rvec, k);
    }
  else if (TARGET_ALTIVEC_ABI
	   && (ALTIVEC_OR_VSX_VECTOR_MODE (mode)
	       || (type && TREE_CODE (type) == VECTOR_TYPE
		   && int_size_in_bytes (type) == 16)))
    {
      if (named || abi == ABI_V4)
	return NULL_RTX;
      else
	{
	  /* Vector parameters to varargs functions under AIX or Darwin
	     get passed in memory and possibly also in GPRs.  */
	  int align, align_words, n_words;
	  enum machine_mode part_mode;

	  /* Vector parameters must be 16-byte aligned.  In 32-bit
	     mode this means we need to take into account the offset
	     to the parameter save area.  In 64-bit mode, they just
	     have to start on an even word, since the parameter save
	     area is 16-byte aligned.  */
	  if (TARGET_32BIT)
	    align = -(rs6000_parm_offset () + cum->words) & 3;
	  else
	    align = cum->words & 1;
	  align_words = cum->words + align;

	  /* Out of registers?  Memory, then.  */
	  if (align_words >= GP_ARG_NUM_REG)
	    return NULL_RTX;

	  if (TARGET_32BIT && TARGET_POWERPC64)
	    return rs6000_mixed_function_arg (mode, type, align_words);

	  /* The vector value goes in GPRs.  Only the part of the
	     value in GPRs is reported here.  */
	  part_mode = mode;
	  n_words = rs6000_arg_size (mode, type);
	  if (align_words + n_words > GP_ARG_NUM_REG)
	    /* Fortunately, there are only two possibilities, the value
	       is either wholly in GPRs or half in GPRs and half not.  */
	    part_mode = DImode;

	  return gen_rtx_REG (part_mode, GP_ARG_MIN_REG + align_words);
	}
    }
  else if (TARGET_SPE_ABI && TARGET_SPE
	   && (SPE_VECTOR_MODE (mode)
	       || (TARGET_E500_DOUBLE && (mode == DFmode
					  || mode == DCmode
					  || mode == TFmode
					  || mode == TCmode))))
    return rs6000_spe_function_arg (cum, mode, type);

  else if (abi == ABI_V4)
    {
      if (TARGET_HARD_FLOAT && TARGET_FPRS
	  && ((TARGET_SINGLE_FLOAT && mode == SFmode)
	      || (TARGET_DOUBLE_FLOAT && mode == DFmode)
	      || (mode == TFmode && !TARGET_IEEEQUAD)
	      || mode == SDmode || mode == DDmode || mode == TDmode))
	{
	  /* _Decimal128 must use an even/odd register pair.  This assumes
	     that the register number is odd when fregno is odd.  */
	  if (mode == TDmode && (cum->fregno % 2) == 1)
	    cum->fregno++;

	  if (cum->fregno + (mode == TFmode || mode == TDmode ? 1 : 0)
	      <= FP_ARG_V4_MAX_REG)
	    return gen_rtx_REG (mode, cum->fregno);
	  else
	    return NULL_RTX;
	}
      else
	{
	  int n_words = rs6000_arg_size (mode, type);
	  int gregno = cum->sysv_gregno;

	  /* Long long and SPE vectors are put in (r3,r4), (r5,r6),
	     (r7,r8) or (r9,r10).  As does any other 2 word item such
	     as complex int due to a historical mistake.  */
	  if (n_words == 2)
	    gregno += (1 - gregno) & 1;

	  /* Multi-reg args are not split between registers and stack.  */
	  if (gregno + n_words - 1 > GP_ARG_MAX_REG)
	    return NULL_RTX;

	  if (TARGET_32BIT && TARGET_POWERPC64)
	    return rs6000_mixed_function_arg (mode, type,
					      gregno - GP_ARG_MIN_REG);
	  return gen_rtx_REG (mode, gregno);
	}
    }
  else
    {
      int align_words = rs6000_parm_start (mode, type, cum->words);

      /* _Decimal128 must be passed in an even/odd float register pair.
	 This assumes that the register number is odd when fregno is odd.  */
      if (elt_mode == TDmode && (cum->fregno % 2) == 1)
	cum->fregno++;

      if (USE_FP_FOR_ARG_P (cum, elt_mode))
	{
	  rtx rvec[GP_ARG_NUM_REG + AGGR_ARG_NUM_REG + 1];
	  rtx r, off;
	  int i, k = 0;
	  unsigned long n_fpreg = (GET_MODE_SIZE (elt_mode) + 7) >> 3;

	  /* Do we also need to pass this argument in the parameter
	     save area?  */
	  if (type && (cum->nargs_prototype <= 0
		       || ((DEFAULT_ABI == ABI_AIX || DEFAULT_ABI == ABI_ELFv2)
			   && TARGET_XL_COMPAT
			   && align_words >= GP_ARG_NUM_REG)))
	    k = rs6000_psave_function_arg (mode, type, align_words, rvec);

	  /* Describe where this argument goes in the fprs.  */
	  for (i = 0; i < n_elts
		      && cum->fregno + i * n_fpreg <= FP_ARG_MAX_REG; i++)
	    {
	      /* Check if the argument is split over registers and memory.
		 This can only ever happen for long double or _Decimal128;
		 complex types are handled via split_complex_arg.  */
	      enum machine_mode fmode = elt_mode;
	      if (cum->fregno + (i + 1) * n_fpreg > FP_ARG_MAX_REG + 1)
		{
		  gcc_assert (fmode == TFmode || fmode == TDmode);
		  fmode = DECIMAL_FLOAT_MODE_P (fmode) ? DDmode : DFmode;
		}

	      r = gen_rtx_REG (fmode, cum->fregno + i * n_fpreg);
	      off = GEN_INT (i * GET_MODE_SIZE (elt_mode));
	      rvec[k++] = gen_rtx_EXPR_LIST (VOIDmode, r, off);
	    }

	  return rs6000_finish_function_arg (mode, rvec, k);
	}
      else if (align_words < GP_ARG_NUM_REG)
	{
	  if (TARGET_32BIT && TARGET_POWERPC64)
	    return rs6000_mixed_function_arg (mode, type, align_words);

	  return gen_rtx_REG (mode, GP_ARG_MIN_REG + align_words);
	}
      else
	return NULL_RTX;
    }
}

/* For an arg passed partly in registers and partly in memory, this is
   the number of bytes passed in registers.  For args passed entirely in
   registers or entirely in memory, zero.  When an arg is described by a
   PARALLEL, perhaps using more than one register type, this function
   returns the number of bytes used by the first element of the PARALLEL.  */

static int
rs6000_arg_partial_bytes (cumulative_args_t cum_v, enum machine_mode mode,
			  tree type, bool named)
{
  CUMULATIVE_ARGS *cum = get_cumulative_args (cum_v);
  bool passed_in_gprs = true;
  int ret = 0;
  int align_words;
  enum machine_mode elt_mode;
  int n_elts;

  rs6000_discover_homogeneous_aggregate (mode, type, &elt_mode, &n_elts);

  if (DEFAULT_ABI == ABI_V4)
    return 0;

  if (USE_ALTIVEC_FOR_ARG_P (cum, elt_mode, named))
    {
      /* If we are passing this arg in the fixed parameter save area
         (gprs or memory) as well as VRs, we do not use the partial
	 bytes mechanism; instead, rs6000_function_arg will return a
	 PARALLEL including a memory element as necessary.  */
      if (TARGET_64BIT && ! cum->prototype)
	return 0;

      /* Otherwise, we pass in VRs only.  Check for partial copies.  */
      passed_in_gprs = false;
      if (cum->vregno + n_elts > ALTIVEC_ARG_MAX_REG + 1)
	ret = (ALTIVEC_ARG_MAX_REG + 1 - cum->vregno) * 16;
    }

  /* In this complicated case we just disable the partial_nregs code.  */
  if (TARGET_MACHO && rs6000_darwin64_struct_check_p (mode, type))
    return 0;

  align_words = rs6000_parm_start (mode, type, cum->words);

  if (USE_FP_FOR_ARG_P (cum, elt_mode))
    {
      unsigned long n_fpreg = (GET_MODE_SIZE (elt_mode) + 7) >> 3;

      /* If we are passing this arg in the fixed parameter save area
         (gprs or memory) as well as FPRs, we do not use the partial
	 bytes mechanism; instead, rs6000_function_arg will return a
	 PARALLEL including a memory element as necessary.  */
      if (type
	  && (cum->nargs_prototype <= 0
	      || ((DEFAULT_ABI == ABI_AIX || DEFAULT_ABI == ABI_ELFv2)
		  && TARGET_XL_COMPAT
		  && align_words >= GP_ARG_NUM_REG)))
	return 0;

      /* Otherwise, we pass in FPRs only.  Check for partial copies.  */
      passed_in_gprs = false;
      if (cum->fregno + n_elts * n_fpreg > FP_ARG_MAX_REG + 1)
	ret = ((FP_ARG_MAX_REG + 1 - cum->fregno)
	       * MIN (8, GET_MODE_SIZE (elt_mode)));
    }

  if (passed_in_gprs
      && align_words < GP_ARG_NUM_REG
      && GP_ARG_NUM_REG < align_words + rs6000_arg_size (mode, type))
    ret = (GP_ARG_NUM_REG - align_words) * (TARGET_32BIT ? 4 : 8);

  if (ret != 0 && TARGET_DEBUG_ARG)
    fprintf (stderr, "rs6000_arg_partial_bytes: %d\n", ret);

  return ret;
}

/* A C expression that indicates when an argument must be passed by
   reference.  If nonzero for an argument, a copy of that argument is
   made in memory and a pointer to the argument is passed instead of
   the argument itself.  The pointer is passed in whatever way is
   appropriate for passing a pointer to that type.

   Under V.4, aggregates and long double are passed by reference.

   As an extension to all 32-bit ABIs, AltiVec vectors are passed by
   reference unless the AltiVec vector extension ABI is in force.

   As an extension to all ABIs, variable sized types are passed by
   reference.  */

static bool
rs6000_pass_by_reference (cumulative_args_t cum ATTRIBUTE_UNUSED,
			  enum machine_mode mode, const_tree type,
			  bool named ATTRIBUTE_UNUSED)
{
  if (DEFAULT_ABI == ABI_V4 && TARGET_IEEEQUAD && mode == TFmode)
    {
      if (TARGET_DEBUG_ARG)
	fprintf (stderr, "function_arg_pass_by_reference: V4 long double\n");
      return 1;
    }

  if (!type)
    return 0;

  if (DEFAULT_ABI == ABI_V4 && AGGREGATE_TYPE_P (type))
    {
      if (TARGET_DEBUG_ARG)
	fprintf (stderr, "function_arg_pass_by_reference: V4 aggregate\n");
      return 1;
    }

  if (int_size_in_bytes (type) < 0)
    {
      if (TARGET_DEBUG_ARG)
	fprintf (stderr, "function_arg_pass_by_reference: variable size\n");
      return 1;
    }

  /* Allow -maltivec -mabi=no-altivec without warning.  Altivec vector
     modes only exist for GCC vector types if -maltivec.  */
  if (TARGET_32BIT && !TARGET_ALTIVEC_ABI && ALTIVEC_VECTOR_MODE (mode))
    {
      if (TARGET_DEBUG_ARG)
	fprintf (stderr, "function_arg_pass_by_reference: AltiVec\n");
      return 1;
    }

  /* Pass synthetic vectors in memory.  */
  if (TREE_CODE (type) == VECTOR_TYPE
      && int_size_in_bytes (type) > (TARGET_ALTIVEC_ABI ? 16 : 8))
    {
      static bool warned_for_pass_big_vectors = false;
      if (TARGET_DEBUG_ARG)
	fprintf (stderr, "function_arg_pass_by_reference: synthetic vector\n");
      if (!warned_for_pass_big_vectors)
	{
	  warning (0, "GCC vector passed by reference: "
		   "non-standard ABI extension with no compatibility guarantee");
	  warned_for_pass_big_vectors = true;
	}
      return 1;
    }

  return 0;
}

/* Process parameter of type TYPE after ARGS_SO_FAR parameters were
   already processes.  Return true if the parameter must be passed
   (fully or partially) on the stack.  */

static bool
rs6000_parm_needs_stack (cumulative_args_t args_so_far, tree type)
{
  enum machine_mode mode;
  int unsignedp;
  rtx entry_parm;

  /* Catch errors.  */
  if (type == NULL || type == error_mark_node)
    return true;

  /* Handle types with no storage requirement.  */
  if (TYPE_MODE (type) == VOIDmode)
    return false;

  /* Handle complex types.  */
  if (TREE_CODE (type) == COMPLEX_TYPE)
    return (rs6000_parm_needs_stack (args_so_far, TREE_TYPE (type))
	    || rs6000_parm_needs_stack (args_so_far, TREE_TYPE (type)));

  /* Handle transparent aggregates.  */
  if ((TREE_CODE (type) == UNION_TYPE || TREE_CODE (type) == RECORD_TYPE)
      && TYPE_TRANSPARENT_AGGR (type))
    type = TREE_TYPE (first_field (type));

  /* See if this arg was passed by invisible reference.  */
  if (pass_by_reference (get_cumulative_args (args_so_far),
			 TYPE_MODE (type), type, true))
    type = build_pointer_type (type);

  /* Find mode as it is passed by the ABI.  */
  unsignedp = TYPE_UNSIGNED (type);
  mode = promote_mode (type, TYPE_MODE (type), &unsignedp);

  /* If we must pass in stack, we need a stack.  */
  if (rs6000_must_pass_in_stack (mode, type))
    return true;

  /* If there is no incoming register, we need a stack.  */
  entry_parm = rs6000_function_arg (args_so_far, mode, type, true);
  if (entry_parm == NULL)
    return true;

  /* Likewise if we need to pass both in registers and on the stack.  */
  if (GET_CODE (entry_parm) == PARALLEL
      && XEXP (XVECEXP (entry_parm, 0, 0), 0) == NULL_RTX)
    return true;

  /* Also true if we're partially in registers and partially not.  */
  if (rs6000_arg_partial_bytes (args_so_far, mode, type, true) != 0)
    return true;

  /* Update info on where next arg arrives in registers.  */
  rs6000_function_arg_advance (args_so_far, mode, type, true);
  return false;
}

/* Return true if FUN has no prototype, has a variable argument
   list, or passes any parameter in memory.  */

static bool
rs6000_function_parms_need_stack (tree fun)
{
  function_args_iterator args_iter;
  tree arg_type;
  CUMULATIVE_ARGS args_so_far_v;
  cumulative_args_t args_so_far;

  if (!fun)
    /* Must be a libcall, all of which only use reg parms.  */
    return false;
  if (!TYPE_P (fun))
    fun = TREE_TYPE (fun);

  /* Varargs functions need the parameter save area.  */
  if (!prototype_p (fun) || stdarg_p (fun))
    return true;

  INIT_CUMULATIVE_INCOMING_ARGS (args_so_far_v, fun, NULL_RTX);
  args_so_far = pack_cumulative_args (&args_so_far_v);

  if (aggregate_value_p (TREE_TYPE (fun), fun))
    {
      tree type = build_pointer_type (TREE_TYPE (fun));
      rs6000_parm_needs_stack (args_so_far, type);
    }

  FOREACH_FUNCTION_ARGS (fun, arg_type, args_iter)
    if (rs6000_parm_needs_stack (args_so_far, arg_type))
      return true;

  return false;
}

/* Return the size of the REG_PARM_STACK_SPACE are for FUN.  This is
   usually a constant depending on the ABI.  However, in the ELFv2 ABI
   the register parameter area is optional when calling a function that
   has a prototype is scope, has no variable argument list, and passes
   all parameters in registers.  */

int
rs6000_reg_parm_stack_space (tree fun)
{
  int reg_parm_stack_space;

  switch (DEFAULT_ABI)
    {
    default:
      reg_parm_stack_space = 0;
      break;

    case ABI_AIX:
    case ABI_DARWIN:
      reg_parm_stack_space = TARGET_64BIT ? 64 : 32;
      break;

    case ABI_ELFv2:
      /* ??? Recomputing this every time is a bit expensive.  Is there
	 a place to cache this information?  */
      if (rs6000_function_parms_need_stack (fun))
	reg_parm_stack_space = TARGET_64BIT ? 64 : 32;
      else
	reg_parm_stack_space = 0;
      break;
    }

  return reg_parm_stack_space;
}

static void
rs6000_move_block_from_reg (int regno, rtx x, int nregs)
{
  int i;
  enum machine_mode reg_mode = TARGET_32BIT ? SImode : DImode;

  if (nregs == 0)
    return;

  for (i = 0; i < nregs; i++)
    {
      rtx tem = adjust_address_nv (x, reg_mode, i * GET_MODE_SIZE (reg_mode));
      if (reload_completed)
	{
	  if (! strict_memory_address_p (reg_mode, XEXP (tem, 0)))
	    tem = NULL_RTX;
	  else
	    tem = simplify_gen_subreg (reg_mode, x, BLKmode,
				       i * GET_MODE_SIZE (reg_mode));
	}
      else
	tem = replace_equiv_address (tem, XEXP (tem, 0));

      gcc_assert (tem);

      emit_move_insn (tem, gen_rtx_REG (reg_mode, regno + i));
    }
}

/* Perform any needed actions needed for a function that is receiving a
   variable number of arguments.

   CUM is as above.

   MODE and TYPE are the mode and type of the current parameter.

   PRETEND_SIZE is a variable that should be set to the amount of stack
   that must be pushed by the prolog to pretend that our caller pushed
   it.

   Normally, this macro will push all remaining incoming registers on the
   stack and set PRETEND_SIZE to the length of the registers pushed.  */

static void
setup_incoming_varargs (cumulative_args_t cum, enum machine_mode mode,
			tree type, int *pretend_size ATTRIBUTE_UNUSED,
			int no_rtl)
{
  CUMULATIVE_ARGS next_cum;
  int reg_size = TARGET_32BIT ? 4 : 8;
  rtx save_area = NULL_RTX, mem;
  int first_reg_offset;
  alias_set_type set;

  /* Skip the last named argument.  */
  next_cum = *get_cumulative_args (cum);
  rs6000_function_arg_advance_1 (&next_cum, mode, type, true, 0);

  if (DEFAULT_ABI == ABI_V4)
    {
      first_reg_offset = next_cum.sysv_gregno - GP_ARG_MIN_REG;

      if (! no_rtl)
	{
	  int gpr_reg_num = 0, gpr_size = 0, fpr_size = 0;
	  HOST_WIDE_INT offset = 0;

	  /* Try to optimize the size of the varargs save area.
	     The ABI requires that ap.reg_save_area is doubleword
	     aligned, but we don't need to allocate space for all
	     the bytes, only those to which we actually will save
	     anything.  */
	  if (cfun->va_list_gpr_size && first_reg_offset < GP_ARG_NUM_REG)
	    gpr_reg_num = GP_ARG_NUM_REG - first_reg_offset;
	  if (TARGET_HARD_FLOAT && TARGET_FPRS
	      && next_cum.fregno <= FP_ARG_V4_MAX_REG
	      && cfun->va_list_fpr_size)
	    {
	      if (gpr_reg_num)
		fpr_size = (next_cum.fregno - FP_ARG_MIN_REG)
			   * UNITS_PER_FP_WORD;
	      if (cfun->va_list_fpr_size
		  < FP_ARG_V4_MAX_REG + 1 - next_cum.fregno)
		fpr_size += cfun->va_list_fpr_size * UNITS_PER_FP_WORD;
	      else
		fpr_size += (FP_ARG_V4_MAX_REG + 1 - next_cum.fregno)
			    * UNITS_PER_FP_WORD;
	    }
	  if (gpr_reg_num)
	    {
	      offset = -((first_reg_offset * reg_size) & ~7);
	      if (!fpr_size && gpr_reg_num > cfun->va_list_gpr_size)
		{
		  gpr_reg_num = cfun->va_list_gpr_size;
		  if (reg_size == 4 && (first_reg_offset & 1))
		    gpr_reg_num++;
		}
	      gpr_size = (gpr_reg_num * reg_size + 7) & ~7;
	    }
	  else if (fpr_size)
	    offset = - (int) (next_cum.fregno - FP_ARG_MIN_REG)
		       * UNITS_PER_FP_WORD
		     - (int) (GP_ARG_NUM_REG * reg_size);

	  if (gpr_size + fpr_size)
	    {
	      rtx reg_save_area
		= assign_stack_local (BLKmode, gpr_size + fpr_size, 64);
	      gcc_assert (GET_CODE (reg_save_area) == MEM);
	      reg_save_area = XEXP (reg_save_area, 0);
	      if (GET_CODE (reg_save_area) == PLUS)
		{
		  gcc_assert (XEXP (reg_save_area, 0)
			      == virtual_stack_vars_rtx);
		  gcc_assert (GET_CODE (XEXP (reg_save_area, 1)) == CONST_INT);
		  offset += INTVAL (XEXP (reg_save_area, 1));
		}
	      else
		gcc_assert (reg_save_area == virtual_stack_vars_rtx);
	    }

	  cfun->machine->varargs_save_offset = offset;
	  save_area = plus_constant (Pmode, virtual_stack_vars_rtx, offset);
	}
    }
  else
    {
      first_reg_offset = next_cum.words;
      save_area = virtual_incoming_args_rtx;

      if (targetm.calls.must_pass_in_stack (mode, type))
	first_reg_offset += rs6000_arg_size (TYPE_MODE (type), type);
    }

  set = get_varargs_alias_set ();
  if (! no_rtl && first_reg_offset < GP_ARG_NUM_REG
      && cfun->va_list_gpr_size)
    {
      int n_gpr, nregs = GP_ARG_NUM_REG - first_reg_offset;

      if (va_list_gpr_counter_field)
	/* V4 va_list_gpr_size counts number of registers needed.  */
	n_gpr = cfun->va_list_gpr_size;
      else
	/* char * va_list instead counts number of bytes needed.  */
	n_gpr = (cfun->va_list_gpr_size + reg_size - 1) / reg_size;

      if (nregs > n_gpr)
	nregs = n_gpr;

      mem = gen_rtx_MEM (BLKmode,
			 plus_constant (Pmode, save_area,
					first_reg_offset * reg_size));
      MEM_NOTRAP_P (mem) = 1;
      set_mem_alias_set (mem, set);
      set_mem_align (mem, BITS_PER_WORD);

      rs6000_move_block_from_reg (GP_ARG_MIN_REG + first_reg_offset, mem,
				  nregs);
    }

  /* Save FP registers if needed.  */
  if (DEFAULT_ABI == ABI_V4
      && TARGET_HARD_FLOAT && TARGET_FPRS
      && ! no_rtl
      && next_cum.fregno <= FP_ARG_V4_MAX_REG
      && cfun->va_list_fpr_size)
    {
      int fregno = next_cum.fregno, nregs;
      rtx cr1 = gen_rtx_REG (CCmode, CR1_REGNO);
      rtx lab = gen_label_rtx ();
      int off = (GP_ARG_NUM_REG * reg_size) + ((fregno - FP_ARG_MIN_REG)
					       * UNITS_PER_FP_WORD);

      emit_jump_insn
	(gen_rtx_SET (VOIDmode,
		      pc_rtx,
		      gen_rtx_IF_THEN_ELSE (VOIDmode,
					    gen_rtx_NE (VOIDmode, cr1,
							const0_rtx),
					    gen_rtx_LABEL_REF (VOIDmode, lab),
					    pc_rtx)));

      for (nregs = 0;
	   fregno <= FP_ARG_V4_MAX_REG && nregs < cfun->va_list_fpr_size;
	   fregno++, off += UNITS_PER_FP_WORD, nregs++)
	{
	  mem = gen_rtx_MEM ((TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT)
			      ? DFmode : SFmode, 
                             plus_constant (Pmode, save_area, off));
  	  MEM_NOTRAP_P (mem) = 1;
  	  set_mem_alias_set (mem, set);
	  set_mem_align (mem, GET_MODE_ALIGNMENT (
			 (TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT)
			  ? DFmode : SFmode));
	  emit_move_insn (mem, gen_rtx_REG (
                          (TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT)
			   ? DFmode : SFmode, fregno));
	}

      emit_label (lab);
    }
}

/* Create the va_list data type.  */

static tree
rs6000_build_builtin_va_list (void)
{
  tree f_gpr, f_fpr, f_res, f_ovf, f_sav, record, type_decl;

  /* For AIX, prefer 'char *' because that's what the system
     header files like.  */
  if (DEFAULT_ABI != ABI_V4)
    return build_pointer_type (char_type_node);

  record = (*lang_hooks.types.make_type) (RECORD_TYPE);
  type_decl = build_decl (BUILTINS_LOCATION, TYPE_DECL,
      			  get_identifier ("__va_list_tag"), record);

  f_gpr = build_decl (BUILTINS_LOCATION, FIELD_DECL, get_identifier ("gpr"),
		      unsigned_char_type_node);
  f_fpr = build_decl (BUILTINS_LOCATION, FIELD_DECL, get_identifier ("fpr"),
		      unsigned_char_type_node);
  /* Give the two bytes of padding a name, so that -Wpadded won't warn on
     every user file.  */
  f_res = build_decl (BUILTINS_LOCATION, FIELD_DECL,
      		      get_identifier ("reserved"), short_unsigned_type_node);
  f_ovf = build_decl (BUILTINS_LOCATION, FIELD_DECL,
      		      get_identifier ("overflow_arg_area"),
		      ptr_type_node);
  f_sav = build_decl (BUILTINS_LOCATION, FIELD_DECL,
      		      get_identifier ("reg_save_area"),
		      ptr_type_node);

  va_list_gpr_counter_field = f_gpr;
  va_list_fpr_counter_field = f_fpr;

  DECL_FIELD_CONTEXT (f_gpr) = record;
  DECL_FIELD_CONTEXT (f_fpr) = record;
  DECL_FIELD_CONTEXT (f_res) = record;
  DECL_FIELD_CONTEXT (f_ovf) = record;
  DECL_FIELD_CONTEXT (f_sav) = record;

  TYPE_STUB_DECL (record) = type_decl;
  TYPE_NAME (record) = type_decl;
  TYPE_FIELDS (record) = f_gpr;
  DECL_CHAIN (f_gpr) = f_fpr;
  DECL_CHAIN (f_fpr) = f_res;
  DECL_CHAIN (f_res) = f_ovf;
  DECL_CHAIN (f_ovf) = f_sav;

  layout_type (record);

  /* The correct type is an array type of one element.  */
  return build_array_type (record, build_index_type (size_zero_node));
}

/* Implement va_start.  */

static void
rs6000_va_start (tree valist, rtx nextarg)
{
  HOST_WIDE_INT words, n_gpr, n_fpr;
  tree f_gpr, f_fpr, f_res, f_ovf, f_sav;
  tree gpr, fpr, ovf, sav, t;

  /* Only SVR4 needs something special.  */
  if (DEFAULT_ABI != ABI_V4)
    {
      std_expand_builtin_va_start (valist, nextarg);
      return;
    }

  f_gpr = TYPE_FIELDS (TREE_TYPE (va_list_type_node));
  f_fpr = DECL_CHAIN (f_gpr);
  f_res = DECL_CHAIN (f_fpr);
  f_ovf = DECL_CHAIN (f_res);
  f_sav = DECL_CHAIN (f_ovf);

  valist = build_simple_mem_ref (valist);
  gpr = build3 (COMPONENT_REF, TREE_TYPE (f_gpr), valist, f_gpr, NULL_TREE);
  fpr = build3 (COMPONENT_REF, TREE_TYPE (f_fpr), unshare_expr (valist),
		f_fpr, NULL_TREE);
  ovf = build3 (COMPONENT_REF, TREE_TYPE (f_ovf), unshare_expr (valist),
		f_ovf, NULL_TREE);
  sav = build3 (COMPONENT_REF, TREE_TYPE (f_sav), unshare_expr (valist),
		f_sav, NULL_TREE);

  /* Count number of gp and fp argument registers used.  */
  words = crtl->args.info.words;
  n_gpr = MIN (crtl->args.info.sysv_gregno - GP_ARG_MIN_REG,
	       GP_ARG_NUM_REG);
  n_fpr = MIN (crtl->args.info.fregno - FP_ARG_MIN_REG,
	       FP_ARG_NUM_REG);

  if (TARGET_DEBUG_ARG)
    fprintf (stderr, "va_start: words = "HOST_WIDE_INT_PRINT_DEC", n_gpr = "
	     HOST_WIDE_INT_PRINT_DEC", n_fpr = "HOST_WIDE_INT_PRINT_DEC"\n",
	     words, n_gpr, n_fpr);

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

#ifdef HAVE_AS_GNU_ATTRIBUTE
      if (call_ABI_of_interest (cfun->decl))
	rs6000_passes_float = true;
#endif
    }

  /* Find the overflow area.  */
  t = make_tree (TREE_TYPE (ovf), virtual_incoming_args_rtx);
  if (words != 0)
    t = fold_build_pointer_plus_hwi (t, words * UNITS_PER_WORD);
  t = build2 (MODIFY_EXPR, TREE_TYPE (ovf), ovf, t);
  TREE_SIDE_EFFECTS (t) = 1;
  expand_expr (t, const0_rtx, VOIDmode, EXPAND_NORMAL);

  /* If there were no va_arg invocations, don't set up the register
     save area.  */
  if (!cfun->va_list_gpr_size
      && !cfun->va_list_fpr_size
      && n_gpr < GP_ARG_NUM_REG
      && n_fpr < FP_ARG_V4_MAX_REG)
    return;

  /* Find the register save area.  */
  t = make_tree (TREE_TYPE (sav), virtual_stack_vars_rtx);
  if (cfun->machine->varargs_save_offset)
    t = fold_build_pointer_plus_hwi (t, cfun->machine->varargs_save_offset);
  t = build2 (MODIFY_EXPR, TREE_TYPE (sav), sav, t);
  TREE_SIDE_EFFECTS (t) = 1;
  expand_expr (t, const0_rtx, VOIDmode, EXPAND_NORMAL);
}

/* Implement va_arg.  */

static tree
rs6000_gimplify_va_arg (tree valist, tree type, gimple_seq *pre_p,
			gimple_seq *post_p)
{
  tree f_gpr, f_fpr, f_res, f_ovf, f_sav;
  tree gpr, fpr, ovf, sav, reg, t, u;
  int size, rsize, n_reg, sav_ofs, sav_scale;
  tree lab_false, lab_over, addr;
  int align;
  tree ptrtype = build_pointer_type_for_mode (type, ptr_mode, true);
  int regalign = 0;
  gimple stmt;

  if (pass_by_reference (NULL, TYPE_MODE (type), type, false))
    {
      t = rs6000_gimplify_va_arg (valist, ptrtype, pre_p, post_p);
      return build_va_arg_indirect_ref (t);
    }

  /* We need to deal with the fact that the darwin ppc64 ABI is defined by an
     earlier version of gcc, with the property that it always applied alignment
     adjustments to the va-args (even for zero-sized types).  The cheapest way
     to deal with this is to replicate the effect of the part of 
     std_gimplify_va_arg_expr that carries out the align adjust, for the case 
     of relevance.  
     We don't need to check for pass-by-reference because of the test above.
     We can return a simplifed answer, since we know there's no offset to add.  */

  if (((TARGET_MACHO
        && rs6000_darwin64_abi)
       || DEFAULT_ABI == ABI_ELFv2
       || (DEFAULT_ABI == ABI_AIX && !rs6000_compat_align_parm))
      && integer_zerop (TYPE_SIZE (type)))
    {
      unsigned HOST_WIDE_INT align, boundary;
      tree valist_tmp = get_initialized_tmp_var (valist, pre_p, NULL);
      align = PARM_BOUNDARY / BITS_PER_UNIT;
      boundary = rs6000_function_arg_boundary (TYPE_MODE (type), type);
      if (boundary > MAX_SUPPORTED_STACK_ALIGNMENT)
	boundary = MAX_SUPPORTED_STACK_ALIGNMENT;
      boundary /= BITS_PER_UNIT;
      if (boundary > align)
	{
	  tree t ;
	  /* This updates arg ptr by the amount that would be necessary
	     to align the zero-sized (but not zero-alignment) item.  */
	  t = build2 (MODIFY_EXPR, TREE_TYPE (valist), valist_tmp,
		      fold_build_pointer_plus_hwi (valist_tmp, boundary - 1));
	  gimplify_and_add (t, pre_p);

	  t = fold_convert (sizetype, valist_tmp);
	  t = build2 (MODIFY_EXPR, TREE_TYPE (valist), valist_tmp,
		  fold_convert (TREE_TYPE (valist),
				fold_build2 (BIT_AND_EXPR, sizetype, t,
					     size_int (-boundary))));
	  t = build2 (MODIFY_EXPR, TREE_TYPE (valist), valist, t);
	  gimplify_and_add (t, pre_p);
	}
      /* Since it is zero-sized there's no increment for the item itself. */
      valist_tmp = fold_convert (build_pointer_type (type), valist_tmp);
      return build_va_arg_indirect_ref (valist_tmp);
    }

  if (DEFAULT_ABI != ABI_V4)
    {
      if (targetm.calls.split_complex_arg && TREE_CODE (type) == COMPLEX_TYPE)
	{
	  tree elem_type = TREE_TYPE (type);
	  enum machine_mode elem_mode = TYPE_MODE (elem_type);
	  int elem_size = GET_MODE_SIZE (elem_mode);

	  if (elem_size < UNITS_PER_WORD)
	    {
	      tree real_part, imag_part;
	      gimple_seq post = NULL;

	      real_part = rs6000_gimplify_va_arg (valist, elem_type, pre_p,
						  &post);
	      /* Copy the value into a temporary, lest the formal temporary
		 be reused out from under us.  */
	      real_part = get_initialized_tmp_var (real_part, pre_p, &post);
	      gimple_seq_add_seq (pre_p, post);

	      imag_part = rs6000_gimplify_va_arg (valist, elem_type, pre_p,
						  post_p);

	      return build2 (COMPLEX_EXPR, type, real_part, imag_part);
	    }
	}

      return std_gimplify_va_arg_expr (valist, type, pre_p, post_p);
    }

  f_gpr = TYPE_FIELDS (TREE_TYPE (va_list_type_node));
  f_fpr = DECL_CHAIN (f_gpr);
  f_res = DECL_CHAIN (f_fpr);
  f_ovf = DECL_CHAIN (f_res);
  f_sav = DECL_CHAIN (f_ovf);

  valist = build_va_arg_indirect_ref (valist);
  gpr = build3 (COMPONENT_REF, TREE_TYPE (f_gpr), valist, f_gpr, NULL_TREE);
  fpr = build3 (COMPONENT_REF, TREE_TYPE (f_fpr), unshare_expr (valist),
		f_fpr, NULL_TREE);
  ovf = build3 (COMPONENT_REF, TREE_TYPE (f_ovf), unshare_expr (valist),
		f_ovf, NULL_TREE);
  sav = build3 (COMPONENT_REF, TREE_TYPE (f_sav), unshare_expr (valist),
		f_sav, NULL_TREE);

  size = int_size_in_bytes (type);
  rsize = (size + 3) / 4;
  align = 1;

  if (TARGET_HARD_FLOAT && TARGET_FPRS
      && ((TARGET_SINGLE_FLOAT && TYPE_MODE (type) == SFmode)
          || (TARGET_DOUBLE_FLOAT 
              && (TYPE_MODE (type) == DFmode 
 	          || TYPE_MODE (type) == TFmode
	          || TYPE_MODE (type) == SDmode
	          || TYPE_MODE (type) == DDmode
	          || TYPE_MODE (type) == TDmode))))
    {
      /* FP args go in FP registers, if present.  */
      reg = fpr;
      n_reg = (size + 7) / 8;
      sav_ofs = ((TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT) ? 8 : 4) * 4;
      sav_scale = ((TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT) ? 8 : 4);
      if (TYPE_MODE (type) != SFmode && TYPE_MODE (type) != SDmode)
	align = 8;
    }
  else
    {
      /* Otherwise into GP registers.  */
      reg = gpr;
      n_reg = rsize;
      sav_ofs = 0;
      sav_scale = 4;
      if (n_reg == 2)
	align = 8;
    }

  /* Pull the value out of the saved registers....  */

  lab_over = NULL;
  addr = create_tmp_var (ptr_type_node, "addr");

  /*  AltiVec vectors never go in registers when -mabi=altivec.  */
  if (TARGET_ALTIVEC_ABI && ALTIVEC_VECTOR_MODE (TYPE_MODE (type)))
    align = 16;
  else
    {
      lab_false = create_artificial_label (input_location);
      lab_over = create_artificial_label (input_location);

      /* Long long and SPE vectors are aligned in the registers.
	 As are any other 2 gpr item such as complex int due to a
	 historical mistake.  */
      u = reg;
      if (n_reg == 2 && reg == gpr)
	{
	  regalign = 1;
	  u = build2 (BIT_AND_EXPR, TREE_TYPE (reg), unshare_expr (reg),
		     build_int_cst (TREE_TYPE (reg), n_reg - 1));
	  u = build2 (POSTINCREMENT_EXPR, TREE_TYPE (reg),
		      unshare_expr (reg), u);
	}
      /* _Decimal128 is passed in even/odd fpr pairs; the stored
	 reg number is 0 for f1, so we want to make it odd.  */
      else if (reg == fpr && TYPE_MODE (type) == TDmode)
	{
	  t = build2 (BIT_IOR_EXPR, TREE_TYPE (reg), unshare_expr (reg),
		      build_int_cst (TREE_TYPE (reg), 1));
	  u = build2 (MODIFY_EXPR, void_type_node, unshare_expr (reg), t);
	}

      t = fold_convert (TREE_TYPE (reg), size_int (8 - n_reg + 1));
      t = build2 (GE_EXPR, boolean_type_node, u, t);
      u = build1 (GOTO_EXPR, void_type_node, lab_false);
      t = build3 (COND_EXPR, void_type_node, t, u, NULL_TREE);
      gimplify_and_add (t, pre_p);

      t = sav;
      if (sav_ofs)
	t = fold_build_pointer_plus_hwi (sav, sav_ofs);

      u = build2 (POSTINCREMENT_EXPR, TREE_TYPE (reg), unshare_expr (reg),
		  build_int_cst (TREE_TYPE (reg), n_reg));
      u = fold_convert (sizetype, u);
      u = build2 (MULT_EXPR, sizetype, u, size_int (sav_scale));
      t = fold_build_pointer_plus (t, u);

      /* _Decimal32 varargs are located in the second word of the 64-bit
	 FP register for 32-bit binaries.  */
      if (!TARGET_POWERPC64
	  && TARGET_HARD_FLOAT && TARGET_FPRS
	  && TYPE_MODE (type) == SDmode)
	t = fold_build_pointer_plus_hwi (t, size);

      gimplify_assign (addr, t, pre_p);

      gimple_seq_add_stmt (pre_p, gimple_build_goto (lab_over));

      stmt = gimple_build_label (lab_false);
      gimple_seq_add_stmt (pre_p, stmt);

      if ((n_reg == 2 && !regalign) || n_reg > 2)
	{
	  /* Ensure that we don't find any more args in regs.
	     Alignment has taken care of for special cases.  */
	  gimplify_assign (reg, build_int_cst (TREE_TYPE (reg), 8), pre_p);
	}
    }

  /* ... otherwise out of the overflow area.  */

  /* Care for on-stack alignment if needed.  */
  t = ovf;
  if (align != 1)
    {
      t = fold_build_pointer_plus_hwi (t, align - 1);
      t = build2 (BIT_AND_EXPR, TREE_TYPE (t), t,
		  build_int_cst (TREE_TYPE (t), -align));
    }
  gimplify_expr (&t, pre_p, NULL, is_gimple_val, fb_rvalue);

  gimplify_assign (unshare_expr (addr), t, pre_p);

  t = fold_build_pointer_plus_hwi (t, size);
  gimplify_assign (unshare_expr (ovf), t, pre_p);

  if (lab_over)
    {
      stmt = gimple_build_label (lab_over);
      gimple_seq_add_stmt (pre_p, stmt);
    }

  if (STRICT_ALIGNMENT
      && (TYPE_ALIGN (type)
	  > (unsigned) BITS_PER_UNIT * (align < 4 ? 4 : align)))
    {
      /* The value (of type complex double, for example) may not be
	 aligned in memory in the saved registers, so copy via a
	 temporary.  (This is the same code as used for SPARC.)  */
      tree tmp = create_tmp_var (type, "va_arg_tmp");
      tree dest_addr = build_fold_addr_expr (tmp);

      tree copy = build_call_expr (builtin_decl_implicit (BUILT_IN_MEMCPY),
				   3, dest_addr, addr, size_int (rsize * 4));

      gimplify_and_add (copy, pre_p);
      addr = dest_addr;
    }

  addr = fold_convert (ptrtype, addr);
  return build_va_arg_indirect_ref (addr);
}

/* Builtins.  */

static void
def_builtin (const char *name, tree type, enum rs6000_builtins code)
{
  tree t;
  unsigned classify = rs6000_builtin_info[(int)code].attr;
  const char *attr_string = "";

  gcc_assert (name != NULL);
  gcc_assert (IN_RANGE ((int)code, 0, (int)RS6000_BUILTIN_COUNT));

  if (rs6000_builtin_decls[(int)code])
    fatal_error ("internal error: builtin function %s already processed", name);

  rs6000_builtin_decls[(int)code] = t =
    add_builtin_function (name, type, (int)code, BUILT_IN_MD, NULL, NULL_TREE);

  /* Set any special attributes.  */
  if ((classify & RS6000_BTC_CONST) != 0)
    {
      /* const function, function only depends on the inputs.  */
      TREE_READONLY (t) = 1;
      TREE_NOTHROW (t) = 1;
      attr_string = ", pure";
    }
  else if ((classify & RS6000_BTC_PURE) != 0)
    {
      /* pure function, function can read global memory, but does not set any
	 external state.  */
      DECL_PURE_P (t) = 1;
      TREE_NOTHROW (t) = 1;
      attr_string = ", const";
    }
  else if ((classify & RS6000_BTC_FP) != 0)
    {
      /* Function is a math function.  If rounding mode is on, then treat the
	 function as not reading global memory, but it can have arbitrary side
	 effects.  If it is off, then assume the function is a const function.
	 This mimics the ATTR_MATHFN_FPROUNDING attribute in
	 builtin-attribute.def that is used for the math functions. */
      TREE_NOTHROW (t) = 1;
      if (flag_rounding_math)
	{
	  DECL_PURE_P (t) = 1;
	  DECL_IS_NOVOPS (t) = 1;
	  attr_string = ", fp, pure";
	}
      else
	{
	  TREE_READONLY (t) = 1;
	  attr_string = ", fp, const";
	}
    }
  else if ((classify & RS6000_BTC_ATTR_MASK) != 0)
    gcc_unreachable ();

  if (TARGET_DEBUG_BUILTIN)
    fprintf (stderr, "rs6000_builtin, code = %4d, %s%s\n",
	     (int)code, name, attr_string);
}

/* Simple ternary operations: VECd = foo (VECa, VECb, VECc).  */

#undef RS6000_BUILTIN_1
#undef RS6000_BUILTIN_2
#undef RS6000_BUILTIN_3
#undef RS6000_BUILTIN_A
#undef RS6000_BUILTIN_D
#undef RS6000_BUILTIN_E
#undef RS6000_BUILTIN_H
#undef RS6000_BUILTIN_P
#undef RS6000_BUILTIN_Q
#undef RS6000_BUILTIN_S
#undef RS6000_BUILTIN_X

#define RS6000_BUILTIN_1(ENUM, NAME, MASK, ATTR, ICODE)
#define RS6000_BUILTIN_2(ENUM, NAME, MASK, ATTR, ICODE)
#define RS6000_BUILTIN_3(ENUM, NAME, MASK, ATTR, ICODE) \
  { MASK, ICODE, NAME, ENUM },

#define RS6000_BUILTIN_A(ENUM, NAME, MASK, ATTR, ICODE)
#define RS6000_BUILTIN_D(ENUM, NAME, MASK, ATTR, ICODE)
#define RS6000_BUILTIN_E(ENUM, NAME, MASK, ATTR, ICODE)
#define RS6000_BUILTIN_H(ENUM, NAME, MASK, ATTR, ICODE)
#define RS6000_BUILTIN_P(ENUM, NAME, MASK, ATTR, ICODE)
#define RS6000_BUILTIN_Q(ENUM, NAME, MASK, ATTR, ICODE)
#define RS6000_BUILTIN_S(ENUM, NAME, MASK, ATTR, ICODE)
#define RS6000_BUILTIN_X(ENUM, NAME, MASK, ATTR, ICODE)

static const struct builtin_description bdesc_3arg[] =
{
#include "rs6000-builtin.def"
};

/* DST operations: void foo (void *, const int, const char).  */

#undef RS6000_BUILTIN_1
#undef RS6000_BUILTIN_2
#undef RS6000_BUILTIN_3
#undef RS6000_BUILTIN_A
#undef RS6000_BUILTIN_D
#undef RS6000_BUILTIN_E
#undef RS6000_BUILTIN_H
#undef RS6000_BUILTIN_P
#undef RS6000_BUILTIN_Q
#undef RS6000_BUILTIN_S
#undef RS6000_BUILTIN_X

#define RS6000_BUILTIN_1(ENUM, NAME, MASK, ATTR, ICODE)
#define RS6000_BUILTIN_2(ENUM, NAME, MASK, ATTR, ICODE)
#define RS6000_BUILTIN_3(ENUM, NAME, MASK, ATTR, ICODE)
#define RS6000_BUILTIN_A(ENUM, NAME, MASK, ATTR, ICODE)
#define RS6000_BUILTIN_D(ENUM, NAME, MASK, ATTR, ICODE) \
  { MASK, ICODE, NAME, ENUM },

#define RS6000_BUILTIN_E(ENUM, NAME, MASK, ATTR, ICODE)
#define RS6000_BUILTIN_H(ENUM, NAME, MASK, ATTR, ICODE)
#define RS6000_BUILTIN_P(ENUM, NAME, MASK, ATTR, ICODE)
#define RS6000_BUILTIN_Q(ENUM, NAME, MASK, ATTR, ICODE)
#define RS6000_BUILTIN_S(ENUM, NAME, MASK, ATTR, ICODE)
#define RS6000_BUILTIN_X(ENUM, NAME, MASK, ATTR, ICODE)

static const struct builtin_description bdesc_dst[] =
{
#include "rs6000-builtin.def"
};

/* Simple binary operations: VECc = foo (VECa, VECb).  */

#undef RS6000_BUILTIN_1
#undef RS6000_BUILTIN_2
#undef RS6000_BUILTIN_3
#undef RS6000_BUILTIN_A
#undef RS6000_BUILTIN_D
#undef RS6000_BUILTIN_E
#undef RS6000_BUILTIN_H
#undef RS6000_BUILTIN_P
#undef RS6000_BUILTIN_Q
#undef RS6000_BUILTIN_S
#undef RS6000_BUILTIN_X

#define RS6000_BUILTIN_1(ENUM, NAME, MASK, ATTR, ICODE)
#define RS6000_BUILTIN_2(ENUM, NAME, MASK, ATTR, ICODE) \
  { MASK, ICODE, NAME, ENUM },

#define RS6000_BUILTIN_3(ENUM, NAME, MASK, ATTR, ICODE)
#define RS6000_BUILTIN_A(ENUM, NAME, MASK, ATTR, ICODE)
#define RS6000_BUILTIN_D(ENUM, NAME, MASK, ATTR, ICODE)
#define RS6000_BUILTIN_E(ENUM, NAME, MASK, ATTR, ICODE)
#define RS6000_BUILTIN_H(ENUM, NAME, MASK, ATTR, ICODE)
#define RS6000_BUILTIN_P(ENUM, NAME, MASK, ATTR, ICODE)
#define RS6000_BUILTIN_Q(ENUM, NAME, MASK, ATTR, ICODE)
#define RS6000_BUILTIN_S(ENUM, NAME, MASK, ATTR, ICODE)
#define RS6000_BUILTIN_X(ENUM, NAME, MASK, ATTR, ICODE)

static const struct builtin_description bdesc_2arg[] =
{
#include "rs6000-builtin.def"
};

#undef RS6000_BUILTIN_1
#undef RS6000_BUILTIN_2
#undef RS6000_BUILTIN_3
#undef RS6000_BUILTIN_A
#undef RS6000_BUILTIN_D
#undef RS6000_BUILTIN_E
#undef RS6000_BUILTIN_H
#undef RS6000_BUILTIN_P
#undef RS6000_BUILTIN_Q
#undef RS6000_BUILTIN_S
#undef RS6000_BUILTIN_X

#define RS6000_BUILTIN_1(ENUM, NAME, MASK, ATTR, ICODE)
#define RS6000_BUILTIN_2(ENUM, NAME, MASK, ATTR, ICODE)
#define RS6000_BUILTIN_3(ENUM, NAME, MASK, ATTR, ICODE)
#define RS6000_BUILTIN_A(ENUM, NAME, MASK, ATTR, ICODE)
#define RS6000_BUILTIN_D(ENUM, NAME, MASK, ATTR, ICODE)
#define RS6000_BUILTIN_E(ENUM, NAME, MASK, ATTR, ICODE)
#define RS6000_BUILTIN_H(ENUM, NAME, MASK, ATTR, ICODE)
#define RS6000_BUILTIN_P(ENUM, NAME, MASK, ATTR, ICODE) \
  { MASK, ICODE, NAME, ENUM },

#define RS6000_BUILTIN_Q(ENUM, NAME, MASK, ATTR, ICODE)
#define RS6000_BUILTIN_S(ENUM, NAME, MASK, ATTR, ICODE)
#define RS6000_BUILTIN_X(ENUM, NAME, MASK, ATTR, ICODE)

/* AltiVec predicates.  */

static const struct builtin_description bdesc_altivec_preds[] =
{
#include "rs6000-builtin.def"
};

/* SPE predicates.  */
#undef RS6000_BUILTIN_1
#undef RS6000_BUILTIN_2
#undef RS6000_BUILTIN_3
#undef RS6000_BUILTIN_A
#undef RS6000_BUILTIN_D
#undef RS6000_BUILTIN_E
#undef RS6000_BUILTIN_H
#undef RS6000_BUILTIN_P
#undef RS6000_BUILTIN_Q
#undef RS6000_BUILTIN_S
#undef RS6000_BUILTIN_X

#define RS6000_BUILTIN_1(ENUM, NAME, MASK, ATTR, ICODE)
#define RS6000_BUILTIN_2(ENUM, NAME, MASK, ATTR, ICODE)
#define RS6000_BUILTIN_3(ENUM, NAME, MASK, ATTR, ICODE)
#define RS6000_BUILTIN_A(ENUM, NAME, MASK, ATTR, ICODE)
#define RS6000_BUILTIN_D(ENUM, NAME, MASK, ATTR, ICODE)
#define RS6000_BUILTIN_E(ENUM, NAME, MASK, ATTR, ICODE)
#define RS6000_BUILTIN_H(ENUM, NAME, MASK, ATTR, ICODE)
#define RS6000_BUILTIN_P(ENUM, NAME, MASK, ATTR, ICODE)
#define RS6000_BUILTIN_Q(ENUM, NAME, MASK, ATTR, ICODE)
#define RS6000_BUILTIN_S(ENUM, NAME, MASK, ATTR, ICODE) \
  { MASK, ICODE, NAME, ENUM },

#define RS6000_BUILTIN_X(ENUM, NAME, MASK, ATTR, ICODE)

static const struct builtin_description bdesc_spe_predicates[] =
{
#include "rs6000-builtin.def"
};

/* SPE evsel predicates.  */
#undef RS6000_BUILTIN_1
#undef RS6000_BUILTIN_2
#undef RS6000_BUILTIN_3
#undef RS6000_BUILTIN_A
#undef RS6000_BUILTIN_D
#undef RS6000_BUILTIN_E
#undef RS6000_BUILTIN_H
#undef RS6000_BUILTIN_P
#undef RS6000_BUILTIN_Q
#undef RS6000_BUILTIN_S
#undef RS6000_BUILTIN_X

#define RS6000_BUILTIN_1(ENUM, NAME, MASK, ATTR, ICODE)
#define RS6000_BUILTIN_2(ENUM, NAME, MASK, ATTR, ICODE)
#define RS6000_BUILTIN_3(ENUM, NAME, MASK, ATTR, ICODE)
#define RS6000_BUILTIN_A(ENUM, NAME, MASK, ATTR, ICODE)
#define RS6000_BUILTIN_D(ENUM, NAME, MASK, ATTR, ICODE)
#define RS6000_BUILTIN_E(ENUM, NAME, MASK, ATTR, ICODE) \
  { MASK, ICODE, NAME, ENUM },

#define RS6000_BUILTIN_H(ENUM, NAME, MASK, ATTR, ICODE)
#define RS6000_BUILTIN_P(ENUM, NAME, MASK, ATTR, ICODE)
#define RS6000_BUILTIN_Q(ENUM, NAME, MASK, ATTR, ICODE)
#define RS6000_BUILTIN_S(ENUM, NAME, MASK, ATTR, ICODE)
#define RS6000_BUILTIN_X(ENUM, NAME, MASK, ATTR, ICODE)

static const struct builtin_description bdesc_spe_evsel[] =
{
#include "rs6000-builtin.def"
};

/* PAIRED predicates.  */
#undef RS6000_BUILTIN_1
#undef RS6000_BUILTIN_2
#undef RS6000_BUILTIN_3
#undef RS6000_BUILTIN_A
#undef RS6000_BUILTIN_D
#undef RS6000_BUILTIN_E
#undef RS6000_BUILTIN_H
#undef RS6000_BUILTIN_P
#undef RS6000_BUILTIN_Q
#undef RS6000_BUILTIN_S
#undef RS6000_BUILTIN_X

#define RS6000_BUILTIN_1(ENUM, NAME, MASK, ATTR, ICODE)
#define RS6000_BUILTIN_2(ENUM, NAME, MASK, ATTR, ICODE)
#define RS6000_BUILTIN_3(ENUM, NAME, MASK, ATTR, ICODE)
#define RS6000_BUILTIN_A(ENUM, NAME, MASK, ATTR, ICODE)
#define RS6000_BUILTIN_D(ENUM, NAME, MASK, ATTR, ICODE)
#define RS6000_BUILTIN_E(ENUM, NAME, MASK, ATTR, ICODE)
#define RS6000_BUILTIN_H(ENUM, NAME, MASK, ATTR, ICODE)
#define RS6000_BUILTIN_P(ENUM, NAME, MASK, ATTR, ICODE)
#define RS6000_BUILTIN_Q(ENUM, NAME, MASK, ATTR, ICODE) \
  { MASK, ICODE, NAME, ENUM },

#define RS6000_BUILTIN_S(ENUM, NAME, MASK, ATTR, ICODE)
#define RS6000_BUILTIN_X(ENUM, NAME, MASK, ATTR, ICODE)

static const struct builtin_description bdesc_paired_preds[] =
{
#include "rs6000-builtin.def"
};

/* ABS* operations.  */

#undef RS6000_BUILTIN_1
#undef RS6000_BUILTIN_2
#undef RS6000_BUILTIN_3
#undef RS6000_BUILTIN_A
#undef RS6000_BUILTIN_D
#undef RS6000_BUILTIN_E
#undef RS6000_BUILTIN_H
#undef RS6000_BUILTIN_P
#undef RS6000_BUILTIN_Q
#undef RS6000_BUILTIN_S
#undef RS6000_BUILTIN_X

#define RS6000_BUILTIN_1(ENUM, NAME, MASK, ATTR, ICODE)
#define RS6000_BUILTIN_2(ENUM, NAME, MASK, ATTR, ICODE)
#define RS6000_BUILTIN_3(ENUM, NAME, MASK, ATTR, ICODE)
#define RS6000_BUILTIN_A(ENUM, NAME, MASK, ATTR, ICODE) \
  { MASK, ICODE, NAME, ENUM },

#define RS6000_BUILTIN_D(ENUM, NAME, MASK, ATTR, ICODE)
#define RS6000_BUILTIN_E(ENUM, NAME, MASK, ATTR, ICODE)
#define RS6000_BUILTIN_H(ENUM, NAME, MASK, ATTR, ICODE)
#define RS6000_BUILTIN_P(ENUM, NAME, MASK, ATTR, ICODE)
#define RS6000_BUILTIN_Q(ENUM, NAME, MASK, ATTR, ICODE)
#define RS6000_BUILTIN_S(ENUM, NAME, MASK, ATTR, ICODE)
#define RS6000_BUILTIN_X(ENUM, NAME, MASK, ATTR, ICODE)

static const struct builtin_description bdesc_abs[] =
{
#include "rs6000-builtin.def"
};

/* Simple unary operations: VECb = foo (unsigned literal) or VECb =
   foo (VECa).  */

#undef RS6000_BUILTIN_1
#undef RS6000_BUILTIN_2
#undef RS6000_BUILTIN_3
#undef RS6000_BUILTIN_A
#undef RS6000_BUILTIN_D
#undef RS6000_BUILTIN_E
#undef RS6000_BUILTIN_H
#undef RS6000_BUILTIN_P
#undef RS6000_BUILTIN_Q
#undef RS6000_BUILTIN_S
#undef RS6000_BUILTIN_X

#define RS6000_BUILTIN_1(ENUM, NAME, MASK, ATTR, ICODE) \
  { MASK, ICODE, NAME, ENUM },

#define RS6000_BUILTIN_2(ENUM, NAME, MASK, ATTR, ICODE)
#define RS6000_BUILTIN_3(ENUM, NAME, MASK, ATTR, ICODE)
#define RS6000_BUILTIN_A(ENUM, NAME, MASK, ATTR, ICODE)
#define RS6000_BUILTIN_D(ENUM, NAME, MASK, ATTR, ICODE)
#define RS6000_BUILTIN_E(ENUM, NAME, MASK, ATTR, ICODE)
#define RS6000_BUILTIN_H(ENUM, NAME, MASK, ATTR, ICODE)
#define RS6000_BUILTIN_P(ENUM, NAME, MASK, ATTR, ICODE)
#define RS6000_BUILTIN_Q(ENUM, NAME, MASK, ATTR, ICODE)
#define RS6000_BUILTIN_S(ENUM, NAME, MASK, ATTR, ICODE)
#define RS6000_BUILTIN_X(ENUM, NAME, MASK, ATTR, ICODE)

static const struct builtin_description bdesc_1arg[] =
{
#include "rs6000-builtin.def"
};

/* HTM builtins.  */
#undef RS6000_BUILTIN_1
#undef RS6000_BUILTIN_2
#undef RS6000_BUILTIN_3
#undef RS6000_BUILTIN_A
#undef RS6000_BUILTIN_D
#undef RS6000_BUILTIN_E
#undef RS6000_BUILTIN_H
#undef RS6000_BUILTIN_P
#undef RS6000_BUILTIN_Q
#undef RS6000_BUILTIN_S
#undef RS6000_BUILTIN_X

#define RS6000_BUILTIN_1(ENUM, NAME, MASK, ATTR, ICODE)
#define RS6000_BUILTIN_2(ENUM, NAME, MASK, ATTR, ICODE)
#define RS6000_BUILTIN_3(ENUM, NAME, MASK, ATTR, ICODE)
#define RS6000_BUILTIN_A(ENUM, NAME, MASK, ATTR, ICODE)
#define RS6000_BUILTIN_D(ENUM, NAME, MASK, ATTR, ICODE)
#define RS6000_BUILTIN_E(ENUM, NAME, MASK, ATTR, ICODE)
#define RS6000_BUILTIN_H(ENUM, NAME, MASK, ATTR, ICODE) \
  { MASK, ICODE, NAME, ENUM },

#define RS6000_BUILTIN_P(ENUM, NAME, MASK, ATTR, ICODE)
#define RS6000_BUILTIN_Q(ENUM, NAME, MASK, ATTR, ICODE)
#define RS6000_BUILTIN_S(ENUM, NAME, MASK, ATTR, ICODE)
#define RS6000_BUILTIN_X(ENUM, NAME, MASK, ATTR, ICODE)

static const struct builtin_description bdesc_htm[] =
{
#include "rs6000-builtin.def"
};

#undef RS6000_BUILTIN_1
#undef RS6000_BUILTIN_2
#undef RS6000_BUILTIN_3
#undef RS6000_BUILTIN_A
#undef RS6000_BUILTIN_D
#undef RS6000_BUILTIN_E
#undef RS6000_BUILTIN_H
#undef RS6000_BUILTIN_P
#undef RS6000_BUILTIN_Q
#undef RS6000_BUILTIN_S

/* Return true if a builtin function is overloaded.  */
bool
rs6000_overloaded_builtin_p (enum rs6000_builtins fncode)
{
  return (rs6000_builtin_info[(int)fncode].attr & RS6000_BTC_OVERLOADED) != 0;
}

/* Expand an expression EXP that calls a builtin without arguments.  */
static rtx
rs6000_expand_zeroop_builtin (enum insn_code icode, rtx target)
{
  rtx pat;
  enum machine_mode tmode = insn_data[icode].operand[0].mode;

  if (icode == CODE_FOR_nothing)
    /* Builtin not supported on this processor.  */
    return 0;

  if (target == 0
      || GET_MODE (target) != tmode
      || ! (*insn_data[icode].operand[0].predicate) (target, tmode))
    target = gen_reg_rtx (tmode);

  pat = GEN_FCN (icode) (target);
  if (! pat)
    return 0;
  emit_insn (pat);

  return target;
}


static rtx
rs6000_expand_mtfsf_builtin (enum insn_code icode, tree exp)
{
  rtx pat;
  tree arg0 = CALL_EXPR_ARG (exp, 0);
  tree arg1 = CALL_EXPR_ARG (exp, 1);
  rtx op0 = expand_normal (arg0);
  rtx op1 = expand_normal (arg1);
  enum machine_mode mode0 = insn_data[icode].operand[0].mode;
  enum machine_mode mode1 = insn_data[icode].operand[1].mode;

  if (icode == CODE_FOR_nothing)
    /* Builtin not supported on this processor.  */
    return 0;

  /* If we got invalid arguments bail out before generating bad rtl.  */
  if (arg0 == error_mark_node || arg1 == error_mark_node)
    return const0_rtx;

  if (GET_CODE (op0) != CONST_INT
      || INTVAL (op0) > 255
      || INTVAL (op0) < 0)
    {
      error ("argument 1 must be an 8-bit field value");
      return const0_rtx;
    }

  if (! (*insn_data[icode].operand[0].predicate) (op0, mode0))
    op0 = copy_to_mode_reg (mode0, op0);

  if (! (*insn_data[icode].operand[1].predicate) (op1, mode1))
    op1 = copy_to_mode_reg (mode1, op1);

  pat = GEN_FCN (icode) (op0, op1);
  if (! pat)
    return const0_rtx;
  emit_insn (pat);

  return NULL_RTX;
}


static rtx
rs6000_expand_unop_builtin (enum insn_code icode, tree exp, rtx target)
{
  rtx pat;
  tree arg0 = CALL_EXPR_ARG (exp, 0);
  rtx op0 = expand_normal (arg0);
  enum machine_mode tmode = insn_data[icode].operand[0].mode;
  enum machine_mode mode0 = insn_data[icode].operand[1].mode;

  if (icode == CODE_FOR_nothing)
    /* Builtin not supported on this processor.  */
    return 0;

  /* If we got invalid arguments bail out before generating bad rtl.  */
  if (arg0 == error_mark_node)
    return const0_rtx;

  if (icode == CODE_FOR_altivec_vspltisb
      || icode == CODE_FOR_altivec_vspltish
      || icode == CODE_FOR_altivec_vspltisw
      || icode == CODE_FOR_spe_evsplatfi
      || icode == CODE_FOR_spe_evsplati)
    {
      /* Only allow 5-bit *signed* literals.  */
      if (GET_CODE (op0) != CONST_INT
	  || INTVAL (op0) > 15
	  || INTVAL (op0) < -16)
	{
	  error ("argument 1 must be a 5-bit signed literal");
	  return const0_rtx;
	}
    }

  if (target == 0
      || GET_MODE (target) != tmode
      || ! (*insn_data[icode].operand[0].predicate) (target, tmode))
    target = gen_reg_rtx (tmode);

  if (! (*insn_data[icode].operand[1].predicate) (op0, mode0))
    op0 = copy_to_mode_reg (mode0, op0);

  pat = GEN_FCN (icode) (target, op0);
  if (! pat)
    return 0;
  emit_insn (pat);

  return target;
}

static rtx
altivec_expand_abs_builtin (enum insn_code icode, tree exp, rtx target)
{
  rtx pat, scratch1, scratch2;
  tree arg0 = CALL_EXPR_ARG (exp, 0);
  rtx op0 = expand_normal (arg0);
  enum machine_mode tmode = insn_data[icode].operand[0].mode;
  enum machine_mode mode0 = insn_data[icode].operand[1].mode;

  /* If we have invalid arguments, bail out before generating bad rtl.  */
  if (arg0 == error_mark_node)
    return const0_rtx;

  if (target == 0
      || GET_MODE (target) != tmode
      || ! (*insn_data[icode].operand[0].predicate) (target, tmode))
    target = gen_reg_rtx (tmode);

  if (! (*insn_data[icode].operand[1].predicate) (op0, mode0))
    op0 = copy_to_mode_reg (mode0, op0);

  scratch1 = gen_reg_rtx (mode0);
  scratch2 = gen_reg_rtx (mode0);

  pat = GEN_FCN (icode) (target, op0, scratch1, scratch2);
  if (! pat)
    return 0;
  emit_insn (pat);

  return target;
}

static rtx
rs6000_expand_binop_builtin (enum insn_code icode, tree exp, rtx target)
{
  rtx pat;
  tree arg0 = CALL_EXPR_ARG (exp, 0);
  tree arg1 = CALL_EXPR_ARG (exp, 1);
  rtx op0 = expand_normal (arg0);
  rtx op1 = expand_normal (arg1);
  enum machine_mode tmode = insn_data[icode].operand[0].mode;
  enum machine_mode mode0 = insn_data[icode].operand[1].mode;
  enum machine_mode mode1 = insn_data[icode].operand[2].mode;

  if (icode == CODE_FOR_nothing)
    /* Builtin not supported on this processor.  */
    return 0;

  /* If we got invalid arguments bail out before generating bad rtl.  */
  if (arg0 == error_mark_node || arg1 == error_mark_node)
    return const0_rtx;

  if (icode == CODE_FOR_altivec_vcfux
      || icode == CODE_FOR_altivec_vcfsx
      || icode == CODE_FOR_altivec_vctsxs
      || icode == CODE_FOR_altivec_vctuxs
      || icode == CODE_FOR_altivec_vspltb
      || icode == CODE_FOR_altivec_vsplth
      || icode == CODE_FOR_altivec_vspltw
      || icode == CODE_FOR_spe_evaddiw
      || icode == CODE_FOR_spe_evldd
      || icode == CODE_FOR_spe_evldh
      || icode == CODE_FOR_spe_evldw
      || icode == CODE_FOR_spe_evlhhesplat
      || icode == CODE_FOR_spe_evlhhossplat
      || icode == CODE_FOR_spe_evlhhousplat
      || icode == CODE_FOR_spe_evlwhe
      || icode == CODE_FOR_spe_evlwhos
      || icode == CODE_FOR_spe_evlwhou
      || icode == CODE_FOR_spe_evlwhsplat
      || icode == CODE_FOR_spe_evlwwsplat
      || icode == CODE_FOR_spe_evrlwi
      || icode == CODE_FOR_spe_evslwi
      || icode == CODE_FOR_spe_evsrwis
      || icode == CODE_FOR_spe_evsubifw
      || icode == CODE_FOR_spe_evsrwiu)
    {
      /* Only allow 5-bit unsigned literals.  */
      STRIP_NOPS (arg1);
      if (TREE_CODE (arg1) != INTEGER_CST
	  || TREE_INT_CST_LOW (arg1) & ~0x1f)
	{
	  error ("argument 2 must be a 5-bit unsigned literal");
	  return const0_rtx;
	}
    }

  if (target == 0
      || GET_MODE (target) != tmode
      || ! (*insn_data[icode].operand[0].predicate) (target, tmode))
    target = gen_reg_rtx (tmode);

  if (! (*insn_data[icode].operand[1].predicate) (op0, mode0))
    op0 = copy_to_mode_reg (mode0, op0);
  if (! (*insn_data[icode].operand[2].predicate) (op1, mode1))
    op1 = copy_to_mode_reg (mode1, op1);

  pat = GEN_FCN (icode) (target, op0, op1);
  if (! pat)
    return 0;
  emit_insn (pat);

  return target;
}

static rtx
altivec_expand_predicate_builtin (enum insn_code icode, tree exp, rtx target)
{
  rtx pat, scratch;
  tree cr6_form = CALL_EXPR_ARG (exp, 0);
  tree arg0 = CALL_EXPR_ARG (exp, 1);
  tree arg1 = CALL_EXPR_ARG (exp, 2);
  rtx op0 = expand_normal (arg0);
  rtx op1 = expand_normal (arg1);
  enum machine_mode tmode = SImode;
  enum machine_mode mode0 = insn_data[icode].operand[1].mode;
  enum machine_mode mode1 = insn_data[icode].operand[2].mode;
  int cr6_form_int;

  if (TREE_CODE (cr6_form) != INTEGER_CST)
    {
      error ("argument 1 of __builtin_altivec_predicate must be a constant");
      return const0_rtx;
    }
  else
    cr6_form_int = TREE_INT_CST_LOW (cr6_form);

  gcc_assert (mode0 == mode1);

  /* If we have invalid arguments, bail out before generating bad rtl.  */
  if (arg0 == error_mark_node || arg1 == error_mark_node)
    return const0_rtx;

  if (target == 0
      || GET_MODE (target) != tmode
      || ! (*insn_data[icode].operand[0].predicate) (target, tmode))
    target = gen_reg_rtx (tmode);

  if (! (*insn_data[icode].operand[1].predicate) (op0, mode0))
    op0 = copy_to_mode_reg (mode0, op0);
  if (! (*insn_data[icode].operand[2].predicate) (op1, mode1))
    op1 = copy_to_mode_reg (mode1, op1);

  scratch = gen_reg_rtx (mode0);

  pat = GEN_FCN (icode) (scratch, op0, op1);
  if (! pat)
    return 0;
  emit_insn (pat);

  /* The vec_any* and vec_all* predicates use the same opcodes for two
     different operations, but the bits in CR6 will be different
     depending on what information we want.  So we have to play tricks
     with CR6 to get the right bits out.

     If you think this is disgusting, look at the specs for the
     AltiVec predicates.  */

  switch (cr6_form_int)
    {
    case 0:
      emit_insn (gen_cr6_test_for_zero (target));
      break;
    case 1:
      emit_insn (gen_cr6_test_for_zero_reverse (target));
      break;
    case 2:
      emit_insn (gen_cr6_test_for_lt (target));
      break;
    case 3:
      emit_insn (gen_cr6_test_for_lt_reverse (target));
      break;
    default:
      error ("argument 1 of __builtin_altivec_predicate is out of range");
      break;
    }

  return target;
}

static rtx
paired_expand_lv_builtin (enum insn_code icode, tree exp, rtx target)
{
  rtx pat, addr;
  tree arg0 = CALL_EXPR_ARG (exp, 0);
  tree arg1 = CALL_EXPR_ARG (exp, 1);
  enum machine_mode tmode = insn_data[icode].operand[0].mode;
  enum machine_mode mode0 = Pmode;
  enum machine_mode mode1 = Pmode;
  rtx op0 = expand_normal (arg0);
  rtx op1 = expand_normal (arg1);

  if (icode == CODE_FOR_nothing)
    /* Builtin not supported on this processor.  */
    return 0;

  /* If we got invalid arguments bail out before generating bad rtl.  */
  if (arg0 == error_mark_node || arg1 == error_mark_node)
    return const0_rtx;

  if (target == 0
      || GET_MODE (target) != tmode
      || ! (*insn_data[icode].operand[0].predicate) (target, tmode))
    target = gen_reg_rtx (tmode);

  op1 = copy_to_mode_reg (mode1, op1);

  if (op0 == const0_rtx)
    {
      addr = gen_rtx_MEM (tmode, op1);
    }
  else
    {
      op0 = copy_to_mode_reg (mode0, op0);
      addr = gen_rtx_MEM (tmode, gen_rtx_PLUS (Pmode, op0, op1));
    }

  pat = GEN_FCN (icode) (target, addr);

  if (! pat)
    return 0;
  emit_insn (pat);

  return target;
}

/* Return a constant vector for use as a little-endian permute control vector
   to reverse the order of elements of the given vector mode.  */
static rtx
swap_selector_for_mode (enum machine_mode mode)
{
  /* These are little endian vectors, so their elements are reversed
     from what you would normally expect for a permute control vector.  */
  unsigned int swap2[16] = {7,6,5,4,3,2,1,0,15,14,13,12,11,10,9,8};
  unsigned int swap4[16] = {3,2,1,0,7,6,5,4,11,10,9,8,15,14,13,12};
  unsigned int swap8[16] = {1,0,3,2,5,4,7,6,9,8,11,10,13,12,15,14};
  unsigned int swap16[16] = {0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15};
  unsigned int *swaparray, i;
  rtx perm[16];

  switch (mode)
    {
    case V2DFmode:
    case V2DImode:
      swaparray = swap2;
      break;
    case V4SFmode:
    case V4SImode:
      swaparray = swap4;
      break;
    case V8HImode:
      swaparray = swap8;
      break;
    case V16QImode:
      swaparray = swap16;
      break;
    default:
      gcc_unreachable ();
    }

  for (i = 0; i < 16; ++i)
    perm[i] = GEN_INT (swaparray[i]);

  return force_reg (V16QImode, gen_rtx_CONST_VECTOR (V16QImode, gen_rtvec_v (16, perm)));
}

/* Generate code for an "lvx", "lvxl", or "lve*x" built-in for a little endian target
   with -maltivec=be specified.  Issue the load followed by an element-reversing
   permute.  */
void
altivec_expand_lvx_be (rtx op0, rtx op1, enum machine_mode mode, unsigned unspec)
{
  rtx tmp = gen_reg_rtx (mode);
  rtx load = gen_rtx_SET (VOIDmode, tmp, op1);
  rtx lvx = gen_rtx_UNSPEC (mode, gen_rtvec (1, const0_rtx), unspec);
  rtx par = gen_rtx_PARALLEL (mode, gen_rtvec (2, load, lvx));
  rtx sel = swap_selector_for_mode (mode);
  rtx vperm = gen_rtx_UNSPEC (mode, gen_rtvec (3, tmp, tmp, sel), UNSPEC_VPERM);

  gcc_assert (REG_P (op0));
  emit_insn (par);
  emit_insn (gen_rtx_SET (VOIDmode, op0, vperm));
}

/* Generate code for a "stvx" or "stvxl" built-in for a little endian target
   with -maltivec=be specified.  Issue the store preceded by an element-reversing
   permute.  */
void
altivec_expand_stvx_be (rtx op0, rtx op1, enum machine_mode mode, unsigned unspec)
{
  rtx tmp = gen_reg_rtx (mode);
  rtx store = gen_rtx_SET (VOIDmode, op0, tmp);
  rtx stvx = gen_rtx_UNSPEC (mode, gen_rtvec (1, const0_rtx), unspec);
  rtx par = gen_rtx_PARALLEL (mode, gen_rtvec (2, store, stvx));
  rtx sel = swap_selector_for_mode (mode);
  rtx vperm;

  gcc_assert (REG_P (op1));
  vperm = gen_rtx_UNSPEC (mode, gen_rtvec (3, op1, op1, sel), UNSPEC_VPERM);
  emit_insn (gen_rtx_SET (VOIDmode, tmp, vperm));
  emit_insn (par);
}

/* Generate code for a "stve*x" built-in for a little endian target with -maltivec=be
   specified.  Issue the store preceded by an element-reversing permute.  */
void
altivec_expand_stvex_be (rtx op0, rtx op1, enum machine_mode mode, unsigned unspec)
{
  enum machine_mode inner_mode = GET_MODE_INNER (mode);
  rtx tmp = gen_reg_rtx (mode);
  rtx stvx = gen_rtx_UNSPEC (inner_mode, gen_rtvec (1, tmp), unspec);
  rtx sel = swap_selector_for_mode (mode);
  rtx vperm;

  gcc_assert (REG_P (op1));
  vperm = gen_rtx_UNSPEC (mode, gen_rtvec (3, op1, op1, sel), UNSPEC_VPERM);
  emit_insn (gen_rtx_SET (VOIDmode, tmp, vperm));
  emit_insn (gen_rtx_SET (VOIDmode, op0, stvx));
}

static rtx
altivec_expand_lv_builtin (enum insn_code icode, tree exp, rtx target, bool blk)
{
  rtx pat, addr;
  tree arg0 = CALL_EXPR_ARG (exp, 0);
  tree arg1 = CALL_EXPR_ARG (exp, 1);
  enum machine_mode tmode = insn_data[icode].operand[0].mode;
  enum machine_mode mode0 = Pmode;
  enum machine_mode mode1 = Pmode;
  rtx op0 = expand_normal (arg0);
  rtx op1 = expand_normal (arg1);

  if (icode == CODE_FOR_nothing)
    /* Builtin not supported on this processor.  */
    return 0;

  /* If we got invalid arguments bail out before generating bad rtl.  */
  if (arg0 == error_mark_node || arg1 == error_mark_node)
    return const0_rtx;

  if (target == 0
      || GET_MODE (target) != tmode
      || ! (*insn_data[icode].operand[0].predicate) (target, tmode))
    target = gen_reg_rtx (tmode);

  op1 = copy_to_mode_reg (mode1, op1);

  if (op0 == const0_rtx)
    {
      addr = gen_rtx_MEM (blk ? BLKmode : tmode, op1);
    }
  else
    {
      op0 = copy_to_mode_reg (mode0, op0);
      addr = gen_rtx_MEM (blk ? BLKmode : tmode, gen_rtx_PLUS (Pmode, op0, op1));
    }

  pat = GEN_FCN (icode) (target, addr);

  if (! pat)
    return 0;
  emit_insn (pat);

  return target;
}

static rtx
spe_expand_stv_builtin (enum insn_code icode, tree exp)
{
  tree arg0 = CALL_EXPR_ARG (exp, 0);
  tree arg1 = CALL_EXPR_ARG (exp, 1);
  tree arg2 = CALL_EXPR_ARG (exp, 2);
  rtx op0 = expand_normal (arg0);
  rtx op1 = expand_normal (arg1);
  rtx op2 = expand_normal (arg2);
  rtx pat;
  enum machine_mode mode0 = insn_data[icode].operand[0].mode;
  enum machine_mode mode1 = insn_data[icode].operand[1].mode;
  enum machine_mode mode2 = insn_data[icode].operand[2].mode;

  /* Invalid arguments.  Bail before doing anything stoopid!  */
  if (arg0 == error_mark_node
      || arg1 == error_mark_node
      || arg2 == error_mark_node)
    return const0_rtx;

  if (! (*insn_data[icode].operand[2].predicate) (op0, mode2))
    op0 = copy_to_mode_reg (mode2, op0);
  if (! (*insn_data[icode].operand[0].predicate) (op1, mode0))
    op1 = copy_to_mode_reg (mode0, op1);
  if (! (*insn_data[icode].operand[1].predicate) (op2, mode1))
    op2 = copy_to_mode_reg (mode1, op2);

  pat = GEN_FCN (icode) (op1, op2, op0);
  if (pat)
    emit_insn (pat);
  return NULL_RTX;
}

static rtx
paired_expand_stv_builtin (enum insn_code icode, tree exp)
{
  tree arg0 = CALL_EXPR_ARG (exp, 0);
  tree arg1 = CALL_EXPR_ARG (exp, 1);
  tree arg2 = CALL_EXPR_ARG (exp, 2);
  rtx op0 = expand_normal (arg0);
  rtx op1 = expand_normal (arg1);
  rtx op2 = expand_normal (arg2);
  rtx pat, addr;
  enum machine_mode tmode = insn_data[icode].operand[0].mode;
  enum machine_mode mode1 = Pmode;
  enum machine_mode mode2 = Pmode;

  /* Invalid arguments.  Bail before doing anything stoopid!  */
  if (arg0 == error_mark_node
      || arg1 == error_mark_node
      || arg2 == error_mark_node)
    return const0_rtx;

  if (! (*insn_data[icode].operand[1].predicate) (op0, tmode))
    op0 = copy_to_mode_reg (tmode, op0);

  op2 = copy_to_mode_reg (mode2, op2);

  if (op1 == const0_rtx)
    {
      addr = gen_rtx_MEM (tmode, op2);
    }
  else
    {
      op1 = copy_to_mode_reg (mode1, op1);
      addr = gen_rtx_MEM (tmode, gen_rtx_PLUS (Pmode, op1, op2));
    }

  pat = GEN_FCN (icode) (addr, op0);
  if (pat)
    emit_insn (pat);
  return NULL_RTX;
}

static rtx
altivec_expand_stv_builtin (enum insn_code icode, tree exp)
{
  tree arg0 = CALL_EXPR_ARG (exp, 0);
  tree arg1 = CALL_EXPR_ARG (exp, 1);
  tree arg2 = CALL_EXPR_ARG (exp, 2);
  rtx op0 = expand_normal (arg0);
  rtx op1 = expand_normal (arg1);
  rtx op2 = expand_normal (arg2);
  rtx pat, addr;
  enum machine_mode tmode = insn_data[icode].operand[0].mode;
  enum machine_mode smode = insn_data[icode].operand[1].mode;
  enum machine_mode mode1 = Pmode;
  enum machine_mode mode2 = Pmode;

  /* Invalid arguments.  Bail before doing anything stoopid!  */
  if (arg0 == error_mark_node
      || arg1 == error_mark_node
      || arg2 == error_mark_node)
    return const0_rtx;

  if (! (*insn_data[icode].operand[1].predicate) (op0, smode))
    op0 = copy_to_mode_reg (smode, op0);

  op2 = copy_to_mode_reg (mode2, op2);

  if (op1 == const0_rtx)
    {
      addr = gen_rtx_MEM (tmode, op2);
    }
  else
    {
      op1 = copy_to_mode_reg (mode1, op1);
      addr = gen_rtx_MEM (tmode, gen_rtx_PLUS (Pmode, op1, op2));
    }

  pat = GEN_FCN (icode) (addr, op0);
  if (pat)
    emit_insn (pat);
  return NULL_RTX;
}

/* Return the appropriate SPR number associated with the given builtin.  */
static inline HOST_WIDE_INT
htm_spr_num (enum rs6000_builtins code)
{
  if (code == HTM_BUILTIN_GET_TFHAR
      || code == HTM_BUILTIN_SET_TFHAR)
    return TFHAR_SPR;
  else if (code == HTM_BUILTIN_GET_TFIAR
	   || code == HTM_BUILTIN_SET_TFIAR)
    return TFIAR_SPR;
  else if (code == HTM_BUILTIN_GET_TEXASR
	   || code == HTM_BUILTIN_SET_TEXASR)
    return TEXASR_SPR;
  gcc_assert (code == HTM_BUILTIN_GET_TEXASRU
	      || code == HTM_BUILTIN_SET_TEXASRU);
  return TEXASRU_SPR;
}

/* Return the appropriate SPR regno associated with the given builtin.  */
static inline HOST_WIDE_INT
htm_spr_regno (enum rs6000_builtins code)
{
  if (code == HTM_BUILTIN_GET_TFHAR
      || code == HTM_BUILTIN_SET_TFHAR)
    return TFHAR_REGNO;
  else if (code == HTM_BUILTIN_GET_TFIAR
	   || code == HTM_BUILTIN_SET_TFIAR)
    return TFIAR_REGNO;
  gcc_assert (code == HTM_BUILTIN_GET_TEXASR
	      || code == HTM_BUILTIN_SET_TEXASR
	      || code == HTM_BUILTIN_GET_TEXASRU
	      || code == HTM_BUILTIN_SET_TEXASRU);
  return TEXASR_REGNO;
}

/* Return the correct ICODE value depending on whether we are
   setting or reading the HTM SPRs.  */
static inline enum insn_code
rs6000_htm_spr_icode (bool nonvoid)
{
  if (nonvoid)
    return (TARGET_64BIT) ? CODE_FOR_htm_mfspr_di : CODE_FOR_htm_mfspr_si;
  else
    return (TARGET_64BIT) ? CODE_FOR_htm_mtspr_di : CODE_FOR_htm_mtspr_si;
}

/* Expand the HTM builtin in EXP and store the result in TARGET.
   Store true in *EXPANDEDP if we found a builtin to expand.  */
static rtx
htm_expand_builtin (tree exp, rtx target, bool * expandedp)
{
  tree fndecl = TREE_OPERAND (CALL_EXPR_FN (exp), 0);
  bool nonvoid = TREE_TYPE (TREE_TYPE (fndecl)) != void_type_node;
  enum rs6000_builtins fcode = (enum rs6000_builtins) DECL_FUNCTION_CODE (fndecl);
  const struct builtin_description *d;
  size_t i;

  *expandedp = false;

  /* Expand the HTM builtins.  */
  d = bdesc_htm;
  for (i = 0; i < ARRAY_SIZE (bdesc_htm); i++, d++)
    if (d->code == fcode)
      {
	rtx op[MAX_HTM_OPERANDS], pat;
	int nopnds = 0;
	tree arg;
	call_expr_arg_iterator iter;
	unsigned attr = rs6000_builtin_info[fcode].attr;
	enum insn_code icode = d->icode;

	if (attr & RS6000_BTC_SPR)
	  icode = rs6000_htm_spr_icode (nonvoid);

	if (nonvoid)
	  {
	    enum machine_mode tmode = insn_data[icode].operand[0].mode;
	    if (!target
		|| GET_MODE (target) != tmode
		|| !(*insn_data[icode].operand[0].predicate) (target, tmode))
	      target = gen_reg_rtx (tmode);
	    op[nopnds++] = target;
	  }

	FOR_EACH_CALL_EXPR_ARG (arg, iter, exp)
	{
	  const struct insn_operand_data *insn_op;

	  if (arg == error_mark_node || nopnds >= MAX_HTM_OPERANDS)
	    return NULL_RTX;

	  insn_op = &insn_data[icode].operand[nopnds];

	  op[nopnds] = expand_normal (arg);

	  if (!(*insn_op->predicate) (op[nopnds], insn_op->mode))
	    {
	      if (!strcmp (insn_op->constraint, "n"))
		{
		  int arg_num = (nonvoid) ? nopnds : nopnds + 1;
		  if (!CONST_INT_P (op[nopnds]))
		    error ("argument %d must be an unsigned literal", arg_num);
		  else
		    error ("argument %d is an unsigned literal that is "
			   "out of range", arg_num);
		  return const0_rtx;
		}
	      op[nopnds] = copy_to_mode_reg (insn_op->mode, op[nopnds]);
	    }

	  nopnds++;
	}

	/* Handle the builtins for extended mnemonics.  These accept
	   no arguments, but map to builtins that take arguments.  */
	switch (fcode)
	  {
	  case HTM_BUILTIN_TENDALL:  /* Alias for: tend. 1  */
	  case HTM_BUILTIN_TRESUME:  /* Alias for: tsr. 1  */
	    op[nopnds++] = GEN_INT (1);
#ifdef ENABLE_CHECKING
	    attr |= RS6000_BTC_UNARY;
#endif
	    break;
	  case HTM_BUILTIN_TSUSPEND: /* Alias for: tsr. 0  */
	    op[nopnds++] = GEN_INT (0);
#ifdef ENABLE_CHECKING
	    attr |= RS6000_BTC_UNARY;
#endif
	    break;
	  default:
	    break;
	  }

	/* If this builtin accesses SPRs, then pass in the appropriate
	   SPR number and SPR regno as the last two operands.  */
	if (attr & RS6000_BTC_SPR)
	  {
	    op[nopnds++] = gen_rtx_CONST_INT (Pmode, htm_spr_num (fcode));
	    op[nopnds++] = gen_rtx_REG (Pmode, htm_spr_regno (fcode));
	  }

#ifdef ENABLE_CHECKING
	int expected_nopnds = 0;
	if ((attr & RS6000_BTC_TYPE_MASK) == RS6000_BTC_UNARY)
	  expected_nopnds = 1;
	else if ((attr & RS6000_BTC_TYPE_MASK) == RS6000_BTC_BINARY)
	  expected_nopnds = 2;
	else if ((attr & RS6000_BTC_TYPE_MASK) == RS6000_BTC_TERNARY)
	  expected_nopnds = 3;
	if (!(attr & RS6000_BTC_VOID))
	  expected_nopnds += 1;
	if (attr & RS6000_BTC_SPR)
	  expected_nopnds += 2;

	gcc_assert (nopnds == expected_nopnds && nopnds <= MAX_HTM_OPERANDS);
#endif

	switch (nopnds)
	  {
	  case 1:
	    pat = GEN_FCN (icode) (op[0]);
	    break;
	  case 2:
	    pat = GEN_FCN (icode) (op[0], op[1]);
	    break;
	  case 3:
	    pat = GEN_FCN (icode) (op[0], op[1], op[2]);
	    break;
	  case 4:
	    pat = GEN_FCN (icode) (op[0], op[1], op[2], op[3]);
	    break;
	  default:
	    gcc_unreachable ();
	  }
	if (!pat)
	  return NULL_RTX;
	emit_insn (pat);

	*expandedp = true;
	if (nonvoid)
	  return target;
	return const0_rtx;
      }

  return NULL_RTX;
}

static rtx
rs6000_expand_ternop_builtin (enum insn_code icode, tree exp, rtx target)
{
  rtx pat;
  tree arg0 = CALL_EXPR_ARG (exp, 0);
  tree arg1 = CALL_EXPR_ARG (exp, 1);
  tree arg2 = CALL_EXPR_ARG (exp, 2);
  rtx op0 = expand_normal (arg0);
  rtx op1 = expand_normal (arg1);
  rtx op2 = expand_normal (arg2);
  enum machine_mode tmode = insn_data[icode].operand[0].mode;
  enum machine_mode mode0 = insn_data[icode].operand[1].mode;
  enum machine_mode mode1 = insn_data[icode].operand[2].mode;
  enum machine_mode mode2 = insn_data[icode].operand[3].mode;

  if (icode == CODE_FOR_nothing)
    /* Builtin not supported on this processor.  */
    return 0;

  /* If we got invalid arguments bail out before generating bad rtl.  */
  if (arg0 == error_mark_node
      || arg1 == error_mark_node
      || arg2 == error_mark_node)
    return const0_rtx;

  /* Check and prepare argument depending on the instruction code.

     Note that a switch statement instead of the sequence of tests
     would be incorrect as many of the CODE_FOR values could be
     CODE_FOR_nothing and that would yield multiple alternatives
     with identical values.  We'd never reach here at runtime in
     this case.  */
  if (icode == CODE_FOR_altivec_vsldoi_v4sf
      || icode == CODE_FOR_altivec_vsldoi_v4si
      || icode == CODE_FOR_altivec_vsldoi_v8hi
      || icode == CODE_FOR_altivec_vsldoi_v16qi)
    {
      /* Only allow 4-bit unsigned literals.  */
      STRIP_NOPS (arg2);
      if (TREE_CODE (arg2) != INTEGER_CST
	  || TREE_INT_CST_LOW (arg2) & ~0xf)
	{
	  error ("argument 3 must be a 4-bit unsigned literal");
	  return const0_rtx;
	}
    }
  else if (icode == CODE_FOR_vsx_xxpermdi_v2df
           || icode == CODE_FOR_vsx_xxpermdi_v2di
           || icode == CODE_FOR_vsx_xxsldwi_v16qi
           || icode == CODE_FOR_vsx_xxsldwi_v8hi
           || icode == CODE_FOR_vsx_xxsldwi_v4si
           || icode == CODE_FOR_vsx_xxsldwi_v4sf
           || icode == CODE_FOR_vsx_xxsldwi_v2di
           || icode == CODE_FOR_vsx_xxsldwi_v2df)
    {
      /* Only allow 2-bit unsigned literals.  */
      STRIP_NOPS (arg2);
      if (TREE_CODE (arg2) != INTEGER_CST
	  || TREE_INT_CST_LOW (arg2) & ~0x3)
	{
	  error ("argument 3 must be a 2-bit unsigned literal");
	  return const0_rtx;
	}
    }
  else if (icode == CODE_FOR_vsx_set_v2df
           || icode == CODE_FOR_vsx_set_v2di)
    {
      /* Only allow 1-bit unsigned literals.  */
      STRIP_NOPS (arg2);
      if (TREE_CODE (arg2) != INTEGER_CST
	  || TREE_INT_CST_LOW (arg2) & ~0x1)
	{
	  error ("argument 3 must be a 1-bit unsigned literal");
	  return const0_rtx;
	}
    }
  else if (icode == CODE_FOR_crypto_vshasigmaw
	   || icode == CODE_FOR_crypto_vshasigmad)
    {
      /* Check whether the 2nd and 3rd arguments are integer constants and in
	 range and prepare arguments.  */
      STRIP_NOPS (arg1);
      if (TREE_CODE (arg1) != INTEGER_CST
	  || !IN_RANGE (TREE_INT_CST_LOW (arg1), 0, 1))
	{
	  error ("argument 2 must be 0 or 1");
	  return const0_rtx;
	}

      STRIP_NOPS (arg2);
      if (TREE_CODE (arg2) != INTEGER_CST
	  || !IN_RANGE (TREE_INT_CST_LOW (arg2), 0, 15))
	{
	  error ("argument 3 must be in the range 0..15");
	  return const0_rtx;
	}
    }

  if (target == 0
      || GET_MODE (target) != tmode
      || ! (*insn_data[icode].operand[0].predicate) (target, tmode))
    target = gen_reg_rtx (tmode);

  if (! (*insn_data[icode].operand[1].predicate) (op0, mode0))
    op0 = copy_to_mode_reg (mode0, op0);
  if (! (*insn_data[icode].operand[2].predicate) (op1, mode1))
    op1 = copy_to_mode_reg (mode1, op1);
  if (! (*insn_data[icode].operand[3].predicate) (op2, mode2))
    op2 = copy_to_mode_reg (mode2, op2);

  if (TARGET_PAIRED_FLOAT && icode == CODE_FOR_selv2sf4)
    pat = GEN_FCN (icode) (target, op0, op1, op2, CONST0_RTX (SFmode));
  else 
    pat = GEN_FCN (icode) (target, op0, op1, op2);
  if (! pat)
    return 0;
  emit_insn (pat);

  return target;
}

/* Expand the lvx builtins.  */
static rtx
altivec_expand_ld_builtin (tree exp, rtx target, bool *expandedp)
{
  tree fndecl = TREE_OPERAND (CALL_EXPR_FN (exp), 0);
  unsigned int fcode = DECL_FUNCTION_CODE (fndecl);
  tree arg0;
  enum machine_mode tmode, mode0;
  rtx pat, op0;
  enum insn_code icode;

  switch (fcode)
    {
    case ALTIVEC_BUILTIN_LD_INTERNAL_16qi:
      icode = CODE_FOR_vector_altivec_load_v16qi;
      break;
    case ALTIVEC_BUILTIN_LD_INTERNAL_8hi:
      icode = CODE_FOR_vector_altivec_load_v8hi;
      break;
    case ALTIVEC_BUILTIN_LD_INTERNAL_4si:
      icode = CODE_FOR_vector_altivec_load_v4si;
      break;
    case ALTIVEC_BUILTIN_LD_INTERNAL_4sf:
      icode = CODE_FOR_vector_altivec_load_v4sf;
      break;
    case ALTIVEC_BUILTIN_LD_INTERNAL_2df:
      icode = CODE_FOR_vector_altivec_load_v2df;
      break;
    case ALTIVEC_BUILTIN_LD_INTERNAL_2di:
      icode = CODE_FOR_vector_altivec_load_v2di;
      break;
    default:
      *expandedp = false;
      return NULL_RTX;
    }

  *expandedp = true;

  arg0 = CALL_EXPR_ARG (exp, 0);
  op0 = expand_normal (arg0);
  tmode = insn_data[icode].operand[0].mode;
  mode0 = insn_data[icode].operand[1].mode;

  if (target == 0
      || GET_MODE (target) != tmode
      || ! (*insn_data[icode].operand[0].predicate) (target, tmode))
    target = gen_reg_rtx (tmode);

  if (! (*insn_data[icode].operand[1].predicate) (op0, mode0))
    op0 = gen_rtx_MEM (mode0, copy_to_mode_reg (Pmode, op0));

  pat = GEN_FCN (icode) (target, op0);
  if (! pat)
    return 0;
  emit_insn (pat);
  return target;
}

/* Expand the stvx builtins.  */
static rtx
altivec_expand_st_builtin (tree exp, rtx target ATTRIBUTE_UNUSED,
			   bool *expandedp)
{
  tree fndecl = TREE_OPERAND (CALL_EXPR_FN (exp), 0);
  unsigned int fcode = DECL_FUNCTION_CODE (fndecl);
  tree arg0, arg1;
  enum machine_mode mode0, mode1;
  rtx pat, op0, op1;
  enum insn_code icode;

  switch (fcode)
    {
    case ALTIVEC_BUILTIN_ST_INTERNAL_16qi:
      icode = CODE_FOR_vector_altivec_store_v16qi;
      break;
    case ALTIVEC_BUILTIN_ST_INTERNAL_8hi:
      icode = CODE_FOR_vector_altivec_store_v8hi;
      break;
    case ALTIVEC_BUILTIN_ST_INTERNAL_4si:
      icode = CODE_FOR_vector_altivec_store_v4si;
      break;
    case ALTIVEC_BUILTIN_ST_INTERNAL_4sf:
      icode = CODE_FOR_vector_altivec_store_v4sf;
      break;
    case ALTIVEC_BUILTIN_ST_INTERNAL_2df:
      icode = CODE_FOR_vector_altivec_store_v2df;
      break;
    case ALTIVEC_BUILTIN_ST_INTERNAL_2di:
      icode = CODE_FOR_vector_altivec_store_v2di;
      break;
    default:
      *expandedp = false;
      return NULL_RTX;
    }

  arg0 = CALL_EXPR_ARG (exp, 0);
  arg1 = CALL_EXPR_ARG (exp, 1);
  op0 = expand_normal (arg0);
  op1 = expand_normal (arg1);
  mode0 = insn_data[icode].operand[0].mode;
  mode1 = insn_data[icode].operand[1].mode;

  if (! (*insn_data[icode].operand[0].predicate) (op0, mode0))
    op0 = gen_rtx_MEM (mode0, copy_to_mode_reg (Pmode, op0));
  if (! (*insn_data[icode].operand[1].predicate) (op1, mode1))
    op1 = copy_to_mode_reg (mode1, op1);

  pat = GEN_FCN (icode) (op0, op1);
  if (pat)
    emit_insn (pat);

  *expandedp = true;
  return NULL_RTX;
}

/* Expand the dst builtins.  */
static rtx
altivec_expand_dst_builtin (tree exp, rtx target ATTRIBUTE_UNUSED,
			    bool *expandedp)
{
  tree fndecl = TREE_OPERAND (CALL_EXPR_FN (exp), 0);
  enum rs6000_builtins fcode = (enum rs6000_builtins) DECL_FUNCTION_CODE (fndecl);
  tree arg0, arg1, arg2;
  enum machine_mode mode0, mode1;
  rtx pat, op0, op1, op2;
  const struct builtin_description *d;
  size_t i;

  *expandedp = false;

  /* Handle DST variants.  */
  d = bdesc_dst;
  for (i = 0; i < ARRAY_SIZE (bdesc_dst); i++, d++)
    if (d->code == fcode)
      {
	arg0 = CALL_EXPR_ARG (exp, 0);
	arg1 = CALL_EXPR_ARG (exp, 1);
	arg2 = CALL_EXPR_ARG (exp, 2);
	op0 = expand_normal (arg0);
	op1 = expand_normal (arg1);
	op2 = expand_normal (arg2);
	mode0 = insn_data[d->icode].operand[0].mode;
	mode1 = insn_data[d->icode].operand[1].mode;

	/* Invalid arguments, bail out before generating bad rtl.  */
	if (arg0 == error_mark_node
	    || arg1 == error_mark_node
	    || arg2 == error_mark_node)
	  return const0_rtx;

	*expandedp = true;
	STRIP_NOPS (arg2);
	if (TREE_CODE (arg2) != INTEGER_CST
	    || TREE_INT_CST_LOW (arg2) & ~0x3)
	  {
	    error ("argument to %qs must be a 2-bit unsigned literal", d->name);
	    return const0_rtx;
	  }

	if (! (*insn_data[d->icode].operand[0].predicate) (op0, mode0))
	  op0 = copy_to_mode_reg (Pmode, op0);
	if (! (*insn_data[d->icode].operand[1].predicate) (op1, mode1))
	  op1 = copy_to_mode_reg (mode1, op1);

	pat = GEN_FCN (d->icode) (op0, op1, op2);
	if (pat != 0)
	  emit_insn (pat);

	return NULL_RTX;
      }

  return NULL_RTX;
}

/* Expand vec_init builtin.  */
static rtx
altivec_expand_vec_init_builtin (tree type, tree exp, rtx target)
{
  enum machine_mode tmode = TYPE_MODE (type);
  enum machine_mode inner_mode = GET_MODE_INNER (tmode);
  int i, n_elt = GET_MODE_NUNITS (tmode);
  rtvec v = rtvec_alloc (n_elt);

  gcc_assert (VECTOR_MODE_P (tmode));
  gcc_assert (n_elt == call_expr_nargs (exp));

  for (i = 0; i < n_elt; ++i)
    {
      rtx x = expand_normal (CALL_EXPR_ARG (exp, i));
      RTVEC_ELT (v, i) = gen_lowpart (inner_mode, x);
    }

  if (!target || !register_operand (target, tmode))
    target = gen_reg_rtx (tmode);

  rs6000_expand_vector_init (target, gen_rtx_PARALLEL (tmode, v));
  return target;
}

/* Return the integer constant in ARG.  Constrain it to be in the range
   of the subparts of VEC_TYPE; issue an error if not.  */

static int
get_element_number (tree vec_type, tree arg)
{
  unsigned HOST_WIDE_INT elt, max = TYPE_VECTOR_SUBPARTS (vec_type) - 1;

  if (!tree_fits_uhwi_p (arg)
      || (elt = tree_to_uhwi (arg), elt > max))
    {
      error ("selector must be an integer constant in the range 0..%wi", max);
      return 0;
    }

  return elt;
}

/* Expand vec_set builtin.  */
static rtx
altivec_expand_vec_set_builtin (tree exp)
{
  enum machine_mode tmode, mode1;
  tree arg0, arg1, arg2;
  int elt;
  rtx op0, op1;

  arg0 = CALL_EXPR_ARG (exp, 0);
  arg1 = CALL_EXPR_ARG (exp, 1);
  arg2 = CALL_EXPR_ARG (exp, 2);

  tmode = TYPE_MODE (TREE_TYPE (arg0));
  mode1 = TYPE_MODE (TREE_TYPE (TREE_TYPE (arg0)));
  gcc_assert (VECTOR_MODE_P (tmode));

  op0 = expand_expr (arg0, NULL_RTX, tmode, EXPAND_NORMAL);
  op1 = expand_expr (arg1, NULL_RTX, mode1, EXPAND_NORMAL);
  elt = get_element_number (TREE_TYPE (arg0), arg2);

  if (GET_MODE (op1) != mode1 && GET_MODE (op1) != VOIDmode)
    op1 = convert_modes (mode1, GET_MODE (op1), op1, true);

  op0 = force_reg (tmode, op0);
  op1 = force_reg (mode1, op1);

  rs6000_expand_vector_set (op0, op1, elt);

  return op0;
}

/* Expand vec_ext builtin.  */
static rtx
altivec_expand_vec_ext_builtin (tree exp, rtx target)
{
  enum machine_mode tmode, mode0;
  tree arg0, arg1;
  int elt;
  rtx op0;

  arg0 = CALL_EXPR_ARG (exp, 0);
  arg1 = CALL_EXPR_ARG (exp, 1);

  op0 = expand_normal (arg0);
  elt = get_element_number (TREE_TYPE (arg0), arg1);

  tmode = TYPE_MODE (TREE_TYPE (TREE_TYPE (arg0)));
  mode0 = TYPE_MODE (TREE_TYPE (arg0));
  gcc_assert (VECTOR_MODE_P (mode0));

  op0 = force_reg (mode0, op0);

  if (optimize || !target || !register_operand (target, tmode))
    target = gen_reg_rtx (tmode);

  rs6000_expand_vector_extract (target, op0, elt);

  return target;
}

/* Expand the builtin in EXP and store the result in TARGET.  Store
   true in *EXPANDEDP if we found a builtin to expand.  */
static rtx
altivec_expand_builtin (tree exp, rtx target, bool *expandedp)
{
  const struct builtin_description *d;
  size_t i;
  enum insn_code icode;
  tree fndecl = TREE_OPERAND (CALL_EXPR_FN (exp), 0);
  tree arg0;
  rtx op0, pat;
  enum machine_mode tmode, mode0;
  enum rs6000_builtins fcode
    = (enum rs6000_builtins) DECL_FUNCTION_CODE (fndecl);

  if (rs6000_overloaded_builtin_p (fcode))
    {
      *expandedp = true;
      error ("unresolved overload for Altivec builtin %qF", fndecl);

      /* Given it is invalid, just generate a normal call.  */
      return expand_call (exp, target, false);
    }

  target = altivec_expand_ld_builtin (exp, target, expandedp);
  if (*expandedp)
    return target;

  target = altivec_expand_st_builtin (exp, target, expandedp);
  if (*expandedp)
    return target;

  target = altivec_expand_dst_builtin (exp, target, expandedp);
  if (*expandedp)
    return target;

  *expandedp = true;

  switch (fcode)
    {
    case ALTIVEC_BUILTIN_STVX_V2DF:
      return altivec_expand_stv_builtin (CODE_FOR_altivec_stvx_v2df, exp);
    case ALTIVEC_BUILTIN_STVX_V2DI:
      return altivec_expand_stv_builtin (CODE_FOR_altivec_stvx_v2di, exp);
    case ALTIVEC_BUILTIN_STVX_V4SF:
      return altivec_expand_stv_builtin (CODE_FOR_altivec_stvx_v4sf, exp);
    case ALTIVEC_BUILTIN_STVX:
    case ALTIVEC_BUILTIN_STVX_V4SI:
      return altivec_expand_stv_builtin (CODE_FOR_altivec_stvx_v4si, exp);
    case ALTIVEC_BUILTIN_STVX_V8HI:
      return altivec_expand_stv_builtin (CODE_FOR_altivec_stvx_v8hi, exp);
    case ALTIVEC_BUILTIN_STVX_V16QI:
      return altivec_expand_stv_builtin (CODE_FOR_altivec_stvx_v16qi, exp);
    case ALTIVEC_BUILTIN_STVEBX:
      return altivec_expand_stv_builtin (CODE_FOR_altivec_stvebx, exp);
    case ALTIVEC_BUILTIN_STVEHX:
      return altivec_expand_stv_builtin (CODE_FOR_altivec_stvehx, exp);
    case ALTIVEC_BUILTIN_STVEWX:
      return altivec_expand_stv_builtin (CODE_FOR_altivec_stvewx, exp);
    case ALTIVEC_BUILTIN_STVXL_V2DF:
      return altivec_expand_stv_builtin (CODE_FOR_altivec_stvxl_v2df, exp);
    case ALTIVEC_BUILTIN_STVXL_V2DI:
      return altivec_expand_stv_builtin (CODE_FOR_altivec_stvxl_v2di, exp);
    case ALTIVEC_BUILTIN_STVXL_V4SF:
      return altivec_expand_stv_builtin (CODE_FOR_altivec_stvxl_v4sf, exp);
    case ALTIVEC_BUILTIN_STVXL:
    case ALTIVEC_BUILTIN_STVXL_V4SI:
      return altivec_expand_stv_builtin (CODE_FOR_altivec_stvxl_v4si, exp);
    case ALTIVEC_BUILTIN_STVXL_V8HI:
      return altivec_expand_stv_builtin (CODE_FOR_altivec_stvxl_v8hi, exp);
    case ALTIVEC_BUILTIN_STVXL_V16QI:
      return altivec_expand_stv_builtin (CODE_FOR_altivec_stvxl_v16qi, exp);

    case ALTIVEC_BUILTIN_STVLX:
      return altivec_expand_stv_builtin (CODE_FOR_altivec_stvlx, exp);
    case ALTIVEC_BUILTIN_STVLXL:
      return altivec_expand_stv_builtin (CODE_FOR_altivec_stvlxl, exp);
    case ALTIVEC_BUILTIN_STVRX:
      return altivec_expand_stv_builtin (CODE_FOR_altivec_stvrx, exp);
    case ALTIVEC_BUILTIN_STVRXL:
      return altivec_expand_stv_builtin (CODE_FOR_altivec_stvrxl, exp);

    case VSX_BUILTIN_STXVD2X_V2DF:
      return altivec_expand_stv_builtin (CODE_FOR_vsx_store_v2df, exp);
    case VSX_BUILTIN_STXVD2X_V2DI:
      return altivec_expand_stv_builtin (CODE_FOR_vsx_store_v2di, exp);
    case VSX_BUILTIN_STXVW4X_V4SF:
      return altivec_expand_stv_builtin (CODE_FOR_vsx_store_v4sf, exp);
    case VSX_BUILTIN_STXVW4X_V4SI:
      return altivec_expand_stv_builtin (CODE_FOR_vsx_store_v4si, exp);
    case VSX_BUILTIN_STXVW4X_V8HI:
      return altivec_expand_stv_builtin (CODE_FOR_vsx_store_v8hi, exp);
    case VSX_BUILTIN_STXVW4X_V16QI:
      return altivec_expand_stv_builtin (CODE_FOR_vsx_store_v16qi, exp);

    case ALTIVEC_BUILTIN_MFVSCR:
      icode = CODE_FOR_altivec_mfvscr;
      tmode = insn_data[icode].operand[0].mode;

      if (target == 0
	  || GET_MODE (target) != tmode
	  || ! (*insn_data[icode].operand[0].predicate) (target, tmode))
	target = gen_reg_rtx (tmode);

      pat = GEN_FCN (icode) (target);
      if (! pat)
	return 0;
      emit_insn (pat);
      return target;

    case ALTIVEC_BUILTIN_MTVSCR:
      icode = CODE_FOR_altivec_mtvscr;
      arg0 = CALL_EXPR_ARG (exp, 0);
      op0 = expand_normal (arg0);
      mode0 = insn_data[icode].operand[0].mode;

      /* If we got invalid arguments bail out before generating bad rtl.  */
      if (arg0 == error_mark_node)
	return const0_rtx;

      if (! (*insn_data[icode].operand[0].predicate) (op0, mode0))
	op0 = copy_to_mode_reg (mode0, op0);

      pat = GEN_FCN (icode) (op0);
      if (pat)
	emit_insn (pat);
      return NULL_RTX;

    case ALTIVEC_BUILTIN_DSSALL:
      emit_insn (gen_altivec_dssall ());
      return NULL_RTX;

    case ALTIVEC_BUILTIN_DSS:
      icode = CODE_FOR_altivec_dss;
      arg0 = CALL_EXPR_ARG (exp, 0);
      STRIP_NOPS (arg0);
      op0 = expand_normal (arg0);
      mode0 = insn_data[icode].operand[0].mode;

      /* If we got invalid arguments bail out before generating bad rtl.  */
      if (arg0 == error_mark_node)
	return const0_rtx;

      if (TREE_CODE (arg0) != INTEGER_CST
	  || TREE_INT_CST_LOW (arg0) & ~0x3)
	{
	  error ("argument to dss must be a 2-bit unsigned literal");
	  return const0_rtx;
	}

      if (! (*insn_data[icode].operand[0].predicate) (op0, mode0))
	op0 = copy_to_mode_reg (mode0, op0);

      emit_insn (gen_altivec_dss (op0));
      return NULL_RTX;

    case ALTIVEC_BUILTIN_VEC_INIT_V4SI:
    case ALTIVEC_BUILTIN_VEC_INIT_V8HI:
    case ALTIVEC_BUILTIN_VEC_INIT_V16QI:
    case ALTIVEC_BUILTIN_VEC_INIT_V4SF:
    case VSX_BUILTIN_VEC_INIT_V2DF:
    case VSX_BUILTIN_VEC_INIT_V2DI:
      return altivec_expand_vec_init_builtin (TREE_TYPE (exp), exp, target);

    case ALTIVEC_BUILTIN_VEC_SET_V4SI:
    case ALTIVEC_BUILTIN_VEC_SET_V8HI:
    case ALTIVEC_BUILTIN_VEC_SET_V16QI:
    case ALTIVEC_BUILTIN_VEC_SET_V4SF:
    case VSX_BUILTIN_VEC_SET_V2DF:
    case VSX_BUILTIN_VEC_SET_V2DI:
      return altivec_expand_vec_set_builtin (exp);

    case ALTIVEC_BUILTIN_VEC_EXT_V4SI:
    case ALTIVEC_BUILTIN_VEC_EXT_V8HI:
    case ALTIVEC_BUILTIN_VEC_EXT_V16QI:
    case ALTIVEC_BUILTIN_VEC_EXT_V4SF:
    case VSX_BUILTIN_VEC_EXT_V2DF:
    case VSX_BUILTIN_VEC_EXT_V2DI:
      return altivec_expand_vec_ext_builtin (exp, target);

    default:
      break;
      /* Fall through.  */
    }

  /* Expand abs* operations.  */
  d = bdesc_abs;
  for (i = 0; i < ARRAY_SIZE (bdesc_abs); i++, d++)
    if (d->code == fcode)
      return altivec_expand_abs_builtin (d->icode, exp, target);

  /* Expand the AltiVec predicates.  */
  d = bdesc_altivec_preds;
  for (i = 0; i < ARRAY_SIZE (bdesc_altivec_preds); i++, d++)
    if (d->code == fcode)
      return altivec_expand_predicate_builtin (d->icode, exp, target);

  /* LV* are funky.  We initialized them differently.  */
  switch (fcode)
    {
    case ALTIVEC_BUILTIN_LVSL:
      return altivec_expand_lv_builtin (CODE_FOR_altivec_lvsl,
					exp, target, false);
    case ALTIVEC_BUILTIN_LVSR:
      return altivec_expand_lv_builtin (CODE_FOR_altivec_lvsr,
					exp, target, false);
    case ALTIVEC_BUILTIN_LVEBX:
      return altivec_expand_lv_builtin (CODE_FOR_altivec_lvebx,
					exp, target, false);
    case ALTIVEC_BUILTIN_LVEHX:
      return altivec_expand_lv_builtin (CODE_FOR_altivec_lvehx,
					exp, target, false);
    case ALTIVEC_BUILTIN_LVEWX:
      return altivec_expand_lv_builtin (CODE_FOR_altivec_lvewx,
					exp, target, false);
    case ALTIVEC_BUILTIN_LVXL_V2DF:
      return altivec_expand_lv_builtin (CODE_FOR_altivec_lvxl_v2df,
					exp, target, false);
    case ALTIVEC_BUILTIN_LVXL_V2DI:
      return altivec_expand_lv_builtin (CODE_FOR_altivec_lvxl_v2di,
					exp, target, false);
    case ALTIVEC_BUILTIN_LVXL_V4SF:
      return altivec_expand_lv_builtin (CODE_FOR_altivec_lvxl_v4sf,
					exp, target, false);
    case ALTIVEC_BUILTIN_LVXL:
    case ALTIVEC_BUILTIN_LVXL_V4SI:
      return altivec_expand_lv_builtin (CODE_FOR_altivec_lvxl_v4si,
					exp, target, false);
    case ALTIVEC_BUILTIN_LVXL_V8HI:
      return altivec_expand_lv_builtin (CODE_FOR_altivec_lvxl_v8hi,
					exp, target, false);
    case ALTIVEC_BUILTIN_LVXL_V16QI:
      return altivec_expand_lv_builtin (CODE_FOR_altivec_lvxl_v16qi,
					exp, target, false);
    case ALTIVEC_BUILTIN_LVX_V2DF:
      return altivec_expand_lv_builtin (CODE_FOR_altivec_lvx_v2df,
					exp, target, false);
    case ALTIVEC_BUILTIN_LVX_V2DI:
      return altivec_expand_lv_builtin (CODE_FOR_altivec_lvx_v2di,
					exp, target, false);
    case ALTIVEC_BUILTIN_LVX_V4SF:
      return altivec_expand_lv_builtin (CODE_FOR_altivec_lvx_v4sf,
					exp, target, false);
    case ALTIVEC_BUILTIN_LVX:
    case ALTIVEC_BUILTIN_LVX_V4SI:
      return altivec_expand_lv_builtin (CODE_FOR_altivec_lvx_v4si,
					exp, target, false);
    case ALTIVEC_BUILTIN_LVX_V8HI:
      return altivec_expand_lv_builtin (CODE_FOR_altivec_lvx_v8hi,
					exp, target, false);
    case ALTIVEC_BUILTIN_LVX_V16QI:
      return altivec_expand_lv_builtin (CODE_FOR_altivec_lvx_v16qi,
					exp, target, false);
    case ALTIVEC_BUILTIN_LVLX:
      return altivec_expand_lv_builtin (CODE_FOR_altivec_lvlx,
					exp, target, true);
    case ALTIVEC_BUILTIN_LVLXL:
      return altivec_expand_lv_builtin (CODE_FOR_altivec_lvlxl,
					exp, target, true);
    case ALTIVEC_BUILTIN_LVRX:
      return altivec_expand_lv_builtin (CODE_FOR_altivec_lvrx,
					exp, target, true);
    case ALTIVEC_BUILTIN_LVRXL:
      return altivec_expand_lv_builtin (CODE_FOR_altivec_lvrxl,
					exp, target, true);
    case VSX_BUILTIN_LXVD2X_V2DF:
      return altivec_expand_lv_builtin (CODE_FOR_vsx_load_v2df,
					exp, target, false);
    case VSX_BUILTIN_LXVD2X_V2DI:
      return altivec_expand_lv_builtin (CODE_FOR_vsx_load_v2di,
					exp, target, false);
    case VSX_BUILTIN_LXVW4X_V4SF:
      return altivec_expand_lv_builtin (CODE_FOR_vsx_load_v4sf,
					exp, target, false);
    case VSX_BUILTIN_LXVW4X_V4SI:
      return altivec_expand_lv_builtin (CODE_FOR_vsx_load_v4si,
					exp, target, false);
    case VSX_BUILTIN_LXVW4X_V8HI:
      return altivec_expand_lv_builtin (CODE_FOR_vsx_load_v8hi,
					exp, target, false);
    case VSX_BUILTIN_LXVW4X_V16QI:
      return altivec_expand_lv_builtin (CODE_FOR_vsx_load_v16qi,
					exp, target, false);
      break;
    default:
      break;
      /* Fall through.  */
    }

  *expandedp = false;
  return NULL_RTX;
}

/* Expand the builtin in EXP and store the result in TARGET.  Store
   true in *EXPANDEDP if we found a builtin to expand.  */
static rtx
paired_expand_builtin (tree exp, rtx target, bool * expandedp)
{
  tree fndecl = TREE_OPERAND (CALL_EXPR_FN (exp), 0);
  enum rs6000_builtins fcode = (enum rs6000_builtins) DECL_FUNCTION_CODE (fndecl);
  const struct builtin_description *d;
  size_t i;

  *expandedp = true;

  switch (fcode)
    {
    case PAIRED_BUILTIN_STX:
      return paired_expand_stv_builtin (CODE_FOR_paired_stx, exp);
    case PAIRED_BUILTIN_LX:
      return paired_expand_lv_builtin (CODE_FOR_paired_lx, exp, target);
    default:
      break;
      /* Fall through.  */
    }

  /* Expand the paired predicates.  */
  d = bdesc_paired_preds;
  for (i = 0; i < ARRAY_SIZE (bdesc_paired_preds); i++, d++)
    if (d->code == fcode)
      return paired_expand_predicate_builtin (d->icode, exp, target);

  *expandedp = false;
  return NULL_RTX;
}

/* Binops that need to be initialized manually, but can be expanded
   automagically by rs6000_expand_binop_builtin.  */
static const struct builtin_description bdesc_2arg_spe[] =
{
  { RS6000_BTM_SPE, CODE_FOR_spe_evlddx, "__builtin_spe_evlddx", SPE_BUILTIN_EVLDDX },
  { RS6000_BTM_SPE, CODE_FOR_spe_evldwx, "__builtin_spe_evldwx", SPE_BUILTIN_EVLDWX },
  { RS6000_BTM_SPE, CODE_FOR_spe_evldhx, "__builtin_spe_evldhx", SPE_BUILTIN_EVLDHX },
  { RS6000_BTM_SPE, CODE_FOR_spe_evlwhex, "__builtin_spe_evlwhex", SPE_BUILTIN_EVLWHEX },
  { RS6000_BTM_SPE, CODE_FOR_spe_evlwhoux, "__builtin_spe_evlwhoux", SPE_BUILTIN_EVLWHOUX },
  { RS6000_BTM_SPE, CODE_FOR_spe_evlwhosx, "__builtin_spe_evlwhosx", SPE_BUILTIN_EVLWHOSX },
  { RS6000_BTM_SPE, CODE_FOR_spe_evlwwsplatx, "__builtin_spe_evlwwsplatx", SPE_BUILTIN_EVLWWSPLATX },
  { RS6000_BTM_SPE, CODE_FOR_spe_evlwhsplatx, "__builtin_spe_evlwhsplatx", SPE_BUILTIN_EVLWHSPLATX },
  { RS6000_BTM_SPE, CODE_FOR_spe_evlhhesplatx, "__builtin_spe_evlhhesplatx", SPE_BUILTIN_EVLHHESPLATX },
  { RS6000_BTM_SPE, CODE_FOR_spe_evlhhousplatx, "__builtin_spe_evlhhousplatx", SPE_BUILTIN_EVLHHOUSPLATX },
  { RS6000_BTM_SPE, CODE_FOR_spe_evlhhossplatx, "__builtin_spe_evlhhossplatx", SPE_BUILTIN_EVLHHOSSPLATX },
  { RS6000_BTM_SPE, CODE_FOR_spe_evldd, "__builtin_spe_evldd", SPE_BUILTIN_EVLDD },
  { RS6000_BTM_SPE, CODE_FOR_spe_evldw, "__builtin_spe_evldw", SPE_BUILTIN_EVLDW },
  { RS6000_BTM_SPE, CODE_FOR_spe_evldh, "__builtin_spe_evldh", SPE_BUILTIN_EVLDH },
  { RS6000_BTM_SPE, CODE_FOR_spe_evlwhe, "__builtin_spe_evlwhe", SPE_BUILTIN_EVLWHE },
  { RS6000_BTM_SPE, CODE_FOR_spe_evlwhou, "__builtin_spe_evlwhou", SPE_BUILTIN_EVLWHOU },
  { RS6000_BTM_SPE, CODE_FOR_spe_evlwhos, "__builtin_spe_evlwhos", SPE_BUILTIN_EVLWHOS },
  { RS6000_BTM_SPE, CODE_FOR_spe_evlwwsplat, "__builtin_spe_evlwwsplat", SPE_BUILTIN_EVLWWSPLAT },
  { RS6000_BTM_SPE, CODE_FOR_spe_evlwhsplat, "__builtin_spe_evlwhsplat", SPE_BUILTIN_EVLWHSPLAT },
  { RS6000_BTM_SPE, CODE_FOR_spe_evlhhesplat, "__builtin_spe_evlhhesplat", SPE_BUILTIN_EVLHHESPLAT },
  { RS6000_BTM_SPE, CODE_FOR_spe_evlhhousplat, "__builtin_spe_evlhhousplat", SPE_BUILTIN_EVLHHOUSPLAT },
  { RS6000_BTM_SPE, CODE_FOR_spe_evlhhossplat, "__builtin_spe_evlhhossplat", SPE_BUILTIN_EVLHHOSSPLAT }
};

/* Expand the builtin in EXP and store the result in TARGET.  Store
   true in *EXPANDEDP if we found a builtin to expand.

   This expands the SPE builtins that are not simple unary and binary
   operations.  */
static rtx
spe_expand_builtin (tree exp, rtx target, bool *expandedp)
{
  tree fndecl = TREE_OPERAND (CALL_EXPR_FN (exp), 0);
  tree arg1, arg0;
  enum rs6000_builtins fcode = (enum rs6000_builtins) DECL_FUNCTION_CODE (fndecl);
  enum insn_code icode;
  enum machine_mode tmode, mode0;
  rtx pat, op0;
  const struct builtin_description *d;
  size_t i;

  *expandedp = true;

  /* Syntax check for a 5-bit unsigned immediate.  */
  switch (fcode)
    {
    case SPE_BUILTIN_EVSTDD:
    case SPE_BUILTIN_EVSTDH:
    case SPE_BUILTIN_EVSTDW:
    case SPE_BUILTIN_EVSTWHE:
    case SPE_BUILTIN_EVSTWHO:
    case SPE_BUILTIN_EVSTWWE:
    case SPE_BUILTIN_EVSTWWO:
      arg1 = CALL_EXPR_ARG (exp, 2);
      if (TREE_CODE (arg1) != INTEGER_CST
	  || TREE_INT_CST_LOW (arg1) & ~0x1f)
	{
	  error ("argument 2 must be a 5-bit unsigned literal");
	  return const0_rtx;
	}
      break;
    default:
      break;
    }

  /* The evsplat*i instructions are not quite generic.  */
  switch (fcode)
    {
    case SPE_BUILTIN_EVSPLATFI:
      return rs6000_expand_unop_builtin (CODE_FOR_spe_evsplatfi,
					 exp, target);
    case SPE_BUILTIN_EVSPLATI:
      return rs6000_expand_unop_builtin (CODE_FOR_spe_evsplati,
					 exp, target);
    default:
      break;
    }

  d = bdesc_2arg_spe;
  for (i = 0; i < ARRAY_SIZE (bdesc_2arg_spe); ++i, ++d)
    if (d->code == fcode)
      return rs6000_expand_binop_builtin (d->icode, exp, target);

  d = bdesc_spe_predicates;
  for (i = 0; i < ARRAY_SIZE (bdesc_spe_predicates); ++i, ++d)
    if (d->code == fcode)
      return spe_expand_predicate_builtin (d->icode, exp, target);

  d = bdesc_spe_evsel;
  for (i = 0; i < ARRAY_SIZE (bdesc_spe_evsel); ++i, ++d)
    if (d->code == fcode)
      return spe_expand_evsel_builtin (d->icode, exp, target);

  switch (fcode)
    {
    case SPE_BUILTIN_EVSTDDX:
      return spe_expand_stv_builtin (CODE_FOR_spe_evstddx, exp);
    case SPE_BUILTIN_EVSTDHX:
      return spe_expand_stv_builtin (CODE_FOR_spe_evstdhx, exp);
    case SPE_BUILTIN_EVSTDWX:
      return spe_expand_stv_builtin (CODE_FOR_spe_evstdwx, exp);
    case SPE_BUILTIN_EVSTWHEX:
      return spe_expand_stv_builtin (CODE_FOR_spe_evstwhex, exp);
    case SPE_BUILTIN_EVSTWHOX:
      return spe_expand_stv_builtin (CODE_FOR_spe_evstwhox, exp);
    case SPE_BUILTIN_EVSTWWEX:
      return spe_expand_stv_builtin (CODE_FOR_spe_evstwwex, exp);
    case SPE_BUILTIN_EVSTWWOX:
      return spe_expand_stv_builtin (CODE_FOR_spe_evstwwox, exp);
    case SPE_BUILTIN_EVSTDD:
      return spe_expand_stv_builtin (CODE_FOR_spe_evstdd, exp);
    case SPE_BUILTIN_EVSTDH:
      return spe_expand_stv_builtin (CODE_FOR_spe_evstdh, exp);
    case SPE_BUILTIN_EVSTDW:
      return spe_expand_stv_builtin (CODE_FOR_spe_evstdw, exp);
    case SPE_BUILTIN_EVSTWHE:
      return spe_expand_stv_builtin (CODE_FOR_spe_evstwhe, exp);
    case SPE_BUILTIN_EVSTWHO:
      return spe_expand_stv_builtin (CODE_FOR_spe_evstwho, exp);
    case SPE_BUILTIN_EVSTWWE:
      return spe_expand_stv_builtin (CODE_FOR_spe_evstwwe, exp);
    case SPE_BUILTIN_EVSTWWO:
      return spe_expand_stv_builtin (CODE_FOR_spe_evstwwo, exp);
    case SPE_BUILTIN_MFSPEFSCR:
      icode = CODE_FOR_spe_mfspefscr;
      tmode = insn_data[icode].operand[0].mode;

      if (target == 0
	  || GET_MODE (target) != tmode
	  || ! (*insn_data[icode].operand[0].predicate) (target, tmode))
	target = gen_reg_rtx (tmode);

      pat = GEN_FCN (icode) (target);
      if (! pat)
	return 0;
      emit_insn (pat);
      return target;
    case SPE_BUILTIN_MTSPEFSCR:
      icode = CODE_FOR_spe_mtspefscr;
      arg0 = CALL_EXPR_ARG (exp, 0);
      op0 = expand_normal (arg0);
      mode0 = insn_data[icode].operand[0].mode;

      if (arg0 == error_mark_node)
	return const0_rtx;

      if (! (*insn_data[icode].operand[0].predicate) (op0, mode0))
	op0 = copy_to_mode_reg (mode0, op0);

      pat = GEN_FCN (icode) (op0);
      if (pat)
	emit_insn (pat);
      return NULL_RTX;
    default:
      break;
    }

  *expandedp = false;
  return NULL_RTX;
}

static rtx
paired_expand_predicate_builtin (enum insn_code icode, tree exp, rtx target)
{
  rtx pat, scratch, tmp;
  tree form = CALL_EXPR_ARG (exp, 0);
  tree arg0 = CALL_EXPR_ARG (exp, 1);
  tree arg1 = CALL_EXPR_ARG (exp, 2);
  rtx op0 = expand_normal (arg0);
  rtx op1 = expand_normal (arg1);
  enum machine_mode mode0 = insn_data[icode].operand[1].mode;
  enum machine_mode mode1 = insn_data[icode].operand[2].mode;
  int form_int;
  enum rtx_code code;

  if (TREE_CODE (form) != INTEGER_CST)
    {
      error ("argument 1 of __builtin_paired_predicate must be a constant");
      return const0_rtx;
    }
  else
    form_int = TREE_INT_CST_LOW (form);

  gcc_assert (mode0 == mode1);

  if (arg0 == error_mark_node || arg1 == error_mark_node)
    return const0_rtx;

  if (target == 0
      || GET_MODE (target) != SImode
      || !(*insn_data[icode].operand[0].predicate) (target, SImode))
    target = gen_reg_rtx (SImode);
  if (!(*insn_data[icode].operand[1].predicate) (op0, mode0))
    op0 = copy_to_mode_reg (mode0, op0);
  if (!(*insn_data[icode].operand[2].predicate) (op1, mode1))
    op1 = copy_to_mode_reg (mode1, op1);

  scratch = gen_reg_rtx (CCFPmode);

  pat = GEN_FCN (icode) (scratch, op0, op1);
  if (!pat)
    return const0_rtx;

  emit_insn (pat);

  switch (form_int)
    {
      /* LT bit.  */
    case 0:
      code = LT;
      break;
      /* GT bit.  */
    case 1:
      code = GT;
      break;
      /* EQ bit.  */
    case 2:
      code = EQ;
      break;
      /* UN bit.  */
    case 3:
      emit_insn (gen_move_from_CR_ov_bit (target, scratch));
      return target;
    default:
      error ("argument 1 of __builtin_paired_predicate is out of range");
      return const0_rtx;
    }

  tmp = gen_rtx_fmt_ee (code, SImode, scratch, const0_rtx);
  emit_move_insn (target, tmp);
  return target;
}

static rtx
spe_expand_predicate_builtin (enum insn_code icode, tree exp, rtx target)
{
  rtx pat, scratch, tmp;
  tree form = CALL_EXPR_ARG (exp, 0);
  tree arg0 = CALL_EXPR_ARG (exp, 1);
  tree arg1 = CALL_EXPR_ARG (exp, 2);
  rtx op0 = expand_normal (arg0);
  rtx op1 = expand_normal (arg1);
  enum machine_mode mode0 = insn_data[icode].operand[1].mode;
  enum machine_mode mode1 = insn_data[icode].operand[2].mode;
  int form_int;
  enum rtx_code code;

  if (TREE_CODE (form) != INTEGER_CST)
    {
      error ("argument 1 of __builtin_spe_predicate must be a constant");
      return const0_rtx;
    }
  else
    form_int = TREE_INT_CST_LOW (form);

  gcc_assert (mode0 == mode1);

  if (arg0 == error_mark_node || arg1 == error_mark_node)
    return const0_rtx;

  if (target == 0
      || GET_MODE (target) != SImode
      || ! (*insn_data[icode].operand[0].predicate) (target, SImode))
    target = gen_reg_rtx (SImode);

  if (! (*insn_data[icode].operand[1].predicate) (op0, mode0))
    op0 = copy_to_mode_reg (mode0, op0);
  if (! (*insn_data[icode].operand[2].predicate) (op1, mode1))
    op1 = copy_to_mode_reg (mode1, op1);

  scratch = gen_reg_rtx (CCmode);

  pat = GEN_FCN (icode) (scratch, op0, op1);
  if (! pat)
    return const0_rtx;
  emit_insn (pat);

  /* There are 4 variants for each predicate: _any_, _all_, _upper_,
     _lower_.  We use one compare, but look in different bits of the
     CR for each variant.

     There are 2 elements in each SPE simd type (upper/lower).  The CR
     bits are set as follows:

     BIT0  | BIT 1  | BIT 2   | BIT 3
     U     |   L    | (U | L) | (U & L)

     So, for an "all" relationship, BIT 3 would be set.
     For an "any" relationship, BIT 2 would be set.  Etc.

     Following traditional nomenclature, these bits map to:

     BIT0  | BIT 1  | BIT 2   | BIT 3
     LT    | GT     | EQ      | OV

     Later, we will generate rtl to look in the LT/EQ/EQ/OV bits.
  */

  switch (form_int)
    {
      /* All variant.  OV bit.  */
    case 0:
      /* We need to get to the OV bit, which is the ORDERED bit.  We
	 could generate (ordered:SI (reg:CC xx) (const_int 0)), but
	 that's ugly and will make validate_condition_mode die.
	 So let's just use another pattern.  */
      emit_insn (gen_move_from_CR_ov_bit (target, scratch));
      return target;
      /* Any variant.  EQ bit.  */
    case 1:
      code = EQ;
      break;
      /* Upper variant.  LT bit.  */
    case 2:
      code = LT;
      break;
      /* Lower variant.  GT bit.  */
    case 3:
      code = GT;
      break;
    default:
      error ("argument 1 of __builtin_spe_predicate is out of range");
      return const0_rtx;
    }

  tmp = gen_rtx_fmt_ee (code, SImode, scratch, const0_rtx);
  emit_move_insn (target, tmp);

  return target;
}

/* The evsel builtins look like this:

     e = __builtin_spe_evsel_OP (a, b, c, d);

   and work like this:

     e[upper] = a[upper] *OP* b[upper] ? c[upper] : d[upper];
     e[lower] = a[lower] *OP* b[lower] ? c[lower] : d[lower];
*/

static rtx
spe_expand_evsel_builtin (enum insn_code icode, tree exp, rtx target)
{
  rtx pat, scratch;
  tree arg0 = CALL_EXPR_ARG (exp, 0);
  tree arg1 = CALL_EXPR_ARG (exp, 1);
  tree arg2 = CALL_EXPR_ARG (exp, 2);
  tree arg3 = CALL_EXPR_ARG (exp, 3);
  rtx op0 = expand_normal (arg0);
  rtx op1 = expand_normal (arg1);
  rtx op2 = expand_normal (arg2);
  rtx op3 = expand_normal (arg3);
  enum machine_mode mode0 = insn_data[icode].operand[1].mode;
  enum machine_mode mode1 = insn_data[icode].operand[2].mode;

  gcc_assert (mode0 == mode1);

  if (arg0 == error_mark_node || arg1 == error_mark_node
      || arg2 == error_mark_node || arg3 == error_mark_node)
    return const0_rtx;

  if (target == 0
      || GET_MODE (target) != mode0
      || ! (*insn_data[icode].operand[0].predicate) (target, mode0))
    target = gen_reg_rtx (mode0);

  if (! (*insn_data[icode].operand[1].predicate) (op0, mode0))
    op0 = copy_to_mode_reg (mode0, op0);
  if (! (*insn_data[icode].operand[1].predicate) (op1, mode1))
    op1 = copy_to_mode_reg (mode0, op1);
  if (! (*insn_data[icode].operand[1].predicate) (op2, mode1))
    op2 = copy_to_mode_reg (mode0, op2);
  if (! (*insn_data[icode].operand[1].predicate) (op3, mode1))
    op3 = copy_to_mode_reg (mode0, op3);

  /* Generate the compare.  */
  scratch = gen_reg_rtx (CCmode);
  pat = GEN_FCN (icode) (scratch, op0, op1);
  if (! pat)
    return const0_rtx;
  emit_insn (pat);

  if (mode0 == V2SImode)
    emit_insn (gen_spe_evsel (target, op2, op3, scratch));
  else
    emit_insn (gen_spe_evsel_fs (target, op2, op3, scratch));

  return target;
}

/* Raise an error message for a builtin function that is called without the
   appropriate target options being set.  */

static void
rs6000_invalid_builtin (enum rs6000_builtins fncode)
{
  size_t uns_fncode = (size_t)fncode;
  const char *name = rs6000_builtin_info[uns_fncode].name;
  HOST_WIDE_INT fnmask = rs6000_builtin_info[uns_fncode].mask;

  gcc_assert (name != NULL);
  if ((fnmask & RS6000_BTM_CELL) != 0)
    error ("Builtin function %s is only valid for the cell processor", name);
  else if ((fnmask & RS6000_BTM_VSX) != 0)
    error ("Builtin function %s requires the -mvsx option", name);
  else if ((fnmask & RS6000_BTM_HTM) != 0)
    error ("Builtin function %s requires the -mhtm option", name);
  else if ((fnmask & RS6000_BTM_ALTIVEC) != 0)
    error ("Builtin function %s requires the -maltivec option", name);
  else if ((fnmask & RS6000_BTM_PAIRED) != 0)
    error ("Builtin function %s requires the -mpaired option", name);
  else if ((fnmask & RS6000_BTM_SPE) != 0)
    error ("Builtin function %s requires the -mspe option", name);
  else
    error ("Builtin function %s is not supported with the current options",
	   name);
}

/* Expand an expression EXP that calls a built-in function,
   with result going to TARGET if that's convenient
   (and in mode MODE if that's convenient).
   SUBTARGET may be used as the target for computing one of EXP's operands.
   IGNORE is nonzero if the value is to be ignored.  */

static rtx
rs6000_expand_builtin (tree exp, rtx target, rtx subtarget ATTRIBUTE_UNUSED,
		       enum machine_mode mode ATTRIBUTE_UNUSED,
		       int ignore ATTRIBUTE_UNUSED)
{
  tree fndecl = TREE_OPERAND (CALL_EXPR_FN (exp), 0);
  enum rs6000_builtins fcode
    = (enum rs6000_builtins)DECL_FUNCTION_CODE (fndecl);
  size_t uns_fcode = (size_t)fcode;
  const struct builtin_description *d;
  size_t i;
  rtx ret;
  bool success;
  HOST_WIDE_INT mask = rs6000_builtin_info[uns_fcode].mask;
  bool func_valid_p = ((rs6000_builtin_mask & mask) == mask);

  if (TARGET_DEBUG_BUILTIN)
    {
      enum insn_code icode = rs6000_builtin_info[uns_fcode].icode;
      const char *name1 = rs6000_builtin_info[uns_fcode].name;
      const char *name2 = ((icode != CODE_FOR_nothing)
			   ? get_insn_name ((int)icode)
			   : "nothing");
      const char *name3;

      switch (rs6000_builtin_info[uns_fcode].attr & RS6000_BTC_TYPE_MASK)
	{
	default:		   name3 = "unknown";	break;
	case RS6000_BTC_SPECIAL:   name3 = "special";	break;
	case RS6000_BTC_UNARY:	   name3 = "unary";	break;
	case RS6000_BTC_BINARY:	   name3 = "binary";	break;
	case RS6000_BTC_TERNARY:   name3 = "ternary";	break;
	case RS6000_BTC_PREDICATE: name3 = "predicate";	break;
	case RS6000_BTC_ABS:	   name3 = "abs";	break;
	case RS6000_BTC_EVSEL:	   name3 = "evsel";	break;
	case RS6000_BTC_DST:	   name3 = "dst";	break;
	}


      fprintf (stderr,
	       "rs6000_expand_builtin, %s (%d), insn = %s (%d), type=%s%s\n",
	       (name1) ? name1 : "---", fcode,
	       (name2) ? name2 : "---", (int)icode,
	       name3,
	       func_valid_p ? "" : ", not valid");
    }	     

  if (!func_valid_p)
    {
      rs6000_invalid_builtin (fcode);

      /* Given it is invalid, just generate a normal call.  */
      return expand_call (exp, target, ignore);
    }

  switch (fcode)
    {
    case RS6000_BUILTIN_RECIP:
      return rs6000_expand_binop_builtin (CODE_FOR_recipdf3, exp, target);

    case RS6000_BUILTIN_RECIPF:
      return rs6000_expand_binop_builtin (CODE_FOR_recipsf3, exp, target);

    case RS6000_BUILTIN_RSQRTF:
      return rs6000_expand_unop_builtin (CODE_FOR_rsqrtsf2, exp, target);

    case RS6000_BUILTIN_RSQRT:
      return rs6000_expand_unop_builtin (CODE_FOR_rsqrtdf2, exp, target);

    case POWER7_BUILTIN_BPERMD:
      return rs6000_expand_binop_builtin (((TARGET_64BIT)
					   ? CODE_FOR_bpermd_di
					   : CODE_FOR_bpermd_si), exp, target);

    case RS6000_BUILTIN_GET_TB:
      return rs6000_expand_zeroop_builtin (CODE_FOR_rs6000_get_timebase,
					   target);

    case RS6000_BUILTIN_MFTB:
      return rs6000_expand_zeroop_builtin (((TARGET_64BIT)
					    ? CODE_FOR_rs6000_mftb_di
					    : CODE_FOR_rs6000_mftb_si),
					   target);

    case RS6000_BUILTIN_MFFS:
      return rs6000_expand_zeroop_builtin (CODE_FOR_rs6000_mffs, target);

    case RS6000_BUILTIN_MTFSF:
      return rs6000_expand_mtfsf_builtin (CODE_FOR_rs6000_mtfsf, exp);

    case ALTIVEC_BUILTIN_MASK_FOR_LOAD:
    case ALTIVEC_BUILTIN_MASK_FOR_STORE:
      {
	int icode = (BYTES_BIG_ENDIAN ? (int) CODE_FOR_altivec_lvsr
		     : (int) CODE_FOR_altivec_lvsl);
	enum machine_mode tmode = insn_data[icode].operand[0].mode;
	enum machine_mode mode = insn_data[icode].operand[1].mode;
	tree arg;
	rtx op, addr, pat;

	gcc_assert (TARGET_ALTIVEC);

	arg = CALL_EXPR_ARG (exp, 0);
	gcc_assert (POINTER_TYPE_P (TREE_TYPE (arg)));
	op = expand_expr (arg, NULL_RTX, Pmode, EXPAND_NORMAL);
	addr = memory_address (mode, op);
	if (fcode == ALTIVEC_BUILTIN_MASK_FOR_STORE)
	  op = addr;
	else
	  {
	    /* For the load case need to negate the address.  */
	    op = gen_reg_rtx (GET_MODE (addr));
	    emit_insn (gen_rtx_SET (VOIDmode, op,
				    gen_rtx_NEG (GET_MODE (addr), addr)));
	  }
	op = gen_rtx_MEM (mode, op);

	if (target == 0
	    || GET_MODE (target) != tmode
	    || ! (*insn_data[icode].operand[0].predicate) (target, tmode))
	  target = gen_reg_rtx (tmode);

	/*pat = gen_altivec_lvsr (target, op);*/
	pat = GEN_FCN (icode) (target, op);
	if (!pat)
	  return 0;
	emit_insn (pat);

	return target;
      }

    case ALTIVEC_BUILTIN_VCFUX:
    case ALTIVEC_BUILTIN_VCFSX:
    case ALTIVEC_BUILTIN_VCTUXS:
    case ALTIVEC_BUILTIN_VCTSXS:
  /* FIXME: There's got to be a nicer way to handle this case than
     constructing a new CALL_EXPR.  */
      if (call_expr_nargs (exp) == 1)
	{
	  exp = build_call_nary (TREE_TYPE (exp), CALL_EXPR_FN (exp),
				 2, CALL_EXPR_ARG (exp, 0), integer_zero_node);
	}
      break;

    default:
      break;
    }

  if (TARGET_ALTIVEC)
    {
      ret = altivec_expand_builtin (exp, target, &success);

      if (success)
	return ret;
    }
  if (TARGET_SPE)
    {
      ret = spe_expand_builtin (exp, target, &success);

      if (success)
	return ret;
    }
  if (TARGET_PAIRED_FLOAT)
    {
      ret = paired_expand_builtin (exp, target, &success);

      if (success)
	return ret;
    }  
  if (TARGET_HTM)
    {
      ret = htm_expand_builtin (exp, target, &success);

      if (success)
	return ret;
    }  

  gcc_assert (TARGET_ALTIVEC || TARGET_VSX || TARGET_SPE || TARGET_PAIRED_FLOAT);

  /* Handle simple unary operations.  */
  d = bdesc_1arg;
  for (i = 0; i < ARRAY_SIZE (bdesc_1arg); i++, d++)
    if (d->code == fcode)
      return rs6000_expand_unop_builtin (d->icode, exp, target);

  /* Handle simple binary operations.  */
  d = bdesc_2arg;
  for (i = 0; i < ARRAY_SIZE (bdesc_2arg); i++, d++)
    if (d->code == fcode)
      return rs6000_expand_binop_builtin (d->icode, exp, target);

  /* Handle simple ternary operations.  */
  d = bdesc_3arg;
  for (i = 0; i < ARRAY_SIZE  (bdesc_3arg); i++, d++)
    if (d->code == fcode)
      return rs6000_expand_ternop_builtin (d->icode, exp, target);

  gcc_unreachable ();
}

static void
rs6000_init_builtins (void)
{
  tree tdecl;
  tree ftype;
  enum machine_mode mode;

  if (TARGET_DEBUG_BUILTIN)
    fprintf (stderr, "rs6000_init_builtins%s%s%s%s\n",
	     (TARGET_PAIRED_FLOAT) ? ", paired"	 : "",
	     (TARGET_SPE)	   ? ", spe"	 : "",
	     (TARGET_ALTIVEC)	   ? ", altivec" : "",
	     (TARGET_VSX)	   ? ", vsx"	 : "");

  V2SI_type_node = build_vector_type (intSI_type_node, 2);
  V2SF_type_node = build_vector_type (float_type_node, 2);
  V2DI_type_node = build_vector_type (intDI_type_node, 2);
  V2DF_type_node = build_vector_type (double_type_node, 2);
  V4HI_type_node = build_vector_type (intHI_type_node, 4);
  V4SI_type_node = build_vector_type (intSI_type_node, 4);
  V4SF_type_node = build_vector_type (float_type_node, 4);
  V8HI_type_node = build_vector_type (intHI_type_node, 8);
  V16QI_type_node = build_vector_type (intQI_type_node, 16);

  unsigned_V16QI_type_node = build_vector_type (unsigned_intQI_type_node, 16);
  unsigned_V8HI_type_node = build_vector_type (unsigned_intHI_type_node, 8);
  unsigned_V4SI_type_node = build_vector_type (unsigned_intSI_type_node, 4);
  unsigned_V2DI_type_node = build_vector_type (unsigned_intDI_type_node, 2);

  opaque_V2SF_type_node = build_opaque_vector_type (float_type_node, 2);
  opaque_V2SI_type_node = build_opaque_vector_type (intSI_type_node, 2);
  opaque_p_V2SI_type_node = build_pointer_type (opaque_V2SI_type_node);
  opaque_V4SI_type_node = build_opaque_vector_type (intSI_type_node, 4);

  /* The 'vector bool ...' types must be kept distinct from 'vector unsigned ...'
     types, especially in C++ land.  Similarly, 'vector pixel' is distinct from
     'vector unsigned short'.  */

  bool_char_type_node = build_distinct_type_copy (unsigned_intQI_type_node);
  bool_short_type_node = build_distinct_type_copy (unsigned_intHI_type_node);
  bool_int_type_node = build_distinct_type_copy (unsigned_intSI_type_node);
  bool_long_type_node = build_distinct_type_copy (unsigned_intDI_type_node);
  pixel_type_node = build_distinct_type_copy (unsigned_intHI_type_node);

  long_integer_type_internal_node = long_integer_type_node;
  long_unsigned_type_internal_node = long_unsigned_type_node;
  long_long_integer_type_internal_node = long_long_integer_type_node;
  long_long_unsigned_type_internal_node = long_long_unsigned_type_node;
  intQI_type_internal_node = intQI_type_node;
  uintQI_type_internal_node = unsigned_intQI_type_node;
  intHI_type_internal_node = intHI_type_node;
  uintHI_type_internal_node = unsigned_intHI_type_node;
  intSI_type_internal_node = intSI_type_node;
  uintSI_type_internal_node = unsigned_intSI_type_node;
  intDI_type_internal_node = intDI_type_node;
  uintDI_type_internal_node = unsigned_intDI_type_node;
  float_type_internal_node = float_type_node;
  double_type_internal_node = double_type_node;
  void_type_internal_node = void_type_node;

  /* Initialize the modes for builtin_function_type, mapping a machine mode to
     tree type node.  */
  builtin_mode_to_type[QImode][0] = integer_type_node;
  builtin_mode_to_type[HImode][0] = integer_type_node;
  builtin_mode_to_type[SImode][0] = intSI_type_node;
  builtin_mode_to_type[SImode][1] = unsigned_intSI_type_node;
  builtin_mode_to_type[DImode][0] = intDI_type_node;
  builtin_mode_to_type[DImode][1] = unsigned_intDI_type_node;
  builtin_mode_to_type[SFmode][0] = float_type_node;
  builtin_mode_to_type[DFmode][0] = double_type_node;
  builtin_mode_to_type[V2SImode][0] = V2SI_type_node;
  builtin_mode_to_type[V2SFmode][0] = V2SF_type_node;
  builtin_mode_to_type[V2DImode][0] = V2DI_type_node;
  builtin_mode_to_type[V2DImode][1] = unsigned_V2DI_type_node;
  builtin_mode_to_type[V2DFmode][0] = V2DF_type_node;
  builtin_mode_to_type[V4HImode][0] = V4HI_type_node;
  builtin_mode_to_type[V4SImode][0] = V4SI_type_node;
  builtin_mode_to_type[V4SImode][1] = unsigned_V4SI_type_node;
  builtin_mode_to_type[V4SFmode][0] = V4SF_type_node;
  builtin_mode_to_type[V8HImode][0] = V8HI_type_node;
  builtin_mode_to_type[V8HImode][1] = unsigned_V8HI_type_node;
  builtin_mode_to_type[V16QImode][0] = V16QI_type_node;
  builtin_mode_to_type[V16QImode][1] = unsigned_V16QI_type_node;

  tdecl = add_builtin_type ("__bool char", bool_char_type_node);
  TYPE_NAME (bool_char_type_node) = tdecl;

  tdecl = add_builtin_type ("__bool short", bool_short_type_node);
  TYPE_NAME (bool_short_type_node) = tdecl;

  tdecl = add_builtin_type ("__bool int", bool_int_type_node);
  TYPE_NAME (bool_int_type_node) = tdecl;

  tdecl = add_builtin_type ("__pixel", pixel_type_node);
  TYPE_NAME (pixel_type_node) = tdecl;

  bool_V16QI_type_node = build_vector_type (bool_char_type_node, 16);
  bool_V8HI_type_node = build_vector_type (bool_short_type_node, 8);
  bool_V4SI_type_node = build_vector_type (bool_int_type_node, 4);
  bool_V2DI_type_node = build_vector_type (bool_long_type_node, 2);
  pixel_V8HI_type_node = build_vector_type (pixel_type_node, 8);

  tdecl = add_builtin_type ("__vector unsigned char", unsigned_V16QI_type_node);
  TYPE_NAME (unsigned_V16QI_type_node) = tdecl;

  tdecl = add_builtin_type ("__vector signed char", V16QI_type_node);
  TYPE_NAME (V16QI_type_node) = tdecl;

  tdecl = add_builtin_type ("__vector __bool char", bool_V16QI_type_node);
  TYPE_NAME ( bool_V16QI_type_node) = tdecl;

  tdecl = add_builtin_type ("__vector unsigned short", unsigned_V8HI_type_node);
  TYPE_NAME (unsigned_V8HI_type_node) = tdecl;

  tdecl = add_builtin_type ("__vector signed short", V8HI_type_node);
  TYPE_NAME (V8HI_type_node) = tdecl;

  tdecl = add_builtin_type ("__vector __bool short", bool_V8HI_type_node);
  TYPE_NAME (bool_V8HI_type_node) = tdecl;

  tdecl = add_builtin_type ("__vector unsigned int", unsigned_V4SI_type_node);
  TYPE_NAME (unsigned_V4SI_type_node) = tdecl;

  tdecl = add_builtin_type ("__vector signed int", V4SI_type_node);
  TYPE_NAME (V4SI_type_node) = tdecl;

  tdecl = add_builtin_type ("__vector __bool int", bool_V4SI_type_node);
  TYPE_NAME (bool_V4SI_type_node) = tdecl;

  tdecl = add_builtin_type ("__vector float", V4SF_type_node);
  TYPE_NAME (V4SF_type_node) = tdecl;

  tdecl = add_builtin_type ("__vector __pixel", pixel_V8HI_type_node);
  TYPE_NAME (pixel_V8HI_type_node) = tdecl;

  tdecl = add_builtin_type ("__vector double", V2DF_type_node);
  TYPE_NAME (V2DF_type_node) = tdecl;

  tdecl = add_builtin_type ("__vector long", V2DI_type_node);
  TYPE_NAME (V2DI_type_node) = tdecl;

  tdecl = add_builtin_type ("__vector unsigned long", unsigned_V2DI_type_node);
  TYPE_NAME (unsigned_V2DI_type_node) = tdecl;

  tdecl = add_builtin_type ("__vector __bool long", bool_V2DI_type_node);
  TYPE_NAME (bool_V2DI_type_node) = tdecl;

  /* Paired and SPE builtins are only available if you build a compiler with
     the appropriate options, so only create those builtins with the
     appropriate compiler option.  Create Altivec and VSX builtins on machines
     with at least the general purpose extensions (970 and newer) to allow the
     use of the target attribute.  */
  if (TARGET_PAIRED_FLOAT)
    paired_init_builtins ();
  if (TARGET_SPE)
    spe_init_builtins ();
  if (TARGET_EXTRA_BUILTINS)
    altivec_init_builtins ();
  if (TARGET_HTM)
    htm_init_builtins ();

  if (TARGET_EXTRA_BUILTINS || TARGET_SPE || TARGET_PAIRED_FLOAT)
    rs6000_common_init_builtins ();

  ftype = builtin_function_type (DFmode, DFmode, DFmode, VOIDmode,
				 RS6000_BUILTIN_RECIP, "__builtin_recipdiv");
  def_builtin ("__builtin_recipdiv", ftype, RS6000_BUILTIN_RECIP);

  ftype = builtin_function_type (SFmode, SFmode, SFmode, VOIDmode,
				 RS6000_BUILTIN_RECIPF, "__builtin_recipdivf");
  def_builtin ("__builtin_recipdivf", ftype, RS6000_BUILTIN_RECIPF);

  ftype = builtin_function_type (DFmode, DFmode, VOIDmode, VOIDmode,
				 RS6000_BUILTIN_RSQRT, "__builtin_rsqrt");
  def_builtin ("__builtin_rsqrt", ftype, RS6000_BUILTIN_RSQRT);

  ftype = builtin_function_type (SFmode, SFmode, VOIDmode, VOIDmode,
				 RS6000_BUILTIN_RSQRTF, "__builtin_rsqrtf");
  def_builtin ("__builtin_rsqrtf", ftype, RS6000_BUILTIN_RSQRTF);

  mode = (TARGET_64BIT) ? DImode : SImode;
  ftype = builtin_function_type (mode, mode, mode, VOIDmode,
				 POWER7_BUILTIN_BPERMD, "__builtin_bpermd");
  def_builtin ("__builtin_bpermd", ftype, POWER7_BUILTIN_BPERMD);

  ftype = build_function_type_list (unsigned_intDI_type_node,
				    NULL_TREE);
  def_builtin ("__builtin_ppc_get_timebase", ftype, RS6000_BUILTIN_GET_TB);

  if (TARGET_64BIT)
    ftype = build_function_type_list (unsigned_intDI_type_node,
				      NULL_TREE);
  else
    ftype = build_function_type_list (unsigned_intSI_type_node,
				      NULL_TREE);
  def_builtin ("__builtin_ppc_mftb", ftype, RS6000_BUILTIN_MFTB);

  ftype = build_function_type_list (double_type_node, NULL_TREE);
  def_builtin ("__builtin_mffs", ftype, RS6000_BUILTIN_MFFS);

  ftype = build_function_type_list (void_type_node,
				    intSI_type_node, double_type_node,
				    NULL_TREE);
  def_builtin ("__builtin_mtfsf", ftype, RS6000_BUILTIN_MTFSF);

#if TARGET_XCOFF
  /* AIX libm provides clog as __clog.  */
  if ((tdecl = builtin_decl_explicit (BUILT_IN_CLOG)) != NULL_TREE)
    set_user_assembler_name (tdecl, "__clog");
#endif

#ifdef SUBTARGET_INIT_BUILTINS
  SUBTARGET_INIT_BUILTINS;
#endif
}

/* Returns the rs6000 builtin decl for CODE.  */

static tree
rs6000_builtin_decl (unsigned code, bool initialize_p ATTRIBUTE_UNUSED)
{
  HOST_WIDE_INT fnmask;

  if (code >= RS6000_BUILTIN_COUNT)
    return error_mark_node;

  fnmask = rs6000_builtin_info[code].mask;
  if ((fnmask & rs6000_builtin_mask) != fnmask)
    {
      rs6000_invalid_builtin ((enum rs6000_builtins)code);
      return error_mark_node;
    }

  return rs6000_builtin_decls[code];
}

static void
spe_init_builtins (void)
{
  tree puint_type_node = build_pointer_type (unsigned_type_node);
  tree pushort_type_node = build_pointer_type (short_unsigned_type_node);
  const struct builtin_description *d;
  size_t i;

  tree v2si_ftype_4_v2si
    = build_function_type_list (opaque_V2SI_type_node,
                                opaque_V2SI_type_node,
                                opaque_V2SI_type_node,
                                opaque_V2SI_type_node,
                                opaque_V2SI_type_node,
                                NULL_TREE);

  tree v2sf_ftype_4_v2sf
    = build_function_type_list (opaque_V2SF_type_node,
                                opaque_V2SF_type_node,
                                opaque_V2SF_type_node,
                                opaque_V2SF_type_node,
                                opaque_V2SF_type_node,
                                NULL_TREE);

  tree int_ftype_int_v2si_v2si
    = build_function_type_list (integer_type_node,
                                integer_type_node,
                                opaque_V2SI_type_node,
                                opaque_V2SI_type_node,
                                NULL_TREE);

  tree int_ftype_int_v2sf_v2sf
    = build_function_type_list (integer_type_node,
                                integer_type_node,
                                opaque_V2SF_type_node,
                                opaque_V2SF_type_node,
                                NULL_TREE);

  tree void_ftype_v2si_puint_int
    = build_function_type_list (void_type_node,
                                opaque_V2SI_type_node,
                                puint_type_node,
                                integer_type_node,
                                NULL_TREE);

  tree void_ftype_v2si_puint_char
    = build_function_type_list (void_type_node,
                                opaque_V2SI_type_node,
                                puint_type_node,
                                char_type_node,
                                NULL_TREE);

  tree void_ftype_v2si_pv2si_int
    = build_function_type_list (void_type_node,
                                opaque_V2SI_type_node,
                                opaque_p_V2SI_type_node,
                                integer_type_node,
                                NULL_TREE);

  tree void_ftype_v2si_pv2si_char
    = build_function_type_list (void_type_node,
                                opaque_V2SI_type_node,
                                opaque_p_V2SI_type_node,
                                char_type_node,
                                NULL_TREE);

  tree void_ftype_int
    = build_function_type_list (void_type_node, integer_type_node, NULL_TREE);

  tree int_ftype_void
    = build_function_type_list (integer_type_node, NULL_TREE);

  tree v2si_ftype_pv2si_int
    = build_function_type_list (opaque_V2SI_type_node,
                                opaque_p_V2SI_type_node,
                                integer_type_node,
                                NULL_TREE);

  tree v2si_ftype_puint_int
    = build_function_type_list (opaque_V2SI_type_node,
                                puint_type_node,
                                integer_type_node,
                                NULL_TREE);

  tree v2si_ftype_pushort_int
    = build_function_type_list (opaque_V2SI_type_node,
                                pushort_type_node,
                                integer_type_node,
                                NULL_TREE);

  tree v2si_ftype_signed_char
    = build_function_type_list (opaque_V2SI_type_node,
                                signed_char_type_node,
                                NULL_TREE);

  add_builtin_type ("__ev64_opaque__", opaque_V2SI_type_node);

  /* Initialize irregular SPE builtins.  */

  def_builtin ("__builtin_spe_mtspefscr", void_ftype_int, SPE_BUILTIN_MTSPEFSCR);
  def_builtin ("__builtin_spe_mfspefscr", int_ftype_void, SPE_BUILTIN_MFSPEFSCR);
  def_builtin ("__builtin_spe_evstddx", void_ftype_v2si_pv2si_int, SPE_BUILTIN_EVSTDDX);
  def_builtin ("__builtin_spe_evstdhx", void_ftype_v2si_pv2si_int, SPE_BUILTIN_EVSTDHX);
  def_builtin ("__builtin_spe_evstdwx", void_ftype_v2si_pv2si_int, SPE_BUILTIN_EVSTDWX);
  def_builtin ("__builtin_spe_evstwhex", void_ftype_v2si_puint_int, SPE_BUILTIN_EVSTWHEX);
  def_builtin ("__builtin_spe_evstwhox", void_ftype_v2si_puint_int, SPE_BUILTIN_EVSTWHOX);
  def_builtin ("__builtin_spe_evstwwex", void_ftype_v2si_puint_int, SPE_BUILTIN_EVSTWWEX);
  def_builtin ("__builtin_spe_evstwwox", void_ftype_v2si_puint_int, SPE_BUILTIN_EVSTWWOX);
  def_builtin ("__builtin_spe_evstdd", void_ftype_v2si_pv2si_char, SPE_BUILTIN_EVSTDD);
  def_builtin ("__builtin_spe_evstdh", void_ftype_v2si_pv2si_char, SPE_BUILTIN_EVSTDH);
  def_builtin ("__builtin_spe_evstdw", void_ftype_v2si_pv2si_char, SPE_BUILTIN_EVSTDW);
  def_builtin ("__builtin_spe_evstwhe", void_ftype_v2si_puint_char, SPE_BUILTIN_EVSTWHE);
  def_builtin ("__builtin_spe_evstwho", void_ftype_v2si_puint_char, SPE_BUILTIN_EVSTWHO);
  def_builtin ("__builtin_spe_evstwwe", void_ftype_v2si_puint_char, SPE_BUILTIN_EVSTWWE);
  def_builtin ("__builtin_spe_evstwwo", void_ftype_v2si_puint_char, SPE_BUILTIN_EVSTWWO);
  def_builtin ("__builtin_spe_evsplatfi", v2si_ftype_signed_char, SPE_BUILTIN_EVSPLATFI);
  def_builtin ("__builtin_spe_evsplati", v2si_ftype_signed_char, SPE_BUILTIN_EVSPLATI);

  /* Loads.  */
  def_builtin ("__builtin_spe_evlddx", v2si_ftype_pv2si_int, SPE_BUILTIN_EVLDDX);
  def_builtin ("__builtin_spe_evldwx", v2si_ftype_pv2si_int, SPE_BUILTIN_EVLDWX);
  def_builtin ("__builtin_spe_evldhx", v2si_ftype_pv2si_int, SPE_BUILTIN_EVLDHX);
  def_builtin ("__builtin_spe_evlwhex", v2si_ftype_puint_int, SPE_BUILTIN_EVLWHEX);
  def_builtin ("__builtin_spe_evlwhoux", v2si_ftype_puint_int, SPE_BUILTIN_EVLWHOUX);
  def_builtin ("__builtin_spe_evlwhosx", v2si_ftype_puint_int, SPE_BUILTIN_EVLWHOSX);
  def_builtin ("__builtin_spe_evlwwsplatx", v2si_ftype_puint_int, SPE_BUILTIN_EVLWWSPLATX);
  def_builtin ("__builtin_spe_evlwhsplatx", v2si_ftype_puint_int, SPE_BUILTIN_EVLWHSPLATX);
  def_builtin ("__builtin_spe_evlhhesplatx", v2si_ftype_pushort_int, SPE_BUILTIN_EVLHHESPLATX);
  def_builtin ("__builtin_spe_evlhhousplatx", v2si_ftype_pushort_int, SPE_BUILTIN_EVLHHOUSPLATX);
  def_builtin ("__builtin_spe_evlhhossplatx", v2si_ftype_pushort_int, SPE_BUILTIN_EVLHHOSSPLATX);
  def_builtin ("__builtin_spe_evldd", v2si_ftype_pv2si_int, SPE_BUILTIN_EVLDD);
  def_builtin ("__builtin_spe_evldw", v2si_ftype_pv2si_int, SPE_BUILTIN_EVLDW);
  def_builtin ("__builtin_spe_evldh", v2si_ftype_pv2si_int, SPE_BUILTIN_EVLDH);
  def_builtin ("__builtin_spe_evlhhesplat", v2si_ftype_pushort_int, SPE_BUILTIN_EVLHHESPLAT);
  def_builtin ("__builtin_spe_evlhhossplat", v2si_ftype_pushort_int, SPE_BUILTIN_EVLHHOSSPLAT);
  def_builtin ("__builtin_spe_evlhhousplat", v2si_ftype_pushort_int, SPE_BUILTIN_EVLHHOUSPLAT);
  def_builtin ("__builtin_spe_evlwhe", v2si_ftype_puint_int, SPE_BUILTIN_EVLWHE);
  def_builtin ("__builtin_spe_evlwhos", v2si_ftype_puint_int, SPE_BUILTIN_EVLWHOS);
  def_builtin ("__builtin_spe_evlwhou", v2si_ftype_puint_int, SPE_BUILTIN_EVLWHOU);
  def_builtin ("__builtin_spe_evlwhsplat", v2si_ftype_puint_int, SPE_BUILTIN_EVLWHSPLAT);
  def_builtin ("__builtin_spe_evlwwsplat", v2si_ftype_puint_int, SPE_BUILTIN_EVLWWSPLAT);

  /* Predicates.  */
  d = bdesc_spe_predicates;
  for (i = 0; i < ARRAY_SIZE (bdesc_spe_predicates); ++i, d++)
    {
      tree type;

      switch (insn_data[d->icode].operand[1].mode)
	{
	case V2SImode:
	  type = int_ftype_int_v2si_v2si;
	  break;
	case V2SFmode:
	  type = int_ftype_int_v2sf_v2sf;
	  break;
	default:
	  gcc_unreachable ();
	}

      def_builtin (d->name, type, d->code);
    }

  /* Evsel predicates.  */
  d = bdesc_spe_evsel;
  for (i = 0; i < ARRAY_SIZE (bdesc_spe_evsel); ++i, d++)
    {
      tree type;

      switch (insn_data[d->icode].operand[1].mode)
	{
	case V2SImode:
	  type = v2si_ftype_4_v2si;
	  break;
	case V2SFmode:
	  type = v2sf_ftype_4_v2sf;
	  break;
	default:
	  gcc_unreachable ();
	}

      def_builtin (d->name, type, d->code);
    }
}

static void
paired_init_builtins (void)
{
  const struct builtin_description *d;
  size_t i;

   tree int_ftype_int_v2sf_v2sf
    = build_function_type_list (integer_type_node,
                                integer_type_node,
                                V2SF_type_node,
                                V2SF_type_node,
                                NULL_TREE);
  tree pcfloat_type_node =
    build_pointer_type (build_qualified_type
			(float_type_node, TYPE_QUAL_CONST));

  tree v2sf_ftype_long_pcfloat = build_function_type_list (V2SF_type_node,
							   long_integer_type_node,
							   pcfloat_type_node,
							   NULL_TREE);
  tree void_ftype_v2sf_long_pcfloat =
    build_function_type_list (void_type_node,
			      V2SF_type_node,
			      long_integer_type_node,
			      pcfloat_type_node,
			      NULL_TREE);


  def_builtin ("__builtin_paired_lx", v2sf_ftype_long_pcfloat,
	       PAIRED_BUILTIN_LX);


  def_builtin ("__builtin_paired_stx", void_ftype_v2sf_long_pcfloat,
	       PAIRED_BUILTIN_STX);

  /* Predicates.  */
  d = bdesc_paired_preds;
  for (i = 0; i < ARRAY_SIZE (bdesc_paired_preds); ++i, d++)
    {
      tree type;

      if (TARGET_DEBUG_BUILTIN)
	fprintf (stderr, "paired pred #%d, insn = %s [%d], mode = %s\n",
		 (int)i, get_insn_name (d->icode), (int)d->icode,
		 GET_MODE_NAME (insn_data[d->icode].operand[1].mode));

      switch (insn_data[d->icode].operand[1].mode)
	{
	case V2SFmode:
	  type = int_ftype_int_v2sf_v2sf;
	  break;
	default:
	  gcc_unreachable ();
	}

      def_builtin (d->name, type, d->code);
    }
}

static void
altivec_init_builtins (void)
{
  const struct builtin_description *d;
  size_t i;
  tree ftype;
  tree decl;

  tree pvoid_type_node = build_pointer_type (void_type_node);

  tree pcvoid_type_node
    = build_pointer_type (build_qualified_type (void_type_node,
						TYPE_QUAL_CONST));

  tree int_ftype_opaque
    = build_function_type_list (integer_type_node,
				opaque_V4SI_type_node, NULL_TREE);
  tree opaque_ftype_opaque
    = build_function_type_list (integer_type_node, NULL_TREE);
  tree opaque_ftype_opaque_int
    = build_function_type_list (opaque_V4SI_type_node,
				opaque_V4SI_type_node, integer_type_node, NULL_TREE);
  tree opaque_ftype_opaque_opaque_int
    = build_function_type_list (opaque_V4SI_type_node,
				opaque_V4SI_type_node, opaque_V4SI_type_node,
				integer_type_node, NULL_TREE);
  tree int_ftype_int_opaque_opaque
    = build_function_type_list (integer_type_node,
                                integer_type_node, opaque_V4SI_type_node,
                                opaque_V4SI_type_node, NULL_TREE);
  tree int_ftype_int_v4si_v4si
    = build_function_type_list (integer_type_node,
				integer_type_node, V4SI_type_node,
				V4SI_type_node, NULL_TREE);
  tree int_ftype_int_v2di_v2di
    = build_function_type_list (integer_type_node,
				integer_type_node, V2DI_type_node,
				V2DI_type_node, NULL_TREE);
  tree void_ftype_v4si
    = build_function_type_list (void_type_node, V4SI_type_node, NULL_TREE);
  tree v8hi_ftype_void
    = build_function_type_list (V8HI_type_node, NULL_TREE);
  tree void_ftype_void
    = build_function_type_list (void_type_node, NULL_TREE);
  tree void_ftype_int
    = build_function_type_list (void_type_node, integer_type_node, NULL_TREE);

  tree opaque_ftype_long_pcvoid
    = build_function_type_list (opaque_V4SI_type_node,
				long_integer_type_node, pcvoid_type_node,
				NULL_TREE);
  tree v16qi_ftype_long_pcvoid
    = build_function_type_list (V16QI_type_node,
				long_integer_type_node, pcvoid_type_node,
				NULL_TREE);
  tree v8hi_ftype_long_pcvoid
    = build_function_type_list (V8HI_type_node,
				long_integer_type_node, pcvoid_type_node,
				NULL_TREE);
  tree v4si_ftype_long_pcvoid
    = build_function_type_list (V4SI_type_node,
				long_integer_type_node, pcvoid_type_node,
				NULL_TREE);
  tree v4sf_ftype_long_pcvoid
    = build_function_type_list (V4SF_type_node,
				long_integer_type_node, pcvoid_type_node,
				NULL_TREE);
  tree v2df_ftype_long_pcvoid
    = build_function_type_list (V2DF_type_node,
				long_integer_type_node, pcvoid_type_node,
				NULL_TREE);
  tree v2di_ftype_long_pcvoid
    = build_function_type_list (V2DI_type_node,
				long_integer_type_node, pcvoid_type_node,
				NULL_TREE);

  tree void_ftype_opaque_long_pvoid
    = build_function_type_list (void_type_node,
				opaque_V4SI_type_node, long_integer_type_node,
				pvoid_type_node, NULL_TREE);
  tree void_ftype_v4si_long_pvoid
    = build_function_type_list (void_type_node,
				V4SI_type_node, long_integer_type_node,
				pvoid_type_node, NULL_TREE);
  tree void_ftype_v16qi_long_pvoid
    = build_function_type_list (void_type_node,
				V16QI_type_node, long_integer_type_node,
				pvoid_type_node, NULL_TREE);
  tree void_ftype_v8hi_long_pvoid
    = build_function_type_list (void_type_node,
				V8HI_type_node, long_integer_type_node,
				pvoid_type_node, NULL_TREE);
  tree void_ftype_v4sf_long_pvoid
    = build_function_type_list (void_type_node,
				V4SF_type_node, long_integer_type_node,
				pvoid_type_node, NULL_TREE);
  tree void_ftype_v2df_long_pvoid
    = build_function_type_list (void_type_node,
				V2DF_type_node, long_integer_type_node,
				pvoid_type_node, NULL_TREE);
  tree void_ftype_v2di_long_pvoid
    = build_function_type_list (void_type_node,
				V2DI_type_node, long_integer_type_node,
				pvoid_type_node, NULL_TREE);
  tree int_ftype_int_v8hi_v8hi
    = build_function_type_list (integer_type_node,
				integer_type_node, V8HI_type_node,
				V8HI_type_node, NULL_TREE);
  tree int_ftype_int_v16qi_v16qi
    = build_function_type_list (integer_type_node,
				integer_type_node, V16QI_type_node,
				V16QI_type_node, NULL_TREE);
  tree int_ftype_int_v4sf_v4sf
    = build_function_type_list (integer_type_node,
				integer_type_node, V4SF_type_node,
				V4SF_type_node, NULL_TREE);
  tree int_ftype_int_v2df_v2df
    = build_function_type_list (integer_type_node,
				integer_type_node, V2DF_type_node,
				V2DF_type_node, NULL_TREE);
  tree v2di_ftype_v2di
    = build_function_type_list (V2DI_type_node, V2DI_type_node, NULL_TREE);
  tree v4si_ftype_v4si
    = build_function_type_list (V4SI_type_node, V4SI_type_node, NULL_TREE);
  tree v8hi_ftype_v8hi
    = build_function_type_list (V8HI_type_node, V8HI_type_node, NULL_TREE);
  tree v16qi_ftype_v16qi
    = build_function_type_list (V16QI_type_node, V16QI_type_node, NULL_TREE);
  tree v4sf_ftype_v4sf
    = build_function_type_list (V4SF_type_node, V4SF_type_node, NULL_TREE);
  tree v2df_ftype_v2df
    = build_function_type_list (V2DF_type_node, V2DF_type_node, NULL_TREE);
  tree void_ftype_pcvoid_int_int
    = build_function_type_list (void_type_node,
				pcvoid_type_node, integer_type_node,
				integer_type_node, NULL_TREE);

  def_builtin ("__builtin_altivec_mtvscr", void_ftype_v4si, ALTIVEC_BUILTIN_MTVSCR);
  def_builtin ("__builtin_altivec_mfvscr", v8hi_ftype_void, ALTIVEC_BUILTIN_MFVSCR);
  def_builtin ("__builtin_altivec_dssall", void_ftype_void, ALTIVEC_BUILTIN_DSSALL);
  def_builtin ("__builtin_altivec_dss", void_ftype_int, ALTIVEC_BUILTIN_DSS);
  def_builtin ("__builtin_altivec_lvsl", v16qi_ftype_long_pcvoid, ALTIVEC_BUILTIN_LVSL);
  def_builtin ("__builtin_altivec_lvsr", v16qi_ftype_long_pcvoid, ALTIVEC_BUILTIN_LVSR);
  def_builtin ("__builtin_altivec_lvebx", v16qi_ftype_long_pcvoid, ALTIVEC_BUILTIN_LVEBX);
  def_builtin ("__builtin_altivec_lvehx", v8hi_ftype_long_pcvoid, ALTIVEC_BUILTIN_LVEHX);
  def_builtin ("__builtin_altivec_lvewx", v4si_ftype_long_pcvoid, ALTIVEC_BUILTIN_LVEWX);
  def_builtin ("__builtin_altivec_lvxl", v4si_ftype_long_pcvoid, ALTIVEC_BUILTIN_LVXL);
  def_builtin ("__builtin_altivec_lvxl_v2df", v2df_ftype_long_pcvoid,
	       ALTIVEC_BUILTIN_LVXL_V2DF);
  def_builtin ("__builtin_altivec_lvxl_v2di", v2di_ftype_long_pcvoid,
	       ALTIVEC_BUILTIN_LVXL_V2DI);
  def_builtin ("__builtin_altivec_lvxl_v4sf", v4sf_ftype_long_pcvoid,
	       ALTIVEC_BUILTIN_LVXL_V4SF);
  def_builtin ("__builtin_altivec_lvxl_v4si", v4si_ftype_long_pcvoid,
	       ALTIVEC_BUILTIN_LVXL_V4SI);
  def_builtin ("__builtin_altivec_lvxl_v8hi", v8hi_ftype_long_pcvoid,
	       ALTIVEC_BUILTIN_LVXL_V8HI);
  def_builtin ("__builtin_altivec_lvxl_v16qi", v16qi_ftype_long_pcvoid,
	       ALTIVEC_BUILTIN_LVXL_V16QI);
  def_builtin ("__builtin_altivec_lvx", v4si_ftype_long_pcvoid, ALTIVEC_BUILTIN_LVX);
  def_builtin ("__builtin_altivec_lvx_v2df", v2df_ftype_long_pcvoid,
	       ALTIVEC_BUILTIN_LVX_V2DF);
  def_builtin ("__builtin_altivec_lvx_v2di", v2di_ftype_long_pcvoid,
	       ALTIVEC_BUILTIN_LVX_V2DI);
  def_builtin ("__builtin_altivec_lvx_v4sf", v4sf_ftype_long_pcvoid,
	       ALTIVEC_BUILTIN_LVX_V4SF);
  def_builtin ("__builtin_altivec_lvx_v4si", v4si_ftype_long_pcvoid,
	       ALTIVEC_BUILTIN_LVX_V4SI);
  def_builtin ("__builtin_altivec_lvx_v8hi", v8hi_ftype_long_pcvoid,
	       ALTIVEC_BUILTIN_LVX_V8HI);
  def_builtin ("__builtin_altivec_lvx_v16qi", v16qi_ftype_long_pcvoid,
	       ALTIVEC_BUILTIN_LVX_V16QI);
  def_builtin ("__builtin_altivec_stvx", void_ftype_v4si_long_pvoid, ALTIVEC_BUILTIN_STVX);
  def_builtin ("__builtin_altivec_stvx_v2df", void_ftype_v2df_long_pvoid,
	       ALTIVEC_BUILTIN_STVX_V2DF);
  def_builtin ("__builtin_altivec_stvx_v2di", void_ftype_v2di_long_pvoid,
	       ALTIVEC_BUILTIN_STVX_V2DI);
  def_builtin ("__builtin_altivec_stvx_v4sf", void_ftype_v4sf_long_pvoid,
	       ALTIVEC_BUILTIN_STVX_V4SF);
  def_builtin ("__builtin_altivec_stvx_v4si", void_ftype_v4si_long_pvoid,
	       ALTIVEC_BUILTIN_STVX_V4SI);
  def_builtin ("__builtin_altivec_stvx_v8hi", void_ftype_v8hi_long_pvoid,
	       ALTIVEC_BUILTIN_STVX_V8HI);
  def_builtin ("__builtin_altivec_stvx_v16qi", void_ftype_v16qi_long_pvoid,
	       ALTIVEC_BUILTIN_STVX_V16QI);
  def_builtin ("__builtin_altivec_stvewx", void_ftype_v4si_long_pvoid, ALTIVEC_BUILTIN_STVEWX);
  def_builtin ("__builtin_altivec_stvxl", void_ftype_v4si_long_pvoid, ALTIVEC_BUILTIN_STVXL);
  def_builtin ("__builtin_altivec_stvxl_v2df", void_ftype_v2df_long_pvoid,
	       ALTIVEC_BUILTIN_STVXL_V2DF);
  def_builtin ("__builtin_altivec_stvxl_v2di", void_ftype_v2di_long_pvoid,
	       ALTIVEC_BUILTIN_STVXL_V2DI);
  def_builtin ("__builtin_altivec_stvxl_v4sf", void_ftype_v4sf_long_pvoid,
	       ALTIVEC_BUILTIN_STVXL_V4SF);
  def_builtin ("__builtin_altivec_stvxl_v4si", void_ftype_v4si_long_pvoid,
	       ALTIVEC_BUILTIN_STVXL_V4SI);
  def_builtin ("__builtin_altivec_stvxl_v8hi", void_ftype_v8hi_long_pvoid,
	       ALTIVEC_BUILTIN_STVXL_V8HI);
  def_builtin ("__builtin_altivec_stvxl_v16qi", void_ftype_v16qi_long_pvoid,
	       ALTIVEC_BUILTIN_STVXL_V16QI);
  def_builtin ("__builtin_altivec_stvebx", void_ftype_v16qi_long_pvoid, ALTIVEC_BUILTIN_STVEBX);
  def_builtin ("__builtin_altivec_stvehx", void_ftype_v8hi_long_pvoid, ALTIVEC_BUILTIN_STVEHX);
  def_builtin ("__builtin_vec_ld", opaque_ftype_long_pcvoid, ALTIVEC_BUILTIN_VEC_LD);
  def_builtin ("__builtin_vec_lde", opaque_ftype_long_pcvoid, ALTIVEC_BUILTIN_VEC_LDE);
  def_builtin ("__builtin_vec_ldl", opaque_ftype_long_pcvoid, ALTIVEC_BUILTIN_VEC_LDL);
  def_builtin ("__builtin_vec_lvsl", v16qi_ftype_long_pcvoid, ALTIVEC_BUILTIN_VEC_LVSL);
  def_builtin ("__builtin_vec_lvsr", v16qi_ftype_long_pcvoid, ALTIVEC_BUILTIN_VEC_LVSR);
  def_builtin ("__builtin_vec_lvebx", v16qi_ftype_long_pcvoid, ALTIVEC_BUILTIN_VEC_LVEBX);
  def_builtin ("__builtin_vec_lvehx", v8hi_ftype_long_pcvoid, ALTIVEC_BUILTIN_VEC_LVEHX);
  def_builtin ("__builtin_vec_lvewx", v4si_ftype_long_pcvoid, ALTIVEC_BUILTIN_VEC_LVEWX);
  def_builtin ("__builtin_vec_st", void_ftype_opaque_long_pvoid, ALTIVEC_BUILTIN_VEC_ST);
  def_builtin ("__builtin_vec_ste", void_ftype_opaque_long_pvoid, ALTIVEC_BUILTIN_VEC_STE);
  def_builtin ("__builtin_vec_stl", void_ftype_opaque_long_pvoid, ALTIVEC_BUILTIN_VEC_STL);
  def_builtin ("__builtin_vec_stvewx", void_ftype_opaque_long_pvoid, ALTIVEC_BUILTIN_VEC_STVEWX);
  def_builtin ("__builtin_vec_stvebx", void_ftype_opaque_long_pvoid, ALTIVEC_BUILTIN_VEC_STVEBX);
  def_builtin ("__builtin_vec_stvehx", void_ftype_opaque_long_pvoid, ALTIVEC_BUILTIN_VEC_STVEHX);

  def_builtin ("__builtin_vsx_lxvd2x_v2df", v2df_ftype_long_pcvoid,
	       VSX_BUILTIN_LXVD2X_V2DF);
  def_builtin ("__builtin_vsx_lxvd2x_v2di", v2di_ftype_long_pcvoid,
	       VSX_BUILTIN_LXVD2X_V2DI);
  def_builtin ("__builtin_vsx_lxvw4x_v4sf", v4sf_ftype_long_pcvoid,
	       VSX_BUILTIN_LXVW4X_V4SF);
  def_builtin ("__builtin_vsx_lxvw4x_v4si", v4si_ftype_long_pcvoid,
	       VSX_BUILTIN_LXVW4X_V4SI);
  def_builtin ("__builtin_vsx_lxvw4x_v8hi", v8hi_ftype_long_pcvoid,
	       VSX_BUILTIN_LXVW4X_V8HI);
  def_builtin ("__builtin_vsx_lxvw4x_v16qi", v16qi_ftype_long_pcvoid,
	       VSX_BUILTIN_LXVW4X_V16QI);
  def_builtin ("__builtin_vsx_stxvd2x_v2df", void_ftype_v2df_long_pvoid,
	       VSX_BUILTIN_STXVD2X_V2DF);
  def_builtin ("__builtin_vsx_stxvd2x_v2di", void_ftype_v2di_long_pvoid,
	       VSX_BUILTIN_STXVD2X_V2DI);
  def_builtin ("__builtin_vsx_stxvw4x_v4sf", void_ftype_v4sf_long_pvoid,
	       VSX_BUILTIN_STXVW4X_V4SF);
  def_builtin ("__builtin_vsx_stxvw4x_v4si", void_ftype_v4si_long_pvoid,
	       VSX_BUILTIN_STXVW4X_V4SI);
  def_builtin ("__builtin_vsx_stxvw4x_v8hi", void_ftype_v8hi_long_pvoid,
	       VSX_BUILTIN_STXVW4X_V8HI);
  def_builtin ("__builtin_vsx_stxvw4x_v16qi", void_ftype_v16qi_long_pvoid,
	       VSX_BUILTIN_STXVW4X_V16QI);
  def_builtin ("__builtin_vec_vsx_ld", opaque_ftype_long_pcvoid,
	       VSX_BUILTIN_VEC_LD);
  def_builtin ("__builtin_vec_vsx_st", void_ftype_opaque_long_pvoid,
	       VSX_BUILTIN_VEC_ST);

  def_builtin ("__builtin_vec_step", int_ftype_opaque, ALTIVEC_BUILTIN_VEC_STEP);
  def_builtin ("__builtin_vec_splats", opaque_ftype_opaque, ALTIVEC_BUILTIN_VEC_SPLATS);
  def_builtin ("__builtin_vec_promote", opaque_ftype_opaque, ALTIVEC_BUILTIN_VEC_PROMOTE);

  def_builtin ("__builtin_vec_sld", opaque_ftype_opaque_opaque_int, ALTIVEC_BUILTIN_VEC_SLD);
  def_builtin ("__builtin_vec_splat", opaque_ftype_opaque_int, ALTIVEC_BUILTIN_VEC_SPLAT);
  def_builtin ("__builtin_vec_extract", opaque_ftype_opaque_int, ALTIVEC_BUILTIN_VEC_EXTRACT);
  def_builtin ("__builtin_vec_insert", opaque_ftype_opaque_opaque_int, ALTIVEC_BUILTIN_VEC_INSERT);
  def_builtin ("__builtin_vec_vspltw", opaque_ftype_opaque_int, ALTIVEC_BUILTIN_VEC_VSPLTW);
  def_builtin ("__builtin_vec_vsplth", opaque_ftype_opaque_int, ALTIVEC_BUILTIN_VEC_VSPLTH);
  def_builtin ("__builtin_vec_vspltb", opaque_ftype_opaque_int, ALTIVEC_BUILTIN_VEC_VSPLTB);
  def_builtin ("__builtin_vec_ctf", opaque_ftype_opaque_int, ALTIVEC_BUILTIN_VEC_CTF);
  def_builtin ("__builtin_vec_vcfsx", opaque_ftype_opaque_int, ALTIVEC_BUILTIN_VEC_VCFSX);
  def_builtin ("__builtin_vec_vcfux", opaque_ftype_opaque_int, ALTIVEC_BUILTIN_VEC_VCFUX);
  def_builtin ("__builtin_vec_cts", opaque_ftype_opaque_int, ALTIVEC_BUILTIN_VEC_CTS);
  def_builtin ("__builtin_vec_ctu", opaque_ftype_opaque_int, ALTIVEC_BUILTIN_VEC_CTU);

  /* Cell builtins.  */
  def_builtin ("__builtin_altivec_lvlx",  v16qi_ftype_long_pcvoid, ALTIVEC_BUILTIN_LVLX);
  def_builtin ("__builtin_altivec_lvlxl", v16qi_ftype_long_pcvoid, ALTIVEC_BUILTIN_LVLXL);
  def_builtin ("__builtin_altivec_lvrx",  v16qi_ftype_long_pcvoid, ALTIVEC_BUILTIN_LVRX);
  def_builtin ("__builtin_altivec_lvrxl", v16qi_ftype_long_pcvoid, ALTIVEC_BUILTIN_LVRXL);

  def_builtin ("__builtin_vec_lvlx",  v16qi_ftype_long_pcvoid, ALTIVEC_BUILTIN_VEC_LVLX);
  def_builtin ("__builtin_vec_lvlxl", v16qi_ftype_long_pcvoid, ALTIVEC_BUILTIN_VEC_LVLXL);
  def_builtin ("__builtin_vec_lvrx",  v16qi_ftype_long_pcvoid, ALTIVEC_BUILTIN_VEC_LVRX);
  def_builtin ("__builtin_vec_lvrxl", v16qi_ftype_long_pcvoid, ALTIVEC_BUILTIN_VEC_LVRXL);

  def_builtin ("__builtin_altivec_stvlx",  void_ftype_v16qi_long_pvoid, ALTIVEC_BUILTIN_STVLX);
  def_builtin ("__builtin_altivec_stvlxl", void_ftype_v16qi_long_pvoid, ALTIVEC_BUILTIN_STVLXL);
  def_builtin ("__builtin_altivec_stvrx",  void_ftype_v16qi_long_pvoid, ALTIVEC_BUILTIN_STVRX);
  def_builtin ("__builtin_altivec_stvrxl", void_ftype_v16qi_long_pvoid, ALTIVEC_BUILTIN_STVRXL);

  def_builtin ("__builtin_vec_stvlx",  void_ftype_v16qi_long_pvoid, ALTIVEC_BUILTIN_VEC_STVLX);
  def_builtin ("__builtin_vec_stvlxl", void_ftype_v16qi_long_pvoid, ALTIVEC_BUILTIN_VEC_STVLXL);
  def_builtin ("__builtin_vec_stvrx",  void_ftype_v16qi_long_pvoid, ALTIVEC_BUILTIN_VEC_STVRX);
  def_builtin ("__builtin_vec_stvrxl", void_ftype_v16qi_long_pvoid, ALTIVEC_BUILTIN_VEC_STVRXL);

  /* Add the DST variants.  */
  d = bdesc_dst;
  for (i = 0; i < ARRAY_SIZE (bdesc_dst); i++, d++)
    def_builtin (d->name, void_ftype_pcvoid_int_int, d->code);

  /* Initialize the predicates.  */
  d = bdesc_altivec_preds;
  for (i = 0; i < ARRAY_SIZE (bdesc_altivec_preds); i++, d++)
    {
      enum machine_mode mode1;
      tree type;

      if (rs6000_overloaded_builtin_p (d->code))
	mode1 = VOIDmode;
      else
	mode1 = insn_data[d->icode].operand[1].mode;

      switch (mode1)
	{
	case VOIDmode:
	  type = int_ftype_int_opaque_opaque;
	  break;
	case V2DImode:
	  type = int_ftype_int_v2di_v2di;
	  break;
	case V4SImode:
	  type = int_ftype_int_v4si_v4si;
	  break;
	case V8HImode:
	  type = int_ftype_int_v8hi_v8hi;
	  break;
	case V16QImode:
	  type = int_ftype_int_v16qi_v16qi;
	  break;
	case V4SFmode:
	  type = int_ftype_int_v4sf_v4sf;
	  break;
	case V2DFmode:
	  type = int_ftype_int_v2df_v2df;
	  break;
	default:
	  gcc_unreachable ();
	}

      def_builtin (d->name, type, d->code);
    }

  /* Initialize the abs* operators.  */
  d = bdesc_abs;
  for (i = 0; i < ARRAY_SIZE (bdesc_abs); i++, d++)
    {
      enum machine_mode mode0;
      tree type;

      mode0 = insn_data[d->icode].operand[0].mode;

      switch (mode0)
	{
	case V2DImode:
	  type = v2di_ftype_v2di;
	  break;
	case V4SImode:
	  type = v4si_ftype_v4si;
	  break;
	case V8HImode:
	  type = v8hi_ftype_v8hi;
	  break;
	case V16QImode:
	  type = v16qi_ftype_v16qi;
	  break;
	case V4SFmode:
	  type = v4sf_ftype_v4sf;
	  break;
	case V2DFmode:
	  type = v2df_ftype_v2df;
	  break;
	default:
	  gcc_unreachable ();
	}

      def_builtin (d->name, type, d->code);
    }

  /* Initialize target builtin that implements
     targetm.vectorize.builtin_mask_for_load.  */

  decl = add_builtin_function ("__builtin_altivec_mask_for_load",
			       v16qi_ftype_long_pcvoid,
			       ALTIVEC_BUILTIN_MASK_FOR_LOAD,
			       BUILT_IN_MD, NULL, NULL_TREE);
  TREE_READONLY (decl) = 1;
  /* Record the decl. Will be used by rs6000_builtin_mask_for_load.  */
  altivec_builtin_mask_for_load = decl;

  /* Access to the vec_init patterns.  */
  ftype = build_function_type_list (V4SI_type_node, integer_type_node,
				    integer_type_node, integer_type_node,
				    integer_type_node, NULL_TREE);
  def_builtin ("__builtin_vec_init_v4si", ftype, ALTIVEC_BUILTIN_VEC_INIT_V4SI);

  ftype = build_function_type_list (V8HI_type_node, short_integer_type_node,
				    short_integer_type_node,
				    short_integer_type_node,
				    short_integer_type_node,
				    short_integer_type_node,
				    short_integer_type_node,
				    short_integer_type_node,
				    short_integer_type_node, NULL_TREE);
  def_builtin ("__builtin_vec_init_v8hi", ftype, ALTIVEC_BUILTIN_VEC_INIT_V8HI);

  ftype = build_function_type_list (V16QI_type_node, char_type_node,
				    char_type_node, char_type_node,
				    char_type_node, char_type_node,
				    char_type_node, char_type_node,
				    char_type_node, char_type_node,
				    char_type_node, char_type_node,
				    char_type_node, char_type_node,
				    char_type_node, char_type_node,
				    char_type_node, NULL_TREE);
  def_builtin ("__builtin_vec_init_v16qi", ftype,
	       ALTIVEC_BUILTIN_VEC_INIT_V16QI);

  ftype = build_function_type_list (V4SF_type_node, float_type_node,
				    float_type_node, float_type_node,
				    float_type_node, NULL_TREE);
  def_builtin ("__builtin_vec_init_v4sf", ftype, ALTIVEC_BUILTIN_VEC_INIT_V4SF);

  /* VSX builtins.  */
  ftype = build_function_type_list (V2DF_type_node, double_type_node,
				    double_type_node, NULL_TREE);
  def_builtin ("__builtin_vec_init_v2df", ftype, VSX_BUILTIN_VEC_INIT_V2DF);

  ftype = build_function_type_list (V2DI_type_node, intDI_type_node,
				    intDI_type_node, NULL_TREE);
  def_builtin ("__builtin_vec_init_v2di", ftype, VSX_BUILTIN_VEC_INIT_V2DI);

  /* Access to the vec_set patterns.  */
  ftype = build_function_type_list (V4SI_type_node, V4SI_type_node,
				    intSI_type_node,
				    integer_type_node, NULL_TREE);
  def_builtin ("__builtin_vec_set_v4si", ftype, ALTIVEC_BUILTIN_VEC_SET_V4SI);

  ftype = build_function_type_list (V8HI_type_node, V8HI_type_node,
				    intHI_type_node,
				    integer_type_node, NULL_TREE);
  def_builtin ("__builtin_vec_set_v8hi", ftype, ALTIVEC_BUILTIN_VEC_SET_V8HI);

  ftype = build_function_type_list (V16QI_type_node, V16QI_type_node,
				    intQI_type_node,
				    integer_type_node, NULL_TREE);
  def_builtin ("__builtin_vec_set_v16qi", ftype, ALTIVEC_BUILTIN_VEC_SET_V16QI);

  ftype = build_function_type_list (V4SF_type_node, V4SF_type_node,
				    float_type_node,
				    integer_type_node, NULL_TREE);
  def_builtin ("__builtin_vec_set_v4sf", ftype, ALTIVEC_BUILTIN_VEC_SET_V4SF);

  ftype = build_function_type_list (V2DF_type_node, V2DF_type_node,
				    double_type_node,
				    integer_type_node, NULL_TREE);
  def_builtin ("__builtin_vec_set_v2df", ftype, VSX_BUILTIN_VEC_SET_V2DF);

  ftype = build_function_type_list (V2DI_type_node, V2DI_type_node,
				    intDI_type_node,
				    integer_type_node, NULL_TREE);
  def_builtin ("__builtin_vec_set_v2di", ftype, VSX_BUILTIN_VEC_SET_V2DI);

  /* Access to the vec_extract patterns.  */
  ftype = build_function_type_list (intSI_type_node, V4SI_type_node,
				    integer_type_node, NULL_TREE);
  def_builtin ("__builtin_vec_ext_v4si", ftype, ALTIVEC_BUILTIN_VEC_EXT_V4SI);

  ftype = build_function_type_list (intHI_type_node, V8HI_type_node,
				    integer_type_node, NULL_TREE);
  def_builtin ("__builtin_vec_ext_v8hi", ftype, ALTIVEC_BUILTIN_VEC_EXT_V8HI);

  ftype = build_function_type_list (intQI_type_node, V16QI_type_node,
				    integer_type_node, NULL_TREE);
  def_builtin ("__builtin_vec_ext_v16qi", ftype, ALTIVEC_BUILTIN_VEC_EXT_V16QI);

  ftype = build_function_type_list (float_type_node, V4SF_type_node,
				    integer_type_node, NULL_TREE);
  def_builtin ("__builtin_vec_ext_v4sf", ftype, ALTIVEC_BUILTIN_VEC_EXT_V4SF);

  ftype = build_function_type_list (double_type_node, V2DF_type_node,
				    integer_type_node, NULL_TREE);
  def_builtin ("__builtin_vec_ext_v2df", ftype, VSX_BUILTIN_VEC_EXT_V2DF);

  ftype = build_function_type_list (intDI_type_node, V2DI_type_node,
				    integer_type_node, NULL_TREE);
  def_builtin ("__builtin_vec_ext_v2di", ftype, VSX_BUILTIN_VEC_EXT_V2DI);
}

static void
htm_init_builtins (void)
{
  HOST_WIDE_INT builtin_mask = rs6000_builtin_mask;
  const struct builtin_description *d;
  size_t i;

  d = bdesc_htm;
  for (i = 0; i < ARRAY_SIZE (bdesc_htm); i++, d++)
    {
      tree op[MAX_HTM_OPERANDS], type;
      HOST_WIDE_INT mask = d->mask;
      unsigned attr = rs6000_builtin_info[d->code].attr;
      bool void_func = (attr & RS6000_BTC_VOID);
      int attr_args = (attr & RS6000_BTC_TYPE_MASK);
      int nopnds = 0;
      tree argtype = (attr & RS6000_BTC_SPR) ? long_unsigned_type_node
					     : unsigned_type_node;

      if ((mask & builtin_mask) != mask)
	{
	  if (TARGET_DEBUG_BUILTIN)
	    fprintf (stderr, "htm_builtin, skip binary %s\n", d->name);
	  continue;
	}

      if (d->name == 0)
	{
	  if (TARGET_DEBUG_BUILTIN)
	    fprintf (stderr, "htm_builtin, bdesc_htm[%ld] no name\n",
		     (long unsigned) i);
	  continue;
	}

      op[nopnds++] = (void_func) ? void_type_node : argtype;

      if (attr_args == RS6000_BTC_UNARY)
	op[nopnds++] = argtype;
      else if (attr_args == RS6000_BTC_BINARY)
	{
	  op[nopnds++] = argtype;
	  op[nopnds++] = argtype;
	}
      else if (attr_args == RS6000_BTC_TERNARY)
	{
	  op[nopnds++] = argtype;
	  op[nopnds++] = argtype;
	  op[nopnds++] = argtype;
	}

      switch (nopnds)
	{
	case 1:
	  type = build_function_type_list (op[0], NULL_TREE);
	  break;
	case 2:
	  type = build_function_type_list (op[0], op[1], NULL_TREE);
	  break;
	case 3:
	  type = build_function_type_list (op[0], op[1], op[2], NULL_TREE);
	  break;
	case 4:
	  type = build_function_type_list (op[0], op[1], op[2], op[3],
					   NULL_TREE);
	  break;
	default:
	  gcc_unreachable ();
	}

      def_builtin (d->name, type, d->code);
    }
}

/* Hash function for builtin functions with up to 3 arguments and a return
   type.  */
static unsigned
builtin_hash_function (const void *hash_entry)
{
  unsigned ret = 0;
  int i;
  const struct builtin_hash_struct *bh =
    (const struct builtin_hash_struct *) hash_entry;

  for (i = 0; i < 4; i++)
    {
      ret = (ret * (unsigned)MAX_MACHINE_MODE) + ((unsigned)bh->mode[i]);
      ret = (ret * 2) + bh->uns_p[i];
    }

  return ret;
}

/* Compare builtin hash entries H1 and H2 for equivalence.  */
static int
builtin_hash_eq (const void *h1, const void *h2)
{
  const struct builtin_hash_struct *p1 = (const struct builtin_hash_struct *) h1;
  const struct builtin_hash_struct *p2 = (const struct builtin_hash_struct *) h2;

  return ((p1->mode[0] == p2->mode[0])
	  && (p1->mode[1] == p2->mode[1])
	  && (p1->mode[2] == p2->mode[2])
	  && (p1->mode[3] == p2->mode[3])
	  && (p1->uns_p[0] == p2->uns_p[0])
	  && (p1->uns_p[1] == p2->uns_p[1])
	  && (p1->uns_p[2] == p2->uns_p[2])
	  && (p1->uns_p[3] == p2->uns_p[3]));
}

/* Map types for builtin functions with an explicit return type and up to 3
   arguments.  Functions with fewer than 3 arguments use VOIDmode as the type
   of the argument.  */
static tree
builtin_function_type (enum machine_mode mode_ret, enum machine_mode mode_arg0,
		       enum machine_mode mode_arg1, enum machine_mode mode_arg2,
		       enum rs6000_builtins builtin, const char *name)
{
  struct builtin_hash_struct h;
  struct builtin_hash_struct *h2;
  void **found;
  int num_args = 3;
  int i;
  tree ret_type = NULL_TREE;
  tree arg_type[3] = { NULL_TREE, NULL_TREE, NULL_TREE };

  /* Create builtin_hash_table.  */
  if (builtin_hash_table == NULL)
    builtin_hash_table = htab_create_ggc (1500, builtin_hash_function,
					  builtin_hash_eq, NULL);

  h.type = NULL_TREE;
  h.mode[0] = mode_ret;
  h.mode[1] = mode_arg0;
  h.mode[2] = mode_arg1;
  h.mode[3] = mode_arg2;
  h.uns_p[0] = 0;
  h.uns_p[1] = 0;
  h.uns_p[2] = 0;
  h.uns_p[3] = 0;

  /* If the builtin is a type that produces unsigned results or takes unsigned
     arguments, and it is returned as a decl for the vectorizer (such as
     widening multiplies, permute), make sure the arguments and return value
     are type correct.  */
  switch (builtin)
    {
      /* unsigned 1 argument functions.  */
    case CRYPTO_BUILTIN_VSBOX:
    case P8V_BUILTIN_VGBBD:
      h.uns_p[0] = 1;
      h.uns_p[1] = 1;
      break;

      /* unsigned 2 argument functions.  */
    case ALTIVEC_BUILTIN_VMULEUB_UNS:
    case ALTIVEC_BUILTIN_VMULEUH_UNS:
    case ALTIVEC_BUILTIN_VMULOUB_UNS:
    case ALTIVEC_BUILTIN_VMULOUH_UNS:
    case CRYPTO_BUILTIN_VCIPHER:
    case CRYPTO_BUILTIN_VCIPHERLAST:
    case CRYPTO_BUILTIN_VNCIPHER:
    case CRYPTO_BUILTIN_VNCIPHERLAST:
    case CRYPTO_BUILTIN_VPMSUMB:
    case CRYPTO_BUILTIN_VPMSUMH:
    case CRYPTO_BUILTIN_VPMSUMW:
    case CRYPTO_BUILTIN_VPMSUMD:
    case CRYPTO_BUILTIN_VPMSUM:
      h.uns_p[0] = 1;
      h.uns_p[1] = 1;
      h.uns_p[2] = 1;
      break;

      /* unsigned 3 argument functions.  */
    case ALTIVEC_BUILTIN_VPERM_16QI_UNS:
    case ALTIVEC_BUILTIN_VPERM_8HI_UNS:
    case ALTIVEC_BUILTIN_VPERM_4SI_UNS:
    case ALTIVEC_BUILTIN_VPERM_2DI_UNS:
    case ALTIVEC_BUILTIN_VSEL_16QI_UNS:
    case ALTIVEC_BUILTIN_VSEL_8HI_UNS:
    case ALTIVEC_BUILTIN_VSEL_4SI_UNS:
    case ALTIVEC_BUILTIN_VSEL_2DI_UNS:
    case VSX_BUILTIN_VPERM_16QI_UNS:
    case VSX_BUILTIN_VPERM_8HI_UNS:
    case VSX_BUILTIN_VPERM_4SI_UNS:
    case VSX_BUILTIN_VPERM_2DI_UNS:
    case VSX_BUILTIN_XXSEL_16QI_UNS:
    case VSX_BUILTIN_XXSEL_8HI_UNS:
    case VSX_BUILTIN_XXSEL_4SI_UNS:
    case VSX_BUILTIN_XXSEL_2DI_UNS:
    case CRYPTO_BUILTIN_VPERMXOR:
    case CRYPTO_BUILTIN_VPERMXOR_V2DI:
    case CRYPTO_BUILTIN_VPERMXOR_V4SI:
    case CRYPTO_BUILTIN_VPERMXOR_V8HI:
    case CRYPTO_BUILTIN_VPERMXOR_V16QI:
    case CRYPTO_BUILTIN_VSHASIGMAW:
    case CRYPTO_BUILTIN_VSHASIGMAD:
    case CRYPTO_BUILTIN_VSHASIGMA:
      h.uns_p[0] = 1;
      h.uns_p[1] = 1;
      h.uns_p[2] = 1;
      h.uns_p[3] = 1;
      break;

      /* signed permute functions with unsigned char mask.  */
    case ALTIVEC_BUILTIN_VPERM_16QI:
    case ALTIVEC_BUILTIN_VPERM_8HI:
    case ALTIVEC_BUILTIN_VPERM_4SI:
    case ALTIVEC_BUILTIN_VPERM_4SF:
    case ALTIVEC_BUILTIN_VPERM_2DI:
    case ALTIVEC_BUILTIN_VPERM_2DF:
    case VSX_BUILTIN_VPERM_16QI:
    case VSX_BUILTIN_VPERM_8HI:
    case VSX_BUILTIN_VPERM_4SI:
    case VSX_BUILTIN_VPERM_4SF:
    case VSX_BUILTIN_VPERM_2DI:
    case VSX_BUILTIN_VPERM_2DF:
      h.uns_p[3] = 1;
      break;

      /* unsigned args, signed return.  */
    case VSX_BUILTIN_XVCVUXDDP_UNS:
    case ALTIVEC_BUILTIN_UNSFLOAT_V4SI_V4SF:
      h.uns_p[1] = 1;
      break;

      /* signed args, unsigned return.  */
    case VSX_BUILTIN_XVCVDPUXDS_UNS:
    case ALTIVEC_BUILTIN_FIXUNS_V4SF_V4SI:
      h.uns_p[0] = 1;
      break;

    default:
      break;
    }

  /* Figure out how many args are present.  */
  while (num_args > 0 && h.mode[num_args] == VOIDmode)
    num_args--;

  if (num_args == 0)
    fatal_error ("internal error: builtin function %s had no type", name);

  ret_type = builtin_mode_to_type[h.mode[0]][h.uns_p[0]];
  if (!ret_type && h.uns_p[0])
    ret_type = builtin_mode_to_type[h.mode[0]][0];

  if (!ret_type)
    fatal_error ("internal error: builtin function %s had an unexpected "
		 "return type %s", name, GET_MODE_NAME (h.mode[0]));

  for (i = 0; i < (int) ARRAY_SIZE (arg_type); i++)
    arg_type[i] = NULL_TREE;

  for (i = 0; i < num_args; i++)
    {
      int m = (int) h.mode[i+1];
      int uns_p = h.uns_p[i+1];

      arg_type[i] = builtin_mode_to_type[m][uns_p];
      if (!arg_type[i] && uns_p)
	arg_type[i] = builtin_mode_to_type[m][0];

      if (!arg_type[i])
	fatal_error ("internal error: builtin function %s, argument %d "
		     "had unexpected argument type %s", name, i,
		     GET_MODE_NAME (m));
    }

  found = htab_find_slot (builtin_hash_table, &h, INSERT);
  if (*found == NULL)
    {
      h2 = ggc_alloc_builtin_hash_struct ();
      *h2 = h;
      *found = (void *)h2;

      h2->type = build_function_type_list (ret_type, arg_type[0], arg_type[1],
					   arg_type[2], NULL_TREE);
    }

  return ((struct builtin_hash_struct *)(*found))->type;
}

static void
rs6000_common_init_builtins (void)
{
  const struct builtin_description *d;
  size_t i;

  tree opaque_ftype_opaque = NULL_TREE;
  tree opaque_ftype_opaque_opaque = NULL_TREE;
  tree opaque_ftype_opaque_opaque_opaque = NULL_TREE;
  tree v2si_ftype_qi = NULL_TREE;
  tree v2si_ftype_v2si_qi = NULL_TREE;
  tree v2si_ftype_int_qi = NULL_TREE;
  HOST_WIDE_INT builtin_mask = rs6000_builtin_mask;

  if (!TARGET_PAIRED_FLOAT)
    {
      builtin_mode_to_type[V2SImode][0] = opaque_V2SI_type_node;
      builtin_mode_to_type[V2SFmode][0] = opaque_V2SF_type_node;
    }

  /* Paired and SPE builtins are only available if you build a compiler with
     the appropriate options, so only create those builtins with the
     appropriate compiler option.  Create Altivec and VSX builtins on machines
     with at least the general purpose extensions (970 and newer) to allow the
     use of the target attribute..  */

  if (TARGET_EXTRA_BUILTINS)
    builtin_mask |= RS6000_BTM_COMMON;

  /* Add the ternary operators.  */
  d = bdesc_3arg;
  for (i = 0; i < ARRAY_SIZE (bdesc_3arg); i++, d++)
    {
      tree type;
      HOST_WIDE_INT mask = d->mask;

      if ((mask & builtin_mask) != mask)
	{
	  if (TARGET_DEBUG_BUILTIN)
	    fprintf (stderr, "rs6000_builtin, skip ternary %s\n", d->name);
	  continue;
	}

      if (rs6000_overloaded_builtin_p (d->code))
	{
	  if (! (type = opaque_ftype_opaque_opaque_opaque))
	    type = opaque_ftype_opaque_opaque_opaque
	      = build_function_type_list (opaque_V4SI_type_node,
					  opaque_V4SI_type_node,
					  opaque_V4SI_type_node,
					  opaque_V4SI_type_node,
					  NULL_TREE);
	}
      else
	{
	  enum insn_code icode = d->icode;
	  if (d->name == 0)
	    {
	      if (TARGET_DEBUG_BUILTIN)
		fprintf (stderr, "rs6000_builtin, bdesc_3arg[%ld] no name\n",
			 (long unsigned)i);

	      continue;
	    }

          if (icode == CODE_FOR_nothing)
	    {
	      if (TARGET_DEBUG_BUILTIN)
		fprintf (stderr, "rs6000_builtin, skip ternary %s (no code)\n",
			 d->name);

	      continue;
	    }

	  type = builtin_function_type (insn_data[icode].operand[0].mode,
					insn_data[icode].operand[1].mode,
					insn_data[icode].operand[2].mode,
					insn_data[icode].operand[3].mode,
					d->code, d->name);
	}

      def_builtin (d->name, type, d->code);
    }

  /* Add the binary operators.  */
  d = bdesc_2arg;
  for (i = 0; i < ARRAY_SIZE (bdesc_2arg); i++, d++)
    {
      enum machine_mode mode0, mode1, mode2;
      tree type;
      HOST_WIDE_INT mask = d->mask;

      if ((mask & builtin_mask) != mask)
	{
	  if (TARGET_DEBUG_BUILTIN)
	    fprintf (stderr, "rs6000_builtin, skip binary %s\n", d->name);
	  continue;
	}

      if (rs6000_overloaded_builtin_p (d->code))
	{
	  if (! (type = opaque_ftype_opaque_opaque))
	    type = opaque_ftype_opaque_opaque
	      = build_function_type_list (opaque_V4SI_type_node,
					  opaque_V4SI_type_node,
					  opaque_V4SI_type_node,
					  NULL_TREE);
	}
      else
	{
	  enum insn_code icode = d->icode;
	  if (d->name == 0)
	    {
	      if (TARGET_DEBUG_BUILTIN)
		fprintf (stderr, "rs6000_builtin, bdesc_2arg[%ld] no name\n",
			 (long unsigned)i);

	      continue;
	    }

          if (icode == CODE_FOR_nothing)
	    {
	      if (TARGET_DEBUG_BUILTIN)
		fprintf (stderr, "rs6000_builtin, skip binary %s (no code)\n",
			 d->name);

	      continue;
	    }

          mode0 = insn_data[icode].operand[0].mode;
          mode1 = insn_data[icode].operand[1].mode;
          mode2 = insn_data[icode].operand[2].mode;

	  if (mode0 == V2SImode && mode1 == V2SImode && mode2 == QImode)
	    {
	      if (! (type = v2si_ftype_v2si_qi))
		type = v2si_ftype_v2si_qi
		  = build_function_type_list (opaque_V2SI_type_node,
					      opaque_V2SI_type_node,
					      char_type_node,
					      NULL_TREE);
	    }

	  else if (mode0 == V2SImode && GET_MODE_CLASS (mode1) == MODE_INT
		   && mode2 == QImode)
	    {
	      if (! (type = v2si_ftype_int_qi))
		type = v2si_ftype_int_qi
		  = build_function_type_list (opaque_V2SI_type_node,
					      integer_type_node,
					      char_type_node,
					      NULL_TREE);
	    }

	  else
	    type = builtin_function_type (mode0, mode1, mode2, VOIDmode,
					  d->code, d->name);
	}

      def_builtin (d->name, type, d->code);
    }

  /* Add the simple unary operators.  */
  d = bdesc_1arg;
  for (i = 0; i < ARRAY_SIZE (bdesc_1arg); i++, d++)
    {
      enum machine_mode mode0, mode1;
      tree type;
      HOST_WIDE_INT mask = d->mask;

      if ((mask & builtin_mask) != mask)
	{
	  if (TARGET_DEBUG_BUILTIN)
	    fprintf (stderr, "rs6000_builtin, skip unary %s\n", d->name);
	  continue;
	}

      if (rs6000_overloaded_builtin_p (d->code))
	{
	  if (! (type = opaque_ftype_opaque))
	    type = opaque_ftype_opaque
	      = build_function_type_list (opaque_V4SI_type_node,
					  opaque_V4SI_type_node,
					  NULL_TREE);
	}
      else
        {
	  enum insn_code icode = d->icode;
	  if (d->name == 0)
	    {
	      if (TARGET_DEBUG_BUILTIN)
		fprintf (stderr, "rs6000_builtin, bdesc_1arg[%ld] no name\n",
			 (long unsigned)i);

	      continue;
	    }

          if (icode == CODE_FOR_nothing)
	    {
	      if (TARGET_DEBUG_BUILTIN)
		fprintf (stderr, "rs6000_builtin, skip unary %s (no code)\n",
			 d->name);

	      continue;
	    }

          mode0 = insn_data[icode].operand[0].mode;
          mode1 = insn_data[icode].operand[1].mode;

	  if (mode0 == V2SImode && mode1 == QImode)
	    {
	      if (! (type = v2si_ftype_qi))
		type = v2si_ftype_qi
		  = build_function_type_list (opaque_V2SI_type_node,
					      char_type_node,
					      NULL_TREE);
	    }

	  else
	    type = builtin_function_type (mode0, mode1, VOIDmode, VOIDmode,
					  d->code, d->name);
	}

      def_builtin (d->name, type, d->code);
    }
}

static void
rs6000_init_libfuncs (void)
{
  if (!TARGET_IEEEQUAD)
      /* AIX/Darwin/64-bit Linux quad floating point routines.  */
    if (!TARGET_XL_COMPAT)
      {
	set_optab_libfunc (add_optab, TFmode, "__gcc_qadd");
	set_optab_libfunc (sub_optab, TFmode, "__gcc_qsub");
	set_optab_libfunc (smul_optab, TFmode, "__gcc_qmul");
	set_optab_libfunc (sdiv_optab, TFmode, "__gcc_qdiv");

	if (!(TARGET_HARD_FLOAT && (TARGET_FPRS || TARGET_E500_DOUBLE)))
	  {
	    set_optab_libfunc (neg_optab, TFmode, "__gcc_qneg");
	    set_optab_libfunc (eq_optab, TFmode, "__gcc_qeq");
	    set_optab_libfunc (ne_optab, TFmode, "__gcc_qne");
	    set_optab_libfunc (gt_optab, TFmode, "__gcc_qgt");
	    set_optab_libfunc (ge_optab, TFmode, "__gcc_qge");
	    set_optab_libfunc (lt_optab, TFmode, "__gcc_qlt");
	    set_optab_libfunc (le_optab, TFmode, "__gcc_qle");

	    set_conv_libfunc (sext_optab, TFmode, SFmode, "__gcc_stoq");
	    set_conv_libfunc (sext_optab, TFmode, DFmode, "__gcc_dtoq");
	    set_conv_libfunc (trunc_optab, SFmode, TFmode, "__gcc_qtos");
	    set_conv_libfunc (trunc_optab, DFmode, TFmode, "__gcc_qtod");
	    set_conv_libfunc (sfix_optab, SImode, TFmode, "__gcc_qtoi");
	    set_conv_libfunc (ufix_optab, SImode, TFmode, "__gcc_qtou");
	    set_conv_libfunc (sfloat_optab, TFmode, SImode, "__gcc_itoq");
	    set_conv_libfunc (ufloat_optab, TFmode, SImode, "__gcc_utoq");
	  }

	if (!(TARGET_HARD_FLOAT && TARGET_FPRS))
	  set_optab_libfunc (unord_optab, TFmode, "__gcc_qunord");
      }
    else
      {
	set_optab_libfunc (add_optab, TFmode, "_xlqadd");
	set_optab_libfunc (sub_optab, TFmode, "_xlqsub");
	set_optab_libfunc (smul_optab, TFmode, "_xlqmul");
	set_optab_libfunc (sdiv_optab, TFmode, "_xlqdiv");
      }
  else
    {
      /* 32-bit SVR4 quad floating point routines.  */

      set_optab_libfunc (add_optab, TFmode, "_q_add");
      set_optab_libfunc (sub_optab, TFmode, "_q_sub");
      set_optab_libfunc (neg_optab, TFmode, "_q_neg");
      set_optab_libfunc (smul_optab, TFmode, "_q_mul");
      set_optab_libfunc (sdiv_optab, TFmode, "_q_div");
      if (TARGET_PPC_GPOPT)
	set_optab_libfunc (sqrt_optab, TFmode, "_q_sqrt");

      set_optab_libfunc (eq_optab, TFmode, "_q_feq");
      set_optab_libfunc (ne_optab, TFmode, "_q_fne");
      set_optab_libfunc (gt_optab, TFmode, "_q_fgt");
      set_optab_libfunc (ge_optab, TFmode, "_q_fge");
      set_optab_libfunc (lt_optab, TFmode, "_q_flt");
      set_optab_libfunc (le_optab, TFmode, "_q_fle");

      set_conv_libfunc (sext_optab, TFmode, SFmode, "_q_stoq");
      set_conv_libfunc (sext_optab, TFmode, DFmode, "_q_dtoq");
      set_conv_libfunc (trunc_optab, SFmode, TFmode, "_q_qtos");
      set_conv_libfunc (trunc_optab, DFmode, TFmode, "_q_qtod");
      set_conv_libfunc (sfix_optab, SImode, TFmode, "_q_qtoi");
      set_conv_libfunc (ufix_optab, SImode, TFmode, "_q_qtou");
      set_conv_libfunc (sfloat_optab, TFmode, SImode, "_q_itoq");
      set_conv_libfunc (ufloat_optab, TFmode, SImode, "_q_utoq");
    }
}


/* Expand a block clear operation, and return 1 if successful.  Return 0
   if we should let the compiler generate normal code.

   operands[0] is the destination
   operands[1] is the length
   operands[3] is the alignment */

int
expand_block_clear (rtx operands[])
{
  rtx orig_dest = operands[0];
  rtx bytes_rtx	= operands[1];
  rtx align_rtx = operands[3];
  bool constp	= (GET_CODE (bytes_rtx) == CONST_INT);
  HOST_WIDE_INT align;
  HOST_WIDE_INT bytes;
  int offset;
  int clear_bytes;
  int clear_step;

  /* If this is not a fixed size move, just call memcpy */
  if (! constp)
    return 0;

  /* This must be a fixed size alignment  */
  gcc_assert (GET_CODE (align_rtx) == CONST_INT);
  align = INTVAL (align_rtx) * BITS_PER_UNIT;

  /* Anything to clear? */
  bytes = INTVAL (bytes_rtx);
  if (bytes <= 0)
    return 1;

  /* Use the builtin memset after a point, to avoid huge code bloat.
     When optimize_size, avoid any significant code bloat; calling
     memset is about 4 instructions, so allow for one instruction to
     load zero and three to do clearing.  */
  if (TARGET_ALTIVEC && align >= 128)
    clear_step = 16;
  else if (TARGET_POWERPC64 && align >= 32)
    clear_step = 8;
  else if (TARGET_SPE && align >= 64)
    clear_step = 8;
  else
    clear_step = 4;

  if (optimize_size && bytes > 3 * clear_step)
    return 0;
  if (! optimize_size && bytes > 8 * clear_step)
    return 0;

  for (offset = 0; bytes > 0; offset += clear_bytes, bytes -= clear_bytes)
    {
      enum machine_mode mode = BLKmode;
      rtx dest;

      if (bytes >= 16 && TARGET_ALTIVEC && align >= 128)
	{
	  clear_bytes = 16;
	  mode = V4SImode;
	}
      else if (bytes >= 8 && TARGET_SPE && align >= 64)
        {
          clear_bytes = 8;
          mode = V2SImode;
        }
      else if (bytes >= 8 && TARGET_POWERPC64
	       /* 64-bit loads and stores require word-aligned
		  displacements.  */
	       && (align >= 64 || (!STRICT_ALIGNMENT && align >= 32)))
	{
	  clear_bytes = 8;
	  mode = DImode;
	}
      else if (bytes >= 4 && (align >= 32 || !STRICT_ALIGNMENT))
	{			/* move 4 bytes */
	  clear_bytes = 4;
	  mode = SImode;
	}
      else if (bytes >= 2 && (align >= 16 || !STRICT_ALIGNMENT))
	{			/* move 2 bytes */
	  clear_bytes = 2;
	  mode = HImode;
	}
      else /* move 1 byte at a time */
	{
	  clear_bytes = 1;
	  mode = QImode;
	}

      dest = adjust_address (orig_dest, mode, offset);

      emit_move_insn (dest, CONST0_RTX (mode));
    }

  return 1;
}


/* Expand a block move operation, and return 1 if successful.  Return 0
   if we should let the compiler generate normal code.

   operands[0] is the destination
   operands[1] is the source
   operands[2] is the length
   operands[3] is the alignment */

#define MAX_MOVE_REG 4

int
expand_block_move (rtx operands[])
{
  rtx orig_dest = operands[0];
  rtx orig_src	= operands[1];
  rtx bytes_rtx	= operands[2];
  rtx align_rtx = operands[3];
  int constp	= (GET_CODE (bytes_rtx) == CONST_INT);
  int align;
  int bytes;
  int offset;
  int move_bytes;
  rtx stores[MAX_MOVE_REG];
  int num_reg = 0;

  /* If this is not a fixed size move, just call memcpy */
  if (! constp)
    return 0;

  /* This must be a fixed size alignment */
  gcc_assert (GET_CODE (align_rtx) == CONST_INT);
  align = INTVAL (align_rtx) * BITS_PER_UNIT;

  /* Anything to move? */
  bytes = INTVAL (bytes_rtx);
  if (bytes <= 0)
    return 1;

  if (bytes > rs6000_block_move_inline_limit)
    return 0;

  for (offset = 0; bytes > 0; offset += move_bytes, bytes -= move_bytes)
    {
      union {
	rtx (*movmemsi) (rtx, rtx, rtx, rtx);
	rtx (*mov) (rtx, rtx);
      } gen_func;
      enum machine_mode mode = BLKmode;
      rtx src, dest;

      /* Altivec first, since it will be faster than a string move
	 when it applies, and usually not significantly larger.  */
      if (TARGET_ALTIVEC && bytes >= 16 && align >= 128)
	{
	  move_bytes = 16;
	  mode = V4SImode;
	  gen_func.mov = gen_movv4si;
	}
      else if (TARGET_SPE && bytes >= 8 && align >= 64)
        {
          move_bytes = 8;
          mode = V2SImode;
          gen_func.mov = gen_movv2si;
        }
      else if (TARGET_STRING
	  && bytes > 24		/* move up to 32 bytes at a time */
	  && ! fixed_regs[5]
	  && ! fixed_regs[6]
	  && ! fixed_regs[7]
	  && ! fixed_regs[8]
	  && ! fixed_regs[9]
	  && ! fixed_regs[10]
	  && ! fixed_regs[11]
	  && ! fixed_regs[12])
	{
	  move_bytes = (bytes > 32) ? 32 : bytes;
	  gen_func.movmemsi = gen_movmemsi_8reg;
	}
      else if (TARGET_STRING
	       && bytes > 16	/* move up to 24 bytes at a time */
	       && ! fixed_regs[5]
	       && ! fixed_regs[6]
	       && ! fixed_regs[7]
	       && ! fixed_regs[8]
	       && ! fixed_regs[9]
	       && ! fixed_regs[10])
	{
	  move_bytes = (bytes > 24) ? 24 : bytes;
	  gen_func.movmemsi = gen_movmemsi_6reg;
	}
      else if (TARGET_STRING
	       && bytes > 8	/* move up to 16 bytes at a time */
	       && ! fixed_regs[5]
	       && ! fixed_regs[6]
	       && ! fixed_regs[7]
	       && ! fixed_regs[8])
	{
	  move_bytes = (bytes > 16) ? 16 : bytes;
	  gen_func.movmemsi = gen_movmemsi_4reg;
	}
      else if (bytes >= 8 && TARGET_POWERPC64
	       /* 64-bit loads and stores require word-aligned
		  displacements.  */
	       && (align >= 64 || (!STRICT_ALIGNMENT && align >= 32)))
	{
	  move_bytes = 8;
	  mode = DImode;
	  gen_func.mov = gen_movdi;
	}
      else if (TARGET_STRING && bytes > 4 && !TARGET_POWERPC64)
	{			/* move up to 8 bytes at a time */
	  move_bytes = (bytes > 8) ? 8 : bytes;
	  gen_func.movmemsi = gen_movmemsi_2reg;
	}
      else if (bytes >= 4 && (align >= 32 || !STRICT_ALIGNMENT))
	{			/* move 4 bytes */
	  move_bytes = 4;
	  mode = SImode;
	  gen_func.mov = gen_movsi;
	}
      else if (bytes >= 2 && (align >= 16 || !STRICT_ALIGNMENT))
	{			/* move 2 bytes */
	  move_bytes = 2;
	  mode = HImode;
	  gen_func.mov = gen_movhi;
	}
      else if (TARGET_STRING && bytes > 1)
	{			/* move up to 4 bytes at a time */
	  move_bytes = (bytes > 4) ? 4 : bytes;
	  gen_func.movmemsi = gen_movmemsi_1reg;
	}
      else /* move 1 byte at a time */
	{
	  move_bytes = 1;
	  mode = QImode;
	  gen_func.mov = gen_movqi;
	}

      src = adjust_address (orig_src, mode, offset);
      dest = adjust_address (orig_dest, mode, offset);

      if (mode != BLKmode)
	{
	  rtx tmp_reg = gen_reg_rtx (mode);

	  emit_insn ((*gen_func.mov) (tmp_reg, src));
	  stores[num_reg++] = (*gen_func.mov) (dest, tmp_reg);
	}

      if (mode == BLKmode || num_reg >= MAX_MOVE_REG || bytes == move_bytes)
	{
	  int i;
	  for (i = 0; i < num_reg; i++)
	    emit_insn (stores[i]);
	  num_reg = 0;
	}

      if (mode == BLKmode)
	{
	  /* Move the address into scratch registers.  The movmemsi
	     patterns require zero offset.  */
	  if (!REG_P (XEXP (src, 0)))
	    {
	      rtx src_reg = copy_addr_to_reg (XEXP (src, 0));
	      src = replace_equiv_address (src, src_reg);
	    }
	  set_mem_size (src, move_bytes);

	  if (!REG_P (XEXP (dest, 0)))
	    {
	      rtx dest_reg = copy_addr_to_reg (XEXP (dest, 0));
	      dest = replace_equiv_address (dest, dest_reg);
	    }
	  set_mem_size (dest, move_bytes);

	  emit_insn ((*gen_func.movmemsi) (dest, src,
					   GEN_INT (move_bytes & 31),
					   align_rtx));
	}
    }

  return 1;
}


/* Return a string to perform a load_multiple operation.
   operands[0] is the vector.
   operands[1] is the source address.
   operands[2] is the first destination register.  */

const char *
rs6000_output_load_multiple (rtx operands[3])
{
  /* We have to handle the case where the pseudo used to contain the address
     is assigned to one of the output registers.  */
  int i, j;
  int words = XVECLEN (operands[0], 0);
  rtx xop[10];

  if (XVECLEN (operands[0], 0) == 1)
    return "lwz %2,0(%1)";

  for (i = 0; i < words; i++)
    if (refers_to_regno_p (REGNO (operands[2]) + i,
			   REGNO (operands[2]) + i + 1, operands[1], 0))
      {
	if (i == words-1)
	  {
	    xop[0] = GEN_INT (4 * (words-1));
	    xop[1] = operands[1];
	    xop[2] = operands[2];
	    output_asm_insn ("lswi %2,%1,%0\n\tlwz %1,%0(%1)", xop);
	    return "";
	  }
	else if (i == 0)
	  {
	    xop[0] = GEN_INT (4 * (words-1));
	    xop[1] = operands[1];
	    xop[2] = gen_rtx_REG (SImode, REGNO (operands[2]) + 1);
	    output_asm_insn ("addi %1,%1,4\n\tlswi %2,%1,%0\n\tlwz %1,-4(%1)", xop);
	    return "";
	  }
	else
	  {
	    for (j = 0; j < words; j++)
	      if (j != i)
		{
		  xop[0] = GEN_INT (j * 4);
		  xop[1] = operands[1];
		  xop[2] = gen_rtx_REG (SImode, REGNO (operands[2]) + j);
		  output_asm_insn ("lwz %2,%0(%1)", xop);
		}
	    xop[0] = GEN_INT (i * 4);
	    xop[1] = operands[1];
	    output_asm_insn ("lwz %1,%0(%1)", xop);
	    return "";
	  }
      }

  return "lswi %2,%1,%N0";
}


/* A validation routine: say whether CODE, a condition code, and MODE
   match.  The other alternatives either don't make sense or should
   never be generated.  */

void
validate_condition_mode (enum rtx_code code, enum machine_mode mode)
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

  /* These should never be generated except for
     flag_finite_math_only.  */
  gcc_assert (mode != CCFPmode
	      || flag_finite_math_only
	      || (code != LE && code != GE
		  && code != UNEQ && code != LTGT
		  && code != UNGT && code != UNLT));

  /* These are invalid; the information is not there.  */
  gcc_assert (mode != CCEQmode || code == EQ || code == NE);
}


/* Return 1 if ANDOP is a mask that has no bits on that are not in the
   mask required to convert the result of a rotate insn into a shift
   left insn of SHIFTOP bits.  Both are known to be SImode CONST_INT.  */

int
includes_lshift_p (rtx shiftop, rtx andop)
{
  unsigned HOST_WIDE_INT shift_mask = ~(unsigned HOST_WIDE_INT) 0;

  shift_mask <<= INTVAL (shiftop);

  return (INTVAL (andop) & 0xffffffff & ~shift_mask) == 0;
}

/* Similar, but for right shift.  */

int
includes_rshift_p (rtx shiftop, rtx andop)
{
  unsigned HOST_WIDE_INT shift_mask = ~(unsigned HOST_WIDE_INT) 0;

  shift_mask >>= INTVAL (shiftop);

  return (INTVAL (andop) & 0xffffffff & ~shift_mask) == 0;
}

/* Return 1 if ANDOP is a mask suitable for use with an rldic insn
   to perform a left shift.  It must have exactly SHIFTOP least
   significant 0's, then one or more 1's, then zero or more 0's.  */

int
includes_rldic_lshift_p (rtx shiftop, rtx andop)
{
  if (GET_CODE (andop) == CONST_INT)
    {
      HOST_WIDE_INT c, lsb, shift_mask;

      c = INTVAL (andop);
      if (c == 0 || c == ~0)
	return 0;

      shift_mask = ~0;
      shift_mask <<= INTVAL (shiftop);

      /* Find the least significant one bit.  */
      lsb = c & -c;

      /* It must coincide with the LSB of the shift mask.  */
      if (-lsb != shift_mask)
	return 0;

      /* Invert to look for the next transition (if any).  */
      c = ~c;

      /* Remove the low group of ones (originally low group of zeros).  */
      c &= -lsb;

      /* Again find the lsb, and check we have all 1's above.  */
      lsb = c & -c;
      return c == -lsb;
    }
  else
    return 0;
}

/* Return 1 if ANDOP is a mask suitable for use with an rldicr insn
   to perform a left shift.  It must have SHIFTOP or more least
   significant 0's, with the remainder of the word 1's.  */

int
includes_rldicr_lshift_p (rtx shiftop, rtx andop)
{
  if (GET_CODE (andop) == CONST_INT)
    {
      HOST_WIDE_INT c, lsb, shift_mask;

      shift_mask = ~0;
      shift_mask <<= INTVAL (shiftop);
      c = INTVAL (andop);

      /* Find the least significant one bit.  */
      lsb = c & -c;

      /* It must be covered by the shift mask.
	 This test also rejects c == 0.  */
      if ((lsb & shift_mask) == 0)
	return 0;

      /* Check we have all 1's above the transition, and reject all 1's.  */
      return c == -lsb && lsb != 1;
    }
  else
    return 0;
}

/* Return 1 if operands will generate a valid arguments to rlwimi
instruction for insert with right shift in 64-bit mode.  The mask may
not start on the first bit or stop on the last bit because wrap-around
effects of instruction do not correspond to semantics of RTL insn.  */

int
insvdi_rshift_rlwimi_p (rtx sizeop, rtx startop, rtx shiftop)
{
  if (INTVAL (startop) > 32
      && INTVAL (startop) < 64
      && INTVAL (sizeop) > 1
      && INTVAL (sizeop) + INTVAL (startop) < 64
      && INTVAL (shiftop) > 0
      && INTVAL (sizeop) + INTVAL (shiftop) < 32
      && (64 - (INTVAL (shiftop) & 63)) >= INTVAL (sizeop))
    return 1;

  return 0;
}

/* Return 1 if REGNO (reg1) == REGNO (reg2) - 1 making them candidates
   for lfq and stfq insns iff the registers are hard registers.   */

int
registers_ok_for_quad_peep (rtx reg1, rtx reg2)
{
  /* We might have been passed a SUBREG.  */
  if (GET_CODE (reg1) != REG || GET_CODE (reg2) != REG)
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

  /* And now for the second addr.  */
  if (GET_CODE (addr2) == PLUS)
    {
      /* If not a REG, return zero.  */
      if (GET_CODE (XEXP (addr2, 0)) != REG)
	return 0;
      else
	{
	  reg2 = REGNO (XEXP (addr2, 0));
	  /* The offset must be constant. */
	  if (GET_CODE (XEXP (addr2, 1)) != CONST_INT)
	    return 0;
	  offset2 = INTVAL (XEXP (addr2, 1));
	}
    }
  else if (GET_CODE (addr2) != REG)
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


rtx
rs6000_secondary_memory_needed_rtx (enum machine_mode mode)
{
  static bool eliminated = false;
  rtx ret;

  if (mode != SDmode || TARGET_NO_SDMODE_STACK)
    ret = assign_stack_local (mode, GET_MODE_SIZE (mode), 0);
  else
    {
      rtx mem = cfun->machine->sdmode_stack_slot;
      gcc_assert (mem != NULL_RTX);

      if (!eliminated)
	{
	  mem = eliminate_regs (mem, VOIDmode, NULL_RTX);
	  cfun->machine->sdmode_stack_slot = mem;
	  eliminated = true;
	}
      ret = mem;
    }

  if (TARGET_DEBUG_ADDR)
    {
      fprintf (stderr, "\nrs6000_secondary_memory_needed_rtx, mode %s, rtx:\n",
	       GET_MODE_NAME (mode));
      if (!ret)
	fprintf (stderr, "\tNULL_RTX\n");
      else
	debug_rtx (ret);
    }

  return ret;
}

/* Return the mode to be used for memory when a secondary memory
   location is needed.  For SDmode values we need to use DDmode, in
   all other cases we can use the same mode.  */
enum machine_mode
rs6000_secondary_memory_needed_mode (enum machine_mode mode)
{
  if (lra_in_progress && mode == SDmode)
    return DDmode;
  return mode;
}

static tree
rs6000_check_sdmode (tree *tp, int *walk_subtrees, void *data ATTRIBUTE_UNUSED)
{
  /* Don't walk into types.  */
  if (*tp == NULL_TREE || *tp == error_mark_node || TYPE_P (*tp))
    {
      *walk_subtrees = 0;
      return NULL_TREE;
    }

  switch (TREE_CODE (*tp))
    {
    case VAR_DECL:
    case PARM_DECL:
    case FIELD_DECL:
    case RESULT_DECL:
    case SSA_NAME:
    case REAL_CST:
    case MEM_REF:
    case VIEW_CONVERT_EXPR:
      if (TYPE_MODE (TREE_TYPE (*tp)) == SDmode)
	return *tp;
      break;
    default:
      break;
    }

  return NULL_TREE;
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

  if (GET_CODE (reg) == SUBREG)
    reg = SUBREG_REG (reg);

  if (!REG_P (reg))
    return NO_REG_TYPE;

  regno = REGNO (reg);
  if (regno >= FIRST_PSEUDO_REGISTER)
    {
      if (!lra_in_progress && !reload_in_progress && !reload_completed)
	return PSEUDO_REG_TYPE;

      regno = true_regnum (reg);
      if (regno < 0 || regno >= FIRST_PSEUDO_REGISTER)
	return PSEUDO_REG_TYPE;
    }	

  gcc_assert (regno >= 0);

  if (is_altivec && ALTIVEC_REGNO_P (regno))
    *is_altivec = true;

  rclass = rs6000_regno_regclass[regno];
  return reg_class_to_reg_type[(int)rclass];
}

/* Helper function for rs6000_secondary_reload to return true if a move to a
   different register classe is really a simple move.  */

static bool
rs6000_secondary_reload_simple_move (enum rs6000_reg_type to_type,
				     enum rs6000_reg_type from_type,
				     enum machine_mode mode)
{
  int size;

  /* Add support for various direct moves available.  In this function, we only
     look at cases where we don't need any extra registers, and one or more
     simple move insns are issued.  At present, 32-bit integers are not allowed
     in FPR/VSX registers.  Single precision binary floating is not a simple
     move because we need to convert to the single precision memory layout.
     The 4-byte SDmode can be moved.  */
  size = GET_MODE_SIZE (mode);
  if (TARGET_DIRECT_MOVE
      && ((mode == SDmode) || (TARGET_POWERPC64 && size == 8))
      && ((to_type == GPR_REG_TYPE && from_type == VSX_REG_TYPE)
	  || (to_type == VSX_REG_TYPE && from_type == GPR_REG_TYPE)))
    return true;

  else if (TARGET_MFPGPR && TARGET_POWERPC64 && size == 8
	   && ((to_type == GPR_REG_TYPE && from_type == FPR_REG_TYPE)
	       || (to_type == FPR_REG_TYPE && from_type == GPR_REG_TYPE)))
    return true;

  else if ((size == 4 || (TARGET_POWERPC64 && size == 8))
	   && ((to_type == GPR_REG_TYPE && from_type == SPR_REG_TYPE)
	       || (to_type == SPR_REG_TYPE && from_type == GPR_REG_TYPE)))
    return true;

  return false;
}

/* Power8 helper function for rs6000_secondary_reload, handle all of the
   special direct moves that involve allocating an extra register, return the
   insn code of the helper function if there is such a function or
   CODE_FOR_nothing if not.  */

static bool
rs6000_secondary_reload_direct_move (enum rs6000_reg_type to_type,
				     enum rs6000_reg_type from_type,
				     enum machine_mode mode,
				     secondary_reload_info *sri,
				     bool altivec_p)
{
  bool ret = false;
  enum insn_code icode = CODE_FOR_nothing;
  int cost = 0;
  int size = GET_MODE_SIZE (mode);

  if (TARGET_POWERPC64)
    {
      if (size == 16)
	{
	  /* Handle moving 128-bit values from GPRs to VSX point registers on
	     power8 when running in 64-bit mode using XXPERMDI to glue the two
	     64-bit values back together.  */
	  if (to_type == VSX_REG_TYPE && from_type == GPR_REG_TYPE)
	    {
	      cost = 3;			/* 2 mtvsrd's, 1 xxpermdi.  */
	      icode = reg_addr[mode].reload_vsx_gpr;
	    }

	  /* Handle moving 128-bit values from VSX point registers to GPRs on
	     power8 when running in 64-bit mode using XXPERMDI to get access to the
	     bottom 64-bit value.  */
	  else if (to_type == GPR_REG_TYPE && from_type == VSX_REG_TYPE)
	    {
	      cost = 3;			/* 2 mfvsrd's, 1 xxpermdi.  */
	      icode = reg_addr[mode].reload_gpr_vsx;
	    }
	}

      else if (mode == SFmode)
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
    }

  if (TARGET_POWERPC64 && size == 16)
    {
      /* Handle moving 128-bit values from GPRs to VSX point registers on
	 power8 when running in 64-bit mode using XXPERMDI to glue the two
	 64-bit values back together.  */
      if (to_type == VSX_REG_TYPE && from_type == GPR_REG_TYPE)
	{
	  cost = 3;			/* 2 mtvsrd's, 1 xxpermdi.  */
	  icode = reg_addr[mode].reload_vsx_gpr;
	}

      /* Handle moving 128-bit values from VSX point registers to GPRs on
	 power8 when running in 64-bit mode using XXPERMDI to get access to the
	 bottom 64-bit value.  */
      else if (to_type == GPR_REG_TYPE && from_type == VSX_REG_TYPE)
	{
	  cost = 3;			/* 2 mfvsrd's, 1 xxpermdi.  */
	  icode = reg_addr[mode].reload_gpr_vsx;
	}
    }

  else if (!TARGET_POWERPC64 && size == 8)
    {
      /* Handle moving 64-bit values from GPRs to floating point registers on
	 power8 when running in 32-bit mode using FMRGOW to glue the two 32-bit
	 values back together.  Altivec register classes must be handled
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
   (using power8's direct move in this case.  */

static bool
rs6000_secondary_reload_move (enum rs6000_reg_type to_type,
			      enum rs6000_reg_type from_type,
			      enum machine_mode mode,
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
			 enum machine_mode mode,
			 secondary_reload_info *sri)
{
  enum reg_class rclass = (enum reg_class) rclass_i;
  reg_class_t ret = ALL_REGS;
  enum insn_code icode;
  bool default_p = false;

  sri->icode = CODE_FOR_nothing;
  icode = ((in_p)
	   ? reg_addr[mode].reload_load
	   : reg_addr[mode].reload_store);

  if (REG_P (x) || register_operand (x, mode))
    {
      enum rs6000_reg_type to_type = reg_class_to_reg_type[(int)rclass];
      bool altivec_p = (rclass == ALTIVEC_REGS);
      enum rs6000_reg_type from_type = register_to_reg_type (x, &altivec_p);

      if (!in_p)
	{
	  enum rs6000_reg_type exchange = to_type;
	  to_type = from_type;
	  from_type = exchange;
	}

      /* Can we do a direct move of some sort?  */
      if (rs6000_secondary_reload_move (to_type, from_type, mode, sri,
					altivec_p))
	{
	  icode = (enum insn_code)sri->icode;
	  default_p = false;
	  ret = NO_REGS;
	}
    }

  /* Handle vector moves with reload helper functions.  */
  if (ret == ALL_REGS && icode != CODE_FOR_nothing)
    {
      ret = NO_REGS;
      sri->icode = CODE_FOR_nothing;
      sri->extra_cost = 0;

      if (GET_CODE (x) == MEM)
	{
	  rtx addr = XEXP (x, 0);

	  /* Loads to and stores from gprs can do reg+offset, and wouldn't need
	     an extra register in that case, but it would need an extra
	     register if the addressing is reg+reg or (reg+reg)&(-16).  Special
	     case load/store quad.  */
	  if (rclass == GENERAL_REGS || rclass == BASE_REGS)
	    {
	      if (TARGET_POWERPC64 && TARGET_QUAD_MEMORY
		  && GET_MODE_SIZE (mode) == 16
		  && quad_memory_operand (x, mode))
		{
		  sri->icode = icode;
		  sri->extra_cost = 2;
		}

	      else if (!legitimate_indirect_address_p (addr, false)
		       && !rs6000_legitimate_offset_address_p (PTImode, addr,
							       false, true))
		{
		  sri->icode = icode;
		  /* account for splitting the loads, and converting the
		     address from reg+reg to reg.  */
		  sri->extra_cost = (((TARGET_64BIT) ? 3 : 5)
				     + ((GET_CODE (addr) == AND) ? 1 : 0));
		}
	    }
         /* Allow scalar loads to/from the traditional floating point
            registers, even if VSX memory is set.  */
         else if ((rclass == FLOAT_REGS || rclass == NO_REGS)
                  && (GET_MODE_SIZE (mode) == 4 || GET_MODE_SIZE (mode) == 8)
                  && (legitimate_indirect_address_p (addr, false)
                      || legitimate_indirect_address_p (addr, false)
                      || rs6000_legitimate_offset_address_p (mode, addr,
                                                             false, true)))

           ;
         /* Loads to and stores from vector registers can only do reg+reg
            addressing.  Altivec registers can also do (reg+reg)&(-16).  Allow
            scalar modes loading up the traditional floating point registers
            to use offset addresses.  */
	  else if (rclass == VSX_REGS || rclass == ALTIVEC_REGS
		   || rclass == FLOAT_REGS || rclass == NO_REGS)
	    {
	      if (!VECTOR_MEM_ALTIVEC_P (mode)
		  && GET_CODE (addr) == AND
		  && GET_CODE (XEXP (addr, 1)) == CONST_INT
		  && INTVAL (XEXP (addr, 1)) == -16
		  && (legitimate_indirect_address_p (XEXP (addr, 0), false)
		      || legitimate_indexed_address_p (XEXP (addr, 0), false)))
		{
		  sri->icode = icode;
		  sri->extra_cost = ((GET_CODE (XEXP (addr, 0)) == PLUS)
				     ? 2 : 1);
		}
	      else if (!legitimate_indirect_address_p (addr, false)
		       && (rclass == NO_REGS
			   || !legitimate_indexed_address_p (addr, false)))
		{
		  sri->icode = icode;
		  sri->extra_cost = 1;
		}
	      else
		icode = CODE_FOR_nothing;
	    }
	  /* Any other loads, including to pseudo registers which haven't been
	     assigned to a register yet, default to require a scratch
	     register.  */
	  else
	    {
	      sri->icode = icode;
	      sri->extra_cost = 2;
	    }
	}
      else if (REG_P (x))
	{
	  int regno = true_regnum (x);

	  icode = CODE_FOR_nothing;
	  if (regno < 0 || regno >= FIRST_PSEUDO_REGISTER)
	    default_p = true;
	  else
	    {
	      enum reg_class xclass = REGNO_REG_CLASS (regno);
	      enum rs6000_reg_type rtype1 = reg_class_to_reg_type[(int)rclass];
	      enum rs6000_reg_type rtype2 = reg_class_to_reg_type[(int)xclass];

	      /* If memory is needed, use default_secondary_reload to create the
		 stack slot.  */
	      if (rtype1 != rtype2 || !IS_STD_REG_TYPE (rtype1))
		default_p = true;
	      else
		ret = NO_REGS;
	    }
	}
      else
	default_p = true;
    }
  else if (TARGET_POWERPC64
	   && reg_class_to_reg_type[(int)rclass] == GPR_REG_TYPE
	   && MEM_P (x)
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
	      if (in_p)
		sri->icode = CODE_FOR_reload_di_load;
	      else
		sri->icode = CODE_FOR_reload_di_store;
	      sri->extra_cost = 2;
	      ret = NO_REGS;
	    }
	  else
	    default_p = true;
	}
      else
	default_p = true;
    }
  else if (!TARGET_POWERPC64
	   && reg_class_to_reg_type[(int)rclass] == GPR_REG_TYPE
	   && MEM_P (x)
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
	    }
	  else
	    default_p = true;
	}
      else
	default_p = true;
    }
  else
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

      if (default_p)
	fprintf (stderr, ", default secondary reload");

      if (sri->icode != CODE_FOR_nothing)
	fprintf (stderr, ", reload func = %s, extra cost = %d\n",
		 insn_data[sri->icode].name, sri->extra_cost);
      else
	fprintf (stderr, "\n");

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
    set = gen_rtx_SET (VOIDmode, mem, reg);
  else
    set = gen_rtx_SET (VOIDmode, reg, mem);

  clobber = gen_rtx_CLOBBER (VOIDmode, scratch);
  debug_rtx (gen_rtx_PARALLEL (VOIDmode, gen_rtvec (2, set, clobber)));
}

static void
rs6000_secondary_reload_fail (int line, rtx reg, rtx mem, rtx scratch,
			      bool store_p)
{
  rs6000_secondary_reload_trace (line, reg, mem, scratch, store_p);
  gcc_unreachable ();
}

/* Fixup reload addresses for Altivec or VSX loads/stores to change SP+offset
   to SP+reg addressing.  */

void
rs6000_secondary_reload_inner (rtx reg, rtx mem, rtx scratch, bool store_p)
{
  int regno = true_regnum (reg);
  enum machine_mode mode = GET_MODE (reg);
  enum reg_class rclass;
  rtx addr;
  rtx and_op2 = NULL_RTX;
  rtx addr_op1;
  rtx addr_op2;
  rtx scratch_or_premodify = scratch;
  rtx and_rtx;
  rtx cc_clobber;

  if (TARGET_DEBUG_ADDR)
    rs6000_secondary_reload_trace (__LINE__, reg, mem, scratch, store_p);

  if (regno < 0 || regno >= FIRST_PSEUDO_REGISTER)
    rs6000_secondary_reload_fail (__LINE__, reg, mem, scratch, store_p);

  if (GET_CODE (mem) != MEM)
    rs6000_secondary_reload_fail (__LINE__, reg, mem, scratch, store_p);

  rclass = REGNO_REG_CLASS (regno);
  addr = find_replacement (&XEXP (mem, 0));

  switch (rclass)
    {
      /* GPRs can handle reg + small constant, all other addresses need to use
	 the scratch register.  */
    case GENERAL_REGS:
    case BASE_REGS:
      if (GET_CODE (addr) == AND)
	{
	  and_op2 = XEXP (addr, 1);
	  addr = find_replacement (&XEXP (addr, 0));
	}

      if (GET_CODE (addr) == PRE_MODIFY)
	{
	  scratch_or_premodify = find_replacement (&XEXP (addr, 0));
	  if (!REG_P (scratch_or_premodify))
	    rs6000_secondary_reload_fail (__LINE__, reg, mem, scratch, store_p);

	  addr = find_replacement (&XEXP (addr, 1));
	  if (GET_CODE (addr) != PLUS)
	    rs6000_secondary_reload_fail (__LINE__, reg, mem, scratch, store_p);
	}

      if (GET_CODE (addr) == PLUS
	  && (and_op2 != NULL_RTX
	      || !rs6000_legitimate_offset_address_p (PTImode, addr,
						      false, true)))
	{
	  /* find_replacement already recurses into both operands of
	     PLUS so we don't need to call it here.  */
	  addr_op1 = XEXP (addr, 0);
	  addr_op2 = XEXP (addr, 1);
	  if (!legitimate_indirect_address_p (addr_op1, false))
	    rs6000_secondary_reload_fail (__LINE__, reg, mem, scratch, store_p);

	  if (!REG_P (addr_op2)
	      && (GET_CODE (addr_op2) != CONST_INT
		  || !satisfies_constraint_I (addr_op2)))
	    {
	      if (TARGET_DEBUG_ADDR)
		{
		  fprintf (stderr,
			   "\nMove plus addr to register %s, mode = %s: ",
			   rs6000_reg_names[REGNO (scratch)],
			   GET_MODE_NAME (mode));
		  debug_rtx (addr_op2);
		}
	      rs6000_emit_move (scratch, addr_op2, Pmode);
	      addr_op2 = scratch;
	    }

	  emit_insn (gen_rtx_SET (VOIDmode,
				  scratch_or_premodify,
				  gen_rtx_PLUS (Pmode,
						addr_op1,
						addr_op2)));

	  addr = scratch_or_premodify;
	  scratch_or_premodify = scratch;
	}
      else if (!legitimate_indirect_address_p (addr, false)
	       && !rs6000_legitimate_offset_address_p (PTImode, addr,
						       false, true))
	{
	  if (TARGET_DEBUG_ADDR)
	    {
	      fprintf (stderr, "\nMove addr to register %s, mode = %s: ",
		       rs6000_reg_names[REGNO (scratch_or_premodify)],
		       GET_MODE_NAME (mode));
	      debug_rtx (addr);
	    }
	  rs6000_emit_move (scratch_or_premodify, addr, Pmode);
	  addr = scratch_or_premodify;
	  scratch_or_premodify = scratch;
	}
      break;

      /* Float registers can do offset+reg addressing for scalar types.  */
    case FLOAT_REGS:
      if (legitimate_indirect_address_p (addr, false)	/* reg */
	  || legitimate_indexed_address_p (addr, false)	/* reg+reg */
	  || ((GET_MODE_SIZE (mode) == 4 || GET_MODE_SIZE (mode) == 8)
	      && and_op2 == NULL_RTX
	      && scratch_or_premodify == scratch
	      && rs6000_legitimate_offset_address_p (mode, addr, false, false)))
	break;

      /* If this isn't a legacy floating point load/store, fall through to the
	 VSX defaults.  */

      /* VSX/Altivec registers can only handle reg+reg addressing.  Move other
	 addresses into a scratch register.  */
    case VSX_REGS:
    case ALTIVEC_REGS:

      /* With float regs, we need to handle the AND ourselves, since we can't
	 use the Altivec instruction with an implicit AND -16.  Allow scalar
	 loads to float registers to use reg+offset even if VSX.  */
      if (GET_CODE (addr) == AND
	  && (rclass != ALTIVEC_REGS || GET_MODE_SIZE (mode) != 16
	      || GET_CODE (XEXP (addr, 1)) != CONST_INT
	      || INTVAL (XEXP (addr, 1)) != -16
	      || !VECTOR_MEM_ALTIVEC_P (mode)))
	{
	  and_op2 = XEXP (addr, 1);
	  addr = find_replacement (&XEXP (addr, 0));
	}

      /* If we aren't using a VSX load, save the PRE_MODIFY register and use it
	 as the address later.  */
      if (GET_CODE (addr) == PRE_MODIFY
	  && ((ALTIVEC_OR_VSX_VECTOR_MODE (mode)
	       && (rclass != FLOAT_REGS
		   || (GET_MODE_SIZE (mode) != 4 && GET_MODE_SIZE (mode) != 8)))
	      || and_op2 != NULL_RTX
	      || !legitimate_indexed_address_p (XEXP (addr, 1), false)))
	{
	  scratch_or_premodify = find_replacement (&XEXP (addr, 0));
	  if (!legitimate_indirect_address_p (scratch_or_premodify, false))
	    rs6000_secondary_reload_fail (__LINE__, reg, mem, scratch, store_p);

	  addr = find_replacement (&XEXP (addr, 1));
	  if (GET_CODE (addr) != PLUS)
	    rs6000_secondary_reload_fail (__LINE__, reg, mem, scratch, store_p);
	}

      if (legitimate_indirect_address_p (addr, false)	/* reg */
	  || legitimate_indexed_address_p (addr, false)	/* reg+reg */
	  || (GET_CODE (addr) == AND			/* Altivec memory */
	      && rclass == ALTIVEC_REGS
	      && GET_CODE (XEXP (addr, 1)) == CONST_INT
	      && INTVAL (XEXP (addr, 1)) == -16
	      && (legitimate_indirect_address_p (XEXP (addr, 0), false)
		  || legitimate_indexed_address_p (XEXP (addr, 0), false))))
	;

      else if (GET_CODE (addr) == PLUS)
	{
	  addr_op1 = XEXP (addr, 0);
	  addr_op2 = XEXP (addr, 1);
	  if (!REG_P (addr_op1))
	    rs6000_secondary_reload_fail (__LINE__, reg, mem, scratch, store_p);

	  if (TARGET_DEBUG_ADDR)
	    {
	      fprintf (stderr, "\nMove plus addr to register %s, mode = %s: ",
		       rs6000_reg_names[REGNO (scratch)], GET_MODE_NAME (mode));
	      debug_rtx (addr_op2);
	    }
	  rs6000_emit_move (scratch, addr_op2, Pmode);
	  emit_insn (gen_rtx_SET (VOIDmode,
				  scratch_or_premodify,
				  gen_rtx_PLUS (Pmode,
						addr_op1,
						scratch)));
	  addr = scratch_or_premodify;
	  scratch_or_premodify = scratch;
	}

      else if (GET_CODE (addr) == SYMBOL_REF || GET_CODE (addr) == CONST
	       || GET_CODE (addr) == CONST_INT || GET_CODE (addr) == LO_SUM
	       || REG_P (addr))
	{
	  if (TARGET_DEBUG_ADDR)
	    {
	      fprintf (stderr, "\nMove addr to register %s, mode = %s: ",
		       rs6000_reg_names[REGNO (scratch_or_premodify)],
		       GET_MODE_NAME (mode));
	      debug_rtx (addr);
	    }

	  rs6000_emit_move (scratch_or_premodify, addr, Pmode);
	  addr = scratch_or_premodify;
	  scratch_or_premodify = scratch;
	}

      else
	rs6000_secondary_reload_fail (__LINE__, reg, mem, scratch, store_p);

      break;

    default:
      rs6000_secondary_reload_fail (__LINE__, reg, mem, scratch, store_p);
    }

  /* If the original address involved a pre-modify that we couldn't use the VSX
     memory instruction with update, and we haven't taken care of already,
     store the address in the pre-modify register and use that as the
     address.  */
  if (scratch_or_premodify != scratch && scratch_or_premodify != addr)
    {
      emit_insn (gen_rtx_SET (VOIDmode, scratch_or_premodify, addr));
      addr = scratch_or_premodify;
    }

  /* If the original address involved an AND -16 and we couldn't use an ALTIVEC
     memory instruction, recreate the AND now, including the clobber which is
     generated by the general ANDSI3/ANDDI3 patterns for the
     andi. instruction.  */
  if (and_op2 != NULL_RTX)
    {
      if (! legitimate_indirect_address_p (addr, false))
	{
	  emit_insn (gen_rtx_SET (VOIDmode, scratch, addr));
	  addr = scratch;
	}

      if (TARGET_DEBUG_ADDR)
	{
	  fprintf (stderr, "\nAnd addr to register %s, mode = %s: ",
		   rs6000_reg_names[REGNO (scratch)], GET_MODE_NAME (mode));
	  debug_rtx (and_op2);
	}

      and_rtx = gen_rtx_SET (VOIDmode,
			     scratch,
			     gen_rtx_AND (Pmode,
					  addr,
					  and_op2));

      cc_clobber = gen_rtx_CLOBBER (CCmode, gen_rtx_SCRATCH (CCmode));
      emit_insn (gen_rtx_PARALLEL (VOIDmode,
				   gen_rtvec (2, and_rtx, cc_clobber)));
      addr = scratch;
    }

  /* Adjust the address if it changed.  */
  if (addr != XEXP (mem, 0))
    {
      mem = replace_equiv_address_nv (mem, addr);
      if (TARGET_DEBUG_ADDR)
	fprintf (stderr, "\nrs6000_secondary_reload_inner, mem adjusted.\n");
    }

  /* Now create the move.  */
  if (store_p)
    emit_insn (gen_rtx_SET (VOIDmode, mem, reg));
  else
    emit_insn (gen_rtx_SET (VOIDmode, reg, mem));

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

  gcc_assert (regno >= 0 && regno < FIRST_PSEUDO_REGISTER);
  gcc_assert (GET_CODE (mem) == MEM);
  rclass = REGNO_REG_CLASS (regno);
  gcc_assert (rclass == GENERAL_REGS || rclass == BASE_REGS);
  addr = XEXP (mem, 0);

  if (GET_CODE (addr) == PRE_MODIFY)
    {
      scratch_or_premodify = XEXP (addr, 0);
      gcc_assert (REG_P (scratch_or_premodify));
      addr = XEXP (addr, 1);
    }
  gcc_assert (GET_CODE (addr) == PLUS || GET_CODE (addr) == LO_SUM);

  rs6000_emit_move (scratch_or_premodify, addr, Pmode);

  mem = replace_equiv_address_nv (mem, scratch_or_premodify);

  /* Now create the move.  */
  if (store_p)
    emit_insn (gen_rtx_SET (VOIDmode, mem, reg));
  else
    emit_insn (gen_rtx_SET (VOIDmode, reg, mem));

  return;
}

/* Allocate a 64-bit stack slot to be used for copying SDmode values through if
   this function has any SDmode references.  If we are on a power7 or later, we
   don't need the 64-bit stack slot since the LFIWZX and STIFWX instructions
   can load/store the value.  */

static void
rs6000_alloc_sdmode_stack_slot (void)
{
  tree t;
  basic_block bb;
  gimple_stmt_iterator gsi;

  gcc_assert (cfun->machine->sdmode_stack_slot == NULL_RTX);
  /* We use a different approach for dealing with the secondary
     memory in LRA.  */
  if (ira_use_lra_p)
    return;

  if (TARGET_NO_SDMODE_STACK)
    return;

  FOR_EACH_BB_FN (bb, cfun)
    for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
      {
	tree ret = walk_gimple_op (gsi_stmt (gsi), rs6000_check_sdmode, NULL);
	if (ret)
	  {
	    rtx stack = assign_stack_local (DDmode, GET_MODE_SIZE (DDmode), 0);
	    cfun->machine->sdmode_stack_slot = adjust_address_nv (stack,
								  SDmode, 0);
	    return;
	  }
      }

  /* Check for any SDmode parameters of the function.  */
  for (t = DECL_ARGUMENTS (cfun->decl); t; t = DECL_CHAIN (t))
    {
      if (TREE_TYPE (t) == error_mark_node)
	continue;

      if (TYPE_MODE (TREE_TYPE (t)) == SDmode
	  || TYPE_MODE (DECL_ARG_TYPE (t)) == SDmode)
	{
	  rtx stack = assign_stack_local (DDmode, GET_MODE_SIZE (DDmode), 0);
	  cfun->machine->sdmode_stack_slot = adjust_address_nv (stack,
								SDmode, 0);
	  return;
	}
    }
}

static void
rs6000_instantiate_decls (void)
{
  if (cfun->machine->sdmode_stack_slot != NULL_RTX)
    instantiate_decl_rtl (cfun->machine->sdmode_stack_slot);
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
  enum machine_mode mode = GET_MODE (x);

  if (TARGET_VSX && x == CONST0_RTX (mode) && VSX_REG_CLASS_P (rclass))
    return rclass;

  if (VECTOR_UNIT_ALTIVEC_OR_VSX_P (mode)
      && (rclass == ALTIVEC_REGS || rclass == VSX_REGS)
      && easy_vector_constant (x, mode))
    return ALTIVEC_REGS;

  if (CONSTANT_P (x) && reg_classes_intersect_p (rclass, FLOAT_REGS))
    return NO_REGS;

  if (GET_MODE_CLASS (mode) == MODE_INT && rclass == NON_SPECIAL_REGS)
    return GENERAL_REGS;

  /* For VSX, prefer the traditional registers for 64-bit values because we can
     use the non-VSX loads.  Prefer the Altivec registers if Altivec is
     handling the vector operations (i.e. V16QI, V8HI, and V4SI), or if we
     prefer Altivec loads..  */
  if (rclass == VSX_REGS)
    {
      if (GET_MODE_SIZE (mode) <= 8)
	return FLOAT_REGS;

      if (VECTOR_UNIT_ALTIVEC_P (mode) || VECTOR_MEM_ALTIVEC_P (mode))
	return ALTIVEC_REGS;

      return rclass;
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
rs6000_secondary_memory_needed (enum reg_class from_class,
				enum reg_class to_class,
				enum machine_mode mode)
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
rs6000_debug_secondary_memory_needed (enum reg_class from_class,
				      enum reg_class to_class,
				      enum machine_mode mode)
{
  bool ret = rs6000_secondary_memory_needed (from_class, to_class, mode);

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
rs6000_secondary_reload_class (enum reg_class rclass, enum machine_mode mode,
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
	  && (GET_CODE (in) == SYMBOL_REF
	      || GET_CODE (in) == HIGH
	      || GET_CODE (in) == LABEL_REF
	      || GET_CODE (in) == CONST))
	return BASE_REGS;
    }

  if (GET_CODE (in) == REG)
    {
      regno = REGNO (in);
      if (regno >= FIRST_PSEUDO_REGISTER)
	{
	  regno = true_regnum (in);
	  if (regno >= FIRST_PSEUDO_REGISTER)
	    regno = -1;
	}
    }
  else if (GET_CODE (in) == SUBREG)
    {
      regno = true_regnum (in);
      if (regno >= FIRST_PSEUDO_REGISTER)
	regno = -1;
    }
  else
    regno = -1;

  /* We can place anything into GENERAL_REGS and can put GENERAL_REGS
     into anything.  */
  if (rclass == GENERAL_REGS || rclass == BASE_REGS
      || (regno >= 0 && INT_REGNO_P (regno)))
    return NO_REGS;

  /* Constants, memory, and FP registers can go into FP registers.  */
  if ((regno == -1 || FP_REGNO_P (regno))
      && (rclass == FLOAT_REGS || rclass == NON_SPECIAL_REGS))
    return (mode != SDmode || lra_in_progress) ? NO_REGS : GENERAL_REGS;

  /* Memory, and FP/altivec registers can go into fp/altivec registers under
     VSX.  However, for scalar variables, use the traditional floating point
     registers so that we can use offset+register addressing.  */
  if (TARGET_VSX
      && (regno == -1 || VSX_REGNO_P (regno))
      && VSX_REG_CLASS_P (rclass))
    {
      if (GET_MODE_SIZE (mode) < 16)
	return FLOAT_REGS;

      return NO_REGS;
    }

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
				     enum machine_mode mode, rtx in)
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

/* Return nonzero if for CLASS a mode change from FROM to TO is invalid.  */

static bool
rs6000_cannot_change_mode_class (enum machine_mode from,
				 enum machine_mode to,
				 enum reg_class rclass)
{
  unsigned from_size = GET_MODE_SIZE (from);
  unsigned to_size = GET_MODE_SIZE (to);

  if (from_size != to_size)
    {
      enum reg_class xclass = (TARGET_VSX) ? VSX_REGS : FLOAT_REGS;

      if (reg_classes_intersect_p (xclass, rclass))
	{
	  unsigned to_nregs = hard_regno_nregs[FIRST_FPR_REGNO][to];
	  unsigned from_nregs = hard_regno_nregs[FIRST_FPR_REGNO][from];

	  /* Don't allow 64-bit types to overlap with 128-bit types that take a
	     single register under VSX because the scalar part of the register
	     is in the upper 64-bits, and not the lower 64-bits.  Types like
	     TFmode/TDmode that take 2 scalar register can overlap.  128-bit
	     IEEE floating point can't overlap, and neither can small
	     values.  */

	  if (TARGET_IEEEQUAD && (to == TFmode || from == TFmode))
	    return true;

	  /* TDmode in floating-mode registers must always go into a register
	     pair with the most significant word in the even-numbered register
	     to match ISA requirements.  In little-endian mode, this does not
	     match subreg numbering, so we cannot allow subregs.  */
	  if (!BYTES_BIG_ENDIAN && (to == TDmode || from == TDmode))
	    return true;

	  if (from_size < 8 || to_size < 8)
	    return true;

	  if (from_size == 8 && (8 * to_nregs) != to_size)
	    return true;

	  if (to_size == 8 && (8 * from_nregs) != from_size)
	    return true;

	  return false;
	}
      else
	return false;
    }

  if (TARGET_E500_DOUBLE
      && ((((to) == DFmode) + ((from) == DFmode)) == 1
	  || (((to) == TFmode) + ((from) == TFmode)) == 1
	  || (((to) == DDmode) + ((from) == DDmode)) == 1
	  || (((to) == TDmode) + ((from) == TDmode)) == 1
	  || (((to) == DImode) + ((from) == DImode)) == 1))
    return true;

  /* Since the VSX register set includes traditional floating point registers
     and altivec registers, just check for the size being different instead of
     trying to check whether the modes are vector modes.  Otherwise it won't
     allow say DF and DI to change classes.  For types like TFmode and TDmode
     that take 2 64-bit registers, rather than a single 128-bit register, don't
     allow subregs of those types to other 128 bit types.  */
  if (TARGET_VSX && VSX_REG_CLASS_P (rclass))
    {
      unsigned num_regs = (from_size + 15) / 16;
      if (hard_regno_nregs[FIRST_FPR_REGNO][to] > num_regs
	  || hard_regno_nregs[FIRST_FPR_REGNO][from] > num_regs)
	return true;

      return (from_size != 8 && from_size != 16);
    }

  if (TARGET_ALTIVEC && rclass == ALTIVEC_REGS
      && (ALTIVEC_VECTOR_MODE (from) + ALTIVEC_VECTOR_MODE (to)) == 1)
    return true;

  if (TARGET_SPE && (SPE_VECTOR_MODE (from) + SPE_VECTOR_MODE (to)) == 1
      && reg_classes_intersect_p (GENERAL_REGS, rclass))
    return true;

  return false;
}

/* Debug version of rs6000_cannot_change_mode_class.  */
static bool
rs6000_debug_cannot_change_mode_class (enum machine_mode from,
				       enum machine_mode to,
				       enum reg_class rclass)
{
  bool ret = rs6000_cannot_change_mode_class (from, to, rclass);

  fprintf (stderr,
	   "rs6000_cannot_change_mode_class, return %s, from = %s, "
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
  enum machine_mode mode = GET_MODE (dest);
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

	  else if (TARGET_VSX && TARGET_DIRECT_MOVE && src_vsx_p)
	    return "#";
	}

      else if (TARGET_VSX && dest_vsx_p)
	{
	  if (src_vsx_p)
	    return "xxlor %x0,%x1,%x1";

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
	  if (mode == V16QImode || mode == V8HImode || mode == V4SImode)
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
	       && altivec_indexed_or_indirect_operand (src, mode))
	return "stvx %1,%y0";

      else if (TARGET_VSX && src_vsx_p)
	{
	  if (mode == V16QImode || mode == V8HImode || mode == V4SImode)
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
	   && (GET_CODE (src) == CONST_INT
	       || GET_CODE (src) == CONST_DOUBLE
	       || GET_CODE (src) == CONST_VECTOR))
    {
      if (dest_gpr_p)
	return "#";

      else if (TARGET_VSX && dest_vsx_p && zero_constant (src, mode))
	return "xxlxor %x0,%x0,%x0";

      else if (TARGET_ALTIVEC && dest_vmx_p)
	return output_vec_const_move (operands);
    }

  if (TARGET_DEBUG_ADDR)
    {
      fprintf (stderr, "\n===== Bad 128 bit move:\n");
      debug_rtx (gen_rtx_SET (VOIDmode, dest, src));
    }

  gcc_unreachable ();
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
  enum machine_mode cc_mode;
  int cc_regnum;
  int base_bit;
  rtx reg;

  if (!COMPARISON_P (op))
    return -1;

  reg = XEXP (op, 0);

  gcc_assert (GET_CODE (reg) == REG && CR_REGNO_P (REGNO (reg)));

  cc_mode = GET_MODE (reg);
  cc_regnum = REGNO (reg);
  base_bit = 4 * (cc_regnum - CR0_REGNO);

  validate_condition_mode (code, cc_mode);

  /* When generating a sCOND operation, only positive conditions are
     allowed.  */
  gcc_assert (!scc_p
	      || code == EQ || code == GT || code == LT || code == UNORDERED
	      || code == GTU || code == LTU);

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
      gcc_unreachable ();
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

static rs6000_stack_t stack_info;

/* Function to init struct machine_function.
   This will be called, via a pointer variable,
   from push_function_context.  */

static struct machine_function *
rs6000_init_machine_status (void)
{
  stack_info.reload_completed = 0;
  return ggc_alloc_cleared_machine_function ();
}

#define INT_P(X) (GET_CODE (X) == CONST_INT && GET_MODE (X) == VOIDmode)

int
extract_MB (rtx op)
{
  int i;
  unsigned long val = INTVAL (op);

  /* If the high bit is zero, the value is the first 1 bit we find
     from the left.  */
  if ((val & 0x80000000) == 0)
    {
      gcc_assert (val & 0xffffffff);

      i = 1;
      while (((val <<= 1) & 0x80000000) == 0)
	++i;
      return i;
    }

  /* If the high bit is set and the low bit is not, or the mask is all
     1's, the value is zero.  */
  if ((val & 1) == 0 || (val & 0xffffffff) == 0xffffffff)
    return 0;

  /* Otherwise we have a wrap-around mask.  Look for the first 0 bit
     from the right.  */
  i = 31;
  while (((val >>= 1) & 1) != 0)
    --i;

  return i;
}

int
extract_ME (rtx op)
{
  int i;
  unsigned long val = INTVAL (op);

  /* If the low bit is zero, the value is the first 1 bit we find from
     the right.  */
  if ((val & 1) == 0)
    {
      gcc_assert (val & 0xffffffff);

      i = 30;
      while (((val >>= 1) & 1) == 0)
	--i;

      return i;
    }

  /* If the low bit is set and the high bit is not, or the mask is all
     1's, the value is 31.  */
  if ((val & 0x80000000) == 0 || (val & 0xffffffff) == 0xffffffff)
    return 31;

  /* Otherwise we have a wrap-around mask.  Look for the first 0 bit
     from the left.  */
  i = 0;
  while (((val <<= 1) & 0x80000000) != 0)
    ++i;

  return i;
}

/* Locate some local-dynamic symbol still in use by this function
   so that we can print its name in some tls_ld pattern.  */

static const char *
rs6000_get_some_local_dynamic_name (void)
{
  rtx insn;

  if (cfun->machine->some_ld_name)
    return cfun->machine->some_ld_name;

  for (insn = get_insns (); insn ; insn = NEXT_INSN (insn))
    if (INSN_P (insn)
	&& for_each_rtx (&PATTERN (insn),
			 rs6000_get_some_local_dynamic_name_1, 0))
      return cfun->machine->some_ld_name;

  gcc_unreachable ();
}

/* Helper function for rs6000_get_some_local_dynamic_name.  */

static int
rs6000_get_some_local_dynamic_name_1 (rtx *px, void *data ATTRIBUTE_UNUSED)
{
  rtx x = *px;

  if (GET_CODE (x) == SYMBOL_REF)
    {
      const char *str = XSTR (x, 0);
      if (SYMBOL_REF_TLS_MODEL (x) == TLS_MODEL_LOCAL_DYNAMIC)
	{
	  cfun->machine->some_ld_name = str;
	  return 1;
	}
    }

  return 0;
}

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

    case 'b':
      /* If constant, low-order 16 bits of constant, unsigned.
	 Otherwise, write normally.  */
      if (INT_P (x))
	fprintf (file, HOST_WIDE_INT_PRINT_DEC, INTVAL (x) & 0xffff);
      else
	print_operand (file, x, 0);
      return;

    case 'B':
      /* If the low-order bit is zero, write 'r'; otherwise, write 'l'
	 for 64-bit mask direction.  */
      putc (((INTVAL (x) & 1) == 0 ? 'r' : 'l'), file);
      return;

      /* %c is output_addr_const if a CONSTANT_ADDRESS_P, otherwise
	 output_operand.  */

    case 'D':
      /* Like 'J' but get to the GT bit only.  */
      gcc_assert (REG_P (x));

      /* Bit 1 is GT bit.  */
      i = 4 * (REGNO (x) - CR0_REGNO) + 1;

      /* Add one for shift count in rlinm for scc.  */
      fprintf (file, "%d", i + 1);
      return;

    case 'E':
      /* X is a CR register.  Print the number of the EQ bit of the CR */
      if (GET_CODE (x) != REG || ! CR_REGNO_P (REGNO (x)))
	output_operand_lossage ("invalid %%E value");
      else
	fprintf (file, "%d", 4 * (REGNO (x) - CR0_REGNO) + 2);
      return;

    case 'f':
      /* X is a CR register.  Print the shift count needed to move it
	 to the high-order four bits.  */
      if (GET_CODE (x) != REG || ! CR_REGNO_P (REGNO (x)))
	output_operand_lossage ("invalid %%f value");
      else
	fprintf (file, "%d", 4 * (REGNO (x) - CR0_REGNO));
      return;

    case 'F':
      /* Similar, but print the count for the rotate in the opposite
	 direction.  */
      if (GET_CODE (x) != REG || ! CR_REGNO_P (REGNO (x)))
	output_operand_lossage ("invalid %%F value");
      else
	fprintf (file, "%d", 32 - 4 * (REGNO (x) - CR0_REGNO));
      return;

    case 'G':
      /* X is a constant integer.  If it is negative, print "m",
	 otherwise print "z".  This is to make an aze or ame insn.  */
      if (GET_CODE (x) != CONST_INT)
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
	      || (GET_CODE (XEXP (XEXP (x, 0), 0)) != SYMBOL_REF
		  && GET_CODE (XEXP (XEXP (x, 0), 0)) != LABEL_REF)
	      || GET_CODE (XEXP (XEXP (x, 0), 1)) != CONST_INT)
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
	  /* Handle possible auto-increment.  Since it is pre-increment and
	     we have already done it, we can just use an offset of word.  */
	  if (GET_CODE (XEXP (x, 0)) == PRE_INC
	      || GET_CODE (XEXP (x, 0)) == PRE_DEC)
	    output_address (plus_constant (Pmode, XEXP (XEXP (x, 0), 0),
					   UNITS_PER_WORD));
	  else if (GET_CODE (XEXP (x, 0)) == PRE_MODIFY)
	    output_address (plus_constant (Pmode, XEXP (XEXP (x, 0), 0),
					   UNITS_PER_WORD));
	  else
	    output_address (XEXP (adjust_address_nv (x, SImode,
						     UNITS_PER_WORD),
				  0));

	  if (small_data_operand (x, GET_MODE (x)))
	    fprintf (file, "@%s(%s)", SMALL_DATA_RELOC,
		     reg_names[SMALL_DATA_REG]);
	}
      return;

    case 'm':
      /* MB value for a mask operand.  */
      if (! mask_operand (x, SImode))
	output_operand_lossage ("invalid %%m value");

      fprintf (file, "%d", extract_MB (x));
      return;

    case 'M':
      /* ME value for a mask operand.  */
      if (! mask_operand (x, SImode))
	output_operand_lossage ("invalid %%M value");

      fprintf (file, "%d", extract_ME (x));
      return;

      /* %n outputs the negative of its operand.  */

    case 'N':
      /* Write the number of elements in the vector times 4.  */
      if (GET_CODE (x) != PARALLEL)
	output_operand_lossage ("invalid %%N value");
      else
	fprintf (file, "%d", XVECLEN (x, 0) * 4);
      return;

    case 'O':
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
      if (GET_CODE (x) != MEM || GET_CODE (XEXP (x, 0)) != REG
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
      if (GET_CODE (x) != REG || ! CR_REGNO_P (REGNO (x)))
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

    case 'S':
      /* PowerPC64 mask position.  All 0's is excluded.
	 CONST_INT 32-bit mask is considered sign-extended so any
	 transition must occur within the CONST_INT, not on the boundary.  */
      if (! mask64_operand (x, DImode))
	output_operand_lossage ("invalid %%S value");

      uval = INTVAL (x);

      if (uval & 1)	/* Clear Left */
	{
#if HOST_BITS_PER_WIDE_INT > 64
	  uval &= ((unsigned HOST_WIDE_INT) 1 << 64) - 1;
#endif
	  i = 64;
	}
      else		/* Clear Right */
	{
	  uval = ~uval;
#if HOST_BITS_PER_WIDE_INT > 64
	  uval &= ((unsigned HOST_WIDE_INT) 1 << 64) - 1;
#endif
	  i = 63;
	}
      while (uval != 0)
	--i, uval >>= 1;
      gcc_assert (i >= 0);
      fprintf (file, "%d", i);
      return;

    case 't':
      /* Like 'J' but get to the OVERFLOW/UNORDERED bit.  */
      gcc_assert (REG_P (x) && GET_MODE (x) == CCmode);

      /* Bit 3 is OV bit.  */
      i = 4 * (REGNO (x) - CR0_REGNO) + 3;

      /* If we want bit 31, write a shift count of zero, not 32.  */
      fprintf (file, "%d", i == 31 ? 0 : i + 1);
      return;

    case 'T':
      /* Print the symbolic name of a branch target register.  */
      if (GET_CODE (x) != REG || (REGNO (x) != LR_REGNO
				  && REGNO (x) != CTR_REGNO))
	output_operand_lossage ("invalid %%T value");
      else if (REGNO (x) == LR_REGNO)
	fputs ("lr", file);
      else
	fputs ("ctr", file);
      return;

    case 'u':
      /* High-order 16 bits of constant for use in unsigned operand.  */
      if (! INT_P (x))
	output_operand_lossage ("invalid %%u value");
      else
	fprintf (file, HOST_WIDE_INT_PRINT_HEX,
		 (INTVAL (x) >> 16) & 0xffff);
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
	  gcc_unreachable ();
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

    case 'W':
      /* MB value for a PowerPC64 rldic operand.  */
      i = clz_hwi (INTVAL (x));

      fprintf (file, "%d", i);
      return;

    case 'x':
      /* X is a FPR or Altivec register used in a VSX context.  */
      if (GET_CODE (x) != REG || !VSX_REGNO_P (REGNO (x)))
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
	  if (GET_CODE (XEXP (x, 0)) == PRE_INC
	      || GET_CODE (XEXP (x, 0)) == PRE_DEC)
	    output_address (plus_constant (Pmode, XEXP (XEXP (x, 0), 0), 8));
	  else if (GET_CODE (XEXP (x, 0)) == PRE_MODIFY)
	    output_address (plus_constant (Pmode, XEXP (XEXP (x, 0), 0), 8));
	  else
	    output_address (XEXP (adjust_address_nv (x, SImode, 8), 0));
	  if (small_data_operand (x, GET_MODE (x)))
	    fprintf (file, "@%s(%s)", SMALL_DATA_RELOC,
		     reg_names[SMALL_DATA_REG]);
	}
      return;

    case 'z':
      /* X is a SYMBOL_REF.  Write out the name preceded by a
	 period and without any trailing data in brackets.  Used for function
	 names.  If we are configured for System V (or the embedded ABI) on
	 the PowerPC, do not emit the period, since those systems do not use
	 TOCs and the like.  */
      gcc_assert (GET_CODE (x) == SYMBOL_REF);

      /* For macho, check to see if we need a stub.  */
      if (TARGET_MACHO)
	{
	  const char *name = XSTR (x, 0);
#if TARGET_MACHO
	  if (darwin_emit_branch_islands
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
	  if (GET_CODE (XEXP (x, 0)) == PRE_INC
	      || GET_CODE (XEXP (x, 0)) == PRE_DEC)
	    output_address (plus_constant (Pmode, XEXP (XEXP (x, 0), 0), 12));
	  else if (GET_CODE (XEXP (x, 0)) == PRE_MODIFY)
	    output_address (plus_constant (Pmode, XEXP (XEXP (x, 0), 0), 12));
	  else
	    output_address (XEXP (adjust_address_nv (x, SImode, 12), 0));
	  if (small_data_operand (x, GET_MODE (x)))
	    fprintf (file, "@%s(%s)", SMALL_DATA_RELOC,
		     reg_names[SMALL_DATA_REG]);
	}
      return;

      /* Print AltiVec or SPE memory operand.  */
    case 'y':
      {
	rtx tmp;

	gcc_assert (MEM_P (x));

	tmp = XEXP (x, 0);

	/* Ugly hack because %y is overloaded.  */
	if ((TARGET_SPE || TARGET_E500_DOUBLE)
	    && (GET_MODE_SIZE (GET_MODE (x)) == 8
		|| GET_MODE (x) == TFmode
		|| GET_MODE (x) == TImode
		|| GET_MODE (x) == PTImode))
	  {
	    /* Handle [reg].  */
	    if (REG_P (tmp))
	      {
		fprintf (file, "0(%s)", reg_names[REGNO (tmp)]);
		break;
	      }
	    /* Handle [reg+UIMM].  */
	    else if (GET_CODE (tmp) == PLUS &&
		     GET_CODE (XEXP (tmp, 1)) == CONST_INT)
	      {
		int x;

		gcc_assert (REG_P (XEXP (tmp, 0)));

		x = INTVAL (XEXP (tmp, 1));
		fprintf (file, "%d(%s)", x, reg_names[REGNO (XEXP (tmp, 0))]);
		break;
	      }

	    /* Fall through.  Must be [reg+reg].  */
	  }
	if (VECTOR_MEM_ALTIVEC_P (GET_MODE (x))
	    && GET_CODE (tmp) == AND
	    && GET_CODE (XEXP (tmp, 1)) == CONST_INT
	    && INTVAL (XEXP (tmp, 1)) == -16)
	  tmp = XEXP (tmp, 0);
	else if (VECTOR_MEM_VSX_P (GET_MODE (x))
		 && GET_CODE (tmp) == PRE_MODIFY)
	  tmp = XEXP (tmp, 1);
	if (REG_P (tmp))
	  fprintf (file, "0,%s", reg_names[REGNO (tmp)]);
	else
	  {
	    if (!GET_CODE (tmp) == PLUS
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
	    output_address (XEXP (XEXP (x, 0), 1));
	  else
	    output_address (XEXP (x, 0));
	}
      else
	{
	  if (toc_relative_expr_p (x, false))
	    /* This hack along with a corresponding hack in
	       rs6000_output_addr_const_extra arranges to output addends
	       where the assembler expects to find them.  eg.
	       (plus (unspec [(symbol_ref ("x")) (reg 2)] tocrel) 4)
	       without this hack would be output as "x@toc+4".  We
	       want "x+4@toc".  */
	    output_addr_const (file, CONST_CAST_RTX (tocrel_base));
	  else
	    output_addr_const (file, x);
	}
      return;

    case '&':
      assemble_name (file, rs6000_get_some_local_dynamic_name ());
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
  else if (GET_CODE (x) == SYMBOL_REF || GET_CODE (x) == CONST
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
	   && GET_CODE (XEXP (x, 1)) == CONST_INT)
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
  else if (toc_relative_expr_p (x, false))
    {
      /* This hack along with a corresponding hack in
	 rs6000_output_addr_const_extra arranges to output addends
	 where the assembler expects to find them.  eg.
	 (lo_sum (reg 9)
	 .       (plus (unspec [(symbol_ref ("x")) (reg 2)] tocrel) 8))
	 without this hack would be output as "x@toc+8@l(9)".  We
	 want "x+8@toc@l(9)".  */
      output_addr_const (file, CONST_CAST_RTX (tocrel_base));
      if (GET_CODE (x) == LO_SUM)
	fprintf (file, "@l(%s)", reg_names[REGNO (XEXP (x, 0))]);
      else
	fprintf (file, "(%s)", reg_names[REGNO (XVECEXP (tocrel_base, 0, 1))]);
    }
  else
    gcc_unreachable ();
}

/* Implement TARGET_OUTPUT_ADDR_CONST_EXTRA.  */

static bool
rs6000_output_addr_const_extra (FILE *file, rtx x)
{
  if (GET_CODE (x) == UNSPEC)
    switch (XINT (x, 1))
      {
      case UNSPEC_TOCREL:
	gcc_checking_assert (GET_CODE (XVECEXP (x, 0, 0)) == SYMBOL_REF
			     && REG_P (XVECEXP (x, 0, 1))
			     && REGNO (XVECEXP (x, 0, 1)) == TOC_REGISTER);
	output_addr_const (file, XVECEXP (x, 0, 0));
	if (x == tocrel_base && tocrel_offset != const0_rtx)
	  {
	    if (INTVAL (tocrel_offset) >= 0)
	      fprintf (file, "+");
	    output_addr_const (file, CONST_CAST_RTX (tocrel_offset));
	  }
	if (!TARGET_AIX || (TARGET_ELF && TARGET_MINIMAL_TOC))
	  {
	    putc ('-', file);
	    assemble_name (file, toc_label_name);
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
      if (TARGET_RELOCATABLE
	  && in_section != toc_section
	  && !recurse
	  && GET_CODE (x) != CONST_INT
	  && GET_CODE (x) != CONST_DOUBLE
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
      else if (GET_CODE (x) == SYMBOL_REF
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
	NULL, "internal", "hidden", "protected"
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
rs6000_reverse_condition (enum machine_mode mode, enum rtx_code code)
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
rs6000_generate_compare (rtx cmp, enum machine_mode mode)
{
  enum machine_mode comp_mode;
  rtx compare_result;
  enum rtx_code code = GET_CODE (cmp);
  rtx op0 = XEXP (cmp, 0);
  rtx op1 = XEXP (cmp, 1);

  if (FLOAT_MODE_P (mode))
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
  if (comp_mode == CCUNSmode && GET_CODE (op1) == CONST_INT
      && INTVAL (op1) < 0)
    {
      op0 = copy_rtx_if_shared (op0);
      op1 = force_reg (GET_MODE (op0), op1);
      cmp = gen_rtx_fmt_ee (code, GET_MODE (cmp), op0, op1);
    }

  /* First, the compare.  */
  compare_result = gen_reg_rtx (comp_mode);

  /* E500 FP compare instructions on the GPRs.  Yuck!  */
  if ((!TARGET_FPRS && TARGET_HARD_FLOAT)
      && FLOAT_MODE_P (mode))
    {
      rtx cmp, or_result, compare_result2;
      enum machine_mode op_mode = GET_MODE (op0);
      bool reverse_p;

      if (op_mode == VOIDmode)
	op_mode = GET_MODE (op1);

      /* First reverse the condition codes that aren't directly supported.  */
      switch (code)
	{
	  case NE:
	  case UNLT:
	  case UNLE:
	  case UNGT:
	  case UNGE:
	    code = reverse_condition_maybe_unordered (code);
	    reverse_p = true;
	    break;

	  case EQ:
	  case LT:
	  case LE:
	  case GT:
	  case GE:
	    reverse_p = false;
	    break;

	  default:
	    gcc_unreachable ();
	}

      /* The E500 FP compare instructions toggle the GT bit (CR bit 1) only.
	 This explains the following mess.  */

      switch (code)
	{
	case EQ:
	  switch (op_mode)
	    {
	    case SFmode:
	      cmp = (flag_finite_math_only && !flag_trapping_math)
		? gen_tstsfeq_gpr (compare_result, op0, op1)
		: gen_cmpsfeq_gpr (compare_result, op0, op1);
	      break;

	    case DFmode:
	      cmp = (flag_finite_math_only && !flag_trapping_math)
		? gen_tstdfeq_gpr (compare_result, op0, op1)
		: gen_cmpdfeq_gpr (compare_result, op0, op1);
	      break;

	    case TFmode:
	      cmp = (flag_finite_math_only && !flag_trapping_math)
		? gen_tsttfeq_gpr (compare_result, op0, op1)
		: gen_cmptfeq_gpr (compare_result, op0, op1);
	      break;

	    default:
	      gcc_unreachable ();
	    }
	  break;

	case GT:
	case GE:
	  switch (op_mode)
	    {
	    case SFmode:
	      cmp = (flag_finite_math_only && !flag_trapping_math)
		? gen_tstsfgt_gpr (compare_result, op0, op1)
		: gen_cmpsfgt_gpr (compare_result, op0, op1);
	      break;

	    case DFmode:
	      cmp = (flag_finite_math_only && !flag_trapping_math)
		? gen_tstdfgt_gpr (compare_result, op0, op1)
		: gen_cmpdfgt_gpr (compare_result, op0, op1);
	      break;

	    case TFmode:
	      cmp = (flag_finite_math_only && !flag_trapping_math)
		? gen_tsttfgt_gpr (compare_result, op0, op1)
		: gen_cmptfgt_gpr (compare_result, op0, op1);
	      break;

	    default:
	      gcc_unreachable ();
	    }
	  break;

	case LT: 
	case LE:
	  switch (op_mode)
	    {
	    case SFmode:
	      cmp = (flag_finite_math_only && !flag_trapping_math)
		? gen_tstsflt_gpr (compare_result, op0, op1)
		: gen_cmpsflt_gpr (compare_result, op0, op1);
	      break;

	    case DFmode:
	      cmp = (flag_finite_math_only && !flag_trapping_math)
		? gen_tstdflt_gpr (compare_result, op0, op1)
		: gen_cmpdflt_gpr (compare_result, op0, op1);
	      break;

	    case TFmode:
	      cmp = (flag_finite_math_only && !flag_trapping_math)
		? gen_tsttflt_gpr (compare_result, op0, op1)
		: gen_cmptflt_gpr (compare_result, op0, op1);
	      break;

	    default:
	      gcc_unreachable ();
	    }
	  break;

        default:
          gcc_unreachable ();
	}

      /* Synthesize LE and GE from LT/GT || EQ.  */
      if (code == LE || code == GE)
	{
	  emit_insn (cmp);

	  compare_result2 = gen_reg_rtx (CCFPmode);

	  /* Do the EQ.  */
	  switch (op_mode)
	    {
	    case SFmode:
	      cmp = (flag_finite_math_only && !flag_trapping_math)
		? gen_tstsfeq_gpr (compare_result2, op0, op1)
		: gen_cmpsfeq_gpr (compare_result2, op0, op1);
	      break;

	    case DFmode:
	      cmp = (flag_finite_math_only && !flag_trapping_math)
		? gen_tstdfeq_gpr (compare_result2, op0, op1)
		: gen_cmpdfeq_gpr (compare_result2, op0, op1);
	      break;

	    case TFmode:
	      cmp = (flag_finite_math_only && !flag_trapping_math)
		? gen_tsttfeq_gpr (compare_result2, op0, op1)
		: gen_cmptfeq_gpr (compare_result2, op0, op1);
	      break;

	    default:
	      gcc_unreachable ();
	    }

	  emit_insn (cmp);

	  /* OR them together.  */
	  or_result = gen_reg_rtx (CCFPmode);
	  cmp = gen_e500_cr_ior_compare (or_result, compare_result,
					 compare_result2);
	  compare_result = or_result;
	}

      code = reverse_p ? NE : EQ;

      emit_insn (cmp);
    }
  else
    {
      /* Generate XLC-compatible TFmode compare as PARALLEL with extra
	 CLOBBERs to match cmptf_internal2 pattern.  */
      if (comp_mode == CCFPmode && TARGET_XL_COMPAT
	  && GET_MODE (op0) == TFmode
	  && !TARGET_IEEEQUAD
	  && TARGET_HARD_FLOAT && TARGET_FPRS && TARGET_LONG_DOUBLE_128)
	emit_insn (gen_rtx_PARALLEL (VOIDmode,
	  gen_rtvec (10,
		     gen_rtx_SET (VOIDmode,
				  compare_result,
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
	emit_insn (gen_rtx_SET (VOIDmode, compare_result,
				gen_rtx_COMPARE (comp_mode, op0, op1)));
    }

  /* Some kinds of FP comparisons need an OR operation;
     under flag_finite_math_only we don't bother.  */
  if (FLOAT_MODE_P (mode)
      && !flag_finite_math_only
      && !(TARGET_HARD_FLOAT && !TARGET_FPRS)
      && (code == LE || code == GE
	  || code == UNEQ || code == LTGT
	  || code == UNGT || code == UNLT))
    {
      enum rtx_code or1, or2;
      rtx or1_rtx, or2_rtx, compare2_rtx;
      rtx or_result = gen_reg_rtx (CCEQmode);

      switch (code)
	{
	case LE: or1 = LT;  or2 = EQ;  break;
	case GE: or1 = GT;  or2 = EQ;  break;
	case UNEQ: or1 = UNORDERED;  or2 = EQ;  break;
	case LTGT: or1 = LT;  or2 = GT;  break;
	case UNGT: or1 = UNORDERED;  or2 = GT;  break;
	case UNLT: or1 = UNORDERED;  or2 = LT;  break;
	default:  gcc_unreachable ();
	}
      validate_condition_mode (or1, comp_mode);
      validate_condition_mode (or2, comp_mode);
      or1_rtx = gen_rtx_fmt_ee (or1, SImode, compare_result, const0_rtx);
      or2_rtx = gen_rtx_fmt_ee (or2, SImode, compare_result, const0_rtx);
      compare2_rtx = gen_rtx_COMPARE (CCEQmode,
				      gen_rtx_IOR (SImode, or1_rtx, or2_rtx),
				      const_true_rtx);
      emit_insn (gen_rtx_SET (VOIDmode, or_result, compare2_rtx));

      compare_result = or_result;
      code = EQ;
    }

  validate_condition_mode (code, GET_MODE (compare_result));

  return gen_rtx_fmt_ee (code, VOIDmode, compare_result, const0_rtx);
}


/* Emit the RTL for an sISEL pattern.  */

void
rs6000_emit_sISEL (enum machine_mode mode ATTRIBUTE_UNUSED, rtx operands[])
{
  rs6000_emit_int_cmove (operands[0], operands[1], const1_rtx, const0_rtx);
}

void
rs6000_emit_sCOND (enum machine_mode mode, rtx operands[])
{
  rtx condition_rtx;
  enum machine_mode op_mode;
  enum rtx_code cond_code;
  rtx result = operands[0];

  if (TARGET_ISEL && (mode == SImode || mode == DImode))
    {
      rs6000_emit_sISEL (mode, operands);
      return;
    }

  condition_rtx = rs6000_generate_compare (operands[1], mode);
  cond_code = GET_CODE (condition_rtx);

  if (FLOAT_MODE_P (mode)
      && !TARGET_FPRS && TARGET_HARD_FLOAT)
    {
      rtx t;

      PUT_MODE (condition_rtx, SImode);
      t = XEXP (condition_rtx, 0);

      gcc_assert (cond_code == NE || cond_code == EQ);

      if (cond_code == NE)
	emit_insn (gen_e500_flip_gt_bit (t, t));

      emit_insn (gen_move_from_CR_gt_bit (result, t));
      return;
    }

  if (cond_code == NE
      || cond_code == GE || cond_code == LE
      || cond_code == GEU || cond_code == LEU
      || cond_code == ORDERED || cond_code == UNGE || cond_code == UNLE)
    {
      rtx not_result = gen_reg_rtx (CCEQmode);
      rtx not_op, rev_cond_rtx;
      enum machine_mode cc_mode;

      cc_mode = GET_MODE (XEXP (condition_rtx, 0));

      rev_cond_rtx = gen_rtx_fmt_ee (rs6000_reverse_condition (cc_mode, cond_code),
				     SImode, XEXP (condition_rtx, 0), const0_rtx);
      not_op = gen_rtx_COMPARE (CCEQmode, rev_cond_rtx, const0_rtx);
      emit_insn (gen_rtx_SET (VOIDmode, not_result, not_op));
      condition_rtx = gen_rtx_EQ (VOIDmode, not_result, const0_rtx);
    }

  op_mode = GET_MODE (XEXP (operands[1], 0));
  if (op_mode == VOIDmode)
    op_mode = GET_MODE (XEXP (operands[1], 1));

  if (TARGET_POWERPC64 && (op_mode == DImode || FLOAT_MODE_P (mode)))
    {
      PUT_MODE (condition_rtx, DImode);
      convert_move (result, condition_rtx, 0);
    }
  else
    {
      PUT_MODE (condition_rtx, SImode);
      emit_insn (gen_rtx_SET (VOIDmode, result, condition_rtx));
    }
}

/* Emit a branch of kind CODE to location LOC.  */

void
rs6000_emit_cbranch (enum machine_mode mode, rtx operands[])
{
  rtx condition_rtx, loc_ref;

  condition_rtx = rs6000_generate_compare (operands[0], mode);
  loc_ref = gen_rtx_LABEL_REF (VOIDmode, operands[3]);
  emit_jump_insn (gen_rtx_SET (VOIDmode, pc_rtx,
			       gen_rtx_IF_THEN_ELSE (VOIDmode, condition_rtx,
						     loc_ref, pc_rtx)));
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
output_cbranch (rtx op, const char *label, int reversed, rtx insn)
{
  static char string[64];
  enum rtx_code code = GET_CODE (op);
  rtx cc_reg = XEXP (op, 0);
  enum machine_mode mode = GET_MODE (cc_reg);
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

  if ((!TARGET_FPRS && TARGET_HARD_FLOAT) && mode == CCFPmode)
    {
      /* The efscmp/tst* instructions twiddle bit 2, which maps nicely
	 to the GT bit.  */
      switch (code)
	{
	case EQ:
	  /* Opposite of GT.  */
	  code = GT;
	  break;

	case NE:
	  code = UNLE;
	  break;

	default:
	  gcc_unreachable ();
	}
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
      int prob = XINT (note, 0) - REG_BR_PROB_BASE / 2;

      /* Only hint for highly probable/improbable branches on newer
	 cpus as static prediction overrides processor dynamic
	 prediction.  For older cpus we may as well always hint, but
	 assume not taken for branches that are very close to 50% as a
	 mispredicted taken branch is more expensive than a
	 mispredicted not-taken branch.  */
      if (rs6000_always_hint
	  || (abs (prob) > REG_BR_PROB_BASE / 100 * 48
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

/* Return the string to flip the GT bit on a CR.  */
char *
output_e500_flip_gt_bit (rtx dst, rtx src)
{
  static char string[64];
  int a, b;

  gcc_assert (GET_CODE (dst) == REG && CR_REGNO_P (REGNO (dst))
	      && GET_CODE (src) == REG && CR_REGNO_P (REGNO (src)));

  /* GT bit.  */
  a = 4 * (REGNO (dst) - CR0_REGNO) + 1;
  b = 4 * (REGNO (src) - CR0_REGNO) + 1;

  sprintf (string, "crnot %d,%d", a, b);
  return string;
}

/* Return insn for VSX or Altivec comparisons.  */

static rtx
rs6000_emit_vector_compare_inner (enum rtx_code code, rtx op0, rtx op1)
{
  rtx mask;
  enum machine_mode mode = GET_MODE (op0);

  switch (code)
    {
    default:
      break;

    case GE:
      if (GET_MODE_CLASS (mode) == MODE_VECTOR_INT)
	return NULL_RTX;

    case EQ:
    case GT:
    case GTU:
    case ORDERED:
    case UNORDERED:
    case UNEQ:
    case LTGT:
      mask = gen_reg_rtx (mode);
      emit_insn (gen_rtx_SET (VOIDmode,
			      mask,
			      gen_rtx_fmt_ee (code, mode, op0, op1)));
      return mask;
    }

  return NULL_RTX;
}

/* Emit vector compare for operands OP0 and OP1 using code RCODE.
   DMODE is expected destination mode. This is a recursive function.  */

static rtx
rs6000_emit_vector_compare (enum rtx_code rcode,
			    rtx op0, rtx op1,
			    enum machine_mode dmode)
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
	{
	  rtx tmp;
	  tmp = op0;
	  op0 = op1;
	  op1 = tmp;
	}

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
  enum machine_mode dest_mode = GET_MODE (dest);
  enum machine_mode mask_mode = GET_MODE (cc_op0);
  enum rtx_code rcode = GET_CODE (cond);
  enum machine_mode cc_mode = CCmode;
  rtx mask;
  rtx cond2;
  rtx tmp;
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

      /* Mark unsigned tests with CCUNSmode.  */
    case GTU:
    case GEU:
    case LTU:
    case LEU:
      cc_mode = CCUNSmode;
      break;

    default:
      break;
    }

  /* Get the vector mask for the given relational operations.  */
  mask = rs6000_emit_vector_compare (rcode, cc_op0, cc_op1, mask_mode);

  if (!mask)
    return 0;

  if (invert_move)
    {
      tmp = op_true;
      op_true = op_false;
      op_false = tmp;
    }

  cond2 = gen_rtx_fmt_ee (NE, cc_mode, gen_lowpart (dest_mode, mask),
			  CONST0_RTX (dest_mode));
  emit_insn (gen_rtx_SET (VOIDmode,
			  dest,
			  gen_rtx_IF_THEN_ELSE (dest_mode,
						cond2,
						op_true,
						op_false)));
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
  REAL_VALUE_TYPE c1;
  enum machine_mode compare_mode = GET_MODE (op0);
  enum machine_mode result_mode = GET_MODE (dest);
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
  else if (TARGET_HARD_FLOAT && !TARGET_FPRS
	   && SCALAR_FLOAT_MODE_P (compare_mode))
    return 0;

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

  if (GET_CODE (op1) == CONST_DOUBLE)
    REAL_VALUE_FROM_CONST_DOUBLE (c1, op1);

  /* We're going to try to implement comparisons by performing
     a subtract, then comparing against zero.  Unfortunately,
     Inf - Inf is NaN which is not zero, and so if we don't
     know that the operand is finite and the comparison
     would treat EQ different to UNORDERED, we can't do it.  */
  if (HONOR_INFINITIES (compare_mode)
      && code != GT && code != UNGE
      && (GET_CODE (op1) != CONST_DOUBLE || real_isinf (&c1))
      /* Constructs of the form (a OP b ? a : b) are safe.  */
      && ((! rtx_equal_p (op0, false_cond) && ! rtx_equal_p (op1, false_cond))
	  || (! rtx_equal_p (op0, true_cond)
	      && ! rtx_equal_p (op1, true_cond))))
    return 0;

  /* At this point we know we can use fsel.  */

  /* Reduce the comparison to a comparison against zero.  */
  if (! is_against_zero)
    {
      temp = gen_reg_rtx (compare_mode);
      emit_insn (gen_rtx_SET (VOIDmode, temp,
			      gen_rtx_MINUS (compare_mode, op0, op1)));
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
      emit_insn (gen_rtx_SET (VOIDmode, temp, gen_rtx_NEG (compare_mode, op0)));
      op0 = temp;
      break;

    case ORDERED:
      temp = gen_reg_rtx (compare_mode);
      emit_insn (gen_rtx_SET (VOIDmode, temp, gen_rtx_ABS (compare_mode, op0)));
      op0 = temp;
      break;

    case EQ:
      temp = gen_reg_rtx (compare_mode);
      emit_insn (gen_rtx_SET (VOIDmode, temp,
			      gen_rtx_NEG (compare_mode,
					   gen_rtx_ABS (compare_mode, op0))));
      op0 = temp;
      break;

    case UNGE:
      /* a UNGE 0 <-> (a GE 0 || -a UNLT 0) */
      temp = gen_reg_rtx (result_mode);
      emit_insn (gen_rtx_SET (VOIDmode, temp,
			      gen_rtx_IF_THEN_ELSE (result_mode,
						    gen_rtx_GE (VOIDmode,
								op0, op1),
						    true_cond, false_cond)));
      false_cond = true_cond;
      true_cond = temp;

      temp = gen_reg_rtx (compare_mode);
      emit_insn (gen_rtx_SET (VOIDmode, temp, gen_rtx_NEG (compare_mode, op0)));
      op0 = temp;
      break;

    case GT:
      /* a GT 0 <-> (a GE 0 && -a UNLT 0) */
      temp = gen_reg_rtx (result_mode);
      emit_insn (gen_rtx_SET (VOIDmode, temp,
			      gen_rtx_IF_THEN_ELSE (result_mode,
						    gen_rtx_GE (VOIDmode,
								op0, op1),
						    true_cond, false_cond)));
      true_cond = false_cond;
      false_cond = temp;

      temp = gen_reg_rtx (compare_mode);
      emit_insn (gen_rtx_SET (VOIDmode, temp, gen_rtx_NEG (compare_mode, op0)));
      op0 = temp;
      break;

    default:
      gcc_unreachable ();
    }

  emit_insn (gen_rtx_SET (VOIDmode, dest,
			  gen_rtx_IF_THEN_ELSE (result_mode,
						gen_rtx_GE (VOIDmode,
							    op0, op1),
						true_cond, false_cond)));
  return 1;
}

/* Same as above, but for ints (isel).  */

static int
rs6000_emit_int_cmove (rtx dest, rtx op, rtx true_cond, rtx false_cond)
{
  rtx condition_rtx, cr;
  enum machine_mode mode = GET_MODE (dest);
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
	rtx t = true_cond;
	true_cond = false_cond;
	false_cond = t;
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

const char *
output_isel (rtx *operands)
{
  enum rtx_code code;

  code = GET_CODE (operands[1]);

  if (code == GE || code == GEU || code == LE || code == LEU || code == NE)
    {
      gcc_assert (GET_CODE (operands[2]) == REG
		  && GET_CODE (operands[3]) == REG);
      PUT_CODE (operands[1], reverse_condition (code));
      return "isel %0,%3,%2,%j1";
    }

  return "isel %0,%2,%3,%j1";
}

void
rs6000_emit_minmax (rtx dest, enum rtx_code code, rtx op0, rtx op1)
{
  enum machine_mode mode = GET_MODE (op0);
  enum rtx_code c;
  rtx target;

  /* VSX/altivec have direct min/max insns.  */
  if ((code == SMAX || code == SMIN)
      && (VECTOR_UNIT_ALTIVEC_OR_VSX_P (mode)
	  || (mode == SFmode && VECTOR_UNIT_VSX_P (DFmode))))
    {
      emit_insn (gen_rtx_SET (VOIDmode,
			      dest,
			      gen_rtx_fmt_ee (code, mode, op0, op1)));
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
  int very_unlikely = REG_BR_PROB_BASE / 100 - 1;
  rtx x;

  x = gen_rtx_IF_THEN_ELSE (VOIDmode, cond, label, pc_rtx);
  x = emit_jump_insn (gen_rtx_SET (VOIDmode, pc_rtx, x));
  add_int_reg_note (x, REG_BR_PROB, very_unlikely);
}

/* A subroutine of the atomic operation splitters.  Emit a load-locked
   instruction in MODE.  For QI/HImode, possibly use a pattern than includes
   the zero_extend operation.  */

static void
emit_load_locked (enum machine_mode mode, rtx reg, rtx mem)
{
  rtx (*fn) (rtx, rtx) = NULL;

  switch (mode)
    {
    case QImode:
      fn = gen_load_lockedqi;
      break;
    case HImode:
      fn = gen_load_lockedhi;
      break;
    case SImode:
      if (GET_MODE (mem) == QImode)
	fn = gen_load_lockedqi_si;
      else if (GET_MODE (mem) == HImode)
	fn = gen_load_lockedhi_si;
      else
	fn = gen_load_lockedsi;
      break;
    case DImode:
      fn = gen_load_lockeddi;
      break;
    case TImode:
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
emit_store_conditional (enum machine_mode mode, rtx res, rtx mem, rtx val)
{
  rtx (*fn) (rtx, rtx, rtx) = NULL;

  switch (mode)
    {
    case QImode:
      fn = gen_store_conditionalqi;
      break;
    case HImode:
      fn = gen_store_conditionalhi;
      break;
    case SImode:
      fn = gen_store_conditionalsi;
      break;
    case DImode:
      fn = gen_store_conditionaldi;
      break;
    case TImode:
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
  int strict_p = (reload_in_progress || reload_completed);

  if (!legitimate_indirect_address_p (addr, strict_p)
      && !legitimate_indexed_address_p (addr, strict_p))
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
  enum machine_mode mode = GET_MODE (orig_mem);

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
  emit_insn (gen_rlwinm (shift, addr, GEN_INT (3), GEN_INT (shift_mask)));
  if (WORDS_BIG_ENDIAN)
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
  emit_insn (gen_rtx_SET (VOIDmode, x,
			  gen_rtx_AND (SImode,
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
  enum machine_mode mode, orig_mode;
  enum memmodel mod_s, mod_f;
  bool is_weak;

  boolval = operands[0];
  retval = operands[1];
  mem = operands[2];
  oldval = operands[3];
  newval = operands[4];
  is_weak = (INTVAL (operands[5]) != 0);
  mod_s = (enum memmodel) INTVAL (operands[6]);
  mod_f = (enum memmodel) INTVAL (operands[7]);
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
    {
      x = expand_simple_binop (SImode, AND, retval, mask,
			       NULL_RTX, 1, OPTAB_LIB_WIDEN);
    }

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

  emit_insn (gen_rtx_SET (VOIDmode, cond, x));

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

  if (mod_f != MEMMODEL_RELAXED)
    emit_label (XEXP (label2, 0));

  rs6000_post_atomic_barrier (mod_s);

  if (mod_f == MEMMODEL_RELAXED)
    emit_label (XEXP (label2, 0));

  if (shift)
    rs6000_finish_atomic_subword (operands[1], retval, shift);
  else if (mode != GET_MODE (operands[1]))
    convert_move (operands[1], retval, 1);

  /* In all cases, CR0 contains EQ on success, and NE on failure.  */
  x = gen_rtx_EQ (SImode, cond, const0_rtx);
  emit_insn (gen_rtx_SET (VOIDmode, boolval, x));
}

/* Expand an atomic exchange operation.  */

void
rs6000_expand_atomic_exchange (rtx operands[])
{
  rtx retval, mem, val, cond;
  enum machine_mode mode;
  enum memmodel model;
  rtx label, x, mask, shift;

  retval = operands[0];
  mem = operands[1];
  val = operands[2];
  model = (enum memmodel) INTVAL (operands[3]);
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
  enum memmodel model = (enum memmodel) INTVAL (model_rtx);
  enum machine_mode mode = GET_MODE (mem);
  enum machine_mode store_mode = mode;
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
	      emit_insn (gen_rtx_SET (VOIDmode, val, x));
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
  enum machine_mode mode;
  /* The mode that the move is being done in, and its size.  */
  enum machine_mode reg_mode;
  int reg_mode_size;
  /* The number of registers that will be moved.  */
  int nregs;

  reg = REG_P (dst) ? REGNO (dst) : REGNO (src);
  mode = GET_MODE (dst);
  nregs = hard_regno_nregs[reg][mode];
  if (FP_REGNO_P (reg))
    reg_mode = DECIMAL_FLOAT_MODE_P (mode) ? DDmode : 
	((TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT) ? DFmode : SFmode);
  else if (ALTIVEC_REGNO_P (reg))
    reg_mode = V16QImode;
  else if (TARGET_E500_DOUBLE && mode == TFmode)
    reg_mode = DFmode;
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

	  emit_insn (gen_rtx_SET (VOIDmode, p_dst, p_src));
	}

      return;
    }

  if (REG_P (src) && REG_P (dst) && (REGNO (src) < REGNO (dst)))
    {
      /* Move register range backwards, if we might have destructive
	 overlap.  */
      int i;
      for (i = nregs - 1; i >= 0; i--)
	emit_insn (gen_rtx_SET (VOIDmode,
				simplify_gen_subreg (reg_mode, dst, mode,
						     i * reg_mode_size),
				simplify_gen_subreg (reg_mode, src, mode,
						     i * reg_mode_size)));
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
	  else if (! rs6000_offsettable_memref_p (src, reg_mode))
	    {
	      if (GET_CODE (XEXP (src, 0)) == PRE_MODIFY)
		{
		  rtx basereg = XEXP (XEXP (src, 0), 0);
		  if (TARGET_UPDATE)
		    {
		      rtx ndst = simplify_gen_subreg (reg_mode, dst, mode, 0);
		      emit_insn (gen_rtx_SET (VOIDmode, ndst,
				 gen_rtx_MEM (reg_mode, XEXP (src, 0))));
		      used_update = true;
		    }
		  else
		    emit_insn (gen_rtx_SET (VOIDmode, basereg,
			       XEXP (XEXP (src, 0), 1)));
		  src = replace_equiv_address (src, basereg);
		}
	      else
		{
		  rtx basereg = gen_rtx_REG (Pmode, reg);
		  emit_insn (gen_rtx_SET (VOIDmode, basereg, XEXP (src, 0)));
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
				: gen_movsi_update (breg, breg, delta_rtx, nsrc))
			     : gen_movdi_di_update (breg, breg, delta_rtx, nsrc));
		  used_update = true;
		}
	      else
		emit_insn (gen_add3_insn (breg, breg, delta_rtx));
	      dst = replace_equiv_address (dst, breg);
	    }
	  else if (!rs6000_offsettable_memref_p (dst, reg_mode)
		   && GET_CODE (XEXP (dst, 0)) != LO_SUM)
	    {
	      if (GET_CODE (XEXP (dst, 0)) == PRE_MODIFY)
		{
		  rtx basereg = XEXP (XEXP (dst, 0), 0);
		  if (TARGET_UPDATE)
		    {
		      rtx nsrc = simplify_gen_subreg (reg_mode, src, mode, 0);
		      emit_insn (gen_rtx_SET (VOIDmode,
				 gen_rtx_MEM (reg_mode, XEXP (dst, 0)), nsrc));
		      used_update = true;
		    }
		  else
		    emit_insn (gen_rtx_SET (VOIDmode, basereg,
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
	    gcc_assert (rs6000_offsettable_memref_p (dst, reg_mode));
	}

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

	  emit_insn (gen_rtx_SET (VOIDmode,
				  simplify_gen_subreg (reg_mode, dst, mode,
						       j * reg_mode_size),
				  simplify_gen_subreg (reg_mode, src, mode,
						       j * reg_mode_size)));
	}
      if (restore_basereg != NULL_RTX)
	emit_insn (restore_basereg);
    }
}


/* This page contains routines that are used to determine what the
   function prologue and epilogue code will do and write them out.  */

static inline bool
save_reg_p (int r)
{
  return !call_used_regs[r] && df_regs_ever_live_p (r);
}

/* Return the first fixed-point register that is required to be
   saved. 32 if none.  */

int
first_reg_to_save (void)
{
  int first_reg;

  /* Find lowest numbered live register.  */
  for (first_reg = 13; first_reg <= 31; first_reg++)
    if (save_reg_p (first_reg))
      break;

  if (first_reg > RS6000_PIC_OFFSET_TABLE_REGNUM
      && ((DEFAULT_ABI == ABI_V4 && flag_pic != 0)
	  || (DEFAULT_ABI == ABI_DARWIN && flag_pic)
	  || (TARGET_TOC && TARGET_MINIMAL_TOC))
      && df_regs_ever_live_p (RS6000_PIC_OFFSET_TABLE_REGNUM))
    first_reg = RS6000_PIC_OFFSET_TABLE_REGNUM;

#if TARGET_MACHO
  if (flag_pic
      && crtl->uses_pic_offset_table
      && first_reg > RS6000_PIC_OFFSET_TABLE_REGNUM)
    return RS6000_PIC_OFFSET_TABLE_REGNUM;
#endif

  return first_reg;
}

/* Similar, for FP regs.  */

int
first_fp_reg_to_save (void)
{
  int first_reg;

  /* Find lowest numbered live register.  */
  for (first_reg = 14 + 32; first_reg <= 63; first_reg++)
    if (save_reg_p (first_reg))
      break;

  return first_reg;
}

/* Similar, for AltiVec regs.  */

static int
first_altivec_reg_to_save (void)
{
  int i;

  /* Stack frame remains as is unless we are in AltiVec ABI.  */
  if (! TARGET_ALTIVEC_ABI)
    return LAST_ALTIVEC_REGNO + 1;

  /* On Darwin, the unwind routines are compiled without
     TARGET_ALTIVEC, and use save_world to save/restore the
     altivec registers when necessary.  */
  if (DEFAULT_ABI == ABI_DARWIN && crtl->calls_eh_return
      && ! TARGET_ALTIVEC)
    return FIRST_ALTIVEC_REGNO + 20;

  /* Find lowest numbered live register.  */
  for (i = FIRST_ALTIVEC_REGNO + 20; i <= LAST_ALTIVEC_REGNO; ++i)
    if (save_reg_p (i))
      break;

  return i;
}

/* Return a 32-bit mask of the AltiVec registers we need to set in
   VRSAVE.  Bit n of the return value is 1 if Vn is live.  The MSB in
   the 32-bit word is 0.  */

static unsigned int
compute_vrsave_mask (void)
{
  unsigned int i, mask = 0;

  /* On Darwin, the unwind routines are compiled without
     TARGET_ALTIVEC, and use save_world to save/restore the
     call-saved altivec registers when necessary.  */
  if (DEFAULT_ABI == ABI_DARWIN && crtl->calls_eh_return
      && ! TARGET_ALTIVEC)
    mask |= 0xFFF;

  /* First, find out if we use _any_ altivec registers.  */
  for (i = FIRST_ALTIVEC_REGNO; i <= LAST_ALTIVEC_REGNO; ++i)
    if (df_regs_ever_live_p (i))
      mask |= ALTIVEC_REG_BIT (i);

  if (mask == 0)
    return mask;

  /* Next, remove the argument registers from the set.  These must
     be in the VRSAVE mask set by the caller, so we don't need to add
     them in again.  More importantly, the mask we compute here is
     used to generate CLOBBERs in the set_vrsave insn, and we do not
     wish the argument registers to die.  */
  for (i = crtl->args.info.vregno - 1; i >= ALTIVEC_ARG_MIN_REG; --i)
    mask &= ~ALTIVEC_REG_BIT (i);

  /* Similarly, remove the return value from the set.  */
  {
    bool yes = false;
    diddle_return_value (is_altivec_return_reg, &yes);
    if (yes)
      mask &= ~ALTIVEC_REG_BIT (ALTIVEC_ARG_RETURN);
  }

  return mask;
}

/* For a very restricted set of circumstances, we can cut down the
   size of prologues/epilogues by calling our own save/restore-the-world
   routines.  */

static void
compute_save_world_info (rs6000_stack_t *info_ptr)
{
  info_ptr->world_save_p = 1;
  info_ptr->world_save_p
    = (WORLD_SAVE_P (info_ptr)
       && DEFAULT_ABI == ABI_DARWIN
       && !cfun->has_nonlocal_label
       && info_ptr->first_fp_reg_save == FIRST_SAVED_FP_REGNO
       && info_ptr->first_gp_reg_save == FIRST_SAVED_GP_REGNO
       && info_ptr->first_altivec_reg_save == FIRST_SAVED_ALTIVEC_REGNO
       && info_ptr->cr_save_p);

  /* This will not work in conjunction with sibcalls.  Make sure there
     are none.  (This check is expensive, but seldom executed.) */
  if (WORLD_SAVE_P (info_ptr))
    {
      rtx insn;
      for (insn = get_last_insn_anywhere (); insn; insn = PREV_INSN (insn))
	if (CALL_P (insn) && SIBLING_CALL_P (insn))
	  {
	    info_ptr->world_save_p = 0;
	    break;
	  }
    }

  if (WORLD_SAVE_P (info_ptr))
    {
      /* Even if we're not touching VRsave, make sure there's room on the
	 stack for it, if it looks like we're calling SAVE_WORLD, which
	 will attempt to save it. */
      info_ptr->vrsave_size  = 4;

      /* If we are going to save the world, we need to save the link register too.  */
      info_ptr->lr_save_p = 1;

      /* "Save" the VRsave register too if we're saving the world.  */
      if (info_ptr->vrsave_mask == 0)
	info_ptr->vrsave_mask = compute_vrsave_mask ();

      /* Because the Darwin register save/restore routines only handle
	 F14 .. F31 and V20 .. V31 as per the ABI, perform a consistency
	 check.  */
      gcc_assert (info_ptr->first_fp_reg_save >= FIRST_SAVED_FP_REGNO
		  && (info_ptr->first_altivec_reg_save
		      >= FIRST_SAVED_ALTIVEC_REGNO));
    }
  return;
}


static void
is_altivec_return_reg (rtx reg, void *xyes)
{
  bool *yes = (bool *) xyes;
  if (REGNO (reg) == ALTIVEC_ARG_RETURN)
    *yes = true;
}


/* Look for user-defined global regs in the range FIRST to LAST-1.
   We should not restore these, and so cannot use lmw or out-of-line
   restore functions if there are any.  We also can't save them
   (well, emit frame notes for them), because frame unwinding during
   exception handling will restore saved registers.  */

static bool
global_regs_p (unsigned first, unsigned last)
{
  while (first < last)
    if (global_regs[first++])
      return true;
  return false;
}

/* Determine the strategy for savings/restoring registers.  */

enum {
  SAVRES_MULTIPLE = 0x1,
  SAVE_INLINE_FPRS = 0x2,
  SAVE_INLINE_GPRS = 0x4,
  REST_INLINE_FPRS = 0x8,
  REST_INLINE_GPRS = 0x10,
  SAVE_NOINLINE_GPRS_SAVES_LR = 0x20,
  SAVE_NOINLINE_FPRS_SAVES_LR = 0x40,
  REST_NOINLINE_FPRS_DOESNT_RESTORE_LR = 0x80,
  SAVE_INLINE_VRS = 0x100,
  REST_INLINE_VRS = 0x200
};

static int
rs6000_savres_strategy (rs6000_stack_t *info,
			bool using_static_chain_p)
{
  int strategy = 0;
  bool lr_save_p;

  if (TARGET_MULTIPLE
      && !TARGET_POWERPC64
      && !(TARGET_SPE_ABI && info->spe_64bit_regs_used)
      && info->first_gp_reg_save < 31
      && !global_regs_p (info->first_gp_reg_save, 32))
    strategy |= SAVRES_MULTIPLE;

  if (crtl->calls_eh_return
      || cfun->machine->ra_need_lr)
    strategy |= (SAVE_INLINE_FPRS | REST_INLINE_FPRS
		 | SAVE_INLINE_GPRS | REST_INLINE_GPRS
		 | SAVE_INLINE_VRS | REST_INLINE_VRS);

  if (info->first_fp_reg_save == 64
      /* The out-of-line FP routines use double-precision stores;
	 we can't use those routines if we don't have such stores.  */
      || (TARGET_HARD_FLOAT && !TARGET_DOUBLE_FLOAT)
      || global_regs_p (info->first_fp_reg_save, 64))
    strategy |= SAVE_INLINE_FPRS | REST_INLINE_FPRS;

  if (info->first_gp_reg_save == 32
      || (!(strategy & SAVRES_MULTIPLE)
	  && global_regs_p (info->first_gp_reg_save, 32)))
    strategy |= SAVE_INLINE_GPRS | REST_INLINE_GPRS;

  if (info->first_altivec_reg_save == LAST_ALTIVEC_REGNO + 1
      || global_regs_p (info->first_altivec_reg_save, LAST_ALTIVEC_REGNO + 1))
    strategy |= SAVE_INLINE_VRS | REST_INLINE_VRS;

  /* Define cutoff for using out-of-line functions to save registers.  */
  if (DEFAULT_ABI == ABI_V4 || TARGET_ELF)
    {
      if (!optimize_size)
	{
	  strategy |= SAVE_INLINE_FPRS | REST_INLINE_FPRS;
	  strategy |= SAVE_INLINE_GPRS | REST_INLINE_GPRS;
	  strategy |= SAVE_INLINE_VRS | REST_INLINE_VRS;
	}
      else
	{
	  /* Prefer out-of-line restore if it will exit.  */
	  if (info->first_fp_reg_save > 61)
	    strategy |= SAVE_INLINE_FPRS;
	  if (info->first_gp_reg_save > 29)
	    {
	      if (info->first_fp_reg_save == 64)
		strategy |= SAVE_INLINE_GPRS;
	      else
		strategy |= SAVE_INLINE_GPRS | REST_INLINE_GPRS;
	    }
	  if (info->first_altivec_reg_save == LAST_ALTIVEC_REGNO)
	    strategy |= SAVE_INLINE_VRS | REST_INLINE_VRS;
	}
    }
  else if (DEFAULT_ABI == ABI_DARWIN)
    {
      if (info->first_fp_reg_save > 60)
	strategy |= SAVE_INLINE_FPRS | REST_INLINE_FPRS;
      if (info->first_gp_reg_save > 29)
	strategy |= SAVE_INLINE_GPRS | REST_INLINE_GPRS;
      strategy |= SAVE_INLINE_VRS | REST_INLINE_VRS;
    }
  else
    {
      gcc_checking_assert (DEFAULT_ABI == ABI_AIX || DEFAULT_ABI == ABI_ELFv2);
      if (info->first_fp_reg_save > 61)
	strategy |= SAVE_INLINE_FPRS | REST_INLINE_FPRS;
      strategy |= SAVE_INLINE_GPRS | REST_INLINE_GPRS;
      strategy |= SAVE_INLINE_VRS | REST_INLINE_VRS;
    }

  /* Don't bother to try to save things out-of-line if r11 is occupied
     by the static chain.  It would require too much fiddling and the
     static chain is rarely used anyway.  FPRs are saved w.r.t the stack
     pointer on Darwin, and AIX uses r1 or r12.  */
  if (using_static_chain_p
      && (DEFAULT_ABI == ABI_V4 || DEFAULT_ABI == ABI_DARWIN))
    strategy |= ((DEFAULT_ABI == ABI_DARWIN ? 0 : SAVE_INLINE_FPRS)
		 | SAVE_INLINE_GPRS
		 | SAVE_INLINE_VRS | REST_INLINE_VRS);

  /* We can only use the out-of-line routines to restore if we've
     saved all the registers from first_fp_reg_save in the prologue.
     Otherwise, we risk loading garbage.  */
  if ((strategy & (SAVE_INLINE_FPRS | REST_INLINE_FPRS)) == SAVE_INLINE_FPRS)
    {
      int i;

      for (i = info->first_fp_reg_save; i < 64; i++)
	if (!save_reg_p (i))
	  {
	    strategy |= REST_INLINE_FPRS;
	    break;
	  }
    }

  /* If we are going to use store multiple, then don't even bother
     with the out-of-line routines, since the store-multiple
     instruction will always be smaller.  */
  if ((strategy & SAVRES_MULTIPLE))
    strategy |= SAVE_INLINE_GPRS;

  /* info->lr_save_p isn't yet set if the only reason lr needs to be
     saved is an out-of-line save or restore.  Set up the value for
     the next test (excluding out-of-line gpr restore).  */
  lr_save_p = (info->lr_save_p
	       || !(strategy & SAVE_INLINE_GPRS)
	       || !(strategy & SAVE_INLINE_FPRS)
	       || !(strategy & SAVE_INLINE_VRS)
	       || !(strategy & REST_INLINE_FPRS)
	       || !(strategy & REST_INLINE_VRS));

  /* The situation is more complicated with load multiple.  We'd
     prefer to use the out-of-line routines for restores, since the
     "exit" out-of-line routines can handle the restore of LR and the
     frame teardown.  However if doesn't make sense to use the
     out-of-line routine if that is the only reason we'd need to save
     LR, and we can't use the "exit" out-of-line gpr restore if we
     have saved some fprs; In those cases it is advantageous to use
     load multiple when available.  */
  if ((strategy & SAVRES_MULTIPLE)
      && (!lr_save_p
	  || info->first_fp_reg_save != 64))
    strategy |= REST_INLINE_GPRS;

  /* Saving CR interferes with the exit routines used on the SPE, so
     just punt here.  */
  if (TARGET_SPE_ABI
      && info->spe_64bit_regs_used
      && info->cr_save_p)
    strategy |= REST_INLINE_GPRS;

  /* We can only use load multiple or the out-of-line routines to
     restore if we've used store multiple or out-of-line routines
     in the prologue, i.e. if we've saved all the registers from
     first_gp_reg_save.  Otherwise, we risk loading garbage.  */
  if ((strategy & (SAVE_INLINE_GPRS | REST_INLINE_GPRS | SAVRES_MULTIPLE))
      == SAVE_INLINE_GPRS)
    {
      int i;

      for (i = info->first_gp_reg_save; i < 32; i++)
	if (!save_reg_p (i))
	  {
	    strategy |= REST_INLINE_GPRS;
	    break;
	  }
    }

  if (TARGET_ELF && TARGET_64BIT)
    {
      if (!(strategy & SAVE_INLINE_FPRS))
	strategy |= SAVE_NOINLINE_FPRS_SAVES_LR;
      else if (!(strategy & SAVE_INLINE_GPRS)
	       && info->first_fp_reg_save == 64)
	strategy |= SAVE_NOINLINE_GPRS_SAVES_LR;
    }
  else if (TARGET_AIX && !(strategy & REST_INLINE_FPRS))
    strategy |= REST_NOINLINE_FPRS_DOESNT_RESTORE_LR;

  if (TARGET_MACHO && !(strategy & SAVE_INLINE_FPRS))
    strategy |= SAVE_NOINLINE_FPRS_SAVES_LR;

  return strategy;
}

/* Calculate the stack information for the current function.  This is
   complicated by having two separate calling sequences, the AIX calling
   sequence and the V.4 calling sequence.

   AIX (and Darwin/Mac OS X) stack frames look like:
							  32-bit  64-bit
	SP---->	+---------------------------------------+
		| back chain to caller			| 0	  0
		+---------------------------------------+
		| saved CR				| 4       8 (8-11)
		+---------------------------------------+
		| saved LR				| 8       16
		+---------------------------------------+
		| reserved for compilers		| 12      24
		+---------------------------------------+
		| reserved for binders			| 16      32
		+---------------------------------------+
		| saved TOC pointer			| 20      40
		+---------------------------------------+
		| Parameter save area (P)		| 24      48
		+---------------------------------------+
		| Alloca space (A)			| 24+P    etc.
		+---------------------------------------+
		| Local variable space (L)		| 24+P+A
		+---------------------------------------+
		| Float/int conversion temporary (X)	| 24+P+A+L
		+---------------------------------------+
		| Save area for AltiVec registers (W)	| 24+P+A+L+X
		+---------------------------------------+
		| AltiVec alignment padding (Y)		| 24+P+A+L+X+W
		+---------------------------------------+
		| Save area for VRSAVE register (Z)	| 24+P+A+L+X+W+Y
		+---------------------------------------+
		| Save area for GP registers (G)	| 24+P+A+X+L+X+W+Y+Z
		+---------------------------------------+
		| Save area for FP registers (F)	| 24+P+A+X+L+X+W+Y+Z+G
		+---------------------------------------+
	old SP->| back chain to caller's caller		|
		+---------------------------------------+

   The required alignment for AIX configurations is two words (i.e., 8
   or 16 bytes).

   The ELFv2 ABI is a variant of the AIX ABI.  Stack frames look like:

	SP---->	+---------------------------------------+
		| Back chain to caller			|  0
		+---------------------------------------+
		| Save area for CR			|  8
		+---------------------------------------+
		| Saved LR				|  16
		+---------------------------------------+
		| Saved TOC pointer			|  24
		+---------------------------------------+
		| Parameter save area (P)		|  32
		+---------------------------------------+
		| Alloca space (A)			|  32+P
		+---------------------------------------+
		| Local variable space (L)		|  32+P+A
		+---------------------------------------+
		| Save area for AltiVec registers (W)	|  32+P+A+L
		+---------------------------------------+
		| AltiVec alignment padding (Y)		|  32+P+A+L+W
		+---------------------------------------+
		| Save area for GP registers (G)	|  32+P+A+L+W+Y
		+---------------------------------------+
		| Save area for FP registers (F)	|  32+P+A+L+W+Y+G
		+---------------------------------------+
	old SP->| back chain to caller's caller		|  32+P+A+L+W+Y+G+F
		+---------------------------------------+


   V.4 stack frames look like:

	SP---->	+---------------------------------------+
		| back chain to caller			| 0
		+---------------------------------------+
		| caller's saved LR			| 4
		+---------------------------------------+
		| Parameter save area (P)		| 8
		+---------------------------------------+
		| Alloca space (A)			| 8+P
		+---------------------------------------+
		| Varargs save area (V)			| 8+P+A
		+---------------------------------------+
		| Local variable space (L)		| 8+P+A+V
		+---------------------------------------+
		| Float/int conversion temporary (X)	| 8+P+A+V+L
		+---------------------------------------+
		| Save area for AltiVec registers (W)	| 8+P+A+V+L+X
		+---------------------------------------+
		| AltiVec alignment padding (Y)		| 8+P+A+V+L+X+W
		+---------------------------------------+
		| Save area for VRSAVE register (Z)	| 8+P+A+V+L+X+W+Y
		+---------------------------------------+
		| SPE: area for 64-bit GP registers	|
		+---------------------------------------+
		| SPE alignment padding			|
		+---------------------------------------+
		| saved CR (C)				| 8+P+A+V+L+X+W+Y+Z
		+---------------------------------------+
		| Save area for GP registers (G)	| 8+P+A+V+L+X+W+Y+Z+C
		+---------------------------------------+
		| Save area for FP registers (F)	| 8+P+A+V+L+X+W+Y+Z+C+G
		+---------------------------------------+
	old SP->| back chain to caller's caller		|
		+---------------------------------------+

   The required alignment for V.4 is 16 bytes, or 8 bytes if -meabi is
   given.  (But note below and in sysv4.h that we require only 8 and
   may round up the size of our stack frame anyways.  The historical
   reason is early versions of powerpc-linux which didn't properly
   align the stack at program startup.  A happy side-effect is that
   -mno-eabi libraries can be used with -meabi programs.)

   The EABI configuration defaults to the V.4 layout.  However,
   the stack alignment requirements may differ.  If -mno-eabi is not
   given, the required stack alignment is 8 bytes; if -mno-eabi is
   given, the required alignment is 16 bytes.  (But see V.4 comment
   above.)  */

#ifndef ABI_STACK_BOUNDARY
#define ABI_STACK_BOUNDARY STACK_BOUNDARY
#endif

static rs6000_stack_t *
rs6000_stack_info (void)
{
  rs6000_stack_t *info_ptr = &stack_info;
  int reg_size = TARGET_32BIT ? 4 : 8;
  int ehrd_size;
  int ehcr_size;
  int save_align;
  int first_gp;
  HOST_WIDE_INT non_fixed_size;
  bool using_static_chain_p;

  if (reload_completed && info_ptr->reload_completed)
    return info_ptr;

  memset (info_ptr, 0, sizeof (*info_ptr));
  info_ptr->reload_completed = reload_completed;

  if (TARGET_SPE)
    {
      /* Cache value so we don't rescan instruction chain over and over.  */
      if (cfun->machine->insn_chain_scanned_p == 0)
	cfun->machine->insn_chain_scanned_p
	  = spe_func_has_64bit_regs_p () + 1;
      info_ptr->spe_64bit_regs_used = cfun->machine->insn_chain_scanned_p - 1;
    }

  /* Select which calling sequence.  */
  info_ptr->abi = DEFAULT_ABI;

  /* Calculate which registers need to be saved & save area size.  */
  info_ptr->first_gp_reg_save = first_reg_to_save ();
  /* Assume that we will have to save RS6000_PIC_OFFSET_TABLE_REGNUM,
     even if it currently looks like we won't.  Reload may need it to
     get at a constant; if so, it will have already created a constant
     pool entry for it.  */
  if (((TARGET_TOC && TARGET_MINIMAL_TOC)
       || (flag_pic == 1 && DEFAULT_ABI == ABI_V4)
       || (flag_pic && DEFAULT_ABI == ABI_DARWIN))
      && crtl->uses_const_pool
      && info_ptr->first_gp_reg_save > RS6000_PIC_OFFSET_TABLE_REGNUM)
    first_gp = RS6000_PIC_OFFSET_TABLE_REGNUM;
  else
    first_gp = info_ptr->first_gp_reg_save;

  info_ptr->gp_size = reg_size * (32 - first_gp);

  /* For the SPE, we have an additional upper 32-bits on each GPR.
     Ideally we should save the entire 64-bits only when the upper
     half is used in SIMD instructions.  Since we only record
     registers live (not the size they are used in), this proves
     difficult because we'd have to traverse the instruction chain at
     the right time, taking reload into account.  This is a real pain,
     so we opt to save the GPRs in 64-bits always if but one register
     gets used in 64-bits.  Otherwise, all the registers in the frame
     get saved in 32-bits.

     So... since when we save all GPRs (except the SP) in 64-bits, the
     traditional GP save area will be empty.  */
  if (TARGET_SPE_ABI && info_ptr->spe_64bit_regs_used != 0)
    info_ptr->gp_size = 0;

  info_ptr->first_fp_reg_save = first_fp_reg_to_save ();
  info_ptr->fp_size = 8 * (64 - info_ptr->first_fp_reg_save);

  info_ptr->first_altivec_reg_save = first_altivec_reg_to_save ();
  info_ptr->altivec_size = 16 * (LAST_ALTIVEC_REGNO + 1
				 - info_ptr->first_altivec_reg_save);

  /* Does this function call anything?  */
  info_ptr->calls_p = (! crtl->is_leaf 
		       || cfun->machine->ra_needs_full_frame);

  /* Determine if we need to save the condition code registers.  */
  if (df_regs_ever_live_p (CR2_REGNO)
      || df_regs_ever_live_p (CR3_REGNO)
      || df_regs_ever_live_p (CR4_REGNO))
    {
      info_ptr->cr_save_p = 1;
      if (DEFAULT_ABI == ABI_V4)
	info_ptr->cr_size = reg_size;
    }

  /* If the current function calls __builtin_eh_return, then we need
     to allocate stack space for registers that will hold data for
     the exception handler.  */
  if (crtl->calls_eh_return)
    {
      unsigned int i;
      for (i = 0; EH_RETURN_DATA_REGNO (i) != INVALID_REGNUM; ++i)
	continue;

      /* SPE saves EH registers in 64-bits.  */
      ehrd_size = i * (TARGET_SPE_ABI
		       && info_ptr->spe_64bit_regs_used != 0
		       ? UNITS_PER_SPE_WORD : UNITS_PER_WORD);
    }
  else
    ehrd_size = 0;

  /* In the ELFv2 ABI, we also need to allocate space for separate
     CR field save areas if the function calls __builtin_eh_return.  */
  if (DEFAULT_ABI == ABI_ELFv2 && crtl->calls_eh_return)
    {
      /* This hard-codes that we have three call-saved CR fields.  */
      ehcr_size = 3 * reg_size;
      /* We do *not* use the regular CR save mechanism.  */
      info_ptr->cr_save_p = 0;
    }
  else
    ehcr_size = 0;

  /* Determine various sizes.  */
  info_ptr->reg_size     = reg_size;
  info_ptr->fixed_size   = RS6000_SAVE_AREA;
  info_ptr->vars_size    = RS6000_ALIGN (get_frame_size (), 8);
  info_ptr->parm_size    = RS6000_ALIGN (crtl->outgoing_args_size,
					 TARGET_ALTIVEC ? 16 : 8);
  if (FRAME_GROWS_DOWNWARD)
    info_ptr->vars_size
      += RS6000_ALIGN (info_ptr->fixed_size + info_ptr->vars_size
		       + info_ptr->parm_size,
		       ABI_STACK_BOUNDARY / BITS_PER_UNIT)
	 - (info_ptr->fixed_size + info_ptr->vars_size
	    + info_ptr->parm_size);

  if (TARGET_SPE_ABI && info_ptr->spe_64bit_regs_used != 0)
    info_ptr->spe_gp_size = 8 * (32 - first_gp);
  else
    info_ptr->spe_gp_size = 0;

  if (TARGET_ALTIVEC_ABI)
    info_ptr->vrsave_mask = compute_vrsave_mask ();
  else
    info_ptr->vrsave_mask = 0;

  if (TARGET_ALTIVEC_VRSAVE && info_ptr->vrsave_mask)
    info_ptr->vrsave_size  = 4;
  else
    info_ptr->vrsave_size  = 0;

  compute_save_world_info (info_ptr);

  /* Calculate the offsets.  */
  switch (DEFAULT_ABI)
    {
    case ABI_NONE:
    default:
      gcc_unreachable ();

    case ABI_AIX:
    case ABI_ELFv2:
    case ABI_DARWIN:
      info_ptr->fp_save_offset   = - info_ptr->fp_size;
      info_ptr->gp_save_offset   = info_ptr->fp_save_offset - info_ptr->gp_size;

      if (TARGET_ALTIVEC_ABI)
	{
	  info_ptr->vrsave_save_offset
	    = info_ptr->gp_save_offset - info_ptr->vrsave_size;

	  /* Align stack so vector save area is on a quadword boundary.
	     The padding goes above the vectors.  */
	  if (info_ptr->altivec_size != 0)
	    info_ptr->altivec_padding_size
	      = info_ptr->vrsave_save_offset & 0xF;
	  else
	    info_ptr->altivec_padding_size = 0;

	  info_ptr->altivec_save_offset
	    = info_ptr->vrsave_save_offset
	    - info_ptr->altivec_padding_size
	    - info_ptr->altivec_size;
	  gcc_assert (info_ptr->altivec_size == 0
		      || info_ptr->altivec_save_offset % 16 == 0);

	  /* Adjust for AltiVec case.  */
	  info_ptr->ehrd_offset = info_ptr->altivec_save_offset - ehrd_size;
	}
      else
	info_ptr->ehrd_offset      = info_ptr->gp_save_offset - ehrd_size;

      info_ptr->ehcr_offset      = info_ptr->ehrd_offset - ehcr_size;
      info_ptr->cr_save_offset   = reg_size; /* first word when 64-bit.  */
      info_ptr->lr_save_offset   = 2*reg_size;
      break;

    case ABI_V4:
      info_ptr->fp_save_offset   = - info_ptr->fp_size;
      info_ptr->gp_save_offset   = info_ptr->fp_save_offset - info_ptr->gp_size;
      info_ptr->cr_save_offset   = info_ptr->gp_save_offset - info_ptr->cr_size;

      if (TARGET_SPE_ABI && info_ptr->spe_64bit_regs_used != 0)
	{
	  /* Align stack so SPE GPR save area is aligned on a
	     double-word boundary.  */
	  if (info_ptr->spe_gp_size != 0 && info_ptr->cr_save_offset != 0)
	    info_ptr->spe_padding_size
	      = 8 - (-info_ptr->cr_save_offset % 8);
	  else
	    info_ptr->spe_padding_size = 0;

	  info_ptr->spe_gp_save_offset
	    = info_ptr->cr_save_offset
	    - info_ptr->spe_padding_size
	    - info_ptr->spe_gp_size;

	  /* Adjust for SPE case.  */
	  info_ptr->ehrd_offset = info_ptr->spe_gp_save_offset;
	}
      else if (TARGET_ALTIVEC_ABI)
	{
	  info_ptr->vrsave_save_offset
	    = info_ptr->cr_save_offset - info_ptr->vrsave_size;

	  /* Align stack so vector save area is on a quadword boundary.  */
	  if (info_ptr->altivec_size != 0)
	    info_ptr->altivec_padding_size
	      = 16 - (-info_ptr->vrsave_save_offset % 16);
	  else
	    info_ptr->altivec_padding_size = 0;

	  info_ptr->altivec_save_offset
	    = info_ptr->vrsave_save_offset
	    - info_ptr->altivec_padding_size
	    - info_ptr->altivec_size;

	  /* Adjust for AltiVec case.  */
	  info_ptr->ehrd_offset = info_ptr->altivec_save_offset;
	}
      else
	info_ptr->ehrd_offset    = info_ptr->cr_save_offset;
      info_ptr->ehrd_offset      -= ehrd_size;
      info_ptr->lr_save_offset   = reg_size;
      break;
    }

  save_align = (TARGET_ALTIVEC_ABI || DEFAULT_ABI == ABI_DARWIN) ? 16 : 8;
  info_ptr->save_size    = RS6000_ALIGN (info_ptr->fp_size
					 + info_ptr->gp_size
					 + info_ptr->altivec_size
					 + info_ptr->altivec_padding_size
					 + info_ptr->spe_gp_size
					 + info_ptr->spe_padding_size
					 + ehrd_size
					 + ehcr_size
					 + info_ptr->cr_size
					 + info_ptr->vrsave_size,
					 save_align);

  non_fixed_size	 = (info_ptr->vars_size
			    + info_ptr->parm_size
			    + info_ptr->save_size);

  info_ptr->total_size = RS6000_ALIGN (non_fixed_size + info_ptr->fixed_size,
				       ABI_STACK_BOUNDARY / BITS_PER_UNIT);

  /* Determine if we need to save the link register.  */
  if (info_ptr->calls_p
      || ((DEFAULT_ABI == ABI_AIX || DEFAULT_ABI == ABI_ELFv2)
	  && crtl->profile
	  && !TARGET_PROFILE_KERNEL)
      || (DEFAULT_ABI == ABI_V4 && cfun->calls_alloca)
#ifdef TARGET_RELOCATABLE
      || (TARGET_RELOCATABLE && (get_pool_size () != 0))
#endif
      || rs6000_ra_ever_killed ())
    info_ptr->lr_save_p = 1;

  using_static_chain_p = (cfun->static_chain_decl != NULL_TREE
			  && df_regs_ever_live_p (STATIC_CHAIN_REGNUM)
			  && call_used_regs[STATIC_CHAIN_REGNUM]);
  info_ptr->savres_strategy = rs6000_savres_strategy (info_ptr,
						      using_static_chain_p);

  if (!(info_ptr->savres_strategy & SAVE_INLINE_GPRS)
      || !(info_ptr->savres_strategy & SAVE_INLINE_FPRS)
      || !(info_ptr->savres_strategy & SAVE_INLINE_VRS)
      || !(info_ptr->savres_strategy & REST_INLINE_GPRS)
      || !(info_ptr->savres_strategy & REST_INLINE_FPRS)
      || !(info_ptr->savres_strategy & REST_INLINE_VRS))
    info_ptr->lr_save_p = 1;

  if (info_ptr->lr_save_p)
    df_set_regs_ever_live (LR_REGNO, true);

  /* Determine if we need to allocate any stack frame:

     For AIX we need to push the stack if a frame pointer is needed
     (because the stack might be dynamically adjusted), if we are
     debugging, if we make calls, or if the sum of fp_save, gp_save,
     and local variables are more than the space needed to save all
     non-volatile registers: 32-bit: 18*8 + 19*4 = 220 or 64-bit: 18*8
     + 18*8 = 288 (GPR13 reserved).

     For V.4 we don't have the stack cushion that AIX uses, but assume
     that the debugger can handle stackless frames.  */

  if (info_ptr->calls_p)
    info_ptr->push_p = 1;

  else if (DEFAULT_ABI == ABI_V4)
    info_ptr->push_p = non_fixed_size != 0;

  else if (frame_pointer_needed)
    info_ptr->push_p = 1;

  else if (TARGET_XCOFF && write_symbols != NO_DEBUG)
    info_ptr->push_p = 1;

  else
    info_ptr->push_p = non_fixed_size > (TARGET_32BIT ? 220 : 288);

  /* Zero offsets if we're not saving those registers.  */
  if (info_ptr->fp_size == 0)
    info_ptr->fp_save_offset = 0;

  if (info_ptr->gp_size == 0)
    info_ptr->gp_save_offset = 0;

  if (! TARGET_ALTIVEC_ABI || info_ptr->altivec_size == 0)
    info_ptr->altivec_save_offset = 0;

  /* Zero VRSAVE offset if not saved and restored.  */
  if (! TARGET_ALTIVEC_VRSAVE || info_ptr->vrsave_mask == 0)
    info_ptr->vrsave_save_offset = 0;

  if (! TARGET_SPE_ABI
      || info_ptr->spe_64bit_regs_used == 0
      || info_ptr->spe_gp_size == 0)
    info_ptr->spe_gp_save_offset = 0;

  if (! info_ptr->lr_save_p)
    info_ptr->lr_save_offset = 0;

  if (! info_ptr->cr_save_p)
    info_ptr->cr_save_offset = 0;

  return info_ptr;
}

/* Return true if the current function uses any GPRs in 64-bit SIMD
   mode.  */

static bool
spe_func_has_64bit_regs_p (void)
{
  rtx insns, insn;

  /* Functions that save and restore all the call-saved registers will
     need to save/restore the registers in 64-bits.  */
  if (crtl->calls_eh_return
      || cfun->calls_setjmp
      || crtl->has_nonlocal_goto)
    return true;

  insns = get_insns ();

  for (insn = NEXT_INSN (insns); insn != NULL_RTX; insn = NEXT_INSN (insn))
    {
      if (INSN_P (insn))
	{
	  rtx i;

	  /* FIXME: This should be implemented with attributes...

	         (set_attr "spe64" "true")....then,
	         if (get_spe64(insn)) return true;

	     It's the only reliable way to do the stuff below.  */

	  i = PATTERN (insn);
	  if (GET_CODE (i) == SET)
	    {
	      enum machine_mode mode = GET_MODE (SET_SRC (i));

	      if (SPE_VECTOR_MODE (mode))
		return true;
	      if (TARGET_E500_DOUBLE && (mode == DFmode || mode == TFmode))
		return true;
	    }
	}
    }

  return false;
}

static void
debug_stack_info (rs6000_stack_t *info)
{
  const char *abi_string;

  if (! info)
    info = rs6000_stack_info ();

  fprintf (stderr, "\nStack information for function %s:\n",
	   ((current_function_decl && DECL_NAME (current_function_decl))
	    ? IDENTIFIER_POINTER (DECL_NAME (current_function_decl))
	    : "<unknown>"));

  switch (info->abi)
    {
    default:		 abi_string = "Unknown";	break;
    case ABI_NONE:	 abi_string = "NONE";		break;
    case ABI_AIX:	 abi_string = "AIX";		break;
    case ABI_ELFv2:	 abi_string = "ELFv2";		break;
    case ABI_DARWIN:	 abi_string = "Darwin";		break;
    case ABI_V4:	 abi_string = "V.4";		break;
    }

  fprintf (stderr, "\tABI                 = %5s\n", abi_string);

  if (TARGET_ALTIVEC_ABI)
    fprintf (stderr, "\tALTIVEC ABI extensions enabled.\n");

  if (TARGET_SPE_ABI)
    fprintf (stderr, "\tSPE ABI extensions enabled.\n");

  if (info->first_gp_reg_save != 32)
    fprintf (stderr, "\tfirst_gp_reg_save   = %5d\n", info->first_gp_reg_save);

  if (info->first_fp_reg_save != 64)
    fprintf (stderr, "\tfirst_fp_reg_save   = %5d\n", info->first_fp_reg_save);

  if (info->first_altivec_reg_save <= LAST_ALTIVEC_REGNO)
    fprintf (stderr, "\tfirst_altivec_reg_save = %5d\n",
	     info->first_altivec_reg_save);

  if (info->lr_save_p)
    fprintf (stderr, "\tlr_save_p           = %5d\n", info->lr_save_p);

  if (info->cr_save_p)
    fprintf (stderr, "\tcr_save_p           = %5d\n", info->cr_save_p);

  if (info->vrsave_mask)
    fprintf (stderr, "\tvrsave_mask         = 0x%x\n", info->vrsave_mask);

  if (info->push_p)
    fprintf (stderr, "\tpush_p              = %5d\n", info->push_p);

  if (info->calls_p)
    fprintf (stderr, "\tcalls_p             = %5d\n", info->calls_p);

  if (info->gp_save_offset)
    fprintf (stderr, "\tgp_save_offset      = %5d\n", info->gp_save_offset);

  if (info->fp_save_offset)
    fprintf (stderr, "\tfp_save_offset      = %5d\n", info->fp_save_offset);

  if (info->altivec_save_offset)
    fprintf (stderr, "\taltivec_save_offset = %5d\n",
	     info->altivec_save_offset);

  if (info->spe_gp_save_offset)
    fprintf (stderr, "\tspe_gp_save_offset  = %5d\n",
	     info->spe_gp_save_offset);

  if (info->vrsave_save_offset)
    fprintf (stderr, "\tvrsave_save_offset  = %5d\n",
	     info->vrsave_save_offset);

  if (info->lr_save_offset)
    fprintf (stderr, "\tlr_save_offset      = %5d\n", info->lr_save_offset);

  if (info->cr_save_offset)
    fprintf (stderr, "\tcr_save_offset      = %5d\n", info->cr_save_offset);

  if (info->varargs_save_offset)
    fprintf (stderr, "\tvarargs_save_offset = %5d\n", info->varargs_save_offset);

  if (info->total_size)
    fprintf (stderr, "\ttotal_size          = "HOST_WIDE_INT_PRINT_DEC"\n",
	     info->total_size);

  if (info->vars_size)
    fprintf (stderr, "\tvars_size           = "HOST_WIDE_INT_PRINT_DEC"\n",
	     info->vars_size);

  if (info->parm_size)
    fprintf (stderr, "\tparm_size           = %5d\n", info->parm_size);

  if (info->fixed_size)
    fprintf (stderr, "\tfixed_size          = %5d\n", info->fixed_size);

  if (info->gp_size)
    fprintf (stderr, "\tgp_size             = %5d\n", info->gp_size);

  if (info->spe_gp_size)
    fprintf (stderr, "\tspe_gp_size         = %5d\n", info->spe_gp_size);

  if (info->fp_size)
    fprintf (stderr, "\tfp_size             = %5d\n", info->fp_size);

  if (info->altivec_size)
    fprintf (stderr, "\taltivec_size        = %5d\n", info->altivec_size);

  if (info->vrsave_size)
    fprintf (stderr, "\tvrsave_size         = %5d\n", info->vrsave_size);

  if (info->altivec_padding_size)
    fprintf (stderr, "\taltivec_padding_size= %5d\n",
	     info->altivec_padding_size);

  if (info->spe_padding_size)
    fprintf (stderr, "\tspe_padding_size    = %5d\n",
	     info->spe_padding_size);

  if (info->cr_size)
    fprintf (stderr, "\tcr_size             = %5d\n", info->cr_size);

  if (info->save_size)
    fprintf (stderr, "\tsave_size           = %5d\n", info->save_size);

  if (info->reg_size != 4)
    fprintf (stderr, "\treg_size            = %5d\n", info->reg_size);

    fprintf (stderr, "\tsave-strategy       =  %04x\n", info->savres_strategy);

  fprintf (stderr, "\n");
}

rtx
rs6000_return_addr (int count, rtx frame)
{
  /* Currently we don't optimize very well between prolog and body
     code and for PIC code the code can be actually quite bad, so
     don't try to be too clever here.  */
  if (count != 0
      || ((DEFAULT_ABI == ABI_V4 || DEFAULT_ABI == ABI_DARWIN) && flag_pic))
    {
      cfun->machine->ra_needs_full_frame = 1;

      return
	gen_rtx_MEM
	  (Pmode,
	   memory_address
	   (Pmode,
	    plus_constant (Pmode,
			   copy_to_reg
			   (gen_rtx_MEM (Pmode,
					 memory_address (Pmode, frame))),
			   RETURN_ADDRESS_OFFSET)));
    }

  cfun->machine->ra_need_lr = 1;
  return get_hard_reg_initial_val (Pmode, LR_REGNO);
}

/* Say whether a function is a candidate for sibcall handling or not.  */

static bool
rs6000_function_ok_for_sibcall (tree decl, tree exp)
{
  tree fntype;

  if (decl)
    fntype = TREE_TYPE (decl);
  else
    fntype = TREE_TYPE (TREE_TYPE (CALL_EXPR_FN (exp)));

  /* We can't do it if the called function has more vector parameters
     than the current function; there's nowhere to put the VRsave code.  */
  if (TARGET_ALTIVEC_ABI
      && TARGET_ALTIVEC_VRSAVE
      && !(decl && decl == current_function_decl))
    {
      function_args_iterator args_iter;
      tree type;
      int nvreg = 0;

      /* Functions with vector parameters are required to have a
	 prototype, so the argument type info must be available
	 here.  */
      FOREACH_FUNCTION_ARGS(fntype, type, args_iter)
	if (TREE_CODE (type) == VECTOR_TYPE
	    && ALTIVEC_OR_VSX_VECTOR_MODE (TYPE_MODE (type)))
	  nvreg++;

      FOREACH_FUNCTION_ARGS(TREE_TYPE (current_function_decl), type, args_iter)
	if (TREE_CODE (type) == VECTOR_TYPE
	    && ALTIVEC_OR_VSX_VECTOR_MODE (TYPE_MODE (type)))
	  nvreg--;

      if (nvreg > 0)
	return false;
    }

  /* Under the AIX or ELFv2 ABIs we can't allow calls to non-local
     functions, because the callee may have a different TOC pointer to
     the caller and there's no way to ensure we restore the TOC when
     we return.  With the secure-plt SYSV ABI we can't make non-local
     calls when -fpic/PIC because the plt call stubs use r30.  */
  if (DEFAULT_ABI == ABI_DARWIN
      || ((DEFAULT_ABI == ABI_AIX || DEFAULT_ABI == ABI_ELFv2)
	  && decl
	  && !DECL_EXTERNAL (decl)
	  && (*targetm.binds_local_p) (decl))
      || (DEFAULT_ABI == ABI_V4
	  && (!TARGET_SECURE_PLT
	      || !flag_pic
	      || (decl
		  && (*targetm.binds_local_p) (decl)))))
    {
      tree attr_list = TYPE_ATTRIBUTES (fntype);

      if (!lookup_attribute ("longcall", attr_list)
	  || lookup_attribute ("shortcall", attr_list))
	return true;
    }

  return false;
}

static int
rs6000_ra_ever_killed (void)
{
  rtx top;
  rtx reg;
  rtx insn;

  if (cfun->is_thunk)
    return 0;

  if (cfun->machine->lr_save_state)
    return cfun->machine->lr_save_state - 1;

  /* regs_ever_live has LR marked as used if any sibcalls are present,
     but this should not force saving and restoring in the
     pro/epilogue.  Likewise, reg_set_between_p thinks a sibcall
     clobbers LR, so that is inappropriate.  */

  /* Also, the prologue can generate a store into LR that
     doesn't really count, like this:

        move LR->R0
        bcl to set PIC register
        move LR->R31
        move R0->LR

     When we're called from the epilogue, we need to avoid counting
     this as a store.  */

  push_topmost_sequence ();
  top = get_insns ();
  pop_topmost_sequence ();
  reg = gen_rtx_REG (Pmode, LR_REGNO);

  for (insn = NEXT_INSN (top); insn != NULL_RTX; insn = NEXT_INSN (insn))
    {
      if (INSN_P (insn))
	{
	  if (CALL_P (insn))
	    {
	      if (!SIBLING_CALL_P (insn))
		return 1;
	    }
	  else if (find_regno_note (insn, REG_INC, LR_REGNO))
	    return 1;
	  else if (set_of (reg, insn) != NULL_RTX
		   && !prologue_epilogue_contains (insn))
	    return 1;
    	}
    }
  return 0;
}

/* Emit instructions needed to load the TOC register.
   This is only needed when TARGET_TOC, TARGET_MINIMAL_TOC, and there is
   a constant pool; or for SVR4 -fpic.  */

void
rs6000_emit_load_toc_table (int fromprolog)
{
  rtx dest;
  dest = gen_rtx_REG (Pmode, RS6000_PIC_OFFSET_TABLE_REGNUM);

  if (TARGET_ELF && TARGET_SECURE_PLT && DEFAULT_ABI == ABI_V4 && flag_pic)
    {
      char buf[30];
      rtx lab, tmp1, tmp2, got;

      lab = gen_label_rtx ();
      ASM_GENERATE_INTERNAL_LABEL (buf, "L", CODE_LABEL_NUMBER (lab));
      lab = gen_rtx_SYMBOL_REF (Pmode, ggc_strdup (buf));
      if (flag_pic == 2)
	got = gen_rtx_SYMBOL_REF (Pmode, toc_label_name);
      else
	got = rs6000_got_sym ();
      tmp1 = tmp2 = dest;
      if (!fromprolog)
	{
	  tmp1 = gen_reg_rtx (Pmode);
	  tmp2 = gen_reg_rtx (Pmode);
	}
      emit_insn (gen_load_toc_v4_PIC_1 (lab));
      emit_move_insn (tmp1, gen_rtx_REG (Pmode, LR_REGNO));
      emit_insn (gen_load_toc_v4_PIC_3b (tmp2, tmp1, got, lab));
      emit_insn (gen_load_toc_v4_PIC_3c (dest, tmp2, got, lab));
    }
  else if (TARGET_ELF && DEFAULT_ABI == ABI_V4 && flag_pic == 1)
    {
      emit_insn (gen_load_toc_v4_pic_si ());
      emit_move_insn (dest, gen_rtx_REG (Pmode, LR_REGNO));
    }
  else if (TARGET_ELF && DEFAULT_ABI == ABI_V4 && flag_pic == 2)
    {
      char buf[30];
      rtx temp0 = (fromprolog
		   ? gen_rtx_REG (Pmode, 0)
		   : gen_reg_rtx (Pmode));

      if (fromprolog)
	{
	  rtx symF, symL;

	  ASM_GENERATE_INTERNAL_LABEL (buf, "LCF", rs6000_pic_labelno);
	  symF = gen_rtx_SYMBOL_REF (Pmode, ggc_strdup (buf));

	  ASM_GENERATE_INTERNAL_LABEL (buf, "LCL", rs6000_pic_labelno);
	  symL = gen_rtx_SYMBOL_REF (Pmode, ggc_strdup (buf));

	  emit_insn (gen_load_toc_v4_PIC_1 (symF));
	  emit_move_insn (dest, gen_rtx_REG (Pmode, LR_REGNO));
	  emit_insn (gen_load_toc_v4_PIC_2 (temp0, dest, symL, symF));
	}
      else
	{
	  rtx tocsym, lab;

	  tocsym = gen_rtx_SYMBOL_REF (Pmode, toc_label_name);
	  lab = gen_label_rtx ();
	  emit_insn (gen_load_toc_v4_PIC_1b (tocsym, lab));
	  emit_move_insn (dest, gen_rtx_REG (Pmode, LR_REGNO));
	  if (TARGET_LINK_STACK)
	    emit_insn (gen_addsi3 (dest, dest, GEN_INT (4)));
	  emit_move_insn (temp0, gen_rtx_MEM (Pmode, dest));
	}
      emit_insn (gen_addsi3 (dest, temp0, dest));
    }
  else if (TARGET_ELF && !TARGET_AIX && flag_pic == 0 && TARGET_MINIMAL_TOC)
    {
      /* This is for AIX code running in non-PIC ELF32.  */
      char buf[30];
      rtx realsym;
      ASM_GENERATE_INTERNAL_LABEL (buf, "LCTOC", 1);
      realsym = gen_rtx_SYMBOL_REF (Pmode, ggc_strdup (buf));

      emit_insn (gen_elf_high (dest, realsym));
      emit_insn (gen_elf_low (dest, dest, realsym));
    }
  else
    {
      gcc_assert (DEFAULT_ABI == ABI_AIX || DEFAULT_ABI == ABI_ELFv2);

      if (TARGET_32BIT)
	emit_insn (gen_load_toc_aix_si (dest));
      else
	emit_insn (gen_load_toc_aix_di (dest));
    }
}

/* Emit instructions to restore the link register after determining where
   its value has been stored.  */

void
rs6000_emit_eh_reg_restore (rtx source, rtx scratch)
{
  rs6000_stack_t *info = rs6000_stack_info ();
  rtx operands[2];

  operands[0] = source;
  operands[1] = scratch;

  if (info->lr_save_p)
    {
      rtx frame_rtx = stack_pointer_rtx;
      HOST_WIDE_INT sp_offset = 0;
      rtx tmp;

      if (frame_pointer_needed
	  || cfun->calls_alloca
	  || info->total_size > 32767)
	{
	  tmp = gen_frame_mem (Pmode, frame_rtx);
	  emit_move_insn (operands[1], tmp);
	  frame_rtx = operands[1];
	}
      else if (info->push_p)
	sp_offset = info->total_size;

      tmp = plus_constant (Pmode, frame_rtx,
			   info->lr_save_offset + sp_offset);
      tmp = gen_frame_mem (Pmode, tmp);
      emit_move_insn (tmp, operands[0]);
    }
  else
    emit_move_insn (gen_rtx_REG (Pmode, LR_REGNO), operands[0]);

  /* Freeze lr_save_p.  We've just emitted rtl that depends on the
     state of lr_save_p so any change from here on would be a bug.  In
     particular, stop rs6000_ra_ever_killed from considering the SET
     of lr we may have added just above.  */ 
  cfun->machine->lr_save_state = info->lr_save_p + 1;
}

static GTY(()) alias_set_type set = -1;

alias_set_type
get_TOC_alias_set (void)
{
  if (set == -1)
    set = new_alias_set ();
  return set;
}

/* This returns nonzero if the current function uses the TOC.  This is
   determined by the presence of (use (unspec ... UNSPEC_TOC)), which
   is generated by the ABI_V4 load_toc_* patterns.  */
#if TARGET_ELF
static int
uses_TOC (void)
{
  rtx insn;

  for (insn = get_insns (); insn; insn = NEXT_INSN (insn))
    if (INSN_P (insn))
      {
	rtx pat = PATTERN (insn);
	int i;

	if (GET_CODE (pat) == PARALLEL)
	  for (i = 0; i < XVECLEN (pat, 0); i++)
	    {
	      rtx sub = XVECEXP (pat, 0, i);
	      if (GET_CODE (sub) == USE)
		{
		  sub = XEXP (sub, 0);
		  if (GET_CODE (sub) == UNSPEC
		      && XINT (sub, 1) == UNSPEC_TOC)
		    return 1;
		}
	    }
      }
  return 0;
}
#endif

rtx
create_TOC_reference (rtx symbol, rtx largetoc_reg)
{
  rtx tocrel, tocreg, hi;

  if (TARGET_DEBUG_ADDR)
    {
      if (GET_CODE (symbol) == SYMBOL_REF)
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

/* Issue assembly directives that create a reference to the given DWARF
   FRAME_TABLE_LABEL from the current function section.  */
void
rs6000_aix_asm_output_dwarf_table_ref (char * frame_table_label)
{
  fprintf (asm_out_file, "\t.ref %s\n",
	   (* targetm.strip_name_encoding) (frame_table_label));
}

/* This ties together stack memory (MEM with an alias set of frame_alias_set)
   and the change to the stack pointer.  */

static void
rs6000_emit_stack_tie (rtx fp, bool hard_frame_needed)
{
  rtvec p;
  int i;
  rtx regs[3];

  i = 0;
  regs[i++] = gen_rtx_REG (Pmode, STACK_POINTER_REGNUM);
  if (hard_frame_needed)
    regs[i++] = gen_rtx_REG (Pmode, HARD_FRAME_POINTER_REGNUM);
  if (!(REGNO (fp) == STACK_POINTER_REGNUM
	|| (hard_frame_needed
	    && REGNO (fp) == HARD_FRAME_POINTER_REGNUM)))
    regs[i++] = fp;

  p = rtvec_alloc (i);
  while (--i >= 0)
    {
      rtx mem = gen_frame_mem (BLKmode, regs[i]);
      RTVEC_ELT (p, i) = gen_rtx_SET (VOIDmode, mem, const0_rtx);
    }

  emit_insn (gen_stack_tie (gen_rtx_PARALLEL (VOIDmode, p)));
}

/* Emit the correct code for allocating stack space, as insns.
   If COPY_REG, make sure a copy of the old frame is left there.
   The generated code may use hard register 0 as a temporary.  */

static void
rs6000_emit_allocate_stack (HOST_WIDE_INT size, rtx copy_reg, int copy_off)
{
  rtx insn;
  rtx stack_reg = gen_rtx_REG (Pmode, STACK_POINTER_REGNUM);
  rtx tmp_reg = gen_rtx_REG (Pmode, 0);
  rtx todec = gen_int_mode (-size, Pmode);
  rtx par, set, mem;

  if (INTVAL (todec) != -size)
    {
      warning (0, "stack frame too large");
      emit_insn (gen_trap ());
      return;
    }

  if (crtl->limit_stack)
    {
      if (REG_P (stack_limit_rtx)
	  && REGNO (stack_limit_rtx) > 1
	  && REGNO (stack_limit_rtx) <= 31)
	{
	  emit_insn (gen_add3_insn (tmp_reg, stack_limit_rtx, GEN_INT (size)));
	  emit_insn (gen_cond_trap (LTU, stack_reg, tmp_reg,
				    const0_rtx));
	}
      else if (GET_CODE (stack_limit_rtx) == SYMBOL_REF
	       && TARGET_32BIT
	       && DEFAULT_ABI == ABI_V4)
	{
	  rtx toload = gen_rtx_CONST (VOIDmode,
				      gen_rtx_PLUS (Pmode,
						    stack_limit_rtx,
						    GEN_INT (size)));

	  emit_insn (gen_elf_high (tmp_reg, toload));
	  emit_insn (gen_elf_low (tmp_reg, tmp_reg, toload));
	  emit_insn (gen_cond_trap (LTU, stack_reg, tmp_reg,
				    const0_rtx));
	}
      else
	warning (0, "stack limit expression is not supported");
    }

  if (copy_reg)
    {
      if (copy_off != 0)
	emit_insn (gen_add3_insn (copy_reg, stack_reg, GEN_INT (copy_off)));
      else
	emit_move_insn (copy_reg, stack_reg);
    }

  if (size > 32767)
    {
      /* Need a note here so that try_split doesn't get confused.  */
      if (get_last_insn () == NULL_RTX)
	emit_note (NOTE_INSN_DELETED);
      insn = emit_move_insn (tmp_reg, todec);
      try_split (PATTERN (insn), insn, 0);
      todec = tmp_reg;
    }
  
  insn = emit_insn (TARGET_32BIT
		    ? gen_movsi_update_stack (stack_reg, stack_reg,
					todec, stack_reg)
		    : gen_movdi_di_update_stack (stack_reg, stack_reg,
					   todec, stack_reg));
  /* Since we didn't use gen_frame_mem to generate the MEM, grab
     it now and set the alias set/attributes. The above gen_*_update
     calls will generate a PARALLEL with the MEM set being the first
     operation. */
  par = PATTERN (insn);
  gcc_assert (GET_CODE (par) == PARALLEL);
  set = XVECEXP (par, 0, 0);
  gcc_assert (GET_CODE (set) == SET);
  mem = SET_DEST (set);
  gcc_assert (MEM_P (mem));
  MEM_NOTRAP_P (mem) = 1;
  set_mem_alias_set (mem, get_frame_alias_set ());

  RTX_FRAME_RELATED_P (insn) = 1;
  add_reg_note (insn, REG_FRAME_RELATED_EXPR,
		gen_rtx_SET (VOIDmode, stack_reg,
			     gen_rtx_PLUS (Pmode, stack_reg,
					   GEN_INT (-size))));
}

#define PROBE_INTERVAL (1 << STACK_CHECK_PROBE_INTERVAL_EXP)

#if PROBE_INTERVAL > 32768
#error Cannot use indexed addressing mode for stack probing
#endif

/* Emit code to probe a range of stack addresses from FIRST to FIRST+SIZE,
   inclusive.  These are offsets from the current stack pointer.  */

static void
rs6000_emit_probe_stack_range (HOST_WIDE_INT first, HOST_WIDE_INT size)
{
  /* See if we have a constant small number of probes to generate.  If so,
     that's the easy case.  */
  if (first + size <= 32768)
    {
      HOST_WIDE_INT i;

      /* Probe at FIRST + N * PROBE_INTERVAL for values of N from 1 until
	 it exceeds SIZE.  If only one probe is needed, this will not
	 generate any code.  Then probe at FIRST + SIZE.  */
      for (i = PROBE_INTERVAL; i < size; i += PROBE_INTERVAL)
	emit_stack_probe (plus_constant (Pmode, stack_pointer_rtx,
					 -(first + i)));

      emit_stack_probe (plus_constant (Pmode, stack_pointer_rtx,
				       -(first + size)));
    }

  /* Otherwise, do the same as above, but in a loop.  Note that we must be
     extra careful with variables wrapping around because we might be at
     the very top (or the very bottom) of the address space and we have
     to be able to handle this case properly; in particular, we use an
     equality test for the loop condition.  */
  else
    {
      HOST_WIDE_INT rounded_size;
      rtx r12 = gen_rtx_REG (Pmode, 12);
      rtx r0 = gen_rtx_REG (Pmode, 0);

      /* Sanity check for the addressing mode we're going to use.  */
      gcc_assert (first <= 32768);

      /* Step 1: round SIZE to the previous multiple of the interval.  */

      rounded_size = size & -PROBE_INTERVAL;


      /* Step 2: compute initial and final value of the loop counter.  */

      /* TEST_ADDR = SP + FIRST.  */
      emit_insn (gen_rtx_SET (VOIDmode, r12,
			      plus_constant (Pmode, stack_pointer_rtx,
					     -first)));

      /* LAST_ADDR = SP + FIRST + ROUNDED_SIZE.  */
      if (rounded_size > 32768)
	{
	  emit_move_insn (r0, GEN_INT (-rounded_size));
	  emit_insn (gen_rtx_SET (VOIDmode, r0,
				  gen_rtx_PLUS (Pmode, r12, r0)));
	}
      else
	emit_insn (gen_rtx_SET (VOIDmode, r0,
			        plus_constant (Pmode, r12, -rounded_size)));


      /* Step 3: the loop

	 while (TEST_ADDR != LAST_ADDR)
	   {
	     TEST_ADDR = TEST_ADDR + PROBE_INTERVAL
	     probe at TEST_ADDR
	   }

	 probes at FIRST + N * PROBE_INTERVAL for values of N from 1
	 until it is equal to ROUNDED_SIZE.  */

      if (TARGET_64BIT)
	emit_insn (gen_probe_stack_rangedi (r12, r12, r0));
      else
	emit_insn (gen_probe_stack_rangesi (r12, r12, r0));


      /* Step 4: probe at FIRST + SIZE if we cannot assert at compile-time
	 that SIZE is equal to ROUNDED_SIZE.  */

      if (size != rounded_size)
	emit_stack_probe (plus_constant (Pmode, r12, rounded_size - size));
    }
}

/* Probe a range of stack addresses from REG1 to REG2 inclusive.  These are
   absolute addresses.  */

const char *
output_probe_stack_range (rtx reg1, rtx reg2)
{
  static int labelno = 0;
  char loop_lab[32], end_lab[32];
  rtx xops[2];

  ASM_GENERATE_INTERNAL_LABEL (loop_lab, "LPSRL", labelno);
  ASM_GENERATE_INTERNAL_LABEL (end_lab, "LPSRE", labelno++);

  ASM_OUTPUT_INTERNAL_LABEL (asm_out_file, loop_lab);

  /* Jump to END_LAB if TEST_ADDR == LAST_ADDR.  */
  xops[0] = reg1;
  xops[1] = reg2;
  if (TARGET_64BIT)
    output_asm_insn ("cmpd 0,%0,%1", xops);
  else
    output_asm_insn ("cmpw 0,%0,%1", xops);

  fputs ("\tbeq 0,", asm_out_file);
  assemble_name_raw (asm_out_file, end_lab);
  fputc ('\n', asm_out_file);

  /* TEST_ADDR = TEST_ADDR + PROBE_INTERVAL.  */
  xops[1] = GEN_INT (-PROBE_INTERVAL);
  output_asm_insn ("addi %0,%0,%1", xops);

  /* Probe at TEST_ADDR and branch.  */
  xops[1] = gen_rtx_REG (Pmode, 0);
  output_asm_insn ("stw %1,0(%0)", xops);
  fprintf (asm_out_file, "\tb ");
  assemble_name_raw (asm_out_file, loop_lab);
  fputc ('\n', asm_out_file);

  ASM_OUTPUT_INTERNAL_LABEL (asm_out_file, end_lab);

  return "";
}

/* Add to 'insn' a note which is PATTERN (INSN) but with REG replaced
   with (plus:P (reg 1) VAL), and with REG2 replaced with RREG if REG2
   is not NULL.  It would be nice if dwarf2out_frame_debug_expr could
   deduce these equivalences by itself so it wasn't necessary to hold
   its hand so much.  Don't be tempted to always supply d2_f_d_e with
   the actual cfa register, ie. r31 when we are using a hard frame
   pointer.  That fails when saving regs off r1, and sched moves the
   r31 setup past the reg saves.  */

static rtx
rs6000_frame_related (rtx insn, rtx reg, HOST_WIDE_INT val,
		      rtx reg2, rtx rreg, rtx split_reg)
{
  rtx real, temp;

  if (REGNO (reg) == STACK_POINTER_REGNUM && reg2 == NULL_RTX)
    {
      /* No need for any replacement.  Just set RTX_FRAME_RELATED_P.  */
      int i;

      gcc_checking_assert (val == 0);
      real = PATTERN (insn);
      if (GET_CODE (real) == PARALLEL)
	for (i = 0; i < XVECLEN (real, 0); i++)
	  if (GET_CODE (XVECEXP (real, 0, i)) == SET)
	    {
	      rtx set = XVECEXP (real, 0, i);

	      RTX_FRAME_RELATED_P (set) = 1;
	    }
      RTX_FRAME_RELATED_P (insn) = 1;
      return insn;
    }

  /* copy_rtx will not make unique copies of registers, so we need to
     ensure we don't have unwanted sharing here.  */
  if (reg == reg2)
    reg = gen_raw_REG (GET_MODE (reg), REGNO (reg));

  if (reg == rreg)
    reg = gen_raw_REG (GET_MODE (reg), REGNO (reg));

  real = copy_rtx (PATTERN (insn));

  if (reg2 != NULL_RTX)
    real = replace_rtx (real, reg2, rreg);

  if (REGNO (reg) == STACK_POINTER_REGNUM)
    gcc_checking_assert (val == 0);
  else
    real = replace_rtx (real, reg,
			gen_rtx_PLUS (Pmode, gen_rtx_REG (Pmode,
							  STACK_POINTER_REGNUM),
				      GEN_INT (val)));

  /* We expect that 'real' is either a SET or a PARALLEL containing
     SETs (and possibly other stuff).  In a PARALLEL, all the SETs
     are important so they all have to be marked RTX_FRAME_RELATED_P.  */

  if (GET_CODE (real) == SET)
    {
      rtx set = real;

      temp = simplify_rtx (SET_SRC (set));
      if (temp)
	SET_SRC (set) = temp;
      temp = simplify_rtx (SET_DEST (set));
      if (temp)
	SET_DEST (set) = temp;
      if (GET_CODE (SET_DEST (set)) == MEM)
	{
	  temp = simplify_rtx (XEXP (SET_DEST (set), 0));
	  if (temp)
	    XEXP (SET_DEST (set), 0) = temp;
	}
    }
  else
    {
      int i;

      gcc_assert (GET_CODE (real) == PARALLEL);
      for (i = 0; i < XVECLEN (real, 0); i++)
	if (GET_CODE (XVECEXP (real, 0, i)) == SET)
	  {
	    rtx set = XVECEXP (real, 0, i);

	    temp = simplify_rtx (SET_SRC (set));
	    if (temp)
	      SET_SRC (set) = temp;
	    temp = simplify_rtx (SET_DEST (set));
	    if (temp)
	      SET_DEST (set) = temp;
	    if (GET_CODE (SET_DEST (set)) == MEM)
	      {
		temp = simplify_rtx (XEXP (SET_DEST (set), 0));
		if (temp)
		  XEXP (SET_DEST (set), 0) = temp;
	      }
	    RTX_FRAME_RELATED_P (set) = 1;
	  }
    }

  /* If a store insn has been split into multiple insns, the
     true source register is given by split_reg.  */
  if (split_reg != NULL_RTX)
    real = gen_rtx_SET (VOIDmode, SET_DEST (real), split_reg);

  RTX_FRAME_RELATED_P (insn) = 1;
  add_reg_note (insn, REG_FRAME_RELATED_EXPR, real);

  return insn;
}

/* Returns an insn that has a vrsave set operation with the
   appropriate CLOBBERs.  */

static rtx
generate_set_vrsave (rtx reg, rs6000_stack_t *info, int epiloguep)
{
  int nclobs, i;
  rtx insn, clobs[TOTAL_ALTIVEC_REGS + 1];
  rtx vrsave = gen_rtx_REG (SImode, VRSAVE_REGNO);

  clobs[0]
    = gen_rtx_SET (VOIDmode,
		   vrsave,
		   gen_rtx_UNSPEC_VOLATILE (SImode,
					    gen_rtvec (2, reg, vrsave),
					    UNSPECV_SET_VRSAVE));

  nclobs = 1;

  /* We need to clobber the registers in the mask so the scheduler
     does not move sets to VRSAVE before sets of AltiVec registers.

     However, if the function receives nonlocal gotos, reload will set
     all call saved registers live.  We will end up with:

     	(set (reg 999) (mem))
	(parallel [ (set (reg vrsave) (unspec blah))
		    (clobber (reg 999))])

     The clobber will cause the store into reg 999 to be dead, and
     flow will attempt to delete an epilogue insn.  In this case, we
     need an unspec use/set of the register.  */

  for (i = FIRST_ALTIVEC_REGNO; i <= LAST_ALTIVEC_REGNO; ++i)
    if (info->vrsave_mask & ALTIVEC_REG_BIT (i))
      {
	if (!epiloguep || call_used_regs [i])
	  clobs[nclobs++] = gen_rtx_CLOBBER (VOIDmode,
					     gen_rtx_REG (V4SImode, i));
	else
	  {
	    rtx reg = gen_rtx_REG (V4SImode, i);

	    clobs[nclobs++]
	      = gen_rtx_SET (VOIDmode,
			     reg,
			     gen_rtx_UNSPEC (V4SImode,
					     gen_rtvec (1, reg), 27));
	  }
      }

  insn = gen_rtx_PARALLEL (VOIDmode, rtvec_alloc (nclobs));

  for (i = 0; i < nclobs; ++i)
    XVECEXP (insn, 0, i) = clobs[i];

  return insn;
}

static rtx
gen_frame_set (rtx reg, rtx frame_reg, int offset, bool store)
{
  rtx addr, mem;

  addr = gen_rtx_PLUS (Pmode, frame_reg, GEN_INT (offset));
  mem = gen_frame_mem (GET_MODE (reg), addr);
  return gen_rtx_SET (VOIDmode, store ? mem : reg, store ? reg : mem);
}

static rtx
gen_frame_load (rtx reg, rtx frame_reg, int offset)
{
  return gen_frame_set (reg, frame_reg, offset, false);
}

static rtx
gen_frame_store (rtx reg, rtx frame_reg, int offset)
{
  return gen_frame_set (reg, frame_reg, offset, true);
}

/* Save a register into the frame, and emit RTX_FRAME_RELATED_P notes.
   Save REGNO into [FRAME_REG + OFFSET] in mode MODE.  */

static rtx
emit_frame_save (rtx frame_reg, enum machine_mode mode,
		 unsigned int regno, int offset, HOST_WIDE_INT frame_reg_to_sp)
{
  rtx reg, insn;

  /* Some cases that need register indexed addressing.  */
  gcc_checking_assert (!((TARGET_ALTIVEC_ABI && ALTIVEC_VECTOR_MODE (mode))
			 || (TARGET_VSX && ALTIVEC_OR_VSX_VECTOR_MODE (mode))
			 || (TARGET_E500_DOUBLE && mode == DFmode)
			 || (TARGET_SPE_ABI
			     && SPE_VECTOR_MODE (mode)
			     && !SPE_CONST_OFFSET_OK (offset))));

  reg = gen_rtx_REG (mode, regno);
  insn = emit_insn (gen_frame_store (reg, frame_reg, offset));
  return rs6000_frame_related (insn, frame_reg, frame_reg_to_sp,
			       NULL_RTX, NULL_RTX, NULL_RTX);
}

/* Emit an offset memory reference suitable for a frame store, while
   converting to a valid addressing mode.  */

static rtx
gen_frame_mem_offset (enum machine_mode mode, rtx reg, int offset)
{
  rtx int_rtx, offset_rtx;

  int_rtx = GEN_INT (offset);

  if ((TARGET_SPE_ABI && SPE_VECTOR_MODE (mode) && !SPE_CONST_OFFSET_OK (offset))
      || (TARGET_E500_DOUBLE && mode == DFmode))
    {
      offset_rtx = gen_rtx_REG (Pmode, FIXED_SCRATCH);
      emit_move_insn (offset_rtx, int_rtx);
    }
  else
    offset_rtx = int_rtx;

  return gen_frame_mem (mode, gen_rtx_PLUS (Pmode, reg, offset_rtx));
}

#ifndef TARGET_FIX_AND_CONTINUE
#define TARGET_FIX_AND_CONTINUE 0
#endif

/* It's really GPR 13 or 14, FPR 14 and VR 20.  We need the smallest.  */
#define FIRST_SAVRES_REGISTER FIRST_SAVED_GP_REGNO
#define LAST_SAVRES_REGISTER 31
#define N_SAVRES_REGISTERS (LAST_SAVRES_REGISTER - FIRST_SAVRES_REGISTER + 1)

enum {
  SAVRES_LR = 0x1,
  SAVRES_SAVE = 0x2,
  SAVRES_REG = 0x0c,
  SAVRES_GPR = 0,
  SAVRES_FPR = 4,
  SAVRES_VR  = 8
};

static GTY(()) rtx savres_routine_syms[N_SAVRES_REGISTERS][12];

/* Temporary holding space for an out-of-line register save/restore
   routine name.  */
static char savres_routine_name[30];

/* Return the name for an out-of-line register save/restore routine.
   We are saving/restoring GPRs if GPR is true.  */

static char *
rs6000_savres_routine_name (rs6000_stack_t *info, int regno, int sel)
{
  const char *prefix = "";
  const char *suffix = "";

  /* Different targets are supposed to define
     {SAVE,RESTORE}_FP_{PREFIX,SUFFIX} with the idea that the needed
     routine name could be defined with:

     sprintf (name, "%s%d%s", SAVE_FP_PREFIX, regno, SAVE_FP_SUFFIX)

     This is a nice idea in practice, but in reality, things are
     complicated in several ways:

     - ELF targets have save/restore routines for GPRs.

     - SPE targets use different prefixes for 32/64-bit registers, and
       neither of them fit neatly in the FOO_{PREFIX,SUFFIX} regimen.

     - PPC64 ELF targets have routines for save/restore of GPRs that
       differ in what they do with the link register, so having a set
       prefix doesn't work.  (We only use one of the save routines at
       the moment, though.)

     - PPC32 elf targets have "exit" versions of the restore routines
       that restore the link register and can save some extra space.
       These require an extra suffix.  (There are also "tail" versions
       of the restore routines and "GOT" versions of the save routines,
       but we don't generate those at present.  Same problems apply,
       though.)

     We deal with all this by synthesizing our own prefix/suffix and
     using that for the simple sprintf call shown above.  */
  if (TARGET_SPE)
    {
      /* No floating point saves on the SPE.  */
      gcc_assert ((sel & SAVRES_REG) == SAVRES_GPR);

      if ((sel & SAVRES_SAVE))
	prefix = info->spe_64bit_regs_used ? "_save64gpr_" : "_save32gpr_";
      else
	prefix = info->spe_64bit_regs_used ? "_rest64gpr_" : "_rest32gpr_";

      if ((sel & SAVRES_LR))
	suffix = "_x";
    }
  else if (DEFAULT_ABI == ABI_V4)
    {
      if (TARGET_64BIT)
	goto aix_names;

      if ((sel & SAVRES_REG) == SAVRES_GPR)
	prefix = (sel & SAVRES_SAVE) ? "_savegpr_" : "_restgpr_";
      else if ((sel & SAVRES_REG) == SAVRES_FPR)
	prefix = (sel & SAVRES_SAVE) ? "_savefpr_" : "_restfpr_";
      else if ((sel & SAVRES_REG) == SAVRES_VR)
	prefix = (sel & SAVRES_SAVE) ? "_savevr_" : "_restvr_";
      else
	abort ();

      if ((sel & SAVRES_LR))
	suffix = "_x";
    }
  else if (DEFAULT_ABI == ABI_AIX || DEFAULT_ABI == ABI_ELFv2)
    {
#if !defined (POWERPC_LINUX) && !defined (POWERPC_FREEBSD)
      /* No out-of-line save/restore routines for GPRs on AIX.  */
      gcc_assert (!TARGET_AIX || (sel & SAVRES_REG) != SAVRES_GPR);
#endif

    aix_names:
      if ((sel & SAVRES_REG) == SAVRES_GPR)
	prefix = ((sel & SAVRES_SAVE)
		  ? ((sel & SAVRES_LR) ? "_savegpr0_" : "_savegpr1_")
		  : ((sel & SAVRES_LR) ? "_restgpr0_" : "_restgpr1_"));
      else if ((sel & SAVRES_REG) == SAVRES_FPR)
	{
#if defined (POWERPC_LINUX) || defined (POWERPC_FREEBSD)
	  if ((sel & SAVRES_LR))
	    prefix = ((sel & SAVRES_SAVE) ? "_savefpr_" : "_restfpr_");
	  else
#endif
	    {
	      prefix = (sel & SAVRES_SAVE) ? SAVE_FP_PREFIX : RESTORE_FP_PREFIX;
	      suffix = (sel & SAVRES_SAVE) ? SAVE_FP_SUFFIX : RESTORE_FP_SUFFIX;
	    }
	}
      else if ((sel & SAVRES_REG) == SAVRES_VR)
	prefix = (sel & SAVRES_SAVE) ? "_savevr_" : "_restvr_";
      else
	abort ();
    }

   if (DEFAULT_ABI == ABI_DARWIN)
    {
      /* The Darwin approach is (slightly) different, in order to be
	 compatible with code generated by the system toolchain.  There is a
	 single symbol for the start of save sequence, and the code here
	 embeds an offset into that code on the basis of the first register
	 to be saved.  */
      prefix = (sel & SAVRES_SAVE) ? "save" : "rest" ;
      if ((sel & SAVRES_REG) == SAVRES_GPR)
	sprintf (savres_routine_name, "*%sGPR%s%s%.0d ; %s r%d-r31", prefix,
		 ((sel & SAVRES_LR) ? "x" : ""), (regno == 13 ? "" : "+"),
		 (regno - 13) * 4, prefix, regno);
      else if ((sel & SAVRES_REG) == SAVRES_FPR)
	sprintf (savres_routine_name, "*%sFP%s%.0d ; %s f%d-f31", prefix,
		 (regno == 14 ? "" : "+"), (regno - 14) * 4, prefix, regno);
      else if ((sel & SAVRES_REG) == SAVRES_VR)
	sprintf (savres_routine_name, "*%sVEC%s%.0d ; %s v%d-v31", prefix,
		 (regno == 20 ? "" : "+"), (regno - 20) * 8, prefix, regno);
      else
	abort ();
    }
  else
    sprintf (savres_routine_name, "%s%d%s", prefix, regno, suffix);

  return savres_routine_name;
}

/* Return an RTL SYMBOL_REF for an out-of-line register save/restore routine.
   We are saving/restoring GPRs if GPR is true.  */

static rtx
rs6000_savres_routine_sym (rs6000_stack_t *info, int sel)
{
  int regno = ((sel & SAVRES_REG) == SAVRES_GPR
	       ? info->first_gp_reg_save
	       : (sel & SAVRES_REG) == SAVRES_FPR
	       ? info->first_fp_reg_save - 32
	       : (sel & SAVRES_REG) == SAVRES_VR
	       ? info->first_altivec_reg_save - FIRST_ALTIVEC_REGNO
	       : -1);
  rtx sym;
  int select = sel;

  /* On the SPE, we never have any FPRs, but we do have 32/64-bit
     versions of the gpr routines.  */
  if (TARGET_SPE_ABI && (sel & SAVRES_REG) == SAVRES_GPR
      && info->spe_64bit_regs_used)
    select ^= SAVRES_FPR ^ SAVRES_GPR;

  /* Don't generate bogus routine names.  */
  gcc_assert (FIRST_SAVRES_REGISTER <= regno
	      && regno <= LAST_SAVRES_REGISTER
	      && select >= 0 && select <= 12);

  sym = savres_routine_syms[regno-FIRST_SAVRES_REGISTER][select];

  if (sym == NULL)
    {
      char *name;

      name = rs6000_savres_routine_name (info, regno, sel);

      sym = savres_routine_syms[regno-FIRST_SAVRES_REGISTER][select]
	= gen_rtx_SYMBOL_REF (Pmode, ggc_strdup (name));
      SYMBOL_REF_FLAGS (sym) |= SYMBOL_FLAG_FUNCTION;
    }

  return sym;
}

/* Emit a sequence of insns, including a stack tie if needed, for
   resetting the stack pointer.  If UPDT_REGNO is not 1, then don't
   reset the stack pointer, but move the base of the frame into
   reg UPDT_REGNO for use by out-of-line register restore routines.  */

static rtx
rs6000_emit_stack_reset (rs6000_stack_t *info,
			 rtx frame_reg_rtx, HOST_WIDE_INT frame_off,
			 unsigned updt_regno)
{
  rtx updt_reg_rtx;

  /* This blockage is needed so that sched doesn't decide to move
     the sp change before the register restores.  */
  if (DEFAULT_ABI == ABI_V4
      || (TARGET_SPE_ABI
	  && info->spe_64bit_regs_used != 0
	  && info->first_gp_reg_save != 32))
    rs6000_emit_stack_tie (frame_reg_rtx, frame_pointer_needed);

  /* If we are restoring registers out-of-line, we will be using the
     "exit" variants of the restore routines, which will reset the
     stack for us.  But we do need to point updt_reg into the
     right place for those routines.  */
  updt_reg_rtx = gen_rtx_REG (Pmode, updt_regno);

  if (frame_off != 0)
    return emit_insn (gen_add3_insn (updt_reg_rtx,
				     frame_reg_rtx, GEN_INT (frame_off)));
  else if (REGNO (frame_reg_rtx) != updt_regno)
    return emit_move_insn (updt_reg_rtx, frame_reg_rtx);

  return NULL_RTX;
}

/* Return the register number used as a pointer by out-of-line
   save/restore functions.  */

static inline unsigned
ptr_regno_for_savres (int sel)
{
  if (DEFAULT_ABI == ABI_AIX || DEFAULT_ABI == ABI_ELFv2)
    return (sel & SAVRES_REG) == SAVRES_FPR || (sel & SAVRES_LR) ? 1 : 12;
  return DEFAULT_ABI == ABI_DARWIN && (sel & SAVRES_REG) == SAVRES_FPR ? 1 : 11;
}

/* Construct a parallel rtx describing the effect of a call to an
   out-of-line register save/restore routine, and emit the insn
   or jump_insn as appropriate.  */

static rtx
rs6000_emit_savres_rtx (rs6000_stack_t *info,
			rtx frame_reg_rtx, int save_area_offset, int lr_offset,
			enum machine_mode reg_mode, int sel)
{
  int i;
  int offset, start_reg, end_reg, n_regs, use_reg;
  int reg_size = GET_MODE_SIZE (reg_mode);
  rtx sym;
  rtvec p;
  rtx par, insn;

  offset = 0;
  start_reg = ((sel & SAVRES_REG) == SAVRES_GPR
	       ? info->first_gp_reg_save
	       : (sel & SAVRES_REG) == SAVRES_FPR
	       ? info->first_fp_reg_save
	       : (sel & SAVRES_REG) == SAVRES_VR
	       ? info->first_altivec_reg_save
	       : -1);
  end_reg = ((sel & SAVRES_REG) == SAVRES_GPR
	     ? 32
	     : (sel & SAVRES_REG) == SAVRES_FPR
	     ? 64
	     : (sel & SAVRES_REG) == SAVRES_VR
	     ? LAST_ALTIVEC_REGNO + 1
	     : -1);
  n_regs = end_reg - start_reg;
  p = rtvec_alloc (3 + ((sel & SAVRES_LR) ? 1 : 0)
		   + ((sel & SAVRES_REG) == SAVRES_VR ? 1 : 0)
		   + n_regs);

  if (!(sel & SAVRES_SAVE) && (sel & SAVRES_LR))
    RTVEC_ELT (p, offset++) = ret_rtx;

  RTVEC_ELT (p, offset++)
    = gen_rtx_CLOBBER (VOIDmode, gen_rtx_REG (Pmode, LR_REGNO));

  sym = rs6000_savres_routine_sym (info, sel);
  RTVEC_ELT (p, offset++) = gen_rtx_USE (VOIDmode, sym);

  use_reg = ptr_regno_for_savres (sel);
  if ((sel & SAVRES_REG) == SAVRES_VR)
    {
      /* Vector regs are saved/restored using [reg+reg] addressing.  */
      RTVEC_ELT (p, offset++)
	= gen_rtx_CLOBBER (VOIDmode, gen_rtx_REG (Pmode, use_reg));
      RTVEC_ELT (p, offset++)
	= gen_rtx_USE (VOIDmode, gen_rtx_REG (Pmode, 0));
    }
  else
    RTVEC_ELT (p, offset++)
      = gen_rtx_USE (VOIDmode, gen_rtx_REG (Pmode, use_reg));

  for (i = 0; i < end_reg - start_reg; i++)
    RTVEC_ELT (p, i + offset)
      = gen_frame_set (gen_rtx_REG (reg_mode, start_reg + i),
		       frame_reg_rtx, save_area_offset + reg_size * i,
		       (sel & SAVRES_SAVE) != 0);

  if ((sel & SAVRES_SAVE) && (sel & SAVRES_LR))
    RTVEC_ELT (p, i + offset)
      = gen_frame_store (gen_rtx_REG (Pmode, 0), frame_reg_rtx, lr_offset);

  par = gen_rtx_PARALLEL (VOIDmode, p);

  if (!(sel & SAVRES_SAVE) && (sel & SAVRES_LR))
    {
      insn = emit_jump_insn (par);
      JUMP_LABEL (insn) = ret_rtx;
    }
  else
    insn = emit_insn (par);
  return insn;
}

/* Emit code to store CR fields that need to be saved into REG.  */

static void
rs6000_emit_move_from_cr (rtx reg)
{
  /* Only the ELFv2 ABI allows storing only selected fields.  */
  if (DEFAULT_ABI == ABI_ELFv2 && TARGET_MFCRF)
    {
      int i, cr_reg[8], count = 0;

      /* Collect CR fields that must be saved.  */
      for (i = 0; i < 8; i++)
	if (save_reg_p (CR0_REGNO + i))
	  cr_reg[count++] = i;

      /* If it's just a single one, use mfcrf.  */
      if (count == 1)
	{
	  rtvec p = rtvec_alloc (1);
	  rtvec r = rtvec_alloc (2);
	  RTVEC_ELT (r, 0) = gen_rtx_REG (CCmode, CR0_REGNO + cr_reg[0]);
	  RTVEC_ELT (r, 1) = GEN_INT (1 << (7 - cr_reg[0]));
	  RTVEC_ELT (p, 0)
	    = gen_rtx_SET (VOIDmode, reg,
			   gen_rtx_UNSPEC (SImode, r, UNSPEC_MOVESI_FROM_CR));

	  emit_insn (gen_rtx_PARALLEL (VOIDmode, p));
	  return;
	}

      /* ??? It might be better to handle count == 2 / 3 cases here
	 as well, using logical operations to combine the values.  */
    }

  emit_insn (gen_movesi_from_cr (reg));
}

/* Determine whether the gp REG is really used.  */

static bool
rs6000_reg_live_or_pic_offset_p (int reg)
{
  /* If the function calls eh_return, claim used all the registers that would
     be checked for liveness otherwise.  This is required for the PIC offset
     register with -mminimal-toc on AIX, as it is advertised as "fixed" for
     register allocation purposes in this case.  */

  return (((crtl->calls_eh_return || df_regs_ever_live_p (reg))
           && (!call_used_regs[reg]
               || (reg == RS6000_PIC_OFFSET_TABLE_REGNUM
		   && !TARGET_SINGLE_PIC_BASE
                   && TARGET_TOC && TARGET_MINIMAL_TOC)))
          || (reg == RS6000_PIC_OFFSET_TABLE_REGNUM
	      && !TARGET_SINGLE_PIC_BASE
              && ((DEFAULT_ABI == ABI_V4 && flag_pic != 0)
                  || (DEFAULT_ABI == ABI_DARWIN && flag_pic))));
}

/* Emit function prologue as insns.  */

void
rs6000_emit_prologue (void)
{
  rs6000_stack_t *info = rs6000_stack_info ();
  enum machine_mode reg_mode = Pmode;
  int reg_size = TARGET_32BIT ? 4 : 8;
  rtx sp_reg_rtx = gen_rtx_REG (Pmode, STACK_POINTER_REGNUM);
  rtx frame_reg_rtx = sp_reg_rtx;
  unsigned int cr_save_regno;
  rtx cr_save_rtx = NULL_RTX;
  rtx insn;
  int strategy;
  int using_static_chain_p = (cfun->static_chain_decl != NULL_TREE
			      && df_regs_ever_live_p (STATIC_CHAIN_REGNUM)
			      && call_used_regs[STATIC_CHAIN_REGNUM]);
  /* Offset to top of frame for frame_reg and sp respectively.  */
  HOST_WIDE_INT frame_off = 0;
  HOST_WIDE_INT sp_off = 0;

#ifdef ENABLE_CHECKING
  /* Track and check usage of r0, r11, r12.  */
  int reg_inuse = using_static_chain_p ? 1 << 11 : 0;
#define START_USE(R) do \
  {						\
    gcc_assert ((reg_inuse & (1 << (R))) == 0);	\
    reg_inuse |= 1 << (R);			\
  } while (0)
#define END_USE(R) do \
  {						\
    gcc_assert ((reg_inuse & (1 << (R))) != 0);	\
    reg_inuse &= ~(1 << (R));			\
  } while (0)
#define NOT_INUSE(R) do \
  {						\
    gcc_assert ((reg_inuse & (1 << (R))) == 0);	\
  } while (0)
#else
#define START_USE(R) do {} while (0)
#define END_USE(R) do {} while (0)
#define NOT_INUSE(R) do {} while (0)
#endif

  if (DEFAULT_ABI == ABI_ELFv2)
    {
      cfun->machine->r2_setup_needed = df_regs_ever_live_p (TOC_REGNUM);

      /* With -mminimal-toc we may generate an extra use of r2 below.  */
      if (!TARGET_SINGLE_PIC_BASE
	  && TARGET_TOC && TARGET_MINIMAL_TOC && get_pool_size () != 0)
	cfun->machine->r2_setup_needed = true;
    }


  if (flag_stack_usage_info)
    current_function_static_stack_size = info->total_size;

  if (flag_stack_check == STATIC_BUILTIN_STACK_CHECK)
    {
      HOST_WIDE_INT size = info->total_size;

      if (crtl->is_leaf && !cfun->calls_alloca)
	{
	  if (size > PROBE_INTERVAL && size > STACK_CHECK_PROTECT)
	    rs6000_emit_probe_stack_range (STACK_CHECK_PROTECT,
					   size - STACK_CHECK_PROTECT);
	}
      else if (size > 0)
	rs6000_emit_probe_stack_range (STACK_CHECK_PROTECT, size);
    }

  if (TARGET_FIX_AND_CONTINUE)
    {
      /* gdb on darwin arranges to forward a function from the old
	 address by modifying the first 5 instructions of the function
	 to branch to the overriding function.  This is necessary to
	 permit function pointers that point to the old function to
	 actually forward to the new function.  */
      emit_insn (gen_nop ());
      emit_insn (gen_nop ());
      emit_insn (gen_nop ());
      emit_insn (gen_nop ());
      emit_insn (gen_nop ());
    }

  if (TARGET_SPE_ABI && info->spe_64bit_regs_used != 0)
    {
      reg_mode = V2SImode;
      reg_size = 8;
    }

  /* Handle world saves specially here.  */
  if (WORLD_SAVE_P (info))
    {
      int i, j, sz;
      rtx treg;
      rtvec p;
      rtx reg0;

      /* save_world expects lr in r0. */
      reg0 = gen_rtx_REG (Pmode, 0);
      if (info->lr_save_p)
	{
	  insn = emit_move_insn (reg0,
				 gen_rtx_REG (Pmode, LR_REGNO));
	  RTX_FRAME_RELATED_P (insn) = 1;
	}

      /* The SAVE_WORLD and RESTORE_WORLD routines make a number of
	 assumptions about the offsets of various bits of the stack
	 frame.  */
      gcc_assert (info->gp_save_offset == -220
		  && info->fp_save_offset == -144
		  && info->lr_save_offset == 8
		  && info->cr_save_offset == 4
		  && info->push_p
		  && info->lr_save_p
		  && (!crtl->calls_eh_return
		      || info->ehrd_offset == -432)
		  && info->vrsave_save_offset == -224
		  && info->altivec_save_offset == -416);

      treg = gen_rtx_REG (SImode, 11);
      emit_move_insn (treg, GEN_INT (-info->total_size));

      /* SAVE_WORLD takes the caller's LR in R0 and the frame size
	 in R11.  It also clobbers R12, so beware!  */

      /* Preserve CR2 for save_world prologues */
      sz = 5;
      sz += 32 - info->first_gp_reg_save;
      sz += 64 - info->first_fp_reg_save;
      sz += LAST_ALTIVEC_REGNO - info->first_altivec_reg_save + 1;
      p = rtvec_alloc (sz);
      j = 0;
      RTVEC_ELT (p, j++) = gen_rtx_CLOBBER (VOIDmode,
					    gen_rtx_REG (SImode,
							 LR_REGNO));
      RTVEC_ELT (p, j++) = gen_rtx_USE (VOIDmode,
					gen_rtx_SYMBOL_REF (Pmode,
							    "*save_world"));
      /* We do floats first so that the instruction pattern matches
	 properly.  */
      for (i = 0; i < 64 - info->first_fp_reg_save; i++)
	RTVEC_ELT (p, j++)
	  = gen_frame_store (gen_rtx_REG (TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT
					  ? DFmode : SFmode,
					  info->first_fp_reg_save + i),
			     frame_reg_rtx,
			     info->fp_save_offset + frame_off + 8 * i);
      for (i = 0; info->first_altivec_reg_save + i <= LAST_ALTIVEC_REGNO; i++)
	RTVEC_ELT (p, j++)
	  = gen_frame_store (gen_rtx_REG (V4SImode,
					  info->first_altivec_reg_save + i),
			     frame_reg_rtx,
			     info->altivec_save_offset + frame_off + 16 * i);
      for (i = 0; i < 32 - info->first_gp_reg_save; i++)
	RTVEC_ELT (p, j++)
	  = gen_frame_store (gen_rtx_REG (reg_mode, info->first_gp_reg_save + i),
			     frame_reg_rtx,
			     info->gp_save_offset + frame_off + reg_size * i);

      /* CR register traditionally saved as CR2.  */
      RTVEC_ELT (p, j++)
	= gen_frame_store (gen_rtx_REG (SImode, CR2_REGNO),
			   frame_reg_rtx, info->cr_save_offset + frame_off);
      /* Explain about use of R0.  */
      if (info->lr_save_p)
	RTVEC_ELT (p, j++)
	  = gen_frame_store (reg0,
			     frame_reg_rtx, info->lr_save_offset + frame_off);
      /* Explain what happens to the stack pointer.  */
      {
	rtx newval = gen_rtx_PLUS (Pmode, sp_reg_rtx, treg);
	RTVEC_ELT (p, j++) = gen_rtx_SET (VOIDmode, sp_reg_rtx, newval);
      }

      insn = emit_insn (gen_rtx_PARALLEL (VOIDmode, p));
      rs6000_frame_related (insn, frame_reg_rtx, sp_off - frame_off,
			    treg, GEN_INT (-info->total_size), NULL_RTX);
      sp_off = frame_off = info->total_size;
    }

  strategy = info->savres_strategy;

  /* For V.4, update stack before we do any saving and set back pointer.  */
  if (! WORLD_SAVE_P (info)
      && info->push_p
      && (DEFAULT_ABI == ABI_V4
	  || crtl->calls_eh_return))
    {
      bool need_r11 = (TARGET_SPE
		       ? (!(strategy & SAVE_INLINE_GPRS)
			  && info->spe_64bit_regs_used == 0)
		       : (!(strategy & SAVE_INLINE_FPRS)
			  || !(strategy & SAVE_INLINE_GPRS)
			  || !(strategy & SAVE_INLINE_VRS)));
      int ptr_regno = -1;
      rtx ptr_reg = NULL_RTX;
      int ptr_off = 0;

      if (info->total_size < 32767)
	frame_off = info->total_size;
      else if (need_r11)
	ptr_regno = 11;
      else if (info->cr_save_p
	       || info->lr_save_p
	       || info->first_fp_reg_save < 64
	       || info->first_gp_reg_save < 32
	       || info->altivec_size != 0
	       || info->vrsave_mask != 0
	       || crtl->calls_eh_return)
	ptr_regno = 12;
      else
	{
	  /* The prologue won't be saving any regs so there is no need
	     to set up a frame register to access any frame save area.
	     We also won't be using frame_off anywhere below, but set
	     the correct value anyway to protect against future
	     changes to this function.  */
	  frame_off = info->total_size;
	}
      if (ptr_regno != -1)
	{
	  /* Set up the frame offset to that needed by the first
	     out-of-line save function.  */
	  START_USE (ptr_regno);
	  ptr_reg = gen_rtx_REG (Pmode, ptr_regno);
	  frame_reg_rtx = ptr_reg;
	  if (!(strategy & SAVE_INLINE_FPRS) && info->fp_size != 0)
	    gcc_checking_assert (info->fp_save_offset + info->fp_size == 0);
	  else if (!(strategy & SAVE_INLINE_GPRS) && info->first_gp_reg_save < 32)
	    ptr_off = info->gp_save_offset + info->gp_size;
	  else if (!(strategy & SAVE_INLINE_VRS) && info->altivec_size != 0)
	    ptr_off = info->altivec_save_offset + info->altivec_size;
	  frame_off = -ptr_off;
	}
      rs6000_emit_allocate_stack (info->total_size, ptr_reg, ptr_off);
      sp_off = info->total_size;
      if (frame_reg_rtx != sp_reg_rtx)
	rs6000_emit_stack_tie (frame_reg_rtx, false);
    }

  /* If we use the link register, get it into r0.  */
  if (!WORLD_SAVE_P (info) && info->lr_save_p)
    {
      rtx addr, reg, mem;

      reg = gen_rtx_REG (Pmode, 0);
      START_USE (0);
      insn = emit_move_insn (reg, gen_rtx_REG (Pmode, LR_REGNO));
      RTX_FRAME_RELATED_P (insn) = 1;

      if (!(strategy & (SAVE_NOINLINE_GPRS_SAVES_LR
			| SAVE_NOINLINE_FPRS_SAVES_LR)))
	{
	  addr = gen_rtx_PLUS (Pmode, frame_reg_rtx,
			       GEN_INT (info->lr_save_offset + frame_off));
	  mem = gen_rtx_MEM (Pmode, addr);
	  /* This should not be of rs6000_sr_alias_set, because of
	     __builtin_return_address.  */

	  insn = emit_move_insn (mem, reg);
	  rs6000_frame_related (insn, frame_reg_rtx, sp_off - frame_off,
				NULL_RTX, NULL_RTX, NULL_RTX);
	  END_USE (0);
	}
    }

  /* If we need to save CR, put it into r12 or r11.  Choose r12 except when
     r12 will be needed by out-of-line gpr restore.  */
  cr_save_regno = ((DEFAULT_ABI == ABI_AIX || DEFAULT_ABI == ABI_ELFv2)
		   && !(strategy & (SAVE_INLINE_GPRS
				    | SAVE_NOINLINE_GPRS_SAVES_LR))
		   ? 11 : 12);
  if (!WORLD_SAVE_P (info)
      && info->cr_save_p
      && REGNO (frame_reg_rtx) != cr_save_regno
      && !(using_static_chain_p && cr_save_regno == 11))
    {
      cr_save_rtx = gen_rtx_REG (SImode, cr_save_regno);
      START_USE (cr_save_regno);
      rs6000_emit_move_from_cr (cr_save_rtx);
    }

  /* Do any required saving of fpr's.  If only one or two to save, do
     it ourselves.  Otherwise, call function.  */
  if (!WORLD_SAVE_P (info) && (strategy & SAVE_INLINE_FPRS))
    {
      int i;
      for (i = 0; i < 64 - info->first_fp_reg_save; i++)
	if (save_reg_p (info->first_fp_reg_save + i))
	  emit_frame_save (frame_reg_rtx,
			   (TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT
			    ? DFmode : SFmode),
			   info->first_fp_reg_save + i,
			   info->fp_save_offset + frame_off + 8 * i,
			   sp_off - frame_off);
    }
  else if (!WORLD_SAVE_P (info) && info->first_fp_reg_save != 64)
    {
      bool lr = (strategy & SAVE_NOINLINE_FPRS_SAVES_LR) != 0;
      int sel = SAVRES_SAVE | SAVRES_FPR | (lr ? SAVRES_LR : 0);
      unsigned ptr_regno = ptr_regno_for_savres (sel);
      rtx ptr_reg = frame_reg_rtx;

      if (REGNO (frame_reg_rtx) == ptr_regno)
	gcc_checking_assert (frame_off == 0);
      else
	{
	  ptr_reg = gen_rtx_REG (Pmode, ptr_regno);
	  NOT_INUSE (ptr_regno);
	  emit_insn (gen_add3_insn (ptr_reg,
				    frame_reg_rtx, GEN_INT (frame_off)));
	}
      insn = rs6000_emit_savres_rtx (info, ptr_reg,
				     info->fp_save_offset,
				     info->lr_save_offset,
				     DFmode, sel);
      rs6000_frame_related (insn, ptr_reg, sp_off,
			    NULL_RTX, NULL_RTX, NULL_RTX);
      if (lr)
	END_USE (0);
    }

  /* Save GPRs.  This is done as a PARALLEL if we are using
     the store-multiple instructions.  */
  if (!WORLD_SAVE_P (info)
      && TARGET_SPE_ABI
      && info->spe_64bit_regs_used != 0
      && info->first_gp_reg_save != 32)
    {
      int i;
      rtx spe_save_area_ptr;
      HOST_WIDE_INT save_off;
      int ool_adjust = 0;

      /* Determine whether we can address all of the registers that need
	 to be saved with an offset from frame_reg_rtx that fits in
	 the small const field for SPE memory instructions.  */
      int spe_regs_addressable
	= (SPE_CONST_OFFSET_OK (info->spe_gp_save_offset + frame_off
				+ reg_size * (32 - info->first_gp_reg_save - 1))
	   && (strategy & SAVE_INLINE_GPRS));

      if (spe_regs_addressable)
	{
	  spe_save_area_ptr = frame_reg_rtx;
	  save_off = frame_off;
	}
      else
	{
	  /* Make r11 point to the start of the SPE save area.  We need
	     to be careful here if r11 is holding the static chain.  If
	     it is, then temporarily save it in r0.  */
	  HOST_WIDE_INT offset;

	  if (!(strategy & SAVE_INLINE_GPRS))
	    ool_adjust = 8 * (info->first_gp_reg_save - FIRST_SAVED_GP_REGNO);
	  offset = info->spe_gp_save_offset + frame_off - ool_adjust;
	  spe_save_area_ptr = gen_rtx_REG (Pmode, 11);
	  save_off = frame_off - offset;

	  if (using_static_chain_p)
	    {
	      rtx r0 = gen_rtx_REG (Pmode, 0);

	      START_USE (0);
	      gcc_assert (info->first_gp_reg_save > 11);

	      emit_move_insn (r0, spe_save_area_ptr);
	    }
	  else if (REGNO (frame_reg_rtx) != 11)
	    START_USE (11);

	  emit_insn (gen_addsi3 (spe_save_area_ptr,
				 frame_reg_rtx, GEN_INT (offset)));
	  if (!using_static_chain_p && REGNO (frame_reg_rtx) == 11)
	    frame_off = -info->spe_gp_save_offset + ool_adjust;
	}

      if ((strategy & SAVE_INLINE_GPRS))
	{
	  for (i = 0; i < 32 - info->first_gp_reg_save; i++)
	    if (rs6000_reg_live_or_pic_offset_p (info->first_gp_reg_save + i))
	      emit_frame_save (spe_save_area_ptr, reg_mode,
			       info->first_gp_reg_save + i,
			       (info->spe_gp_save_offset + save_off
				+ reg_size * i),
			       sp_off - save_off);
	}
      else
	{
	  insn = rs6000_emit_savres_rtx (info, spe_save_area_ptr,
					 info->spe_gp_save_offset + save_off,
					 0, reg_mode,
					 SAVRES_SAVE | SAVRES_GPR);

	  rs6000_frame_related (insn, spe_save_area_ptr, sp_off - save_off,
				NULL_RTX, NULL_RTX, NULL_RTX);
	}

      /* Move the static chain pointer back.  */
      if (!spe_regs_addressable)
	{
	  if (using_static_chain_p)
	    {
	      emit_move_insn (spe_save_area_ptr, gen_rtx_REG (Pmode, 0));
	      END_USE (0);
	    }
	  else if (REGNO (frame_reg_rtx) != 11)
	    END_USE (11);
	}
    }
  else if (!WORLD_SAVE_P (info) && !(strategy & SAVE_INLINE_GPRS))
    {
      bool lr = (strategy & SAVE_NOINLINE_GPRS_SAVES_LR) != 0;
      int sel = SAVRES_SAVE | SAVRES_GPR | (lr ? SAVRES_LR : 0);
      unsigned ptr_regno = ptr_regno_for_savres (sel);
      rtx ptr_reg = frame_reg_rtx;
      bool ptr_set_up = REGNO (ptr_reg) == ptr_regno;
      int end_save = info->gp_save_offset + info->gp_size;
      int ptr_off;

      if (!ptr_set_up)
	ptr_reg = gen_rtx_REG (Pmode, ptr_regno);

      /* Need to adjust r11 (r12) if we saved any FPRs.  */
      if (end_save + frame_off != 0)
	{
	  rtx offset = GEN_INT (end_save + frame_off);

	  if (ptr_set_up)
	    frame_off = -end_save;
	  else
	    NOT_INUSE (ptr_regno);
	  emit_insn (gen_add3_insn (ptr_reg, frame_reg_rtx, offset));
	}
      else if (!ptr_set_up)
	{
	  NOT_INUSE (ptr_regno);
	  emit_move_insn (ptr_reg, frame_reg_rtx);
	}
      ptr_off = -end_save;
      insn = rs6000_emit_savres_rtx (info, ptr_reg,
				     info->gp_save_offset + ptr_off,
				     info->lr_save_offset + ptr_off,
				     reg_mode, sel);
      rs6000_frame_related (insn, ptr_reg, sp_off - ptr_off,
			    NULL_RTX, NULL_RTX, NULL_RTX);
      if (lr)
	END_USE (0);
    }
  else if (!WORLD_SAVE_P (info) && (strategy & SAVRES_MULTIPLE))
    {
      rtvec p;
      int i;
      p = rtvec_alloc (32 - info->first_gp_reg_save);
      for (i = 0; i < 32 - info->first_gp_reg_save; i++)
	RTVEC_ELT (p, i)
	  = gen_frame_store (gen_rtx_REG (reg_mode, info->first_gp_reg_save + i),
			     frame_reg_rtx,
			     info->gp_save_offset + frame_off + reg_size * i);
      insn = emit_insn (gen_rtx_PARALLEL (VOIDmode, p));
      rs6000_frame_related (insn, frame_reg_rtx, sp_off - frame_off,
			    NULL_RTX, NULL_RTX, NULL_RTX);
    }
  else if (!WORLD_SAVE_P (info))
    {
      int i;
      for (i = 0; i < 32 - info->first_gp_reg_save; i++)
	if (rs6000_reg_live_or_pic_offset_p (info->first_gp_reg_save + i))
	  emit_frame_save (frame_reg_rtx, reg_mode,
			   info->first_gp_reg_save + i,
			   info->gp_save_offset + frame_off + reg_size * i,
			   sp_off - frame_off);
    }

  if (crtl->calls_eh_return)
    {
      unsigned int i;
      rtvec p;

      for (i = 0; ; ++i)
	{
	  unsigned int regno = EH_RETURN_DATA_REGNO (i);
	  if (regno == INVALID_REGNUM)
	    break;
	}

      p = rtvec_alloc (i);

      for (i = 0; ; ++i)
	{
	  unsigned int regno = EH_RETURN_DATA_REGNO (i);
	  if (regno == INVALID_REGNUM)
	    break;

	  insn
	    = gen_frame_store (gen_rtx_REG (reg_mode, regno),
			       sp_reg_rtx,
			       info->ehrd_offset + sp_off + reg_size * (int) i);
	  RTVEC_ELT (p, i) = insn;
	  RTX_FRAME_RELATED_P (insn) = 1;
	}

      insn = emit_insn (gen_blockage ());
      RTX_FRAME_RELATED_P (insn) = 1;
      add_reg_note (insn, REG_FRAME_RELATED_EXPR, gen_rtx_PARALLEL (VOIDmode, p));
    }

  /* In AIX ABI we need to make sure r2 is really saved.  */
  if (TARGET_AIX && crtl->calls_eh_return)
    {
      rtx tmp_reg, tmp_reg_si, hi, lo, compare_result, toc_save_done, jump;
      rtx save_insn, join_insn, note;
      long toc_restore_insn;

      tmp_reg = gen_rtx_REG (Pmode, 11);
      tmp_reg_si = gen_rtx_REG (SImode, 11);
      if (using_static_chain_p)
	{
	  START_USE (0);
	  emit_move_insn (gen_rtx_REG (Pmode, 0), tmp_reg);
	}
      else
	START_USE (11);
      emit_move_insn (tmp_reg, gen_rtx_REG (Pmode, LR_REGNO));
      /* Peek at instruction to which this function returns.  If it's
	 restoring r2, then we know we've already saved r2.  We can't
	 unconditionally save r2 because the value we have will already
	 be updated if we arrived at this function via a plt call or
	 toc adjusting stub.  */
      emit_move_insn (tmp_reg_si, gen_rtx_MEM (SImode, tmp_reg));
      toc_restore_insn = ((TARGET_32BIT ? 0x80410000 : 0xE8410000)
			  + RS6000_TOC_SAVE_SLOT);
      hi = gen_int_mode (toc_restore_insn & ~0xffff, SImode);
      emit_insn (gen_xorsi3 (tmp_reg_si, tmp_reg_si, hi));
      compare_result = gen_rtx_REG (CCUNSmode, CR0_REGNO);
      validate_condition_mode (EQ, CCUNSmode);
      lo = gen_int_mode (toc_restore_insn & 0xffff, SImode);
      emit_insn (gen_rtx_SET (VOIDmode, compare_result,
			      gen_rtx_COMPARE (CCUNSmode, tmp_reg_si, lo)));
      toc_save_done = gen_label_rtx ();
      jump = gen_rtx_IF_THEN_ELSE (VOIDmode,
				   gen_rtx_EQ (VOIDmode, compare_result,
					       const0_rtx),
				   gen_rtx_LABEL_REF (VOIDmode, toc_save_done),
				   pc_rtx);
      jump = emit_jump_insn (gen_rtx_SET (VOIDmode, pc_rtx, jump));
      JUMP_LABEL (jump) = toc_save_done;
      LABEL_NUSES (toc_save_done) += 1;

      save_insn = emit_frame_save (frame_reg_rtx, reg_mode,
				   TOC_REGNUM, frame_off + RS6000_TOC_SAVE_SLOT,
				   sp_off - frame_off);

      emit_label (toc_save_done);

      /* ??? If we leave SAVE_INSN as marked as saving R2, then we'll
	 have a CFG that has different saves along different paths.
	 Move the note to a dummy blockage insn, which describes that
	 R2 is unconditionally saved after the label.  */
      /* ??? An alternate representation might be a special insn pattern
	 containing both the branch and the store.  That might let the
	 code that minimizes the number of DW_CFA_advance opcodes better
	 freedom in placing the annotations.  */
      note = find_reg_note (save_insn, REG_FRAME_RELATED_EXPR, NULL);
      if (note)
	remove_note (save_insn, note);
      else
	note = alloc_reg_note (REG_FRAME_RELATED_EXPR,
			       copy_rtx (PATTERN (save_insn)), NULL_RTX);
      RTX_FRAME_RELATED_P (save_insn) = 0;

      join_insn = emit_insn (gen_blockage ());
      REG_NOTES (join_insn) = note;
      RTX_FRAME_RELATED_P (join_insn) = 1;

      if (using_static_chain_p)
	{
	  emit_move_insn (tmp_reg, gen_rtx_REG (Pmode, 0));
	  END_USE (0);
	}
      else
	END_USE (11);
    }

  /* Save CR if we use any that must be preserved.  */
  if (!WORLD_SAVE_P (info) && info->cr_save_p)
    {
      rtx addr = gen_rtx_PLUS (Pmode, frame_reg_rtx,
			       GEN_INT (info->cr_save_offset + frame_off));
      rtx mem = gen_frame_mem (SImode, addr);

      /* If we didn't copy cr before, do so now using r0.  */
      if (cr_save_rtx == NULL_RTX)
	{
	  START_USE (0);
	  cr_save_rtx = gen_rtx_REG (SImode, 0);
	  rs6000_emit_move_from_cr (cr_save_rtx);
	}

      /* Saving CR requires a two-instruction sequence: one instruction
	 to move the CR to a general-purpose register, and a second
	 instruction that stores the GPR to memory.

	 We do not emit any DWARF CFI records for the first of these,
	 because we cannot properly represent the fact that CR is saved in
	 a register.  One reason is that we cannot express that multiple
	 CR fields are saved; another reason is that on 64-bit, the size
	 of the CR register in DWARF (4 bytes) differs from the size of
	 a general-purpose register.

	 This means if any intervening instruction were to clobber one of
	 the call-saved CR fields, we'd have incorrect CFI.  To prevent
	 this from happening, we mark the store to memory as a use of
	 those CR fields, which prevents any such instruction from being
	 scheduled in between the two instructions.  */
      rtx crsave_v[9];
      int n_crsave = 0;
      int i;

      crsave_v[n_crsave++] = gen_rtx_SET (VOIDmode, mem, cr_save_rtx);
      for (i = 0; i < 8; i++)
	if (save_reg_p (CR0_REGNO + i))
	  crsave_v[n_crsave++]
	    = gen_rtx_USE (VOIDmode, gen_rtx_REG (CCmode, CR0_REGNO + i));

      insn = emit_insn (gen_rtx_PARALLEL (VOIDmode,
					  gen_rtvec_v (n_crsave, crsave_v)));
      END_USE (REGNO (cr_save_rtx));

      /* Now, there's no way that dwarf2out_frame_debug_expr is going to
	 understand '(unspec:SI [(reg:CC 68) ...] UNSPEC_MOVESI_FROM_CR)',
	 so we need to construct a frame expression manually.  */
      RTX_FRAME_RELATED_P (insn) = 1;

      /* Update address to be stack-pointer relative, like
	 rs6000_frame_related would do.  */
      addr = gen_rtx_PLUS (Pmode, gen_rtx_REG (Pmode, STACK_POINTER_REGNUM),
			   GEN_INT (info->cr_save_offset + sp_off));
      mem = gen_frame_mem (SImode, addr);

      if (DEFAULT_ABI == ABI_ELFv2)
	{
	  /* In the ELFv2 ABI we generate separate CFI records for each
	     CR field that was actually saved.  They all point to the
	     same 32-bit stack slot.  */
	  rtx crframe[8];
	  int n_crframe = 0;

	  for (i = 0; i < 8; i++)
	    if (save_reg_p (CR0_REGNO + i))
	      {
		crframe[n_crframe]
		  = gen_rtx_SET (VOIDmode, mem,
				 gen_rtx_REG (SImode, CR0_REGNO + i));

		RTX_FRAME_RELATED_P (crframe[n_crframe]) = 1;
		n_crframe++;
	     }

	  add_reg_note (insn, REG_FRAME_RELATED_EXPR,
			gen_rtx_PARALLEL (VOIDmode,
					  gen_rtvec_v (n_crframe, crframe)));
	}
      else
	{
	  /* In other ABIs, by convention, we use a single CR regnum to
	     represent the fact that all call-saved CR fields are saved.
	     We use CR2_REGNO to be compatible with gcc-2.95 on Linux.  */
	  rtx set = gen_rtx_SET (VOIDmode, mem,
				 gen_rtx_REG (SImode, CR2_REGNO));
	  add_reg_note (insn, REG_FRAME_RELATED_EXPR, set);
	}
    }

  /* In the ELFv2 ABI we need to save all call-saved CR fields into
     *separate* slots if the routine calls __builtin_eh_return, so
     that they can be independently restored by the unwinder.  */
  if (DEFAULT_ABI == ABI_ELFv2 && crtl->calls_eh_return)
    {
      int i, cr_off = info->ehcr_offset;
      rtx crsave;

      /* ??? We might get better performance by using multiple mfocrf
	 instructions.  */
      crsave = gen_rtx_REG (SImode, 0);
      emit_insn (gen_movesi_from_cr (crsave));

      for (i = 0; i < 8; i++)
	if (!call_used_regs[CR0_REGNO + i])
	  {
	    rtvec p = rtvec_alloc (2);
	    RTVEC_ELT (p, 0)
	      = gen_frame_store (crsave, frame_reg_rtx, cr_off + frame_off);
	    RTVEC_ELT (p, 1)
	      = gen_rtx_USE (VOIDmode, gen_rtx_REG (CCmode, CR0_REGNO + i));

	    insn = emit_insn (gen_rtx_PARALLEL (VOIDmode, p));

	    RTX_FRAME_RELATED_P (insn) = 1;
	    add_reg_note (insn, REG_FRAME_RELATED_EXPR,
			  gen_frame_store (gen_rtx_REG (SImode, CR0_REGNO + i),
					   sp_reg_rtx, cr_off + sp_off));

	    cr_off += reg_size;
	  }
    }

  /* Update stack and set back pointer unless this is V.4,
     for which it was done previously.  */
  if (!WORLD_SAVE_P (info) && info->push_p
      && !(DEFAULT_ABI == ABI_V4 || crtl->calls_eh_return))
    {
      rtx ptr_reg = NULL;
      int ptr_off = 0;

      /* If saving altivec regs we need to be able to address all save
	 locations using a 16-bit offset.  */
      if ((strategy & SAVE_INLINE_VRS) == 0
	  || (info->altivec_size != 0
	      && (info->altivec_save_offset + info->altivec_size - 16
		  + info->total_size - frame_off) > 32767)
	  || (info->vrsave_size != 0
	      && (info->vrsave_save_offset
		  + info->total_size - frame_off) > 32767))
	{
	  int sel = SAVRES_SAVE | SAVRES_VR;
	  unsigned ptr_regno = ptr_regno_for_savres (sel);

	  if (using_static_chain_p
	      && ptr_regno == STATIC_CHAIN_REGNUM)
	    ptr_regno = 12;
	  if (REGNO (frame_reg_rtx) != ptr_regno)
	    START_USE (ptr_regno);
	  ptr_reg = gen_rtx_REG (Pmode, ptr_regno);
	  frame_reg_rtx = ptr_reg;
	  ptr_off = info->altivec_save_offset + info->altivec_size;
	  frame_off = -ptr_off;
	}
      else if (REGNO (frame_reg_rtx) == 1)
	frame_off = info->total_size;
      rs6000_emit_allocate_stack (info->total_size, ptr_reg, ptr_off);
      sp_off = info->total_size;
      if (frame_reg_rtx != sp_reg_rtx)
	rs6000_emit_stack_tie (frame_reg_rtx, false);
    }

  /* Set frame pointer, if needed.  */
  if (frame_pointer_needed)
    {
      insn = emit_move_insn (gen_rtx_REG (Pmode, HARD_FRAME_POINTER_REGNUM),
			     sp_reg_rtx);
      RTX_FRAME_RELATED_P (insn) = 1;
    }

  /* Save AltiVec registers if needed.  Save here because the red zone does
     not always include AltiVec registers.  */
  if (!WORLD_SAVE_P (info) && TARGET_ALTIVEC_ABI
      && info->altivec_size != 0 && (strategy & SAVE_INLINE_VRS) == 0)
    {
      int end_save = info->altivec_save_offset + info->altivec_size;
      int ptr_off;
      /* Oddly, the vector save/restore functions point r0 at the end
	 of the save area, then use r11 or r12 to load offsets for
	 [reg+reg] addressing.  */
      rtx ptr_reg = gen_rtx_REG (Pmode, 0);
      int scratch_regno = ptr_regno_for_savres (SAVRES_SAVE | SAVRES_VR);
      rtx scratch_reg = gen_rtx_REG (Pmode, scratch_regno);

      gcc_checking_assert (scratch_regno == 11 || scratch_regno == 12);
      NOT_INUSE (0);
      if (end_save + frame_off != 0)
	{
	  rtx offset = GEN_INT (end_save + frame_off);

	  emit_insn (gen_add3_insn (ptr_reg, frame_reg_rtx, offset));
	}
      else
	emit_move_insn (ptr_reg, frame_reg_rtx);

      ptr_off = -end_save;
      insn = rs6000_emit_savres_rtx (info, scratch_reg,
				     info->altivec_save_offset + ptr_off,
				     0, V4SImode, SAVRES_SAVE | SAVRES_VR);
      rs6000_frame_related (insn, scratch_reg, sp_off - ptr_off,
			    NULL_RTX, NULL_RTX, NULL_RTX);
      if (REGNO (frame_reg_rtx) == REGNO (scratch_reg))
	{
	  /* The oddity mentioned above clobbered our frame reg.  */
	  emit_move_insn (frame_reg_rtx, ptr_reg);
	  frame_off = ptr_off;
	}
    }
  else if (!WORLD_SAVE_P (info) && TARGET_ALTIVEC_ABI
	   && info->altivec_size != 0)
    {
      int i;

      for (i = info->first_altivec_reg_save; i <= LAST_ALTIVEC_REGNO; ++i)
	if (info->vrsave_mask & ALTIVEC_REG_BIT (i))
	  {
	    rtx areg, savereg, mem, split_reg;
	    int offset;

	    offset = (info->altivec_save_offset + frame_off
		      + 16 * (i - info->first_altivec_reg_save));

	    savereg = gen_rtx_REG (V4SImode, i);

	    NOT_INUSE (0);
	    areg = gen_rtx_REG (Pmode, 0);
	    emit_move_insn (areg, GEN_INT (offset));

	    /* AltiVec addressing mode is [reg+reg].  */
	    mem = gen_frame_mem (V4SImode,
				 gen_rtx_PLUS (Pmode, frame_reg_rtx, areg));

	    insn = emit_move_insn (mem, savereg);

	    /* When we split a VSX store into two insns, we need to make
	       sure the DWARF info knows which register we are storing.
	       Pass it in to be used on the appropriate note.  */
	    if (!BYTES_BIG_ENDIAN
		&& GET_CODE (PATTERN (insn)) == SET
		&& GET_CODE (SET_SRC (PATTERN (insn))) == VEC_SELECT)
	      split_reg = savereg;
	    else
	      split_reg = NULL_RTX;

	    rs6000_frame_related (insn, frame_reg_rtx, sp_off - frame_off,
				  areg, GEN_INT (offset), split_reg);
	  }
    }

  /* VRSAVE is a bit vector representing which AltiVec registers
     are used.  The OS uses this to determine which vector
     registers to save on a context switch.  We need to save
     VRSAVE on the stack frame, add whatever AltiVec registers we
     used in this function, and do the corresponding magic in the
     epilogue.  */

  if (!WORLD_SAVE_P (info)
      && TARGET_ALTIVEC
      && TARGET_ALTIVEC_VRSAVE
      && info->vrsave_mask != 0)
    {
      rtx reg, vrsave;
      int offset;
      int save_regno;

      /* Get VRSAVE onto a GPR.  Note that ABI_V4 and ABI_DARWIN might
	 be using r12 as frame_reg_rtx and r11 as the static chain
	 pointer for nested functions.  */
      save_regno = 12;
      if ((DEFAULT_ABI == ABI_AIX || DEFAULT_ABI == ABI_ELFv2)
	  && !using_static_chain_p)
	save_regno = 11;
      else if (REGNO (frame_reg_rtx) == 12)
	{
	  save_regno = 11;
	  if (using_static_chain_p)
	    save_regno = 0;
	}

      NOT_INUSE (save_regno);
      reg = gen_rtx_REG (SImode, save_regno);
      vrsave = gen_rtx_REG (SImode, VRSAVE_REGNO);
      if (TARGET_MACHO)
	emit_insn (gen_get_vrsave_internal (reg));
      else
	emit_insn (gen_rtx_SET (VOIDmode, reg, vrsave));

      /* Save VRSAVE.  */
      offset = info->vrsave_save_offset + frame_off;
      insn = emit_insn (gen_frame_store (reg, frame_reg_rtx, offset));

      /* Include the registers in the mask.  */
      emit_insn (gen_iorsi3 (reg, reg, GEN_INT ((int) info->vrsave_mask)));

      insn = emit_insn (generate_set_vrsave (reg, info, 0));
    }

  /* If we are using RS6000_PIC_OFFSET_TABLE_REGNUM, we need to set it up.  */
  if (!TARGET_SINGLE_PIC_BASE
      && ((TARGET_TOC && TARGET_MINIMAL_TOC && get_pool_size () != 0)
	  || (DEFAULT_ABI == ABI_V4
	      && (flag_pic == 1 || (flag_pic && TARGET_SECURE_PLT))
	      && df_regs_ever_live_p (RS6000_PIC_OFFSET_TABLE_REGNUM))))
    {
      /* If emit_load_toc_table will use the link register, we need to save
	 it.  We use R12 for this purpose because emit_load_toc_table
	 can use register 0.  This allows us to use a plain 'blr' to return
	 from the procedure more often.  */
      int save_LR_around_toc_setup = (TARGET_ELF
				      && DEFAULT_ABI == ABI_V4
				      && flag_pic
				      && ! info->lr_save_p
				      && EDGE_COUNT (EXIT_BLOCK_PTR_FOR_FN (cfun)->preds) > 0);
      if (save_LR_around_toc_setup)
	{
	  rtx lr = gen_rtx_REG (Pmode, LR_REGNO);
	  rtx tmp = gen_rtx_REG (Pmode, 12);

	  insn = emit_move_insn (tmp, lr);
	  RTX_FRAME_RELATED_P (insn) = 1;

	  rs6000_emit_load_toc_table (TRUE);

	  insn = emit_move_insn (lr, tmp);
	  add_reg_note (insn, REG_CFA_RESTORE, lr);
	  RTX_FRAME_RELATED_P (insn) = 1;
	}
      else
	rs6000_emit_load_toc_table (TRUE);
    }

#if TARGET_MACHO
  if (!TARGET_SINGLE_PIC_BASE
      && DEFAULT_ABI == ABI_DARWIN
      && flag_pic && crtl->uses_pic_offset_table)
    {
      rtx lr = gen_rtx_REG (Pmode, LR_REGNO);
      rtx src = gen_rtx_SYMBOL_REF (Pmode, MACHOPIC_FUNCTION_BASE_NAME);

      /* Save and restore LR locally around this call (in R0).  */
      if (!info->lr_save_p)
	emit_move_insn (gen_rtx_REG (Pmode, 0), lr);

      emit_insn (gen_load_macho_picbase (src));

      emit_move_insn (gen_rtx_REG (Pmode,
				   RS6000_PIC_OFFSET_TABLE_REGNUM),
		      lr);

      if (!info->lr_save_p)
	emit_move_insn (lr, gen_rtx_REG (Pmode, 0));
    }
#endif

  /* If we need to, save the TOC register after doing the stack setup.
     Do not emit eh frame info for this save.  The unwinder wants info,
     conceptually attached to instructions in this function, about
     register values in the caller of this function.  This R2 may have
     already been changed from the value in the caller.
     We don't attempt to write accurate DWARF EH frame info for R2
     because code emitted by gcc for a (non-pointer) function call
     doesn't save and restore R2.  Instead, R2 is managed out-of-line
     by a linker generated plt call stub when the function resides in
     a shared library.  This behaviour is costly to describe in DWARF,
     both in terms of the size of DWARF info and the time taken in the
     unwinder to interpret it.  R2 changes, apart from the
     calls_eh_return case earlier in this function, are handled by
     linux-unwind.h frob_update_context.  */
  if (rs6000_save_toc_in_prologue_p ())
    {
      rtx reg = gen_rtx_REG (reg_mode, TOC_REGNUM);
      emit_insn (gen_frame_store (reg, sp_reg_rtx, RS6000_TOC_SAVE_SLOT));
    }
}

/* Write function prologue.  */

static void
rs6000_output_function_prologue (FILE *file,
				 HOST_WIDE_INT size ATTRIBUTE_UNUSED)
{
  rs6000_stack_t *info = rs6000_stack_info ();

  if (TARGET_DEBUG_STACK)
    debug_stack_info (info);

  /* Write .extern for any function we will call to save and restore
     fp values.  */
  if (info->first_fp_reg_save < 64
      && !TARGET_MACHO
      && !TARGET_ELF)
    {
      char *name;
      int regno = info->first_fp_reg_save - 32;

      if ((info->savres_strategy & SAVE_INLINE_FPRS) == 0)
	{
	  bool lr = (info->savres_strategy & SAVE_NOINLINE_FPRS_SAVES_LR) != 0;
	  int sel = SAVRES_SAVE | SAVRES_FPR | (lr ? SAVRES_LR : 0);
	  name = rs6000_savres_routine_name (info, regno, sel);
	  fprintf (file, "\t.extern %s\n", name);
	}
      if ((info->savres_strategy & REST_INLINE_FPRS) == 0)
	{
	  bool lr = (info->savres_strategy
		     & REST_NOINLINE_FPRS_DOESNT_RESTORE_LR) == 0;
	  int sel = SAVRES_FPR | (lr ? SAVRES_LR : 0);
	  name = rs6000_savres_routine_name (info, regno, sel);
	  fprintf (file, "\t.extern %s\n", name);
	}
    }

  /* ELFv2 ABI r2 setup code and local entry point.  This must follow
     immediately after the global entry point label.  */
  if (DEFAULT_ABI == ABI_ELFv2 && cfun->machine->r2_setup_needed)
    {
      const char *name = XSTR (XEXP (DECL_RTL (current_function_decl), 0), 0);

      fprintf (file, "0:\taddis 2,12,.TOC.-0b@ha\n");
      fprintf (file, "\taddi 2,2,.TOC.-0b@l\n");

      fputs ("\t.localentry\t", file);
      assemble_name (file, name);
      fputs (",.-", file);
      assemble_name (file, name);
      fputs ("\n", file);
    }

  /* Output -mprofile-kernel code.  This needs to be done here instead of
     in output_function_profile since it must go after the ELFv2 ABI
     local entry point.  */
  if (TARGET_PROFILE_KERNEL && crtl->profile)
    {
      gcc_assert (DEFAULT_ABI == ABI_AIX || DEFAULT_ABI == ABI_ELFv2);
      gcc_assert (!TARGET_32BIT);

      asm_fprintf (file, "\tmflr %s\n", reg_names[0]);
      asm_fprintf (file, "\tstd %s,16(%s)\n", reg_names[0], reg_names[1]);

      /* In the ELFv2 ABI we have no compiler stack word.  It must be
	 the resposibility of _mcount to preserve the static chain
	 register if required.  */
      if (DEFAULT_ABI != ABI_ELFv2
	  && cfun->static_chain_decl != NULL)
	{
	  asm_fprintf (file, "\tstd %s,24(%s)\n",
		       reg_names[STATIC_CHAIN_REGNUM], reg_names[1]);
	  fprintf (file, "\tbl %s\n", RS6000_MCOUNT);
	  asm_fprintf (file, "\tld %s,24(%s)\n",
		       reg_names[STATIC_CHAIN_REGNUM], reg_names[1]);
	}
      else
	fprintf (file, "\tbl %s\n", RS6000_MCOUNT);
    }

  rs6000_pic_labelno++;
}

/* Non-zero if vmx regs are restored before the frame pop, zero if
   we restore after the pop when possible.  */
#define ALWAYS_RESTORE_ALTIVEC_BEFORE_POP 0

/* Restoring cr is a two step process: loading a reg from the frame
   save, then moving the reg to cr.  For ABI_V4 we must let the
   unwinder know that the stack location is no longer valid at or
   before the stack deallocation, but we can't emit a cfa_restore for
   cr at the stack deallocation like we do for other registers.
   The trouble is that it is possible for the move to cr to be
   scheduled after the stack deallocation.  So say exactly where cr
   is located on each of the two insns.  */

static rtx
load_cr_save (int regno, rtx frame_reg_rtx, int offset, bool exit_func)
{
  rtx mem = gen_frame_mem_offset (SImode, frame_reg_rtx, offset);
  rtx reg = gen_rtx_REG (SImode, regno);
  rtx insn = emit_move_insn (reg, mem);

  if (!exit_func && DEFAULT_ABI == ABI_V4)
    {
      rtx cr = gen_rtx_REG (SImode, CR2_REGNO);
      rtx set = gen_rtx_SET (VOIDmode, reg, cr);

      add_reg_note (insn, REG_CFA_REGISTER, set);
      RTX_FRAME_RELATED_P (insn) = 1;
    }
  return reg;
}

/* Reload CR from REG.  */

static void
restore_saved_cr (rtx reg, int using_mfcr_multiple, bool exit_func)
{
  int count = 0;
  int i;

  if (using_mfcr_multiple)
    {
      for (i = 0; i < 8; i++)
	if (save_reg_p (CR0_REGNO + i))
	  count++;
      gcc_assert (count);
    }

  if (using_mfcr_multiple && count > 1)
    {
      rtx insn;
      rtvec p;
      int ndx;

      p = rtvec_alloc (count);

      ndx = 0;
      for (i = 0; i < 8; i++)
	if (save_reg_p (CR0_REGNO + i))
	  {
	    rtvec r = rtvec_alloc (2);
	    RTVEC_ELT (r, 0) = reg;
	    RTVEC_ELT (r, 1) = GEN_INT (1 << (7-i));
	    RTVEC_ELT (p, ndx) =
	      gen_rtx_SET (VOIDmode, gen_rtx_REG (CCmode, CR0_REGNO + i),
			   gen_rtx_UNSPEC (CCmode, r, UNSPEC_MOVESI_TO_CR));
	    ndx++;
	  }
      insn = emit_insn (gen_rtx_PARALLEL (VOIDmode, p));
      gcc_assert (ndx == count);

      /* For the ELFv2 ABI we generate a CFA_RESTORE for each
	 CR field separately.  */
      if (!exit_func && DEFAULT_ABI == ABI_ELFv2 && flag_shrink_wrap)
	{
	  for (i = 0; i < 8; i++)
	    if (save_reg_p (CR0_REGNO + i))
	      add_reg_note (insn, REG_CFA_RESTORE,
			    gen_rtx_REG (SImode, CR0_REGNO + i));

	  RTX_FRAME_RELATED_P (insn) = 1;
	}
    }
  else
    for (i = 0; i < 8; i++)
      if (save_reg_p (CR0_REGNO + i))
	{
	  rtx insn = emit_insn (gen_movsi_to_cr_one
				 (gen_rtx_REG (CCmode, CR0_REGNO + i), reg));

	  /* For the ELFv2 ABI we generate a CFA_RESTORE for each
	     CR field separately, attached to the insn that in fact
	     restores this particular CR field.  */
	  if (!exit_func && DEFAULT_ABI == ABI_ELFv2 && flag_shrink_wrap)
	    {
	      add_reg_note (insn, REG_CFA_RESTORE,
			    gen_rtx_REG (SImode, CR0_REGNO + i));

	      RTX_FRAME_RELATED_P (insn) = 1;
	    }
	}

  /* For other ABIs, we just generate a single CFA_RESTORE for CR2.  */
  if (!exit_func && DEFAULT_ABI != ABI_ELFv2
      && (DEFAULT_ABI == ABI_V4 || flag_shrink_wrap))
    {
      rtx insn = get_last_insn ();
      rtx cr = gen_rtx_REG (SImode, CR2_REGNO);

      add_reg_note (insn, REG_CFA_RESTORE, cr);
      RTX_FRAME_RELATED_P (insn) = 1;
    }
}

/* Like cr, the move to lr instruction can be scheduled after the
   stack deallocation, but unlike cr, its stack frame save is still
   valid.  So we only need to emit the cfa_restore on the correct
   instruction.  */

static void
load_lr_save (int regno, rtx frame_reg_rtx, int offset)
{
  rtx mem = gen_frame_mem_offset (Pmode, frame_reg_rtx, offset);
  rtx reg = gen_rtx_REG (Pmode, regno);

  emit_move_insn (reg, mem);
}

static void
restore_saved_lr (int regno, bool exit_func)
{
  rtx reg = gen_rtx_REG (Pmode, regno);
  rtx lr = gen_rtx_REG (Pmode, LR_REGNO);
  rtx insn = emit_move_insn (lr, reg);

  if (!exit_func && flag_shrink_wrap)
    {
      add_reg_note (insn, REG_CFA_RESTORE, lr);
      RTX_FRAME_RELATED_P (insn) = 1;
    }
}

static rtx
add_crlr_cfa_restore (const rs6000_stack_t *info, rtx cfa_restores)
{
  if (DEFAULT_ABI == ABI_ELFv2)
    {
      int i;
      for (i = 0; i < 8; i++)
	if (save_reg_p (CR0_REGNO + i))
	  {
	    rtx cr = gen_rtx_REG (SImode, CR0_REGNO + i);
	    cfa_restores = alloc_reg_note (REG_CFA_RESTORE, cr,
					   cfa_restores);
	  }
    }
  else if (info->cr_save_p)
    cfa_restores = alloc_reg_note (REG_CFA_RESTORE,
				   gen_rtx_REG (SImode, CR2_REGNO),
				   cfa_restores);

  if (info->lr_save_p)
    cfa_restores = alloc_reg_note (REG_CFA_RESTORE,
				   gen_rtx_REG (Pmode, LR_REGNO),
				   cfa_restores);
  return cfa_restores;
}

/* Return true if OFFSET from stack pointer can be clobbered by signals.
   V.4 doesn't have any stack cushion, AIX ABIs have 220 or 288 bytes
   below stack pointer not cloberred by signals.  */

static inline bool
offset_below_red_zone_p (HOST_WIDE_INT offset)
{
  return offset < (DEFAULT_ABI == ABI_V4
		   ? 0
		   : TARGET_32BIT ? -220 : -288);
}

/* Append CFA_RESTORES to any existing REG_NOTES on the last insn.  */

static void
emit_cfa_restores (rtx cfa_restores)
{
  rtx insn = get_last_insn ();
  rtx *loc = &REG_NOTES (insn);

  while (*loc)
    loc = &XEXP (*loc, 1);
  *loc = cfa_restores;
  RTX_FRAME_RELATED_P (insn) = 1;
}

/* Emit function epilogue as insns.  */

void
rs6000_emit_epilogue (int sibcall)
{
  rs6000_stack_t *info;
  int restoring_GPRs_inline;
  int restoring_FPRs_inline;
  int using_load_multiple;
  int using_mtcr_multiple;
  int use_backchain_to_restore_sp;
  int restore_lr;
  int strategy;
  HOST_WIDE_INT frame_off = 0;
  rtx sp_reg_rtx = gen_rtx_REG (Pmode, 1);
  rtx frame_reg_rtx = sp_reg_rtx;
  rtx cfa_restores = NULL_RTX;
  rtx insn;
  rtx cr_save_reg = NULL_RTX;
  enum machine_mode reg_mode = Pmode;
  int reg_size = TARGET_32BIT ? 4 : 8;
  int i;
  bool exit_func;
  unsigned ptr_regno;

  info = rs6000_stack_info ();

  if (TARGET_SPE_ABI && info->spe_64bit_regs_used != 0)
    {
      reg_mode = V2SImode;
      reg_size = 8;
    }

  strategy = info->savres_strategy;
  using_load_multiple = strategy & SAVRES_MULTIPLE;
  restoring_FPRs_inline = sibcall || (strategy & REST_INLINE_FPRS);
  restoring_GPRs_inline = sibcall || (strategy & REST_INLINE_GPRS);
  using_mtcr_multiple = (rs6000_cpu == PROCESSOR_PPC601
			 || rs6000_cpu == PROCESSOR_PPC603
			 || rs6000_cpu == PROCESSOR_PPC750
			 || optimize_size);
  /* Restore via the backchain when we have a large frame, since this
     is more efficient than an addis, addi pair.  The second condition
     here will not trigger at the moment;  We don't actually need a
     frame pointer for alloca, but the generic parts of the compiler
     give us one anyway.  */
  use_backchain_to_restore_sp = (info->total_size > 32767 - info->lr_save_offset
				 || (cfun->calls_alloca
				     && !frame_pointer_needed));
  restore_lr = (info->lr_save_p
		&& (restoring_FPRs_inline
		    || (strategy & REST_NOINLINE_FPRS_DOESNT_RESTORE_LR))
		&& (restoring_GPRs_inline
		    || info->first_fp_reg_save < 64));

  if (WORLD_SAVE_P (info))
    {
      int i, j;
      char rname[30];
      const char *alloc_rname;
      rtvec p;

      /* eh_rest_world_r10 will return to the location saved in the LR
	 stack slot (which is not likely to be our caller.)
	 Input: R10 -- stack adjustment.  Clobbers R0, R11, R12, R7, R8.
	 rest_world is similar, except any R10 parameter is ignored.
	 The exception-handling stuff that was here in 2.95 is no
	 longer necessary.  */

      p = rtvec_alloc (9
		       + 1
		       + 32 - info->first_gp_reg_save
		       + LAST_ALTIVEC_REGNO + 1 - info->first_altivec_reg_save
		       + 63 + 1 - info->first_fp_reg_save);

      strcpy (rname, ((crtl->calls_eh_return) ?
		      "*eh_rest_world_r10" : "*rest_world"));
      alloc_rname = ggc_strdup (rname);

      j = 0;
      RTVEC_ELT (p, j++) = ret_rtx;
      RTVEC_ELT (p, j++) = gen_rtx_USE (VOIDmode,
					gen_rtx_REG (Pmode,
						     LR_REGNO));
      RTVEC_ELT (p, j++)
	= gen_rtx_USE (VOIDmode, gen_rtx_SYMBOL_REF (Pmode, alloc_rname));
      /* The instruction pattern requires a clobber here;
	 it is shared with the restVEC helper. */
      RTVEC_ELT (p, j++)
	= gen_rtx_CLOBBER (VOIDmode, gen_rtx_REG (Pmode, 11));

      {
	/* CR register traditionally saved as CR2.  */
	rtx reg = gen_rtx_REG (SImode, CR2_REGNO);
	RTVEC_ELT (p, j++)
	  = gen_frame_load (reg, frame_reg_rtx, info->cr_save_offset);
	if (flag_shrink_wrap)
	  {
	    cfa_restores = alloc_reg_note (REG_CFA_RESTORE,
					   gen_rtx_REG (Pmode, LR_REGNO),
					   cfa_restores);
	    cfa_restores = alloc_reg_note (REG_CFA_RESTORE, reg, cfa_restores);
	  }
      }

      for (i = 0; i < 32 - info->first_gp_reg_save; i++)
	{
	  rtx reg = gen_rtx_REG (reg_mode, info->first_gp_reg_save + i);
	  RTVEC_ELT (p, j++)
	    = gen_frame_load (reg,
			      frame_reg_rtx, info->gp_save_offset + reg_size * i);
	  if (flag_shrink_wrap)
	    cfa_restores = alloc_reg_note (REG_CFA_RESTORE, reg, cfa_restores);
	}
      for (i = 0; info->first_altivec_reg_save + i <= LAST_ALTIVEC_REGNO; i++)
	{
	  rtx reg = gen_rtx_REG (V4SImode, info->first_altivec_reg_save + i);
	  RTVEC_ELT (p, j++)
	    = gen_frame_load (reg,
			      frame_reg_rtx, info->altivec_save_offset + 16 * i);
	  if (flag_shrink_wrap)
	    cfa_restores = alloc_reg_note (REG_CFA_RESTORE, reg, cfa_restores);
	}
      for (i = 0; info->first_fp_reg_save + i <= 63; i++)
	{
	  rtx reg = gen_rtx_REG ((TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT
				  ? DFmode : SFmode),
				 info->first_fp_reg_save + i);
	  RTVEC_ELT (p, j++)
	    = gen_frame_load (reg, frame_reg_rtx, info->fp_save_offset + 8 * i);
	  if (flag_shrink_wrap)
	    cfa_restores = alloc_reg_note (REG_CFA_RESTORE, reg, cfa_restores);
	}
      RTVEC_ELT (p, j++)
	= gen_rtx_CLOBBER (VOIDmode, gen_rtx_REG (Pmode, 0));
      RTVEC_ELT (p, j++)
	= gen_rtx_CLOBBER (VOIDmode, gen_rtx_REG (SImode, 12));
      RTVEC_ELT (p, j++)
	= gen_rtx_CLOBBER (VOIDmode, gen_rtx_REG (SImode, 7));
      RTVEC_ELT (p, j++)
	= gen_rtx_CLOBBER (VOIDmode, gen_rtx_REG (SImode, 8));
      RTVEC_ELT (p, j++)
	= gen_rtx_USE (VOIDmode, gen_rtx_REG (SImode, 10));
      insn = emit_jump_insn (gen_rtx_PARALLEL (VOIDmode, p));

      if (flag_shrink_wrap)
	{
	  REG_NOTES (insn) = cfa_restores;
	  add_reg_note (insn, REG_CFA_DEF_CFA, sp_reg_rtx);
	  RTX_FRAME_RELATED_P (insn) = 1;
	}
      return;
    }

  /* frame_reg_rtx + frame_off points to the top of this stack frame.  */
  if (info->push_p)
    frame_off = info->total_size;

  /* Restore AltiVec registers if we must do so before adjusting the
     stack.  */
  if (TARGET_ALTIVEC_ABI
      && info->altivec_size != 0
      && (ALWAYS_RESTORE_ALTIVEC_BEFORE_POP
	  || (DEFAULT_ABI != ABI_V4
	      && offset_below_red_zone_p (info->altivec_save_offset))))
    {
      int i;
      int scratch_regno = ptr_regno_for_savres (SAVRES_VR);

      gcc_checking_assert (scratch_regno == 11 || scratch_regno == 12);
      if (use_backchain_to_restore_sp)
	{
	  int frame_regno = 11;

	  if ((strategy & REST_INLINE_VRS) == 0)
	    {
	      /* Of r11 and r12, select the one not clobbered by an
		 out-of-line restore function for the frame register.  */
	      frame_regno = 11 + 12 - scratch_regno;
	    }
	  frame_reg_rtx = gen_rtx_REG (Pmode, frame_regno);
	  emit_move_insn (frame_reg_rtx,
			  gen_rtx_MEM (Pmode, sp_reg_rtx));
	  frame_off = 0;
	}
      else if (frame_pointer_needed)
	frame_reg_rtx = hard_frame_pointer_rtx;

      if ((strategy & REST_INLINE_VRS) == 0)
	{
	  int end_save = info->altivec_save_offset + info->altivec_size;
	  int ptr_off;
	  rtx ptr_reg = gen_rtx_REG (Pmode, 0);
	  rtx scratch_reg = gen_rtx_REG (Pmode, scratch_regno);

	  if (end_save + frame_off != 0)
	    {
	      rtx offset = GEN_INT (end_save + frame_off);

	      emit_insn (gen_add3_insn (ptr_reg, frame_reg_rtx, offset));
	    }
	  else
	    emit_move_insn (ptr_reg, frame_reg_rtx);

	  ptr_off = -end_save;
	  insn = rs6000_emit_savres_rtx (info, scratch_reg,
					 info->altivec_save_offset + ptr_off,
					 0, V4SImode, SAVRES_VR);
	}
      else
	{
	  for (i = info->first_altivec_reg_save; i <= LAST_ALTIVEC_REGNO; ++i)
	    if (info->vrsave_mask & ALTIVEC_REG_BIT (i))
	      {
		rtx addr, areg, mem, reg;

		areg = gen_rtx_REG (Pmode, 0);
		emit_move_insn
		  (areg, GEN_INT (info->altivec_save_offset
				  + frame_off
				  + 16 * (i - info->first_altivec_reg_save)));

		/* AltiVec addressing mode is [reg+reg].  */
		addr = gen_rtx_PLUS (Pmode, frame_reg_rtx, areg);
		mem = gen_frame_mem (V4SImode, addr);

		reg = gen_rtx_REG (V4SImode, i);
		emit_move_insn (reg, mem);
	      }
	}

      for (i = info->first_altivec_reg_save; i <= LAST_ALTIVEC_REGNO; ++i)
	if (((strategy & REST_INLINE_VRS) == 0
	     || (info->vrsave_mask & ALTIVEC_REG_BIT (i)) != 0)
	    && (flag_shrink_wrap
		|| (offset_below_red_zone_p
		    (info->altivec_save_offset
		     + 16 * (i - info->first_altivec_reg_save)))))
	  {
	    rtx reg = gen_rtx_REG (V4SImode, i);
	    cfa_restores = alloc_reg_note (REG_CFA_RESTORE, reg, cfa_restores);
	  }
    }

  /* Restore VRSAVE if we must do so before adjusting the stack.  */
  if (TARGET_ALTIVEC
      && TARGET_ALTIVEC_VRSAVE
      && info->vrsave_mask != 0
      && (ALWAYS_RESTORE_ALTIVEC_BEFORE_POP
	  || (DEFAULT_ABI != ABI_V4
	      && offset_below_red_zone_p (info->vrsave_save_offset))))
    {
      rtx reg;

      if (frame_reg_rtx == sp_reg_rtx)
	{
	  if (use_backchain_to_restore_sp)
	    {
	      frame_reg_rtx = gen_rtx_REG (Pmode, 11);
	      emit_move_insn (frame_reg_rtx,
			      gen_rtx_MEM (Pmode, sp_reg_rtx));
	      frame_off = 0;
	    }
	  else if (frame_pointer_needed)
	    frame_reg_rtx = hard_frame_pointer_rtx;
	}

      reg = gen_rtx_REG (SImode, 12);
      emit_insn (gen_frame_load (reg, frame_reg_rtx,
				 info->vrsave_save_offset + frame_off));

      emit_insn (generate_set_vrsave (reg, info, 1));
    }

  insn = NULL_RTX;
  /* If we have a large stack frame, restore the old stack pointer
     using the backchain.  */
  if (use_backchain_to_restore_sp)
    {
      if (frame_reg_rtx == sp_reg_rtx)
	{
	  /* Under V.4, don't reset the stack pointer until after we're done
	     loading the saved registers.  */
	  if (DEFAULT_ABI == ABI_V4)
	    frame_reg_rtx = gen_rtx_REG (Pmode, 11);

	  insn = emit_move_insn (frame_reg_rtx,
				 gen_rtx_MEM (Pmode, sp_reg_rtx));
	  frame_off = 0;
	}
      else if (ALWAYS_RESTORE_ALTIVEC_BEFORE_POP
	       && DEFAULT_ABI == ABI_V4)
	/* frame_reg_rtx has been set up by the altivec restore.  */
	;
      else
	{
	  insn = emit_move_insn (sp_reg_rtx, frame_reg_rtx);
	  frame_reg_rtx = sp_reg_rtx;
	}
    }
  /* If we have a frame pointer, we can restore the old stack pointer
     from it.  */
  else if (frame_pointer_needed)
    {
      frame_reg_rtx = sp_reg_rtx;
      if (DEFAULT_ABI == ABI_V4)
	frame_reg_rtx = gen_rtx_REG (Pmode, 11);
      /* Prevent reordering memory accesses against stack pointer restore.  */
      else if (cfun->calls_alloca
	       || offset_below_red_zone_p (-info->total_size))
	rs6000_emit_stack_tie (frame_reg_rtx, true);

      insn = emit_insn (gen_add3_insn (frame_reg_rtx, hard_frame_pointer_rtx,
				       GEN_INT (info->total_size)));
      frame_off = 0;
    }
  else if (info->push_p
	   && DEFAULT_ABI != ABI_V4
	   && !crtl->calls_eh_return)
    {
      /* Prevent reordering memory accesses against stack pointer restore.  */
      if (cfun->calls_alloca
	  || offset_below_red_zone_p (-info->total_size))
	rs6000_emit_stack_tie (frame_reg_rtx, false);
      insn = emit_insn (gen_add3_insn (sp_reg_rtx, sp_reg_rtx,
				       GEN_INT (info->total_size)));
      frame_off = 0;
    }
  if (insn && frame_reg_rtx == sp_reg_rtx)
    {
      if (cfa_restores)
	{
	  REG_NOTES (insn) = cfa_restores;
	  cfa_restores = NULL_RTX;
	}
      add_reg_note (insn, REG_CFA_DEF_CFA, sp_reg_rtx);
      RTX_FRAME_RELATED_P (insn) = 1;
    }

  /* Restore AltiVec registers if we have not done so already.  */
  if (!ALWAYS_RESTORE_ALTIVEC_BEFORE_POP
      && TARGET_ALTIVEC_ABI
      && info->altivec_size != 0
      && (DEFAULT_ABI == ABI_V4
	  || !offset_below_red_zone_p (info->altivec_save_offset)))
    {
      int i;

      if ((strategy & REST_INLINE_VRS) == 0)
	{
	  int end_save = info->altivec_save_offset + info->altivec_size;
	  int ptr_off;
	  rtx ptr_reg = gen_rtx_REG (Pmode, 0);
	  int scratch_regno = ptr_regno_for_savres (SAVRES_VR);
	  rtx scratch_reg = gen_rtx_REG (Pmode, scratch_regno);

	  if (end_save + frame_off != 0)
	    {
	      rtx offset = GEN_INT (end_save + frame_off);

	      emit_insn (gen_add3_insn (ptr_reg, frame_reg_rtx, offset));
	    }
	  else
	    emit_move_insn (ptr_reg, frame_reg_rtx);

	  ptr_off = -end_save;
	  insn = rs6000_emit_savres_rtx (info, scratch_reg,
					 info->altivec_save_offset + ptr_off,
					 0, V4SImode, SAVRES_VR);
	  if (REGNO (frame_reg_rtx) == REGNO (scratch_reg))
	    {
	      /* Frame reg was clobbered by out-of-line save.  Restore it
		 from ptr_reg, and if we are calling out-of-line gpr or
		 fpr restore set up the correct pointer and offset.  */
	      unsigned newptr_regno = 1;
	      if (!restoring_GPRs_inline)
		{
		  bool lr = info->gp_save_offset + info->gp_size == 0;
		  int sel = SAVRES_GPR | (lr ? SAVRES_LR : 0);
		  newptr_regno = ptr_regno_for_savres (sel);
		  end_save = info->gp_save_offset + info->gp_size;
		}
	      else if (!restoring_FPRs_inline)
		{
		  bool lr = !(strategy & REST_NOINLINE_FPRS_DOESNT_RESTORE_LR);
		  int sel = SAVRES_FPR | (lr ? SAVRES_LR : 0);
		  newptr_regno = ptr_regno_for_savres (sel);
		  end_save = info->gp_save_offset + info->gp_size;
		}

	      if (newptr_regno != 1 && REGNO (frame_reg_rtx) != newptr_regno)
		frame_reg_rtx = gen_rtx_REG (Pmode, newptr_regno);
		
	      if (end_save + ptr_off != 0)
		{
		  rtx offset = GEN_INT (end_save + ptr_off);

		  frame_off = -end_save;
		  emit_insn (gen_add3_insn (frame_reg_rtx, ptr_reg, offset));
		}
	      else
		{
		  frame_off = ptr_off;
		  emit_move_insn (frame_reg_rtx, ptr_reg);
		}
	    }
	}
      else
	{
	  for (i = info->first_altivec_reg_save; i <= LAST_ALTIVEC_REGNO; ++i)
	    if (info->vrsave_mask & ALTIVEC_REG_BIT (i))
	      {
		rtx addr, areg, mem, reg;

		areg = gen_rtx_REG (Pmode, 0);
		emit_move_insn
		  (areg, GEN_INT (info->altivec_save_offset
				  + frame_off
				  + 16 * (i - info->first_altivec_reg_save)));

		/* AltiVec addressing mode is [reg+reg].  */
		addr = gen_rtx_PLUS (Pmode, frame_reg_rtx, areg);
		mem = gen_frame_mem (V4SImode, addr);

		reg = gen_rtx_REG (V4SImode, i);
		emit_move_insn (reg, mem);
	      }
	}

      for (i = info->first_altivec_reg_save; i <= LAST_ALTIVEC_REGNO; ++i)
	if (((strategy & REST_INLINE_VRS) == 0
	     || (info->vrsave_mask & ALTIVEC_REG_BIT (i)) != 0)
	    && (DEFAULT_ABI == ABI_V4 || flag_shrink_wrap))
	  {
	    rtx reg = gen_rtx_REG (V4SImode, i);
	    cfa_restores = alloc_reg_note (REG_CFA_RESTORE, reg, cfa_restores);
	  }
    }

  /* Restore VRSAVE if we have not done so already.  */
  if (!ALWAYS_RESTORE_ALTIVEC_BEFORE_POP
      && TARGET_ALTIVEC
      && TARGET_ALTIVEC_VRSAVE
      && info->vrsave_mask != 0
      && (DEFAULT_ABI == ABI_V4
	  || !offset_below_red_zone_p (info->vrsave_save_offset)))
    {
      rtx reg;

      reg = gen_rtx_REG (SImode, 12);
      emit_insn (gen_frame_load (reg, frame_reg_rtx,
				 info->vrsave_save_offset + frame_off));

      emit_insn (generate_set_vrsave (reg, info, 1));
    }

  /* If we exit by an out-of-line restore function on ABI_V4 then that
     function will deallocate the stack, so we don't need to worry
     about the unwinder restoring cr from an invalid stack frame
     location.  */
  exit_func = (!restoring_FPRs_inline
	       || (!restoring_GPRs_inline
		   && info->first_fp_reg_save == 64));

  /* In the ELFv2 ABI we need to restore all call-saved CR fields from
     *separate* slots if the routine calls __builtin_eh_return, so
     that they can be independently restored by the unwinder.  */
  if (DEFAULT_ABI == ABI_ELFv2 && crtl->calls_eh_return)
    {
      int i, cr_off = info->ehcr_offset;

      for (i = 0; i < 8; i++)
	if (!call_used_regs[CR0_REGNO + i])
	  {
	    rtx reg = gen_rtx_REG (SImode, 0);
	    emit_insn (gen_frame_load (reg, frame_reg_rtx,
				       cr_off + frame_off));

	    insn = emit_insn (gen_movsi_to_cr_one
				(gen_rtx_REG (CCmode, CR0_REGNO + i), reg));

	    if (!exit_func && flag_shrink_wrap)
	      {
		add_reg_note (insn, REG_CFA_RESTORE,
			      gen_rtx_REG (SImode, CR0_REGNO + i));

		RTX_FRAME_RELATED_P (insn) = 1;
	      }

	    cr_off += reg_size;
	  }
    }

  /* Get the old lr if we saved it.  If we are restoring registers
     out-of-line, then the out-of-line routines can do this for us.  */
  if (restore_lr && restoring_GPRs_inline)
    load_lr_save (0, frame_reg_rtx, info->lr_save_offset + frame_off);

  /* Get the old cr if we saved it.  */
  if (info->cr_save_p)
    {
      unsigned cr_save_regno = 12;

      if (!restoring_GPRs_inline)
	{
	  /* Ensure we don't use the register used by the out-of-line
	     gpr register restore below.  */
	  bool lr = info->gp_save_offset + info->gp_size == 0;
	  int sel = SAVRES_GPR | (lr ? SAVRES_LR : 0);
	  int gpr_ptr_regno = ptr_regno_for_savres (sel);

	  if (gpr_ptr_regno == 12)
	    cr_save_regno = 11;
	  gcc_checking_assert (REGNO (frame_reg_rtx) != cr_save_regno);
	}
      else if (REGNO (frame_reg_rtx) == 12)
	cr_save_regno = 11;

      cr_save_reg = load_cr_save (cr_save_regno, frame_reg_rtx,
				  info->cr_save_offset + frame_off,
				  exit_func);
    }

  /* Set LR here to try to overlap restores below.  */
  if (restore_lr && restoring_GPRs_inline)
    restore_saved_lr (0, exit_func);

  /* Load exception handler data registers, if needed.  */
  if (crtl->calls_eh_return)
    {
      unsigned int i, regno;

      if (TARGET_AIX)
	{
	  rtx reg = gen_rtx_REG (reg_mode, 2);
	  emit_insn (gen_frame_load (reg, frame_reg_rtx,
				     frame_off + RS6000_TOC_SAVE_SLOT));
	}

      for (i = 0; ; ++i)
	{
	  rtx mem;

	  regno = EH_RETURN_DATA_REGNO (i);
	  if (regno == INVALID_REGNUM)
	    break;

	  /* Note: possible use of r0 here to address SPE regs.  */
	  mem = gen_frame_mem_offset (reg_mode, frame_reg_rtx,
				      info->ehrd_offset + frame_off
				      + reg_size * (int) i);

	  emit_move_insn (gen_rtx_REG (reg_mode, regno), mem);
	}
    }

  /* Restore GPRs.  This is done as a PARALLEL if we are using
     the load-multiple instructions.  */
  if (TARGET_SPE_ABI
      && info->spe_64bit_regs_used
      && info->first_gp_reg_save != 32)
    {
      /* Determine whether we can address all of the registers that need
	 to be saved with an offset from frame_reg_rtx that fits in
	 the small const field for SPE memory instructions.  */
      int spe_regs_addressable
	= (SPE_CONST_OFFSET_OK (info->spe_gp_save_offset + frame_off
				+ reg_size * (32 - info->first_gp_reg_save - 1))
	   && restoring_GPRs_inline);

      if (!spe_regs_addressable)
	{
	  int ool_adjust = 0;
	  rtx old_frame_reg_rtx = frame_reg_rtx;
	  /* Make r11 point to the start of the SPE save area.  We worried about
	     not clobbering it when we were saving registers in the prologue.
	     There's no need to worry here because the static chain is passed
	     anew to every function.  */

	  if (!restoring_GPRs_inline)
	    ool_adjust = 8 * (info->first_gp_reg_save - FIRST_SAVED_GP_REGNO);
	  frame_reg_rtx = gen_rtx_REG (Pmode, 11);
	  emit_insn (gen_addsi3 (frame_reg_rtx, old_frame_reg_rtx,
				 GEN_INT (info->spe_gp_save_offset
					  + frame_off
					  - ool_adjust)));
	  /* Keep the invariant that frame_reg_rtx + frame_off points
	     at the top of the stack frame.  */
	  frame_off = -info->spe_gp_save_offset + ool_adjust;
	}

      if (restoring_GPRs_inline)
	{
	  HOST_WIDE_INT spe_offset = info->spe_gp_save_offset + frame_off;

	  for (i = 0; i < 32 - info->first_gp_reg_save; i++)
	    if (rs6000_reg_live_or_pic_offset_p (info->first_gp_reg_save + i))
	      {
		rtx offset, addr, mem, reg;

		/* We're doing all this to ensure that the immediate offset
		   fits into the immediate field of 'evldd'.  */
		gcc_assert (SPE_CONST_OFFSET_OK (spe_offset + reg_size * i));

		offset = GEN_INT (spe_offset + reg_size * i);
		addr = gen_rtx_PLUS (Pmode, frame_reg_rtx, offset);
		mem = gen_rtx_MEM (V2SImode, addr);
		reg = gen_rtx_REG (reg_mode, info->first_gp_reg_save + i);

		emit_move_insn (reg, mem);
	      }
	}
      else
	rs6000_emit_savres_rtx (info, frame_reg_rtx,
				info->spe_gp_save_offset + frame_off,
				info->lr_save_offset + frame_off,
				reg_mode,
				SAVRES_GPR | SAVRES_LR);
    }
  else if (!restoring_GPRs_inline)
    {
      /* We are jumping to an out-of-line function.  */
      rtx ptr_reg;
      int end_save = info->gp_save_offset + info->gp_size;
      bool can_use_exit = end_save == 0;
      int sel = SAVRES_GPR | (can_use_exit ? SAVRES_LR : 0);
      int ptr_off;

      /* Emit stack reset code if we need it.  */
      ptr_regno = ptr_regno_for_savres (sel);
      ptr_reg = gen_rtx_REG (Pmode, ptr_regno);
      if (can_use_exit)
	rs6000_emit_stack_reset (info, frame_reg_rtx, frame_off, ptr_regno);
      else if (end_save + frame_off != 0)
	emit_insn (gen_add3_insn (ptr_reg, frame_reg_rtx,
				  GEN_INT (end_save + frame_off)));
      else if (REGNO (frame_reg_rtx) != ptr_regno)
	emit_move_insn (ptr_reg, frame_reg_rtx);
      if (REGNO (frame_reg_rtx) == ptr_regno)
	frame_off = -end_save;

      if (can_use_exit && info->cr_save_p)
	restore_saved_cr (cr_save_reg, using_mtcr_multiple, true);

      ptr_off = -end_save;
      rs6000_emit_savres_rtx (info, ptr_reg,
			      info->gp_save_offset + ptr_off,
			      info->lr_save_offset + ptr_off,
			      reg_mode, sel);
    }
  else if (using_load_multiple)
    {
      rtvec p;
      p = rtvec_alloc (32 - info->first_gp_reg_save);
      for (i = 0; i < 32 - info->first_gp_reg_save; i++)
	RTVEC_ELT (p, i)
	  = gen_frame_load (gen_rtx_REG (reg_mode, info->first_gp_reg_save + i),
			    frame_reg_rtx,
			    info->gp_save_offset + frame_off + reg_size * i);
      emit_insn (gen_rtx_PARALLEL (VOIDmode, p));
    }
  else
    {
      for (i = 0; i < 32 - info->first_gp_reg_save; i++)
	if (rs6000_reg_live_or_pic_offset_p (info->first_gp_reg_save + i))
	  emit_insn (gen_frame_load
		     (gen_rtx_REG (reg_mode, info->first_gp_reg_save + i),
		      frame_reg_rtx,
		      info->gp_save_offset + frame_off + reg_size * i));
    }

  if (DEFAULT_ABI == ABI_V4 || flag_shrink_wrap)
    {
      /* If the frame pointer was used then we can't delay emitting
	 a REG_CFA_DEF_CFA note.  This must happen on the insn that
	 restores the frame pointer, r31.  We may have already emitted
	 a REG_CFA_DEF_CFA note, but that's OK;  A duplicate is
	 discarded by dwarf2cfi.c/dwarf2out.c, and in any case would
	 be harmless if emitted.  */
      if (frame_pointer_needed)
	{
	  insn = get_last_insn ();
	  add_reg_note (insn, REG_CFA_DEF_CFA,
			plus_constant (Pmode, frame_reg_rtx, frame_off));
	  RTX_FRAME_RELATED_P (insn) = 1;
	}

      /* Set up cfa_restores.  We always need these when
	 shrink-wrapping.  If not shrink-wrapping then we only need
	 the cfa_restore when the stack location is no longer valid.
	 The cfa_restores must be emitted on or before the insn that
	 invalidates the stack, and of course must not be emitted
	 before the insn that actually does the restore.  The latter
	 is why it is a bad idea to emit the cfa_restores as a group
	 on the last instruction here that actually does a restore:
	 That insn may be reordered with respect to others doing
	 restores.  */
      if (flag_shrink_wrap
	  && !restoring_GPRs_inline
	  && info->first_fp_reg_save == 64)
	cfa_restores = add_crlr_cfa_restore (info, cfa_restores);

      for (i = info->first_gp_reg_save; i < 32; i++)
	if (!restoring_GPRs_inline
	    || using_load_multiple
	    || rs6000_reg_live_or_pic_offset_p (i))
	  {
	    rtx reg = gen_rtx_REG (reg_mode, i);

	    cfa_restores = alloc_reg_note (REG_CFA_RESTORE, reg, cfa_restores);
	  }
    }

  if (!restoring_GPRs_inline
      && info->first_fp_reg_save == 64)
    {
      /* We are jumping to an out-of-line function.  */
      if (cfa_restores)
	emit_cfa_restores (cfa_restores);
      return;
    }

  if (restore_lr && !restoring_GPRs_inline)
    {
      load_lr_save (0, frame_reg_rtx, info->lr_save_offset + frame_off);
      restore_saved_lr (0, exit_func);
    }

  /* Restore fpr's if we need to do it without calling a function.  */
  if (restoring_FPRs_inline)
    for (i = 0; i < 64 - info->first_fp_reg_save; i++)
      if (save_reg_p (info->first_fp_reg_save + i))
	{
	  rtx reg = gen_rtx_REG ((TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT
				  ? DFmode : SFmode),
				 info->first_fp_reg_save + i);
	  emit_insn (gen_frame_load (reg, frame_reg_rtx,
				     info->fp_save_offset + frame_off + 8 * i));
	  if (DEFAULT_ABI == ABI_V4 || flag_shrink_wrap)
	    cfa_restores = alloc_reg_note (REG_CFA_RESTORE, reg, cfa_restores);
	}

  /* If we saved cr, restore it here.  Just those that were used.  */
  if (info->cr_save_p)
    restore_saved_cr (cr_save_reg, using_mtcr_multiple, exit_func);

  /* If this is V.4, unwind the stack pointer after all of the loads
     have been done, or set up r11 if we are restoring fp out of line.  */
  ptr_regno = 1;
  if (!restoring_FPRs_inline)
    {
      bool lr = (strategy & REST_NOINLINE_FPRS_DOESNT_RESTORE_LR) == 0;
      int sel = SAVRES_FPR | (lr ? SAVRES_LR : 0);
      ptr_regno = ptr_regno_for_savres (sel);
    }

  insn = rs6000_emit_stack_reset (info, frame_reg_rtx, frame_off, ptr_regno);
  if (REGNO (frame_reg_rtx) == ptr_regno)
    frame_off = 0;

  if (insn && restoring_FPRs_inline)
    {
      if (cfa_restores)
	{
	  REG_NOTES (insn) = cfa_restores;
	  cfa_restores = NULL_RTX;
	}
      add_reg_note (insn, REG_CFA_DEF_CFA, sp_reg_rtx);
      RTX_FRAME_RELATED_P (insn) = 1;
    }

  if (crtl->calls_eh_return)
    {
      rtx sa = EH_RETURN_STACKADJ_RTX;
      emit_insn (gen_add3_insn (sp_reg_rtx, sp_reg_rtx, sa));
    }

  if (!sibcall)
    {
      rtvec p;
      bool lr = (strategy & REST_NOINLINE_FPRS_DOESNT_RESTORE_LR) == 0;
      if (! restoring_FPRs_inline)
	{
	  p = rtvec_alloc (4 + 64 - info->first_fp_reg_save);
	  RTVEC_ELT (p, 0) = ret_rtx;
	}
      else
	{
	  if (cfa_restores)
	    {
	      /* We can't hang the cfa_restores off a simple return,
		 since the shrink-wrap code sometimes uses an existing
		 return.  This means there might be a path from
		 pre-prologue code to this return, and dwarf2cfi code
		 wants the eh_frame unwinder state to be the same on
		 all paths to any point.  So we need to emit the
		 cfa_restores before the return.  For -m64 we really
		 don't need epilogue cfa_restores at all, except for
		 this irritating dwarf2cfi with shrink-wrap
		 requirement;  The stack red-zone means eh_frame info
		 from the prologue telling the unwinder to restore
		 from the stack is perfectly good right to the end of
		 the function.  */
	      emit_insn (gen_blockage ());
	      emit_cfa_restores (cfa_restores);
	      cfa_restores = NULL_RTX;
	    }
	  p = rtvec_alloc (2);
	  RTVEC_ELT (p, 0) = simple_return_rtx;
	}

      RTVEC_ELT (p, 1) = ((restoring_FPRs_inline || !lr)
			  ? gen_rtx_USE (VOIDmode,
					 gen_rtx_REG (Pmode, LR_REGNO))
			  : gen_rtx_CLOBBER (VOIDmode,
					     gen_rtx_REG (Pmode, LR_REGNO)));

      /* If we have to restore more than two FP registers, branch to the
	 restore function.  It will return to our caller.  */
      if (! restoring_FPRs_inline)
	{
	  int i;
	  int reg;
	  rtx sym;

	  if (flag_shrink_wrap)
	    cfa_restores = add_crlr_cfa_restore (info, cfa_restores);

	  sym = rs6000_savres_routine_sym (info,
					   SAVRES_FPR | (lr ? SAVRES_LR : 0));
	  RTVEC_ELT (p, 2) = gen_rtx_USE (VOIDmode, sym);
	  reg = (DEFAULT_ABI == ABI_AIX || DEFAULT_ABI == ABI_ELFv2)? 1 : 11;
	  RTVEC_ELT (p, 3) = gen_rtx_USE (VOIDmode, gen_rtx_REG (Pmode, reg));

	  for (i = 0; i < 64 - info->first_fp_reg_save; i++)
	    {
	      rtx reg = gen_rtx_REG (DFmode, info->first_fp_reg_save + i);

	      RTVEC_ELT (p, i + 4)
		= gen_frame_load (reg, sp_reg_rtx, info->fp_save_offset + 8 * i);
	      if (flag_shrink_wrap)
		cfa_restores = alloc_reg_note (REG_CFA_RESTORE, reg,
					       cfa_restores);
	    }
	}

      emit_jump_insn (gen_rtx_PARALLEL (VOIDmode, p));
    }

  if (cfa_restores)
    {
      if (sibcall)
	/* Ensure the cfa_restores are hung off an insn that won't
	   be reordered above other restores.  */
	emit_insn (gen_blockage ());

      emit_cfa_restores (cfa_restores);
    }
}

/* Write function epilogue.  */

static void
rs6000_output_function_epilogue (FILE *file,
				 HOST_WIDE_INT size ATTRIBUTE_UNUSED)
{
#if TARGET_MACHO
  macho_branch_islands ();
  /* Mach-O doesn't support labels at the end of objects, so if
     it looks like we might want one, insert a NOP.  */
  {
    rtx insn = get_last_insn ();
    rtx deleted_debug_label = NULL_RTX;
    while (insn
	   && NOTE_P (insn)
	   && NOTE_KIND (insn) != NOTE_INSN_DELETED_LABEL)
      {
	/* Don't insert a nop for NOTE_INSN_DELETED_DEBUG_LABEL
	   notes only, instead set their CODE_LABEL_NUMBER to -1,
	   otherwise there would be code generation differences
	   in between -g and -g0.  */
	if (NOTE_P (insn) && NOTE_KIND (insn) == NOTE_INSN_DELETED_DEBUG_LABEL)
	  deleted_debug_label = insn;
	insn = PREV_INSN (insn);
      }
    if (insn
	&& (LABEL_P (insn)
	    || (NOTE_P (insn)
		&& NOTE_KIND (insn) == NOTE_INSN_DELETED_LABEL)))
      fputs ("\tnop\n", file);
    else if (deleted_debug_label)
      for (insn = deleted_debug_label; insn; insn = NEXT_INSN (insn))
	if (NOTE_KIND (insn) == NOTE_INSN_DELETED_DEBUG_LABEL)
	  CODE_LABEL_NUMBER (insn) = -1;
  }
#endif

  /* Output a traceback table here.  See /usr/include/sys/debug.h for info
     on its format.

     We don't output a traceback table if -finhibit-size-directive was
     used.  The documentation for -finhibit-size-directive reads
     ``don't output a @code{.size} assembler directive, or anything
     else that would cause trouble if the function is split in the
     middle, and the two halves are placed at locations far apart in
     memory.''  The traceback table has this property, since it
     includes the offset from the start of the function to the
     traceback table itself.

     System V.4 Powerpc's (and the embedded ABI derived from it) use a
     different traceback table.  */
  if ((DEFAULT_ABI == ABI_AIX || DEFAULT_ABI == ABI_ELFv2)
      && ! flag_inhibit_size_directive
      && rs6000_traceback != traceback_none && !cfun->is_thunk)
    {
      const char *fname = NULL;
      const char *language_string = lang_hooks.name;
      int fixed_parms = 0, float_parms = 0, parm_info = 0;
      int i;
      int optional_tbtab;
      rs6000_stack_t *info = rs6000_stack_info ();

      if (rs6000_traceback == traceback_full)
	optional_tbtab = 1;
      else if (rs6000_traceback == traceback_part)
	optional_tbtab = 0;
      else
	optional_tbtab = !optimize_size && !TARGET_ELF;

      if (optional_tbtab)
	{
	  fname = XSTR (XEXP (DECL_RTL (current_function_decl), 0), 0);
	  while (*fname == '.')	/* V.4 encodes . in the name */
	    fname++;

	  /* Need label immediately before tbtab, so we can compute
	     its offset from the function start.  */
	  ASM_OUTPUT_INTERNAL_LABEL_PREFIX (file, "LT");
	  ASM_OUTPUT_LABEL (file, fname);
	}

      /* The .tbtab pseudo-op can only be used for the first eight
	 expressions, since it can't handle the possibly variable
	 length fields that follow.  However, if you omit the optional
	 fields, the assembler outputs zeros for all optional fields
	 anyways, giving each variable length field is minimum length
	 (as defined in sys/debug.h).  Thus we can not use the .tbtab
	 pseudo-op at all.  */

      /* An all-zero word flags the start of the tbtab, for debuggers
	 that have to find it by searching forward from the entry
	 point or from the current pc.  */
      fputs ("\t.long 0\n", file);

      /* Tbtab format type.  Use format type 0.  */
      fputs ("\t.byte 0,", file);

      /* Language type.  Unfortunately, there does not seem to be any
	 official way to discover the language being compiled, so we
	 use language_string.
	 C is 0.  Fortran is 1.  Pascal is 2.  Ada is 3.  C++ is 9.
	 Java is 13.  Objective-C is 14.  Objective-C++ isn't assigned
	 a number, so for now use 9.  LTO and Go aren't assigned numbers
	 either, so for now use 0.  */
      if (! strcmp (language_string, "GNU C")
	  || ! strcmp (language_string, "GNU GIMPLE")
	  || ! strcmp (language_string, "GNU Go"))
	i = 0;
      else if (! strcmp (language_string, "GNU F77")
	       || ! strcmp (language_string, "GNU Fortran"))
	i = 1;
      else if (! strcmp (language_string, "GNU Pascal"))
	i = 2;
      else if (! strcmp (language_string, "GNU Ada"))
	i = 3;
      else if (! strcmp (language_string, "GNU C++")
	       || ! strcmp (language_string, "GNU Objective-C++"))
	i = 9;
      else if (! strcmp (language_string, "GNU Java"))
	i = 13;
      else if (! strcmp (language_string, "GNU Objective-C"))
	i = 14;
      else
	gcc_unreachable ();
      fprintf (file, "%d,", i);

      /* 8 single bit fields: global linkage (not set for C extern linkage,
	 apparently a PL/I convention?), out-of-line epilogue/prologue, offset
	 from start of procedure stored in tbtab, internal function, function
	 has controlled storage, function has no toc, function uses fp,
	 function logs/aborts fp operations.  */
      /* Assume that fp operations are used if any fp reg must be saved.  */
      fprintf (file, "%d,",
	       (optional_tbtab << 5) | ((info->first_fp_reg_save != 64) << 1));

      /* 6 bitfields: function is interrupt handler, name present in
	 proc table, function calls alloca, on condition directives
	 (controls stack walks, 3 bits), saves condition reg, saves
	 link reg.  */
      /* The `function calls alloca' bit seems to be set whenever reg 31 is
	 set up as a frame pointer, even when there is no alloca call.  */
      fprintf (file, "%d,",
	       ((optional_tbtab << 6)
		| ((optional_tbtab & frame_pointer_needed) << 5)
		| (info->cr_save_p << 1)
		| (info->lr_save_p)));

      /* 3 bitfields: saves backchain, fixup code, number of fpr saved
	 (6 bits).  */
      fprintf (file, "%d,",
	       (info->push_p << 7) | (64 - info->first_fp_reg_save));

      /* 2 bitfields: spare bits (2 bits), number of gpr saved (6 bits).  */
      fprintf (file, "%d,", (32 - first_reg_to_save ()));

      if (optional_tbtab)
	{
	  /* Compute the parameter info from the function decl argument
	     list.  */
	  tree decl;
	  int next_parm_info_bit = 31;

	  for (decl = DECL_ARGUMENTS (current_function_decl);
	       decl; decl = DECL_CHAIN (decl))
	    {
	      rtx parameter = DECL_INCOMING_RTL (decl);
	      enum machine_mode mode = GET_MODE (parameter);

	      if (GET_CODE (parameter) == REG)
		{
		  if (SCALAR_FLOAT_MODE_P (mode))
		    {
		      int bits;

		      float_parms++;

		      switch (mode)
			{
			case SFmode:
			case SDmode:
			  bits = 0x2;
			  break;

			case DFmode:
			case DDmode:
			case TFmode:
			case TDmode:
			  bits = 0x3;
			  break;

			default:
			  gcc_unreachable ();
			}

		      /* If only one bit will fit, don't or in this entry.  */
		      if (next_parm_info_bit > 0)
			parm_info |= (bits << (next_parm_info_bit - 1));
		      next_parm_info_bit -= 2;
		    }
		  else
		    {
		      fixed_parms += ((GET_MODE_SIZE (mode)
				       + (UNITS_PER_WORD - 1))
				      / UNITS_PER_WORD);
		      next_parm_info_bit -= 1;
		    }
		}
	    }
	}

      /* Number of fixed point parameters.  */
      /* This is actually the number of words of fixed point parameters; thus
	 an 8 byte struct counts as 2; and thus the maximum value is 8.  */
      fprintf (file, "%d,", fixed_parms);

      /* 2 bitfields: number of floating point parameters (7 bits), parameters
	 all on stack.  */
      /* This is actually the number of fp registers that hold parameters;
	 and thus the maximum value is 13.  */
      /* Set parameters on stack bit if parameters are not in their original
	 registers, regardless of whether they are on the stack?  Xlc
	 seems to set the bit when not optimizing.  */
      fprintf (file, "%d\n", ((float_parms << 1) | (! optimize)));

      if (! optional_tbtab)
	return;

      /* Optional fields follow.  Some are variable length.  */

      /* Parameter types, left adjusted bit fields: 0 fixed, 10 single float,
	 11 double float.  */
      /* There is an entry for each parameter in a register, in the order that
	 they occur in the parameter list.  Any intervening arguments on the
	 stack are ignored.  If the list overflows a long (max possible length
	 34 bits) then completely leave off all elements that don't fit.  */
      /* Only emit this long if there was at least one parameter.  */
      if (fixed_parms || float_parms)
	fprintf (file, "\t.long %d\n", parm_info);

      /* Offset from start of code to tb table.  */
      fputs ("\t.long ", file);
      ASM_OUTPUT_INTERNAL_LABEL_PREFIX (file, "LT");
      RS6000_OUTPUT_BASENAME (file, fname);
      putc ('-', file);
      rs6000_output_function_entry (file, fname);
      putc ('\n', file);

      /* Interrupt handler mask.  */
      /* Omit this long, since we never set the interrupt handler bit
	 above.  */

      /* Number of CTL (controlled storage) anchors.  */
      /* Omit this long, since the has_ctl bit is never set above.  */

      /* Displacement into stack of each CTL anchor.  */
      /* Omit this list of longs, because there are no CTL anchors.  */

      /* Length of function name.  */
      if (*fname == '*')
	++fname;
      fprintf (file, "\t.short %d\n", (int) strlen (fname));

      /* Function name.  */
      assemble_string (fname, strlen (fname));

      /* Register for alloca automatic storage; this is always reg 31.
	 Only emit this if the alloca bit was set above.  */
      if (frame_pointer_needed)
	fputs ("\t.byte 31\n", file);

      fputs ("\t.align 2\n", file);
    }
}

/* A C compound statement that outputs the assembler code for a thunk
   function, used to implement C++ virtual function calls with
   multiple inheritance.  The thunk acts as a wrapper around a virtual
   function, adjusting the implicit object parameter before handing
   control off to the real function.

   First, emit code to add the integer DELTA to the location that
   contains the incoming first argument.  Assume that this argument
   contains a pointer, and is the one used to pass the `this' pointer
   in C++.  This is the incoming argument *before* the function
   prologue, e.g. `%o0' on a sparc.  The addition must preserve the
   values of all other incoming arguments.

   After the addition, emit code to jump to FUNCTION, which is a
   `FUNCTION_DECL'.  This is a direct pure jump, not a call, and does
   not touch the return address.  Hence returning from FUNCTION will
   return to whoever called the current `thunk'.

   The effect must be as if FUNCTION had been called directly with the
   adjusted first argument.  This macro is responsible for emitting
   all of the code for a thunk function; output_function_prologue()
   and output_function_epilogue() are not invoked.

   The THUNK_FNDECL is redundant.  (DELTA and FUNCTION have already
   been extracted from it.)  It might possibly be useful on some
   targets, but probably not.

   If you do not define this macro, the target-independent code in the
   C++ frontend will generate a less efficient heavyweight thunk that
   calls FUNCTION instead of jumping to it.  The generic approach does
   not support varargs.  */

static void
rs6000_output_mi_thunk (FILE *file, tree thunk_fndecl ATTRIBUTE_UNUSED,
			HOST_WIDE_INT delta, HOST_WIDE_INT vcall_offset,
			tree function)
{
  rtx this_rtx, insn, funexp;

  reload_completed = 1;
  epilogue_completed = 1;

  /* Mark the end of the (empty) prologue.  */
  emit_note (NOTE_INSN_PROLOGUE_END);

  /* Find the "this" pointer.  If the function returns a structure,
     the structure return pointer is in r3.  */
  if (aggregate_value_p (TREE_TYPE (TREE_TYPE (function)), function))
    this_rtx = gen_rtx_REG (Pmode, 4);
  else
    this_rtx = gen_rtx_REG (Pmode, 3);

  /* Apply the constant offset, if required.  */
  if (delta)
    emit_insn (gen_add3_insn (this_rtx, this_rtx, GEN_INT (delta)));

  /* Apply the offset from the vtable, if required.  */
  if (vcall_offset)
    {
      rtx vcall_offset_rtx = GEN_INT (vcall_offset);
      rtx tmp = gen_rtx_REG (Pmode, 12);

      emit_move_insn (tmp, gen_rtx_MEM (Pmode, this_rtx));
      if (((unsigned HOST_WIDE_INT) vcall_offset) + 0x8000 >= 0x10000)
	{
	  emit_insn (gen_add3_insn (tmp, tmp, vcall_offset_rtx));
	  emit_move_insn (tmp, gen_rtx_MEM (Pmode, tmp));
	}
      else
	{
	  rtx loc = gen_rtx_PLUS (Pmode, tmp, vcall_offset_rtx);

	  emit_move_insn (tmp, gen_rtx_MEM (Pmode, loc));
	}
      emit_insn (gen_add3_insn (this_rtx, this_rtx, tmp));
    }

  /* Generate a tail call to the target function.  */
  if (!TREE_USED (function))
    {
      assemble_external (function);
      TREE_USED (function) = 1;
    }
  funexp = XEXP (DECL_RTL (function), 0);
  funexp = gen_rtx_MEM (FUNCTION_MODE, funexp);

#if TARGET_MACHO
  if (MACHOPIC_INDIRECT)
    funexp = machopic_indirect_call_target (funexp);
#endif

  /* gen_sibcall expects reload to convert scratch pseudo to LR so we must
     generate sibcall RTL explicitly.  */
  insn = emit_call_insn (
	   gen_rtx_PARALLEL (VOIDmode,
	     gen_rtvec (4,
			gen_rtx_CALL (VOIDmode,
				      funexp, const0_rtx),
			gen_rtx_USE (VOIDmode, const0_rtx),
			gen_rtx_USE (VOIDmode,
				     gen_rtx_REG (SImode,
						  LR_REGNO)),
			simple_return_rtx)));
  SIBLING_CALL_P (insn) = 1;
  emit_barrier ();

  /* Ensure we have a global entry point for the thunk.   ??? We could
     avoid that if the target routine doesn't need a global entry point,
     but we do not know whether this is the case at this point.  */
  if (DEFAULT_ABI == ABI_ELFv2)
    cfun->machine->r2_setup_needed = true;

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
  enum machine_mode mode = GET_MODE (k);
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

    case CONST_DOUBLE:
      if (mode != VOIDmode)
	return real_hash (CONST_DOUBLE_REAL_VALUE (k)) * result;
      flen = 2;
      break;

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

static unsigned
toc_hash_function (const void *hash_entry)
{
  const struct toc_hash_struct *thc =
    (const struct toc_hash_struct *) hash_entry;
  return rs6000_hash_constant (thc->key) ^ thc->key_mode;
}

/* Compare H1 and H2 for equivalence.  */

static int
toc_hash_eq (const void *h1, const void *h2)
{
  rtx r1 = ((const struct toc_hash_struct *) h1)->key;
  rtx r2 = ((const struct toc_hash_struct *) h2)->key;

  if (((const struct toc_hash_struct *) h1)->key_mode
      != ((const struct toc_hash_struct *) h2)->key_mode)
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
  /* Currently C++ toc references to vtables can be emitted before it
     is decided whether the vtable is public or private.  If this is
     the case, then the linker will eventually complain that there is
     a reference to an unknown section.  Thus, for vtables only,
     we emit the TOC reference to reference the symbol and not the
     section.  */
  const char *name = XSTR (x, 0);

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
output_toc (FILE *file, rtx x, int labelno, enum machine_mode mode)
{
  char buf[256];
  const char *name = buf;
  rtx base = x;
  HOST_WIDE_INT offset = 0;

  gcc_assert (!TARGET_NO_TOC);

  /* When the linker won't eliminate them, don't output duplicate
     TOC entries (this happens on AIX if there is any kind of TOC,
     and on SVR4 under -fPIC or -mrelocatable).  Don't do this for
     CODE_LABELs.  */
  if (TARGET_TOC && GET_CODE (x) != LABEL_REF)
    {
      struct toc_hash_struct *h;
      void * * found;

      /* Create toc_hash_table.  This can't be done at TARGET_OPTION_OVERRIDE
	 time because GGC is not initialized at that point.  */
      if (toc_hash_table == NULL)
	toc_hash_table = htab_create_ggc (1021, toc_hash_function,
					  toc_hash_eq, NULL);

      h = ggc_alloc_toc_hash_struct ();
      h->key = x;
      h->key_mode = mode;
      h->labelno = labelno;

      found = htab_find_slot (toc_hash_table, h, INSERT);
      if (*found == NULL)
	*found = h;
      else  /* This is indeed a duplicate.
	       Set this label equal to that label.  */
	{
	  fputs ("\t.set ", file);
	  ASM_OUTPUT_INTERNAL_LABEL_PREFIX (file, "LC");
	  fprintf (file, "%d,", labelno);
	  ASM_OUTPUT_INTERNAL_LABEL_PREFIX (file, "LC");
	  fprintf (file, "%d\n", ((*(const struct toc_hash_struct **)
					      found)->labelno));

#ifdef HAVE_AS_TLS
	  if (TARGET_XCOFF && GET_CODE (x) == SYMBOL_REF
	      && (SYMBOL_REF_TLS_MODEL (x) == TLS_MODEL_GLOBAL_DYNAMIC
		  || SYMBOL_REF_TLS_MODEL (x) == TLS_MODEL_LOCAL_DYNAMIC))
	    {
	      fputs ("\t.set ", file);
	      ASM_OUTPUT_INTERNAL_LABEL_PREFIX (file, "LCM");
	      fprintf (file, "%d,", labelno);
	      ASM_OUTPUT_INTERNAL_LABEL_PREFIX (file, "LCM");
	      fprintf (file, "%d\n", ((*(const struct toc_hash_struct **)
			       			      found)->labelno));
	    }
#endif
	  return;
	}
    }

  /* If we're going to put a double constant in the TOC, make sure it's
     aligned properly when strict alignment is on.  */
  if (GET_CODE (x) == CONST_DOUBLE
      && STRICT_ALIGNMENT
      && GET_MODE_BITSIZE (mode) >= 64
      && ! (TARGET_NO_FP_IN_TOC && ! TARGET_MINIMAL_TOC)) {
    ASM_OUTPUT_ALIGN (file, 3);
  }

  (*targetm.asm_out.internal_label) (file, "LC", labelno);

  /* Handle FP constants specially.  Note that if we have a minimal
     TOC, things we put here aren't actually in the TOC, so we can allow
     FP constants.  */
  if (GET_CODE (x) == CONST_DOUBLE &&
      (GET_MODE (x) == TFmode || GET_MODE (x) == TDmode))
    {
      REAL_VALUE_TYPE rv;
      long k[4];

      REAL_VALUE_FROM_CONST_DOUBLE (rv, x);
      if (DECIMAL_FLOAT_MODE_P (GET_MODE (x)))
	REAL_VALUE_TO_TARGET_DECIMAL128 (rv, k);
      else
	REAL_VALUE_TO_TARGET_LONG_DOUBLE (rv, k);

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
  else if (GET_CODE (x) == CONST_DOUBLE &&
	   (GET_MODE (x) == DFmode || GET_MODE (x) == DDmode))
    {
      REAL_VALUE_TYPE rv;
      long k[2];

      REAL_VALUE_FROM_CONST_DOUBLE (rv, x);

      if (DECIMAL_FLOAT_MODE_P (GET_MODE (x)))
	REAL_VALUE_TO_TARGET_DECIMAL64 (rv, k);
      else
	REAL_VALUE_TO_TARGET_DOUBLE (rv, k);

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
  else if (GET_CODE (x) == CONST_DOUBLE &&
	   (GET_MODE (x) == SFmode || GET_MODE (x) == SDmode))
    {
      REAL_VALUE_TYPE rv;
      long l;

      REAL_VALUE_FROM_CONST_DOUBLE (rv, x);
      if (DECIMAL_FLOAT_MODE_P (GET_MODE (x)))
	REAL_VALUE_TO_TARGET_DECIMAL32 (rv, l);
      else
	REAL_VALUE_TO_TARGET_SINGLE (rv, l);

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
  else if (GET_MODE (x) == VOIDmode && GET_CODE (x) == CONST_INT)
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
		  && GET_CODE (XEXP (XEXP (x, 0), 1)) == CONST_INT);

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
  if (TARGET_XCOFF && GET_CODE (base) == SYMBOL_REF
      && SYMBOL_REF_TLS_MODEL (base) != 0)
    {
      if (SYMBOL_REF_TLS_MODEL (base) == TLS_MODEL_LOCAL_EXEC)
	fputs ("@le", file);
      else if (SYMBOL_REF_TLS_MODEL (base) == TLS_MODEL_INITIAL_EXEC)
	fputs ("@ie", file);
      /* Use global-dynamic for local-dynamic.  */
      else if (SYMBOL_REF_TLS_MODEL (base) == TLS_MODEL_GLOBAL_DYNAMIC
	       || SYMBOL_REF_TLS_MODEL (base) == TLS_MODEL_LOCAL_DYNAMIC)
	{
	  putc ('\n', file);
	  (*targetm.asm_out.internal_label) (file, "LCM", labelno);
	  fputs ("\t.tc .", file);
	  RS6000_OUTPUT_BASENAME (file, name);
	  fputs ("[TC],", file);
	  output_addr_const (file, x);
	  fputs ("@m", file);
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
			   LCT_NORMAL, VOIDmode, 0);
      else
	{
	  char buf[30];
	  const char *label_name;
	  rtx fun;

	  ASM_GENERATE_INTERNAL_LABEL (buf, "LP", labelno);
	  label_name = ggc_strdup ((*targetm.strip_name_encoding) (buf));
	  fun = gen_rtx_SYMBOL_REF (Pmode, label_name);

	  emit_library_call (init_one_libfunc (RS6000_MCOUNT),
			     LCT_NORMAL, VOIDmode, 1, fun, Pmode);
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
			 LCT_NORMAL, VOIDmode, 1,
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

static rtx last_scheduled_insn;

/* The following variable helps to balance issuing of load and
   store instructions */

static int load_store_pendulum;

/* Power4 load update and store update instructions are cracked into a
   load or store and an integer insn which are executed in the same cycle.
   Branches have their own dispatch slot which does not count against the
   GCC issue rate, but it changes the program flow so there are no other
   instructions to issue in this cycle.  */

static int
rs6000_variable_issue_1 (rtx insn, int more)
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

  if (rs6000_cpu_attr == CPU_CELL && is_nonpipeline_insn (insn))
    return 0;

  cached_can_issue_more = more - 1;
  return cached_can_issue_more;
}

static int
rs6000_variable_issue (FILE *stream, int verbose, rtx insn, int more)
{
  int r = rs6000_variable_issue_1 (insn, more);
  if (verbose)
    fprintf (stream, "// rs6000_variable_issue (more = %d) = %d\n", more, r);
  return r;
}

/* Adjust the cost of a scheduling dependency.  Return the new cost of
   a dependency LINK or INSN on DEP_INSN.  COST is the current cost.  */

static int
rs6000_adjust_cost (rtx insn, rtx link, rtx dep_insn, int cost)
{
  enum attr_type attr_type;

  if (! recog_memoized (insn))
    return 0;

  switch (REG_NOTE_KIND (link))
    {
    case REG_DEP_TRUE:
      {
        /* Data dependency; DEP_INSN writes a register that INSN reads
	   some cycles later.  */

	/* Separate a load from a narrower, dependent store.  */
	if (rs6000_sched_groups
	    && GET_CODE (PATTERN (insn)) == SET
	    && GET_CODE (PATTERN (dep_insn)) == SET
	    && GET_CODE (XEXP (PATTERN (insn), 1)) == MEM
	    && GET_CODE (XEXP (PATTERN (dep_insn), 0)) == MEM
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
            if ((rs6000_cpu_attr == CPU_PPC603
                 || rs6000_cpu_attr == CPU_PPC604
                 || rs6000_cpu_attr == CPU_PPC604E
                 || rs6000_cpu_attr == CPU_PPC620
                 || rs6000_cpu_attr == CPU_PPC630
                 || rs6000_cpu_attr == CPU_PPC750
                 || rs6000_cpu_attr == CPU_PPC7400
                 || rs6000_cpu_attr == CPU_PPC7450
                 || rs6000_cpu_attr == CPU_PPCE5500
                 || rs6000_cpu_attr == CPU_PPCE6500
                 || rs6000_cpu_attr == CPU_POWER4
                 || rs6000_cpu_attr == CPU_POWER5
		 || rs6000_cpu_attr == CPU_POWER7
		 || rs6000_cpu_attr == CPU_POWER8
                 || rs6000_cpu_attr == CPU_CELL)
                && recog_memoized (dep_insn)
                && (INSN_CODE (dep_insn) >= 0))

              switch (get_attr_type (dep_insn))
                {
                case TYPE_CMP:
                case TYPE_COMPARE:
                case TYPE_DELAYED_COMPARE:
                case TYPE_IMUL_COMPARE:
                case TYPE_LMUL_COMPARE:
                case TYPE_FPCOMPARE:
                case TYPE_CR_LOGICAL:
                case TYPE_DELAYED_CR:
		  return cost + 2;
		default:
		  break;
		}
            break;

          case TYPE_STORE:
          case TYPE_STORE_U:
          case TYPE_STORE_UX:
          case TYPE_FPSTORE:
          case TYPE_FPSTORE_U:
          case TYPE_FPSTORE_UX:
            if ((rs6000_cpu == PROCESSOR_POWER6)
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
                  case TYPE_LOAD_U:
                  case TYPE_LOAD_UX:
                  case TYPE_CNTLZ:
                    {
                      if (! store_data_bypass_p (dep_insn, insn))
                        return 4;
                      break;
                    }
                  case TYPE_LOAD_EXT:
                  case TYPE_LOAD_EXT_U:
                  case TYPE_LOAD_EXT_UX:
                  case TYPE_VAR_SHIFT_ROTATE:
                  case TYPE_VAR_DELAYED_COMPARE:
                    {
                      if (! store_data_bypass_p (dep_insn, insn))
                        return 6;
                      break;
		    }
                  case TYPE_INTEGER:
                  case TYPE_COMPARE:
                  case TYPE_FAST_COMPARE:
                  case TYPE_EXTS:
                  case TYPE_SHIFT:
                  case TYPE_INSERT_WORD:
                  case TYPE_INSERT_DWORD:
                  case TYPE_FPLOAD_U:
                  case TYPE_FPLOAD_UX:
                  case TYPE_STORE_U:
                  case TYPE_STORE_UX:
                  case TYPE_FPSTORE_U:
                  case TYPE_FPSTORE_UX:
                    {
                      if (! store_data_bypass_p (dep_insn, insn))
                        return 3;
                      break;
                    }
                  case TYPE_IMUL:
                  case TYPE_IMUL2:
                  case TYPE_IMUL3:
                  case TYPE_LMUL:
                  case TYPE_IMUL_COMPARE:
                  case TYPE_LMUL_COMPARE:
                    {
                      if (! store_data_bypass_p (dep_insn, insn))
                        return 17;
                      break;
                    }
                  case TYPE_IDIV:
                    {
                      if (! store_data_bypass_p (dep_insn, insn))
                        return 45;
                      break;
                    }
                  case TYPE_LDIV:
                    {
                      if (! store_data_bypass_p (dep_insn, insn))
                        return 57;
                      break;
                    }
                  default:
                    break;
                  }
              }
	    break;

          case TYPE_LOAD:
          case TYPE_LOAD_U:
          case TYPE_LOAD_UX:
          case TYPE_LOAD_EXT:
          case TYPE_LOAD_EXT_U:
          case TYPE_LOAD_EXT_UX:
            if ((rs6000_cpu == PROCESSOR_POWER6)
                && recog_memoized (dep_insn)
                && (INSN_CODE (dep_insn) >= 0))
              {

                /* Adjust the cost for the case where the value written
                   by a fixed point instruction is used within the address
                   gen portion of a subsequent load(u)(x) */
                switch (get_attr_type (dep_insn))
                  {
                  case TYPE_LOAD:
                  case TYPE_LOAD_U:
                  case TYPE_LOAD_UX:
                  case TYPE_CNTLZ:
                    {
                      if (set_to_load_agen (dep_insn, insn))
                        return 4;
                      break;
                    }
                  case TYPE_LOAD_EXT:
                  case TYPE_LOAD_EXT_U:
                  case TYPE_LOAD_EXT_UX:
                  case TYPE_VAR_SHIFT_ROTATE:
                  case TYPE_VAR_DELAYED_COMPARE:
                    {
                      if (set_to_load_agen (dep_insn, insn))
                        return 6;
                      break;
                    }
                  case TYPE_INTEGER:
                  case TYPE_COMPARE:
                  case TYPE_FAST_COMPARE:
                  case TYPE_EXTS:
                  case TYPE_SHIFT:
                  case TYPE_INSERT_WORD:
                  case TYPE_INSERT_DWORD:
                  case TYPE_FPLOAD_U:
                  case TYPE_FPLOAD_UX:
                  case TYPE_STORE_U:
                  case TYPE_STORE_UX:
                  case TYPE_FPSTORE_U:
                  case TYPE_FPSTORE_UX:
                    {
                      if (set_to_load_agen (dep_insn, insn))
                        return 3;
                      break;
                    }
                  case TYPE_IMUL:
                  case TYPE_IMUL2:
                  case TYPE_IMUL3:
                  case TYPE_LMUL:
                  case TYPE_IMUL_COMPARE:
                  case TYPE_LMUL_COMPARE:
                    {
                      if (set_to_load_agen (dep_insn, insn))
                        return 17;
                      break;
                    }
                  case TYPE_IDIV:
                    {
                      if (set_to_load_agen (dep_insn, insn))
                        return 45;
                      break;
                    }
                  case TYPE_LDIV:
                    {
                      if (set_to_load_agen (dep_insn, insn))
                        return 57;
                      break;
                    }
                  default:
                    break;
                  }
              }
            break;

          case TYPE_FPLOAD:
            if ((rs6000_cpu == PROCESSOR_POWER6)
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
      if ((rs6000_cpu == PROCESSOR_POWER6)
          && recog_memoized (dep_insn)
          && (INSN_CODE (dep_insn) >= 0))
        {
          attr_type = get_attr_type (insn);

          switch (attr_type)
            {
            case TYPE_FP:
              if (get_attr_type (dep_insn) == TYPE_FP)
                return 1;
              break;
            case TYPE_FPLOAD:
              if (get_attr_type (dep_insn) == TYPE_MFFGPR)
                return 2;
              break;
            default:
              break;
            }
        }
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
rs6000_debug_adjust_cost (rtx insn, rtx link, rtx dep_insn, int cost)
{
  int ret = rs6000_adjust_cost (insn, link, dep_insn, cost);

  if (ret != cost)
    {
      const char *dep;

      switch (REG_NOTE_KIND (link))
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
is_microcoded_insn (rtx insn)
{
  if (!insn || !NONDEBUG_INSN_P (insn)
      || GET_CODE (PATTERN (insn)) == USE
      || GET_CODE (PATTERN (insn)) == CLOBBER)
    return false;

  if (rs6000_cpu_attr == CPU_CELL)
    return get_attr_cell_micro (insn) == CELL_MICRO_ALWAYS;

  if (rs6000_sched_groups
      && (rs6000_cpu == PROCESSOR_POWER4 || rs6000_cpu == PROCESSOR_POWER5))
    {
      enum attr_type type = get_attr_type (insn);
      if (type == TYPE_LOAD_EXT_U
	  || type == TYPE_LOAD_EXT_UX
	  || type == TYPE_LOAD_UX
	  || type == TYPE_STORE_UX
	  || type == TYPE_MFCR)
	return true;
    }

  return false;
}

/* The function returns true if INSN is cracked into 2 instructions
   by the processor (and therefore occupies 2 issue slots).  */

static bool
is_cracked_insn (rtx insn)
{
  if (!insn || !NONDEBUG_INSN_P (insn)
      || GET_CODE (PATTERN (insn)) == USE
      || GET_CODE (PATTERN (insn)) == CLOBBER)
    return false;

  if (rs6000_sched_groups
      && (rs6000_cpu == PROCESSOR_POWER4 || rs6000_cpu == PROCESSOR_POWER5))
    {
      enum attr_type type = get_attr_type (insn);
      if (type == TYPE_LOAD_U || type == TYPE_STORE_U
	  || type == TYPE_FPLOAD_U || type == TYPE_FPSTORE_U
	  || type == TYPE_FPLOAD_UX || type == TYPE_FPSTORE_UX
	  || type == TYPE_LOAD_EXT || type == TYPE_DELAYED_CR
	  || type == TYPE_COMPARE || type == TYPE_DELAYED_COMPARE
	  || type == TYPE_IMUL_COMPARE || type == TYPE_LMUL_COMPARE
	  || type == TYPE_IDIV || type == TYPE_LDIV
	  || type == TYPE_INSERT_WORD)
	return true;
    }

  return false;
}

/* The function returns true if INSN can be issued only from
   the branch slot.  */

static bool
is_branch_slot_insn (rtx insn)
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
set_to_load_agen (rtx out_insn, rtx in_insn)
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

  if (GET_CODE (XEXP (mem, 0)) == PRE_MODIFY)
    addr_rtx = XEXP (XEXP (mem, 0), 1);
  else
    addr_rtx = (XEXP (mem, 0));

  if (GET_CODE (addr_rtx) == REG)
    {
      *base = addr_rtx;
      *offset = 0;
    }
  else if (GET_CODE (addr_rtx) == PLUS
	   && CONST_INT_P (XEXP (addr_rtx, 1)))
    {
      *base = XEXP (addr_rtx, 0);
      *offset = INTVAL (XEXP (addr_rtx, 1));
    }
  else
    return false;

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
rs6000_adjust_priority (rtx insn ATTRIBUTE_UNUSED, int priority)
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

  switch (rs6000_cpu_attr) {
  case CPU_PPC750:
    switch (get_attr_type (insn))
      {
      default:
	break;

      case TYPE_IMUL:
      case TYPE_IDIV:
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

  if (rs6000_cpu == PROCESSOR_POWER6
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
is_nonpipeline_insn (rtx insn)
{
  enum attr_type type;
  if (!insn || !NONDEBUG_INSN_P (insn)
      || GET_CODE (PATTERN (insn)) == USE
      || GET_CODE (PATTERN (insn)) == CLOBBER)
    return false;

  type = get_attr_type (insn);
  if (type == TYPE_IMUL
      || type == TYPE_IMUL2
      || type == TYPE_IMUL3
      || type == TYPE_LMUL
      || type == TYPE_IDIV
      || type == TYPE_LDIV
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

  switch (rs6000_cpu_attr) {
  case CPU_RS64A:
  case CPU_PPC601: /* ? */
  case CPU_PPC7450:
    return 3;
  case CPU_PPC440:
  case CPU_PPC603:
  case CPU_PPC750:
  case CPU_PPC7400:
  case CPU_PPC8540:
  case CPU_PPC8548:
  case CPU_CELL:
  case CPU_PPCE300C2:
  case CPU_PPCE300C3:
  case CPU_PPCE500MC:
  case CPU_PPCE500MC64:
  case CPU_PPCE5500:
  case CPU_PPCE6500:
  case CPU_TITAN:
    return 2;
  case CPU_PPC476:
  case CPU_PPC604:
  case CPU_PPC604E:
  case CPU_PPC620:
  case CPU_PPC630:
    return 4;
  case CPU_POWER4:
  case CPU_POWER5:
  case CPU_POWER6:
  case CPU_POWER7:
    return 5;
  case CPU_POWER8:
    return 7;
  default:
    return 1;
  }
}

/* Return how many instructions to look ahead for better insn
   scheduling.  */

static int
rs6000_use_sched_lookahead (void)
{
  switch (rs6000_cpu_attr)
    {
    case CPU_PPC8540:
    case CPU_PPC8548:
      return 4;

    case CPU_CELL:
      return (reload_completed ? 8 : 0);

    default:
      return 0;
    }
}

/* We are choosing insn from the ready queue.  Return nonzero if INSN can be chosen.  */
static int
rs6000_use_sched_lookahead_guard (rtx insn)
{
  if (rs6000_cpu_attr != CPU_CELL)
    return 1;

   if (insn == NULL_RTX || !INSN_P (insn))
     abort ();

  if (!reload_completed
      || is_nonpipeline_insn (insn)
      || is_microcoded_insn (insn))
    return 0;

  return 1;
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

  if (GET_CODE (pat) == MEM)
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

static rtx
get_next_active_insn (rtx insn, rtx tail)
{
  if (insn == NULL_RTX || insn == tail)
    return NULL_RTX;

  while (1)
    {
      insn = NEXT_INSN (insn);
      if (insn == NULL_RTX || insn == tail)
	return NULL_RTX;

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

/* We are about to begin issuing insns for this clock cycle. */

static int
rs6000_sched_reorder (FILE *dump ATTRIBUTE_UNUSED, int sched_verbose,
                        rtx *ready ATTRIBUTE_UNUSED,
                        int *pn_ready ATTRIBUTE_UNUSED,
		        int clock_var ATTRIBUTE_UNUSED)
{
  int n_ready = *pn_ready;

  if (sched_verbose)
    fprintf (dump, "// rs6000_sched_reorder :\n");

  /* Reorder the ready list, if the second to last ready insn
     is a nonepipeline insn.  */
  if (rs6000_cpu_attr == CPU_CELL && n_ready > 1)
  {
    if (is_nonpipeline_insn (ready[n_ready - 1])
        && (recog_memoized (ready[n_ready - 2]) > 0))
      /* Simply swap first two insns.  */
      {
	rtx tmp = ready[n_ready - 1];
	ready[n_ready - 1] = ready[n_ready - 2];
	ready[n_ready - 2] = tmp;
      }
  }

  if (rs6000_cpu == PROCESSOR_POWER6)
    load_store_pendulum = 0;

  return rs6000_issue_rate ();
}

/* Like rs6000_sched_reorder, but called after issuing each insn.  */

static int
rs6000_sched_reorder2 (FILE *dump, int sched_verbose, rtx *ready,
		         int *pn_ready, int clock_var ATTRIBUTE_UNUSED)
{
  if (sched_verbose)
    fprintf (dump, "// rs6000_sched_reorder2 :\n");

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
  if (rs6000_cpu == PROCESSOR_POWER6 && last_scheduled_insn)
    {
      int pos;
      int i;
      rtx tmp, load_mem, str_mem;

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
      if ((load_store_pendulum == 0) || (*pn_ready <= 1))
        return cached_can_issue_more;

      if (load_store_pendulum == 1)
        {
          /* A load has been issued in this cycle.  Scan the ready list
             for another load to issue with it */
          pos = *pn_ready-1;

          while (pos >= 0)
            {
              if (is_load_insn (ready[pos], &load_mem))
                {
                  /* Found a load.  Move it to the head of the ready list,
                     and adjust it's priority so that it is more likely to
                     stay there */
                  tmp = ready[pos];
                  for (i=pos; i<*pn_ready-1; i++)
                    ready[i] = ready[i + 1];
                  ready[*pn_ready-1] = tmp;

                  if (!sel_sched_p () && INSN_PRIORITY_KNOWN (tmp))
                    INSN_PRIORITY (tmp)++;
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
          pos = *pn_ready-1;

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

          pos = *pn_ready-1;

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
                      tmp = ready[pos];
                      for (i=pos; i<*pn_ready-1; i++)
                        ready[i] = ready[i + 1];
                      ready[*pn_ready-1] = tmp;

                      if (!sel_sched_p () && INSN_PRIORITY_KNOWN (tmp))
                        INSN_PRIORITY (tmp)++;

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
              tmp = ready[first_store_pos];
              for (i=first_store_pos; i<*pn_ready-1; i++)
                ready[i] = ready[i + 1];
              ready[*pn_ready-1] = tmp;
              if (!sel_sched_p () && INSN_PRIORITY_KNOWN (tmp))
                INSN_PRIORITY (tmp)++;
            }
        }
      else if (load_store_pendulum == 2)
       {
           /* Two loads have been issued in this cycle.  Increase the priority
              of the first store in the ready list to favor it for issuing in
              the next cycle. */
          pos = *pn_ready-1;

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
    }

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
insn_terminates_group_p (rtx insn, enum group_termination which_group)
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
insn_must_be_first_in_group (rtx insn)
{
  enum attr_type type;

  if (!insn
      || NOTE_P (insn)
      || DEBUG_INSN_P (insn)
      || GET_CODE (PATTERN (insn)) == USE
      || GET_CODE (PATTERN (insn)) == CLOBBER)
    return false;

  switch (rs6000_cpu)
    {
    case PROCESSOR_POWER5:
      if (is_cracked_insn (insn))
        return true;
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
        case TYPE_DELAYED_CR:
        case TYPE_CR_LOGICAL:
        case TYPE_MTJMPR:
        case TYPE_MFJMPR:
        case TYPE_IDIV:
        case TYPE_LDIV:
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
        case TYPE_INSERT_DWORD:
        case TYPE_EXTS:
        case TYPE_CNTLZ:
        case TYPE_SHIFT:
        case TYPE_VAR_SHIFT_ROTATE:
        case TYPE_TRAP:
        case TYPE_IMUL:
        case TYPE_IMUL2:
        case TYPE_IMUL3:
        case TYPE_LMUL:
        case TYPE_IDIV:
        case TYPE_INSERT_WORD:
        case TYPE_DELAYED_COMPARE:
        case TYPE_IMUL_COMPARE:
        case TYPE_LMUL_COMPARE:
        case TYPE_FPCOMPARE:
        case TYPE_MFCR:
        case TYPE_MTCR:
        case TYPE_MFJMPR:
        case TYPE_MTJMPR:
        case TYPE_ISYNC:
        case TYPE_SYNC:
        case TYPE_LOAD_L:
        case TYPE_STORE_C:
        case TYPE_LOAD_U:
        case TYPE_LOAD_UX:
        case TYPE_LOAD_EXT_UX:
        case TYPE_STORE_U:
        case TYPE_STORE_UX:
        case TYPE_FPLOAD_U:
        case TYPE_FPLOAD_UX:
        case TYPE_FPSTORE_U:
        case TYPE_FPSTORE_UX:
          return true;
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
        case TYPE_IDIV:
        case TYPE_LDIV:
        case TYPE_COMPARE:
        case TYPE_DELAYED_COMPARE:
        case TYPE_VAR_DELAYED_COMPARE:
        case TYPE_ISYNC:
        case TYPE_LOAD_L:
        case TYPE_STORE_C:
        case TYPE_LOAD_U:
        case TYPE_LOAD_UX:
        case TYPE_LOAD_EXT:
        case TYPE_LOAD_EXT_U:
        case TYPE_LOAD_EXT_UX:
        case TYPE_STORE_U:
        case TYPE_STORE_UX:
        case TYPE_FPLOAD_U:
        case TYPE_FPLOAD_UX:
        case TYPE_FPSTORE_U:
        case TYPE_FPSTORE_UX:
        case TYPE_MFJMPR:
        case TYPE_MTJMPR:
          return true;
        default:
          break;
        }
      break;
    case PROCESSOR_POWER8:
      type = get_attr_type (insn);

      switch (type)
        {
        case TYPE_CR_LOGICAL:
        case TYPE_DELAYED_CR:
        case TYPE_MFCR:
        case TYPE_MFCRF:
        case TYPE_MTCR:
        case TYPE_COMPARE:
        case TYPE_DELAYED_COMPARE:
        case TYPE_VAR_DELAYED_COMPARE:
        case TYPE_IMUL_COMPARE:
        case TYPE_LMUL_COMPARE:
        case TYPE_SYNC:
        case TYPE_ISYNC:
        case TYPE_LOAD_L:
        case TYPE_STORE_C:
        case TYPE_LOAD_U:
        case TYPE_LOAD_UX:
        case TYPE_LOAD_EXT:
        case TYPE_LOAD_EXT_U:
        case TYPE_LOAD_EXT_UX:
        case TYPE_STORE_UX:
        case TYPE_VECSTORE:
        case TYPE_MFJMPR:
        case TYPE_MTJMPR:
          return true;
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
insn_must_be_last_in_group (rtx insn)
{
  enum attr_type type;

  if (!insn
      || NOTE_P (insn)
      || DEBUG_INSN_P (insn)
      || GET_CODE (PATTERN (insn)) == USE
      || GET_CODE (PATTERN (insn)) == CLOBBER)
    return false;

  switch (rs6000_cpu) {
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
      case TYPE_SHIFT:
      case TYPE_VAR_SHIFT_ROTATE:
      case TYPE_TRAP:
      case TYPE_IMUL:
      case TYPE_IMUL2:
      case TYPE_IMUL3:
      case TYPE_LMUL:
      case TYPE_IDIV:
      case TYPE_DELAYED_COMPARE:
      case TYPE_IMUL_COMPARE:
      case TYPE_LMUL_COMPARE:
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
      case TYPE_LOAD_EXT_U:
      case TYPE_LOAD_EXT_UX:
      case TYPE_STORE_UX:
        return true;
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
      case TYPE_LOAD_EXT_U:
      case TYPE_LOAD_EXT_UX:
      case TYPE_STORE_UX:
        return true;
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
		 rtx next_insn, bool *group_end, int can_issue_more,
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
      if (rs6000_cpu_attr == CPU_POWER6 || rs6000_cpu_attr == CPU_POWER7
	  || rs6000_cpu_attr == CPU_POWER8)
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
redefine_groups (FILE *dump, int sched_verbose, rtx prev_head_insn, rtx tail)
{
  rtx insn, next_insn;
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
pad_groups (FILE *dump, int sched_verbose, rtx prev_head_insn, rtx tail)
{
  rtx insn, next_insn;
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
  last_scheduled_insn = NULL_RTX;
  load_store_pendulum = 0;
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

struct _rs6000_sched_context
{
  short cached_can_issue_more;
  rtx last_scheduled_insn;
  int load_store_pendulum;
};

typedef struct _rs6000_sched_context rs6000_sched_context_def;
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
      sc->last_scheduled_insn = NULL_RTX;
      sc->load_store_pendulum = 0;
    }
  else
    {
      sc->cached_can_issue_more = cached_can_issue_more;
      sc->last_scheduled_insn = last_scheduled_insn;
      sc->load_store_pendulum = load_store_pendulum;
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
}

/* Free _SC.  */
static void
rs6000_free_sched_context (void *_sc)
{
  gcc_assert (_sc != NULL);

  free (_sc);
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
	  error ("You cannot take the address of a nested function if you use "
		 "the -mno-pointers-to-nested-functions option.");

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
			 LCT_NORMAL, VOIDmode, 4,
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
  enum machine_mode mode;
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
		   "64-bit code without -mvsx");
	  else if (rs6000_warn_altivec_long)
	    warning (0, "use of %<long%> in AltiVec types is deprecated; "
		     "use %<int%>");
	}
      else if (type == long_long_unsigned_type_node
	       || type == long_long_integer_type_node)
	error ("use of %<long long%> in AltiVec types is invalid without "
	       "-mvsx");
      else if (type == double_type_node)
	error ("use of %<double%> in AltiVec types is invalid without -mvsx");
    }

  switch (altivec_type)
    {
    case 'v':
      unsigned_p = TYPE_UNSIGNED (type);
      switch (mode)
	{
	case DImode:
	  result = (unsigned_p ? unsigned_V2DI_type_node : V2DI_type_node);
	  break;
	case SImode:
	  result = (unsigned_p ? unsigned_V4SI_type_node : V4SI_type_node);
	  break;
	case HImode:
	  result = (unsigned_p ? unsigned_V8HI_type_node : V8HI_type_node);
	  break;
	case QImode:
	  result = (unsigned_p ? unsigned_V16QI_type_node : V16QI_type_node);
	  break;
	case SFmode: result = V4SF_type_node; break;
	case DFmode: result = V2DF_type_node; break;
	  /* If the user says 'vector int bool', we may be handed the 'bool'
	     attribute _before_ the 'vector' attribute, and so select the
	     proper type in the 'b' case below.  */
	case V4SImode: case V8HImode: case V16QImode: case V4SFmode:
	case V2DImode: case V2DFmode:
	  result = type;
	default: break;
	}
      break;
    case 'b':
      switch (mode)
	{
	case DImode: case V2DImode: result = bool_V2DI_type_node; break;
	case SImode: case V4SImode: result = bool_V4SI_type_node; break;
	case HImode: case V8HImode: result = bool_V8HI_type_node; break;
	case QImode: case V16QImode: result = bool_V16QI_type_node;
	default: break;
	}
      break;
    case 'p':
      switch (mode)
	{
	case V8HImode: result = pixel_V8HI_type_node;
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

/* AltiVec defines four built-in scalar types that serve as vector
   elements; we must teach the compiler how to mangle them.  */

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
  if (type == bool_long_type_node) return "U6__booll";

  /* Mangle IBM extended float long double as `g' (__float128) on
     powerpc*-linux where long-double-64 previously was the default.  */
  if (TYPE_MAIN_VARIANT (type) == long_double_type_node
      && TARGET_ELF
      && TARGET_LONG_DOUBLE_128
      && !TARGET_IEEEQUAD)
    return "g";

  /* For all other types, use normal C++ mangling.  */
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

rtx
rs6000_longcall_ref (rtx call_ref)
{
  const char *call_name;
  tree node;

  if (GET_CODE (call_ref) != SYMBOL_REF)
    return call_ref;

  /* System V adds '.' to the internal name, so skip them.  */
  call_name = XSTR (call_ref, 0);
  if (*call_name == '.')
    {
      while (*call_name == '.')
	call_name++;

      node = get_identifier (call_name);
      call_ref = gen_rtx_SYMBOL_REF (VOIDmode, IDENTIFIER_POINTER (node));
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
      && TARGET_MINIMAL_TOC
      && !TARGET_RELOCATABLE)
    {
      if (!toc_initialized)
	{
	  toc_initialized = 1;
	  fprintf (asm_out_file, "%s\n", TOC_SECTION_ASM_OP);
	  (*targetm.asm_out.internal_label) (asm_out_file, "LCTOC", 0);
	  fprintf (asm_out_file, "\t.tc ");
	  ASM_OUTPUT_INTERNAL_LABEL_PREFIX (asm_out_file, "LCTOC1[TC],");
	  ASM_OUTPUT_INTERNAL_LABEL_PREFIX (asm_out_file, "LCTOC1");
	  fprintf (asm_out_file, "\n");

	  fprintf (asm_out_file, "%s\n", MINIMAL_TOC_SECTION_ASM_OP);
	  ASM_OUTPUT_INTERNAL_LABEL_PREFIX (asm_out_file, "LCTOC1");
	  fprintf (asm_out_file, " = .+32768\n");
	}
      else
	fprintf (asm_out_file, "%s\n", MINIMAL_TOC_SECTION_ASM_OP);
    }
  else if ((DEFAULT_ABI == ABI_AIX || DEFAULT_ABI == ABI_ELFv2)
	   && !TARGET_RELOCATABLE)
    fprintf (asm_out_file, "%s\n", TOC_SECTION_ASM_OP);
  else
    {
      fprintf (asm_out_file, "%s\n", MINIMAL_TOC_SECTION_ASM_OP);
      if (!toc_initialized)
	{
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
rs6000_elf_select_rtx_section (enum machine_mode mode, rtx x,
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
      const char *section = TREE_STRING_POINTER (DECL_SECTION_NAME (decl));
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
rs6000_use_blocks_for_constant_p (enum machine_mode mode, const_rtx x)
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
      if (GET_CODE (XEXP (addr, 0)) == REG
	  && REGNO (XEXP (addr, 0)) != 0)
	addr = XEXP (addr, 0);
      else if (GET_CODE (XEXP (addr, 1)) == REG
	       && REGNO (XEXP (addr, 1)) != 0)
	addr = XEXP (addr, 1);
      else if (CONSTANT_P (XEXP (addr, 0)))
	addr = XEXP (addr, 1);
      else if (CONSTANT_P (XEXP (addr, 1)))
	addr = XEXP (addr, 0);
      else
	gcc_unreachable ();
    }
  gcc_assert (GET_CODE (addr) == REG && REGNO (addr) != 0);
  return addr;
}

void
rs6000_fatal_bad_address (rtx op)
{
  fatal_insn ("bad address", op);
}

#if TARGET_MACHO

typedef struct branch_island_d {
  tree function_name;
  tree label_name;
  int line_number;
} branch_island;


static vec<branch_island, va_gc> *branch_islands;

/* Remember to generate a branch island for far calls to the given
   function.  */

static void
add_compiler_branch_island (tree label_name, tree function_name,
			    int line_number)
{
  branch_island bi = {function_name, label_name, line_number};
  vec_safe_push (branch_islands, bi);
}

/* Generate far-jump branch islands for everything recorded in
   branch_islands.  Invoked immediately after the last instruction of
   the epilogue has been emitted; the branch islands must be appended
   to, and contiguous with, the function body.  Mach-O stubs are
   generated in machopic_output_stub().  */

static void
macho_branch_islands (void)
{
  char tmp_buf[512];

  while (!vec_safe_is_empty (branch_islands))
    {
      branch_island *bi = &branch_islands->last ();
      const char *label = IDENTIFIER_POINTER (bi->label_name);
      const char *name = IDENTIFIER_POINTER (bi->function_name);
      char name_buf[512];
      /* Cheap copy of the details from the Darwin ASM_OUTPUT_LABELREF().  */
      if (name[0] == '*' || name[0] == '&')
	strcpy (name_buf, name+1);
      else
	{
	  name_buf[0] = '_';
	  strcpy (name_buf+1, name);
	}
      strcpy (tmp_buf, "\n");
      strcat (tmp_buf, label);
#if defined (DBX_DEBUGGING_INFO) || defined (XCOFF_DEBUGGING_INFO)
      if (write_symbols == DBX_DEBUG || write_symbols == XCOFF_DEBUG)
	dbxout_stabd (N_SLINE, bi->line_number);
#endif /* DBX_DEBUGGING_INFO || XCOFF_DEBUGGING_INFO */
      if (flag_pic)
	{
	  if (TARGET_LINK_STACK)
	    {
	      char name[32];
	      get_ppc476_thunk_name (name);
	      strcat (tmp_buf, ":\n\tmflr r0\n\tbl ");
	      strcat (tmp_buf, name);
	      strcat (tmp_buf, "\n");
	      strcat (tmp_buf, label);
	      strcat (tmp_buf, "_pic:\n\tmflr r11\n");
	    }
	  else
	    {
	      strcat (tmp_buf, ":\n\tmflr r0\n\tbcl 20,31,");
	      strcat (tmp_buf, label);
	      strcat (tmp_buf, "_pic\n");
	      strcat (tmp_buf, label);
	      strcat (tmp_buf, "_pic:\n\tmflr r11\n");
	    }

	  strcat (tmp_buf, "\taddis r11,r11,ha16(");
	  strcat (tmp_buf, name_buf);
	  strcat (tmp_buf, " - ");
	  strcat (tmp_buf, label);
	  strcat (tmp_buf, "_pic)\n");

	  strcat (tmp_buf, "\tmtlr r0\n");

	  strcat (tmp_buf, "\taddi r12,r11,lo16(");
	  strcat (tmp_buf, name_buf);
	  strcat (tmp_buf, " - ");
	  strcat (tmp_buf, label);
	  strcat (tmp_buf, "_pic)\n");

	  strcat (tmp_buf, "\tmtctr r12\n\tbctr\n");
	}
      else
	{
	  strcat (tmp_buf, ":\nlis r12,hi16(");
	  strcat (tmp_buf, name_buf);
	  strcat (tmp_buf, ")\n\tori r12,r12,lo16(");
	  strcat (tmp_buf, name_buf);
	  strcat (tmp_buf, ")\n\tmtctr r12\n\tbctr");
	}
      output_asm_insn (tmp_buf, 0);
#if defined (DBX_DEBUGGING_INFO) || defined (XCOFF_DEBUGGING_INFO)
      if (write_symbols == DBX_DEBUG || write_symbols == XCOFF_DEBUG)
	dbxout_stabd (N_SLINE, bi->line_number);
#endif /* DBX_DEBUGGING_INFO || XCOFF_DEBUGGING_INFO */
      branch_islands->pop ();
    }
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

/* INSN is either a function call or a millicode call.  It may have an
   unconditional jump in its delay slot.

   CALL_DEST is the routine we are calling.  */

char *
output_call (rtx insn, rtx *operands, int dest_operand_number,
	     int cookie_operand_number)
{
  static char buf[256];
  if (darwin_emit_branch_islands
      && GET_CODE (operands[dest_operand_number]) == SYMBOL_REF
      && (INTVAL (operands[cookie_operand_number]) & CALL_LONG))
    {
      tree labelname;
      tree funname = get_identifier (XSTR (operands[dest_operand_number], 0));

      if (no_previous_def (funname))
	{
	  rtx label_rtx = gen_label_rtx ();
	  char *label_buf, temp_buf[256];
	  ASM_GENERATE_INTERNAL_LABEL (temp_buf, "L",
				       CODE_LABEL_NUMBER (label_rtx));
	  label_buf = temp_buf[0] == '*' ? temp_buf + 1 : temp_buf;
	  labelname = get_identifier (label_buf);
	  add_compiler_branch_island (labelname, funname, insn_line (insn));
	}
      else
	labelname = get_prev_label (funname);

      /* "jbsr foo, L42" is Mach-O for "Link as 'bl foo' if a 'bl'
	 instruction will reach 'foo', otherwise link as 'bl L42'".
	 "L42" should be a 'branch island', that will do a far jump to
	 'foo'.  Branch islands are generated in
	 macho_branch_islands().  */
      sprintf (buf, "jbsr %%z%d,%.246s",
	       dest_operand_number, IDENTIFIER_POINTER (labelname));
    }
  else
    sprintf (buf, "bl %%z%d", dest_operand_number);
  return buf;
}

/* Generate PIC and indirect symbol stubs.  */

void
machopic_output_stub (FILE *file, const char *symb, const char *stub)
{
  unsigned int length;
  char *symbol_name, *lazy_ptr_name;
  char *local_label_0;
  static int label = 0;

  /* Lose our funky encoding stuff so it doesn't contaminate the stub.  */
  symb = (*targetm.strip_name_encoding) (symb);


  length = strlen (symb);
  symbol_name = XALLOCAVEC (char, length + 32);
  GEN_SYMBOL_NAME_FOR_SYMBOL (symbol_name, symb, length);

  lazy_ptr_name = XALLOCAVEC (char, length + 32);
  GEN_LAZY_PTR_NAME_FOR_SYMBOL (lazy_ptr_name, symb, length);

  if (flag_pic == 2)
    switch_to_section (darwin_sections[machopic_picsymbol_stub1_section]);
  else
    switch_to_section (darwin_sections[machopic_symbol_stub1_section]);

  if (flag_pic == 2)
    {
      fprintf (file, "\t.align 5\n");

      fprintf (file, "%s:\n", stub);
      fprintf (file, "\t.indirect_symbol %s\n", symbol_name);

      label++;
      local_label_0 = XALLOCAVEC (char, sizeof ("\"L00000000000$spb\""));
      sprintf (local_label_0, "\"L%011d$spb\"", label);

      fprintf (file, "\tmflr r0\n");
      if (TARGET_LINK_STACK)
	{
	  char name[32];
	  get_ppc476_thunk_name (name);
	  fprintf (file, "\tbl %s\n", name);
	  fprintf (file, "%s:\n\tmflr r11\n", local_label_0);
	}
      else
	{
	  fprintf (file, "\tbcl 20,31,%s\n", local_label_0);
	  fprintf (file, "%s:\n\tmflr r11\n", local_label_0);
	}
      fprintf (file, "\taddis r11,r11,ha16(%s-%s)\n",
	       lazy_ptr_name, local_label_0);
      fprintf (file, "\tmtlr r0\n");
      fprintf (file, "\t%s r12,lo16(%s-%s)(r11)\n",
	       (TARGET_64BIT ? "ldu" : "lwzu"),
	       lazy_ptr_name, local_label_0);
      fprintf (file, "\tmtctr r12\n");
      fprintf (file, "\tbctr\n");
    }
  else
    {
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
rs6000_machopic_legitimize_pic_address (rtx orig, enum machine_mode mode,
					rtx reg)
{
  rtx base, offset;

  if (reg == NULL && ! reload_in_progress && ! reload_completed)
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

      if (GET_CODE (offset) == CONST_INT)
	{
	  if (SMALL_INT (offset))
	    return plus_constant (Pmode, base, INTVAL (offset));
	  else if (! reload_in_progress && ! reload_completed)
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
  char buf[16];

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

  if (TARGET_RELOCATABLE)
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
  char buf[16];

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

  if (TARGET_RELOCATABLE)
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

  if (TARGET_RELOCATABLE
      && !TARGET_SECURE_PLT
      && (get_pool_size () != 0 || crtl->profile)
      && uses_TOC ())
    {
      char buf[256];

      (*targetm.asm_out.internal_label) (file, "LCL", rs6000_pic_labelno);

      ASM_GENERATE_INTERNAL_LABEL (buf, "LCTOC", 1);
      fprintf (file, "\t.long ");
      assemble_name (file, buf);
      putc ('-', file);
      ASM_GENERATE_INTERNAL_LABEL (buf, "LCF", rs6000_pic_labelno);
      assemble_name (file, buf);
      putc ('\n', file);
    }

  ASM_OUTPUT_TYPE_DIRECTIVE (file, name, "function");
  ASM_DECLARE_RESULT (file, DECL_RESULT (decl));

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
  if (TARGET_32BIT && DEFAULT_ABI == ABI_V4)
    {
      if (rs6000_passes_float)
	fprintf (asm_out_file, "\t.gnu_attribute 4, %d\n",
		 ((TARGET_HARD_FLOAT && TARGET_FPRS && TARGET_DOUBLE_FLOAT) ? 1 
		  : (TARGET_HARD_FLOAT && TARGET_FPRS && TARGET_SINGLE_FLOAT) ? 3 
		  : 2));
      if (rs6000_passes_vector)
	fprintf (asm_out_file, "\t.gnu_attribute 8, %d\n",
		 (TARGET_ALTIVEC_ABI ? 2
		  : TARGET_SPE_ABI ? 3
		  : 1));
      if (rs6000_returns_struct)
	fprintf (asm_out_file, "\t.gnu_attribute 12, %d\n",
		 aix_struct_return ? 2 : 1);
    }
#endif
#if defined (POWERPC_LINUX) || defined (POWERPC_FREEBSD)
  if (TARGET_32BIT || DEFAULT_ABI == ABI_ELFv2)
    file_end_indicate_exec_stack ();
#endif
}
#endif

#if TARGET_XCOFF
static void
rs6000_xcoff_asm_output_anchor (rtx symbol)
{
  char buffer[100];

  sprintf (buffer, "$ + " HOST_WIDE_INT_PRINT_DEC,
	   SYMBOL_REF_BLOCK_OFFSET (symbol));
  ASM_OUTPUT_DEF (asm_out_file, XSTR (symbol, 0), buffer);
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

  tls_data_section
    = get_unnamed_section (SECTION_TLS,
			   rs6000_xcoff_output_tls_section_asm_op,
			   &xcoff_tls_data_section_name);

  tls_private_data_section
    = get_unnamed_section (SECTION_TLS,
			   rs6000_xcoff_output_tls_section_asm_op,
			   &xcoff_private_data_section_name);

  read_only_private_data_section
    = get_unnamed_section (0, rs6000_xcoff_output_readonly_section_asm_op,
			   &xcoff_private_data_section_name);

  toc_section
    = get_unnamed_section (0, rs6000_xcoff_output_toc_section_asm_op, NULL);

  readonly_data_section = read_only_data_section;
  exception_section = data_section;
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
  static const char * const suffix[4] = { "PR", "RO", "RW", "TL" };

  if (flags & SECTION_CODE)
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
   && DECL_SECTION_NAME (DECL) != NULL_TREE)

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
  DECL_SECTION_NAME (decl) = build_string (strlen (name), name);
}

/* Select section for constant in constant pool.

   On RS/6000, all constants are in the private read-only data area.
   However, if this is being placed in the TOC it must be output as a
   toc entry.  */

static section *
rs6000_xcoff_select_rtx_section (enum machine_mode mode, rtx x,
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

#ifdef HAVE_AS_TLS
static void
rs6000_xcoff_encode_section_info (tree decl, rtx rtl, int first)
{
  rtx symbol;
  int flags;

  default_encode_section_info (decl, rtl, first);

  /* Careful not to prod global register variables.  */
  if (!MEM_P (rtl))
    return;
  symbol = XEXP (rtl, 0);
  if (GET_CODE (symbol) != SYMBOL_REF)
    return;

  flags = SYMBOL_REF_FLAGS (symbol);

  if (TREE_CODE (decl) == VAR_DECL && DECL_THREAD_LOCAL_P (decl))
    flags &= ~SYMBOL_FLAG_HAS_BLOCK_INFO;

  SYMBOL_REF_FLAGS (symbol) = flags;
}
#endif /* HAVE_AS_TLS */
#endif /* TARGET_XCOFF */

/* Compute a (partial) cost for rtx X.  Return true if the complete
   cost has been computed, and false if subexpressions should be
   scanned.  In either case, *TOTAL contains the cost result.  */

static bool
rs6000_rtx_costs (rtx x, int code, int outer_code, int opno ATTRIBUTE_UNUSED,
		  int *total, bool speed)
{
  enum machine_mode mode = GET_MODE (x);

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
		      : satisfies_constraint_J (x))
		  || mask_operand (x, mode)
		  || (mode == DImode
		      && mask64_operand (x, DImode))))
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
    case CONST:
    case HIGH:
    case SYMBOL_REF:
    case MEM:
      /* When optimizing for size, MEM should be slightly more expensive
	 than generating address, e.g., (plus (reg) (const)).
	 L1 cache latency is about two instructions.  */
      *total = !speed ? COSTS_N_INSNS (1) + 1 : COSTS_N_INSNS (2);
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
      if (GET_CODE (XEXP (x, 1)) == CONST_INT
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
      if (GET_CODE (XEXP (x, 1)) == CONST_INT
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
      /* Add in shift and subtract for MOD. */
      if (code == MOD || code == UMOD)
	*total += COSTS_N_INSNS (2);
      return false;

    case CTZ:
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
	{
	  *total = 0;
	  return false;
	}
      /* FALLTHRU */

    case AND:
    case CLZ:
    case IOR:
    case XOR:
    case ZERO_EXTRACT:
      *total = COSTS_N_INSNS (1);
      return false;

    case ASHIFT:
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
      if (GET_CODE (XEXP (x, 0)) == MEM)
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
	*total = 0;
      else
	*total = rs6000_cost->fp;
      return false;

    case UNSPEC:
      switch (XINT (x, 1))
	{
	case UNSPEC_FRSP:
	  *total = rs6000_cost->fp;
	  return true;

	default:
	  break;
	}
      break;

    case CALL:
    case IF_THEN_ELSE:
      if (!speed)
	{
	  *total = COSTS_N_INSNS (1);
	  return true;
	}
      else if (FLOAT_MODE_P (mode)
	       && TARGET_PPC_GFXOPT && TARGET_HARD_FLOAT && TARGET_FPRS)
	{
	  *total = rs6000_cost->fp;
	  return false;
	}
      break;

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
      if (outer_code == SET)
	{
	  if (XEXP (x, 1) == const0_rtx)
	    {
	      if (TARGET_ISEL && !TARGET_MFCRF)
		*total = COSTS_N_INSNS (8);
	      else
		*total = COSTS_N_INSNS (2);
	      return true;
	    }
	  else if (mode == Pmode)
	    {
	      *total = COSTS_N_INSNS (3);
	      return false;
	    }
	}
      /* FALLTHRU */

    case GT:
    case LT:
    case UNORDERED:
      if (outer_code == SET && (XEXP (x, 1) == const0_rtx))
	{
	  if (TARGET_ISEL && !TARGET_MFCRF)
	    *total = COSTS_N_INSNS (8);
	  else
	    *total = COSTS_N_INSNS (2);
	  return true;
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
rs6000_debug_rtx_costs (rtx x, int code, int outer_code, int opno, int *total,
			bool speed)
{
  bool ret = rs6000_rtx_costs (x, code, outer_code, opno, total, speed);

  fprintf (stderr,
	   "\nrs6000_rtx_costs, return = %s, code = %s, outer_code = %s, "
	   "opno = %d, total = %d, speed = %s, x:\n",
	   ret ? "complete" : "scan inner",
	   GET_RTX_NAME (code),
	   GET_RTX_NAME (outer_code),
	   opno,
	   *total,
	   speed ? "true" : "false");

  debug_rtx (x);

  return ret;
}

/* Debug form of ADDRESS_COST that is selected if -mdebug=cost.  */

static int
rs6000_debug_address_cost (rtx x, enum machine_mode mode,
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
rs6000_register_move_cost (enum machine_mode mode,
			   reg_class_t from, reg_class_t to)
{
  int ret;

  if (TARGET_DEBUG_COST)
    dbg_cost_ctrl++;

  /*  Moves from/to GENERAL_REGS.  */
  if (reg_classes_intersect_p (to, GENERAL_REGS)
      || reg_classes_intersect_p (from, GENERAL_REGS))
    {
      reg_class_t rclass = from;

      if (! reg_classes_intersect_p (to, GENERAL_REGS))
	rclass = to;

      if (rclass == FLOAT_REGS || rclass == ALTIVEC_REGS || rclass == VSX_REGS)
	ret = (rs6000_memory_move_cost (mode, rclass, false)
	       + rs6000_memory_move_cost (mode, GENERAL_REGS, false));

      /* It's more expensive to move CR_REGS than CR0_REGS because of the
	 shift.  */
      else if (rclass == CR_REGS)
	ret = 4;

      /* For those processors that have slow LR/CTR moves, make them more
         expensive than memory in order to bias spills to memory .*/
      else if ((rs6000_cpu == PROCESSOR_POWER6
		|| rs6000_cpu == PROCESSOR_POWER7
		|| rs6000_cpu == PROCESSOR_POWER8)
	       && reg_classes_intersect_p (rclass, LINK_OR_CTR_REGS))
        ret = 6 * hard_regno_nregs[0][mode];

      else
	/* A move will cost one instruction per GPR moved.  */
	ret = 2 * hard_regno_nregs[0][mode];
    }

  /* If we have VSX, we can easily move between FPR or Altivec registers.  */
  else if (VECTOR_MEM_VSX_P (mode)
	   && reg_classes_intersect_p (to, VSX_REGS)
	   && reg_classes_intersect_p (from, VSX_REGS))
    ret = 2 * hard_regno_nregs[32][mode];

  /* Moving between two similar registers is just one instruction.  */
  else if (reg_classes_intersect_p (to, from))
    ret = (mode == TFmode || mode == TDmode) ? 4 : 2;

  /* Everything else has to go through GENERAL_REGS.  */
  else
    ret = (rs6000_register_move_cost (mode, GENERAL_REGS, to)
	   + rs6000_register_move_cost (mode, from, GENERAL_REGS));

  if (TARGET_DEBUG_COST)
    {
      if (dbg_cost_ctrl == 1)
	fprintf (stderr,
		 "rs6000_register_move_cost:, ret=%d, mode=%s, from=%s, to=%s\n",
		 ret, GET_MODE_NAME (mode), reg_class_names[from],
		 reg_class_names[to]);
      dbg_cost_ctrl--;
    }

  return ret;
}

/* A C expressions returning the cost of moving data of MODE from a register to
   or from memory.  */

static int
rs6000_memory_move_cost (enum machine_mode mode, reg_class_t rclass,
			 bool in ATTRIBUTE_UNUSED)
{
  int ret;

  if (TARGET_DEBUG_COST)
    dbg_cost_ctrl++;

  if (reg_classes_intersect_p (rclass, GENERAL_REGS))
    ret = 4 * hard_regno_nregs[0][mode];
  else if ((reg_classes_intersect_p (rclass, FLOAT_REGS)
	    || reg_classes_intersect_p (rclass, VSX_REGS)))
    ret = 4 * hard_regno_nregs[32][mode];
  else if (reg_classes_intersect_p (rclass, ALTIVEC_REGS))
    ret = 4 * hard_regno_nregs[FIRST_ALTIVEC_REGNO][mode];
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

/* Returns a code for a target-specific builtin that implements
   reciprocal of the function, or NULL_TREE if not available.  */

static tree
rs6000_builtin_reciprocal (unsigned int fn, bool md_fn,
			   bool sqrt ATTRIBUTE_UNUSED)
{
  if (optimize_insn_for_size_p ())
    return NULL_TREE;

  if (md_fn)
    switch (fn)
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

  else
    switch (fn)
      {
      case BUILT_IN_SQRT:
	if (!RS6000_RECIP_AUTO_RSQRTE_P (DFmode))
	  return NULL_TREE;

	return rs6000_builtin_decls[RS6000_BUILTIN_RSQRT];

      case BUILT_IN_SQRTF:
	if (!RS6000_RECIP_AUTO_RSQRTE_P (SFmode))
	  return NULL_TREE;

	return rs6000_builtin_decls[RS6000_BUILTIN_RSQRTF];

      default:
	return NULL_TREE;
      }
}

/* Load up a constant.  If the mode is a vector mode, splat the value across
   all of the vector elements.  */

static rtx
rs6000_load_constant_and_splat (enum machine_mode mode, REAL_VALUE_TYPE dconst)
{
  rtx reg;

  if (mode == SFmode || mode == DFmode)
    {
      rtx d = CONST_DOUBLE_FROM_REAL_VALUE (dconst, mode);
      reg = force_reg (mode, d);
    }
  else if (mode == V4SFmode)
    {
      rtx d = CONST_DOUBLE_FROM_REAL_VALUE (dconst, SFmode);
      rtvec v = gen_rtvec (4, d, d, d, d);
      reg = gen_reg_rtx (mode);
      rs6000_expand_vector_init (reg, gen_rtx_PARALLEL (mode, v));
    }
  else if (mode == V2DFmode)
    {
      rtx d = CONST_DOUBLE_FROM_REAL_VALUE (dconst, DFmode);
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
  enum machine_mode mode = GET_MODE (target);
  rtx dst;

  dst = expand_ternary_op (mode, fma_optab, m1, m2, a, target, 0);
  gcc_assert (dst != NULL);

  if (dst != target)
    emit_move_insn (target, dst);
}

/* Generate a FMSUB instruction: dst = fma(m1, m2, -a).  */

static void
rs6000_emit_msub (rtx target, rtx m1, rtx m2, rtx a)
{
  enum machine_mode mode = GET_MODE (target);
  rtx dst;

  /* Altivec does not support fms directly;
     generate in terms of fma in that case.  */
  if (optab_handler (fms_optab, mode) != CODE_FOR_nothing)
    dst = expand_ternary_op (mode, fms_optab, m1, m2, a, target, 0);
  else
    {
      a = expand_unop (mode, neg_optab, a, NULL_RTX, 0);
      dst = expand_ternary_op (mode, fma_optab, m1, m2, a, target, 0);
    }
  gcc_assert (dst != NULL);

  if (dst != target)
    emit_move_insn (target, dst);
}
    
/* Generate a FNMSUB instruction: dst = -fma(m1, m2, -a).  */

static void
rs6000_emit_nmsub (rtx dst, rtx m1, rtx m2, rtx a)
{
  enum machine_mode mode = GET_MODE (dst);
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
  emit_insn (gen_rtx_SET (VOIDmode, dst, r));
}

/* Newton-Raphson approximation of floating point divide DST = N/D.  If NOTE_P,
   add a reg_note saying that this was a division.  Support both scalar and
   vector divide.  Assumes no trapping math and finite arguments.  */

void
rs6000_emit_swdiv (rtx dst, rtx n, rtx d, bool note_p)
{
  enum machine_mode mode = GET_MODE (dst);
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
  emit_insn (gen_rtx_SET (VOIDmode, x0,
			  gen_rtx_UNSPEC (mode, gen_rtvec (1, d),
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

/* Newton-Raphson approximation of single/double-precision floating point
   rsqrt.  Assumes no trapping math and finite arguments.  */

void
rs6000_emit_swrsqrt (rtx dst, rtx src)
{
  enum machine_mode mode = GET_MODE (src);
  rtx x0 = gen_reg_rtx (mode);
  rtx y = gen_reg_rtx (mode);

  /* Low precision estimates guarantee 5 bits of accuracy.  High
     precision estimates guarantee 14 bits of accuracy.  SFmode
     requires 23 bits of accuracy.  DFmode requires 52 bits of
     accuracy.  Each pass at least doubles the accuracy, leading
     to the following.  */
  int passes = (TARGET_RECIP_PRECISION) ? 1 : 3;
  if (mode == DFmode || mode == V2DFmode)
    passes++;

  REAL_VALUE_TYPE dconst3_2;
  int i;
  rtx halfthree;
  enum insn_code code = optab_handler (smul_optab, mode);
  insn_gen_fn gen_mul = GEN_FCN (code);

  gcc_assert (code != CODE_FOR_nothing);

  /* Load up the constant 1.5 either as a scalar, or as a vector.  */
  real_from_integer (&dconst3_2, VOIDmode, 3, 0, 0);
  SET_REAL_EXP (&dconst3_2, REAL_EXP (&dconst3_2) - 1);

  halfthree = rs6000_load_constant_and_splat (mode, dconst3_2);

  /* x0 = rsqrt estimate */
  emit_insn (gen_rtx_SET (VOIDmode, x0,
			  gen_rtx_UNSPEC (mode, gen_rtvec (1, src),
					  UNSPEC_RSQRT)));

  /* y = 0.5 * src = 1.5 * src - src -> fewer constants */
  rs6000_emit_msub (y, src, halfthree, src);

  for (i = 0; i < passes; i++)
    {
      rtx x1 = gen_reg_rtx (mode);
      rtx u = gen_reg_rtx (mode);
      rtx v = gen_reg_rtx (mode);

      /* x1 = x0 * (1.5 - y * (x0 * x0)) */
      emit_insn (gen_mul (u, x0, x0));
      rs6000_emit_nmsub (v, y, u, halfthree);
      emit_insn (gen_mul (x1, x0, v));
      x0 = x1;
    }

  emit_move_insn (dst, x0);
  return;
}

/* Emit popcount intrinsic on TARGET_POPCNTB (Power5) and TARGET_POPCNTD
   (Power7) targets.  DST is the target, and SRC is the argument operand.  */

void
rs6000_emit_popcount (rtx dst, rtx src)
{
  enum machine_mode mode = GET_MODE (dst);
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
  enum machine_mode mode = GET_MODE (dst);
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

void
altivec_expand_vec_perm_const_le (rtx operands[4])
{
  unsigned int i;
  rtx perm[16];
  rtx constv, unspec;
  rtx target = operands[0];
  rtx op0 = operands[1];
  rtx op1 = operands[2];
  rtx sel = operands[3];

  /* Unpack and adjust the constant selector.  */
  for (i = 0; i < 16; ++i)
    {
      rtx e = XVECEXP (sel, 0, i);
      unsigned int elt = 31 - (INTVAL (e) & 31);
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
   generate a vector splat/subtract to do the adjustment.  */

void
altivec_expand_vec_perm_le (rtx operands[4])
{
  rtx splat, unspec;
  rtx target = operands[0];
  rtx op0 = operands[1];
  rtx op1 = operands[2];
  rtx sel = operands[3];
  rtx tmp = target;
  rtx splatreg = gen_reg_rtx (V16QImode);
  enum machine_mode mode = GET_MODE (target);

  /* Get everything in regs so the pattern matches.  */
  if (!REG_P (op0))
    op0 = force_reg (mode, op0);
  if (!REG_P (op1))
    op1 = force_reg (mode, op1);
  if (!REG_P (sel))
    sel = force_reg (V16QImode, sel);
  if (!REG_P (target))
    tmp = gen_reg_rtx (mode);

  /* SEL = splat(31) - SEL.  */
  /* We want to subtract from 31, but we can't vspltisb 31 since
     it's out of range.  -1 works as well because only the low-order
     five bits of the permute control vector elements are used.  */
  splat = gen_rtx_VEC_DUPLICATE (V16QImode,
				 gen_rtx_CONST_INT (QImode, -1));
  emit_move_insn (splatreg, splat);
  sel = gen_rtx_MINUS (V16QImode, splatreg, sel);
  emit_move_insn (splatreg, sel);

  /* Permute with operands reversed and adjusted selector.  */
  unspec = gen_rtx_UNSPEC (mode, gen_rtvec (3, op1, op0, splatreg), UNSPEC_VPERM);

  /* Copy into target, possibly by way of a register.  */
  if (!REG_P (target))
    {
      emit_move_insn (tmp, unspec);
      unspec = tmp;
    }

  emit_move_insn (target, unspec);
}

/* Expand an Altivec constant permutation.  Return true if we match
   an efficient implementation; false to fall back to VPERM.  */

bool
altivec_expand_vec_perm_const (rtx operands[4])
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
    { OPTION_MASK_P8_VECTOR, CODE_FOR_p8_vmrgew,
      {  0,  1,  2,  3, 16, 17, 18, 19,  8,  9, 10, 11, 24, 25, 26, 27 } },
    { OPTION_MASK_P8_VECTOR, CODE_FOR_p8_vmrgow,
      {  4,  5,  6,  7, 20, 21, 22, 23, 12, 13, 14, 15, 28, 29, 30, 31 } }
  };

  unsigned int i, j, elt, which;
  unsigned char perm[16];
  rtx target, op0, op1, sel, x;
  bool one_vec;

  target = operands[0];
  op0 = operands[1];
  op1 = operands[2];
  sel = operands[3];

  /* Unpack the constant selector.  */
  for (i = which = 0; i < 16; ++i)
    {
      rtx e = XVECEXP (sel, 0, i);
      elt = INTVAL (e) & 31;
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
	  enum machine_mode omode = insn_data[icode].operand[0].mode;
	  enum machine_mode imode = insn_data[icode].operand[1].mode;

	  /* For little-endian, don't use vpkuwum and vpkuhum if the
	     underlying vector type is not V4SI and V8HI, respectively.
	     For example, using vpkuwum with a V8HI picks up the even
	     halfwords (BE numbering) when the even halfwords (LE
	     numbering) are what we need.  */
	  if (!BYTES_BIG_ENDIAN
	      && icode == CODE_FOR_altivec_vpkuwum_direct
	      && ((GET_CODE (op0) == REG
		   && GET_MODE (op0) != V4SImode)
		  || (GET_CODE (op0) == SUBREG
		      && GET_MODE (XEXP (op0, 0)) != V4SImode)))
	    continue;
	  if (!BYTES_BIG_ENDIAN
	      && icode == CODE_FOR_altivec_vpkuhum_direct
	      && ((GET_CODE (op0) == REG
		   && GET_MODE (op0) != V8HImode)
		  || (GET_CODE (op0) == SUBREG
		      && GET_MODE (XEXP (op0, 0)) != V8HImode)))
	    continue;

          /* For little-endian, the two input operands must be swapped
             (or swapped back) to ensure proper right-to-left numbering
             from 0 to 2N-1.  */
	  if (swapped ^ !BYTES_BIG_ENDIAN)
	    x = op0, op0 = op1, op1 = x;
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
      altivec_expand_vec_perm_const_le (operands);
      return true;
    }

  return false;
}

/* Expand a Paired Single, VSX Permute Doubleword, or SPE constant permutation.
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
      x = op0, op0 = op1, op1 = x;
    }
  /* If the second selector does not come from the second operand, fail.  */
  else if ((perm1 & 2) == 0)
    return false;

  /* Success! */
  if (target != NULL)
    {
      enum machine_mode vmode, dmode;
      rtvec v;

      vmode = GET_MODE (target);
      gcc_assert (GET_MODE_NUNITS (vmode) == 2);
      dmode = mode_for_vector (GET_MODE_INNER (vmode), 4);
      x = gen_rtx_VEC_CONCAT (dmode, op0, op1);
      v = gen_rtvec (2, GEN_INT (perm0), GEN_INT (perm1));
      x = gen_rtx_VEC_SELECT (vmode, x, gen_rtx_PARALLEL (VOIDmode, v));
      emit_insn (gen_rtx_SET (VOIDmode, target, x));
    }
  return true;
}

bool
rs6000_expand_vec_perm_const (rtx operands[4])
{
  rtx target, op0, op1, sel;
  unsigned char perm0, perm1;

  target = operands[0];
  op0 = operands[1];
  op1 = operands[2];
  sel = operands[3];

  /* Unpack the constant selector.  */
  perm0 = INTVAL (XVECEXP (sel, 0, 0)) & 3;
  perm1 = INTVAL (XVECEXP (sel, 0, 1)) & 3;

  return rs6000_expand_vec_perm_const_1 (target, op0, op1, perm0, perm1);
}

/* Test whether a constant permutation is supported.  */

static bool
rs6000_vectorize_vec_perm_const_ok (enum machine_mode vmode,
				    const unsigned char *sel)
{
  /* AltiVec (and thus VSX) can handle arbitrary permutations.  */
  if (TARGET_ALTIVEC)
    return true;

  /* Check for ps_merge* or evmerge* insns.  */
  if ((TARGET_PAIRED_FLOAT && vmode == V2SFmode)
      || (TARGET_SPE && vmode == V2SImode))
    {
      rtx op0 = gen_raw_REG (vmode, LAST_VIRTUAL_REGISTER + 1);
      rtx op1 = gen_raw_REG (vmode, LAST_VIRTUAL_REGISTER + 2);
      return rs6000_expand_vec_perm_const_1 (NULL, op0, op1, sel[0], sel[1]);
    }

  return false;
}

/* A subroutine for rs6000_expand_extract_even & rs6000_expand_interleave.  */

static void
rs6000_do_expand_vec_perm (rtx target, rtx op0, rtx op1,
			   enum machine_mode vmode, unsigned nelt, rtx perm[])
{
  enum machine_mode imode;
  rtx x;

  imode = vmode;
  if (GET_MODE_CLASS (vmode) != MODE_VECTOR_INT)
    {
      imode = GET_MODE_INNER (vmode);
      imode = mode_for_size (GET_MODE_BITSIZE (imode), MODE_INT, 0);
      imode = mode_for_vector (imode, nelt);
    }

  x = gen_rtx_CONST_VECTOR (imode, gen_rtvec_v (nelt, perm));
  x = expand_vec_perm (vmode, op0, op1, x, target);
  if (x != target)
    emit_move_insn (target, x);
}

/* Expand an extract even operation.  */

void
rs6000_expand_extract_even (rtx target, rtx op0, rtx op1)
{
  enum machine_mode vmode = GET_MODE (target);
  unsigned i, nelt = GET_MODE_NUNITS (vmode);
  rtx perm[16];

  for (i = 0; i < nelt; i++)
    perm[i] = GEN_INT (i * 2);

  rs6000_do_expand_vec_perm (target, op0, op1, vmode, nelt, perm);
}

/* Expand a vector interleave operation.  */

void
rs6000_expand_interleave (rtx target, rtx op0, rtx op1, bool highp)
{
  enum machine_mode vmode = GET_MODE (target);
  unsigned i, high, nelt = GET_MODE_NUNITS (vmode);
  rtx perm[16];

  high = (highp ? 0 : nelt / 2);
  for (i = 0; i < nelt / 2; i++)
    {
      perm[i * 2] = GEN_INT (i + high);
      perm[i * 2 + 1] = GEN_INT (i + nelt + high);
    }

  rs6000_do_expand_vec_perm (target, op0, op1, vmode, nelt, perm);
}

/* Return an RTX representing where to find the function value of a
   function returning MODE.  */
static rtx
rs6000_complex_function_value (enum machine_mode mode)
{
  unsigned int regno;
  rtx r1, r2;
  enum machine_mode inner = GET_MODE_INNER (mode);
  unsigned int inner_bytes = GET_MODE_SIZE (inner);

  if (FLOAT_MODE_P (mode) && TARGET_HARD_FLOAT && TARGET_FPRS)
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

/* Target hook for TARGET_FUNCTION_VALUE.

   On the SPE, both FPs and vectors are returned in r3.

   On RS/6000 an integer value is in r3 and a floating-point value is in
   fp1, unless -msoft-float.  */

static rtx
rs6000_function_value (const_tree valtype,
		       const_tree fn_decl_or_type ATTRIBUTE_UNUSED,
		       bool outgoing ATTRIBUTE_UNUSED)
{
  enum machine_mode mode;
  unsigned int regno;
  enum machine_mode elt_mode;
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

  /* The ELFv2 ABI returns homogeneous VFP aggregates in registers.  */
  if (rs6000_discover_homogeneous_aggregate (TYPE_MODE (valtype), valtype,
					     &elt_mode, &n_elts))
    {
      int first_reg, n_regs, i;
      rtx par;

      if (SCALAR_FLOAT_MODE_P (elt_mode))
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

      par = gen_rtx_PARALLEL (TYPE_MODE (valtype), rtvec_alloc (n_elts));
      for (i = 0; i < n_elts; i++)
	{
	  rtx r = gen_rtx_REG (elt_mode, first_reg + i * n_regs);
	  rtx off = GEN_INT (i * GET_MODE_SIZE (elt_mode));
	  XVECEXP (par, 0, i) = gen_rtx_EXPR_LIST (VOIDmode, r, off);
	}

      return par;
    }

  if (TARGET_32BIT && TARGET_POWERPC64 && TYPE_MODE (valtype) == DImode)
    {
      /* Long long return value need be split in -mpowerpc64, 32bit ABI.  */
      return gen_rtx_PARALLEL (DImode,
	gen_rtvec (2,
		   gen_rtx_EXPR_LIST (VOIDmode,
				      gen_rtx_REG (SImode, GP_ARG_RETURN),
				      const0_rtx),
		   gen_rtx_EXPR_LIST (VOIDmode,
				      gen_rtx_REG (SImode,
						   GP_ARG_RETURN + 1),
				      GEN_INT (4))));
    }
  if (TARGET_32BIT && TARGET_POWERPC64 && TYPE_MODE (valtype) == DCmode)
    {
      return gen_rtx_PARALLEL (DCmode,
	gen_rtvec (4,
		   gen_rtx_EXPR_LIST (VOIDmode,
				      gen_rtx_REG (SImode, GP_ARG_RETURN),
				      const0_rtx),
		   gen_rtx_EXPR_LIST (VOIDmode,
				      gen_rtx_REG (SImode,
						   GP_ARG_RETURN + 1),
				      GEN_INT (4)),
		   gen_rtx_EXPR_LIST (VOIDmode,
				      gen_rtx_REG (SImode,
						   GP_ARG_RETURN + 2),
				      GEN_INT (8)),
		   gen_rtx_EXPR_LIST (VOIDmode,
				      gen_rtx_REG (SImode,
						   GP_ARG_RETURN + 3),
				      GEN_INT (12))));
    }

  mode = TYPE_MODE (valtype);
  if ((INTEGRAL_TYPE_P (valtype) && GET_MODE_BITSIZE (mode) < BITS_PER_WORD)
      || POINTER_TYPE_P (valtype))
    mode = TARGET_32BIT ? SImode : DImode;

  if (DECIMAL_FLOAT_MODE_P (mode) && TARGET_HARD_FLOAT && TARGET_FPRS)
    /* _Decimal128 must use an even/odd register pair.  */
    regno = (mode == TDmode) ? FP_ARG_RETURN + 1 : FP_ARG_RETURN;
  else if (SCALAR_FLOAT_TYPE_P (valtype) && TARGET_HARD_FLOAT && TARGET_FPRS
	   && ((TARGET_SINGLE_FLOAT && (mode == SFmode)) || TARGET_DOUBLE_FLOAT))
    regno = FP_ARG_RETURN;
  else if (TREE_CODE (valtype) == COMPLEX_TYPE
	   && targetm.calls.split_complex_arg)
    return rs6000_complex_function_value (mode);
  /* VSX is a superset of Altivec and adds V2DImode/V2DFmode.  Since the same
     return register is used in both cases, and we won't see V2DImode/V2DFmode
     for pure altivec, combine the two cases.  */
  else if (TREE_CODE (valtype) == VECTOR_TYPE
	   && TARGET_ALTIVEC && TARGET_ALTIVEC_ABI
	   && ALTIVEC_OR_VSX_VECTOR_MODE (mode))
    regno = ALTIVEC_ARG_RETURN;
  else if (TARGET_E500_DOUBLE && TARGET_HARD_FLOAT
	   && (mode == DFmode || mode == DCmode
	       || mode == TFmode || mode == TCmode))
    return spe_build_register_parallel (mode, GP_ARG_RETURN);
  else
    regno = GP_ARG_RETURN;

  return gen_rtx_REG (mode, regno);
}

/* Define how to find the value returned by a library function
   assuming the value has mode MODE.  */
rtx
rs6000_libcall_value (enum machine_mode mode)
{
  unsigned int regno;

  if (TARGET_32BIT && TARGET_POWERPC64 && mode == DImode)
    {
      /* Long long return value need be split in -mpowerpc64, 32bit ABI.  */
      return gen_rtx_PARALLEL (DImode,
	gen_rtvec (2,
		   gen_rtx_EXPR_LIST (VOIDmode,
				      gen_rtx_REG (SImode, GP_ARG_RETURN),
				      const0_rtx),
		   gen_rtx_EXPR_LIST (VOIDmode,
				      gen_rtx_REG (SImode,
						   GP_ARG_RETURN + 1),
				      GEN_INT (4))));
    }

  if (DECIMAL_FLOAT_MODE_P (mode) && TARGET_HARD_FLOAT && TARGET_FPRS)
    /* _Decimal128 must use an even/odd register pair.  */
    regno = (mode == TDmode) ? FP_ARG_RETURN + 1 : FP_ARG_RETURN;
  else if (SCALAR_FLOAT_MODE_P (mode)
	   && TARGET_HARD_FLOAT && TARGET_FPRS
           && ((TARGET_SINGLE_FLOAT && mode == SFmode) || TARGET_DOUBLE_FLOAT))
    regno = FP_ARG_RETURN;
  /* VSX is a superset of Altivec and adds V2DImode/V2DFmode.  Since the same
     return register is used in both cases, and we won't see V2DImode/V2DFmode
     for pure altivec, combine the two cases.  */
  else if (ALTIVEC_OR_VSX_VECTOR_MODE (mode)
	   && TARGET_ALTIVEC && TARGET_ALTIVEC_ABI)
    regno = ALTIVEC_ARG_RETURN;
  else if (COMPLEX_MODE_P (mode) && targetm.calls.split_complex_arg)
    return rs6000_complex_function_value (mode);
  else if (TARGET_E500_DOUBLE && TARGET_HARD_FLOAT
	   && (mode == DFmode || mode == DCmode
	       || mode == TFmode || mode == TCmode))
    return spe_build_register_parallel (mode, GP_ARG_RETURN);
  else
    regno = GP_ARG_RETURN;

  return gen_rtx_REG (mode, regno);
}


/* Return true if we use LRA instead of reload pass.  */
static bool
rs6000_lra_p (void)
{
  return rs6000_lra_flag;
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
            ? ! TARGET_MINIMAL_TOC || TARGET_NO_TOC || get_pool_size () == 0
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

static rtx
rs6000_dwarf_register_span (rtx reg)
{
  rtx parts[8];
  int i, words;
  unsigned regno = REGNO (reg);
  enum machine_mode mode = GET_MODE (reg);

  if (TARGET_SPE
      && regno < 32
      && (SPE_VECTOR_MODE (GET_MODE (reg))
	  || (TARGET_E500_DOUBLE && FLOAT_MODE_P (mode)
	      && mode != SFmode && mode != SDmode && mode != SCmode)))
    ;
  else
    return NULL_RTX;

  regno = REGNO (reg);

  /* The duality of the SPE register size wreaks all kinds of havoc.
     This is a way of distinguishing r0 in 32-bits from r0 in
     64-bits.  */
  words = (GET_MODE_SIZE (mode) + UNITS_PER_FP_WORD - 1) / UNITS_PER_FP_WORD;
  gcc_assert (words <= 4);
  for (i = 0; i < words; i++, regno++)
    {
      if (BYTES_BIG_ENDIAN)
	{
	  parts[2 * i] = gen_rtx_REG (SImode, regno + 1200);
	  parts[2 * i + 1] = gen_rtx_REG (SImode, regno);
	}
      else
	{
	  parts[2 * i] = gen_rtx_REG (SImode, regno);
	  parts[2 * i + 1] = gen_rtx_REG (SImode, regno + 1200);
	}
    }

  return gen_rtx_PARALLEL (VOIDmode, gen_rtvec_v (words * 2, parts));
}

/* Fill in sizes for SPE register high parts in table used by unwinder.  */

static void
rs6000_init_dwarf_reg_sizes_extra (tree address)
{
  if (TARGET_SPE)
    {
      int i;
      enum machine_mode mode = TYPE_MODE (char_type_node);
      rtx addr = expand_expr (address, NULL_RTX, VOIDmode, EXPAND_NORMAL);
      rtx mem = gen_rtx_MEM (BLKmode, addr);
      rtx value = gen_int_mode (4, mode);

      for (i = 1201; i < 1232; i++)
	{
	  int column = DWARF_REG_TO_UNWIND_COLUMN (i);
	  HOST_WIDE_INT offset
	    = DWARF_FRAME_REGNUM (column) * GET_MODE_SIZE (mode);

	  emit_move_insn (adjust_address (mem, mode, offset), value);
	}
    }

  if (TARGET_MACHO && ! TARGET_ALTIVEC)
    {
      int i;
      enum machine_mode mode = TYPE_MODE (char_type_node);
      rtx addr = expand_expr (address, NULL_RTX, VOIDmode, EXPAND_NORMAL);
      rtx mem = gen_rtx_MEM (BLKmode, addr);
      rtx value = gen_int_mode (16, mode);

      /* On Darwin, libgcc may be built to run on both G3 and G4/5.
	 The unwinder still needs to know the size of Altivec registers.  */

      for (i = FIRST_ALTIVEC_REGNO; i < LAST_ALTIVEC_REGNO+1; i++)
	{
	  int column = DWARF_REG_TO_UNWIND_COLUMN (i);
	  HOST_WIDE_INT offset
	    = DWARF_FRAME_REGNUM (column) * GET_MODE_SIZE (mode);

	  emit_move_insn (adjust_address (mem, mode, offset), value);
	}
    }
}

/* Map internal gcc register numbers to DWARF2 register numbers.  */

unsigned int
rs6000_dbx_register_number (unsigned int regno)
{
  if (regno <= 63 || write_symbols != DWARF2_DEBUG)
    return regno;
  if (regno == LR_REGNO)
    return 108;
  if (regno == CTR_REGNO)
    return 109;
  if (CR_REGNO_P (regno))
    return regno - CR0_REGNO + 86;
  if (regno == CA_REGNO)
    return 101;  /* XER */
  if (ALTIVEC_REGNO_P (regno))
    return regno - FIRST_ALTIVEC_REGNO + 1124;
  if (regno == VRSAVE_REGNO)
    return 356;
  if (regno == VSCR_REGNO)
    return 67;
  if (regno == SPE_ACC_REGNO)
    return 99;
  if (regno == SPEFSCR_REGNO)
    return 612;
  /* SPE high reg number.  We get these values of regno from
     rs6000_dwarf_register_span.  */
  gcc_assert (regno >= 1200 && regno < 1232);
  return regno;
}

/* target hook eh_return_filter_mode */
static enum machine_mode
rs6000_eh_return_filter_mode (void)
{
  return TARGET_32BIT ? SImode : word_mode;
}

/* Target hook for scalar_mode_supported_p.  */
static bool
rs6000_scalar_mode_supported_p (enum machine_mode mode)
{
  if (DECIMAL_FLOAT_MODE_P (mode))
    return default_decimal_float_supported_p ();
  else
    return default_scalar_mode_supported_p (mode);
}

/* Target hook for vector_mode_supported_p.  */
static bool
rs6000_vector_mode_supported_p (enum machine_mode mode)
{

  if (TARGET_PAIRED_FLOAT && PAIRED_VECTOR_MODE (mode))
    return true;

  if (TARGET_SPE && SPE_VECTOR_MODE (mode))
    return true;

  else if (VECTOR_MEM_ALTIVEC_OR_VSX_P (mode))
    return true;

  else
    return false;
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

void
rs6000_final_prescan_insn (rtx insn, rtx *operand ATTRIBUTE_UNUSED,
			   int num_operands ATTRIBUTE_UNUSED)
{
  if (rs6000_warn_cell_microcode)
    {
      const char *temp;
      int insn_code_number = recog_memoized (insn);
      location_t location = INSN_LOCATION (insn);

      /* Punt on insns we cannot recognize.  */
      if (insn_code_number < 0)
	return;

      temp = get_insn_template (insn_code_number, insn);

      if (get_attr_cell_micro (insn) == CELL_MICRO_ALWAYS)
	warning_at (location, OPT_mwarn_cell_microcode,
		    "emitting microcode insn %s\t[%s] #%d",
		    temp, insn_data[INSN_CODE (insn)].name, INSN_UID (insn)); 
      else if (get_attr_cell_micro (insn) == CELL_MICRO_CONDITIONAL)
	warning_at (location, OPT_mwarn_cell_microcode,
		    "emitting conditional microcode insn %s\t[%s] #%d",
		    temp, insn_data[INSN_CODE (insn)].name, INSN_UID (insn));
    }
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
   64/32-bit, endianess, hard/soft floating point, etc. that would have
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
  { "fprnd",			OPTION_MASK_FPRND,		false, true  },
  { "hard-dfp",			OPTION_MASK_DFP,		false, true  },
  { "htm",			OPTION_MASK_HTM,		false, true  },
  { "isel",			OPTION_MASK_ISEL,		false, true  },
  { "mfcrf",			OPTION_MASK_MFCRF,		false, true  },
  { "mfpgpr",			OPTION_MASK_MFPGPR,		false, true  },
  { "mulhw",			OPTION_MASK_MULHW,		false, true  },
  { "multiple",			OPTION_MASK_MULTIPLE,		false, true  },
  { "popcntb",			OPTION_MASK_POPCNTB,		false, true  },
  { "popcntd",			OPTION_MASK_POPCNTD,		false, true  },
  { "power8-fusion",		OPTION_MASK_P8_FUSION,		false, true  },
  { "power8-fusion-sign",	OPTION_MASK_P8_FUSION_SIGN,	false, true  },
  { "power8-vector",		OPTION_MASK_P8_VECTOR,		false, true  },
  { "powerpc-gfxopt",		OPTION_MASK_PPC_GFXOPT,		false, true  },
  { "powerpc-gpopt",		OPTION_MASK_PPC_GPOPT,		false, true  },
  { "quad-memory",		OPTION_MASK_QUAD_MEMORY,	false, true  },
  { "quad-memory-atomic",	OPTION_MASK_QUAD_MEMORY_ATOMIC,	false, true  },
  { "recip-precision",		OPTION_MASK_RECIP_PRECISION,	false, true  },
  { "string",			OPTION_MASK_STRING,		false, true  },
  { "update",			OPTION_MASK_NO_UPDATE,		true , true  },
  { "upper-regs-df",		OPTION_MASK_UPPER_REGS_DF,	false, false },
  { "upper-regs-sf",		OPTION_MASK_UPPER_REGS_SF,	false, false },
  { "vsx",			OPTION_MASK_VSX,		false, true  },
  { "vsx-timode",		OPTION_MASK_VSX_TIMODE,		false, true  },
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
  { "string",			OPTION_MASK_STRING,		false, false },
};

/* Builtin mask mapping for printing the flags.  */
static struct rs6000_opt_mask const rs6000_builtin_mask_names[] =
{
  { "altivec",		 RS6000_BTM_ALTIVEC,	false, false },
  { "vsx",		 RS6000_BTM_VSX,	false, false },
  { "spe",		 RS6000_BTM_SPE,	false, false },
  { "paired",		 RS6000_BTM_PAIRED,	false, false },
  { "fre",		 RS6000_BTM_FRE,	false, false },
  { "fres",		 RS6000_BTM_FRES,	false, false },
  { "frsqrte",		 RS6000_BTM_FRSQRTE,	false, false },
  { "frsqrtes",		 RS6000_BTM_FRSQRTES,	false, false },
  { "popcntd",		 RS6000_BTM_POPCNTD,	false, false },
  { "cell",		 RS6000_BTM_CELL,	false, false },
  { "power8-vector",	 RS6000_BTM_P8_VECTOR,	false, false },
  { "crypto",		 RS6000_BTM_CRYPTO,	false, false },
  { "htm",		 RS6000_BTM_HTM,	false, false },
};

/* Option variables that we want to support inside attribute((target)) and
   #pragma GCC target operations.  */

struct rs6000_opt_var {
  const char *name;		/* option name */
  size_t global_offset;		/* offset of the option in global_options.  */
  size_t target_offset;		/* offset of the option in target optiosn.  */
};

static struct rs6000_opt_var const rs6000_opt_vars[] =
{
  { "friz",
    offsetof (struct gcc_options, x_TARGET_FRIZ),
    offsetof (struct cl_target_option, x_TARGET_FRIZ), },
  { "avoid-indexed-addresses",
    offsetof (struct gcc_options, x_TARGET_AVOID_XFORM),
    offsetof (struct cl_target_option, x_TARGET_AVOID_XFORM) },
  { "paired",
    offsetof (struct gcc_options, x_rs6000_paired_float),
    offsetof (struct cl_target_option, x_rs6000_paired_float), },
  { "longcall",
    offsetof (struct gcc_options, x_rs6000_default_long_calls),
    offsetof (struct cl_target_option, x_rs6000_default_long_calls), },
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
			   altivec.  */
			if (mask == OPTION_MASK_VSX && !invert)
			  mask |= OPTION_MASK_ALTIVEC;

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
		error ("invalid cpu \"%s\" for %s\"%s\"%s", cpu_opt, eprefix,
		       q, esuffix);
	      else if (not_valid_p)
		error ("%s\"%s\"%s is not allowed", eprefix, q, esuffix);
	      else
		error ("%s\"%s\"%s is invalid", eprefix, q, esuffix);
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
    gcc_unreachable ();

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
  tree old_optimize = build_optimization_node (&global_options);
  tree new_target, new_optimize;
  tree func_optimize = DECL_FUNCTION_SPECIFIC_OPTIMIZATION (fndecl);

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

/* Establish appropriate back-end context for processing the function
   FNDECL.  The argument might be NULL to indicate processing at top
   level, outside of any function scope.  */
static void
rs6000_set_current_function (tree fndecl)
{
  tree old_tree = (rs6000_previous_fndecl
		   ? DECL_FUNCTION_SPECIFIC_TARGET (rs6000_previous_fndecl)
		   : NULL_TREE);

  tree new_tree = (fndecl
		   ? DECL_FUNCTION_SPECIFIC_TARGET (fndecl)
		   : NULL_TREE);

  if (TARGET_DEBUG_TARGET)
    {
      bool print_final = false;
      fprintf (stderr, "\n==================== rs6000_set_current_function");

      if (fndecl)
	fprintf (stderr, ", fndecl %s (%p)",
		 (DECL_NAME (fndecl)
		  ? IDENTIFIER_POINTER (DECL_NAME (fndecl))
		  : "<unknown>"), (void *)fndecl);

      if (rs6000_previous_fndecl)
	fprintf (stderr, ", prev_fndecl (%p)", (void *)rs6000_previous_fndecl);

      fprintf (stderr, "\n");
      if (new_tree)
	{
	  fprintf (stderr, "\nnew fndecl target specific options:\n");
	  debug_tree (new_tree);
	  print_final = true;
	}

      if (old_tree)
	{
	  fprintf (stderr, "\nold fndecl target specific options:\n");
	  debug_tree (old_tree);
	  print_final = true;
	}

      if (print_final)
	fprintf (stderr, "--------------------\n");
    }

  /* Only change the context if the function changes.  This hook is called
     several times in the course of compiling a function, and we don't want to
     slow things down too much or call target_reinit when it isn't safe.  */
  if (fndecl && fndecl != rs6000_previous_fndecl)
    {
      rs6000_previous_fndecl = fndecl;
      if (old_tree == new_tree)
	;

      else if (new_tree)
	{
	  cl_target_option_restore (&global_options,
				    TREE_TARGET_OPTION (new_tree));
	  if (TREE_TARGET_GLOBALS (new_tree))
	    restore_target_globals (TREE_TARGET_GLOBALS (new_tree));
	  else
	    TREE_TARGET_GLOBALS (new_tree)
	      = save_target_globals_default_opts ();
	}

      else if (old_tree)
	{
	  new_tree = target_option_current_node;
	  cl_target_option_restore (&global_options,
				    TREE_TARGET_OPTION (new_tree));
	  if (TREE_TARGET_GLOBALS (new_tree))
	    restore_target_globals (TREE_TARGET_GLOBALS (new_tree));
	  else if (new_tree == target_option_default_node)
	    restore_target_globals (&default_target_globals);
	  else
	    TREE_TARGET_GLOBALS (new_tree)
	      = save_target_globals_default_opts ();
	}
    }
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
  size_t max_column = 76;
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
      if ((flags & opts[i].mask) != 0)
	{
	  const char *no_str = rs6000_opt_masks[i].invert ? "no-" : "";
	  size_t len = (strlen (comma)
			+ strlen (prefix)
			+ strlen (no_str)
			+ strlen (rs6000_opt_masks[i].name));

	  cur_column += len;
	  if (cur_column > max_column)
	    {
	      fprintf (stderr, ", \\\n%*s", (int)start_column, "");
	      cur_column = start_column + len;
	      comma = "";
	    }

	  fprintf (file, "%s%s%s%s", comma, prefix, no_str,
		   rs6000_opt_masks[i].name);
	  flags &= ~ opts[i].mask;
	  comma = ", ";
	}
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


/* Hook to determine if one function can safely inline another.  */

static bool
rs6000_can_inline_p (tree caller, tree callee)
{
  bool ret = false;
  tree caller_tree = DECL_FUNCTION_SPECIFIC_TARGET (caller);
  tree callee_tree = DECL_FUNCTION_SPECIFIC_TARGET (callee);

  /* If callee has no option attributes, then it is ok to inline.  */
  if (!callee_tree)
    ret = true;

  /* If caller has no option attributes, but callee does then it is not ok to
     inline.  */
  else if (!caller_tree)
    ret = false;

  else
    {
      struct cl_target_option *caller_opts = TREE_TARGET_OPTION (caller_tree);
      struct cl_target_option *callee_opts = TREE_TARGET_OPTION (callee_tree);

      /* Callee's options should a subset of the caller's, i.e. a vsx function
	 can inline an altivec function but a non-vsx function can't inline a
	 vsx function.  */
      if ((caller_opts->x_rs6000_isa_flags & callee_opts->x_rs6000_isa_flags)
	  == callee_opts->x_rs6000_isa_flags)
	ret = true;
    }

  if (TARGET_DEBUG_TARGET)
    fprintf (stderr, "rs6000_can_inline_p:, caller %s, callee %s, %s inline\n",
	     (DECL_NAME (caller)
	      ? IDENTIFIER_POINTER (DECL_NAME (caller))
	      : "<unknown>"),
	     (DECL_NAME (callee)
	      ? IDENTIFIER_POINTER (DECL_NAME (callee))
	      : "<unknown>"),
	     (ret ? "can" : "cannot"));

  return ret;
}

/* Allocate a stack temp and fixup the address so it meets the particular
   memory requirements (either offetable or REG+REG addressing).  */

rtx
rs6000_allocate_stack_temp (enum machine_mode mode,
			    bool offsettable_p,
			    bool reg_reg_p)
{
  rtx stack = assign_stack_temp (mode, GET_MODE_SIZE (mode));
  rtx addr = XEXP (stack, 0);
  int strict_p = (reload_in_progress || reload_completed);

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

/* Given a memory reference, if it is not a reg or reg+reg addressing, convert
   to such a form to deal with memory reference instructions like STFIWX that
   only take reg+reg addressing.  */

rtx
rs6000_address_for_fpconvert (rtx x)
{
  int strict_p = (reload_in_progress || reload_completed);
  rtx addr;

  gcc_assert (MEM_P (x));
  addr = XEXP (x, 0);
  if (! legitimate_indirect_address_p (addr, strict_p)
      && ! legitimate_indexed_address_p (addr, strict_p))
    {
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

      x = replace_equiv_address (x, copy_addr_to_reg (addr));
    }

  return x;
}

/* Given a memory reference, if it is not in the form for altivec memory
   reference instructions (i.e. reg or reg+reg addressing with AND of -16),
   convert to the altivec format.  */

rtx
rs6000_address_for_altivec (rtx x)
{
  gcc_assert (MEM_P (x));
  if (!altivec_indexed_or_indirect_operand (x, GET_MODE (x)))
    {
      rtx addr = XEXP (x, 0);
      int strict_p = (reload_in_progress || reload_completed);

      if (!legitimate_indexed_address_p (addr, strict_p)
	  && !legitimate_indirect_address_p (addr, strict_p))
	addr = copy_to_mode_reg (Pmode, addr);

      addr = gen_rtx_AND (Pmode, addr, GEN_INT (-16));
      x = change_address (x, GET_MODE (x), addr);
    }

  return x;
}

/* Implement TARGET_LEGITIMATE_CONSTANT_P.

   On the RS/6000, all integer constants are acceptable, most won't be valid
   for particular insns, though.  Only easy FP constants are acceptable.  */

static bool
rs6000_legitimate_constant_p (enum machine_mode mode, rtx x)
{
  if (TARGET_ELF && rs6000_tls_referenced_p (x))
    return false;

  return ((GET_CODE (x) != CONST_DOUBLE && GET_CODE (x) != CONST_VECTOR)
	  || GET_MODE (x) == VOIDmode
	  || (TARGET_POWERPC64 && mode == DImode)
	  || easy_fp_constant (x, mode)
	  || easy_vector_constant (x, mode));
}



/* Expand code to perform a call under the AIX or ELFv2 ABI.  */

void
rs6000_call_aix (rtx value, rtx func_desc, rtx flag, rtx cookie)
{
  rtx toc_reg = gen_rtx_REG (Pmode, TOC_REGNUM);
  rtx toc_load = NULL_RTX;
  rtx toc_restore = NULL_RTX;
  rtx func_addr;
  rtx abi_reg = NULL_RTX;
  rtx call[4];
  int n_call;
  rtx insn;

  /* Handle longcall attributes.  */
  if (INTVAL (cookie) & CALL_LONG)
    func_desc = rs6000_longcall_ref (func_desc);

  /* Handle indirect calls.  */
  if (GET_CODE (func_desc) != SYMBOL_REF
      || (DEFAULT_ABI == ABI_AIX && !SYMBOL_REF_FUNCTION_P (func_desc)))
    {
      /* Save the TOC into its reserved slot before the call,
	 and prepare to restore it after the call.  */
      rtx stack_ptr = gen_rtx_REG (Pmode, STACK_POINTER_REGNUM);
      rtx stack_toc_offset = GEN_INT (RS6000_TOC_SAVE_SLOT);
      rtx stack_toc_mem = gen_frame_mem (Pmode,
					 gen_rtx_PLUS (Pmode, stack_ptr,
						       stack_toc_offset));
      toc_restore = gen_rtx_SET (VOIDmode, toc_reg, stack_toc_mem);

      /* Can we optimize saving the TOC in the prologue or
	 do we need to do it at every call?  */
      if (TARGET_SAVE_TOC_INDIRECT && !cfun->calls_alloca)
	cfun->machine->save_toc_in_prologue = true;
      else
	{
	  MEM_VOLATILE_P (stack_toc_mem) = 1;
	  emit_move_insn (stack_toc_mem, toc_reg);
	}

      if (DEFAULT_ABI == ABI_ELFv2)
	{
	  /* A function pointer in the ELFv2 ABI is just a plain address, but
	     the ABI requires it to be loaded into r12 before the call.  */
	  func_addr = gen_rtx_REG (Pmode, 12);
	  emit_move_insn (func_addr, func_desc);
	  abi_reg = func_addr;
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
	  func_desc = force_reg (Pmode, func_desc);
	  func_addr = gen_reg_rtx (Pmode);
	  emit_move_insn (func_addr, gen_rtx_MEM (Pmode, func_desc));

	  /* Prepare to load the TOC of the called function.  Note that the
	     TOC load must happen immediately before the actual call so
	     that unwinding the TOC registers works correctly.  See the
	     comment in frob_update_context.  */
	  rtx func_toc_offset = GEN_INT (GET_MODE_SIZE (Pmode));
	  rtx func_toc_mem = gen_rtx_MEM (Pmode,
					  gen_rtx_PLUS (Pmode, func_desc,
							func_toc_offset));
	  toc_load = gen_rtx_USE (VOIDmode, func_toc_mem);

	  /* If we have a static chain, load it up.  */
	  if (TARGET_POINTERS_TO_NESTED_FUNCTIONS)
	    {
	      rtx sc_reg = gen_rtx_REG (Pmode, STATIC_CHAIN_REGNUM);
	      rtx func_sc_offset = GEN_INT (2 * GET_MODE_SIZE (Pmode));
	      rtx func_sc_mem = gen_rtx_MEM (Pmode,
					     gen_rtx_PLUS (Pmode, func_desc,
							   func_sc_offset));
	      emit_move_insn (sc_reg, func_sc_mem);
	      abi_reg = sc_reg;
	    }
	}
    }
  else
    {
      /* Direct calls use the TOC: for local calls, the callee will
	 assume the TOC register is set; for non-local calls, the
	 PLT stub needs the TOC register.  */
      abi_reg = toc_reg;
      func_addr = func_desc;
    }

  /* Create the call.  */
  call[0] = gen_rtx_CALL (VOIDmode, gen_rtx_MEM (SImode, func_addr), flag);
  if (value != NULL_RTX)
    call[0] = gen_rtx_SET (VOIDmode, value, call[0]);
  n_call = 1;

  if (toc_load)
    call[n_call++] = toc_load;
  if (toc_restore)
    call[n_call++] = toc_restore;

  call[n_call++] = gen_rtx_CLOBBER (VOIDmode, gen_rtx_REG (Pmode, LR_REGNO));

  insn = gen_rtx_PARALLEL (VOIDmode, gen_rtvec_v (n_call, call));
  insn = emit_call_insn (insn);

  /* Mention all registers defined by the ABI to hold information
     as uses in CALL_INSN_FUNCTION_USAGE.  */
  if (abi_reg)
    use_reg (&CALL_INSN_FUNCTION_USAGE (insn), abi_reg);
}

/* Expand code to perform a sibling call under the AIX or ELFv2 ABI.  */

void
rs6000_sibcall_aix (rtx value, rtx func_desc, rtx flag, rtx cookie)
{
  rtx call[2];
  rtx insn;

  gcc_assert (INTVAL (cookie) == 0);

  /* Create the call.  */
  call[0] = gen_rtx_CALL (VOIDmode, gen_rtx_MEM (SImode, func_desc), flag);
  if (value != NULL_RTX)
    call[0] = gen_rtx_SET (VOIDmode, value, call[0]);

  call[1] = simple_return_rtx;

  insn = gen_rtx_PARALLEL (VOIDmode, gen_rtvec_v (2, call));
  insn = emit_call_insn (insn);

  /* Note use of the TOC register.  */
  use_reg (&CALL_INSN_FUNCTION_USAGE (insn), gen_rtx_REG (Pmode, TOC_REGNUM));
  /* We need to also mark a use of the link register since the function we
     sibling-call to will use it to return to our caller.  */
  use_reg (&CALL_INSN_FUNCTION_USAGE (insn), gen_rtx_REG (Pmode, LR_REGNO));
}

/* Return whether we need to always update the saved TOC pointer when we update
   the stack pointer.  */

static bool
rs6000_save_toc_in_prologue_p (void)
{
  return (cfun && cfun->machine && cfun->machine->save_toc_in_prologue);
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
  if (USE_HIDDEN_LINKONCE)
    {
      DECL_COMDAT_GROUP (decl) = DECL_ASSEMBLER_NAME (decl);
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
      && get_pool_size () != 0)
    add_to_hard_reg_set (&set->set, Pmode, RS6000_PIC_OFFSET_TABLE_REGNUM);
}


/* Helper function for rs6000_split_logical to emit a logical instruction after
   spliting the operation to single GPR registers.

   DEST is the destination register.
   OP1 and OP2 are the input source registers.
   CODE is the base operation (AND, IOR, XOR, NOT).
   MODE is the machine mode.
   If COMPLEMENT_FINAL_P is true, wrap the whole operation with NOT.
   If COMPLEMENT_OP1_P is true, wrap operand1 with NOT.
   If COMPLEMENT_OP2_P is true, wrap operand2 with NOT.
   CLOBBER_REG is either NULL or a scratch register of type CC to allow
   formation of the AND instructions.  */

static void
rs6000_split_logical_inner (rtx dest,
			    rtx op1,
			    rtx op2,
			    enum rtx_code code,
			    enum machine_mode mode,
			    bool complement_final_p,
			    bool complement_op1_p,
			    bool complement_op2_p,
			    rtx clobber_reg)
{
  rtx bool_rtx;
  rtx set_rtx;

  /* Optimize AND of 0/0xffffffff and IOR/XOR of 0.  */
  if (op2 && GET_CODE (op2) == CONST_INT
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
	      emit_insn (gen_rtx_SET (VOIDmode, dest, const0_rtx));
	      return;
	    }

	  else if (value == mask)
	    {
	      if (!rtx_equal_p (dest, op1))
		emit_insn (gen_rtx_SET (VOIDmode, dest, op1));
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
		emit_insn (gen_rtx_SET (VOIDmode, dest, op1));
	      return;
	    }
	}
    }

  if (complement_op1_p)
    op1 = gen_rtx_NOT (mode, op1);

  if (complement_op2_p)
    op2 = gen_rtx_NOT (mode, op2);

  bool_rtx = ((code == NOT)
	      ? gen_rtx_NOT (mode, op1)
	      : gen_rtx_fmt_ee (code, mode, op1, op2));

  if (complement_final_p)
    bool_rtx = gen_rtx_NOT (mode, bool_rtx);

  set_rtx = gen_rtx_SET (VOIDmode, dest, bool_rtx);

  /* Is this AND with an explicit clobber?  */
  if (clobber_reg)
    {
      rtx clobber = gen_rtx_CLOBBER (VOIDmode, clobber_reg);
      set_rtx = gen_rtx_PARALLEL (VOIDmode, gen_rtvec (2, set_rtx, clobber));
    }

  emit_insn (set_rtx);
  return;
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
			 bool complement_op2_p,
			 rtx clobber_reg)
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
      if (GET_CODE (operands[2]) != CONST_INT)
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
	  && GET_CODE (op2_hi_lo[i]) == CONST_INT
	  && !complement_final_p
	  && !complement_op1_p
	  && !complement_op2_p
	  && clobber_reg == NULL_RTX
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
				      code, SImode, false, false, false,
				      NULL_RTX);

	  rs6000_split_logical_inner (op0_hi_lo[i], tmp, GEN_INT (lo_16bits),
				      code, SImode, false, false, false,
				      NULL_RTX);
	}
      else
	rs6000_split_logical_inner (op0_hi_lo[i], op1_hi_lo[i], op2_hi_lo[i],
				    code, SImode, complement_final_p,
				    complement_op1_p, complement_op2_p,
				    clobber_reg);
    }

  return;
}

/* Split the insns that make up boolean operations operating on multiple GPR
   registers.  The boolean MD patterns ensure that the inputs either are
   exactly the same as the output registers, or there is no overlap.

   OPERANDS is an array containing the destination and two input operands.
   CODE is the base operation (AND, IOR, XOR, NOT).
   MODE is the machine mode.
   If COMPLEMENT_FINAL_P is true, wrap the whole operation with NOT.
   If COMPLEMENT_OP1_P is true, wrap operand1 with NOT.
   If COMPLEMENT_OP2_P is true, wrap operand2 with NOT.
   CLOBBER_REG is either NULL or a scratch register of type CC to allow
   formation of the AND instructions.  */

void
rs6000_split_logical (rtx operands[3],
		      enum rtx_code code,
		      bool complement_final_p,
		      bool complement_op1_p,
		      bool complement_op2_p,
		      rtx clobber_reg)
{
  enum machine_mode mode = GET_MODE (operands[0]);
  enum machine_mode sub_mode;
  rtx op0, op1, op2;
  int sub_size, regno0, regno1, nregs, i;

  /* If this is DImode, use the specialized version that can run before
     register allocation.  */
  if (mode == DImode && !TARGET_POWERPC64)
    {
      rs6000_split_logical_di (operands, code, complement_final_p,
			       complement_op1_p, complement_op2_p,
			       clobber_reg);
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
				  complement_op2_p, clobber_reg);
    }

  return;
}


/* Return true if the peephole2 can combine a load involving a combination of
   an addis instruction and a load with an offset that can be fused together on
   a power8.

   The operands are:
	operands[0]	register set with addis
	operands[1]	value set via addis
	operands[2]	target register being loaded
	operands[3]	D-form memory reference using operands[0].

   In addition, we are passed a boolean that is true if this is a peephole2,
   and we can use see if the addis_reg is dead after the insn and can be
   replaced by the target register.  */

bool
fusion_gpr_load_p (rtx *operands, bool peep2_p)
{
  rtx addis_reg = operands[0];
  rtx addis_value = operands[1];
  rtx target = operands[2];
  rtx mem = operands[3];
  rtx addr;
  rtx base_reg;

  /* Validate arguments.  */
  if (!base_reg_operand (addis_reg, GET_MODE (addis_reg)))
    return false;

  if (!base_reg_operand (target, GET_MODE (target)))
    return false;

  if (!fusion_gpr_addis (addis_value, GET_MODE (addis_value)))
    return false;

  if (!fusion_gpr_mem_load (mem, GET_MODE (mem)))
    return false;

  /* Allow sign/zero extension.  */
  if (GET_CODE (mem) == ZERO_EXTEND
      || (GET_CODE (mem) == SIGN_EXTEND && TARGET_P8_FUSION_SIGN))
    mem = XEXP (mem, 0);

  if (!MEM_P (mem))
    return false;

  addr = XEXP (mem, 0);			/* either PLUS or LO_SUM.  */
  if (GET_CODE (addr) != PLUS && GET_CODE (addr) != LO_SUM)
    return false;

  /* Validate that the register used to load the high value is either the
     register being loaded, or we can safely replace its use in a peephole2.

     If this is a peephole2, we assume that there are 2 instructions in the
     peephole (addis and load), so we want to check if the target register was
     not used in the memory address and the register to hold the addis result
     is dead after the peephole.  */
  if (REGNO (addis_reg) != REGNO (target))
    {
      if (!peep2_p)
	return false;

      if (reg_mentioned_p (target, mem))
	return false;

      if (!peep2_reg_dead_p (2, addis_reg))
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
  enum machine_mode target_mode = GET_MODE (target);
  enum machine_mode extend_mode = target_mode;
  enum machine_mode ptr_mode = Pmode;
  enum rtx_code extend = UNKNOWN;
  rtx addis_reg = ((ptr_mode == target_mode)
		   ? target
		   : simplify_subreg (ptr_mode, target, target_mode, 0));

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
  new_addr = gen_rtx_fmt_ee (plus_or_lo_sum, ptr_mode, addis_reg, offset);
  new_mem = change_address (orig_mem, target_mode, new_addr);

  if (extend != UNKNOWN)
    new_mem = gen_rtx_fmt_e (ZERO_EXTEND, extend_mode, new_mem);

  emit_insn (gen_rtx_SET (VOIDmode, addis_reg, addis_value));
  emit_insn (gen_rtx_SET (VOIDmode, target, new_mem));

  if (extend == SIGN_EXTEND)
    {
      int sub_off = ((BYTES_BIG_ENDIAN)
		     ? GET_MODE_SIZE (extend_mode) - GET_MODE_SIZE (target_mode)
		     : 0);
      rtx sign_reg
	= simplify_subreg (target_mode, target, extend_mode, sub_off);

      emit_insn (gen_rtx_SET (VOIDmode, target,
			      gen_rtx_SIGN_EXTEND (extend_mode, sign_reg)));
    }

  return;
}

/* Return a string to fuse an addis instruction with a gpr load to the same
   register that we loaded up the addis instruction.  The code is complicated,
   so we call output_asm_insn directly, and just return "".

   The operands are:
	operands[0]	register set with addis (must be same reg as target).
	operands[1]	value set via addis
	operands[2]	target register being loaded
	operands[3]	D-form memory reference using operands[0].  */

const char *
emit_fusion_gpr_load (rtx *operands)
{
  rtx addis_reg = operands[0];
  rtx addis_value = operands[1];
  rtx target = operands[2];
  rtx mem = operands[3];
  rtx fuse_ops[10];
  rtx addr;
  rtx load_offset;
  const char *addis_str = NULL;
  const char *load_str = NULL;
  const char *extend_insn = NULL;
  const char *mode_name = NULL;
  char insn_template[80];
  enum machine_mode mode;
  const char *comment_str = ASM_COMMENT_START;
  bool sign_p = false;

  gcc_assert (REG_P (addis_reg) && REG_P (target));
  gcc_assert (REGNO (addis_reg) == REGNO (target));

  if (*comment_str == ' ')
    comment_str++;

  /* Allow sign/zero extension.  */
  if (GET_CODE (mem) == ZERO_EXTEND)
    mem = XEXP (mem, 0);

  else if (GET_CODE (mem) == SIGN_EXTEND && TARGET_P8_FUSION_SIGN)
    {
      sign_p = true;
      mem = XEXP (mem, 0);
    }

  gcc_assert (MEM_P (mem));
  addr = XEXP (mem, 0);
  if (GET_CODE (addr) != PLUS && GET_CODE (addr) != LO_SUM)
    gcc_unreachable ();

  load_offset = XEXP (addr, 1);

  /* Now emit the load instruction to the same register.  */
  mode = GET_MODE (mem);
  switch (mode)
    {
    case QImode:
      mode_name = "char";
      load_str = "lbz";
      extend_insn = "extsb %0,%0";
      break;

    case HImode:
      mode_name = "short";
      load_str = "lhz";
      extend_insn = "extsh %0,%0";
      break;

    case SImode:
      mode_name = "int";
      load_str = "lwz";
      extend_insn = "extsw %0,%0";
      break;

    case DImode:
      if (TARGET_POWERPC64)
	{
	  mode_name = "long";
	  load_str = "ld";
	}
      else
	gcc_unreachable ();
      break;

    default:
      gcc_unreachable ();
    }

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

  sprintf (insn_template, "%s\t\t%s gpr load fusion, type %s", addis_str,
	   comment_str, mode_name);
  output_asm_insn (insn_template, fuse_ops);

  /* Emit the D-form load instruction.  */
  if (CONST_INT_P (load_offset) && satisfies_constraint_I (load_offset))
    {
      sprintf (insn_template, "%s %%0,%%1(%%0)", load_str);
      fuse_ops[1] = load_offset;
      output_asm_insn (insn_template, fuse_ops);
    }

  else if (GET_CODE (load_offset) == UNSPEC
	   && XINT (load_offset, 1) == UNSPEC_TOCREL)
    {
      if (TARGET_ELF)
	sprintf (insn_template, "%s %%0,%%1@toc@l(%%0)", load_str);

      else if (TARGET_XCOFF)
	sprintf (insn_template, "%s %%0,%%1@l(%%0)", load_str);

      else
	gcc_unreachable ();

      fuse_ops[1] = XVECEXP (load_offset, 0, 0);
      output_asm_insn (insn_template, fuse_ops);
    }

  else if (GET_CODE (load_offset) == PLUS
	   && GET_CODE (XEXP (load_offset, 0)) == UNSPEC
	   && XINT (XEXP (load_offset, 0), 1) == UNSPEC_TOCREL
	   && CONST_INT_P (XEXP (load_offset, 1)))
    {
      rtx tocrel_unspec = XEXP (load_offset, 0);
      if (TARGET_ELF)
	sprintf (insn_template, "%s %%0,%%1+%%2@toc@l(%%0)", load_str);

      else if (TARGET_XCOFF)
	sprintf (insn_template, "%s %%0,%%1+%%2@l(%%0)", load_str);

      else
	gcc_unreachable ();

      fuse_ops[1] = XVECEXP (tocrel_unspec, 0, 0);
      fuse_ops[2] = XEXP (load_offset, 1);
      output_asm_insn (insn_template, fuse_ops);
    }

  else if (TARGET_ELF && !TARGET_POWERPC64 && CONSTANT_P (load_offset))
    {
      sprintf (insn_template, "%s %%0,%%1@l(%%0)", load_str);

      fuse_ops[1] = load_offset;
      output_asm_insn (insn_template, fuse_ops);
    }

  else
    fatal_insn ("Unable to generate load offset for fusion", load_offset);

  /* Handle sign extension.  The peephole2 pass generates this as a separate
     insn, but we handle it just in case it got reattached.  */
  if (sign_p)
    {
      gcc_assert (extend_insn != NULL);
      output_asm_insn (extend_insn, fuse_ops);
    }

  return "";
}


struct gcc_target targetm = TARGET_INITIALIZER;

#include "gt-rs6000.h"
