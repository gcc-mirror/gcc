/* Definitions for Toshiba Media Processor
   Copyright (C) 2001, 2003, 2004, 2005, 2007, 2008, 2009
   Free Software Foundation, Inc.
   Contributed by Red Hat, Inc.

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


#undef  CPP_SPEC
#define CPP_SPEC "\
-D__MEP__ -D__MeP__ \
-D__section(_x)=__attribute__((section(_x))) \
-D__align(_x)=__attribute__((aligned(_x))) \
-D__io(_x)=__attribute__((io(_x))) \
-D__cb(_x)=__attribute__((cb(_x))) \
-D__based=__attribute__((based)) \
-D__tiny=__attribute__((tiny)) \
-D__near=__attribute__((near)) \
-D__far=__attribute__((far)) \
-D__vliw=__attribute__((vliw)) \
-D__interrupt=__attribute__((interrupt)) \
-D__disinterrupt=__attribute__((disinterrupt)) \
%{!meb:%{!mel:-D__BIG_ENDIAN__}} \
%{meb:-U__LITTLE_ENDIAN__ -D__BIG_ENDIAN__} \
%{mel:-U__BIG_ENDIAN__ -D__LITTLE_ENDIAN__} \
%{mconfig=*:-D__MEP_CONFIG_%*} \
%{mivc2:-D__MEP_CONFIG_CP_DATA_BUS_WIDTH=64} \
"

#undef  CC1_SPEC
#define CC1_SPEC "%{!mlibrary:%(config_cc_spec)} \
%{!.cc:%{O2:%{!funroll*:--param max-completely-peeled-insns=6 \
                        --param max-unrolled-insns=6 -funroll-loops}}}"

#undef  CC1PLUS_SPEC
#define CC1PLUS_SPEC "%{!mlibrary:%(config_cc_spec)}"

#undef  ASM_SPEC
#define ASM_SPEC "%{mconfig=*} %{meb:-EB} %{mel:-EL} \
%{mno-satur} %{msatur} %{mno-clip} %{mclip} %{mno-minmax} %{mminmax} \
%{mno-absdiff} %{mabsdiff} %{mno-leadz} %{mleadz} %{mno-bitops} %{mbitops} \
%{mno-div} %{mdiv} %{mno-mult} %{mmult} %{mno-average} %{maverage} \
%{mcop32} %{mno-debug} %{mdebug} %{mlibrary}"

/* The MeP config tool will edit this spec.  */
#undef  STARTFILE_SPEC
#define STARTFILE_SPEC "%{msdram:%{msim:simsdram-crt0.o%s}} \
%{mno-sdram:%{msim:sim-crt0.o%s}} \
%{msdram:%{!msim*:sdram-crt0.o%s}} \
%{mno-sdram:%{!msim*:crt0.o%s}} \
%(config_start_spec) \
%{msimnovec:simnovec-crt0.o%s} \
crtbegin.o%s"

#undef  LIB_SPEC
#define LIB_SPEC "-( -lc %{msim*:-lsim}%{!msim*:-lnosys} -) %(config_link_spec)"

#undef  LINK_SPEC
#define LINK_SPEC "%{meb:-EB} %{mel:-EL}"

#undef  ENDFILE_SPEC
#define ENDFILE_SPEC "crtend.o%s %{msim*:sim-crtn.o%s}%{!msim*:crtn.o%s}"

/* The MeP config tool will edit this spec.  */
#define CONFIG_CC_SPEC "\
%{mconfig=default: -mbitops -mleadz -mabsdiff -maverage -mminmax -mclip -msatur -mvl64 -mvliw -mcop64 -D__MEP_CONFIG_CP_DATA_BUS_WIDTH=64 -mivc2}\
"
/* end-config-cc-spec */

/* The MeP config tool will edit this spec.  */
#define CONFIG_LINK_SPEC "\
%{mconfig=default: %{!T*:-Tdefault.ld}}\
"
/* end-config-link-spec */

/* The MeP config tool will edit this spec.  */
#define CONFIG_START_SPEC "\
%{!msdram:%{!mno-sdram:%{!msim*:crt0.o%s}}} \
%{!msdram:%{!mno-sdram:%{msim:sim-crt0.o%s}}} \
"
/* end-config-start-spec */

#define EXTRA_SPECS \
  { "config_cc_spec",  CONFIG_CC_SPEC }, \
  { "config_link_spec",  CONFIG_LINK_SPEC }, \
  { "config_start_spec",  CONFIG_START_SPEC },


#define TARGET_CPU_CPP_BUILTINS() 		\
  do						\
    {						\
      builtin_define_std ("mep");		\
      builtin_assert ("machine=mep");		\
    }						\
  while (0)

extern int target_flags;

/* Controlled by MeP-Integrator.  */
#define TARGET_H1		0

#define MEP_ALL_OPTS	(MASK_OPT_AVERAGE	\
			 | MASK_OPT_MULT	\
			 | MASK_OPT_DIV		\
			 | MASK_OPT_BITOPS	\
			 | MASK_OPT_LEADZ	\
			 | MASK_OPT_ABSDIFF	\
			 | MASK_OPT_MINMAX	\
			 | MASK_OPT_CLIP	\
			 | MASK_OPT_SATUR )

#define TARGET_DEFAULT		(MASK_IO_VOLATILE | MASK_OPT_REPEAT | MEP_ALL_OPTS | MASK_LITTLE_ENDIAN)

#define TARGET_IO_NO_VOLATILE	(! (target_flags & MASK_IO_VOLATILE))
#define TARGET_OPT_NOREPEAT	(! (target_flags & MASK_OPT_REPEAT))
#define TARGET_32BIT_CR_REGS	(! (target_flags & MASK_64BIT_CR_REGS))
#define TARGET_BIG_ENDIAN	(! (target_flags & MASK_LITTLE_ENDIAN))

#define TARGET_COPRO_MULT	0

#define TARGET_VERSION fprintf (stderr, " (Toshiba Media Processor (MeP))");

#define OVERRIDE_OPTIONS mep_override_options ();

/* The MeP config tool will add TARGET_OPTION_TRANSLATE_TABLE here.  */
#define TARGET_OPTION_TRANSLATE_TABLE \
  {"-mall-opts", "-maverage -mmult -mdiv -mbitops -mleadz \
                  -mabsdiff -mminmax -mclip -msatur -mdebug" }, \
  {"-mno-opts", "-mno-average -mno-mult -mno-div -mno-bitops -mno-leadz \
                  -mno-absdiff -mno-minmax -mno-clip -mno-satur -mno-debug" }, \
  {"-mfar", "-ml -mtf -mc=far" } \
/* start-target-option-table */ \
, {"-mconfig=default", "-mconfig=default -mmult -mdiv -D__MEP_CONFIG_ISA=1" } \
/* end-target-option-table */

/* The MeP config tool will replace this as appropriate.  */
#define DEFAULT_ENDIAN_SPEC "%{!meb: -mel}"

/* The MeP config tool will replace this with an -mconfig= switch.  */
#define LIBRARY_CONFIG_SPEC "-mconfig=default"

/* Don't add an endian option when building the libraries.  */
#define DRIVER_SELF_SPECS \
  "%{!mlibrary:" DEFAULT_ENDIAN_SPEC "}", \
  "%{mlibrary: " LIBRARY_CONFIG_SPEC " %{!mel:-meb}}"

/* The MeP config tool will add COPROC_SELECTION_TABLE here.  */
/* start-coproc-selection-table */
#define COPROC_SELECTION_TABLE \
{"default", ISA_EXT1}
/* end-coproc-selection-table */

#define CAN_DEBUG_WITHOUT_FP

#define OPTIMIZATION_OPTIONS(LEVEL, FOR_SIZE) mep_optimization_options ()


#define BITS_BIG_ENDIAN 0
#define BYTES_BIG_ENDIAN (TARGET_LITTLE_ENDIAN ? 0 : 1)
#define WORDS_BIG_ENDIAN (TARGET_LITTLE_ENDIAN ? 0 : 1)

#ifdef __LITTLE_ENDIAN__
#define LIBGCC2_WORDS_BIG_ENDIAN 0
#else
#define LIBGCC2_WORDS_BIG_ENDIAN 1
#endif

#define UNITS_PER_WORD 4

#define PROMOTE_MODE(MODE, UNSIGNEDP, TYPE)	\
  do						\
    {						\
      if (GET_MODE_CLASS (MODE) == MODE_INT	\
          && GET_MODE_SIZE (MODE) < 4)		\
        (MODE) = SImode;		\
    }						\
  while (0)

#define PARM_BOUNDARY 32
#define STACK_BOUNDARY 32
#define PREFERRED_STACK_BOUNDARY 64
#define FUNCTION_BOUNDARY 16
#define BIGGEST_ALIGNMENT 64

#define DATA_ALIGNMENT(TYPE, ALIGN)		\
  (TREE_CODE (TYPE) == ARRAY_TYPE		\
   && TYPE_MODE (TREE_TYPE (TYPE)) == QImode	\
   && (ALIGN) < BITS_PER_WORD ? BITS_PER_WORD : (ALIGN))

#define CONSTANT_ALIGNMENT(EXP, ALIGN)  \
  (TREE_CODE (EXP) == STRING_CST	\
   && (ALIGN) < BITS_PER_WORD ? BITS_PER_WORD : (ALIGN))

#define STRICT_ALIGNMENT 1

#define PCC_BITFIELD_TYPE_MATTERS 1

#define DEFAULT_VTABLE_THUNKS 1


#define INT_TYPE_SIZE         32
#define SHORT_TYPE_SIZE       16
#define LONG_TYPE_SIZE        32
#define LONG_LONG_TYPE_SIZE   64
#define CHAR_TYPE_SIZE         8
#define FLOAT_TYPE_SIZE       32
#define DOUBLE_TYPE_SIZE      64
#define LONG_DOUBLE_TYPE_SIZE 64
#define DEFAULT_SIGNED_CHAR    1

/* Register numbers:
 	0..15	core registers
	16..47	control registers
	48..79	coprocessor registers
	80..111	coprocessor control registers
	112	virtual arg pointer register  */

#define FIRST_PSEUDO_REGISTER (LAST_SHADOW_REGISTER + 1)

  /* R12 is optionally FP.  R13 is TP, R14 is GP, R15 is SP. */
  /* hi and lo can be used as general registers.  Others have
     immutable bits.  */
/* A "1" here means the register is generally not available to gcc,
   and is assumed to remain unchanged or unused throughout.  */
#define FIXED_REGISTERS {				\
  /* core registers */					\
  0, 0, 0, 0,  0, 0, 0, 0,  0, 0, 0, 0,  0, 0, 0, 1,	\
  /* control registers */				\
  1, 1, 1, 1,  1, 1, 1, 0,  0, 1, 1, 1,  1, 1, 1, 1,	\
  1, 1, 1, 1,  1, 1, 1, 1,  1, 1, 1, 1,  1, 1, 1, 1,	\
  /* coprocessor registers */				\
  1, 1, 1, 1,  1, 1, 1, 1,  1, 1, 1, 1,  1, 1, 1, 1,	\
  1, 1, 1, 1,  1, 1, 1, 1,  1, 1, 1, 1,  1, 1, 1, 1,	\
  /* coprocessor control registers */			\
  1, 1, 1, 1,  1, 1, 1, 1,  1, 1, 1, 1,  1, 1, 1, 1,	\
  1, 1, 1, 1,  1, 1, 1, 1,  1, 1, 1, 1,  1, 1, 1, 1,	\
  /* virtual arg pointer */				\
  1, FIXED_SHADOW_REGISTERS				\
  }

/* This is a call-clobbered reg not used for args or return value,
   that we use as a temp for saving control registers in the prolog
   and restoring them in the epilog. */
#define REGSAVE_CONTROL_TEMP	11

/* A "1" here means a register may be changed by a function without
   needing to preserve its previous value.  */
#define CALL_USED_REGISTERS {				\
  /* core registers */					\
  1, 1, 1, 1,  1, 0, 0, 0,  0, 1, 1, 1,  1, 0, 0, 1,	\
  /* control registers */				\
  1, 1, 1, 1,  1, 1, 1, 1,  1, 1, 1, 1,  1, 1, 1, 1,	\
  1, 1, 1, 1,  1, 1, 1, 1,  1, 1, 1, 1,  1, 1, 1, 1,	\
  /* coprocessor registers */				\
  1, 1, 1, 1,  1, 1, 1, 1,  1, 1, 1, 1,  1, 1, 1, 1,	\
  1, 1, 1, 1,  1, 1, 1, 1,  1, 1, 1, 1,  1, 1, 1, 1,	\
  /* coprocessor control registers */			\
  1, 1, 1, 1,  1, 1, 1, 1,  1, 1, 1, 1,  1, 1, 1, 1,	\
  1, 1, 1, 1,  1, 1, 1, 1,  1, 1, 1, 1,  1, 1, 1, 1,	\
  /* virtual arg pointer */				\
  1, CALL_USED_SHADOW_REGISTERS				\
  }

#define CONDITIONAL_REGISTER_USAGE \
	mep_conditional_register_usage (fixed_regs, call_used_regs);

#define REG_ALLOC_ORDER {						\
  /* core registers */							\
  3, 2, 1, 0, 9, 10, 11, 12, 4, 5, 6, 7, 8, 13, 14, 15, 		\
  /* control registers */						\
  16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31,	\
  32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47,	\
  /* coprocessor registers */						\
  /* Prefer to use the non-loadable registers when looking for a	\
     member of CR_REGS (as opposed to LOADABLE_CR_REGS).  */		\
  64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 48, 49, 50, 51, 52, 58,	\
  59, 60, 61, 62, 63, 53, 54, 55, 56, 57, 74, 75, 76, 77, 78, 79,	\
  /* coprocessor control registers */					\
  80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91, 92, 93, 94, 95,	\
  96, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111, \
  /* virtual arg pointer */						\
  112, SHADOW_REG_ALLOC_ORDER						\
  }

/* We must somehow disable register remapping for interrupt functions.  */
extern char mep_leaf_registers[];
#define LEAF_REGISTERS mep_leaf_registers
#define LEAF_REG_REMAP(REG) (REG)


#define FIRST_GR_REGNO 0
#define FIRST_CONTROL_REGNO (FIRST_GR_REGNO + 16)
#define FIRST_CR_REGNO (FIRST_CONTROL_REGNO + 32)
#define FIRST_CCR_REGNO (FIRST_CR_REGNO + 32)

#define GR_REGNO_P(REGNO) \
  ((unsigned) ((REGNO) - FIRST_GR_REGNO) < 16)

#define CONTROL_REGNO_P(REGNO) \
  ((unsigned) ((REGNO) - FIRST_CONTROL_REGNO) < 32)

#define LOADABLE_CR_REGNO_P(REGNO) \
  ((unsigned) ((REGNO) - FIRST_CR_REGNO) < 16)

#define CR_REGNO_P(REGNO) \
  ((unsigned) ((REGNO) - FIRST_CR_REGNO) < 32)

#define CCR_REGNO_P(REGNO) \
  ((unsigned) ((REGNO) - FIRST_CCR_REGNO) < 32)

#define ANY_CONTROL_REGNO_P(REGNO) \
  (CONTROL_REGNO_P (REGNO) || CCR_REGNO_P (REGNO))

#define HARD_REGNO_NREGS(REGNO, MODE)		\
  ((CR_REGNO_P (REGNO) && TARGET_64BIT_CR_REGS)	\
   ? (GET_MODE_SIZE (MODE) + 8 - 1) / 8		\
   : (GET_MODE_SIZE (MODE) + 4 - 1) / 4)

#define HARD_REGNO_MODE_OK(REGNO, MODE) 1

#define MODES_TIEABLE_P(MODE1, MODE2) 1

#define CANNOT_CHANGE_MODE_CLASS(FROM, TO, CLASS) \
  mep_cannot_change_mode_class (FROM, TO, CLASS)

enum reg_class
{
  NO_REGS,
  SP_REGS,
  TP_REGS,
  GP_REGS,
  R0_REGS,
  RPC_REGS,
  HI_REGS,
  LO_REGS,
  HILO_REGS,
  TPREL_REGS,
  GENERAL_NOT_R0_REGS,
  GENERAL_REGS,
  CONTROL_REGS,
  CONTROL_OR_GENERAL_REGS,
  USER0_REGS,
  USER1_REGS,
  USER2_REGS,
  USER3_REGS,
  LOADABLE_CR_REGS,
  CR_REGS,
  CCR_REGS,
  ALL_REGS,
  LIM_REG_CLASSES
};

#define N_REG_CLASSES ((int) LIM_REG_CLASSES)

#define REG_CLASS_NAMES { \
  "NO_REGS", \
  "SP_REGS", \
  "TP_REGS", \
  "GP_REGS", \
  "R0_REGS", \
  "RPC_REGS", \
  "HI_REGS", \
  "LO_REGS", \
  "HILO_REGS", \
  "TPREL_REGS", \
  "GENERAL_NOT_R0_REGS", \
  "GENERAL_REGS", \
  "CONTROL_REGS", \
  "CONTROL_OR_GENERAL_REGS", \
  "USER0_REGS", \
  "USER1_REGS", \
  "USER2_REGS", \
  "USER3_REGS", \
  "LOADABLE_CR_REGS", \
  "CR_REGS", \
  "CCR_REGS", \
  "ALL_REGS" }

#define REG_CLASS_CONTENTS { \
  { 0x00000000, 0x00000000, 0x00000000, 0x00000000 }, /* NO_REGS */ \
  { 0x00008000, 0x00000000, 0x00000000, 0x00000000 }, /* SP_REGS */ \
  { 0x00002000, 0x00000000, 0x00000000, 0x00000000 }, /* TP_REGS */ \
  { 0x00004000, 0x00000000, 0x00000000, 0x00000000 }, /* GP_REGS */ \
  { 0x00000001, 0x00000000, 0x00000000, 0x00000000 }, /* R0_REGS */ \
  { 0x00400000, 0x00000000, 0x00000000, 0x00000000 }, /* RPC_REGS */ \
  { 0x00800000, 0x00000000, 0x00000000, 0x00000000 }, /* HI_REGS */ \
  { 0x01000000, 0x00000000, 0x00000000, 0x00000000 }, /* LO_REGS */ \
  { 0x01800000, 0x00000000, 0x00000000, 0x00000000 }, /* HILO_REGS */ \
  { 0x000000ff, 0x00000000, 0x00000000, 0x00000000 }, /* TPREL_REGS */ \
  { 0x0000fffe, 0x00000000, 0x00000000, 0x00000000 }, /* GENERAL_NOT_R0_REGS */ \
  { 0x0000ffff, 0x00000000, 0x00000000, 0x00010000 }, /* GENERAL_REGS */ \
  { 0xffff0000, 0x0000ffff, 0x00000000, 0x00000000 }, /* CONTROL_REGS */ \
  { 0xffffffff, 0x0000ffff, 0x00000000, 0x00000000 }, /* CONTROL_OR_GENERAL_REGS */ \
  { 0x00000000, 0x00000000, 0x00000000, 0x00000000 }, /* USER0_REGS */ \
  { 0x00000000, 0x00000000, 0x00000000, 0x00000000 }, /* USER1_REGS */ \
  { 0x00000000, 0x00000000, 0x00000000, 0x00000000 }, /* USER2_REGS */ \
  { 0x00000000, 0x00000000, 0x00000000, 0x00000000 }, /* USER3_REGS */ \
  { 0x00000000, 0xffff0000, 0x00000000, 0x00000000 }, /* LOADABLE_CR_REGS */ \
  { 0x00000000, 0xffff0000, 0x0000ffff, 0x00000000 }, /* CR_REGS */ \
  { 0x00000000, 0x00000000, 0xffff0000, 0x0000ffff }, /* CCR_REGS */ \
  { 0xffffffff, 0xffffffff, 0xffffffff, 0x0001ffff }, /* ALL_REGS */ \
  }

#define REGNO_REG_CLASS(REGNO) mep_regno_reg_class (REGNO)

#define IRA_COVER_CLASSES { GENERAL_REGS, CONTROL_REGS, CR_REGS, CCR_REGS, LIM_REG_CLASSES }

#define BASE_REG_CLASS GENERAL_REGS
#define INDEX_REG_CLASS GENERAL_REGS

#if 0
#define REG_CLASS_FROM_CONSTRAINT(CHAR, STRING) \
	mep_reg_class_from_constraint (CHAR, STRING)
#endif

#define REGNO_OK_FOR_BASE_P(NUM) (GR_REGNO_P (NUM) \
	|| (NUM) == ARG_POINTER_REGNUM \
	|| (NUM) >= FIRST_PSEUDO_REGISTER)

#define REGNO_OK_FOR_INDEX_P(NUM) REGNO_OK_FOR_BASE_P (NUM)

#define PREFERRED_RELOAD_CLASS(X, CLASS) mep_preferred_reload_class (X, CLASS)

#define SECONDARY_INPUT_RELOAD_CLASS(CLASS, MODE, X) \
	mep_secondary_input_reload_class (CLASS, MODE, X)
#define SECONDARY_OUTPUT_RELOAD_CLASS(CLASS, MODE, X) \
	mep_secondary_output_reload_class (CLASS, MODE, X)
#define SECONDARY_MEMORY_NEEDED(CLASS1, CLASS2, MODE) \
	mep_secondary_memory_needed (CLASS1, CLASS2, MODE)

#define CLASS_MAX_NREGS(CLASS, MODE) \
  ((GET_MODE_SIZE (MODE) + UNITS_PER_WORD - 1) / UNITS_PER_WORD)

#if 0
#define CONST_OK_FOR_LETTER_P(VALUE, C) mep_const_ok_for_letter_p (VALUE, C)

#define CONST_DOUBLE_OK_FOR_LETTER_P(VALUE, C) 0

#define CONSTRAINT_LEN(C, STR) \
	((C) == 'e' ? 2 : DEFAULT_CONSTRAINT_LEN (C, STR))
#define EXTRA_CONSTRAINT(VALUE, C) mep_extra_constraint (VALUE, C)
#endif

#define WANT_GCC_DECLARATIONS
#include "mep-intrin.h"
#undef WANT_GCC_DECLARATIONS

extern int mep_intrinsic_insn[];
extern unsigned int mep_selected_isa;

/* True if intrinsic X is available.  X is a mep_* value declared
   in mep-intrin.h.  */
#define MEP_INTRINSIC_AVAILABLE_P(X) (mep_intrinsic_insn[X] >= 0)

/* Used to define CGEN_ENABLE_INTRINSIC_P in mep-intrin.h.  */
#define CGEN_CURRENT_ISAS mep_selected_isa
#define CGEN_CURRENT_GROUP \
  (mep_vliw_function_p (cfun->decl) ? GROUP_VLIW : GROUP_NORMAL)



#define STACK_GROWS_DOWNWARD       1
#define FRAME_GROWS_DOWNWARD	   1
#define STARTING_FRAME_OFFSET      0
#define FIRST_PARM_OFFSET(FUNDECL) 0
#define INCOMING_FRAME_SP_OFFSET   0

#define RETURN_ADDR_RTX(COUNT, FRAMEADDR) mep_return_addr_rtx (COUNT)
#define INCOMING_RETURN_ADDR_RTX          gen_rtx_REG (SImode, LP_REGNO)
#define DWARF_FRAME_RETURN_COLUMN         LP_REGNO

#define STACK_POINTER_REGNUM          15
#define FRAME_POINTER_REGNUM           8
#define ARG_POINTER_REGNUM            112
#define RETURN_ADDRESS_POINTER_REGNUM 17
#define STATIC_CHAIN_REGNUM            0



#define ELIMINABLE_REGS						\
{								\
  {ARG_POINTER_REGNUM,	 STACK_POINTER_REGNUM},			\
  {ARG_POINTER_REGNUM,	 FRAME_POINTER_REGNUM},			\
  {FRAME_POINTER_REGNUM, STACK_POINTER_REGNUM}			\
}

#define INITIAL_ELIMINATION_OFFSET(FROM, TO, OFFSET) \
	(OFFSET) = mep_elimination_offset (FROM, TO)

#define ACCUMULATE_OUTGOING_ARGS 1

#define RETURN_POPS_ARGS(FUNDECL, FUNTYPE, STACK_SIZE) 0



/* The ABI is thus: Arguments are in $1, $2, $3, $4, stack.  Arguments
   larger than 4 bytes are passed indirectly.  Return value in 0,
   unless bigger than 4 bytes, then the caller passes a pointer as the
   first arg.  For varargs, we copy $1..$4 to the stack.  */

#define FUNCTION_ARG(CUM, MODE, TYPE, NAMED) \
	mep_function_arg (CUM, MODE, TYPE, NAMED)

#define FUNCTION_ARG_CALLEE_COPIES(CUM, MODE, TYPE, NAMED) 1

typedef struct
{
  int nregs;
  int vliw;
} CUMULATIVE_ARGS;

#define INIT_CUMULATIVE_ARGS(CUM, FNTYPE, LIBNAME, FNDECL, N_NAMED_ARGS) \
	mep_init_cumulative_args (& (CUM), FNTYPE, LIBNAME, FNDECL)

#define FUNCTION_ARG_ADVANCE(CUM, MODE, TYPE, NAMED)		\
	mep_arg_advance (& (CUM), MODE, TYPE, NAMED)

#define FUNCTION_ARG_REGNO_P(REGNO) \
	(((REGNO) >= 1 && (REGNO) <= 4) \
	 || ((REGNO) >= FIRST_CR_REGNO + 1 \
	     && (REGNO) <= FIRST_CR_REGNO + 4 \
	     && TARGET_COP))

#define RETURN_VALUE_REGNUM	 0

#define FUNCTION_VALUE(VALTYPE, FUNC) mep_function_value (VALTYPE, FUNC)
#define LIBCALL_VALUE(MODE) mep_libcall_value (MODE)

#define FUNCTION_VALUE_REGNO_P(REGNO)				\
  ((REGNO) == RETURN_VALUE_REGNUM)

#define DEFAULT_PCC_STRUCT_RETURN 0

#define STRUCT_VALUE 0

#define FUNCTION_OK_FOR_SIBCALL(DECL) mep_function_ok_for_sibcall(DECL)

/* Prologue and epilogues are all handled via RTL.  */

#define EXIT_IGNORE_STACK 1

#define EPILOGUE_USES(REGNO)  mep_epilogue_uses (REGNO)

/* Profiling is supported.  */
     
#define FUNCTION_PROFILER(FILE, LABELNO) mep_function_profiler (FILE);
#undef TARGET_HAS_F_SETLKW
#define NO_PROFILE_COUNTERS 1

/* Trampolines are built at run-time.  The cache is invalidated at
   run-time also.  */

#define TRAMPOLINE_SIZE 20

#define INITIALIZE_TRAMPOLINE(ADDR, FNADDR, STATIC_CHAIN) \
	mep_init_trampoline (ADDR, FNADDR, STATIC_CHAIN)



#define CONSTANT_ADDRESS_P(X) CONSTANT_P (X)

#define MAX_REGS_PER_ADDRESS 1

#ifdef REG_OK_STRICT
#define GO_IF_LEGITIMATE_ADDRESS(MODE, X, LABEL) 		\
	if (mep_legitimate_address ((MODE), (X), 1)) goto LABEL
#else
#define GO_IF_LEGITIMATE_ADDRESS(MODE, X, LABEL) 		\
	if (mep_legitimate_address ((MODE), (X), 0)) goto LABEL
#endif

#ifdef REG_OK_STRICT
#define REG_OK_FOR_BASE_P(X) GR_REGNO_P (REGNO (X))
#else
#define REG_OK_FOR_BASE_P(X) (GR_REGNO_P (REGNO (X)) \
				|| REGNO (X) == ARG_POINTER_REGNUM \
				|| REGNO (X) >= FIRST_PSEUDO_REGISTER)
#endif

#define REG_OK_FOR_INDEX_P(X) REG_OK_FOR_BASE_P (X)

#define LEGITIMIZE_RELOAD_ADDRESS(X, MODE, OPNUM, TYPE, IND_LEVELS, WIN) \
  if (mep_legitimize_reload_address (&(X), (MODE), (OPNUM), (TYPE), (IND_LEVELS))) \
    goto WIN

#define GO_IF_MODE_DEPENDENT_ADDRESS(ADDR, LABEL)

#define LEGITIMATE_CONSTANT_P(X) \
  mep_legitimate_constant_p(X)

#define SELECT_CC_MODE(OP, X, Y)  CCmode


/* Moves between control regs need a scratch.  */
#define REGISTER_MOVE_COST(MODE, FROM, TO) mep_register_move_cost (MODE, FROM, TO)

#define SLOW_BYTE_ACCESS 1

/* Define this macro if it is as good or better to call a constant function
   address than to call an address kept in a register.  */
#define NO_FUNCTION_CSE 


#define TEXT_SECTION_ASM_OP "\t.text\n\t.core"
#define DATA_SECTION_ASM_OP "\t.data"
#define BSS_SECTION_ASM_OP  ".bss"

#define USE_SELECT_SECTION_FOR_FUNCTIONS 1

#define TARGET_ASM_FILE_END mep_file_cleanups

#define ASM_APP_ON "#APP\n"
#define ASM_APP_OFF "#NO_APP\n"

#define ASM_OUTPUT_DOUBLE(FILE, VALUE)				\
  do								\
    {								\
      long l[2];						\
								\
      REAL_VALUE_TO_TARGET_DOUBLE (VALUE, l);			\
      fprintf (FILE, "\t.long\t0x%lx,0x%lx\n", l[0], l[1]);	\
    }								\
  while (0)

#define ASM_OUTPUT_FLOAT(FILE, VALUE)		\
  do						\
    {						\
      long l;					\
						\
      REAL_VALUE_TO_TARGET_SINGLE (VALUE, l);	\
      fprintf ((FILE), "\t.long\t0x%lx\n", l);	\
    }						\
  while (0)

#define ASM_OUTPUT_CHAR(FILE, VALUE)		\
  do						\
    {						\
      fprintf (FILE, "\t.byte\t");		\
      output_addr_const (FILE, (VALUE));	\
      fprintf (FILE, "\n");			\
    }						\
  while (0)

#define ASM_OUTPUT_SHORT(FILE, VALUE)		\
  do						\
    {						\
      fprintf (FILE, "\t.hword\t");		\
      output_addr_const (FILE, (VALUE));	\
      fprintf (FILE, "\n");			\
    }						\
  while (0)

#define ASM_OUTPUT_INT(FILE, VALUE)		\
  do						\
    {						\
      fprintf (FILE, "\t.word\t");		\
      output_addr_const (FILE, (VALUE));	\
      fprintf (FILE, "\n");			\
    }						\
  while (0)

#define ASM_OUTPUT_BYTE(STREAM, VALUE) \
  fprintf (STREAM, "\t%s\t0x%x\n", ASM_BYTE_OP, (VALUE))

/* Most of these are here to support based/tiny/far/io attributes.  */

#define ASM_OUTPUT_ALIGNED_DECL_COMMON(STREAM, DECL, NAME, SIZE, ALIGNMENT) \
	mep_output_aligned_common (STREAM, DECL, NAME, SIZE, ALIGNMENT, 1)

#define ASM_OUTPUT_ALIGNED_DECL_LOCAL(STREAM, DECL, NAME, SIZE, ALIGNMENT) \
	mep_output_aligned_common (STREAM, DECL, NAME, SIZE, ALIGNMENT, 0)

#define ASM_OUTPUT_LABEL(STREAM, NAME)		\
  do						\
    {						\
      assemble_name (STREAM, NAME);		\
      fputs (":\n", STREAM);			\
    }						\
  while (0)

/* Globalizing directive for a label.  */
#define GLOBAL_ASM_OP "\t.globl "

#define ASM_OUTPUT_LABELREF(STREAM, NAME) \
  asm_fprintf ((STREAM), "%U%s", mep_strip_name_encoding (NAME))

#define ASM_FORMAT_PRIVATE_NAME(OUTVAR, NAME, NUMBER)		\
  do								\
    {								\
      (OUTVAR) = (char *) alloca (strlen ((NAME)) + 12);	\
      sprintf ((OUTVAR), "%s.%ld", (NAME), (long)(NUMBER));	\
    }								\
  while (0)


#define REGISTER_NAMES							\
{									\
  /* Core registers.  */						\
  "$0", "$1", "$2", "$3", "$4", "$5", "$6", "$7",			\
  "$8", "$9", "$10", "$11", "$12", "$tp", "$gp", "$sp",			\
  /* Control registers.  */						\
  "$pc", "$lp", "$sar", "3", "$rpb", "$rpe", "$rpc", "$hi",		\
  "$lo", "9", "10", "11", "$mb0", "$me0", "$mb1", "$me1",		\
  "$psw", "$id", "$tmp", "$epc", "$exc", "$cfg", "22", "$npc",		\
  "$dbg", "$depc", "$opt", "$rcfg", "$ccfg", "29", "30", "31",		\
  /* Coprocessor registers.  */						\
  "$c0", "$c1", "$c2", "$c3", "$c4", "$c5", "$c6", "$c7",		\
  "$c8", "$c9", "$c10", "$c11", "$c12", "$c13", "$c14", "$c15",		\
  "$c16", "$c17", "$c18", "$c19", "$c20", "$c21", "$c22", "$c23",	\
  "$c24", "$c25", "$c26", "$c27", "$c28", "$c29", "$c30", "$c31",	\
  /* Coprocessor control registers.  */					\
  "$ccr0", "$ccr1", "$ccr2", "$ccr3", "$ccr4", "$ccr5", "$ccr6",	\
  "$ccr7", "$ccr8", "$ccr9", "$ccr10", "$ccr11", "$ccr12", "$ccr13",	\
  "$ccr14", "$ccr15", "$ccr16", "$ccr17", "$ccr18", "$ccr19", "$ccr20", \
  "$ccr21", "$ccr22", "$ccr23", "$ccr24", "$ccr25", "$ccr26", "$ccr27", \
  "$ccr28", "$ccr29", "$ccr30", "$ccr31",				\
  /* Virtual arg pointer.  */						\
  "$argp", SHADOW_REGISTER_NAMES					\
}

/* We duplicate some of the above because we twiddle the above
   according to *how* the registers are used.  Likewise, we include
   the standard names for coprocessor control registers so that
   coprocessor options can rename them in the default table.  Note
   that these are compared to stripped names (see REGISTER_PREFIX
   below).  */
#define ADDITIONAL_REGISTER_NAMES		\
{						\
  {  "8",  8 }, { "fp",  8 },			\
  { "13", 13 }, { "tp", 13 },			\
  { "14", 14 }, { "gp", 14 },			\
  { "15", 15 }, { "sp", 15 },			\
  { "ccr0", FIRST_CCR_REGNO + 0 },		\
  { "ccr1", FIRST_CCR_REGNO + 1 },		\
  { "ccr2", FIRST_CCR_REGNO + 2 },		\
  { "ccr3", FIRST_CCR_REGNO + 3 },		\
  { "ccr4", FIRST_CCR_REGNO + 4 },		\
  { "ccr5", FIRST_CCR_REGNO + 5 },		\
  { "ccr6", FIRST_CCR_REGNO + 6 },		\
  { "ccr7", FIRST_CCR_REGNO + 7 },		\
  { "ccr8", FIRST_CCR_REGNO + 8 },		\
  { "ccr9", FIRST_CCR_REGNO + 9 },		\
  { "ccr10", FIRST_CCR_REGNO + 10 },		\
  { "ccr11", FIRST_CCR_REGNO + 11 },		\
  { "ccr12", FIRST_CCR_REGNO + 12 },		\
  { "ccr13", FIRST_CCR_REGNO + 13 },		\
  { "ccr14", FIRST_CCR_REGNO + 14 },		\
  { "ccr15", FIRST_CCR_REGNO + 15 },		\
  { "ccr16", FIRST_CCR_REGNO + 16 },		\
  { "ccr17", FIRST_CCR_REGNO + 17 },		\
  { "ccr18", FIRST_CCR_REGNO + 18 },		\
  { "ccr19", FIRST_CCR_REGNO + 19 },		\
  { "ccr20", FIRST_CCR_REGNO + 20 },		\
  { "ccr21", FIRST_CCR_REGNO + 21 },		\
  { "ccr22", FIRST_CCR_REGNO + 22 },		\
  { "ccr23", FIRST_CCR_REGNO + 23 },		\
  { "ccr24", FIRST_CCR_REGNO + 24 },		\
  { "ccr25", FIRST_CCR_REGNO + 25 },		\
  { "ccr26", FIRST_CCR_REGNO + 26 },		\
  { "ccr27", FIRST_CCR_REGNO + 27 },		\
  { "ccr28", FIRST_CCR_REGNO + 28 },		\
  { "ccr29", FIRST_CCR_REGNO + 29 },		\
  { "ccr30", FIRST_CCR_REGNO + 30 },		\
  { "ccr31", FIRST_CCR_REGNO + 31 }		\
}

/* We watch for pipeline hazards with these */
#define ASM_OUTPUT_OPCODE(STREAM, PTR) mep_asm_output_opcode (STREAM, PTR)
#define FINAL_PRESCAN_INSN(INSN, OPVEC, NOPERANDS) mep_final_prescan_insn (INSN, OPVEC, NOPERANDS)

#define PRINT_OPERAND(STREAM, X, CODE) mep_print_operand (STREAM, X, CODE)

#define PRINT_OPERAND_PUNCT_VALID_P(CODE) ((CODE) == '!' || (CODE) == '<')

#define PRINT_OPERAND_ADDRESS(STREAM, X) mep_print_operand_address (STREAM, X)

#define REGISTER_PREFIX    "$"
#define LOCAL_LABEL_PREFIX "."
#define USER_LABEL_PREFIX  ""
#define IMMEDIATE_PREFIX   ""



#define ASM_OUTPUT_ADDR_VEC_ELT(STREAM, VALUE) \
  fprintf (STREAM, "\t.word .L%d\n", VALUE)



#undef  PREFERRED_DEBUGGING_TYPE
#define PREFERRED_DEBUGGING_TYPE  DWARF2_DEBUG
#define DWARF2_DEBUGGING_INFO     1
#define DWARF2_UNWIND_INFO        1

#define EH_RETURN_DATA_REGNO(N) ((N) < 2 ? (N) + 10 : INVALID_REGNUM)

#define EH_RETURN_STACKADJ_RTX mep_return_stackadj_rtx ()
#define EH_RETURN_HANDLER_RTX  mep_return_handler_rtx ()

#define DBX_REGISTER_NUMBER(REGNO) (REGNO)



#define ASM_OUTPUT_ALIGN(STREAM, POWER) \
  fprintf ((STREAM), "\t.p2align %d\n", (POWER))



#define CASE_VECTOR_MODE SImode

#define WORD_REGISTER_OPERATIONS
#define LOAD_EXTEND_OP(MODE) SIGN_EXTEND

#define SHORT_IMMEDIATES_SIGN_EXTEND

#define MOVE_MAX 4

#define SHIFT_COUNT_TRUNCATED 1

#define TRULY_NOOP_TRUNCATION(OUTPREC, INPREC) 1

#define STORE_FLAG_VALUE 1

#define Pmode SImode

#define FUNCTION_MODE SImode

#define REGISTER_TARGET_PRAGMAS()	 mep_register_pragmas ()

#define HANDLE_PRAGMA_PACK_PUSH_POP 1
   
/* If defined, a C expression to determine the base term of address X.
   This macro is used in only one place: `find_base_term' in alias.c.

   It is always safe for this macro to not be defined.  It exists so
   that alias analysis can understand machine-dependent addresses.

   The typical use of this macro is to handle addresses containing
   a label_ref or symbol_ref within an UNSPEC.  */
#define FIND_BASE_TERM(X) mep_find_base_term (X)
