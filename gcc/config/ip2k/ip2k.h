/* Definitions of target machine for GCC,
   For Ubicom IP2022 Communications Controller

   Copyright (C) 2000, 2001, 2002, 2003, 2004 Free Software Foundation, Inc.
   Contributed by Red Hat, Inc and Ubicom, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */


#undef ASM_SPEC	/* We have a GAS assembler.  */

#define TARGET_CPU_CPP_BUILTINS()		\
  do						\
    {						\
      builtin_define_std ("IP2K");		\
      builtin_define ("_DOUBLE_IS_32BITS");	\
      builtin_define ("_BUFSIZ=512");		\
      builtin_define ("__FILENAME_MAX__=128");	\
    }						\
  while (0)

#define TARGET_VERSION fprintf (stderr, " (ip2k, GNU assembler syntax)")

/* Caller-saves is not a win for the IP2K.  Pretty much anywhere that
   a register is permitted allows SP-relative addresses too.

   This machine doesn't have PIC addressing modes, so disable that also.  */

#define OVERRIDE_OPTIONS	\
    do {			\
	flag_caller_saves = 0;	\
	flag_pic = 0;		\
    } while (0)

/* Put each function in its own section so that PAGE-instruction
   relaxation can do its best.  */
#define OPTIMIZATION_OPTIONS(LEVEL, SIZEFLAG)	\
    do {					\
	if ((LEVEL) || (SIZEFLAG))		\
	    flag_function_sections = 1;	\
    } while (0)

#define BITS_BIG_ENDIAN 0
#define BYTES_BIG_ENDIAN 1
#define WORDS_BIG_ENDIAN 1
#define BITS_PER_WORD 8
#define UNITS_PER_WORD (BITS_PER_WORD / BITS_PER_UNIT)

/* Width in bits of a pointer.
   See also the macro `Pmode' defined below.  */
#define POINTER_SIZE 16

/* Maximum sized of reasonable data type DImode or Dfmode ...  */
#define MAX_FIXED_MODE_SIZE 64

#define PARM_BOUNDARY 8
#define FUNCTION_BOUNDARY 16
#define EMPTY_FIELD_BOUNDARY 8
#define BIGGEST_ALIGNMENT 8

#define STRICT_ALIGNMENT 0

#define PCC_BITFIELD_TYPE_MATTERS 1

#undef INT_TYPE_SIZE
#define INT_TYPE_SIZE 16

#undef SHORT_TYPE_SIZE
#define SHORT_TYPE_SIZE 16

#undef LONG_TYPE_SIZE
#define LONG_TYPE_SIZE 32

#undef LONG_LONG_TYPE_SIZE
#define LONG_LONG_TYPE_SIZE	64

#undef CHAR_TYPE_SIZE
#define  CHAR_TYPE_SIZE 8

#undef FLOAT_TYPE_SIZE
#define FLOAT_TYPE_SIZE 32

#undef DOUBLE_TYPE_SIZE
#define DOUBLE_TYPE_SIZE 32

#undef LONG_DOUBLE_TYPE_SIZE
#define LONG_DOUBLE_TYPE_SIZE	32

#define DEFAULT_SIGNED_CHAR 1

#define SIZE_TYPE "unsigned int"

#define PTRDIFF_TYPE "int"

#undef WCHAR_TYPE
#define WCHAR_TYPE "int"
#undef WCHAR_TYPE_SIZE
#define WCHAR_TYPE_SIZE	16

#define HARD_REG_SIZE           (UNITS_PER_WORD)
/* Standard register usage.

   for the IP2K, we are going to have a LOT of registers, but only some of them
   are named.  */
 
#define FIRST_PSEUDO_REGISTER (0x104) /* Skip over physical regs, VFP, AP.  */

#define REG_IP		0x4
#define REG_IPH		REG_IP
#define REG_IPL		0x5

#define REG_SP		0x6
#define REG_SPH		REG_SP
#define REG_SPL		0x7

#define REG_PCH		0x8
#define REG_PCL		0x9

#define REG_W		0xa
#define REG_STATUS	0xb

#define REG_DP		0xc
#define REG_DPH		REG_DP
#define REG_DPL		0xd

#define REG_MULH	0xf

#define REG_CALLH	0x7e		/* Call-stack readout.  */
#define REG_CALLL	0x7f


#define REG_RESULT	0x80	/* Result register (upto 8 bytes).  */
#define REG_FP		0xfd	/* 2 bytes for FRAME chain  */

#define REG_ZERO	0xff	/* Initialized to zero by runtime.  */

#define REG_VFP		0x100	/* Virtual frame pointer.  */
#define REG_AP		0x102	/* Virtual arg pointer.  */

/* Status register bits.  */
#define Z_FLAG	0x2	 
#define DC_FLAG	0x1
#define C_FLAG	0x0

#define FIXED_REGISTERS {\
1,1,1,1,0,0,1,1,1,1,1,1,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,/*  r0.. r31*/\
1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,/* r32.. r63*/\
1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,/* r64.. r95*/\
1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,/* r96..r127*/\
0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,/*r128..r159*/\
1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,/*r160..r191*/\
1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,/*r192..r223*/\
1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,/*r224..r255*/\
1,1,1,1}

#define CALL_USED_REGISTERS {			\
1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,/*  r0.. r31*/\
1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,/* r32.. r63*/\
1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,/* r64.. r95*/\
1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,/* r96..r127*/\
1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,/*r128..r159*/\
1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,/*r160..r191*/\
1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,/*r192..r223*/\
1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,/*r224..r255*/\
1,1,1,1}

#define REG_ALLOC_ORDER {			\
    0x88,0x89,0x8a,0x8b,0x8c,0x8d,0x8e,0x8f,	\
    0x90,0x91,0x92,0x93,0x94,0x95,0x96,0x97,	\
    0x98,0x99,0x9a,0x9b,0x9c,0x9d,0x9e,0x9f,	\
    0x80,0x81,0x82,0x83,0x84,0x85,0x86,0x87,	\
    0xa0,0xa1,0xa2,0xa3,0xa4,0xa5,0xa6,0xa7,	\
    0xa8,0xa9,0xaa,0xab,0xac,0xad,0xae,0xaf,	\
    0xb0,0xb1,0xb2,0xb3,0xb4,0xb5,0xb6,0xb7,	\
    0xb8,0xb9,0xba,0xbb,0xbc,0xbd,0xbe,0xbf,	\
    0xc0,0xc1,0xc2,0xc3,0xc4,0xc5,0xc6,0xc7,	\
    0xc8,0xc9,0xca,0xcb,0xcc,0xcd,0xce,0xcf,	\
    0xd0,0xd1,0xd2,0xd3,0xd4,0xd5,0xd6,0xd7,	\
    0xd8,0xd9,0xda,0xdb,0xdc,0xdd,0xde,0xdf,	\
    0xe0,0xe1,0xe2,0xe3,0xe4,0xe5,0xe6,0xe7,	\
    0xe8,0xe9,0xea,0xeb,0xec,0xed,0xee,0xef,	\
    0xf0,0xf1,0xf2,0xf3,0xf4,0xf5,0xf6,0xf7,	\
    0xf8,0xf9,0xfa,0xfb,0xfc,0xfd,0xfe,0xff,	\
    0x00,0x01,0x02,0x03,0x0c,0x0d,0x06,0x07,	\
    0x08,0x09,0x0a,0x0b,0x04,0x05,0x0e,0x0f,	\
    0x10,0x11,0x12,0x13,0x14,0x15,0x16,0x17,	\
    0x18,0x19,0x1a,0x1b,0x1c,0x1d,0x1e,0x1f,	\
    0x20,0x21,0x22,0x23,0x24,0x25,0x26,0x27,	\
    0x28,0x29,0x2a,0x2b,0x2c,0x2d,0x2e,0x2f,	\
    0x30,0x31,0x32,0x33,0x34,0x35,0x36,0x37,	\
    0x38,0x39,0x3a,0x3b,0x3c,0x3d,0x3e,0x3f,	\
    0x40,0x41,0x42,0x43,0x44,0x45,0x46,0x47,	\
    0x48,0x49,0x4a,0x4b,0x4c,0x4d,0x4e,0x4f,	\
    0x50,0x51,0x52,0x53,0x54,0x55,0x56,0x57,	\
    0x58,0x59,0x5a,0x5b,0x5c,0x5d,0x5e,0x5f,	\
    0x60,0x61,0x62,0x63,0x64,0x65,0x66,0x67,	\
    0x68,0x69,0x6a,0x6b,0x6c,0x6d,0x6e,0x6f,	\
    0x70,0x71,0x72,0x73,0x74,0x75,0x76,0x77,	\
    0x78,0x79,0x7a,0x7b,0x7c,0x7d,0x7e,0x7f,	\
    0x100,0x101,0x102,0x103}


#define ORDER_REGS_FOR_LOCAL_ALLOC ip2k_init_local_alloc (reg_alloc_order)

/* Are we allowed to rename registers?  For some reason, regrename was
   changing DP to IP (when it appeared in addresses like (plus:HI
   (reg: DP) (const_int 37)) - and that's bad because IP doesn't
   permit offsets!  */

#define HARD_REGNO_RENAME_OK(REG, NREG)				\
  (((REG) == REG_DPH) ? 0					\
    : ((REG) == REG_IPH) ? ((NREG) == REG_DPH)			\
    : (((NREG) == REG_IPL) || ((NREG) == REG_DPL)) ? 0 : 1)

#define HARD_REGNO_NREGS(REGNO, MODE) \
  ((GET_MODE_SIZE (MODE) + UNITS_PER_WORD - 1) / UNITS_PER_WORD)

#define HARD_REGNO_MODE_OK(REGNO, MODE) 1

#define MODES_TIEABLE_P(MODE1, MODE2)		\
   (((MODE1) == QImode && (MODE2) == HImode)	\
    || ((MODE2) == QImode && (MODE1) == HImode))
/* We originally had this as follows - this isn't a win on the IP2k
   though as registers just get in our way!
   
   #define MODES_TIEABLE_P(MODE1, MODE2) \
    (((MODE1) > HImode && (MODE2) == HImode)
     || ((MODE1) == HImode && (MODE2) > HImode))  */

enum reg_class {
  NO_REGS,
  DPH_REGS,
  DPL_REGS,
  DP_REGS,
  SP_REGS,
  IPH_REGS,
  IPL_REGS,
  IP_REGS,
  DP_SP_REGS,
  PTR_REGS,
  NONPTR_REGS,
  NONSP_REGS,
  GENERAL_REGS,
  ALL_REGS = GENERAL_REGS,
  LIM_REG_CLASSES
};

#define N_REG_CLASSES (int)LIM_REG_CLASSES

#define REG_CLASS_NAMES {			\
		"NO_REGS",			\
		"DPH_REGS",			\
		"DPL_REGS",			\
		"DP_REGS",			\
		"SP_REGS",			\
		"IPH_REGS",			\
		"IPL_REGS",			\
		"IP_REGS",			\
		"DP_SP_REGS",			\
		"PTR_REGS",			\
	        "NONPTR_REGS",			\
		"NONSP_REGS",			\
		"GENERAL_REGS"			\
		}


#define REG_CLASS_CONTENTS {			 	\
{0x00000000, 0, 0, 0, 0, 0, 0, 0, 0}, /* NO_REGS */	\
{0x00001000, 0, 0, 0, 0, 0, 0, 0, 0}, /* DPH_REGS */	\
{0x00002000, 0, 0, 0, 0, 0, 0, 0, 0}, /* DPL_REGS */	\
{0x00003000, 0, 0, 0, 0, 0, 0, 0, 0}, /* DP_REGS */	\
{0x000000c0, 0, 0, 0, 0, 0, 0, 0, 0}, /* SP_REGS */	\
{0x00000010, 0, 0, 0, 0, 0, 0, 0, 0}, /* IPH_REGS */	\
{0x00000020, 0, 0, 0, 0, 0, 0, 0, 0}, /* IPL_REGS */	\
{0x00000030, 0, 0, 0, 0, 0, 0, 0, 0}, /* IP_REGS */	\
{0x000030c0, 0, 0, 0, 0, 0, 0, 0, 0}, /* DP_SP_REGS */	\
{0x000030f0, 0, 0, 0, 0, 0, 0, 0, 0}, /* PTR_REGS */	\
{0xffffcf0f,-1,-1,-1,-1,-1,-1,-1, 0}, /* NONPTR_REGS */	\
{0xffffff3f,-1,-1,-1,-1,-1,-1,-1, 0}, /* NONSP_REGS */	\
{0xffffffff,-1,-1,-1,-1,-1,-1,-1,15}  /* GENERAL_REGS */ \
}

#define REGNO_REG_CLASS(R)	\
  ( (R) == REG_IPH ? IPH_REGS	\
  : (R) == REG_IPL ? IPL_REGS	\
  : (R) == REG_DPH ? DPH_REGS	\
  : (R) == REG_DPL ? DPL_REGS	\
  : (R) == REG_SPH ? SP_REGS	\
  : (R) == REG_SPL ? SP_REGS	\
  : NONPTR_REGS)

#define MODE_BASE_REG_CLASS(MODE) ((MODE) == QImode ? PTR_REGS : DP_SP_REGS)

#define BASE_REG_CLASS PTR_REGS

#define INDEX_REG_CLASS NO_REGS

#define REG_CLASS_FROM_LETTER(C)	\
  ( (C) == 'j' ? IPH_REGS		\
  : (C) == 'k' ? IPL_REGS		\
  : (C) == 'f' ? IP_REGS		\
  : (C) == 'y' ? DPH_REGS		\
  : (C) == 'z' ? DPL_REGS		\
  : (C) == 'b' ? DP_REGS		\
  : (C) == 'u' ? NONSP_REGS		\
  : (C) == 'q' ? SP_REGS		\
  : (C) == 'c' ? DP_SP_REGS		\
  : (C) == 'a' ? PTR_REGS		\
  : (C) == 'd' ? NONPTR_REGS 		\
  : NO_REGS)

#define REGNO_OK_FOR_BASE_P(R) \
  ((R) == REG_DP || (R) == REG_IP || (R) == REG_SP)

#define REGNO_MODE_OK_FOR_BASE_P(R,M) 		\
  ((R) == REG_DP || (R) == REG_SP		\
   || ((R) == REG_IP && GET_MODE_SIZE (M) <= 1))

#define REGNO_OK_FOR_INDEX_P(NUM) 0

#define PREFERRED_RELOAD_CLASS(X, CLASS) (CLASS)

#define SMALL_REGISTER_CLASSES 1

#define CLASS_LIKELY_SPILLED_P(CLASS)  class_likely_spilled_p(CLASS)

#define CLASS_MAX_NREGS(CLASS, MODE)   GET_MODE_SIZE (MODE)

#define CONST_OK_FOR_LETTER_P(VALUE, C)				\
  ((C) == 'I' ? (VALUE) >= -255 && (VALUE) <= -1 :		\
   (C) == 'J' ? (VALUE) >= 0 && (VALUE) <= 7 :			\
   (C) == 'K' ? (VALUE) >= 0 && (VALUE) <= 127 :		\
   (C) == 'L' ? (VALUE) > 0 && (VALUE) < 128:			\
   (C) == 'M' ? (VALUE) == -1:					\
   (C) == 'N' ? (VALUE) == 1:					\
   (C) == 'O' ? (VALUE) == 0:					\
   (C) == 'P' ? (VALUE) >= 0 && (VALUE) <= 255:			\
   0)

#define CONST_DOUBLE_OK_FOR_LETTER_P(VALUE, C) 0

#define EXTRA_CONSTRAINT(X, C) ip2k_extra_constraint (X, C)

/* This is an undocumented variable which describes
   how GCC will pop a data.  */
#define STACK_POP_CODE PRE_INC

#define STACK_PUSH_CODE POST_DEC

#define STACK_CHECK_BUILTIN	1
/* Prologue code will do stack checking as necessary.  */
  
#define STARTING_FRAME_OFFSET (0)	

#define FRAME_GROWS_DOWNWARD	1
#define STACK_GROWS_DOWNWARD	1

/* On IP2K arg pointer is virtual and resolves to either SP or FP
   after we've resolved what registers are saved (fp chain, return
   pc, etc.  */

#define FIRST_PARM_OFFSET(FUNDECL) 0

#define STACK_POINTER_OFFSET 1
/* IP2K stack is post-decremented, so 0(sp) is address of open space
   and 1(sp) is offset to the location avobe the forst location at which
   outgoing arguments are placed.  */

#define STACK_BOUNDARY 8

#define STACK_POINTER_REGNUM REG_SP

#define FRAME_POINTER_REGNUM REG_VFP
#define HARD_FRAME_POINTER_REGNUM REG_FP

#define ARG_POINTER_REGNUM  REG_AP

/* We don't really want to support nested functions.  But we'll crash
   in various testsuite tests if we don't at least define the register
   to contain the static chain. The return value register is about as
   bad a place as any for this.  */

#define STATIC_CHAIN_REGNUM	REG_RESULT

#define FRAME_POINTER_REQUIRED (!flag_omit_frame_pointer)

#define ELIMINABLE_REGS	{ 					\
        {ARG_POINTER_REGNUM, STACK_POINTER_REGNUM},		\
	{ARG_POINTER_REGNUM, HARD_FRAME_POINTER_REGNUM},	\
	{FRAME_POINTER_REGNUM, STACK_POINTER_REGNUM},		\
	{FRAME_POINTER_REGNUM, HARD_FRAME_POINTER_REGNUM},	\
	{HARD_FRAME_POINTER_REGNUM, STACK_POINTER_REGNUM},	\
}

#define CAN_ELIMINATE(FROM, TO) 				\
  ((FROM) == HARD_FRAME_POINTER_REGNUM				\
   ? (flag_omit_frame_pointer && !frame_pointer_needed) : 1)
/* Don't eliminate FP unless we EXPLICITLY_ASKED  */

#define INITIAL_ELIMINATION_OFFSET(FROM, TO, OFFSET) \
  ((OFFSET) = ip2k_init_elim_offset ((FROM), (TO)))

#define RETURN_ADDR_RTX(COUNT, X) \
  (((COUNT) == 0) ? gen_rtx_REG (HImode, REG_CALLH) : NULL_RTX)

#define PUSH_ROUNDING(NPUSHED) (NPUSHED)

#define RETURN_POPS_ARGS(FUNDECL,FUNTYPE,SIZE) \
  ip2k_return_pops_args ((FUNDECL), (FUNTYPE), (SIZE))

#define FUNCTION_ARG(CUM, MODE, TYPE, NAMED) 0

#define CUMULATIVE_ARGS	int

#define INIT_CUMULATIVE_ARGS(CUM, FNTYPE, LIBNAME, INDIRECT, N_NAMED_ARGS) \
  ((CUM) = 0)

#define FUNCTION_ARG_ADVANCE(CUM, MODE, TYPE, NAMED)

/* All arguments are passed on stack - do nothing here.  */

#define FUNCTION_ARG_REGNO_P(R) 0

#define FUNCTION_VALUE(VALTYPE, FUNC) 				\
   ((TYPE_MODE (VALTYPE) == QImode)				\
    ? gen_rtx_REG (TYPE_MODE (VALTYPE), REG_RESULT + 1)	\
    : gen_rtx_REG (TYPE_MODE (VALTYPE), REG_RESULT))

/* Because functions returning 'char' actually widen to 'int', we have to
   use $81 as the return location if we think we only have a 'char'.  */

#define LIBCALL_VALUE(MODE)  gen_rtx_REG ((MODE), REG_RESULT)

#define FUNCTION_VALUE_REGNO_P(N) ((N) == REG_RESULT)

#define DEFAULT_PCC_STRUCT_RETURN 0

#define EPILOGUE_USES(REGNO) 0


/*  Hmmm.  We don't actually like constants as addresses - they always need
    to be loaded to a register, except for function calls which take an
    address by immediate value.  But changing this to zero had negative
    effects, causing the compiler to get very confused....  */

#define CONSTANT_ADDRESS_P(X) CONSTANT_P (X)

#define MAX_REGS_PER_ADDRESS 1

#ifdef REG_OK_STRICT
#  define GO_IF_LEGITIMATE_ADDRESS(MODE, OPERAND, ADDR)	\
{							\
  if (legitimate_address_p ((MODE), (OPERAND), 1))	\
    goto ADDR;						\
}
#else
#  define GO_IF_LEGITIMATE_ADDRESS(MODE, OPERAND, ADDR)	\
{							\
  if (legitimate_address_p ((MODE), (OPERAND), 0))	\
    goto ADDR;						\
}
#endif

#define REG_OK_FOR_BASE_STRICT_P(X) REGNO_OK_FOR_BASE_P (REGNO (X))

#define REG_OK_FOR_BASE_NOSTRICT_P(X) 	\
  (REGNO (X) >= FIRST_PSEUDO_REGISTER 	\
   || (REGNO (X) == REG_FP)		\
   || (REGNO (X) == REG_VFP)		\
   || (REGNO (X) == REG_AP)		\
   || REG_OK_FOR_BASE_STRICT_P(X))

#ifdef REG_OK_STRICT
#  define REG_OK_FOR_BASE_P(X) REG_OK_FOR_BASE_STRICT_P (X)
#else
#  define REG_OK_FOR_BASE_P(X) REG_OK_FOR_BASE_NOSTRICT_P (X)
#endif

#define REG_OK_FOR_INDEX_P(X) 0

#define LEGITIMIZE_ADDRESS(X,OLDX,MODE,WIN)		\
do { rtx orig_x = (X);					\
  (X) = legitimize_address ((X), (OLDX), (MODE), 0);	\
  if ((X) != orig_x && memory_address_p ((MODE), (X)))	\
    goto WIN;						\
} while (0)

/* Is X a legitimate register to reload, or is it a pseudo stack-temp
   that is problematic for push_reload() ?  */

#define LRA_REG(X)						\
  (! (reg_equiv_memory_loc[REGNO (X)]				\
      && (reg_equiv_address[REGNO (X)]				\
	  || num_not_at_initial_offset)))

/* Given a register X that failed the LRA_REG test, replace X
   by its memory equivalent, find the reloads needed for THAT memory
   location and substitute that back for the higher-level reload
   that we're conducting...  */

/* WARNING: we reference 'ind_levels' and 'insn' which are local variables
   in find_reloads_address (), where the LEGITIMIZE_RELOAD_ADDRESS macro
   expands.  */

#define FRA_REG(X,MODE,OPNUM,TYPE)					\
do {									\
  rtx tem = make_memloc ((X), REGNO (X));				\
									\
  if (! strict_memory_address_p (GET_MODE (tem), XEXP (tem, 0)))	\
    {									\
      /* Note that we're doing address in address - cf. ADDR_TYPE  */	\
      find_reloads_address (GET_MODE (tem), &tem, XEXP (tem, 0),	\
 			    &XEXP (tem, 0), (OPNUM),			\
			    ADDR_TYPE (TYPE), ind_levels, insn);	\
    }									\
  (X) = tem;								\
} while (0)


/* For the IP2K, we want to be clever about picking IP vs DP for a
   base pointer since IP only directly supports a zero displacement.
   (Note that we have modified all the HI patterns to correctly handle
   IP references by manipulating iph:ipl as we fetch the pieces).  */
#define LEGITIMIZE_RELOAD_ADDRESS(X,MODE,OPNUM,TYPE,IND,WIN)		     \
{									     \
  if (GET_CODE (X) == PLUS						     \
      && REG_P (XEXP (X, 0))						     \
      && GET_CODE (XEXP (X, 1)) == CONST_INT)				     \
    {									     \
      int disp = INTVAL (XEXP (X, 1));					     \
      int fit = (disp >= 0 && disp <= (127 - 2 * GET_MODE_SIZE (MODE)));     \
      rtx reg = XEXP (X, 0);						     \
      if (!fit)								     \
	{								     \
          push_reload ((X), NULL_RTX, &(X),				     \
		       NULL, MODE_BASE_REG_CLASS (MODE), GET_MODE (X),	     \
		       VOIDmode, 0, 0, OPNUM, TYPE);			     \
	  goto WIN;							     \
	}								     \
      if (reg_equiv_memory_loc[REGNO (reg)]				     \
          && (reg_equiv_address[REGNO (reg)] || num_not_at_initial_offset))  \
        {								     \
	  rtx mem = make_memloc (reg, REGNO (reg));			     \
	  if (! strict_memory_address_p (GET_MODE (mem), XEXP (mem, 0)))     \
	    {								     \
	      /* Note that we're doing address in address - cf. ADDR_TYPE  */\
               find_reloads_address (GET_MODE (mem), &mem, XEXP (mem, 0),    \
 			            &XEXP (mem, 0), (OPNUM),		     \
			            ADDR_TYPE (TYPE), (IND), insn);	     \
	    }								     \
          push_reload (mem, NULL, &XEXP (X, 0), NULL,			     \
		       GENERAL_REGS, Pmode, VOIDmode, 0, 0,		     \
		       OPNUM, TYPE);					     \
          push_reload (X, NULL, &X, NULL,				     \
		       MODE_BASE_REG_CLASS (MODE), GET_MODE (X), VOIDmode,   \
		       0, 0, OPNUM, TYPE);				     \
	  goto WIN;							     \
	}								     \
   }									     \
}

#define GO_IF_MODE_DEPENDENT_ADDRESS(ADDR,LABEL)		\
    do {							\
	if (ip2k_mode_dependent_address (ADDR)) goto LABEL;	\
    } while (0)

#define LEGITIMATE_CONSTANT_P(X) 1

#define REGISTER_MOVE_COST(MODE, CLASS1, CLASS2) 7

#define MEMORY_MOVE_COST(MODE,CLASS,IN) 6

#define SLOW_BYTE_ACCESS 0

#define NO_FUNCTION_CSE

#define TEXT_SECTION_ASM_OP ".text"
#define DATA_SECTION_ASM_OP ".data"

#define JUMP_TABLES_IN_TEXT_SECTION 1

#define ASM_COMMENT_START " ; "

#define ASM_APP_ON "/* #APP */\n"

#define ASM_APP_OFF "/* #NOAPP */\n"

#define ASM_OUTPUT_DOUBLE(STREAM, VALUE) \
  fprintf ((STREAM), ".double %.20e\n", (VALUE))
#define ASM_OUTPUT_FLOAT(STREAM, VALUE) \
  asm_output_float ((STREAM), (VALUE))

#define ASM_OUTPUT_INT(FILE, VALUE)			\
 ( fprintf ((FILE), "\t.long "),			\
   output_addr_const ((FILE), (VALUE)),			\
   fputs ("\n", (FILE)))

#define ASM_OUTPUT_SHORT(FILE,VALUE) \
  asm_output_short ((FILE), (VALUE))
#define ASM_OUTPUT_CHAR(FILE,VALUE) \
  asm_output_char ((FILE), (VALUE))

#define ASM_OUTPUT_BYTE(FILE,VALUE) \
  asm_output_byte ((FILE), (VALUE))

#define IS_ASM_LOGICAL_LINE_SEPARATOR(C) \
  ((C) == '\n' || ((C) == '$'))

#define ASM_OUTPUT_COMMON(STREAM, NAME, SIZE, ROUNDED)	\
do {							\
     fputs ("\t.comm ", (STREAM));			\
     assemble_name ((STREAM), (NAME));			\
     fprintf ((STREAM), ",%d\n", (int)(SIZE));		\
} while (0)

#define ASM_OUTPUT_LOCAL(STREAM, NAME, SIZE, ROUNDED)	\
do {							\
     fputs ("\t.lcomm ", (STREAM));			\
     assemble_name ((STREAM), (NAME));			\
     fprintf ((STREAM), ",%d\n", (int)(SIZE));		\
} while (0)

#undef WEAK_ASM_OP
#define WEAK_ASM_OP	".weak"

#undef ASM_DECLARE_FUNCTION_SIZE
#define ASM_DECLARE_FUNCTION_SIZE(FILE, FNAME, DECL)		\
  do {								\
    if (!flag_inhibit_size_directive)				\
      ASM_OUTPUT_MEASURED_SIZE (FILE, FNAME);			\
  } while (0)

#define ESCAPES \
"\1\1\1\1\1\1\1\1btn\1fr\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\
\0\0\"\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\
\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\\\0\0\0\
\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\1\
\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\
\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\
\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\
\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1\1"
/* A table of bytes codes used by the ASM_OUTPUT_ASCII and
   ASM_OUTPUT_LIMITED_STRING macros.  Each byte in the table
   corresponds to a particular byte value [0..255].  For any
   given byte value, if the value in the corresponding table
   position is zero, the given character can be output directly.
   If the table value is 1, the byte must be output as a \ooo
   octal escape.  If the tables value is anything else, then the
   byte value should be output as a \ followed by the value
   in the table.  Note that we can use standard UN*X escape
   sequences for many control characters, but we don't use
   \a to represent BEL because some svr4 assemblers (e.g. on
   the i386) don't know about that.  Also, we don't use \v
   since some versions of gas, such as 2.2 did not accept it.  */

/* Globalizing directive for a label.  */
#define GLOBAL_ASM_OP ".global\t"

#define REGISTER_NAMES	{					\
  "$00","$01","$02","$03","iph","ipl","sph","spl",		\
  "pch","pcl","wreg","status","dph","dpl","$0e","mulh",		\
  "$10","$11","$12","$13","$14","$15","$16","$17",		\
  "$18","$19","$1a","$1b","$1c","$1d","$1e","$1f",		\
  "$20","$21","$22","$23","$24","$25","$26","$27",		\
  "$28","$29","$2a","$2b","$2c","$2d","$2e","$2f",		\
  "$30","$31","$32","$33","$34","$35","$36","$37",		\
  "$38","$39","$3a","$3b","$3c","$3d","$3e","$3f",		\
  "$40","$41","$42","$43","$44","$45","$46","$47",		\
  "$48","$49","$4a","$4b","$4c","$4d","$4e","$4f",		\
  "$50","$51","$52","$53","$54","$55","$56","$57",		\
  "$58","$59","$5a","$5b","$5c","$5d","$5e","$5f",		\
  "$60","$61","$62","$63","$64","$65","$66","$67",		\
  "$68","$69","$6a","$6b","$6c","$6d","$6e","$6f",		\
  "$70","$71","$72","$73","$74","$75","$76","$77",		\
  "$78","$79","$7a","$7b","$7c","$7d","callh","calll",		\
  "$80","$81","$82","$83","$84","$85","$86","$87",		\
  "$88","$89","$8a","$8b","$8c","$8d","$8e","$8f",		\
  "$90","$91","$92","$93","$94","$95","$96","$97",		\
  "$98","$99","$9a","$9b","$9c","$9d","$9e","$9f",		\
  "$a0","$a1","$a2","$a3","$a4","$a5","$a6","$a7",		\
  "$a8","$a9","$aa","$ab","$ac","$ad","$ae","$af",		\
  "$b0","$b1","$b2","$b3","$b4","$b5","$b6","$b7",		\
  "$b8","$b9","$ba","$bb","$bc","$bd","$be","$bf",		\
  "$c0","$c1","$c2","$c3","$c4","$c5","$c6","$c7",		\
  "$c8","$c9","$ca","$cb","$cc","$cd","$ce","$cf",		\
  "$d0","$d1","$d2","$d3","$d4","$d5","$d6","$d7",		\
  "$d8","$d9","$da","$db","$dc","$dd","$de","$df",		\
  "$e0","$e1","$e2","$e3","$e4","$e5","$e6","$e7",		\
  "$e8","$e9","$ea","$eb","$ec","$ed","$ee","$ef",		\
  "$f0","$f1","$f2","$f3","$f4","$f5","$f6","$f7",		\
  "$f8","$f9","$fa","$fb","$fc","$fd","$fe","$ff",		\
  "vfph","vfpl","vaph","vapl"}

#define PRINT_OPERAND(STREAM, X, CODE) \
  print_operand ((STREAM), (X), (CODE))

#define PRINT_OPERAND_PUNCT_VALID_P(CODE) \
  ((CODE) == '<' || (CODE) == '>')

#define PRINT_OPERAND_ADDRESS(STREAM, X) print_operand_address(STREAM, X)

/* Since register names don't have a prefix, we must preface all
   user identifiers with the '_' to prevent confusion.  */

#undef USER_LABEL_PREFIX
#define USER_LABEL_PREFIX "_"
#define LOCAL_LABEL_PREFIX ".L"

#define ASM_OUTPUT_ADDR_DIFF_ELT(STREAM, BODY, VALUE, REL)		\
  asm_fprintf ((STREAM), "\tpage\t%L%d\n\tjmp\t%L%d\n", (VALUE), (VALUE))

/* elfos.h presumes that we will want switch/case dispatch tables aligned.
   This is not so for the ip2k.  */
#undef ASM_OUTPUT_CASE_LABEL

#undef ASM_OUTPUT_ADDR_VEC_ELT
#define ASM_OUTPUT_ADDR_VEC_ELT(STREAM, VALUE)				\
  asm_fprintf ((STREAM), "\tpage\t%L%d\n\tjmp\t%L%d\n", (VALUE), (VALUE))

#define ASM_OUTPUT_ALIGN(STREAM, POWER) \
  fprintf ((STREAM), "\t.align %d\n", (POWER))

/* Since instructions are 16 bit word addresses, we should lie and claim that
   the dispatch vectors are in QImode.  Otherwise the offset into the jump
   table will be scaled by the MODE_SIZE.  */

#define CASE_VECTOR_MODE QImode

#undef WORD_REGISTER_OPERATIONS

#define MOVE_MAX 1

#define MOVE_RATIO		3
/* MOVE_RATIO is the number of move instructions that is better than a
   block move.  Make this small on the IP2k, since the code size grows very
   large with each move.  */

#define TRULY_NOOP_TRUNCATION(OUTPREC, INPREC) 1

#define Pmode HImode

#define FUNCTION_MODE HImode

#define DOLLARS_IN_IDENTIFIERS 0

extern int ip2k_reorg_in_progress;
/* Flag if we're in the middle of IP2k-specific reorganization.  */

extern int ip2k_reorg_completed;
/* Flag if we've completed our IP2k-specific reorganization.  If we have
   then we allow quite a few more tricks than before.  */

extern int ip2k_reorg_split_dimode;
extern int ip2k_reorg_split_simode;
extern int ip2k_reorg_split_qimode;
extern int ip2k_reorg_split_himode;
/* Flags for various split operations that we run in sequence.  */

extern int ip2k_reorg_merge_qimode;
/* Flag to indicate that it's safe to merge QImode operands.  */

#define TRAMPOLINE_TEMPLATE(FILE) abort ()

#define TRAMPOLINE_SIZE 4

#define INITIALIZE_TRAMPOLINE(TRAMP, FNADDR, CXT)			\
{									\
  emit_move_insn (gen_rtx_MEM (HImode, plus_constant ((TRAMP), 2)),	\
		   	   CXT);    					\
  emit_move_insn (gen_rtx_MEM (HImode, plus_constant ((TRAMP), 6)),	\
			   FNADDR);					\
}
/* Store in cc_status the expressions
   that the condition codes will describe
   after execution of an instruction whose pattern is EXP.
   Do not alter them if the instruction would not alter the cc's.  */

#define NOTICE_UPDATE_CC(EXP, INSN) (void)(0)

/* Output assembler code to FILE to increment profiler label # LABELNO
   for profiling a function entry.  */

#define FUNCTION_PROFILER(FILE, LABELNO)  \
  fprintf ((FILE), "/* profiler %d */", (LABELNO))

#undef ENDFILE_SPEC
#undef LINK_SPEC
#undef STARTFILE_SPEC

#if defined(__STDC__) || defined(ALMOST_STDC)
#define AS2(a,b,c) #a "\t" #b "," #c
#define AS1(a,b) #a "\t" #b
#else
#define AS1(a,b) "a	b"
#define AS2(a,b,c) "a	b,c"
#endif
#define OUT_AS1(a,b) output_asm_insn (AS1 (a,b), operands)
#define OUT_AS2(a,b,c) output_asm_insn (AS2 (a,b,c), operands)
#define CR_TAB "\n\t"

#define PREDICATE_CODES					\
  {"ip2k_ip_operand", {MEM}},				\
  {"ip2k_short_operand", {MEM}},			\
  {"ip2k_gen_operand", {MEM, REG, SUBREG}},		\
  {"ip2k_nonptr_operand", {REG, SUBREG}},		\
  {"ip2k_ptr_operand", {REG, SUBREG}},			\
  {"ip2k_split_dest_operand", {REG, SUBREG, MEM}}, 	\
  {"ip2k_sp_operand", {REG}},				\
  {"ip2k_nonsp_reg_operand", {REG, SUBREG}}, 		\
  {"ip2k_symbol_ref_operand", {SYMBOL_REF}}, 		\
  {"ip2k_binary_operator", {PLUS, MINUS, MULT, DIV,	\
			    UDIV, MOD, UMOD, AND, IOR,	\
			    XOR, COMPARE, ASHIFT,	\
			    ASHIFTRT, LSHIFTRT}},	\
  {"ip2k_unary_operator", {NEG, NOT, SIGN_EXTEND,	\
			   ZERO_EXTEND}},		\
  {"ip2k_unsigned_comparison_operator", {LTU, GTU, NE,	\
					 EQ, LEU, GEU}},\
  {"ip2k_signed_comparison_operator", {LT, GT, LE, GE}},

#define DWARF2_DEBUGGING_INFO 1

#define DWARF2_ASM_LINE_DEBUG_INFO	1

#define DBX_REGISTER_NUMBER(REGNO)	(REGNO)

/* Miscellaneous macros to describe machine specifics.  */

#define IS_PSEUDO_P(R)	(REGNO (R) >= FIRST_PSEUDO_REGISTER)

/* Default calculations would cause DWARF address sizes to be 2 bytes,
   but the Harvard architecture of the IP2k and the word-addressed 64k
   of instruction memory causes us to want a 32-bit "address" field.  */
#undef DWARF2_ADDR_SIZE
#define DWARF2_ADDR_SIZE	4

