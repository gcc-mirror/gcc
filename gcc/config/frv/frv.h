/* Target macros for the FRV port of GCC.
   Copyright (C) 1999-2017 Free Software Foundation, Inc.
   Contributed by Red Hat Inc.

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

#ifndef __FRV_H__
#define __FRV_H__

/* Frv general purpose macros.  */
/* Align an address.  */
#define ADDR_ALIGN(addr,align) (((addr) + (align) - 1) & ~((align) - 1))

/* Driver configuration.  */

/* -fpic and -fPIC used to imply the -mlibrary-pic multilib, but with
    FDPIC which multilib to use depends on whether FDPIC is in use or
    not.  The trick we use is to introduce -multilib-library-pic as a
    pseudo-flag that selects the library-pic multilib, and map fpic
    and fPIC to it only if fdpic is not selected.  Also, if fdpic is
    selected and no PIC/PIE options are present, we imply -fPIE.
    Otherwise, if -fpic or -fPIC are enabled and we're optimizing for
    speed, or if we have -On with n>=3, enable inlining of PLTs.  As
    for -mgprel-ro, we want to enable it by default, but not for -fpic or
    -fpie.  */

#define DRIVER_SELF_SPECS SUBTARGET_DRIVER_SELF_SPECS \
"%{mno-pack:\
   %{!mhard-float:-msoft-float}\
   %{!mmedia:-mno-media}}\
 %{!mfdpic:%{" FPIC_SPEC ": -multilib-library-pic}}\
 %{mfdpic:%{!fpic:%{!fpie:%{!fPIC:%{!fPIE:\
   	    %{!fno-pic:%{!fno-pie:%{!fno-PIC:%{!fno-PIE:-fPIE}}}}}}}} \
	  %{!mno-inline-plt:%{O*:%{!O0:%{!Os:%{" FPIC_SPEC ":-minline-plt} \
                    %{" NO_FPIC_SPEC ":%{!O:%{!O1:%{!O2:-minline-plt}}}}}}}} \
	  %{!mno-gprel-ro:%{" NO_FPIE1_AND_FPIC1_SPEC ":-mgprel-ro}}} \
"
#ifndef SUBTARGET_DRIVER_SELF_SPECS
# define SUBTARGET_DRIVER_SELF_SPECS
#endif

#undef  ASM_SPEC
#define ASM_SPEC "\
%{G*} \
%{mtomcat-stats} \
%{!mno-eflags: \
    %{mcpu=*} \
    %{mgpr-*} %{mfpr-*} \
    %{msoft-float} %{mhard-float} \
    %{mdword} %{mno-dword} \
    %{mdouble} %{mno-double} \
    %{mmedia} %{mno-media} \
    %{mmuladd} %{mno-muladd} \
    %{mpack} %{mno-pack} \
    %{mno-fdpic:-mnopic} %{mfdpic} \
    %{" FPIE1_OR_FPIC1_SPEC ":-mpic} %{" FPIE2_OR_FPIC2_SPEC ":-mPIC} %{mlibrary-pic}}"

#undef  STARTFILE_SPEC
#define STARTFILE_SPEC "crt0%O%s frvbegin%O%s"

#undef  ENDFILE_SPEC
#define ENDFILE_SPEC "frvend%O%s"


#define MASK_DEFAULT_FRV	\
  (MASK_MEDIA			\
   | MASK_DOUBLE		\
   | MASK_MULADD		\
   | MASK_DWORD			\
   | MASK_PACK)

#define MASK_DEFAULT_FR500 \
  (MASK_MEDIA | MASK_DWORD | MASK_PACK)

#define MASK_DEFAULT_FR550 \
  (MASK_MEDIA | MASK_DWORD | MASK_PACK)

#define MASK_DEFAULT_FR450	\
  (MASK_GPR_32			\
   | MASK_FPR_32		\
   | MASK_MEDIA			\
   | MASK_SOFT_FLOAT		\
   | MASK_DWORD			\
   | MASK_PACK)

#define MASK_DEFAULT_FR400	\
  (MASK_GPR_32			\
   | MASK_FPR_32		\
   | MASK_MEDIA			\
   | MASK_ACC_4			\
   | MASK_SOFT_FLOAT		\
   | MASK_DWORD			\
   | MASK_PACK)

#define MASK_DEFAULT_SIMPLE \
  (MASK_GPR_32 | MASK_SOFT_FLOAT)

/* A C string constant that tells the GCC driver program options to pass to
   `cc1'.  It can also specify how to translate options you give to GCC into
   options for GCC to pass to the `cc1'.

   Do not define this macro if it does not need to do anything.  */
/* For ABI compliance, we need to put bss data into the normal data section.  */
#define CC1_SPEC "%{G*}"

#undef	LINK_SPEC
#define LINK_SPEC "\
%{h*} %{v:-V} \
%{mfdpic:-melf32frvfd -z text} \
%{static:-dn -Bstatic} \
%{shared:-Bdynamic} \
%{symbolic:-Bsymbolic} \
%{G*}"

#undef  LIB_SPEC
#define LIB_SPEC "--start-group -lc -lsim --end-group"

#ifndef CPU_TYPE
#define CPU_TYPE		FRV_CPU_FR500
#endif

/* Run-time target specifications */

#define TARGET_CPU_CPP_BUILTINS()					\
  do									\
    {									\
      int issue_rate;							\
									\
      builtin_define ("__frv__");					\
      builtin_assert ("machine=frv");					\
									\
      issue_rate = frv_issue_rate ();					\
      if (issue_rate > 1)						\
	builtin_define_with_int_value ("__FRV_VLIW__", issue_rate);	\
      builtin_define_with_int_value ("__FRV_GPR__", NUM_GPRS);		\
      builtin_define_with_int_value ("__FRV_FPR__", NUM_FPRS);		\
      builtin_define_with_int_value ("__FRV_ACC__", NUM_ACCS);		\
									\
      switch (frv_cpu_type)						\
	{								\
	case FRV_CPU_GENERIC:						\
	  builtin_define ("__CPU_GENERIC__");				\
	  break;							\
	case FRV_CPU_FR550:						\
	  builtin_define ("__CPU_FR550__");				\
	  break;							\
	case FRV_CPU_FR500:						\
	case FRV_CPU_TOMCAT:						\
	  builtin_define ("__CPU_FR500__");				\
	  break;							\
	case FRV_CPU_FR450:						\
	  builtin_define ("__CPU_FR450__");				\
	  break;							\
	case FRV_CPU_FR405:						\
	  builtin_define ("__CPU_FR405__");				\
	  break;							\
	case FRV_CPU_FR400:						\
	  builtin_define ("__CPU_FR400__");				\
	  break;							\
	case FRV_CPU_FR300:						\
	case FRV_CPU_SIMPLE:						\
	  builtin_define ("__CPU_FR300__");				\
	  break;							\
	}								\
									\
      if (TARGET_HARD_FLOAT)						\
	builtin_define ("__FRV_HARD_FLOAT__");				\
      if (TARGET_DWORD)							\
	builtin_define ("__FRV_DWORD__");				\
      if (TARGET_FDPIC)							\
	builtin_define ("__FRV_FDPIC__");				\
      if (flag_leading_underscore > 0)					\
	builtin_define ("__FRV_UNDERSCORE__");				\
    }									\
  while (0)


#define TARGET_HAS_FPRS		(TARGET_HARD_FLOAT || TARGET_MEDIA)

#define NUM_GPRS		(TARGET_GPR_32? 32 : 64)
#define NUM_FPRS		(!TARGET_HAS_FPRS? 0 : TARGET_FPR_32? 32 : 64)
#define NUM_ACCS		(!TARGET_MEDIA? 0 : TARGET_ACC_4? 4 : 8)

/* X is a valid accumulator number if (X & ACC_MASK) == X.  */
#define ACC_MASK						\
  (!TARGET_MEDIA ? 0						\
   : TARGET_ACC_4 ? 3						\
   : frv_cpu_type == FRV_CPU_FR450 ? 11				\
   : 7)

/* Macros to identify the blend of media instructions available.  Revision 1
   is the one found on the FR500.  Revision 2 includes the changes made for
   the FR400.

   Treat the generic processor as a revision 1 machine for now, for
   compatibility with earlier releases.  */

#define TARGET_MEDIA_REV1					\
  (TARGET_MEDIA							\
   && (frv_cpu_type == FRV_CPU_GENERIC				\
       || frv_cpu_type == FRV_CPU_FR500))

#define TARGET_MEDIA_REV2					\
  (TARGET_MEDIA							\
   && (frv_cpu_type == FRV_CPU_FR400				\
       || frv_cpu_type == FRV_CPU_FR405				\
       || frv_cpu_type == FRV_CPU_FR450				\
       || frv_cpu_type == FRV_CPU_FR550))

#define TARGET_MEDIA_FR450					\
  (frv_cpu_type == FRV_CPU_FR450)

#define TARGET_FR500_FR550_BUILTINS				\
   (frv_cpu_type == FRV_CPU_FR500				\
    || frv_cpu_type == FRV_CPU_FR550)

#define TARGET_FR405_BUILTINS					\
  (frv_cpu_type == FRV_CPU_FR405				\
   || frv_cpu_type == FRV_CPU_FR450)

#ifndef HAVE_AS_TLS
#define HAVE_AS_TLS 0
#endif

#define LABEL_ALIGN_AFTER_BARRIER(LABEL) (TARGET_ALIGN_LABELS ? 3 : 0)

/* Small Data Area Support.  */
/* Maximum size of variables that go in .sdata/.sbss.
   The -msdata=foo switch also controls how small variables are handled.  */
#ifndef SDATA_DEFAULT_SIZE
#define SDATA_DEFAULT_SIZE 8
#endif


/* Storage Layout */

/* Define this macro to have the value 1 if the most significant bit in a byte
   has the lowest number; otherwise define it to have the value zero.  This
   means that bit-field instructions count from the most significant bit.  If
   the machine has no bit-field instructions, then this must still be defined,
   but it doesn't matter which value it is defined to.  This macro need not be
   a constant.

   This macro does not affect the way structure fields are packed into bytes or
   words; that is controlled by `BYTES_BIG_ENDIAN'.  */
#define BITS_BIG_ENDIAN 1

/* Define this macro to have the value 1 if the most significant byte in a word
   has the lowest number.  This macro need not be a constant.  */
#define BYTES_BIG_ENDIAN 1

/* Define this macro to have the value 1 if, in a multiword object, the most
   significant word has the lowest number.  This applies to both memory
   locations and registers; GCC fundamentally assumes that the order of
   words in memory is the same as the order in registers.  This macro need not
   be a constant.  */
#define WORDS_BIG_ENDIAN 1

/* Number of storage units in a word; normally 4.  */
#define UNITS_PER_WORD 4

/* A macro to update MODE and UNSIGNEDP when an object whose type is TYPE and
   which has the specified mode and signedness is to be stored in a register.
   This macro is only called when TYPE is a scalar type.

   On most RISC machines, which only have operations that operate on a full
   register, define this macro to set M to `word_mode' if M is an integer mode
   narrower than `BITS_PER_WORD'.  In most cases, only integer modes should be
   widened because wider-precision floating-point operations are usually more
   expensive than their narrower counterparts.

   For most machines, the macro definition does not change UNSIGNEDP.  However,
   some machines, have instructions that preferentially handle either signed or
   unsigned quantities of certain modes.  For example, on the DEC Alpha, 32-bit
   loads from memory and 32-bit add instructions sign-extend the result to 64
   bits.  On such machines, set UNSIGNEDP according to which kind of extension
   is more efficient.

   Do not define this macro if it would never modify MODE.  */
#define PROMOTE_MODE(MODE, UNSIGNEDP, TYPE)	\
  do						\
    {						\
      if (GET_MODE_CLASS (MODE) == MODE_INT	\
	  && GET_MODE_SIZE (MODE) < 4)		\
	(MODE) = SImode;			\
    }						\
  while (0)

/* Normal alignment required for function parameters on the stack, in bits.
   All stack parameters receive at least this much alignment regardless of data
   type.  On most machines, this is the same as the size of an integer.  */
#define PARM_BOUNDARY 32

/* Define this macro if you wish to preserve a certain alignment for the stack
   pointer.  The definition is a C expression for the desired alignment
   (measured in bits).

   If `PUSH_ROUNDING' is not defined, the stack will always be aligned to the
   specified boundary.  If `PUSH_ROUNDING' is defined and specifies a less
   strict alignment than `STACK_BOUNDARY', the stack may be momentarily
   unaligned while pushing arguments.  */
#define STACK_BOUNDARY 64

/* Alignment required for a function entry point, in bits.  */
#define FUNCTION_BOUNDARY 128

/* Biggest alignment that any data type can require on this machine,
   in bits.  */
#define BIGGEST_ALIGNMENT 64

/* @@@ A hack, needed because libobjc wants to use ADJUST_FIELD_ALIGN for
   some reason.  */
#ifdef IN_TARGET_LIBS
#define BIGGEST_FIELD_ALIGNMENT 64
#else
/* An expression for the alignment of a structure field FIELD if the
   alignment computed in the usual way is COMPUTED.  GCC uses this
   value instead of the value in `BIGGEST_ALIGNMENT' or
   `BIGGEST_FIELD_ALIGNMENT', if defined, for structure fields only.  */
#define ADJUST_FIELD_ALIGN(FIELD, TYPE, COMPUTED) 			\
  frv_adjust_field_align (FIELD, COMPUTED)
#endif

/* If defined, a C expression to compute the alignment for a static variable.
   TYPE is the data type, and ALIGN is the alignment that the object
   would ordinarily have.  The value of this macro is used instead of that
   alignment to align the object.

   If this macro is not defined, then ALIGN is used.

   One use of this macro is to increase alignment of medium-size data to make
   it all fit in fewer cache lines.  Another is to cause character arrays to be
   word-aligned so that `strcpy' calls that copy constants to character arrays
   can be done inline.  */
#define DATA_ALIGNMENT(TYPE, ALIGN)		\
  (TREE_CODE (TYPE) == ARRAY_TYPE		\
   && TYPE_MODE (TREE_TYPE (TYPE)) == QImode	\
   && (ALIGN) < BITS_PER_WORD ? BITS_PER_WORD : (ALIGN))

/* If defined, a C expression to compute the alignment given to a constant that
   is being placed in memory.  CONSTANT is the constant and ALIGN is the
   alignment that the object would ordinarily have.  The value of this macro is
   used instead of that alignment to align the object.

   If this macro is not defined, then ALIGN is used.

   The typical use of this macro is to increase alignment for string constants
   to be word aligned so that `strcpy' calls that copy constants can be done
   inline.  */
#define CONSTANT_ALIGNMENT(EXP, ALIGN)  \
  (TREE_CODE (EXP) == STRING_CST	\
   && (ALIGN) < BITS_PER_WORD ? BITS_PER_WORD : (ALIGN))

/* Define this macro to be the value 1 if instructions will fail to work if
   given data not on the nominal alignment.  If instructions will merely go
   slower in that case, define this macro as 0.  */
#define STRICT_ALIGNMENT 1

#define PCC_BITFIELD_TYPE_MATTERS 1


/* Layout of Source Language Data Types.  */

#define CHAR_TYPE_SIZE         8
#define SHORT_TYPE_SIZE       16
#define INT_TYPE_SIZE         32
#define LONG_TYPE_SIZE        32
#define LONG_LONG_TYPE_SIZE   64
#define FLOAT_TYPE_SIZE       32
#define DOUBLE_TYPE_SIZE      64
#define LONG_DOUBLE_TYPE_SIZE 64

/* An expression whose value is 1 or 0, according to whether the type `char'
   should be signed or unsigned by default.  The user can always override this
   default with the options `-fsigned-char' and `-funsigned-char'.  */
#define DEFAULT_SIGNED_CHAR 1

#undef  SIZE_TYPE
#define SIZE_TYPE "unsigned int"

#undef  PTRDIFF_TYPE
#define PTRDIFF_TYPE "int"

#undef  WCHAR_TYPE
#define WCHAR_TYPE "long int"

#undef  WCHAR_TYPE_SIZE
#define WCHAR_TYPE_SIZE BITS_PER_WORD


/* General purpose registers.  */
#define GPR_FIRST       0                       /* First gpr */
#define GPR_LAST        (GPR_FIRST + 63)        /* Last gpr */
#define GPR_R0          GPR_FIRST               /* R0, constant 0 */
#define GPR_FP          (GPR_FIRST + 2)         /* Frame pointer */
#define GPR_SP          (GPR_FIRST + 1)         /* Stack pointer */
						/* small data register */
#define SDA_BASE_REG    ((unsigned)(TARGET_FDPIC ? -1 : flag_pic ? PIC_REGNO : (GPR_FIRST + 16)))
#define PIC_REGNO       (GPR_FIRST + (TARGET_FDPIC?15:17))        /* PIC register.  */
#define FDPIC_FPTR_REGNO  (GPR_FIRST + 14)        /* uClinux PIC function pointer register.  */
#define FDPIC_REGNO   (GPR_FIRST + 15)        /* uClinux PIC register.  */

#define HARD_REGNO_RENAME_OK(from,to) (TARGET_FDPIC ? ((to) != FDPIC_REG) : 1)

#define OUR_FDPIC_REG	get_hard_reg_initial_val (SImode, FDPIC_REGNO)

#define FPR_FIRST       64			/* First FP reg */
#define FPR_LAST        127			/* Last  FP reg */

#define GPR_TEMP_NUM	frv_condexec_temps	/* # gprs to reserve for temps */

/* We reserve the last CR and CCR in each category to be used as a reload
   register to reload the CR/CCR registers.  This is a kludge.  */
#define CC_FIRST	128			/* First ICC/FCC reg */
#define CC_LAST		135			/* Last  ICC/FCC reg */
#define ICC_FIRST	(CC_FIRST + 4)		/* First ICC reg */
#define ICC_LAST	(CC_FIRST + 7)		/* Last  ICC reg */
#define ICC_TEMP	(CC_FIRST + 7)		/* Temporary ICC reg */
#define FCC_FIRST	(CC_FIRST)		/* First FCC reg */
#define FCC_LAST	(CC_FIRST + 3)		/* Last  FCC reg */

/* Amount to shift a value to locate a ICC or FCC register in the CCR
   register and shift it to the bottom 4 bits.  */
#define CC_SHIFT_RIGHT(REGNO) (((REGNO) - CC_FIRST) << 2)

/* Mask to isolate a single ICC/FCC value.  */
#define CC_MASK		0xf

/* Masks to isolate the various bits in an ICC field.  */
#define ICC_MASK_N	0x8	/* negative */
#define ICC_MASK_Z	0x4	/* zero */
#define ICC_MASK_V	0x2	/* overflow */
#define ICC_MASK_C	0x1	/* carry */

/* Mask to isolate the N/Z flags in an ICC.  */
#define ICC_MASK_NZ (ICC_MASK_N | ICC_MASK_Z)

/* Mask to isolate the Z/C flags in an ICC.  */
#define ICC_MASK_ZC (ICC_MASK_Z | ICC_MASK_C)

/* Masks to isolate the various bits in a FCC field.  */
#define FCC_MASK_E	0x8	/* equal */
#define FCC_MASK_L	0x4	/* less than */
#define FCC_MASK_G	0x2	/* greater than */
#define FCC_MASK_U	0x1	/* unordered */

/* For CCR registers, the machine wants CR4..CR7 to be used for integer
   code and CR0..CR3 to be used for floating point.  */
#define CR_FIRST	136			/* First CCR */
#define CR_LAST		143			/* Last  CCR */
#define CR_NUM		(CR_LAST-CR_FIRST+1)	/* # of CCRs (8) */
#define ICR_FIRST	(CR_FIRST + 4)		/* First integer CCR */
#define ICR_LAST	(CR_FIRST + 7)		/* Last  integer CCR */
#define ICR_TEMP	ICR_LAST		/* Temp  integer CCR */
#define FCR_FIRST	(CR_FIRST + 0)		/* First float CCR */
#define FCR_LAST	(CR_FIRST + 3)		/* Last  float CCR */

/* Amount to shift a value to locate a CR register in the CCCR special purpose
   register and shift it to the bottom 2 bits.  */
#define CR_SHIFT_RIGHT(REGNO) (((REGNO) - CR_FIRST) << 1)

/* Mask to isolate a single CR value.  */
#define CR_MASK		0x3

#define ACC_FIRST	144			/* First acc register */
#define ACC_LAST	155			/* Last  acc register */

#define ACCG_FIRST	156			/* First accg register */
#define ACCG_LAST	167			/* Last  accg register */

#define AP_FIRST	168			/* fake argument pointer */

#define SPR_FIRST	169
#define SPR_LAST	172
#define LR_REGNO	(SPR_FIRST)
#define LCR_REGNO	(SPR_FIRST + 1)
#define IACC_FIRST	(SPR_FIRST + 2)
#define IACC_LAST	(SPR_FIRST + 3)

#define GPR_P(R)	IN_RANGE (R, GPR_FIRST, GPR_LAST)
#define GPR_OR_AP_P(R)	(GPR_P (R) || (R) == ARG_POINTER_REGNUM)
#define FPR_P(R)	IN_RANGE (R, FPR_FIRST, FPR_LAST)
#define CC_P(R)		IN_RANGE (R, CC_FIRST, CC_LAST)
#define ICC_P(R)	IN_RANGE (R, ICC_FIRST, ICC_LAST)
#define FCC_P(R)	IN_RANGE (R, FCC_FIRST, FCC_LAST)
#define CR_P(R)		IN_RANGE (R, CR_FIRST, CR_LAST)
#define ICR_P(R)	IN_RANGE (R, ICR_FIRST, ICR_LAST)
#define FCR_P(R)	IN_RANGE (R, FCR_FIRST, FCR_LAST)
#define ACC_P(R)	IN_RANGE (R, ACC_FIRST, ACC_LAST)
#define ACCG_P(R)	IN_RANGE (R, ACCG_FIRST, ACCG_LAST)
#define SPR_P(R)	IN_RANGE (R, SPR_FIRST, SPR_LAST)

#define GPR_OR_PSEUDO_P(R)	(GPR_P (R) || (R) >= FIRST_PSEUDO_REGISTER)
#define FPR_OR_PSEUDO_P(R)	(FPR_P (R) || (R) >= FIRST_PSEUDO_REGISTER)
#define GPR_AP_OR_PSEUDO_P(R)	(GPR_OR_AP_P (R) || (R) >= FIRST_PSEUDO_REGISTER)
#define CC_OR_PSEUDO_P(R)	(CC_P (R) || (R) >= FIRST_PSEUDO_REGISTER)
#define ICC_OR_PSEUDO_P(R)	(ICC_P (R) || (R) >= FIRST_PSEUDO_REGISTER)
#define FCC_OR_PSEUDO_P(R)	(FCC_P (R) || (R) >= FIRST_PSEUDO_REGISTER)
#define CR_OR_PSEUDO_P(R)	(CR_P (R) || (R) >= FIRST_PSEUDO_REGISTER)
#define ICR_OR_PSEUDO_P(R)	(ICR_P (R) || (R) >= FIRST_PSEUDO_REGISTER)
#define FCR_OR_PSEUDO_P(R)	(FCR_P (R) || (R) >= FIRST_PSEUDO_REGISTER)
#define ACC_OR_PSEUDO_P(R)	(ACC_P (R) || (R) >= FIRST_PSEUDO_REGISTER)
#define ACCG_OR_PSEUDO_P(R)	(ACCG_P (R) || (R) >= FIRST_PSEUDO_REGISTER)

#define MAX_STACK_IMMEDIATE_OFFSET 2047


/* Register Basics.  */

/* Number of hardware registers known to the compiler.  They receive numbers 0
   through `FIRST_PSEUDO_REGISTER-1'; thus, the first pseudo register's number
   really is assigned the number `FIRST_PSEUDO_REGISTER'.  */
#define FIRST_PSEUDO_REGISTER (SPR_LAST + 1)

/* The first/last register that can contain the arguments to a function.  */
#define FIRST_ARG_REGNUM	(GPR_FIRST + 8)
#define LAST_ARG_REGNUM		(FIRST_ARG_REGNUM + FRV_NUM_ARG_REGS - 1)

/* Registers used by the exception handling functions.  These should be
   registers that are not otherwise used by the calling sequence.  */
#define FIRST_EH_REGNUM		14
#define LAST_EH_REGNUM		15

/* Scratch registers used in the prologue, epilogue and thunks.
   OFFSET_REGNO is for loading constant addends that are too big for a
   single instruction.  TEMP_REGNO is used for transferring SPRs to and from
   the stack, and various other activities.  */
#define OFFSET_REGNO		4
#define TEMP_REGNO		5

/* Registers used in the prologue.  OLD_SP_REGNO is the old stack pointer,
   which is sometimes used to set up the frame pointer.  */
#define OLD_SP_REGNO		6

/* Registers used in the epilogue.  STACKADJ_REGNO stores the exception
   handler's stack adjustment.  */
#define STACKADJ_REGNO		6

/* Registers used in thunks.  JMP_REGNO is used for loading the target
   address.  */
#define JUMP_REGNO		6

#define EH_RETURN_DATA_REGNO(N)	((N) <= (LAST_EH_REGNUM - FIRST_EH_REGNUM)? \
				 (N) + FIRST_EH_REGNUM : INVALID_REGNUM)
#define EH_RETURN_STACKADJ_RTX	gen_rtx_REG (SImode, STACKADJ_REGNO)
#define EH_RETURN_HANDLER_RTX   RETURN_ADDR_RTX (0, frame_pointer_rtx)

#define EPILOGUE_USES(REGNO) ((REGNO) == LR_REGNO)

/* An initializer that says which registers are used for fixed purposes all
   throughout the compiled code and are therefore not available for general
   allocation.  These would include the stack pointer, the frame pointer
   (except on machines where that can be used as a general register when no
   frame pointer is needed), the program counter on machines where that is
   considered one of the addressable registers, and any other numbered register
   with a standard use.

   This information is expressed as a sequence of numbers, separated by commas
   and surrounded by braces.  The Nth number is 1 if register N is fixed, 0
   otherwise.

   The table initialized from this macro, and the table initialized by the
   following one, may be overridden at run time either automatically, by the
   actions of the macro `CONDITIONAL_REGISTER_USAGE', or by the user with the
   command options `-ffixed-REG', `-fcall-used-REG' and `-fcall-saved-REG'.  */

/* gr0  -- Hard Zero
   gr1  -- Stack Pointer
   gr2  -- Frame Pointer
   gr3  -- Hidden Parameter
   gr16 -- Small Data reserved
   gr17 -- Pic reserved
   gr28 -- OS reserved
   gr29 -- OS reserved
   gr30 -- OS reserved
   gr31 -- OS reserved
   cr3  -- reserved to reload FCC registers.
   cr7  -- reserved to reload ICC registers.  */
#define FIXED_REGISTERS							\
{	/* Integer Registers */						\
	1, 1, 1, 1, 0, 0, 0, 0,		/* 000-007, gr0  - gr7  */	\
	0, 0, 0, 0, 0, 0, 0, 0,		/* 008-015, gr8  - gr15 */	\
	1, 1, 0, 0, 0, 0, 0, 0,		/* 016-023, gr16 - gr23 */	\
	0, 0, 0, 0, 1, 1, 1, 1,		/* 024-031, gr24 - gr31 */	\
	0, 0, 0, 0, 0, 0, 0, 0,		/* 032-039, gr32 - gr39 */	\
	0, 0, 0, 0, 0, 0, 0, 0,		/* 040-040, gr48 - gr47 */	\
	0, 0, 0, 0, 0, 0, 0, 0,		/* 048-055, gr48 - gr55 */	\
	0, 0, 0, 0, 0, 0, 0, 0,		/* 056-063, gr56 - gr63 */	\
	/* Float Registers */						\
	0, 0, 0, 0, 0, 0, 0, 0,		/* 064-071, fr0  - fr7  */	\
	0, 0, 0, 0, 0, 0, 0, 0,		/* 072-079, fr8  - fr15 */	\
	0, 0, 0, 0, 0, 0, 0, 0,		/* 080-087, fr16 - fr23 */	\
	0, 0, 0, 0, 0, 0, 0, 0,		/* 088-095, fr24 - fr31 */	\
	0, 0, 0, 0, 0, 0, 0, 0,		/* 096-103, fr32 - fr39 */	\
	0, 0, 0, 0, 0, 0, 0, 0,		/* 104-111, fr48 - fr47 */	\
	0, 0, 0, 0, 0, 0, 0, 0,		/* 112-119, fr48 - fr55 */	\
	0, 0, 0, 0, 0, 0, 0, 0,		/* 120-127, fr56 - fr63 */	\
	/* Condition Code Registers */					\
	0, 0, 0, 0,			/* 128-131, fcc0 - fcc3  */	\
	0, 0, 0, 1,			/* 132-135, icc0 - icc3 */	\
	/* Conditional execution Registers (CCR) */			\
	0, 0, 0, 0, 0, 0, 0, 1,		/* 136-143, cr0 - cr7 */	\
	/* Accumulators */						\
	1, 1, 1, 1, 1, 1, 1, 1,		/* 144-151, acc0  - acc7 */	\
	1, 1, 1, 1,			/* 152-155, acc8  - acc11 */	\
	1, 1, 1, 1, 1, 1, 1, 1,		/* 156-163, accg0 - accg7 */	\
	1, 1, 1, 1,			/* 164-167, accg8 - accg11 */	\
	/* Other registers */						\
	1,				/* 168, AP   - fake arg ptr */	\
	1,				/* 169, LR   - Link register*/	\
	0,				/* 170, LCR  - Loop count reg*/	\
	1, 1				/* 171-172, iacc0 */		\
}

/* Like `FIXED_REGISTERS' but has 1 for each register that is clobbered (in
   general) by function calls as well as for fixed registers.  This macro
   therefore identifies the registers that are not available for general
   allocation of values that must live across function calls.

   If a register has 0 in `CALL_USED_REGISTERS', the compiler automatically
   saves it on function entry and restores it on function exit, if the register
   is used within the function.  */
#define CALL_USED_REGISTERS						\
{	/* Integer Registers */						\
	1, 1, 1, 1, 1, 1, 1, 1,		/* 000-007, gr0  - gr7  */	\
	1, 1, 1, 1, 1, 1, 1, 1,		/* 008-015, gr8  - gr15 */	\
	1, 1, 0, 0, 0, 0, 0, 0,		/* 016-023, gr16 - gr23 */	\
	0, 0, 0, 0, 1, 1, 1, 1,		/* 024-031, gr24 - gr31 */	\
	1, 1, 1, 1, 1, 1, 1, 1,		/* 032-039, gr32 - gr39 */	\
	1, 1, 1, 1, 1, 1, 1, 1,		/* 040-040, gr48 - gr47 */	\
	0, 0, 0, 0, 0, 0, 0, 0,		/* 048-055, gr48 - gr55 */	\
	0, 0, 0, 0, 0, 0, 0, 0,		/* 056-063, gr56 - gr63 */	\
	/* Float Registers */						\
	1, 1, 1, 1, 1, 1, 1, 1,		/* 064-071, fr0  - fr7  */	\
	1, 1, 1, 1, 1, 1, 1, 1,		/* 072-079, fr8  - fr15 */	\
	0, 0, 0, 0, 0, 0, 0, 0,		/* 080-087, fr16 - fr23 */	\
	0, 0, 0, 0, 0, 0, 0, 0,		/* 088-095, fr24 - fr31 */	\
	1, 1, 1, 1, 1, 1, 1, 1,		/* 096-103, fr32 - fr39 */	\
	1, 1, 1, 1, 1, 1, 1, 1,		/* 104-111, fr48 - fr47 */	\
	0, 0, 0, 0, 0, 0, 0, 0,		/* 112-119, fr48 - fr55 */	\
	0, 0, 0, 0, 0, 0, 0, 0,		/* 120-127, fr56 - fr63 */	\
	/* Condition Code Registers */					\
	1, 1, 1, 1,			/* 128-131, fcc0 - fcc3 */	\
	1, 1, 1, 1,			/* 132-135, icc0 - icc3  */	\
	/* Conditional execution Registers (CCR) */			\
	1, 1, 1, 1, 1, 1, 1, 1,		/* 136-143, cr0 - cr7 */	\
	/* Accumulators */						\
	1, 1, 1, 1, 1, 1, 1, 1,		/* 144-151, acc0 - acc7 */	\
	1, 1, 1, 1,			/* 152-155, acc8 - acc11 */	\
	1, 1, 1, 1, 1, 1, 1, 1,		/* 156-163, accg0 - accg7 */	\
	1, 1, 1, 1,			/* 164-167, accg8 - accg11 */	\
	/* Other registers */						\
	1,				/* 168, AP  - fake arg ptr */	\
	1,				/* 169, LR  - Link register*/	\
	1,				/* 170, LCR - Loop count reg */	\
	1, 1				/* 171-172, iacc0 */		\
}


/* Order of allocation of registers.  */

/* If defined, an initializer for a vector of integers, containing the numbers
   of hard registers in the order in which GCC should prefer to use them
   (from most preferred to least).

   If this macro is not defined, registers are used lowest numbered first (all
   else being equal).

   One use of this macro is on machines where the highest numbered registers
   must always be saved and the save-multiple-registers instruction supports
   only sequences of consecutive registers.  On such machines, define
   `REG_ALLOC_ORDER' to be an initializer that lists the highest numbered
   allocatable register first.  */

/* On the FRV, allocate GR16 and GR17 after other saved registers so that we
   have a better chance of allocating 2 registers at a time and can use the
   double word load/store instructions in the prologue.  */
#define REG_ALLOC_ORDER							\
{									\
  /* volatile registers */						\
  GPR_FIRST  +  4, GPR_FIRST  +  5, GPR_FIRST  +  6, GPR_FIRST 	+  7,	\
  GPR_FIRST  +  8, GPR_FIRST  +  9, GPR_FIRST  + 10, GPR_FIRST 	+ 11,	\
  GPR_FIRST  + 12, GPR_FIRST  + 13, GPR_FIRST  + 14, GPR_FIRST 	+ 15,	\
  GPR_FIRST  + 32, GPR_FIRST  + 33, GPR_FIRST  + 34, GPR_FIRST 	+ 35,	\
  GPR_FIRST  + 36, GPR_FIRST  + 37, GPR_FIRST  + 38, GPR_FIRST 	+ 39,	\
  GPR_FIRST  + 40, GPR_FIRST  + 41, GPR_FIRST  + 42, GPR_FIRST 	+ 43,	\
  GPR_FIRST  + 44, GPR_FIRST  + 45, GPR_FIRST  + 46, GPR_FIRST 	+ 47,	\
									\
  FPR_FIRST  +  0, FPR_FIRST  +  1, FPR_FIRST  +  2, FPR_FIRST 	+  3,	\
  FPR_FIRST  +  4, FPR_FIRST  +  5, FPR_FIRST  +  6, FPR_FIRST 	+  7,	\
  FPR_FIRST  +  8, FPR_FIRST  +  9, FPR_FIRST  + 10, FPR_FIRST 	+ 11,	\
  FPR_FIRST  + 12, FPR_FIRST  + 13, FPR_FIRST  + 14, FPR_FIRST 	+ 15,	\
  FPR_FIRST  + 32, FPR_FIRST  + 33, FPR_FIRST  + 34, FPR_FIRST 	+ 35,	\
  FPR_FIRST  + 36, FPR_FIRST  + 37, FPR_FIRST  + 38, FPR_FIRST 	+ 39,	\
  FPR_FIRST  + 40, FPR_FIRST  + 41, FPR_FIRST  + 42, FPR_FIRST 	+ 43,	\
  FPR_FIRST  + 44, FPR_FIRST  + 45, FPR_FIRST  + 46, FPR_FIRST 	+ 47,	\
									\
  ICC_FIRST  +  0, ICC_FIRST  +  1, ICC_FIRST  +  2, ICC_FIRST 	+  3,	\
  FCC_FIRST  +  0, FCC_FIRST  +  1, FCC_FIRST  +  2, FCC_FIRST 	+  3,	\
  CR_FIRST   +  0, CR_FIRST   +  1, CR_FIRST   +  2, CR_FIRST  	+  3,	\
  CR_FIRST   +  4, CR_FIRST   +  5, CR_FIRST   +  6, CR_FIRST  	+  7,	\
									\
  /* saved registers */							\
  GPR_FIRST  + 18, GPR_FIRST  + 19,					\
  GPR_FIRST  + 20, GPR_FIRST  + 21, GPR_FIRST  + 22, GPR_FIRST 	+ 23,	\
  GPR_FIRST  + 24, GPR_FIRST  + 25, GPR_FIRST  + 26, GPR_FIRST 	+ 27,	\
  GPR_FIRST  + 48, GPR_FIRST  + 49, GPR_FIRST  + 50, GPR_FIRST 	+ 51,	\
  GPR_FIRST  + 52, GPR_FIRST  + 53, GPR_FIRST  + 54, GPR_FIRST 	+ 55,	\
  GPR_FIRST  + 56, GPR_FIRST  + 57, GPR_FIRST  + 58, GPR_FIRST 	+ 59,	\
  GPR_FIRST  + 60, GPR_FIRST  + 61, GPR_FIRST  + 62, GPR_FIRST 	+ 63,	\
  GPR_FIRST  + 16, GPR_FIRST  + 17,					\
									\
  FPR_FIRST  + 16, FPR_FIRST  + 17, FPR_FIRST  + 18, FPR_FIRST 	+ 19,	\
  FPR_FIRST  + 20, FPR_FIRST  + 21, FPR_FIRST  + 22, FPR_FIRST 	+ 23,	\
  FPR_FIRST  + 24, FPR_FIRST  + 25, FPR_FIRST  + 26, FPR_FIRST 	+ 27,	\
  FPR_FIRST  + 28, FPR_FIRST  + 29, FPR_FIRST  + 30, FPR_FIRST 	+ 31,	\
  FPR_FIRST  + 48, FPR_FIRST  + 49, FPR_FIRST  + 50, FPR_FIRST 	+ 51,	\
  FPR_FIRST  + 52, FPR_FIRST  + 53, FPR_FIRST  + 54, FPR_FIRST 	+ 55,	\
  FPR_FIRST  + 56, FPR_FIRST  + 57, FPR_FIRST  + 58, FPR_FIRST 	+ 59,	\
  FPR_FIRST  + 60, FPR_FIRST  + 61, FPR_FIRST  + 62, FPR_FIRST 	+ 63,	\
									\
  /* special or fixed registers */					\
  GPR_FIRST  +  0, GPR_FIRST  +  1, GPR_FIRST  +  2, GPR_FIRST 	+  3,	\
  GPR_FIRST  + 28, GPR_FIRST  + 29, GPR_FIRST  + 30, GPR_FIRST 	+ 31,	\
  ACC_FIRST  +  0, ACC_FIRST  +  1, ACC_FIRST  +  2, ACC_FIRST 	+  3,	\
  ACC_FIRST  +  4, ACC_FIRST  +  5, ACC_FIRST  +  6, ACC_FIRST 	+  7,	\
  ACC_FIRST  +  8, ACC_FIRST  +  9, ACC_FIRST  + 10, ACC_FIRST 	+ 11,	\
  ACCG_FIRST +  0, ACCG_FIRST +  1, ACCG_FIRST +  2, ACCG_FIRST	+  3,	\
  ACCG_FIRST +  4, ACCG_FIRST +  5, ACCG_FIRST +  6, ACCG_FIRST	+  7,	\
  ACCG_FIRST +  8, ACCG_FIRST +  9, ACCG_FIRST + 10, ACCG_FIRST	+ 11,	\
  AP_FIRST, 	   LR_REGNO,       LCR_REGNO,				\
  IACC_FIRST +  0, IACC_FIRST +  1					\
}


/* How Values Fit in Registers.  */

/* A C expression for the number of consecutive hard registers, starting at
   register number REGNO, required to hold a value of mode MODE.

   On a machine where all registers are exactly one word, a suitable definition
   of this macro is

        #define HARD_REGNO_NREGS(REGNO, MODE)            \
           ((GET_MODE_SIZE (MODE) + UNITS_PER_WORD - 1)  \
            / UNITS_PER_WORD))  */

/* On the FRV, make the CC modes take 3 words in the integer registers, so that
   we can build the appropriate instructions to properly reload the values.  */
#define HARD_REGNO_NREGS(REGNO, MODE) frv_hard_regno_nregs (REGNO, MODE)

/* A C expression that is nonzero if it is desirable to choose register
   allocation so as to avoid move instructions between a value of mode MODE1
   and a value of mode MODE2.

   If `TARGET_HARD_REGNO_MODE_OK (R, MODE1)' and
   `TARGET_HARD_REGNO_MODE_OK (R, MODE2)' are ever different for any R,
   then `MODES_TIEABLE_P (MODE1, MODE2)' must be zero.  */
#define MODES_TIEABLE_P(MODE1, MODE2) (MODE1 == MODE2)

/* Define this macro if the compiler should avoid copies to/from CCmode
   registers.  You should only define this macro if support fo copying to/from
   CCmode is incomplete.  */
#define AVOID_CCMODE_COPIES


/* Register Classes.  */

/* An enumeral type that must be defined with all the register class names as
   enumeral values.  `NO_REGS' must be first.  `ALL_REGS' must be the last
   register class, followed by one more enumeral value, `LIM_REG_CLASSES',
   which is not a register class but rather tells how many classes there are.

   Each register class has a number, which is the value of casting the class
   name to type `int'.  The number serves as an index in many of the tables
   described below.  */
enum reg_class
{
  NO_REGS,
  ICC_REGS,
  FCC_REGS,
  CC_REGS,
  ICR_REGS,
  FCR_REGS,
  CR_REGS,
  LCR_REG,
  LR_REG,
  GR8_REGS,
  GR9_REGS,
  GR89_REGS,
  FDPIC_REGS,
  FDPIC_FPTR_REGS,
  FDPIC_CALL_REGS,
  SPR_REGS,
  QUAD_ACC_REGS,
  ACCG_REGS,
  QUAD_FPR_REGS,
  QUAD_REGS,
  GPR_REGS,
  ALL_REGS,
  LIM_REG_CLASSES
};

#define GENERAL_REGS GPR_REGS

/* The number of distinct register classes, defined as follows:

        #define N_REG_CLASSES (int) LIM_REG_CLASSES  */
#define N_REG_CLASSES ((int) LIM_REG_CLASSES)

/* An initializer containing the names of the register classes as C string
   constants.  These names are used in writing some of the debugging dumps.  */
#define REG_CLASS_NAMES {						\
   "NO_REGS",								\
   "ICC_REGS",								\
   "FCC_REGS",								\
   "CC_REGS",								\
   "ICR_REGS",								\
   "FCR_REGS",								\
   "CR_REGS",								\
   "LCR_REG",								\
   "LR_REG",								\
   "GR8_REGS",                                                          \
   "GR9_REGS",                                                          \
   "GR89_REGS",                                                         \
   "FDPIC_REGS",							\
   "FDPIC_FPTR_REGS",							\
   "FDPIC_CALL_REGS",							\
   "SPR_REGS",								\
   "QUAD_ACC_REGS",							\
   "ACCG_REGS",								\
   "QUAD_FPR_REGS",							\
   "QUAD_REGS",								\
   "GPR_REGS",								\
   "ALL_REGS"								\
}

/* An initializer containing the contents of the register classes, as integers
   which are bit masks.  The Nth integer specifies the contents of class N.
   The way the integer MASK is interpreted is that register R is in the class
   if `MASK & (1 << R)' is 1.

   When the machine has more than 32 registers, an integer does not suffice.
   Then the integers are replaced by sub-initializers, braced groupings
   containing several integers.  Each sub-initializer must be suitable as an
   initializer for the type `HARD_REG_SET' which is defined in
   `hard-reg-set.h'.  */
#define REG_CLASS_CONTENTS						       \
{  /* gr0-gr31 gr32-gr63  fr0-fr31   fr32-fr-63 cc/ccr/acc ap/spr */	       \
  { 0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x0}, /* NO_REGS  */\
  { 0x00000000,0x00000000,0x00000000,0x00000000,0x000000f0,0x0}, /* ICC_REGS */\
  { 0x00000000,0x00000000,0x00000000,0x00000000,0x0000000f,0x0}, /* FCC_REGS */\
  { 0x00000000,0x00000000,0x00000000,0x00000000,0x000000ff,0x0}, /* CC_REGS  */\
  { 0x00000000,0x00000000,0x00000000,0x00000000,0x0000f000,0x0}, /* ICR_REGS */\
  { 0x00000000,0x00000000,0x00000000,0x00000000,0x00000f00,0x0}, /* FCR_REGS */\
  { 0x00000000,0x00000000,0x00000000,0x00000000,0x0000ff00,0x0}, /* CR_REGS  */\
  { 0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x400}, /* LCR_REGS */\
  { 0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x200}, /* LR_REGS  */\
  { 0x00000100,0x00000000,0x00000000,0x00000000,0x00000000,0x0}, /* GR8_REGS */\
  { 0x00000200,0x00000000,0x00000000,0x00000000,0x00000000,0x0}, /* GR9_REGS */\
  { 0x00000300,0x00000000,0x00000000,0x00000000,0x00000000,0x0}, /* GR89_REGS */\
  { 0x00008000,0x00000000,0x00000000,0x00000000,0x00000000,0x0}, /* FDPIC_REGS */\
  { 0x00004000,0x00000000,0x00000000,0x00000000,0x00000000,0x0}, /* FDPIC_FPTR_REGS */\
  { 0x0000c000,0x00000000,0x00000000,0x00000000,0x00000000,0x0}, /* FDPIC_CALL_REGS */\
  { 0x00000000,0x00000000,0x00000000,0x00000000,0x00000000,0x1e00}, /* SPR_REGS */\
  { 0x00000000,0x00000000,0x00000000,0x00000000,0x0fff0000,0x0}, /* QUAD_ACC */\
  { 0x00000000,0x00000000,0x00000000,0x00000000,0xf0000000,0xff}, /* ACCG_REGS*/\
  { 0x00000000,0x00000000,0xffffffff,0xffffffff,0x00000000,0x0}, /* QUAD_FPR */\
  { 0x0ffffffc,0xffffffff,0x00000000,0x00000000,0x00000000,0x0}, /* QUAD_REGS*/\
  { 0xffffffff,0xffffffff,0x00000000,0x00000000,0x00000000,0x100}, /* GPR_REGS */\
  { 0xffffffff,0xffffffff,0xffffffff,0xffffffff,0xffffffff,0x1fff}, /* ALL_REGS */\
}

#define EVEN_ACC_REGS   QUAD_ACC_REGS
#define ACC_REGS        QUAD_ACC_REGS
#define FEVEN_REGS      QUAD_FPR_REGS
#define FPR_REGS        QUAD_FPR_REGS
#define EVEN_REGS       QUAD_REGS

/* A C expression whose value is a register class containing hard register
   REGNO.  In general there is more than one such class; choose a class which
   is "minimal", meaning that no smaller class also contains the register.  */

extern enum reg_class regno_reg_class[];
#define REGNO_REG_CLASS(REGNO) regno_reg_class [REGNO]

/* A macro whose definition is the name of the class to which a valid base
   register must belong.  A base register is one used in an address which is
   the register value plus a displacement.  */
#define BASE_REG_CLASS GPR_REGS

/* A macro whose definition is the name of the class to which a valid index
   register must belong.  An index register is one used in an address where its
   value is either multiplied by a scale factor or added to another register
   (as well as added to a displacement).  */
#define INDEX_REG_CLASS GPR_REGS

/* A C expression which is nonzero if register number NUM is suitable for use
   as a base register in operand addresses.  It may be either a suitable hard
   register or a pseudo register that has been allocated such a hard register.  */
#define REGNO_OK_FOR_BASE_P(NUM)           \
  ((NUM) < FIRST_PSEUDO_REGISTER           \
   ? GPR_P (NUM)                           \
   : (reg_renumber [NUM] >= 0 && GPR_P (reg_renumber [NUM])))

/* A C expression which is nonzero if register number NUM is suitable for use
   as an index register in operand addresses.  It may be either a suitable hard
   register or a pseudo register that has been allocated such a hard register.

   The difference between an index register and a base register is that the
   index register may be scaled.  If an address involves the sum of two
   registers, neither one of them scaled, then either one may be labeled the
   "base" and the other the "index"; but whichever labeling is used must fit
   the machine's constraints of which registers may serve in each capacity.
   The compiler will try both labelings, looking for one that is valid, and
   will reload one or both registers only if neither labeling works.  */
#define REGNO_OK_FOR_INDEX_P(NUM)                                       \
  ((NUM) < FIRST_PSEUDO_REGISTER                                        \
   ? GPR_P (NUM)                                                        \
   : (reg_renumber [NUM] >= 0 && GPR_P (reg_renumber [NUM])))

#define SECONDARY_INPUT_RELOAD_CLASS(CLASS, MODE, X) \
  frv_secondary_reload_class (CLASS, MODE, X)

#define SECONDARY_OUTPUT_RELOAD_CLASS(CLASS, MODE, X) \
  frv_secondary_reload_class (CLASS, MODE, X)

/* A C expression for the maximum number of consecutive registers of
   class CLASS needed to hold a value of mode MODE.

   This is closely related to the macro `HARD_REGNO_NREGS'.  In fact, the value
   of the macro `CLASS_MAX_NREGS (CLASS, MODE)' should be the maximum value of
   `HARD_REGNO_NREGS (REGNO, MODE)' for all REGNO values in the class CLASS.

   This macro helps control the handling of multiple-word values in
   the reload pass.

   This declaration is required.  */
#define CLASS_MAX_NREGS(CLASS, MODE) frv_class_max_nregs (CLASS, MODE)

#define ZERO_P(x) (x == CONST0_RTX (GET_MODE (x)))


/* Basic Stack Layout.  */

/* Structure to describe information about a saved range of registers */

typedef struct frv_stack_regs {
  const char * name;		/* name of the register ranges */
  int first;			/* first register in the range */
  int last;			/* last register in the range */
  int size_1word;		/* # of bytes to be stored via 1 word stores */
  int size_2words;		/* # of bytes to be stored via 2 word stores */
  unsigned char field_p;	/* true if the registers are a single SPR */
  unsigned char dword_p;	/* true if we can do dword stores */
  unsigned char special_p;	/* true if the regs have a fixed save loc.  */
} frv_stack_regs_t;

/* Register ranges to look into saving.  */
#define STACK_REGS_GPR		0	/* Gprs (normally gr16..gr31, gr48..gr63) */
#define STACK_REGS_FPR		1	/* Fprs (normally fr16..fr31, fr48..fr63) */
#define STACK_REGS_LR		2	/* LR register */
#define STACK_REGS_CC		3	/* CCrs (normally not saved) */
#define STACK_REGS_LCR		5	/* lcr register */
#define STACK_REGS_STDARG	6	/* stdarg registers */
#define STACK_REGS_STRUCT	7	/* structure return (gr3) */
#define STACK_REGS_FP		8	/* FP register */
#define STACK_REGS_MAX		9	/* # of register ranges */

/* Values for save_p field.  */
#define REG_SAVE_NO_SAVE	0	/* register not saved */
#define REG_SAVE_1WORD		1	/* save the register */
#define REG_SAVE_2WORDS		2	/* save register and register+1 */

/* Structure used to define the frv stack.  */

typedef struct frv_stack {
  int total_size;		/* total bytes allocated for stack */
  int vars_size;		/* variable save area size */
  int parameter_size;		/* outgoing parameter size */
  int stdarg_size;		/* size of regs needed to be saved for stdarg */
  int regs_size;		/* size of the saved registers */
  int regs_size_1word;		/* # of bytes to be stored via 1 word stores */
  int regs_size_2words;		/* # of bytes to be stored via 2 word stores */
  int header_size;		/* size of the old FP, struct ret., LR save */
  int pretend_size;		/* size of pretend args */
  int vars_offset;		/* offset to save local variables from new SP*/
  int regs_offset;		/* offset to save registers from new SP */
				/* register range information */
  frv_stack_regs_t regs[STACK_REGS_MAX];
				/* offset to store each register */
  int reg_offset[FIRST_PSEUDO_REGISTER];
				/* whether to save register (& reg+1) */
  unsigned char save_p[FIRST_PSEUDO_REGISTER];
} frv_stack_t;

/* Define this macro if pushing a word onto the stack moves the stack pointer
   to a smaller address.  */
#define STACK_GROWS_DOWNWARD 1

/* Define this macro to nonzero if the addresses of local variable slots
   are at negative offsets from the frame pointer.  */
#define FRAME_GROWS_DOWNWARD 1

/* Offset from the frame pointer to the first local variable slot to be
   allocated.

   If `FRAME_GROWS_DOWNWARD', find the next slot's offset by subtracting the
   first slot's length from `STARTING_FRAME_OFFSET'.  Otherwise, it is found by
   adding the length of the first slot to the value `STARTING_FRAME_OFFSET'.  */
#define STARTING_FRAME_OFFSET 0

/* Offset from the stack pointer register to the first location at which
   outgoing arguments are placed.  If not specified, the default value of zero
   is used.  This is the proper value for most machines.

   If `ARGS_GROW_DOWNWARD', this is the offset to the location above the first
   location at which outgoing arguments are placed.  */
#define STACK_POINTER_OFFSET 0

/* Offset from the argument pointer register to the first argument's address.
   On some machines it may depend on the data type of the function.

   If `ARGS_GROW_DOWNWARD', this is the offset to the location above the first
   argument's address.  */
#define FIRST_PARM_OFFSET(FUNDECL) 0

/* A C expression whose value is RTL representing the address in a stack frame
   where the pointer to the caller's frame is stored.  Assume that FRAMEADDR is
   an RTL expression for the address of the stack frame itself.

   If you don't define this macro, the default is to return the value of
   FRAMEADDR--that is, the stack frame address is also the address of the stack
   word that points to the previous frame.  */
#define DYNAMIC_CHAIN_ADDRESS(FRAMEADDR) frv_dynamic_chain_address (FRAMEADDR)

/* A C expression whose value is RTL representing the value of the return
   address for the frame COUNT steps up from the current frame, after the
   prologue.  FRAMEADDR is the frame pointer of the COUNT frame, or the frame
   pointer of the COUNT - 1 frame if `RETURN_ADDR_IN_PREVIOUS_FRAME' is
   defined.

   The value of the expression must always be the correct address when COUNT is
   zero, but may be `NULL_RTX' if there is not way to determine the return
   address of other frames.  */
#define RETURN_ADDR_RTX(COUNT, FRAMEADDR) frv_return_addr_rtx (COUNT, FRAMEADDR)

#define RETURN_POINTER_REGNUM LR_REGNO

/* A C expression whose value is RTL representing the location of the incoming
   return address at the beginning of any function, before the prologue.  This
   RTL is either a `REG', indicating that the return value is saved in `REG',
   or a `MEM' representing a location in the stack.

   You only need to define this macro if you want to support call frame
   debugging information like that provided by DWARF 2.  */
#define INCOMING_RETURN_ADDR_RTX gen_rtx_REG (SImode, RETURN_POINTER_REGNUM)


/* Register That Address the Stack Frame.  */

/* The register number of the stack pointer register, which must also be a
   fixed register according to `FIXED_REGISTERS'.  On most machines, the
   hardware determines which register this is.  */
#define STACK_POINTER_REGNUM (GPR_FIRST + 1)

/* The register number of the frame pointer register, which is used to access
   automatic variables in the stack frame.  On some machines, the hardware
   determines which register this is.  On other machines, you can choose any
   register you wish for this purpose.  */
#define FRAME_POINTER_REGNUM (GPR_FIRST + 2)

/* The register number of the arg pointer register, which is used to access the
   function's argument list.  On some machines, this is the same as the frame
   pointer register.  On some machines, the hardware determines which register
   this is.  On other machines, you can choose any register you wish for this
   purpose.  If this is not the same register as the frame pointer register,
   then you must mark it as a fixed register according to `FIXED_REGISTERS', or
   arrange to be able to eliminate it.  */

/* On frv this is a fake register that is eliminated in
   terms of either the frame pointer or stack pointer.  */
#define ARG_POINTER_REGNUM AP_FIRST

/* Register numbers used for passing a function's static chain pointer.  If
   register windows are used, the register number as seen by the called
   function is `STATIC_CHAIN_INCOMING_REGNUM', while the register number as
   seen by the calling function is `STATIC_CHAIN_REGNUM'.  If these registers
   are the same, `STATIC_CHAIN_INCOMING_REGNUM' need not be defined.

   The static chain register need not be a fixed register.

   If the static chain is passed in memory, these macros should not be defined;
   instead, the next two macros should be defined.  */
#define STATIC_CHAIN_REGNUM (GPR_FIRST + 7)
#define STATIC_CHAIN_INCOMING_REGNUM (GPR_FIRST + 7)


/* Eliminating the Frame Pointer and the Arg Pointer.  */

/* If defined, this macro specifies a table of register pairs used to eliminate
   unneeded registers that point into the stack frame.  If it is not defined,
   the only elimination attempted by the compiler is to replace references to
   the frame pointer with references to the stack pointer.

   The definition of this macro is a list of structure initializations, each of
   which specifies an original and replacement register.

   On some machines, the position of the argument pointer is not known until
   the compilation is completed.  In such a case, a separate hard register must
   be used for the argument pointer.  This register can be eliminated by
   replacing it with either the frame pointer or the argument pointer,
   depending on whether or not the frame pointer has been eliminated.

   In this case, you might specify:
        #define ELIMINABLE_REGS  \
        {{ARG_POINTER_REGNUM, STACK_POINTER_REGNUM}, \
         {ARG_POINTER_REGNUM, FRAME_POINTER_REGNUM}, \
         {FRAME_POINTER_REGNUM, STACK_POINTER_REGNUM}}

   Note that the elimination of the argument pointer with the stack pointer is
   specified first since that is the preferred elimination.  */

#define ELIMINABLE_REGS							\
{									\
  {ARG_POINTER_REGNUM,	 STACK_POINTER_REGNUM},				\
  {ARG_POINTER_REGNUM,	 FRAME_POINTER_REGNUM},				\
  {FRAME_POINTER_REGNUM, STACK_POINTER_REGNUM}				\
}

/* This macro returns the initial difference between the specified pair
   of registers.  */

#define INITIAL_ELIMINATION_OFFSET(FROM, TO, OFFSET)			\
  (OFFSET) = frv_initial_elimination_offset (FROM, TO)


/* Passing Function Arguments on the Stack.  */

/* If defined, the maximum amount of space required for outgoing arguments will
   be computed and placed into the variable
   `crtl->outgoing_args_size'.  No space will be pushed onto the
   stack for each call; instead, the function prologue should increase the
   stack frame size by this amount.

   Defining both `PUSH_ROUNDING' and `ACCUMULATE_OUTGOING_ARGS' is not
   proper.  */
#define ACCUMULATE_OUTGOING_ARGS 1


/* The number of register assigned to holding function arguments.  */

#define FRV_NUM_ARG_REGS        6

/* A C type for declaring a variable that is used as the first argument of
   `FUNCTION_ARG' and other related values.  For some target machines, the type
   `int' suffices and can hold the number of bytes of argument so far.

   There is no need to record in `CUMULATIVE_ARGS' anything about the arguments
   that have been passed on the stack.  The compiler has other variables to
   keep track of that.  For target machines on which all arguments are passed
   on the stack, there is no need to store anything in `CUMULATIVE_ARGS';
   however, the data structure must exist and should not be empty, so use
   `int'.  */
#define CUMULATIVE_ARGS int

/* A C statement (sans semicolon) for initializing the variable CUM for the
   state at the beginning of the argument list.  The variable has type
   `CUMULATIVE_ARGS'.  The value of FNTYPE is the tree node for the data type
   of the function which will receive the args, or 0 if the args are to a
   compiler support library function.  The value of INDIRECT is nonzero when
   processing an indirect call, for example a call through a function pointer.
   The value of INDIRECT is zero for a call to an explicitly named function, a
   library function call, or when `INIT_CUMULATIVE_ARGS' is used to find
   arguments for the function being compiled.

   When processing a call to a compiler support library function, LIBNAME
   identifies which one.  It is a `symbol_ref' rtx which contains the name of
   the function, as a string.  LIBNAME is 0 when an ordinary C function call is
   being processed.  Thus, each time this macro is called, either LIBNAME or
   FNTYPE is nonzero, but never both of them at once.  */

#define INIT_CUMULATIVE_ARGS(CUM, FNTYPE, LIBNAME, FNDECL, N_NAMED_ARGS) \
  frv_init_cumulative_args (&CUM, FNTYPE, LIBNAME, FNDECL, FALSE)

/* Like `INIT_CUMULATIVE_ARGS' but overrides it for the purposes of finding the
   arguments for the function being compiled.  If this macro is undefined,
   `INIT_CUMULATIVE_ARGS' is used instead.

   The value passed for LIBNAME is always 0, since library routines with
   special calling conventions are never compiled with GCC.  The argument
   LIBNAME exists for symmetry with `INIT_CUMULATIVE_ARGS'.  */

#define INIT_CUMULATIVE_INCOMING_ARGS(CUM, FNTYPE, LIBNAME) \
  frv_init_cumulative_args (&CUM, FNTYPE, LIBNAME, NULL, TRUE)

/* A C expression that is nonzero if REGNO is the number of a hard register in
   which function arguments are sometimes passed.  This does *not* include
   implicit arguments such as the static chain and the structure-value address.
   On many machines, no registers can be used for this purpose since all
   function arguments are pushed on the stack.  */
#define FUNCTION_ARG_REGNO_P(REGNO) \
  ((REGNO) >= FIRST_ARG_REGNUM && ((REGNO) <= LAST_ARG_REGNUM))


/* How Scalar Function Values are Returned.  */

/* The number of the hard register that is used to return a scalar value from a
   function call.  */
#define RETURN_VALUE_REGNUM	(GPR_FIRST + 8)

#define FUNCTION_VALUE_REGNO_P(REGNO) frv_function_value_regno_p (REGNO)


/* How Large Values are Returned.  */

/* The number of the register that is used to pass the structure
   value address.  */
#define FRV_STRUCT_VALUE_REGNUM (GPR_FIRST + 3)


/* Function Entry and Exit.  */

/* Define this macro as a C expression that is nonzero if the return
   instruction or the function epilogue ignores the value of the stack pointer;
   in other words, if it is safe to delete an instruction to adjust the stack
   pointer before a return from the function.

   Note that this macro's value is relevant only for functions for which frame
   pointers are maintained.  It is never safe to delete a final stack
   adjustment in a function that has no frame pointer, and the compiler knows
   this regardless of `EXIT_IGNORE_STACK'.  */
#define EXIT_IGNORE_STACK 1

/* Generating Code for Profiling.  */

/* A C statement or compound statement to output to FILE some assembler code to
   call the profiling subroutine `mcount'.  Before calling, the assembler code
   must load the address of a counter variable into a register where `mcount'
   expects to find the address.  The name of this variable is `LP' followed by
   the number LABELNO, so you would generate the name using `LP%d' in a
   `fprintf'.

   The details of how the address should be passed to `mcount' are determined
   by your operating system environment, not by GCC.  To figure them out,
   compile a small program for profiling using the system's installed C
   compiler and look at the assembler code that results.

   This declaration must be present, but it can be an abort if profiling is
   not implemented.  */

#define FUNCTION_PROFILER(FILE, LABELNO)

/* Trampolines for Nested Functions.  */

/* A C expression for the size in bytes of the trampoline, as an integer.  */
#define TRAMPOLINE_SIZE frv_trampoline_size ()

/* Alignment required for trampolines, in bits.

   If you don't define this macro, the value of `BIGGEST_ALIGNMENT' is used for
   aligning trampolines.  */
#define TRAMPOLINE_ALIGNMENT (TARGET_FDPIC ? 64 : 32)

/* Define this macro if trampolines need a special subroutine to do their work.
   The macro should expand to a series of `asm' statements which will be
   compiled with GCC.  They go in a library function named
   `__transfer_from_trampoline'.

   If you need to avoid executing the ordinary prologue code of a compiled C
   function when you jump to the subroutine, you can do so by placing a special
   label of your own in the assembler code.  Use one `asm' statement to
   generate an assembler label, and another to make the label global.  Then
   trampolines can use that label to jump directly to your special assembler
   code.  */

#ifdef __FRV_UNDERSCORE__
#define TRAMPOLINE_TEMPLATE_NAME "___trampoline_template"
#else
#define TRAMPOLINE_TEMPLATE_NAME "__trampoline_template"
#endif

#define Twrite _write

#if ! __FRV_FDPIC__
#define TRANSFER_FROM_TRAMPOLINE					\
extern int Twrite (int, const void *, unsigned);			\
									\
void									\
__trampoline_setup (short * addr, int size, int fnaddr, int sc)		\
{									\
  extern short __trampoline_template[];					\
  short * to = addr;							\
  short * from = &__trampoline_template[0];				\
  int i;								\
									\
  if (size < 20)							\
    {									\
      Twrite (2, "__trampoline_setup bad size\n",			\
	      sizeof ("__trampoline_setup bad size\n") - 1);		\
      exit (-1);							\
    }									\
									\
  to[0] = from[0];							\
  to[1] = (short)(fnaddr);						\
  to[2] = from[2];							\
  to[3] = (short)(sc);							\
  to[4] = from[4];							\
  to[5] = (short)(fnaddr >> 16);					\
  to[6] = from[6];							\
  to[7] = (short)(sc >> 16);						\
  to[8] = from[8];							\
  to[9] = from[9];							\
									\
  for (i = 0; i < 20; i++)						\
    __asm__ volatile ("dcf @(%0,%1)\n\tici @(%0,%1)" :: "r" (to), "r" (i)); \
}									\
									\
__asm__("\n"								\
	"\t.globl " TRAMPOLINE_TEMPLATE_NAME "\n"			\
	"\t.text\n"							\
	TRAMPOLINE_TEMPLATE_NAME ":\n"					\
	"\tsetlos #0, gr6\n"	/* jump register */			\
	"\tsetlos #0, gr7\n"	/* static chain */			\
	"\tsethi #0, gr6\n"						\
	"\tsethi #0, gr7\n"						\
	"\tjmpl @(gr0,gr6)\n");
#else
#define TRANSFER_FROM_TRAMPOLINE					\
extern int Twrite (int, const void *, unsigned);			\
									\
void									\
__trampoline_setup (addr, size, fnaddr, sc)				\
     short * addr;							\
     int size;								\
     int fnaddr;							\
     int sc;								\
{									\
  extern short __trampoline_template[];					\
  short * from = &__trampoline_template[0];				\
  int i;								\
  short **desc = (short **)addr;					\
  short * to = addr + 4;						\
									\
  if (size != 32)							\
    {									\
      Twrite (2, "__trampoline_setup bad size\n",			\
	      sizeof ("__trampoline_setup bad size\n") - 1);		\
      exit (-1);							\
    }									\
									\
  /* Create a function descriptor with the address of the code below	\
     and NULL as the FDPIC value.  We don't need the real GOT value	\
     here, since we don't use it, so we use NULL, that is just as	\
     good.  */								\
  desc[0] = to;								\
  desc[1] = NULL;							\
  size -= 8;								\
									\
  to[0] = from[0];							\
  to[1] = (short)(fnaddr);						\
  to[2] = from[2];							\
  to[3] = (short)(sc);							\
  to[4] = from[4];							\
  to[5] = (short)(fnaddr >> 16);					\
  to[6] = from[6];							\
  to[7] = (short)(sc >> 16);						\
  to[8] = from[8];							\
  to[9] = from[9];							\
  to[10] = from[10];							\
  to[11] = from[11];							\
									\
  for (i = 0; i < size; i++)						\
    __asm__ volatile ("dcf @(%0,%1)\n\tici @(%0,%1)" :: "r" (to), "r" (i)); \
}									\
									\
__asm__("\n"								\
	"\t.globl " TRAMPOLINE_TEMPLATE_NAME "\n"			\
	"\t.text\n"							\
	TRAMPOLINE_TEMPLATE_NAME ":\n"					\
	"\tsetlos #0, gr6\n"	/* Jump register.  */			\
	"\tsetlos #0, gr7\n"	/* Static chain.  */			\
	"\tsethi #0, gr6\n"						\
	"\tsethi #0, gr7\n"						\
	"\tldd @(gr6,gr0),gr14\n"					\
	"\tjmpl @(gr14,gr0)\n"						\
	);
#endif


/* Addressing Modes.  */

/* A number, the maximum number of registers that can appear in a valid memory
   address.  Note that it is up to you to specify a value equal to the maximum
   number that `TARGET_LEGITIMATE_ADDRESS_P' would ever accept.  */
#define MAX_REGS_PER_ADDRESS 2

/* A C expression that is nonzero if X (assumed to be a `reg' RTX) is valid for
   use as a base register.  For hard registers, it should always accept those
   which the hardware permits and reject the others.  Whether the macro accepts
   or rejects pseudo registers must be controlled by `REG_OK_STRICT' as
   described above.  This usually requires two variant definitions, of which
   `REG_OK_STRICT' controls the one actually used.  */
#ifdef REG_OK_STRICT
#define REG_OK_FOR_BASE_P(X) GPR_P (REGNO (X))
#else
#define REG_OK_FOR_BASE_P(X) GPR_AP_OR_PSEUDO_P (REGNO (X))
#endif

/* A C expression that is nonzero if X (assumed to be a `reg' RTX) is valid for
   use as an index register.

   The difference between an index register and a base register is that the
   index register may be scaled.  If an address involves the sum of two
   registers, neither one of them scaled, then either one may be labeled the
   "base" and the other the "index"; but whichever labeling is used must fit
   the machine's constraints of which registers may serve in each capacity.
   The compiler will try both labelings, looking for one that is valid, and
   will reload one or both registers only if neither labeling works.  */
#define REG_OK_FOR_INDEX_P(X) REG_OK_FOR_BASE_P (X)

#define FIND_BASE_TERM frv_find_base_term

/* The load-and-update commands allow pre-modification in addresses.
   The index has to be in a register.  */
#define HAVE_PRE_MODIFY_REG 1


/* We define extra CC modes in frv-modes.def so we need a selector.  */

#define SELECT_CC_MODE frv_select_cc_mode

/* A C expression whose value is one if it is always safe to reverse a
   comparison whose mode is MODE.  If `SELECT_CC_MODE' can ever return MODE for
   a floating-point inequality comparison, then `REVERSIBLE_CC_MODE (MODE)'
   must be zero.

   You need not define this macro if it would always returns zero or if the
   floating-point format is anything other than `IEEE_FLOAT_FORMAT'.  For
   example, here is the definition used on the SPARC, where floating-point
   inequality comparisons are always given `CCFPEmode':

        #define REVERSIBLE_CC_MODE(MODE)  ((MODE) != CCFPEmode)  */

/* On frv, don't consider floating point comparisons to be reversible.  In
   theory, fp equality comparisons can be reversible.  */
#define REVERSIBLE_CC_MODE(MODE) \
  ((MODE) == CCmode || (MODE) == CC_UNSmode || (MODE) == CC_NZmode)


/* Describing Relative Costs of Operations.  */

/* A C expression for the cost of a branch instruction.  A value of 1 is the
   default; other values are interpreted relative to that.  */
#define BRANCH_COST(speed_p, predictable_p) frv_branch_cost_int

/* Define this macro as a C expression which is nonzero if accessing less than
   a word of memory (i.e. a `char' or a `short') is no faster than accessing a
   word of memory, i.e., if such access require more than one instruction or if
   there is no difference in cost between byte and (aligned) word loads.

   When this macro is not defined, the compiler will access a field by finding
   the smallest containing object; when it is defined, a fullword load will be
   used if alignment permits.  Unless bytes accesses are faster than word
   accesses, using word accesses is preferable since it may eliminate
   subsequent memory access if subsequent accesses occur to other fields in the
   same word of the structure, but to different bytes.  */
#define SLOW_BYTE_ACCESS 1

/* Define this macro if it is as good or better to call a constant function
   address than to call an address kept in a register.  */
#define NO_FUNCTION_CSE 1


/* Dividing the output into sections.  */

/* A C expression whose value is a string containing the assembler operation
   that should precede instructions and read-only data.  Normally `".text"' is
   right.  */
#define TEXT_SECTION_ASM_OP "\t.text"

/* A C expression whose value is a string containing the assembler operation to
   identify the following data as writable initialized data.  Normally
   `".data"' is right.  */
#define DATA_SECTION_ASM_OP "\t.data"

#define BSS_SECTION_ASM_OP "\t.section .bss,\"aw\""

/* Short Data Support */
#define SDATA_SECTION_ASM_OP	"\t.section .sdata,\"aw\""

#undef	INIT_SECTION_ASM_OP
#undef	FINI_SECTION_ASM_OP
#define INIT_SECTION_ASM_OP	"\t.section .init,\"ax\""
#define FINI_SECTION_ASM_OP	"\t.section .fini,\"ax\""

#undef CTORS_SECTION_ASM_OP
#undef DTORS_SECTION_ASM_OP
#define CTORS_SECTION_ASM_OP	"\t.section\t.ctors,\"a\""
#define DTORS_SECTION_ASM_OP	"\t.section\t.dtors,\"a\""

/* A C expression whose value is a string containing the assembler operation to
   switch to the fixup section that records all initialized pointers in a -fpic
   program so they can be changed program startup time if the program is loaded
   at a different address than linked for.  */
#define FIXUP_SECTION_ASM_OP	"\t.section .rofixup,\"a\""

/* Position Independent Code.  */

/* A C expression that is nonzero if X is a legitimate immediate operand on the
   target machine when generating position independent code.  You can assume
   that X satisfies `CONSTANT_P', so you need not check this.  You can also
   assume FLAG_PIC is true, so you need not check it either.  You need not
   define this macro if all constants (including `SYMBOL_REF') can be immediate
   operands when generating position independent code.  */
#define LEGITIMATE_PIC_OPERAND_P(X)					\
  (   GET_CODE (X) == CONST_INT						\
   || GET_CODE (X) == CONST_DOUBLE					\
   || (GET_CODE (X) == HIGH && GET_CODE (XEXP (X, 0)) == CONST_INT)	\
   || got12_operand (X, VOIDmode))					\


/* The Overall Framework of an Assembler File.  */

/* A C string constant describing how to begin a comment in the target
   assembler language.  The compiler assumes that the comment will end at the
   end of the line.  */
#define ASM_COMMENT_START ";"

/* A C string constant for text to be output before each `asm' statement or
   group of consecutive ones.  Normally this is `"#APP"', which is a comment
   that has no effect on most assemblers but tells the GNU assembler that it
   must check the lines that follow for all valid assembler constructs.  */
#define ASM_APP_ON "#APP\n"

/* A C string constant for text to be output after each `asm' statement or
   group of consecutive ones.  Normally this is `"#NO_APP"', which tells the
   GNU assembler to resume making the time-saving assumptions that are valid
   for ordinary compiler output.  */
#define ASM_APP_OFF "#NO_APP\n"


/* Output of Data.  */

/* This is how to output a label to dwarf/dwarf2.  */
#define ASM_OUTPUT_DWARF_ADDR(STREAM, LABEL)				\
do {									\
  fprintf (STREAM, "\t.picptr\t");					\
  assemble_name (STREAM, LABEL);					\
} while (0)

/* Whether to emit the gas specific dwarf2 line number support.  */
#define DWARF2_ASM_LINE_DEBUG_INFO (TARGET_DEBUG_LOC)

/* Output of Uninitialized Variables.  */

/* A C statement (sans semicolon) to output to the stdio stream STREAM the
   assembler definition of a local-common-label named NAME whose size is SIZE
   bytes.  The variable ROUNDED is the size rounded up to whatever alignment
   the caller wants.

   Use the expression `assemble_name (STREAM, NAME)' to output the name itself;
   before and after that, output the additional assembler syntax for defining
   the name, and a newline.

   This macro controls how the assembler definitions of uninitialized static
   variables are output.  */
#undef ASM_OUTPUT_LOCAL

#undef ASM_OUTPUT_ALIGNED_LOCAL

/* This is for final.c, because it is used by ASM_DECLARE_OBJECT_NAME.  */
extern int size_directive_output;

/* Like `ASM_OUTPUT_ALIGNED_LOCAL' except that it takes an additional
   parameter - the DECL of variable to be output, if there is one.
   This macro can be called with DECL == NULL_TREE.  If you define
   this macro, it is used in place of `ASM_OUTPUT_LOCAL' and
   `ASM_OUTPUT_ALIGNED_LOCAL', and gives you more flexibility in
   handling the destination of the variable.  */
#undef ASM_OUTPUT_ALIGNED_DECL_LOCAL
#define ASM_OUTPUT_ALIGNED_DECL_LOCAL(STREAM, DECL, NAME, SIZE, ALIGN)	\
do {                                                                   	\
  if ((SIZE) > 0 && (SIZE) <= (unsigned HOST_WIDE_INT) g_switch_value)	\
    switch_to_section (get_named_section (NULL, ".sbss", 0));           \
  else                                                                 	\
    switch_to_section (bss_section);                                  	\
  ASM_OUTPUT_ALIGN (STREAM, floor_log2 ((ALIGN) / BITS_PER_UNIT));     	\
  ASM_DECLARE_OBJECT_NAME (STREAM, NAME, DECL);                        	\
  ASM_OUTPUT_SKIP (STREAM, (SIZE) ? (SIZE) : 1);                       	\
} while (0)


/* Output and Generation of Labels.  */

/* A C statement (sans semicolon) to output to the stdio stream STREAM the
   assembler definition of a label named NAME.  Use the expression
   `assemble_name (STREAM, NAME)' to output the name itself; before and after
   that, output the additional assembler syntax for defining the name, and a
   newline.  */
#define ASM_OUTPUT_LABEL(STREAM, NAME)					\
do {									\
  assemble_name (STREAM, NAME);						\
  fputs (":\n", STREAM);						\
} while (0)

/* Globalizing directive for a label.  */
#define GLOBAL_ASM_OP "\t.globl "

#undef ASM_GENERATE_INTERNAL_LABEL
#define ASM_GENERATE_INTERNAL_LABEL(LABEL, PREFIX, NUM)			\
do {									\
  sprintf (LABEL, "*.%s%ld", PREFIX, (long)NUM);			\
} while (0)


/* Macros Controlling Initialization Routines.  */

#undef INIT_SECTION_ASM_OP

/* If defined, `main' will call `__main' despite the presence of
   `INIT_SECTION_ASM_OP'.  This macro should be defined for systems where the
   init section is not actually run automatically, but is still useful for
   collecting the lists of constructors and destructors.  */
#define INVOKE__main

/* Output of Assembler Instructions.  */

/* A C initializer containing the assembler's names for the machine registers,
   each one as a C string constant.  This is what translates register numbers
   in the compiler into assembler language.  */
#define REGISTER_NAMES							\
{									\
 "gr0",  "sp",   "fp",   "gr3",  "gr4",  "gr5",  "gr6",  "gr7",		\
  "gr8",  "gr9",  "gr10", "gr11", "gr12", "gr13", "gr14", "gr15",	\
  "gr16", "gr17", "gr18", "gr19", "gr20", "gr21", "gr22", "gr23",	\
  "gr24", "gr25", "gr26", "gr27", "gr28", "gr29", "gr30", "gr31",	\
  "gr32", "gr33", "gr34", "gr35", "gr36", "gr37", "gr38", "gr39",	\
  "gr40", "gr41", "gr42", "gr43", "gr44", "gr45", "gr46", "gr47",	\
  "gr48", "gr49", "gr50", "gr51", "gr52", "gr53", "gr54", "gr55",	\
  "gr56", "gr57", "gr58", "gr59", "gr60", "gr61", "gr62", "gr63",	\
									\
  "fr0",  "fr1",  "fr2",  "fr3",  "fr4",  "fr5",  "fr6",  "fr7",	\
  "fr8",  "fr9",  "fr10", "fr11", "fr12", "fr13", "fr14", "fr15",	\
  "fr16", "fr17", "fr18", "fr19", "fr20", "fr21", "fr22", "fr23",	\
  "fr24", "fr25", "fr26", "fr27", "fr28", "fr29", "fr30", "fr31",	\
  "fr32", "fr33", "fr34", "fr35", "fr36", "fr37", "fr38", "fr39",	\
  "fr40", "fr41", "fr42", "fr43", "fr44", "fr45", "fr46", "fr47",	\
  "fr48", "fr49", "fr50", "fr51", "fr52", "fr53", "fr54", "fr55",	\
  "fr56", "fr57", "fr58", "fr59", "fr60", "fr61", "fr62", "fr63",	\
									\
  "fcc0", "fcc1", "fcc2", "fcc3", "icc0", "icc1", "icc2", "icc3",	\
  "cc0",  "cc1",  "cc2",  "cc3",  "cc4",  "cc5",  "cc6",  "cc7",	\
  "acc0", "acc1", "acc2", "acc3", "acc4", "acc5", "acc6", "acc7",	\
  "acc8", "acc9", "acc10", "acc11",					\
  "accg0","accg1","accg2","accg3","accg4","accg5","accg6","accg7",	\
  "accg8", "accg9", "accg10", "accg11",					\
  "ap",   "lr",   "lcr",  "iacc0h", "iacc0l"				\
}

/* Define this macro if you are using an unusual assembler that
   requires different names for the machine instructions.

   The definition is a C statement or statements which output an
   assembler instruction opcode to the stdio stream STREAM.  The
   macro-operand PTR is a variable of type `char *' which points to
   the opcode name in its "internal" form--the form that is written
   in the machine description.  The definition should output the
   opcode name to STREAM, performing any translation you desire, and
   increment the variable PTR to point at the end of the opcode so
   that it will not be output twice.

   In fact, your macro definition may process less than the entire
   opcode name, or more than the opcode name; but if you want to
   process text that includes `%'-sequences to substitute operands,
   you must take care of the substitution yourself.  Just be sure to
   increment PTR over whatever text should not be output normally.

   If you need to look at the operand values, they can be found as the
   elements of `recog_operand'.

   If the macro definition does nothing, the instruction is output in
   the usual way.  */

#define ASM_OUTPUT_OPCODE(STREAM, PTR)\
   (PTR) = frv_asm_output_opcode (STREAM, PTR)

/* If defined, a C statement to be executed just prior to the output
   of assembler code for INSN, to modify the extracted operands so
   they will be output differently.

   Here the argument OPVEC is the vector containing the operands
   extracted from INSN, and NOPERANDS is the number of elements of
   the vector which contain meaningful data for this insn.  The
   contents of this vector are what will be used to convert the insn
   template into assembler code, so you can change the assembler
   output by changing the contents of the vector.

   This macro is useful when various assembler syntaxes share a single
   file of instruction patterns; by defining this macro differently,
   you can cause a large class of instructions to be output
   differently (such as with rearranged operands).  Naturally,
   variations in assembler syntax affecting individual insn patterns
   ought to be handled by writing conditional output routines in
   those patterns.

   If this macro is not defined, it is equivalent to a null statement.  */

#define FINAL_PRESCAN_INSN(INSN, OPVEC, NOPERANDS)\
  frv_final_prescan_insn (INSN, OPVEC, NOPERANDS)

#undef USER_LABEL_PREFIX
#define USER_LABEL_PREFIX ""
#define REGISTER_PREFIX ""
#define LOCAL_LABEL_PREFIX "."
#define IMMEDIATE_PREFIX "#"


/* Output of dispatch tables.  */

/* This macro should be provided on machines where the addresses in a dispatch
   table are relative to the table's own address.

   The definition should be a C statement to output to the stdio stream STREAM
   an assembler pseudo-instruction to generate a difference between two labels.
   VALUE and REL are the numbers of two internal labels.  The definitions of
   these labels are output using `(*targetm.asm_out.internal_label)', and they must be
   printed in the same way here.  For example,

        fprintf (STREAM, "\t.word L%d-L%d\n", VALUE, REL)  */
#define ASM_OUTPUT_ADDR_DIFF_ELT(STREAM, BODY, VALUE, REL) \
fprintf (STREAM, "\t.word .L%d-.L%d\n", VALUE, REL)

/* This macro should be provided on machines where the addresses in a dispatch
   table are absolute.

   The definition should be a C statement to output to the stdio stream STREAM
   an assembler pseudo-instruction to generate a reference to a label.  VALUE
   is the number of an internal label whose definition is output using
   `(*targetm.asm_out.internal_label)'.  For example,

        fprintf (STREAM, "\t.word L%d\n", VALUE)  */
#define ASM_OUTPUT_ADDR_VEC_ELT(STREAM, VALUE) \
fprintf (STREAM, "\t.word .L%d\n", VALUE)

#define JUMP_TABLES_IN_TEXT_SECTION (flag_pic)

/* Assembler Commands for Exception Regions.  */

/* Define this macro to 0 if your target supports DWARF 2 frame unwind
   information, but it does not yet work with exception handling.  Otherwise,
   if your target supports this information (if it defines
   `INCOMING_RETURN_ADDR_RTX' and `OBJECT_FORMAT_ELF'), GCC will provide
   a default definition of 1.

   If this macro is defined to 1, the DWARF 2 unwinder will be the default
   exception handling mechanism; otherwise, setjmp/longjmp will be used by
   default.

   If this macro is defined to anything, the DWARF 2 unwinder will be used
   instead of inline unwinders and __unwind_function in the non-setjmp case.  */
#define DWARF2_UNWIND_INFO 1

#define DWARF_FRAME_RETURN_COLUMN DWARF_FRAME_REGNUM (LR_REGNO)

/* Assembler Commands for Alignment.  */

#undef  ASM_OUTPUT_SKIP
#define ASM_OUTPUT_SKIP(STREAM, NBYTES) \
  fprintf (STREAM, "\t.zero\t%u\n", (int)(NBYTES))

/* A C statement to output to the stdio stream STREAM an assembler command to
   advance the location counter to a multiple of 2 to the POWER bytes.  POWER
   will be a C expression of type `int'.  */
#define ASM_OUTPUT_ALIGN(STREAM, POWER) \
  fprintf ((STREAM), "\t.p2align %d\n", (POWER))

/* Inside the text section, align with unpacked nops rather than zeros.  */
#define ASM_OUTPUT_ALIGN_WITH_NOP(STREAM, POWER) \
  fprintf ((STREAM), "\t.p2alignl %d,0x80880000\n", (POWER))

/* Macros Affecting all Debug Formats.  */

/* A C expression that returns the DBX register number for the compiler
   register number REGNO.  In simple cases, the value of this expression may be
   REGNO itself.  But sometimes there are some registers that the compiler
   knows about and DBX does not, or vice versa.  In such cases, some register
   may need to have one number in the compiler and another for DBX.

   If two registers have consecutive numbers inside GCC, and they can be
   used as a pair to hold a multiword value, then they *must* have consecutive
   numbers after renumbering with `DBX_REGISTER_NUMBER'.  Otherwise, debuggers
   will be unable to access such a pair, because they expect register pairs to
   be consecutive in their own numbering scheme.

   If you find yourself defining `DBX_REGISTER_NUMBER' in way that does not
   preserve register pairs, then what you must do instead is redefine the
   actual register numbering scheme.

   This declaration is required.  */
#define DBX_REGISTER_NUMBER(REGNO) (REGNO)

#undef  PREFERRED_DEBUGGING_TYPE
#define PREFERRED_DEBUGGING_TYPE DWARF2_DEBUG

/* Miscellaneous Parameters.  */

/* An alias for a machine mode name.  This is the machine mode that elements of
   a jump-table should have.  */
#define CASE_VECTOR_MODE SImode

/* Define this macro if operations between registers with integral mode smaller
   than a word are always performed on the entire register.  Most RISC machines
   have this property and most CISC machines do not.  */
#define WORD_REGISTER_OPERATIONS 1

/* Define this macro to be a C expression indicating when insns that read
   memory in MODE, an integral mode narrower than a word, set the bits outside
   of MODE to be either the sign-extension or the zero-extension of the data
   read.  Return `SIGN_EXTEND' for values of MODE for which the insn
   sign-extends, `ZERO_EXTEND' for which it zero-extends, and `UNKNOWN' for other
   modes.

   This macro is not called with MODE non-integral or with a width greater than
   or equal to `BITS_PER_WORD', so you may return any value in this case.  Do
   not define this macro if it would always return `UNKNOWN'.  On machines where
   this macro is defined, you will normally define it as the constant
   `SIGN_EXTEND' or `ZERO_EXTEND'.  */
#define LOAD_EXTEND_OP(MODE) SIGN_EXTEND

/* Define if loading short immediate values into registers sign extends.  */
#define SHORT_IMMEDIATES_SIGN_EXTEND 1

/* The maximum number of bytes that a single instruction can move quickly from
   memory to memory.  */
#define MOVE_MAX 8

/* A C expression which is nonzero if on this machine it is safe to "convert"
   an integer of INPREC bits to one of OUTPREC bits (where OUTPREC is smaller
   than INPREC) by merely operating on it as if it had only OUTPREC bits.

   On many machines, this expression can be 1.

   When `TRULY_NOOP_TRUNCATION' returns 1 for a pair of sizes for modes for
   which `MODES_TIEABLE_P' is 0, suboptimal code can result.  If this is the
   case, making `TRULY_NOOP_TRUNCATION' return 0 in such cases may improve
   things.  */
#define TRULY_NOOP_TRUNCATION(OUTPREC, INPREC) 1

/* An alias for the machine mode for pointers.  On most machines, define this
   to be the integer mode corresponding to the width of a hardware pointer;
   `SImode' on 32-bit machine or `DImode' on 64-bit machines.  On some machines
   you must define this to be one of the partial integer modes, such as
   `PSImode'.

   The width of `Pmode' must be at least as large as the value of
   `POINTER_SIZE'.  If it is not equal, you must define the macro
   `POINTERS_EXTEND_UNSIGNED' to specify how pointers are extended to `Pmode'.  */
#define Pmode SImode

/* An alias for the machine mode used for memory references to functions being
   called, in `call' RTL expressions.  On most machines this should be
   `QImode'.  */
#define FUNCTION_MODE QImode

/* A C expression for the maximum number of instructions to execute via
   conditional execution instructions instead of a branch.  A value of
   BRANCH_COST+1 is the default if the machine does not use
   cc0, and 1 if it does use cc0.  */
#define MAX_CONDITIONAL_EXECUTE frv_condexec_insns

/* A C expression to modify the code described by the conditional if
   information CE_INFO, possibly updating the tests in TRUE_EXPR, and
   FALSE_EXPR for converting if-then and if-then-else code to conditional
   instructions.  Set either TRUE_EXPR or FALSE_EXPR to a null pointer if the
   tests cannot be converted.  */
#define IFCVT_MODIFY_TESTS(CE_INFO, TRUE_EXPR, FALSE_EXPR)		\
frv_ifcvt_modify_tests (CE_INFO, &TRUE_EXPR, &FALSE_EXPR)

/* A C expression to modify the code described by the conditional if
   information CE_INFO, for the basic block BB, possibly updating the tests in
   TRUE_EXPR, and FALSE_EXPR for converting the && and || parts of if-then or
   if-then-else code to conditional instructions.  OLD_TRUE and OLD_FALSE are
   the previous tests.  Set either TRUE_EXPR or FALSE_EXPR to a null pointer if
   the tests cannot be converted.  */
#define IFCVT_MODIFY_MULTIPLE_TESTS(CE_INFO, BB, TRUE_EXPR, FALSE_EXPR) \
frv_ifcvt_modify_multiple_tests (CE_INFO, BB, &TRUE_EXPR, &FALSE_EXPR)

/* A C expression to modify the code described by the conditional if
   information CE_INFO with the new PATTERN in INSN.  If PATTERN is a null
   pointer after the IFCVT_MODIFY_INSN macro executes, it is assumed that that
   insn cannot be converted to be executed conditionally.  */
#define IFCVT_MODIFY_INSN(CE_INFO, PATTERN, INSN) \
(PATTERN) = frv_ifcvt_modify_insn (CE_INFO, PATTERN, INSN)

/* A C expression to perform any final machine dependent modifications in
   converting code to conditional execution in the code described by the
   conditional if information CE_INFO.  */
#define IFCVT_MODIFY_FINAL(CE_INFO) frv_ifcvt_modify_final (CE_INFO)

/* A C expression to cancel any machine dependent modifications in converting
   code to conditional execution in the code described by the conditional if
   information CE_INFO.  */
#define IFCVT_MODIFY_CANCEL(CE_INFO) frv_ifcvt_modify_cancel (CE_INFO)

/* Initialize the machine-specific static data for if-conversion.  */
#define IFCVT_MACHDEP_INIT(CE_INFO) frv_ifcvt_machdep_init (CE_INFO)

/* The definition of the following macro results in that the 2nd jump
   optimization (after the 2nd insn scheduling) is minimal.  It is
   necessary to define when start cycle marks of insns (TImode is used
   for this) is used for VLIW insn packing.  Some jump optimizations
   make such marks invalid.  These marks are corrected for some
   (minimal) optimizations.  ??? Probably the macro is temporary.
   Final solution could making the 2nd jump optimizations before the
   2nd instruction scheduling or corrections of the marks for all jump
   optimizations.  Although some jump optimizations are actually
   deoptimizations for VLIW (super-scalar) processors.  */

#define MINIMAL_SECOND_JUMP_OPTIMIZATION


/* If the following macro is defined and nonzero and deterministic
   finite state automata are used for pipeline hazard recognition, we
   will try to exchange insns in queue ready to improve the schedule.
   The more macro value, the more tries will be made.  */
#define FIRST_CYCLE_MULTIPASS_SCHEDULING 1

/* The following macro is used only when value of
   FIRST_CYCLE_MULTIPASS_SCHEDULING is nonzero.  The more macro value,
   the more tries will be made to choose better schedule.  If the
   macro value is zero or negative there will be no multi-pass
   scheduling.  */
#define FIRST_CYCLE_MULTIPASS_SCHEDULING_LOOKAHEAD frv_sched_lookahead

enum frv_builtins
{
  FRV_BUILTIN_MAND,
  FRV_BUILTIN_MOR,
  FRV_BUILTIN_MXOR,
  FRV_BUILTIN_MNOT,
  FRV_BUILTIN_MAVEH,
  FRV_BUILTIN_MSATHS,
  FRV_BUILTIN_MSATHU,
  FRV_BUILTIN_MADDHSS,
  FRV_BUILTIN_MADDHUS,
  FRV_BUILTIN_MSUBHSS,
  FRV_BUILTIN_MSUBHUS,
  FRV_BUILTIN_MPACKH,
  FRV_BUILTIN_MQADDHSS,
  FRV_BUILTIN_MQADDHUS,
  FRV_BUILTIN_MQSUBHSS,
  FRV_BUILTIN_MQSUBHUS,
  FRV_BUILTIN_MUNPACKH,
  FRV_BUILTIN_MDPACKH,
  FRV_BUILTIN_MBTOH,
  FRV_BUILTIN_MHTOB,
  FRV_BUILTIN_MCOP1,
  FRV_BUILTIN_MCOP2,
  FRV_BUILTIN_MROTLI,
  FRV_BUILTIN_MROTRI,
  FRV_BUILTIN_MWCUT,
  FRV_BUILTIN_MSLLHI,
  FRV_BUILTIN_MSRLHI,
  FRV_BUILTIN_MSRAHI,
  FRV_BUILTIN_MEXPDHW,
  FRV_BUILTIN_MEXPDHD,
  FRV_BUILTIN_MMULHS,
  FRV_BUILTIN_MMULHU,
  FRV_BUILTIN_MMULXHS,
  FRV_BUILTIN_MMULXHU,
  FRV_BUILTIN_MMACHS,
  FRV_BUILTIN_MMACHU,
  FRV_BUILTIN_MMRDHS,
  FRV_BUILTIN_MMRDHU,
  FRV_BUILTIN_MQMULHS,
  FRV_BUILTIN_MQMULHU,
  FRV_BUILTIN_MQMULXHU,
  FRV_BUILTIN_MQMULXHS,
  FRV_BUILTIN_MQMACHS,
  FRV_BUILTIN_MQMACHU,
  FRV_BUILTIN_MCPXRS,
  FRV_BUILTIN_MCPXRU,
  FRV_BUILTIN_MCPXIS,
  FRV_BUILTIN_MCPXIU,
  FRV_BUILTIN_MQCPXRS,
  FRV_BUILTIN_MQCPXRU,
  FRV_BUILTIN_MQCPXIS,
  FRV_BUILTIN_MQCPXIU,
  FRV_BUILTIN_MCUT,
  FRV_BUILTIN_MCUTSS,
  FRV_BUILTIN_MWTACC,
  FRV_BUILTIN_MWTACCG,
  FRV_BUILTIN_MRDACC,
  FRV_BUILTIN_MRDACCG,
  FRV_BUILTIN_MTRAP,
  FRV_BUILTIN_MCLRACC,
  FRV_BUILTIN_MCLRACCA,
  FRV_BUILTIN_MDUNPACKH,
  FRV_BUILTIN_MBTOHE,
  FRV_BUILTIN_MQXMACHS,
  FRV_BUILTIN_MQXMACXHS,
  FRV_BUILTIN_MQMACXHS,
  FRV_BUILTIN_MADDACCS,
  FRV_BUILTIN_MSUBACCS,
  FRV_BUILTIN_MASACCS,
  FRV_BUILTIN_MDADDACCS,
  FRV_BUILTIN_MDSUBACCS,
  FRV_BUILTIN_MDASACCS,
  FRV_BUILTIN_MABSHS,
  FRV_BUILTIN_MDROTLI,
  FRV_BUILTIN_MCPLHI,
  FRV_BUILTIN_MCPLI,
  FRV_BUILTIN_MDCUTSSI,
  FRV_BUILTIN_MQSATHS,
  FRV_BUILTIN_MQLCLRHS,
  FRV_BUILTIN_MQLMTHS,
  FRV_BUILTIN_MQSLLHI,
  FRV_BUILTIN_MQSRAHI,
  FRV_BUILTIN_MHSETLOS,
  FRV_BUILTIN_MHSETLOH,
  FRV_BUILTIN_MHSETHIS,
  FRV_BUILTIN_MHSETHIH,
  FRV_BUILTIN_MHDSETS,
  FRV_BUILTIN_MHDSETH,
  FRV_BUILTIN_SMUL,
  FRV_BUILTIN_UMUL,
  FRV_BUILTIN_PREFETCH0,
  FRV_BUILTIN_PREFETCH,
  FRV_BUILTIN_SMASS,
  FRV_BUILTIN_SMSSS,
  FRV_BUILTIN_SMU,
  FRV_BUILTIN_SCUTSS,
  FRV_BUILTIN_ADDSS,
  FRV_BUILTIN_SUBSS,
  FRV_BUILTIN_SLASS,
  FRV_BUILTIN_IACCreadll,
  FRV_BUILTIN_IACCreadl,
  FRV_BUILTIN_IACCsetll,
  FRV_BUILTIN_IACCsetl,
  FRV_BUILTIN_SCAN,
  FRV_BUILTIN_READ8,
  FRV_BUILTIN_READ16,
  FRV_BUILTIN_READ32,
  FRV_BUILTIN_READ64,
  FRV_BUILTIN_WRITE8,
  FRV_BUILTIN_WRITE16,
  FRV_BUILTIN_WRITE32,
  FRV_BUILTIN_WRITE64
};
#define FRV_BUILTIN_FIRST_NONMEDIA FRV_BUILTIN_SMUL

/* Enable prototypes on the call rtl functions.  */
#define MD_CALL_PROTOTYPES 1

#define CPU_UNITS_QUERY 1

#endif /* __FRV_H__ */
