/* Declarations for the C-SKY back end.
   Copyright (C) 2018-2024 Free Software Foundation, Inc.
   Contributed by C-SKY Microsystems and Mentor Graphics.

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


#ifndef GCC_CSKY_H
#define GCC_CSKY_H

/* In some places e.g. csky_secondary_reload, we use -1 to indicate an
   invalid register.  In other places where N is unsigned the comparison
   to zero would give an error, so explicitly cast to int here.  */
#define CSKY_GENERAL_REGNO_P(N)			\
  ((N) < CSKY_NGPR_REGS && (int)(N) >= 0)

#define CSKY_VREG_LO_P(N) \
  ((N) >= CSKY_FIRST_VFP_REGNUM \
   && (N) <= CSKY_LAST_VFP_REGNUM)

 #define CSKY_VREG_HI_P(N) \
   ((N) >= CSKY_FIRST_VFP3_REGNUM \
    && (N) <= CSKY_LAST_VFP3_REGNUM)

 #define CSKY_VREG_P(N)    \
   (CSKY_VREG_LO_P(N)     \
    || CSKY_VREG_HI_P(N))

#define CSKY_HILO_REG_P(N)   \
  ((N) == CSKY_HI_REGNUM || (N) == CSKY_LO_REGNUM)

/* Helper macros for constant constraints and predicates.  */
#define CSKY_VALUE_BETWEEN(VALUE, LOW, HIGH)	\
  ((VALUE) >= (LOW) && (VALUE) <= (HIGH))

#define CSKY_CONST_OK_FOR_I(VALUE)  \
  CSKY_VALUE_BETWEEN (VALUE, 0, 65535)

#define CSKY_CONST_OK_FOR_J(VALUE)  \
  CSKY_VALUE_BETWEEN (VALUE, 1, 32)

#define CSKY_CONST_OK_FOR_K(VALUE)  \
  CSKY_VALUE_BETWEEN (VALUE, 0, 31)

#define CSKY_CONST_OK_FOR_L(VALUE)  \
  CSKY_VALUE_BETWEEN (VALUE, 1, 8)

#define CSKY_CONST_OK_FOR_M(VALUE)  \
  CSKY_VALUE_BETWEEN (VALUE, 1, 4096)

#define CSKY_CONST_OK_FOR_N(VALUE)  \
  CSKY_VALUE_BETWEEN (VALUE, 1, 256)

#define CSKY_CONST_OK_FOR_O(VALUE)  \
  CSKY_VALUE_BETWEEN (VALUE, 0, 4095)

#define CSKY_CONST_OK_FOR_P(VALUE)  \
  (((VALUE) & 0x3) == 0 && CSKY_VALUE_BETWEEN (VALUE, 4, 508))

#define CSKY_CONST_OK_FOR_T(VALUE)  \
  CSKY_VALUE_BETWEEN (VALUE, -256, -1)

#define CSKY_CONST_OK_FOR_Ub(VALUE)  \
  (exact_log2 (VALUE & 0xFFFFFFFF) >= 0)

#define CSKY_CONST_OK_FOR_Uc(VALUE)	     \
  ((VALUE) == (HOST_WIDE_INT) -1	     \
   || (exact_log2 ((VALUE) + 1) >= 0	     \
       && exact_log2 ((VALUE) + 1) <= 31))

#define CSKY_CONST_OK_FOR_Ud(VALUE)				\
  ((CSKY_CONST_OK_FOR_I ((VALUE) & 0xffffffff)			\
    || CSKY_CONST_OK_FOR_Ub ((VALUE))				\
    || CSKY_CONST_OK_FOR_Uc (((VALUE) << 32) >> 32))		\
   && (CSKY_CONST_OK_FOR_I ((VALUE) >> 32)			\
       || CSKY_CONST_OK_FOR_Ub ((VALUE) >> 32)			\
       || CSKY_CONST_OK_FOR_Uc ((VALUE) >> 32)))		\

#define CSKY_CONST_OK_FOR_Ug(VALUE)  \
  (((VALUE) & 0x3) == 0 && CSKY_VALUE_BETWEEN (VALUE, -508, -4))

#define CSKY_CONST_OK_FOR_Uh(VALUE)  \
  CSKY_VALUE_BETWEEN (VALUE, -31, 0)

#define CSKY_CONST_OK_FOR_Uj(VALUE)  \
  (((VALUE) & 0x3) == 0 && CSKY_VALUE_BETWEEN (VALUE, 1, 1024))

#define CSKY_CONST_OK_FOR_Uk(VALUE)  \
  CSKY_VALUE_BETWEEN (VALUE, 1, 65536)

#define CSKY_CONST_OK_FOR_Ul(VALUE)  \
  (((VALUE) & 0x3) == 0 && CSKY_VALUE_BETWEEN (VALUE, -1024, -4))

#define CSKY_CONST_OK_FOR_Um(VALUE)  \
  CSKY_VALUE_BETWEEN (VALUE, -4096, -1)

#define CSKY_CONST_OK_FOR_US(VALUE) \
  CSKY_VALUE_BETWEEN (VALUE, -8, -1)

#define CSKY_CONST_OK_FOR_MOVIH(VALUE)		\
  (((VALUE) & 0xFFFF) == 0)

#ifndef TARGET_CPU_DEFAULT
#define TARGET_CPU_DEFAULT CSKY_TARGET_CORE_GET(ck810f)
#endif

/* Options that are enabled by default are specified as such in the
   .opt file.  */
#define TARGET_DEFAULT 0

/* The highest CSKY architecture version supported by the target.  */
#define CSKY_TARGET_ARCH(arch) \
  (csky_base_arch == CSKY_TARGET_ARCH_GET (arch))

/* Define some macros for target code generation options.  */
#define TARGET_SOFT_FPU \
  (csky_fpu_index == TARGET_FPU_fpv2_sf)
#define TARGET_CASESI \
  (optimize_size && TARGET_CONSTANT_POOL \
   && (CSKY_TARGET_ARCH (CK801) || CSKY_TARGET_ARCH (CK802)))
#define TARGET_TLS \
  (CSKY_TARGET_ARCH (CK807) || CSKY_TARGET_ARCH (CK810) || CSKY_TARGET_ARCH (CK860))

/* Run-time Target Specification.  */
#define TARGET_SOFT_FLOAT       (csky_float_abi == CSKY_FLOAT_ABI_SOFT)
/* Use hardware floating point instructions. */
#define TARGET_HARD_FLOAT       (csky_float_abi != CSKY_FLOAT_ABI_SOFT)
/* Use hardware floating point calling convention.  */
#define TARGET_HARD_FLOAT_ABI   (csky_float_abi == CSKY_FLOAT_ABI_HARD)

#define TARGET_SINGLE_FPU     (csky_fpu_index == TARGET_FPU_fpv2_sf \
			       || csky_fpu_index == TARGET_FPU_fpv3_hsf \
			       || csky_fpu_index == TARGET_FPU_fpv3_hf)
#define TARGET_DOUBLE_FPU     (TARGET_HARD_FLOAT && !TARGET_SINGLE_FPU)

#define FUNCTION_VARG_REGNO_P(REGNO)      \
  (TARGET_HARD_FLOAT_ABI                  \
   && IN_RANGE ((REGNO), CSKY_FIRST_VFP_REGNUM, \
		CSKY_FIRST_VFP_REGNUM + CSKY_NPARM_FREGS - 1))

#define CSKY_VREG_MODE_P(mode) \
  ((mode) == SFmode || (mode) == DFmode \
   || (CSKY_ISA_FEATURE(fpv3_hf) && (mode) == HFmode))

#define FUNCTION_VARG_MODE_P(mode)  \
  (TARGET_HARD_FLOAT_ABI            \
   && CSKY_VREG_MODE_P(mode)        \
   && !(mode == DFmode && TARGET_SINGLE_FPU))

#define TARGET_SUPPORT_FPV3 (CSKY_ISA_FEATURE (fpv3_hf)    \
			     || CSKY_ISA_FEATURE (fpv3_sf) \
			     || CSKY_ISA_FEATURE (fpv3_df))

#define TARGET_SUPPORT_FPV2 (CSKY_ISA_FEATURE(fpv2_sf)    \
			     || CSKY_ISA_FEATURE(fpv2_df) \
			     || CSKY_ISA_FEATURE(fpv2_divd))

/* Number of loads/stores handled by ldm/stm.  */
#define CSKY_MIN_MULTIPLE_STLD	3
#define CSKY_MAX_MULTIPLE_STLD	12

/* Pull in enums and defines for processor/arch variants.  This makes
   it possible to use CSKY_TARGET_ARCH in macros defined in this file.  */
#include "csky_opts.h"
extern enum csky_base_architecture csky_base_arch;

/* Pull in enums and defines for ISA features.  Likewise required to
   support use of CSKY_ISA_FEATURE in this file.
   Note that the CSKY_ISA_FEATURE macro tests properties of the
   particular processor we're compiling for, not code generation
   options that may have dependencies on those features.  The latter
   are handled by TARGET_xxxx macros/variables instead.  See csky.opt.  */
#include "csky_isa.h"
extern int csky_arch_isa_features[];
#define CSKY_ISA_FEATURE(IDENT) \
  csky_arch_isa_features[CSKY_ISA_FEATURE_GET (IDENT)]

/******************************************************************
 *			   Storage Layout			  *
 ******************************************************************/


/* Define this if most significant bit is lowest numbered
   in instructions that operate on numbered bit-fields.  */
#define BITS_BIG_ENDIAN	 0

/* If the most significant byte of a word is the lowest numbered.  */
#define BYTES_BIG_ENDIAN (TARGET_BIG_ENDIAN != 0)

/* If the most significant word of a multiword number is the lowest.  */
#define WORDS_BIG_ENDIAN (BYTES_BIG_ENDIAN)

/* Width of a word, in units (bytes).  */
#define UNITS_PER_WORD 4

/* Define this macro if it is advisable to hold scalars in registers
   in a wider mode than that declared by the program.  In such cases,
   the value is constrained to be within the bounds of the declared
   type, but kept valid in the wider mode.  The signedness of the
   extension may differ from that of the type.  */
#define PROMOTE_MODE(MODE, UNSIGNEDP, TYPE)	\
  if (GET_MODE_CLASS (MODE) == MODE_INT		\
      && GET_MODE_SIZE (MODE) < UNITS_PER_WORD) \
    (MODE) = SImode;


/* Allocation boundary (in *bits*) for storing arguments in argument list.  */
#define PARM_BOUNDARY	32

/* Boundary (in *bits*) on which stack pointer should be aligned.
   Per C-SKY, the published V2 ABI document is incorrect and the proper
   alignment is on a 4-byte boundary rather than 8 bytes.  */
#define STACK_BOUNDARY	32

/* Align definitions of arrays, unions and structures so that
   initializations and copies can be made more efficient.  This is not
   ABI-changing, so it only affects places where we can see the
   definition. Increasing the alignment tends to introduce padding,
   so don't do this when optimizing for size/conserving stack space. */
#define CSKY_EXPAND_ALIGNMENT(COND, EXP, ALIGN) \
  (((COND) && ((ALIGN) < BITS_PER_WORD)		 \
    && (TREE_CODE (EXP) == ARRAY_TYPE		 \
	|| TREE_CODE (EXP) == UNION_TYPE	 \
	|| TREE_CODE (EXP) == RECORD_TYPE))	 \
   ? BITS_PER_WORD : (ALIGN))

/* Align global data. */
#define DATA_ALIGNMENT(EXP, ALIGN)	\
  CSKY_EXPAND_ALIGNMENT (!optimize_size, EXP, ALIGN)

/* Similarly, make sure that objects on the stack are sensibly aligned.  */
#define LOCAL_ALIGNMENT(EXP, ALIGN)	  \
  CSKY_EXPAND_ALIGNMENT (!flag_conserve_stack, EXP, ALIGN)

/* No data type wants to be aligned rounder than this.  */
#define BIGGEST_ALIGNMENT 32

/* Every structures size must be a multiple of 8 bits.  */
#define STRUCTURE_SIZE_BOUNDARY 8

/* Look at the fundamental type that is used for a bit-field and use
   that to impose alignment on the enclosing structure.
   struct s {int a:8}; should have same alignment as "int", not "char".  */
#define PCC_BITFIELD_TYPE_MATTERS 1

/* Largest integer machine mode for structures.  If undefined, the default
   is GET_MODE_SIZE(DImode).  */
#define MAX_FIXED_MODE_SIZE 64

/* Allocation boundary (in *bits*) for the code of a function.
   Optimize ck801 and ck802 a little harder for size.  */
#define FUNCTION_BOUNDARY					\
  (((CSKY_TARGET_ARCH (CK801) || CSKY_TARGET_ARCH (CK802))	\
    && optimize_size)						\
   ? 16 : 32)

/* C-SKY does not support unaligned access.  */
#define STRICT_ALIGNMENT    1

#undef SIZE_TYPE
#define SIZE_TYPE "unsigned int"

#undef PTRDIFF_TYPE
#define PTRDIFF_TYPE "int"

#undef WCHAR_TYPE
#define WCHAR_TYPE "long int"

#undef UINT_LEAST32_TYPE
#define UINT_LEAST32_TYPE "unsigned int"

#undef INT_LEAST32_TYPE
#define INT_LEAST32_TYPE "int"

#undef WCHAR_TYPE_SIZE
#define WCHAR_TYPE_SIZE BITS_PER_WORD

/******************************************************************
 *		Layout of Source Language Data Types		  *
 ******************************************************************/


/* 'char' is unsigned by default for backward compatibility.  */
#define DEFAULT_SIGNED_CHAR    0


/******************************************************************
 *		Stack Layout and Calling Conventions		  *
 ******************************************************************/


/* Basic Stack Layout  */


/* Define this if pushing a word on the stack
   makes the stack pointer a smaller address.  */
#define STACK_GROWS_DOWNWARD	1

/* Define this to nonzero if the nominal address of the stack frame
   is at the high-address end of the local variables;
   that is, each additional local variable allocated
   goes at a more negative offset in the frame.  */
#define FRAME_GROWS_DOWNWARD	1

/* Offset of first parameter from the argument pointer register value.  */
#define FIRST_PARM_OFFSET(FNDECL) 0

/* A C expression whose value is RTL representing the value of the return
   address for the frame COUNT steps up from the current frame.  */
#define RETURN_ADDR_RTX(COUNT, FRAME) \
  csky_return_addr (COUNT, FRAME)

/* Pick up the return address upon entry to a procedure. Used for
   dwarf2 unwind information.  This also enables the table driven
   mechanism.  */
#define INCOMING_RETURN_ADDR_RTX  gen_rtx_REG (Pmode, CSKY_LR_REGNUM)


/* Exception Handling Support  */

/* The register that holds the return address in exception handlers.  */
#define EH_RETURN_STACKADJ_RTX	gen_rtx_REG (SImode, CSKY_EH_STACKADJ_REGNUM)

/* Select a format to encode pointers in exception handling data.  */
#define ASM_PREFERRED_EH_DATA_FORMAT(CODE, GLOBAL) \
  (((GLOBAL) ? DW_EH_PE_indirect : 0) | DW_EH_PE_pcrel | DW_EH_PE_sdata4)

/* Registers That Address the Stack Frame  */


/* Register to use for pushing function arguments.  */
#define STACK_POINTER_REGNUM  CSKY_SP_REGNUM

/* Base register for access to local variables of the function.  */
#define FRAME_POINTER_REGNUM  36
#define HARD_FRAME_POINTER_REGNUM  8

/* Base register for access to arguments of the function.  This is a fake
   register that is always eliminated.  */
#define ARG_POINTER_REGNUM    32

/* Static chain register.
   Register use is more restricted on CK801.  */
#define STATIC_CHAIN_REGNUM   (CSKY_TARGET_ARCH (CK801) ? 13 : 12)


/* Eliminating Frame Pointer and Arg Pointer  */


/* Definitions for register eliminations.

   This is an array of structures.  Each structure initializes one pair
   of eliminable registers.  The "from" register number is given first,
   followed by "to".  Eliminations of the same "from" register are listed
   in order of preference.

   We have two registers that can be eliminated on the CSKY.  First, the
   arg pointer register can often be eliminated in favor of the stack
   pointer register.  Secondly, the pseudo frame pointer register can always
   be eliminated; it is replaced with the stack pointer.  */
#define ELIMINABLE_REGS		  \
{{ ARG_POINTER_REGNUM,	      STACK_POINTER_REGNUM	      },\
 { ARG_POINTER_REGNUM,	      FRAME_POINTER_REGNUM	      },\
 { ARG_POINTER_REGNUM,	      HARD_FRAME_POINTER_REGNUM       },\
 { FRAME_POINTER_REGNUM,      STACK_POINTER_REGNUM	      },\
 { FRAME_POINTER_REGNUM,      HARD_FRAME_POINTER_REGNUM	      }}

/* Define the offset between two registers, one to be eliminated, and the
   other its replacement, at the start of a routine.  */
#define INITIAL_ELIMINATION_OFFSET(FROM, TO, OFFSET)	  \
  (OFFSET) = csky_initial_elimination_offset (FROM, TO)


/* Passing Function Arguments on the Stack  */


/* Define this if the maximum size of all the outgoing args is to be
   accumulated and pushed during the prologue.  The amount can be
   found in the variable crtl->outgoing_args_size.  */
#define ACCUMULATE_OUTGOING_ARGS 1


/* Passing Arguments in Registers  */


/* A C type for declaring a variable that is used as the first argument of
   TARGET_ FUNCTION_ARG and other related values.  */
#if !defined (USED_FOR_TARGET)
typedef struct
{
  int reg;
  int freg;
  bool is_stdarg;
} CUMULATIVE_ARGS;
#endif

/* Initialize a variable CUM of type CUMULATIVE_ARGS
   for a call to a function whose data type is FNTYPE.
   For a library call, FNTYPE is 0.

   On CSKY, the offset always starts at 0: the first parm reg is always
   the same reg.  */
#define INIT_CUMULATIVE_ARGS(CUM, FNTYPE, LIBNAME, INDIRECT, N_NAMED_ARGS) \
  csky_init_cumulative_args (&(CUM), (FNTYPE), (LIBNAME), (INDIRECT))

/* True if N is a possible register number for function argument passing.
   On the CSKY, r0-r3 are used to pass args.
   The int cast is to prevent a complaint about unsigned comparison to
   zero, since CSKY_FIRST_PARM_REGNUM is zero.  */
#define FUNCTION_ARG_REGNO_P(REGNO)                          \
  (((int)(REGNO) >= CSKY_FIRST_PARM_REGNUM                   \
    && (REGNO) < (CSKY_NPARM_REGS + CSKY_FIRST_PARM_REGNUM)) \
   || FUNCTION_VARG_REGNO_P(REGNO))

/* How Large Values Are Returned  */


/* Define DEFAULT_PCC_STRUCT_RETURN to 1 if all structure and union return
   values must be in memory.  On the CSKY, small
   structures (eight bytes or fewer) are returned in
   the register pair r0/r1.  */
#define DEFAULT_PCC_STRUCT_RETURN 0

/* EXIT_IGNORE_STACK should be nonzero if, when returning from a function,
   the stack pointer does not matter.  The value is tested only in
   functions that have frame pointers.
   No definition is equivalent to always zero.

   On the CSKY, the function epilogue recovers the stack pointer from the
   frame.  */
#define EXIT_IGNORE_STACK 1


/******************************************************************
 *		Register Usage & Register Classes		  *
 ******************************************************************/


#define FIRST_PSEUDO_REGISTER 202

/* 1 for registers that have pervasive standard uses
   and are not available for the register allocator.
   On C-SKY, r14 is SP, r26 is used by linker,
   r27 is used by assembler, r28 is data base address,
   r29 is GOT base address, r30 is handler base address,
   r31 is TLS register.  */
#define FIXED_REGISTERS							\
 /*  r0	   r1	 r2    r3    r4	   r5	 r6    r7  */			\
{    0,	   0,	 0,    0,    0,	   0,	 0,    0,			\
 /*  r8	   r9	 r10   r11   r12   r13	 r14   r15 */			\
     0,	   0,	 0,    0,    0,	   0,	 1,    0,			\
 /*  r16   r17	 r18   r19   r20   r21	 r22   r23 */			\
     0,	   0,	 0,    0,    0,	   0,	 0,    0,			\
 /*  r24   r25	 r26   r27   r28   r29	 r30   tls */			\
     0,	   0,	 1,    1,    1,	   1,	 1,    1,			\
 /*  reserved	 c     hi    lo	 */					\
     1,		 1,    0,    0,						\
 /*  reserved */							\
     1,	   1,	 1,    1,    1,	   1,	 1,    1,			\
     1,	   1,	 1,    1,    1,	   1,	 1,    1,			\
 /*  vr0   vr1	 vr2   vr3   vr4   vr5	 vr6   vr7  */			\
     0,	   0,	 0,    0,    0,	   0,	 0,    0,			\
 /*  vr8   vr9	 vr10  vr11  vr12  vr13	 vr14  vr15 */			\
     0,	   0,	 0,    0,    0,	   0,	 0,    0 ,			\
 /*  reserved */							\
     1,	   1,								\
 /*  epc */								\
     1,									\
 /* vr16  vr17  vr18  vr19  vr20  vr21  vr22  vr23 */			\
     0,    0,    0,    0,    0,    0,    0,    0,			\
 /* vr24  vr25  vr26  vr27  vr28  vr29  vr30  vr31 */			\
     0,    0,    0,    0,    0,    0,    0,    0 ,			\
 /* reserved */								\
     1,    1,    1,    1,    1,    1,    1,    1,			\
     1,    1,    1,    1,    1,    1,    1,    1,			\
 /* reserved */								\
     1,    1,    1,    1,    1,    1,    1,    1,			\
     1,    1,    1,    1,    1,    1,    1,    1,			\
     1,    1,    1,    1,    1,    1,    1,    1,			\
     1,    1,    1,    1,    1,    1,    1,    1,			\
									\
     1,    1,    1,    1,    1,    1,    1,    1,			\
     1,    1,    1,    1,    1,    1,    1,    1,			\
     1,    1,    1,    1,    1,    1,    1,    1,			\
     1,    1,    1,    1,    1,    1,    1,    1,			\
									\
     1,    1,    1,    1,    1,    1,    1,    1,			\
     1,    1,    1,    1,    1,    1,    1,    1,			\
     1,    1,    1,    1,    1,    1,    1,    1,			\
     1,    1,    1,    1,    1,    1,    1,    1,			\
									\
     1,    1,    1							\
}

/* Like `CALL_USED_REGISTERS' but used to overcome a historical
   problem which makes CALL_USED_REGISTERS *always* include
   all the FIXED_REGISTERS.  Until this problem has been
   resolved this macro can be used to overcome this situation.
   In particular, block_propagate() requires this list
   be accurate, or we can remove registers which should be live.
   This macro is used in get_csky_live_regs().  */
#define CALL_REALLY_USED_REGISTERS \
 /*  r0	   r1	 r2    r3    r4	   r5	 r6    r7  */			\
{    1,	   1,	 1,    1,    0,	   0,	 0,    0,			\
 /*  r8	   r9	 r10   r11   r12   r13	 r14   r15 */			\
     0,	   0,	 0,    0,    1,	   1,	 1,    0,			\
 /*  r16   r17	 r18   r19   r20   r21	 r22   r23 */			\
     0,	   0,	 1,    1,    1,	   1,	 1,    1,			\
 /*  r24   r25	 r26   r27   r28   r29	 r30   r31 */			\
     1,	   1,	 1,    1,    1,	   1,	 1,    1,			\
 /*  reserved	 c     hi    lo */					\
     1,		 1,    1,    1,						\
 /*  reserved */							\
     1,	   1,	 1,    1,    1,	   1,	 1,    1,			\
     1,	   1,	 1,    1,    1,	   1,	 1,    1,			\
 /*  vr0   vr1	 vr2   vr3   vr4   vr5	 vr6   vr7 */			\
     1,	   1,	 1,    1,    1,	   1,	 1,    1,			\
 /*  vr8   vr9	 vr10  vr11  vr12  vr13	 vr14  vr15 */			\
     1,	   1,	 1,    1,    1,	   1,	 1,    1,			\
 /*  reserved */							\
     1,	   1,								\
 /*  epc */								\
     1,									\
 /*  vr16  vr17  vr18  vr19  vr20  vr21  vr22  vr23*/			\
     1,	   1,	 1,    1,    1,	   1,	 1,    1,			\
 /*  vr24  vr25 vr26  vr27  vr28  vr29	 vr30  vr31 */			\
     1,	   1,	 1,    1,    1,	   1,	 1,    1,			\
 /*  reserved */							\
     1,	   1,	 1,    1,    1,	   1,	 1,    1,			\
     1,	   1,	 1,    1,    1,	   1,	 1,    1,			\
 /* reserved */								\
     1,    1,    1,    1,    1,    1,    1,    1,			\
     1,    1,    1,    1,    1,    1,    1,    1,			\
     1,    1,    1,    1,    1,    1,    1,    1,			\
     1,    1,    1,    1,    1,    1,    1,    1,			\
									\
     1,    1,    1,    1,    1,    1,    1,    1,			\
     1,    1,    1,    1,    1,    1,    1,    1,			\
     1,    1,    1,    1,    1,    1,    1,    1,			\
     1,    1,    1,    1,    1,    1,    1,    1,			\
									\
     1,    1,    1,    1,    1,    1,    1,    1,			\
     1,    1,    1,    1,    1,    1,    1,    1,			\
     1,    1,    1,    1,    1,    1,    1,    1,			\
     1,    1,    1,    1,    1,    1,    1,    1,			\
									\
     1,    1,    1							\
}

#define REGISTER_NAMES							\
{									\
  "a0",	 "a1",	"a2",  "a3",  "l0",  "l1",  "l2",  "l3",		\
  "l4",	 "l5",	"l6",  "l7",  "t0",  "t1",  "sp",  "lr",		\
  "l8",	 "l9",	"t2",  "t3",  "t4",  "t5",  "t6",  "t7",		\
  "t8",	 "t9",	"r26", "r27", "gb",  "r29", "svbr", "r31",		\
  /* reserved */							\
  "reserved",								\
  /* CC register: 33 */							\
  "c",									\
  /* DSP instruction register: 34, 35 */				\
  "hi", "lo",								\
  "reserved", "reserved", "reserved", "reserved", "reserved",		\
  "reserved", "reserved", "reserved", "reserved", "reserved",		\
  "reserved", "reserved", "reserved", "reserved", "reserved",		\
  "reserved",								\
  /* V registers: 52~67 */						\
  "vr0", "vr1", "vr2",	"vr3",	"vr4",	"vr5",	"vr6",	"vr7",		\
  "vr8", "vr9", "vr10", "vr11", "vr12", "vr13", "vr14", "vr15",		\
  "reserved", "reserved",						\
  "epc",								\
  /* V registers: 71~86 */						\
  "vr16", "vr17", "vr18", "vr19", "vr20", "vr21", "vr22", "vr23",	\
  "vr24", "vr25", "vr26", "vr27", "vr28", "vr29", "vr30", "vr31",	\
  "reserved", "reserved", "reserved", "reserved", "reserved",		\
  "reserved", "reserved", "reserved", "reserved", "reserved",		\
  "reserved", "reserved", "reserved", "reserved", "reserved",		\
  "reserved",								\
  /* reserved: 87~201*/							\
  "reserved", "reserved", "reserved", "reserved", "reserved",		\
  "reserved", "reserved", "reserved", "reserved", "reserved",		\
  "reserved", "reserved", "reserved", "reserved", "reserved",		\
  "reserved", "reserved", "reserved", "reserved", "reserved",		\
  "reserved", "reserved", "reserved", "reserved", "reserved",		\
  "reserved", "reserved", "reserved", "reserved", "reserved",		\
  "reserved", "reserved",						\
  "reserved", "reserved", "reserved", "reserved", "reserved",		\
  "reserved", "reserved", "reserved", "reserved", "reserved",		\
  "reserved", "reserved", "reserved", "reserved", "reserved",		\
  "reserved", "reserved", "reserved", "reserved", "reserved",		\
  "reserved", "reserved", "reserved", "reserved", "reserved",		\
  "reserved", "reserved", "reserved", "reserved", "reserved",		\
  "reserved", "reserved",						\
  "reserved", "reserved", "reserved", "reserved", "reserved",		\
  "reserved", "reserved", "reserved", "reserved", "reserved",		\
  "reserved", "reserved", "reserved", "reserved", "reserved",		\
  "reserved", "reserved", "reserved", "reserved", "reserved",		\
  "reserved", "reserved", "reserved", "reserved", "reserved",		\
  "reserved", "reserved", "reserved", "reserved", "reserved",		\
  "reserved", "reserved",						\
  "reserved", "reserved", "reserved"					\
}

/* Table of additional register names to use in user input.  */
#define ADDITIONAL_REGISTER_NAMES   \
{				    \
  {"r0",  0},			    \
  {"r1",  1},			    \
  {"r2",  2},			    \
  {"r3",  3},			    \
  {"r4",  4},			    \
  {"r5",  5},			    \
  {"r6",  6},			    \
  {"r7",  7},			    \
  {"r8",  8},			    \
  {"r9",  9},			    \
  {"r10", 10},			    \
  {"r11", 11},			    \
  {"r12", 12},			    \
  {"r13", 13},			    \
  {"r14", 14},			    \
  {"r15", 15},			    \
  {"r16", 16},			    \
  {"r17", 17},			    \
  {"r18", 18},			    \
  {"r19", 19},			    \
  {"r20", 20},			    \
  {"r21", 21},			    \
  {"r22", 22},			    \
  {"r23", 23},			    \
  {"r24", 24},			    \
  {"r25", 25},			    \
  {"r26", 26},			    \
  {"r27", 27},			    \
  {"r28", 28},			    \
  {"r29", 29},			    \
  {"r30", 30},			    \
  {"r31", 31},			    \
}

/* The order in which registers should be allocated.
   It is better to use the registers the caller need not save.
   Allocate r0 through r3 in reverse order since r3 is least likely
   to contain a function parameter; in addition results are returned
   in r0.  It is quite good to use lr since other calls may clobber
   it anyway.  */
#define REG_ALLOC_ORDER						\
/*   r3	   r2	 r1    r0   r12	  r13	r18   r19 */		\
  {   3,    2,	  1,	0,   12,   13,	 18,   19,		\
/*  r20	  r21	r22   r23   r24	  r25 */			\
     20,   21,	 22,   23,   24,   25,				\
/*   r15   r4	 r5   r6     r7	   r8	 r9   r10   r11 */	\
     15,    4,	  5,   6,     7,    8,	  9,   10,   11,	\
/*  r16	  r17	r26   r27   r28	  r29	r30    hi    lo	 */	\
     16,   17,	 26,   27,   28,   29,	 30,   34,   35,	\
/*  vr0	  vr1	vr2   vr3   vr4	  vr5	vr6   vr7  */		\
     52,   53,	 54,   55,   56,   57,	 58,   59,		\
/*  vr8	  vr9	vr10  vr11  vr12  vr13	vr14  vr15 */		\
     60,   61,	 62,   63,   64,   65,	 66,   67,		\
/*  vr16  vr17  vr18  vr18  vr20  vr21	vr22  vr23 */		\
     71,   72,	 73,   74,   75,   76,	 77,   78,		\
/*  vr24  vr25	vr26  vr27  vr28  vr28	vr30  vr31 */		\
     79,   80,	 81,   82,   83,   84,	 85,   86,		\
/*  reserved  */						\
     36,   37,	 38,   39,   40,   41,	 42,   43,		\
     44,   45,	 46,   47,   48,   49,	 50,   51,		\
/*  reserved  */						\
     87,   88,	 89,   90,   91,   92,	 93,   94,		\
     95,   96,	 97,   98,   99,   100,  101,  102,		\
/*  sp	  tls	reserved     c	   reserved	    epc */	\
     14,   31,	 32,	     33,   68,	 69,	     70	 }

/*  Register classes.  */
enum reg_class
{
  NO_REGS,
  MINI_REGS,
  SP_REGS,
  LOW_REGS,
  GENERAL_REGS,
  C_REGS,
  HILO_REGS,
  V_REGS,
  OTHER_REGS,
  RESERVE_REGS,
  ALL_REGS,
  LIM_REG_CLASSES
};

#define N_REG_CLASSES  (int) LIM_REG_CLASSES

/* Give names of register classes as strings for dump file.  */
#define REG_CLASS_NAMES \
{			\
  "NO_REGS",		\
  "MINI_REGS",		\
  "SP_REGS",		\
  "LOW_REGS",		\
  "GENERAL_REGS",	\
  "C_REGS",		\
  "HILO_REGS",		\
  "V_REGS",		\
  "OTHER_REGS",		\
  "RESERVE_REGS",	\
  "ALL_REGS",		\
}

/* Define which registers fit in which classes.  This is an initializer
   for a vector of HARD_REG_SET of length N_REG_CLASSES.  */
#define REG_CLASS_CONTENTS						      \
{									      \
  {0x00000000, 0x00000000, 0x00000000, 0x00000000,			      \
   0x00000000, 0x00000000, 0x00000000},			/* NO_REGS	 */   \
  {0x000000FF, 0x00000000, 0x00000000, 0x00000000,			      \
   0x00000000, 0x00000000, 0x00000000},			/* MINI_REGS     */   \
  {0x00004000, 0x00000000, 0x00000000, 0x00000000,			      \
   0x00000000, 0x00000000, 0x00000000},			/* SP_REGS	 */   \
  {0x0000FFFF, 0x00000000, 0x00000000, 0x00000000,			      \
   0x00000000, 0x00000000, 0x00000000},			/* LOW_REGS      */   \
  {0xFFFFFFFF, 0x00000000, 0x00000000, 0x00000000,			      \
   0x00000000, 0x00000000, 0x00000000},			/* GENERAL_REGS  */   \
  {0x00000000, 0x00000002, 0x00000000, 0x00000000,			      \
   0x00000000, 0x00000000, 0x00000000},			/* C_REGS	 */   \
  {0x00000000, 0x0000000c, 0x00000000, 0x00000000,			      \
   0x00000000, 0x00000000, 0x00000000},			/* HILO_REGS     */   \
  {0x00000000, 0xFFF00000, 0x007FFF8F, 0x00000000,			      \
   0x00000000, 0x00000000, 0x00000000},			/* V_REGS	 */   \
  {0x00000000, 0x00000000, 0x00000040, 0x00000000,			      \
   0x00000000, 0x00000000, 0x00000000},			/* OTHER_REGS    */   \
  {0x00000000, 0x000FFFF1, 0xFF800030, 0xFFFFFFFF,			      \
   0xFFFFFFFF, 0xFFFFFFFF, 0x000003FF},			/* RESERVE_REGS  */   \
  {0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF, 0xFFFFFFFF,			      \
   0xFFFFFFFF, 0xFFFFFFFF, 0x000003FF},			/* ALL_REGS      */   \
}

/* Return register class from regno.  */
extern enum reg_class regno_reg_class[FIRST_PSEUDO_REGISTER];
#define REGNO_REG_CLASS(REGNO) regno_reg_class[REGNO]

/* The class value for index registers, and the one for base regs.  */
#define INDEX_REG_CLASS	 (CSKY_ISA_FEATURE (2E3) ? GENERAL_REGS : NO_REGS)
#define BASE_REG_CLASS	GENERAL_REGS

/* TODO is it necessary to set it to MINI_REGS to emit more 16-bit
   instructions?  */
#define MODE_BASE_REG_CLASS(MODE) GENERAL_REGS

/* The macros REG_OK_FOR..._P assume that the arg is a REG rtx
   and check its validity for a certain class.
   We have two alternate definitions for each of them.
   The usual definition accepts all pseudo regs; the other rejects
   them unless they have been allocated suitable hard regs.
   The symbol REG_OK_STRICT causes the latter definition to be used.

   Most source files want to accept pseudo regs in the hope that
   they will get allocated to the class that the insn wants them to be in.
   Source files for reload pass need to be strict.
   After reload, it makes no difference, since pseudo regs have
   been eliminated by then.

   The reg_renumber is used to map pseudo regs into hardware
   regs, it is set up as a result of register allocation.  */
#ifdef REG_OK_STRICT
#define REGNO_OK_FOR_BASE_P(REGNO)		       \
  (CSKY_GENERAL_REGNO_P (REGNO)			       \
   || CSKY_GENERAL_REGNO_P (reg_renumber[(REGNO)]) )
#else
#define REGNO_OK_FOR_BASE_P(REGNO)		       \
  (CSKY_GENERAL_REGNO_P (REGNO)			       \
   || (REGNO) >= FIRST_PSEUDO_REGISTER)
#endif


#ifdef REG_OK_STRICT
#define REGNO_OK_FOR_INDEX_P(REGNO)			\
  (CSKY_GENERAL_REGNO_P (REGNO)				\
   || CSKY_GENERAL_REGNO_P (reg_renumber[(REGNO)]) )
#else
#define REGNO_OK_FOR_INDEX_P(REGNO)		      \
  (CSKY_GENERAL_REGNO_P (REGNO)			      \
   || (REGNO) >= FIRST_PSEUDO_REGISTER)
#endif


/******************************************************************
 *			  Addressing Modes			  *
 ******************************************************************/


/* Recognize any constant value that is a valid address.  */
#define CONSTANT_ADDRESS_P(X) \
  (GET_CODE (X) == LABEL_REF || GET_CODE (X) == SYMBOL_REF)

/* Maximum number of registers that can appear in a valid memory address.
   Shifts in addresses can't be by a register.  */
#define MAX_REGS_PER_ADDRESS 2


/******************************************************************
 *			  Run-time Target			  *
 ******************************************************************/


#define TARGET_CPU_CPP_BUILTINS()		      \
  csky_cpu_cpp_builtins (pfile)

/******************************************************************
 *			Per-function Data			  *
 ******************************************************************/


/* Initialize data used by insn expanders.  This is called from insn_emit,
   once for every function before code is generated.  */
#define INIT_EXPANDERS	csky_init_expanders ()


/******************************************************************
 *    Dividing the Output into Sections (Texts, Data, . . . )	  *
 ******************************************************************/


/* Switch to the text or data segment.  */
#define TEXT_SECTION_ASM_OP  "\t.text"
#define DATA_SECTION_ASM_OP  "\t.data"

/* The subroutine calls in the .init and .fini sections create literal
   pools which must be jumped around...  */
#define FORCE_CODE_SECTION_ALIGN    \
  asm ("br 1f ; .literals ; .align 2 ; 1:");

/* Define this macro to be an expression with a nonzero value if
   jump tables (for tablejump insns) should be output in the text section,
   along with the assembler instructions.  */
#define JUMP_TABLES_IN_TEXT_SECTION TARGET_CASESI


/******************************************************************
 *			Assembler Format			  *
 ******************************************************************/


/* A C string constant for text to be output before(after) each asm
   statement or group of consecutive ones.  */
#undef	ASM_APP_ON
#define ASM_APP_ON    "// inline asm begin\n"
#undef	ASM_APP_OFF
#define ASM_APP_OFF   "// inline asm end\n"

/* A C string constant describing how to begin a comment in the target
   assembler language.  */
#define ASM_COMMENT_START "\t//"

/* This says how to output an assembler line
   to define a global common symbol, with alignment information.  */
#undef	ASM_OUTPUT_ALIGNED_COMMON
#define ASM_OUTPUT_ALIGNED_COMMON(STREAM, NAME, SIZE, ALIGN)	\
  do								\
    {								\
      fputs ("\t.comm\t", STREAM);				\
      assemble_name (STREAM, NAME);				\
      fprintf (STREAM, ",%lu, %u\n", (unsigned long)(SIZE),	\
	       (ALIGN) / BITS_PER_UNIT);			\
    }								\
while (0)

/* Define a local common symbol whose alignment we wish to specify.
   ALIGN comes in as bits, we have to turn it into bytes.  */
#undef	ASM_OUTPUT_ALIGNED_LOCAL
#define ASM_OUTPUT_ALIGNED_LOCAL(STREAM, NAME, SIZE, ALIGN)	\
  do								\
{								\
  fputs ("\t.bss\t", (STREAM));					\
  assemble_name ((STREAM), (NAME));				\
  fprintf ((STREAM), ",%d, %d\n", (int)(SIZE),			\
	   (ALIGN) / BITS_PER_UNIT);				\
}								\
while (0)

/* Globalizing directive for a label.  */
#define GLOBAL_ASM_OP "\t.global\t"

/* Output a reference to a label.  */
#undef	ASM_OUTPUT_LABELREF
#define ASM_OUTPUT_LABELREF(STREAM, NAME)     \
  fprintf (STREAM, "%s%s", user_label_prefix, \
	   (* targetm.strip_name_encoding) (NAME))

/* Make an internal label into a string.  */
#undef	ASM_GENERATE_INTERNAL_LABEL
#define ASM_GENERATE_INTERNAL_LABEL(STRING, PREFIX, NUM)  \
  sprintf (STRING, "*.%s%ld", PREFIX, (long) NUM)

/* This is how to output an insn to push a register on the stack.
   It need not be very fast code.  */
#define ASM_OUTPUT_REG_PUSH(STREAM,REGNO)		    \
  fprintf (STREAM, "\tsubi\t %s,%d\n\tst.w\t %s,(%s)\n",    \
	   reg_names[STACK_POINTER_REGNUM],		    \
	   (STACK_BOUNDARY / BITS_PER_UNIT),		    \
	   reg_names[REGNO],				    \
	   reg_names[STACK_POINTER_REGNUM])

/* This is how to output an insn to pop a register from the stack.  */
#define ASM_OUTPUT_REG_POP(STREAM,REGNO)		    \
  fprintf (STREAM, "\tld.w\t %s,(%s)\n\taddi\t %s,%d\n",    \
	   reg_names[REGNO],				    \
	   reg_names[STACK_POINTER_REGNUM],		    \
	   reg_names[STACK_POINTER_REGNUM],		    \
	   (STACK_BOUNDARY / BITS_PER_UNIT))

/* Output an element of a dispatch table.  */
#define ASM_OUTPUT_ADDR_VEC_ELT(STREAM,VALUE)  \
  fprintf (STREAM, "\t.long\t.L%d\n", VALUE)

/* This is how to output an assembler line
   that says to advance the location counter by SIZE bytes.  */
#undef	ASM_OUTPUT_SKIP
#define ASM_OUTPUT_SKIP(STREAM,SIZE)  \
  fprintf (STREAM, "\t.fill %d, 1\n", (int)(SIZE))

/* Align output to a power of two.  Note ".align 0" is redundant,
   and also GAS will treat it as ".align 2" which we do not want.  */
#define ASM_OUTPUT_ALIGN(STREAM, POWER)			\
  do							\
    {							\
      if ((POWER) > 0)					\
	fprintf (STREAM, "\t.align\t%d\n", POWER);	\
    }							\
  while (0)


/******************************************************************
 *		Controlling the Compilation Driver		  *
 ******************************************************************/


/* Define this macro as a C expression for the initializer of an
   array of string to tell the driver program which options are
   defaults for this target and thus do not need to be handled
   specially when using MULTILIB_OPTIONS.  */
#undef MULTILIB_DEFAULTS
#define MULTILIB_DEFAULTS    \
    {"mlittle-endian", "mcpu=ck810f", "mfloat-abi=soft"}

/* Support for a compile-time default CPU, et cetera.  The rules are:
   --with-arch is ignored if -march or -mcpu are specified.
   --with-cpu is ignored if -march or -mcpu are specified, and is overridden
    by --with-arch. */
#define OPTION_DEFAULT_SPECS \
  {"arch", "%{!march=*:%{!mcpu=*:-march=%(VALUE)}}" }, \
  {"cpu", "%{!march=*:%{!mcpu=*:-mcpu=%(VALUE)}}" }, \
  {"endian", "%{!mbig-endian:%{!mlittle-endian:-m%(VALUE)-endian}}" }, \
  {"float", "%{!mfloat-abi=*:-mfloat-abi=%(VALUE)}" },


/******************************************************************
 *		      Position Independent Code			  *
 ******************************************************************/

/* Define the global table register.  */
#define PIC_OFFSET_TABLE_REGNUM (flag_pic ? CSKY_GB_REGNUM : INVALID_REGNUM)

/* Nonzero if x is a legitimate immediate operand on the target machine
   when generating position-independent code.  */
#define LEGITIMATE_PIC_OPERAND_P(X) \
  csky_legitimate_pic_operand_p (X)


/******************************************************************
 *	      Controlling Debugging Information Format		  *
 ******************************************************************/


/* Define this macro if GCC should produce dwarf version 2 format debugging
   output in response to the `-g' option.  */
#define DWARF2_DEBUGGING_INFO 1

/* Define this macro to 0 if your target supports DWARF 2 frame unwind
   information, but it does not yet work with exception handling.  */
#define DWARF2_UNWIND_INFO 1

/* Define this if you have arranged for GCC to support
   more than one format of debugging output.
   The value of this macro only affects the default debugging output.  */
#define PREFERRED_DEBUGGING_TYPE DWARF2_DEBUG

/* Define this macro if the target’s representation
   for dwarf registers used in .eh_frame or .debug_frame
   is different from that used in other debug info sections.
   Given a GCC hard register number,
   this macro should return the .eh_frame register number.*/
#define DWARF_FRAME_REGNUM(REG)	 DEBUGGER_REGNO (REG)

/* If INCOMING_RETURN_ADDR_RTX is defined & the RTL is REG,
   define DWARF_FRAME_RETURN_COLUMN to DWARF_FRAME_REGNUM.  */
#define DWARF_FRAME_RETURN_COLUMN DWARF_FRAME_REGNUM (CSKY_LR_REGNUM)

/* Use r0 and r1 to pass exception handling information.  */
#define EH_RETURN_DATA_REGNO(N) ((N) < 2 ? N : INVALID_REGNUM)

/* How to renumber registers for gdb.  */
extern const int csky_debugger_regno[];
#define DEBUGGER_REGNO(REGNO) ((unsigned int) csky_debugger_regno[REGNO])


/******************************************************************
 *		      Miscellaneous Parameters			  *
 ******************************************************************/


/* Specify the machine mode that this machine uses
   for the index in the tablejump instruction.  */
#define CASE_VECTOR_MODE SImode

/* Define if operations between registers always perform the operation
   on the full register even if a narrower mode is specified.  */
#define WORD_REGISTER_OPERATIONS 1

/* Define if loading in MODE, an integral mode narrower than BITS_PER_WORD
   will either zero-extend or sign-extend.  The value of this macro should
   be the code that says which one of the two operations is implicitly
   done, UNKNOWN if none.  */
#define LOAD_EXTEND_OP(MODE) ZERO_EXTEND

/* Max number of bytes we can move from memory to memory
   in one reasonably fast instruction.  */
#define MOVE_MAX 4

/* Shift counts are truncated to 6-bits (0 to 63) instead of the expected
   5-bits, so we cannot define SHIFT_COUNT_TRUNCATED to true for this
   target.  */
#define SHIFT_COUNT_TRUNCATED 0

#define CLZ_DEFINED_VALUE_AT_ZERO(MODE, VALUE) ((VALUE) = 32, 1)

/* The machine modes of pointers and functions.  */
#define Pmode  SImode
#define FUNCTION_MODE  Pmode

/* Define this macro to be a C expression to indicate when jump-tables
   should contain relative addresses.  */
#define CASE_VECTOR_PC_RELATIVE \
  (optimize_size && TARGET_CONSTANT_POOL \
   && (CSKY_TARGET_ARCH (CK802) || CSKY_TARGET_ARCH (CK801)))

/* Return the preferred mode for an addr_diff_vec when the minimum
   and maximum offset are known.  */
#define CASE_VECTOR_SHORTEN_MODE(min, max, body)		    \
  (min >= 0 && max < 512					    \
   ? (ADDR_DIFF_VEC_FLAGS (body).offset_unsigned = 1, QImode)	    \
   : min >= -256 && max < 256					    \
     ? (ADDR_DIFF_VEC_FLAGS (body).offset_unsigned = 0, QImode)	    \
     : min >= 0 && max < 8192					    \
       ? (ADDR_DIFF_VEC_FLAGS (body).offset_unsigned = 1, HImode)   \
       : min >= -4096 && max < 4096				    \
	 ? (ADDR_DIFF_VEC_FLAGS (body).offset_unsigned = 0, HImode) \
	 : SImode)

/* This is how to output an element of a case-vector that is relative.  */
#define ASM_OUTPUT_ADDR_DIFF_ELT(STREAM, BODY, VALUE, REL)	    \
  do								    \
    {								    \
      if (optimize_size && TARGET_CONSTANT_POOL			    \
	  && (CSKY_TARGET_ARCH (CK802) || CSKY_TARGET_ARCH (CK801)))  \
	{							    \
	  switch (GET_MODE (BODY))				    \
	    {							    \
	    case E_QImode:					    \
	      asm_fprintf (STREAM, "\t.byte\t(.L%d-.L%d)/2\n",	    \
			   VALUE, REL);				    \
	      break;						    \
	    case E_HImode: /* TBH */				    \
	      asm_fprintf (STREAM, "\t.short\t(.L%d-.L%d)/2\n",	    \
			   VALUE, REL);				    \
	      break;						    \
	    case E_SImode:					    \
	      asm_fprintf (STREAM, "\t.long\t.L%d-.L%d\n",	    \
			   VALUE, REL);				    \
	      break;						    \
	    default:						    \
	      gcc_unreachable ();				    \
	    }							    \
	}							    \
      else							    \
	asm_fprintf (STREAM, "\t.long\t.L%d@GOTOFF\n", VALUE);	    \
    } while (0)

/* This macro is not documented yet.
   But we do need it to make jump table vector aligned.  */
#define ADDR_VEC_ALIGN(JUMPTABLE) 0

/* We have to undef this first to override the version from elfos.h.  */
#undef	ASM_OUTPUT_CASE_LABEL
#define ASM_OUTPUT_CASE_LABEL(stream, prefix, num, table)	\
  do								\
    {								\
      if (GET_MODE (PATTERN (table)) == SImode)			\
	ASM_OUTPUT_ALIGN (stream, 2);				\
      (*targetm.asm_out.internal_label) (stream, prefix, num);	\
    } while (0)

/* Make sure subsequent insns are aligned after a byte-sized jump offset
   table.  */
#define ASM_OUTPUT_CASE_END(stream, num, table)	  \
  do						  \
    {						  \
      if (GET_MODE (PATTERN (table)) == QImode)	  \
	ASM_OUTPUT_ALIGN (stream, 1);		  \
    } while (0)




/******************************************************************
 *		  Trampolines for Nested Functions		  *
 ******************************************************************/


/* Length in units of the trampoline for entering a nested function.  */
#define TRAMPOLINE_SIZE	 (CSKY_ISA_FEATURE (2E3) ? 16 : 20)

/* Alignment required for a trampoline in bits.  */
#define TRAMPOLINE_ALIGNMENT  32


/******************************************************************
 *	      Describing Relative Costs of Operations		  *
 ******************************************************************/


/* Nonzero if access to memory by bytes is slow and undesirable.
   For RISC chips, it means that access to memory by bytes is no
   better than access by words when possible, so grab a whole word
   and maybe make use of that.  */
#define SLOW_BYTE_ACCESS  0

/* On C-SKY, function CSE would allow use of 16-bit jsr instructions
   instead of normal 32-bit calls.  But it also needs a separate constant
   pool entry for the function address and an instruction to load it, and
   may cause additional spills due to increased register pressure, etc.
   It doesn't seem like a good idea overall.  */
#define NO_FUNCTION_CSE 1

/* Try to generate sequences that don't involve branches, we can then use
   conditional instructions.  */
#define BRANCH_COST(speed_p, predictable_p)			\
  csky_default_branch_cost (speed_p, predictable_p)

/* False if short circuit operation is preferred.  */
#define LOGICAL_OP_NON_SHORT_CIRCUIT \
  (csky_default_logical_op_non_short_circuit ())


/******************************************************************
 *		   Generating Code for Profiling		  *
 ******************************************************************/


#define FUNCTION_PROFILER(FILE, LABELNO)

#endif /* GCC_CSKY_H */
