/* Definitions of target machine for GNU compiler, for the HP Spectrum.
   Copyright (C) 1992, 1993, 1994, 1995, 1996, 1997, 1998, 1999, 2000,
   2001, 2002 Free Software Foundation, Inc.
   Contributed by Michael Tiemann (tiemann@cygnus.com) of Cygnus Support
   and Tim Moore (moore@defmacro.cs.utah.edu) of the Center for
   Software Science at the University of Utah.

This file is part of GNU CC.

GNU CC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU CC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU CC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

enum cmp_type				/* comparison type */
{
  CMP_SI,				/* compare integers */
  CMP_SF,				/* compare single precision floats */
  CMP_DF,				/* compare double precision floats */
  CMP_MAX				/* max comparison type */
};

/* For long call handling.  */
extern unsigned long total_code_bytes;

/* Which processor to schedule for.  */

enum processor_type
{
  PROCESSOR_700,
  PROCESSOR_7100,
  PROCESSOR_7100LC,
  PROCESSOR_7200,
  PROCESSOR_7300,
  PROCESSOR_8000
};

/* For -mschedule= option.  */
extern const char *pa_cpu_string;
extern enum processor_type pa_cpu;

#define pa_cpu_attr ((enum attr_cpu)pa_cpu)

/* Which architecture to generate code for.  */

enum architecture_type
{
  ARCHITECTURE_10,
  ARCHITECTURE_11,
  ARCHITECTURE_20
};

struct rtx_def;

/* For -march= option.  */
extern const char *pa_arch_string;
extern enum architecture_type pa_arch;

/* Print subsidiary information on the compiler version in use.  */

#define TARGET_VERSION fputs (" (hppa)", stderr);

/* Run-time compilation parameters selecting different hardware subsets.  */

extern int target_flags;

/* compile code for HP-PA 1.1 ("Snake").  */

#define MASK_PA_11 1

/* Disable all FP registers (they all become fixed).  This may be necessary
   for compiling kernels which perform lazy context switching of FP regs.
   Note if you use this option and try to perform floating point operations
   the compiler will abort!  */

#define MASK_DISABLE_FPREGS 2
#define TARGET_DISABLE_FPREGS (target_flags & MASK_DISABLE_FPREGS)

/* Generate code which assumes that all space register are equivalent.
   Triggers aggressive unscaled index addressing and faster
   builtin_return_address.  */
#define MASK_NO_SPACE_REGS 4
#define TARGET_NO_SPACE_REGS (target_flags & MASK_NO_SPACE_REGS)

/* Allow unconditional jumps in the delay slots of call instructions.  */
#define MASK_JUMP_IN_DELAY 8
#define TARGET_JUMP_IN_DELAY (target_flags & MASK_JUMP_IN_DELAY)

/* Disable indexed addressing modes.  */
#define MASK_DISABLE_INDEXING 32
#define TARGET_DISABLE_INDEXING (target_flags & MASK_DISABLE_INDEXING)

/* Emit code which follows the new portable runtime calling conventions
   HP wants everyone to use for ELF objects.  If at all possible you want
   to avoid this since it's a performance loss for non-prototyped code.

   Note TARGET_PORTABLE_RUNTIME also forces all calls to use inline
   long-call stubs which is quite expensive.  */
#define MASK_PORTABLE_RUNTIME 64
#define TARGET_PORTABLE_RUNTIME (target_flags & MASK_PORTABLE_RUNTIME)

/* Emit directives only understood by GAS.  This allows parameter
   relocations to work for static functions.  There is no way
   to make them work the HP assembler at this time.  */
#define MASK_GAS 128
#define TARGET_GAS (target_flags & MASK_GAS)

/* Emit code for processors which do not have an FPU.  */
#define MASK_SOFT_FLOAT 256
#define TARGET_SOFT_FLOAT (target_flags & MASK_SOFT_FLOAT)

/* Use 3-insn load/store sequences for access to large data segments
   in shared libraries on hpux10.  */
#define MASK_LONG_LOAD_STORE 512
#define TARGET_LONG_LOAD_STORE (target_flags & MASK_LONG_LOAD_STORE)

/* Use a faster sequence for indirect calls.  This assumes that calls
   through function pointers will never cross a space boundary, and
   that the executable is not dynamically linked.  Such assumptions
   are generally safe for building kernels and statically linked
   executables.  Code compiled with this option will fail miserably if
   the executable is dynamically linked or uses nested functions!  */
#define MASK_FAST_INDIRECT_CALLS 1024
#define TARGET_FAST_INDIRECT_CALLS (target_flags & MASK_FAST_INDIRECT_CALLS)

/* Generate code with big switch statements to avoid out of range branches
   occurring within the switch table.  */
#define MASK_BIG_SWITCH 2048
#define TARGET_BIG_SWITCH (target_flags & MASK_BIG_SWITCH)

/* Generate code for the HPPA 2.0 architecture.  TARGET_PA_11 should also be
   true when this is true.  */
#define MASK_PA_20 4096

/* Generate cpp defines for server I/O.  */
#define MASK_SIO 8192
#define TARGET_SIO (target_flags & MASK_SIO)

/* Assume GNU linker by default.  */
#define MASK_GNU_LD 16384
#ifndef TARGET_GNU_LD
#define TARGET_GNU_LD (target_flags & MASK_GNU_LD)
#endif

/* Force generation of long calls.  */
#define MASK_LONG_CALLS 32768
#ifndef TARGET_LONG_CALLS
#define TARGET_LONG_CALLS (target_flags & MASK_LONG_CALLS)
#endif

#ifndef TARGET_PA_10
#define TARGET_PA_10 (target_flags & (MASK_PA_11 | MASK_PA_20) == 0)
#endif

#ifndef TARGET_PA_11
#define TARGET_PA_11 (target_flags & MASK_PA_11)
#endif

#ifndef TARGET_PA_20
#define TARGET_PA_20 (target_flags & MASK_PA_20)
#endif

/* Generate code for the HPPA 2.0 architecture in 64bit mode.  */
#ifndef TARGET_64BIT
#define TARGET_64BIT 0
#endif

/* Generate code for ELF32 ABI.  */
#ifndef TARGET_ELF32
#define TARGET_ELF32 0
#endif

/* Generate code for SOM 32bit ABI.  */
#ifndef TARGET_SOM
#define TARGET_SOM 0
#endif

/* The following three defines are potential target switches.  The current
   defines are optimal given the current capabilities of GAS and GNU ld.  */

/* Define to a C expression evaluating to true to use long absolute calls.
   Currently, only the HP assembler and SOM linker support long absolute
   calls.  They are used only in non-pic code.  */
#define TARGET_LONG_ABS_CALL (TARGET_SOM && !TARGET_GAS)

/* Define to a C expression evaluating to true to use long pic symbol
   difference calls.  This is a call variant similar to the long pic
   pc-relative call.  Long pic symbol difference calls are only used with
   the HP SOM linker.  Currently, only the HP assembler supports these
   calls.  GAS doesn't allow an arbritrary difference of two symbols.  */
#define TARGET_LONG_PIC_SDIFF_CALL (!TARGET_GAS)

/* Define to a C expression evaluating to true to use long pic
   pc-relative calls.  Long pic pc-relative calls are only used with
   GAS.  Currently, they are usable for calls within a module but
   not for external calls.  */
#define TARGET_LONG_PIC_PCREL_CALL 0

/* Define to a C expression evaluating to true to use SOM secondary
   definition symbols for weak support.  Linker support for secondary
   definition symbols is buggy prior to HP-UX 11.X.  */
#define TARGET_SOM_SDEF 0

/* Define to a C expression evaluating to true to save the entry value
   of SP in the current frame marker.  This is normally unnecessary.
   However, the HP-UX unwind library looks at the SAVE_SP callinfo flag.
   HP compilers don't use this flag but it is supported by the assembler.
   We set this flag to indicate that register %r3 has been saved at the
   start of the frame.  Thus, when the HP unwind library is used, we
   need to generate additional code to save SP into the frame marker.  */
#define TARGET_HPUX_UNWIND_LIBRARY 0

/* Macro to define tables used to set the flags.  This is a
   list in braces of target switches with each switch being
   { "NAME", VALUE, "HELP_STRING" }.  VALUE is the bits to set,
   or minus the bits to clear.  An empty string NAME is used to
   identify the default VALUE.  Do not mark empty strings for
   translation.  */

#define TARGET_SWITCHES \
  {{ "snake",			 MASK_PA_11,				\
     N_("Generate PA1.1 code") },					\
   { "nosnake",			-(MASK_PA_11 | MASK_PA_20),		\
     N_("Generate PA1.0 code") },					\
   { "pa-risc-1-0",		-(MASK_PA_11 | MASK_PA_20),		\
     N_("Generate PA1.0 code") },					\
   { "pa-risc-1-1",		 MASK_PA_11,				\
     N_("Generate PA1.1 code") },					\
   { "pa-risc-2-0",		 MASK_PA_20,				\
     N_("Generate PA2.0 code (requires binutils 2.10 or later)") },	\
   { "disable-fpregs",		 MASK_DISABLE_FPREGS,			\
     N_("Disable FP regs") },						\
   { "no-disable-fpregs",	-MASK_DISABLE_FPREGS,			\
     N_("Do not disable FP regs") },					\
   { "no-space-regs",		 MASK_NO_SPACE_REGS,			\
     N_("Disable space regs") },					\
   { "space-regs",		-MASK_NO_SPACE_REGS,			\
     N_("Do not disable space regs") },					\
   { "jump-in-delay",		 MASK_JUMP_IN_DELAY,			\
     N_("Put jumps in call delay slots") },				\
   { "no-jump-in-delay",	-MASK_JUMP_IN_DELAY,			\
     N_("Do not put jumps in call delay slots") },			\
   { "disable-indexing",	 MASK_DISABLE_INDEXING,			\
     N_("Disable indexed addressing") },				\
   { "no-disable-indexing",	-MASK_DISABLE_INDEXING,			\
     N_("Do not disable indexed addressing") },				\
   { "portable-runtime",	 MASK_PORTABLE_RUNTIME,			\
     N_("Use portable calling conventions") },				\
   { "no-portable-runtime",	-MASK_PORTABLE_RUNTIME,			\
     N_("Do not use portable calling conventions") },			\
   { "gas",			 MASK_GAS,				\
     N_("Assume code will be assembled by GAS") },			\
   { "no-gas",			-MASK_GAS,				\
     N_("Do not assume code will be assembled by GAS") },		\
   { "soft-float",		 MASK_SOFT_FLOAT,			\
     N_("Use software floating point") },				\
   { "no-soft-float",		-MASK_SOFT_FLOAT,			\
     N_("Do not use software floating point") },			\
   { "long-load-store",		 MASK_LONG_LOAD_STORE,			\
     N_("Emit long load/store sequences") },				\
   { "no-long-load-store",	-MASK_LONG_LOAD_STORE,			\
     N_("Do not emit long load/store sequences") },			\
   { "fast-indirect-calls",	 MASK_FAST_INDIRECT_CALLS,		\
     N_("Generate fast indirect calls") },				\
   { "no-fast-indirect-calls",	-MASK_FAST_INDIRECT_CALLS,		\
     N_("Do not generate fast indirect calls") },			\
   { "big-switch",		 MASK_BIG_SWITCH,			\
     N_("Generate code for huge switch statements") },			\
   { "no-big-switch",		-MASK_BIG_SWITCH,			\
     N_("Do not generate code for huge switch statements") },		\
   { "long-calls",		 MASK_LONG_CALLS,			\
     N_("Always generate long calls") },				\
   { "no-long-calls",		-MASK_LONG_CALLS,			\
     N_("Generate long calls only when needed") },			\
   { "linker-opt",		 0,					\
     N_("Enable linker optimizations") },				\
   SUBTARGET_SWITCHES							\
   { "",			 TARGET_DEFAULT | TARGET_CPU_DEFAULT,	\
     NULL }}

#ifndef TARGET_DEFAULT
#define TARGET_DEFAULT (MASK_GAS | MASK_JUMP_IN_DELAY)
#endif

#ifndef TARGET_CPU_DEFAULT
#define TARGET_CPU_DEFAULT 0
#endif

#ifndef SUBTARGET_SWITCHES
#define SUBTARGET_SWITCHES
#endif

#ifndef TARGET_SCHED_DEFAULT
#define TARGET_SCHED_DEFAULT "8000"
#endif

#define TARGET_OPTIONS							\
{									\
  { "schedule=",		&pa_cpu_string,				\
    N_("Specify CPU for scheduling purposes") },			\
  { "arch=",			&pa_arch_string,			\
    N_("Specify architecture for code generation.  Values are 1.0, 1.1, and 2.0.  2.0 requires gas snapshot 19990413 or later.") }\
}

/* Specify the dialect of assembler to use.  New mnemonics is dialect one
   and the old mnemonics are dialect zero.  */
#define ASSEMBLER_DIALECT (TARGET_PA_20 ? 1 : 0)

#define OVERRIDE_OPTIONS override_options ()

/* stabs-in-som is nearly identical to stabs-in-elf.  To avoid useless
   code duplication we simply include this file and override as needed.  */
#include "dbxelf.h"

/* We do not have to be compatible with dbx, so we enable gdb extensions
   by default.  */
#define DEFAULT_GDB_EXTENSIONS 1

/* This used to be zero (no max length), but big enums and such can
   cause huge strings which killed gas.

   We also have to avoid lossage in dbxout.c -- it does not compute the
   string size accurately, so we are real conservative here.  */
#undef DBX_CONTIN_LENGTH
#define DBX_CONTIN_LENGTH 3000

/* Only labels should ever begin in column zero.  */
#define ASM_STABS_OP "\t.stabs\t"
#define ASM_STABN_OP "\t.stabn\t"

/* GDB always assumes the current function's frame begins at the value
   of the stack pointer upon entry to the current function.  Accessing
   local variables and parameters passed on the stack is done using the
   base of the frame + an offset provided by GCC.

   For functions which have frame pointers this method works fine;
   the (frame pointer) == (stack pointer at function entry) and GCC provides
   an offset relative to the frame pointer.

   This loses for functions without a frame pointer; GCC provides an offset
   which is relative to the stack pointer after adjusting for the function's
   frame size.  GDB would prefer the offset to be relative to the value of
   the stack pointer at the function's entry.  Yuk!  */
#define DEBUGGER_AUTO_OFFSET(X) \
  ((GET_CODE (X) == PLUS ? INTVAL (XEXP (X, 1)) : 0) \
    + (frame_pointer_needed ? 0 : compute_frame_size (get_frame_size (), 0)))

#define DEBUGGER_ARG_OFFSET(OFFSET, X) \
  ((GET_CODE (X) == PLUS ? OFFSET : 0) \
    + (frame_pointer_needed ? 0 : compute_frame_size (get_frame_size (), 0)))

#define TARGET_CPU_CPP_BUILTINS()				\
do {								\
     builtin_assert("cpu=hppa");				\
     builtin_assert("machine=hppa");				\
     builtin_define("__hppa");					\
     builtin_define("__hppa__");				\
     if (TARGET_64BIT)						\
       {							\
	 builtin_define("_LP64");				\
	 builtin_define("__LP64__");				\
       }							\
     if (TARGET_PA_20)						\
       builtin_define("_PA_RISC2_0");				\
     else if (TARGET_PA_11)					\
       builtin_define("_PA_RISC1_1");				\
     else							\
       builtin_define("_PA_RISC1_0");				\
} while (0)

/* An old set of OS defines for various BSD-like systems.  */
#define TARGET_OS_CPP_BUILTINS()				\
  do								\
    {								\
	builtin_define_std ("REVARGV");				\
	builtin_define_std ("hp800");				\
	builtin_define_std ("hp9000");				\
	builtin_define_std ("hp9k8");				\
	if (c_language != clk_cplusplus				\
	    && !flag_iso)					\
	  builtin_define ("hppa");				\
	builtin_define_std ("spectrum");			\
	builtin_define_std ("unix");				\
	builtin_assert ("system=bsd");				\
	builtin_assert ("system=unix");				\
    }								\
  while (0)

#define CC1_SPEC "%{pg:} %{p:}"

#define LINK_SPEC "%{mlinker-opt:-O} %{!shared:-u main} %{shared:-b}"

/* We don't want -lg.  */
#ifndef LIB_SPEC
#define LIB_SPEC "%{!p:%{!pg:-lc}}%{p:-lc_p}%{pg:-lc_p}"
#endif

/* This macro defines command-line switches that modify the default
   target name.

   The definition is be an initializer for an array of structures.  Each
   array element has have three elements: the switch name, one of the
   enumeration codes ADD or DELETE to indicate whether the string should be
   inserted or deleted, and the string to be inserted or deleted.  */
#define MODIFY_TARGET_NAME {{"-32", DELETE, "64"}, {"-64", ADD, "64"}}

/* Make gcc agree with <machine/ansi.h> */

#define SIZE_TYPE "unsigned int"
#define PTRDIFF_TYPE "int"
#define WCHAR_TYPE "unsigned int"
#define WCHAR_TYPE_SIZE 32

/* Show we can debug even without a frame pointer.  */
#define CAN_DEBUG_WITHOUT_FP

/* Machine dependent reorg pass.  */
#define MACHINE_DEPENDENT_REORG(X) pa_reorg(X)


/* target machine storage layout */

/* Define this macro if it is advisable to hold scalars in registers
   in a wider mode than that declared by the program.  In such cases, 
   the value is constrained to be within the bounds of the declared
   type, but kept valid in the wider mode.  The signedness of the
   extension may differ from that of the type.  */

#define PROMOTE_MODE(MODE,UNSIGNEDP,TYPE)  \
  if (GET_MODE_CLASS (MODE) == MODE_INT	\
      && GET_MODE_SIZE (MODE) < UNITS_PER_WORD)  	\
    (MODE) = word_mode;

/* Define this if most significant bit is lowest numbered
   in instructions that operate on numbered bit-fields.  */
#define BITS_BIG_ENDIAN 1

/* Define this if most significant byte of a word is the lowest numbered.  */
/* That is true on the HP-PA.  */
#define BYTES_BIG_ENDIAN 1

/* Define this if most significant word of a multiword number is lowest
   numbered.  */
#define WORDS_BIG_ENDIAN 1

#define MAX_BITS_PER_WORD 64
#define MAX_LONG_TYPE_SIZE 32

/* Width of a word, in units (bytes).  */
#define UNITS_PER_WORD (TARGET_64BIT ? 8 : 4)
#define MIN_UNITS_PER_WORD 4

/* Allocation boundary (in *bits*) for storing arguments in argument list.  */
#define PARM_BOUNDARY BITS_PER_WORD

/* Largest alignment required for any stack parameter, in bits.
   Don't define this if it is equal to PARM_BOUNDARY */
#define MAX_PARM_BOUNDARY BIGGEST_ALIGNMENT

/* Boundary (in *bits*) on which stack pointer is always aligned;
   certain optimizations in combine depend on this.

   The HP-UX runtime documents mandate 64-byte and 16-byte alignment for
   the stack on the 32 and 64-bit ports, respectively.  However, we
   are only guaranteed that the stack is aligned to BIGGEST_ALIGNMENT
   in main.  Thus, we treat the former as the preferred alignment.  */
#define STACK_BOUNDARY BIGGEST_ALIGNMENT
#define PREFERRED_STACK_BOUNDARY (TARGET_64BIT ? 128 : 512)

/* Allocation boundary (in *bits*) for the code of a function.  */
#define FUNCTION_BOUNDARY BITS_PER_WORD

/* Alignment of field after `int : 0' in a structure.  */
#define EMPTY_FIELD_BOUNDARY 32

/* Every structure's size must be a multiple of this.  */
#define STRUCTURE_SIZE_BOUNDARY 8

/* A bit-field declared as `int' forces `int' alignment for the struct.  */
#define PCC_BITFIELD_TYPE_MATTERS 1

/* No data type wants to be aligned rounder than this.  */
#define BIGGEST_ALIGNMENT (2 * BITS_PER_WORD)

/* Get around hp-ux assembler bug, and make strcpy of constants fast.  */
#define CONSTANT_ALIGNMENT(CODE, TYPEALIGN) \
  ((TYPEALIGN) < 32 ? 32 : (TYPEALIGN))

/* Make arrays of chars word-aligned for the same reasons.  */
#define DATA_ALIGNMENT(TYPE, ALIGN)		\
  (TREE_CODE (TYPE) == ARRAY_TYPE		\
   && TYPE_MODE (TREE_TYPE (TYPE)) == QImode	\
   && (ALIGN) < BITS_PER_WORD ? BITS_PER_WORD : (ALIGN))

/* Set this nonzero if move instructions will actually fail to work
   when given unaligned data.  */
#define STRICT_ALIGNMENT 1

/* Generate calls to memcpy, memcmp and memset.  */
#define TARGET_MEM_FUNCTIONS

/* Value is 1 if it is a good idea to tie two pseudo registers
   when one has mode MODE1 and one has mode MODE2.
   If HARD_REGNO_MODE_OK could produce different values for MODE1 and MODE2,
   for any hard reg, then this must be 0 for correct output.  */
#define MODES_TIEABLE_P(MODE1, MODE2) \
  (GET_MODE_CLASS (MODE1) == GET_MODE_CLASS (MODE2))

/* Specify the registers used for certain standard purposes.
   The values of these macros are register numbers.  */

/* The HP-PA pc isn't overloaded on a register that the compiler knows about.  */
/* #define PC_REGNUM  */

/* Register to use for pushing function arguments.  */
#define STACK_POINTER_REGNUM 30

/* Base register for access to local variables of the function.  */
#define FRAME_POINTER_REGNUM 3

/* Value should be nonzero if functions must have frame pointers.  */
#define FRAME_POINTER_REQUIRED \
  (current_function_calls_alloca)

/* C statement to store the difference between the frame pointer
   and the stack pointer values immediately after the function prologue.

   Note, we always pretend that this is a leaf function because if
   it's not, there's no point in trying to eliminate the
   frame pointer.  If it is a leaf function, we guessed right!  */
#define INITIAL_FRAME_POINTER_OFFSET(VAR) \
  do {(VAR) = - compute_frame_size (get_frame_size (), 0);} while (0)

/* Base register for access to arguments of the function.  */
#define ARG_POINTER_REGNUM 3

/* Register in which static-chain is passed to a function.  */
#define STATIC_CHAIN_REGNUM 29

/* Register which holds offset table for position-independent
   data references.  */

#define PIC_OFFSET_TABLE_REGNUM (TARGET_64BIT ? 27 : 19)
#define PIC_OFFSET_TABLE_REG_CALL_CLOBBERED 1

/* Function to return the rtx used to save the pic offset table register
   across function calls.  */
extern struct rtx_def *hppa_pic_save_rtx PARAMS ((void));

#define DEFAULT_PCC_STRUCT_RETURN 0

/* SOM ABI says that objects larger than 64 bits are returned in memory.
   PA64 ABI says that objects larger than 128 bits are returned in memory.
   Note, int_size_in_bytes can return -1 if the size of the object is
   variable or larger than the maximum value that can be expressed as
   a HOST_WIDE_INT.   It can also return zero for an empty type.  The
   simplest way to handle variable and empty types is to pass them in
   memory.  This avoids problems in defining the boundaries of argument
   slots, allocating registers, etc.  */
#define RETURN_IN_MEMORY(TYPE)	\
  (int_size_in_bytes (TYPE) > (TARGET_64BIT ? 16 : 8)	\
   || int_size_in_bytes (TYPE) <= 0)

/* Register in which address to store a structure value
   is passed to a function.  */
#define STRUCT_VALUE_REGNUM 28

/* Describe how we implement __builtin_eh_return.  */
#define EH_RETURN_DATA_REGNO(N)	\
  ((N) < 3 ? (N) + 20 : (N) == 3 ? 31 : INVALID_REGNUM)
#define EH_RETURN_STACKADJ_RTX	gen_rtx_REG (Pmode, 29)
#define EH_RETURN_HANDLER_RTX \
  gen_rtx_MEM (word_mode,						\
	       gen_rtx_PLUS (word_mode, frame_pointer_rtx,		\
			     TARGET_64BIT ? GEN_INT (-16) : GEN_INT (-20)))
			  	

/* Offset from the argument pointer register value to the top of
   stack.  This is different from FIRST_PARM_OFFSET because of the
   frame marker.  */
#define ARG_POINTER_CFA_OFFSET(FNDECL) 0

/* The letters I, J, K, L and M in a register constraint string
   can be used to stand for particular ranges of immediate operands.
   This macro defines what the ranges are.
   C is the letter, and VALUE is a constant value.
   Return 1 if VALUE is in the range specified by C.

   `I' is used for the 11 bit constants.
   `J' is used for the 14 bit constants.
   `K' is used for values that can be moved with a zdepi insn.
   `L' is used for the 5 bit constants.
   `M' is used for 0.
   `N' is used for values with the least significant 11 bits equal to zero
	                  and when sign extended from 32 to 64 bits the
			  value does not change.
   `O' is used for numbers n such that n+1 is a power of 2.
   */

#define CONST_OK_FOR_LETTER_P(VALUE, C)  \
  ((C) == 'I' ? VAL_11_BITS_P (VALUE)					\
   : (C) == 'J' ? VAL_14_BITS_P (VALUE)					\
   : (C) == 'K' ? zdepi_cint_p (VALUE)					\
   : (C) == 'L' ? VAL_5_BITS_P (VALUE)					\
   : (C) == 'M' ? (VALUE) == 0						\
   : (C) == 'N' ? (((VALUE) & (((HOST_WIDE_INT) -1 << 31) | 0x7ff)) == 0 \
		   || (((VALUE) & (((HOST_WIDE_INT) -1 << 31) | 0x7ff))	\
		       == (HOST_WIDE_INT) -1 << 31))			\
   : (C) == 'O' ? (((VALUE) & ((VALUE) + 1)) == 0)			\
   : (C) == 'P' ? and_mask_p (VALUE)					\
   : 0)

/* Similar, but for floating or large integer constants, and defining letters
   G and H.   Here VALUE is the CONST_DOUBLE rtx itself.

   For PA, `G' is the floating-point constant zero.  `H' is undefined.  */

#define CONST_DOUBLE_OK_FOR_LETTER_P(VALUE, C)  			\
  ((C) == 'G' ? (GET_MODE_CLASS (GET_MODE (VALUE)) == MODE_FLOAT	\
		 && (VALUE) == CONST0_RTX (GET_MODE (VALUE)))		\
   : 0)

/* The class value for index registers, and the one for base regs.  */
#define INDEX_REG_CLASS GENERAL_REGS
#define BASE_REG_CLASS GENERAL_REGS

#define FP_REG_CLASS_P(CLASS) \
  ((CLASS) == FP_REGS || (CLASS) == FPUPPER_REGS)

/* True if register is floating-point.  */
#define FP_REGNO_P(N) ((N) >= FP_REG_FIRST && (N) <= FP_REG_LAST)

/* Given an rtx X being reloaded into a reg required to be
   in class CLASS, return the class of reg to actually use.
   In general this is just CLASS; but on some machines
   in some cases it is preferable to use a more restrictive class.  */
#define PREFERRED_RELOAD_CLASS(X,CLASS) (CLASS)

/* Return the register class of a scratch register needed to copy IN into
   or out of a register in CLASS in MODE.  If it can be done directly
   NO_REGS is returned. 

  Avoid doing any work for the common case calls.  */

#define SECONDARY_RELOAD_CLASS(CLASS,MODE,IN) \
  ((CLASS == BASE_REG_CLASS && GET_CODE (IN) == REG		\
    && REGNO (IN) < FIRST_PSEUDO_REGISTER)			\
   ? NO_REGS : secondary_reload_class (CLASS, MODE, IN))

/* On the PA it is not possible to directly move data between
   GENERAL_REGS and FP_REGS.  */
#define SECONDARY_MEMORY_NEEDED(CLASS1, CLASS2, MODE)  \
  (FP_REG_CLASS_P (CLASS1) != FP_REG_CLASS_P (CLASS2))

/* Return the stack location to use for secondary memory needed reloads.  */
#define SECONDARY_MEMORY_NEEDED_RTX(MODE) \
  gen_rtx_MEM (MODE, gen_rtx_PLUS (Pmode, stack_pointer_rtx, GEN_INT (-16)))


/* Stack layout; function entry, exit and calling.  */

/* Define this if pushing a word on the stack
   makes the stack pointer a smaller address.  */
/* #define STACK_GROWS_DOWNWARD */

/* Believe it or not.  */
#define ARGS_GROW_DOWNWARD

/* Define this if the nominal address of the stack frame
   is at the high-address end of the local variables;
   that is, each additional local variable allocated
   goes at a more negative offset in the frame.  */
/* #define FRAME_GROWS_DOWNWARD */

/* Offset within stack frame to start allocating local variables at.
   If FRAME_GROWS_DOWNWARD, this is the offset to the END of the
   first local allocated.  Otherwise, it is the offset to the BEGINNING
   of the first local allocated.  The start of the locals must lie on
   a STACK_BOUNDARY or else the frame size of leaf functions will not
   be zero.  */
#define STARTING_FRAME_OFFSET (TARGET_64BIT ? 16 : 8)

/* If we generate an insn to push BYTES bytes,
   this says how many the stack pointer really advances by.
   On the HP-PA, don't define this because there are no push insns.  */
/*  #define PUSH_ROUNDING(BYTES) */

/* Offset of first parameter from the argument pointer register value.
   This value will be negated because the arguments grow down.
   Also note that on STACK_GROWS_UPWARD machines (such as this one)
   this is the distance from the frame pointer to the end of the first
   argument, not it's beginning.  To get the real offset of the first
   argument, the size of the argument must be added.  */

#define FIRST_PARM_OFFSET(FNDECL) (TARGET_64BIT ? -64 : -32)

/* When a parameter is passed in a register, stack space is still
   allocated for it.  */
#define REG_PARM_STACK_SPACE(DECL) (TARGET_64BIT ? 64 : 16)

/* Define this if the above stack space is to be considered part of the
   space allocated by the caller.  */
#define OUTGOING_REG_PARM_STACK_SPACE

/* Keep the stack pointer constant throughout the function.
   This is both an optimization and a necessity: longjmp
   doesn't behave itself when the stack pointer moves within
   the function!  */
#define ACCUMULATE_OUTGOING_ARGS 1

/* The weird HPPA calling conventions require a minimum of 48 bytes on
   the stack: 16 bytes for register saves, and 32 bytes for magic.
   This is the difference between the logical top of stack and the
   actual sp.

   On the 64-bit port, the HP C compiler allocates a 48-byte frame
   marker, although the runtime documentation only describes a 16
   byte marker.  For compatibility, we allocate 48 bytes.  */
#define STACK_POINTER_OFFSET \
  (TARGET_64BIT ? -(current_function_outgoing_args_size + 48): -32)

#define STACK_DYNAMIC_OFFSET(FNDECL)	\
  (TARGET_64BIT				\
   ? (STACK_POINTER_OFFSET)		\
   : ((STACK_POINTER_OFFSET) - current_function_outgoing_args_size))

/* Value is 1 if returning from a function call automatically
   pops the arguments described by the number-of-args field in the call.
   FUNDECL is the declaration node of the function (as a tree),
   FUNTYPE is the data type of the function (as a tree),
   or for a library call it is an identifier node for the subroutine name.  */

#define RETURN_POPS_ARGS(FUNDECL,FUNTYPE,SIZE) 0

/* Define how to find the value returned by a function.
   VALTYPE is the data type of the value (as a tree).
   If the precise function being called is known, FUNC is its FUNCTION_DECL;
   otherwise, FUNC is 0.  */

#define FUNCTION_VALUE(VALTYPE, FUNC) function_value (VALTYPE, FUNC)

/* Define how to find the value returned by a library function
   assuming the value has mode MODE.  */

#define LIBCALL_VALUE(MODE)	\
  gen_rtx_REG (MODE,							\
	       (! TARGET_SOFT_FLOAT					\
		&& ((MODE) == SFmode || (MODE) == DFmode) ? 32 : 28))

/* 1 if N is a possible register number for a function value
   as seen by the caller.  */

#define FUNCTION_VALUE_REGNO_P(N) \
  ((N) == 28 || (! TARGET_SOFT_FLOAT && (N) == 32))


/* Define a data type for recording info about an argument list
   during the scan of that argument list.  This data type should
   hold all necessary information about the function itself
   and about the args processed so far, enough to enable macros
   such as FUNCTION_ARG to determine where the next arg should go.

   On the HP-PA, the WORDS field holds the number of words
   of arguments scanned so far (including the invisible argument,
   if any, which holds the structure-value-address).  Thus, 4 or
   more means all following args should go on the stack.
   
   The INCOMING field tracks whether this is an "incoming" or
   "outgoing" argument.
   
   The INDIRECT field indicates whether this is is an indirect
   call or not.
   
   The NARGS_PROTOTYPE field indicates that an argument does not
   have a prototype when it less than or equal to 0.  */

struct hppa_args {int words, nargs_prototype, incoming, indirect; };

#define CUMULATIVE_ARGS struct hppa_args

/* Initialize a variable CUM of type CUMULATIVE_ARGS
   for a call to a function whose data type is FNTYPE.
   For a library call, FNTYPE is 0.  */

#define INIT_CUMULATIVE_ARGS(CUM,FNTYPE,LIBNAME,INDIRECT) \
  (CUM).words = 0, 							\
  (CUM).incoming = 0,							\
  (CUM).indirect = INDIRECT,						\
  (CUM).nargs_prototype = (FNTYPE && TYPE_ARG_TYPES (FNTYPE)		\
			   ? (list_length (TYPE_ARG_TYPES (FNTYPE)) - 1	\
			      + (TYPE_MODE (TREE_TYPE (FNTYPE)) == BLKmode \
				 || RETURN_IN_MEMORY (TREE_TYPE (FNTYPE)))) \
			   : 0)



/* Similar, but when scanning the definition of a procedure.  We always
   set NARGS_PROTOTYPE large so we never return a PARALLEL.  */

#define INIT_CUMULATIVE_INCOMING_ARGS(CUM,FNTYPE,IGNORE) \
  (CUM).words = 0,				\
  (CUM).incoming = 1,				\
  (CUM).indirect = 0,				\
  (CUM).nargs_prototype = 1000

/* Figure out the size in words of the function argument.  The size
   returned by this macro should always be greater than zero because
   we pass variable and zero sized objects by reference.  */

#define FUNCTION_ARG_SIZE(MODE, TYPE)	\
  ((((MODE) != BLKmode \
     ? (HOST_WIDE_INT) GET_MODE_SIZE (MODE) \
     : int_size_in_bytes (TYPE)) + UNITS_PER_WORD - 1) / UNITS_PER_WORD)

/* Update the data in CUM to advance over an argument
   of mode MODE and data type TYPE.
   (TYPE is null for libcalls where that information may not be available.)  */

#define FUNCTION_ARG_ADVANCE(CUM, MODE, TYPE, NAMED)			\
{ (CUM).nargs_prototype--;						\
  (CUM).words += FUNCTION_ARG_SIZE(MODE, TYPE)	 			\
    + (((CUM).words & 01) && (TYPE) != 0				\
	&& FUNCTION_ARG_SIZE(MODE, TYPE) > 1);				\
}

/* Determine where to put an argument to a function.
   Value is zero to push the argument on the stack,
   or a hard register in which to store the argument.

   MODE is the argument's machine mode.
   TYPE is the data type of the argument (as a tree).
    This is null for libcalls where that information may
    not be available.
   CUM is a variable of type CUMULATIVE_ARGS which gives info about
    the preceding args and about the function being called.
   NAMED is nonzero if this argument is a named parameter
    (otherwise it is an extra parameter matching an ellipsis).

   On the HP-PA the first four words of args are normally in registers
   and the rest are pushed.  But any arg that won't entirely fit in regs
   is pushed.

   Arguments passed in registers are either 1 or 2 words long.

   The caller must make a distinction between calls to explicitly named
   functions and calls through pointers to functions -- the conventions
   are different!  Calls through pointers to functions only use general
   registers for the first four argument words.

   Of course all this is different for the portable runtime model
   HP wants everyone to use for ELF.  Ugh.  Here's a quick description
   of how it's supposed to work.

   1) callee side remains unchanged.  It expects integer args to be
   in the integer registers, float args in the float registers and
   unnamed args in integer registers.

   2) caller side now depends on if the function being called has
   a prototype in scope (rather than if it's being called indirectly).

      2a) If there is a prototype in scope, then arguments are passed
      according to their type (ints in integer registers, floats in float
      registers, unnamed args in integer registers.

      2b) If there is no prototype in scope, then floating point arguments
      are passed in both integer and float registers.  egad.

  FYI: The portable parameter passing conventions are almost exactly like
  the standard parameter passing conventions on the RS6000.  That's why
  you'll see lots of similar code in rs6000.h.  */

#define FUNCTION_ARG_PADDING(MODE, TYPE) function_arg_padding ((MODE), (TYPE))

/* Do not expect to understand this without reading it several times.  I'm
   tempted to try and simply it, but I worry about breaking something.  */

#define FUNCTION_ARG(CUM, MODE, TYPE, NAMED) \
  function_arg (&CUM, MODE, TYPE, NAMED)

/* Nonzero if we do not know how to pass TYPE solely in registers.  */
#define MUST_PASS_IN_STACK(MODE,TYPE) \
  ((TYPE) != 0							\
   && (TREE_CODE (TYPE_SIZE (TYPE)) != INTEGER_CST		\
       || TREE_ADDRESSABLE (TYPE)))

/* For an arg passed partly in registers and partly in memory,
   this is the number of registers used.
   For args passed entirely in registers or entirely in memory, zero.  */

/* For PA32 there are never split arguments. PA64, on the other hand, can
   pass arguments partially in registers and partially in memory.  */
#define FUNCTION_ARG_PARTIAL_NREGS(CUM, MODE, TYPE, NAMED) \
  (TARGET_64BIT ? function_arg_partial_nregs (&CUM, MODE, TYPE, NAMED) : 0)

/* If defined, a C expression that gives the alignment boundary, in
   bits, of an argument with the specified mode and type.  If it is
   not defined,  `PARM_BOUNDARY' is used for all arguments.  */

/* Arguments larger than one word are double word aligned.  */

#define FUNCTION_ARG_BOUNDARY(MODE, TYPE)				\
  (((TYPE)								\
    ? (integer_zerop (TYPE_SIZE (TYPE))					\
       || !TREE_CONSTANT (TYPE_SIZE (TYPE))				\
       || int_size_in_bytes (TYPE) <= UNITS_PER_WORD)			\
    : GET_MODE_SIZE(MODE) <= UNITS_PER_WORD)				\
   ? PARM_BOUNDARY : MAX_PARM_BOUNDARY)

/* In the 32-bit runtime, arguments larger than eight bytes are passed
   by invisible reference.  As a GCC extension, we also pass anything
   with a zero or variable size by reference.

   The 64-bit runtime does not describe passing any types by invisible
   reference.  The internals of GCC can't currently handle passing
   empty structures, and zero or variable length arrays when they are
   not passed entirely on the stack or by reference.  Thus, as a GCC
   extension, we pass these types by reference.  The HP compiler doesn't
   support these types, so hopefully there shouldn't be any compatibility
   issues.  This may have to be revisited when HP releases a C99 compiler
   or updates the ABI.  */
#define FUNCTION_ARG_PASS_BY_REFERENCE(CUM, MODE, TYPE, NAMED)		\
  (TARGET_64BIT								\
   ? ((TYPE) && int_size_in_bytes (TYPE) <= 0)				\
   : (((TYPE) && (int_size_in_bytes (TYPE) > 8				\
		  || int_size_in_bytes (TYPE) <= 0))			\
      || ((MODE) && GET_MODE_SIZE (MODE) > 8)))
 
#define FUNCTION_ARG_CALLEE_COPIES(CUM, MODE, TYPE, NAMED) 		\
  FUNCTION_ARG_PASS_BY_REFERENCE (CUM, MODE, TYPE, NAMED)


extern GTY(()) rtx hppa_compare_op0;
extern GTY(()) rtx hppa_compare_op1;
extern enum cmp_type hppa_branch_type;

/* On HPPA, we emit profiling code as rtl via PROFILE_HOOK rather than
   as assembly via FUNCTION_PROFILER.  Just output a local label.
   We can't use the function label because the GAS SOM target can't
   handle the difference of a global symbol and a local symbol.  */

#ifndef FUNC_BEGIN_PROLOG_LABEL
#define FUNC_BEGIN_PROLOG_LABEL        "LFBP"
#endif

#define FUNCTION_PROFILER(FILE, LABEL) \
  ASM_OUTPUT_INTERNAL_LABEL (FILE, FUNC_BEGIN_PROLOG_LABEL, LABEL)

#define PROFILE_HOOK(label_no) hppa_profile_hook (label_no)
void hppa_profile_hook PARAMS ((int label_no));

/* The profile counter if emitted must come before the prologue.  */
#define PROFILE_BEFORE_PROLOGUE 1

/* EXIT_IGNORE_STACK should be nonzero if, when returning from a function,
   the stack pointer does not matter.  The value is tested only in
   functions that have frame pointers.
   No definition is equivalent to always zero.  */

extern int may_call_alloca;

#define EXIT_IGNORE_STACK	\
 (get_frame_size () != 0	\
  || current_function_calls_alloca || current_function_outgoing_args_size)

/* Output assembler code for a block containing the constant parts
   of a trampoline, leaving space for the variable parts.\

   The trampoline sets the static chain pointer to STATIC_CHAIN_REGNUM
   and then branches to the specified routine.

   This code template is copied from text segment to stack location
   and then patched with INITIALIZE_TRAMPOLINE to contain
   valid values, and then entered as a subroutine.

   It is best to keep this as small as possible to avoid having to
   flush multiple lines in the cache.  */

#define TRAMPOLINE_TEMPLATE(FILE) 					\
  {									\
    if (! TARGET_64BIT)							\
      {									\
	fputs ("\tldw	36(%r22),%r21\n", FILE);			\
	fputs ("\tbb,>=,n	%r21,30,.+16\n", FILE);			\
	if (ASSEMBLER_DIALECT == 0)					\
	  fputs ("\tdepi	0,31,2,%r21\n", FILE);			\
	else								\
	  fputs ("\tdepwi	0,31,2,%r21\n", FILE);			\
	fputs ("\tldw	4(%r21),%r19\n", FILE);				\
	fputs ("\tldw	0(%r21),%r21\n", FILE);				\
	fputs ("\tldsid	(%r21),%r1\n", FILE);				\
	fputs ("\tmtsp	%r1,%sr0\n", FILE);				\
	fputs ("\tbe	0(%sr0,%r21)\n", FILE);				\
	fputs ("\tldw	40(%r22),%r29\n", FILE);			\
	fputs ("\t.word	0\n", FILE);					\
	fputs ("\t.word	0\n", FILE);					\
	fputs ("\t.word	0\n", FILE);					\
	fputs ("\t.word	0\n", FILE);					\
      }									\
    else								\
      {									\
	fputs ("\t.dword 0\n", FILE);					\
	fputs ("\t.dword 0\n", FILE);					\
	fputs ("\t.dword 0\n", FILE);					\
	fputs ("\t.dword 0\n", FILE);					\
	fputs ("\tmfia	%r31\n", FILE);					\
	fputs ("\tldd	24(%r31),%r1\n", FILE);				\
	fputs ("\tldd	24(%r1),%r27\n", FILE);				\
	fputs ("\tldd	16(%r1),%r1\n", FILE);				\
	fputs ("\tbve	(%r1)\n", FILE);				\
	fputs ("\tldd	32(%r31),%r31\n", FILE);			\
	fputs ("\t.dword 0  ; fptr\n", FILE);				\
	fputs ("\t.dword 0  ; static link\n", FILE);			\
      }									\
  }

/* Length in units of the trampoline for entering a nested function.

   Flush the cache entries corresponding to the first and last addresses
   of the trampoline.  This is necessary as the trampoline may cross two
   cache lines.

   If the code part of the trampoline ever grows to > 32 bytes, then it
   will become necessary to hack on the cacheflush pattern in pa.md.  */

#define TRAMPOLINE_SIZE (TARGET_64BIT ? 72 : 52)

/* Emit RTL insns to initialize the variable parts of a trampoline.
   FNADDR is an RTX for the address of the function's pure code.
   CXT is an RTX for the static chain value for the function.

   Move the function address to the trampoline template at offset 36.
   Move the static chain value to trampoline template at offset 40.
   Move the trampoline address to trampoline template at offset 44.
   Move r19 to trampoline template at offset 48.  The latter two
   words create a plabel for the indirect call to the trampoline.  */

#define INITIALIZE_TRAMPOLINE(TRAMP, FNADDR, CXT) 			\
{									\
  if (! TARGET_64BIT)							\
    {									\
      rtx start_addr, end_addr;						\
									\
      start_addr = memory_address (Pmode, plus_constant ((TRAMP), 36));	\
      emit_move_insn (gen_rtx_MEM (Pmode, start_addr), (FNADDR));	\
      start_addr = memory_address (Pmode, plus_constant ((TRAMP), 40));	\
      emit_move_insn (gen_rtx_MEM (Pmode, start_addr), (CXT));		\
      start_addr = memory_address (Pmode, plus_constant ((TRAMP), 44));	\
      emit_move_insn (gen_rtx_MEM (Pmode, start_addr), (TRAMP));	\
      start_addr = memory_address (Pmode, plus_constant ((TRAMP), 48));	\
      emit_move_insn (gen_rtx_MEM (Pmode, start_addr),			\
		      gen_rtx_REG (Pmode, 19));				\
      /* fdc and fic only use registers for the address to flush,	\
	 they do not accept integer displacements.  */ 			\
      start_addr = force_reg (Pmode, (TRAMP));				\
      end_addr = force_reg (Pmode, plus_constant ((TRAMP), 32));	\
      emit_insn (gen_dcacheflush (start_addr, end_addr));		\
      end_addr = force_reg (Pmode, plus_constant (start_addr, 32));	\
      emit_insn (gen_icacheflush (start_addr, end_addr, start_addr,	\
				  gen_reg_rtx (Pmode), gen_reg_rtx (Pmode)));\
    }									\
  else									\
    {									\
      rtx start_addr, end_addr;						\
									\
      start_addr = memory_address (Pmode, plus_constant ((TRAMP), 56));	\
      emit_move_insn (gen_rtx_MEM (Pmode, start_addr), (FNADDR));	\
      start_addr = memory_address (Pmode, plus_constant ((TRAMP), 64));	\
      emit_move_insn (gen_rtx_MEM (Pmode, start_addr), (CXT));		\
      /* Create a fat pointer for the trampoline.  */			\
      end_addr = force_reg (Pmode, plus_constant ((TRAMP), 32));	\
      start_addr = memory_address (Pmode, plus_constant ((TRAMP), 16));	\
      emit_move_insn (gen_rtx_MEM (Pmode, start_addr), end_addr);	\
      end_addr = gen_rtx_REG (Pmode, 27);				\
      start_addr = memory_address (Pmode, plus_constant ((TRAMP), 24));	\
      emit_move_insn (gen_rtx_MEM (Pmode, start_addr), end_addr);	\
      /* fdc and fic only use registers for the address to flush,	\
	 they do not accept integer displacements.  */ 			\
      start_addr = force_reg (Pmode, (TRAMP));				\
      end_addr = force_reg (Pmode, plus_constant ((TRAMP), 32));	\
      emit_insn (gen_dcacheflush (start_addr, end_addr));		\
      end_addr = force_reg (Pmode, plus_constant (start_addr, 32));	\
      emit_insn (gen_icacheflush (start_addr, end_addr, start_addr,	\
				  gen_reg_rtx (Pmode), gen_reg_rtx (Pmode)));\
    }									\
}

/* Perform any machine-specific adjustment in the address of the trampoline.
   ADDR contains the address that was passed to INITIALIZE_TRAMPOLINE.
   Adjust the trampoline address to point to the plabel at offset 44.  */
   
#define TRAMPOLINE_ADJUST_ADDRESS(ADDR) \
  if (!TARGET_64BIT) (ADDR) = memory_address (Pmode, plus_constant ((ADDR), 46))

/* Emit code for a call to builtin_saveregs.  We must emit USE insns which
   reference the 4 integer arg registers and 4 fp arg registers.
   Ordinarily they are not call used registers, but they are for
   _builtin_saveregs, so we must make this explicit.  */

#define EXPAND_BUILTIN_SAVEREGS() hppa_builtin_saveregs ()

/* Implement `va_start' for varargs and stdarg.  */

#define EXPAND_BUILTIN_VA_START(valist, nextarg) \
  hppa_va_start (valist, nextarg)

/* Implement `va_arg'.  */

#define EXPAND_BUILTIN_VA_ARG(valist, type) \
  hppa_va_arg (valist, type)

/* Addressing modes, and classification of registers for them. 

   Using autoincrement addressing modes on PA8000 class machines is
   not profitable.  */

#define HAVE_POST_INCREMENT (pa_cpu < PROCESSOR_8000)
#define HAVE_POST_DECREMENT (pa_cpu < PROCESSOR_8000)

#define HAVE_PRE_DECREMENT (pa_cpu < PROCESSOR_8000)
#define HAVE_PRE_INCREMENT (pa_cpu < PROCESSOR_8000)

/* Macros to check register numbers against specific register classes.  */

/* These assume that REGNO is a hard or pseudo reg number.
   They give nonzero only if REGNO is a hard reg of the suitable class
   or a pseudo reg currently allocated to a suitable hard reg.
   Since they use reg_renumber, they are safe only once reg_renumber
   has been allocated, which happens in local-alloc.c.  */

#define REGNO_OK_FOR_INDEX_P(REGNO) \
  ((REGNO) && ((REGNO) < 32 || (unsigned) reg_renumber[REGNO] < 32))
#define REGNO_OK_FOR_BASE_P(REGNO)  \
  ((REGNO) && ((REGNO) < 32 || (unsigned) reg_renumber[REGNO] < 32))
#define REGNO_OK_FOR_FP_P(REGNO) \
  (FP_REGNO_P (REGNO) || FP_REGNO_P (reg_renumber[REGNO]))

/* Now macros that check whether X is a register and also,
   strictly, whether it is in a specified class.

   These macros are specific to the HP-PA, and may be used only
   in code for printing assembler insns and in conditions for
   define_optimization.  */

/* 1 if X is an fp register.  */

#define FP_REG_P(X) (REG_P (X) && REGNO_OK_FOR_FP_P (REGNO (X)))

/* Maximum number of registers that can appear in a valid memory address.  */

#define MAX_REGS_PER_ADDRESS 2

/* Recognize any constant value that is a valid address except
   for symbolic addresses.  We get better CSE by rejecting them
   here and allowing hppa_legitimize_address to break them up.  We
   use most of the constants accepted by CONSTANT_P, except CONST_DOUBLE.  */

#define CONSTANT_ADDRESS_P(X) \
  ((GET_CODE (X) == LABEL_REF || GET_CODE (X) == SYMBOL_REF		\
   || GET_CODE (X) == CONST_INT || GET_CODE (X) == CONST		\
   || GET_CODE (X) == HIGH) 						\
   && (reload_in_progress || reload_completed || ! symbolic_expression_p (X)))

/* Include all constant integers and constant doubles, but not
   floating-point, except for floating-point zero.

   Reject LABEL_REFs if we're not using gas or the new HP assembler. 

   ?!? For now also reject CONST_DOUBLES in 64bit mode.  This will need
   further work.  */
#ifndef NEW_HP_ASSEMBLER
#define NEW_HP_ASSEMBLER 0
#endif
#define LEGITIMATE_CONSTANT_P(X)				\
  ((GET_MODE_CLASS (GET_MODE (X)) != MODE_FLOAT			\
    || (X) == CONST0_RTX (GET_MODE (X)))			\
   && (NEW_HP_ASSEMBLER || TARGET_GAS || GET_CODE (X) != LABEL_REF)	\
   && !(TARGET_64BIT && GET_CODE (X) == CONST_DOUBLE)		\
   && !(TARGET_64BIT && GET_CODE (X) == CONST_INT		\
	&& !(HOST_BITS_PER_WIDE_INT <= 32			\
	     || (INTVAL (X) >= (HOST_WIDE_INT) -32 << 31	\
		 && INTVAL (X) < (HOST_WIDE_INT) 32 << 31)	\
	     || cint_ok_for_move (INTVAL (X))))			\
   && !function_label_operand (X, VOIDmode))

/* Subroutine for EXTRA_CONSTRAINT.

   Return 1 iff OP is a pseudo which did not get a hard register and
   we are running the reload pass.  */

#define IS_RELOADING_PSEUDO_P(OP) \
  ((reload_in_progress					\
    && GET_CODE (OP) == REG				\
    && REGNO (OP) >= FIRST_PSEUDO_REGISTER		\
    && reg_renumber [REGNO (OP)] < 0))

/* Optional extra constraints for this machine. Borrowed from sparc.h.

   For the HPPA, `Q' means that this is a memory operand but not a
   symbolic memory operand.  Note that an unassigned pseudo register
   is such a memory operand.  Needed because reload will generate
   these things in insns and then not re-recognize the insns, causing
   constrain_operands to fail.

   `R' is used for scaled indexed addresses.

   `S' is the constant 31.

   `T' is for fp loads and stores.  */
#define EXTRA_CONSTRAINT(OP, C)				\
  ((C) == 'Q' ?						\
   (IS_RELOADING_PSEUDO_P (OP)				\
    || (GET_CODE (OP) == MEM				\
	&& (memory_address_p (GET_MODE (OP), XEXP (OP, 0))\
	    || reload_in_progress)			\
	&& ! symbolic_memory_operand (OP, VOIDmode)	\
        && !(GET_CODE (XEXP (OP, 0)) == PLUS		\
	     && (GET_CODE (XEXP (XEXP (OP, 0), 0)) == MULT\
		 || GET_CODE (XEXP (XEXP (OP, 0), 1)) == MULT))))\
   : ((C) == 'R' ?					\
     (GET_CODE (OP) == MEM				\
      && GET_CODE (XEXP (OP, 0)) == PLUS		\
      && (GET_CODE (XEXP (XEXP (OP, 0), 0)) == MULT	\
	  || GET_CODE (XEXP (XEXP (OP, 0), 1)) == MULT)	\
      && (move_operand (OP, GET_MODE (OP))		\
	  || memory_address_p (GET_MODE (OP), XEXP (OP, 0))\
	  || reload_in_progress))			\
   : ((C) == 'T' ? 					\
      (GET_CODE (OP) == MEM				\
       /* Using DFmode forces only short displacements	\
	  to be recognized as valid in reg+d addresses. \
	  However, this is not necessary for PA2.0 since\
	  it has long FP loads/stores.			\
							\
	  FIXME: the ELF32 linker clobbers the LSB of	\
	  the FP register number in {fldw,fstw} insns.	\
	  Thus, we only allow long FP loads/stores on	\
	  TARGET_64BIT.  */				\
       && memory_address_p ((TARGET_PA_20		\
			     && !TARGET_ELF32		\
			     ? GET_MODE (OP)		\
			     : DFmode),			\
			    XEXP (OP, 0))		\
       && !(GET_CODE (XEXP (OP, 0)) == LO_SUM		\
	    && GET_CODE (XEXP (XEXP (OP, 0), 0)) == REG \
	    && REG_OK_FOR_BASE_P (XEXP (XEXP (OP, 0), 0))\
	    && GET_CODE (XEXP (XEXP (OP, 0), 1)) == UNSPEC\
	    && GET_MODE (XEXP (OP, 0)) == Pmode)	\
       && !(GET_CODE (XEXP (OP, 0)) == PLUS		\
	    && (GET_CODE (XEXP (XEXP (OP, 0), 0)) == MULT\
		|| GET_CODE (XEXP (XEXP (OP, 0), 1)) == MULT)))\
   : ((C) == 'U' ?					\
      (GET_CODE (OP) == CONST_INT && INTVAL (OP) == 63)	\
   : ((C) == 'A' ?					\
      (GET_CODE (OP) == MEM				\
       && GET_CODE (XEXP (OP, 0)) == LO_SUM		\
       && GET_CODE (XEXP (XEXP (OP, 0), 0)) == REG	\
       && REG_OK_FOR_BASE_P (XEXP (XEXP (OP, 0), 0))	\
       && GET_CODE (XEXP (XEXP (OP, 0), 1)) == UNSPEC		\
       && GET_MODE (XEXP (OP, 0)) == Pmode)			\
   : ((C) == 'S' ?					\
      (GET_CODE (OP) == CONST_INT && INTVAL (OP) == 31) : 0))))))
	

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
   been eliminated by then.  */

#ifndef REG_OK_STRICT

/* Nonzero if X is a hard reg that can be used as an index
   or if it is a pseudo reg.  */
#define REG_OK_FOR_INDEX_P(X) \
(REGNO (X) && (REGNO (X) < 32 || REGNO (X) >= FIRST_PSEUDO_REGISTER))
/* Nonzero if X is a hard reg that can be used as a base reg
   or if it is a pseudo reg.  */
#define REG_OK_FOR_BASE_P(X) \
(REGNO (X) && (REGNO (X) < 32 || REGNO (X) >= FIRST_PSEUDO_REGISTER))

#else

/* Nonzero if X is a hard reg that can be used as an index.  */
#define REG_OK_FOR_INDEX_P(X) REGNO_OK_FOR_INDEX_P (REGNO (X))
/* Nonzero if X is a hard reg that can be used as a base reg.  */
#define REG_OK_FOR_BASE_P(X) REGNO_OK_FOR_BASE_P (REGNO (X))

#endif

/* GO_IF_LEGITIMATE_ADDRESS recognizes an RTL expression
   that is a valid memory address for an instruction.
   The MODE argument is the machine mode for the MEM expression
   that wants to use this address.

   On the HP-PA, the actual legitimate addresses must be
   REG+REG, REG+(REG*SCALE) or REG+SMALLINT.
   But we can treat a SYMBOL_REF as legitimate if it is part of this
   function's constant-pool, because such addresses can actually
   be output as REG+SMALLINT. 

   Note we only allow 5 bit immediates for access to a constant address;
   doing so avoids losing for loading/storing a FP register at an address
   which will not fit in 5 bits.  */

#define VAL_5_BITS_P(X) ((unsigned HOST_WIDE_INT)(X) + 0x10 < 0x20)
#define INT_5_BITS(X) VAL_5_BITS_P (INTVAL (X))

#define VAL_U5_BITS_P(X) ((unsigned HOST_WIDE_INT)(X) < 0x20)
#define INT_U5_BITS(X) VAL_U5_BITS_P (INTVAL (X))

#define VAL_11_BITS_P(X) ((unsigned HOST_WIDE_INT)(X) + 0x400 < 0x800)
#define INT_11_BITS(X) VAL_11_BITS_P (INTVAL (X))

#define VAL_14_BITS_P(X) ((unsigned HOST_WIDE_INT)(X) + 0x2000 < 0x4000)
#define INT_14_BITS(X) VAL_14_BITS_P (INTVAL (X))

#define GO_IF_LEGITIMATE_ADDRESS(MODE, X, ADDR)  \
{							\
  if ((REG_P (X) && REG_OK_FOR_BASE_P (X))		\
      || ((GET_CODE (X) == PRE_DEC || GET_CODE (X) == POST_DEC		\
	   || GET_CODE (X) == PRE_INC || GET_CODE (X) == POST_INC)	\
	  && REG_P (XEXP (X, 0))			\
	  && REG_OK_FOR_BASE_P (XEXP (X, 0))))		\
    goto ADDR;						\
  else if (GET_CODE (X) == PLUS)			\
    {							\
      rtx base = 0, index = 0;				\
      if (REG_P (XEXP (X, 0))				\
	  && REG_OK_FOR_BASE_P (XEXP (X, 0)))		\
	base = XEXP (X, 0), index = XEXP (X, 1);	\
      else if (REG_P (XEXP (X, 1))			\
	       && REG_OK_FOR_BASE_P (XEXP (X, 1)))	\
	base = XEXP (X, 1), index = XEXP (X, 0);	\
      if (base != 0)					\
	if (GET_CODE (index) == CONST_INT		\
	    && ((INT_14_BITS (index)			\
		 && (TARGET_SOFT_FLOAT			\
		     || (TARGET_PA_20			\
			 && ((MODE == SFmode		\
			      && (INTVAL (index) % 4) == 0)\
			     || (MODE == DFmode		\
				 && (INTVAL (index) % 8) == 0)))\
		     || ((MODE) != SFmode && (MODE) != DFmode))) \
		|| INT_5_BITS (index)))			\
	  goto ADDR;					\
      if (! TARGET_SOFT_FLOAT				\
	  && ! TARGET_DISABLE_INDEXING			\
	  && base					\
	  && ((MODE) == SFmode || (MODE) == DFmode)	\
	  && GET_CODE (index) == MULT			\
	  && GET_CODE (XEXP (index, 0)) == REG		\
	  && REG_OK_FOR_BASE_P (XEXP (index, 0))	\
	  && GET_CODE (XEXP (index, 1)) == CONST_INT	\
	  && INTVAL (XEXP (index, 1)) == ((MODE) == SFmode ? 4 : 8))\
	goto ADDR;					\
    }							\
  else if (GET_CODE (X) == LO_SUM			\
	   && GET_CODE (XEXP (X, 0)) == REG		\
	   && REG_OK_FOR_BASE_P (XEXP (X, 0))		\
	   && CONSTANT_P (XEXP (X, 1))			\
	   && (TARGET_SOFT_FLOAT			\
	       /* We can allow symbolic LO_SUM addresses\
		  for PA2.0.  */			\
	       || (TARGET_PA_20				\
		   && !TARGET_ELF32			\
	           && GET_CODE (XEXP (X, 1)) != CONST_INT)\
	       || ((MODE) != SFmode			\
		   && (MODE) != DFmode)))		\
    goto ADDR;						\
  else if (GET_CODE (X) == LO_SUM			\
	   && GET_CODE (XEXP (X, 0)) == SUBREG		\
	   && GET_CODE (SUBREG_REG (XEXP (X, 0))) == REG\
	   && REG_OK_FOR_BASE_P (SUBREG_REG (XEXP (X, 0)))\
	   && CONSTANT_P (XEXP (X, 1))			\
	   && (TARGET_SOFT_FLOAT			\
	       /* We can allow symbolic LO_SUM addresses\
		  for PA2.0.  */			\
	       || (TARGET_PA_20				\
		   && !TARGET_ELF32			\
	           && GET_CODE (XEXP (X, 1)) != CONST_INT)\
	       || ((MODE) != SFmode			\
		   && (MODE) != DFmode)))		\
    goto ADDR;						\
  else if (GET_CODE (X) == LABEL_REF			\
	   || (GET_CODE (X) == CONST_INT		\
	       && INT_5_BITS (X)))			\
    goto ADDR;						\
  /* Needed for -fPIC */				\
  else if (GET_CODE (X) == LO_SUM			\
	   && GET_CODE (XEXP (X, 0)) == REG             \
	   && REG_OK_FOR_BASE_P (XEXP (X, 0))		\
	   && GET_CODE (XEXP (X, 1)) == UNSPEC		\
	   && (TARGET_SOFT_FLOAT			\
	       || (TARGET_PA_20	&& !TARGET_ELF32)	\
	       || ((MODE) != SFmode			\
		   && (MODE) != DFmode)))		\
    goto ADDR;						\
}

/* Look for machine dependent ways to make the invalid address AD a
   valid address.

   For the PA, transform:

        memory(X + <large int>)

   into:

        if (<large int> & mask) >= 16
          Y = (<large int> & ~mask) + mask + 1  Round up.
        else
          Y = (<large int> & ~mask)             Round down.
        Z = X + Y
        memory (Z + (<large int> - Y));

   This makes reload inheritance and reload_cse work better since Z
   can be reused.

   There may be more opportunities to improve code with this hook.  */
#define LEGITIMIZE_RELOAD_ADDRESS(AD, MODE, OPNUM, TYPE, IND, WIN) 	\
do { 									\
  int offset, newoffset, mask;						\
  rtx new, temp = NULL_RTX;						\
									\
  mask = (GET_MODE_CLASS (MODE) == MODE_FLOAT				\
	  ? (TARGET_PA_20 && !TARGET_ELF32 ? 0x3fff : 0x1f) : 0x3fff);	\
									\
  if (optimize								\
      && GET_CODE (AD) == PLUS)						\
    temp = simplify_binary_operation (PLUS, Pmode,			\
				      XEXP (AD, 0), XEXP (AD, 1));	\
									\
  new = temp ? temp : AD;						\
									\
  if (optimize								\
      && GET_CODE (new) == PLUS						\
      && GET_CODE (XEXP (new, 0)) == REG				\
      && GET_CODE (XEXP (new, 1)) == CONST_INT)				\
    {									\
      offset = INTVAL (XEXP ((new), 1));				\
									\
      /* Choose rounding direction.  Round up if we are >= halfway.  */	\
      if ((offset & mask) >= ((mask + 1) / 2))				\
	newoffset = (offset & ~mask) + mask + 1;			\
      else								\
	newoffset = offset & ~mask;					\
									\
      if (newoffset != 0						\
	  && VAL_14_BITS_P (newoffset))					\
	{								\
									\
	  temp = gen_rtx_PLUS (Pmode, XEXP (new, 0),			\
			       GEN_INT (newoffset));			\
	  AD = gen_rtx_PLUS (Pmode, temp, GEN_INT (offset - newoffset));\
	  push_reload (XEXP (AD, 0), 0, &XEXP (AD, 0), 0,		\
			     BASE_REG_CLASS, Pmode, VOIDmode, 0, 0,	\
			     (OPNUM), (TYPE));				\
	  goto WIN;							\
	}								\
    }									\
} while (0)




/* Try machine-dependent ways of modifying an illegitimate address
   to be legitimate.  If we find one, return the new, valid address.
   This macro is used in only one place: `memory_address' in explow.c.

   OLDX is the address as it was before break_out_memory_refs was called.
   In some cases it is useful to look at this to decide what needs to be done.

   MODE and WIN are passed so that this macro can use
   GO_IF_LEGITIMATE_ADDRESS.

   It is always safe for this macro to do nothing.  It exists to recognize
   opportunities to optimize the output.  */

#define LEGITIMIZE_ADDRESS(X, OLDX, MODE, WIN)	\
{ rtx orig_x = (X);				\
  (X) = hppa_legitimize_address (X, OLDX, MODE);	\
  if ((X) != orig_x && memory_address_p (MODE, X)) \
    goto WIN; }

/* Go to LABEL if ADDR (a legitimate address expression)
   has an effect that depends on the machine mode it is used for.  */

#define GO_IF_MODE_DEPENDENT_ADDRESS(ADDR,LABEL)	\
  if (GET_CODE (ADDR) == PRE_DEC	\
      || GET_CODE (ADDR) == POST_DEC	\
      || GET_CODE (ADDR) == PRE_INC	\
      || GET_CODE (ADDR) == POST_INC)	\
    goto LABEL

#define TARGET_ASM_SELECT_SECTION  pa_select_section
   
/* Define this macro if references to a symbol must be treated
   differently depending on something about the variable or
   function named by the symbol (such as what section it is in).

   The macro definition, if any, is executed immediately after the
   rtl for DECL or other node is created.
   The value of the rtl will be a `mem' whose address is a
   `symbol_ref'.

   The usual thing for this macro to do is to a flag in the
   `symbol_ref' (such as `SYMBOL_REF_FLAG') or to store a modified
   name string in the `symbol_ref' (if one bit is not enough
   information).

   On the HP-PA we use this to indicate if a symbol is in text or
   data space.  Also, function labels need special treatment.  */

#define TEXT_SPACE_P(DECL)\
  (TREE_CODE (DECL) == FUNCTION_DECL					\
   || (TREE_CODE (DECL) == VAR_DECL					\
       && TREE_READONLY (DECL) && ! TREE_SIDE_EFFECTS (DECL)		\
       && (! DECL_INITIAL (DECL) || ! reloc_needed (DECL_INITIAL (DECL))) \
       && !flag_pic)							\
   || (TREE_CODE_CLASS (TREE_CODE (DECL)) == 'c'			\
       && !(TREE_CODE (DECL) == STRING_CST && flag_writable_strings)))

#define FUNCTION_NAME_P(NAME)  (*(NAME) == '@')

/* Specify the machine mode that this machine uses
   for the index in the tablejump instruction.  */
#define CASE_VECTOR_MODE (TARGET_BIG_SWITCH ? TImode : DImode)

/* Jump tables must be 32 bit aligned, no matter the size of the element.  */
#define ADDR_VEC_ALIGN(ADDR_VEC) 2

/* Define this as 1 if `char' should by default be signed; else as 0.  */
#define DEFAULT_SIGNED_CHAR 1

/* Max number of bytes we can move from memory to memory
   in one reasonably fast instruction.  */
#define MOVE_MAX 8

/* Higher than the default as we prefer to use simple move insns
   (better scheduling and delay slot filling) and because our
   built-in block move is really a 2X unrolled loop. 

   Believe it or not, this has to be big enough to allow for copying all
   arguments passed in registers to avoid infinite recursion during argument
   setup for a function call.  Why?  Consider how we copy the stack slots
   reserved for parameters when they may be trashed by a call.  */
#define MOVE_RATIO (TARGET_64BIT ? 8 : 4)

/* Define if operations between registers always perform the operation
   on the full register even if a narrower mode is specified.  */
#define WORD_REGISTER_OPERATIONS

/* Define if loading in MODE, an integral mode narrower than BITS_PER_WORD
   will either zero-extend or sign-extend.  The value of this macro should
   be the code that says which one of the two operations is implicitly
   done, NIL if none.  */
#define LOAD_EXTEND_OP(MODE) ZERO_EXTEND

/* Nonzero if access to memory by bytes is slow and undesirable.  */
#define SLOW_BYTE_ACCESS 1

/* Value is 1 if truncating an integer of INPREC bits to OUTPREC bits
   is done just by pretending it is already truncated.  */
#define TRULY_NOOP_TRUNCATION(OUTPREC, INPREC) 1

/* We assume that the store-condition-codes instructions store 0 for false
   and some other value for true.  This is the value stored for true.  */

#define STORE_FLAG_VALUE 1

/* When a prototype says `char' or `short', really pass an `int'.  */
#define PROMOTE_PROTOTYPES 1
#define PROMOTE_FUNCTION_RETURN 1

/* Specify the machine mode that pointers have.
   After generation of rtl, the compiler makes no further distinction
   between pointers and any other objects of this machine mode.  */
#define Pmode word_mode

/* Given a comparison code (EQ, NE, etc.) and the first operand of a COMPARE,
   return the mode to be used for the comparison.  For floating-point, CCFPmode
   should be used.  CC_NOOVmode should be used when the first operand is a
   PLUS, MINUS, or NEG.  CCmode should be used when no special processing is
   needed.  */
#define SELECT_CC_MODE(OP,X,Y) \
  (GET_MODE_CLASS (GET_MODE (X)) == MODE_FLOAT ? CCFPmode : CCmode)    \

/* A function address in a call instruction
   is a byte address (for indexing purposes)
   so give the MEM rtx a byte's mode.  */
#define FUNCTION_MODE SImode

/* Define this if addresses of constant functions
   shouldn't be put through pseudo regs where they can be cse'd.
   Desirable on machines where ordinary constants are expensive
   but a CALL with constant address is cheap.  */
#define NO_FUNCTION_CSE

/* Define this to be nonzero if shift instructions ignore all but the low-order
   few bits.  */
#define SHIFT_COUNT_TRUNCATED 1

/* Compute the cost of computing a constant rtl expression RTX
   whose rtx-code is CODE.  The body of this macro is a portion
   of a switch statement.  If the code is computed here,
   return it with a return statement.  Otherwise, break from the switch.  */

#define CONST_COSTS(RTX,CODE,OUTER_CODE) \
  case CONST_INT:							\
    if (INTVAL (RTX) == 0) return 0;					\
    if (INT_14_BITS (RTX)) return 1;					\
  case HIGH:								\
    return 2;								\
  case CONST:								\
  case LABEL_REF:							\
  case SYMBOL_REF:							\
    return 4;								\
  case CONST_DOUBLE:							\
    if ((RTX == CONST0_RTX (DFmode) || RTX == CONST0_RTX (SFmode))	\
	&& OUTER_CODE != SET)						\
      return 0;								\
    else								\
      return 8;

#define ADDRESS_COST(RTX) \
  (GET_CODE (RTX) == REG ? 1 : hppa_address_cost (RTX))

/* Compute extra cost of moving data between one register class
   and another.

   Make moves from SAR so expensive they should never happen.  We used to
   have 0xffff here, but that generates overflow in rare cases.

   Copies involving a FP register and a non-FP register are relatively
   expensive because they must go through memory.

   Other copies are reasonably cheap.  */
#define REGISTER_MOVE_COST(MODE, CLASS1, CLASS2) \
 (CLASS1 == SHIFT_REGS ? 0x100					\
  : FP_REG_CLASS_P (CLASS1) && ! FP_REG_CLASS_P (CLASS2) ? 16	\
  : FP_REG_CLASS_P (CLASS2) && ! FP_REG_CLASS_P (CLASS1) ? 16	\
  : 2)


/* Provide the costs of a rtl expression.  This is in the body of a
   switch on CODE.  The purpose for the cost of MULT is to encourage
   `synth_mult' to find a synthetic multiply when reasonable.  */

#define RTX_COSTS(X,CODE,OUTER_CODE)					\
  case MULT:								\
    if (GET_MODE_CLASS (GET_MODE (X)) == MODE_FLOAT)			\
      return COSTS_N_INSNS (3);						\
    return (TARGET_PA_11 && ! TARGET_DISABLE_FPREGS && ! TARGET_SOFT_FLOAT) \
	    ? COSTS_N_INSNS (8) : COSTS_N_INSNS (20);	\
  case DIV:								\
    if (GET_MODE_CLASS (GET_MODE (X)) == MODE_FLOAT)			\
      return COSTS_N_INSNS (14);					\
  case UDIV:								\
  case MOD:								\
  case UMOD:								\
    return COSTS_N_INSNS (60);						\
  case PLUS: /* this includes shNadd insns */				\
  case MINUS:								\
    if (GET_MODE_CLASS (GET_MODE (X)) == MODE_FLOAT)			\
      return COSTS_N_INSNS (3);						\
    return COSTS_N_INSNS (1);						\
  case ASHIFT:								\
  case ASHIFTRT:							\
  case LSHIFTRT:							\
    return COSTS_N_INSNS (1);

/* Adjust the cost of branches.  */
#define BRANCH_COST (pa_cpu == PROCESSOR_8000 ? 2 : 1)

/* Handling the special cases is going to get too complicated for a macro,
   just call `pa_adjust_insn_length' to do the real work.  */
#define ADJUST_INSN_LENGTH(INSN, LENGTH)	\
  LENGTH += pa_adjust_insn_length (INSN, LENGTH);

/* Millicode insns are actually function calls with some special
   constraints on arguments and register usage.

   Millicode calls always expect their arguments in the integer argument
   registers, and always return their result in %r29 (ret1).  They
   are expected to clobber their arguments, %r1, %r29, and the return
   pointer which is %r31 on 32-bit and %r2 on 64-bit, and nothing else.

   This macro tells reorg that the references to arguments and
   millicode calls do not appear to happen until after the millicode call.
   This allows reorg to put insns which set the argument registers into the
   delay slot of the millicode call -- thus they act more like traditional
   CALL_INSNs.

   Note we can not consider side effects of the insn to be delayed because
   the branch and link insn will clobber the return pointer.  If we happened
   to use the return pointer in the delay slot of the call, then we lose.

   get_attr_type will try to recognize the given insn, so make sure to
   filter out things it will not accept -- SEQUENCE, USE and CLOBBER insns
   in particular.  */
#define INSN_REFERENCES_ARE_DELAYED(X) (insn_refs_are_delayed (X))


/* Control the assembler format that we output.  */

/* Output to assembler file text saying following lines
   may contain character constants, extra white space, comments, etc.  */

#define ASM_APP_ON ""

/* Output to assembler file text saying following lines
   no longer contain unusual constructs.  */

#define ASM_APP_OFF ""

/* Output deferred plabels at the end of the file.  */

#define ASM_FILE_END(FILE) output_deferred_plabels (FILE)

/* This is how to output the definition of a user-level label named NAME,
   such as the label on a static function or variable NAME.  */

#define ASM_OUTPUT_LABEL(FILE, NAME)	\
  do { assemble_name (FILE, NAME); 	\
       fputc ('\n', FILE); } while (0)

/* This is how to output a reference to a user-level label named NAME.
   `assemble_name' uses this.  */

#define ASM_OUTPUT_LABELREF(FILE,NAME)	\
  do {					\
    const char *xname = (NAME);		\
    if (FUNCTION_NAME_P (NAME))		\
      xname += 1;			\
    if (xname[0] == '*')		\
      xname += 1;			\
    else				\
      fputs (user_label_prefix, FILE);	\
    fputs (xname, FILE);		\
  } while (0)

/* This is how to output an internal numbered label where
   PREFIX is the class of label and NUM is the number within the class.  */

#define ASM_OUTPUT_INTERNAL_LABEL(FILE,PREFIX,NUM)	\
  {fprintf (FILE, "%c$%s%04d\n", (PREFIX)[0], (PREFIX) + 1, NUM);}

/* This is how to store into the string LABEL
   the symbol_ref name of an internal numbered label where
   PREFIX is the class of label and NUM is the number within the class.
   This is suitable for output with `assemble_name'.  */

#define ASM_GENERATE_INTERNAL_LABEL(LABEL,PREFIX,NUM)	\
  sprintf (LABEL, "*%c$%s%04ld", (PREFIX)[0], (PREFIX) + 1, (long)(NUM))

#define TARGET_ASM_GLOBALIZE_LABEL pa_globalize_label

#define ASM_OUTPUT_ASCII(FILE, P, SIZE)  \
  output_ascii ((FILE), (P), (SIZE))

/* This is how to output an element of a case-vector that is absolute.
   Note that this method makes filling these branch delay slots
   impossible.  */

#define ASM_OUTPUT_ADDR_VEC_ELT(FILE, VALUE)  \
  if (TARGET_BIG_SWITCH)					\
    fprintf (FILE, "\tstw %%r1,-16(%%r30)\n\tldil LR'L$%04d,%%r1\n\tbe RR'L$%04d(%%sr4,%%r1)\n\tldw -16(%%r30),%%r1\n", VALUE, VALUE);		\
  else								\
    fprintf (FILE, "\tb L$%04d\n\tnop\n", VALUE)

/* Jump tables are executable code and live in the TEXT section on the PA.  */
#define JUMP_TABLES_IN_TEXT_SECTION 1

/* This is how to output an element of a case-vector that is relative.
   This must be defined correctly as it is used when generating PIC code.

   I believe it safe to use the same definition as ASM_OUTPUT_ADDR_VEC_ELT
   on the PA since ASM_OUTPUT_ADDR_VEC_ELT uses pc-relative jump instructions
   rather than a table of absolute addresses.  */

#define ASM_OUTPUT_ADDR_DIFF_ELT(FILE, BODY, VALUE, REL)  \
  if (TARGET_BIG_SWITCH)					\
    fprintf (FILE, "\tstw %%r1,-16(%%r30)\n\tldw T'L$%04d(%%r19),%%r1\n\tbv %%r0(%%r1)\n\tldw -16(%%r30),%%r1\n", VALUE);				\
  else								\
    fprintf (FILE, "\tb L$%04d\n\tnop\n", VALUE)

/* This is how to output an assembler line
   that says to advance the location counter
   to a multiple of 2**LOG bytes.  */

#define ASM_OUTPUT_ALIGN(FILE,LOG)	\
    fprintf (FILE, "\t.align %d\n", (1<<(LOG)))

#define ASM_OUTPUT_SKIP(FILE,SIZE)  \
  fprintf (FILE, "\t.blockz %d\n", (SIZE))

/* This says how to output an assembler line to define a global common symbol
   with size SIZE (in bytes) and alignment ALIGN (in bits).  */

#define ASM_OUTPUT_ALIGNED_COMMON(FILE, NAME, SIZE, ALIGNED)  		\
{ bss_section ();							\
  assemble_name ((FILE), (NAME));					\
  fputs ("\t.comm ", (FILE));						\
  fprintf ((FILE), "%d\n", MAX ((SIZE), ((ALIGNED) / BITS_PER_UNIT)));}

/* This says how to output an assembler line to define a local common symbol
   with size SIZE (in bytes) and alignment ALIGN (in bits).  */

#define ASM_OUTPUT_ALIGNED_LOCAL(FILE, NAME, SIZE, ALIGNED)		\
{ bss_section ();							\
  fprintf ((FILE), "\t.align %d\n", ((ALIGNED) / BITS_PER_UNIT));	\
  assemble_name ((FILE), (NAME));				\
  fprintf ((FILE), "\n\t.block %d\n", (SIZE));}
  
/* Store in OUTPUT a string (made with alloca) containing
   an assembler-name for a local static variable named NAME.
   LABELNO is an integer which is different for each call.  */

#define ASM_FORMAT_PRIVATE_NAME(OUTPUT, NAME, LABELNO)	\
( (OUTPUT) = (char *) alloca (strlen ((NAME)) + 12),	\
  sprintf ((OUTPUT), "%s___%d", (NAME), (LABELNO)))

/* All HP assemblers use "!" to separate logical lines.  */
#define IS_ASM_LOGICAL_LINE_SEPARATOR(C) ((C) == '!')

#define PRINT_OPERAND_PUNCT_VALID_P(CHAR) \
  ((CHAR) == '@' || (CHAR) == '#' || (CHAR) == '*' || (CHAR) == '^')

/* Print operand X (an rtx) in assembler syntax to file FILE.
   CODE is a letter or dot (`z' in `%z0') or 0 if no letter was specified.
   For `%' followed by punctuation, CODE is the punctuation and X is null.

   On the HP-PA, the CODE can be `r', meaning this is a register-only operand
   and an immediate zero should be represented as `r0'.

   Several % codes are defined:
   O an operation
   C compare conditions
   N extract conditions
   M modifier to handle preincrement addressing for memory refs.
   F modifier to handle preincrement addressing for fp memory refs */

#define PRINT_OPERAND(FILE, X, CODE) print_operand (FILE, X, CODE)


/* Print a memory address as an operand to reference that memory location.  */

#define PRINT_OPERAND_ADDRESS(FILE, ADDR)  \
{ register rtx addr = ADDR;						\
  register rtx base;							\
  int offset;								\
  switch (GET_CODE (addr))						\
    {									\
    case REG:								\
      fprintf (FILE, "0(%s)", reg_names [REGNO (addr)]);		\
      break;								\
    case PLUS:								\
      if (GET_CODE (XEXP (addr, 0)) == CONST_INT)			\
	offset = INTVAL (XEXP (addr, 0)), base = XEXP (addr, 1);	\
      else if (GET_CODE (XEXP (addr, 1)) == CONST_INT)			\
	offset = INTVAL (XEXP (addr, 1)), base = XEXP (addr, 0);	\
      else								\
	abort ();							\
      fprintf (FILE, "%d(%s)", offset, reg_names [REGNO (base)]);	\
      break;								\
    case LO_SUM:							\
      if (!symbolic_operand (XEXP (addr, 1), VOIDmode))			\
	fputs ("R'", FILE);						\
      else if (flag_pic == 0)						\
	fputs ("RR'", FILE);						\
      else								\
	fputs ("RT'", FILE);						\
      output_global_address (FILE, XEXP (addr, 1), 0);			\
      fputs ("(", FILE);						\
      output_operand (XEXP (addr, 0), 0);				\
      fputs (")", FILE);						\
      break;								\
    case CONST_INT:							\
      fprintf (FILE, HOST_WIDE_INT_PRINT_DEC, INTVAL (addr));		\
      fprintf (FILE, "(%%r0)");						\
      break;								\
    default:								\
      output_addr_const (FILE, addr);					\
    }}


/* Find the return address associated with the frame given by
   FRAMEADDR.  */
#define RETURN_ADDR_RTX(COUNT, FRAMEADDR)				 \
  (return_addr_rtx (COUNT, FRAMEADDR))

/* Used to mask out junk bits from the return address, such as
   processor state, interrupt status, condition codes and the like.  */
#define MASK_RETURN_ADDR						\
  /* The privilege level is in the two low order bits, mask em out	\
     of the return address.  */						\
  (GEN_INT (-4))

/* The number of Pmode words for the setjmp buffer.  */
#define JMP_BUF_SIZE 50

/* Only direct calls to static functions are allowed to be sibling (tail)
   call optimized.

   This restriction is necessary because some linker generated stubs will
   store return pointers into rp' in some cases which might clobber a
   live value already in rp'.

   In a sibcall the current function and the target function share stack
   space.  Thus if the path to the current function and the path to the
   target function save a value in rp', they save the value into the
   same stack slot, which has undesirable consequences.

   Because of the deferred binding nature of shared libraries any function
   with external scope could be in a different load module and thus require
   rp' to be saved when calling that function.  So sibcall optimizations
   can only be safe for static function.

   Note that GCC never needs return value relocations, so we don't have to
   worry about static calls with return value relocations (which require
   saving rp').

   It is safe to perform a sibcall optimization when the target function
   will never return.  */
#define FUNCTION_OK_FOR_SIBCALL(DECL) \
  (DECL \
   && ! TARGET_PORTABLE_RUNTIME \
   && ! TARGET_64BIT \
   && ! TREE_PUBLIC (DECL))

#define PREDICATE_CODES							\
  {"reg_or_0_operand", {SUBREG, REG, CONST_INT}},			\
  {"call_operand_address", {LABEL_REF, SYMBOL_REF, CONST_INT,		\
			    CONST_DOUBLE, CONST, HIGH, CONSTANT_P_RTX}}, \
  {"symbolic_operand", {SYMBOL_REF, LABEL_REF, CONST}},			\
  {"symbolic_memory_operand", {SUBREG, MEM}},				\
  {"reg_before_reload_operand", {REG, MEM}},				\
  {"reg_or_nonsymb_mem_operand", {SUBREG, REG, MEM}},			\
  {"reg_or_0_or_nonsymb_mem_operand", {SUBREG, REG, MEM, CONST_INT,	\
				       CONST_DOUBLE}},			\
  {"move_operand", {SUBREG, REG, CONSTANT_P_RTX, CONST_INT, MEM}},	\
  {"reg_or_cint_move_operand", {SUBREG, REG, CONST_INT}},		\
  {"pic_label_operand", {LABEL_REF, CONST}},				\
  {"fp_reg_operand", {REG}},						\
  {"arith_operand", {SUBREG, REG, CONST_INT}},				\
  {"arith11_operand", {SUBREG, REG, CONST_INT}},			\
  {"pre_cint_operand", {CONST_INT}},					\
  {"post_cint_operand", {CONST_INT}},					\
  {"arith_double_operand", {SUBREG, REG, CONST_DOUBLE}},		\
  {"ireg_or_int5_operand", {CONST_INT, REG}},				\
  {"int5_operand", {CONST_INT}},					\
  {"uint5_operand", {CONST_INT}},					\
  {"int11_operand", {CONST_INT}},					\
  {"uint32_operand", {CONST_INT,					\
   HOST_BITS_PER_WIDE_INT > 32 ? 0 : CONST_DOUBLE}},			\
  {"arith5_operand", {SUBREG, REG, CONST_INT}},				\
  {"and_operand", {SUBREG, REG, CONST_INT}},				\
  {"ior_operand", {CONST_INT}},						\
  {"lhs_lshift_cint_operand", {CONST_INT}},				\
  {"lhs_lshift_operand", {SUBREG, REG, CONST_INT}},			\
  {"arith32_operand", {SUBREG, REG, CONST_INT}},			\
  {"pc_or_label_operand", {PC, LABEL_REF}},				\
  {"plus_xor_ior_operator", {PLUS, XOR, IOR}},				\
  {"shadd_operand", {CONST_INT}},					\
  {"basereg_operand", {REG}},						\
  {"div_operand", {REG, CONST_INT}},					\
  {"ireg_operand", {REG}},						\
  {"cmpib_comparison_operator", {EQ, NE, LT, LE, LEU,			\
   GT, GTU, GE}},							\
  {"movb_comparison_operator", {EQ, NE, LT, GE}},

/* We need a libcall to canonicalize function pointers on TARGET_ELF32.  */
#define CANONICALIZE_FUNCPTR_FOR_COMPARE_LIBCALL \
  "__canonicalize_funcptr_for_compare"
