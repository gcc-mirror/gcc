/* Definitions for Motorola 68k running Linux with ELF format.
   Copyright (C) 1995 Free Software Foundation, Inc.

This file is part of GNU CC.

GNU CC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 1, or (at your option)
any later version.

GNU CC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU CC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

#define LINUX_DEFAULT_ELF
#define MOTOROLA		/* Use Motorola syntax */
#define USE_GAS			/* But GAS wants jbsr instead of jsr */

#include <m68k/m68k.h>
#include <linux.h>		/* some common stuff */

#undef TARGET_VERSION
#define TARGET_VERSION fprintf (stderr, " (68k Linux/ELF)");

/* 68020 with 68881 */
#define TARGET_DEFAULT 7

#undef SUBTARGET_SWITCHES
#define SUBTARGET_SWITCHES	{"ieee-fp", 0},

/* Here are four prefixes that are used by asm_fprintf to
   facilitate customization for alternate assembler syntaxes.
   Machines with no likelihood of an alternate syntax need not
   define these and need not use asm_fprintf.  */

/* The prefix for register names.  Note that REGISTER_NAMES
   is supposed to include this prefix. Also note that this is NOT an
   fprintf format string, it is a literal string */

#undef REGISTER_PREFIX
#define REGISTER_PREFIX "%"

/* The prefix for local (compiler generated) labels.
   These labels will not appear in the symbol table. */

#undef LOCAL_LABEL_PREFIX
#define LOCAL_LABEL_PREFIX "."

/* The prefix to add to user-visible assembler symbols. */

#undef USER_LABEL_PREFIX
#define USER_LABEL_PREFIX ""

/* How to refer to registers in assembler output.
   This sequence is indexed by compiler's hard-register-number.
   Motorola format uses different register names than defined in m68k.h. */

#undef REGISTER_NAMES

#ifndef SUPPORT_SUN_FPA

#define REGISTER_NAMES \
{"%d0", "%d1", "%d2", "%d3", "%d4", "%d5", "%d6", "%d7", \
 "%a0", "%a1", "%a2", "%a3", "%a4", "%a5", "%a6", "%sp", \
 "%fp0", "%fp1", "%fp2", "%fp3", "%fp4", "%fp5", "%fp6", "%fp7" }

#else /* SUPPORTED_SUN_FPA */

#define REGISTER_NAMES \
{"%d0", "%d1", "%d2", "%d3", "%d4", "%d5", "%d6", "%d7", \
 "%a0", "%a1", "%a2", "%a3", "%a4", "%a5", "%a6", "%sp", \
 "%fp0", "%fp1", "%fp2", "%fp3", "%fp4", "%fp5", "%fp6", "%fp7", \
 "%fpa0", "%fpa1", "%fpa2", "%fpa3", "%fpa4", "%fpa5", "%fpa6", "%fpa7", \
 "%fpa8", "%fpa9", "%fpa10","%fpa11","%fpa12","%fpa13","%fpa14","%fpa15", \
 "%fpa16","%fpa17","%fpa18","%fpa19","%fpa20","%fpa21","%fpa22","%fpa23", \
 "%fpa24","%fpa25","%fpa26","%fpa27","%fpa28","%fpa29","%fpa30","%fpa31" }

#endif /* defined SUPPORT_SUN_FPA */

#undef SIZE_TYPE
#define SIZE_TYPE "unsigned int"
 
#undef PTRDIFF_TYPE
#define PTRDIFF_TYPE "int"
  
#undef WCHAR_TYPE
#define WCHAR_TYPE "long int"
   
#undef WCHAR_TYPE_SIZE
#define WCHAR_TYPE_SIZE BITS_PER_WORD

#undef ASM_SPEC
#define ASM_SPEC "%{m68030} %{m68040} %{fpic:-k} %{fPIC:-k}"

#define CPP_PREDEFINES \
  "-D__ELF__ -Dunix -Dmc68000 -Dmc68020 -Dlinux -Asystem(unix) -Asystem(posix) -Acpu(m68k) -Amachine(m68k)"

#undef CPP_SPEC
#if TARGET_DEFAULT & 2
#define CPP_SPEC \
  "%{fPIC:-D__PIC__ -D__pic__} %{fpic:-D__PIC__ -D__pic__} %{!msoft-float:-D__HAVE_68881__} %{posix:-D_POSIX_SOURCE}"
#else
#define CPP_SPEC \
  "%{fPIC:-D__PIC__ -D__pic__} %{fpic:-D__PIC__ -D__pic__} %{m68881:-D__HAVE_68881__} %{posix:-D_POSIX_SOURCE}"
#endif

#undef	LIB_SPEC
#if 1
/* We no longer link with libc_p.a or libg.a by default.  If you want
   to profile or debug the Linux C library, please add -lc_p or -ggdb
   to LDFLAGS at the link time, respectively.  */
#define LIB_SPEC \
  "%{!shared:%{!symbolic: %{mieee-fp:-lieee} %{p:-lgmon} %{pg:-lgmon} \
     %{!ggdb:-lc} %{ggdb:-lg}}}"
#else
#define LIB_SPEC \
  "%{!shared:%{!symbolic: \
     %{mieee-fp:-lieee} %{p:-lgmon -lc_p} %{pg:-lgmon -lc_p} \
     %{!p:%{!pg:%{!g*:-lc} %{g*:-lg}}}}}"
#endif

/* Provide a LINK_SPEC appropriate for Linux.  Here we provide support
   for the special GCC options -static and -shared, which allow us to
   link things in one of these three modes by applying the appropriate
   combinations of options at link-time.  We like to support here for
   as many of the other GNU linker options as possible.  But I don't
   have the time to search for those flags.  I am sure how to add
   support for -soname shared_object_name. H.J.

   I took out %{v:%{!V:-V}}.  It is too much :-(.  They can use
   -Wl,-V.

   When the -shared link option is used a final link is not being
   done.  */

/* If ELF is the default format, we should not use /lib/elf. */

#undef	LINK_SPEC
#ifndef LINUX_DEFAULT_ELF
#define LINK_SPEC "-m m68kelf %{shared} %{symbolic:-shared -Bsymbolic} \
  %{!shared:%{!symbolic: \
    %{!static: \
      %{rdynamic:-export-dynamic} \
      %{!dynamic-linker*:-dynamic-linker /lib/elf/ld-linux.so.1} \
      %{!rpath*:-rpath /lib/elf/}} %{static}}}"
#else
#define LINK_SPEC "-m m68kelf %{shared} %{symbolic:-shared -Bsymbolic} \
  %{!shared:%{!symbolic: \
    %{!static: \
      %{rdynamic:-export-dynamic} \
      %{!dynamic-linker*:-dynamic-linker /lib/ld-linux.so.1}} \
    %{static}}}"
#endif

/* For compatibility with linux/a.out */

#undef PCC_BITFIELD_TYPE_MATTERS

/* Currently, JUMP_TABLES_IN_TEXT_SECTION must be defined in order to
   keep switch tables in the text section.  */
   
#define JUMP_TABLES_IN_TEXT_SECTION 1

/* Use the default action for outputting the case label.  */
#undef ASM_OUTPUT_CASE_LABEL

#define ASM_RETURN_CASE_JUMP \
  return "jmp (2,%%pc,%0.w)"

/* This is how to output an assembler line that says to advance the
   location counter to a multiple of 2**LOG bytes.  */

#undef ASM_OUTPUT_ALIGN
#define ASM_OUTPUT_ALIGN(FILE,LOG)				\
  if ((LOG) > 0)						\
    fprintf ((FILE), "\t%s \t%u\n", ALIGN_ASM_OP, 1 << (LOG));

/* Output assembler code to FILE to increment profiler label # LABELNO
   for profiling a function entry.  */

#undef FUNCTION_PROFILER
#define FUNCTION_PROFILER(FILE, LABELNO) \
{									\
  asm_fprintf (FILE, "\tlea (%LLP%d,%Rpc),%Ra1\n", (LABELNO));		\
  if (flag_pic)								\
    fprintf (FILE, "\tbsr.l _mcount@PLTPC\n");				\
  else									\
    fprintf (FILE, "\tjbsr _mcount\n");					\
}

/* How to renumber registers for dbx and gdb.
   On the Sun-3, the floating point registers have numbers
   18 to 25, not 16 to 23 as they do in the compiler.  */

#define DBX_REGISTER_NUMBER(REGNO) ((REGNO) < 16 ? (REGNO) : (REGNO) + 2)

/* Do not break .stabs pseudos into continuations.  */

#define DBX_CONTIN_LENGTH 0

/* Allow folding division by zero.  */
#define REAL_INFINITY

/* 1 if N is a possible register number for a function value.  For
   m68k/SVR4 allow d0, a0, or fp0 as return registers, for integral,
   pointer, or floating types, respectively.  Reject fp0 if not using
   a 68881 coprocessor.  */

#undef FUNCTION_VALUE_REGNO_P
#define FUNCTION_VALUE_REGNO_P(N) \
  ((N) == 0 || (N) == 8 || (TARGET_68881 && (N) == 16))

/* Define this to be true when FUNCTION_VALUE_REGNO_P is true for
   more than one register.  */

#undef NEEDS_UNTYPED_CALL
#define NEEDS_UNTYPED_CALL 1

/* Define how to generate (in the callee) the output value of a
   function and how to find (in the caller) the value returned by a
   function.  VALTYPE is the data type of the value (as a tree).  If
   the precise function being called is known, FUNC is its
   FUNCTION_DECL; otherwise, FUNC is 0.  For m68k/SVR4 generate the
   result in d0, a0, or fp0 as appropriate. */
   
#undef FUNCTION_VALUE
#define FUNCTION_VALUE(VALTYPE, FUNC)					\
  (TREE_CODE (VALTYPE) == REAL_TYPE && TARGET_68881			\
   ? gen_rtx (REG, TYPE_MODE (VALTYPE), 16)				\
   : (POINTER_TYPE_P (VALTYPE)						\
      ? gen_rtx (REG, TYPE_MODE (VALTYPE), 8)				\
      : gen_rtx (REG, TYPE_MODE (VALTYPE), 0)))

/* For compatibility with the large body of existing code which does
   not always properly declare external functions returning pointer
   types, the m68k/SVR4 convention is to copy the value returned for
   pointer functions from a0 to d0 in the function epilogue, so that
   callers that have neglected to properly declare the callee can
   still find the correct return value.  */

extern int current_function_returns_pointer;
#define FUNCTION_EXTRA_EPILOGUE(FILE, SIZE)				\
do {									\
  if ((current_function_returns_pointer) && 				\
      ! find_equiv_reg (0, get_last_insn (), 0, 0, 0, 8, Pmode))	\
    asm_fprintf (FILE, "\tmove.l %Ra0,%Rd0\n");				\
} while (0);

/* Define how to find the value returned by a library function
   assuming the value has mode MODE.
   For m68k/SVR4 look for integer values in d0, pointer values in d0
   (returned in both d0 and a0), and floating values in fp0.  */

#undef LIBCALL_VALUE
#define LIBCALL_VALUE(MODE)						\
  (((MODE) == SFmode || (MODE) == DFmode) && TARGET_68881		\
   ? gen_rtx (REG, (MODE), 16)						\
   : gen_rtx (REG, (MODE), 0))

/* In m68k svr4, a symbol_ref rtx can be a valid PIC operand if it is
   an operand of a function call. */
#undef LEGITIMATE_PIC_OPERAND_P
#define LEGITIMATE_PIC_OPERAND_P(X) \
  (! symbolic_operand (X, VOIDmode) \
   || (GET_CODE (X) == SYMBOL_REF && SYMBOL_REF_FLAG (X)))

/* Turn off function cse if we are doing PIC. We always want function
   call to be done as `bsr foo@PLTPC', so it will force the assembler
   to create the PLT entry for `foo'.  Doing function cse will cause
   the address of `foo' to be loaded into a register, which is exactly
   what we want to avoid when we are doing PIC on svr4 m68k.  */
#undef SUBTARGET_OVERRIDE_OPTIONS
#define SUBTARGET_OVERRIDE_OPTIONS \
  if (flag_pic) flag_no_function_cse = 1;

/* For m68k SVR4, structures are returned using the reentrant
   technique. */
#undef PCC_STATIC_STRUCT_RETURN
#define DEFAULT_PCC_STRUCT_RETURN 0
