/* Definitions of target machine for GNU compiler, for SPARC running Solaris 2
   Copyright 1992, 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002
   Free Software Foundation, Inc.
   Contributed by Ron Guilmette (rfg@netcom.com).
   Additional changes by David V. Henkel-Wallace (gumby@cygnus.com).

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

/* Supposedly the same as vanilla sparc svr4, except for the stuff below: */

#undef CPP_PREDEFINES
#define CPP_PREDEFINES "-Dsparc"

/* This is here rather than in sparc.h because it's not known what
   other assemblers will accept.  */

#if TARGET_CPU_DEFAULT == TARGET_CPU_v9
#undef ASM_CPU_DEFAULT_SPEC
#define ASM_CPU_DEFAULT_SPEC "-xarch=v8plus"
#endif

#if TARGET_CPU_DEFAULT == TARGET_CPU_ultrasparc
#undef ASM_CPU_DEFAULT_SPEC
#define ASM_CPU_DEFAULT_SPEC "-xarch=v8plusa"
#endif

#undef ASM_CPU_SPEC
#define ASM_CPU_SPEC "\
%{mcpu=v8plus:-xarch=v8plus} \
%{mcpu=v9:-xarch=v8plus} \
%{mcpu=ultrasparc:-xarch=v8plusa} \
%{!mcpu*:%(asm_cpu_default)} \
"

#undef SUBTARGET_EXTRA_SPECS
#define SUBTARGET_EXTRA_SPECS \
  { "startfile_arch",	STARTFILE_ARCH_SPEC },	\
  { "link_arch",	LINK_ARCH_SPEC }

/* However it appears that Solaris 2.0 uses the same reg numbering as
   the old BSD-style system did.  */

/* Same as sparc.h */
#undef DBX_REGISTER_NUMBER
#define DBX_REGISTER_NUMBER(REGNO) \
  (TARGET_FLAT && (REGNO) == HARD_FRAME_POINTER_REGNUM ? 31 : REGNO)

/* The Solaris 2 assembler uses .skip, not .zero, so put this back.  */
#undef ASM_OUTPUT_SKIP
#define ASM_OUTPUT_SKIP(FILE,SIZE)  \
  fprintf (FILE, "\t.skip %u\n", (SIZE))

#undef  LOCAL_LABEL_PREFIX
#define LOCAL_LABEL_PREFIX  "."

/* This is how to output a definition of an internal numbered label where
   PREFIX is the class of label and NUM is the number within the class.  */

#undef  ASM_OUTPUT_INTERNAL_LABEL
#define ASM_OUTPUT_INTERNAL_LABEL(FILE,PREFIX,NUM)	\
  fprintf (FILE, ".L%s%d:\n", PREFIX, NUM)

/* This is how to output a reference to an internal numbered label where
   PREFIX is the class of label and NUM is the number within the class.  */

#undef  ASM_OUTPUT_INTERNAL_LABELREF
#define ASM_OUTPUT_INTERNAL_LABELREF(FILE,PREFIX,NUM)	\
  fprintf (FILE, ".L%s%d", PREFIX, NUM)

/* This is how to store into the string LABEL
   the symbol_ref name of an internal numbered label where
   PREFIX is the class of label and NUM is the number within the class.
   This is suitable for output with `assemble_name'.  */

#undef  ASM_GENERATE_INTERNAL_LABEL
#define ASM_GENERATE_INTERNAL_LABEL(LABEL,PREFIX,NUM)	\
  sprintf ((LABEL), "*.L%s%ld", (PREFIX), (long)(NUM))



#undef  ENDFILE_SPEC
#define ENDFILE_SPEC \
  "%{ffast-math|funsafe-math-optimizations:crtfastmath.o%s} \
   crtend.o%s crtn.o%s"

/* Select a format to encode pointers in exception handling data.  CODE
   is 0 for data, 1 for code labels, 2 for function pointers.  GLOBAL is
   true if the symbol may be affected by dynamic relocations.

   Some Solaris dynamic linkers don't handle unaligned section relative
   relocs properly, so force them to be aligned.  */
#ifndef HAVE_AS_SPARC_UA_PCREL
#define ASM_PREFERRED_EH_DATA_FORMAT(CODE,GLOBAL)		\
  ((flag_pic || GLOBAL) ? DW_EH_PE_aligned : DW_EH_PE_absptr)
#endif

/* ??? This does not work in SunOS 4.x, so it is not enabled in sparc.h.
   Instead, it is enabled here, because it does work under Solaris.  */
/* Define for support of TFmode long double.
   SPARC ABI says that long double is 4 words.  */
#define LONG_DOUBLE_TYPE_SIZE 128

/* But indicate that it isn't supported by the hardware.  */
#define WIDEST_HARDWARE_FP_SIZE 64

#define MULDI3_LIBCALL "__mul64"
#define DIVDI3_LIBCALL "__div64"
#define UDIVDI3_LIBCALL "__udiv64"
#define MODDI3_LIBCALL "__rem64"
#define UMODDI3_LIBCALL "__urem64"

/* Solaris's _Qp_* library routine implementation clobbers the output
   memory before the inputs are fully consumed.  */

#undef TARGET_BUGGY_QP_LIB
#define TARGET_BUGGY_QP_LIB	1

#undef INIT_SUBTARGET_OPTABS
#define INIT_SUBTARGET_OPTABS						\
  fixsfdi_libfunc							\
    = init_one_libfunc (TARGET_ARCH64 ? "__ftol" : "__ftoll");		\
  fixunssfdi_libfunc							\
    = init_one_libfunc (TARGET_ARCH64 ? "__ftoul" : "__ftoull");	\
  fixdfdi_libfunc							\
    = init_one_libfunc (TARGET_ARCH64 ? "__dtol" : "__dtoll");		\
  fixunsdfdi_libfunc							\
    = init_one_libfunc (TARGET_ARCH64 ? "__dtoul" : "__dtoull")

/* Solaris allows 64 bit out and global registers in 32 bit mode.
   sparc_override_options will disable V8+ if not generating V9 code.  */
#undef TARGET_DEFAULT
#define TARGET_DEFAULT (MASK_V8PLUS + MASK_FPU + MASK_LONG_DOUBLE_128)
