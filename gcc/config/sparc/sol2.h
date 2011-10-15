/* Definitions of target machine for GCC, for SPARC running Solaris 2
   Copyright 1992, 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2004, 2005,
   2006, 2007, 2008, 2010, 2011 Free Software Foundation, Inc.
   Contributed by Ron Guilmette (rfg@netcom.com).
   Additional changes by David V. Henkel-Wallace (gumby@cygnus.com).

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

/* Solaris allows 64-bit out and global registers to be used in 32-bit mode.
   sparc_override_options will disable V8+ if either not generating V9 code
   or generating 64-bit code.  */
#undef TARGET_DEFAULT
#ifdef TARGET_64BIT_DEFAULT
#define TARGET_DEFAULT \
  (MASK_V9 + MASK_64BIT + MASK_PTR64 + MASK_STACK_BIAS + \
   MASK_V8PLUS + MASK_APP_REGS + MASK_FPU + MASK_LONG_DOUBLE_128)
#else
#define TARGET_DEFAULT \
  (MASK_V8PLUS + MASK_APP_REGS + MASK_FPU + MASK_LONG_DOUBLE_128)
#endif

/* The default code model used to be CM_MEDANY on Solaris
   but even Sun eventually found it to be quite wasteful
   and changed it to CM_MEDMID in the Studio 9 compiler.  */
#undef SPARC_DEFAULT_CMODEL
#define SPARC_DEFAULT_CMODEL CM_MEDMID

/* Select a format to encode pointers in exception handling data.  CODE
   is 0 for data, 1 for code labels, 2 for function pointers.  GLOBAL is
   true if the symbol may be affected by dynamic relocations.

   Some Solaris dynamic linkers don't handle unaligned section relative
   relocs properly, so force them to be aligned.  */
#ifndef HAVE_AS_SPARC_UA_PCREL
#define ASM_PREFERRED_EH_DATA_FORMAT(CODE,GLOBAL)		\
  ((flag_pic || GLOBAL) ? DW_EH_PE_aligned : DW_EH_PE_absptr)
#endif



/* Supposedly the same as vanilla sparc svr4, except for the stuff below: */

/* This is here rather than in sparc.h because it's not known what
   other assemblers will accept.  */

#ifndef USE_GAS
#define AS_SPARC64_FLAG	"-xarch=v9"
#else
#define AS_SPARC64_FLAG	"-TSO -64 -Av9"
#endif

#undef ASM_CPU32_DEFAULT_SPEC
#define ASM_CPU32_DEFAULT_SPEC	""
#undef ASM_CPU64_DEFAULT_SPEC
#define ASM_CPU64_DEFAULT_SPEC	AS_SPARC64_FLAG

#if TARGET_CPU_DEFAULT == TARGET_CPU_v9
#undef CPP_CPU64_DEFAULT_SPEC
#define CPP_CPU64_DEFAULT_SPEC ""
#undef ASM_CPU32_DEFAULT_SPEC
#define ASM_CPU32_DEFAULT_SPEC "-xarch=v8plus"
#undef ASM_CPU_DEFAULT_SPEC
#define ASM_CPU_DEFAULT_SPEC ASM_CPU32_DEFAULT_SPEC
#endif

#if TARGET_CPU_DEFAULT == TARGET_CPU_ultrasparc
#undef CPP_CPU64_DEFAULT_SPEC
#define CPP_CPU64_DEFAULT_SPEC ""
#undef ASM_CPU32_DEFAULT_SPEC
#define ASM_CPU32_DEFAULT_SPEC "-xarch=v8plusa"
#undef ASM_CPU64_DEFAULT_SPEC
#define ASM_CPU64_DEFAULT_SPEC AS_SPARC64_FLAG "a"
#undef ASM_CPU_DEFAULT_SPEC
#define ASM_CPU_DEFAULT_SPEC ASM_CPU32_DEFAULT_SPEC
#endif

#if TARGET_CPU_DEFAULT == TARGET_CPU_ultrasparc3
#undef CPP_CPU64_DEFAULT_SPEC
#define CPP_CPU64_DEFAULT_SPEC ""
#undef ASM_CPU32_DEFAULT_SPEC
#define ASM_CPU32_DEFAULT_SPEC "-xarch=v8plusb"
#undef ASM_CPU64_DEFAULT_SPEC
#define ASM_CPU64_DEFAULT_SPEC AS_SPARC64_FLAG "b"
#undef ASM_CPU_DEFAULT_SPEC
#define ASM_CPU_DEFAULT_SPEC ASM_CPU32_DEFAULT_SPEC
#endif

#if TARGET_CPU_DEFAULT == TARGET_CPU_niagara
#undef CPP_CPU64_DEFAULT_SPEC
#define CPP_CPU64_DEFAULT_SPEC ""
#undef ASM_CPU32_DEFAULT_SPEC
#define ASM_CPU32_DEFAULT_SPEC "-xarch=v8plusb"
#undef ASM_CPU64_DEFAULT_SPEC
#define ASM_CPU64_DEFAULT_SPEC AS_SPARC64_FLAG "b"
#undef ASM_CPU_DEFAULT_SPEC
#define ASM_CPU_DEFAULT_SPEC ASM_CPU32_DEFAULT_SPEC
#endif

#if TARGET_CPU_DEFAULT == TARGET_CPU_niagara2
#undef CPP_CPU64_DEFAULT_SPEC
#define CPP_CPU64_DEFAULT_SPEC ""
#undef ASM_CPU32_DEFAULT_SPEC
#define ASM_CPU32_DEFAULT_SPEC "-xarch=v8plusb"
#undef ASM_CPU64_DEFAULT_SPEC
#define ASM_CPU64_DEFAULT_SPEC AS_SPARC64_FLAG "b"
#undef ASM_CPU_DEFAULT_SPEC
#define ASM_CPU_DEFAULT_SPEC ASM_CPU32_DEFAULT_SPEC
#endif

#if TARGET_CPU_DEFAULT == TARGET_CPU_niagara3
#undef CPP_CPU64_DEFAULT_SPEC
#define CPP_CPU64_DEFAULT_SPEC ""
#undef ASM_CPU32_DEFAULT_SPEC
#define ASM_CPU32_DEFAULT_SPEC "-xarch=v8plus" AS_NIAGARA3_FLAG
#undef ASM_CPU64_DEFAULT_SPEC
#define ASM_CPU64_DEFAULT_SPEC AS_SPARC64_FLAG AS_NIAGARA3_FLAG
#undef ASM_CPU_DEFAULT_SPEC
#define ASM_CPU_DEFAULT_SPEC ASM_CPU32_DEFAULT_SPEC
#endif

#if TARGET_CPU_DEFAULT == TARGET_CPU_niagara4
#undef CPP_CPU64_DEFAULT_SPEC
#define CPP_CPU64_DEFAULT_SPEC ""
#undef ASM_CPU32_DEFAULT_SPEC
#define ASM_CPU32_DEFAULT_SPEC "-xarch=v8plusb"
#undef ASM_CPU64_DEFAULT_SPEC
#define ASM_CPU64_DEFAULT_SPEC AS_SPARC64_FLAG "b"
#undef ASM_CPU_DEFAULT_SPEC
#define ASM_CPU_DEFAULT_SPEC ASM_CPU32_DEFAULT_SPEC
#endif

/* Both Sun as and GNU as understand -K PIC.  */
#undef ASM_SPEC
#define ASM_SPEC ASM_SPEC_BASE ASM_PIC_SPEC

#undef CPP_CPU_SPEC
#define CPP_CPU_SPEC "\
%{mcpu=sparclet|mcpu=tsc701:-D__sparclet__} \
%{mcpu=sparclite|mcpu-f930|mcpu=f934:-D__sparclite__} \
%{mcpu=v8:" DEF_ARCH32_SPEC("-D__sparcv8") "} \
%{mcpu=supersparc:-D__supersparc__ " DEF_ARCH32_SPEC("-D__sparcv8") "} \
%{mcpu=v9|mcpu=ultrasparc|mcpu=ultrasparc3|mcpu=niagara|mcpu=niagara2|mcpu=niagara3|mcpu=niagara4:" DEF_ARCH32_SPEC("-D__sparcv8") "} \
%{!mcpu*:%(cpp_cpu_default)} \
"

#undef CPP_CPU_DEFAULT_SPEC
#define CPP_CPU_DEFAULT_SPEC \
(DEFAULT_ARCH32_P ? "\
%{m64:" CPP_CPU64_DEFAULT_SPEC "} \
%{!m64:" CPP_CPU32_DEFAULT_SPEC "} \
" : "\
%{m32:" CPP_CPU32_DEFAULT_SPEC "} \
%{!m32:" CPP_CPU64_DEFAULT_SPEC "} \
")

#undef CPP_ARCH32_SPEC
#define CPP_ARCH32_SPEC ""
#undef CPP_ARCH64_SPEC
#define CPP_ARCH64_SPEC "-D__arch64__ -D__sparcv9"

#undef CPP_ARCH_SPEC
#define CPP_ARCH_SPEC "\
%{m32:%(cpp_arch32)} \
%{m64:%(cpp_arch64)} \
%{!m32:%{!m64:%(cpp_arch_default)}} \
"

/* -mcpu=native handling only makes sense with compiler running on
   a SPARC chip.  */
#if defined(__sparc__) && defined(__SVR4)
extern const char *host_detect_local_cpu (int argc, const char **argv);
# define EXTRA_SPEC_FUNCTIONS						\
  { "local_cpu_detect", host_detect_local_cpu },

# define MCPU_MTUNE_NATIVE_SPECS					\
   " %{mcpu=native:%<mcpu=native %:local_cpu_detect(cpu)}"		\
   " %{mtune=native:%<mtune=native %:local_cpu_detect(tune)}"
#else
# define MCPU_MTUNE_NATIVE_SPECS ""
#endif

#define DRIVER_SELF_SPECS MCPU_MTUNE_NATIVE_SPECS

#undef	CC1_SPEC
#if DEFAULT_ARCH32_P
#define CC1_SPEC "\
%{m64:%{m32:%emay not use both -m32 and -m64}} \
%{m64:-mptr64 -mstack-bias -mno-v8plus \
  %{!mcpu*:-%{!mv8plus:mcpu=v9}}} \
"
#else
#define CC1_SPEC "\
%{m32:%{m64:%emay not use both -m32 and -m64}} \
%{m32:-mptr32 -mno-stack-bias \
  %{!mcpu*:%{!mv8plus:-mcpu=v9}}} \
%{mv8plus:-m32 -mptr32 -mno-stack-bias \
  %{!mcpu*:-mcpu=v9}} \
"
#endif

/* Support for a compile-time default CPU, et cetera.  The rules are:
   --with-cpu is ignored if -mcpu is specified.
   --with-tune is ignored if -mtune is specified.
   --with-float is ignored if -mhard-float, -msoft-float, -mfpu, or -mno-fpu
     are specified.
   In the SPARC_BI_ARCH compiler we cannot pass %{!mcpu=*:-mcpu=%(VALUE)}
   here, otherwise say -mcpu=v7 would be passed even when -m64.
   CC1_SPEC above takes care of this instead.  */
#undef OPTION_DEFAULT_SPECS
#if DEFAULT_ARCH32_P
#define OPTION_DEFAULT_SPECS \
  {"cpu", "%{!m64:%{!mcpu=*:-mcpu=%(VALUE)}}" }, \
  {"tune", "%{!mtune=*:-mtune=%(VALUE)}" }, \
  {"float", "%{!msoft-float:%{!mhard-float:%{!mfpu:%{!mno-fpu:-m%(VALUE)-float}}}}" }
#else
#define OPTION_DEFAULT_SPECS \
  {"cpu", "%{!m32:%{!mcpu=*:-mcpu=%(VALUE)}}" }, \
  {"tune", "%{!mtune=*:-mtune=%(VALUE)}" }, \
  {"float", "%{!msoft-float:%{!mhard-float:%{!mfpu:%{!mno-fpu:-m%(VALUE)-float}}}}" }
#endif

#undef ASM_CPU_SPEC
#define ASM_CPU_SPEC "\
%{mcpu=v9:" DEF_ARCH32_SPEC("-xarch=v8plus") DEF_ARCH64_SPEC(AS_SPARC64_FLAG) "} \
%{mcpu=ultrasparc:" DEF_ARCH32_SPEC("-xarch=v8plusa") DEF_ARCH64_SPEC(AS_SPARC64_FLAG "a") "} \
%{mcpu=ultrasparc3:" DEF_ARCH32_SPEC("-xarch=v8plusb") DEF_ARCH64_SPEC(AS_SPARC64_FLAG "b") "} \
%{mcpu=niagara:" DEF_ARCH32_SPEC("-xarch=v8plusb") DEF_ARCH64_SPEC(AS_SPARC64_FLAG "b") "} \
%{mcpu=niagara2:" DEF_ARCH32_SPEC("-xarch=v8plusb") DEF_ARCH64_SPEC(AS_SPARC64_FLAG "b") "} \
%{mcpu=niagara3:" DEF_ARCH32_SPEC("-xarch=v8plus" AS_NIAGARA3_FLAG) DEF_ARCH64_SPEC(AS_SPARC64_FLAG AS_NIAGARA3_FLAG) "} \
%{mcpu=niagara4:" DEF_ARCH32_SPEC("-xarch=v8plus" AS_NIAGARA3_FLAG) DEF_ARCH64_SPEC(AS_SPARC64_FLAG AS_NIAGARA3_FLAG) "} \
%{!mcpu=niagara4:%{!mcpu=niagara3:%{!mcpu=niagara2:%{!mcpu=niagara:%{!mcpu=ultrasparc3:%{!mcpu=ultrasparc:%{!mcpu=v9:%{mcpu*:" DEF_ARCH32_SPEC("-xarch=v8") DEF_ARCH64_SPEC(AS_SPARC64_FLAG) "}}}}}}}} \
%{!mcpu*:%(asm_cpu_default)} \
"

#undef ASM_ARCH32_SPEC
#define ASM_ARCH32_SPEC ""

#undef ASM_ARCH64_SPEC
#define ASM_ARCH64_SPEC ""

#undef ASM_ARCH_DEFAULT_SPEC
#define ASM_ARCH_DEFAULT_SPEC ""

#undef ASM_ARCH_SPEC
#define ASM_ARCH_SPEC ""

#ifdef USE_GLD
/* Since binutils 2.21, GNU ld supports new *_sol2 emulations to strictly
   follow the Solaris 2 ABI.  Prefer them if present.  */
#ifdef HAVE_LD_SOL2_EMULATION
#define ARCH32_EMULATION "elf32_sparc_sol2"
#define ARCH64_EMULATION "elf64_sparc_sol2"
#else
#define ARCH32_EMULATION "elf32_sparc"
#define ARCH64_EMULATION "elf64_sparc"
#endif
#endif

#define ARCH64_SUBDIR "sparcv9"

#define SUBTARGET_CPU_EXTRA_SPECS



/* Register the Solaris-specific #pragma directives.  */
#define REGISTER_TARGET_PRAGMAS() solaris_register_pragmas ()

#if defined(USE_GAS) && defined(HAVE_AS_TLS)
/* Use GNU extensions to TLS support.  */
#undef TARGET_SUN_TLS
#undef TARGET_GNU_TLS
#define TARGET_SUN_TLS 0
#define TARGET_GNU_TLS 1
#endif

#undef  LOCAL_LABEL_PREFIX
#define LOCAL_LABEL_PREFIX  "."

/* The Solaris 2 assembler uses .skip, not .zero, so put this back.  */
#undef ASM_OUTPUT_SKIP
#define ASM_OUTPUT_SKIP(FILE,SIZE)  \
  fprintf (FILE, "\t.skip %u\n", (int)(SIZE))

/* This is how to store into the string LABEL
   the symbol_ref name of an internal numbered label where
   PREFIX is the class of label and NUM is the number within the class.
   This is suitable for output with `assemble_name'.  */

#undef  ASM_GENERATE_INTERNAL_LABEL
#define ASM_GENERATE_INTERNAL_LABEL(LABEL,PREFIX,NUM)	\
  sprintf ((LABEL), "*.L%s%lu", (PREFIX), (unsigned long)(NUM))

/* The native TLS-enabled assembler requires the directive #tls_object
   to be put on objects in TLS sections (as of v7.1).  This is not
   required by GNU as but supported on SPARC.  */
#undef  ASM_DECLARE_OBJECT_NAME
#define ASM_DECLARE_OBJECT_NAME(FILE, NAME, DECL)		\
  do								\
    {								\
      HOST_WIDE_INT size;					\
								\
      if (targetm.have_tls && DECL_THREAD_LOCAL_P (DECL))	\
	ASM_OUTPUT_TYPE_DIRECTIVE (FILE, NAME, "tls_object");	\
      else							\
	ASM_OUTPUT_TYPE_DIRECTIVE (FILE, NAME, "object");	\
								\
      size_directive_output = 0;				\
      if (!flag_inhibit_size_directive				\
	  && (DECL) && DECL_SIZE (DECL))			\
	{							\
	  size_directive_output = 1;				\
	  size = int_size_in_bytes (TREE_TYPE (DECL));		\
	  ASM_OUTPUT_SIZE_DIRECTIVE (FILE, NAME, size);		\
	}							\
								\
      ASM_OUTPUT_LABEL (FILE, NAME);				\
    }								\
  while (0)

/* Output a simple call for .init/.fini.  */
#define ASM_OUTPUT_CALL(FILE, FN)				        \
  do									\
    {									\
      fprintf (FILE, "\tcall\t");					\
      targetm.asm_out.print_operand (FILE, XEXP (DECL_RTL (FN), 0), 0);	\
      fprintf (FILE, "\n\tnop\n");					\
    }									\
  while (0)

#ifndef USE_GAS
/* This is how to output an assembler line that says to advance
   the location counter to a multiple of 2**LOG bytes using the
   NOP instruction as padding.  The filler pattern doesn't work
   with GNU as. */
#define ASM_OUTPUT_ALIGN_WITH_NOP(FILE,LOG)   \
  if ((LOG) != 0)                             \
    fprintf (FILE, "\t.align %d,0x1000000\n", (1<<(LOG)))

/* Use Solaris ELF section syntax with Sun as.  */
#undef TARGET_ASM_NAMED_SECTION
#define TARGET_ASM_NAMED_SECTION sparc_solaris_elf_asm_named_section

/* Sun as requires doublequoted section names on SPARC.  While GNU as
   supports that, too, we prefer the standard variant.  */
#undef SECTION_NAME_FORMAT
#define SECTION_NAME_FORMAT	"\"%s\""
#endif /* !USE_GAS */

/* Undefine this so that attribute((init_priority)) works with GNU ld.  */
#ifdef USE_GLD
#undef CTORS_SECTION_ASM_OP
#undef DTORS_SECTION_ASM_OP
#endif



/* Define for support of TFmode long double.
   SPARC ABI says that long double is 4 words.  */
#define LONG_DOUBLE_TYPE_SIZE 128

/* Solaris's _Qp_* library routine implementation clobbers the output
   memory before the inputs are fully consumed.  */

#undef TARGET_BUGGY_QP_LIB
#define TARGET_BUGGY_QP_LIB	1

#undef SUN_CONVERSION_LIBFUNCS
#define SUN_CONVERSION_LIBFUNCS 1

#undef DITF_CONVERSION_LIBFUNCS
#define DITF_CONVERSION_LIBFUNCS 1

#undef SUN_INTEGER_MULTIPLY_64
#define SUN_INTEGER_MULTIPLY_64 1
