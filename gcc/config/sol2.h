/* Operating system specific defines to be used when targeting GCC for any
   Solaris 2 system.
   Copyright 2002, 2003, 2004, 2007, 2008, 2009, 2010, 2011, 2012
   Free Software Foundation, Inc.

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

/* We are compiling for Solaris 2 now.  */
#define TARGET_SOLARIS 1

/* Solaris 2 (at least as of 2.5.1) uses a 32-bit wchar_t.  */
#undef WCHAR_TYPE
#define WCHAR_TYPE "long int"

#undef WCHAR_TYPE_SIZE
#define WCHAR_TYPE_SIZE BITS_PER_WORD

/* Solaris 2 uses a wint_t different from the default. This is required
   by the SCD 2.4.1, p. 6-83, Figure 6-66.  */
#undef	WINT_TYPE
#define	WINT_TYPE "long int"

#undef	WINT_TYPE_SIZE
#define	WINT_TYPE_SIZE BITS_PER_WORD

#define SIG_ATOMIC_TYPE "int"

/* ??? This definition of int8_t follows the system header but does
   not conform to C99.  Likewise int_fast8_t, int_least8_t.  */
#define INT8_TYPE "char"
#define INT16_TYPE "short int"
#define INT32_TYPE "int"
#define INT64_TYPE (LONG_TYPE_SIZE == 64 ? "long int" : "long long int")
#define UINT8_TYPE "unsigned char"
#define UINT16_TYPE "short unsigned int"
#define UINT32_TYPE "unsigned int"
#define UINT64_TYPE (LONG_TYPE_SIZE == 64 ? "long unsigned int" : "long long unsigned int")

#define INT_LEAST8_TYPE "char"
#define INT_LEAST16_TYPE "short int"
#define INT_LEAST32_TYPE "int"
#define INT_LEAST64_TYPE (LONG_TYPE_SIZE == 64 ? "long int" : "long long int")
#define UINT_LEAST8_TYPE "unsigned char"
#define UINT_LEAST16_TYPE "short unsigned int"
#define UINT_LEAST32_TYPE "unsigned int"
#define UINT_LEAST64_TYPE (LONG_TYPE_SIZE == 64 ? "long unsigned int" : "long long unsigned int")

#define INT_FAST8_TYPE "char"
#define INT_FAST16_TYPE "int"
#define INT_FAST32_TYPE "int"
#define INT_FAST64_TYPE (LONG_TYPE_SIZE == 64 ? "long int" : "long long int")
#define UINT_FAST8_TYPE "unsigned char"
#define UINT_FAST16_TYPE "unsigned int"
#define UINT_FAST32_TYPE "unsigned int"
#define UINT_FAST64_TYPE (LONG_TYPE_SIZE == 64 ? "long unsigned int" : "long long unsigned int")

#define INTPTR_TYPE (LONG_TYPE_SIZE == 64 ? "long int" : "int")
#define UINTPTR_TYPE (LONG_TYPE_SIZE == 64 ? "long unsigned int" : "unsigned int")

#undef CPP_SUBTARGET_SPEC
#define CPP_SUBTARGET_SPEC "\
%{pthreads|pthread:-D_REENTRANT -D_PTHREADS}"

/* Names to predefine in the preprocessor for this target machine.  */
#define TARGET_SUB_OS_CPP_BUILTINS()
#define TARGET_OS_CPP_BUILTINS()			\
    do {						\
	builtin_define_std ("unix");			\
	builtin_define_std ("sun");			\
	builtin_define ("__svr4__");			\
	builtin_define ("__SVR4");			\
	builtin_assert ("system=unix");			\
	builtin_assert ("system=svr4");			\
	/* For C++ we need to add some additional macro	\
	   definitions required by the C++ standard	\
	   library.  */					\
	if (c_dialect_cxx ())				\
	  {						\
	    builtin_define ("__STDC_VERSION__=199901L");\
	    builtin_define ("_XOPEN_SOURCE=600");	\
	    builtin_define ("_LARGEFILE_SOURCE=1");	\
	    builtin_define ("_LARGEFILE64_SOURCE=1");	\
	    builtin_define ("__EXTENSIONS__");		\
	  }						\
	TARGET_SUB_OS_CPP_BUILTINS();			\
    } while (0)

#define SUBTARGET_OVERRIDE_OPTIONS			\
  do {							\
    solaris_override_options ();			\
  } while (0)


/* It's safe to pass -s always, even if -g is not used.  Those options are
   handled by both Sun as and GNU as.  */
#define ASM_SPEC_BASE \
"%{v:-V} %{Qy:} %{!Qn:-Qy} %{Ym,*} -s %(asm_cpu)"

#define ASM_PIC_SPEC " %{fpic|fpie|fPIC|fPIE:-K PIC}"

#undef LIB_SPEC
#define LIB_SPEC \
  "%{!symbolic:\
     %{pthreads|pthread:-lpthread} \
     %{pthreads|pthread|fprofile-generate*:" LIB_TLS_SPEC "} \
     %{p|pg:-ldl} -lc}"

#ifndef CROSS_DIRECTORY_STRUCTURE
#undef MD_EXEC_PREFIX
#define MD_EXEC_PREFIX "/usr/ccs/bin/"

#undef MD_STARTFILE_PREFIX
#define MD_STARTFILE_PREFIX "/usr/ccs/lib/"
#endif

#undef STARTFILE_ARCH32_SPEC
#define STARTFILE_ARCH32_SPEC "%{ansi:values-Xc.o%s} \
			    %{!ansi:values-Xa.o%s}"

#undef STARTFILE_ARCH_SPEC
#define STARTFILE_ARCH_SPEC STARTFILE_ARCH32_SPEC

/* We don't use the standard svr4 STARTFILE_SPEC because it's wrong for us.  */
#undef STARTFILE_SPEC
#define STARTFILE_SPEC "%{!shared: \
			 %{!symbolic: \
			  %{p:mcrt1.o%s} \
                          %{!p: \
	                    %{pg:gcrt1.o%s gmon.o%s} \
                            %{!pg:crt1.o%s}}}} \
			crti.o%s %(startfile_arch) \
			crtbegin.o%s"

#undef  ENDFILE_SPEC
#define ENDFILE_SPEC \
  "%{Ofast|ffast-math|funsafe-math-optimizations:crtfastmath.o%s} \
   crtend.o%s crtn.o%s"

#undef LINK_ARCH32_SPEC_BASE
#define LINK_ARCH32_SPEC_BASE \
  "%{G:-G} \
   %{YP,*} \
   %{R*} \
   %{!YP,*:%{p|pg:-Y P,%R/usr/ccs/lib/libp:%R/usr/lib/libp:%R/usr/ccs/lib:%R/lib:%R/usr/lib} \
	   %{!p:%{!pg:-Y P,%R/usr/ccs/lib:%R/lib:%R/usr/lib}}}"

#undef LINK_ARCH32_SPEC
#define LINK_ARCH32_SPEC LINK_ARCH32_SPEC_BASE

#undef LINK_ARCH_SPEC
#define LINK_ARCH_SPEC LINK_ARCH32_SPEC

#ifndef USE_GLD
/* With Sun ld, -rdynamic is a no-op.  */
#define RDYNAMIC_SPEC ""
#else
/* GNU ld needs --export-dynamic to implement -rdynamic.  */
#define RDYNAMIC_SPEC "--export-dynamic"
#endif

#undef  LINK_SPEC
#define LINK_SPEC \
  "%{h*} %{v:-V} \
   %{!shared:%{!static:%{rdynamic: " RDYNAMIC_SPEC "}}} \
   %{static:-dn -Bstatic} \
   %{shared:-G -dy %{!mimpure-text:-z text}} \
   %{symbolic:-Bsymbolic -G -dy -z text} \
   %(link_arch) \
   %{Qy:} %{!Qn:-Qy}"

#ifdef USE_GLD
/* Solaris 11 build 135+ implements dl_iterate_phdr.  GNU ld needs
   --eh-frame-hdr to create the required .eh_frame_hdr sections.  */
#if defined(HAVE_LD_EH_FRAME_HDR) && defined(TARGET_DL_ITERATE_PHDR)
#define LINK_EH_SPEC "%{!static:--eh-frame-hdr} "
#endif /* HAVE_LD_EH_FRAME && TARGET_DL_ITERATE_PHDR */
#endif

#ifndef USE_GLD
/* The default MFLIB_SPEC is GNU ld specific.  */
#define MFLIB_SPEC ""
#endif

/* collect2.c can only parse GNU nm -n output.  Solaris nm needs -png to
   produce the same format.  */
#define NM_FLAGS "-png"

/* The system headers under Solaris 2 are C++-aware since 2.0.  */
#define NO_IMPLICIT_EXTERN_C

#define STDC_0_IN_SYSTEM_HEADERS 1

/* Support Solaris-specific format checking for cmn_err.  */
#define TARGET_N_FORMAT_TYPES 1
#define TARGET_FORMAT_TYPES solaris_format_types

/* #pragma init and #pragma fini are implemented on top of init and
   fini attributes.  */
#define SOLARIS_ATTRIBUTE_TABLE						\
  { "init",      0, 0, true,  false,  false, NULL, false },		\
  { "fini",      0, 0, true,  false,  false, NULL, false }

/* Solaris-specific #pragmas are implemented on top of attributes.  Hook in
   the bits from config/sol2.c.  */
#define SUBTARGET_INSERT_ATTRIBUTES solaris_insert_attributes
#define SUBTARGET_ATTRIBUTE_TABLE SOLARIS_ATTRIBUTE_TABLE

/* Allow macro expansion in #pragma pack.  */
#define HANDLE_PRAGMA_PACK_WITH_EXPANSION

#define TARGET_CXX_DECL_MANGLING_CONTEXT solaris_cxx_decl_mangling_context

/* Solaris/x86 as and gas support unquoted section names.  */
#define SECTION_NAME_FORMAT	"%s"

/* This is how to declare the size of a function.  For Solaris, we output
   any .init or .fini entries here.  */
#undef ASM_DECLARE_FUNCTION_SIZE
#define ASM_DECLARE_FUNCTION_SIZE(FILE, FNAME, DECL)		\
  do								\
    {								\
      if (!flag_inhibit_size_directive)				\
	ASM_OUTPUT_MEASURED_SIZE (FILE, FNAME);			\
      solaris_output_init_fini (FILE, DECL);			\
    }								\
  while (0)

/* Solaris as has a bug: a .common directive in .tbss or .tdata section
   behaves as .tls_common rather than normal non-TLS .common.  */
#undef  ASM_OUTPUT_ALIGNED_COMMON
#define ASM_OUTPUT_ALIGNED_COMMON(FILE, NAME, SIZE, ALIGN)		\
  do									\
    {									\
      if (TARGET_SUN_TLS						\
	  && in_section							\
	  && ((in_section->common.flags & SECTION_TLS) == SECTION_TLS))	\
	switch_to_section (bss_section);				\
      fprintf ((FILE), "%s", COMMON_ASM_OP);				\
      assemble_name ((FILE), (NAME));					\
      fprintf ((FILE), ","HOST_WIDE_INT_PRINT_UNSIGNED",%u\n",		\
	       (SIZE), (ALIGN) / BITS_PER_UNIT);			\
    }									\
  while (0)

#ifndef USE_GAS
#undef TARGET_ASM_ASSEMBLE_VISIBILITY
#define TARGET_ASM_ASSEMBLE_VISIBILITY solaris_assemble_visibility

#define AS_NEEDS_DASH_FOR_PIPED_INPUT

/* The Solaris assembler cannot grok .stabd directives.  */
#undef NO_DBX_BNSYM_ENSYM
#define NO_DBX_BNSYM_ENSYM 1
#endif

#ifndef USE_GLD
/* The Solaris linker doesn't understand constructor priorities.  */
#undef SUPPORTS_INIT_PRIORITY
#define SUPPORTS_INIT_PRIORITY 0
#endif

/* Solaris has an implementation of __enable_execute_stack.  */
#define HAVE_ENABLE_EXECUTE_STACK

/* Static stack checking is supported by means of probes.  */
#define STACK_CHECK_STATIC_BUILTIN 1

#define TARGET_POSIX_IO

extern GTY(()) tree solaris_pending_aligns;
extern GTY(()) tree solaris_pending_inits;
extern GTY(()) tree solaris_pending_finis;
