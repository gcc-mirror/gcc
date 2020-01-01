/* Operating system specific defines to be used when targeting GCC for any
   Solaris 2 system.
   Copyright (C) 2002-2020 Free Software Foundation, Inc.

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

/* wchar_t is called differently in <wchar.h> for 32 and 64-bit
   compilations.  This is called for by SCD 2.4.1, p. 6-83, Figure 6-65
   (32-bit) and p. 6P-10, Figure 6.38 (64-bit).  */

#undef WCHAR_TYPE
#define WCHAR_TYPE (TARGET_64BIT ? "int" : "long int")

#undef WCHAR_TYPE_SIZE
#define WCHAR_TYPE_SIZE 32

/* Same for wint_t.  See SCD 2.4.1, p. 6-83, Figure 6-66 (32-bit).  There's
   no corresponding 64-bit definition, but this is what Solaris 8
   <iso/wchar_iso.h> uses.  */

#undef WINT_TYPE
#define WINT_TYPE (TARGET_64BIT ? "int" : "long int")

#undef WINT_TYPE_SIZE
#define WINT_TYPE_SIZE 32

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
  do {							\
    builtin_define_std ("unix");			\
    builtin_define_std ("sun");				\
    builtin_define ("__svr4__");			\
    builtin_define ("__SVR4");				\
    builtin_assert ("system=unix");			\
    builtin_assert ("system=svr4");			\
    /* For C++ we need to add some additional macro	\
       definitions required by the C++ standard		\
       library.  */					\
    if (c_dialect_cxx ())				\
      {							\
	switch (cxx_dialect)				\
	  {						\
	  case cxx98:					\
	  case cxx11:					\
	  case cxx14:					\
	    /* C++11 and C++14 are based on C99.	\
	       libstdc++ makes use of C99 features	\
	       even for C++98.  */			\
	    builtin_define ("__STDC_VERSION__=199901L");\
	    break;					\
							\
	  default:					\
	    /* C++17 is based on C11.  */		\
	    builtin_define ("__STDC_VERSION__=201112L");\
	    break;					\
	  }						\
	builtin_define ("_XOPEN_SOURCE=600");		\
	builtin_define ("_LARGEFILE_SOURCE=1");		\
	builtin_define ("_LARGEFILE64_SOURCE=1");	\
	builtin_define ("_FILE_OFFSET_BITS=64");	\
	builtin_define ("__EXTENSIONS__");		\
      }							\
    TARGET_SUB_OS_CPP_BUILTINS();			\
  } while (0)

#define SUBTARGET_OVERRIDE_OPTIONS			\
  do {							\
    solaris_override_options ();			\
  } while (0)

#if DEFAULT_ARCH32_P
#define MULTILIB_DEFAULTS { "m32" }
#else
#define MULTILIB_DEFAULTS { "m64" }
#endif

#if DEFAULT_ARCH32_P
#define DEF_ARCH32_SPEC(__str) "%{!m64:" __str "}"
#define DEF_ARCH64_SPEC(__str) "%{m64:" __str "}"
#else
#define DEF_ARCH32_SPEC(__str) "%{m32:" __str "}"
#define DEF_ARCH64_SPEC(__str) "%{!m32:" __str "}"
#endif

/* Solaris needs -fasynchronous-unwind-tables to generate unwind info.  */
#define ASAN_CC1_SPEC "%{%:sanitize(address):-fasynchronous-unwind-tables}"

/* It's safe to pass -s always, even if -g is not used.  Those options are
   handled by both Sun as and GNU as.  */
#define ASM_SPEC_BASE \
"%{v:-V} %{Qy:} %{!Qn:-Qy} %{Ym,*} -s %(asm_cpu)"

#define ASM_PIC_SPEC " %{" FPIE_OR_FPIC_SPEC ":-K PIC}"

#undef ASM_CPU_DEFAULT_SPEC
#define ASM_CPU_DEFAULT_SPEC \
(DEFAULT_ARCH32_P ? "\
%{m64:" ASM_CPU64_DEFAULT_SPEC "} \
%{!m64:" ASM_CPU32_DEFAULT_SPEC "} \
" : "\
%{m32:" ASM_CPU32_DEFAULT_SPEC "} \
%{!m32:" ASM_CPU64_DEFAULT_SPEC "} \
")

#undef LIB_SPEC
#define LIB_SPEC \
  "%{!symbolic:\
     %{pthreads|pthread:-lpthread} \
     %{p|pg:-ldl} -lc}"

#ifndef CROSS_DIRECTORY_STRUCTURE
#undef MD_EXEC_PREFIX
#define MD_EXEC_PREFIX "/usr/ccs/bin/"
#endif

/* Enable constructor priorities if the configured linker supports it.  */
#undef SUPPORTS_INIT_PRIORITY
#define SUPPORTS_INIT_PRIORITY HAVE_INITFINI_ARRAY_SUPPORT

/* Solaris libc and libm implement multiple behaviours for various
   interfaces that have changed over the years in different versions of the
   C standard.  The behaviour is controlled by linking corresponding
   values-*.o objects.  Each of these objects contain alternate definitions
   of one or more variables that the libraries use to select which
   conflicting behaviour they should exhibit.  There are two sets of these
   objects, values-X*.o and values-xpg*.o.

   The values-X[ac].o objects set the variable _lib_version.  The Studio C
   compilers use values-Xc.o with either -Xc or (since Studio 12.6)
   -pedantic to select strictly conformant ISO C behaviour, otherwise
   values-Xa.o.  Since -pedantic is a diagnostic option only in GCC, we
   need to specifiy the -std=c* options and -std=iso9899:199409.  We
   traditionally include -ansi, which affects C and C++, and also -std=c++*
   for consistency.

   The values-xpg[46].o objects define either or both __xpg[46] variables,
   selecting XPG4 mode (__xpg4) and conforming C99/SUSv3 behavior (__xpg6).

   Since GCC 5, gcc defaults to -std=gnu11 or higher, so we link
   values-xpg6.o to get C99 semantics.  Besides, most of the runtime
   libraries always require C99 semantics.

   Since only one instance of _lib_version and __xpg[46] takes effekt (the
   first in ld.so.1's search path), we only link the values-*.o files into
   executable programs.  */
#undef STARTFILE_ARCH_SPEC
#define STARTFILE_ARCH_SPEC \
  "%{!shared:%{!symbolic: \
     %{ansi|std=c*|std=iso9899\\:199409:values-Xc.o%s; :values-Xa.o%s} \
     %{std=c90|std=gnu90:values-xpg4.o%s; :values-xpg6.o%s}}}"

#if defined(HAVE_LD_PIE) && defined(HAVE_SOLARIS_CRTS)
#define STARTFILE_CRTBEGIN_SPEC "%{static:crtbegin.o%s; \
				   shared|" PIE_SPEC ":crtbeginS.o%s; \
				   :crtbegin.o%s}"
#else
#define STARTFILE_CRTBEGIN_SPEC	"crtbegin.o%s"
#endif

#if ENABLE_VTABLE_VERIFY
#if SUPPORTS_INIT_PRIORITY
#define STARTFILE_VTV_SPEC \
  "%{fvtable-verify=none:%s; \
     fvtable-verify=preinit:vtv_start_preinit.o%s; \
     fvtable-verify=std:vtv_start.o%s}"
#define ENDFILE_VTV_SPEC \
  "%{fvtable-verify=none:%s; \
     fvtable-verify=preinit:vtv_end_preinit.o%s; \
     fvtable-verify=std:vtv_end.o%s}"
#else /* !SUPPORTS_INIT_PRIORITY */
#define STARTFILE_VTV_SPEC \
  "%{fvtable-verify=*: \
     %e-fvtable-verify=%* is not supported in this configuration}"
#define ENDFILE_VTV_SPEC ""
#endif /* !SUPPORTS_INIT_PRIORITY */
#else /* !ENABLE_VTABLE_VERIFY */
#define STARTFILE_VTV_SPEC ""
#define ENDFILE_VTV_SPEC ""
#endif /* !ENABLE_VTABLE_VERIFY */

/* Link -lasan early on the command line.  For -static-libasan, don't link
   it for -shared link, the executable should be compiled with -static-libasan
   in that case, and for executable link with --{,no-}whole-archive around
   it to force everything into the executable.  */

#ifndef USE_GNU_LD
#define LD_WHOLE_ARCHIVE_OPTION "-z allextract"
#define LD_NO_WHOLE_ARCHIVE_OPTION "-z defaultextract"
#else
#define LD_WHOLE_ARCHIVE_OPTION "--whole-archive"
#define LD_NO_WHOLE_ARCHIVE_OPTION "--no-whole-archive"
#endif

/* Allow rejecting -fsanitize=address, e.g. for specific multilibs.  */
#ifndef ASAN_REJECT_SPEC
#define ASAN_REJECT_SPEC ""
#endif

#define LIBASAN_EARLY_SPEC ASAN_REJECT_SPEC \
  " %{!shared:libasan_preinit%O%s} \
    %{static-libasan:%{!shared: -Bstatic "\
    LD_WHOLE_ARCHIVE_OPTION " -lasan " LD_NO_WHOLE_ARCHIVE_OPTION \
    "-Bdynamic}}%{!static-libasan:-lasan}"

/* Error out on -fsanitize=thread|leak.  */
#define LIBTSAN_EARLY_SPEC "\
  %e-fsanitize=thread is not supported in this configuration"
#define LIBLSAN_EARLY_SPEC "\
  %e-fsanitize=leak is not supported in this configuration"

/* We don't use the standard svr4 STARTFILE_SPEC because it's wrong for us.  */
#undef STARTFILE_SPEC
#ifdef HAVE_SOLARIS_CRTS
/* Since Solaris 11.4, the OS delivers crt1.o, crti.o, and crtn.o, with a hook
   for compiler-dependent stuff like profile handling.  */
#define STARTFILE_SPEC "%{!shared:%{!symbolic: \
			  crt1.o%s \
			  %{p:%e-p is not supported; \
			    pg:crtpg.o%s gmon.o%s; \
			      :crtp.o%s}}} \
			crti.o%s %(startfile_arch) %(startfile_crtbegin) \
			%(startfile_vtv)"
#else
#define STARTFILE_SPEC "%{!shared:%{!symbolic: \
			  %{p:mcrt1.o%s; \
                            pg:gcrt1.o%s gmon.o%s; \
                              :crt1.o%s}}} \
			crti.o%s %(startfile_arch) %(startfile_crtbegin) \
			%(startfile_vtv)"
#endif

#if defined(HAVE_LD_PIE) && defined(HAVE_SOLARIS_CRTS)
#define ENDFILE_CRTEND_SPEC "%{static:crtend.o%s; \
			       shared|" PIE_SPEC ":crtendS.o%s; \
			       :crtend.o%s}"
#else
#define ENDFILE_CRTEND_SPEC "crtend.o%s"
#endif

#undef  ENDFILE_SPEC
#define ENDFILE_SPEC \
  "%{Ofast|ffast-math|funsafe-math-optimizations:crtfastmath.o%s} \
   %(endfile_arch) %(endfile_vtv) %(endfile_crtend) crtn.o%s"

#undef LINK_ARCH32_SPEC_BASE
#define LINK_ARCH32_SPEC_BASE \
  "%{G:-G} \
   %{YP,*} \
   %{R*} \
   %{!YP,*:%{p|pg:-Y P,%R/usr/lib/libp%R/lib:%R/usr/lib} \
	   %{!p:%{!pg:-Y P,%R/lib:%R/usr/lib}}}"

#undef LINK_ARCH32_SPEC
#define LINK_ARCH32_SPEC LINK_ARCH32_SPEC_BASE

/* This should be the same as LINK_ARCH32_SPEC_BASE, except with
   ARCH64_SUBDIR appended to the paths.  */
#undef LINK_ARCH64_SPEC_BASE
#define LINK_ARCH64_SPEC_BASE \
  "%{G:-G} \
   %{YP,*} \
   %{R*} \
   %{!YP,*:%{p|pg:-Y P,%R/usr/lib/libp/" ARCH64_SUBDIR ":%R/lib/" ARCH64_SUBDIR ":%R/usr/lib/" ARCH64_SUBDIR "}	\
	   %{!p:%{!pg:-Y P,%R/lib/" ARCH64_SUBDIR ":%R/usr/lib/" ARCH64_SUBDIR "}}}"

#undef LINK_ARCH64_SPEC
#ifndef USE_GLD
/* FIXME: Used to be SPARC-only.  Not SPARC-specfic but for the model name!  */
#define LINK_ARCH64_SPEC \
  "%{mcmodel=medlow:-M /usr/lib/ld/" ARCH64_SUBDIR "/map.below4G} " \
  LINK_ARCH64_SPEC_BASE
#else
#define LINK_ARCH64_SPEC LINK_ARCH64_SPEC_BASE
#endif

#ifdef USE_GLD
#if DEFAULT_ARCH32_P
#define ARCH_DEFAULT_EMULATION ARCH32_EMULATION
#else
#define ARCH_DEFAULT_EMULATION ARCH64_EMULATION
#endif
#define TARGET_LD_EMULATION "%{m32:-m " ARCH32_EMULATION "}" \
			    "%{m64:-m " ARCH64_EMULATION "}" \
			    "%{!m32:%{!m64:-m " ARCH_DEFAULT_EMULATION "}} "
#else
#define TARGET_LD_EMULATION ""
#endif

#undef LINK_ARCH_SPEC
#if DISABLE_MULTILIB
#if DEFAULT_ARCH32_P
#define LINK_ARCH_SPEC TARGET_LD_EMULATION " \
%{m32:%(link_arch32)} \
%{m64:%edoes not support multilib} \
%{!m32:%{!m64:%(link_arch_default)}} \
"
#else
#define LINK_ARCH_SPEC TARGET_LD_EMULATION " \
%{m32:%edoes not support multilib} \
%{m64:%(link_arch64)} \
%{!m32:%{!m64:%(link_arch_default)}} \
"
#endif
#else
#define LINK_ARCH_SPEC TARGET_LD_EMULATION " \
%{m32:%(link_arch32)} \
%{m64:%(link_arch64)} \
%{!m32:%{!m64:%(link_arch_default)}}"
#endif

#define LINK_ARCH_DEFAULT_SPEC \
(DEFAULT_ARCH32_P ? LINK_ARCH32_SPEC : LINK_ARCH64_SPEC)

#undef SUBTARGET_EXTRA_SPECS
#define SUBTARGET_EXTRA_SPECS \
  { "startfile_arch",	 	STARTFILE_ARCH_SPEC },		\
  { "startfile_crtbegin",	STARTFILE_CRTBEGIN_SPEC },	\
  { "startfile_vtv",		STARTFILE_VTV_SPEC },		\
  { "link_arch32",       	LINK_ARCH32_SPEC },		\
  { "link_arch64",       	LINK_ARCH64_SPEC },		\
  { "link_arch_default", 	LINK_ARCH_DEFAULT_SPEC },	\
  { "link_arch",	 	LINK_ARCH_SPEC },		\
  { "endfile_arch",	 	ENDFILE_ARCH_SPEC },		\
  { "endfile_crtend",		ENDFILE_CRTEND_SPEC },		\
  { "endfile_vtv",		ENDFILE_VTV_SPEC },		\
  SUBTARGET_CPU_EXTRA_SPECS

/* C++11 programs need -lrt for nanosleep.  */
#define TIME_LIBRARY "rt"

#ifndef USE_GLD
/* With Sun ld, -rdynamic is a no-op.  */
#define RDYNAMIC_SPEC ""
#else
/* GNU ld needs --export-dynamic to implement -rdynamic.  */
#define RDYNAMIC_SPEC "--export-dynamic"
#endif

#ifndef USE_GLD
/* Prefer native form with Solaris ld.  */
#define SYSROOT_SPEC "-z sysroot=%R"
#endif

#if !defined(USE_GLD) && defined(ENABLE_SHARED_LIBGCC)
/* With Sun ld, use mapfile to enforce direct binding to libgcc_s unwinder.  */
#define LINK_LIBGCC_MAPFILE_SPEC \
  "%{shared|shared-libgcc:-M %slibgcc-unwind.map}"
#else
/* GNU ld doesn't support direct binding.  */
#define LINK_LIBGCC_MAPFILE_SPEC ""
#endif

/* Clear hardware capabilities, either explicitly or with OpenMP:
   #pragma openmp declare simd creates clones for SSE2, AVX, and AVX2.  */
#ifdef HAVE_LD_CLEARCAP
#define LINK_CLEARCAP_SPEC " %{mclear-hwcap|fopenmp*:-M %sclearcap.map}"
#else
#define LINK_CLEARCAP_SPEC ""
#endif

#undef  LINK_SPEC
#define LINK_SPEC \
  "%{h*} %{v:-V} \
   %{!shared:%{!static:%{rdynamic: " RDYNAMIC_SPEC "}}} \
   %{static:-dn -Bstatic} \
   %{shared:-G -dy %{!mimpure-text:-z text}} " \
   LINK_LIBGCC_MAPFILE_SPEC LINK_CLEARCAP_SPEC " \
   %{symbolic:-Bsymbolic -G -dy -z text} \
   %(link_arch) \
   %{Qy:} %{!Qn:-Qy}"

/* Use --as-needed/-z ignore -lgcc_s for eh support.  */
#ifdef HAVE_LD_AS_NEEDED
#define USE_LD_AS_NEEDED 1
#endif

#ifdef USE_GLD
/* Solaris 11 build 135+ implements dl_iterate_phdr.  GNU ld needs
   --eh-frame-hdr to create the required .eh_frame_hdr sections.  */
#if defined(HAVE_LD_EH_FRAME_HDR) && defined(TARGET_DL_ITERATE_PHDR)
#define LINK_EH_SPEC "%{!static|static-pie:--eh-frame-hdr} "
#endif /* HAVE_LD_EH_FRAME && TARGET_DL_ITERATE_PHDR */
#endif

#if defined(HAVE_LD_PIE) && defined(HAVE_SOLARIS_CRTS)
#ifdef USE_GLD
/* Assert -z text by default to match Solaris ld.  */
#define LD_PIE_SPEC "-pie %{!mimpure-text:-z text}"
#else
/* Solaris ld needs -z type=pie instead of -pie.  */
#define LD_PIE_SPEC "-z type=pie %{mimpure-text:-z textoff}"
#endif
#else
/* Error out if some part of PIE support is missing.  */
#define LINK_PIE_SPEC \
  "%{no-pie:} %{pie:%e-pie is not supported in this configuration} "
#endif

/* collect2.c can only parse GNU nm -n output.  Solaris nm needs -png to
   produce the same format.  */
#define NM_FLAGS "-png"

#define STDC_0_IN_SYSTEM_HEADERS 1

/* Support Solaris-specific format checking for cmn_err.  */
#define TARGET_N_FORMAT_TYPES 1
#define TARGET_FORMAT_TYPES solaris_format_types

/* #pragma init and #pragma fini are implemented on top of init and
   fini attributes.  */
#define SOLARIS_ATTRIBUTE_TABLE						\
  { "init",      0, 0, true,  false,  false, false, NULL, NULL },	\
  { "fini",      0, 0, true,  false,  false, false, NULL, NULL }

/* Solaris-specific #pragmas are implemented on top of attributes.  Hook in
   the bits from config/sol2.c.  */
#define SUBTARGET_INSERT_ATTRIBUTES solaris_insert_attributes
#define SUBTARGET_ATTRIBUTE_TABLE SOLARIS_ATTRIBUTE_TABLE

/* Allow macro expansion in #pragma pack.  */
#define HANDLE_PRAGMA_PACK_WITH_EXPANSION

#define TARGET_CXX_DECL_MANGLING_CONTEXT solaris_cxx_decl_mangling_context

/* Solaris/x86 as and gas support unquoted section names.  */
#ifndef SECTION_NAME_FORMAT
#define SECTION_NAME_FORMAT	"%s"
#endif

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

#ifndef USE_GAS
#undef TARGET_ASM_ASSEMBLE_VISIBILITY
#define TARGET_ASM_ASSEMBLE_VISIBILITY solaris_assemble_visibility

#define AS_NEEDS_DASH_FOR_PIPED_INPUT

/* The Solaris assembler cannot grok .stabd directives.  */
#undef NO_DBX_BNSYM_ENSYM
#define NO_DBX_BNSYM_ENSYM 1
#endif

/* Solaris has an implementation of __enable_execute_stack.  */
#define HAVE_ENABLE_EXECUTE_STACK

/* Static stack checking is supported by means of probes.  */
#define STACK_CHECK_STATIC_BUILTIN 1

#define TARGET_POSIX_IO

/* Solaris 10 has the float and long double forms of math functions.
   We redefine this hook so the version from elfos.h header won't be used.  */
#undef TARGET_LIBC_HAS_FUNCTION
#define TARGET_LIBC_HAS_FUNCTION default_libc_has_function

extern GTY(()) tree solaris_pending_aligns;
extern GTY(()) tree solaris_pending_inits;
extern GTY(()) tree solaris_pending_finis;
