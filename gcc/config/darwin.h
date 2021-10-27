/* Target definitions for Darwin (Mac OS X) systems.
   Copyright (C) 1989-2021 Free Software Foundation, Inc.
   Contributed by Apple Computer Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

Under Section 7 of GPL version 3, you are granted additional
permissions described in the GCC Runtime Library Exception, version
3.1, as published by the Free Software Foundation.

You should have received a copy of the GNU General Public License and
a copy of the GCC Runtime Library Exception along with this program;
see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
<http://www.gnu.org/licenses/>.  */

#ifndef CONFIG_DARWIN_H
#define CONFIG_DARWIN_H

/* The definitions in this file are common to all processor types
   running Darwin, which is the kernel for Mac OS X.  Darwin is
   basically a BSD user layer laid over a Mach kernel, then evolved
   for many years (at NeXT) in parallel with other Unix systems.  So
   while the runtime is a somewhat idiosyncratic Mach-based thing,
   other definitions look like they would for a BSD variant.  */

/* Although NeXT ran on many different architectures, as of Jan 2001
   the only supported Darwin targets are PowerPC and x86.  */

/* One of Darwin's NeXT legacies is the Mach-O format, which is partly
   like a.out and partly like COFF, with additional features like
   multi-architecture binary support.  */

#define DARWIN_X86 0
#define DARWIN_PPC 0

/* Suppress g++ attempt to link in the math library automatically. */
#define MATH_LIBRARY ""

/* We have atexit.  */

#define HAVE_ATEXIT

/* Define an empty body for the function do_global_dtors() in libgcc2.c.  */

#define DO_GLOBAL_DTORS_BODY

/* The string value for __SIZE_TYPE__.  */

#ifndef SIZE_TYPE
#define SIZE_TYPE "long unsigned int"
#endif

/* Type used for ptrdiff_t, as a string used in a declaration.  */

#undef  PTRDIFF_TYPE
#define PTRDIFF_TYPE "int"

/* wchar_t is int.  */

#undef	WCHAR_TYPE
#define WCHAR_TYPE "int"
#undef	WCHAR_TYPE_SIZE
#define WCHAR_TYPE_SIZE 32

#define INT8_TYPE "signed char"
#define INT16_TYPE "short int"
#define INT32_TYPE "int"
#define INT64_TYPE "long long int"
#define UINT8_TYPE "unsigned char"
#define UINT16_TYPE "short unsigned int"
#define UINT32_TYPE "unsigned int"
#define UINT64_TYPE "long long unsigned int"

#define INT_LEAST8_TYPE "signed char"
#define INT_LEAST16_TYPE "short int"
#define INT_LEAST32_TYPE "int"
#define INT_LEAST64_TYPE "long long int"
#define UINT_LEAST8_TYPE "unsigned char"
#define UINT_LEAST16_TYPE "short unsigned int"
#define UINT_LEAST32_TYPE "unsigned int"
#define UINT_LEAST64_TYPE "long long unsigned int"

#define INT_FAST8_TYPE "signed char"
#define INT_FAST16_TYPE "short int"
#define INT_FAST32_TYPE "int"
#define INT_FAST64_TYPE "long long int"
#define UINT_FAST8_TYPE "unsigned char"
#define UINT_FAST16_TYPE "short unsigned int"
#define UINT_FAST32_TYPE "unsigned int"
#define UINT_FAST64_TYPE "long long unsigned int"

#define INTPTR_TYPE "long int"
#define UINTPTR_TYPE "long unsigned int"

#define SIG_ATOMIC_TYPE "int"

/* Default to using the NeXT-style runtime, since that's what is
   pre-installed on Darwin systems.  */

#define NEXT_OBJC_RUNTIME 100508

/* Don't default to pcc-struct-return, because gcc is the only compiler, and
   we want to retain compatibility with older gcc versions.  */

#undef	DEFAULT_PCC_STRUCT_RETURN
#define DEFAULT_PCC_STRUCT_RETURN 0

/* True if pragma ms_struct is in effect.  */
extern GTY(()) int darwin_ms_struct;

/* Darwin has a user convenience feature where some linker options are exposed
   at the driver level (so one can type "-all_load" instead of "-Wl,-all_load"
   or "-Xlinker -all_load").  We retain this, but now these options are all
   marked as 'Driver' and we process them as early as possible so that they
   get allocated to the right toolchain command.  There are a couple of special
   cases where these driver opts are used multiple times, or to control
   operations on more than one command (e.g. dynamiclib).  These are handled
   specially and we then add %<xxxx specs for the commands that _don't_ need
   them.  NOTE: the order of 'shared' and 'dynamiclib' is significant, hence
   they are placed out of alphabetical order at the start.  Likewise, we keep
   a couple of cases where a negative option originally appeared after the
   positive alternate, potentially overriding it.
   When we report an error with %e, it seems necessary to strip the option
   before doing so, otherwise it survives to the cc1 command line (%e doesn't
   appear to abort the program before this).
   Right now there's no mechanism to split up the "variable portion" (%*) of
   the matched spec string, so where we have some driver specs that take 2
   or 3 arguments, these cannot be processed here, but are deferred until the
   LINK_SPEC, where they are copied verbatim.
   We have a "safe" version of the MacOS version string, that's been sanity-
   checked and truncated to minor version.  If the 'tiny' (3rd) portion of the
   value is not significant, it's better to use this in version-compare().  */

#undef SUBTARGET_DRIVER_SELF_SPECS
#define SUBTARGET_DRIVER_SELF_SPECS					\
  "%{shared:%{!dynamiclib:-dynamiclib}} %<shared",			\
  "%{static:%{dynamic|dynamiclib:%econflicting code generation switches}}",\
  "%{dynamiclib:-Xlinker -dylib \
     %{allowable_client*:-Xlinker -allowable_client -Xlinker %*} \
       %<allowable_client* \
     %{bundle_loader*: %<bundle_loader* \
       %e-bundle_loader not allowed with -dynamiclib} \
     %{client_name*: %<client_name* \
       %e-client_name not allowed with -dynamiclib} \
     %{compatibility_version*:\
       -Xlinker -dylib_compatibility_version -Xlinker %*} \
       %<compatibility_version* \
     %{current_version*:-Xlinker -dylib_current_version -Xlinker %*} \
       %<current_version* \
     %{install_name*:-Xlinker -dylib_install_name -Xlinker %* } \
       %<install_name* \
     %{keep_private_externs: %<keep_private_externs \
       %e-keep_private_externs not allowed with -dynamiclib} \
     %{private_bundle: %<private_bundle \
       %e-private_bundle not allowed with -dynamiclib} \
    }",									\
  "%{!dynamiclib: \
     %{bundle_loader*:-Xlinker -bundle_loader -Xlinker %*} \
       %<bundle_loader* \
     %{client_name*:-Xlinker -client_name -Xlinker %*} \
       %<client_name* \
     %{compatibility_version*: %<compatibility_version* \
       %e-compatibility_version only allowed with -dynamiclib} \
     %{current_version*: %<current_version* \
       %e-current_version only allowed with -dynamiclib} \
     %{install_name*: %<install_name* \
       %e-install_name only allowed with -dynamiclib} \
     %{keep_private_externs:-Xlinker -keep_private_externs} \
       %<keep_private_externs \
     %{private_bundle:-Xlinker -private_bundle} \
       %<private_bundle \
    }",									\
  "%{all_load:-Xlinker -all_load} %<all_load",				\
  "%{arch_errors_fatal:-Xlinker -arch_errors_fatal} \
    %<arch_errors_fatal",						\
  "%{bind_at_load:-Xlinker -bind_at_load} %<bind_at_load",		\
  "%{bundle:%{!dynamiclib:-Xlinker -bundle; \
              :%e-bundle not allowed with -dynamiclib}}",	\
  "%{dead_strip:-Xlinker -dead_strip} %<dead_strip",			\
  "%{dylib_file*:-Xlinker -dylib_file -Xlinker %*} %<dylib_file*",	\
  "%{dylinker:-Xlinker -dylinker} %<dylinker",				\
  "%{dylinker_install_name*:-Xlinker -dylinker_install_name -Xlinker %*}\
    %<dylinker_install_name*",						\
  "%{exported_symbols_list*:-Xlinker -exported_symbols_list -Xlinker %*}\
    %<exported_symbols_list",						\
  "%{findirect-virtual-calls: -fapple-kext} %<findirect-virtual-calls", \
  "%{fterminated-vtables: -fapple-kext} %<fterminated-vtables",		\
  "%{fapple-kext|mkernel:-static}",					\
  "%{filelist*:-Xlinker -filelist -Xlinker %*} %<filelist*",		\
  "%{flat_namespace:-Xlinker -flat_namespace} %<flat_namespace",	\
  "%{force_cpusubtype_ALL:-Xassembler -force_cpusubtype_ALL} ",		\
  "%{force_flat_namespace: \
     %{!dynamiclib:-Xlinker -force_flat_namespace; \
       :%e-force_flat_namespace not allowed with -dynamiclib}} \
    %<force_flat_namespace",						\
  "%{framework*:-Xlinker -framework -Xlinker %*} %<framework*",		\
  "%{gfull:-g -fno-eliminate-unused-debug-symbols} %<gfull",		\
  "%{gused:-g -feliminate-unused-debug-symbols} %<gused",		\
  "%{gsplit-dwarf:%ngsplit-dwarf is not supported on this platform} \
    %<gsplit-dwarf",							\
  "%{headerpad_max_install_names:-Xlinker -headerpad_max_install_names}\
    %<headerpad_max_install_names",					\
  "%{image_base*:-Xlinker -image_base -Xlinker %*} %<image_base*",	\
  "%{init*:-Xlinker -init -Xlinker %*} %<init*",			\
  "%{multi_module:-Xlinker -multi_module} %<multi_module",		\
  "%{multiply_defined*:-Xlinker -multiply_defined -Xlinker %*; \
     :%{shared-libgcc: \
       %:version-compare(< 10.5 asm_macosx_version_min= -Xlinker) \
       %:version-compare(< 10.5 asm_macosx_version_min= -multiply_defined) \
       %:version-compare(< 10.5 asm_macosx_version_min= -Xlinker) \
       %:version-compare(< 10.5 asm_macosx_version_min= suppress)}} \
     %<multiply_defined*",						\
  "%{multiplydefinedunused*:\
     -Xlinker -multiply_defined_unused -Xlinker %*} \
     %<multiplydefinedunused* ",					\
  "%{no_dead_strip_inits_and_terms:\
     -Xlinker -no_dead_strip_inits_and_terms} \
     %<no_dead_strip_inits_and_terms",					\
  "%{nofixprebinding:-Xlinker -nofixprebinding} %<nofixprebinding",	\
  "%{nomultidefs:-Xlinker -nomultidefs} %<nomultidefs",			\
  "%{pagezero_size*:-Xlinker -pagezero_size -Xlinker %*} \
    %<pagezero_size",							\
  "%{prebind:-Xlinker -prebind} %<prebind",				\
  "%{noprebind:-Xlinker -noprebind} %<noprebind",			\
  "%{prebind_all_twolevel_modules:\
     -Xlinker -prebind_all_twolevel_modules} \
     %<prebind_all_twolevel_modules",					\
  "%{preload:-Xlinker -preload} %<preload",				\
  "%{read_only_relocs*:-Xlinker -read_only_relocs -Xlinker %*} \
     %<read_only_relocs*",						\
  "%{rpath*: -Xlinker -rpath -Xlinker %*}",				\
  "%{seg_addr_table_filename*: \
     -Xlinker -seg_addr_table_filename -Xlinker %*} \
     %<seg_addr_table_filename*",					\
  "%{seg_addr_table*:-Xlinker -seg_addr_table -Xlinker %*} \
     %<seg_addr_table*",						\
  "%{seg1addr*:-Xlinker -image_base -Xlinker %*} %<seg1addr*",		\
  "%{seglinkedit:-Xlinker -seglinkedit} %<seglinkedit",			\
  "%{noseglinkedit:-Xlinker -noseglinkedit} %<noseglinkedit",		\
  "%{segs_read_only_addr*:-Xlinker -segs_read_only_addr -Xlinker %*} \
    %<segs_read_only_addr*",						\
  "%{segs_read_write_addr*:-Xlinker -segs_read_write_addr -Xlinker %*} \
    %<segs_read_write_addr*",						\
  "%{single_module:-Xlinker -single_module} %<single_module",		\
  "%{sub_library*:-Xlinker -sub_library -Xlinker %*} %<sub_library*",	\
  "%{sub_umbrella*:-Xlinker -sub_umbrella -Xlinker %*} %<sub_umbrella*",\
  "%{twolevel_namespace:-Xlinker -twolevel_namespace} \
     %<twolevel_namespace",						\
  "%{twolevel_namespace_hints:-Xlinker -twolevel_namespace_hints} \
     %<twolevel_namespace_hints",					\
  "%{umbrella*:-Xlinker -umbrella -Xlinker %*} %<umbrella*",		\
  "%{undefined*:-Xlinker -undefined -Xlinker %*} %<undefined*",		\
  "%{unexported_symbols_list*:\
     -Xlinker -unexported_symbols_list -Xlinker %*} \
     %<unexported_symbols_list*",					\
  "%{!weak_reference_mismatches*:\
     %:version-compare(< 10.5 asm_macosx_version_min= -Xlinker) \
 %:version-compare(< 10.5 asm_macosx_version_min= -weak_reference_mismatches) \
     %:version-compare(< 10.5 asm_macosx_version_min= -Xlinker) \
     %:version-compare(< 10.5 asm_macosx_version_min= non-weak)}",	\
  "%{weak_reference_mismatches*:\
    -Xlinker -weak_reference_mismatches -Xlinker %*} \
    %<weak_reference_mismatches*",					\
  "%{whyload:-Xlinker -whyload} %<whyload",				\
  "%{whatsloaded:-Xlinker -whatsloaded} %<whatsloaded",			\
  "%{w:-Xlinker -w}",							\
  "%<y*",								\
  "%<Mach "

#if LD64_HAS_EXPORT_DYNAMIC
#define DARWIN_RDYNAMIC "%{rdynamic:-export_dynamic}"
#else
#define DARWIN_RDYNAMIC "%{rdynamic:%nrdynamic is not supported}"
#endif

/* FIXME: we should check that the linker supports the -pie and -no_pie.
   options.  */
#define DARWIN_PIE_SPEC \
"%{pie|fpie|fPIE:\
   %{mdynamic-no-pic: \
     %n'-mdynamic-no-pic' overrides '-pie', '-fpie' or '-fPIE'; \
     :%:version-compare(>= 10.5 mmacosx-version-min= -pie) }} "

#define DARWIN_NOPIE_SPEC \
"%{no-pie|fno-pie|fno-PIE: \
   %:version-compare(>= 10.7 mmacosx-version-min= -no_pie) }"

#define DARWIN_CC1_SPEC							\
  "%<dynamic %<dynamiclib %<force_cpusubtype_ALL "

#define SUBSUBTARGET_OVERRIDE_OPTIONS					\
  do {									\
    darwin_override_options ();						\
  } while (0)

#define SUBTARGET_C_COMMON_OVERRIDE_OPTIONS do {                        \
    if (flag_mkernel || flag_apple_kext)				\
      {									\
	if (flag_use_cxa_atexit == 2)					\
	  flag_use_cxa_atexit = 0;					\
	/* kexts should always be built without the coalesced sections	\
	   because the kernel loader doesn't grok such sections.  */	\
	flag_weak = 0;							\
	/* No RTTI in kexts.  */					\
	flag_rtti = 0;							\
      }									\
  } while (0)

/* Machine dependent cpp options.  Don't add more options here, add
   them to darwin_cpp_builtins in darwin-c.c.  */

#undef	CPP_SPEC
#define CPP_SPEC "%{static:%{!dynamic:-D__STATIC__}}%{!static:-D__DYNAMIC__}" \
	" %{pthread:-D_REENTRANT} "

/* This is a fix for PR41260 by passing -no_compact_unwind on darwin10 and
   later until the assembler, linker and libunwind are able to deal with the
   output from GCC.

   FIXME: we should check that the linker supports the option.
*/

#define DARWIN_NOCOMPACT_UNWIND \
" %:version-compare(>= 10.6 mmacosx-version-min= -no_compact_unwind) "

/* In Darwin linker specs we can put -lcrt0.o and ld will search the library
   path for crt0.o or -lcrtx.a and it will search for for libcrtx.a.  As for
   other ports, we can also put xxx.{o,a}%s and get the appropriate complete
   startfile absolute directory.  This latter point is important when we want
   to override ld's rule of .dylib being found ahead of .a and the user wants
   the convenience library to be linked.  */

/* The LINK_COMMAND spec is mostly a clone of the standard LINK_COMMAND_SPEC,
   plus precomp, libtool, and fat build additions.

   In general, random Darwin linker flags should go into LINK_SPEC
   instead of LINK_COMMAND_SPEC.  The command spec is better for
   specifying the handling of options understood by generic Unix
   linkers, and for positional arguments like libraries.  */

#define LINK_COMMAND_SPEC_A \
   "%{!c:%{!E:%{!S:%{!M:%{!MM:%{!fsyntax-only:%{!fdump=*: \
    %(linker)" \
    LINK_PLUGIN_SPEC \
    "%{flto*:%<fcompare-debug*} \
     %{flto} %{fno-lto} %{flto=*} \
    %l " LINK_COMPRESS_DEBUG_SPEC \
   "%X %{s} %{t} %{Z} %{u*} \
    %{e*} %{r} \
    %{o*}%{!o:-o a.out} \
    %{!r:%{!nostdlib:%{!nostartfiles:%S}}} \
    %{L*} %(link_libgcc) %o \
    %{!r:%{!nostdlib:%{!nodefaultlibs:\
      %{fprofile-arcs|fprofile-generate*|coverage:-lgcov} \
      %{fopenacc|fopenmp|%:gt(%{ftree-parallelize-loops=*:%*} 1): \
	%{static|static-libgcc|static-libstdc++|static-libgfortran: \
	  libgomp.a%s; : -lgomp }} \
      %{fgnu-tm: \
	%{static|static-libgcc|static-libstdc++|static-libgfortran: \
	  libitm.a%s; : -litm }} \
      %{%:sanitize(address): -lasan } \
      %{%:sanitize(undefined): -lubsan } \
      %(link_ssp) \
      %:version-compare(>< 10.6 10.7 mmacosx-version-min= -ld10-uwfef) \
      %(link_gcc_c_sequence) \
    }}}\
    %{!r:%{!nostdlib:%{!nostartfiles:%E}}} %{T*} %{F*} "\
    DARWIN_PIE_SPEC \
    DARWIN_NOPIE_SPEC \
    DARWIN_RDYNAMIC \
    DARWIN_NOCOMPACT_UNWIND \
    "}}}}}}} %<pie %<no-pie %<rdynamic %<X %<rpath "

/* Spec that controls whether the debug linker is run automatically for
   a link step.  This needs to be done if there is a source file on the
   command line which will result in a temporary object (and debug is
   enabled).  */

#define DSYMUTIL_SPEC \
  "%{!c:%{!E:%{!S:%{!r:%{!M:%{!MM:%{!fsyntax-only:%{!fdump=*:\
     %{g*:%{!gctf:%{!gbtf:%{!gstabs*:%{%:debug-level-gt(0): -idsym \
       %{.c|.cc|.C|.cpp|.cp|.c++|.cxx|.CPP|.m|.mm|.s|.f|.f90|\
	 .f95|.f03|.f77|.for|.F|.F90|.F95|.F03|.d: -dsym }\
      }}}}}\
   }}}}}}}}"

#define LINK_COMMAND_SPEC LINK_COMMAND_SPEC_A DSYMUTIL_SPEC

/* Tell collect2 to run dsymutil for us as necessary.  */
#define COLLECT_RUN_DSYMUTIL 1

/* Fix PR47558 by linking against libSystem ahead of libgcc. See also
   PR 80556 and the fallout from this.  */

#undef  LINK_GCC_C_SEQUENCE_SPEC
#define LINK_GCC_C_SEQUENCE_SPEC \
"%{!static:%{!static-libgcc: \
    %:version-compare(>= 10.6 mmacosx-version-min= -lSystem) } } \
  %G %{!nolibc:%L}"

/* ld64 supports a sysroot, it just has a different name and there's no easy
   way to check for it at config time.  */
#undef HAVE_LD_SYSROOT
#define HAVE_LD_SYSROOT 1
/* It seems the only (working) way to get a space after %R is to append a
   dangling '/'.  */
#define SYSROOT_SPEC "%{!isysroot*:-syslibroot %R/ } "

/* Do the same as clang, for now, and insert the sysroot for ld when an
   isysroot is specified.  */
#define LINK_SYSROOT_SPEC "%{isysroot*:-syslibroot %*} "

/* Suppress the addition of extra prefix paths when a sysroot is in use.  */
#define STANDARD_STARTFILE_PREFIX_1 ""
#define STANDARD_STARTFILE_PREFIX_2 ""


/* Please keep the random linker options in alphabetical order.
   Note that options taking arguments may appear multiple times on a command
   line with different arguments each time, so put a * after their names so
   all of them get passed.  */
#define LINK_SPEC  \
  "%{static}%{!static:%{!dynamic:-dynamic}} \
   %:remove-outfile(-ldl) \
   %:remove-outfile(-lm) \
   %:remove-outfile(-lpthread) \
   %{fgnu-runtime: %{static|static-libgcc: \
                     %:replace-outfile(-lobjc libobjc-gnu.a%s); \
                    :%:replace-outfile(-lobjc -lobjc-gnu )}}\
   %{static|static-libgcc|static-libgfortran:%:replace-outfile(-lgfortran libgfortran.a%s)}\
   %{static|static-libgcc|static-libstdc++|static-libgfortran:%:replace-outfile(-lgomp libgomp.a%s)}\
   %{static|static-libgcc|static-libstdc++:%:replace-outfile(-lstdc++ libstdc++.a%s)}\
   %{force_cpusubtype_ALL:-arch %(darwin_arch)} \
   %{!force_cpusubtype_ALL:-arch %(darwin_subarch)} "\
   LINK_SYSROOT_SPEC \
  "%{mmacosx-version-min=*:-macosx_version_min %*} \
   %{sectalign*} %{sectcreate*} %{sectobjectsymbols*}  %{sectorder*} \
   %{segaddr*} %{segcreate*} %{segprot*} "

/* Machine dependent libraries.  */

#define LIB_SPEC "%{!static:-lSystem}"

/* Support -mmacosx-version-min by supplying different (stub) libgcc_s.dylib
   libraries to link against, and by not linking against libgcc_s on
   earlier-than-10.3.9.  If we need exceptions, prior to 10.3.9, then we have
   to link the static eh lib, since there's no shared version on the system.

   Note that by default, except as above, -lgcc_eh is not linked against.
   This is because,in general, we need to unwind through system libraries that
   are linked with the shared unwinder in libunwind (or libgcc_s for 10.4/5).

   The static version of the current libgcc unwinder (which differs from the
   implementation in libunwind.dylib on systems Darwin10 [10.6]+) can be used
   by specifying -static-libgcc.

   If libgcc_eh is linked against, it has to be before -lgcc, because it might
   need symbols from -lgcc.  */

#undef REAL_LIBGCC_SPEC
#define REAL_LIBGCC_SPEC						   \
   "%{static-libgcc|static: -lgcc_eh -lgcc;				   \
      shared-libgcc|fexceptions|fobjc-exceptions|fgnu-runtime:		   \
       %:version-compare(!> 10.3.9 mmacosx-version-min= -lgcc_eh)	   \
       %:version-compare(>< 10.3.9 10.5 mmacosx-version-min= -lgcc_s.10.4) \
       %:version-compare(>< 10.5 10.6 mmacosx-version-min= -lgcc_s.10.5)   \
       %:version-compare(>< 10.3.9 10.5 mmacosx-version-min= -lgcc_ext.10.4) \
       %:version-compare(>= 10.5 mmacosx-version-min= -lgcc_ext.10.5)	   \
       -lgcc ;								   \
      :%:version-compare(>< 10.3.9 10.5 mmacosx-version-min= -lgcc_s.10.4) \
       %:version-compare(>< 10.5 10.6 mmacosx-version-min= -lgcc_s.10.5)   \
       %:version-compare(>< 10.3.9 10.5 mmacosx-version-min= -lgcc_ext.10.4) \
       %:version-compare(>= 10.5 mmacosx-version-min= -lgcc_ext.10.5)	   \
       -lgcc }"

/* We specify crt0.o as -lcrt0.o so that ld will search the library path.  */

#undef  STARTFILE_SPEC
#define STARTFILE_SPEC							    \
"%{dynamiclib: %(darwin_dylib1) %{fgnu-tm: -lcrttms.o}}			   \
 %{!dynamiclib:%{bundle:%(darwin_bundle1)}				    \
     %{!bundle:%{pg:%{static:-lgcrt0.o}					    \
                     %{!static:%{object:-lgcrt0.o}			    \
                               %{!object:%{preload:-lgcrt0.o}		    \
                                 %{!preload:-lgcrt1.o                       \
                                 %:version-compare(>= 10.8 mmacosx-version-min= -no_new_main) \
                                 %(darwin_crt2)}}}}    \
                %{!pg:%{static:-lcrt0.o}				    \
                      %{!static:%{object:-lcrt0.o}			    \
                                %{!object:%{preload:-lcrt0.o}		    \
                                  %{!preload: %(darwin_crt1)		    \
					      %(darwin_crt2)}}}}}}	    \
 %(darwin_crt3) %<dynamiclib "

/* We want a destructor last in the list.  */
#define TM_DESTRUCTOR "%{fgnu-tm: -lcrttme.o}"
#define ENDFILE_SPEC TM_DESTRUCTOR

#define DARWIN_EXTRA_SPECS						\
  { "darwin_crt1", DARWIN_CRT1_SPEC },					\
  { "darwin_crt2", DARWIN_CRT2_SPEC },					\
  { "darwin_crt3", DARWIN_CRT3_SPEC },					\
  { "darwin_dylib1", DARWIN_DYLIB1_SPEC },				\
  { "darwin_bundle1", DARWIN_BUNDLE1_SPEC },

#define DARWIN_CRT1_SPEC						\
  "%:version-compare(!> 10.5 mmacosx-version-min= -lcrt1.o)		\
   %:version-compare(>< 10.5 10.6 mmacosx-version-min= -lcrt1.10.5.o)	\
   %:version-compare(>< 10.6 10.8 mmacosx-version-min= -lcrt1.10.6.o)	\
   %{fgnu-tm: -lcrttms.o}"

#define DARWIN_CRT2_SPEC ""

/* crt3.o provides __cxa_atexit on systems that don't have it (and a fix
   up for faulty versions on 10.4).  Since it's only used with C++, which
   requires passing -shared-libgcc, key off that to avoid unnecessarily
   adding a destructor to every program built for 10.4 or earlier.  */

#define DARWIN_CRT3_SPEC \
"%{shared-libgcc:%:version-compare(< 10.5 mmacosx-version-min= crt3.o%s)}"

#define DARWIN_DYLIB1_SPEC						\
  "%:version-compare(!> 10.5 mmacosx-version-min= -ldylib1.o)		\
   %:version-compare(>< 10.5 10.6 mmacosx-version-min= -ldylib1.10.5.o)"

#define DARWIN_BUNDLE1_SPEC \
"%{!static:%:version-compare(< 10.6 mmacosx-version-min= -lbundle1.o)	\
	   %{fgnu-tm: -lcrttms.o}}"

#ifdef HAVE_AS_MMACOSX_VERSION_MIN_OPTION
/* Emit macosx version (but only major).  */
#define ASM_MMACOSX_VERSION_MIN_SPEC \
"%{asm_macosx_version_min=*: -mmacosx-version-min=%* } \
   %<asm_macosx_version_min=* "
#else
#define ASM_MMACOSX_VERSION_MIN_SPEC " %<asm_macosx_version_min=* "
#endif

#if HAVE_GNU_AS
/* The options are added in gcc.c for this case.  */
#define ASM_OPTIONS ""
#else
/* When we detect that we're cctools or llvm as, we need to insert the right
   additional options.  Actually, currently these are the same as GAS.  */
#define ASM_OPTIONS "%{v} %{w:-W} %{I*}"
#endif

/* Default Darwin ASM_SPEC, very simple. */
#define ASM_SPEC \
"%{static} -arch %(darwin_arch) " \
ASM_OPTIONS ASM_MMACOSX_VERSION_MIN_SPEC

#ifdef HAVE_AS_STABS_DIRECTIVE
/* We only pass a debug option to the assembler if that supports stabs, since
   dwarf is not uniformly supported in the assemblers.  */
#define ASM_DEBUG_SPEC  "%{g*:%{%:debug-level-gt(0):%{!gdwarf*:--gstabs}}}"
#else
#define ASM_DEBUG_SPEC  ""
#endif

#undef  ASM_DEBUG_OPTION_SPEC
#define ASM_DEBUG_OPTION_SPEC	""

#define ASM_FINAL_SPEC \
  "%{gsplit-dwarf:%ngsplit-dwarf is not supported on this platform} \
     %<gsplit-dwarf"

/* We now require C++11 to bootstrap and newer tools than those based on
   stabs, so require DWARF-2, even if stabs is supported by the assembler.  */

#define DWARF2_DEBUGGING_INFO 1
#define PREFERRED_DEBUGGING_TYPE DWARF2_DEBUG

#ifdef HAVE_AS_STABS_DIRECTIVE
#define DBX_DEBUGGING_INFO 1
#endif

#define DEBUG_FRAME_SECTION	  "__DWARF,__debug_frame,regular,debug"
#define DEBUG_INFO_SECTION	  "__DWARF,__debug_info,regular,debug"
#define DEBUG_ABBREV_SECTION	  "__DWARF,__debug_abbrev,regular,debug"
#define DEBUG_ARANGES_SECTION	  "__DWARF,__debug_aranges,regular,debug"
#define DEBUG_MACINFO_SECTION	  "__DWARF,__debug_macinfo,regular,debug"
#define DEBUG_LINE_SECTION	  "__DWARF,__debug_line,regular,debug"
#define DEBUG_LOC_SECTION	  "__DWARF,__debug_loc,regular,debug"
#define DEBUG_LOCLISTS_SECTION    "__DWARF,__debug_loclists,regular,debug"

#define DEBUG_STR_SECTION	  "__DWARF,__debug_str,regular,debug"
#define DEBUG_STR_OFFSETS_SECTION "__DWARF,__debug_str_offs,regular,debug"
#define DEBUG_RANGES_SECTION	  "__DWARF,__debug_ranges,regular,debug"
#define DEBUG_RNGLISTS_SECTION    "__DWARF,__debug_rnglists,regular,debug"
#define DEBUG_MACRO_SECTION       "__DWARF,__debug_macro,regular,debug"

#define DEBUG_LTO_INFO_SECTION	  "__GNU_DWARF_LTO,__debug_info,regular,debug"
#define DEBUG_LTO_ABBREV_SECTION  "__GNU_DWARF_LTO,__debug_abbrev,regular,debug"
#define DEBUG_LTO_MACINFO_SECTION "__GNU_DWARF_LTO,__debug_macinfo,regular,debug"
#define DEBUG_LTO_LINE_SECTION	  "__GNU_DWARF_LTO,__debug_line,regular,debug"
#define DEBUG_LTO_STR_SECTION	  "__GNU_DWARF_LTO,__debug_str,regular,debug"
#define DEBUG_LTO_MACRO_SECTION   "__GNU_DWARF_LTO,__debug_macro,regular,debug"

#define TARGET_WANT_DEBUG_PUB_SECTIONS true
#define DEBUG_PUBNAMES_SECTION   ((debug_generate_pub_sections == 2) \
                               ? "__DWARF,__debug_gnu_pubn,regular,debug" \
                               : "__DWARF,__debug_pubnames,regular,debug")

#define DEBUG_PUBTYPES_SECTION   ((debug_generate_pub_sections == 2) \
                               ? "__DWARF,__debug_gnu_pubt,regular,debug" \
                               : "__DWARF,__debug_pubtypes,regular,debug")

/* When generating stabs debugging, use N_BINCL entries.  */

#define DBX_USE_BINCL

/* There is no limit to the length of stabs strings.  */

#define DBX_CONTIN_LENGTH 0

/* gdb needs a null N_SO at the end of each file for scattered loading.  */

#define DBX_OUTPUT_NULL_N_SO_AT_MAIN_SOURCE_FILE_END

/* GCC's definition of 'one_only' is the same as its definition of 'weak'.  */
#define MAKE_DECL_ONE_ONLY(DECL) (DECL_WEAK (DECL) = 1)

/* Mach-O supports 'weak imports', and 'weak definitions' in coalesced
   sections.  machopic_select_section ensures that weak variables go in
   coalesced sections.  Weak aliases (or any other kind of aliases) are
   not supported.  Weak symbols that aren't visible outside the .s file
   are not supported.  */
#define ASM_WEAKEN_DECL(FILE, DECL, NAME, ALIAS)			\
  do {									\
    if (ALIAS)								\
      {									\
	warning (0, "alias definitions not supported in Mach-O; ignored");	\
	break;								\
      }									\
 									\
    if (! DECL_EXTERNAL (DECL) && TREE_PUBLIC (DECL))			\
      targetm.asm_out.globalize_label (FILE, NAME);			\
    if (DECL_EXTERNAL (DECL))						\
      fputs ("\t.weak_reference ", FILE);				\
    else if (lookup_attribute ("weak_import", DECL_ATTRIBUTES (DECL)))	\
      break;								\
    else if (TREE_PUBLIC (DECL))					\
      fputs ("\t.weak_definition ", FILE);				\
    else								\
      break;								\
    assemble_name (FILE, NAME);						\
    fputc ('\n', FILE);							\
  } while (0)

/* Darwin has the pthread routines in libSystem, which every program
   links to, so there's no need for weak-ness for that.  */
#define GTHREAD_USE_WEAK 0

/* On Darwin, we don't (at the time of writing) have linkonce sections
   with names, so it's safe to make the class data not comdat.  */
#define TARGET_CXX_CLASS_DATA_ALWAYS_COMDAT hook_bool_void_false

/* For efficiency, on Darwin the RTTI information that is always
   emitted in the standard C++ library should not be COMDAT.  */
#define TARGET_CXX_LIBRARY_RTTI_COMDAT hook_bool_void_false

/* We make exception information linkonce. */
#undef TARGET_USES_WEAK_UNWIND_INFO
#define TARGET_USES_WEAK_UNWIND_INFO 1

/* We need to use a nonlocal label for the start of an EH frame: the
   Darwin linker requires that a coalesced section start with a label.
   Unfortunately, it also requires that 'debug' sections don't contain
   labels.  */
#undef FRAME_BEGIN_LABEL
#define FRAME_BEGIN_LABEL (for_eh ? "EH_frame" : "Lframe")

/* Emit a label for the FDE corresponding to DECL.  EMPTY means
   emit a label for an empty FDE. */
#define TARGET_ASM_EMIT_UNWIND_LABEL darwin_emit_unwind_label

/* Emit a label to separate the exception table.  */
#define TARGET_ASM_EMIT_EXCEPT_TABLE_LABEL darwin_emit_except_table_label

/* Make an EH (personality or LDSA) symbol indirect as needed.  */
#define TARGET_ASM_MAKE_EH_SYMBOL_INDIRECT darwin_make_eh_symbol_indirect

/* Some of Darwin's unwinders need current frame address state to be reset
   after a DW_CFA_restore_state recovers the register values.  */
#undef TARGET_ASM_SHOULD_RESTORE_CFA_STATE
#define TARGET_ASM_SHOULD_RESTORE_CFA_STATE darwin_should_restore_cfa_state

/* Our profiling scheme doesn't LP labels and counter words.  */

#define NO_PROFILE_COUNTERS	1

#undef	INIT_SECTION_ASM_OP
#define INIT_SECTION_ASM_OP ""

#undef	INVOKE__main

#define TARGET_ASM_CONSTRUCTOR  machopic_asm_out_constructor
#define TARGET_ASM_DESTRUCTOR   machopic_asm_out_destructor

/* Always prefix with an underscore.  */

#define USER_LABEL_PREFIX "_"

/* A dummy symbol that will be replaced with the function base name.  */
#define MACHOPIC_FUNCTION_BASE_NAME "<pic base>"

/* Don't output a .file directive.  That is only used by the assembler for
   error reporting.  */
#undef	TARGET_ASM_FILE_START_FILE_DIRECTIVE
#define TARGET_ASM_FILE_START_FILE_DIRECTIVE false

#undef  TARGET_ASM_FILE_END
#define TARGET_ASM_FILE_END darwin_file_end

/* Because Mach-O relocations have a counter from 1 to 255 for the
   section number they apply to, it is necessary to output all
   normal sections before the LTO sections, to make sure that the
   sections that may have relocations always have a section number
   smaller than 255.  */
#undef  TARGET_ASM_LTO_START
#define TARGET_ASM_LTO_START darwin_asm_lto_start
#undef  TARGET_ASM_LTO_END
#define TARGET_ASM_LTO_END darwin_asm_lto_end

#define ASM_OUTPUT_SKIP(FILE,SIZE)  \
  fprintf (FILE, "\t.space " HOST_WIDE_INT_PRINT_UNSIGNED"\n", SIZE)

/* Give ObjC methods pretty symbol names.  */

#undef	OBJC_GEN_METHOD_LABEL
#define OBJC_GEN_METHOD_LABEL(BUF,IS_INST,CLASS_NAME,CAT_NAME,SEL_NAME,NUM) \
  do { if (CAT_NAME)							\
	 sprintf (BUF, "%c[%s(%s) %s]", (IS_INST) ? '-' : '+',		\
		  (CLASS_NAME), (CAT_NAME), (SEL_NAME));		\
       else								\
	 sprintf (BUF, "%c[%s %s]", (IS_INST) ? '-' : '+',		\
		  (CLASS_NAME), (SEL_NAME));				\
     } while (0)

#undef ASM_DECLARE_OBJECT_NAME
#define ASM_DECLARE_OBJECT_NAME(FILE, NAME, DECL) \
	darwin_asm_declare_object_name ((FILE), (NAME), (DECL))

/* The RTTI data (e.g., __ti4name) is common and public (and static),
   but it does need to be referenced via indirect PIC data pointers.
   The machopic_define_symbol calls are telling the machopic subsystem
   that the name *is* defined in this module, so it doesn't need to
   make them indirect.  */

#undef ASM_DECLARE_FUNCTION_NAME
#define ASM_DECLARE_FUNCTION_NAME(FILE, NAME, DECL)			\
  do {									\
    const char *xname = NAME;						\
    if (GET_CODE (XEXP (DECL_RTL (DECL), 0)) != SYMBOL_REF)		\
      xname = IDENTIFIER_POINTER (DECL_NAME (DECL));			\
    if (! DECL_WEAK (DECL)						\
        && ((TREE_STATIC (DECL)						\
	     && (!DECL_COMMON (DECL) || !TREE_PUBLIC (DECL)))		\
            || DECL_INITIAL (DECL)))					\
        machopic_define_symbol (DECL_RTL (DECL));			\
    if ((TREE_STATIC (DECL)						\
	 && (!DECL_COMMON (DECL) || !TREE_PUBLIC (DECL)))		\
        || DECL_INITIAL (DECL))						\
      (* targetm.encode_section_info) (DECL, DECL_RTL (DECL), false);	\
    ASM_OUTPUT_FUNCTION_LABEL (FILE, xname, DECL);			\
  } while (0)

#undef TARGET_ASM_DECLARE_CONSTANT_NAME
#define TARGET_ASM_DECLARE_CONSTANT_NAME darwin_asm_declare_constant_name

/* Wrap new method names in quotes so the assembler doesn't gag.
   Make Objective-C internal symbols local and in doing this, we need 
   to accommodate the name mangling done by c++ on file scope locals.  */

int darwin_label_is_anonymous_local_objc_name (const char *name);

#undef	ASM_OUTPUT_LABELREF
#define ASM_OUTPUT_LABELREF(FILE,NAME)					     \
  do {									     \
       const char *xname = (NAME);					     \
       if (! strcmp (xname, MACHOPIC_FUNCTION_BASE_NAME))		     \
         machopic_output_function_base_name(FILE);                           \
       else if (xname[0] == '&' || xname[0] == '*')			     \
         {								     \
           int len = strlen (xname);					     \
	   if (len > 6 && !strcmp ("$stub", xname + len - 5))		     \
	     machopic_validate_stub_or_non_lazy_ptr (xname);		     \
	   else if (len > 7 && !strcmp ("$stub\"", xname + len - 6))	     \
	     machopic_validate_stub_or_non_lazy_ptr (xname);		     \
	   else if (len > 14 && !strcmp ("$non_lazy_ptr", xname + len - 13)) \
	     machopic_validate_stub_or_non_lazy_ptr (xname);		     \
	   else if (len > 15 && !strcmp ("$non_lazy_ptr\"", xname + len - 14)) \
	     machopic_validate_stub_or_non_lazy_ptr (xname);		     \
	   if (xname[1] != '"' && name_needs_quotes (&xname[1]))	     \
	     fprintf (FILE, "\"%s\"", &xname[1]);			     \
	   else								     \
	     fputs (&xname[1], FILE); 					     \
	 }								     \
       else if (xname[0] == '+' || xname[0] == '-')			     \
         fprintf (FILE, "\"%s\"", xname);				     \
       else if (darwin_label_is_anonymous_local_objc_name (xname))	     \
         fprintf (FILE, "L%s", xname);					     \
       else if (xname[0] != '"' && name_needs_quotes (xname))		     \
	 asm_fprintf (FILE, "\"%U%s\"", xname);				     \
       else								     \
         asm_fprintf (FILE, "%U%s", xname);				     \
  } while (0)

/* Output before executable code.  */
#undef TEXT_SECTION_ASM_OP
#define TEXT_SECTION_ASM_OP "\t.text"

/* Output before writable data.  */

#undef DATA_SECTION_ASM_OP
#define DATA_SECTION_ASM_OP "\t.data"

#undef	ALIGN_ASM_OP
#define ALIGN_ASM_OP		".align"

#undef	ASM_OUTPUT_ALIGN
#define ASM_OUTPUT_ALIGN(FILE,LOG)	\
  if ((LOG) != 0)			\
    fprintf (FILE, "\t%s\t%d\n", ALIGN_ASM_OP, (LOG))

/* The maximum alignment which the object file format can support in
   bits.  For Mach-O, this is 2^15 bytes.  */

#undef	MAX_OFILE_ALIGNMENT
#define MAX_OFILE_ALIGNMENT (0x8000 * 8)

#define L2_MAX_OFILE_ALIGNMENT 15

/*  These are the three variants that emit referenced blank space.  */
#define ASM_OUTPUT_ALIGNED_BSS(FILE, DECL, NAME, SIZE, ALIGN)		\
	darwin_output_aligned_bss ((FILE), (DECL), (NAME), (SIZE), (ALIGN))

#undef	ASM_OUTPUT_ALIGNED_DECL_LOCAL
#define ASM_OUTPUT_ALIGNED_DECL_LOCAL(FILE, DECL, NAME, SIZE, ALIGN)	\
	darwin_asm_output_aligned_decl_local				\
				  ((FILE), (DECL), (NAME), (SIZE), (ALIGN))

#undef  ASM_OUTPUT_ALIGNED_DECL_COMMON
#define ASM_OUTPUT_ALIGNED_DECL_COMMON(FILE, DECL, NAME, SIZE, ALIGN)	\
	darwin_asm_output_aligned_decl_common				\
				   ((FILE), (DECL), (NAME), (SIZE), (ALIGN))

/* The generic version, archs should over-ride where required.  */
#define MACHOPIC_NL_SYMBOL_PTR_SECTION ".non_lazy_symbol_pointer"

/* Declare the section variables.  */
#ifndef USED_FOR_TARGET
enum darwin_section_enum {
#define DEF_SECTION(NAME, FLAGS, DIRECTIVE, OBJC) NAME,
#include "darwin-sections.def"
#undef DEF_SECTION
  NUM_DARWIN_SECTIONS
};
extern GTY(()) section * darwin_sections[NUM_DARWIN_SECTIONS];
#endif

#undef	TARGET_ASM_SELECT_SECTION
#define TARGET_ASM_SELECT_SECTION machopic_select_section

#undef	TARGET_ASM_FUNCTION_SECTION
#define TARGET_ASM_FUNCTION_SECTION darwin_function_section

#undef	TARGET_ASM_SELECT_RTX_SECTION
#define TARGET_ASM_SELECT_RTX_SECTION machopic_select_rtx_section
#undef  TARGET_ASM_UNIQUE_SECTION
#define TARGET_ASM_UNIQUE_SECTION darwin_unique_section
#undef  TARGET_ASM_FUNCTION_RODATA_SECTION
#define TARGET_ASM_FUNCTION_RODATA_SECTION default_no_function_rodata_section

#undef  TARGET_ASM_TM_CLONE_TABLE_SECTION
#define TARGET_ASM_TM_CLONE_TABLE_SECTION darwin_tm_clone_table_section

#undef  TARGET_ASM_RELOC_RW_MASK
#define TARGET_ASM_RELOC_RW_MASK machopic_reloc_rw_mask

/* Globalizing directive for a label.  */
#define GLOBAL_ASM_OP "\t.globl "
#define TARGET_ASM_GLOBALIZE_LABEL darwin_globalize_label

/* Emit an assembler directive to set visibility for a symbol.  Used
   to support visibility attribute and Darwin's private extern
   feature.  */
#undef TARGET_ASM_ASSEMBLE_VISIBILITY
#define TARGET_ASM_ASSEMBLE_VISIBILITY darwin_assemble_visibility

/* Extra attributes for Darwin.  */
#define SUBTARGET_ATTRIBUTE_TABLE					     \
  /* { name, min_len, max_len, decl_req, type_req, fn_type_req,		     \
       affects_type_identity, handler, exclude } */			     \
  { "apple_kext_compatibility", 0, 0, false, true, false, false,	     \
    darwin_handle_kext_attribute, NULL },				     \
  { "weak_import", 0, 0, true, false, false, false,			     \
    darwin_handle_weak_import_attribute, NULL }

/* Make local constant labels linker-visible, so that if one follows a
   weak_global constant, ld64 will be able to separate the atoms.  */
#undef ASM_GENERATE_INTERNAL_LABEL
#define ASM_GENERATE_INTERNAL_LABEL(LABEL,PREFIX,NUM)	\
  do {							\
    if (strcmp ("LC", PREFIX) == 0)			\
      sprintf (LABEL, "*%s%ld", "lC", (long)(NUM));	\
    else if (strcmp ("Lubsan_data", PREFIX) == 0)	\
      sprintf (LABEL, "*%s%ld", "lubsan_data", (long)(NUM));\
    else if (strcmp ("Lubsan_type", PREFIX) == 0)	\
      sprintf (LABEL, "*%s%ld", "lubsan_type", (long)(NUM));\
    else if (strcmp ("LASAN", PREFIX) == 0)	\
      sprintf (LABEL, "*%s%ld", "lASAN", (long)(NUM));\
    else						\
      sprintf (LABEL, "*%s%ld", PREFIX, (long)(NUM));	\
  } while (0)

#undef TARGET_ASM_MARK_DECL_PRESERVED
#define TARGET_ASM_MARK_DECL_PRESERVED darwin_mark_decl_preserved

/* Any port using this header needs to define the first available
   subtarget symbol bit: SYMBOL_FLAG_SUBT_DEP.  */

/* Is a variable. */
#define MACHO_SYMBOL_FLAG_VARIABLE (SYMBOL_FLAG_SUBT_DEP)
#define MACHO_SYMBOL_VARIABLE_P(RTX) \
  ((SYMBOL_REF_FLAGS (RTX) & MACHO_SYMBOL_FLAG_VARIABLE) != 0)

/* Set on a symbol that must be indirected, even when there is a
   definition in the TU.  The ABI mandates that common symbols are so
   indirected, as are weak.  If 'fix-and-continue' is operational then
   data symbols might also be.  */

#define MACHO_SYMBOL_FLAG_MUST_INDIRECT ((SYMBOL_FLAG_SUBT_DEP) << 1)
#define MACHO_SYMBOL_MUST_INDIRECT_P(RTX) \
  ((SYMBOL_REF_FLAGS (RTX) & MACHO_SYMBOL_FLAG_MUST_INDIRECT) != 0)

/* Set on a symbol with SYMBOL_FLAG_FUNCTION or MACHO_SYMBOL_FLAG_VARIABLE
   to indicate that the function or variable is considered defined in this
   translation unit.  */

#define MACHO_SYMBOL_FLAG_DEFINED ((SYMBOL_FLAG_SUBT_DEP) << 2)
#define MACHO_SYMBOL_DEFINED_P(RTX) \
  ((SYMBOL_REF_FLAGS (RTX) & MACHO_SYMBOL_FLAG_DEFINED) != 0)

/* Set on a symbol that has specified non-default visibility.  */

#define MACHO_SYMBOL_FLAG_HIDDEN_VIS ((SYMBOL_FLAG_SUBT_DEP) << 3)
#define MACHO_SYMBOL_HIDDEN_VIS_P(RTX) \
  ((SYMBOL_REF_FLAGS (RTX) & MACHO_SYMBOL_FLAG_HIDDEN_VIS) != 0)

/* Set on a symbol that should be made visible to the linker (overriding
   'L' symbol prefixes).  */

#define MACHO_SYMBOL_FLAG_LINKER_VIS ((SYMBOL_FLAG_SUBT_DEP) << 4)
#define MACHO_SYMBOL_LINKER_VIS_P(RTX) \
  ((SYMBOL_REF_FLAGS (RTX) & MACHO_SYMBOL_FLAG_LINKER_VIS) != 0)

/* Set on a symbol that is a pic stub or symbol indirection (i.e. the
   L_xxxxx${stub,non_lazy_ptr,lazy_ptr}.  */

#define MACHO_SYMBOL_FLAG_INDIRECTION ((SYMBOL_FLAG_SUBT_DEP) << 5)
#define MACHO_SYMBOL_INDIRECTION_P(RTX) \
  ((SYMBOL_REF_FLAGS (RTX) & MACHO_SYMBOL_FLAG_INDIRECTION) != 0)

/* Set on a symbol to indicate when fix-and-continue style code
   generation is being used and the symbol refers to a static symbol
   that should be rebound from new instances of a translation unit to
   the original instance of the data.  */

#define MACHO_SYMBOL_FLAG_STATIC ((SYMBOL_FLAG_SUBT_DEP) << 6)
#define MACHO_SYMBOL_STATIC_P(RTX) \
  ((SYMBOL_REF_FLAGS (RTX) & MACHO_SYMBOL_FLAG_STATIC) != 0)

/* Symbolic names for various things we might know about a symbol.  */

enum machopic_addr_class {
  MACHOPIC_UNDEFINED,
  MACHOPIC_DEFINED_DATA,
  MACHOPIC_UNDEFINED_DATA,
  MACHOPIC_DEFINED_FUNCTION,
  MACHOPIC_UNDEFINED_FUNCTION
};

/* Macros defining the various PIC cases.  */

#undef  MACHO_DYNAMIC_NO_PIC_P
#define MACHO_DYNAMIC_NO_PIC_P	(TARGET_MACHO_DYNAMIC_NO_PIC)
#undef  MACHOPIC_INDIRECT
#define MACHOPIC_INDIRECT	(flag_pic || MACHO_DYNAMIC_NO_PIC_P)
#define MACHOPIC_JUST_INDIRECT	(MACHO_DYNAMIC_NO_PIC_P)
#undef  MACHOPIC_PURE
#define MACHOPIC_PURE		(flag_pic && ! MACHO_DYNAMIC_NO_PIC_P)

#undef TARGET_ENCODE_SECTION_INFO
#define TARGET_ENCODE_SECTION_INFO  darwin_encode_section_info
#undef TARGET_STRIP_NAME_ENCODING
#define TARGET_STRIP_NAME_ENCODING  default_strip_name_encoding

#define GEN_BINDER_NAME_FOR_STUB(BUF,STUB,STUB_LENGTH)		\
  do {								\
    const char *const stub_ = (STUB);				\
    char *buffer_ = (BUF);					\
    strcpy (buffer_, stub_);					\
    if (stub_[0] == '"')					\
      {								\
	strcpy (buffer_ + (STUB_LENGTH) - 1, "_binder\"");	\
      }								\
    else							\
      {								\
	strcpy (buffer_ + (STUB_LENGTH), "_binder");		\
      }								\
  } while (0)

#define GEN_SYMBOL_NAME_FOR_SYMBOL(BUF,SYMBOL,SYMBOL_LENGTH)	\
  do {								\
    const char *const symbol_ = (SYMBOL);			\
    char *buffer_ = (BUF);					\
    if (name_needs_quotes (symbol_) && symbol_[0] != '"')	\
      {								\
	  sprintf (buffer_, "\"%s\"", symbol_);			\
      }								\
    else							\
      {								\
	strcpy (buffer_, symbol_);				\
      }								\
  } while (0)

/* Given a symbol name string, create the lazy pointer version
   of the symbol name.  */

#define GEN_LAZY_PTR_NAME_FOR_SYMBOL(BUF,SYMBOL,SYMBOL_LENGTH)	\
  do {								\
    const char *symbol_ = (SYMBOL);                             \
    char *buffer_ = (BUF);					\
    if (symbol_[0] == '"')					\
      {								\
        strcpy (buffer_, "\"L");				\
        strcpy (buffer_ + 2, symbol_ + 1);			\
	strcpy (buffer_ + (SYMBOL_LENGTH), "$lazy_ptr\"");	\
      }								\
    else if (name_needs_quotes (symbol_))			\
      {								\
        strcpy (buffer_, "\"L");				\
        strcpy (buffer_ + 2, symbol_);				\
	strcpy (buffer_ + (SYMBOL_LENGTH) + 2, "$lazy_ptr\"");	\
      }								\
    else							\
      {								\
        strcpy (buffer_, "L");					\
        strcpy (buffer_ + 1, symbol_);				\
	strcpy (buffer_ + (SYMBOL_LENGTH) + 1, "$lazy_ptr");	\
      }								\
  } while (0)

#define EH_FRAME_SECTION_NAME   "__TEXT"
#define EH_FRAME_SECTION_ATTR ",coalesced,no_toc+strip_static_syms+live_support"

#undef ASM_PREFERRED_EH_DATA_FORMAT
#define ASM_PREFERRED_EH_DATA_FORMAT(CODE,GLOBAL)  \
  (((CODE) == 2 && (GLOBAL) == 1) \
   ? (DW_EH_PE_pcrel | DW_EH_PE_indirect | DW_EH_PE_sdata4) : \
     ((CODE) == 1 || (GLOBAL) == 0) ? DW_EH_PE_pcrel : DW_EH_PE_absptr)

#define ASM_OUTPUT_DWARF_DELTA(FILE,SIZE,LABEL1,LABEL2)  \
  darwin_asm_output_dwarf_delta (FILE, SIZE, LABEL1, LABEL2, 0)

#define ASM_OUTPUT_DWARF_OFFSET(FILE,SIZE,LABEL,OFFSET,BASE)  \
  darwin_asm_output_dwarf_offset (FILE, SIZE, LABEL, OFFSET, BASE)

#define ASM_MAYBE_OUTPUT_ENCODED_ADDR_RTX(ASM_OUT_FILE, ENCODING, SIZE, ADDR, DONE)	\
      if (ENCODING == ASM_PREFERRED_EH_DATA_FORMAT (2, 1)) {				\
	darwin_non_lazy_pcrel (ASM_OUT_FILE, ADDR);					\
	goto DONE;									\
      }

/* Experimentally, putting jump tables in text is faster on SPEC.
   Also this is needed for correctness for coalesced functions.  */

#ifndef JUMP_TABLES_IN_TEXT_SECTION
#define JUMP_TABLES_IN_TEXT_SECTION 1
#endif

#define TARGET_TERMINATE_DW2_EH_FRAME_INFO false

#define TARGET_ASM_INIT_SECTIONS darwin_init_sections
#undef TARGET_ASM_NAMED_SECTION
#define TARGET_ASM_NAMED_SECTION darwin_asm_named_section

#define DARWIN_REGISTER_TARGET_PRAGMAS()			\
  do {								\
    if (!flag_preprocess_only)					\
      cpp_register_pragma (parse_in, NULL, "mark",		\
			   darwin_pragma_ignore, false);	\
    c_register_pragma (0, "options", darwin_pragma_options);	\
    c_register_pragma (0, "segment", darwin_pragma_ignore);	\
    c_register_pragma (0, "unused", darwin_pragma_unused);	\
    c_register_pragma (0, "ms_struct", darwin_pragma_ms_struct); \
  } while (0)

#undef ASM_APP_ON
#define ASM_APP_ON ""
#undef ASM_APP_OFF
#define ASM_APP_OFF ""

void darwin_register_frameworks (const char *, const char *, int);
void darwin_register_objc_includes (const char *, const char *, int);
#define TARGET_EXTRA_PRE_INCLUDES darwin_register_objc_includes
#define TARGET_EXTRA_INCLUDES darwin_register_frameworks

void add_framework_path (char *);
#define TARGET_OPTF add_framework_path

#define TARGET_POSIX_IO

#define WINT_TYPE "int"

/* Every program on darwin links against libSystem which contains the pthread
   routines, so there's no need to explicitly call out when doing threaded
   work.  */

#undef GOMP_SELF_SPECS
#define GOMP_SELF_SPECS ""
#undef GTM_SELF_SPECS
#define GTM_SELF_SPECS ""

/* Darwin disables section anchors by default.  
   They should be enabled per arch where support exists in that arch.  */
#define TARGET_ASM_OUTPUT_ANCHOR NULL
#define DARWIN_SECTION_ANCHORS 0

#define HAVE_ENABLE_EXECUTE_STACK

/* For Apple KEXTs, we make the constructors return this to match gcc
   2.95.  */
#define TARGET_CXX_CDTOR_RETURNS_THIS (darwin_kextabi_p)
#define TARGET_KEXTABI flag_apple_kext

/* We have target-specific builtins.  */
#define SUBTARGET_FOLD_BUILTIN darwin_fold_builtin

#define TARGET_N_FORMAT_TYPES 1
#define TARGET_FORMAT_TYPES darwin_additional_format_types

#ifndef USED_FOR_TARGET
extern void darwin_driver_init (unsigned int *,struct cl_decoded_option **);
#define GCC_DRIVER_HOST_INITIALIZATION \
  darwin_driver_init (&decoded_options_count, &decoded_options)
#endif

/* The Apple assembler and linker do not support constructor priorities.  */
#undef SUPPORTS_INIT_PRIORITY
#define SUPPORTS_INIT_PRIORITY 0

#undef STACK_CHECK_STATIC_BUILTIN
#define STACK_CHECK_STATIC_BUILTIN 1

/* When building cross-compilers (and native crosses) we shall default to 
   providing an osx-version-min of this unless overridden by the User.
   10.5 is the only version that fully supports all our archs so that's the
   fall-back default.  */
#ifndef DEF_MIN_OSX_VERSION
#define DEF_MIN_OSX_VERSION "10.5"
#endif

/* Later versions of ld64 support coalescing weak code/data without requiring
   that they be placed in specially identified sections.  This is the earliest
   _tested_ version known to support this so far.  */
#define MIN_LD64_NO_COAL_SECTS "236.3"

/* From at least version 62.1, ld64 can build symbol indirection stubs as
   needed, and there is no need for the compiler to emit them.  */
#define MIN_LD64_OMIT_STUBS "62.1"

/* Emit start labels for init and term sections from this version.  */
#define MIN_LD64_INIT_TERM_START_LABELS "136.0"

/* If we have no definition for the linker version, pick the minimum version
   that will bootstrap the compiler.  */
#ifndef LD64_VERSION
# ifndef  DEF_LD64
#  define LD64_VERSION "85.2.1"
# else
#  define LD64_VERSION DEF_LD64
# endif
#endif

/* CTF and BTF support.  */
#undef CTF_INFO_SECTION_NAME
#define CTF_INFO_SECTION_NAME "__CTF_BTF,__ctf,regular,debug"
#undef BTF_INFO_SECTION_NAME
#define BTF_INFO_SECTION_NAME "__CTF_BTF,__btf,regular,debug"

#endif /* CONFIG_DARWIN_H */
