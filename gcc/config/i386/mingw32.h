/* Operating system specific defines to be used when targeting GCC for
   hosting on Windows32, using GNU tools and the Windows32 API Library.
   Copyright (C) 1997-2022 Free Software Foundation, Inc.

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

#undef DEFAULT_ABI
#define DEFAULT_ABI MS_ABI

/* By default, target has a 80387, uses IEEE compatible arithmetic,
   returns float values in the 387 and needs stack probes.
   We also align doubles to 64-bits for MSVC default compatibility.
   Additionally we enable MS_BITFIELD_LAYOUT by default.  */

#undef TARGET_SUBTARGET_DEFAULT
#define TARGET_SUBTARGET_DEFAULT \
	(MASK_80387 | MASK_IEEE_FP | MASK_FLOAT_RETURNS \
	 | MASK_STACK_PROBE | MASK_ALIGN_DOUBLE \
	 | MASK_MS_BITFIELD_LAYOUT)

#ifndef TARGET_USING_MCFGTHREAD
#define TARGET_USING_MCFGTHREAD  0
#endif

/* See i386/crtdll.h for an alternative definition. _INTEGRAL_MAX_BITS
   is for compatibility with native compiler.  */
#define EXTRA_OS_CPP_BUILTINS()					\
  do								\
    {								\
      builtin_define ("__MSVCRT__");				\
      builtin_define ("__MINGW32__");			   	\
      builtin_define ("_WIN32");				\
      builtin_define_std ("WIN32");				\
      builtin_define_std ("WINNT");				\
      builtin_define_with_int_value ("_INTEGRAL_MAX_BITS",	\
				     TYPE_PRECISION (intmax_type_node));\
      if (TARGET_64BIT && ix86_abi == MS_ABI)			\
	{							\
	  builtin_define ("__MINGW64__");			\
	  builtin_define_std ("WIN64");				\
	  builtin_define ("_WIN64");				\
	}							\
      if (TARGET_USING_MCFGTHREAD)				\
	builtin_define ("__USING_MCFGTHREAD__");		\
    }								\
  while (0)

#ifndef TARGET_USE_PTHREAD_BY_DEFAULT
#define SPEC_PTHREAD1 "pthread"
#define SPEC_PTHREAD2 "!no-pthread"
#else
#define SPEC_PTHREAD1 "!no-pthread"
#define SPEC_PTHREAD2 "pthread"
#endif

#undef SUB_LINK_ENTRY32
#undef SUB_LINK_ENTRY64
#define SUB_LINK_ENTRY32 "-e _DllMainCRTStartup@12"
#if defined(USE_MINGW64_LEADING_UNDERSCORES)
#define SUB_LINK_ENTRY64 "-e _DllMainCRTStartup"
#else
#define SUB_LINK_ENTRY64 "-e DllMainCRTStartup"
#endif

#undef SUB_LINK_ENTRY
#if TARGET_64BIT_DEFAULT
#define SUB_LINK_ENTRY SUB_LINK_ENTRY64
#else
#define SUB_LINK_ENTRY SUB_LINK_ENTRY32
#endif

#undef NATIVE_SYSTEM_HEADER_COMPONENT
#define NATIVE_SYSTEM_HEADER_COMPONENT "MINGW"

#undef CPP_SPEC
#define CPP_SPEC "%{posix:-D_POSIX_SOURCE} %{mthreads:-D_MT} " \
		 "%{" SPEC_PTHREAD1 ":-D_REENTRANT} " \
		 "%{" SPEC_PTHREAD2 ": } "

/* For Windows applications, include more libraries, but always include
   kernel32.  */
#undef LIB_SPEC
#define LIB_SPEC "%{pg:-lgmon} %{" SPEC_PTHREAD1 ":-lpthread} " \
		 "%{" SPEC_PTHREAD2 ": } " \
		 "%{mwindows:-lgdi32 -lcomdlg32} " \
     "%{fvtable-verify=preinit:-lvtv -lpsapi; \
        fvtable-verify=std:-lvtv -lpsapi} " \
                 "-ladvapi32 -lshell32 -luser32 -lkernel32"

/* Weak symbols do not get resolved if using a Windows dll import lib.
   Make the unwind registration references strong undefs.  */
#if DWARF2_UNWIND_INFO
/* DW2-unwind is just available for 32-bit mode.  */
#if TARGET_64BIT_DEFAULT
#define SHARED_LIBGCC_UNDEFS_SPEC \
  "%{m32: %{shared-libgcc: -u ___register_frame_info -u ___deregister_frame_info}}"
#else
#define SHARED_LIBGCC_UNDEFS_SPEC \
 "%{shared-libgcc: -u ___register_frame_info -u ___deregister_frame_info}"
#endif
#else
#define SHARED_LIBGCC_UNDEFS_SPEC ""
#endif

#undef  SUBTARGET_EXTRA_SPECS
#define SUBTARGET_EXTRA_SPECS						\
  { "shared_libgcc_undefs", SHARED_LIBGCC_UNDEFS_SPEC }

#if ! MINGW_DEFAULT_LARGE_ADDR_AWARE
/* This is used without --enable-large-address-aware.  */
# define LINK_SPEC_LARGE_ADDR_AWARE ""
#elif ! TARGET_BI_ARCH
/* This is used on i686-pc-mingw32 with --enable-large-address-aware.  */
# define LINK_SPEC_LARGE_ADDR_AWARE \
  "%{!shared:%{!mdll:--large-address-aware}}"
#elif TARGET_64BIT_DEFAULT
/* This is used on x86_64-pc-mingw32 with --enable-large-address-aware.
   ??? It probably doesn't work, because the linker emulation defaults
   to i386pep, the 64-bit mode that does not support
   --large-address-aware, and x86_64-pc-mingw32 does not override the
   emulation to i386pe for -m32, unlike x86_64-w64-mingw32.  */
# define LINK_SPEC_LARGE_ADDR_AWARE \
  "%{!shared:%{!mdll:%{m32:--large-address-aware}}}"
#else
/* This would only be used if someone introduced a biarch
   configuration that defaulted to 32-bit.  */
# define LINK_SPEC_LARGE_ADDR_AWARE \
  "%{!shared:%{!mdll:%{!m64:--large-address-aware}}}"
#endif

#if HAVE_LD_PE_DISABLE_DYNAMICBASE
# define LINK_SPEC_DISABLE_DYNAMICBASE \
  "%{!shared:%{!mdll:%{no-pie:--disable-dynamicbase}}}"
#else
# define LINK_SPEC_DISABLE_DYNAMICBASE ""
#endif

#define LINK_SPEC "%{mwindows:--subsystem windows} \
  %{mconsole:--subsystem console} \
  %{shared: %{mdll: %eshared and mdll are not compatible}} \
  %{shared: --shared} %{mdll:--dll} \
  %{static:-Bstatic} %{!static:-Bdynamic} \
  %{shared|mdll: " SUB_LINK_ENTRY " --enable-auto-image-base} \
  " LINK_SPEC_LARGE_ADDR_AWARE "\
  " LINK_SPEC_DISABLE_DYNAMICBASE "\
  %(shared_libgcc_undefs)"

/* Include in the mingw32 libraries with libgcc */
#ifdef ENABLE_SHARED_LIBGCC
#define SHARED_LIBGCC_SPEC " \
 %{static|static-libgcc:-lgcc -lgcc_eh} \
 %{!static: \
   %{!static-libgcc: \
     %{!shared: \
       %{!shared-libgcc:-lgcc -lgcc_eh} \
       %{shared-libgcc:-lgcc_s -lgcc} \
      } \
     %{shared:-lgcc_s -lgcc} \
    } \
  } "
#else
#define SHARED_LIBGCC_SPEC " -lgcc "
#endif
#if TARGET_USING_MCFGTHREAD
#define MCFGTHREAD_SPEC  " -lmcfgthread -lkernel32 -lntdll "
#else
#define MCFGTHREAD_SPEC  ""
#endif
#undef REAL_LIBGCC_SPEC
#define REAL_LIBGCC_SPEC \
  "%{mthreads:-lmingwthrd} -lmingw32 \
   " SHARED_LIBGCC_SPEC " \
   -lmoldname -lmingwex -lmsvcrt -lkernel32 " MCFGTHREAD_SPEC

#undef STARTFILE_SPEC
#define STARTFILE_SPEC "%{shared|mdll:dllcrt2%O%s} \
  %{!shared:%{!mdll:crt2%O%s}} %{pg:gcrt2%O%s} \
  crtbegin.o%s \
  %{fvtable-verify=none:%s; \
    fvtable-verify=preinit:vtv_start.o%s; \
    fvtable-verify=std:vtv_start.o%s}"

#undef ENDFILE_SPEC
#define ENDFILE_SPEC \
  "%{Ofast|ffast-math|funsafe-math-optimizations:crtfastmath.o%s} \
   %{!shared:%:if-exists(default-manifest.o%s)}\
   %{fvtable-verify=none:%s; \
    fvtable-verify=preinit:vtv_end.o%s; \
    fvtable-verify=std:vtv_end.o%s} \
  crtend.o%s"

/* Override startfile prefix defaults.  */
#ifndef STANDARD_STARTFILE_PREFIX_1
#define STANDARD_STARTFILE_PREFIX_1 "/mingw/lib/"
#endif
#ifndef STANDARD_STARTFILE_PREFIX_2
#define STANDARD_STARTFILE_PREFIX_2 ""
#endif

/* For native mingw-version we need to take care that NATIVE_SYSTEM_HEADER_DIR
   macro contains POSIX-style path.  See bug 52947.  */
#undef NATIVE_SYSTEM_HEADER_DIR
#define NATIVE_SYSTEM_HEADER_DIR "/mingw/include"

/* Output STRING, a string representing a filename, to FILE.
   We canonicalize it to be in Unix format (backslashes are replaced
   forward slashes.  */
#undef OUTPUT_QUOTED_STRING
#define OUTPUT_QUOTED_STRING(FILE, STRING)               \
do {						         \
  const char *_string = (const char *) (STRING);	 \
  char c;					         \
						         \
  putc ('\"', (FILE));				         \
						         \
  while ((c = *_string++) != 0)			         \
    {						         \
      if (c == '\\')				         \
	c = '/';				         \
						         \
      if (ISPRINT (c))                                   \
        {                                                \
          if (c == '\"')			         \
	    putc ('\\', (FILE));		         \
          putc (c, (FILE));			         \
        }                                                \
      else                                               \
        fprintf ((FILE), "\\%03o", (unsigned char) c);	 \
    }						         \
						         \
  putc ('\"', (FILE));					 \
} while (0)

/* Define as short unsigned for compatibility with MS runtime.  */
#undef WINT_TYPE
#define WINT_TYPE "short unsigned int"

/* mingw32 uses the  -mthreads option to enable thread support.  */
#undef GOMP_SELF_SPECS
#define GOMP_SELF_SPECS "%{fopenacc|fopenmp|%:gt(%{ftree-parallelize-loops=*:%*} 1): " \
			"-mthreads -pthread}"
#undef GTM_SELF_SPECS
#define GTM_SELF_SPECS "%{fgnu-tm:-mthreads -pthread}"

/* mingw32 atexit function is safe to use in shared libraries.  Use it
   to register C++ static destructors.  */
#define TARGET_CXX_USE_ATEXIT_FOR_CXA_ATEXIT hook_bool_void_true

/* Contains a pointer to type target_ovr_attr defining the target specific
   overrides of format attributes.  See c-format.h for structure
   definition.  */
#undef TARGET_OVERRIDES_FORMAT_ATTRIBUTES
#define TARGET_OVERRIDES_FORMAT_ATTRIBUTES mingw_format_attribute_overrides

/* Specify the count of elements in TARGET_OVERRIDES_ATTRIBUTE.  */
#undef TARGET_OVERRIDES_FORMAT_ATTRIBUTES_COUNT
#define TARGET_OVERRIDES_FORMAT_ATTRIBUTES_COUNT 3

/* Custom initialization for warning -Wpedantic-ms-format for c-format.  */
#undef TARGET_OVERRIDES_FORMAT_INIT
#define TARGET_OVERRIDES_FORMAT_INIT msformat_init

/* MS specific format attributes for ms_printf, ms_scanf, ms_strftime.  */
#undef TARGET_FORMAT_TYPES
#define TARGET_FORMAT_TYPES mingw_format_attributes

#undef TARGET_N_FORMAT_TYPES
#define TARGET_N_FORMAT_TYPES 3

#define HAVE_ENABLE_EXECUTE_STACK
#undef  CHECK_EXECUTE_STACK_ENABLED
#define CHECK_EXECUTE_STACK_ENABLED flag_setstackexecutable

/* This matches SHLIB_SONAME and SHLIB_SOVERSION in t-cygming. */
/* This matches SHLIB_SONAME and SHLIB_SOVERSION in t-cygwin. */
#if DWARF2_UNWIND_INFO
#define LIBGCC_EH_EXTN "_dw2"
#else
#define LIBGCC_EH_EXTN "_sjlj"
#endif
#define LIBGCC_SONAME "libgcc_s" LIBGCC_EH_EXTN "-1.dll"

