/* Operating system specific defines to be used when targeting GCC for
   hosting on Windows32, using GNU tools and the Windows32 API Library.
   Copyright (C) 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2007, 2008,
   2009, 2010 Free Software Foundation, Inc.

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

#undef TARGET_VERSION
#if TARGET_64BIT_DEFAULT
#define TARGET_VERSION fprintf (stderr,"(x86_64 MinGW");
#else
#define TARGET_VERSION fprintf (stderr," (x86 MinGW)");
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
    }								\
  while (0)

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

/* Override the standard choice of /usr/include as the default prefix
   to try when searching for header files.  */
#undef STANDARD_INCLUDE_DIR
#define STANDARD_INCLUDE_DIR "/mingw/include"
#undef STANDARD_INCLUDE_COMPONENT
#define STANDARD_INCLUDE_COMPONENT "MINGW"

#undef CPP_SPEC
#define CPP_SPEC "%{posix:-D_POSIX_SOURCE} %{mthreads:-D_MT}"

/* For Windows applications, include more libraries, but always include
   kernel32.  */
#undef LIB_SPEC
#define LIB_SPEC "%{pg:-lgmon} %{mwindows:-lgdi32 -lcomdlg32} \
                  -ladvapi32 -lshell32 -luser32 -lkernel32"

/* Weak symbols do not get resolved if using a Windows dll import lib.
   Make the unwind registration references strong undefs.  */
#if DWARF2_UNWIND_INFO
/* DW2-unwind is just available for 32-bit mode.  */
#if TARGET_64BIT_DEFAULT
#error DW2 unwind is not available for 64-bit.
#endif
#define SHARED_LIBGCC_UNDEFS_SPEC \
 "%{shared-libgcc: -u ___register_frame_info -u ___deregister_frame_info}"
#else
#define SHARED_LIBGCC_UNDEFS_SPEC ""
#endif

#undef  SUBTARGET_EXTRA_SPECS
#define SUBTARGET_EXTRA_SPECS						\
  { "shared_libgcc_undefs", SHARED_LIBGCC_UNDEFS_SPEC }

#define LINK_SPEC "%{mwindows:--subsystem windows} \
  %{mconsole:--subsystem console} \
  %{shared: %{mdll: %eshared and mdll are not compatible}} \
  %{shared: --shared} %{mdll:--dll} \
  %{static:-Bstatic} %{!static:-Bdynamic} \
  %{shared|mdll: " SUB_LINK_ENTRY " --enable-auto-image-base} \
  %(shared_libgcc_undefs)"

/* Include in the mingw32 libraries with libgcc */
#ifdef ENABLE_SHARED_LIBGCC
#define SHARED_LIBGCC_SPEC "%{shared-libgcc:-lgcc_s} %{!shared-libgcc:-lgcc_eh}"
#else
#define SHARED_LIBGCC_SPEC /*empty*/
#endif
#undef REAL_LIBGCC_SPEC
#define REAL_LIBGCC_SPEC \
  "%{mthreads:-lmingwthrd} -lmingw32 \
   "SHARED_LIBGCC_SPEC" \
   -lgcc \
   -lmoldname -lmingwex -lmsvcrt"

#undef STARTFILE_SPEC
#define STARTFILE_SPEC "%{shared|mdll:dllcrt2%O%s} \
  %{!shared:%{!mdll:crt2%O%s}} %{pg:gcrt2%O%s} \
  crtbegin.o%s"

#undef ENDFILE_SPEC
#define ENDFILE_SPEC \
  "%{Ofast|ffast-math|funsafe-math-optimizations:crtfastmath.o%s} \
  crtend.o%s"

/* Override startfile prefix defaults.  */
#ifndef STANDARD_STARTFILE_PREFIX_1
#define STANDARD_STARTFILE_PREFIX_1 "/mingw/lib/"
#endif
#ifndef STANDARD_STARTFILE_PREFIX_2
#define STANDARD_STARTFILE_PREFIX_2 ""
#endif

/* Output STRING, a string representing a filename, to FILE.
   We canonicalize it to be in Unix format (backslashes are replaced
   forward slashes.  */
#undef OUTPUT_QUOTED_STRING
#define OUTPUT_QUOTED_STRING(FILE, STRING)               \
do {						         \
  char c;					         \
						         \
  putc ('\"', asm_file);			         \
						         \
  while ((c = *string++) != 0)			         \
    {						         \
      if (c == '\\')				         \
	c = '/';				         \
						         \
      if (ISPRINT (c))                                   \
        {                                                \
          if (c == '\"')			         \
	    putc ('\\', asm_file);		         \
          putc (c, asm_file);			         \
        }                                                \
      else                                               \
        fprintf (asm_file, "\\%03o", (unsigned char) c); \
    }						         \
						         \
  putc ('\"', asm_file);			         \
} while (0)

/* Define as short unsigned for compatibility with MS runtime.  */
#undef WINT_TYPE
#define WINT_TYPE "short unsigned int"

/* mingw32 uses the  -mthreads option to enable thread support.  */
#undef GOMP_SELF_SPECS
#define GOMP_SELF_SPECS "%{fopenmp: -mthreads}"

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

/* Let defaults.h definition of TARGET_USE_JCR_SECTION apply. */
#undef TARGET_USE_JCR_SECTION

#undef MINGW_ENABLE_EXECUTE_STACK
#define MINGW_ENABLE_EXECUTE_STACK     \
extern void __enable_execute_stack (void *);    \
void         \
__enable_execute_stack (void *addr)					\
{									\
  MEMORY_BASIC_INFORMATION b;						\
  if (!VirtualQuery (addr, &b, sizeof(b)))				\
    abort ();								\
  VirtualProtect (b.BaseAddress, b.RegionSize, PAGE_EXECUTE_READWRITE,	\
		  &b.Protect);						\
}

#undef ENABLE_EXECUTE_STACK
#define ENABLE_EXECUTE_STACK MINGW_ENABLE_EXECUTE_STACK
#undef  CHECK_EXECUTE_STACK_ENABLED
#define CHECK_EXECUTE_STACK_ENABLED flag_setstackexecutable

#ifdef IN_LIBGCC2
#include <windows.h>
#endif

/* For 64-bit Windows we can't use DW2 unwind info. Also for multilib
   builds we can't use it, too.  */
#if !TARGET_64BIT_DEFAULT && !defined (TARGET_BI_ARCH)
#define MD_UNWIND_SUPPORT "config/i386/w32-unwind.h"
#endif

/* This matches SHLIB_SONAME and SHLIB_SOVERSION in t-cygming. */
/* This matches SHLIB_SONAME and SHLIB_SOVERSION in t-cygwin. */
#if DWARF2_UNWIND_INFO
#define LIBGCC_EH_EXTN "_dw2"
#else
#define LIBGCC_EH_EXTN "_sjlj"
#endif
#define LIBGCC_SONAME "libgcc_s" LIBGCC_EH_EXTN "-1.dll"

/* We should find a way to not have to update this manually.  */
#define LIBGCJ_SONAME "libgcj" /*LIBGCC_EH_EXTN*/ "-12.dll"

