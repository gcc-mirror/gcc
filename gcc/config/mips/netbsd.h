/* Definitions of target machine for GNU compiler, for MIPS NetBSD systems.
   Copyright (C) 1993, 1995, 1996, 1997, 1999, 2000, 2001, 2002
   Free Software Foundation, Inc.

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


/* Define default target values. */

#ifndef TARGET_ENDIAN_DEFAULT
#define TARGET_ENDIAN_DEFAULT MASK_BIG_ENDIAN
#endif

#ifndef MACHINE_TYPE
#if TARGET_ENDIAN_DEFAULT != 0
#define MACHINE_TYPE "NetBSD/mipseb ELF"
#else
#define MACHINE_TYPE "NetBSD/mipsel ELF"
#endif
#endif

#define TARGET_DEFAULT (MASK_GAS|MASK_ABICALLS)


/* XXX Don't use DWARF-2 debugging info, for now.  */
#undef DBX_DEBUGGING_INFO
#define DBX_DEBUGGING_INFO
#undef PREFERRED_DEBUGGING_TYPE
#define PREFERRED_DEBUGGING_TYPE DBX_DEBUG


/* Include the generic MIPS ELF configuration.  */
#include <mips/elf.h>

/* Now clean up after it.  */
#undef OBJECT_FORMAT_COFF
#undef MD_EXEC_PREFIX
#undef MD_STARTFILE_PREFIX
#undef US_SOFTWARE_GOFAST
#undef INIT_SUBTARGET_OPTABS
#define INIT_SUBTARGET_OPTABS


/* Get generic NetBSD definitions.  */
#include <netbsd.h>


/* Get generic NetBSD ELF definitions.  */
#include <netbsd-elf.h>


/* Provide CPP predefines appropriate for NetBSD.  We default to
   MIPS-I.  */

#undef CPP_PREDEFINES
#if TARGET_ENDIAN_DEFAULT != 0 
#define CPP_PREDEFINES							\
  "-D__NetBSD__ -D__ELF__ -D__mips__ -D__mips=1 -D__MIPSEB__		\
   -D__NO_LEADING_UNDERSCORES__	-D__GP_SUPPORT__			\
   -Asystem=unix -Asystem=NetBSD -Amachine=mips"
#else
#define CPP_PREDEFINES							\
  "-D__NetBSD__ -D__ELF__ -D__mips__ -D__mips=1 -D__MIPSEL__		\
   -D__NO_LEADING_UNDERSCORES__	-D__GP_SUPPORT__			\
   -Asystem=unix -Asystem=NetBSD -Amachine=mips"
#endif


/* Provide a CPP_SPEC appropriate for NetBSD.  This is a simplified
   CPP_SPEC from <mips/mips.h>.  We use the SUBTARGET_CPP_SPEC to
   deal with NetBSD-specific CPP options.  */

#undef CPP_SPEC
#define CPP_SPEC							\
  "%(subtarget_cpp_size_spec)						\
   %{mips3:-U__mips -D__mips=3 -D__mips64}				\
   %{mips4:-U__mips -D__mips=4 -D__mips64}				\
   %{mips32:-U__mips -D__mips=32}					\
   %{mips64:-U__mips -D__mips=64 -D__mips64}				\
   %{mgp32:-U__mips64} %{mgp64:-D__mips64}				\
   %{mfp32:-D__mips_fpr=32} %{mfp64:-D__mips_fpr=64}			\
   %{!mfp32:								\
     %{!mfp64:								\
       %{mgp32:-D__mips_fpr=32}						\
       %{!mgp32: %(cpp_fpr_spec)}}}					\
   %{msingle-float:							\
     %{!msoft-float:-D__mips_single_float}}				\
   %{m4650:								\
     %{!msoft-float:-D__mips_single_float}}				\
   %{msoft-float:-D__mips_soft_float}					\
   %{mabi=eabi:-D__mips_eabi}						\
   %{mips16:%{!mno-mips16:-D__mips16}}					\
   %{EB:-U__MIPSEL__ -D__MIPSEB__}					\
   %{EL:-U__MIPSEB__ -D__MIPSEL__}					\
   %(subtarget_cpp_spec) "


/* Provide a SUBTARGET_CPP_SIZE_SPEC appropriate for NetBSD.  In
   addition to the normal work done by this spec, we also define
   __LONG64 or not (so that <machine/ansi.h> can tell).  */

#undef SUBTARGET_CPP_SIZE_SPEC
#define SUBTARGET_CPP_SIZE_SPEC						\
  "%{mlong64:								\
     %{!mips1:								\
       %{!mips2:							\
	 %{!mips32:-D__SIZE_TYPE__=long\\ unsigned\\ int -D__PTRDIFF_TYPE__=long\\ int -D__LONG64}}}} \
   %{!mlong64:-D__SIZE_TYPE__=unsigned\\ int -D__PTRDIFF_TYPE__=int -U__LONG64}"


/* Provide a SUBTARGET_CPP_SPEC appropriate for NetBSD.  Currently,
   we just deal with the GCC option '-posix'.  */

#undef SUBTARGET_CPP_SPEC
#define SUBTARGET_CPP_SPEC "%{posix:-D_POSIX_SOURCE}"


/* Provide a LINK_SPEC appropriate for a NetBSD/mips target.
   This is a copy of LINK_SPEC from <netbsd-elf.h> tweaked for
   the MIPS target.  */

#undef LINK_SPEC
#define LINK_SPEC							\
  "%{assert*} %{R*} %{rpath*}						\
   %{EL:-m elf32lmip}							\
   %{EB:-m elf32bmip}							\
   %(endian_spec)							\
   %{G*} %{mips1} %{mips2} %{mips3} %{mips4} %{mips32} %{mips64}	\
   %{bestGnum} %{call_shared} %{no_archive} %{exact_version}		\
   %{shared:-shared}							\
   %{!shared:								\
     -dc -dp								\
     %{!nostdlib:							\
       %{!r*:								\
	 %{!e*:-e __start}}}						\
     %{!static:								\
       %{rdynamic:-export-dynamic}					\
       %{!dynamic-linker:-dynamic-linker /usr/libexec/ld.elf_so}}	\
     %{static:-static}}"


#undef SUBTARGET_ASM_SPEC
#define SUBTARGET_ASM_SPEC						\
  "%{fpic:-KPIC} %{fPIC:-KPIC}"


/* -G is incompatible with -KPIC which is the default, so only allow objects
   in the small data section if the user explicitly asks for it.  */

#undef MIPS_DEFAULT_GVALUE 
#define MIPS_DEFAULT_GVALUE 0 


/* This defines which switch letters take arguments.  -G is a MIPS
   special.  */

#undef SWITCH_TAKES_ARG
#define SWITCH_TAKES_ARG(CHAR)						\
  (DEFAULT_SWITCH_TAKES_ARG (CHAR)					\
   || (CHAR) == 'R'							\
   || (CHAR) == 'G')


#undef ASM_FINAL_SPEC
#undef SET_ASM_OP


/* NetBSD hasn't historically provided _flush_cache(), but rather
   _cacheflush(), which takes the same arguments as the former.  */
#undef CACHE_FLUSH_FUNC
#define CACHE_FLUSH_FUNC "_cacheflush"


/* Make gcc agree with <machine/ansi.h> */

#undef WCHAR_TYPE
#define WCHAR_TYPE "int"

#undef WCHAR_UNSIGNED
#define WCHAR_UNSIGNED 0

#undef WCHAR_TYPE_SIZE
#define WCHAR_TYPE_SIZE 32

#undef WINT_TYPE
#define WINT_TYPE "int"
