/* Definitions of target machine for GNU compiler.
   64-bit VxWorks Sparc version.
   Copyright (C) 2001 Free Software Foundation, Inc.

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


/* Specify what to link with.  */
/* VxWorks does all the library stuff itself.  */
#undef	LIB_SPEC
#define	LIB_SPEC "-r"

/* VxWorks provides the functionality of crt0.o and friends itself.  */
#undef  STARTFILE_SPEC
#undef	ENDFILE_SPEC
#define	STARTFILE_SPEC ""
#define	ENDFILE_SPEC ""

#undef LINK_SPEC
#define LINK_SPEC ""

/* We need to prohibit dots in constructor labels so that we can build a
   table of { string, address } entries for each non-static name in a
   program.  The address, being of the form &name, it cannot contain a dot or
   C will try to parse it as a &struct.field phrase.  */
#undef NO_DOLLAR_IN_LABEL
#undef DOLLARS_IN_IDENTIFIERS
#define DOLLARS_IN_IDENTIFIERS 1
#define NO_DOT_IN_LABEL

/* Enable #pragma pack(n) */
#define HANDLE_SYSV_PRAGMA

/* We use stabs for debugging */
#undef PREFERRED_DEBUGGING_TYPE
#define PREFERRED_DEBUGGING_TYPE DBX_DEBUG

/* Longs are still only 32bits for vxWorks, even for UltraSPARC */
#undef LONG_TYPE_SIZE
#define LONG_TYPE_SIZE          32

#undef CPP_ARCH32_SPEC
#define CPP_ARCH32_SPEC "-Acpu(sparc) -Amachine(sparc)"
#undef CPP_ARCH64_SPEC
#define CPP_ARCH64_SPEC \
"-Dsparc64 -D__arch64__ -Acpu(sparc64) -Amachine(sparc64)"

#undef CPP_PREDEFINES
#define CPP_PREDEFINES "-D__vxworks -D__sparc__ -Dsparc -D__GCC_NEW_VARARGS__"

/* Note that we define CPU here even if the user has specified -ansi.
   This violates user namespace, but the VxWorks headers, and potentially
   user code, all explicitly rely upon the definition of CPU in order to get
   the proper processor information.  */
#undef CPP_SPEC
#define CPP_SPEC "%(cpp_cpu) %(cpp_arch) -DCPU=ULTRASPARC -D__CPU__=CPU"

#undef TARGET_DEFAULT
#define TARGET_DEFAULT (MASK_APP_REGS | MASK_FPU \
			| MASK_LONG_DOUBLE_128 | MASK_64BIT)

#undef SPARC_DEFAULT_CMODEL
#define SPARC_DEFAULT_CMODEL CM_MEDLOW

#undef PTRDIFF_TYPE
#undef SIZE_TYPE
#undef WCHAR_TYPE
#undef WCHAR_TYPE_SIZE

#define PTRDIFF_TYPE "long int"
#define SIZE_TYPE "unsigned int"
#define WCHAR_TYPE "char"
#define WCHAR_TYPE_SIZE 8

/* US Software GOFAST library support.  */
#undef INIT_SUBTARGET_OPTABS
#define INIT_SUBTARGET_OPTABS INIT_GOFAST_OPTABS
