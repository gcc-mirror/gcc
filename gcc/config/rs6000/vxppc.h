/* Definitions of target machine for GNU compiler.  Vxworks PowerPC version.
   Copyright (C) 1996, 2000 Free Software Foundation, Inc.

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

/* This file just exists to give specs for the PowerPC running on VxWorks.  */

/* Reset defaults */
#undef	CPP_OS_DEFAULT_SPEC
#define CPP_OS_DEFAULT_SPEC "%(cpp_os_vxworks)"

#undef	LIB_DEFAULT_SPEC
#define LIB_DEFAULT_SPEC "%(lib_vxworks)"

#undef	STARTFILE_DEFAULT_SPEC
#define STARTFILE_DEFAULT_SPEC "%(startfile_vxworks)"

#undef	ENDFILE_DEFAULT_SPEC
#define ENDFILE_DEFAULT_SPEC "%(endfile_vxworks)"

#undef	LINK_START_DEFAULT_SPEC
#define LINK_START_DEFAULT_SPEC "%(link_start_vxworks)"

#undef	LINK_OS_DEFAULT_SPEC
#define LINK_OS_DEFAULT_SPEC "%(link_os_vxworks)"

#undef CPP_PREDEFINES
#define CPP_PREDEFINES "\
-D__vxworks -D__vxworks__ -Asystem=vxworks -Asystem=embedded \
-Acpu=powerpc -Amachine=powerpc"

/* Don't define _LITTLE_ENDIAN or _BIG_ENDIAN */
#undef	CPP_ENDIAN_BIG_SPEC
#define CPP_ENDIAN_BIG_SPEC "-D__BIG_ENDIAN__ -Amachine=bigendian"

#undef	CPP_ENDIAN_LITTLE_SPEC
#define CPP_ENDIAN_LITTLE_SPEC "-D__LITTLE_ENDIAN__ -Amachine=littleendian"

/* We use stabs-in-elf for debugging */
#undef PREFERRED_DEBUGGING_TYPE
#define PREFERRED_DEBUGGING_TYPE DBX_DEBUG
