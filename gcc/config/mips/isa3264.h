/* Definitions of target machine for GNU compiler.
   MIPS 32 and MIPS 64 version.
   Copyright (C) 1999, 2000 Free Software Foundation, Inc.

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

#ifndef MIPS_ISA_DEFAULT
#define MIPS_ISA_DEFAULT 32
#endif

#ifndef MIPS_ABI_DEFAULT
#define MIPS_ABI_DEFAULT ABI_MEABI
#endif

#ifndef MIPS_ENABLE_EMBEDDED_O32
#define MIPS_ENABLE_EMBEDDED_O32 1
#endif

#ifndef PREFERRED_DEBUGGING_TYPE
#define PREFERRED_DEBUGGING_TYPE DBX_DEBUG
#endif

#include "mips/elf.h"

#if MIPS_ABI_DEFAULT == ABI_MEABI
/* For MEABI, don't link with crt0 files, let the linker start files specify
   the appropriate crt0 file.  */
#undef  STARTFILE_SPEC
#define STARTFILE_SPEC "crti%O%s crtbegin%O%s %{!mno-crt0: }"
#endif
