/* Definitions for DECstation running BSD as target machine for GNU compiler.
   Copyright (C) 1993, 1995, 1996 Free Software Foundation, Inc.

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

#define DECSTATION

#ifndef CPP_PREDEFINES
#define CPP_PREDEFINES "-D__ANSI_COMPAT \
-DMIPSEL -DR3000 -DSYSTYPE_BSD -D_SYSTYPE_BSD -Dbsd4_4 -Dhost_mips -Dmips \
-Dunix -D_mips -D_unix -D_host_mips -D_MIPSEL -D_R3000 \
-Asystem=unix -Asystem=bsd -Amachine=mips"
#endif

/* Always uses GNU ld.  */
#ifndef LINK_SPEC
#define LINK_SPEC "%{G*} %{EB} %{EL} %{mips1} %{mips2} %{mips3}"
#endif

#define LIB_SPEC ""
#define STARTFILE_SPEC ""

#ifndef MACHINE_TYPE
#define MACHINE_TYPE "DECstation running BSD 4.4"
#endif

#define TARGET_DEFAULT MASK_GAS
#define PREFERRED_DEBUGGING_TYPE DBX_DEBUG

#include "mips/mips.h"

/* Since gas and gld are standard on 4.4 BSD, we don't need these */
#undef MD_EXEC_PREFIX
#undef MD_STARTFILE_PREFIX
#undef ASM_FINAL_SPEC
#undef LIB_SPEC
#undef STARTFILE_SPEC

