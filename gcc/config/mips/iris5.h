/* Definitions of target machine for GNU compiler.  Iris version 5.
   Copyright (C) 1993 Free Software Foundation, Inc.

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
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

#define TARGET_DEFAULT MASK_ABICALLS
#define ABICALLS_ASM_OP ".option pic2"

#include "mips/iris4.h"

/* mips-tfile doesn't work yet.  No debugging is supported.  */
#undef ASM_FINAL_SPEC
#undef SDB_DEBUGGING_INFO
#undef DBX_DEBUGGING_INFO

#undef CPP_PREDEFINES
#define CPP_PREDEFINES	"\
-D_MIPS_FPSET=16 -D_MIPS_ISA=_MIPS_ISA_MIPS1 -D_MIPS_SIM_ABI32 \
-Dunix -Dmips -Dsgi -DSVR3 -Dhost_mips -DMIPSEB -DSYSTYPE_SYSV \
-D_SYSTYPE_SVR4 -D_LONGLONG -D_MIPSEB -D_SGI_SOURCE -D__SDO__  \
-D_MIPS_SZLONG=32 -D_MIPS_SZINT=32 -D_MIPS_SZPTR=32 -D_SVR4_SOURCE \
-Asystem(unix) -Asystem(svr3) -Acpu(mips) -Amachine(mips)"

#undef ASM_SPEC
#define ASM_SPEC "-elf -KPIC -EB %{O*:-O2 -fullasopt} %{!O*:-O1}"

#undef LINK_SPEC
#define LINK_SPEC "-elf -_SYSTYPE_SVR4 -require_dynamic_link \
 _rld_new_interface -no_unresolved -KPIC -call_shared -transitive_link"
