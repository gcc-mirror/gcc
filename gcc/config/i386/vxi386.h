/* Definitions of target machine for GNU compiler.  VxWorks i386 version.
   Copyright (C) 1998 Free Software Foundation, Inc.

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

#undef CPP_CPU_SPEC
#define CPP_CPU_SPEC "\
-Asystem=unix -Acpu=i386 -Amachine=i386 \
%{!ansi:-Di386} -D__i386 -D__i386__ \
%{march=i386:-DCPU=I80386} \
%{march=i486:-DCPU=I80486 %(cpp_486)} \
%{march=pentium:-DCPU=PENTIUM -DCPU_VARIANT=PENTIUM %(cpp_586)} \
%{march=pentiumpro:-DCPU=PENTIUM -DCPU_VARIANT=PENTIUMPRO %(cpp_686)} \
%{!march=*: \
 %{mcpu=i386:-DCPU=I80386} \
 %{mcpu=i486:-DCPU=I80486 %(cpp_486)} %{m486:-DCPU=I80486 %(cpp_486)} \
 %{mpentium:-DCPU=PENTIUM -DCPU_VARIANT=PENTIUM %(cpp_586)} \
 %{mcpu=pentium:-DCPU=PENTIUM -DCPU_VARIANT=PENTIUM %(cpp_586)} \
 %{mpentiumpro:-DCPU=PENTIUM -DCPU_VARIANT=PENTIUMPRO %(cpp_686)} \
 %{mcpu=pentiumpro:-DCPU=PENTIUM -DCPU_VARIANT=PENTIUMPRO %(cpp_686)} \
 %{!mcpu*:%{!m486:%{!mpentium*:-DCPU=I80386 %(cpp_cpu_default)}}}}"

#include "i386/i386-aout.h"

#define HANDLE_SYSV_PRAGMA

#undef CPP_PREDEFINES
#define CPP_PREDEFINES "-D__vxworks"

/* VxWorks does all the library stuff itself.  */

#undef LIB_SPEC
#define LIB_SPEC ""

/* VxWorks uses object files, not loadable images.  make linker just
   combine objects.  */

#undef LINK_SPEC
#define LINK_SPEC "-r"

/* VxWorks provides the functionality of crt0.o and friends itself.  */

#undef STARTFILE_SPEC
#define STARTFILE_SPEC ""

#undef ENDFILE_SPEC
#define ENDFILE_SPEC ""
