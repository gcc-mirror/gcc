/* Definitions of target machine for GNU compiler.  Vxworks Alpha version.
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

/* This file just exists to give specs for the Alpha running on VxWorks.  */

#undef CPP_SPEC
#define CPP_SPEC "\
%{mvxsim:-DCPU=SIMALPHADUNIX} \
%{!mvxsim: %{!mcpu*:-DCPU=21064} \
           %{mcpu=21064:-DCPU=21064} \
           %{mcpu=21164:-DCPU=21164}} \
%{posix: -D_POSIX_SOURCE} \
%{!.S:	-D__LANGUAGE_C__ -D__LANGUAGE_C %{!ansi:-DLANGUAGE_C}}  \
%{.S:	-D__LANGUAGE_ASSEMBLY__ -D__LANGUAGE_ASSEMBLY %{!ansi:-DLANGUAGE_ASSEMBLY}}"

#undef CPP_PREDEFINES
#define CPP_PREDEFINES "\
-D__vxworks -D__alpha_vxworks -Asystem(vxworks) \
-Asystem(embedded) -D_LONGLONG  -Acpu(alpha) -Amachine(alpha)"

/* VxWorks does all the library stuff itself.  */

#undef LIB_SPEC
#define LIB_SPEC ""

/* VxWorks uses object files, not loadable images.  make linker just
   combine objects. */

#undef LINK_SPEC
#define LINK_SPEC "-r"

/* VxWorks provides the functionality of crt0.o and friends itself.  */

#undef STARTFILE_SPEC
#define STARTFILE_SPEC ""

#undef ENDFILE_SPEC
#define ENDFILE_SPEC ""

#undef TRANSFER_FROM_TRAMPOLINE
