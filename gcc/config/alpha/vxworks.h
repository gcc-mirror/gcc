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

#undef CPP_SUBTARGET_SPEC
#define CPP_SUBTARGET_SPEC "\
%{mvxsim:-DCPU=SIMALPHADUNIX} \
%{!mvxsim: %{!mcpu*|mcpu=21064:-DCPU=21064} %{mcpu=21164:-DCPU=21164}} \
%{posix: -D_POSIX_SOURCE}"

#undef CPP_PREDEFINES
#define CPP_PREDEFINES "\
-D__vxworks -D__alpha_vxworks -Asystem=vxworks \
-Asystem=embedded -D_LONGLONG"

/* VxWorks does all the library stuff itself.  */

#undef LIB_SPEC
#define LIB_SPEC ""

/* VxWorks uses object files, not loadable images.  Make linker just combine
   objects.  Also show using 32 bit mode and set start of text to 0.  */

#undef LINK_SPEC
#define LINK_SPEC "-r -taso -T 0"

/* VxWorks provides the functionality of crt0.o and friends itself.  */

#undef STARTFILE_SPEC
#define STARTFILE_SPEC ""

#undef ENDFILE_SPEC
#define ENDFILE_SPEC ""
