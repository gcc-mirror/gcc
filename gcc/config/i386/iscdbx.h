/* Definitions for Intel 386 running Interactive Unix System V,
   using dbx-in-coff encapsulation.
   Specifically, this is for recent versions that support POSIX.
   Copyright (C) 1992 Free Software Foundation, Inc.

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

/* Mostly it's like AT&T Unix System V with dbx-in-coff. */

#include "i386/svr3dbx.h"

/* But with a few changes.  */
#undef ENDFILE_SPEC
#include "i386/isc.h"

/* Overridden defines for ifile usage. */

#undef STARTFILE_SPEC
#define STARTFILE_SPEC \
  "%{!r:%{!z:svr3.ifile%s}%{z:svr3z.ifile%s}}\
   %{!shlib:%{posix:%{pg:mcrtp1.o%s}%{!pg:%{p:mcrtp1.o%s}%{!p:crtp1.o%s}}}\
   %{!posix:%{pg:mcrt1.o%s}%{!pg:%{p:mcrt1.o%s}%{!p:crt1.o%s}}}\
   %{p:-L/usr/lib/libp} %{pg:-L/usr/lib/libp}}\
   %{shlib:%{posix:crtp1.o%s}%{!posix:crt1.o%s}}"
  
#undef ENDFILE_SPEC
#define ENDFILE_SPEC "crtn.o%s"
