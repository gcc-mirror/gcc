/* Operating system specific defines to be used when targeting GCC for
   hosting on Windows32, using GNU tools and the Windows32 API Library.
   This variant uses CRTDLL.DLL insted of MSVCRTDLL.DLL.
   Copyright (C) 1998, 1999, 2000 Free Software Foundation, Inc.

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

#undef CPP_PREDEFINES
#define CPP_PREDEFINES "-D_WIN32 -DWIN32 \
  -D__MINGW32__=0.2 -DWINNT  -D_X86_=1 \
  -Asystem=winnt"

#undef LIBGCC_SPEC
#define LIBGCC_SPEC \
  "%{mthreads:-lmingwthrd} -lmingw32 -lgcc -lmoldname -lcrtdll"

/* Specify a different entry point when linking a DLL */
#undef STARTFILE_SPEC
#define STARTFILE_SPEC "%{mdll:dllcrt1%O%s} %{!mdll:crt1%O%s} %{pg:gcrt1%O%s}"

