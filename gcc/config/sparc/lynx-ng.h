/* Definitions for SPARC running LynxOS, using Lynx's old as and ld.
   Copyright (C) 1993, 1995 Free Software Foundation, Inc.

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

#include <sparc/sparc.h>
#include <lynx-ng.h>

/* ??? Must redefine to get sparclite and v8 defines.  Can this be done
   differently?  */

#undef CPP_SPEC
#define CPP_SPEC "%{mthreads:-D_MULTITHREADED}  \
  %{mposix:-D_POSIX_SOURCE}  \
  %{msystem-v:-I/usr/include_v}  \
  %{msparclite:-D__sparclite__} %{mv8:-D__sparc_v8__}"

/* Names to predefine in the preprocessor for this target machine.  */

#undef CPP_PREDEFINES
#define CPP_PREDEFINES "-Dunix -Dsparc -DLynx -DIBITS32 -Asystem(unix) -Asystem(lynx) -Acpu(sparc) -Amachine(sparc)"

/* Provide required defaults for linker switches.  */

#undef LINK_SPEC
#define LINK_SPEC "-e __main -T 0 %{msystem-v:-V} %{mcoff:-k}"
