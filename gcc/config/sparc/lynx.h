/* Definitions for SPARC running LynxOS.
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

#include "sparc/sparc.h"
#include "lynx.h"

/* ??? Must redefine to get sparclite and v8 defines.  Can this be done
   differently?  */
#undef CPP_SPEC
#define CPP_SPEC "%{mthreads:-D_MULTITHREADED} %{msparclite:-D__sparclite__} %{mv8:-D__sparc_v8__} %{mposix:-D_POSIX_SOURCE} %{msystem-v:-lynx-sysv}"

/* Names to predefine in the preprocessor for this target machine.  */
#undef CPP_PREDEFINES
#define CPP_PREDEFINES "-Dunix -Dsparc -DLynx -DIBITS32 -Asystem(unix) -Asystem(lynx) -Acpu(sparc) -Amachine(sparc)"
