/* Configuration for GNU C-compiler for hosts running GNU.
   Copyright (C) 1994 Free Software Foundation, Inc.

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

/* This file defines machine-independent things specific to a host
   running GNU.  This file should not be specified as $xm_file itself;
   instead $xm_file should be CPU/xm-gnu.h, which should include both
   CPU/xm-CPU.h and this file xm-gnu.h.  */
   
/* GNU has strerror.  */
#define HAVE_STRERROR

/* Get a definition of O_RDONLY; some of the GCC files don't include this
   properly and will define it themselves to be zero. */
#include <fcntl.h>
