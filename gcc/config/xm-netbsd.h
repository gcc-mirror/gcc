/* Configuration for GNU C-compiler for hosts running NetBSD.
   Copyright (C) 1995 Free Software Foundation, Inc.

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

/* This file defines machine-independent things specific to a host
   running NetBSD.  This file should not be specified as $xm_file itself;
   instead $xm_file should be CPU/xm-netbsd.h, which should include both
   CPU/xm-CPU.h and this file xm-netbsd.h.  */
   
#define HAVE_VPRINTF
