/* Configuration for GNU C-compiler for PC532 running Minix
   Copyright (C) 1987,1990 Free Software Foundation, Inc.
   Contributed by Jyrki Kuoppala <jkp@cs.hut.fi>, August 1990

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

/* We have USG-style include files and time functions */

#define USG

#include "ns32k/xm-ns32k.h"

#ifndef HZ
#define HZ 60
#endif

#ifndef MAXPATHLEN
#define MAXPATHLEN 1024
#endif
