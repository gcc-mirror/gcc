/* Configuration for GCC for Intel i386 running Linux-based GNU systems.
   Copyright (C) 1995, 1996, 1997, 1999 Free Software Foundation, Inc.
   Contributed by H.J. Lu (hjl@nynexst.com)

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

#undef  POSIX
#define POSIX

/* We do have one, but I'd like to use the one come with gcc since
   we have been doing that for a long time with USG defined.  H.J. */
#undef HAVE_STAB_H
