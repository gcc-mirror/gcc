/* Configuration for GNU compiler for an Alpha running Windows NT 3.x.
   Copyright (C) 1995, 1996 Free Software Foundation, Inc.
   Contributed by Douglas B. Rupp (drupp@cs.washington.edu)

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

#undef HOST_BITS_PER_LONG
#define	HOST_BITS_PER_LONG	32

#undef POSIX

#define access _access
#define close _close
#define mktemp _mktemp
#define open _open
#define read _read
#define write _write

#undef HAS_INIT_SECTION
