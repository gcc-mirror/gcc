/* Configuration for GNU C-compiler for openVMS/Alpha.
   Copyright (C) 2001 Free Software Foundation, Inc.
   Contributed by Douglas Rupp (rupp@gnat.com).

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

#undef HOST_WIDE_INT

/* Since DEC C long != Gnu C long */
#define HOST_PTR_PRINTF "%llx"
#define HOST_WIDE_INT_PRINT_DEC "%lld"
#define HOST_WIDE_INT_PRINT_UNSIGNED "%llu"
#define HOST_WIDE_INT_PRINT_HEX "0x%llx"
#define HOST_WIDE_INT_PRINT_DOUBLE_HEX "0x%llx%016llx"
