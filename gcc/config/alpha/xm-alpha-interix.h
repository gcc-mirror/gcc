/* Configuration for GNU compiler
   for an DEC/Compaq Alpha
   Copyright (C) 1999 Free Software Foundation, Inc.
   Donn Terry, Softway Systems, Inc.
     derived from code by Douglas B. Rupp (drupp@cs.washington.edu)

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

#include <alpha/xm-alpha.h>

#undef HOST_BITS_PER_LONG
#define HOST_BITS_PER_LONG	32

#define HOST_BITS_PER_WIDE_INT 64
#ifdef __GNUC__
#   define HOST_WIDE_INT long long
#else
#   define HOST_WIDE_INT __int64
#endif


#define HOST_BITS_PER_WIDEST_INT HOST_BITS_PER_LONGLONG
#ifdef __GNUC__
#   define HOST_WIDEST_INT long long
#else
#   define HOST_WIDEST_INT __int64
#endif
#define HOST_WIDEST_INT_PRINT_DEC "%lld"
#define HOST_WIDEST_INT_PRINT_UNSIGNED "%llu"
#define HOST_WIDEST_INT_PRINT_HEX "0x%llx"
