/* Definitions for specs for GNU CHILL.
   Copyright (C) 1995, 1998, 1999 Free Software Foundation, Inc..

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

/* This is the contribution to the `default_compilers' array in gcc.c for
   CHILL.  */

  {".ch",  "@chill"},
  {".chi", "@chill"},
  {"@chill",
     "tradcpp0 -lang-chill %{!no-gcc:-D__GNUCHILL__=%v1} %(cpp_options)\
	      %{!M:%{!MM:%{!E:%{!pipe:%g.i} |\n\
      cc1chill %{!pipe:%g.i} %(cc1_options)\
      %{!fsyntax-only:%(invoke_as)}}}}\n"},
