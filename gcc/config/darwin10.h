/* Target definitions for Darwin (Mac OS X) systems.
   Copyright (C) 2009 Free Software Foundation, Inc.
   Contributed by Jack Howarth <howarth@bromo.med.uc.edu>.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

/* Fix PR41260 by passing -no_compact_unwind on darwin10 and later until
unwinder in libSystem is fixed to digest new epilog unwinding notes. */

#undef LIB_SPEC
#define LIB_SPEC "%{!static:-no_compact_unwind -lSystem}"
