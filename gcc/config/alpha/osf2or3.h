/* Definitions of target machine for GNU compiler, for DEC Alpha, osf[23].
   Copyright (C) 1997 Free Software Foundation, Inc.

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

/* In OSF 2 or 3, linking with -lprof1 doesn't require -lpdf.  */

#undef LIB_SPEC
#define LIB_SPEC "%{p:-lprof1} %{pg:-lprof1} %{a:-lprof2} -lc"

/* As of OSF 3.2, as still can't subtract adjacent labels.  */

#undef TARGET_AS_CAN_SUBTRACT_LABELS
#define TARGET_AS_CAN_SUBTRACT_LABELS 0

