/* Definitions of target machine for GNU compiler, for DEC Alpha.
   Copyright (C) 1992, 1993 Free Software Foundation, Inc.
   Contributed by Richard Kenner (kenner@nyu.edu)

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


#include "alpha/alpha.h"

/* In OSF 1.2, there is a linker bug that prevents use of -O3 to
   the linker.  */

#undef LINK_SPEC
#define LINK_SPEC  \
  "-G 8 -O1 %{!shared:-init __main} %{static:-non_shared}
   %{!static:%{shared:-shared} %{!shared:-call_shared}}"
