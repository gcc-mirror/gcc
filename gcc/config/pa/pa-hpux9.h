/* Definitions of target machine for GNU compiler, for HP PA-RISC 1.1
   Copyright (C) 1995 Free Software Foundation, Inc.
   Contributed by Tim Moore (moore@defmacro.cs.utah.edu)

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

#define TARGET_DEFAULT 0

#include "pa/pa-hpux.h"

/* We can debug dynamically linked executables on hpux9.  */
#undef LINK_SPEC
#define LINK_SPEC \
  "%{!shared:-u main} %{static:-a archive} %{shared:-b}"
