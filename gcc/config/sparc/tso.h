/* Include fragment for Sparc TSO operating systems.
   Copyright (C) 2011-2016 Free Software Foundation, Inc.

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


/* This operating system sets PSTATE.MM to the TSO memory model.  */
#undef SUBTARGET_DEFAULT_MEMORY_MODEL
#define SUBTARGET_DEFAULT_MEMORY_MODEL	SMM_TSO
