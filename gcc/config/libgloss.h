/* libgloss.h -- operating system specific defines to be used when
   targeting GCC for Libgloss supported targets.
   Copyright (C) 1996 Free Software Foundation, Inc.

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

/* The libgloss standard for crt0.s has the name based on the command line
   option. */
#undef STARTFILE_SPEC
#define STARTFILE_SPEC "%{!shared:%{pg:pgcrt0%O%s}%{!pg:%{p:pcrt0%O%s}%{!p:crt0%O%s}}}"

/* This file used to force LINK_SPEC to be the null string, but that is not
   correct.  LINK_SPEC is used to pass machine specific arguments to the
   linker and hence can not be redefined here.  LINK_SPEC is never used to
   specify startup files or libraries, so it should never conflict with
   libgloss.  */

/* Don't set the target flags, this is done by the linker script */
#undef LIB_SPEC
#define LIB_SPEC ""
