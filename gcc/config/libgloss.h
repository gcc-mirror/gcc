/* libgloss.h -- operating system specific defines to be used when
   targeting GCC for Libgloss supported targets.
   Copyright (C) 1996, 2004, 2007 Free Software Foundation, Inc.

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

/* This file should not be used for ELF targets, as this definition of
   STARTFILE_SPEC is all wrong.  */

/* The libgloss standard for crt0.s has the name based on the command line
   option.  */
#undef STARTFILE_SPEC
#define STARTFILE_SPEC "%{!shared:%{pg:pgcrt0%O%s}%{!pg:%{p:pcrt0%O%s}%{!p:crt0%O%s}}}"

/* This file used to force LINK_SPEC to be the null string, but that is not
   correct.  LINK_SPEC is used to pass machine specific arguments to the
   linker and hence cannot be redefined here.  LINK_SPEC is never used to
   specify startup files or libraries, so it should never conflict with
   libgloss.  */

/* Don't set the target flags, this is done by the linker script */
#undef LIB_SPEC
#define LIB_SPEC ""
