/* Definitions for Rs6000 running LynxOS.
   Copyright (C) 2003 Free Software Foundation, Inc.
   Contributed by David Henkel-Wallace, Cygnus Support (gumby@cygnus.com)

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published
   by the Free Software Foundation; either version 2, or (at your
   option) any later version.

   GCC is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
   or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
   License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING.  If not, write to the
   Free Software Foundation, 59 Temple Place - Suite 330, Boston,
   MA 02111-1307, USA.  */

/* Definitions we want to override with those from rs6000.h:  */
#undef LIB_SPEC
#undef PTRDIFF_TYPE
#undef SIZE_TYPE
#undef WCHAR_TYPE
#undef WCHAR_TYPE_SIZE
#undef EXTRA_SECTIONS
#undef READONLY_DATA_SECTION
#undef READONLY_DATA_SECTION_ASM_OP
#undef EXTRA_SECTION_FUNCTIONS
#undef TARGET_ASM_SELECT_RTX_SECTION
#undef TARGET_ASM_SELECT_SECTION
#undef USER_LABEL_PREFIX
#undef ASM_OUTPUT_LABELREF
#undef ASM_GENERATE_INTERNAL_LABEL
#undef ASM_OUTPUT_COMMON
#undef ASM_OUTPUT_LOCAL

#undef SDB_DEBUGGING_INFO
#undef DBX_DEBUGGING_INFO
#undef PREFERRED_DEBUGGING_TYPE

#undef FUNCTION_PROFILER
#undef SUBTARGET_SWITCHES
