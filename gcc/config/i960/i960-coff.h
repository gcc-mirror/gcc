/* Definitions of target machine for GNU compiler, for "naked" Intel
   80960 using coff object format and coff debugging symbols.
   Copyright (C) 1988, 1989, 1991, 1996, 2000 Free Software Foundation.
   Contributed by Steven McGeady (mcg@omepd.intel.com)
   Additional work by Glenn Colon-Bonet, Jonathan Shapiro, Andy Wilson
   Converted to GCC 2.0 by Michael Tiemann, Cygnus Support.

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

/* Support -gstabs using stabs in COFF sections.  */

/* Generate SDB_DEBUGGING_INFO by default.  */
#undef PREFERRED_DEBUGGING_TYPE
#define PREFERRED_DEBUGGING_TYPE SDB_DEBUG

/* This is intended to be used with Cygnus's newlib library, so we want to
   use the standard definition of LIB_SPEC.  */
#undef LIB_SPEC

#undef ASM_FILE_START
#define ASM_FILE_START(FILE) \
  output_file_directive ((FILE), main_input_filename)

/* Support the ctors and dtors sections for g++.  */

#define CTORS_SECTION_ASM_OP	"\t.section\t.ctors,\"x\""
#define DTORS_SECTION_ASM_OP	"\t.section\t.dtors,\"x\""

/* end of i960-coff.h */
