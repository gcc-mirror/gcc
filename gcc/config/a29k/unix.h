/* Definitions of target machine for GNU compiler, for AMD Am29000 CPU, Unix.
   Copyright (C) 1991, 1993, 1994 Free Software Foundation, Inc.
   Contributed by Richard Kenner (kenner@vlsi1.ultra.nyu.edu)

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


/* This is mostly the same as a29k.h, except that we define unix instead of
   EPI and define unix-style machine names.  */

#include "a29k/a29k.h"

/* Set our default target to be the 29050; that is the more interesting chip
   for Unix systems.  */

#undef TARGET_DEFAULT
#define TARGET_DEFAULT (1+2+16+128)

#undef CPP_PREDEFINES
#define CPP_PREDEFINES "-Dam29k -Da29k -Dam29000 -Asystem(unix) -Acpu(a29k) -Amachine(a29k)"

#undef CPP_SPEC
#define CPP_SPEC "%{!m29000:-Dam29050 -D__am29050__}"

/* Use a default linker configuration file.  */
#undef LINK_SPEC
#define LINK_SPEC "-T default.gld%s"

/* Define the magic numbers that we recognize as COFF.  */

#define MY_ISCOFF(magic) ((magic) == SIPFBOMAGIC || (magic) == SIPRBOMAGIC)

/* For some systems, it is best if double-word objects are aligned on a 
   doubleword boundary.  We want to maintain compatibility with MetaWare in
   a29k.h, but do not feel constrained to do so here.  */

#undef BIGGEST_ALIGNMENT
#define BIGGEST_ALIGNMENT 64

/* Add shared data as a kludge for now.  */

#undef ASM_FILE_START
#define ASM_FILE_START(FILE)					\
{ char *p, *after_dir = main_input_filename;			\
  if (TARGET_29050)						\
    fprintf (FILE, "\t.cputype 29050\n");			\
  for (p = main_input_filename; *p; p++)			\
    if (*p == '/')						\
      after_dir = p + 1;					\
  fprintf (FILE, "\t.file ");					\
  output_quoted_string (FILE, after_dir);			\
  fprintf (FILE, "\n");						\
  if (flag_shared_data)						\
    fprintf (FILE, "\t.sect .shdata,data\n");			\
  fprintf (FILE, "\t.sect .lit,lit\n");  }

/* Output before shared  data.  */

#define SHARED_SECTION_ASM_OP "\t.use .shdata"

/* If we want shared data, we have to turn off commons.  */

#define OVERRIDE_OPTIONS if (flag_shared_data) flag_no_common = 1;

/* Default to -fno-pcc-struct-return, since we don't have to worry about
   compatibility.  */
#define DEFAULT_PCC_STRUCT_RETURN 0

#if 0 /* This would be needed except that the 29k doesn't have strict
	 alignment requirements.  */

#define FUNCTION_ARG_BOUNDARY(MODE, TYPE)				\
  (((TYPE) != 0)							\
	? ((TYPE_ALIGN(TYPE) <= PARM_BOUNDARY)				\
		? PARM_BOUNDARY						\
		: TYPE_ALIGN(TYPE))					\
	: ((GET_MODE_ALIGNMENT(MODE) <= PARM_BOUNDARY)			\
		? PARM_BOUNDARY						\
		: GET_MODE_ALIGNMENT(MODE)))
#endif
