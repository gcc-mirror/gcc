/* aoutos.h  --  operating system specific defines to be used when
   targeting GCC for some system that uses a.out file format.
   Copyright (C) 1992 Free Software Foundation, Inc.

   Written by Ron Guilmette (rfg@netcom.com).

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

/* To use this file, make up a file with a name like:

	?????aout.h

   where ????? is replaced by the name of the basic hardware that you
   are targeting for.  Then, in the file ?????aout.h, put something
   like:

	#include "?????.h"
	#include "aoutos.h"

   followed by any really system-specific defines (or overrides of
   defines) which you find that you need.  Now, modify the configure
   or configure.in script to properly use the new ?????aout.h file
   when configuring for the system.  */

/* Define a symbol indicating that we are using aoutos.h.  */
#define USING_AOUTOS_H

/* A C statement (sans semicolon) to output an element in the table of
   global constructors. 
   If using GNU LD, tell it that this is part of the static destructor set. 
   This code works for any machine provided you use GNU as/ld.
   If not using GNU LD, rely on a "collect" program to look for names defined
   in the particular form we choose as global constructor function names.  */

#define ASM_OUTPUT_CONSTRUCTOR(FILE,NAME)				\
  do {									\
    if (flag_gnu_linker)						\
      {									\
	/* Output an N_SETT (0x16, 22.) for the name.  */		\
	fprintf (FILE, "%s \"___CTOR_LIST__\",22,0,0,", ASM_STABS_OP);	\
	assemble_name (FILE, NAME);					\
	fputc ('\n', FILE);						\
      }									\
  } while (0)


/* A C statement (sans semicolon) to output an element in the table of
   global destructors.  */

#define ASM_OUTPUT_DESTRUCTOR(FILE,NAME)       				\
  do {									\
    if (flag_gnu_linker)						\
      {									\
	/* Output an N_SETT (0x16, 22.) for the name.  */		\
	fprintf (FILE, "%s \"___DTOR_LIST__\",22,0,0,", ASM_STABS_OP);	\
	assemble_name (FILE, NAME);					\
	fputc ('\n', FILE);						\
      }									\
  } while (0)

/* Likewise for entries we want to record for garbage collection.
   Garbage collection is still under development.  */

#define ASM_OUTPUT_GC_ENTRY(FILE,NAME)       				\
  do {									\
    if (flag_gnu_linker)						\
      {									\
	/* Output an N_SETT (0x16, 22.) for the name.  */		\
	fprintf (FILE, "%s \"___PTR_LIST__\",22,0,0,", ASM_STABS_OP);	\
	assemble_name (FILE, NAME);					\
	fputc ('\n', FILE);						\
      }									\
  } while (0)
