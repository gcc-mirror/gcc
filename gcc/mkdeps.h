/* Dependency generator for Makefile fragments.
   Copyright (C) 2000 Free Software Foundation, Inc.
   Contributed by Zack Weinberg, Mar 2000

This program is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2, or (at your option) any
later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

 In other words, you are welcome to use, share and improve this program.
 You are forbidden to forbid anyone else to use, share and improve
 what you give them.   Help stamp out software-hoarding!  */

#ifndef __GCC_MKDEPS__
#define __GCC_MKDEPS__

/* This is the data structure used by all the functions in mkdeps.c.
   It's quite straightforward, but should be treated as opaque.  */

struct deps
{
  const char **targetv;
  unsigned int ntargets;	/* number of slots actually occupied */
  unsigned int targets_size;	/* amt of allocated space - in words */

  const char **depv;
  unsigned int ndeps;
  unsigned int deps_size;
};

/* Create a deps buffer.  */
extern struct deps *deps_init	PARAMS ((void));

/* Destroy a deps buffer.  */
extern void deps_free		PARAMS ((struct deps *));

/* Add a target (appears on left side of the colon) to the deps list. */
extern void deps_add_target	PARAMS ((struct deps *, const char *));

/* Given the name of the primary source file, calculate and add the
   name of the target.  This is done by locating and stripping the
   file extension (if any) and adding .o (OBJECT_SUFFIX).  In addition,
   any directory components of the path are discarded.  */
extern void deps_calc_target	PARAMS ((struct deps *, const char *));

/* Add a dependency (appears on the right side of the colon) to the
   deps list.  Dependencies will be printed in the order that they
   were entered with this function.  By convention, the first
   dependency entered should be the primary source file.  */
extern void deps_add_dep	PARAMS ((struct deps *, const char *));

/* Write out a deps buffer to a specified file.  The third argument
   is the number of columns to word-wrap at (0 means don't wrap).  */
extern void deps_write		PARAMS ((const struct deps *, FILE *,
					 unsigned int));

/* For each dependency *except the first*, emit a dummy rule for that
   file, causing it to depend on nothing.  This is used to work around
   the intermediate-file deletion misfeature in Make, in some
   automatic dependency schemes.  */
extern void deps_dummy_targets	PARAMS ((const struct deps *, FILE *));

#endif
