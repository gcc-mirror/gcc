/* MD reader definitions.
   Copyright (C) 1987, 1991, 1992, 1993, 1994, 1995, 1996, 1997, 1998, 1999,
   2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2010
   Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#include "obstack.h"

extern int read_rtx_lineno;
extern const char *read_rtx_filename;
extern struct obstack string_obstack;

extern void copy_rtx_ptr_loc (const void *, const void *);
extern void print_rtx_ptr_loc (const void *);
extern const char *join_c_conditions (const char *, const char *);
extern void print_c_condition (const char *);
extern void message_with_line (int, const char *, ...) ATTRIBUTE_PRINTF_2;
extern void fatal_with_file_and_line (FILE *, const char *, ...)
  ATTRIBUTE_PRINTF_2 ATTRIBUTE_NORETURN;
extern void fatal_expected_char (FILE *, int, int) ATTRIBUTE_NORETURN;
extern int read_skip_spaces (FILE *);
extern char *read_quoted_string (FILE *);
extern char *read_string (FILE *, int);
extern int n_comma_elts (const char *);
extern const char *scan_comma_elt (const char **);
extern void init_md_reader (void);
