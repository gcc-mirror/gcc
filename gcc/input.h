/* Declarations for variables relating to reading the source file.
   Used by parsers, lexical analyzers, and error message routines.
   Copyright (C) 1993, 1997, 1998, 2000, 2003 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 2, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to the Free
Software Foundation, 59 Temple Place - Suite 330, Boston, MA
02111-1307, USA.  */

#ifndef GCC_INPUT_H
#define GCC_INPUT_H

/* The data structure used to record a location in a translation unit.  */
/* Long-term, we want to get rid of this and typedef fileline location_t.  */
struct location_s GTY (())
{
  /* The name of the source file involved.  */
  const char *file;

  /* The line-location in the source file.  */
  int line;
};
typedef struct location_s location_t;

struct file_stack
{
  struct file_stack *next;
  location_t location;
};

/* Top-level source file.  */
extern const char *main_input_filename;

extern location_t input_location;
#define input_line (input_location.line)
#define input_filename (input_location.file)

/* Stack of currently pending input files.
   The line member is not accurate for the innermost file on the stack.  */
extern struct file_stack *input_file_stack;

/* Stack of EXPR_WITH_FILE_LOCATION nested expressions.  */
extern struct file_stack *expr_wfl_stack;

/* Incremented on each change to input_file_stack.  */
extern int input_file_stack_tick;

extern void push_srcloc (const char *name, int line);
extern void pop_srcloc (void);

#endif
