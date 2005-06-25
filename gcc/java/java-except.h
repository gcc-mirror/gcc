/* Definitions for exception handling for use by the GNU compiler
   for the Java(TM) language compiler.
   Copyright (C) 1997, 1998, 1999, 2000, 2003, 2004, 2005
   Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to
the Free Software Foundation, 51 Franklin Street, Fifth Floor,
Boston, MA 02110-1301, USA.  

Java and all Java-based marks are trademarks or registered trademarks
of Sun Microsystems, Inc. in the United States and other countries.
The Free Software Foundation is independent of Sun Microsystems, Inc.  */

struct eh_range
  {
    /* The (byte-code PC) range of the handled block. */
    int start_pc;
    int end_pc;

    /* A list of handlers.  For each element in the list,
       the TREE_PURPOSE is the handled class (NULL_EXPR for a finally block),
       and the TREE_VALUE is the LABEL_DECL of the handler. */
    tree handlers;

    /* Surrounding handler, if any. */
    struct eh_range *outer;

    /* The first child range.  It is is nested inside this range
       (i.e. this.start_pc <= first_child.end_pc
       && this.end_pc >= first_child.end_pc).
       The children are linked together using next_sibling, and are sorted
       by increasing start_pc and end_pc (we do not support non-nested
       overlapping ranges). */
    struct eh_range *first_child;

    /* The next child of outer, in address order. */
    struct eh_range *next_sibling;

    /* True if this range has already been expanded. */
    int expanded;

    /* The TRY_CATCH_EXPR for this EH range.  */
    tree stmt;
  };

/* A dummy range that represents the entire method. */
extern struct eh_range whole_range;

#define NULL_EH_RANGE (&whole_range)

extern struct eh_range * find_handler (int);
extern void method_init_exceptions (void);
extern void maybe_start_try (int, int);
extern void add_handler (int, int, tree, tree);
extern void expand_end_java_handler (struct eh_range *);
extern bool sanity_check_exception_range (struct eh_range *);
