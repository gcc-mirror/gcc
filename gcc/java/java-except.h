/* Definitions for exception handling for use by the GNU compiler
   for the Java(TM) language compiler.
   Copyright (C) 1997, 1998, 1999 Free Software Foundation, Inc.

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
Boston, MA 02111-1307, USA.  

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

    /* Surrunding handler, if any. */
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
  };

/* A dummy range that represents the entire method. */
extern struct eh_range whole_range;

#define NULL_EH_RANGE (&whole_range)

extern struct eh_range * find_handler PROTO ((int));

extern void method_init_exceptions PROTO ((void));

extern void emit_handlers PROTO ((void));

extern void maybe_start_try PROTO ((int));

extern void maybe_end_try PROTO ((int));

extern void add_handler PROTO ((int, int, tree, tree));

extern void handle_nested_ranges PROTO ((void));

extern void expand_resume_after_catch PROTO ((void));
