/* Definitions for the cross reference backend xref.c
   Copyright (C) 1999, 2000, 2003 Free Software Foundation, Inc.
   Contributed by Alexandre Petit-Bianco (apbianco@cygnus.com)

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
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.

Java and all Java-based marks are trademarks or registered trademarks
of Sun Microsystems, Inc. in the United States and other countries.
The Free Software Foundation is independent of Sun Microsystems, Inc.  */

/* Exported functions. */
void expand_xref (tree);

/* Lookup table to be used with the value of flag_emit_xref */
typedef struct {
  char *key;			         /* Activator in -fxref=<key>  */
  void (*expand) (FILE *, tree);         /* Function to write xrefs out */
  FILE *fp;			         /* fp to use during the call.  */
  void *data;			         /* Placeholder for additional data */
} xref_flag_table;
