/* Definitions for the cross reference backend xref.c
   Copyright (C) 1999 Free Software Foundation, Inc.
   Contributed by Alexandre Petit-Bianco (apbianco@cygnus.com)

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

/* Exported functions. */
int xref_flag_value PROTO ((char *));
void expand_xref PROTO ((tree));
void xref_set_data PROTO ((int, void *));
void *xref_get_data PROTO ((int));
void xref_set_current_fp PROTO ((FILE *));

/* flag_emit_xref range of possible values. */

enum {
  XREF_NONE = 0
};

/* Lookup table to be used with the value of flag_emit_xref */

typedef struct {
  char *key;			         /* Activator in -fxref=<key>  */
  void (*expand) PROTO ((FILE *, tree)); /* Function to write xrefs out */
  FILE *fp;			         /* fp to use during the call.  */
  void *data;			         /* Placeholder for additional data */
} xref_flag_table;

#define XREF_GET_DATA(FLAG, T) ((T)xref_get_data (FLAG))
