/* Write cross reference information extracted from Java(TM)
   source and bytecode files, in one of formats documented below.
   Copyright (C) 1999, 2000 Free Software Foundation, Inc.
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

#include "config.h"
#include "system.h"
#include "tree.h"
#include "java-tree.h"
#include "xref.h"
#include "jcf.h"
#include "parse.h"

static xref_flag_table xref_table [] = {
  {NULL, NULL, NULL, NULL},
};

/* Decode an xref flag value. Return 0 if the flag wasn't found. */

int
xref_flag_value (flag)
     const char *flag;
{
  int i;
  for (i = 0; xref_table [i].key; i++)
    if (!strcmp (flag, xref_table [i].key))
      return i+1;
  return 0;
}

void
xref_set_data (flag, data)
     int flag;
     void *data;
{
  xref_table [flag-1].data = data;
}

void *
xref_get_data (flag)
     int flag;
{
  return xref_table [flag-1].data;
}

void
xref_set_current_fp (fp)
     FILE *fp;
{
  xref_table [flag_emit_xref-1].fp = fp;
}

/* Branch to the right xref "back-end".  */

void
expand_xref (node)
     tree node;
{
  /* Maintain these two cached. */
  static FILE *fp = NULL;
  static void (*current_expand) PARAMS ((FILE *, tree)) = NULL;

  if ( !flag_emit_xref )
    return;

  if (!fp)
    fp = xref_table [flag_emit_xref-1].fp;
  if (!current_expand)
    current_expand = xref_table [flag_emit_xref-1].expand;

  (*current_expand) (fp, node);
}

/* Implementation of the xref back-ends. */

