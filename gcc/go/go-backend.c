/* go-backend.c -- Go frontend interface to gcc backend.
   Copyright (C) 2010 Free Software Foundation, Inc.

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

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tree.h"
#include "tm.h"
#include "tm_p.h"

#include "go-c.h"

/* This file holds all the cases where the Go frontend needs
   information from gcc's backend.  */

/* Return the alignment in bytes of a value of type T.  */

unsigned int
go_type_alignment (tree t)
{
  return TYPE_ALIGN_UNIT (t);
}

/* Return the alignment in bytes of a struct field of type T.  */

unsigned int
go_field_alignment (tree t)
{
  unsigned int v;

  v = TYPE_ALIGN (t);

#ifdef BIGGEST_FIELD_ALIGNMENT
  if (v > BIGGEST_FIELD_ALIGNMENT)
    v = BIGGEST_FIELD_ALIGNMENT;
#endif

#ifdef ADJUST_FIELD_ALIGN
  {
    tree field;
    field = build_decl (UNKNOWN_LOCATION, FIELD_DECL, NULL, t);
    v = ADJUST_FIELD_ALIGN (field, v);
  }
#endif

  return v / BITS_PER_UNIT;
}

/* Return the size and alignment of a trampoline.  */

void
go_trampoline_info (unsigned int *size, unsigned int *alignment)
{
  *size = TRAMPOLINE_SIZE;
  *alignment = TRAMPOLINE_ALIGNMENT;
}
