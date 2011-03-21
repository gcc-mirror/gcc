/* go-backend.c -- Go frontend interface to gcc backend.
   Copyright (C) 2010, 2011 Free Software Foundation, Inc.

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
#include "tm.h"
#include "rtl.h"
#include "tree.h"
#include "tm_p.h"
#include "target.h"

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
    tree field ATTRIBUTE_UNUSED;
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

/* This is called by the Go frontend proper if the unsafe package was
   imported.  When that happens we can not do type-based alias
   analysis.  */

void
go_imported_unsafe (void)
{
  flag_strict_aliasing = false;

  /* This is a real hack.  init_varasm_once has already grabbed an
     alias set, which we don't want when we aren't doing strict
     aliasing.  We reinitialize to make it do it again.  This should
     be OK in practice since we haven't really done anything yet.  */
  init_varasm_once ();

  /* Let the backend know that the options have changed.  */
  targetm.override_options_after_change ();
}
