/* Definitions for C++ name lookup routines.
   Copyright (C) 2003 Free Software Foundation, Inc.
   Contributed by Gabriel Dos Reis <gdr@integrable-solutions.net>

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
Boston, MA 02111-1307, USA.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "tree.h"
#include "cp-tree.h"
#include "name-lookup.h"

/* A free list of "cxx_binding"s, connected by their PREVIOUS.  */
static GTY((deletable (""))) cxx_binding *free_bindings;

/* (GC)-allocate a binding object with VALUE and TYPE member initialized.  */
cxx_binding *
cxx_binding_make (tree value, tree type)
{
  cxx_binding *binding;
  if (free_bindings)
    {
      binding = free_bindings;
      free_bindings = binding->previous;
    }
  else
    binding = ggc_alloc (sizeof (cxx_binding));

  binding->value = value;
  binding->type = type;

  return binding;
}

/* Put BINDING back on the free list.  */
void
cxx_binding_free (cxx_binding *binding)
{
  binding->previous = free_bindings;
  free_bindings = binding;
}
