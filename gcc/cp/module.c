
/* -*- C++ -*- modules.  Experimental!
   Copyright (C) 2017 Free Software Foundation, Inc.
   Written by Nathan Sidwell <nathan@acm.org>

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   GCC is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "cp-tree.h"
#include "stringpool.h"

static module_name_t GTY(()) *declared_module;
static bool is_interface; // Probably change for file handle?
static module_name_t GTY(()) *proclaimer;
static location_t module_location;
static int export_depth; /* -1 for singleton export.  */

/* Nest a module export level.  Return true if we were already in a
   level.  */

int
push_module_export (bool singleton, module_name_t *proclaiming)
{
  int previous = export_depth;

  if (proclaiming)
    {
      proclaimer = proclaimer;
      export_depth = -2;
    }
  else if (singleton)
    export_depth = -1;
  else
    export_depth = +1;
  return previous;
}

/* Unnest a module export level.  */

void
pop_module_export (int previous)
{
  proclaimer = NULL;
  export_depth = previous;
}

int
module_exporting_level ()
{
  return export_depth;
}

/* Return true iff we're in the purview of a named module.  */

bool
module_purview_p ()
{
  return declared_module;
}

/* Return true iff we're the interface TU (this also means we're in a
   module perview.  */

bool
module_interface_p ()
{
  return is_interface;
}

/* Declare the name of the current module to be NAME. ATTRS is used to
   determine if this is the interface or not.  */

void
declare_module (location_t loc, module_name_t *name, tree attrs)
{
  if (declared_module)
    {
      error_at (loc, "module already declared");
      inform (module_location, "existing declaration");
      return;
    }

  /* Look for 'interface' attribute.  There's no point caching the
     identifier, because module declaration occurs at most once.  */
  if (lookup_attribute ("interface", attrs))
    is_interface = true;
  else
    {
      // FIXME: Command line switches or file suffix check?
    }

  declared_module = name;
  module_location = loc;
}

/* Import the module NAME into the current TU.  */

void
import_module (location_t loc, module_name_t *name, tree attrs)
{
  // FIXME: some code needed here
}

/* Import the module NAME into the current TU and re-export it.  */

void
export_module (location_t loc, module_name_t *name, tree attrs)
{
  // FIXME: some code needed here
}
