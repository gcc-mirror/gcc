
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

static GTY(()) tree declared_module;
static bool is_interface; // Probably change for file handle?
static GTY(()) tree proclaimer;
static location_t module_location;
static int export_depth; /* -1 for singleton export.  */

/* The set of imported modules.  The current declared module is
   included in this set too.  */
static GTY(()) hash_set<tree> *imported_modules;

/* Nest a module export level.  Return true if we were already in a
   level.  */

int
push_module_export (bool singleton, tree proclaiming)
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

static bool
import_add (tree name)
{
  if (!imported_modules)
    imported_modules = new hash_set<tree>;
  return imported_modules->add (name);
}

/* Import the module NAME into the current TU.  Return true on
   success.  */

bool
import_module (location_t loc, tree name, tree attrs)
{
  if (import_add (name))
    {
      /* Already imported or interface declared.  */
      if (module_interface_p () && name == declared_module)
	error_at (loc, "module %qE already declared as interface", name);
      return true;
    }

  // Path search along the -I path
  // FIXME: some code needed here
  // FIXME: Think about make dependency generation
  return true;
}

/* Import the module NAME into the current TU and re-export it.  */

void
export_module (location_t loc, tree name, tree attrs)
{
  if (!import_module (loc, name, attrs))
    return;

  if (!declared_module)
    /* We've already emitted an error about this.  */
    return;

  // FIXME: some code needed here
}

/* Declare the name of the current module to be NAME. ATTRS is used to
   determine if this is the interface or not.  */

void
declare_module (location_t loc, tree name, tree attrs)
{
  if (declared_module)
    {
      error_at (loc, "module %qE already declared", name);
      inform (module_location, "existing declaration");
      return;
    }

  /* Look for 'interface' attribute.  There's no point caching the
     identifier, because module declaration occurs at most once.  */
  bool inter = lookup_attribute ("interface", attrs) != NULL_TREE;

  if (!inter)
    {
      // FIXME: Command line switches or file suffix check?
    }

  if (!inter)
    import_module (loc, name, attrs);
  else if (import_add (name))
    {
      /* It was already in the import map.  */
      error_at (loc, "module %qE already imported", name);
      return;
    }

  declared_module = name;
  module_location = loc;

  if (!inter)
    return;

  is_interface = true;

  // FIXME:Open the module file
  // FIXME:Write header
  // FIXME:Write 'important' flags etc
}

/* Finalize the module at end of parsing.  */

void
finish_module ()
{
  delete imported_modules;
  imported_modules = NULL;
  
  if (!declared_module)
    return;

  // FIXME: flush and close the module file
}

#include "gt-cp-module.h"
