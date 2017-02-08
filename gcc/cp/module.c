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

/* Mangling for module files.  */
#define MOD_FNAME_PFX "g++-"
#define MOD_FNAME_SFX ".nms" /* New Module System.  Honest.  */
#define MOD_FNAME_DOT '-'

/* Mangling for module symbol.  */
#define MOD_SYM_PFX "_M"
#if !defined (NO_DOT_IN_LABEL)
#define MOD_SYM_DOT '.'
#elif !defined (NO_DOLLAR_IN_LABEL)
#define MOD_SYM_DOT '$'
#else
#define MOD_SYM_DOT '_'
#endif

static FILE *mstream;
static char *mfname;
static GTY(()) tree module_namespace_name;
static GTY(()) tree module_name;
static location_t module_loc;
static GTY(()) tree proclaimer;
static bool is_interface; // Probably change for file handle?
static int export_depth; /* -1 for singleton export.  */

/* The set of imported modules.  The current declared module is
   included in this set too.

   Use a flat hash set until proven inefficient.  Might want to map to
   first import location?  */
static GTY(()) hash_set<tree> *imported_modules;

void
push_module_namespace ()
{
  gcc_assert (TREE_CODE (current_scope ()) == NAMESPACE_DECL);
  if (module_name && push_namespace (module_namespace_name) < 0)
    {
      NAMESPACE_MODULE_P (current_namespace) = true;
      make_namespace_inline ();
    }
}

void
pop_module_namespace ()
{
  gcc_assert (TREE_CODE (current_scope ()) == NAMESPACE_DECL);
  if (NAMESPACE_MODULE_P (current_namespace))
    pop_namespace ();
}

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
  return module_name;
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

/* Convert a module name into a file name.  The name is malloced.

   (for the moment) this replaces '.' with '-' adds a prefix and
   suffix.

   FIXME: Add host-applicable hooks.  */

static char *
module_to_ext (tree id, const char *pfx, const char *sfx, char dot)
{
  char *name = concat (pfx, IDENTIFIER_POINTER (id), sfx, NULL);
  char *ptr = name + strlen (pfx);
  size_t len = IDENTIFIER_LENGTH (id);

  if (dot != '.')
    for (; len--; ptr++)
      if (*ptr == '.')
	*ptr = dot;

  return name;
}

static char *
module_to_filename (tree id)
{
  return module_to_ext (id, MOD_FNAME_PFX, MOD_FNAME_SFX, MOD_FNAME_DOT);
}

/* Import the module NAME into the current TU.  Return true on
   success.  */

bool
import_module (location_t loc, tree name, tree attrs)
{
  if (import_add (name))
    {
      /* Already imported or interface declared.  */
      if (module_interface_p () && name == module_name)
	error_at (loc, "module %qE already declared as interface", name);
      return true;
    }

  // FIXME:Path search along the -I path
  char *fname = module_to_filename (name);
  FILE *fd = fopen (fname, "rb");
  if (!fd)
    {
      error_at (loc, "cannot find module %qE (%qs): %m",
		name, fname);
      free (fname);
      return false;
    }

  // FIXME: some code needed here
  // FIXME: Think about make dependency generation

  fclose (fd);
  free (fname);
  return true;
}

/* Import the module NAME into the current TU and re-export it.  */

void
export_module (location_t loc, tree name, tree attrs)
{
  if (!import_module (loc, name, attrs))
    return;

  if (!mstream)
    /* We've already emitted an error about this.  */
    return;

  // FIXME: some code needed here
}

/* Declare the name of the current module to be NAME. ATTRS is used to
   determine if this is the interface or not.  */

void
declare_module (location_t loc, tree name, tree attrs)
{
  if (module_name)
    {
      error_at (loc, "module %qE already declared", name);
      inform (module_loc, "existing declaration");
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

  module_name = name;
  module_loc = loc;
  char *sym = module_to_ext (name, MOD_SYM_PFX, NULL, MOD_SYM_DOT);
  module_namespace_name = get_identifier (sym);
  free (sym);

  push_module_namespace ();
  if (!inter)
    return;

  is_interface = true;

  // FIXME:option to specify location? take dirname from output file?
  mfname = module_to_filename (name);
  mstream = fopen (mfname, "wb");
  if (!mstream)
    {
      error_at (module_loc, "cannot open module interface %qE (%qs): %m",
		module_name, mfname);
      return;
    }

  // FIXME:Write header
  // FIXME:Write 'important' flags etc
}

/* Finalize the module at end of parsing.  */

void
finish_module ()
{
  delete imported_modules;
  imported_modules = NULL;
  
  if (!mstream)
    return;

  // FIXME: flush the module file?
  if (fclose (mstream))
    error_at (module_loc, "error closing module interface %qE (%qs): %m",
	      module_name, mfname);
  if (errorcount)
    unlink (mfname);
  mstream = NULL;
  free (mfname);
  mfname = NULL;
}

#include "gt-cp-module.h"
