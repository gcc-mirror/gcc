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
#include "dumpfile.h"

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

static GTY(()) tree module_namespace_name;
static GTY(()) tree module_name;
static location_t module_loc;
static GTY(()) tree proclaimer;
static bool is_interface; // Probably change for file handle?
static int export_depth; /* -1 for singleton export.  */

/* The set of imported modules.  The current declared module is
   included in this set too.  Maps to an import_kind.  */
static GTY(()) hash_map<tree, unsigned> *imported_modules;
enum import_kind
{
  ik_import,  /* Regular import.  */
  ik_export,  /* Exported import.  */
  ik_impl,    /* The implementation */
  ik_inter    /* The interface.  */
};

/* Lazily open the dumping stream, if enabled. */
static inline FILE *
dopen ()
{
  return dump_begin (TDI_lang, NULL);
}

static inline void
dclose (FILE *stream)
{
  if (stream)
    dump_end (TDI_lang, stream);
}

/* If we're in the purview of a module, push its local namespace.  */

void
push_module_namespace ()
{
  gcc_assert (TREE_CODE (current_scope ()) == NAMESPACE_DECL);
  if (module_namespace_name && push_namespace (module_namespace_name) < 0)
    {
      MODULE_NAMESPACE_P (current_namespace) = true;
      make_namespace_inline ();
    }
}

/* If we're in the current module's local namespace, pop out of it.  */

void
pop_module_namespace ()
{
  gcc_assert (TREE_CODE (current_scope ()) == NAMESPACE_DECL);
  if (CURRENT_MODULE_NAMESPACE_P (current_namespace))
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


static void
read_module (FILE *stream, tree name)
{
  FILE *d =  dopen ();
  
  if (d)
    fprintf (d, "importing '%s'\n", IDENTIFIER_POINTER (name));
  
  // FIXME: some code needed here
  dclose (d);
}

/* Import the module NAME into the current TU. */

static void
do_import_module (location_t loc, tree name, tree attrs, import_kind kind)
{
  if (!imported_modules)
    imported_modules = new hash_map<tree, unsigned>;

  bool existed;
  unsigned *val = &imported_modules->get_or_insert (name, &existed);

  if (!existed)
    *val = kind;
  else
    {
      if (*val >= ik_impl)
	{
	  error_at (loc, "already declared as module %qE", name);
	  return;
	}
      else if (kind >= ik_impl)
	{
	  error_at (loc, "module %qE already imported", name);
	  return;
	}

      if (*val < kind)
	*val = kind;
    }
  if (kind == ik_inter)
    return;

  // FIXME:Path search along the -I path
  // FIXME: Think about make dependency generation
  char *fname = module_to_filename (name);
  FILE *stream = fopen (fname, "rb");

  if (!stream)
    error_at (loc, "cannot find module %qE (%qs): %m", name, fname);
  else
    {
      read_module (stream, name);
      fclose (stream);
    }
  free (fname);
  return;
}

void
import_module (location_t loc, tree name, tree attrs)
{
  do_import_module (loc, name, attrs, ik_import);
}

/* Import the module NAME into the current TU and re-export it.  */

void
export_module (location_t loc, tree name, tree attrs)
{
  do_import_module (loc, name, attrs, ik_export);
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

  module_name = name;
  module_loc = loc;
  char *sym = module_to_ext (name, MOD_SYM_PFX, NULL, MOD_SYM_DOT);
  module_namespace_name = get_identifier (sym);
  free (sym);

  do_import_module (loc, name, attrs, inter ? ik_inter : ik_impl);

  push_module_namespace ();
  is_interface = inter;
}

static void
write_module (FILE *stream)
{
  FILE *d = dopen ();

  if (d)
    fprintf (d, "writing module '%s'\n", IDENTIFIER_POINTER (module_name));
  
  // FIXME:Write header
  // FIXME:Write 'important' flags etc
  // FIXME:Write import table

  dclose (d);
}

/* Finalize the module at end of parsing.  */

void
finish_module ()
{
  if (is_interface)
    {
      // FIXME:option to specify location? take dirname from output file?
      char *fname = module_to_filename (module_name);

      if (!errorcount)
	{
	  FILE *stream = fopen (fname, "wb");

	  if (!stream)
	    error_at (module_loc, "cannot open module interface %qE (%qs): %m",
		      module_name, fname);
	  else
	    {
	      write_module (stream);
	      // FIXME: flush the module file?
	      if (fclose (stream))
		error_at (module_loc,
			  "error closing module interface %qE (%qs): %m",
			  module_name, fname);
	    }
	}
      if (errorcount)
	unlink (fname);
      free (fname);
    }

  delete imported_modules;
  imported_modules = NULL;
}

#include "gt-cp-module.h"
