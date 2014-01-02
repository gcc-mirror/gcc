/* Functions for handling dependency tracking when reading .class files.

   Copyright (C) 1998-2014 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  

Java and all Java-based marks are trademarks or registered trademarks
of Sun Microsystems, Inc. in the United States and other countries.
The Free Software Foundation is independent of Sun Microsystems, Inc.  */

/* Written by Tom Tromey <tromey@cygnus.com>, October 1998.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "mkdeps.h"

#include "jcf.h"



/* The dependency structure used for this invocation.  */
struct deps *dependencies;

/* The output file, or NULL if we aren't doing dependency tracking.  */
static FILE *dep_out = NULL;

/* Nonzero if system files should be added.  */
static int system_files;

/* Nonzero if we are dumping out dummy dependencies.  */
static int print_dummies;



/* Call this to reset the dependency module.  This is required if
   multiple dependency files are being generated from a single tool
   invocation.  FIXME: we should change our API or just completely use
   the one in mkdeps.h.  */
void
jcf_dependency_reset (void)
{
  if (dep_out != NULL)
    {
      if (dep_out != stdout)
	fclose (dep_out);
      dep_out = NULL;
    }

  if (dependencies != NULL)
    {
      deps_free (dependencies);
      dependencies = NULL;
    }
}

void
jcf_dependency_set_target (const char *name)
{
  /* We just handle this the same as an `add_target'.  */
  if (dependencies != NULL && name != NULL)
    deps_add_target (dependencies, name, 1);
}

void
jcf_dependency_add_target (const char *name)
{
  if (dependencies != NULL)
    deps_add_target (dependencies, name, 1);
}

void
jcf_dependency_set_dep_file (const char *name)
{
  gcc_assert (dep_out != stdout);
  if (dep_out)
    fclose (dep_out);
  if (! strcmp (name, "-"))
    dep_out = stdout;
  else
    dep_out = fopen (name, "w");
}

void
jcf_dependency_add_file (const char *filename ATTRIBUTE_UNUSED, int system_p)
{
  if (! dependencies)
    return;

  /* Just omit system files.  */
  if (system_p && ! system_files)
    return;


  /* FIXME: Don't emit any dependencies.  In many cases we'll just see
     temporary files emitted by ecj... */
  /* deps_add_dep (dependencies, filename); */
}

void
jcf_dependency_init (int system_p)
{
  gcc_assert (! dependencies);
  system_files = system_p;
  dependencies = deps_init ();
}

void
jcf_dependency_print_dummies (void)
{
  print_dummies = 1;
}

void
jcf_dependency_write (void)
{
  if (! dep_out)
    return;

  gcc_assert (dependencies);

  deps_write (dependencies, dep_out, 72);
  if (print_dummies)
    deps_phony_targets (dependencies, dep_out);
  fflush (dep_out);
}
