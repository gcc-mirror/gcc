/* d-incpath.cc -- Set up combined import paths for the D frontend.
   Copyright (C) 2006-2025 Free Software Foundation, Inc.

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
<http://www.gnu.org/licenses/>.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"

#include "dmd/globals.h"
#include "d-frontend.h"

#include "cppdefault.h"

/* Look for directories that start with the standard prefix.
   "Translate" them, i.e: replace /usr/local/lib/gcc with
   IPREFIX and search them first.  Based on incpath.cc.  */

static char *
prefixed_path (const char *path, const char *iprefix)
{
  if (cpp_relocated () && cpp_PREFIX_len != 0)
  {
    if (!filename_ncmp (path, cpp_PREFIX, cpp_PREFIX_len))
      {
	static const char *relocated_prefix;
	/* If this path starts with the configure-time prefix,
	   but the compiler has been relocated, replace it
	   with the run-time prefix.  */
	if (!relocated_prefix)
	  {
	    /* Make relative prefix expects the first argument
	       to be a program, not a directory.  */
	    char *dummy = concat (gcc_exec_prefix, "dummy", NULL);
	    relocated_prefix
	      = make_relative_prefix (dummy,
				      cpp_EXEC_PREFIX,
				      cpp_PREFIX);
	    free (dummy);
	  }

	return concat (relocated_prefix, path + cpp_PREFIX_len, NULL);
      }
  }

  if (iprefix && cpp_GCC_INCLUDE_DIR_len != 0)
    {
      if (!filename_ncmp (path, cpp_GCC_INCLUDE_DIR, cpp_GCC_INCLUDE_DIR_len))
	return concat (iprefix, path + cpp_GCC_INCLUDE_DIR_len, NULL);
    }

  return xstrdup (path);
}

/* Add PATHS to the global import lookup path.  */

static void
add_globalpaths (Strings &paths)
{
  for (size_t i = 0; i < paths.length; i++)
    {
      const char *path = paths[i];
      const char *target = lrealpath (path);

      if (target == NULL || !FileName::exists (target))
	{
	  if (target)
	    free (CONST_CAST (char *, target));
	  continue;
	}

      global.path.push (target);
    }
}

/* Add PATHS to the global file import lookup path.  */

static void
add_filepaths (Strings &paths)
{
  for (size_t i = 0; i < paths.length; i++)
    {
      const char *path = paths[i];
      const char *target = lrealpath (path);

      if (!FileName::exists (target))
	{
	  free (CONST_CAST (char *, target));
	  continue;
	}

      global.filePath.push (target);
    }
}

/* Add all search directories to compiler runtime.
   if STDINC, also include standard library paths.  */

void
add_import_paths (const char *iprefix, const char *imultilib, bool stdinc)
{
  if (stdinc)
    {
      for (const default_include *p = cpp_include_defaults; p->fname; p++)
	{
	  char *path;

	  /* Ignore C++ paths.  */
	  if (p->cplusplus)
	    continue;

	  if (!p->add_sysroot)
	    path = prefixed_path (p->fname, iprefix);
	  else
	    path = xstrdup (p->fname);

	  /* Add D-specific suffix.  */
	  path = concat (path, "/d", NULL);

	  /* Ignore duplicate entries.  */
	  bool found = false;
	  for (size_t i = 0; i < global.params.imppath.length; i++)
	    {
	      if (strcmp (path, global.params.imppath[i].path) == 0)
		{
		  found = true;
		  break;
		}
	    }

	  if (found)
	    {
	      free (path);
	      continue;
	    }

	  /* Multilib support.  */
	  if (imultilib)
	    {
	      char *target_path = concat (path, "/", imultilib, NULL);
	      global.params.imppath.shift (target_path);
	    }

	  global.params.imppath.shift (path);
	}
    }

  /* Add import search paths.  */
  for (size_t i = 0; i < global.params.imppath.length; i++)
    {
      const char *path = global.params.imppath[i].path;
      if (path)
	{
	  Strings array;
	  FileName::appendSplitPath (path, array);
	  add_globalpaths (array);
	}
    }

  /* Add string import search paths.  */
  for (size_t i = 0; i < global.params.fileImppath.length; i++)
    {
      const char *path = global.params.fileImppath[i];
      if (path)
	{
	  Strings array;
	  FileName::appendSplitPath (path, array);
	  add_filepaths (array);
	}
    }
}
