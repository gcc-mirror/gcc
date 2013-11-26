/* Code to maintain a C++ template repository.
   Copyright (C) 1995-2013 Free Software Foundation, Inc.
   Contributed by Jason Merrill (jason@cygnus.com)

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
<http://www.gnu.org/licenses/>.  */

/* My strategy here is as follows:

   Everything should be emitted in a translation unit where it is used.
   The results of the automatic process should be easily reproducible with
   explicit code.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "tree.h"
#include "stringpool.h"
#include "cp-tree.h"
#include "input.h"
#include "obstack.h"
#include "toplev.h"
#include "diagnostic-core.h"
#include "flags.h"

static const char *extract_string (const char **);
static const char *get_base_filename (const char *);
static FILE *open_repo_file (const char *);
static char *afgets (FILE *);
static FILE *reopen_repo_file_for_write (void);

static GTY(()) vec<tree, va_gc> *pending_repo;
static char *repo_name;

static const char *old_args, *old_dir, *old_main;

static struct obstack temporary_obstack;
static bool temporary_obstack_initialized_p;

/* Parse a reasonable subset of shell quoting syntax.  */

static const char *
extract_string (const char **pp)
{
  const char *p = *pp;
  int backquote = 0;
  int inside = 0;

  for (;;)
    {
      char c = *p;
      if (c == '\0')
	break;
      ++p;
      if (backquote)
	{
	  obstack_1grow (&temporary_obstack, c);
	  backquote = 0;
	}
      else if (! inside && c == ' ')
	break;
      else if (! inside && c == '\\')
	backquote = 1;
      else if (c == '\'')
	inside = !inside;
      else
	obstack_1grow (&temporary_obstack, c);
    }

  obstack_1grow (&temporary_obstack, '\0');
  *pp = p;
  return (char *) obstack_finish (&temporary_obstack);
}

static const char *
get_base_filename (const char *filename)
{
  const char *p = getenv ("COLLECT_GCC_OPTIONS");
  const char *output = NULL;
  int compiling = 0;

  while (p && *p)
    {
      const char *q = extract_string (&p);

      if (strcmp (q, "-o") == 0)
	{
	  if (flag_compare_debug)
	    /* Just in case aux_base_name was based on a name with two
	       or more '.'s, add an arbitrary extension that will be
	       stripped by the caller.  */
	    output = concat (aux_base_name, ".o", NULL);
	  else
	    output = extract_string (&p);
	}
      else if (strcmp (q, "-c") == 0)
	compiling = 1;
    }

  if (compiling && output)
    return output;

  if (p && ! compiling)
    {
      warning (0, "-frepo must be used with -c");
      flag_use_repository = 0;
      return NULL;
    }

  return lbasename (filename);
}

static FILE *
open_repo_file (const char *filename)
{
  const char *p;
  const char *s = get_base_filename (filename);

  if (s == NULL)
    return NULL;

  p = lbasename (s);
  p = strrchr (p, '.');
  if (! p)
    p = s + strlen (s);

  repo_name = XNEWVEC (char, p - s + 5);
  memcpy (repo_name, s, p - s);
  memcpy (repo_name + (p - s), ".rpo", 5);

  return fopen (repo_name, "r");
}

static char *
afgets (FILE *stream)
{
  int c;
  while ((c = getc (stream)) != EOF && c != '\n')
    obstack_1grow (&temporary_obstack, c);
  if (obstack_object_size (&temporary_obstack) == 0)
    return NULL;
  obstack_1grow (&temporary_obstack, '\0');
  return (char *) obstack_finish (&temporary_obstack);
}

void
init_repo (void)
{
  char *buf;
  const char *p;
  FILE *repo_file;

  if (! flag_use_repository)
    return;

  /* When a PCH file is loaded, the entire identifier table is
     replaced, with the result that IDENTIFIER_REPO_CHOSEN is cleared.
     So, we have to reread the repository file.  */
  lang_post_pch_load = init_repo;

  if (!temporary_obstack_initialized_p)
    gcc_obstack_init (&temporary_obstack);

  repo_file = open_repo_file (main_input_filename);

  if (repo_file == 0)
    return;

  while ((buf = afgets (repo_file)))
    {
      switch (buf[0])
	{
	case 'A':
	  old_args = ggc_strdup (buf + 2);
	  break;
	case 'D':
	  old_dir = ggc_strdup (buf + 2);
	  break;
	case 'M':
	  old_main = ggc_strdup (buf + 2);
	  break;
	case 'O':
	  /* A symbol that we were able to define the last time this
	     file was compiled.  */
	  break;
	case 'C':
	  /* A symbol that the prelinker has requested that we
	     define.  */
	  {
	    tree id = get_identifier (buf + 2);
	    IDENTIFIER_REPO_CHOSEN (id) = 1;
	  }
	  break;
	default:
	  error ("mysterious repository information in %s", repo_name);
	}
      obstack_free (&temporary_obstack, buf);
    }
  fclose (repo_file);

  if (old_args && !get_random_seed (true)
      && (p = strstr (old_args, "'-frandom-seed=")))
    set_random_seed (extract_string (&p) + strlen ("-frandom-seed="));
}

static FILE *
reopen_repo_file_for_write (void)
{
  FILE *repo_file = fopen (repo_name, "w");

  if (repo_file == 0)
    {
      error ("can%'t create repository information file %qs", repo_name);
      flag_use_repository = 0;
    }

  return repo_file;
}

/* Emit any pending repos.  */

void
finish_repo (void)
{
  tree val;
  char *dir, *args;
  FILE *repo_file;
  unsigned ix;

  if (!flag_use_repository || flag_compare_debug)
    return;

  if (seen_error ())
    return;

  repo_file = reopen_repo_file_for_write ();
  if (repo_file == 0)
    goto out;

  fprintf (repo_file, "M %s\n", main_input_filename);
  dir = getpwd ();
  fprintf (repo_file, "D %s\n", dir);
  args = getenv ("COLLECT_GCC_OPTIONS");
  if (args)
    {
      fprintf (repo_file, "A %s", args);
      /* If -frandom-seed is not among the ARGS, then add the value
	 that we chose.  That will ensure that the names of types from
	 anonymous namespaces will get the same mangling when this
	 file is recompiled.  */
      if (!strstr (args, "'-frandom-seed="))
	fprintf (repo_file, " '-frandom-seed=" HOST_WIDE_INT_PRINT_HEX_PURE "'", 
		 get_random_seed (false));
      fprintf (repo_file, "\n");
    }

  FOR_EACH_VEC_SAFE_ELT_REVERSE (pending_repo, ix, val)
    {
      tree name = DECL_ASSEMBLER_NAME (val);
      char type = IDENTIFIER_REPO_CHOSEN (name) ? 'C' : 'O';
      fprintf (repo_file, "%c %s\n", type, IDENTIFIER_POINTER (name));
    }

 out:
  if (repo_file)
    fclose (repo_file);
}

/* DECL is a FUNCTION_DECL or VAR_DECL with vague linkage whose
   definition is available in this translation unit.  Returns 0 if
   this definition should not be emitted in this translation unit
   because it will be emitted elsewhere.  Returns 1 if the repository
   file indicates that that DECL should be emitted in this translation
   unit, or 2 if the repository file is not in use.  */

int
repo_emit_p (tree decl)
{
  int ret = 0;
  gcc_assert (TREE_PUBLIC (decl));
  gcc_assert (VAR_OR_FUNCTION_DECL_P (decl));
  gcc_assert (!DECL_REALLY_EXTERN (decl));

  /* When not using the repository, emit everything.  */
  if (!flag_use_repository)
    return 2;

  /* Only template instantiations are managed by the repository.  This
     is an artificial restriction; the code in the prelinker and here
     will work fine if all entities with vague linkage are managed by
     the repository.  */
  if (VAR_P (decl))
    {
      tree type = NULL_TREE;
      if (DECL_VTABLE_OR_VTT_P (decl))
	type = DECL_CONTEXT (decl);
      else if (DECL_TINFO_P (decl))
	type = TREE_TYPE (DECL_NAME (decl));
      if (!DECL_TEMPLATE_INSTANTIATION (decl)
	  && (!TYPE_LANG_SPECIFIC (type)
	      || !CLASSTYPE_TEMPLATE_INSTANTIATION (type)))
	return 2;
      /* Const static data members initialized by constant expressions must
	 be processed where needed so that their definitions are
	 available.  Still record them into *.rpo files, so if they
	 weren't actually emitted and collect2 requests them, they can
	 be provided.  */
      if (decl_maybe_constant_var_p (decl)
	  && DECL_CLASS_SCOPE_P (decl))
	ret = 2;
    }
  else if (!DECL_TEMPLATE_INSTANTIATION (decl))
    return 2;

  if (DECL_EXPLICIT_INSTANTIATION (decl))
    return 2;

  /* For constructors and destructors, the repository contains
     information about the clones -- not the original function --
     because only the clones are emitted in the object file.  */
  if (DECL_MAYBE_IN_CHARGE_CONSTRUCTOR_P (decl)
      || DECL_MAYBE_IN_CHARGE_DESTRUCTOR_P (decl))
    {
      int emit_p = 0;
      tree clone;
      /* There is no early exit from this loop because we want to
	 ensure that all of the clones are marked as available in this
	 object file.  */
      FOR_EACH_CLONE (clone, decl)
	/* The only possible results from the recursive call to
	   repo_emit_p are 0 or 1.  */
	if (repo_emit_p (clone))
	  emit_p = 1;
      return emit_p;
    }

  /* Keep track of all available entities.  */
  if (!DECL_REPO_AVAILABLE_P (decl))
    {
      DECL_REPO_AVAILABLE_P (decl) = 1;
      vec_safe_push (pending_repo, decl);
    }

  return IDENTIFIER_REPO_CHOSEN (DECL_ASSEMBLER_NAME (decl)) ? 1 : ret;
}

/* Returns true iff the prelinker has explicitly marked CLASS_TYPE for
   export from this translation unit.  */

bool
repo_export_class_p (const_tree class_type)
{
  if (!flag_use_repository)
    return false;
  if (!CLASSTYPE_VTABLES (class_type))
    return false;
  /* If the virtual table has been assigned to this translation unit,
     export the class.  */
  return (IDENTIFIER_REPO_CHOSEN
	  (DECL_ASSEMBLER_NAME (CLASSTYPE_VTABLES (class_type))));
}

#include "gt-cp-repo.h"
