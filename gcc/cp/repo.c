/* Code to maintain a C++ template repository.
   Copyright (C) 1995, 1996, 1997, 1998, 2000, 2001 Free Software Foundation, Inc.
   Contributed by Jason Merrill (jason@cygnus.com)

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

/* My strategy here is as follows:

   Everything should be emitted in a translation unit where it is used.
   The results of the automatic process should be easily reproducible with
   explicit code.  */

#include "config.h"
#include "system.h"
#include "tree.h"
#include "cp-tree.h"
#include "input.h"
#include "obstack.h"
#include "toplev.h"
#include "ggc.h"
#include "diagnostic.h"

static tree repo_get_id PARAMS ((tree));
static char *extract_string PARAMS ((char **));
static const char *get_base_filename PARAMS ((const char *));
static void open_repo_file PARAMS ((const char *));
static char *afgets PARAMS ((FILE *));
static void reopen_repo_file_for_write PARAMS ((void));

static GTY(()) tree pending_repo;
static GTY(()) tree original_repo;
static char *repo_name;
static FILE *repo_file;

static const char *old_args, *old_dir, *old_main;

static struct obstack temporary_obstack;

#define IDENTIFIER_REPO_USED(NODE)   (TREE_LANG_FLAG_3 (NODE))
#define IDENTIFIER_REPO_CHOSEN(NODE) (TREE_LANG_FLAG_4 (NODE))

#if 0
/* Record the flags used to compile this translation unit.  */

void
repo_compile_flags (argc, argv)
     int argc;
     char **argv;
{
}

/* If this template has not been seen before, add a note to the repository
   saying where the declaration was.  This may be used to find the
   definition at link time.  */

void
repo_template_declared (t)
     tree t;
{}

/* Note where the definition of a template lives so that instantiations can
   be generated later.  */

void
repo_template_defined (t)
     tree t;
{}

/* Note where the definition of a class lives to that template
   instantiations can use it.  */

void
repo_class_defined (t)
     tree t;
{}
#endif

static tree
repo_get_id (t)
     tree t;
{
  if (TYPE_P (t))
    {
      tree vtable;

      /* If we're not done setting up the class, we may not have set up
	 the vtable, so going ahead would give the wrong answer.
         See g++.pt/instantiate4.C.  */
      if (!COMPLETE_TYPE_P (t) || TYPE_BEING_DEFINED (t))
	abort ();

      vtable = get_vtbl_decl_for_binfo (TYPE_BINFO (t));

      t = vtable;
      if (t == NULL_TREE)
	return t;
    }
  return DECL_ASSEMBLER_NAME (t);
}

/* Note that a template has been used.  If we can see the definition, offer
   to emit it.  */

void
repo_template_used (t)
     tree t;
{
  tree id;

  if (! flag_use_repository)
    return;

  id = repo_get_id (t);
  if (id == NULL_TREE)
    return;
  
  if (TYPE_P (t))
    {
      if (IDENTIFIER_REPO_CHOSEN (id))
	mark_class_instantiated (t, 0);
    }
  else if (DECL_P (t))
    {
      if (IDENTIFIER_REPO_CHOSEN (id))
	/* It doesn't make sense to instantiate a clone, so we
	   instantiate the cloned function instead.  Note that this
	   approach will not work correctly if collect2 assigns
	   different clones to different files -- but it shouldn't.  */
	mark_decl_instantiated (DECL_CLONED_FUNCTION_P (t)
				? DECL_CLONED_FUNCTION (t) : t, 
				0);
    }
  else
    abort ();

  if (! IDENTIFIER_REPO_USED (id))
    {
      IDENTIFIER_REPO_USED (id) = 1;
      pending_repo = tree_cons (NULL_TREE, id, pending_repo);
    }
}

#if 0
/* Note that the vtable for a class has been used, and offer to emit it.  */

static void
repo_vtable_used (t)
     tree t;
{
  if (! flag_use_repository)
    return;

  pending_repo = tree_cons (NULL_TREE, t, pending_repo);
}

/* Note that an inline with external linkage has been used, and offer to
   emit it.  */

void
repo_inline_used (fn)
     tree fn;
{
  if (! flag_use_repository)
    return;

  /* Member functions of polymorphic classes go with their vtables.  */
  if (DECL_FUNCTION_MEMBER_P (fn) 
      && TYPE_POLYMORPHIC_P (DECL_CONTEXT (fn)))
    {
      repo_vtable_used (DECL_CONTEXT (fn));
      return;
    }

  pending_repo = tree_cons (NULL_TREE, fn, pending_repo);
}

/* Note that a particular typeinfo node has been used, and offer to
   emit it.  */

void
repo_tinfo_used (ti)
     tree ti;
{
}
#endif

void
repo_template_instantiated (t, extern_p)
     tree t;
     int extern_p;
{
  if (! extern_p)
    {
      tree id = repo_get_id (t);
      if (id)
	IDENTIFIER_REPO_CHOSEN (id) = 1;
    }
}

/* Parse a reasonable subset of shell quoting syntax.  */

static char *
extract_string (pp)
     char **pp;
{
  char *p = *pp;
  int backquote = 0;
  int inside = 0;

  for (;;)
    {
      char c = *p;
      if (c == '\0')
	break;
      ++p;
      if (backquote)
	obstack_1grow (&temporary_obstack, c);
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
  return obstack_finish (&temporary_obstack);
}

const char *
get_base_filename (filename)
     const char *filename;
{
  char *p = getenv ("COLLECT_GCC_OPTIONS");
  char *output = NULL;
  int compiling = 0;

  while (p && *p)
    {
      char *q = extract_string (&p);

      if (strcmp (q, "-o") == 0)
	output = extract_string (&p);
      else if (strcmp (q, "-c") == 0)
	compiling = 1;
      }

  if (compiling && output)
    return output;

  if (p && ! compiling)
    {
      warning ("-frepo must be used with -c");
      flag_use_repository = 0;
      return NULL;
    }

  return lbasename (filename);
}        

static void
open_repo_file (filename)
     const char *filename;
{
  register const char *p;
  const char *s = get_base_filename (filename);

  if (s == NULL)
    return;

  p = lbasename (s);
  p = strrchr (p, '.');
  if (! p)
    p = s + strlen (s);

  repo_name = xmalloc (p - s + 5);
  memcpy (repo_name, s, p - s);
  memcpy (repo_name + (p - s), ".rpo", 5);

  repo_file = fopen (repo_name, "r");
}

static char *
afgets (stream)
     FILE *stream;
{
  int c;
  while ((c = getc (stream)) != EOF && c != '\n')
    obstack_1grow (&temporary_obstack, c);
  if (obstack_object_size (&temporary_obstack) == 0)
    return NULL;
  obstack_1grow (&temporary_obstack, '\0');
  return obstack_finish (&temporary_obstack);
}

void
init_repo (filename)
     const char *filename;
{
  char *buf;

  if (! flag_use_repository)
    return;

  gcc_obstack_init (&temporary_obstack);

  open_repo_file (filename);

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
	case 'C':
	case 'O':
	  {
	    tree id = get_identifier (buf + 2);
	    tree orig;

	    if (buf[0] == 'C')
	      {
		IDENTIFIER_REPO_CHOSEN (id) = 1;
		orig = integer_one_node;
	      }
	    else
	      orig = NULL_TREE;

	    original_repo = tree_cons (orig, id, original_repo);
	  }
	  break;
	default:
	  error ("mysterious repository information in %s", repo_name);
	}
      obstack_free (&temporary_obstack, buf);
    }
}

static void
reopen_repo_file_for_write ()
{
  if (repo_file)
    fclose (repo_file);
  repo_file = fopen (repo_name, "w");

  if (repo_file == 0)
    {
      error ("can't create repository information file `%s'", repo_name);
      flag_use_repository = 0;
    }
}

/* Emit any pending repos.  */

void
finish_repo ()
{
  tree t;
  int repo_changed = 0;
  char *dir, *args;

  if (! flag_use_repository)
    return;

  /* Do we have to write out a new info file?  */

  /* Are there any old templates that aren't used any longer or that are
     newly chosen?  */
  
  for (t = original_repo; t; t = TREE_CHAIN (t))
    {
      if (! IDENTIFIER_REPO_USED (TREE_VALUE (t))
	  || (! TREE_PURPOSE (t) && IDENTIFIER_REPO_CHOSEN (TREE_VALUE (t))))
	{
	  repo_changed = 1;
	  break;
	}
      IDENTIFIER_REPO_USED (TREE_VALUE (t)) = 0;
    }

  /* Are there any templates that are newly used?  */
  
  if (! repo_changed)
    for (t = pending_repo; t; t = TREE_CHAIN (t))
      {
	if (IDENTIFIER_REPO_USED (TREE_VALUE (t)))
	  {
	    repo_changed = 1;
	    break;
	  }
      }

  dir = getpwd ();
  args = getenv ("COLLECT_GCC_OPTIONS");

  if (! repo_changed && pending_repo)
    if (strcmp (old_main, main_input_filename) != 0
	|| strcmp (old_dir, dir) != 0
	|| (args == NULL) != (old_args == NULL)
	|| (args && strcmp (old_args, args) != 0))
      repo_changed = 1;

  if (! repo_changed || errorcount || sorrycount)
    goto out;

  reopen_repo_file_for_write ();

  if (repo_file == 0)
    goto out;

  fprintf (repo_file, "M %s\n", main_input_filename);
  fprintf (repo_file, "D %s\n", dir);
  if (args)
    fprintf (repo_file, "A %s\n", args);

  for (t = pending_repo; t; t = TREE_CHAIN (t))
    {
      tree val = TREE_VALUE (t);
      char type = IDENTIFIER_REPO_CHOSEN (val) ? 'C' : 'O';

      fprintf (repo_file, "%c %s\n", type, IDENTIFIER_POINTER (val));
    }

 out:
  if (repo_file)
    fclose (repo_file);
}

#include "gt-cp-repo.h"
