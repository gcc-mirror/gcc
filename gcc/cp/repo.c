/* Code to maintain a C++ template repository.
   Copyright (C) 1995 Free Software Foundation, Inc.
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
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

/* My strategy here is as follows:

   Everything should be emitted in a translation unit where it is used.
   The results of the automatic process should be easily reproducible with
   explicit code.  */

#include <stdio.h>
#include "config.h"
#include "tree.h"
#include "cp-tree.h"
#include "input.h"

extern char * rindex ();
extern char * getenv ();

static tree pending_repo;
static char repo_name[1024];
static FILE *repo_file;

extern int flag_use_repository;
extern int errorcount, sorrycount;

static int repo_changed;

#define IDENTIFIER_REPO_USED(NODE)   (TREE_LANG_FLAG_3 (NODE))
#define IDENTIFIER_REPO_CHOSEN(NODE) (TREE_LANG_FLAG_4 (NODE))

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

/* Note that a template has been used.  If we can see the definition, offer
   to emit it. */

void
repo_template_used (t)
     tree t;
{
  tree id;

  if (! flag_use_repository)
    return;

  if (TREE_CODE_CLASS (TREE_CODE (t)) == 't')
    {
      id = DECL_ASSEMBLER_NAME (TYPE_MAIN_DECL (t));
      if (IDENTIFIER_REPO_CHOSEN (id))
	mark_class_instantiated (t, 0);
    }
  else if (TREE_CODE_CLASS (TREE_CODE (t)) == 'd')
    {
      id = DECL_ASSEMBLER_NAME (t);
      if (IDENTIFIER_REPO_CHOSEN (id))
	mark_function_instantiated (t, 0);
    }
  else
    my_friendly_abort (1);

  if (! IDENTIFIER_REPO_USED (id))
    {
      repo_changed = 1;
      IDENTIFIER_REPO_USED (id) = 1;
    }
  pending_repo = perm_tree_cons (NULL_TREE, t, pending_repo);
}

/* Note that the vtable for a class has been used, and offer to emit it.  */

void
repo_vtable_used (t)
     tree t;
{
  if (! flag_use_repository)
    return;

  pending_repo = perm_tree_cons (NULL_TREE, t, pending_repo);
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
  if (DECL_FUNCTION_MEMBER_P (fn) && TYPE_VIRTUAL_P (DECL_CLASS_CONTEXT (fn)))
    {
      repo_vtable_used (DECL_CLASS_CONTEXT (fn));
      return;
    }

  pending_repo = perm_tree_cons (NULL_TREE, fn, pending_repo);
}

/* Note that a particular typeinfo node has been used, and offer to
   emit it.  */

void
repo_tinfo_used (ti)
     tree ti;
{
}

static char *
save_string (s, len)
     char *s;
     int len;
{
  register char *result = xmalloc (len + 1);

  bcopy (s, result, len);
  result[len] = 0;
  return result;
}

static char *
get_base_filename (filename)
     char *filename;
{
  char *p = getenv ("COLLECT_GCC_OPTIONS");
  char *output = 0;
  int compiling = 0;

  if (p)
    while (*p)
      {
	char *q = p;
	while (*q && *q != ' ') q++;
	if (*p == '-' && p[1] == 'o')
	  {
	    p += 2;
	    if (p == q)
	      {
		p++; q++;
		if (*q)
		  while (*q && *q != ' ') q++;
	      }

	    output = save_string (p, q - p);
	  }
	else if (*p == '-' && p[1] == 'c')
	  compiling = 1;
	if (*q) q++;
	p = q;
      }

  if (compiling && output)
    return output;

  return save_string (filename, strlen (filename));
}        

static void
open_repo_file (filename)
     char *filename;
{
  register char *p, *q;
  char *file = get_base_filename (filename);
  char *s = rindex (file, '/');
  if (s == NULL)
    s = file;
  else
    ++s;

  for (p = repo_name, q = file; q < s; )
    *p++ = *q++;
  *p++ = '.';
  if ((s = rindex (q, '.')) == NULL)
    strcpy (p, q);
  else
    for (; q < s;)
      *p++ = *q++;
  strcat (p, ".repo");

  repo_file = fopen (repo_name, "r");
  free (file);
}

void
init_repo (filename)
     char *filename;
{
  char buf[1024];

  if (! flag_use_repository)
    return;

  open_repo_file ();

  if (repo_file == 0)
    return;

  while (fgets (buf, 1024, repo_file))
    {
      int len = strlen (buf) - 1;
      if (buf[len] != '\n')
	error ("repository info line too long in %s", repo_name);
      buf[len] = '\0';

      switch (buf[0])
	{
	case 'A':
	case 'G':
	case 'M':
	  break;
	case 'C':
	case 'O':
	  {
	    tree id = get_identifier (&buf[2]);
	    IDENTIFIER_REPO_USED (id) = 1;
	    if (buf[0] == 'C')
	      IDENTIFIER_REPO_CHOSEN (id) = 1;
	  }
	  break;
	default:
	  error ("mysterious repository information in %s", repo_name);
	}
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
  char *p;

  if (! flag_use_repository)
    return;

  /* Do we have to write out a new info file?  */

  if (! repo_changed || errorcount || sorrycount)
    goto out;

  reopen_repo_file_for_write ();

  if (repo_file == 0)
    goto out;

  fprintf (repo_file, "M %s\n", main_input_filename);

  p = getenv ("COLLECT_GCC");
  if (p != 0)
    fprintf (repo_file, "G %s\n", p);

  p = getenv ("COLLECT_GCC_OPTIONS");
  if (p != 0)
    fprintf (repo_file, "A %s\n", p);

  for (t = pending_repo; t; t = TREE_CHAIN (t))
    {
      tree val = TREE_VALUE (t);
      char type;

      if (TREE_CODE_CLASS (TREE_CODE (val)) == 't')
	val = TYPE_MAIN_DECL (val);
      val = DECL_ASSEMBLER_NAME (val);

      if (! IDENTIFIER_REPO_USED (val))
	continue;
      IDENTIFIER_REPO_USED (val) = 0;

      type = IDENTIFIER_REPO_CHOSEN (val) ? 'C' : 'O';

      fprintf (repo_file, "%c %s\n", type, IDENTIFIER_POINTER (val));
    }

 out:
  if (repo_file)
    fclose (repo_file);
}
