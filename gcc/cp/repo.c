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
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

/* My strategy here is as follows:

   Everything should be emitted in a translation unit where it is used.
   The results of the automatic process should be easily reproducible with
   explicit code.  */

#include <stdio.h>
#include "config.h"
#include "tree.h"
#include "cp-tree.h"
#include "input.h"
#include "obstack.h"

extern char * rindex ();
extern char * getenv ();
extern char * getpwd ();

static tree pending_repo;
static tree original_repo;
static char *repo_name;
static FILE *repo_file;

extern int flag_use_repository;
extern int errorcount, sorrycount;
extern struct obstack temporary_obstack;
extern struct obstack permanent_obstack;

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

tree
repo_get_id (t)
     tree t;
{
  if (TREE_CODE_CLASS (TREE_CODE (t)) == 't')
    {
      t = TYPE_BINFO_VTABLE (t);
      if (t == NULL_TREE)
	return t;
    }
  return DECL_ASSEMBLER_NAME (t);
}

/* Note that a template has been used.  If we can see the definition, offer
   to emit it. */

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
  
  if (TREE_CODE_CLASS (TREE_CODE (t)) == 't')
    {
      if (IDENTIFIER_REPO_CHOSEN (id))
	mark_class_instantiated (t, 0);
    }
  else if (TREE_CODE_CLASS (TREE_CODE (t)) == 'd')
    {
      if (IDENTIFIER_REPO_CHOSEN (id))
	mark_function_instantiated (t, 0);
    }
  else
    my_friendly_abort (1);

  if (! IDENTIFIER_REPO_USED (id))
    {
      IDENTIFIER_REPO_USED (id) = 1;
      pending_repo = perm_tree_cons (NULL_TREE, id, pending_repo);
    }
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

static char *
save_string (s, len)
     char *s;
     int len;
{
  return obstack_copy0 (&temporary_obstack, s, len);
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

  if (p && ! compiling)
    {
      warning ("-frepo must be used with -c");
      flag_use_repository = 0;
      return NULL;
    }

  p = rindex (filename, '/');
  if (p)
    return p+1;
  else
    return filename;
}        

static void
open_repo_file (filename)
     char *filename;
{
  register char *p, *q;
  char *s = get_base_filename (filename);

  if (s == NULL)
    return;

  p = rindex (s, '/');
  if (! p)
    p = s;
  p = rindex (p, '.');
  if (! p)
    p = s + strlen (s);

  obstack_grow (&permanent_obstack, s, p - s);
  repo_name = obstack_copy0 (&permanent_obstack, ".rpo", 4);

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
     char *filename;
{
  char *buf;

  if (! flag_use_repository)
    return;

  open_repo_file (filename);

  if (repo_file == 0)
    return;

  while (buf = afgets (repo_file))
    {
      switch (buf[0])
	{
	case 'A':
	case 'D':
	case 'M':
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

	    original_repo = perm_tree_cons (orig, id, original_repo);
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
  char *p;
  int repo_changed = 0;

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

  if (! repo_changed || errorcount || sorrycount)
    goto out;

  reopen_repo_file_for_write ();

  if (repo_file == 0)
    goto out;

  fprintf (repo_file, "M %s\n", main_input_filename);

  p = getpwd ();
  fprintf (repo_file, "D %s\n", p);

  p = getenv ("COLLECT_GCC_OPTIONS");
  if (p != 0)
    fprintf (repo_file, "A %s\n", p);

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
