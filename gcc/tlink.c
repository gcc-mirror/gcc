/* CYGNUS LOCAL: whole file jason */
/* Scan linker error messages for missing template instantiations and provide
   them.

   Copyright (C) 1995 Free Software Foundation, Inc.
   Contributed by Jason Merrill (jason@cygnus.com).

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

#include <stdio.h>
#include <ctype.h>
#include "config.h"
#include "hash.h"
#include "demangle.h"

#define MAX_ITERATIONS 17

/* Obstack allocation and deallocation routines.  */
#define obstack_chunk_alloc xmalloc
#define obstack_chunk_free free

extern char * xmalloc PARAMS((unsigned));
extern void free ();
extern char * getenv ();

/* Defined in collect2.c.  */
extern int vflag, debug;
extern char *ldout;
extern char *c_file_name;
extern struct obstack temporary_obstack;
extern struct obstack permanent_obstack;
extern char * temporary_firstobj;

/* Defined in the automatically-generated underscore.c.  */
extern int prepends_underscore;

static int tlink_verbose;

/* Hash table code.  */

typedef struct symbol_hash_entry
{
  struct hash_entry root;
  struct file_hash_entry *file;
  int chosen;
  int tweaking;
  int tweaked;
} symbol;

typedef struct file_hash_entry
{
  struct hash_entry root;
  const char *args;
  const char *dir;
  const char *main;
  int tweaking;
} file;

typedef struct demangled_hash_entry
{
  struct hash_entry root;
  const char *mangled;
} demangled;

static struct hash_table symbol_table;

static struct hash_entry *
symbol_hash_newfunc (entry, table, string)
     struct hash_entry *entry;
     struct hash_table *table;
     const char *string;
{
  struct symbol_hash_entry *ret = (struct symbol_hash_entry *) entry;
  if (ret == NULL)
    {
      ret = ((struct symbol_hash_entry *)
	     hash_allocate (table, sizeof (struct symbol_hash_entry)));
      if (ret == NULL)
	return NULL;
    }
  ret = ((struct symbol_hash_entry *)
     	 hash_newfunc ((struct hash_entry *) ret, table, string));
  ret->file = NULL;
  ret->chosen = 0;
  ret->tweaking = 0;
  ret->tweaked = 0;
  return (struct hash_entry *) ret;
}

static struct symbol_hash_entry *
symbol_hash_lookup (string, create)
     const char *string;
     boolean create;
{
  return ((struct symbol_hash_entry *)
	  hash_lookup (&symbol_table, string, create, true));
}

static struct hash_table file_table;

static struct hash_entry *
file_hash_newfunc (entry, table, string)
     struct hash_entry *entry;
     struct hash_table *table;
     const char *string;
{
   struct file_hash_entry *ret = (struct file_hash_entry *) entry;
  if (ret == NULL)
    {
      ret = ((struct file_hash_entry *)
	     hash_allocate (table, sizeof (struct file_hash_entry)));
      if (ret == NULL)
	return NULL;
    }
  ret = ((struct file_hash_entry *)
     	 hash_newfunc ((struct hash_entry *) ret, table, string));
  ret->args = NULL;
  ret->dir = NULL;
  ret->main = NULL;
  ret->tweaking = 0;
  return (struct hash_entry *) ret;
}

static struct file_hash_entry *
file_hash_lookup (string)
     const char *string;
{
  return ((struct file_hash_entry *)
	  hash_lookup (&file_table, string, true, true));
}

static struct hash_table demangled_table;

static struct hash_entry *
demangled_hash_newfunc (entry, table, string)
     struct hash_entry *entry;
     struct hash_table *table;
     const char *string;
{
  struct demangled_hash_entry *ret = (struct demangled_hash_entry *) entry;
  if (ret == NULL)
    {
      ret = ((struct demangled_hash_entry *)
	     hash_allocate (table, sizeof (struct demangled_hash_entry)));
      if (ret == NULL)
	return NULL;
    }
  ret = ((struct demangled_hash_entry *)
     	 hash_newfunc ((struct hash_entry *) ret, table, string));
  ret->mangled = NULL;
  return (struct hash_entry *) ret;
}

static struct demangled_hash_entry *
demangled_hash_lookup (string, create)
     const char *string;
     boolean create;
{
  return ((struct demangled_hash_entry *)
	  hash_lookup (&demangled_table, string, create, true));
}

/* Stack code.  */

struct symbol_stack_entry
{
  symbol *value;
  struct symbol_stack_entry *next;
};
struct obstack symbol_stack_obstack;
struct symbol_stack_entry *symbol_stack;

struct file_stack_entry
{
  file *value;
  struct file_stack_entry *next;
};
struct obstack file_stack_obstack;
struct file_stack_entry *file_stack;

static void
symbol_push (p)
     symbol *p;
{
  struct symbol_stack_entry *ep = (struct symbol_stack_entry *) obstack_alloc
    (&symbol_stack_obstack, sizeof (struct symbol_stack_entry));
  ep->value = p;
  ep->next = symbol_stack;
  symbol_stack = ep;
}

static symbol *
symbol_pop ()
{
  struct symbol_stack_entry *ep = symbol_stack;
  symbol *p;
  if (ep == NULL)
    return NULL;
  p = ep->value;
  symbol_stack = ep->next;
  obstack_free (&symbol_stack_obstack, ep);
  return p;
}

static void
file_push (p)
     file *p;
{
  struct file_stack_entry *ep;

  if (p->tweaking)
    return;

  ep = (struct file_stack_entry *) obstack_alloc
    (&file_stack_obstack, sizeof (struct file_stack_entry));
  ep->value = p;
  ep->next = file_stack;
  file_stack = ep;
  p->tweaking = 1;
}

static file *
file_pop ()
{
  struct file_stack_entry *ep = file_stack;
  file *p;
  if (ep == NULL)
    return NULL;
  p = ep->value;
  file_stack = ep->next;
  obstack_free (&file_stack_obstack, ep);
  p->tweaking = 0;
  return p;
}

/* Other machinery.  */

static void
tlink_init ()
{
  char *p;

  hash_table_init (&symbol_table, symbol_hash_newfunc);
  hash_table_init (&file_table, file_hash_newfunc);
  hash_table_init (&demangled_table, demangled_hash_newfunc);
  obstack_begin (&symbol_stack_obstack, 0);
  obstack_begin (&file_stack_obstack, 0);

  p = getenv ("TLINK_VERBOSE");
  if (p)
    tlink_verbose = atoi (p);
  else
    {
      tlink_verbose = 1;
      if (vflag)
	tlink_verbose = 2;
      if (debug)
	tlink_verbose = 3;
    }
}

static int
tlink_execute (prog, argv, redir)
     char *prog;
     char **argv;
     char *redir;
{
  collect_execute (prog, argv, redir);
  return collect_wait (prog);
} 

static char *
frob_extension (s, ext)
     char *s, *ext;
{
  char *p = (char *) rindex (s, '/');
  if (! p)
    p = s;
  p = (char *) rindex (p, '.');
  if (! p)
    p = s + strlen (s);

  obstack_grow (&temporary_obstack, s, p - s);
  return obstack_copy0 (&temporary_obstack, ext, strlen (ext));
}

static char *
obstack_fgets (stream, ob)
     FILE *stream;
     struct obstack *ob;
{
  int c;
  while ((c = getc (stream)) != EOF && c != '\n')
    obstack_1grow (ob, c);
  if (obstack_object_size (ob) == 0)
    return NULL;
  obstack_1grow (ob, '\0');
  return obstack_finish (ob);
}

static char *
tfgets (stream)
     FILE *stream;
{
  return obstack_fgets (stream, &temporary_obstack);
}

static char *
pfgets (stream)
     FILE *stream;
{
  return obstack_fgets (stream, &permanent_obstack);
}

/* Real tlink code.  */

static void
freadsym (stream, f, chosen)
     FILE *stream;
     file *f;
     int chosen;
{
  symbol *sym;

  {
    char *name = tfgets (stream);
    sym = symbol_hash_lookup (name, true);
  }

  if (sym->file == NULL)
    {
      symbol_push (sym);
      sym->file = f;
      sym->chosen = chosen;
    }
  else if (chosen)
    {
      if (sym->chosen && sym->file != f)
	{
	  if (sym->chosen == 1)
	    file_push (sym->file);
	  else
	    {
	      file_push (f);
	      f = sym->file;
	      chosen = sym->chosen;
	    }
	}
      sym->file = f;
      sym->chosen = chosen;
    }
}

static void
read_repo_file (f)
     file *f;
{
  char c;
  FILE *stream = fopen (f->root.string, "r");

  if (tlink_verbose >= 2)
    fprintf (stderr, "collect: reading %s\n", f->root.string);

  while (fscanf (stream, "%c ", &c) == 1)
    {
      switch (c)
	{
	case 'A':
	  f->args = pfgets (stream);
	  break;
	case 'D':
	  f->dir = pfgets (stream);
	  break;
	case 'M':
	  f->main = pfgets (stream);
	  break;
	case 'P':
	  freadsym (stream, f, 2);
	  break;
	case 'C':
	  freadsym (stream, f, 1);
	  break;
	case 'O':
	  freadsym (stream, f, 0);
	  break;
	}
      obstack_free (&temporary_obstack, temporary_firstobj);
    }
  fclose (stream);
  if (f->args == NULL)
    f->args = getenv ("COLLECT_GCC_OPTIONS");
  if (f->dir == NULL)
    f->dir = ".";
}

static void
maybe_tweak (line, f)
     char *line;
     file *f;
{
  symbol *sym = symbol_hash_lookup (line + 2, false);

  if ((sym->file == f && sym->tweaking)
      || (sym->file != f && line[0] == 'C'))
    {
      sym->tweaking = 0;
      sym->tweaked = 1;

      if (line[0] == 'O')
	line[0] = 'C';
      else
	line[0] = 'O';
    }
}

static int
recompile_files ()
{
  file *f;

  while ((f = file_pop ()) != NULL)
    {
      char *line, *command;
      FILE *stream = fopen (f->root.string, "r");
      char *outname = frob_extension (f->root.string, ".rnw");
      FILE *output = fopen (outname, "w");

      while ((line = tfgets (stream)) != NULL)
	{
	  switch (line[0])
	    {
	    case 'C':
	    case 'O':
	      maybe_tweak (line, f);
	    }
	  fprintf (output, "%s\n", line);
	}
      fclose (stream);
      fclose (output);
      rename (outname, f->root.string);

      obstack_grow (&temporary_obstack, "cd ", 3);
      obstack_grow (&temporary_obstack, f->dir, strlen (f->dir));
      obstack_grow (&temporary_obstack, "; ", 2);
      obstack_grow (&temporary_obstack, c_file_name, strlen (c_file_name));
      obstack_1grow (&temporary_obstack, ' ');
      obstack_grow (&temporary_obstack, f->args, strlen (f->args));
      obstack_1grow (&temporary_obstack, ' ');
      command = obstack_copy0 (&temporary_obstack, f->main, strlen (f->main));

      if (tlink_verbose)
	fprintf (stderr, "collect: recompiling %s\n", f->main);
      if (tlink_verbose >= 3)
	fprintf (stderr, "%s\n", command);

      if (system (command) != 0)
	return 0;

      read_repo_file (f);

      obstack_free (&temporary_obstack, temporary_firstobj);
    }
  return 1;
}

static int
read_repo_files (object_lst)
     char **object_lst;
{
  char **object = object_lst;

  for (; *object; object++)
    {
      char *p = frob_extension (*object, ".rpo");
      file *f;

      if (! file_exists (p))
	continue;

      f = file_hash_lookup (p);

      read_repo_file (f);
    }

  if (file_stack != NULL && ! recompile_files ())
    return 0;

  return (symbol_stack != NULL);
}

static void
demangle_new_symbols ()
{
  symbol *sym;

  while ((sym = symbol_pop ()) != NULL)
    {
      demangled *dem;
      char *p = cplus_demangle (sym->root.string, DMGL_PARAMS | DMGL_ANSI);

      if (! p)
	continue;

      dem = demangled_hash_lookup (p, true);
      dem->mangled = sym->root.string;
    }
}

static int
scan_linker_output (fname)
     char *fname;
{
  FILE *stream = fopen (fname, "r");
  char *line;

  while ((line = tfgets (stream)) != NULL)
    {
      char *p = line, *q;
      symbol *sym;
      int end;
      
      while (*p && isspace (*p))
	++p;

      if (! *p)
	continue;

      for (q = p; *q && ! isspace (*q); ++q)
	;

      /* Try the first word on the line.  */
      if (*p == '.')
	++p;
      if (*p == '_' && prepends_underscore)
	++p;

      end = ! *q;
      *q = 0;
      sym = symbol_hash_lookup (p, false);

      if (! sym && ! end)
	/* Try a mangled name in `quotes'.  */
	{
	  demangled *dem = 0;
	  p = (char *) index (q+1, '`');
	  q = 0;

#define MUL "multiple definition of "
#define UND "undefined reference to "

	  if (p && (p - line > sizeof (MUL)))
	    {
	      char *beg = p - sizeof (MUL) + 1;
	      *p = 0;
	      if (!strcmp (beg, MUL) || !strcmp (beg, UND))
		p++, q = (char *) index (p, '\'');
	    }
	  if (q)
	    *q = 0, dem = demangled_hash_lookup (p, false);
	  if (dem)
	    sym = symbol_hash_lookup (dem->mangled, false);
	}

      if (sym && sym->tweaked)
	return 0;
      if (sym && !sym->tweaking)
	{
	  if (tlink_verbose >= 2)
	    fprintf (stderr, "collect: tweaking %s in %s\n",
		     sym->root.string, sym->file->root.string);
	  sym->tweaking = 1;
	  file_push (sym->file);
	}
	
      obstack_free (&temporary_obstack, temporary_firstobj);
    }

  return (file_stack != NULL);
}

void
do_tlink (ld_argv, object_lst)
     char **ld_argv, **object_lst;
{
  int exit = tlink_execute ("ld", ld_argv, ldout);

  tlink_init ();

  if (exit)
    {
      int i = 0;

      /* Until collect does a better job of figuring out which are object
	 files, assume that everything on the command line could be.  */
      if (read_repo_files (ld_argv))
	while (exit && i++ < MAX_ITERATIONS)
	  {
	    if (tlink_verbose >= 3)
	      dump_file (ldout);
	    demangle_new_symbols ();
	    if (! scan_linker_output (ldout))
	      break;
	    if (! recompile_files ())
	      break;
	    if (tlink_verbose)
	      fprintf (stderr, "collect: relinking\n");
	    exit = tlink_execute ("ld", ld_argv, ldout);
	  }
    }

  dump_file (ldout);
  unlink (ldout);
  if (exit)
    {
      error ("ld returned %d exit status", exit);
      collect_exit (exit);
    }
}
