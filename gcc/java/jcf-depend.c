/* Functions for handling dependency tracking when reading .class files.

   Copyright (C) 1998, 1999, 2000  Free Software Foundation, Inc.

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU CC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  

Java and all Java-based marks are trademarks or registered trademarks
of Sun Microsystems, Inc. in the United States and other countries.
The Free Software Foundation is independent of Sun Microsystems, Inc.  */

/* Written by Tom Tromey <tromey@cygnus.com>, October 1998.  */

#include "config.h"
#include "system.h"

#include <assert.h>

#include "jcf.h"



/* We keep a linked list of all the files we've already read.  */
struct entry
{
  char *file;
  struct entry *next;
};

static void free_entry PARAMS ((struct entry **));
static void add_entry PARAMS ((struct entry **, const char *));
static const char *munge PARAMS ((const char *));
static int print_ents PARAMS ((struct entry *, int));

/* List of files.  */
static struct entry *dependencies = NULL;

/* Name of targets.  We support multiple targets when writing .class
   files.  */
static struct entry *targets = NULL;

/* Number of columns in output.  */
#define MAX_OUTPUT_COLUMNS 72

/* The output file, or NULL if we aren't doing dependency tracking.  */
static FILE *dep_out = NULL;

/* Nonzero if system files should be added.  */
static int system_files;



/* Helper to free an entry list.  */
static void
free_entry (entp)
     struct entry **entp;
{
  struct entry *ent, *next;

  for (ent = *entp; ent != NULL; ent = next)
    {
      next = ent->next;
      free (ent->file);
      free (ent);
    }
  *entp = NULL;
}

/* Helper to add to the end of the entry list.  */
static void
add_entry (entp, name)
     struct entry **entp;
     const char *name;
{
  struct entry *ent, *last;

  for (last = ent = *entp; ent != NULL; last = ent, ent = ent->next)
    if (! strcmp (ent->file, name))
      return;

  ent = (struct entry *) xmalloc (sizeof (struct entry));
  ent->file = xstrdup (name);
  ent->next = NULL;

  if (last == ent)
    {
      // This is only true the first time through, when the entry list
      // is empty.
      *entp = ent;
    }     
  else
    last->next = ent;
}

/* Call this to reset the dependency module.  This is required if
   multiple dependency files are being generated from a single tool
   invocation.  */
void
jcf_dependency_reset ()
{
  free_entry (&dependencies);
  free_entry (&targets);

  if (dep_out != NULL)
    {
      if (dep_out != stdout)
	fclose (dep_out);
      dep_out = NULL;
    }
}

void
jcf_dependency_set_target (name)
     const char *name;
{
  free_entry (&targets);
  if (name != NULL)
    add_entry (&targets, name);
}

void
jcf_dependency_add_target (name)
     const char *name;
{
  add_entry (&targets, name);
}

void
jcf_dependency_set_dep_file (name)
     const char *name;
{
  assert (dep_out != stdout);
  if (dep_out)
    fclose (dep_out);
  if (! strcmp (name, "-"))
    dep_out = stdout;
  else
    dep_out = fopen (name, "w");
}

void
jcf_dependency_add_file (filename, system_p)
     const char *filename;
     int system_p;
{
  /* Just omit system files.  */
  if (system_p && ! system_files)
    return;

  add_entry (&dependencies, filename);
}

void
jcf_dependency_init (system_p)
     int system_p;
{
  system_files = system_p;
}

/* FIXME: this is taken almost directly from cccp.c.  Such duplication
   is bad.  */
static const char *
munge (filename)
     const char *filename;
{
  static char *buffer = NULL;
  static int buflen = 0;

  int len = 2 * strlen (filename) + 1;
  const char *p;
  char *dst;

  if (buflen < len)
    {
      buflen = len;
      buffer = xrealloc (buffer, buflen);
    }

  dst = buffer;
  for (p = filename; *p; ++p)
    {
      switch (*p)
	{
	case ' ':
	case '\t':
	  {
	    /* GNU make uses a weird quoting scheme for white space.
	       A space or tab preceded by 2N+1 backslashes represents
	       N backslashes followed by space; a space or tab
	       preceded by 2N backslashes represents N backslashes at
	       the end of a file name; and backslashes in other
	       contexts should not be doubled.  */
	    const char *q;
	    for (q = p - 1; filename < q && q[-1] == '\\';  q--)
	      *dst++ = '\\';
	  }
	  *dst++ = '\\';
	  goto ordinary_char;

	case '$':
	  *dst++ = '$';
	  /* Fall through.  This can mishandle things like "$(" but
	     there's no easy fix.  */
	default:
	ordinary_char:
	  /* This can mishandle characters in the string "\0\n%*?[\\~";
	     exactly which chars are mishandled depends on the `make' version.
	     We know of no portable solution for this;
	     even GNU make 3.76.1 doesn't solve the problem entirely.
	     (Also, '\0' is mishandled due to our calling conventions.)  */
	  *dst++ = *p;
	  break;
	}
    }

  *dst++ = '\0';
  return buffer;
}

/* Helper to print list of files.  */
static int
print_ents (ent, column)
     struct entry *ent;
     int column;
{
  int first = 1;

  for (; ent != NULL; ent = ent->next)
    {
      const char *depname = munge (ent->file);
      int len = strlen (depname);

      if (column + len + 2 > MAX_OUTPUT_COLUMNS)
	{
	  fprintf (dep_out, " \\\n ");
	  column = 1;
	}

      if (! first)
	fputs (" ", dep_out);
      fputs (depname, dep_out);
      first = 0;
      column += len + 1;
    }

  return column;
}

void
jcf_dependency_write ()
{
  int column = 0;

  if (! dep_out)
    return;

  assert (targets);
  column = print_ents (targets, 0);
  fputs (" : ", dep_out);

  print_ents (dependencies, column);
  fputs ("\n", dep_out);
  fflush (dep_out);
}
