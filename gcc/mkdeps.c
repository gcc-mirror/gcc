/* Dependency generator for Makefile fragments.
   Copyright (C) 2000 Free Software Foundation, Inc.
   Contributed by Zack Weinberg, Mar 2000

This program is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2, or (at your option) any
later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

 In other words, you are welcome to use, share and improve this program.
 You are forbidden to forbid anyone else to use, share and improve
 what you give them.   Help stamp out software-hoarding!  */

#include "config.h"
#include "system.h"
#include "mkdeps.h"

static const char *munge	PARAMS ((const char *));
static const char *base_name	PARAMS ((const char *));

#ifndef OBJECT_SUFFIX
# define OBJECT_SUFFIX ".o"
#endif

/* Given a filename, quote characters in that filename which are
   significant to Make.  Note that it's not possible to quote all such
   characters - e.g. \n, %, *, ?, [, \ (in some contexts), and ~ are
   not properly handled.  It isn't possible to get this right in any
   current version of Make.  (??? Still true?  Old comment referred to
   3.76.1.)  */
   
static const char *
munge (filename)
     const char *filename;
{
  int len;
  const char *p, *q;
  char *dst, *buffer;

  for (p = filename, len = 0; *p; p++, len++)
    {
      switch (*p)
	{
	case ' ':
	case '\t':
	  /* GNU make uses a weird quoting scheme for white space.
	     A space or tab preceded by 2N+1 backslashes represents
	     N backslashes followed by space; a space or tab
	     preceded by 2N backslashes represents N backslashes at
	     the end of a file name; and backslashes in other
	     contexts should not be doubled.  */
	  for (q = p - 1; q < filename && q[-1] == '\\';  q--)
	    len++;
	  len++;
	  break;

	case '$':
	  /* '$' is quoted by doubling it. This can mishandle things
	     like "$(" but there's no easy fix.  */
	  len++;
	  break;
	}
    }

  /* Now we know how big to make the buffer.  */
  buffer = malloc (len + 1);

  for (p = filename, dst = buffer; *p; p++, dst++)
    {
      switch (*p)
	{
	case ' ':
	case '\t':
	  for (q = p - 1; filename < q && q[-1] == '\\';  q--)
	    *dst++ = '\\';
	  *dst++ = '\\';
	  break;

	case '$':
	  *dst++ = '$';
	  break;

	default:
	  /* nothing */;
	}
      *dst = *p;
    }

  *dst = '\0';
  return buffer;
}

/* Given a pathname, calculate the non-directory part.  This always
   knows how to handle Unix-style pathnames, and understands VMS and
   DOS paths on those systems.  */

/* Find the base name of a (partial) pathname FNAME.
   Returns a pointer into the string passed in.
   Accepts Unix (/-separated) paths on all systems,
   DOS and VMS paths on those systems.  */

static const char *
base_name (fname)
     const char *fname;
{
  const char *s = fname;
  const char *p;
#if defined (HAVE_DOS_BASED_FILE_SYSTEM)
  if (ISALPHA (s[0]) && s[1] == ':') s += 2;
  if ((p = strrchr (s, '\\'))) s = p + 1;
#elif defined VMS
  if ((p = strrchr (s, ':'))) s = p + 1; /* Skip device.  */
  if ((p = strrchr (s, ']'))) s = p + 1; /* Skip directory.  */
  if ((p = strrchr (s, '>'))) s = p + 1; /* Skip alternate (int'n'l) dir.  */
#endif
  if ((p = strrchr (s, '/'))) s = p + 1;
  return s;
}

/* Public routines.  */

struct deps *
deps_init ()
{
  struct deps *d = (struct deps *) xmalloc (sizeof (struct deps));

  /* Allocate space for the vectors now.  */

  d->targetv = xmalloc (2 * sizeof (const char *));
  d->depv = xmalloc (8 * sizeof (const char *));

  d->ntargets = 0;
  d->targets_size = 2;
  d->ndeps = 0;
  d->deps_size = 8;

  return d;
}

void
deps_free (d)
     struct deps *d;
{
  unsigned int i;

  for (i = 0; i < d->ntargets; i++)
    free ((PTR) d->targetv[i]);

  for (i = 0; i < d->ndeps; i++)
    free ((PTR) d->depv[i]);

  free (d->targetv);
  free (d->depv);
  free (d);
}

void
deps_add_target (d, t)
     struct deps *d;
     const char *t;
{
  t = munge (t);  /* Also makes permanent copy.  */

  if (d->ntargets == d->targets_size)
    {
      d->targets_size *= 2;
      d->targetv = xrealloc (d->targetv,
			     d->targets_size * sizeof (const char *));
    }

  d->targetv[d->ntargets++] = t;
}

void
deps_calc_target (d, t)
     struct deps *d;
     const char *t;
{
  char *o, *suffix;

  t = base_name (t);
  o = alloca (strlen (t) + 8);

  strcpy (o, t);
  suffix = strrchr (o, '.');
  if (suffix)
    strcpy (suffix, OBJECT_SUFFIX);
  else
    strcat (o, OBJECT_SUFFIX);

  deps_add_target (d, o);
}

void
deps_add_dep (d, t)
     struct deps *d;
     const char *t;
{
  t = munge (t);  /* Also makes permanent copy.  */

  if (d->ndeps == d->deps_size)
    {
      d->deps_size *= 2;
      d->depv = xrealloc (d->depv, d->deps_size * sizeof (const char *));
    }
  d->depv[d->ndeps++] = t;
}

void
deps_write (d, fp, colmax)
     const struct deps *d;
     FILE *fp;
     unsigned int colmax;
{
  unsigned int size, i, column;

  column = 0;
  if (colmax && colmax < 34)
    colmax = 34;

  for (i = 0; i < d->ntargets; i++)
    {
      size = strlen (d->targetv[i]);
      column += size;
      if (colmax && column > colmax)
	{
	  fputs (" \\\n ", fp);
	  column = 1 + size;
	}
      if (i)
	{
	  putc (' ', fp);
	  column++;
	}
      fputs (d->targetv[i], fp);
    }

  putc (':', fp);
  putc (' ', fp);
  column += 2;

  for (i = 0; i < d->ndeps; i++)
    {
      size = strlen (d->depv[i]);
      column += size;
      if (colmax && column > colmax)
	{
	  fputs (" \\\n ", fp);
	  column = 1 + size;
	}
      if (i)
	{
	  putc (' ', fp);
	  column++;
	}
      fputs (d->depv[i], fp);
    }
  putc ('\n', fp);
}
  
void
deps_dummy_targets (d, fp)
     const struct deps *d;
     FILE *fp;
{
  unsigned int i;

  for (i = 1; i < d->ndeps; i++)
    {
      fputs (d->depv[i], fp);
      putc (':', fp);
      putc ('\n', fp);
    }
}
