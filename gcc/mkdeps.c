/* Dependency generator for Makefile fragments.
   Copyright (C) 2000, 2001 Free Software Foundation, Inc.
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

/* Keep this structure local to this file, so clients don't find it
   easy to start making assumptions.  */
struct deps
{
  const char **targetv;
  unsigned int ntargets;	/* number of slots actually occupied */
  unsigned int targets_size;	/* amt of allocated space - in words */

  const char **depv;
  unsigned int ndeps;
  unsigned int deps_size;
};

static const char *munge	PARAMS ((const char *));
static const char *base_name	PARAMS ((const char *));

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
	  for (q = p - 1; filename <= q && *q == '\\';  q--)
	    len++;
	  len++;
	  break;

	case '$':
	  /* '$' is quoted by doubling it.  */
	  len++;
	  break;
	}
    }

  /* Now we know how big to make the buffer.  */
  buffer = xmalloc (len + 1);

  for (p = filename, dst = buffer; *p; p++, dst++)
    {
      switch (*p)
	{
	case ' ':
	case '\t':
	  for (q = p - 1; filename <= q && *q == '\\';  q--)
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

  d->targetv = (const char **) xmalloc (2 * sizeof (const char *));
  d->depv = (const char **) xmalloc (8 * sizeof (const char *));

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

/* Adds a target T.  We make a copy, so it need not be a permanent
   string.  QUOTE is true if the string should be quoted.  */
void
deps_add_target (d, t, quote)
     struct deps *d;
     const char *t;
     int quote;
{
  if (d->ntargets == d->targets_size)
    {
      d->targets_size *= 2;
      d->targetv = (const char **) xrealloc (d->targetv,
			     d->targets_size * sizeof (const char *));
    }

  if (quote)
    t = munge (t);  /* Also makes permanent copy.  */
  else
    t = xstrdup (t);

  d->targetv[d->ntargets++] = t;
}

/* Sets the default target if none has been given already.  An empty
   string as the default target in interpreted as stdin.  The string
   is quoted for MAKE.  */
void
deps_add_default_target (d, tgt)
     struct deps *d;
     const char *tgt;
{
  char *o, *suffix;

  /* Only if we have no targets.  */
  if (d->ntargets)
    return;

  if (tgt[0] == '\0')
    deps_add_target (d, "-", 1);
  else
    {
      tgt = base_name (tgt);
      o = (char *) alloca (strlen (tgt) + 8);

      strcpy (o, tgt);
      suffix = strrchr (o, '.');

#ifndef OBJECT_SUFFIX
# define OBJECT_SUFFIX ".o"
#endif

      if (suffix)
	strcpy (suffix, OBJECT_SUFFIX);
      else
	strcat (o, OBJECT_SUFFIX);
      deps_add_target (d, o, 1);
    }
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
      d->depv = (const char **)
	xrealloc (d->depv, d->deps_size * sizeof (const char *));
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
deps_phony_targets (d, fp)
     const struct deps *d;
     FILE *fp;
{
  unsigned int i;

  for (i = 1; i < d->ndeps; i++)
    {
      putc ('\n', fp);
      fputs (d->depv[i], fp);
      putc (':', fp);
      putc ('\n', fp);
    }
}
