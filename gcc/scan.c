/* Utility functions for scan-decls and fix-header programs.
   Copyright (C) 1993, 1994 Free Software Foundation, Inc.

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
Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

#include "scan.h"
#include "hconfig.h"
#include <ctype.h>

int lineno = 1;
int source_lineno = 1;
sstring source_filename;

void
make_sstring_space (str, count)
     sstring *str;
     int count;
{
  int cur_pos = str->ptr - str->base;
  int cur_size = str->limit - str->base;
  int new_size = cur_pos + count + 100;

  if (new_size <= cur_size)
    return;
  
  if (str->base == NULL)
    str->base = xmalloc (new_size);
  else
    str->base = xrealloc (str->base, new_size);
  str->ptr = str->base + cur_size;
  str->limit = str->base + new_size;
}

void
sstring_append (dst, src)
     sstring *dst;
     sstring *src;
{
  register char *d, *s;
  register count = SSTRING_LENGTH(src);
  MAKE_SSTRING_SPACE(dst, count + 1);
  d = dst->ptr;
  s = src->base;
  while (--count >= 0) *d++ = *s++;
  dst->ptr = d;
  *d = 0;  
}

memory_full ()
{
  abort();
}

char *
xmalloc (size)
     unsigned size;
{
  register char *ptr = (char *) malloc (size);
  if (ptr != 0) return (ptr);
  memory_full ();
  /*NOTREACHED*/
  return 0;
}


char *
xrealloc (old, size)
     char *old;
     unsigned size;
{
  register char *ptr = (char *) realloc (old, size);
  if (ptr != 0) return (ptr);
  memory_full ();
  /*NOTREACHED*/
  return 0;
}

int
scan_ident (fp, s, c)
     register FILE *fp;
     register sstring *s;
     int c;
{
  s->ptr = s->base;
  if (isalpha(c) || c == '_')
    {
      for (;;)
	{
	  SSTRING_PUT(s, c);
	  c = getc (fp);
	  if (c == EOF || !(isalnum(c) || c == '_'))
	    break;
	}
    }
  MAKE_SSTRING_SPACE(s, 1);
  *s->ptr = 0;
  return c;
}

int
scan_string (fp, s, init)
     register FILE *fp;
     register sstring *s;
{
  int c;
  for (;;)
    {
      c = getc (fp);
      if (c == EOF || c == '\n')
	break;
      if (c == init)
	{
	  c = getc (fp);
	  break;
	}
      if (c == '\\')
	{
	  c = getc (fp);
	  if (c == EOF)
	    break;
	  if (c == '\n')
	    continue;
	}
      SSTRING_PUT(s, c);
    }
  MAKE_SSTRING_SPACE(s, 1);
  *s->ptr = 0;
  return c;
}

/* Skip horizontal white spaces (spaces, tabs, and C-style comments). */

int
skip_spaces (fp, c)
     register FILE *fp;
     int c;
{
  for (;;)
    {
      if (c == ' ' || c == '\t')
	c = getc (fp);
      else if (c == '/')
	{
	  c = getc (fp);
	  if (c != '*')
	    {
	      ungetc (c, fp);
	      return '/';
	    }
	  c = getc (fp);
	  for (;;)
	    {
	      if (c == EOF)
		return EOF;
	      else if (c != '*')
		{
		  if (c == '\n')
		    source_lineno++, lineno++;
		  c = getc (fp);
		}
	      else if ((c = getc (fp)) == '/')
		return getc (fp);
	    }
	}
      else
	break;
    }
  return c;
}

int
read_upto (fp, str, delim)
     FILE *fp;
     sstring *str;
     int delim;
{
  int ch;
  for (;;)
    {
      ch = getc (fp);
      if (ch == EOF || ch == delim)
	break;
      SSTRING_PUT(str, ch);
    }
  MAKE_SSTRING_SPACE(str, 1);
  *str->ptr = 0;
  return ch;
}

int
get_token (fp, s)
     register FILE *fp;
     register sstring *s;
{
  int c;
  s->ptr = s->base;
 retry:
  c = ' ';
  c = skip_spaces (fp, c);
  if (c == '\n')
    {
      source_lineno++;
      lineno++;
      goto retry;
    }
  if (c == '#')
    {
      c = get_token (fp, s);
      if (c == INT_TOKEN)
	{
	  source_lineno = atoi (s->base) - 1; /* '\n' will add 1 */
	  get_token (fp, &source_filename);
	}
      for (;;)
	{
	  c = getc (fp);
	  if (c == EOF)
	    return EOF;
	  if (c == '\n')
	    {
	    source_lineno++;
	    lineno++;
	    goto retry;
	    }
	}
    }
  if (c == EOF)
    return EOF;
  if (isdigit (c))
    {
      do
	{
	  SSTRING_PUT(s, c);
	  c = getc (fp);
	} while (c != EOF && isdigit(c));
      ungetc (c, fp);
      c = INT_TOKEN;
      goto done;
    }
  if (isalpha (c) || c == '_')
    {
      c = scan_ident (fp, s, c);
      ungetc (c, fp);
      return IDENTIFIER_TOKEN;
    }
  if (c == '\'' || c == '"')
    {
      c = scan_string (fp, s, c);
      ungetc (c, fp);
      return c == '\'' ? CHAR_TOKEN : STRING_TOKEN;
    }
  SSTRING_PUT(s, c);
 done:
  MAKE_SSTRING_SPACE(s, 1);
  *s->ptr = 0;
  return c;
}

unsigned long
hash (str)
     char *str;
{
  int h = 0;
  /* Replace this with something faster/better! FIXME! */
  while (*str) h = (h << 3) + *str++;
  return h & 0x7FFFFFFF;
}
