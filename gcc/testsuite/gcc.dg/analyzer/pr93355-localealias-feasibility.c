/* Simplified version of test to ensure we issue a FILE * leak diagnostic,
   reproducing a feasibility issue.
   Adapted from intl/localealias.c, with all #includes removed.  */

/* { dg-do "compile" } */

/* Handle aliases for locale names.
   Copyright (C) 1995-1999, 2000-2001, 2003 Free Software Foundation, Inc.

   This program is free software; you can redistribute it and/or modify it
   under the terms of the GNU Library General Public License as published
   by the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Library General Public License for more details.

   You should have received a copy of the GNU Library General Public
   License along with this program; if not, write to the Free Software
   Foundation, Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1301,
   USA.  */

/* Minimal version of system headers.  */

typedef __SIZE_TYPE__ size_t;
#define NULL ((void *)0)

typedef struct _IO_FILE FILE;
extern FILE *fopen (const char *__restrict __filename,
		    const char *__restrict __modes);
extern size_t fread(void *ptr, size_t size, size_t nmemb, FILE *stream);
extern int fclose (FILE *__stream);

extern int isspace (int) __attribute__((__nothrow__, __leaf__));

/* Cleaned-up body of localealias.c follows.  */

size_t
read_alias_file (const char *fname, int fname_len)
{
  FILE *fp;
  size_t added;
  char buf[400];
  char *alias;
  char *value;
  char *cp;

  fp = fopen (fname, "r"); /* { dg-message "opened here" } */
  if (fp == NULL)
    return 0;

  if (fread (buf, sizeof buf, 1, fp) != 1)
    {
      fclose (fp);
      return 0;
    }

  cp = buf;

  /* Ignore leading white space.  */
  while (isspace ((unsigned char)cp[0]))
    ++cp;

  if (cp[0] != '\0' && cp[0] != '#')
    {
      alias = cp++;
      while (cp[0] != '\0' && !isspace ((unsigned char)cp[0]))
	++cp;
      if (cp[0] != '\0')
	*cp++ = '\0';

      while (isspace ((unsigned char)cp[0]))
	++cp;

      if (cp[0] != '\0')
	return 42; /* { dg-warning "leak of FILE 'fp'" } */
    }

  fclose(fp);

  return 0;
}
