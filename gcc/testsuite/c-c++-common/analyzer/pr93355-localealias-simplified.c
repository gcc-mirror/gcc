/* Simplified version of test for ensuring we issue a FILE * leak diagnostic,
   made trivial.
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
#define NULL ((void *) 0)
typedef struct _IO_FILE FILE;
extern FILE *fopen(const char *__restrict __filename,
		   const char *__restrict __modes);
extern int fclose(FILE *__stream);

void
read_alias_file (int flag)
{
  FILE *fp;

  fp = fopen ("name", "r"); /* { dg-message "opened here" } */
  if (fp == NULL)
    return;

  if (flag)
    return; /* { dg-warning "leak of FILE 'fp'" } */

  fclose (fp);
}
