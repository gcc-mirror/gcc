/* sts.c -- Implementation File (module.c template V1.0)
   Copyright (C) 1995 Free Software Foundation, Inc.
   Contributed by James Craig Burley.

This file is part of GNU Fortran.

GNU Fortran is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU Fortran is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Fortran; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330, Boston, MA
02111-1307, USA.

   Related Modules:
      None (despite the name, it doesn't really depend on ffest*)

   Description:
      Provides an arbitrary-length string facility for the limited needs of
      GNU Fortran FORMAT statement generation.

   Modifications:
*/

/* Include files. */

#include "proj.h"
#include "sts.h"
#include "com.h"
#include "malloc.h"

/* Externals defined here. */


/* Simple definitions and enumerations. */


/* Internal typedefs. */


/* Private include files. */


/* Internal structure definitions. */


/* Static objects accessed by functions in this module. */


/* Static functions (internal). */


/* Internal macros. */


/* ffests_kill -- Kill a varying-length string

   ffests s;
   ffests_kill(s);

   The storage associated with the string <s> is freed.	 */

void
ffests_kill (ffests s)
{
  if (s->text_ != NULL)
    malloc_kill_ksr (s->pool_, s->text_, s->max_);
}

/* ffests_new -- Make a varying-length string

   ffests s;
   ffests_new(s,malloc_pool_image(),0);

   The string is initialized to hold, in this case, 0 characters, and
   current and future heap manipulations to hold the string will use
   the image pool.  */

void
ffests_new (ffests s, mallocPool pool, ffestsLength size)
{
  s->pool_ = pool;
  s->len_ = 0;
  s->max_ = size;

  if (size == 0)
    s->text_ = NULL;
  else
    s->text_ = malloc_new_ksr (pool, "ffests", size);
}

/* ffests_printf_1D -- printf("...%ld...",(long)) to a string

   ffests s;
   ffests_printf_1D(s,"...%ld...",1);

   Like printf, but into a string.  */

void
ffests_printf_1D (ffests s, const char *ctl, long arg1)
{
  char quickbuf[40];
  char *buff;
  ffestsLength len;

  if ((len = strlen (ctl) + 21) < ARRAY_SIZE (quickbuf))
    /* No # bigger than 20 digits. */
    {
      sprintf (&quickbuf[0], ctl, arg1);
      ffests_puttext (s, &quickbuf[0], strlen (quickbuf));
    }
  else
    {
      buff = malloc_new_ks (malloc_pool_image (), "ffests_printf_1D", len);
      sprintf (buff, ctl, arg1);
      ffests_puttext (s, buff, strlen (buff));
      malloc_kill_ks (malloc_pool_image (), buff, len);
    }
}

/* ffests_printf_1U -- printf("...%lu...",(unsigned long)) to a string

   ffests s;
   ffests_printf_1U(s,"...%lu...",1);

   Like printf, but into a string.  */

void
ffests_printf_1U (ffests s, const char *ctl, unsigned long arg1)
{
  char quickbuf[40];
  char *buff;
  ffestsLength len;

  if ((len = strlen (ctl) + 21) < ARRAY_SIZE (quickbuf))
    /* No # bigger than 20 digits. */
    {
      sprintf (&quickbuf[0], ctl, arg1);
      ffests_puttext (s, &quickbuf[0], strlen (quickbuf));
    }
  else
    {
      buff = malloc_new_ks (malloc_pool_image (), "ffests_printf_1U", len);
      sprintf (buff, ctl, arg1);
      ffests_puttext (s, buff, strlen (buff));
      malloc_kill_ks (malloc_pool_image (), buff, len);
    }
}

/* ffests_printf_1s -- printf("...%s...",(char *)) to a string

   ffests s;
   ffests_printf_1s(s,"...%s...","hi there!");

   Like printf, but into a string.  */

void
ffests_printf_1s (ffests s, const char *ctl, const char *arg1)
{
  char quickbuf[40];
  char *buff;
  ffestsLength len;

  if ((len = strlen (ctl) + strlen (arg1) - 1) < ARRAY_SIZE (quickbuf))
    {
      sprintf (&quickbuf[0], ctl, arg1);
      ffests_puttext (s, &quickbuf[0], strlen (quickbuf));
    }
  else
    {
      buff = malloc_new_ks (malloc_pool_image (), "ffests_printf_1s", len);
      sprintf (buff, ctl, arg1);
      ffests_puttext (s, buff, strlen (buff));
      malloc_kill_ks (malloc_pool_image (), buff, len);
    }
}

/* ffests_printf_2Us -- printf("...%lu...%s...",...) to a string

   ffests s;
   ffests_printf_2Us(s,"...%lu...%s...",1,"hi there!");

   Like printf, but into a string.  */

void
ffests_printf_2Us (ffests s, const char *ctl, unsigned long arg1, const char *arg2)
{
  char quickbuf[60];
  char *buff;
  ffestsLength len;

  if ((len = strlen (ctl) + 21 + strlen (arg2) - 1) < ARRAY_SIZE (quickbuf))
    /* No # bigger than 20 digits. */
    {
      sprintf (&quickbuf[0], ctl, arg1, arg2);
      ffests_puttext (s, &quickbuf[0], strlen (quickbuf));
    }
  else
    {
      buff = malloc_new_ks (malloc_pool_image (), "ffests_printf_2Us", len);
      sprintf (buff, ctl, arg1, arg2);
      ffests_puttext (s, buff, strlen (buff));
      malloc_kill_ks (malloc_pool_image (), buff, len);
    }
}

/* ffests_putc -- Put a single character into string

   ffests s;
   ffests_putc(s,'*');	*/

void
ffests_putc (ffests s, char c)
{
  ffests_puttext (s, &c, 1);
}

/* ffests_puts -- Put a zero-terminated (C-style) string into string

   ffests s;
   ffests_puts(s,"append me");	*/

void
ffests_puts (ffests s, const char *string)
{
  ffests_puttext (s, string, strlen (string));
}

/* ffests_puttext -- Put a number of characters into string

   ffests s;
   ffests_puttext(s,"hi there",8);

   The string need not be 0-terminated, because the passed length is used,
   and may be 0.  */

void
ffests_puttext (ffests s, const char *text, ffestsLength length)
{
  ffestsLength newlen;
  ffestsLength newmax;

  if (length <= 0)
    return;

  newlen = s->len_ + length;
  if (newlen > s->max_)
    {
      if (s->text_ == NULL)
	{
	  s->max_ = 40;
	  s->text_ = malloc_new_ksr (s->pool_, "ffests", s->max_);
	}
      else
	{
	  newmax = s->max_ << 1;
	  while (newmax < newlen)
	    newmax <<= 1;
	  s->text_ = malloc_resize_ksr (s->pool_, s->text_, newmax, s->max_);
	  s->max_ = newmax;
	}
    }

  memcpy (s->text_ + s->len_, text, length);
  s->len_ = newlen;
}
