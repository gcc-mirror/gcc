/* Part of CPP library.  (memory allocation - xmalloc etc)
   Copyright (C) 1986, 87, 89, 92, 93, 94, 1995 Free Software Foundation, Inc.
   Written by Per Bothner, 1994.
   Based on CCCP program by by Paul Rubin, June 1986
   Adapted to ANSI C, Richard Stallman, Jan 1987

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
Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

 In other words, you are welcome to use, share and improve this program.
 You are forbidden to forbid anyone else to use, share and improve
 what you give them.   Help stamp out software-hoarding!  */

static void
memory_full ()
{
  fatal ("Memory exhausted.");
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

char *
xcalloc (number, size)
     unsigned number, size;
{
  register unsigned total = number * size;
  register char *ptr = (char *) malloc (total);
  if (ptr != 0) {
    if (total > 100)
      bzero (ptr, total);
    else {
      /* It's not too long, so loop, zeroing by longs.
	 It must be safe because malloc values are always well aligned.  */
      register long *zp = (long *) ptr;
      register long *zl = (long *) (ptr + total - 4);
      register int i = total - 4;
      while (zp < zl)
	*zp++ = 0;
      if (i < 0)
	i = 0;
      while (i < total)
	ptr[i++] = 0;
    }
    return ptr;
  }
  memory_full ();
  /*NOTREACHED*/
  return 0;
}
