/* bit.c -- Implementation File (module.c template V1.0)
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
      None

   Description:
      Tracks arrays of booleans in useful ways.

   Modifications:
*/

/* Include files. */

#include "proj.h"
#include "glimits.h"
#include "bit.h"
#include "malloc.h"

/* Externals defined here. */


/* Simple definitions and enumerations. */


/* Internal typedefs. */


/* Private include files. */


/* Internal structure definitions. */


/* Static objects accessed by functions in this module. */


/* Static functions (internal). */


/* Internal macros. */


/* ffebit_count -- Count # of bits set a particular way

   ffebit b;  // the ffebit object
   ffebitCount offset;	// 0..size-1
   bool value;	// FALSE (0), TRUE (1)
   ffebitCount range;  // # bits to test
   ffebitCount number;	// # bits equal to value
   ffebit_count(b,offset,value,range,&number);

   Sets <number> to # bits at <offset> through <offset + range - 1> set to
   <value>.  If <range> is 0, <number> is set to 0.  */

void
ffebit_count (ffebit b, ffebitCount offset, bool value, ffebitCount range,
	      ffebitCount *number)
{
  ffebitCount element;
  ffebitCount bitno;

  assert (offset + range <= b->size);

  for (*number = 0; range != 0; --range, ++offset)
    {
      element = offset / CHAR_BIT;
      bitno = offset % CHAR_BIT;
      if (value
	  == ((b->bits[element] & ((unsigned char) 1 << bitno)) == 0 ? FALSE : TRUE))
	++ * number;
    }
}

/* ffebit_new -- Create a new ffebit object

   ffebit b;
   ffebit_kill(b);

   Destroys an ffebit object obtained via ffebit_new.  */

void
ffebit_kill (ffebit b)
{
  malloc_kill_ks (b->pool, b,
		  offsetof (struct _ffebit_, bits)
		  + (b->size + CHAR_BIT - 1) / CHAR_BIT);
}

/* ffebit_new -- Create a new ffebit object

   ffebit b;
   mallocPool pool;
   ffebitCount size;
   b = ffebit_new(pool,size);

   Allocates an ffebit object that holds the values of <size> bits in pool
   <pool>.  */

ffebit
ffebit_new (mallocPool pool, ffebitCount size)
{
  ffebit b;

  b = malloc_new_zks (pool, "ffebit",
		      offsetof (struct _ffebit_, bits)
		      + (size + CHAR_BIT - 1) / CHAR_BIT,
		      0);
  b->pool = pool;
  b->size = size;

  return b;
}

/* ffebit_set -- Set value of # of bits

   ffebit b;  // the ffebit object
   ffebitCount offset;	// 0..size-1
   bool value;	// FALSE (0), TRUE (1)
   ffebitCount length;	// # bits to set starting at offset (usually 1)
   ffebit_set(b,offset,value,length);

   Sets bit #s <offset> through <offset + length - 1> to <value>.  */

void
ffebit_set (ffebit b, ffebitCount offset, bool value, ffebitCount length)
{
  ffebitCount i;
  ffebitCount element;
  ffebitCount bitno;

  assert (offset + length <= b->size);

  for (i = 0; i < length; ++i, ++offset)
    {
      element = offset / CHAR_BIT;
      bitno = offset % CHAR_BIT;
      b->bits[element] = (((unsigned char) (value ? 1 : 0)) << bitno)
	| (b->bits[element] & ~((unsigned char) 1 << bitno));
    }
}

/* ffebit_test -- Test value of # of bits

   ffebit b;  // the ffebit object
   ffebitCount offset;	// 0..size-1
   bool value;	// FALSE (0), TRUE (1)
   ffebitCount length;	// # bits with same value
   ffebit_test(b,offset,&value,&length);

   Returns value of bits at <offset> through <offset + length - 1> in
   <value>.  If <offset> is already at the end of the bit array (if
   offset == ffebit_size(b)), <length> is set to 0 and <value> is
   undefined.  */

void
ffebit_test (ffebit b, ffebitCount offset, bool *value, ffebitCount *length)
{
  ffebitCount i;
  ffebitCount element;
  ffebitCount bitno;

  if (offset >= b->size)
    {
      assert (offset == b->size);
      *length = 0;
      return;
    }

  element = offset / CHAR_BIT;
  bitno = offset % CHAR_BIT;
  *value = (b->bits[element] & ((unsigned char) 1 << bitno)) == 0 ? FALSE : TRUE;
  *length = 1;

  for (i = b->size - offset - 1, ++offset; i != 0; --i, ++offset, ++*length)
    {
      element = offset / CHAR_BIT;
      bitno = offset % CHAR_BIT;
      if (*value
	  != ((b->bits[element] & ((unsigned char) 1 << bitno)) == 0 ? FALSE : TRUE))
	break;
    }
}
