/* Copyright (C) 2002  Free Software Foundation.

   Test memcpy with various combinations of pointer alignments and lengths to
   make sure any optimizations in the library are correct.

   Written by Michael Meissner, March 9, 2002.  */

#include <string.h>

#ifndef MAX_OFFSET
#define MAX_OFFSET (sizeof (long long))
#endif

#ifndef MAX_COPY
#define MAX_COPY (8 * sizeof (long long))
#endif

#ifndef MAX_EXTRA
#define MAX_EXTRA (sizeof (long long))
#endif

#define MAX_LENGTH (MAX_OFFSET + MAX_COPY + MAX_EXTRA)

static union {
  char buf[MAX_LENGTH];
  long long align_int;
  long double align_fp;
} u1, u2;

main ()
{
  int off1, off2, len, i;
  char *p, *q;

  for (off1 = 0; off1 < MAX_OFFSET; off1++)
    for (off2 = 0; off2 < MAX_OFFSET; off2++)
      for (len = 1; len < MAX_COPY; len++)
	{
	  for (i = 0; i < MAX_LENGTH; i++)
	    {
	      u1.buf[i] = 'a';
	      u2.buf[i] = 'A';
	    }

	  p = memcpy (u1.buf + off1, u2.buf + off2, len);
	  if (p != u1.buf + off1)
	    abort ();

	  q = u1.buf;
	  for (i = 0; i < off1; i++, q++)
	    if (*q != 'a')
	      abort ();

	  for (i = 0; i < len; i++, q++)
	    if (*q != 'A')
	      abort ();

	  for (i = 0; i < MAX_EXTRA; i++, q++)
	    if (*q != 'a')
	      abort ();
	}

  exit (0);
}
