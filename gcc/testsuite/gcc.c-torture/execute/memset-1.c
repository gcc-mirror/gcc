/* Copyright (C) 2002  Free Software Foundation.

   Test memset with various combinations of pointer alignments and lengths to
   make sure any optimizations in the library are correct.

   Written by Michael Meissner, March 9, 2002.  */

#include <string.h>

#ifndef MAX_OFFSET
#define MAX_OFFSET (sizeof (long long))
#endif

#ifndef MAX_COPY
#define MAX_COPY (10 * sizeof (long long))
#endif

#ifndef MAX_EXTRA
#define MAX_EXTRA (sizeof (long long))
#endif

#define MAX_LENGTH (MAX_OFFSET + MAX_COPY + MAX_EXTRA)

static union {
  char buf[MAX_LENGTH];
  long long align_int;
  long double align_fp;
} u;

char A = 'A';

main ()
{
  int off, len, i;
  char *p, *q;

  for (off = 0; off < MAX_OFFSET; off++)
    for (len = 1; len < MAX_COPY; len++)
      {
	for (i = 0; i < MAX_LENGTH; i++)
	  u.buf[i] = 'a';

	p = memset (u.buf + off, '\0', len);
	if (p != u.buf + off)
	  abort ();

	q = u.buf;
	for (i = 0; i < off; i++, q++)
	  if (*q != 'a')
	    abort ();

	for (i = 0; i < len; i++, q++)
	  if (*q != '\0')
	    abort ();

	for (i = 0; i < MAX_EXTRA; i++, q++)
	  if (*q != 'a')
	    abort ();

	p = memset (u.buf + off, A, len);
	if (p != u.buf + off)
	  abort ();

	q = u.buf;
	for (i = 0; i < off; i++, q++)
	  if (*q != 'a')
	    abort ();

	for (i = 0; i < len; i++, q++)
	  if (*q != 'A')
	    abort ();

	for (i = 0; i < MAX_EXTRA; i++, q++)
	  if (*q != 'a')
	    abort ();

	p = memset (u.buf + off, 'B', len);
	if (p != u.buf + off)
	  abort ();

	q = u.buf;
	for (i = 0; i < off; i++, q++)
	  if (*q != 'a')
	    abort ();

	for (i = 0; i < len; i++, q++)
	  if (*q != 'B')
	    abort ();

	for (i = 0; i < MAX_EXTRA; i++, q++)
	  if (*q != 'a')
	    abort ();
      }

  exit (0);
}
