/* Copyright (C) 2002  Free Software Foundation.

   Test strlen with various combinations of pointer alignments and lengths to
   make sure any optimizations in the library are correct.

   Written by Michael Meissner, March 9, 2002.  */

#include <string.h>
#include <stddef.h>

#ifndef MAX_OFFSET
#define MAX_OFFSET (sizeof (long long))
#endif

#ifndef MAX_TEST
#define MAX_TEST (8 * sizeof (long long))
#endif

#ifndef MAX_EXTRA
#define MAX_EXTRA (sizeof (long long))
#endif

#define MAX_LENGTH (MAX_OFFSET + MAX_TEST + MAX_EXTRA + 1)

static union {
  char buf[MAX_LENGTH];
  long long align_int;
  long double align_fp;
} u;

main ()
{
  size_t off, len, len2, i;
  char *p;

  for (off = 0; off < MAX_OFFSET; off++)
    for (len = 0; len < MAX_TEST; len++)
      {
	p = u.buf;
	for (i = 0; i < off; i++)
	  *p++ = '\0';

	for (i = 0; i < len; i++)
	  *p++ = 'a';

	*p++ = '\0';
	for (i = 0; i < MAX_EXTRA; i++)
	  *p++ = 'b';

	p = u.buf + off;
	len2 = strlen (p);
	if (len != len2)
	  abort ();
      }

  exit (0);
}
