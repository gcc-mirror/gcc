/* Copyright (C) 2002  Free Software Foundation.

   Test strncmp with various combinations of pointer alignments and lengths to
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

#define MAX_LENGTH (MAX_OFFSET + MAX_TEST + MAX_EXTRA)

static union {
  unsigned char buf[MAX_LENGTH];
  long long align_int;
  long double align_fp;
} u1, u2;

void
test (const unsigned char *s1, const unsigned char *s2, size_t len, int expected)
{
  int value = strncmp ((char *) s1, (char *) s2, len);

  if (expected < 0 && value >= 0)
    abort ();
  else if (expected == 0 && value != 0)
    abort ();
  else if (expected > 0 && value <= 0)
    abort ();
}

main ()
{
  size_t off1, off2, len, i;
  unsigned char *buf1, *buf2;
  unsigned char *mod1, *mod2;
  unsigned char *p1, *p2;

  for (off1 = 0; off1 < MAX_OFFSET; off1++)
    for (off2 = 0; off2 < MAX_OFFSET; off2++)
      for (len = 0; len < MAX_TEST; len++)
	{
	  p1 = u1.buf;
	  for (i = 0; i < off1; i++)
	    *p1++ = '\0';

	  buf1 = p1;
	  for (i = 0; i < len; i++)
	    *p1++ = 'a';

	  mod1 = p1;
	  for (i = 0; i < MAX_EXTRA; i++)
	    *p1++ = 'x';

	  p2 = u2.buf;
	  for (i = 0; i < off2; i++)
	    *p2++ = '\0';

	  buf2 = p2;
	  for (i = 0; i < len; i++)
	    *p2++ = 'a';

	  mod2 = p2;
	  for (i = 0; i < MAX_EXTRA; i++)
	    *p2++ = 'x';

	  mod1[0] = '\0';
	  mod2[0] = '\0';
	  test (buf1, buf2, MAX_LENGTH, 0);
	  test (buf1, buf2, len, 0);

	  mod1[0] = 'a';
	  mod1[1] = '\0';
	  mod2[0] = '\0';
	  test (buf1, buf2, MAX_LENGTH, +1);
	  test (buf1, buf2, len, 0);

	  mod1[0] = '\0';
	  mod2[0] = 'a';
	  mod2[1] = '\0';
	  test (buf1, buf2, MAX_LENGTH, -1);
	  test (buf1, buf2, len, 0);

	  mod1[0] = 'b';
	  mod1[1] = '\0';
	  mod2[0] = 'c';
	  mod2[1] = '\0';
	  test (buf1, buf2, MAX_LENGTH, -1);
	  test (buf1, buf2, len, 0);

	  mod1[0] = 'c';
	  mod1[1] = '\0';
	  mod2[0] = 'b';
	  mod2[1] = '\0';
	  test (buf1, buf2, MAX_LENGTH, +1);
	  test (buf1, buf2, len, 0);

	  mod1[0] = 'b';
	  mod1[1] = '\0';
	  mod2[0] = (unsigned char)'\251';
	  mod2[1] = '\0';
	  test (buf1, buf2, MAX_LENGTH, -1);
	  test (buf1, buf2, len, 0);

	  mod1[0] = (unsigned char)'\251';
	  mod1[1] = '\0';
	  mod2[0] = 'b';
	  mod2[1] = '\0';
	  test (buf1, buf2, MAX_LENGTH, +1);
	  test (buf1, buf2, len, 0);

	  mod1[0] = (unsigned char)'\251';
	  mod1[1] = '\0';
	  mod2[0] = (unsigned char)'\252';
	  mod2[1] = '\0';
	  test (buf1, buf2, MAX_LENGTH, -1);
	  test (buf1, buf2, len, 0);

	  mod1[0] = (unsigned char)'\252';
	  mod1[1] = '\0';
	  mod2[0] = (unsigned char)'\251';
	  mod2[1] = '\0';
	  test (buf1, buf2, MAX_LENGTH, +1);
	  test (buf1, buf2, len, 0);
	}

  exit (0);
}
