/* Copyright (C) 2002  Free Software Foundation.

   Test memset with various combinations of pointer alignments and constant
   lengths to make sure any optimizations in the compiler are correct.

   Written by Roger Sayle, April 22, 2002.  */

#include <string.h>

void abort (void);
void exit (int);

#ifndef MAX_OFFSET
#define MAX_OFFSET (sizeof (long long))
#endif

#ifndef MAX_COPY
#define MAX_COPY 15
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

void reset ()
{
  int i;

  for (i = 0; i < MAX_LENGTH; i++)
    u.buf[i] = 'a';
}

void check (int off, int len, int ch)
{
  char *q;
  int i;

  q = u.buf;
  for (i = 0; i < off; i++, q++)
    if (*q != 'a')
      abort ();

  for (i = 0; i < len; i++, q++)
    if (*q != ch)
      abort ();

  for (i = 0; i < MAX_EXTRA; i++, q++)
    if (*q != 'a')
      abort ();
}

int main ()
{
  int off;
  char *p;

  /* len == 1 */
  for (off = 0; off < MAX_OFFSET; off++)
    {
      reset ();

      p = memset (u.buf + off, '\0', 1);
      if (p != u.buf + off) abort ();
      check (off, 1, '\0');

      p = memset (u.buf + off, A, 1);
      if (p != u.buf + off) abort ();
      check (off, 1, 'A');

      p = memset (u.buf + off, 'B', 1);
      if (p != u.buf + off) abort ();
      check (off, 1, 'B');
    }

  /* len == 2 */
  for (off = 0; off < MAX_OFFSET; off++)
    {
      reset ();

      p = memset (u.buf + off, '\0', 2);
      if (p != u.buf + off) abort ();
      check (off, 2, '\0');

      p = memset (u.buf + off, A, 2);
      if (p != u.buf + off) abort ();
      check (off, 2, 'A');

      p = memset (u.buf + off, 'B', 2);
      if (p != u.buf + off) abort ();
      check (off, 2, 'B');
    }

  /* len == 3 */
  for (off = 0; off < MAX_OFFSET; off++)
    {
      reset ();

      p = memset (u.buf + off, '\0', 3);
      if (p != u.buf + off) abort ();
      check (off, 3, '\0');

      p = memset (u.buf + off, A, 3);
      if (p != u.buf + off) abort ();
      check (off, 3, 'A');

      p = memset (u.buf + off, 'B', 3);
      if (p != u.buf + off) abort ();
      check (off, 3, 'B');
    }

  /* len == 4 */
  for (off = 0; off < MAX_OFFSET; off++)
    {
      reset ();

      p = memset (u.buf + off, '\0', 4);
      if (p != u.buf + off) abort ();
      check (off, 4, '\0');

      p = memset (u.buf + off, A, 4);
      if (p != u.buf + off) abort ();
      check (off, 4, 'A');

      p = memset (u.buf + off, 'B', 4);
      if (p != u.buf + off) abort ();
      check (off, 4, 'B');
    }

  /* len == 5 */
  for (off = 0; off < MAX_OFFSET; off++)
    {
      reset ();

      p = memset (u.buf + off, '\0', 5);
      if (p != u.buf + off) abort ();
      check (off, 5, '\0');

      p = memset (u.buf + off, A, 5);
      if (p != u.buf + off) abort ();
      check (off, 5, 'A');

      p = memset (u.buf + off, 'B', 5);
      if (p != u.buf + off) abort ();
      check (off, 5, 'B');
    }

  /* len == 6 */
  for (off = 0; off < MAX_OFFSET; off++)
    {
      reset ();

      p = memset (u.buf + off, '\0', 6);
      if (p != u.buf + off) abort ();
      check (off, 6, '\0');

      p = memset (u.buf + off, A, 6);
      if (p != u.buf + off) abort ();
      check (off, 6, 'A');

      p = memset (u.buf + off, 'B', 6);
      if (p != u.buf + off) abort ();
      check (off, 6, 'B');
    }

  /* len == 7 */
  for (off = 0; off < MAX_OFFSET; off++)
    {
      reset ();

      p = memset (u.buf + off, '\0', 7);
      if (p != u.buf + off) abort ();
      check (off, 7, '\0');

      p = memset (u.buf + off, A, 7);
      if (p != u.buf + off) abort ();
      check (off, 7, 'A');

      p = memset (u.buf + off, 'B', 7);
      if (p != u.buf + off) abort ();
      check (off, 7, 'B');
    }

  /* len == 8 */
  for (off = 0; off < MAX_OFFSET; off++)
    {
      reset ();

      p = memset (u.buf + off, '\0', 8);
      if (p != u.buf + off) abort ();
      check (off, 8, '\0');

      p = memset (u.buf + off, A, 8);
      if (p != u.buf + off) abort ();
      check (off, 8, 'A');

      p = memset (u.buf + off, 'B', 8);
      if (p != u.buf + off) abort ();
      check (off, 8, 'B');
    }

  /* len == 9 */
  for (off = 0; off < MAX_OFFSET; off++)
    {
      reset ();

      p = memset (u.buf + off, '\0', 9);
      if (p != u.buf + off) abort ();
      check (off, 9, '\0');

      p = memset (u.buf + off, A, 9);
      if (p != u.buf + off) abort ();
      check (off, 9, 'A');

      p = memset (u.buf + off, 'B', 9);
      if (p != u.buf + off) abort ();
      check (off, 9, 'B');
    }

  /* len == 10 */
  for (off = 0; off < MAX_OFFSET; off++)
    {
      reset ();

      p = memset (u.buf + off, '\0', 10);
      if (p != u.buf + off) abort ();
      check (off, 10, '\0');

      p = memset (u.buf + off, A, 10);
      if (p != u.buf + off) abort ();
      check (off, 10, 'A');

      p = memset (u.buf + off, 'B', 10);
      if (p != u.buf + off) abort ();
      check (off, 10, 'B');
    }

  /* len == 11 */
  for (off = 0; off < MAX_OFFSET; off++)
    {
      reset ();

      p = memset (u.buf + off, '\0', 11);
      if (p != u.buf + off) abort ();
      check (off, 11, '\0');

      p = memset (u.buf + off, A, 11);
      if (p != u.buf + off) abort ();
      check (off, 11, 'A');

      p = memset (u.buf + off, 'B', 11);
      if (p != u.buf + off) abort ();
      check (off, 11, 'B');
    }

  /* len == 12 */
  for (off = 0; off < MAX_OFFSET; off++)
    {
      reset ();

      p = memset (u.buf + off, '\0', 12);
      if (p != u.buf + off) abort ();
      check (off, 12, '\0');

      p = memset (u.buf + off, A, 12);
      if (p != u.buf + off) abort ();
      check (off, 12, 'A');

      p = memset (u.buf + off, 'B', 12);
      if (p != u.buf + off) abort ();
      check (off, 12, 'B');
    }

  /* len == 13 */
  for (off = 0; off < MAX_OFFSET; off++)
    {
      reset ();

      p = memset (u.buf + off, '\0', 13);
      if (p != u.buf + off) abort ();
      check (off, 13, '\0');

      p = memset (u.buf + off, A, 13);
      if (p != u.buf + off) abort ();
      check (off, 13, 'A');

      p = memset (u.buf + off, 'B', 13);
      if (p != u.buf + off) abort ();
      check (off, 13, 'B');
    }

  /* len == 14 */
  for (off = 0; off < MAX_OFFSET; off++)
    {
      reset ();

      p = memset (u.buf + off, '\0', 14);
      if (p != u.buf + off) abort ();
      check (off, 14, '\0');

      p = memset (u.buf + off, A, 14);
      if (p != u.buf + off) abort ();
      check (off, 14, 'A');

      p = memset (u.buf + off, 'B', 14);
      if (p != u.buf + off) abort ();
      check (off, 14, 'B');
    }

  /* len == 15 */
  for (off = 0; off < MAX_OFFSET; off++)
    {
      reset ();

      p = memset (u.buf + off, '\0', 15);
      if (p != u.buf + off) abort ();
      check (off, 15, '\0');

      p = memset (u.buf + off, A, 15);
      if (p != u.buf + off) abort ();
      check (off, 15, 'A');

      p = memset (u.buf + off, 'B', 15);
      if (p != u.buf + off) abort ();
      check (off, 15, 'B');
    }

  exit (0);
}

