/* Copyright (C) 2004, 2005  Free Software Foundation.

   Ensure builtin __memset_chk performs correctly.  */

extern void abort (void);
typedef __SIZE_TYPE__ size_t;
extern size_t strlen(const char *);
extern void *memcpy (void *, const void *, size_t);
extern void *memset (void *, int, size_t);
extern int memcmp (const void *, const void *, size_t);

#include "chk.h"

char buffer[32];
int argc = 1;
size_t l1 = 1;
char *s3 = "FGH";
char *s4;

void
__attribute__((noinline))
test1 (void)
{
  memset_disallowed = 1;
  chk_calls = 0;
  memset (buffer, argc, 0);
  memset (buffer, argc, 1);
  memset (buffer, argc, 2);
  memset (buffer, argc, 3);
  memset (buffer, argc, 4);
  memset (buffer, argc, 5);
  memset (buffer, argc, 6);
  memset (buffer, argc, 7);
  memset (buffer, argc, 8);
  memset (buffer, argc, 9);
  memset (buffer, argc, 10);
  memset (buffer, argc, 11);
  memset (buffer, argc, 12);
  memset (buffer, argc, 13);
  memset (buffer, argc, 14);
  memset (buffer, argc, 15);
  memset (buffer, argc, 16);
  memset (buffer, argc, 17);
  memset_disallowed = 0;
  if (chk_calls)
    abort ();
}

/* Test whether compile time checking is done where it should
   and so is runtime object size checking.  */
void
__attribute__((noinline))
test2 (void)
{
  struct A { char buf1[10]; char buf2[10]; } a;
  char *r = l1 == 1 ? &a.buf1[5] : &a.buf2[4];
  char buf3[20];
  int i;
  size_t l;

  /* The following calls should do runtime checking
     - length is not known, but destination is.  */
  chk_calls = 0;
  memset (a.buf1 + 2, 'a', l1);
  memset (r, '\0', l1 + 1);
  r = l1 == 1 ? __builtin_alloca (4) : &a.buf2[7];
  memset (r, argc, l1 + 2);
  memset (r + 2, 'Q', l1);
  r = buf3;
  for (i = 0; i < 4; ++i)
    {
      if (i == l1 - 1)
	r = &a.buf1[1];
      else if (i == l1)
	r = &a.buf2[7];
      else if (i == l1 + 1)
	r = &buf3[5];
      else if (i == l1 + 2)
	r = &a.buf1[9];
    }
  memset (r, '\0', l1);
  if (chk_calls != 5)
    abort ();

  /* Following have known destination and known length,
     so if optimizing certainly shouldn't result in the checking
     variants.  */
  chk_calls = 0;
  memset (a.buf1 + 2, '\0', 1);
  memset (r, argc, 2);
  r = l1 == 1 ? __builtin_alloca (4) : &a.buf2[7];
  memset (r, 'N', 3);
  r = buf3;
  l = 4;
  for (i = 0; i < 4; ++i)
    {
      if (i == l1 - 1)
	r = &a.buf1[1], l = 2;
      else if (i == l1)
	r = &a.buf2[7], l = 3;
      else if (i == l1 + 1)
	r = &buf3[5], l = 4;
      else if (i == l1 + 2)
	r = &a.buf1[9], l = 1;
    }
  memset (r, 'H', 1);
  /* Here, l is known to be at most 4 and __builtin_object_size (&buf3[16], 0)
     is 4, so this doesn't need runtime checking.  */
  memset (&buf3[16], 'd', l);
  /* Neither length nor destination known.  Doesn't need runtime checking.  */
  memset (s4, 'a', l1);
  memset (s4 + 2, '\0', l1 + 2);
  /* Destination unknown.  */
  memset (s4 + 4, 'b', 2);
  memset (s4 + 6, '\0', 4);
  if (chk_calls)
    abort ();
  chk_calls = 0;
}

/* Test whether runtime and/or compile time checking catches
   buffer overflows.  */
void
__attribute__((noinline))
test3 (void)
{
  struct A { char buf1[10]; char buf2[10]; } a;
  char buf3[20];

  chk_fail_allowed = 1;
  /* Runtime checks.  */
  if (__builtin_setjmp (chk_fail_buf) == 0)
    {
      memset (&a.buf2[9], '\0', l1 + 1);
      abort ();
    }
  if (__builtin_setjmp (chk_fail_buf) == 0)
    {
      memset (&a.buf2[7], 'T', strlen (s3) + 1);
      abort ();
    }
  /* This should be detectable at compile time already.  */
  if (__builtin_setjmp (chk_fail_buf) == 0)
    {
      memset (&buf3[19], 'b', 2);
      abort ();
    }
  chk_fail_allowed = 0;
}

#ifndef MAX_OFFSET
#define MAX_OFFSET (sizeof (long long))
#endif

#ifndef MAX_COPY
#define MAX_COPY (10 * sizeof (long long))
#define MAX_COPY2 15
#else
#define MAX_COPY2 MAX_COPY
#endif

#ifndef MAX_EXTRA
#define MAX_EXTRA (sizeof (long long))
#endif

#define MAX_LENGTH (MAX_OFFSET + MAX_COPY + MAX_EXTRA)
#define MAX_LENGTH2 (MAX_OFFSET + MAX_COPY2 + MAX_EXTRA)

static union {
  char buf[MAX_LENGTH];
  long long align_int;
  long double align_fp;
} u;

char A = 'A';

void
__attribute__((noinline))
test4 (void)
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
}

static union {
  char buf[MAX_LENGTH2];
  long long align_int;
  long double align_fp;
} u2;

void reset ()
{
  int i;

  for (i = 0; i < MAX_LENGTH2; i++)
    u2.buf[i] = 'a';
}

void check (int off, int len, int ch)
{
  char *q;
  int i;

  q = u2.buf;
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

void
__attribute__((noinline))
test5 (void)
{
  int off;
  char *p;

  /* len == 1 */
  for (off = 0; off < MAX_OFFSET; off++)
    {
      reset ();

      p = memset (u2.buf + off, '\0', 1);
      if (p != u2.buf + off) abort ();
      check (off, 1, '\0');

      p = memset (u2.buf + off, A, 1);
      if (p != u2.buf + off) abort ();
      check (off, 1, 'A');

      p = memset (u2.buf + off, 'B', 1);
      if (p != u2.buf + off) abort ();
      check (off, 1, 'B');
    }

  /* len == 2 */
  for (off = 0; off < MAX_OFFSET; off++)
    {
      reset ();

      p = memset (u2.buf + off, '\0', 2);
      if (p != u2.buf + off) abort ();
      check (off, 2, '\0');

      p = memset (u2.buf + off, A, 2);
      if (p != u2.buf + off) abort ();
      check (off, 2, 'A');

      p = memset (u2.buf + off, 'B', 2);
      if (p != u2.buf + off) abort ();
      check (off, 2, 'B');
    }

  /* len == 3 */
  for (off = 0; off < MAX_OFFSET; off++)
    {
      reset ();

      p = memset (u2.buf + off, '\0', 3);
      if (p != u2.buf + off) abort ();
      check (off, 3, '\0');

      p = memset (u2.buf + off, A, 3);
      if (p != u2.buf + off) abort ();
      check (off, 3, 'A');

      p = memset (u2.buf + off, 'B', 3);
      if (p != u2.buf + off) abort ();
      check (off, 3, 'B');
    }

  /* len == 4 */
  for (off = 0; off < MAX_OFFSET; off++)
    {
      reset ();

      p = memset (u2.buf + off, '\0', 4);
      if (p != u2.buf + off) abort ();
      check (off, 4, '\0');

      p = memset (u2.buf + off, A, 4);
      if (p != u2.buf + off) abort ();
      check (off, 4, 'A');

      p = memset (u2.buf + off, 'B', 4);
      if (p != u2.buf + off) abort ();
      check (off, 4, 'B');
    }

  /* len == 5 */
  for (off = 0; off < MAX_OFFSET; off++)
    {
      reset ();

      p = memset (u2.buf + off, '\0', 5);
      if (p != u2.buf + off) abort ();
      check (off, 5, '\0');

      p = memset (u2.buf + off, A, 5);
      if (p != u2.buf + off) abort ();
      check (off, 5, 'A');

      p = memset (u2.buf + off, 'B', 5);
      if (p != u2.buf + off) abort ();
      check (off, 5, 'B');
    }

  /* len == 6 */
  for (off = 0; off < MAX_OFFSET; off++)
    {
      reset ();

      p = memset (u2.buf + off, '\0', 6);
      if (p != u2.buf + off) abort ();
      check (off, 6, '\0');

      p = memset (u2.buf + off, A, 6);
      if (p != u2.buf + off) abort ();
      check (off, 6, 'A');

      p = memset (u2.buf + off, 'B', 6);
      if (p != u2.buf + off) abort ();
      check (off, 6, 'B');
    }

  /* len == 7 */
  for (off = 0; off < MAX_OFFSET; off++)
    {
      reset ();

      p = memset (u2.buf + off, '\0', 7);
      if (p != u2.buf + off) abort ();
      check (off, 7, '\0');

      p = memset (u2.buf + off, A, 7);
      if (p != u2.buf + off) abort ();
      check (off, 7, 'A');

      p = memset (u2.buf + off, 'B', 7);
      if (p != u2.buf + off) abort ();
      check (off, 7, 'B');
    }

  /* len == 8 */
  for (off = 0; off < MAX_OFFSET; off++)
    {
      reset ();

      p = memset (u2.buf + off, '\0', 8);
      if (p != u2.buf + off) abort ();
      check (off, 8, '\0');

      p = memset (u2.buf + off, A, 8);
      if (p != u2.buf + off) abort ();
      check (off, 8, 'A');

      p = memset (u2.buf + off, 'B', 8);
      if (p != u2.buf + off) abort ();
      check (off, 8, 'B');
    }

  /* len == 9 */
  for (off = 0; off < MAX_OFFSET; off++)
    {
      reset ();

      p = memset (u2.buf + off, '\0', 9);
      if (p != u2.buf + off) abort ();
      check (off, 9, '\0');

      p = memset (u2.buf + off, A, 9);
      if (p != u2.buf + off) abort ();
      check (off, 9, 'A');

      p = memset (u2.buf + off, 'B', 9);
      if (p != u2.buf + off) abort ();
      check (off, 9, 'B');
    }

  /* len == 10 */
  for (off = 0; off < MAX_OFFSET; off++)
    {
      reset ();

      p = memset (u2.buf + off, '\0', 10);
      if (p != u2.buf + off) abort ();
      check (off, 10, '\0');

      p = memset (u2.buf + off, A, 10);
      if (p != u2.buf + off) abort ();
      check (off, 10, 'A');

      p = memset (u2.buf + off, 'B', 10);
      if (p != u2.buf + off) abort ();
      check (off, 10, 'B');
    }

  /* len == 11 */
  for (off = 0; off < MAX_OFFSET; off++)
    {
      reset ();

      p = memset (u2.buf + off, '\0', 11);
      if (p != u2.buf + off) abort ();
      check (off, 11, '\0');

      p = memset (u2.buf + off, A, 11);
      if (p != u2.buf + off) abort ();
      check (off, 11, 'A');

      p = memset (u2.buf + off, 'B', 11);
      if (p != u2.buf + off) abort ();
      check (off, 11, 'B');
    }

  /* len == 12 */
  for (off = 0; off < MAX_OFFSET; off++)
    {
      reset ();

      p = memset (u2.buf + off, '\0', 12);
      if (p != u2.buf + off) abort ();
      check (off, 12, '\0');

      p = memset (u2.buf + off, A, 12);
      if (p != u2.buf + off) abort ();
      check (off, 12, 'A');

      p = memset (u2.buf + off, 'B', 12);
      if (p != u2.buf + off) abort ();
      check (off, 12, 'B');
    }

  /* len == 13 */
  for (off = 0; off < MAX_OFFSET; off++)
    {
      reset ();

      p = memset (u2.buf + off, '\0', 13);
      if (p != u2.buf + off) abort ();
      check (off, 13, '\0');

      p = memset (u2.buf + off, A, 13);
      if (p != u2.buf + off) abort ();
      check (off, 13, 'A');

      p = memset (u2.buf + off, 'B', 13);
      if (p != u2.buf + off) abort ();
      check (off, 13, 'B');
    }

  /* len == 14 */
  for (off = 0; off < MAX_OFFSET; off++)
    {
      reset ();

      p = memset (u2.buf + off, '\0', 14);
      if (p != u2.buf + off) abort ();
      check (off, 14, '\0');

      p = memset (u2.buf + off, A, 14);
      if (p != u2.buf + off) abort ();
      check (off, 14, 'A');

      p = memset (u2.buf + off, 'B', 14);
      if (p != u2.buf + off) abort ();
      check (off, 14, 'B');
    }

  /* len == 15 */
  for (off = 0; off < MAX_OFFSET; off++)
    {
      reset ();

      p = memset (u2.buf + off, '\0', 15);
      if (p != u2.buf + off) abort ();
      check (off, 15, '\0');

      p = memset (u2.buf + off, A, 15);
      if (p != u2.buf + off) abort ();
      check (off, 15, 'A');

      p = memset (u2.buf + off, 'B', 15);
      if (p != u2.buf + off) abort ();
      check (off, 15, 'B');
    }
}

void
__attribute__((noinline))
test6 (void)
{
  int len;
  char *p;

  /* off == 0 */
  for (len = 0; len < MAX_COPY2; len++)
    {
      reset ();

      p = memset (u2.buf, '\0', len);
      if (p != u2.buf) abort ();
      check (0, len, '\0');

      p = memset (u2.buf, A, len);
      if (p != u2.buf) abort ();
      check (0, len, 'A');

      p = memset (u2.buf, 'B', len);
      if (p != u2.buf) abort ();
      check (0, len, 'B');
    }

  /* off == 1 */
  for (len = 0; len < MAX_COPY2; len++)
    {
      reset ();

      p = memset (u2.buf+1, '\0', len);
      if (p != u2.buf+1) abort ();
      check (1, len, '\0');

      p = memset (u2.buf+1, A, len);
      if (p != u2.buf+1) abort ();
      check (1, len, 'A');

      p = memset (u2.buf+1, 'B', len);
      if (p != u2.buf+1) abort ();
      check (1, len, 'B');
    }

  /* off == 2 */
  for (len = 0; len < MAX_COPY2; len++)
    {
      reset ();

      p = memset (u2.buf+2, '\0', len);
      if (p != u2.buf+2) abort ();
      check (2, len, '\0');

      p = memset (u2.buf+2, A, len);
      if (p != u2.buf+2) abort ();
      check (2, len, 'A');

      p = memset (u2.buf+2, 'B', len);
      if (p != u2.buf+2) abort ();
      check (2, len, 'B');
    }

  /* off == 3 */
  for (len = 0; len < MAX_COPY2; len++)
    {
      reset ();

      p = memset (u2.buf+3, '\0', len);
      if (p != u2.buf+3) abort ();
      check (3, len, '\0');

      p = memset (u2.buf+3, A, len);
      if (p != u2.buf+3) abort ();
      check (3, len, 'A');

      p = memset (u2.buf+3, 'B', len);
      if (p != u2.buf+3) abort ();
      check (3, len, 'B');
    }

  /* off == 4 */
  for (len = 0; len < MAX_COPY2; len++)
    {
      reset ();

      p = memset (u2.buf+4, '\0', len);
      if (p != u2.buf+4) abort ();
      check (4, len, '\0');

      p = memset (u2.buf+4, A, len);
      if (p != u2.buf+4) abort ();
      check (4, len, 'A');

      p = memset (u2.buf+4, 'B', len);
      if (p != u2.buf+4) abort ();
      check (4, len, 'B');
    }

  /* off == 5 */
  for (len = 0; len < MAX_COPY2; len++)
    {
      reset ();

      p = memset (u2.buf+5, '\0', len);
      if (p != u2.buf+5) abort ();
      check (5, len, '\0');

      p = memset (u2.buf+5, A, len);
      if (p != u2.buf+5) abort ();
      check (5, len, 'A');

      p = memset (u2.buf+5, 'B', len);
      if (p != u2.buf+5) abort ();
      check (5, len, 'B');
    }

  /* off == 6 */
  for (len = 0; len < MAX_COPY2; len++)
    {
      reset ();

      p = memset (u2.buf+6, '\0', len);
      if (p != u2.buf+6) abort ();
      check (6, len, '\0');

      p = memset (u2.buf+6, A, len);
      if (p != u2.buf+6) abort ();
      check (6, len, 'A');

      p = memset (u2.buf+6, 'B', len);
      if (p != u2.buf+6) abort ();
      check (6, len, 'B');
    }

  /* off == 7 */
  for (len = 0; len < MAX_COPY2; len++)
    {
      reset ();

      p = memset (u2.buf+7, '\0', len);
      if (p != u2.buf+7) abort ();
      check (7, len, '\0');

      p = memset (u2.buf+7, A, len);
      if (p != u2.buf+7) abort ();
      check (7, len, 'A');

      p = memset (u2.buf+7, 'B', len);
      if (p != u2.buf+7) abort ();
      check (7, len, 'B');
    }
}

void
main_test (void)
{
#ifndef __OPTIMIZE__
  /* Object size checking is only intended for -O[s123].  */
  return;
#endif
  __asm ("" : "=r" (l1) : "0" (l1));
  s4 = buffer;
  test1 ();
  test2 ();
  test3 ();
  test4 ();
  test5 ();
  test6 ();
}
