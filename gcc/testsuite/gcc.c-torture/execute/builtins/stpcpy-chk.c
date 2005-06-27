/* Copyright (C) 2004, 2005  Free Software Foundation.

   Ensure builtin __stpcpy_chk performs correctly.  */

extern void abort (void);
typedef __SIZE_TYPE__ size_t;
extern size_t strlen(const char *);
extern void *memcpy (void *, const void *, size_t);
extern char *stpcpy (char *, const char *);
extern int memcmp (const void *, const void *, size_t);

#include "chk.h"

const char s1[] = "123";
char p[32] = "";
char *s2 = "defg";
char *s3 = "FGH";
char *s4;
size_t l1 = 1;

void
__attribute__((noinline))
test1 (void)
{
  int i = 8;

#if defined __i386__ || defined __x86_64__
  /* The functions below might not be optimized into direct stores on all
     arches.  It depends on how many instructions would be generated and
     what limits the architecture chooses in STORE_BY_PIECES_P.  */
  stpcpy_disallowed = 1;
#endif
  if (stpcpy (p, "abcde") != p + 5 || memcmp (p, "abcde", 6))
    abort ();
  if (stpcpy (p + 16, "vwxyz" + 1) != p + 16 + 4 || memcmp (p + 16, "wxyz", 5))
    abort ();
  if (stpcpy (p + 1, "") != p + 1 + 0 || memcmp (p, "a\0cde", 6))
    abort ();
  if (stpcpy (p + 3, "fghij") != p + 3 + 5 || memcmp (p, "a\0cfghij", 9))
    abort ();

  if (stpcpy ((i++, p + 20 + 1), "23") != (p + 20 + 1 + 2)
      || i != 9 || memcmp (p + 19, "z\0""23\0", 5))
    abort ();

  if (stpcpy (stpcpy (p, "ABCD"), "EFG") != p + 7 || memcmp (p, "ABCDEFG", 8))
    abort();

  /* Test at least one instance of the __builtin_ style.  We do this
     to ensure that it works and that the prototype is correct.  */
  if (__builtin_stpcpy (p, "abcde") != p + 5 || memcmp (p, "abcde", 6))
    abort ();

  /* If return value of stpcpy is ignored, it should be optimized into
     strcpy call.  */
  stpcpy_disallowed = 1;
  stpcpy (p + 1, "abcd");
  stpcpy_disallowed = 0;
  if (memcmp (p, "aabcd", 6))
    abort ();

  if (chk_calls)
    abort ();

  chk_calls = 0;
  strcpy_disallowed = 1;
  if (stpcpy (p, s2) != p + 4 || memcmp (p, "defg\0", 6))
    abort ();
  strcpy_disallowed = 0;
  stpcpy_disallowed = 1;
  stpcpy (p + 2, s3);
  stpcpy_disallowed = 0;
  if (memcmp (p, "deFGH", 6))
    abort ();
  if (chk_calls != 2)
    abort ();
}

#ifndef MAX_OFFSET
#define MAX_OFFSET (sizeof (long long))
#endif

#ifndef MAX_COPY
#define MAX_COPY (10 * sizeof (long long))
#endif

#ifndef MAX_EXTRA
#define MAX_EXTRA (sizeof (long long))
#endif

#define MAX_LENGTH (MAX_OFFSET + MAX_COPY + 1 + MAX_EXTRA)

/* Use a sequence length that is not divisible by two, to make it more
   likely to detect when words are mixed up.  */
#define SEQUENCE_LENGTH 31

static union {
  char buf[MAX_LENGTH];
  long long align_int;
  long double align_fp;
} u1, u2;

volatile char *vx;

void
__attribute__((noinline))
test2 (void)
{
  int off1, off2, len, i;
  char *p, *q, c;

  for (off1 = 0; off1 < MAX_OFFSET; off1++)
    for (off2 = 0; off2 < MAX_OFFSET; off2++)
      for (len = 1; len < MAX_COPY; len++)
	{
	  for (i = 0, c = 'A'; i < MAX_LENGTH; i++, c++)
	    {
	      u1.buf[i] = 'a';
	      if (c >= 'A' + SEQUENCE_LENGTH)
		c = 'A';
	      u2.buf[i] = c;
	    }
	  u2.buf[off2 + len] = '\0';

	  p = stpcpy (u1.buf + off1, u2.buf + off2);
	  if (p != u1.buf + off1 + len)
	    abort ();

	  q = u1.buf;
	  for (i = 0; i < off1; i++, q++)
	    if (*q != 'a')
	      abort ();

	  for (i = 0, c = 'A' + off2; i < len; i++, q++, c++)
	    {
	      if (c >= 'A' + SEQUENCE_LENGTH)
		c = 'A';
	      if (*q != c)
		abort ();
	    }

	  if (*q++ != '\0')
	    abort ();
	  for (i = 0; i < MAX_EXTRA; i++, q++)
	    if (*q != 'a')
	      abort ();
	}
}

/* Test whether compile time checking is done where it should
   and so is runtime object size checking.  */
void
__attribute__((noinline))
test3 (void)
{
  struct A { char buf1[10]; char buf2[10]; } a;
  char *r = l1 == 1 ? &a.buf1[5] : &a.buf2[4];
  char buf3[20];
  int i;
  const char *l;

  /* The following calls should do runtime checking
     - source length is not known, but destination is.  */
  chk_calls = 0;
  vx = stpcpy (a.buf1 + 2, s3 + 3);
  vx = stpcpy (r, s3 + 2);
  r = l1 == 1 ? __builtin_alloca (4) : &a.buf2[7];
  vx = stpcpy (r, s2 + 2);
  vx = stpcpy (r + 2, s3 + 3);
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
  vx = stpcpy (r, s2 + 4);
  if (chk_calls != 5)
    abort ();

  /* Following have known destination and known source length,
     so if optimizing certainly shouldn't result in the checking
     variants.  */
  chk_calls = 0;
  vx = stpcpy (a.buf1 + 2, "");
  vx = stpcpy (r, "a");
  r = l1 == 1 ? __builtin_alloca (4) : &a.buf2[7];
  vx = stpcpy (r, s1 + 1);
  r = buf3;
  l = "abc";
  for (i = 0; i < 4; ++i)
    {
      if (i == l1 - 1)
	r = &a.buf1[1], l = "e";
      else if (i == l1)
	r = &a.buf2[7], l = "gh";
      else if (i == l1 + 1)
	r = &buf3[5], l = "jkl";
      else if (i == l1 + 2)
	r = &a.buf1[9], l = "";
    }
  vx = stpcpy (r, "");
  /* Here, strlen (l) + 1 is known to be at most 4 and
     __builtin_object_size (&buf3[16], 0) is 4, so this doesn't need
     runtime checking.  */
  vx = stpcpy (&buf3[16], l);
  /* Unknown destination and source, no checking.  */
  vx = stpcpy (s4, s3);
  stpcpy (s4 + 4, s3);
  if (chk_calls)
    abort ();
  chk_calls = 0;
}

/* Test whether runtime and/or compile time checking catches
   buffer overflows.  */
void
__attribute__((noinline))
test4 (void)
{
  struct A { char buf1[10]; char buf2[10]; } a;
  char buf3[20];

  chk_fail_allowed = 1;
  /* Runtime checks.  */
  if (__builtin_setjmp (chk_fail_buf) == 0)
    {
      vx = stpcpy (&a.buf2[9], s2 + 3);
      abort ();
    }
  if (__builtin_setjmp (chk_fail_buf) == 0)
    {
      vx = stpcpy (&a.buf2[7], s3 + strlen (s3) - 3);
      abort ();
    }
  /* This should be detectable at compile time already.  */
  if (__builtin_setjmp (chk_fail_buf) == 0)
    {
      vx = stpcpy (&buf3[19], "a");
      abort ();
    }
  chk_fail_allowed = 0;
}

void
main_test (void)
{
#ifndef __OPTIMIZE__
  /* Object size checking is only intended for -O[s123].  */
  return;
#endif
  __asm ("" : "=r" (s2) : "0" (s2));
  __asm ("" : "=r" (s3) : "0" (s3));
  __asm ("" : "=r" (l1) : "0" (l1));
  test1 ();
  s4 = p;
  test2 ();
  test3 ();
  test4 ();
}
