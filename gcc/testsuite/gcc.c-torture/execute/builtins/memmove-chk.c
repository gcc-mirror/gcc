/* Copyright (C) 2004, 2005  Free Software Foundation.

   Ensure builtin __memcpy_chk performs correctly.  */

extern void abort (void);
typedef __SIZE_TYPE__ size_t;
extern size_t strlen(const char *);
extern void *memcpy (void *, const void *, size_t);
extern void *memmove (void *, const void *, size_t);
extern int memcmp (const void *, const void *, size_t);

#include "chk.h"

const char s1[] = "123"; 
char p[32] = "";
volatile char *s2 = "defg"; /* prevent constant propagation to happen when whole program assumptions are made.  */
volatile char *s3 = "FGH"; /* prevent constant propagation to happen when whole program assumptions are made.  */
volatile size_t l1 = 1; /* prevent constant propagation to happen when whole program assumptions are made.  */

void
__attribute__((noinline))
test1 (void)
{
  int i;

#if defined __i386__ || defined __x86_64__
  /* The functions below might not be optimized into direct stores on all
     arches.  It depends on how many instructions would be generated and
     what limits the architecture chooses in STORE_BY_PIECES_P.  */
  memmove_disallowed = 1;
  memcpy_disallowed = 1;
#endif

  /* All the memmove calls in this routine except last have fixed length, so
     object size checking should be done at compile time if optimizing.  */
  chk_calls = 0;

  if (memmove (p, "ABCDE", 6) != p || memcmp (p, "ABCDE", 6))
    abort ();
  if (memmove (p + 16, "VWX" + 1, 2) != p + 16
      || memcmp (p + 16, "WX\0\0", 5))
    abort ();
  if (memmove (p + 1, "", 1) != p + 1 || memcmp (p, "A\0CDE", 6))
    abort ();
  if (memmove (p + 3, "FGHI", 4) != p + 3 || memcmp (p, "A\0CFGHI", 8))
    abort ();

  i = 8;
  memmove (p + 20, "qrstu", 6);
  memmove (p + 25, "QRSTU", 6);
  if (memmove (p + 25 + 1, s1, 3) != p + 25 + 1
      || memcmp (p + 25, "Q123U", 6))
    abort ();

  if (memmove (memmove (p, "abcdEFG", 4) + 4, "efg", 4) != p + 4
      || memcmp (p, "abcdefg", 8))
    abort();

  /* Test at least one instance of the __builtin_ style.  We do this
     to ensure that it works and that the prototype is correct.  */
  if (__builtin_memmove (p, "ABCDE", 6) != p || memcmp (p, "ABCDE", 6))
    abort ();

  memmove (p + 5, s3, 1);
  if (memcmp (p, "ABCDEFg", 8))
    abort ();

  memmove_disallowed = 0;
  memcpy_disallowed = 0;
  if (chk_calls)
    abort ();
  chk_calls = 0;

  memmove (p + 6, s1 + 1, l1);
  if (memcmp (p, "ABCDEF2", 8))
    abort ();

  /* The above memmove copies into an object with known size, but
     unknown length, so it should be a __memmove_chk call.  */
  if (chk_calls != 1)
    abort ();
}

long buf1[64];
char *buf2 = (char *) (buf1 + 32);
long buf5[20];
char buf7[20];

void
__attribute__((noinline))
test2_sub (long *buf3, char *buf4, char *buf6, int n)
{
  int i = 0;

  /* All the memmove/__builtin_memmove/__builtin___memmove_chk
     calls in this routine are either fixed length, or have
     side-effects in __builtin_object_size arguments, or
     dst doesn't point into a known object.  */
  chk_calls = 0;

  /* These should probably be handled by store_by_pieces on most arches.  */
  if (memmove (buf1, "ABCDEFGHI", 9) != (char *) buf1
      || memcmp (buf1, "ABCDEFGHI\0", 11))
    abort ();

  if (memmove (buf1, "abcdefghijklmnopq", 17) != (char *) buf1
      || memcmp (buf1, "abcdefghijklmnopq\0", 19))
    abort ();

  if (__builtin_memmove (buf3, "ABCDEF", 6) != (char *) buf1
      || memcmp (buf1, "ABCDEFghijklmnopq\0", 19))
    abort ();

  if (__builtin_memmove (buf3, "a", 1) != (char *) buf1
      || memcmp (buf1, "aBCDEFghijklmnopq\0", 19))
    abort ();

  if (memmove ((char *) buf3 + 2, "bcd" + ++i, 2) != (char *) buf1 + 2
      || memcmp (buf1, "aBcdEFghijklmnopq\0", 19)
      || i != 1)
    abort ();

  /* These should probably be handled by move_by_pieces on most arches.  */
  if (memmove ((char *) buf3 + 4, buf5, 6) != (char *) buf1 + 4
      || memcmp (buf1, "aBcdRSTUVWklmnopq\0", 19))
    abort ();

  if (__builtin_memmove ((char *) buf1 + ++i + 8, (char *) buf5 + 1, 1)
      != (char *) buf1 + 10
      || memcmp (buf1, "aBcdRSTUVWSlmnopq\0", 19)
      || i != 2)
    abort ();

  if (memmove ((char *) buf3 + 14, buf6, 2) != (char *) buf1 + 14
      || memcmp (buf1, "aBcdRSTUVWSlmnrsq\0", 19))
    abort ();

  if (memmove (buf3, buf5, 8) != (char *) buf1
      || memcmp (buf1, "RSTUVWXYVWSlmnrsq\0", 19))
    abort ();

  if (memmove (buf3, buf5, 17) != (char *) buf1
      || memcmp (buf1, "RSTUVWXYZ01234567\0", 19))
    abort ();

  __builtin_memmove (buf3, "aBcdEFghijklmnopq\0", 19);

  /* These should be handled either by movmemendM or memmove
     call.  */

  /* buf3 points to an unknown object, so __memmove_chk should not be done.  */
  if (memmove ((char *) buf3 + 4, buf5, n + 6) != (char *) buf1 + 4
      || memcmp (buf1, "aBcdRSTUVWklmnopq\0", 19))
    abort ();

  /* This call has side-effects in dst, therefore no checking.  */
  if (__builtin___memmove_chk ((char *) buf1 + ++i + 8, (char *) buf5 + 1,
			       n + 1, os ((char *) buf1 + ++i + 8))
      != (char *) buf1 + 11
      || memcmp (buf1, "aBcdRSTUVWkSmnopq\0", 19)
      || i != 3)
    abort ();

  if (memmove ((char *) buf3 + 14, buf6, n + 2) != (char *) buf1 + 14
      || memcmp (buf1, "aBcdRSTUVWkSmnrsq\0", 19))
    abort ();

  i = 1;

  /* These might be handled by store_by_pieces.  */
  if (memmove (buf2, "ABCDEFGHI", 9) != buf2
      || memcmp (buf2, "ABCDEFGHI\0", 11))
    abort ();

  if (memmove (buf2, "abcdefghijklmnopq", 17) != buf2
      || memcmp (buf2, "abcdefghijklmnopq\0", 19))
    abort ();

  if (__builtin_memmove (buf4, "ABCDEF", 6) != buf2
      || memcmp (buf2, "ABCDEFghijklmnopq\0", 19))
    abort ();

  if (__builtin_memmove (buf4, "a", 1) != buf2
      || memcmp (buf2, "aBCDEFghijklmnopq\0", 19))
    abort ();

  if (memmove (buf4 + 2, "bcd" + i++, 2) != buf2 + 2
      || memcmp (buf2, "aBcdEFghijklmnopq\0", 19)
      || i != 2)
    abort ();

  /* These might be handled by move_by_pieces.  */
  if (memmove (buf4 + 4, buf7, 6) != buf2 + 4
      || memcmp (buf2, "aBcdRSTUVWklmnopq\0", 19))
    abort ();

  /* Side effect.  */
  if (__builtin___memmove_chk (buf2 + i++ + 8, buf7 + 1, 1,
			       os (buf2 + i++ + 8))
      != buf2 + 10
      || memcmp (buf2, "aBcdRSTUVWSlmnopq\0", 19)
      || i != 3)
    abort ();

  if (memmove (buf4 + 14, buf6, 2) != buf2 + 14
      || memcmp (buf2, "aBcdRSTUVWSlmnrsq\0", 19))
    abort ();

  __builtin_memmove (buf4, "aBcdEFghijklmnopq\0", 19);

  /* These should be handled either by movmemendM or memmove
     call.  */
  if (memmove (buf4 + 4, buf7, n + 6) != buf2 + 4
      || memcmp (buf2, "aBcdRSTUVWklmnopq\0", 19))
    abort ();

  /* Side effect.  */
  if (__builtin___memmove_chk (buf2 + i++ + 8, buf7 + 1, n + 1,
			       os (buf2 + i++ + 8))
      != buf2 + 11
      || memcmp (buf2, "aBcdRSTUVWkSmnopq\0", 19)
      || i != 4)
    abort ();

  if (memmove (buf4 + 14, buf6, n + 2) != buf2 + 14
      || memcmp (buf2, "aBcdRSTUVWkSmnrsq\0", 19))
    abort ();

  if (chk_calls)
    abort ();
}

void
__attribute__((noinline))
test2 (void)
{
  long *x;
  char *y;
  int z;
  __builtin_memmove (buf5, "RSTUVWXYZ0123456789", 20);
  __builtin_memmove (buf7, "RSTUVWXYZ0123456789", 20);
 __asm ("" : "=r" (x) : "0" (buf1));
 __asm ("" : "=r" (y) : "0" (buf2));
 __asm ("" : "=r" (z) : "0" (0));
  test2_sub (x, y, "rstuvwxyz", z);
}

static const struct foo
{
  char *s;
  double d;
  long l;
} foo[] =
{
  { "hello world1", 3.14159, 101L },
  { "hello world2", 3.14159, 102L },
  { "hello world3", 3.14159, 103L },
  { "hello world4", 3.14159, 104L },
  { "hello world5", 3.14159, 105L },
  { "hello world6", 3.14159, 106L }
};

static const struct bar
{
  char *s;
  const struct foo f[3];
} bar[] =
{
  {
    "hello world10",
    {
      { "hello1", 3.14159, 201L },
      { "hello2", 3.14159, 202L },
      { "hello3", 3.14159, 203L },
    }
  },
  {
    "hello world11",
    {
      { "hello4", 3.14159, 204L },
      { "hello5", 3.14159, 205L },
      { "hello6", 3.14159, 206L },
    }
  }
};

static const int baz[] = { 1, 2, 3, 4, 5, 6, 7, 8, 9, 0 };

void
__attribute__((noinline))
test3 (void)
{
  const char *s;
  struct foo f1[sizeof foo/sizeof*foo];
  struct bar b1[sizeof bar/sizeof*bar];
  int bz[sizeof baz/sizeof*baz];

  /* All the memmove/__builtin_memmove calls in this routine have fixed
     length.  */
  chk_calls = 0;

  /* All the *memmove calls below have src in read-only memory, so all
     of them should be optimized into memcpy.  */
  memmove_disallowed = 1;
  if (memmove (f1, foo, sizeof (foo)) != f1 || memcmp (f1, foo, sizeof (foo)))
    abort ();
  if (memmove (b1, bar, sizeof (bar)) != b1 || memcmp (b1, bar, sizeof (bar)))
    abort ();
  memmove (bz, baz, sizeof (baz));
  if (memcmp (bz, baz, sizeof (baz)))
    abort ();

  if (memmove (p, "abcde", 6) != p || memcmp (p, "abcde", 6))
    abort ();
  s = s1;
  if (memmove (p + 2, ++s, 0) != p + 2 || memcmp (p, "abcde", 6) || s != s1 + 1)
    abort ();
  if (__builtin_memmove (p + 3, "", 1) != p + 3 || memcmp (p, "abc\0e", 6))
    abort ();
  memmove (p + 2, "fghijk", 4);
  if (memcmp (p, "abfghi", 7))
    abort ();
  s = s1 + 1;
  memmove (p + 1, s++, 0);
  if (memcmp (p, "abfghi", 7) || s != s1 + 2)
    abort ();
  __builtin_memmove (p + 4, "ABCDE", 1);
  if (memcmp (p, "abfgAi", 7))
    abort ();

  /* memmove with length 1 can be optimized into memcpy if it can be
     expanded inline.  */
  if (memmove (p + 2, p + 3, 1) != p + 2)
    abort ();
  if (memcmp (p, "abggAi", 7))
    abort ();

  if (chk_calls)
    abort ();
  memmove_disallowed = 0;
}

/* Test whether compile time checking is done where it should
   and so is runtime object size checking.  */
void
__attribute__((noinline))
test4 (void)
{
  struct A { char buf1[10]; char buf2[10]; } a;
  char *r = l1 == 1 ? &a.buf1[5] : &a.buf2[4];
  char buf3[20];
  int i;
  size_t l;

  /* The following calls should do runtime checking
     - length is not known, but destination is.  */
  chk_calls = 0;
  memmove (a.buf1 + 2, s3, l1);
  memmove (r, s3, l1 + 1);
  r = l1 == 1 ? __builtin_alloca (4) : &a.buf2[7];
  memmove (r, s2, l1 + 2);
  memmove (r + 2, s3, l1);
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
  memmove (r, s2, l1);
  if (chk_calls != 5)
    abort ();

  /* Following have known destination and known length,
     so if optimizing certainly shouldn't result in the checking
     variants.  */
  chk_calls = 0;
  memmove (a.buf1 + 2, s3, 1);
  memmove (r, s3, 2);
  r = l1 == 1 ? __builtin_alloca (4) : &a.buf2[7];
  memmove (r, s2, 3);
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
  memmove (r, s2, 1);
  /* Here, l is known to be at most 4 and __builtin_object_size (&buf3[16], 0)
     is 4, so this doesn't need runtime checking.  */
  memmove (&buf3[16], s2, l);
  if (chk_calls)
    abort ();
  chk_calls = 0;
}

/* Test whether runtime and/or compile time checking catches
   buffer overflows.  */
void
__attribute__((noinline))
test5 (void)
{
  struct A { char buf1[10]; char buf2[10]; } a;
  char buf3[20];

  chk_fail_allowed = 1;
  /* Runtime checks.  */
  if (__builtin_setjmp (chk_fail_buf) == 0)
    {
      memmove (&a.buf2[9], s2, l1 + 1);
      abort ();
    }
  if (__builtin_setjmp (chk_fail_buf) == 0)
    {
      memmove (&a.buf2[7], s3, strlen (s3) + 1);
      abort ();
    }
  /* This should be detectable at compile time already.  */
  if (__builtin_setjmp (chk_fail_buf) == 0)
    {
      memmove (&buf3[19], "ab", 2);
      abort ();
    }
  chk_fail_allowed = 0;
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

#define MAX_LENGTH (MAX_OFFSET + MAX_COPY + MAX_EXTRA)

/* Use a sequence length that is not divisible by two, to make it more
   likely to detect when words are mixed up.  */
#define SEQUENCE_LENGTH 31

static union {
  char buf[MAX_LENGTH];
  long long align_int;
  long double align_fp;
} u1, u2;

void
__attribute__((noinline))
test6 (void)
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

	  p = memmove (u1.buf + off1, u2.buf + off2, len);
	  if (p != u1.buf + off1)
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

	  for (i = 0; i < MAX_EXTRA; i++, q++)
	    if (*q != 'a')
	      abort ();
	}
}

#define TESTSIZE 80

char srcb[TESTSIZE] __attribute__ ((aligned));
char dstb[TESTSIZE] __attribute__ ((aligned));

void
__attribute__((noinline))
check (char *test, char *match, int n)
{
  if (memcmp (test, match, n))
    abort ();
}

#define TN(n) \
{ memset (dstb, 0, n); memmove (dstb, srcb, n); check (dstb, srcb, n); }
#define T(n) \
TN (n) \
TN ((n) + 1) \
TN ((n) + 2) \
TN ((n) + 3)

void
__attribute__((noinline))
test7 (void)
{
  int i;

  chk_calls = 0;

  for (i = 0; i < sizeof (srcb); ++i)
      srcb[i] = 'a' + i % 26;

  T (0);
  T (4);
  T (8);
  T (12);
  T (16);
  T (20);
  T (24);
  T (28);
  T (32);
  T (36);
  T (40);
  T (44);
  T (48);
  T (52);
  T (56);
  T (60);
  T (64);
  T (68);
  T (72);
  T (76);

  /* All memmove calls in this routine have constant arguments.  */
  if (chk_calls)
    abort ();
}

void
main_test (void)
{
#ifndef __OPTIMIZE__
  /* Object size checking is only intended for -O[s123].  */
  return;
#endif
  __asm ("" : "=r" (l1) : "0" (l1));
  test1 ();
  test2 ();
  __builtin_memset (p, '\0', sizeof (p));
  test3 ();
  test4 ();
  test5 ();
  test6 ();
  test7 ();
}
