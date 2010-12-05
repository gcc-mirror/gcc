/* Copyright (C) 2004, 2005  Free Software Foundation.

   Ensure builtin __strncpy_chk performs correctly.  */

extern void abort (void);
typedef __SIZE_TYPE__ size_t;
extern size_t strlen(const char *);
extern void *memcpy (void *, const void *, size_t);
extern char *strncpy (char *, const char *, size_t);
extern int memcmp (const void *, const void *, size_t);
extern int strcmp (const char *, const char *);
extern int strncmp (const char *, const char *, size_t);
extern void *memset (void *, int, size_t);

#include "chk.h"

const char s1[] = "123";
char p[32] = "";
char * volatile s2 = "defg";  /* prevent constant propagation to happen when whole program assumptions are made.  */
char * volatile s3 = "FGH";  /* prevent constant propagation to happen when whole program assumptions are made.  */
char *s4;
volatile size_t l1 = 1;  /* prevent constant propagation to happen when whole program assumptions are made.  */
int i;

void
__attribute__((noinline))
test1 (void)
{
  const char *const src = "hello world";
  const char *src2;
  char dst[64], *dst2;

  strncpy_disallowed = 1;
  chk_calls = 0;

  memset (dst, 0, sizeof (dst));
  if (strncpy (dst, src, 4) != dst || strncmp (dst, src, 4))
    abort();

  memset (dst, 0, sizeof (dst));
  if (strncpy (dst+16, src, 4) != dst+16 || strncmp (dst+16, src, 4))
    abort();

  memset (dst, 0, sizeof (dst));
  if (strncpy (dst+32, src+5, 4) != dst+32 || strncmp (dst+32, src+5, 4))
    abort();

  memset (dst, 0, sizeof (dst));
  dst2 = dst;
  if (strncpy (++dst2, src+5, 4) != dst+1 || strncmp (dst2, src+5, 4)
      || dst2 != dst+1)
    abort();

  memset (dst, 0, sizeof (dst));
  if (strncpy (dst, src, 0) != dst || strcmp (dst, ""))
    abort();
  
  memset (dst, 0, sizeof (dst));
  dst2 = dst; src2 = src;
  if (strncpy (++dst2, ++src2, 0) != dst+1 || strcmp (dst2, "")
      || dst2 != dst+1 || src2 != src+1)
    abort();

  memset (dst, 0, sizeof (dst));
  dst2 = dst; src2 = src;
  if (strncpy (++dst2+5, ++src2+5, 0) != dst+6 || strcmp (dst2+5, "")
      || dst2 != dst+1 || src2 != src+1)
    abort();

  memset (dst, 0, sizeof (dst));
  if (strncpy (dst, src, 12) != dst || strcmp (dst, src))
    abort();

  /* Test at least one instance of the __builtin_ style.  We do this
     to ensure that it works and that the prototype is correct.  */
  memset (dst, 0, sizeof (dst));
  if (__builtin_strncpy (dst, src, 4) != dst || strncmp (dst, src, 4))
    abort();

  memset (dst, 0, sizeof (dst));
  if (strncpy (dst, i++ ? "xfoo" + 1 : "bar", 4) != dst
      || strcmp (dst, "bar")
      || i != 1)
    abort ();

  if (chk_calls)
    abort ();
  strncpy_disallowed = 0;
}

void
__attribute__((noinline))
test2 (void)
{
  chk_calls = 0;
  /* No runtime checking should be done here, both destination
     and length are unknown.  */
  strncpy (s4, "abcd", l1 + 1);
  if (chk_calls)
    abort ();
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
  size_t l2;

  /* The following calls should do runtime checking
     - source length is not known, but destination is.  */
  chk_calls = 0;
  strncpy (a.buf1 + 2, s3 + 3, l1);
  strncpy (r, s3 + 2, l1 + 2);
  r = l1 == 1 ? __builtin_alloca (4) : &a.buf2[7];
  strncpy (r, s2 + 2, l1 + 2);
  strncpy (r + 2, s3 + 3, l1);
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
  strncpy (r, s2 + 4, l1);
  if (chk_calls != 5)
    abort ();

  /* Following have known destination and known length,
     so if optimizing certainly shouldn't result in the checking
     variants.  */
  chk_calls = 0;
  strncpy (a.buf1 + 2, "", 3);
  strncpy (a.buf1 + 2, "", 0);
  strncpy (r, "a", 1);
  strncpy (r, "a", 3);
  r = l1 == 1 ? __builtin_alloca (4) : &a.buf2[7];
  strncpy (r, s1 + 1, 3);
  strncpy (r, s1 + 1, 2);
  r = buf3;
  l = "abc";
  l2 = 4;
  for (i = 0; i < 4; ++i)
    {
      if (i == l1 - 1)
	r = &a.buf1[1], l = "e", l2 = 2;
      else if (i == l1)
	r = &a.buf2[7], l = "gh", l2 = 3;
      else if (i == l1 + 1)
	r = &buf3[5], l = "jkl", l2 = 4;
      else if (i == l1 + 2)
	r = &a.buf1[9], l = "", l2 = 1;
    }
  strncpy (r, "", 1);
  /* Here, strlen (l) + 1 is known to be at most 4 and
     __builtin_object_size (&buf3[16], 0) is 4, so this doesn't need
     runtime checking.  */
  strncpy (&buf3[16], l, l2);
  strncpy (&buf3[15], "abc", l2);
  strncpy (&buf3[10], "fghij", l2);
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
      strncpy (&a.buf2[9], s2 + 4, l1 + 1);
      abort ();
    }
  if (__builtin_setjmp (chk_fail_buf) == 0)
    {
      strncpy (&a.buf2[7], s3, l1 + 4);
      abort ();
    }
  /* This should be detectable at compile time already.  */
  if (__builtin_setjmp (chk_fail_buf) == 0)
    {
      strncpy (&buf3[19], "abc", 2);
      abort ();
    }
  if (__builtin_setjmp (chk_fail_buf) == 0)
    {
      strncpy (&buf3[18], "", 3);
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
