/* Copyright (C) 2004, 2005  Free Software Foundation.

   Ensure builtin __strncat_chk performs correctly.  */

extern void abort (void);
typedef __SIZE_TYPE__ size_t;
extern size_t strlen (const char *);
extern void *memcpy (void *, const void *, size_t);
extern char *strcat (char *, const char *);
extern char *strncat (char *, const char *, size_t);
extern int memcmp (const void *, const void *, size_t);
extern char *strcpy (char *, const char *);
extern int strcmp (const char *, const char *);
extern void *memset (void *, int, size_t);

#include "chk.h"

const char s1[] = "123";
char p[32] = "";
char *s2 = "defg";
char *s3 = "FGH";
char *s4;
size_t l1 = 1;
char *s5;
int x = 123;

void
__attribute__((noinline))
test1 (void)
{
  const char *const s1 = "hello world";
  const char *const s2 = "";
  const char *s3;
  char dst[64], *d2;

  /* Following strncat calls should be all optimized out.  */
  chk_calls = 0;
  strncat_disallowed = 1;
  strcat_disallowed = 1;
  strcpy (dst, s1);
  if (strncat (dst, "", 100) != dst || strcmp (dst, s1))
    abort ();
  strcpy (dst, s1);
  if (strncat (dst, s2, 100) != dst || strcmp (dst, s1))
    abort ();
  strcpy (dst, s1); d2 = dst;
  if (strncat (++d2, s2, 100) != dst+1 || d2 != dst+1 || strcmp (dst, s1))
    abort ();
  strcpy (dst, s1); d2 = dst;
  if (strncat (++d2+5, s2, 100) != dst+6 || d2 != dst+1 || strcmp (dst, s1))
    abort ();
  strcpy (dst, s1); d2 = dst;
  if (strncat (++d2+5, s1+11, 100) != dst+6 || d2 != dst+1 || strcmp (dst, s1))
    abort ();
  strcpy (dst, s1); d2 = dst;
  if (strncat (++d2+5, s1, 0) != dst+6 || d2 != dst+1 || strcmp (dst, s1))
    abort ();
  strcpy (dst, s1); d2 = dst; s3 = s1;
  if (strncat (++d2+5, ++s3, 0) != dst+6 || d2 != dst+1 || strcmp (dst, s1)
      || s3 != s1 + 1)
    abort ();
  strcpy (dst, s1); d2 = dst;
  if (strncat (++d2+5, "", ++x) != dst+6 || d2 != dst+1 || x != 124
      || strcmp (dst, s1))
    abort ();
  if (chk_calls)
    abort ();
  strcat_disallowed = 0;

  /* These __strncat_chk calls should be optimized into __strcat_chk,
     as strlen (src) <= len.  */
  strcpy (dst, s1);
  if (strncat (dst, "foo", 3) != dst || strcmp (dst, "hello worldfoo"))
    abort ();
  strcpy (dst, s1);
  if (strncat (dst, "foo", 100) != dst || strcmp (dst, "hello worldfoo"))
    abort ();
  strcpy (dst, s1);
  if (strncat (dst, s1, 100) != dst || strcmp (dst, "hello worldhello world"))
    abort ();
  if (chk_calls != 3)
    abort ();

  chk_calls = 0;
  /* The following calls have side-effects in dest, so are not checked.  */
  strcpy (dst, s1); d2 = dst;
  if (__builtin___strncat_chk (++d2, s1, 100, os (++d2)) != dst+1
      || d2 != dst+1 || strcmp (dst, "hello worldhello world"))
    abort ();
  strcpy (dst, s1); d2 = dst;
  if (__builtin___strncat_chk (++d2+5, s1, 100, os (++d2+5)) != dst+6
      || d2 != dst+1 || strcmp (dst, "hello worldhello world"))
    abort ();
  strcpy (dst, s1); d2 = dst;
  if (__builtin___strncat_chk (++d2+5, s1+5, 100, os (++d2+5)) != dst+6
      || d2 != dst+1 || strcmp (dst, "hello world world"))
    abort ();
  if (chk_calls)
    abort ();

  chk_calls = 0;
  strcat_disallowed = 1;

  /* Test at least one instance of the __builtin_ style.  We do this
     to ensure that it works and that the prototype is correct.  */
  strcpy (dst, s1);
  if (__builtin_strncat (dst, "", 100) != dst || strcmp (dst, s1))
    abort ();

  if (chk_calls)
    abort ();
  strncat_disallowed = 0;
  strcat_disallowed = 0;
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

  /* The following calls should do runtime checking.  */
  memset (&a, '\0', sizeof (a));
  s5 = (char *) &a;
  __asm __volatile ("" : : "r" (s5) : "memory");
  chk_calls = 0;
  strncat (a.buf1 + 2, s3 + 3, l1 - 1);
  strncat (r, s3 + 2, l1);
  r = l1 == 1 ? __builtin_alloca (4) : &a.buf2[7];
  memset (r, '\0', 3);
  __asm __volatile ("" : : "r" (r) : "memory");
  strncat (r, s2 + 2, l1 + 1);
  strncat (r + 2, s3 + 3, l1 - 1);
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
  strncat (r, s2 + 4, l1);
  if (chk_calls != 5)
    abort ();

  /* Following have known destination and known source length,
     but we don't know the length of dest string, so runtime checking
     is needed too.  */
  memset (&a, '\0', sizeof (a));
  chk_calls = 0;
  s5 = (char *) &a;
  __asm __volatile ("" : : "r" (s5) : "memory");
  strncat (a.buf1 + 2, "a", 5);
  strncat (r, "def", 0);
  r = l1 == 1 ? __builtin_alloca (4) : &a.buf2[7];
  memset (r, '\0', 3);
  __asm __volatile ("" : : "r" (r) : "memory");
  strncat (r, s1 + 1, 2);
  if (chk_calls != 2)
    abort ();
  chk_calls = 0;
  strcat_disallowed = 1;
  /* Unknown destination and source, no checking.  */
  strncat (s4, s3, l1 + 1);
  strcat_disallowed = 0;
  if (chk_calls)
    abort ();
}

/* Test whether runtime and/or compile time checking catches
   buffer overflows.  */
void
__attribute__((noinline))
test3 (void)
{
  struct A { char buf1[10]; char buf2[10]; } a;
  char buf3[20];

  memset (&a, '\0', sizeof (a));
  memset (buf3, '\0', sizeof (buf3));
  s5 = (char *) &a;
  __asm __volatile ("" : : "r" (s5) : "memory");
  s5 = buf3;
  __asm __volatile ("" : : "r" (s5) : "memory");
  chk_fail_allowed = 1;
  /* Runtime checks.  */
  if (__builtin_setjmp (chk_fail_buf) == 0)
    {
      strncat (&a.buf2[9], s2 + 3, 4);
      abort ();
    }
  if (__builtin_setjmp (chk_fail_buf) == 0)
    {
      strncat (&a.buf2[7], s3 + strlen (s3) - 3, 3);
      abort ();
    }
  if (__builtin_setjmp (chk_fail_buf) == 0)
    {
      strncat (&buf3[19], "abcde", 1);
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
  s4 = p;
  test1 ();
  memset (p, '\0', sizeof (p));
  test2 ();
  test3 ();
}
