/* Copyright (C) 2004, 2005  Free Software Foundation.

   Ensure builtin __snprintf_chk performs correctly.  */

extern void abort (void);
typedef __SIZE_TYPE__ size_t;
extern size_t strlen(const char *);
extern void *memcpy (void *, const void *, size_t);
extern char *strcpy (char *, const char *);
extern int memcmp (const void *, const void *, size_t);
extern void *memset (void *, int, size_t);
extern int sprintf (char *, const char *, ...);
extern int snprintf (char *, size_t, const char *, ...);

#include "chk.h"

const char s1[] = "123";
char p[32] = "";
char *s2 = "defg";
char *s3 = "FGH";
char *s4;
size_t l1 = 1;
static char buffer[32];
char *ptr = "barf";

void
__attribute__((noinline))
test1 (void)
{
  chk_calls = 0;
  /* snprintf_disallowed = 1; */

  memset (buffer, 'A', 32);
  snprintf (buffer, 4, "foo");
  if (memcmp (buffer, "foo", 4) || buffer[4] != 'A')
    abort ();

  memset (buffer, 'A', 32);
  if (snprintf (buffer, 4, "foo bar") != 7)
    abort ();
  if (memcmp (buffer, "foo", 4) || buffer[4] != 'A')
    abort ();

  memset (buffer, 'A', 32);
  snprintf (buffer, 32, "%s", "bar");
  if (memcmp (buffer, "bar", 4) || buffer[4] != 'A')
    abort ();

  memset (buffer, 'A', 32);
  if (snprintf (buffer, 21, "%s", "bar") != 3)
    abort ();
  if (memcmp (buffer, "bar", 4) || buffer[4] != 'A')
    abort ();

  snprintf_disallowed = 0;

  memset (buffer, 'A', 32);
  if (snprintf (buffer, 4, "%d%d%d", (int) l1, (int) l1 + 1, (int) l1 + 12)
      != 4)
    abort ();
  if (memcmp (buffer, "121", 4) || buffer[4] != 'A')
    abort ();

  memset (buffer, 'A', 32);
  if (snprintf (buffer, 32, "%d%d%d", (int) l1, (int) l1 + 1, (int) l1 + 12)
      != 4)
    abort ();
  if (memcmp (buffer, "1213", 5) || buffer[5] != 'A')
    abort ();

  if (chk_calls)
    abort ();

  memset (buffer, 'A', 32);
  snprintf (buffer, strlen (ptr) + 1, "%s", ptr);
  if (memcmp (buffer, "barf", 5) || buffer[5] != 'A')
    abort ();

  memset (buffer, 'A', 32);
  snprintf (buffer, l1 + 31, "%d - %c", (int) l1 + 27, *ptr);
  if (memcmp (buffer, "28 - b\0AAAAA", 12))
    abort ();

  if (chk_calls != 2)
    abort ();
  chk_calls = 0;

  memset (s4, 'A', 32);
  snprintf (s4, l1 + 6, "%d - %c", (int) l1 - 17, ptr[1]);
  if (memcmp (s4, "-16 - \0AAA", 10))
    abort ();
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

  /* The following calls should do runtime checking
     - length is not known, but destination is.  */
  chk_calls = 0;
  snprintf (a.buf1 + 2, l1, "%s", s3 + 3);
  snprintf (r, l1 + 4, "%s%c", s3 + 3, s3[3]);
  r = l1 == 1 ? __builtin_alloca (4) : &a.buf2[7];
  snprintf (r, strlen (s2) - 2, "%c %s", s2[2], s2 + 4);
  snprintf (r + 2, l1, s3 + 3);
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
  snprintf (r, l1, s2 + 4);
  if (chk_calls != 5)
    abort ();

  /* Following have known destination and known source length,
     so if optimizing certainly shouldn't result in the checking
     variants.  */
  chk_calls = 0;
  /* snprintf_disallowed = 1; */
  snprintf (a.buf1 + 2, 4, "");
  snprintf (r, 1, "a");
  r = l1 == 1 ? __builtin_alloca (4) : &a.buf2[7];
  snprintf (r, 3, "%s", s1 + 1);
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
  snprintf (r, 1, "%s", "");
  snprintf (r, 0, "%s", "");
  snprintf_disallowed = 0;
  /* Unknown destination and source, no checking.  */
  snprintf (s4, l1 + 31, "%s %d", s3, 0);
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

  chk_fail_allowed = 1;
  /* Runtime checks.  */
  if (__builtin_setjmp (chk_fail_buf) == 0)
    {
      snprintf (&a.buf2[9], l1 + 1, "%c%s", s2[3], s2 + 4);
      abort ();
    }
  if (__builtin_setjmp (chk_fail_buf) == 0)
    {
      snprintf (&a.buf2[7], l1 + 30, "%s%c", s3 + strlen (s3) - 2, *s3);
      abort ();
    }
  if (__builtin_setjmp (chk_fail_buf) == 0)
    {
      snprintf (&a.buf2[7], l1 + 3, "%d", (int) l1 + 9999);
      abort ();
    }
  /* This should be detectable at compile time already.  */
  if (__builtin_setjmp (chk_fail_buf) == 0)
    {
      snprintf (&buf3[19], 2, "a");
      abort ();
    }
  if (__builtin_setjmp (chk_fail_buf) == 0)
    {
      snprintf (&buf3[17], 4, "a");
      abort ();
    }
  if (__builtin_setjmp (chk_fail_buf) == 0)
    {
      snprintf (&buf3[17], 4, "%s", "abc");
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
  test2 ();
  test3 ();
}
