/* PR preprocessor/79214 - -Wno-system-header defeats strncat buffer overflow
   warnings
   { dg-do compile }
   { dg-options "-O2" } */

#include "pr79214.h"

typedef __SIZE_TYPE__ size_t;

char d[3];
char s[4];

static size_t range (void)
{
  extern size_t size ();
  size_t n = size ();
  if (n <= sizeof d)
    return sizeof d + 1;

  return n;
}

void test_bzero (void)
{
  bzero (d, range ());   /* { dg-warning ".__builtin_(bzero|memset). writing 4 or more bytes into a region of size 3 overflows the destination" "pr?????" { xfail { *-*-* } } } */
}

void test_memcpy (void)
{
  memcpy (d, s, range ());   /* { dg-warning ".__builtin_memcpy. writing 4 or more bytes into a region of size 3 overflows the destination" "pr?????" { xfail { *-*-* } } } */
}

void test_memmove (void)
{
  memmove (d, d + 1, range ());   /* { dg-warning ".__builtin_memmove. writing 4 or more bytes into a region of size 3 overflows the destination" "pr?????" { xfail { *-*-* } } } */
}

void test_mempcpy (void)
{
  mempcpy (d, s, range ());   /* { dg-warning ".__builtin_mempcpy. writing 4 or more bytes into a region of size 3 overflows the destination" "pr?????" { xfail { *-*-* } } } */
}

void test_memset (int n)
{
  memset (d, n, range ());   /* { dg-warning ".__builtin_memset. writing 4 or more bytes into a region of size 3 overflows the destination" "pr?????" { xfail { *-*-* } } } */
}

void test_strcat (int i)
{
  const char *s = i < 0 ? "123" : "4567";

  strcat (d, s);   /* { dg-warning ".__builtin_strcat. writing between 4 and 5 bytes into a region of size 3 overflows the destination" "pr?????" { xfail { *-*-* } } } */
}

char* test_stpcpy (int i)
{
  const char *s = i < 0 ? "123" : "4567";

  return stpcpy (d, s);   /* { dg-warning ".__builtin_stpcpy. writing between 4 and 5 bytes into a region of size 3 overflows the destination" "pr?????" { xfail { *-*-* } } } */
}

char* test_stpncpy (int i)
{
  const char *s = i < 0 ? "123" : "4567";

  return stpncpy (d, s, range ());   /* { dg-warning ".__builtin_stpncpy. writing 4 or more bytes into a region of size 3 overflows the destination" "pr?????" { xfail { *-*-* } } } */
}

char* test_strcpy (int i)
{
  const char *s = i < 0 ? "123" : "4567";

  return strcpy (d, s);   /* { dg-warning ".__builtin_strcpy. writing between 4 and 5 bytes into a region of size 3 overflows the destination" "pr?????" { xfail { *-*-* } } } */
}

char* test_strncpy (int i)
{
  const char *s = i < 0 ? "123" : "4567";

  return strncpy (d, s, range ());   /* { dg-warning ".__builtin_strncpy. writing 4 or more bytes into a region of size 3 overflows the destination" "pr?????" { xfail { *-*-* } } } */
}

char* test_strncat (int i)
{
  const char *s = i < 0 ? "123" : "4567";

  return strncat (d, s, range ());   /* { dg-warning ".__builtin_strncat. specified bound \\\[4, \[0-9\]+] exceeds destination size 3" "pr?????" { xfail { *-*-* } } } */
}
