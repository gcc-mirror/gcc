/* PR preprocessor/79214 - -Wno-system-header defeats strncat buffer overflow
   warnings
   { dg-do compile }
   { dg-options "-O2" } */

#include "pr79214.h"

typedef __SIZE_TYPE__ size_t;

char d[3];
char s[4];

size_t range (void)
{
  extern size_t size ();
  size_t n = size ();
  if (n <= sizeof d)
    return sizeof d + 1;

  return n;
}

void test_bzero (void)
{
  bzero (d, range ());   /* { dg-warning ".__builtin_bzero. writing between 4 and \[0-9\]+ bytes into a region of size 3 overflows the destination" } */
}

void test_memcpy (void)
{
  memcpy (d, s, range ());   /* { dg-warning ".__builtin_memcpy. writing between 4 and \[0-9\]+ bytes into a region of size 3 overflows the destination" } */
}

void test_memmove (void)
{
  memmove (d, d + 1, range ());   /* { dg-warning ".__builtin_memmove. writing between 4 and \[0-9\]+ bytes into a region of size 3 overflows the destination" } */
}

void test_mempcpy (void)
{
  mempcpy (d, s, range ());   /* { dg-warning ".__builtin_mempcpy. writing between 4 and \[0-9\]+ bytes into a region of size 3 overflows the destination" } */
}

void test_memset (int n)
{
  memset (d, n, range ());   /* { dg-warning ".__builtin_memset. writing between 4 and \[0-9\]+ bytes into a region of size 3 overflows the destination" } */
}

void test_strcat (int i)
{
  const char *s = i < 0 ? "123" : "4567";

  strcat (d, s);   /* { dg-warning ".__builtin_strcat. writing 4 bytes into a region of size 3 overflows the destination" } */
}

char* test_stpcpy (int i)
{
  const char *s = i < 0 ? "123" : "4567";

  return stpcpy (d, s);   /* { dg-warning ".__builtin_stpcpy. writing 4 bytes into a region of size 3 overflows the destination" } */
}

char* test_stpncpy (int i)
{
  const char *s = i < 0 ? "123" : "4567";

  return stpncpy (d, s, range ());   /* { dg-warning ".__builtin_stpncpy. writing between 4 and \[0-9\]+ bytes into a region of size 3 overflows the destination" } */
}

char* test_strcpy (int i)
{
  const char *s = i < 0 ? "123" : "4567";

  return strcpy (d, s);   /* { dg-warning ".__builtin_strcpy. writing 4 bytes into a region of size 3 overflows the destination" } */
}

char* test_strncpy (int i)
{
  const char *s = i < 0 ? "123" : "4567";

  return strncpy (d, s, range ());   /* { dg-warning ".__builtin_strncpy. writing between 4 and \[0-9\]+ bytes into a region of size 3 overflows the destination" } */
}

char* test_strncat (int i)
{
  const char *s = i < 0 ? "123" : "4567";

  return strncat (d, s, range ());   /* { dg-warning ".__builtin_strncat.: specified bound between 4 and \[0-9\]+" } */
}
