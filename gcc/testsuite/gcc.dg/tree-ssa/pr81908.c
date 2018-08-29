/* PR 81908 - FAIL: gfortran.dg/alloc_comp_auto_array_2.f90 -O3 -g -m32
   { dg-do compile }
   { dg-options "-O2 -Wall" } */

#define SIZE_MAX __SIZE_MAX__
typedef __SIZE_TYPE__ size_t;

void f0 (char *d, const char *s, size_t n)
{
  if (n > 0 && n <= SIZE_MAX / 2 - 1)
    n = 0;

  __builtin_memcpy (d, s, n);
}

void f1 (char *d, const char *s, size_t n)
{
  if (n > 0 && n <= SIZE_MAX / 2)
    n = 0;

  __builtin_memcpy (d, s, n);   /* { dg-bogus "\\\[-Wstringop-overflow=]" } */
}

void f2 (char *d, const char *s, size_t n)
{
  if (n > 0 && n <= SIZE_MAX / 2 + 1)
    n = 0;

  __builtin_memcpy (d, s, n);
}

void f3 (char *d, const char *s, size_t n)
{
  if (n > 0 && n <= SIZE_MAX - 1)
    n = 0;

  __builtin_memcpy (d, s, n);   /* { dg-bogus "\\\[-Wstringop-overflow=]" } */
}

void f4 (char *d, const char *s, size_t n)
{
  if (n > 0 && n <= SIZE_MAX)
    n = 0;

  __builtin_memcpy (d, s, n);
}
