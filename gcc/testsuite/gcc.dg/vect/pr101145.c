/* { dg-require-effective-target vect_int } */
/* { dg-additional-options "-O3" } */
#include <limits.h>

unsigned __attribute__ ((noinline))
foo (int *__restrict__ a, int *__restrict__ b, unsigned l, unsigned n)
{
  while (n < ++l)
    *a++ = *b++ + 1;
  return l;
}

unsigned __attribute__ ((noinline))
foo_1 (int *__restrict__ a, int *__restrict__ b, unsigned l, unsigned)
{
  while (UINT_MAX - 64 < ++l)
    *a++ = *b++ + 1;
  return l;
}

unsigned __attribute__ ((noinline))
foo_2 (int *__restrict__ a, int *__restrict__ b, unsigned l, unsigned n)
{
  l = UINT_MAX - 32;
  while (n < ++l)
    *a++ = *b++ + 1;
  return l;
}

unsigned __attribute__ ((noinline))
foo_3 (int *__restrict__ a, int *__restrict__ b, unsigned l, unsigned n)
{
  while (n <= ++l)
    *a++ = *b++ + 1;
  return l;
}

unsigned __attribute__ ((noinline))
foo_4 (int *__restrict__ a, int *__restrict__ b, unsigned l, unsigned n)
{  // infininate 
  while (0 <= ++l)
    *a++ = *b++ + 1;
  return l;
}

unsigned __attribute__ ((noinline))
foo_5 (int *__restrict__ a, int *__restrict__ b, unsigned l, unsigned n)
{
  //no loop
  l = UINT_MAX;
  while (n < ++l)
    *a++ = *b++ + 1;
  return l;
}

unsigned __attribute__ ((noinline))
bar (int *__restrict__ a, int *__restrict__ b, unsigned l, unsigned n)
{
  while (--l < n)
    *a++ = *b++ + 1;
  return l;
}

unsigned __attribute__ ((noinline))
bar_1 (int *__restrict__ a, int *__restrict__ b, unsigned l, unsigned)
{
  while (--l < 64)
    *a++ = *b++ + 1;
  return l;
}

unsigned __attribute__ ((noinline))
bar_2 (int *__restrict__ a, int *__restrict__ b, unsigned l, unsigned n)
{
  l = 32;
  while (--l < n)
    *a++ = *b++ + 1;
  return l;
}


int a[3200], b[3200];
int fail;

int
main ()
{
  unsigned l, n;
  unsigned res;
  /* l > n*/
  n = UINT_MAX - 64;
  l = n + 32;
  res = foo (a, b, l, n);
  if (res != 0)
    fail++;

  l = n;
  res = foo (a, b, l, n);
  if (res != 0)
    fail++;

  l = n - 1;
  res = foo (a, b, l, n);
  if (res != l + 1)
    fail++;
  
  l = n - 32;
  res = foo (a, b, l, n);
  if (res != l + 1)
    fail++;

  l = UINT_MAX;
  res = foo (a, b, l, n);
  if (res != 0)
    fail++;

  l = n + 32;
  res = foo_1 (a, b, l, n);
  if (res != 0)
    fail++;

  l = n + 32;
  res = foo_2 (a, b, l, n);
  if (res != 0)
    fail++;

  l = n;
  res = foo_3 (a, b, l, n);
  if (res != 0)
    fail++;

  l = n - 1;
  res = foo_3 (a, b, l, n);
  if (res != 0)
    fail++;

  l = n - 2;
  res = foo_3 (a, b, l, n);
  if (res != l + 1)
    fail++;

  res = foo_5 (a, b, l, n);
  if (res != 0)
    fail++;

  n = 64;
  l = n - 32;
  res = bar (a, b, l, n);
  res++;
  if (res != 0)
    fail++;

  l = n;
  res = bar (a, b, l, n);
  res++;
  if (res != 0)
    fail++;

  l = n + 1;
  res = bar (a, b, l, n);
  res++;
  if (res != l)
    fail++;

  l = 0;
  res = bar (a, b, l, n);
  res++;
  if (res != l)
    fail++;

  l = 32;
  res = bar_1 (a, b, l, n);
  res++;
  if (res != 0)
    fail++;  

  res = bar_1 (a, b, l, n);
  res++;
  if (res != 0)
    fail++;  

  if (fail)
    __builtin_abort ();
  return 0;
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 7 "vect" } } */
