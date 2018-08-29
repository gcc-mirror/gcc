/* PR tree-optimization/83446 - Bootstrap failed on i686
   { dg-do compile }
   { dg-options "-O2 -Warray-bounds" } */

char a[4];

void f0i (void *d, int n)
{
  if (n < 0) n = 0;

  __builtin_memcpy (d, a + sizeof a - n, n);
}

void f0L (void *d, long n)
{
  if (n < 0) n = 0;

  __builtin_memcpy (d, a + sizeof a - n, n);
}

void f0u (void *d, unsigned n)
{
  if (n < 0) n = 1;

  __builtin_memcpy (d, a + sizeof a - n, n);   /* { dg-bogus "\\\[-Warray-bounds" } */
}

void f1lu (void *d, unsigned long n)
{
  if (n < 1) n = 1;

  __builtin_memcpy (d, a + sizeof a - n, n);   /* { dg-bogus "\\\[-Warray-bounds" } */
}
