/* Test prefetch support.  */
/* { dg-options "-mcpu=fr400" } */
/* { dg-do run } */

unsigned char global[64];

int foo (unsigned int *x, int n)
{
  unsigned short local[16];

  __data_prefetch0 (x);
  __data_prefetch0 (&x[8]);
  __data_prefetch0 (&x[n]);
  __data_prefetch0 (local);
  __data_prefetch0 (&local[16]);
  __data_prefetch0 (&local[n]);
  __data_prefetch0 (global);
  __data_prefetch0 (&global[32]);
  __data_prefetch0 (&global[n]);
}

int main ()
{
  unsigned int i[16];

  foo (i, 2);
  return 0;
}
