/* Test prefetch support.  */
/* { dg-do run } */
extern void abort (void);
extern void exit (int);

unsigned char global[64];

void
foo (unsigned int *x, int n)
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

#if __FRV_VLIW__ > 1
  __data_prefetch (x);
  __data_prefetch (&x[8]);
  __data_prefetch (&x[n]);
  __data_prefetch (local);
  __data_prefetch (&local[16]);
  __data_prefetch (&local[n]);
  __data_prefetch (global);
  __data_prefetch (&global[32]);
  __data_prefetch (&global[n]);
#endif
}

int main ()
{
  unsigned int i[16];

  foo (i, 2);
  exit (0);
}
