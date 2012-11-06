/* Inspired from bitmap_or_and function in sbitmap.c.  */
/* { dg-do run } */
/* { dg-options "-O2 -fmodulo-sched -fmodulo-sched-allow-regmoves -fdump-rtl-sms" } */
/* { dg-options "-O2 -fmodulo-sched -fmodulo-sched-allow-regmoves -fdump-rtl-sms --param sms-min-sc=1" { target powerpc*-*-* } } */

extern void abort (void);

int a[5] = { 0, 1, 0, 0, 0 };
int b[5] = { 0, 1, 0, 1, 0 };
int c[5] = { 0, 0, 1, 1, 0 };
int dst[5] = { 0, 0, 0, 0, 0 };

__attribute__ ((noinline))
void
foo (int size, int *ap, int *bp, int *cp, int *dstp)
{
  unsigned int i, n = size;
  int changed = 0;

  for (i = 0; i < n; i++)
    {
      const int tmp = *ap++ | (*bp++ & *cp++);
      changed |= *dstp ^ tmp;
      *dstp++ = tmp;
    }

  if (changed == 0)
    abort ();
}

int
main ()
{
  foo (5, a, b, c, dst);
  return 0;
}

/* { dg-final { scan-rtl-dump-times "SMS succeeded" 1 "sms" { target spu-*-* powerpc*-*-* } } } */
/* { dg-final { cleanup-rtl-dump "sms" } } */

