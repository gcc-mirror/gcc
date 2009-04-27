/* { dg-do run } */
/* { dg-options "-O2 -fmodulo-sched -fmodulo-sched-allow-regmoves -funroll-loops -fdump-rtl-sms" } */
/* This is the same test as loop-2e.c test.  It is related to a fix in
   the generation of the prolog and epilog.  */

extern void abort (void);

__attribute__ ((noinline))
void f (int *p, int **q)
{
  int i;
  for (i = 0; i < 40; i++)
    {
      *q++ = &p[i];
    }
}

int main ()
{
  void *p;
  int *q[40];
  __SIZE_TYPE__ start;

  /* Find the signed middle of the address space.  */
  if (sizeof(start) == sizeof(int))
    start = (__SIZE_TYPE__) __INT_MAX__;
  else if (sizeof(start) == sizeof(long))
    start = (__SIZE_TYPE__) __LONG_MAX__;
  else if (sizeof(start) == sizeof(long long))
    start = (__SIZE_TYPE__) __LONG_LONG_MAX__;
  else
    return 0;

  /* Arbitrarily align the pointer.  */
  start &= -32;

  /* Pretend that's good enough to start address arithmetic.  */
  p = (void *)start;

  /* Verify that GIV replacement computes the correct results.  */
  q[39] = 0;
  f (p, q);
  if (q[39] != (int *)p + 39)
    abort ();

  return 0;
}

/* { dg-final { scan-rtl-dump-times "SMS succeeded" 1 "sms"  { target powerpc*-*-* spu-*-* } } } */

/* { dg-final { cleanup-rtl-dump "sms" } } */

