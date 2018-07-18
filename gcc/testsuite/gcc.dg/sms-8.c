/*  This test is a reduced test case for a bug that caused
    bootstrapping with -fmodulo-sched.  Related to a broken anti-dep
    that was not fixed by reg-moves.  */

 /* { dg-do run } */
 /* { dg-options "-O2 -fmodulo-sched -fmodulo-sched-allow-regmoves -fdump-rtl-sms --param sms-min-sc=1" } */

extern void abort (void);

__attribute__ ((noinline))
unsigned long long
foo (long long ixi, unsigned ctr)
{
  unsigned long long irslt = 1;
  long long ix = ixi;

  for (; ctr; ctr--)
    {
      irslt *= ix;
      ix *= ix;
    }

  if (irslt != 14348907)
    abort ();
  return irslt;
}


int
main ()
{
  unsigned long long res;

  res = foo (3, 4);
  return 0;
}

/* { dg-final { scan-rtl-dump-times "SMS succeeded" 1 "sms" { target powerpc*-*-* } } } */


