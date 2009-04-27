/* { dg-do run } */
/* { dg-options "-O2 -fmodulo-sched -funroll-loops -fdump-rtl-sms" } */

extern void abort (void);

int X[1000]={0};
int Y[1000]={0};

extern void abort (void);

__attribute__ ((noinline))
int
foo (int len, long a)
{
  int i;
  long res = a;

  len = 1000;
  for (i = 0; i < len; i++)
    res += X[i]* Y[i];

  if (res != 601)
    abort ();

}

int
main ()
{
  X[0] = Y[1] = 2;
  Y[0] = X[1] = 21;
  X[2] = Y[3] = 3;
  Y[2] = X[3] = 31;
  X[4] = Y[5] = 4;
  Y[4] = X[5] = 41;

  foo (6, 3);
  return 0;
}

/* { dg-final { scan-rtl-dump-times "SMS succeeded" 1 "sms" { target spu-*-* powerpc*-*-* } } } */
/* { dg-final { cleanup-rtl-dump "sms" } } */

