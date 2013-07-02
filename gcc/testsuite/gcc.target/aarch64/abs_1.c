/* { dg-do run } */
/* { dg-options "-O2 -fno-inline --save-temps" } */

extern long long llabs (long long);
extern void abort (void);

long long
abs64 (long long a)
{
  /* { dg-final { scan-assembler "eor\t" } } */
  /* { dg-final { scan-assembler "sub\t" } } */
  return llabs (a);
}

long long
abs64_in_dreg (long long a)
{
  /* { dg-final { scan-assembler "abs\td\[0-9\]+, d\[0-9\]+" } } */
  register long long x asm ("d8") = a;
  register long long y asm ("d9");
  asm volatile ("" : : "w" (x));
  y = llabs (x);
  asm volatile ("" : : "w" (y));
  return y;
}

int
main (void)
{
  volatile long long ll0 = 0LL, ll1 = 1LL, llm1 = -1LL;

  if (abs64 (ll0) != 0LL)
    abort ();

  if (abs64 (ll1) != 1LL)
    abort ();

  if (abs64 (llm1) != 1LL)
    abort ();

  if (abs64_in_dreg (ll0) != 0LL)
    abort ();

  if (abs64_in_dreg (ll1) != 1LL)
    abort ();

  if (abs64_in_dreg (llm1) != 1LL)
    abort ();

  return 0;
}

/* { dg-final { cleanup-saved-temps } } */
