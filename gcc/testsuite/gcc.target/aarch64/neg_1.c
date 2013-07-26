/* { dg-do run } */
/* { dg-options "-O2 -fno-inline --save-temps" } */

extern void abort (void);

long long
neg64 (long long a)
{
  /* { dg-final { scan-assembler "neg\tx\[0-9\]+" } } */
  return 0 - a;
}

long long
neg64_in_dreg (long long a)
{
  /* { dg-final { scan-assembler "neg\td\[0-9\]+, d\[0-9\]+" } } */
  register long long x asm ("d8") = a;
  register long long y asm ("d9");
  asm volatile ("" : : "w" (x));
  y = 0 - x;
  asm volatile ("" : : "w" (y));
  return y;
}

int
neg32 (int a)
{
  /* { dg-final { scan-assembler "neg\tw\[0-9\]+" } } */
  return 0 - a;
}

int
neg32_in_sreg (int a)
{
  /* { dg-final { scan-assembler "neg\tv\[0-9\]+\.2s, v\[0-9\]+\.2s" } } */
  register int x asm ("s8") = a;
  register int y asm ("s9");
  asm volatile ("" : : "w" (x));
  y = 0 - x;
  asm volatile ("" : : "w" (y));
  return y;
}

int
main (void)
{
  long long a;
  int b;
  a = 61;
  b = 313;

  if (neg64 (a) != -61)
    abort ();

  if (neg64_in_dreg (a) != -61)
    abort ();

  if (neg32 (b) != -313)
    abort ();

  if (neg32_in_sreg (b) != -313)
    abort ();

  return 0;
}

/* { dg-final { cleanup-saved-temps } } */
