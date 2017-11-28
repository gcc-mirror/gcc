/* { dg-do compile } */
/* { dg-options "-O2" } */

long long
f1 (long long x, int i)
{

  return x >> (64 - i);
}

unsigned long long
f2 (unsigned long long x, unsigned int i)
{

  return x >> (64 - i);
}

int
f3 (int x, int i)
{

  return x >> (32 - i);
}

unsigned int
f4 (unsigned int x, unsigned int i)
{

  return x >> (32 - i);
}

int
f5 (int x, int i)
{
  return x << (32 - i);
}

long long
f6 (long long x, int i)
{
  return x << (64 - i);
}

/* { dg-final { scan-assembler "lsl\tw\[0-9\]+, w\[0-9\]+, w\[0-9\]+" } } */
/* { dg-final { scan-assembler "lsl\tx\[0-9\]+, x\[0-9\]+, x\[0-9\]+" } } */
/* { dg-final { scan-assembler "lsr\tw\[0-9\]+, w\[0-9\]+, w\[0-9\]+" } } */
/* { dg-final { scan-assembler "lsr\tx\[0-9\]+, x\[0-9\]+, x\[0-9\]+" } } */
/* { dg-final { scan-assembler "asr\tw\[0-9\]+, w\[0-9\]+, w\[0-9\]+" } } */
/* { dg-final { scan-assembler "asr\tx\[0-9\]+, x\[0-9\]+, x\[0-9\]+" } } */
/* { dg-final { scan-assembler-times "neg\tw\[0-9\]+, w\[0-9\]+" 6 } } */
/* { dg-final { scan-assembler-not "sub\tw\[0-9\]+, w\[0-9\]+, w\[0-9\]+" } } */
