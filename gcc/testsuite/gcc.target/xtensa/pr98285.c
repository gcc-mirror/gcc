/* { dg-do compile } */
/* { dg-options "-O2" } */

int mul3(int v)
{
  return v * 3;
}

int mul5(int v)
{
  return v * 5;
}

int mul7(int v)
{
  return v * 7;
}

int mul9(int v)
{
  return v * 9;
}

int mul2sub(int a, int b)
{
  return a * 2 - b;
}

int mul4sub(int a, int b)
{
  return a * 4 - b;
}

short index2(short *p, int i)
{
  return p[i];
}

int index4(int *p, int i)
{
  return p[i];
}

long long index8(long long *p, int i)
{
  return p[i];
}

/* { dg-final { scan-assembler-times "addx2" 2 } } */
/* { dg-final { scan-assembler-times "addx4" 2 } } */
/* { dg-final { scan-assembler-times "addx8" 2 } } */
/* { dg-final { scan-assembler-times "subx2" 1 } } */
/* { dg-final { scan-assembler-times "subx4" 1 } } */
/* { dg-final { scan-assembler-times "subx8" 1 } } */
