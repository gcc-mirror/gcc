/* { dg-compile } */

/* and with complement */

int
ncrk (int a, int b)
{
  return a & ~b;
}

/* { dg-final { scan-assembler-times "\tncrk\t" 1 } } */

long long
ncgrk (long long a, long long b)
{
  return a & ~b;
}

/* { dg-final { scan-assembler-times "\tncgrk\t" 1 } } */

/* or with complement */

int
ocrk (int a, int b)
{
  return a | ~b;
}

/* { dg-final { scan-assembler-times "\tocrk\t" 1 } } */

long long
ocgrk (long long a, long long b)
{
  return a | ~b;
}

/* { dg-final { scan-assembler-times "\tocgrk\t" 1 } } */

/* nand */

int
nnrk (int a, int b)
{
  return ~(a & b);
}

/* { dg-final { scan-assembler-times "\tnnrk\t" 1 } } */

long long
nngrk (long long a, long long b)
{
  return ~(a & b);
}

/* { dg-final { scan-assembler-times "\tnngrk\t" 1 } } */

/* nor */

int
nork (int a, int b)
{
  return ~(a | b);
}

/* { dg-final { scan-assembler-times "\tnork\t" 1 } } */

long long
nogrk (long long a, long long b)
{
  return ~(a | b);
}

/* { dg-final { scan-assembler-times "\tnogrk\t" 1 } } */

/* nxor */

int
nxrk (int a, int b)
{
  return ~(a ^ b);
}

/* { dg-final { scan-assembler-times "\tnxrk\t" 1 } } */

long long
nxgrk (long long a, long long b)
{
  return ~(a ^ b);
}

/* { dg-final { scan-assembler-times "\tnxgrk\t" 1 } } */
