/* { dg-do compile } */
/* { dg-options "-O2" } */

/* Each function reads *P twice, in separate full expressions.  Combine
   must not merge the two volatile reads when it simplifies the
   arithmetic on them.  */

int
dbl (volatile int *p)
{
  int a = *p;
  return 2 * a - *p;
}

int
shl3 (volatile int *p)
{
  int a = *p;
  int b = *p;
  return (a << 3) - b;
}

long
lshl3 (volatile long *p)
{
  long a = *p;
  long b = *p;
  return (a << 3) - b;
}

unsigned
poll2 (volatile unsigned *p)
{
  unsigned s1 = *p;
  unsigned s2 = *p;
  return (s1 << 4) - s2;
}

short
sh2 (volatile short *p)
{
  short a = *p;
  short b = *p;
  return (short) ((a << 2) - b);
}

int
xp1 (volatile int *p, int x)
{
  int a = *p;
  int b = *p;
  return (x + 1) * a - b;
}

int
xm1 (volatile int *p, int x)
{
  int a = *p;
  int b = *p;
  return (x - 1) * a + b;
}

int
neg2 (volatile int *p)
{
  int a = *p;
  int b = *p;
  return -a - b;
}

int
mul42 (volatile int *p)
{
  int a = *p;
  int b = *p;
  return a * 4 - b * 2;
}

/* { dg-final { scan-assembler-times {\[x0\]} 18 } } */
