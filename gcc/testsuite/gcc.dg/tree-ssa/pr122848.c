/* PR tree-optimization/122848 */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

int f1 (unsigned char a)
{
  int t = a;
  int t1 = (signed char) a;
  return t & t1;
}

int f2 (unsigned char a)
{
  int t = a;
  int t1 = (signed char) a;
  return t1 & t;
}

int f3 (unsigned char a)
{
  return (int) a & (int) (signed char) a;
}

int f4 (unsigned short a)
{
  return (int) a & (int) (short) a;
}

long f5 (unsigned int a)
{
  return (long) a & (long) (int) a;
}

unsigned int f6 (unsigned char a)
{
  return (unsigned int) a & (unsigned int) (signed char) a;
}

int f7 (unsigned char a)
{
  return (int) a | (int) (signed char) a;
}

int f8 (unsigned short a)
{
  return (int) a | (int) (short) a;
}

int f9 (signed char a)
{
  return (int) (unsigned char) a & (int) a;
}

int f10 (signed char a)
{
  return (int) a & (int) (unsigned char) a;
}

int f11 (signed char a)
{
  return (int) (unsigned char) a | (int) a;
}

int f12 (signed short a)
{
  return (int) (unsigned short) a | (int) a;
}

/* { dg-final { scan-tree-dump-not " & " "optimized" } } */
/* { dg-final { scan-tree-dump-not " \\| " "optimized" } } */
