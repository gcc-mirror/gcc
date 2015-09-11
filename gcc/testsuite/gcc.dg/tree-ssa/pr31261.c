/* PR tree-optimization/31261 */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-original" } */

unsigned int
f1 (unsigned int a)
{
  return (8 - (a & 7)) & 7;
}

long int
f2 (long int b)
{
  return (16 + (b & 7)) & 15;
}

char
f3 (char c)
{
  return -(c & 63) & 31;
}

int
f4 (int d)
{
  return (12 - (d & 15)) & 7;
}

int
f5 (int e)
{
  return (12 - (e & 7)) & 15;
}

/* { dg-final { scan-tree-dump-times "return -a \& 7;" 1 "original" } } */
/* { dg-final { scan-tree-dump-times "return b \& 7;" 1 "original" } } */
/* { dg-final { scan-tree-dump-times "return \\(char\\) -\\(unsigned char\\) c \& 31;" 1 "original" } } */
/* { dg-final { scan-tree-dump-times "return \\(int\\) \\(12 - \\(unsigned int\\) d\\) \& 7;" 1 "original" { target { ! int16 } } } } */
/* { dg-final { scan-tree-dump-times "return \\(int\\) \\(12 - \\(unsigned short\\) d\\) \& 7;" 1 "original" { target { int16 } } } } */
/* { dg-final { scan-tree-dump-times "return 12 - \\(e \& 7\\) \& 15;" 1 "original" } } */
