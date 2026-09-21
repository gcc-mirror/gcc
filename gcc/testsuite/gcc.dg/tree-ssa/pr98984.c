/* PR tree-optimization/98984 */
/* { dg-do compile } */
/* { dg-options "-O2 -fno-trapping-math -fdump-tree-optimized" } */


signed char
f1 (unsigned long long n)
{
  return (float)n;
}

signed char
f2 (long long n)
{
  return (float)n;
}

unsigned char
f3 (unsigned int n)
{
  return (float)n;
}

short
f4 (unsigned long long n)
{
  return (float)n;
}

unsigned short
f5 (unsigned long long n)
{
  return (double)n;
}

/* { dg-final { scan-tree-dump-not "\\(float\\)"  "optimized" } } */
/* { dg-final { scan-tree-dump-not "\\(double\\)" "optimized" } } */
