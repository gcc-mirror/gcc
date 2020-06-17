/* PR rtl-optimization/94344 */
/* { dg-do compile { target { ilp32 || lp64 } } } */
/* { dg-options "-O2 -fdump-tree-forwprop1" } */
/* { dg-final { scan-tree-dump-times " r>> 27;" 4 "forwprop1" } } */
/* { dg-final { scan-tree-dump-times " r>> 59;" 4 "forwprop1" } } */

int
f1 (int x)
{
  return (x << 5) | (int)((unsigned int)x >> 27);
}

unsigned int
f2 (int x)
{
  return (x << 5) | ((unsigned int)x >> 27);
}

long long int
f3 (long long int x)
{
  return (x << 5) | (long long int)((unsigned long long int)x >> 59);
}

unsigned long long int
f4 (long long int x)
{
  return (x << 5) | ((unsigned long long int)x >> 59);
}

int
f5 (int x)
{
  return (int)((unsigned int)x >> 27) | (x << 5);
}

unsigned int
f6 (int x)
{
  return ((unsigned int)x >> 27) | (x << 5);
}

long long int
f7 (long long int x)
{
  return (long long int)((unsigned long long int)x >> 59) | (x << 5);
}

unsigned long long int
f8 (long long int x)
{
  return ((unsigned long long int)x >> 59) | (x << 5);
}
