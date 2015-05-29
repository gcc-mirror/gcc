/* PR tree-optimization/64454 */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-vrp1 -fno-ipa-icf" } */

unsigned
f1 (unsigned x)
{
  return (x % 5) % 5;
}

int
f2 (int x)
{
  return (x % 5) % 5;
}

int
f3 (int x)
{
  return (x % -5) % -5;
}

unsigned
f4 (unsigned x)
{
  return (x % 5) % 6;
}

int
f5 (int x)
{
  return (x % 5) % 6;
}

int
f6 (int x)
{
  return (x % -5) % -6;
}

/* { dg-final { scan-tree-dump-times "% 5" 6 "vrp1" } } */
/* { dg-final { scan-tree-dump-times "% 6" 0 "vrp1" } } */
