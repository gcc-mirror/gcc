/* PR tree-optimization/112104 */
/* { dg-do compile } */
/* { dg-options "-O1 -fdump-tree-sccp-details" } */

unsigned int
__attribute__ ((noipa))
f_xor (unsigned int tmp, int n, unsigned int inv)
{
  unsigned int r = tmp;
  for (int i = 0; i < n; i++)
    r ^= inv;
  return r;
}

int
__attribute__ ((noipa))
f_xor1 (int n)
{
  int j = 0;
  for (int i = 0; i < n; i++)
    j ^= 1;
  return j;
}

unsigned long long
__attribute__ ((noipa))
f_xor64 (unsigned long long tmp, long n, unsigned long long inv)
{
  unsigned long long r = tmp;
  for (long i = 0; i < n; i++)
    r ^= inv;
  return r;
}

unsigned int
__attribute__ ((noipa))
f_xorc (unsigned int tmp, int n)
{
  unsigned int r = tmp;
  for (int i = 0; i < n; i++)
    r ^= 11304;
  return r;
}

/* { dg-final { scan-tree-dump-times {final value replacement} 4 "sccp" } } */
