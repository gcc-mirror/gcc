/* { dg-do compile } */
/* { dg-additional-options "-mautovec-preference=sve-only -fdump-tree-vect-details -O3 --param vect-epilogues-nomask=0" } */

int p[128];

bool __attribute__((noipa))
fand (int n, bool r1, bool r2)
{
  bool r = true;
  for (int i = 0; i < (n/2); i+=2)
    {
      r &= (p[i] != 0) & r1;
      r &= (p[i+1] != 0) & r2;
    }
  return r;
}
/* { dg-final { scan-tree-dump-times "optimized: loop vectorized" 1 "vect" } } */
