/* { dg-do compile } */ 
/* { dg-options "-O2 -fdump-tree-pre-stats -fno-tree-loop-im" } */
unsigned foo1 (unsigned a, unsigned b, unsigned j, unsigned k)
{
  unsigned i;
  for (i = 0; i != a; i++)
    {
      j += 4*b;
      k += 4*a;
    }
  return j + k;
}
/* We should eliminate both 4*b and 4*a from the main body of the loop */
/* { dg-final { scan-tree-dump-times "Eliminated: 2" 1 "pre"} } */
