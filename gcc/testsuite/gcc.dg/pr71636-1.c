/* { dg-do compile } */
/* { dg-options "-fdump-tree-gimple" } */

unsigned f(unsigned x, unsigned b)
{
  return x & ((1U << b) - 1);
}

/* { dg-final { scan-tree-dump-not "1 <<" "gimple" } } */
