/* { dg-do compile } */
/* { dg-options "-fdump-tree-gimple" } */

unsigned f1(unsigned a, unsigned b)
{
  return (a ^ b) | a; 
}

/* { dg-final { scan-tree-dump "a | b" "gimple" } } */
