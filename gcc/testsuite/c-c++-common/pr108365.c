/* PR c++/108365 */
/* { dg-do compile { target { { ilp32 || lp64 } || llp64 } } } */
/* { dg-options "-O2 -fdump-tree-gimple" } */
/* { dg-final { scan-tree-dump-not " \\\((int|unsigned short int|long long int|unsigned int)\\\) " "gimple" } } */

unsigned short
foo (unsigned short x, unsigned short y)
{
  return (unsigned) x / y;
}

unsigned int
bar (unsigned int x, unsigned int y)
{
  return (long long) x / y;
}
