/* PR target/100302 */
/* { dg-do compile } */
/* { dg-options "-march=armv8.2-a+sve -O1 -ftree-loop-vectorize -fno-tree-scev-cprop --param vect-partial-vector-usage=0 -fvect-cost-model=unlimited" } */

long int x;

void
foo (void)
{
  for (x = 0; x >= 0; ++x)
    ;
}
