/* PR target/126480 */
/* { dg-do compile } */
/* { dg-additional-options "-O2 -march=armv8.2-a -fdump-tree-slp2-details" } */

void
foo (unsigned int *foo, unsigned long *a, unsigned long *b)
{
  foo[0] = a[0] >> 33;
  foo[1] = a[0] >> 44;
}

/* If costs are pessimistically charged for scalar truncations, then the
   function is vectorized.  Truncation is expected to be free as part of the
   store operations, therefore the scalar alternative is expected to be more
   efficient than the vectorized version.  */
/* { dg-final { scan-tree-dump-not "basic block part vectorized" "slp2" } } */

/* { dg-final { scan-assembler-not {\tushl\tv[0-9]+.2d, v[0-9]+.2d, v[0-9]+.2d\n} } } */
/* { dg-final { scan-assembler-times {\tlsr\tx[0-9]+, x[0-9]+, 33\n} 1 } } */
/* { dg-final { scan-assembler-times {\tlsr\tx[0-9]+, x[0-9]+, 44\n} 1 } } */
