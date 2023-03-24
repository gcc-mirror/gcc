/* { dg-additional-options "-fdump-tree-omp_transform_loops" }
 * { dg-additional-options "-fdump-tree-original" } */

extern void dummy (int);

void
test1 ()
{
  int i;
#pragma omp unroll full
  for (int i = 0; i < 10; i++)
    dummy (i);
}

 /* Loop should be removed with 10 copies of the body remaining
  * { dg-final { scan-tree-dump-times "dummy" 10 "omp_transform_loops" } }
  * { dg-final { scan-tree-dump "#pragma omp loop_transform" "original" } }
  * { dg-final { scan-tree-dump-not "#pragma omp" "omp_transform_loops" } } */
