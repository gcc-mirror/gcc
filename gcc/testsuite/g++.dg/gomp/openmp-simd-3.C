/* { dg-do compile } */
/* { dg-options "-fopenmp-simd -fdump-tree-original" } */

/* PR c/65586 */

void foo() { }

int main() {
#pragma omp for collapse(1)
  for (int i = 1; i <= 151; i+=31)
     foo();
}

/* { dg-final { scan-tree-dump-not "omp" "original" } } */
