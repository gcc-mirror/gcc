/* { dg-do compile } */
/* { dg-require-effective-target vect_simd_clones } */
/* { dg-additional-options "-fopenmp-simd" } */

extern long a[];
#pragma omp declare simd
long my_fun(long);
long foo_i;
void foo() {
#pragma omp simd
  for (foo_i = 0; foo_i < 3; foo_i += 1)
    a[foo_i] = my_fun(foo_i);
}
