/* { dg-do compile } */
/* { dg-options "-fopenmp" } */

int step (int);
int val;
#pragma omp declare simd linear (val (x) : step (1))	/* { dg-error "is neither constant nor a parameter" } */
int bar (int x, int y, int z);
#pragma omp declare simd linear (val (x) : val)		/* { dg-error "is neither constant nor a parameter" } */
int baz (int x, int y, int z);
