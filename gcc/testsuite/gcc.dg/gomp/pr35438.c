/* PR c/35438 */
/* { dg-do compile } */
/* { dg-options "-fopenmp" } */

void foo ();
#pragma omp threadprivate(foo)	/* { dg-error "is not a variable" } */
