/* PR c/35439 */
/* { dg-do compile } */
/* { dg-options "-fopenmp" } */

void x[1];	/* { dg-error "array of voids" } */
#pragma omp threadprivate(x)
