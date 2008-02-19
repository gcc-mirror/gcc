/* PR c++/34964 */
/* { dg-do compile } */
/* { dg-options "-fopenmp" } */

char x[] = 0;	/* { dg-error "invalid initializer" } */
#pragma omp threadprivate (x)
