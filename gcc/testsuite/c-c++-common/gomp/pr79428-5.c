/* PR c/79428 */
/* { dg-options "-fopenmp" } */
#pragma omp ordered /* { dg-error "expected declaration specifiers before end of line" } */
