/* PR c/79428 */
/* { dg-options "-fopenmp" } */
#pragma omp target /* { dg-error "expected declaration specifiers before end of line" } */
