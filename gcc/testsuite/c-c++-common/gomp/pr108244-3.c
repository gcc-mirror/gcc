/* { dg-do preprocess } */
/* { dg-additional-options "-fdirectives-only -fopenmp" } */
#pragma omp parallel
#ifdef t
#endif
