/* { dg-require-effective-target fopenmp } */
/* { dg-do preprocess } */

#ifdef _OPENMP
# error _OPENMP defined
#endif
