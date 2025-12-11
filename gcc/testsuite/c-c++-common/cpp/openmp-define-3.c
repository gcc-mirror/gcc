/* { dg-options "-fopenmp" } */
/* { dg-do preprocess } */
/* { dg-require-effective-target fopenmp } */

#ifndef _OPENMP
# error _OPENMP not defined
#endif

#if _OPENMP != 202111
# error _OPENMP defined to wrong value
#endif
