! { dg-options "-cpp" }
! { dg-do preprocess }
! { dg-require-effective-target fopenmp }

#ifdef _OPENMP
# error _OPENMP defined
#endif
