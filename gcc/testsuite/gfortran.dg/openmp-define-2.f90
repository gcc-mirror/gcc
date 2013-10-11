! { dg-require-effective-target fopenmp }
! { dg-options "-cpp -fno-openmp" }
! { dg-do preprocess }

#ifdef _OPENMP
# error _OPENMP defined
#endif
