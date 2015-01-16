! { dg-options "-cpp" }
! { dg-do preprocess }
! { dg-require-effective-target fopenacc }

#ifdef _OPENACC
# error _OPENACC defined
#endif
