! { dg-options "-cpp -fno-openacc" }
! { dg-do preprocess }
! { dg-require-effective-target fopenacc }

#ifdef _OPENACC
# error _OPENACC defined
#endif
