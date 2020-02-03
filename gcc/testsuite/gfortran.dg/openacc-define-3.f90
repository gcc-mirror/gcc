! { dg-options "-cpp -fopenacc" }
! { dg-do preprocess }
! { dg-require-effective-target fopenacc }

#ifndef _OPENACC
# error _OPENACC not defined
#endif

#if _OPENACC != 201711
# error _OPENACC defined to wrong value
#endif
