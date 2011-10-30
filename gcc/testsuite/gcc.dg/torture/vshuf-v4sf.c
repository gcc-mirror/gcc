/* { dg-do run } */
/* { dg-options "-DEXPENSIVE" { target run_expensive_tests } } */
/* { dg-options "-fno-common" { target hppa*-*-hpux* } } */
/* { dg-skip-if "" { ! run_expensive_tests } { "*" } { "-O2" } } */
/* { dg-skip-if "" { ! run_expensive_tests } { "-flto" } { "" } } */

#if __SIZEOF_FLOAT__ == 4
typedef float V __attribute__((vector_size(16)));
# if __SIZEOF_INT__ == 4
typedef unsigned int VI __attribute__((vector_size(16)));
# elif __SIZEOF_LONG__ == 4
typedef unsigned long VI __attribute__((vector_size(16)));
# else
#  define UNSUPPORTED
# endif
#else
# define UNSUPPORTED
#endif

#include "vshuf-4.inc"
#include "vshuf-main.inc"
