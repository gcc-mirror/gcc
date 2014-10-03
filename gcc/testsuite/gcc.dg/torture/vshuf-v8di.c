/* { dg-do run } */
/* { dg-options "-DEXPENSIVE" { target run_expensive_tests } } */
/* { dg-options "-fno-common" { target hppa*-*-hpux* } } */
/* { dg-skip-if "" { ! run_expensive_tests } { "*" } { "-O2" } } */
/* { dg-skip-if "" { ! run_expensive_tests } { "-flto" } { "" } } */

#if __SIZEOF_LONG_LONG__ == 8
typedef unsigned long long V __attribute__((vector_size(64)));
typedef V VI;
#else
#define UNSUPPORTED
#endif

#include "vshuf-8.inc"
#include "vshuf-main.inc"
