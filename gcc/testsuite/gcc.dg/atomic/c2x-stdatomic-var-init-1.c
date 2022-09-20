/* Test ATOMIC_VAR_INIT not in C2x.  */
/* { dg-do compile } */
/* { dg-options "-std=c2x -pedantic-errors" } */

#include <stdatomic.h>

#ifdef ATOMIC_VAR_INIT
#error "ATOMIC_VAR_INIT defined"
#endif
