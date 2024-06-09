/* Test ATOMIC_VAR_INIT not in C23.  */
/* { dg-do compile } */
/* { dg-options "-std=c23 -pedantic-errors" } */

#include <stdatomic.h>

#ifdef ATOMIC_VAR_INIT
#error "ATOMIC_VAR_INIT defined"
#endif
