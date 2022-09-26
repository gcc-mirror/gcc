/* Test INFINITY macro.  Test when infinities not supported.  */
/* { dg-do compile { target { ! inff } } } */
/* { dg-options "-std=c2x" } */

#include <float.h>

#ifdef INFINITY
#error "INFINITY defined"
#endif
