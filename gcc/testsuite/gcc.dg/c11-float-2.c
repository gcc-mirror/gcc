/* Test DECIMAL_DIG equals LDBL_DECIMAL_DIG; see DR#501 and N2108.  */
/* { dg-do preprocess } */
/* { dg-options "-std=c11 -pedantic-errors" } */

#include <float.h>

#if DECIMAL_DIG != LDBL_DECIMAL_DIG
# error "DECIMAL_DIG != LDBL_DECIMAL_DIG"
#endif
