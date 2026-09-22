/* { dg-do compile } */
/* { dg-final { check-function-bodies "**" "" } } */

#include "arm_neon_test.h"

/*
** test_vldrq_p128:
** ldp	x0, x1, \[x0\]
** ret
*/
TEST_UNARY (vldrq_p128, poly128_t, const poly128_t *)
