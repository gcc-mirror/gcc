/* { dg-do compile } */
/* { dg-final { check-function-bodies "**" "" } } */

#include "arm_neon_test.h"

/*
** test_vstrq_p128:
** stp	x0, x1, \[x2\]
** ret
*/
TEST_STORE (vstrq_p128, poly128_t *, poly128_t)
