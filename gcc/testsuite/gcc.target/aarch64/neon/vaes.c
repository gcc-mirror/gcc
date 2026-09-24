/* { dg-do compile } */
/* { dg-final { check-function-bodies "**" "" } } */

#include "arm_neon_test.h"

/*
** test_vaeseq_u8:
** aese	v0\.16b, v1\.16b
** ret
*/
TEST_UNIFORM_BINARY (vaeseq_u8, uint8x16_t)

/*
** test_vaesdq_u8:
** aesd	v0\.16b, v1\.16b
** ret
*/
TEST_UNIFORM_BINARY (vaesdq_u8, uint8x16_t)

/*
** test_vaesmcq_u8:
** aesmc	v0\.16b, v0\.16b
** ret
*/
TEST_UNIFORM_UNARY (vaesmcq_u8, uint8x16_t)

/*
** test_vaesimcq_u8:
** aesimc	v0\.16b, v0\.16b
** ret
*/
TEST_UNIFORM_UNARY (vaesimcq_u8, uint8x16_t)
