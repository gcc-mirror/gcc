/* { dg-do assemble } */
/* { dg-require-effective-target arm_hard_ok } */
/* { dg-require-effective-target arm_v8_2a_bf16_neon_ok } */
/* { dg-add-options arm_v8_2a_bf16_neon } */
/* { dg-additional-options "-save-temps -O2 -mfloat-abi=hard" }  */
/* { dg-final { check-function-bodies "**" "" } } */

#include "arm_neon.h"


/*
**test_vld2_bf16:
**	...
**	vld[0-9]+.16	{d[0-9]+-d[0-9]+}, \[r[0-9]+\]
**	...
*/
bfloat16x4x2_t
test_vld2_bf16 (bfloat16_t * ptr)
{
  return vld2_bf16 (ptr);
}

/*
**test_vld2q_bf16:
**	...
**	vld[0-9]+.16	{d[0-9]+-d[0-9]+}, \[r[0-9]+\]
**	...
*/
bfloat16x8x2_t
test_vld2q_bf16 (bfloat16_t * ptr)
{
  return vld2q_bf16 (ptr);
}

/*
**test_vld2_dup_bf16:
**	...
**	vld[0-9]+.16	{d[0-9]+\[\], d[0-9]+\[\]}, \[r[0-9]+\]
**	...
*/
bfloat16x4x2_t
test_vld2_dup_bf16 (bfloat16_t * ptr)
{
  return vld2_dup_bf16 (ptr);
}

/*
**test_vld2q_dup_bf16:
**	...
**	vld[0-9]+.16	{d[0-9]+, d[0-9]+, d[0-9]+, d[0-9]+}, \[r[0-9]+\]
**	...
*/
bfloat16x8x2_t
test_vld2q_dup_bf16 (bfloat16_t * ptr)
{
  return vld2q_dup_bf16 (ptr);
}

/*
**test_vld3_bf16:
**	...
**	vld[0-9]+.16	{d[0-9]+-d[0-9]+}, \[r[0-9]+\]
**	...
*/
bfloat16x4x3_t
test_vld3_bf16 (bfloat16_t * ptr)
{
  return vld3_bf16 (ptr);
}

/*
**test_vld3q_bf16:
**	...
**	vld[0-9]+.16	{d[0-9]+, d[0-9]+, d[0-9]+}, \[r[0-9]+\]
**	...
*/
bfloat16x8x3_t
test_vld3q_bf16 (bfloat16_t * ptr)
{
  return vld3q_bf16 (ptr);
}

/*
**test_vld3_dup_bf16:
**	...
**	vld[0-9]+.16	{d[0-9]+\[\], d[0-9]+\[\], d[0-9]+\[\]}, \[r[0-9]+\]
**	...
*/
bfloat16x4x3_t
test_vld3_dup_bf16 (bfloat16_t * ptr)
{
  return vld3_dup_bf16 (ptr);
}

/*
**test_vld3q_dup_bf16:
**	...
**	vld[0-9]+.16	{d[0-9]+\[\], d[0-9]+\[\], d[0-9]+\[\]}, \[r[0-9]+\]
**	...
*/
bfloat16x8x3_t
test_vld3q_dup_bf16 (bfloat16_t * ptr)
{
  return vld3q_dup_bf16 (ptr);
}

/*
**test_vld4_bf16:
**	...
**	vld4.16	{d[0-9]+-d[0-9]+}, \[r[0-9]+\]
**	...
*/
bfloat16x4x4_t
test_vld4_bf16 (bfloat16_t * ptr)
{
  return vld4_bf16 (ptr);
}

/*
**test_vld4q_bf16:
**	...
**	vld4.16	{d[0-9]+, d[0-9]+, d[0-9]+, d[0-9]+}, \[r[0-9]+\]
**	...
*/
bfloat16x8x4_t
test_vld4q_bf16 (bfloat16_t * ptr)
{
  return vld4q_bf16 (ptr);
}

/*
**test_vld4_dup_bf16:
**	...
**	vld4.16	{d[0-9]+\[\], d[0-9]+\[\], d[0-9]+\[\], d[0-9]+\[\]}, \[r[0-9]+\]
**	...
*/
bfloat16x4x4_t
test_vld4_dup_bf16 (bfloat16_t * ptr)
{
  return vld4_dup_bf16 (ptr);
}

/*
**test_vld4q_dup_bf16:
**	...
**	vld4.16	{d[0-9]+\[\], d[0-9]+\[\], d[0-9]+\[\], d[0-9]+\[\]}, \[r[0-9]+\]
**	...
*/
bfloat16x8x4_t
test_vld4q_dup_bf16 (bfloat16_t * ptr)
{
  return vld4q_dup_bf16 (ptr);
}
