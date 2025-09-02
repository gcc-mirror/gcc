/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-forwprop1-details" } */
/* { dg-final { check-function-bodies "**" "" "" } } */
/* PR tree-optimization/89606 */

#include <arm_neon.h>

/*
**func1:
**	ld2	{v0.2d - v1.2d}, \[x0\]
**	ld2	{v0.d - v1.d}\[1\], \[x1\]
**	ret
*/
float64x2x2_t func1(const double *p1, const double *p2)
{
    float64x2x2_t v = vld2q_f64(p1);
    return vld2q_lane_f64(p2, v, 1);
}

/*
**func2:
**	ld2	{v0.2s - v1.2s}, \[x0\]
**	ld2	{v0.s - v1.s}\[1\], \[x1\]
**	ret
*/
float32x2x2_t func2(const float *p1, const float *p2)
{
    float32x2x2_t v = vld2_f32(p1);
    return vld2_lane_f32(p2, v, 1);
}

/*
**func3:
**	ld2	{v([0-9]+).2s - v([0-9]+).2s}, \[x1\]
**	ld2	{v\1.s - v\2.s}\[1\], \[x2\]
**	stp	d\1, d\2, \[x0\]
**	ret
*/
void func3(float32x2x2_t *p, const float *p1, const float *p2)
{
    float32x2x2_t v = vld2_f32(p1);
    *p = vld2_lane_f32(p2, v, 1);
}

/* 2 copy props for each function  */
/* { dg-final { scan-tree-dump-times "after previous" 6 "forwprop1" } } */
