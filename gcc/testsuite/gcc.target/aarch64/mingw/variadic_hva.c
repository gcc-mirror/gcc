/* { dg-do compile } */
/* { dg-additional-options "-std=c99" } */
/* { dg-final { check-function-bodies "**" "" } } */

#include <arm_neon.h>
#include <stdarg.h>

typedef struct {
  float32x4_t a;
  float32x4_t b;
  float32x4_t c;
  float32x4_t d;
} mat4x4;

mat4x4 accumulate(int count, ...) {
    va_list va;
    va_start(va, count);

    mat4x4 result = {
      vdupq_n_f32(0.0f),
      vdupq_n_f32(0.0f),
      vdupq_n_f32(0.0f),
      vdupq_n_f32(0.0f)
    };

    for (int i = 0; i < count; ++i) {
        mat4x4 v = va_arg(va, mat4x4);
        result.a = vaddq_f32(result.a, v.a);
        result.b = vaddq_f32(result.b, v.b);
        result.c = vaddq_f32(result.c, v.c);
        result.d = vaddq_f32(result.d, v.d);
    }

    va_end(va);
    return result;
}


/**
 * For aarch64-w64-mingw32 target, the Homogeneous Vector Aggregate (HVA) types
 * are not treated specially.
 *
 * This is in contrast to to aarch64-linux-gnu target where float32x4n args
 * would be loaded into 128 bit Q registers.
 */


/*
** main:
**	...
** 	ldr	q\d+, \[x\d+, #:lo\d+:\.LC\d+\]
** 	str	q\d+, \[sp, \d+\]
**	...
** 	ldr	q\d+, \[x\d+, #:lo\d+:\.LC\d+\]
** 	str	q\d+, \[sp, \d+\]
**	...
** 	ldr	q\d+, \[x\d+, #:lo\d+:\.LC\d+\]
** 	str	q\d+, \[sp, \d+\]
**	...
** 	ldr	q\d+, \[x\d+, #:lo\d+:\.LC\d+\]
** 	str	q\d+, \[sp, \d+\]
**  ...
** 	ldr	x\d+, \[sp, \d+\]
**  ...
** 	ldr	x\d+, \[sp, \d+\]
**  ...
** 	ldr	x\d+, \[sp, \d+\]
**  ...
** 	ldr	x\d+, \[sp, \d+\]
**  ...
** 	ldr	x\d+, \[sp, \d+\]
**  ...
** 	ldr	x\d+, \[sp, \d+\]
**  ...
** 	ldr	x\d+, \[sp, \d+\]
**  ...
** 	ldr	x\d+, \[sp, \d+\]
**  ...
*/
int main()
{
  float32x4_t x = {1.0, 2.0, 3.0, 4.0};
  float32x4_t y = {2.0, 3.0, 4.0, 5.0};
  float32x4_t z = {3.0, 4.0, 5.0, 6.0};
  float32x4_t w = {4.0, 5.0, 6.0, 7.0};

  accumulate (4, x, y, z, w);
  return 0;
}