/* { dg-do compile } */
/* { dg-additional-options "--save-temps -O1" } */
/* { dg-final { check-function-bodies "**" "" "" } } */

#include <stdint.h>

#pragma GCC target "+cssc"

#define MIN(X, Y) ((X) > (Y) ? (Y) : (X))
#define MAX(X, Y) ((X) > (Y) ? (X) : (Y))

#define FUNC(T, OP, IMM)                \
T                                       \
T##_##OP##_##IMM (T a)                  \
{                                       \
  return OP (a, IMM);                   \
}                                       \

#define FUNCNEG(T, OP, IMM)             \
T                                       \
T##_##OP##_m##IMM (T a)                 \
{                                       \
  return OP (a, - (IMM));               \
}                                       \

/*
** uint32_t_MIN_255:
**      umin	w0, w0, 255
**      ret
*/

FUNC (uint32_t, MIN, 255)

/*
** uint64_t_MIN_255:
**      umin	x0, x0, 255
**      ret
*/

FUNC (uint64_t, MIN, 255)

/*
** uint32_t_MAX_255:
**      umax	w0, w0, 255
**      ret
*/

FUNC (uint32_t, MAX, 255)


/*
** uint64_t_MAX_255:
**      umax	x0, x0, 255
**      ret
*/

FUNC (uint64_t, MAX, 255)

/*
** int32_t_MIN_m128:
**      smin	w0, w0, -128
**      ret
*/

FUNCNEG (int32_t, MIN, 128)

/*
** int32_t_MIN_127:
**      smin	w0, w0, 127
**      ret
*/

FUNC (int32_t, MIN, 127)

/*
** int64_t_MIN_m128:
**      smin	x0, x0, -128
**      ret
*/

FUNCNEG (int64_t, MIN, 128)

/*
** int64_t_MIN_127:
**      smin	x0, x0, 127
**      ret
*/

FUNC (int64_t, MIN, 127)

/*
** int32_t_MAX_m128:
**      smax	w0, w0, -128
**      ret
*/

FUNCNEG (int32_t, MAX, 128)

/*
** int32_t_MAX_127:
**      smax	w0, w0, 127
**      ret
*/

FUNC (int32_t, MAX, 127)

/*
** int64_t_MAX_m128:
**      smax	x0, x0, -128
**      ret
*/

FUNCNEG (int64_t, MAX, 128)

/*
** int64_t_MAX_127:
**      smax	x0, x0, 127
**      ret
*/

FUNC (int64_t, MAX, 127)

/*
** int32_t_MIN_0:
**      smin	w0, w0, 0
**      ret
*/

FUNC (int32_t, MIN, 0)

/*
** int64_t_MIN_0:
**      smin	x0, x0, 0
**      ret
*/

FUNC (int64_t, MIN, 0)

/*
** int32_t_MAX_0:
**      smax	w0, w0, 0
**      ret
*/

FUNC (int32_t, MAX, 0)

/*
** int64_t_MAX_0:
**      smax	x0, x0, 0
**      ret
*/

FUNC (int64_t, MAX, 0)

