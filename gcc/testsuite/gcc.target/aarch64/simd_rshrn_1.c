/* { dg-do compile } */
/* { dg-options "-O3 -march=armv8-a" } */
/* { dg-final { check-function-bodies "**" "" } } */

#include <stdint.h>

typedef int16_t v8hi __attribute__((vector_size (16)));
typedef int8_t v8qi __attribute__((vector_size (8)));
typedef uint16_t v8uhi __attribute__((vector_size (16)));
typedef uint8_t v8uqi __attribute__((vector_size (8)));
typedef int32_t v4si __attribute__((vector_size (16)));
typedef int16_t v4hi __attribute__((vector_size (8)));
typedef uint32_t v4usi __attribute__((vector_size (16)));
typedef uint16_t v4uhi __attribute__((vector_size (8)));

/*
** s16_s8:
**	rshrn	v0\.8b, v0\.8h, 6
**	ret
*/
v8qi
s16_s8 (v8hi x)
{
  return __builtin_convertvector ((x + 32) >> 6, v8qi);
}

/*
** u16_u8:
**	rshrn	v0\.8b, v0\.8h, 5
**	ret
*/
v8uqi
u16_u8 (v8uhi x)
{
  return __builtin_convertvector ((x + 16) >> 5, v8uqi);
}

/*
** s32_s16:
**	rshrn	v0\.4h, v0\.4s, 8
**	ret
*/
v4hi
s32_s16 (v4si x)
{
  return __builtin_convertvector ((x + 128) >> 8, v4hi);
}

/*
** u32_u16:
**	rshrn	v0\.4h, v0\.4s, 12
**	ret
*/
v4uhi
u32_u16 (v4usi x)
{
  return __builtin_convertvector ((x + 2048) >> 12, v4uhi);
}

#define ROUND_NARROW(NAME, TO, FROM, N, COUNT)				\
  void									\
  NAME (TO *__restrict d, const FROM *__restrict s)			\
  {									\
    for (int i = 0; i < COUNT; i++)					\
      d[i] = (TO) ((s[i] + (1 << (N - 1))) >> N);			\
  }

/*
** loop_s16_s8:
**	ldp	q31, q30, \[x1\]
**	rshrn	v31\.8b, v31\.8h, 6
**	rshrn2	v31\.16b, v30\.8h, 6
**	str	q31, \[x0\]
**	ret
*/
ROUND_NARROW (loop_s16_s8, int8_t, int16_t, 6, 16)

/*
** loop_u16_u8:
**	ldp	q31, q30, \[x1\]
**	rshrn	v31\.8b, v31\.8h, 5
**	rshrn2	v31\.16b, v30\.8h, 5
**	str	q31, \[x0\]
**	ret
*/
ROUND_NARROW (loop_u16_u8, uint8_t, uint16_t, 5, 16)

/*
** loop_s32_s16:
**	ldp	q31, q30, \[x1\]
**	rshrn	v31\.4h, v31\.4s, 8
**	rshrn2	v31\.8h, v30\.4s, 8
**	str	q31, \[x0\]
**	ret
*/
ROUND_NARROW (loop_s32_s16, int16_t, int32_t, 8, 8)

/*
** loop_u32_u16:
**	ldp	q31, q30, \[x1\]
**	rshrn	v31\.4h, v31\.4s, 12
**	rshrn2	v31\.8h, v30\.4s, 12
**	str	q31, \[x0\]
**	ret
*/
ROUND_NARROW (loop_u32_u16, uint16_t, uint32_t, 12, 8)
