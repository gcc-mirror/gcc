/* { dg-do compile } */
/* { dg-options "-O2 -march=armv8-a" } */
/* { dg-final { check-function-bodies "**" "" } } */

#include <stdint.h>

typedef int8_t v8qi __attribute__((vector_size (8)));
typedef int16_t v8hi __attribute__((vector_size (16)));
typedef uint8_t v8uqi __attribute__((vector_size (8)));
typedef uint16_t v8uhi __attribute__((vector_size (16)));
typedef int16_t v4hi __attribute__((vector_size (8)));
typedef int32_t v4si __attribute__((vector_size (16)));
typedef uint16_t v4uhi __attribute__((vector_size (8)));
typedef uint32_t v4usi __attribute__((vector_size (16)));

/*
** s8:
**	srshr	v0\.8b, v0\.8b, 3
**	ret
*/
v8qi
s8 (v8qi x)
{
  v8hi w = __builtin_convertvector (x, v8hi);
  return __builtin_convertvector ((w + 4) >> 3, v8qi);
}

/*
** u8:
**	urshr	v0\.8b, v0\.8b, 3
**	ret
*/
v8uqi
u8 (v8uqi x)
{
  v8uhi w = __builtin_convertvector (x, v8uhi);
  return __builtin_convertvector ((w + 4) >> 3, v8uqi);
}

/*
** s16:
**	srshr	v0\.4h, v0\.4h, 6
**	ret
*/
v4hi
s16 (v4hi x)
{
  v4si w = __builtin_convertvector (x, v4si);
  return __builtin_convertvector ((w + 32) >> 6, v4hi);
}

/*
** u16:
**	urshr	v0\.4h, v0\.4h, 6
**	ret
*/
v4uhi
u16 (v4uhi x)
{
  v4usi w = __builtin_convertvector (x, v4usi);
  return __builtin_convertvector ((w + 32) >> 6, v4uhi);
}

/*
** loop_s8:
**	ldr	d31, \[x1\]
**	srshr	v31\.8b, v31\.8b, 3
**	str	d31, \[x0\]
**	ret
*/
void
loop_s8 (int8_t *__restrict d, const int8_t *__restrict s)
{
  for (int i = 0; i < 8; i++)
    d[i] = (int8_t) ((s[i] + 4) >> 3);
}

/*
** loop_u8:
**	ldr	d31, \[x1\]
**	urshr	v31\.8b, v31\.8b, 3
**	str	d31, \[x0\]
**	ret
*/
void
loop_u8 (uint8_t *__restrict d, const uint8_t *__restrict s)
{
  for (int i = 0; i < 8; i++)
    d[i] = (uint8_t) ((s[i] + 4) >> 3);
}
