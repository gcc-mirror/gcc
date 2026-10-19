/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-final { check-function-bodies "**" "" } } */

#include <arm_sve.h>
#include <string.h>

/* Insert a scalar from a general register into an otherwise zero vector.
   The scalar move also clears the rest of the SVE register.  */

/*
** zero_8:
**	fmov	h0, w0
**	and	z0\.h, z0\.h, #0xff
**	ret
*/
__attribute__ ((noipa))
svuint8_t
zero_8 (uint8_t value)
{
  return svinsr_n_u8 (svdup_n_u8 (0), value);
}

/*
** nonzero_8:
**	insr	z0\.b, w0
**	ret
*/
__attribute__ ((noipa))
svuint8_t
nonzero_8 (svuint8_t vector, uint8_t value)
{
  return svinsr_n_u8 (vector, value);
}

/*
** zero_16:
**	fmov	h0, w0
**	ret
*/
__attribute__ ((noipa))
svuint16_t
zero_16 (uint16_t value)
{
  return svinsr_n_u16 (svdup_n_u16 (0), value);
}

/*
** nonzero_16:
**	insr	z0\.h, w0
**	ret
*/
__attribute__ ((noipa))
svuint16_t
nonzero_16 (svuint16_t vector, uint16_t value)
{
  return svinsr_n_u16 (vector, value);
}

/*
** zero_32:
**	fmov	s0, w0
**	ret
*/
__attribute__ ((noipa))
svuint32_t
zero_32 (uint32_t value)
{
  return svinsr_n_u32 (svdup_n_u32 (0), value);
}

/*
** nonzero_32:
**	insr	z0\.s, w0
**	ret
*/
__attribute__ ((noipa))
svuint32_t
nonzero_32 (svuint32_t vector, uint32_t value)
{
  return svinsr_n_u32 (vector, value);
}

/*
** zero_64:
**	fmov	d0, x0
**	ret
*/
__attribute__ ((noipa))
svuint64_t
zero_64 (uint64_t value)
{
  return svinsr_n_u64 (svdup_n_u64 (0), value);
}

/*
** nonzero_64:
**	insr	z0\.d, x0
**	ret
*/
__attribute__ ((noipa))
svuint64_t
nonzero_64 (svuint64_t vector, uint64_t value)
{
  return svinsr_n_u64 (vector, value);
}

/* The same applies to floating-point elements whose bits are in a general
   register.  */

/*
** zero_f16:
**	fmov	h0, w0
**	ret
*/
__attribute__ ((noipa))
svfloat16_t
zero_f16 (uint16_t bits)
{
  float16_t value;
  memcpy (&value, &bits, sizeof (value));
  return svinsr_n_f16 (svdup_n_f16 (0), value);
}

/*
** zero_bf16:
**	fmov	h0, w0
**	ret
*/
__attribute__ ((noipa))
svbfloat16_t
zero_bf16 (uint16_t bits)
{
  bfloat16_t value;
  memcpy (&value, &bits, sizeof (value));
  return svinsr_n_bf16 (svreinterpret_bf16 (svdup_n_u16 (0)), value);
}

/*
** zero_f32:
**	fmov	s0, w0
**	ret
*/
__attribute__ ((noipa))
svfloat32_t
zero_f32 (uint32_t bits)
{
  float32_t value;
  memcpy (&value, &bits, sizeof (value));
  return svinsr_n_f32 (svdup_n_f32 (0), value);
}

/*
** zero_f64:
**	fmov	d0, x0
**	ret
*/
__attribute__ ((noipa))
svfloat64_t
zero_f64 (uint64_t bits)
{
  float64_t value;
  memcpy (&value, &bits, sizeof (value));
  return svinsr_n_f64 (svdup_n_f64 (0), value);
}
