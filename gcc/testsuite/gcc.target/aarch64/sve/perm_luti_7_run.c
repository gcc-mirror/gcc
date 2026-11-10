/* { dg-do run { target { { aarch64_sve_hw && aarch64_lut_hw } && le } } } */
/* { dg-options "-O2 -march=armv9-a+lut -msve-vector-bits=scalable" } */

#include "perm_luti_7.c"

int
main (void)
{
  unsigned int nb = svcntb ();
  unsigned int nh = svcnth ();
  uint8_t bytes[nb], byte_result[nb];
  uint16_t x[nh], y[nh], halfword_result[nh];
  const unsigned char indices[4] = { 2, 0, 3, 1 };
  for (unsigned int i = 0; i < nb; ++i)
    bytes[i] = i * 7 + 3;
  for (unsigned int i = 0; i < nh; ++i)
    {
      x[i] = i * 101 + 123;
      y[i] = i * 201 + 456;
    }

  svuint8_t b = svld1_u8 (svptrue_b8 (), bytes);
  svuint16_t h = svld1_u16 (svptrue_b16 (), x);
  svuint16_t h2 = svld1_u16 (svptrue_b16 (), y);

  svst1_u8 (svptrue_b8 (), byte_result, luti2_b (b));
  for (unsigned int i = 0; i < nb; ++i)
    if (byte_result[i] != bytes[indices[i % 4]])
      __builtin_abort ();

  svst1_u8 (svptrue_b8 (), byte_result, luti4_b (b));
  for (unsigned int i = 0; i < nb; ++i)
    if (byte_result[i] != bytes[i % 2 ? 9 : 10])
      __builtin_abort ();

  svst1_u16 (svptrue_b16 (), halfword_result, luti2_h (h));
  for (unsigned int i = 0; i < nh; ++i)
    if (halfword_result[i] != x[indices[i % 4]])
      __builtin_abort ();

  svst1_u16 (svptrue_b16 (), halfword_result, luti4_h (h));
  for (unsigned int i = 0; i < nh; ++i)
    if (halfword_result[i] != x[i % 2 ? 6 : 7])
      __builtin_abort ();

  svst1_u16 (svptrue_b16 (), halfword_result, luti4_h2 (h, h2));
  for (unsigned int i = 0; i < nh; ++i)
    if (halfword_result[i] != (i % 2 ? y[1] : x[2]))
      __builtin_abort ();

  return 0;
}
