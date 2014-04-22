/* Test vreinterpret_f64_* and vreinterpret_*_f64 intrinsics work correctly.  */
/* { dg-do run } */
/* { dg-options "-O3" } */

#include <arm_neon.h>

extern void abort (void);

#define ABS(a) __builtin_fabs (a)
#define ISNAN(a) __builtin_isnan (a)

#define DOUBLE_EQUALS(a, b, epsilon)		\
(						\
 ((a) == (b))					\
  || (ISNAN (a) && ISNAN (b))			\
  || (ABS (a - b) < epsilon)			\
)

/* Pi accurate up to 16 digits.
   Further digits are a closest binary approximation.  */
#define PI_F64 3.14159265358979311599796346854
/* Hex representation in Double (IEEE754 Double precision 64-bit) is:
   0x400921FB54442D18.  */

/* E accurate up to 16 digits.
   Further digits are a closest binary approximation.  */
#define E_F64 2.71828182845904509079559829843
/* Hex representation in Double (IEEE754 Double precision 64-bit) is:
   0x4005BF0A8B145769.  */

float32x2_t __attribute__ ((noinline))
wrap_vreinterpret_f32_f64 (float64x1_t __a)
{
  return vreinterpret_f32_f64 (__a);
}

int __attribute__ ((noinline))
test_vreinterpret_f32_f64 ()
{
  float64x1_t a;
  float32x2_t b;
  float64_t c[1] = { PI_F64 };
  /* Values { 0x54442D18, 0x400921FB } reinterpreted as f32.  */
  float32_t d[2] = { 3.3702805504E12, 2.1426990032196044921875E0 };
  float32_t e[2];
  int i;

  a = vld1_f64 (c);
  b = wrap_vreinterpret_f32_f64 (a);
  vst1_f32 (e, b);
  for (i = 0; i < 2; i++)
    if (!DOUBLE_EQUALS (d[i], e[i], __FLT_EPSILON__))
      return 1;
  return 0;
};

int8x8_t __attribute__ ((noinline))
wrap_vreinterpret_s8_f64 (float64x1_t __a)
{
  return vreinterpret_s8_f64 (__a);
}

int __attribute__ ((noinline))
test_vreinterpret_s8_f64 ()
{
  float64x1_t a;
  int8x8_t b;
  float64_t c[1] = { PI_F64 };
  int8_t d[8] = { 0x18, 0x2D, 0x44, 0x54, 0xFB, 0x21, 0x09, 0x40 };
  int8_t e[8];
  int i;

  a = vld1_f64 (c);
  b = wrap_vreinterpret_s8_f64 (a);
  vst1_s8 (e, b);
  for (i = 0; i < 8; i++)
    if (d[i] != e[i])
      return 1;
  return 0;
};

int16x4_t __attribute__ ((noinline))
wrap_vreinterpret_s16_f64 (float64x1_t __a)
{
  return vreinterpret_s16_f64 (__a);
}

int __attribute__ ((noinline))
test_vreinterpret_s16_f64 ()
{
  float64x1_t a;
  int16x4_t b;
  float64_t c[1] = { PI_F64 };
  int16_t d[4] = { 0x2D18, 0x5444, 0x21FB, 0x4009 };
  int16_t e[4];
  int i;

  a = vld1_f64 (c);
  b = wrap_vreinterpret_s16_f64 (a);
  vst1_s16 (e, b);
  for (i = 0; i < 4; i++)
    if (d[i] != e[i])
      return 1;
  return 0;
};

int32x2_t __attribute__ ((noinline))
wrap_vreinterpret_s32_f64 (float64x1_t __a)
{
  return vreinterpret_s32_f64 (__a);
}

int __attribute__ ((noinline))
test_vreinterpret_s32_f64 ()
{
  float64x1_t a;
  int32x2_t b;
  float64_t c[1] = { PI_F64 };
  int32_t d[2] = { 0x54442D18, 0x400921FB };
  int32_t e[2];
  int i;

  a = vld1_f64 (c);
  b = wrap_vreinterpret_s32_f64 (a);
  vst1_s32 (e, b);
  for (i = 0; i < 2; i++)
    if (d[i] != e[i])
      return 1;
  return 0;
};

int64x1_t __attribute__ ((noinline))
wrap_vreinterpret_s64_f64 (float64x1_t __a)
{
  return vreinterpret_s64_f64 (__a);
}

int __attribute__ ((noinline))
test_vreinterpret_s64_f64 ()
{
  float64x1_t a;
  int64x1_t b;
  float64_t c[1] = { PI_F64 };
  int64_t d[1] = { 0x400921FB54442D18 };
  int64_t e[1];
  int i;

  a = vld1_f64 (c);
  b = wrap_vreinterpret_s64_f64 (a);
  vst1_s64 (e, b);
  if (d[0] != e[0])
    return 1;
  return 0;
};

float32x4_t __attribute__ ((noinline))
wrap_vreinterpretq_f32_f64 (float64x2_t __a)
{
  return vreinterpretq_f32_f64 (__a);
}

int __attribute__ ((noinline))
test_vreinterpretq_f32_f64 ()
{
  float64x2_t a;
  float32x4_t b;
  float64_t c[2] = { PI_F64, E_F64 };

  /* Values corresponding to f32 reinterpret of
     { 0x54442D18, 0x400921FB, 0x8B145769, 0x4005BF0A }.  */
  float32_t d[4] = { 3.3702805504E12,
		     2.1426990032196044921875E0,
		     -2.8569523269651966444143014594E-32,
		     2.089785099029541015625E0 };
  float32_t e[4];
  int i;

  a = vld1q_f64 (c);
  b = wrap_vreinterpretq_f32_f64 (a);
  vst1q_f32 (e, b);
  for (i = 0; i < 4; i++)
    {
      if (!DOUBLE_EQUALS (d[i], e[i], __FLT_EPSILON__))
	return 1;
    }
  return 0;
};

int8x16_t __attribute__ ((noinline))
wrap_vreinterpretq_s8_f64 (float64x2_t __a)
{
  return vreinterpretq_s8_f64 (__a);
}

int __attribute__ ((noinline))
test_vreinterpretq_s8_f64 ()
{
  float64x2_t a;
  int8x16_t b;
  float64_t c[2] = { PI_F64, E_F64 };
  int8_t d[16] = { 0x18, 0x2D, 0x44, 0x54, 0xFB, 0x21, 0x09, 0x40,
		   0x69, 0x57, 0x14, 0x8B, 0x0A, 0xBF, 0x05, 0x40 };
  int8_t e[16];
  int i;

  a = vld1q_f64 (c);
  b = wrap_vreinterpretq_s8_f64 (a);
  vst1q_s8 (e, b);
  for (i = 0; i < 16; i++)
    if (d[i] != e[i])
      return 1;
  return 0;
};

int16x8_t __attribute__ ((noinline))
wrap_vreinterpretq_s16_f64 (float64x2_t __a)
{
  return vreinterpretq_s16_f64 (__a);
}

int __attribute__ ((noinline))
test_vreinterpretq_s16_f64 ()
{
  float64x2_t a;
  int16x8_t b;
  float64_t c[2] = { PI_F64, E_F64 };
  int16_t d[8] = { 0x2D18, 0x5444, 0x21FB, 0x4009,
		   0x5769, 0x8B14, 0xBF0A, 0x4005 };
  int16_t e[8];
  int i;

  a = vld1q_f64 (c);
  b = wrap_vreinterpretq_s16_f64 (a);
  vst1q_s16 (e, b);
  for (i = 0; i < 8; i++)
    if (d[i] != e[i])
      return 1;
  return 0;
};

int32x4_t __attribute__ ((noinline))
wrap_vreinterpretq_s32_f64 (float64x2_t __a)
{
  return vreinterpretq_s32_f64 (__a);
}

int __attribute__ ((noinline))
test_vreinterpretq_s32_f64 ()
{
  float64x2_t a;
  int32x4_t b;
  float64_t c[2] = { PI_F64, E_F64 };
  int32_t d[4] = { 0x54442D18, 0x400921FB, 0x8B145769, 0x4005BF0A };
  int32_t e[4];
  int i;

  a = vld1q_f64 (c);
  b = wrap_vreinterpretq_s32_f64 (a);
  vst1q_s32 (e, b);
  for (i = 0; i < 4; i++)
    if (d[i] != e[i])
      return 1;
  return 0;
};

int64x2_t __attribute__ ((noinline))
wrap_vreinterpretq_s64_f64 (float64x2_t __a)
{
  return vreinterpretq_s64_f64 (__a);
}

int __attribute__ ((noinline))
test_vreinterpretq_s64_f64 ()
{
  float64x2_t a;
  int64x2_t b;
  float64_t c[2] = { PI_F64, E_F64 };
  int64_t d[2] = { 0x400921FB54442D18, 0x4005BF0A8B145769 };
  int64_t e[2];
  int i;

  a = vld1q_f64 (c);
  b = wrap_vreinterpretq_s64_f64 (a);
  vst1q_s64 (e, b);
  for (i = 0; i < 2; i++)
    if (d[i] != e[i])
      return 1;
  return 0;
};

float64x1_t __attribute__ ((noinline))
wrap_vreinterpret_f64_f32 (float32x2_t __a)
{
  return vreinterpret_f64_f32 (__a);
}

int __attribute__ ((noinline))
test_vreinterpret_f64_f32 ()
{
  float32x2_t a;
  float64x1_t b;
  /* Values { 0x54442D18, 0x400921FB } reinterpreted as f32.  */
  float32_t c[2] = { 3.3702805504E12, 2.1426990032196044921875E0 };
  float64_t d[1] = { PI_F64 };
  float64_t e[1];
  int i;

  a = vld1_f32 (c);
  b = wrap_vreinterpret_f64_f32 (a);
  vst1_f64 (e, b);
  if (!DOUBLE_EQUALS (d[0], e[0], __DBL_EPSILON__))
    return 1;
  return 0;
};

float64x1_t __attribute__ ((noinline))
wrap_vreinterpret_f64_s8 (int8x8_t __a)
{
  return vreinterpret_f64_s8 (__a);
}

int __attribute__ ((noinline))
test_vreinterpret_f64_s8 ()
{
  int8x8_t a;
  float64x1_t b;
  int8_t c[8] = { 0x18, 0x2D, 0x44, 0x54, 0xFB, 0x21, 0x09, 0x40 };
  float64_t d[1] = { PI_F64 };
  float64_t e[1];
  int i;

  a = vld1_s8 (c);
  b = wrap_vreinterpret_f64_s8 (a);
  vst1_f64 (e, b);
  if (!DOUBLE_EQUALS (d[0], e[0], __DBL_EPSILON__))
    return 1;
  return 0;
};

float64x1_t __attribute__ ((noinline))
wrap_vreinterpret_f64_s16 (int16x4_t __a)
{
  return vreinterpret_f64_s16 (__a);
}

int __attribute__ ((noinline))
test_vreinterpret_f64_s16 ()
{
  int16x4_t a;
  float64x1_t b;
  int16_t c[4] = { 0x2D18, 0x5444, 0x21FB, 0x4009 };
  float64_t d[1] = { PI_F64 };
  float64_t e[1];
  int i;

  a = vld1_s16 (c);
  b = wrap_vreinterpret_f64_s16 (a);
  vst1_f64 (e, b);
  if (!DOUBLE_EQUALS (d[0], e[0], __DBL_EPSILON__))
    return 1;
  return 0;
};

float64x1_t __attribute__ ((noinline))
wrap_vreinterpret_f64_s32 (int32x2_t __a)
{
  return vreinterpret_f64_s32 (__a);
}

int __attribute__ ((noinline))
test_vreinterpret_f64_s32 ()
{
  int32x2_t a;
  float64x1_t b;
  int32_t c[2] = { 0x54442D18, 0x400921FB };
  float64_t d[1] = { PI_F64 };
  float64_t e[1];
  int i;

  a = vld1_s32 (c);
  b = wrap_vreinterpret_f64_s32 (a);
  vst1_f64 (e, b);
  if (!DOUBLE_EQUALS (d[0], e[0], __DBL_EPSILON__))
    return 1;
  return 0;
};

float64x1_t __attribute__ ((noinline))
wrap_vreinterpret_f64_s64 (int64x1_t __a)
{
  return vreinterpret_f64_s64 (__a);
}

int __attribute__ ((noinline))
test_vreinterpret_f64_s64 ()
{
  int64x1_t a;
  float64x1_t b;
  int64_t c[1] = { 0x400921FB54442D18 };
  float64_t d[1] = { PI_F64 };
  float64_t e[1];

  a = vld1_s64 (c);
  b = wrap_vreinterpret_f64_s64 (a);
  vst1_f64 (e, b);
  if (!DOUBLE_EQUALS (d[0], e[0], __DBL_EPSILON__))
    return 1;
  return 0;
};

float64x2_t __attribute__ ((noinline))
wrap_vreinterpretq_f64_f32 (float32x4_t __a)
{
  return vreinterpretq_f64_f32 (__a);
}

int __attribute__ ((noinline))
test_vreinterpretq_f64_f32 ()
{
  float32x4_t a;
  float64x2_t b;
  /* Values corresponding to f32 reinterpret of
     { 0x54442D18, 0x400921FB, 0x8B145769, 0x4005BF0A }.  */
  float32_t c[4] = { 3.3702805504E12,
		     2.1426990032196044921875E0,
		     -2.8569523269651966444143014594E-32,
		     2.089785099029541015625E0 };

  float64_t d[2] = { PI_F64, E_F64 };
  float64_t e[2];
  int i;

  a = vld1q_f32 (c);
  b = wrap_vreinterpretq_f64_f32 (a);
  vst1q_f64 (e, b);
  for (i = 0; i < 2; i++)
    if (!DOUBLE_EQUALS (d[i], e[i], __DBL_EPSILON__))
      return 1;
  return 0;
};

float64x2_t __attribute__ ((noinline))
wrap_vreinterpretq_f64_s8 (int8x16_t __a)
{
  return vreinterpretq_f64_s8 (__a);
}

int __attribute__ ((noinline))
test_vreinterpretq_f64_s8 ()
{
  int8x16_t a;
  float64x2_t b;
  int8_t c[16] = { 0x18, 0x2D, 0x44, 0x54, 0xFB, 0x21, 0x09, 0x40,
		   0x69, 0x57, 0x14, 0x8B, 0x0A, 0xBF, 0x05, 0x40 };
  float64_t d[2] = { PI_F64, E_F64 };
  float64_t e[2];
  int i;

  a = vld1q_s8 (c);
  b = wrap_vreinterpretq_f64_s8 (a);
  vst1q_f64 (e, b);
  for (i = 0; i < 2; i++)
    if (!DOUBLE_EQUALS (d[i], e[i], __DBL_EPSILON__))
      return 1;
  return 0;
};

float64x2_t __attribute__ ((noinline))
wrap_vreinterpretq_f64_s16 (int16x8_t __a)
{
  return vreinterpretq_f64_s16 (__a);
}

int __attribute__ ((noinline))
test_vreinterpretq_f64_s16 ()
{
  int16x8_t a;
  float64x2_t b;
  int16_t c[8] = { 0x2D18, 0x5444, 0x21FB, 0x4009,
		   0x5769, 0x8B14, 0xBF0A, 0x4005 };
  float64_t d[2] = { PI_F64, E_F64 };
  float64_t e[2];
  int i;

  a = vld1q_s16 (c);
  b = wrap_vreinterpretq_f64_s16 (a);
  vst1q_f64 (e, b);
  for (i = 0; i < 2; i++)
    if (!DOUBLE_EQUALS (d[i], e[i], __DBL_EPSILON__))
      return 1;
  return 0;
};

float64x2_t __attribute__ ((noinline))
wrap_vreinterpretq_f64_s32 (int32x4_t __a)
{
  return vreinterpretq_f64_s32 (__a);
}

int __attribute__ ((noinline))
test_vreinterpretq_f64_s32 ()
{
  int32x4_t a;
  float64x2_t b;
  int32_t c[4] = { 0x54442D18, 0x400921FB, 0x8B145769, 0x4005BF0A };
  float64_t d[2] = { PI_F64, E_F64 };
  float64_t e[2];
  int i;

  a = vld1q_s32 (c);
  b = wrap_vreinterpretq_f64_s32 (a);
  vst1q_f64 (e, b);
  for (i = 0; i < 2; i++)
    if (!DOUBLE_EQUALS (d[i], e[i], __DBL_EPSILON__))
      return 1;
  return 0;
};

float64x2_t __attribute__ ((noinline))
wrap_vreinterpretq_f64_s64 (int64x2_t __a)
{
  return vreinterpretq_f64_s64 (__a);
}

int __attribute__ ((noinline))
test_vreinterpretq_f64_s64 ()
{
  int64x2_t a;
  float64x2_t b;
  int64_t c[2] = { 0x400921FB54442D18, 0x4005BF0A8B145769 };
  float64_t d[2] = { PI_F64, E_F64 };
  float64_t e[2];
  int i;

  a = vld1q_s64 (c);
  b = wrap_vreinterpretq_f64_s64 (a);
  vst1q_f64 (e, b);
  for (i = 0; i < 2; i++)
    if (!DOUBLE_EQUALS (d[i], e[i], __DBL_EPSILON__))
      return 1;
  return 0;
};

int
main (int argc, char **argv)
{
  if (test_vreinterpret_f32_f64 ())
    abort ();

  if (test_vreinterpret_s8_f64 ())
    abort ();
  if (test_vreinterpret_s16_f64 ())
    abort ();
  if (test_vreinterpret_s32_f64 ())
    abort ();
  if (test_vreinterpret_s64_f64 ())
    abort ();

  if (test_vreinterpretq_f32_f64 ())
    abort ();

  if (test_vreinterpretq_s8_f64 ())
    abort ();
  if (test_vreinterpretq_s16_f64 ())
    abort ();
  if (test_vreinterpretq_s32_f64 ())
    abort ();
  if (test_vreinterpretq_s64_f64 ())
    abort ();

  if (test_vreinterpret_f64_f32 ())
    abort ();

  if (test_vreinterpret_f64_s8 ())
    abort ();
  if (test_vreinterpret_f64_s16 ())
    abort ();
  if (test_vreinterpret_f64_s32 ())
    abort ();
  if (test_vreinterpret_f64_s64 ())
    abort ();

  if (test_vreinterpretq_f64_f32 ())
    abort ();

  if (test_vreinterpretq_f64_s8 ())
    abort ();
  if (test_vreinterpretq_f64_s16 ())
    abort ();
  if (test_vreinterpretq_f64_s32 ())
    abort ();
  if (test_vreinterpretq_f64_s64 ())
    abort ();

  return 0;
}
