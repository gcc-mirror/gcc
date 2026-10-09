/* PR middle-end/121105 */
/* { dg-do run { target aarch64_sve_hw } } */
/* { dg-options "-O2" } */

#include <arm_sve.h>

static int
nonzero (svuint8_t v)
{
  return svptest_any (svptrue_b8 (), svcmpne (svptrue_b8 (), v, 0));
}

__attribute__((noipa)) svfloat32x4_t
dirty4 (void)
{
  svfloat32_t v = svdup_f32 (-1.0f);
  return svcreate4 (v, v, v, v);
}

__attribute__((noipa)) svfloat32x4_t
ret4 (void)
{
  svfloat32x4_t x = {};
  return x;
}

__attribute__((noipa)) svint8x2_t
ret2 (void)
{
  return (svint8x2_t) {};
}

__attribute__((noipa)) void
mem3 (svuint64x3_t *p)
{
  *p = (svuint64x3_t) {};
}

__attribute__((noipa)) void
dirty_stack (void)
{
  volatile unsigned char buf[4096];
  for (int i = 0; i < 4096; i++)
    buf[i] = 0xff;
}

__attribute__((noipa)) int
check2 (svint16x2_t *p)
{
  return (nonzero (svreinterpret_u8 (svget2 (*p, 0)))
	  || nonzero (svreinterpret_u8 (svget2 (*p, 1))));
}

__attribute__((noipa)) int
addr2 (void)
{
  svint16x2_t x = {};
  return check2 (&x);
}

__attribute__((noipa)) svboolx4_t
dirtyb (void)
{
  svbool_t t = svptrue_b8 ();
  return svcreate4 (t, t, t, t);
}

__attribute__((noipa)) svboolx4_t
retb4 (void)
{
  return (svboolx4_t) {};
}

__attribute__((noipa)) void
memb2 (svboolx2_t *p)
{
  *p = (svboolx2_t) {};
}

int
main (void)
{
  dirty4 ();
  svfloat32x4_t a = ret4 ();
  if (nonzero (svreinterpret_u8 (svget4 (a, 0)))
      || nonzero (svreinterpret_u8 (svget4 (a, 1)))
      || nonzero (svreinterpret_u8 (svget4 (a, 2)))
      || nonzero (svreinterpret_u8 (svget4 (a, 3))))
    __builtin_abort ();

  dirty4 ();
  svint8x2_t b = ret2 ();
  if (nonzero (svreinterpret_u8 (svget2 (b, 0)))
      || nonzero (svreinterpret_u8 (svget2 (b, 1))))
    __builtin_abort ();

  svuint64_t m1 = svdup_u64 (-1);
  svuint64x3_t c = svcreate3 (m1, m1, m1);
  mem3 (&c);
  if (nonzero (svreinterpret_u8 (svget3 (c, 0)))
      || nonzero (svreinterpret_u8 (svget3 (c, 1)))
      || nonzero (svreinterpret_u8 (svget3 (c, 2))))
    __builtin_abort ();

  dirty_stack ();
  if (addr2 ())
    __builtin_abort ();

  svbool_t all = svptrue_b8 ();
  dirtyb ();
  svboolx4_t d = retb4 ();
  if (svptest_any (all, svget4 (d, 0)) || svptest_any (all, svget4 (d, 1))
      || svptest_any (all, svget4 (d, 2)) || svptest_any (all, svget4 (d, 3)))
    __builtin_abort ();

  svboolx2_t e = svcreate2 (all, all);
  memb2 (&e);
  if (svptest_any (all, svget2 (e, 0)) || svptest_any (all, svget2 (e, 1)))
    __builtin_abort ();
  return 0;
}
