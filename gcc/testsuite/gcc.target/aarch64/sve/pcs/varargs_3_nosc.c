/* { dg-do run { target aarch64_sve_hw } } */
/* { dg-options "-O0 -fno-cprop-registers -fdisable-rtl-combine -g" } */

#include <arm_sve.h>
#include <stdarg.h>

void __attribute__((noipa))
callee (int foo, ...)
{
  va_list va;
  svbool_t pg, p;
  svint8_t s8;
  svuint16x4_t u16;
  svfloat32x3_t f32;
  svint64x2_t s64;

  va_start (va, foo);
  p = va_arg (va, svbool_t);
  s8 = va_arg (va, svint8_t);
  u16 = va_arg (va, svuint16x4_t);
  f32 = va_arg (va, svfloat32x3_t);
  s64 = va_arg (va, svint64x2_t);

  pg = svptrue_b8 ();

  if (svptest_any (pg, sveor_z (pg, p, svptrue_pat_b8 (SV_VL7))))
    __builtin_abort ();

  if (svptest_any (pg, svcmpne (pg, s8, svindex_s8 (1, 2))))
    __builtin_abort ();

  if (svptest_any (pg, svcmpne (pg, svget4 (u16, 0), svindex_u16 (2, 3))))
    __builtin_abort ();

  if (svptest_any (pg, svcmpne (pg, svget4 (u16, 1), svindex_u16 (3, 4))))
    __builtin_abort ();

  if (svptest_any (pg, svcmpne (pg, svget4 (u16, 2), svindex_u16 (4, 5))))
    __builtin_abort ();

  if (svptest_any (pg, svcmpne (pg, svget4 (u16, 3), svindex_u16 (5, 6))))
    __builtin_abort ();

  if (svptest_any (pg, svcmpne (pg, svget3 (f32, 0), svdup_f32 (1.0))))
    __builtin_abort ();

  if (svptest_any (pg, svcmpne (pg, svget3 (f32, 1), svdup_f32 (2.0))))
    __builtin_abort ();

  if (svptest_any (pg, svcmpne (pg, svget3 (f32, 2), svdup_f32 (3.0))))
    __builtin_abort ();

  if (svptest_any (pg, svcmpne (pg, svget2 (s64, 0), svindex_s64 (6, 7))))
    __builtin_abort ();

  if (svptest_any (pg, svcmpne (pg, svget2 (s64, 1), svindex_s64 (7, 8))))
    __builtin_abort ();
}

int __attribute__((noipa))
main (void)
{
  callee (100,
	  svptrue_pat_b8 (SV_VL7),
	  svindex_s8 (1, 2),
	  svcreate4 (svindex_u16 (2, 3),
		     svindex_u16 (3, 4),
		     svindex_u16 (4, 5),
		     svindex_u16 (5, 6)),
	  svcreate3 (svdup_f32 (1.0),
		     svdup_f32 (2.0),
		     svdup_f32 (3.0)),
	  svcreate2 (svindex_s64 (6, 7),
		     svindex_s64 (7, 8)));
}
