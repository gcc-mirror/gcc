/* Test _Decimal64x in C23 mode.  */
/* { dg-do run } */
/* { dg-options "-std=c23 -pedantic-errors" } */

int a = (int) 1.1D64x;
int b = (int) 2.d64x;
_Decimal64x c = 1;

#define expr_has_type(e, t) _Generic (e, default : 0, t : 1)
static_assert (expr_has_type (1.1D64x, _Decimal64x));
static_assert (expr_has_type (2.d64x, _Decimal64x));
static_assert (expr_has_type (.33D64x, _Decimal64x));
static_assert (expr_has_type (2e1d64x, _Decimal64x));
static_assert (expr_has_type (.3e2D64x, _Decimal64x));
static_assert (expr_has_type (4.5e3d64x, _Decimal64x));
static_assert (expr_has_type (5.e0D64x, _Decimal64x));
static_assert (expr_has_type (1e+2d64x, _Decimal64x));
static_assert (expr_has_type (1000e-3D64x, _Decimal64x));
static_assert (expr_has_type (__DEC64X_MIN__, _Decimal64x));
static_assert (expr_has_type (__DEC64X_MAX__, _Decimal64x));
static_assert (expr_has_type (__DEC64X_EPSILON__, _Decimal64x));
static_assert (expr_has_type (__DEC64X_SUBNORMAL_MIN__, _Decimal64x));
#if __DEC64X_MANT_DIG__ == __DEC128_MANT_DIG__ \
    && __DEC64X_MAX_EXP__ == __DEC128_MAX_EXP__
static_assert (expr_has_type (1.1D32 + 1.1D64x, _Decimal64x));
static_assert (expr_has_type (1.1D64 + 1.1D64x, _Decimal64x));
static_assert (expr_has_type (1.1D64x + 1.1D64x, _Decimal64x));
static_assert (expr_has_type (1.1D128 + 1.1D64x, _Decimal128));
static_assert (expr_has_type (1 + 1.1D64x, _Decimal64x));
static_assert (expr_has_type (1U + 1.1D64x, _Decimal64x));
static_assert (expr_has_type (1L + 1.1D64x, _Decimal64x));
static_assert (expr_has_type (1UL + 1.1D64x, _Decimal64x));
static_assert (expr_has_type (1LL + 1.1D64x, _Decimal64x));
static_assert (expr_has_type (1ULL + 1.1D64x, _Decimal64x));
static_assert (expr_has_type (1.1D64x + 1.1D32, _Decimal64x));
static_assert (expr_has_type (1.1D64x + 1.1D64, _Decimal64x));
static_assert (expr_has_type (1.1D64x + 1.1D64x, _Decimal64x));
static_assert (expr_has_type (1.1D64x + 1.1D128, _Decimal128));
static_assert (expr_has_type (1.1D64x + 1, _Decimal64x));
static_assert (expr_has_type (1.1D64x + 1U, _Decimal64x));
static_assert (expr_has_type (1.1D64x + 1L, _Decimal64x));
static_assert (expr_has_type (1.1D64x + 1UL, _Decimal64x));
static_assert (expr_has_type (1.1D64x + 1LL, _Decimal64x));
static_assert (expr_has_type (1.1D64x + 1ULL, _Decimal64x));
#endif

_Decimal64x
foo (_Decimal64x x, _Decimal64x y)
{
  return x + y;
}

int
main ()
{
#if __DEC64X_MANT_DIG__ == __DEC128_MANT_DIG__ \
    && __DEC64X_MAX_EXP__ == __DEC128_MAX_EXP__
  if (1.1D64x != 1.1dl
      || 2.d64x != 2.dl
      || .33D64x != .33dl
      || 2e1d64x != 2e1dl
      || .3e2D64x != .3e2dl
      || 4.5e3d64x != 4.5e3dl
      || 5.e0D64x != 5.e0dl
      || 1e+2d64x != 1e+2dl
      || 1000e-3D64x != 1000e-3dl
      || __DEC64X_MIN__ != __DEC128_MIN__
      || __DEC64X_MAX__ != __DEC128_MAX__
      || __DEC64X_EPSILON__ != __DEC128_EPSILON__
      || __DEC64X_SUBNORMAL_MIN__ != __DEC128_SUBNORMAL_MIN__
      || foo (0.5d64x, 0.5D64x) != 1.D64x)
    __builtin_abort ();
#endif
}
