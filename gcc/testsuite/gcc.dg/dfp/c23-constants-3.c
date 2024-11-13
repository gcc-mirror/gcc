/* Test that DFP constants are accepted in C23 mode.  */
/* { dg-do run } */
/* { dg-options "-std=c23 -pedantic-errors" } */

int a = (int) 1.1D32;
int b = (int) 2.d32;
int c = (int) .33D64;
int d = (int) 2e1d64;
int e = (int) .3e2D128;
int f = (int) 4.5e3d128;
int g = (int) 5.e0D32;
int h = (int) 1e+2d32;
int i = (int) 1000e-3D128;

#define expr_has_type(e, t) _Generic (e, default : 0, t : 1)
static_assert (expr_has_type (1.1D32, _Decimal32));
static_assert (expr_has_type (2.d32, _Decimal32));
static_assert (expr_has_type (.33D64, _Decimal64));
static_assert (expr_has_type (2e1d64, _Decimal64));
static_assert (expr_has_type (.3e2D128, _Decimal128));
static_assert (expr_has_type (4.5e3d128, _Decimal128));
static_assert (expr_has_type (5.e0D32, _Decimal32));
static_assert (expr_has_type (1e+2d32, _Decimal32));
static_assert (expr_has_type (1000e-3D128, _Decimal128));

int
main ()
{
  if (1.1D32 != 1.1df
      || 2.d32 != 2.df
      || .33D64 != .33dd
      || 2e1d64 != 2e1dd
      || .3e2D128 != .3e2dl
      || 4.5e3d128 != 4.5e3dl
      || 5.e0D32 != 5.e0df
      || 1e+2d32 != 1e+2df
      || 1000e-3D128 != 1000e-3dl)
    __builtin_abort ();
}
