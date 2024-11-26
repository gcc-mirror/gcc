/* PR middle-end/102674 */
/* { dg-do run } */
/* { dg-options "-O2" } */

#define FP_NAN 0
#define FP_INFINITE 1
#define FP_ZERO 2
#define FP_SUBNORMAL 3
#define FP_NORMAL 4

__attribute__((noipa)) int
foo (_Decimal32 x)
{
  return __builtin_fpclassify (FP_NAN, FP_INFINITE, FP_NORMAL,
			       FP_SUBNORMAL, FP_ZERO, x);
}

__attribute__((noipa)) int
bar (_Decimal64 x)
{
  return __builtin_fpclassify (FP_NAN, FP_INFINITE, FP_NORMAL,
			       FP_SUBNORMAL, FP_ZERO, x);
}

__attribute__((noipa)) int
baz (_Decimal128 x)
{
  return __builtin_fpclassify (FP_NAN, FP_INFINITE, FP_NORMAL,
			       FP_SUBNORMAL, FP_ZERO, x);
}

int
main ()
{
  if (foo (__builtin_infd32 ()) != FP_INFINITE
      || foo (-__builtin_infd32 ()) != FP_INFINITE
      || foo (__builtin_nand32 ("")) != FP_NAN
      || foo (9.999999E96DF) != FP_NORMAL
      || foo (-1E-95DF) != FP_NORMAL
      || foo (0.999999E-95DF) != FP_SUBNORMAL
      || foo (-0.000001E-95DF) != FP_SUBNORMAL
      || foo (0.000DF) != FP_ZERO
      || foo (-0.00000DF) != FP_ZERO)
    __builtin_abort ();
  if (bar (__builtin_infd64 ()) != FP_INFINITE
      || bar (-__builtin_infd64 ()) != FP_INFINITE
      || bar (__builtin_nand64 ("")) != FP_NAN
      || bar (9.999999999999999E384DD) != FP_NORMAL
      || bar (-1E-383DD) != FP_NORMAL
      || bar (0.999999999999999E-383DD) != FP_SUBNORMAL
      || bar (-0.000000000000001E-383DD) != FP_SUBNORMAL
      || bar (0.000DD) != FP_ZERO
      || bar (-0.0000000000DD) != FP_ZERO)
    __builtin_abort ();
  if (baz (__builtin_infd128 ()) != FP_INFINITE
      || baz (-__builtin_infd128 ()) != FP_INFINITE
      || baz (__builtin_nand128 ("")) != FP_NAN
      || baz (9.999999999999999999999999999999999E6144DL) != FP_NORMAL
      || baz (-1E-6143DL) != FP_NORMAL
      || baz (0.999999999999999999999999999999999E-6143DL) != FP_SUBNORMAL
      || baz (-0.000000000000000000000000000000001E-6143DL) != FP_SUBNORMAL
      || baz (0.000DL) != FP_ZERO
      || baz (-0.0000000000000000000000DL) != FP_ZERO)
    __builtin_abort ();
}
