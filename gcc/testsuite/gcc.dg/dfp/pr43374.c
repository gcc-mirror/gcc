/* PR middle-end/43374 */
/* { dg-options "-O2" } */

__attribute__((noipa)) int
foo (_Decimal32 x)
{
  return __builtin_isinf (x);
}

__attribute__((noipa)) int
bar (_Decimal64 x)
{
  return __builtin_isfinite (x);
}

__attribute__((noipa)) int
baz (_Decimal128 x)
{
  return __builtin_isnormal (x);
}

int
main ()
{
  if (!foo (__builtin_infd32 ())
      || !foo (-__builtin_infd32 ())
      || foo (__builtin_nand32 (""))
      || foo (9.999999E96DF)
      || foo (-1E-95DF)
      || foo (0.999999E-95DF)
      || foo (-0.000001E-95DF)
      || foo (0.000DF)
      || foo (-0.00000DF))
    __builtin_abort ();
  if (bar (__builtin_infd64 ())
      || bar (-__builtin_infd64 ())
      || bar (__builtin_nand64 (""))
      || !bar (9.999999999999999E384DD)
      || !bar (-1E-383DD)
      || !bar (0.999999999999999E-383DD)
      || !bar (-0.000000000000001E-383DD)
      || !bar (0.000DD)
      || !bar (-0.0000000000DD))
    __builtin_abort ();
  if (baz (__builtin_infd128 ())
      || baz (-__builtin_infd128 ())
      || baz (__builtin_nand128 (""))
      || !baz (9.999999999999999999999999999999999E6144DL)
      || !baz (-1E-6143DL)
      || baz (0.999999999999999999999999999999999E-6143DL)
      || baz (-0.000000000000000000000000000000001E-6143DL)
      || baz (0.000DL)
      || baz (-0.0000000000000000000000DL))
    __builtin_abort ();
}
