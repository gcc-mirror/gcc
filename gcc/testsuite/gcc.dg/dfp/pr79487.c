/* PR target/79487 */
/* { dg-options "-O2" } */

int
main ()
{
  _Decimal32 a = (-9223372036854775807LL - 1LL); 
  _Decimal32 b = -9.223372E+18DF;
  if (b - a != 0.0DF)
    __builtin_abort ();
  _Decimal64 c = (-9223372036854775807LL - 1LL); 
  _Decimal64 d = -9.223372036854776E+18DD;
  if (d - c != 0.0DD)
    __builtin_abort ();
  return 0;
}
