/* PR middle-end/120631 */
/* { dg-do run } */
/* { dg-options "-O2" } */

_Decimal64 a = 1234567891357900000.0dd;
long long b = 1234567891357900000LL;
_Decimal32 c = 1234567000000000000.0df;
long long d = 1234567000000000000LL;

int
main ()
{
  if (a != b || (long long) a != b || c != d || (long long) c != d)
    __builtin_abort ();
  _Decimal64 e = 1234567891357900000.0dd;
  long long f = 1234567891357900000LL;
  _Decimal64 g = 1234567891357900000LL;
  long long h = 1234567891357900000.0dd;
  _Decimal32 i = 1234567000000000000.0df;
  long long j = 1234567000000000000LL;
  _Decimal32 k = 1234567000000000000LL;
  long long l = 1234567000000000000.0df;
  if (e != g || f != h || i != k || j != l)
    __builtin_abort ();
}
