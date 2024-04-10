/* PR tree-optimization/108068 */
/* { dg-options "-O2" } */

int
main ()
{
  _Decimal64 x = -1;
  while (x != 0)
    x /= 10;
  double d = x;
  if (!__builtin_signbit (d))
    __builtin_abort ();
}
