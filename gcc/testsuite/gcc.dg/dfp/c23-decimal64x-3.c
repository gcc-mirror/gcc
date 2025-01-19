/* Test _Decimal64x in C23 mode - builtins.  */
/* { dg-do run } */
/* { dg-options "-std=c23 -pedantic-errors" } */

_Decimal64x a = __builtin_infd64x ();
_Decimal64x b = __builtin_nand64x ("");
_Decimal64x c = __builtin_nansd64x ("");
_Decimal64x d = -42.D64x;
_Decimal64x e = 5.25d64x;

int
main ()
{
  if (__builtin_fabsd64x (-2.5d64x) != 2.5D64x
      || __builtin_fabsd64x (42.25D64x) != 42.25d64x
      || __builtin_fabsd64x (d) != 42.d64x
      || __builtin_fabsd64x (e) != 5.25D64x
      || __builtin_isinf (42.d64x)
      || __builtin_isnan (0.d64x)
      || !__builtin_isinf (__builtin_infd64x ())
      || !__builtin_isnan (__builtin_nand64x (""))
      /* || !__builtin_isinf (a) */
      || !__builtin_isnan (b)
      || !__builtin_isnan (c))
    __builtin_abort ();
}
