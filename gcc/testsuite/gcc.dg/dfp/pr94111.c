/* PR middle-end/94111 */
/* { dg-do run } */
/* { dg-options "-O2" } */

int
main ()
{
  _Decimal32 d = (_Decimal32) __builtin_inff ();
  if (!__builtin_isinf ((double) d))
    __builtin_abort ();
  return 0;
}
