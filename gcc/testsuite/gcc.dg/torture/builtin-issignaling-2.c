/* { dg-do run } */
/* { dg-require-effective-target dfp } */
/* { dg-additional-options "-fsignaling-nans" } */

int
f1 (void)
{
  return __builtin_issignaling (__builtin_nansd32 (""));
}

int
f2 (void)
{
  return __builtin_issignaling (__builtin_nand64 (""));
}

int
f3 (void)
{
  return __builtin_issignaling (0.0DD);
}

int
f4 (_Decimal32 x)
{
  return __builtin_issignaling (x);
}

int
f5 (_Decimal64 x)
{
  return __builtin_issignaling (x);
}

int
f6 (_Decimal128 x)
{
  return __builtin_issignaling (x);
}

_Decimal32 x;
_Decimal64 y;
_Decimal128 z;

int
main ()
{
  if (!f1 () || f2 () || f3 ())
    __builtin_abort ();
  asm volatile ("" : : : "memory");
  if (f4 (x) || !f4 (__builtin_nansd32 ("0x123")) || f4 (42.0DF) || f4 (__builtin_nand32 ("0x234"))
      || f4 (__builtin_infd32 ()) || f4 (-__builtin_infd32 ()) || f4 (-42.0DF) || f4 (-0.0DF) || f4 (0.0DF))
    __builtin_abort ();
  x = __builtin_nansd32 ("");
  asm volatile ("" : : : "memory");
  if (!f4 (x))
    __builtin_abort ();
  if (f5 (y) || !f5 (__builtin_nansd64 ("0x123")) || f5 (42.0DD) || f5 (__builtin_nand64 ("0x234"))
      || f5 (__builtin_infd64 ()) || f5 (-__builtin_infd64 ()) || f5 (-42.0DD) || f5 (-0.0DD) || f5 (0.0DD))
    __builtin_abort ();
  y = __builtin_nansd64 ("");
  asm volatile ("" : : : "memory");
  if (!f5 (y))
    __builtin_abort ();
  if (f6 (z) || !f6 (__builtin_nansd128 ("0x123")) || f6 (42.0DL) || f6 (__builtin_nand128 ("0x234"))
      || f6 (__builtin_infd128 ()) || f6 (-__builtin_infd128 ()) || f6 (-42.0DL) || f6 (-0.0DL) || f6 (0.0DL))
    __builtin_abort ();
  z = __builtin_nansd128 ("");
  asm volatile ("" : : : "memory");
  if (!f6 (z))
    __builtin_abort ();
  return 0;
}
