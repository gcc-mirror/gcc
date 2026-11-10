/* { dg-do run } */
/* { dg-require-effective-target int32 } */

/* Check that, when a mask is applied to a right-shifted value before a
   narrowing conversion, mask bits that survive the conversion but lie
   above the shifted field still test extensions of its sign bit, while
   mask bits discarded by the conversion do not.  */

/* Bit 12 of the mask survives the conversion and tests a copy of bit 31
   of X.  */
__attribute__((noipa)) int
f1 (int x)
{
  int a = x >> 20;
  int t = a & 0x1006;
  short s = (short) t;
  if (s != 0)
    return 0;
  if ((x & 0x0ff00000) != 0)
    return 0;
  return 1;
}

/* Likewise, with an unsigned narrow type.  */
__attribute__((noipa)) int
f2 (int x)
{
  int a = x >> 20;
  int t = a & 0x1006;
  unsigned short s = (unsigned short) t;
  if (s != 0)
    return 0;
  if ((x & 0x0ff00000) != 0)
    return 0;
  return 1;
}

/* Mask bits 16 and up are discarded by the conversion, and bits 12 to 15
   are clear, so the sign bit of X is not tested.  */
__attribute__((noipa)) int
f3 (int x)
{
  int a = x >> 20;
  int t = a & 0xffff0006;
  short s = (short) t;
  if (s != 0)
    return 0;
  if ((x & 0x0ff00000) != 0)
    return 0;
  return 1;
}

/* Mask bits 16 and up are discarded, but bit 15 survives and tests a
   copy of bit 31 of X.  */
__attribute__((noipa)) int
f4 (int x)
{
  int a = x >> 20;
  int t = a & 0x7fff8006;
  short s = (short) t;
  if (s != 0)
    return 0;
  if ((x & 0x0ff00000) != 0)
    return 0;
  return 1;
}

int
main (void)
{
  int min = -0x7fffffff - 1;
  if (f1 (min) != 0 || f1 (0) != 1 || f1 (0x00600000) != 0)
    __builtin_abort ();
  if (f2 (min) != 0 || f2 (0) != 1 || f2 (0x00600000) != 0)
    __builtin_abort ();
  if (f3 (min) != 1 || f3 (0) != 1 || f3 (0x00600000) != 0)
    __builtin_abort ();
  if (f4 (min) != 0 || f4 (0) != 1 || f4 (0x00600000) != 0)
    __builtin_abort ();
  return 0;
}
