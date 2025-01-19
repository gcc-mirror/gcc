/* { dg-do run } */
/* { dg-options "-O1" } */

/* PR tree-optimization/118206 */
/* Check that shifts, whether before or after narrowing conversions, mask out
   the bits that are to be discarded.  */

/* This only uses bits from the least significant byte in the short.  */
__attribute__((noipa)) int
foo (const void *x)
{
  unsigned short b;
  __builtin_memcpy (&b, x, sizeof (short));
  if ((b & 15) != 8)
    return 1;
  if ((((unsigned char) b) >> 4) > 7)
    return 1;
  return 0;
}

__attribute__((noipa)) int
bar (const void *x)
{
  unsigned short b;
  __builtin_memcpy (&b, x, sizeof (short));
  if ((b & 15) != 8)
    return 1;
  if ((unsigned char)(b >> 4) > 7)
    return 1;
  return 0;
}

int
main ()
{
  unsigned short a = 0x78 - 0x80 - 0x80;
  if (foo (&a) != 0 || bar (&a) != (a > 0xff))
    __builtin_abort ();
  unsigned short b = 0x88;
  if (foo (&b) != 1 || bar (&b) != 1)
    __builtin_abort ();
  unsigned short c = 8;
  if (foo (&c) != 0 || bar (&c) != 0)
    __builtin_abort ();
  return 0;
}
