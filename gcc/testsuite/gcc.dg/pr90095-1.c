/* PR middle-end/90095 */
/* { dg-do run } */
/* { dg-options "-Os -fno-tree-bit-ccp" } */

unsigned long long a;
unsigned int b;

int
main ()
{
  unsigned int c = 255, d = c |= b;
  if (__CHAR_BIT__ != 8 || __SIZEOF_INT__ != 4 || __SIZEOF_LONG_LONG__ != 8)
    return 0;
  d = __builtin_mul_overflow (-(unsigned long long) d, (unsigned char) - c, &a);
  if (d != 0)
    __builtin_abort ();
  return 0;
}
