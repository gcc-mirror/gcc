/* PR rtl-optimization/89435 */
/* { dg-do run } */
/* { dg-options "-O1 -fno-forward-propagate -fno-tree-forwprop -fno-tree-ccp" } */

unsigned short a;
unsigned int b, c, d, e, f;

int
main ()
{
#if __CHAR_BIT__ == 8 && __SIZEOF_INT__ == 4
  unsigned char g = e = __builtin_mul_overflow_p (5, 542624702, 0);
  d = __builtin_bswap64 (a);
  b = __builtin_sub_overflow ((unsigned char) -e, (unsigned int) d, &g);
  e = __builtin_mul_overflow (b, c, &a);
  f = g + e;
  if (f != 0xff)
    __builtin_abort ();
#endif
  return 0;
}
