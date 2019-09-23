/* PR rtl-optimization/89795 */
/* { dg-do run } */
/* { dg-options "-O2 -fno-dce -fno-forward-propagate -fno-sched-pressure" } */

unsigned char a;
unsigned b, c, d;

int
main ()
{
#if __CHAR_BIT__ == 8
  unsigned x;
  int e, f;
  unsigned char g;
  e = __builtin_bswap32 (a);
  f = __builtin_ffs (~(unsigned short) e);
  a = __builtin_mul_overflow ((unsigned char) 0xf7, f, &g);
  a |= __builtin_sub_overflow_p (c, 0, (unsigned char) 0);
  d = g + b;
  x = d;
  if (x != 0xf7)
    __builtin_abort ();
#endif
  return 0;
}
