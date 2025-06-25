/* { dg-do run } */
/* { dg-options "-O -fschedule-insns -favoid-store-forwarding" } */

unsigned a, b, c;

void
foo (_BitInt(2) b2, unsigned _BitInt(255) by, unsigned _BitInt(5) b5,
     unsigned _BitInt(256) *ret)
{
  unsigned _BitInt(255) bx = b2;
  by += 0x80000000000000000000000000000000wb;
  __builtin_memmove (&b, &c, 3);
  unsigned d = b;
  unsigned e = __builtin_stdc_rotate_right (0x1uwb % b5, a);
  unsigned _BitInt(256) r = by + bx + d + e;
  *ret = r;
}

int
main ()
{
  unsigned  _BitInt(256) x;
  foo (0, -1, 2, &x);
  if (x != 0x80000000000000000000000000000000wb)
    __builtin_abort();
}