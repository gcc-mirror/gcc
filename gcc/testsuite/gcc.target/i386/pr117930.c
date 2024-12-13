/* PR target/117930 */
/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-final { scan-assembler-not "sub\[bwlq\]\t" } } */
/* { dg-final { scan-assembler-not "add\[bwlq\]\t" } } */
/* { dg-final { scan-assembler-not "lea\[lq\]\t" } } */

static inline
unsigned lrotate (unsigned x, int t)
{
  unsigned tl = x << t;
  unsigned th = x >> (-t & 31);
  return tl | th;
}

static inline
unsigned rrotate (unsigned x, int t)
{
  unsigned tl = x >> t;
  unsigned th = x << (-t & 31);
  return tl | th;
}

unsigned
f1 (unsigned x, int t)
{
  return lrotate (x, 32 - t);
}

unsigned
f2 (unsigned x, int t)
{
  return lrotate (x, 64 - t);
}

unsigned
f3 (unsigned x, int t)
{
  return lrotate (x, 32 + t);
}

unsigned
f4 (unsigned x, int t)
{
  return lrotate (x, 64 + t);
}

unsigned
f5 (unsigned x, int t)
{
  return rrotate (x, 32 - t);
}

unsigned
f6 (unsigned x, int t)
{
  return rrotate (x, 64 - t);
}

unsigned
f7 (unsigned x, int t)
{
  return rrotate (x, 32 + t);
}

unsigned
f8 (unsigned x, int t)
{
  return rrotate (x, 64 + t);
}

unsigned
f9 (int t)
{
  return lrotate (0xdeadbeefU, 32 - t);
}

unsigned
f10 (int t)
{
  return lrotate (0xdeadbeefU, 64 - t);
}

unsigned
f11 (int t)
{
  return lrotate (0xdeadbeefU, 32 + t);
}

unsigned
f12 (int t)
{
  return lrotate (0xdeadbeefU, 64 + t);
}

unsigned
f13 (int t)
{
  return rrotate (0xdeadbeefU, 32 - t);
}

unsigned
f14 (int t)
{
  return rrotate (0xdeadbeefU, 64 - t);
}

unsigned
f15 (int t)
{
  return rrotate (0xdeadbeefU, 32 + t);
}

unsigned
f16 (int t)
{
  return rrotate (0xdeadbeefU, 64 + t);
}
