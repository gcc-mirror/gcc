/* PR target/29776 */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-vrp1" } */
/* { dg-final { scan-tree-dump-not "link_error" "vrp1"} } */
/* { dg-final { cleanup-tree-dump "vrp1" } } */

#define A(fn, arg, min, max) \
  if (__builtin_##fn (arg) < min || __builtin_##fn (arg) > max) \
    link_error ();
#define B(fn, min, max) \
  A (fn, a, min, max) A (fn##l, b, min, max) A (fn##ll, c, min, max)
#define C(fn, min, sub) \
  A (fn, a, min, ((int) sizeof (a) * __CHAR_BIT__ - sub)) \
  A (fn##l, b, min, ((int) sizeof (b) * __CHAR_BIT__ - sub)) \
  A (fn##ll, c, min, ((int) sizeof (c) * __CHAR_BIT__ - sub))
#define D(fn, sub1, sub2) \
  A (fn, a, ((int) sizeof (a) * __CHAR_BIT__ - sub1), \
     ((int) sizeof (a) * __CHAR_BIT__ - sub2)) \
  A (fn##l, b, ((int) sizeof (b) * __CHAR_BIT__ - sub1), \
     ((int) sizeof (b) * __CHAR_BIT__ - sub2)) \
  A (fn##ll, c, ((int) sizeof (c) * __CHAR_BIT__ - sub1), \
     ((int) sizeof (c) * __CHAR_BIT__ - sub2))

extern void link_error (void);

unsigned int d;
unsigned long e;
unsigned long long f;

void
foo (unsigned int a, unsigned long b, unsigned long long c)
{
  B (parity, 0, 1)
  C (ffs, 0, 0)
  C (popcount, 0, 0)
  C (clz, 0, 0)
  C (ctz, -1, 0)
  a &= 63;
  b &= 63;
  c &= 63;
  B (ffs, 0, 6)
  B (popcount, 0, 6)
  a += 3; b += 3; c += 3;
  B (ffs, 1, 7)
  B (popcount, 1, 7)
  a = 32U + (d & 1023U);
  b = 32UL + (e & 1023UL);
  c = 32ULL + (f & 1023ULL);
  D (clz, 11, 6)
  B (ctz, 0, 10)
}

void
bar (int a, long b, long long c)
{
  C (clrsb, 0, 1)
}
