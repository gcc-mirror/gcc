/* PR target/29776 */
/* PR tree-optimization/61725 */
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

extern void link_error (void);

unsigned int d;
unsigned long e;
unsigned long long f;

void
foo (int a, long b, long long c)
{
  C (ffs, 0, 0)
  a &= 63; b &= 63; c &= 63;
  B (ffs, 0, 6)
  a++; b++; c++;
  B (ffs, 1, 7)
  a -= 2; b -= 2; c -= 2;
  C (ffs, 0, 0)
  a -= 63; b -= 63; c -= 63;
  C (ffs, 1, 0)
}
