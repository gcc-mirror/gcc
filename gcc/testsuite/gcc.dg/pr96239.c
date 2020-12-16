/* PR tree-optimization/96239 */
/* { dg-do run { target { ilp32 || lp64 } } } */
/* { dg-options "-O2 -fdump-tree-optimized" } */
/* { dg-final { scan-tree-dump-times " r>> 8;" 1 "optimized" { target bswap } } } */
/* { dg-final { scan-tree-dump-times " = __builtin_bswap64 " 1 "optimized" { target bswap } } } */
/* { dg-final { scan-tree-dump-not " >> \(8\|16\|24\|32\|40\|48\|56\);" "optimized" { target bswap } } } */

typedef unsigned char V __attribute__((vector_size (2)));
typedef unsigned char W __attribute__((vector_size (8)));

__attribute__((noipa)) void
foo (unsigned short x, V *p)
{
  *p = (V) { x >> 8, x };
}

__attribute__((noipa)) void
bar (unsigned long long x, W *p)
{
  *p = (W) { x >> 56, x >> 48, x >> 40, x >> 32, x >> 24, x >> 16, x >> 8, x };
}

__attribute__((noipa)) void
baz (unsigned short x, V *p)
{
  *p = (V) { x, x >> 8 };
}

__attribute__((noipa)) void
qux (unsigned long long x, W *p)
{
  *p = (W) { x, x >> 8, x >> 16, x >> 24, x >> 32, x >> 40, x >> 48, x >> 56 };
}

int
main ()
{
  V a, c, e, g;
  W b, d, f, h;
  foo (0xcafe, &a);
  bar (0xdeadbeefcafebabeULL, &b);
  baz (0xdead, &c);
  qux (0xfeedbac1beefdeadULL, &d);
  e = (V) { 0xca, 0xfe };
  f = (W) { 0xde, 0xad, 0xbe, 0xef, 0xca, 0xfe, 0xba, 0xbe };
  g = (V) { 0xad, 0xde };
  h = (W) { 0xad, 0xde, 0xef, 0xbe, 0xc1, 0xba, 0xed, 0xfe };
  if (__builtin_memcmp (&a, &e, sizeof (V))
      || __builtin_memcmp (&b, &f, sizeof (W))
      || __builtin_memcmp (&c, &g, sizeof (V))
      || __builtin_memcmp (&d, &h, sizeof (W)))
    __builtin_abort ();
  return 0;
}
