/* PR target/88905 */
/* { dg-do compile } */
/* { dg-options "-Og -fno-tree-ccp" } */
/* { dg-additional-options "-mabm" { target { i?86-*-* x86_64-*-* } } } */

int a, b, c;
extern void baz (int);

static inline int
bar (unsigned u)
{
  int i = __builtin_popcountll (-(unsigned long long) u);
  baz (i & c);
  return a + b + c;
}

void
foo (void)
{
  bar (2376498292ULL);
}
