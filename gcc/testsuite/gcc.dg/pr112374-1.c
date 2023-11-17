/* PR tree-optimization/112374 */
/* { dg-do compile } */
/* { dg-options "-fcompare-debug -gno-statement-frontiers -O2 -w" } */
/* { dg-additional-options "-march=skylake-avx512" { target i?86-*-* x86_64-*-* } } */
/* { dg-additional-options "-march=armv9-a" { target aarch64*-*-* } } */

void foo (int, int);
struct S { char s[4]; };
int a, b, c;

void
bar ()
{
  struct S d;
  long e = 0;
  for (c = 0; c < 4; ++c)
    e |= (d.s[c] ? 3 : 0) << c;
  if (e)
    foo (a, b);
}
