/* PR tree-optimization/112374 */
/* { dg-do compile } */
/* { dg-options "-fcompare-debug -gno-statement-frontiers -O2" } */
/* { dg-additional-options "-march=skylake-avx512" { target i?86-*-* x86_64-*-* } } */
/* { dg-additional-options "-march=armv9-a" { target aarch64*-*-* } } */

void foo (int, int);
struct S { char s[64]; } *p;
char a, b;
unsigned char c;
int d, e;

void
bar (void)
{
  unsigned i;
  long j = 0;
  for (i = 0; i < b; ++i)
    j |= (p->s[i] ? 3 : 0) << i;
  if (p->s[i + 1])
  lab:
    for (;;)
      ;
  for (i = 0; i < 4; ++i)
    j |= p->s[i] << i;
  for (; i; i += 2)
    if (c + 1 != a)
      goto lab;
  for (; i < 8; ++i)
    j |= p->s[i] >= 6;
  if (j)
    foo (d, e);
}
