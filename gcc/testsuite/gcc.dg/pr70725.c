/* PR tree-optimization/70725 */
/* { dg-do compile } */
/* { dg-options "-O3" } */
/* { dg-additional-options "-march=skylake-avx512" { target { i?86-*-* x86_64-*-* } } } */

extern short a;
extern int b, d;
extern int c[100];
extern int e;
extern int f;

void
fn1 ()
{
  for (; e < 2; e = e + 1)
    d = a;
  for (;;)
    for (int g = 0; g < 5; g = g + 1)
      for (int h = 0; h < 2; h = h + 1)
	for (int i = 0; i < 3; i = i + 1)
	  c[f + i] = a && b;
}
