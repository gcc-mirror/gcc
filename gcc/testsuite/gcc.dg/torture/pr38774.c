/* PR rtl-optimization/38774 */
/* { dg-do compile } */
/* { dg-options "" } */
/* { dg-options "-march=i686" { target { { i?86-*-* x86_64-*-* } && ia32 } } } */

extern int bar (void);
volatile int g;

int
foo (void)
{
  int a = 1 >= bar ();
  if ((1 > 9223372036854775807LL - a && 1 - a ? : 1 + a) & 1)
    return g;
  return 0;
}
