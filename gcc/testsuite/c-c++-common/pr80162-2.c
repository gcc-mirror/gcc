/* PR middle-end/80162 */
/* { dg-do compile { target { { i?86-*-* x86_64-*-* } && lp64 } } } */
/* { dg-options "-mavx2 -ffixed-xmm7" } */

typedef int V __attribute__ ((vector_size (32)));
register V u asm ("xmm7");

int
foo (int i)
{
  return u[i];
}

int
bar (void)
{
  return u[5];
}
