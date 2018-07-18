/* PR middle-end/80162 */
/* { dg-do compile { target { { i?86-*-* x86_64-*-* } && lp64 } } } */
/* { dg-options "-msse2 -ffixed-xmm7" } */

typedef int v8 __attribute__ ((vector_size (8)));
struct U { v8 a; v8 b; };
register struct U u asm ("xmm7");

int *
foo (int i)
{
  return &u.a[i];	/* { dg-error "address of \[^ \n\r]* register variable" } */
}
