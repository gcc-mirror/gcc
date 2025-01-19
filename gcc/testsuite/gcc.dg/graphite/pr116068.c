/* PR ipa/116068 */
/* { dg-do compile { target { lto && { bitint && int128 } } } } */
/* { dg-options "-Os -flto -ffat-lto-objects -floop-parallelize-all -ftree-parallelize-loops=2 --param=parloops-schedule=dynamic" } */

#if __BITINT_MAXWIDTH__ >= 1024
typedef _BitInt (1024) A;
typedef __attribute__((__vector_size__ (16))) char B;
typedef __attribute__((__vector_size__ (16))) int C;
B a;
A b;
int c;
unsigned int *p;

void
foo0 (unsigned _BitInt (512) x)
{
  C d = {};
  _BitInt (1024) e = x | *(A *) __builtin_memset (&b, c, 8);
  unsigned h = __builtin_stdc_first_leading_zero (*p);
  C f = *(C *) __builtin_memset (&d, h, 6);
  B g = (B) f;
  a = g + (B) (__int128) e;
}
#else
int i;
#endif
