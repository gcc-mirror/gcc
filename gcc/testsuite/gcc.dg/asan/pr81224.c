/* PR sanitizer/80659 */
/* { dg-do compile { target { i?86-*-* x86_64-*-* } } } */
/* { dg-additional-options "-msse2" } */

int a;
int
b ()
{
  register __attribute__ ((__vector_size__ (4 * sizeof (int)))) int c asm("xmm0");
  return c[a];
}
