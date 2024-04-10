/* PR target/113871 */
/* { dg-do compile } */
/* { dg-options "-O2 -msse2" } */

typedef short vect32 __attribute__((vector_size(4)));

void f (vect32 *a)
{
  *a = __builtin_shufflevector(*a, (vect32){0}, 1, 2);
}

/* { dg-final { scan-assembler "psrld" } } */

void g(vect32 *a)
{
  *a = __builtin_shufflevector((vect32){0}, *a, 1, 2);
}

/* { dg-final { scan-assembler "pslld" } } */
