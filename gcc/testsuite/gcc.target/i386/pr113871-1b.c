/* PR target/113871 */
/* { dg-do compile } */
/* { dg-options "-O2 -msse2" } */

typedef char vect32 __attribute__((vector_size(4)));

void f (vect32 *a)
{
  *a = __builtin_shufflevector(*a, (vect32){0}, 1, 2, 3, 4);
}

/* { dg-final { scan-assembler "psrld" } } */

void g(vect32 *a)
{
  *a = __builtin_shufflevector((vect32){0}, *a, 3, 4, 5, 6);
}

/* { dg-final { scan-assembler "pslld" } } */
