/* PR target/113871 */
/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2" } */

typedef char vect64 __attribute__((vector_size(8)));

void f (vect64 *a)
{
  *a = __builtin_shufflevector(*a, (vect64){0}, 1, 2, 3, 4, 5, 6, 7, 8);
}

/* { dg-final { scan-assembler "psrlq" } } */

void g(vect64 *a)
{
  *a = __builtin_shufflevector((vect64){0}, *a, 7, 8, 9, 10, 11, 12, 13, 14);
}

/* { dg-final { scan-assembler "psllq" } } */
