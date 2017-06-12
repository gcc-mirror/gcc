/* PR target/80310 */
/* { dg-do compile } */
/* { dg-options "-O2 -fsanitize=signed-integer-overflow" } */
/* { dg-additional-options "-mno-avx" { target i?86-*-* x86_64-*-* } } */

typedef int V __attribute__((vector_size (32)));

void
foo (V *a, V *b, V *c)
{
  *a = *b + *c;		/* { dg-bogus "AVX vector return without AVX enabled changes the ABI" "" { target i?86-*-* x86_64-*-* } } */
}
