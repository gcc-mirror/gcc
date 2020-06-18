/* PR target/95713 */
/* { dg-do compile } */
/* { dg-options "-O2 -Wno-psabi -w" } */
/* { dg-additional-options "-mavx512bw" { target i?86-*-* x86_64-*-* } } */

typedef int v2si __attribute__((vector_size (8)));
typedef short int v2hi __attribute__((vector_size (4)));
void foo (v2hi);

void
bar (v2si x)
{
  v2hi a = (v2hi) { (short) x[0], (short) x[1] };
  foo (4 > a);
}
