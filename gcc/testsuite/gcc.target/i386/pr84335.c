/* PR target/84335 */
/* { dg-do compile } */
/* { dg-options "-O2 -maes -mno-sse2" } */
typedef long long V __attribute__ ((__vector_size__ (16)));

V
foo (V *a, V *b)
{
  return __builtin_ia32_aesenc128 (*a, *b);	/* { dg-error "needs isa option" } */
}
