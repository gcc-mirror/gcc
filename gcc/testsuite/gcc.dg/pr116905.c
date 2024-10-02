/* { dg-do compile } */
/* { dg-require-effective-target float16 } */
/* { dg-options "-frounding-math" } */
/* { dg-add-options float16 } */
/* { dg-additional-options "-mavx" { target avx } } */

typedef __attribute__((__vector_size__(16))) _Float16 F;
typedef __attribute__((__vector_size__(32))) int V;
F f;

void
foo()
{
  f += __builtin_convertvector((V){3307}, F);
}
