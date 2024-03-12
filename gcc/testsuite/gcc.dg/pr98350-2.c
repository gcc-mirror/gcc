/* { dg-do compile } */
/* { dg-options "-Ofast --param tree-reassoc-width=2 -fdump-tree-widening_mul" } */
/* { dg-additional-options "-mfpmath=sse -mfma" { target { i?86-*-* x86_64-*-* } } } */

/* Test that the compiler rearrange the ops to generate more FMA.  */

float
foo1 (float a, float b, float c, float d, float *e)
{
   return   *e + a * b + c * d ;
}
/* { dg-final { scan-tree-dump-times { = \.FMA \(} 2 "widening_mul" { target { i?86-*-* x86_64-*-* } } } } */
