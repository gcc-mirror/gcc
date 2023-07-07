/* Check that the default case of the vec_init expander does its job.  */

/* { dg-do compile } */
/* { dg-options "-O3 -mzarch -march=z13" } */

typedef __attribute__((vector_size(16))) signed int v4si;

extern v4si G;

v4si
n (signed int a)
{
  return G == (v4si){ a };
}
/* { dg-final { scan-assembler-times "vzero" 1 } } */
/* { dg-final { scan-assembler-times "vlvgf\t" 1 } } */
/* { dg-final { scan-assembler-not "vleif\t" } } */
