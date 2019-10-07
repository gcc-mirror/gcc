/* { dg-do compile } */

int __attribute__((aarch64_vector_pcs)) (*callee) (void);

int __attribute__ ((aarch64_vector_pcs))
caller (int *x)
{
  return callee () + 1;
}

/* { dg-final { scan-assembler-not {\tstp\tq} } } */
/* { dg-final { scan-assembler-not {\tldp\tq} } } */
/* { dg-final { scan-assembler-not {\tstr\tq} } } */
/* { dg-final { scan-assembler-not {\tldr\tq} } } */
