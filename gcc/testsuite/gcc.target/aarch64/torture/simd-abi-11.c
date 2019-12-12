/* { dg-do compile } */

int (*callee) (void);

int __attribute__ ((aarch64_vector_pcs))
caller (int *x)
{
  return callee () + 1;
}

/* { dg-final { scan-assembler {\sstp\tq8, q9} } } */
/* { dg-final { scan-assembler {\sstp\tq10, q11} } } */
/* { dg-final { scan-assembler {\sstp\tq12, q13} } } */
/* { dg-final { scan-assembler {\sstp\tq14, q15} } } */
/* { dg-final { scan-assembler {\sstp\tq16, q17} } } */
/* { dg-final { scan-assembler {\sstp\tq18, q19} } } */
/* { dg-final { scan-assembler {\sstp\tq20, q21} } } */
/* { dg-final { scan-assembler {\sstp\tq22, q23} } } */
/* { dg-final { scan-assembler {\sldp\tq8, q9} } } */
/* { dg-final { scan-assembler {\sldp\tq10, q11} } } */
/* { dg-final { scan-assembler {\sldp\tq12, q13} } } */
/* { dg-final { scan-assembler {\sldp\tq14, q15} } } */
/* { dg-final { scan-assembler {\sldp\tq16, q17} } } */
/* { dg-final { scan-assembler {\sldp\tq18, q19} } } */
/* { dg-final { scan-assembler {\sldp\tq20, q21} } } */
/* { dg-final { scan-assembler {\sldp\tq22, q23} } } */
