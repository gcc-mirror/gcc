/* { dg-do compile } */

extern void g (void);

void __attribute__ ((aarch64_vector_pcs))
f (void)
{
	g();
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
/* { dg-final { scan-assembler-not {\sstp\tq[034567]} } } */
/* { dg-final { scan-assembler-not {\sldp\tq[034567]} } } */
/* { dg-final { scan-assembler-not {\sstp\tq2[456789]} } } */
/* { dg-final { scan-assembler-not {\sldp\tq2[456789]} } } */
/* { dg-final { scan-assembler-not {\sstp\td} } } */
/* { dg-final { scan-assembler-not {\sldp\td} } } */
/* { dg-final { scan-assembler-not {\sstr\t} } } */
/* { dg-final { scan-assembler-not {\sldr\t} } } */
