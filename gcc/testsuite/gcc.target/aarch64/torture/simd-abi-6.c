/* { dg-do compile } */

void __attribute__ ((aarch64_vector_pcs))
f (void)
{
  /* Clobber some fp/simd regs and verify that only those are saved
     and restored in the prologue and epilogue of a SIMD function. */
  __asm__ __volatile__ ("" :::  "q8", "q10", "q11");
}

/* { dg-final { scan-assembler {\sstp\tq8, q10} } } */
/* { dg-final { scan-assembler {\sstr\tq11} } } */
/* { dg-final { scan-assembler-not {\sstp\tq[0345679]} } } */
/* { dg-final { scan-assembler-not {\sldp\tq[0345679]} } } */
/* { dg-final { scan-assembler-not {\sstp\tq1[123456789]} } } */
/* { dg-final { scan-assembler-not {\sldp\tq1[123456789]} } } */
/* { dg-final { scan-assembler-not {\sstp\tq2[456789]} } } */
/* { dg-final { scan-assembler-not {\sldp\tq2[456789]} } } */
/* { dg-final { scan-assembler-not {\sstp\td} } } */
/* { dg-final { scan-assembler-not {\sldp\td} } } */
/* { dg-final { scan-assembler-not {\sstr\tq[023456789]} } } */
/* { dg-final { scan-assembler-not {\sldr\tq[023456789]} } } */
/* { dg-final { scan-assembler-not {\sstr\tq1[023456789]} } } */
/* { dg-final { scan-assembler-not {\sldr\tq1[023456789]} } } */
