/* { dg-do compile } */

void __attribute__ ((aarch64_vector_pcs))
f (void)
{
  /* Clobber some fp/simd regs and verify that only those are saved
     and restored in the prologue and epilogue of a SIMD function. */
  __asm__ __volatile__ ("" :::  "q8",  "q9", "q10", "q11");
}

/* { dg-final { scan-assembler {\sstp\tq8, q9} } } */
/* { dg-final { scan-assembler {\sstp\tq10, q11} } } */
/* { dg-final { scan-assembler-not {\sstp\tq[034567]} } } */
/* { dg-final { scan-assembler-not {\sldp\tq[034567]} } } */
/* { dg-final { scan-assembler-not {\sstp\tq1[23456789]} } } */
/* { dg-final { scan-assembler-not {\sldp\tq1[23456789]} } } */
/* { dg-final { scan-assembler-not {\sstp\tq2[456789]} } } */
/* { dg-final { scan-assembler-not {\sldp\tq2[456789]} } } */
/* { dg-final { scan-assembler-not {\sstp\td} } } */
/* { dg-final { scan-assembler-not {\sldp\td} } } */
/* { dg-final { scan-assembler-not {\sstr\t} } } */
/* { dg-final { scan-assembler-not {\sldr\t} } } */
