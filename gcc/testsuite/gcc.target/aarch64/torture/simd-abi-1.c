/* { dg-do compile } */

void __attribute__ ((aarch64_vector_pcs))
f (void)
{
  /* Clobber all fp/simd regs and verify that the correct ones are saved
     and restored in the prologue and epilogue of a SIMD function. */
  __asm__ __volatile__ ("" :::  "q0",  "q1",  "q2",  "q3");
  __asm__ __volatile__ ("" :::  "q4",  "q5",  "q6",  "q7");
  __asm__ __volatile__ ("" :::  "q8",  "q9", "q10", "q11");
  __asm__ __volatile__ ("" ::: "q12", "q13", "q14", "q15");
  __asm__ __volatile__ ("" ::: "q16", "q17", "q18", "q19");
  __asm__ __volatile__ ("" ::: "q20", "q21", "q22", "q23");
  __asm__ __volatile__ ("" ::: "q24", "q25", "q26", "q27");
  __asm__ __volatile__ ("" ::: "q28", "q29", "q30", "q31");
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
