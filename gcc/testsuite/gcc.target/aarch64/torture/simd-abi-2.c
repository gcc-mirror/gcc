/* { dg-do compile } */

void
f (void)
{
  /* Clobber all fp/simd regs and verify that the correct ones are saved
     and restored in the prologue and epilogue of a normal non-SIMD function. */
  __asm__ __volatile__ ("" :::  "q0",  "q1",  "q2",  "q3");
  __asm__ __volatile__ ("" :::  "q4",  "q5",  "q6",  "q7");
  __asm__ __volatile__ ("" :::  "q8",  "q9", "q10", "q11");
  __asm__ __volatile__ ("" ::: "q12", "q13", "q14", "q15");
  __asm__ __volatile__ ("" ::: "q16", "q17", "q18", "q19");
  __asm__ __volatile__ ("" ::: "q20", "q21", "q22", "q23");
  __asm__ __volatile__ ("" ::: "q24", "q25", "q26", "q27");
  __asm__ __volatile__ ("" ::: "q28", "q29", "q30", "q31");
}

/* { dg-final { scan-assembler {\sstp\td8, d9} } } */
/* { dg-final { scan-assembler {\sstp\td10, d11} } } */
/* { dg-final { scan-assembler {\sstp\td12, d13} } } */
/* { dg-final { scan-assembler {\sstp\td14, d15} } } */
/* { dg-final { scan-assembler {\sldp\td8, d9} } } */
/* { dg-final { scan-assembler {\sldp\td10, d11} } } */
/* { dg-final { scan-assembler {\sldp\td12, d13} } } */
/* { dg-final { scan-assembler {\sldp\td14, d15} } } */
/* { dg-final { scan-assembler-not {\sstp\tq} } } */
/* { dg-final { scan-assembler-not {\sldp\tq} } } */
/* { dg-final { scan-assembler-not {\sstp\tq[01234567]} } } */
/* { dg-final { scan-assembler-not {\sldp\tq[01234567]} } } */
/* { dg-final { scan-assembler-not {\sstp\tq1[6789]} } } */
/* { dg-final { scan-assembler-not {\sldp\tq1[6789]} } } */
/* { dg-final { scan-assembler-not {\sstr\t} } } */
/* { dg-final { scan-assembler-not {\sldr\t} } } */
