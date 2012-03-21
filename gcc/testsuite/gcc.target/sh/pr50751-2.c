/* Check that the mov.b displacement addressing insn is generated and the 
   base address is adjusted only once.  On SH2A this test is skipped because
   there is a 4 byte mov.b insn that can handle larger displacements.  Thus
   on SH2A the base address will not be adjusted in this case.  */
/* { dg-do compile { target "sh*-*-*" } } */
/* { dg-options "-O1" } */
/* { dg-skip-if "" { "sh*-*-*" } { "-m5*" "-m2a*" } { "" } } */
/* { dg-final { scan-assembler-times "add" 2 } } */

void
testfunc_00 (const char* ap, char* bp)
{
  bp[0] = ap[15];
  bp[2] = ap[5];
  bp[9] = ap[7];
  bp[0] = ap[25];
}

void
testfunc_01 (volatile const char* ap, volatile char* bp)
{
  bp[0] = ap[15];
  bp[2] = ap[5];
  bp[9] = ap[7];
  bp[0] = ap[25];
}

