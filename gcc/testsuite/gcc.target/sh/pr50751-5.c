/* Check that the mov.w displacement addressing insn is generated and the 
   base address is adjusted only once.  On SH2A this test is skipped because
   there is a 4 byte mov.w insn that can handle larger displacements.  Thus
   on SH2A the base address will not be adjusted in this case.  */
/* { dg-do compile { target { ! sh2a } } }  */
/* { dg-options "-O1" } */
/* { dg-final { scan-assembler-times "add" 2 } } */

void
testfunc_00 (const short* ap, short* bp)
{
  bp[0] = ap[15];
  bp[2] = ap[5];
  bp[9] = ap[7];
  bp[0] = ap[25];
}

void
testfunc_01 (volatile const short* ap, volatile short* bp)
{
  bp[0] = ap[15];
  bp[2] = ap[5];
  bp[9] = ap[7];
  bp[0] = ap[25];
}

