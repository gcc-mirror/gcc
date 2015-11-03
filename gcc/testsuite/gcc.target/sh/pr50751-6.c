/* Check that on SH2A the 4 byte mov.w displacement insn is generated to
   handle larger displacements.  If it is generated correctly, there should
   be no base address adjustments outside the mov.w insns.  */
/* { dg-do compile { target { sh2a } } }  */
/* { dg-options "-O1" } */
/* { dg-final { scan-assembler-not "add|sub" } } */

void
testfunc_00 (const short* ap, short* bp)
{
  bp[100] = ap[15];
  bp[200] = ap[50];
  bp[900] = ap[71];
  bp[0] = ap[25];
}

void
testfunc_01 (volatile const short* ap, volatile short* bp)
{
  bp[100] = ap[15];
  bp[200] = ap[50];
  bp[900] = ap[71];
  bp[0] = ap[25];
}

