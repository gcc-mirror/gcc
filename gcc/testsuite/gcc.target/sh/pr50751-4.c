/* Check that the mov.w displacement addressing insn is generated.
   If the insn is generated as expected, there should be no address 
   calculations outside the mov insns.  */
/* { dg-do compile }  */
/* { dg-options "-O1" } */
/* { dg-skip-if "" { "sh*-*-*" } { "-m5*"} { "" } } */
/* { dg-final { scan-assembler-not "add|sub" } } */

void
testfunc_00 (const short* ap, short* bp, short val)
{
  bp[0] = ap[15];
  bp[2] = ap[5];
  bp[9] = ap[7];
  bp[0] = ap[15];
  bp[4] = val;
  bp[14] = val;
}

void
testfunc_01 (volatile const short* ap, volatile short* bp, short val)
{
  bp[0] = ap[15];
  bp[2] = ap[5];
  bp[9] = ap[7];
  bp[0] = ap[15];
  bp[4] = val;
  bp[14] = val;
}

