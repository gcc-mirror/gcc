/* Check that the mov.b displacement addressing insn is generated.
   If the insn is generated as expected, there should be no address 
   calculations outside the mov insns.  */
/* { dg-do compile }  */
/* { dg-options "-O1" } */
/* { dg-skip-if "" { "sh*-*-*" } { "-m5*"} { "" } } */
/* { dg-final { scan-assembler-not "add|sub" } } */

void
testfunc_00 (const char* ap, char* bp, char val)
{
  bp[0] = ap[15];
  bp[2] = ap[5];
  bp[9] = ap[7];
  bp[0] = ap[15];
  bp[4] = val;
  bp[14] = val;
}

void
testfunc_01 (volatile const char* ap, volatile char* bp, char val)
{
  bp[0] = ap[15];
  bp[2] = ap[5];
  bp[9] = ap[7];
  bp[0] = ap[15];
  bp[4] = val;
  bp[14] = val;
}

