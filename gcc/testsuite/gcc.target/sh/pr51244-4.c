/* Check that storing the (negated) T bit as all ones or zeros in a reg
   uses the subc instruction.  On SH2A a sequence with the movrt instruction
   is also OK instead of subc.  */
/* { dg-do compile }  */
/* { dg-options "-O1 -mbranch-cost=2" } */
/* { dg-skip-if "" { "sh*-*-*" } { "-m5*"} { "" } } */
/* { dg-final { scan-assembler-not "movt|tst|negc" } } */
/* { dg-final { scan-assembler "subc|movrt|neg|not" } } */

int test_00 (int x, int y)
{
  return x != y ? -1 : 0;
}

int test_01 (int x, int y)
{
  return x == y ? -1 : 0;
}

