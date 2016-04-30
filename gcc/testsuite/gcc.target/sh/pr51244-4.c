/* Check that storing the (negated) T bit as all ones or zeros in a reg
   uses the subc instruction.  */
/* { dg-do compile }  */
/* { dg-options "-O1 -mbranch-cost=2" } */
/* { dg-final { scan-assembler-not "movt|tst|negc|movrt" } } */
/* { dg-final { scan-assembler-times "subc" 3 } }  */
/* { dg-final { scan-assembler-times "not\t" 1 } }  */
/* { dg-final { scan-assembler-times "shll" 1 } }  */
/* { dg-final { scan-assembler-not "cmp/gt" } }  */

int
test_00 (int x, int y)
{
  /* 1x subc, 1x not  */
  return x != y ? -1 : 0;
}

int
test_01 (int x, int y)
{
  /* 1x subc  */
  return x == y ? -1 : 0;
}

int
test_02 (int x)
{
  /* 1x shll, 1x subc  */
  return 0 <= x ? 0 : -1;
}
