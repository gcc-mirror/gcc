/* Check that a comparison 'unsigned int <= 0x7FFFFFFF' results in code
   utilizing the cmp/pz instruction.  */
/* { dg-do compile }  */
/* { dg-options "-O1" } */
/* { dg-skip-if "" { "sh*-*-*" } { "-m5*"} { "" } }  */
/* { dg-final { scan-assembler-not "not\[ \t\]" } } */
/* { dg-final { scan-assembler-times "cmp/pz" 7 } } */
/* { dg-final { scan-assembler-times "shll" 1 } } */
/* { dg-final { scan-assembler-times "movt" 4 } } */

int
test_00 (unsigned int a)
{
  return !(a > 0x7FFFFFFF);
}

int
test_01 (unsigned int a)
{
  return !(a > 0x7FFFFFFF) ? -5 : 10;
}

int
test_02 (unsigned int a)
{
  /* 1x shll, 1x movt  */
  return a >= 0x80000000;
}

int
test_03 (unsigned int a)
{
  return a >= 0x80000000 ? -5 : 10;
}

int
test_04 (unsigned int a)
{
  return a <= 0x7FFFFFFF;
}

int
test_05 (unsigned int a)
{
  return a <= 0x7FFFFFFF ? -5 : 10;
}

int
test_06 (unsigned int a)
{
  return a < 0x80000000;
}

int
test_07 (unsigned int a)
{
  return a < 0x80000000 ? -5 : 10;
}
