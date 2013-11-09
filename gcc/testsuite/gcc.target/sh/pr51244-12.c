/* Check that the negc instruction is generated as expected for the cases
   below.  If we see a movrt or #-1 negc sequence it means that the pattern
   which handles the inverted case does not work properly.  */
/* { dg-do compile }  */
/* { dg-options "-O1" } */
/* { dg-skip-if "" { "sh*-*-*" } { "-m5*" } { "" } } */
/* { dg-final { scan-assembler-times "negc" 10 } } */
/* { dg-final { scan-assembler-not "movrt|#-1|add|sub" } } */

int
test00 (int a, int b, int* x)
{
  return (a == b) ? 0x7FFFFFFF : 0x80000000;
}

int
test00_inv (int a, int b)
{
  return (a != b) ? 0x80000000 : 0x7FFFFFFF;
}

int
test01 (int a, int b)
{
  return (a >= b) ? 0x7FFFFFFF : 0x80000000;
}

int
test01_inv (int a, int b)
{
  return (a < b) ? 0x80000000 : 0x7FFFFFFF;
}

int
test02 (int a, int b)
{
  return (a > b) ? 0x7FFFFFFF : 0x80000000;
}

int
test02_inv (int a, int b)
{
  return (a <= b) ? 0x80000000 : 0x7FFFFFFF;
}

int
test03 (int a, int b)
{
  return ((a & b) == 0) ? 0x7FFFFFFF : 0x80000000;
}

int
test03_inv (int a, int b)
{
  return ((a & b) != 0) ? 0x80000000 : 0x7FFFFFFF;
}

int
test04 (int a)
{
  return ((a & 0x55) == 0) ? 0x7FFFFFFF : 0x80000000;
}

int
test04_inv (int a)
{
  return ((a & 0x55) != 0) ? 0x80000000 : 0x7FFFFFFF;
}
