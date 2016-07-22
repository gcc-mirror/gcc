/* Check that the negc instruction is generated as expected for the cases
   below.  If we see a movrt or #-1 negc sequence it means that the pattern
   which handles the inverted case does not work properly.  */
/* { dg-do compile }  */
/* { dg-options "-O1" } */

/* { dg-final { scan-assembler-times "negc" 15 { target { ! sh2a } } } } */
/* { dg-final { scan-assembler-times "addc" 3 { target { ! sh2a } } } } */

/* { dg-final { scan-assembler-times "negc" 13 { target { sh2a } } } } */
/* { dg-final { scan-assembler-times "addc" 5 { target { sh2a } } } } */
/* { dg-final { scan-assembler-times "bld" 2 { target { sh2a } } } } */

/* { dg-final { scan-assembler-not "movrt|#-1|add\t|sub\t|movt" } } */

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

int
test05 (int a, int b)
{
  /* 1x addc  */
  return a != b ? 0x7FFFFFFF : 0x80000000;
}

int
test06 (char a)
{
  return ((a & 0x03) == 0) ? 0x7FFFFFFF : 0x80000000;
}

int
test07 (char a)
{
  return ((a & 0x80) == 0) ? 0x7FFFFFFF : 0x80000000;
}

int
test08 (char a)
{
  return ((a & 1) == 0) ? 0x7FFFFFFF : 0x80000000;
}

int
test09 (int a)
{
  /* 1x cmp/pz, 1x addc  */
  return a < 0 ? 0x7FFFFFFF : 0x80000000;
}

int
test10 (int a)
{
  /* 1x cmp/pz, 1x negc  */
  return a >= 0 ? 0x7FFFFFFF : 0x80000000;
}

int
test11 (int a)
{
  /* 1x cmp/pl, 1x negc  */
  return a > 0 ? 0x7FFFFFFF : 0x80000000;
}

int
test12 (int a)
{
  /* 1x cmp/pl, 1x addc  */
  return a <= 0 ? 0x7FFFFFFF : 0x80000000;
}
