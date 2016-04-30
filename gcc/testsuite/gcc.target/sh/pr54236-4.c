/* Check that addc and subc patterns are converted if the T value is
   inverted.  */
/* { dg-do compile }  */
/* { dg-options "-O2" }  */

/* { dg-final { scan-assembler-times "cmp/eq" 7 } }  */

/* { dg-final { scan-assembler-times "subc" 5 { target { ! sh2a } } } }  */
/* { dg-final { scan-assembler-times "not\t" 3 { target { ! sh2a } } } }  */
/* { dg-final { scan-assembler-times "addc" 2 { target { ! sh2a } } } }  */

/* { dg-final { scan-assembler-times "subc" 2 { target { sh2a } } } }  */
/* { dg-final { scan-assembler-times "addc" 5 { target { sh2a } } } }  */
/* { dg-final { scan-assembler-times "nott" 3 { target { sh2a } } } }  */

/* { dg-final { scan-assembler-not "movt" } }  */
/* { dg-final { scan-assembler-not "negc" } }  */
/* { dg-final { scan-assembler-not "movrt" } }  */

int
test_0 (int a, int b, int c)
{
  /* 1x cmp/eq, 1x subc  */
  return c + (a != b);
}

int
test_1 (int a, int b, int c, int d)
{
  /* 1x cmp/eq, 1x not, 1x subc
     SH2A: 1x cmp/eq, 1x nott, 1x addc  */
  return (a != b) + c + d;
}

int
test_2 (int a, int b, int c, int d)
{
  /* 1x cmp/eq, 1x not, 1x subc
     SH2A: 1x cmp/eq, 1x nott, 1x addc  */
  return c + (a != b) + d;
}

int
test_3 (int a, int b, int c, int d)
{
  /* 1x cmp/eq, 1x not, 1x subc
     SH2A: 1x cmp/eq, 1x nott, 1x addc  */
  return c + d + (a != b);
}

int
test_4 (int a, int b, int c, int d)
{
  /* 1x cmp/eq, 1x subc  */
  return (a != b) - c;
}

int
test_5 (int a, int b, int c, int d)
{
  /* 1x cmp/eq, 1x add #-1, 1x addc  */
  return c - (a != b);
}

int
test_6 (int a, int b, int c, int d)
{
  /* 1x cmp/eq, 1x add #-1, 1x addc  */
  return c - (a != b) + d;
}
