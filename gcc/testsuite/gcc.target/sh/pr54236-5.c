/* Check that addc and subc instructions are generated as expected in
   combination with ifcvt.  */
/* { dg-do compile }  */
/* { dg-options "-O2" }  */

/* { dg-final { scan-assembler-times "subc" 4 { target { ! sh2a } } } }  */
/* { dg-final { scan-assembler-times "addc" 4 { target { ! sh2a } } } }  */
/* { dg-final { scan-assembler-times "not\t" 0 { target { ! sh2a } } } }  */

/* { dg-final { scan-assembler-times "subc" 4 { target { sh2a } } } }  */
/* { dg-final { scan-assembler-times "addc" 4 { target { sh2a } } } }  */
/* { dg-final { scan-assembler-times "nott" 0 { target { sh2a } } } }  */

/* { dg-final { scan-assembler-times "tst\t" 4 } }  */
/* { dg-final { scan-assembler-times "cmp/eq" 1 } }  */
/* { dg-final { scan-assembler-times "cmp/pl" 2 } }  */
/* { dg-final { scan-assembler-times "cmp/gt" 1 } }  */

/* { dg-final { scan-assembler-not "movt" } }  */
/* { dg-final { scan-assembler-not "negc" } }  */
/* { dg-final { scan-assembler-not "movrt" } }  */

int
test_00 (int x, int y)
{
  /* 1x tst, 1x subc  */
  if (y)
    ++x;
  return x;
}

int
test_01 (int x, int y)
{
  /* 1x tst, 1x addc  */
  if (y)
    --x;
  return x;
}

int
test_02 (int x, int y)
{
  /* 1x tst, 1x addc  */
  if (!y)
    ++x;
  return x;
}

int
test_03 (int x, int y)
{
  /* 1x tst, 1x subc  */
  if (!y)
    --x;
  return x;
}

int
test_04 (int x, int y)
{
  /* 1x cmp/eq, 1x addc  */
  if (y == x)
    ++x;
  return x;
}

int
test_05 (int x, int y)
{
  /* 1x cmp/gt, 1x subc  */
  if (y < 4)
    ++x;
  return x;
}

int
test_06 (int x)
{
  /* 1x cmp/pl, 1x addc  */
  return x > 0 ? x + 1 : x;
}

int
test_07 (int x)
{
  /* 1x cmp/pl, 1x subc  */
  return x > 0 ? x - 1 : x;
}
