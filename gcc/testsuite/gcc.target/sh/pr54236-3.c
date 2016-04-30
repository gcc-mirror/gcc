/* Tests to check the utilization of the addc and subc instructions.
   If everything works as expected we won't see any movt instructions in
   these cases.  */
/* { dg-do compile }  */
/* { dg-options "-O2" } */
/* { dg-final { scan-assembler-times "addc" 4 } }  */
/* { dg-final { scan-assembler-times "subc" 5 } }  */
/* { dg-final { scan-assembler-times "movt" 1 } }  */
/* { dg-final { scan-assembler-times "sub\t" 1 } }  */
/* { dg-final { scan-assembler-times "neg\t" 2 } }  */

int
test_000 (int* x, unsigned int c)
{
  /* 1x addc  */
  int s = 0;
  unsigned int i;
  for (i = 0; i < c; ++i)
    s += ! (x[i] & 0x3000);
  return s;
}

int
test_001 (int* x, unsigned int c)
{
  /* 1x subc  */
  int s = 0;
  unsigned int i;
  for (i = 0; i < c; ++i)
    s -= ! (x[i] & 0x3000);
  return s;
}

int
test_002 (int a, int b, int c)
{
  /* 1x tst, 1x subc  */
  return ((a & b) != 0) - c;
}

int
test_003 (int a, int b, int c)
{
  /* 1x tst, 1x movt, 1x sub  */
  return ((a & b) == 0) - c;
}

int
test_004 (int a, int b, int c)
{
  /* 1x tst, 1x addc  */
  return c - ((a & b) != 0);
}

int
test_005 (int a, int b, int c)
{
  /* 1x shll, 1x subc  */
  int x = a < 0;
  return c - (b + x);
}

int
test_006 (int a, int b, int c)
{
  /* 1x neg, 1x cmp/pl, 1x addc  */
  int x = a > 0;
  int y = b + x;
  return y - c;
}

int
test_007 (int a, int b, int c)
{
  /* 1x add #-1, 1x cmp/eq, 1x addc  */
  int x = a != 1;
  int y = b - x;
  return c + y;
}

int
test_008 (int a, int b, int c)
{
  /* 1x neg, 1x cmp/gt, 1x subc  */
  int x = a > 1;
  int y = b - x;
  return c + y;
}

int
test_009 (int a, int b, int c, int d)
{
  /* 1x div0s, 1x subc  */
  return c - d - (a < 0 != b < 0);
}
