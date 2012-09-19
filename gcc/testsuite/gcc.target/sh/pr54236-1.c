/* Tests to check the utilization of addc, subc and negc instructions in
   special cases.  If everything works as expected we won't see any
   movt instructions in these cases.  */
/* { dg-do compile { target "sh*-*-*" } } */
/* { dg-options "-O1" } */
/* { dg-skip-if "" { "sh*-*-*" } { "-m5*"} { "" } } */
/* { dg-final { scan-assembler-times "addc" 4 } } */
/* { dg-final { scan-assembler-times "subc" 3 } } */
/* { dg-final { scan-assembler-times "sett" 5 } } */
/* { dg-final { scan-assembler-times "negc" 1 } } */
/* { dg-final { scan-assembler-not "movt" } } */

int
test_00 (int a, int b, int c, int d)
{
  /* 1x addc, 1x sett  */
  return a + b + 1;
}

int
test_01 (int a, int b, int c, int d)
{
  /* 1x addc  */
  return a + (c == d);
}

int
test_02 (int a, int b, int c, int d)
{
  /* 1x subc, 1x sett  */
  return a - b - 1;
}

int
test_03 (int a, int b, int c, int d)
{
  /* 1x subc  */
  return a - (c == d);
}

int
test_04 (int a, int b, int c, int d)
{
  /* 1x addc, 1x sett  */
  return a + b + c + 1;
}

int
test_05 (int a, int b, int c, int d)
{
  /* 1x subc, 1x sett  */
  return a - b - c - 1;
}

int
test_06 (int a, int b, int c, int d)
{
  /* 1x negc  */
  return 0 - a - (b == c);
}

int
test_07 (int *vec)
{
  /* Must not see a 'sett' or 'addc' here.
     This is a case where combine tries to produce
     'a + (0 - b) + 1' out of 'a - b + 1'.  */
  int z = vec[0];
  int vi = vec[1];
  int zi = vec[2];

  if (zi != 0 && z < -1)
    vi -= (((vi >> 7) & 0x01) << 1) - 1;

  return vi;
}

int
test_08 (int a)
{
  /* 1x addc, 1x sett  */
  return (a << 1) + 1;
}
