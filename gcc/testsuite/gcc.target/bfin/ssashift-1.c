/* { dg-do compile } */
/* { dg-options "-O2" } */

int test_ok_pos()
{
  int x = 100;
  return __builtin_bfin_shl_fr1x32(x,24);
}

int test_ok_neg()
{
  int x = -100;
  return __builtin_bfin_shl_fr1x32(x,24);
}

int test_sat_max()
{
  int x = 10000;
  return __builtin_bfin_shl_fr1x32(x,24);
}

int test_sat_min()
{
  int x = -10000;
  return __builtin_bfin_shl_fr1x32(x,24);
}

short stest_ok_pos()
{
  short x = 100;
  return __builtin_bfin_shl_fr1x16(x,8);
}

short stest_ok_neg()
{
  short x = -100;
  return __builtin_bfin_shl_fr1x16(x,8);
}

short stest_sat_max()
{
  short x = 10000;
  return __builtin_bfin_shl_fr1x16(x,8);
}

short stest_sat_min()
{
  short x = -10000;
  return __builtin_bfin_shl_fr1x16(x,8);
}
/* { dg-final { scan-assembler-not "\\(S\\)" } } */
/* { dg-final { scan-assembler-not "\\(V,S\\)" } } */
