/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-phiopt1" } */

int test_01 (int a)
{
  if (127 <= a)
    a = 127;
  else if (a <= -128)
    a = -128;
  return a;
}
int test_02 (int a)
{
  if (127 < a)
    a = 127;
  else if (a <= -128)
    a = -128;
  return a;
}
int test_03 (int a)
{
  if (127 <= a)
    a = 127;
  else if (a < -128)
    a = -128;
  return a;
}
int test_04 (int a)
{
  if (127 < a)
    a = 127;
  else if (a < -128)
    a = -128;
  return a;
}

/* { dg-final { scan-tree-dump-not "if" "phiopt1" } } */
