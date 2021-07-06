/* { dg-options "-O2 -fno-signed-zeros -fdump-tree-phiopt" } */

float f0(float A)
{
//     A == 0? A : -A    same as -A
  if (A == 0)  return A;
  return -A;
}

float f1(float A)
{
//     A != 0? A : -A    same as A
  if (A != 0)  return A;
  return -A;
}
float f2(float A)
{
//     A >= 0? A : -A    same as abs (A)
  if (A >= 0)  return A;
  return -A;
}
float f3(float A)
{
//     A > 0?  A : -A    same as abs (A)
  if (A > 0)  return A;
  return -A;
}
float f4(float A)
{
//     A <= 0? A : -A    same as -abs (A)
  if (A <= 0)  return A;
  return -A;
}
float f5(float A)
{
//     A < 0?  A : -A    same as -abs (A)
  if (A < 0)  return A;
  return -A;
}

/* These should be optimized in phiopt1 but is confused by predicts. */
/* { dg-final { scan-tree-dump-not "if" "phiopt1" { xfail *-*-* } } } */
/* { dg-final { scan-tree-dump-not "if" "phiopt2" } } */

