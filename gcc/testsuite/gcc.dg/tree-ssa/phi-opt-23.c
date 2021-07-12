/* { dg-options "-O2 -fdump-tree-phiopt" } */

int f0(int A)
{
//     A == 0? A : -A    same as -A
  if (A == 0)  return A;
  return -A;
}

int f1(int A)
{
//     A != 0? A : -A    same as A
  if (A != 0)  return A;
  return -A;
}
int f2(int A)
{
//     A >= 0? A : -A    same as abs (A)
  if (A >= 0)  return A;
  return -A;
}
int f3(int A)
{
//     A > 0?  A : -A    same as abs (A)
  if (A > 0)  return A;
  return -A;
}
int f4(int A)
{
//     A <= 0? A : -A    same as -abs (A)
  if (A <= 0)  return A;
  return -A;
}
int f5(int A)
{
//     A < 0?  A : -A    same as -abs (A)
  if (A < 0)  return A;
  return -A;
}

/* These should be optimized in phiopt1 but is confused by predicts. */
/* { dg-final { scan-tree-dump-not "if" "phiopt1" { xfail *-*-* } } } */
/* { dg-final { scan-tree-dump-not "if" "phiopt2" } } */

