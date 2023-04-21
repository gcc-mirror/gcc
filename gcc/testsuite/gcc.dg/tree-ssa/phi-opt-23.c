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

/* f4 and f5 are not allowed to be optimized in early phi-opt. */
/* { dg-final { scan-tree-dump-times "if" 2 "phiopt1" } } */
/* { dg-final { scan-tree-dump-not "if" "phiopt2" } } */

