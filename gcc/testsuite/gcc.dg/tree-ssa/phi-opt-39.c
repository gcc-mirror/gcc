/* { dg-options "-O2 -fdump-tree-phiopt" } */

unsigned f0(int A)
{
//     A == 0? A : -A    same as -A
  if (A == 0)  return A;
  return -A;
}

unsigned f1(int A)
{
//     A != 0? A : -A    same as A
  if (A != 0)  return A;
  return -A;
}
unsigned f2(int A)
{
//     A >= 0? A : -A    same as abs (A)
  if (A >= 0)  return A;
  return -A;
}
unsigned f3(int A)
{
//     A > 0?  A : -A    same as abs (A)
  if (A > 0)  return A;
  return -A;
}
unsigned f4(int A)
{
//     A <= 0? A : -A    same as -abs (A)
  if (A <= 0)  return A;
  return -A;
}
unsigned f5(int A)
{
//     A < 0?  A : -A    same as -abs (A)
  if (A < 0)  return A;
  return -A;
}

/* f4 and f5 are not allowed to be optimized in early phi-opt. */
/* { dg-final { scan-tree-dump-times "if" 2 "phiopt1" } } */
/* { dg-final { scan-tree-dump-not "if" "phiopt2" } } */
