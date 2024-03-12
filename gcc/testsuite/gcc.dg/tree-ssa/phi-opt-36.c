/* { dg-options "-O2 -fdump-tree-phiopt" } */

unsigned f0(int A)
{
  unsigned t = A;
//     A == 0? A : -A    same as -A
  if (A == 0)  return t;
  return -t;
}

unsigned f1(int A)
{
  unsigned t = A;
//     A != 0? A : -A    same as A
  if (A != 0)  return t;
  return -t;
}
unsigned f2(int A)
{
  unsigned t = A;
//     A >= 0? A : -A    same as abs (A)
  if (A >= 0)  return t;
  return -t;
}
unsigned f3(int A)
{
  unsigned t = A;
//     A > 0?  A : -A    same as abs (A)
  if (A > 0)  return t;
  return -t;
}
unsigned f4(int A)
{
  unsigned t = A;
//     A <= 0? A : -A    same as -abs (A)
  if (A <= 0)  return t;
  return -t;
}
unsigned f5(int A)
{
  unsigned t = A;
//     A < 0?  A : -A    same as -abs (A)
  if (A < 0)  return t;
  return -t;
}

/* f4 and f5 are not allowed to be optimized in early phi-opt. */
/* { dg-final { scan-tree-dump-times "if " 2 "phiopt1" } } */
/* { dg-final { scan-tree-dump-not "if " "phiopt2" } } */


