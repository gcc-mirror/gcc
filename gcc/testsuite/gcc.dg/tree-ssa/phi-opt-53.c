/* { dg-options "-O2 -fno-signed-zeros -fno-trapping-math -fdump-tree-phiopt" } */

/* Swapped arms of phi-opt-24.c.  A < 0 ? -A : A is abs (A).
   ABS does not trap, so -fno-trapping-math is required while NaNs are
   honored.  A floating compare is not inverted into A >= 0 ? A : -A,
   so these folds need their own patterns.  */

float f0(float A)
{
//     A < 0 ? -A : A    same as abs (A)
  if (A < 0)  return -A;
  return A;
}
float f1(float A)
{
//     A <= 0 ? -A : A    same as abs (A)
  if (A <= 0)  return -A;
  return A;
}
float f2(float A)
{
//     A > 0 ? -A : A    same as -abs (A)
  if (A > 0)  return -A;
  return A;
}
float f3(float A)
{
//     A >= 0 ? -A : A    same as -abs (A)
  if (A >= 0)  return -A;
  return A;
}

/* f2 and f3 are -abs, not allowed in early phi-opt. */
/* { dg-final { scan-tree-dump-times "if" 2 "phiopt1" } } */
/* { dg-final { scan-tree-dump-not "if" "phiopt2" } } */
