/* setup_incoming_promotions should detect x to be already sign-extended due
   to PROMOTE_MODE.  Thus the truncation should be removed by combine.  Based
   on gcc.c-torture/execute/pr34070-2.c.  */
/* { dg-options "-mgp64" } */
/* { dg-skip-if "code quality test" { *-*-* } { "-O0" } { "" } } */
/* { dg-final { scan-assembler-not "\tsll\t\[^\n\]*,0" } } */

NOMIPS16 int f(unsigned int x, int n, int *p)
{
  if (p)
    *p = 1;
  return ((int)x) / (1 << n);
}
