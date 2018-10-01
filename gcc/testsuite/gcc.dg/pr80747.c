/* PR rtl-optimization/80747 */
/* { dg-do compile } */
/* { dg-options "-fprofile-use -freorder-blocks-and-partition -O1 -foptimize-sibling-calls -Wno-missing-profile" } */

int
foo (int a)
{
  int r;
  if (a & 1)
    r = foo (a - 1);
  else if (a)
    r = foo (a - 2);
  else
    return 0;
  if (r)
    r = r;
  return r;
}
