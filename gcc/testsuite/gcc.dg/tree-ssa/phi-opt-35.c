/* PR tree-optimization/106164 */
/* { dg-do compile } */
/* { dg-options "-O1 -fdump-tree-phiopt2" } */

_Bool f2(int a)
{
  if (a != 2)
    return a > 1;
  return 0;
}
/* phiopt2 should be able to optimize this to `a > 2` via match and simplify */
/* { dg-final { scan-tree-dump "_\[0-9\]+ = a_\[0-9\]+.D. > 2" "phiopt2" } }  */
/* { dg-final { scan-tree-dump-not "a_\[0-9\]+.D. != 2" "phiopt2" } }  */
