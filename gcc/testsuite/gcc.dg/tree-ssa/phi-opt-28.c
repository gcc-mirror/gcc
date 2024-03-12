/* { dg-do compile } */
/* { dg-options "-O1 -fdump-tree-optimized" } */

_Bool f1(_Bool a, _Bool b)
{
  if (a)
    return 1;
  return b;
}


/* There should be no if statements and be fold into just return a & b. */
/* { dg-final { scan-tree-dump-not "if" "optimized" } } */
/* { dg-final { scan-tree-dump-times " \\\| " 1 "optimized" } } */
