/* { dg-do compile } */
/* { dg-options "-O1 -fdump-tree-optimized --param logical-op-non-short-circuit=0" } */

_Bool f1(_Bool a, _Bool b)
{
  if (a)
   {
     if (b)
      return 1;
     else
      return 0;
   }
  return 0;
}


/* There should be no if statements and be fold into just return a & b.
   This can be done without ifcombine but in phiopt where a ? b : 0 is
   converted into a & b. */
/* { dg-final { scan-tree-dump-not "if" "optimized" } } */
/* { dg-final { scan-tree-dump-times " & " 1 "optimized" } } */
