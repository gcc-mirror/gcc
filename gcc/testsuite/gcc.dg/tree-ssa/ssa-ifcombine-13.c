/* { dg-do compile } */
/* { dg-options "-O1 -fdump-tree-optimized-details-blocks --param logical-op-non-short-circuit=1" } */

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


/* For LOGICAL_OP_NON_SHORT_CIRCUIT, this should be optimized
   into return a & b;, with no ifs.  */
/* { dg-final { scan-tree-dump-not "if" "optimized" } } */
/* { dg-final { scan-tree-dump-not "Invalid sum" "optimized" } } */
