/* With -fwrapv, signed overflow is defined, so a range is required.  */
/* { dg-do compile } */
/* { dg-options "-O2 -fwrapv -fdump-tree-optimized" } */
/* { dg-require-effective-target int32plus } */

/* Unknown range: t * 19 may wrap, no transformation.  */
int
no_range_signed (int t)
{
  return t * 19 / 18;
}

/* Known range: transformed.  */
int
range_signed (short b)
{
  int t = b;
  return t * 11 / 10;
}

/* { dg-final { scan-tree-dump "\\* 19;" "optimized" } } */
/* { dg-final { scan-tree-dump-not "\\* 11;" "optimized" } } */
/* { dg-final { scan-tree-dump " / 10;" "optimized" } } */
