/* { dg-do compile } */
/* { dg-options "-O -fno-trapping-math -fdump-tree-ifcombine-details-blocks" } */

double test1 (double i, double j)
{
  if (i >= j)
    if (i <= j)
      goto plif;
    else
      goto plouf;
  else
    goto plif;

plif:
  return 0;
plouf:
  return -1;
}

/* The above should be optimized to a i > j test by ifcombine.
   The transformation would also be legal with -ftrapping-math.
   Instead we get u<=, which is acceptable with -fno-trapping-math.  */

/* { dg-final { scan-tree-dump " u<= " "ifcombine" } } */
/* { dg-final { scan-tree-dump-not "Invalid sum" "ifcombine" } } */
