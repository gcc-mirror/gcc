/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-ifcombine-details" } */

/* Looking through the narrowing conversion of the tested name is valid
   even with non-constant masks.  */

int
f1 (long x, int m)
{
  int t = (int) x;
  if ((t & m) == 0 && (x & 4) == 0)
    return 1;
  return 0;
}

int
f2 (long x, int m, long m2)
{
  int t = (int) x;
  if ((t & m) == 0 && (x & m2) == 0)
    return 1;
  return 0;
}

/* { dg-final { scan-tree-dump-times "optimizing bits or bits test" 2 "ifcombine" } } */
