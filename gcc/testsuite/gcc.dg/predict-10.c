/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-profile_estimate" } */
int
ee(int i)
{
  if (i>2)
    return (ee(i-1)+ee(i-2))/2;
  else
    return i;
}
/* { dg-final { scan-tree-dump-times "recursive call" 1 "profile_estimate"} } */
