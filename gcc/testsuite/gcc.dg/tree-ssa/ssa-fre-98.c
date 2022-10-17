/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-fre1-details" } */

int foo (int a, int s, unsigned int k)
{
  int i = a, j = a;
  do
    {
      i += s;
      j += j;
      j -= a;
    }
  while (k--);
  return j+i;
}

/* We want the redundant PHI for j to disappear.  */
/* { dg-final { scan-tree-dump "Replaced redundant PHI node defining j" "fre1" } } */
