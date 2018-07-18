/* { dg-do compile } */
/* { dg-options "-O3 -fdump-tree-ifcvt" } */

int foo (short a[], int x)
{
  unsigned int i;
  for (i = 0; i < 1000; i++)
    {
      x = a[i];
      a[i] = (short)(x <= 0 ? 0 : x);
    }  return x;
}


/* { dg-final { scan-tree-dump " = MAX_EXPR <" "ifcvt" } } */
