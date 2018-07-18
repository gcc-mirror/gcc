/* { dg-do compile } */
/* { dg-options "-O3 -fdump-tree-ifcvt" } */

int foo (unsigned short a[], unsigned int x)
{
  unsigned int i;
  for (i = 0; i < 1000; i++)
    {
      x = a[i];
      a[i] = (unsigned short)(x >= 255 ? 255 : x);
    }  return x;
}

/* { dg-final { scan-tree-dump " = MIN_EXPR <" "ifcvt" } } */
