/* { dg-do compile } */
/* { dg-options "-O2 -fno-strict-aliasing -fdump-tree-optimized" } */
/* { dg-require-effective-target label_values } */

void foo (int b)
{
  void *p;
lab:
  if (b)
    p = &&lab;
  else
    {
lab2:
      p = &&lab2;
    }
  *(char *)p = 1;
}

/* We should keep the store to the label locations.  */
/* { dg-final { scan-tree-dump " = 1;" "optimized" } } */
