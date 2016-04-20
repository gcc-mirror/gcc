/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize -fdump-tree-ifcvt-stats" { target *-*-* } } */

void foo (int a[], int b[])
{
  int i;
  for (i = 0; i < 100; i++)
    {
      if (a[i] == 0)
	a[i] = b[i]*4;
      else
	a[i] = b[i]*3;
    }
}

/* { dg-final { scan-tree-dump-times "Applying if-conversion" 1 "ifcvt" } } */
