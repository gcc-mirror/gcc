/* { dg-do compile } */
/* { dg-options "-O3 -fdump-tree-ifcvt" } */

void foo (long *a)
{
  int i;
  for (i = 0; i < 100; i+=2)
    {
      long *p = &a[i+1];
      if (a[i] == 0)
	{
	  *p = 2;
	  a[i] = 3;
	}
      else
	{
	  *p = 3;
	  a[i] = 4;
	}
    }
}

/* { dg-final { scan-tree-dump "Applying if-conversion" "ifcvt" } } */
