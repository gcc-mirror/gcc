/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize -mavx2" { target avx2 } } */

int a[1024];

int
foo (int *p)
{
  int i;
  for (i = 0; i < 1024; i++)
    {
      int t;
      if (a[i] < 30)
	t = *p;
      else
	t = a[i] + 12;
      a[i] = t;
    }
}

