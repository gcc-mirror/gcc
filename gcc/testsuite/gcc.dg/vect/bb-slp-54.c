/* { dg-do compile } */
/* { dg-require-effective-target vect_double } */

double a[2], b[2], c[2];

void foo(int flag)
{
  double tem1, tem2;
  if (flag)
    {
      tem1 = a[0];
      tem2 = a[1];
    }
  else
    {
      tem1 = b[0];
      tem2 = b[1];
    }
  c[0] = tem1;
  c[1] = tem2;
}

/* { dg-final { scan-tree-dump-times "transform load" 2 "slp2" } } */
