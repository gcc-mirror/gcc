/* { dg-do compile } */
/* { dg-options "-Ofast -fnon-call-exceptions -fdump-tree-recip" } */

double res, res2, tmp;
void
foo1 (double a, double b)
{
  try {
    tmp = 1.0 / __builtin_sqrt (a);
    res = tmp * tmp;
    res2 = a * tmp;
  }
  catch (...)
    { ; }
}

void
foo4 (double a, double b, int c, int d)
{
  try {
    tmp = 1.0 / __builtin_sqrt (a);
  }
  catch (...)
    {
      if (c)
	res = tmp * tmp;

      if (d)
	res2 = a * tmp;
    }
}

void
foo5 (double a, double b, int c, int d)
{
  try {
    tmp = 1.0 / __builtin_sqrt (a);
    res = tmp * tmp;

    if (d)
      res2 = a * tmp;
  }
  catch (...)
    { ; }
}

/* { dg-final { scan-tree-dump-times "Optimizing reciprocal sqrt multiplications" 2 "recip" } } */
/* { dg-final { scan-tree-dump-times "Replacing squaring multiplication" 2 "recip" } } */
/* { dg-final { scan-tree-dump-times "Replacing original division" 2 "recip" } } */
