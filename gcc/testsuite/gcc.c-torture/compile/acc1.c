/* { dg-options "-O2 -ffast-math" } */

/* Fast maths allows tail recursion to be turned into iteration.  */

double
foo (int n, double f)
{
  if (n == 0)
    return f;
  else
    return f + foo (n - 1, f);
}

double
bar (int n, double f)
{
  if (n == 0)
    return f;
  else
    return f * bar (n - 1, f);
}
