double
foo (double x, double *cof)
{
  int i;
  double tmp, value;

  for (i = 10; i >= 0; i--)
    {
      value += cof[i] / tmp;
      tmp -= 1.0;
    }

  return value;
}
