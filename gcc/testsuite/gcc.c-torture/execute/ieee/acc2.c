/* Tail call optimizations would reverse the order of multiplications
   in func().  */

double func (const double *array)
{
  double d = *array;
  if (d == 1.0)
    return d;
  else
    return d * func (array + 1);
}

int main ()
{
  double values[] = { __DBL_MAX__, 2.0, 0.5, 1.0 };
  if (func (values) != __DBL_MAX__)
    abort ();
  exit (0);
}
