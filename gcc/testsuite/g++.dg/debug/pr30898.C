// { dg-do compile }

double foo()
{
  union
  {
    int i;
    double d;
  };

  i = 0;
  return d;
}
