double
f(double x)
{
  return x*x;
}

double
Int(double (*f)(double), double a)
{
  return (*f)(a);
}

main()
{
  if (Int(&f,2.0) != 4.0)
    abort();
  exit (0);
}
