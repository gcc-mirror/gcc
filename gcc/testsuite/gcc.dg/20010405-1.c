__inline__ double bar(double *x)
{
  static double a;
  a = *x >= 0 ? *x : -*x;
  return a;
}

int main(void)
{
  extern double bar();
  double a;
  static double b;
  int r;
  for (r = 1; r < 3; r++) {
    a = 1.0;
    b = bar(&a);
  }
  return 0;
}
