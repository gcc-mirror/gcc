/* { dg-do compile } */
/* { dg-options "-O2 -Wall" } */
double frexp (double, int*);
double modf (double, double*);
double remquo (double, double, int*);

int f (void)
{
  int y;
  frexp (1.0, &y);
  return y;
}

double g (void)
{
  double y;
  modf (1.0, &y);
  return y;
}

int h (void)
{
  int y;
  remquo (1.0, 1.0, &y);
  return y;
}

