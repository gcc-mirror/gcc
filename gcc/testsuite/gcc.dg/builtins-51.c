/* { dg-do run } */
/* { dg-options "-O2 -ffast-math" } */

extern double pow(double, double);
extern double fabs(double);
extern void abort(void);

double test2_1(double x)
{
  return pow(x,2.0);
}

double test2_2(double x)
{
  return pow(-x,2.0);
}

double test2_3(double x)
{
  return pow(fabs(x),2.0);
}

double test3_1(double x)
{
  return pow(x,3.0);
}

double test3_2(double x)
{
  return pow(-x,3.0);
}

double test3_3(double x)
{
  return pow(fabs(x),3.0);
}

double test6_1(double x)
{
  return pow(x,6.0);
}

double test6_2(double x)
{
  return pow(-x,6.0);
}

double test6_3(double x)
{
  return pow(fabs(x),6.0);
}


int main()
{
  if (test2_1(1.0) != 1.0)
    abort();
  if (test2_1(2.0) != 4.0)
    abort();
  if (test2_1(0.0) != 0.0)
    abort();
  if (test2_1(-1.0) != 1.0)
    abort();
  if (test2_1(-2.0) != 4.0)
    abort();

  if (test2_2(1.0) != 1.0)
    abort();
  if (test2_2(2.0) != 4.0)
    abort();
  if (test2_2(0.0) != 0.0)
    abort();
  if (test2_2(-1.0) != 1.0)
    abort();
  if (test2_2(-2.0) != 4.0)
    abort();

  if (test2_3(1.0) != 1.0)
    abort();
  if (test2_3(2.0) != 4.0)
    abort();
  if (test2_3(0.0) != 0.0)
    abort();
  if (test2_3(-1.0) != 1.0)
    abort();
  if (test2_3(2.0) != 4.0)
    abort();

  if (test3_1(1.0) != 1.0)
    abort();
  if (test3_1(2.0) != 8.0)
    abort();
  if (test3_1(0.0) != 0.0)
    abort();
  if (test3_1(-1.0) != -1.0)
    abort();
  if (test3_1(-2.0) != -8.0)
    abort();

  if (test3_2(1.0) != -1.0)
    abort();
  if (test3_2(2.0) != -8.0)
    abort();
  if (test3_2(0.0) != -0.0)
    abort();
  if (test3_2(-1.0) != 1.0)
    abort();
  if (test3_2(-2.0) != 8.0)
    abort();

  if (test3_3(1.0) != 1.0)
    abort();
  if (test3_3(2.0) != 8.0)
    abort();
  if (test3_3(0.0) != 0.0)
    abort();
  if (test3_3(-1.0) != 1.0)
    abort();
  if (test3_3(-2.0) != 8.0)
    abort();

  if (test6_1(1.0) != 1.0)
    abort();
  if (test6_1(2.0) != 64.0)
    abort();
  if (test6_1(0.0) != 0.0)
    abort();
  if (test6_1(-1.0) != 1.0)
    abort();
  if (test6_1(-2.0) != 64.0)
    abort();

  if (test6_2(1.0) != 1.0)
    abort();
  if (test6_2(2.0) != 64.0)
    abort();
  if (test6_2(0.0) != 0.0)
    abort();
  if (test6_2(-1.0) != 1.0)
    abort();
  if (test6_2(-2.0) != 64.0)
    abort();

  if (test6_3(1.0) != 1.0)
    abort();
  if (test6_3(2.0) != 64.0)
    abort();
  if (test6_3(0.0) != 0.0)
    abort();
  if (test6_3(-1.0) != 1.0)
    abort();
  if (test6_3(-2.0) != 64.0)
    abort();

  return 0;
}

