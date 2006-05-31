/* { dg-do link } */
/* { dg-options "-O2 -ffast-math" } */

double cabs(__complex__ double);
float cabsf(__complex__ float);
long double cabsl(__complex__ long double);

void link_error (void);

void test(__complex__ double x)
{
  if (cabs(x) != cabs(-x))
    link_error();

  if (cabs(x) != cabs(~x))
    link_error();
}

void testf(__complex__ float x)
{
  if (cabsf(x) != cabsf(-x))
    link_error();

  if (cabsf(x) != cabsf(~x))
    link_error();
}

void testl(__complex__ long double x)
{
  if (cabsl(x) != cabsl(-x))
    link_error();

  if (cabsl(x) != cabsl(~x))
    link_error();
}

int main()
{
  test(0.0);
  testf(0.0);
  testl(0.0);
  return 0;
}

