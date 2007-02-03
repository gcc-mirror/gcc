/* { dg-do link } */
/* { dg-options "-O2 -ffast-math" } */

double fabs(double);
float fabsf(float);
long double fabsl(long double);
double cabs(__complex__ double);
float cabsf(__complex__ float);
long double cabsl(__complex__ long double);

void link_error (void);

void test(__complex__ double x, double a, double b)
{
  if (cabs(x) != cabs(-x))
    link_error();

  if (cabs(x) != cabs(~x))
    link_error();

  if (fabs(a) * __builtin_sqrt(2) != cabs (a+a*1i))
    link_error();

  if (fabs(a) * __builtin_sqrt(2) != cabs (a*1i+a))
    link_error();

  if (fabs(a) * __builtin_sqrt(2) != cabs (-a+a*-1i))
    link_error();

  if (fabs(a) * __builtin_sqrt(2) != cabs (-a+-a*1i))
    link_error();

  if (fabs(a) * __builtin_sqrt(2) != cabs (-a-a*1i))
    link_error();

  if (fabs(a) * __builtin_sqrt(2) != cabs (a*-1i-a))
    link_error();

  if (fabs(a) * __builtin_sqrt(2) != cabs (-a*1i-a))
    link_error();

  if (fabs(a) * __builtin_sqrt(2) != cabs (a*-1i+-a))
    link_error();

  if (fabs(a) * __builtin_sqrt(2) != cabs (-a*1i+-a))
    link_error();

  if (fabs(a*b) * __builtin_sqrt(2) != cabs (a*b-(-b*a*1i)))
    link_error();

  if (fabs(a*b) * __builtin_sqrt(2) != cabs (a*b*1i-a*-b))
    link_error();
}

void testf(__complex__ float x, float a, float b)
{
  if (cabsf(x) != cabsf(-x))
    link_error();

  if (cabsf(x) != cabsf(~x))
    link_error();

  if (fabsf(a) * __builtin_sqrtf(2) != cabsf (a+a*1i))
    link_error();

  if (fabsf(a) * __builtin_sqrtf(2) != cabsf (a*1i+a))
    link_error();

  if (fabsf(a) * __builtin_sqrtf(2) != cabsf (-a+a*-1i))
    link_error();

  if (fabsf(a) * __builtin_sqrtf(2) != cabsf (-a+-a*1i))
    link_error();

  if (fabsf(a) * __builtin_sqrtf(2) != cabsf (-a-a*1i))
    link_error();

  if (fabsf(a) * __builtin_sqrtf(2) != cabsf (a*-1i-a))
    link_error();

  if (fabsf(a) * __builtin_sqrtf(2) != cabsf (-a*1i-a))
    link_error();

  if (fabsf(a) * __builtin_sqrtf(2) != cabsf (a*-1i+-a))
    link_error();

  if (fabsf(a) * __builtin_sqrtf(2) != cabsf (-a*1i+-a))
    link_error();

  if (fabsf(a*b) * __builtin_sqrtf(2) != cabsf (a*b-(-b*a*1i)))
    link_error();

  if (fabsf(a*b) * __builtin_sqrtf(2) != cabsf (a*b*1i-a*-b))
    link_error();
}

void testl(__complex__ long double x, long double a, long double b)
{
  if (cabsl(x) != cabsl(-x))
    link_error();

  if (cabsl(x) != cabsl(~x))
    link_error();

  if (fabsl(a) * __builtin_sqrtl(2) != cabsl (a+a*1i))
    link_error();

  if (fabsl(a) * __builtin_sqrtl(2) != cabsl (a*1i+a))
    link_error();

  if (fabsl(a) * __builtin_sqrtl(2) != cabsl (-a+a*-1i))
    link_error();

  if (fabsl(a) * __builtin_sqrtl(2) != cabsl (-a+-a*1i))
    link_error();

  if (fabsl(a) * __builtin_sqrtl(2) != cabsl (-a-a*1i))
    link_error();

  if (fabsl(a) * __builtin_sqrtl(2) != cabsl (a*-1i-a))
    link_error();

  if (fabsl(a) * __builtin_sqrtl(2) != cabsl (-a*1i-a))
    link_error();

  if (fabsl(a) * __builtin_sqrtl(2) != cabsl (a*-1i+-a))
    link_error();

  if (fabsl(a) * __builtin_sqrtl(2) != cabsl (-a*1i+-a))
    link_error();

  if (fabsl(a*b) * __builtin_sqrtl(2) != cabsl (a*b-(-b*a*1i)))
    link_error();

  if (fabsl(a*b) * __builtin_sqrtl(2) != cabsl (a*b*1i-a*-b))
    link_error();
}

int main()
{
  test(0, 0, 0);
  testf(0, 0, 0);
  testl(0, 0, 0);
  return 0;
}

