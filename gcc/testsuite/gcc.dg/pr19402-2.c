/* { dg-do run } */
/* { dg-options "-fno-inline -Os" } */

void abort(void);


float powif(float x, int n)
{
  return __builtin_powif(x, n);
}

double powi(double x, int n)
{
  return __builtin_powi(x, n);
}

long double powil(long double x, int n)
{
  return __builtin_powil(x, n);
}


float powcif(float x)
{
  return __builtin_powif(x, 5);
}

double powci(double x)
{
  return __builtin_powi(x, 5);
}

long double powcil(long double x)
{
  return __builtin_powil(x, 5);
}


float powicf(int n)
{
  return __builtin_powif(2.0, n);
}

double powic(int n)
{
  return __builtin_powi(2.0, n);
}

long double powicl(int n)
{
  return __builtin_powil(2.0, n);
}


int main()
{
  if (__builtin_powi(1.0, 5) != 1.0)
    abort();
  if (__builtin_powif(1.0, 5) != 1.0)
    abort();
  if (__builtin_powil(1.0, 5) != 1.0)
    abort();
  if (powci(1.0) != 1.0)
    abort();
  if (powcif(1.0) != 1.0)
    abort();
  if (powcil(1.0) != 1.0)
    abort();
  if (powi(1.0, -5) != 1.0)
    abort();
  if (powif(1.0, -5) != 1.0)
    abort();
  if (powil(1.0, -5) != 1.0)
    abort();
  if (powic(1) != 2.0)
    abort();
  if (powicf(1) != 2.0)
    abort();
  if (powicl(1) != 2.0)
    abort();
  return 0;
}
