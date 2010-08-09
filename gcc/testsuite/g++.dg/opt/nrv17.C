// { dg-do run }

#include <cstdlib>
#include <complex>

void __attribute__((noinline))
h(std::complex<double> x)
{
  if (x.real() != 2.0)
    std::abort ();
}

void __attribute__((noinline))
g(std::complex<double> x)
{
  if (x.real() != 0.5)
    std::abort ();
}

void __attribute__((noinline))
f(std::complex<double> x)
{
  h (x);
  x = 1.0 / x;
  g (x);
}

int main()
{
  f(2.0);
  return 0;
}
