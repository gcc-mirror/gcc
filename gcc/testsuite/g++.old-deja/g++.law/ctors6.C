// { dg-do assemble  }
// { dg-skip-if "requires hosted libstdc++ for complex" { ! hostedlib } }
// GROUPS passed constructors
#include <complex>

double foo(std::complex<double> *a)
{
  return 0.0;
}


double bar(void)
{
  std::complex<double> v[10];
  return foo(v);
}
