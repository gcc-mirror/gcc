// Build don't link: 
// GROUPS passed constructors
#include <complex.h>

double foo(double_complex *a)
{
  return 0.0;
}


double bar(void)
{
  double_complex v[10];
  return foo(v);
}
