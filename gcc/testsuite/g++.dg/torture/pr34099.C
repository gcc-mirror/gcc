/* { dg-do run } */
/* { dg-skip-if "requires hosted libstdc++ for complex" { ! hostedlib } } */

#include <complex>

typedef std::complex<double> NumType;

void
multiply(NumType a, NumType b, unsigned ac, NumType &ab)
{
  NumType s;
  for (unsigned j=0; j<ac; j++)
    s = a * b;
  ab = s;
}
extern "C" void abort (void);
int main()
{
  NumType a(1,2), b(3,-2), c;
  multiply(a, b, 1, c);
  if (c.real() != 7
      || c.imag() != 4)
    abort ();
  return 0;
}

