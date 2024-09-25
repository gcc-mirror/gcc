// { dg-do assemble  }
// { dg-skip-if "requires hosted libstdc++ for complex" { ! hostedlib } }
// GROUPS passed old-abort
#include <complex>
typedef std::complex<double> Complex;

Complex ComputeVVself()
{
Complex temp1;
Complex self[3][3];

   self[1][2] = 100.0;
   return self[1][2];

}
