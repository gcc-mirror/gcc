// Build don't link: 
// GROUPS passed old-abort
#include <complex>
typedef complex<double> Complex;

Complex ComputeVVself()
{
Complex temp1;
Complex self[3][3];

   self[1][2] = 100.0;
   return self[1][2];

}
