#include "f2c.h"

#ifdef KR_headers
double erf();
double G77_derf_0 (x) doublereal *x;
#else
extern double erf(double);
double G77_derf_0 (doublereal *x)
#endif
{
return( erf(*x) );
}
