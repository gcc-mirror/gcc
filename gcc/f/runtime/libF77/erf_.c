#include "f2c.h"

#ifdef KR_headers
double erf();
double G77_erf_0 (x) real *x;
#else
extern double erf(double);
double G77_erf_0 (real *x)
#endif
{
return( erf(*x) );
}
