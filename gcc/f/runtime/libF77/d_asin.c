#include "f2c.h"

#ifdef KR_headers
double asin();
double d_asin(x) doublereal *x;
#else
#undef abs
#include <math.h>
double d_asin(doublereal *x)
#endif
{
return( asin(*x) );
}
