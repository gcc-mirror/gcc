#include "f2c.h"

#ifdef KR_headers
double acos();
double d_acos(x) doublereal *x;
#else
#undef abs
#include <math.h>
double d_acos(doublereal *x)
#endif
{
return( acos(*x) );
}
