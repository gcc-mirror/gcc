#include "f2c.h"

#ifdef KR_headers
double d_abs(x) doublereal *x;
#else
double d_abs(doublereal *x)
#endif
{
if(*x >= 0)
	return(*x);
return(- *x);
}
