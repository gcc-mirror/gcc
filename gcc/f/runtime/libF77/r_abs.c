#include "f2c.h"

#ifdef KR_headers
double r_abs(x) real *x;
#else
double r_abs(real *x)
#endif
{
if(*x >= 0)
	return(*x);
return(- *x);
}
