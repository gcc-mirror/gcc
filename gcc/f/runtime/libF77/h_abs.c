#include "f2c.h"

#ifdef KR_headers
shortint h_abs(x) shortint *x;
#else
shortint h_abs(shortint *x)
#endif
{
if(*x >= 0)
	return(*x);
return(- *x);
}
