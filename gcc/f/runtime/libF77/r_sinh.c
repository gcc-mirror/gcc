#include "f2c.h"

#ifdef KR_headers
double sinh();
double r_sinh(x) real *x;
#else
#undef abs
#include <math.h>
double r_sinh(real *x)
#endif
{
return( sinh(*x) );
}
