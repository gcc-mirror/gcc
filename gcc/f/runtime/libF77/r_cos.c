#include "f2c.h"

#ifdef KR_headers
double cos();
double r_cos(x) real *x;
#else
#undef abs
#include <math.h>
double r_cos(real *x)
#endif
{
return( cos(*x) );
}
