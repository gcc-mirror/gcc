#include "f2c.h"

#ifdef KR_headers
double d_prod(x,y) real *x, *y;
#else
double d_prod(real *x, real *y)
#endif
{
return( (*x) * (*y) );
}
