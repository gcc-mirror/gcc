#include "f2c.h"

#ifdef KR_headers
double log(), f__cabs(), atan2();
VOID z_log(resx, z) doublecomplex *resx, *z;
#else
#undef abs
#include <math.h>
extern double f__cabs(double, double);
void z_log(doublecomplex *resx, doublecomplex *z)
#endif
{
doublecomplex res;

res.i = atan2(z->i, z->r);
res.r = log( f__cabs( z->r, z->i ) );

resx->r = res.r;
resx->i = res.i;
}
