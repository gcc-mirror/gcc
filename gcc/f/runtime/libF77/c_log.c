#include "f2c.h"

#ifdef KR_headers
extern double log(), f__cabs(), atan2();
VOID c_log(resx, z) complex *resx, *z;
#else
#undef abs
#include <math.h>
extern double f__cabs(double, double);

void c_log(complex *resx, complex *z)
#endif
{
complex res;

res.i = atan2(z->i, z->r);
res.r = log( f__cabs(z->r, z->i) );

resx->r = res.r;
resx->i = res.i;
}
