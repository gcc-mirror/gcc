#include "f2c.h"

#ifdef KR_headers
extern double exp(), cos(), sin();

 VOID c_exp(resx, z) complex *resx, *z;
#else
#undef abs
#include <math.h>

void c_exp(complex *resx, complex *z)
#endif
{
double expx;
complex res;

expx = exp(z->r);
res.r = expx * cos(z->i);
res.i = expx * sin(z->i);

resx->r = res.r;
resx->i = res.i;
}
