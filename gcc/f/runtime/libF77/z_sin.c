#include "f2c.h"

#ifdef KR_headers
double sin(), cos(), sinh(), cosh();
VOID z_sin(resx, z) doublecomplex *resx, *z;
#else
#undef abs
#include <math.h>
void z_sin(doublecomplex *resx, doublecomplex *z)
#endif
{
doublecomplex res;

res.r = sin(z->r) * cosh(z->i);
res.i = cos(z->r) * sinh(z->i);

resx->r = res.r;
resx->i = res.i;
}
