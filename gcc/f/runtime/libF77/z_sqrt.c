#include "f2c.h"

#ifdef KR_headers
double sqrt(), f__cabs();
VOID z_sqrt(resx, z) doublecomplex *resx, *z;
#else
#undef abs
#include <math.h>
extern double f__cabs(double, double);
void z_sqrt(doublecomplex *resx, doublecomplex *z)
#endif
{
double mag;
doublecomplex res;

if( (mag = f__cabs(z->r, z->i)) == 0.)
	res.r = res.i = 0.;
else if(z->r > 0)
	{
	res.r = sqrt(0.5 * (mag + z->r) );
	res.i = z->i / res.r / 2;
	}
else
	{
	res.i = sqrt(0.5 * (mag - z->r) );
	if(z->i < 0)
		res.i = - res.i;
	res.r = z->i / res.i / 2;
	}

resx->r = res.r;
resx->i = res.i;
}
