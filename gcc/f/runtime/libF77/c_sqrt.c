#include "f2c.h"

#ifdef KR_headers
extern double sqrt(), f__cabs();

VOID c_sqrt(resx, z) complex *resx, *z;
#else
#undef abs
#include <math.h>
extern double f__cabs(double, double);

void c_sqrt(complex *resx, complex *z)
#endif
{
double mag, t;
complex res;

if( (mag = f__cabs(z->r, z->i)) == 0.)
	res.r = res.i = 0.;
else if(z->r > 0)
	{
	res.r = t = sqrt(0.5 * (mag + z->r) );
	t = z->i / t;
	res.i = 0.5 * t;
	}
else
	{
	t = sqrt(0.5 * (mag - z->r) );
	if(z->i < 0)
		t = -t;
	res.i = t;
	t = z->i / t;
	res.r = 0.5 * t;
	}

resx->r = res.r;
resx->i = res.i;
}
