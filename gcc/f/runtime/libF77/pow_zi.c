#include "f2c.h"

#ifdef KR_headers
VOID pow_zi(resx, a, b) 	/* p = a**b  */
 doublecomplex *resx, *a; integer *b;
#else
extern void z_div(doublecomplex*, doublecomplex*, doublecomplex*);
void pow_zi(doublecomplex *resx, doublecomplex *a, integer *b) 	/* p = a**b  */
#endif
{
integer n;
unsigned long u;
double t;
doublecomplex x;
doublecomplex res;
static doublecomplex one = {1.0, 0.0};

n = *b;

if(n == 0)
	{
	resx->r = 1;
	resx->i = 0;
	return;
	}

res.r = 1;
res.i = 0;

if(n < 0)
	{
	n = -n;
	z_div(&x, &one, a);
	}
else
	{
	x.r = a->r;
	x.i = a->i;
	}

for(u = n; ; )
	{
	if(u & 01)
		{
		t = res.r * x.r - res.i * x.i;
		res.i = res.r * x.i + res.i * x.r;
		res.r = t;
		}
	if(u >>= 1)
		{
		t = x.r * x.r - x.i * x.i;
		x.i = 2 * x.r * x.i;
		x.r = t;
		}
	else
		break;
	}

resx->r = res.r;
resx->i = res.i;
}
