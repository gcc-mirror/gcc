#include "f2c.h"

#ifdef KR_headers
VOID r_cnjg(resx, z) complex *resx, *z;
#else
VOID r_cnjg(complex *resx, complex *z)
#endif
{
complex res;

res.r = z->r;
res.i = - z->i;

resx->r = res.r;
resx->i = res.i;
}
