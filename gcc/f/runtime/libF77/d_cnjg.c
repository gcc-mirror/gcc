#include "f2c.h"

 VOID
#ifdef KR_headers
d_cnjg(resx, z) doublecomplex *resx, *z;
#else
d_cnjg(doublecomplex *resx, doublecomplex *z)
#endif
{
doublecomplex res;

res.r = z->r;
res.i = - z->i;

resx->r = res.r;
resx->i = res.i;
}
