#include "f2c.h"

#ifdef KR_headers
double r_imag(z) complex *z;
#else
double r_imag(complex *z)
#endif
{
return(z->i);
}
