#include "f2c.h"

 integer
#ifdef KR_headers
lbit_shift(a, b) integer a; integer b;
#else
lbit_shift(integer a, integer b)
#endif
{
	return b >= 0 ? a << b : (integer)((uinteger)a >> -b);
	}
