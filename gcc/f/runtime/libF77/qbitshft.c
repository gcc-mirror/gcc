#include "f2c.h"

 longint
#ifdef KR_headers
qbit_shift(a, b) longint a; integer b;
#else
qbit_shift(longint a, integer b)
#endif
{
	return b >= 0 ? a << b : (longint)((ulongint)a >> -b);
	}
