#include "scalar-by-value-y.h"

long double d = 1234.0L + 0x0.abcdp-70L;
#ifndef SKIP_COMPLEX
_Complex long double cd = 234.0L + 0x0.abcp-70L + 567.0Li +0x0defp-70Li;
#endif

#include "scalar-by-value-6.c"
