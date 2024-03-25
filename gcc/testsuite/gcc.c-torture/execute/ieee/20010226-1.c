#include <float.h>

void abort (void);

long double dfrom = 1.1L;
long double m1;
long double m2;
unsigned long mant_long;

int main()
{
  /* Some targets don't support a conforming long double type.  This is
     common with very small parts which set long double == float.   Look
     to see if the type has at least 32 bits of precision.  */
  if (LDBL_EPSILON > 0x1p-31L)
    return 0;

  m1 = dfrom / 2.0L;
  m2 = m1 * 4294967296.0L;
  mant_long = ((unsigned long) m2) & 0xffffffff;

  if (mant_long == 0x8ccccccc)
    return 0;
  else
    abort();
}
