/* { dg-options "-std=gnu99" } */

/* N1150 4: Characteristics of decimal floating types <decfloat.h>.
   C99 5.2.4.2.2a[3]: New.

   Verify constants about range of decimal float and three components of
   decimal float defined in decfloat.h.  */

/* Make sure we are exporting the right values to decfloat.h. */
#include <decfloat.h>

extern void abort (void);

int main ()
{
  if (DEC32_MANT_DIG != 7) abort();
  if (DEC64_MANT_DIG != 16) abort();
  if (DEC128_MANT_DIG != 34) abort();

  if (DEC32_MIN_EXP != -95) abort();
  if (DEC64_MIN_EXP != -383) abort();
  if (DEC128_MIN_EXP != -6143) abort();

  if (DEC32_MAX_EXP != 96) abort();
  if (DEC64_MAX_EXP != 384) abort();
  if (DEC128_MAX_EXP != 6144) abort();

  if (DEC32_MAX != 9.999999E96DF) abort();
  if (DEC64_MAX != 9.999999999999999E384DD) abort();
  if (DEC128_MAX != 9.999999999999999999999999999999999E6144DL) abort();

  if (DEC32_EPSILON != 1E-6DF) abort();
  if (DEC64_EPSILON != 1E-15DD) abort();
  if (DEC128_EPSILON != 1E-33DL) abort();
  
  if (DEC32_MIN != 1E-95DF) abort();
  if (DEC64_MIN != 1E-383DD) abort();
  if (DEC128_MIN != 1E-6143DL) abort();

  if (DEC32_DEN != 0.000001E-95DF) abort();
  if (DEC64_DEN != 0.000000000000001E-383DD) abort();
  if (DEC128_DEN != 0.000000000000000000000000000000001E-6143DL) abort();

  return 0;
}
