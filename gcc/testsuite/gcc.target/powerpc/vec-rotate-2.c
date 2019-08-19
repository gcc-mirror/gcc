/* { dg-options "-O3 -mdejagnu-cpu=power8" } */

/* Check vectorizer can exploit vector rotation instructions on Power8, mainly
   for the case rotation count is const number.

   Check for vrld which is available on Power8 and above.  */

#define N 256
unsigned long long sud[N], rud[N];

void
testULL ()
{
  for (int i = 0; i < 256; ++i)
    rud[i] = (sud[i] >> 8) | (sud[i] << (sizeof (sud[0]) * 8 - 8));
}

/* { dg-final { scan-assembler {\mvrld\M} } } */
