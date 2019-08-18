/* { dg-options "-O3 -maltivec" } */
/* { dg-require-effective-target powerpc_altivec_ok } */

/* Check vectorizer can exploit vector rotation instructions on Power, mainly
   for the case rotation count is const number.

   Check for instructions vrlb/vrlh/vrlw only available if altivec supported. */

#define N 256
unsigned int suw[N], ruw[N];
unsigned short suh[N], ruh[N];
unsigned char sub[N], rub[N];

void
testUW ()
{
  for (int i = 0; i < 256; ++i)
    ruw[i] = (suw[i] >> 8) | (suw[i] << (sizeof (suw[0]) * 8 - 8));
}

void
testUH ()
{
  for (int i = 0; i < 256; ++i)
    ruh[i] = (unsigned short) (suh[i] >> 9)
	     | (unsigned short) (suh[i] << (sizeof (suh[0]) * 8 - 9));
}

void
testUB ()
{
  for (int i = 0; i < 256; ++i)
    rub[i] = (unsigned char) (sub[i] >> 5)
	     | (unsigned char) (sub[i] << (sizeof (sub[0]) * 8 - 5));
}

/* { dg-final { scan-assembler {\mvrlw\M} } } */
/* { dg-final { scan-assembler {\mvrlh\M} } } */
/* { dg-final { scan-assembler {\mvrlb\M} } } */
