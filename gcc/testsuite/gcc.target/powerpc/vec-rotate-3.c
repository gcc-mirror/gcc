/* { dg-options "-O3 -maltivec" } */
/* { dg-require-effective-target powerpc_altivec_ok } */

/* Check vectorizer can exploit vector rotation instructions on Power, mainly
   for the case rotation count isn't const number.

   Check for instructions vrlb/vrlh/vrlw only available if altivec supported. */

#define N 256
unsigned int suw[N], ruw[N];
unsigned short suh[N], ruh[N];
unsigned char sub[N], rub[N];
extern unsigned char rot_cnt;

void
testUW ()
{
  for (int i = 0; i < 256; ++i)
    ruw[i] = (suw[i] >> rot_cnt) | (suw[i] << (sizeof (suw[0]) * 8 - rot_cnt));
}

void
testUH ()
{
  for (int i = 0; i < 256; ++i)
    ruh[i] = (unsigned short) (suh[i] >> rot_cnt)
	     | (unsigned short) (suh[i] << (sizeof (suh[0]) * 8 - rot_cnt));
}

void
testUB ()
{
  for (int i = 0; i < 256; ++i)
    rub[i] = (unsigned char) (sub[i] >> rot_cnt)
	     | (unsigned char) (sub[i] << (sizeof (sub[0]) * 8 - rot_cnt));
}

/* { dg-final { scan-assembler {\mvrlw\M} } } */
/* { dg-final { scan-assembler {\mvrlh\M} } } */
/* { dg-final { scan-assembler {\mvrlb\M} } } */
