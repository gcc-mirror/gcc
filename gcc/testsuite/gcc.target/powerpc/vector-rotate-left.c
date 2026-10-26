/* { dg-do compile } */
/* { dg-require-effective-target powerpc_future_assemble_ok } */
/* { dg-options "-mdejagnu-cpu=future -O2" } */

/* Test whether the xvrl (vector word rotate left using VSX registers insead of
   Altivec registers is generated.  */

#include <altivec.h>

typedef vector unsigned int  v4si_t;

v4si_t
rotl_v4si_scalar (v4si_t x, unsigned long n)
{
  __asm__ (" # %x0" : "+f" (x));
  return (x << n) | (x >> (32 - n));	/* xvrlw.  */
}

v4si_t
rotr_v4si_scalar (v4si_t x, unsigned long n)
{
  __asm__ (" # %x0" : "+f" (x));
  return (x >> n) | (x << (32 - n));	/* xvrlw.  */
}

v4si_t
rotl_v4si_vector (v4si_t x, v4si_t y)
{
  __asm__ (" # %x0" : "+f" (x));	/* xvrlw.  */
  return vec_rl (x, y);
}

/* { dg-final { scan-assembler-times {\mxvrlw\M} 3  } } */
/* { dg-final { scan-assembler-not   {\mvrlw\M}     } } */
