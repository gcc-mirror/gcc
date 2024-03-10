/* { dg-do compile } */
/* { dg-options "-O2" } */
typedef unsigned long int mp_limb_t;
typedef const mp_limb_t * mp_srcptr;

int
refmpn_tstbit_bad (mp_srcptr ptr, unsigned long bit)
{
  return (((ptr)[(bit)/(32 - 0)] & (((mp_limb_t) 1L) << ((bit)%(32 - 0)))) != 0);
}

/* { dg-final { scan-assembler "bt\[ql\]" } } */
/* { dg-final { scan-assembler "setc" } } */
