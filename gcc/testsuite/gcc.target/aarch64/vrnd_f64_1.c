/* Test vrnd_f64 works correctly.  */
/* { dg-do run } */
/* { dg-options "--save-temps" } */

#include "arm_neon.h"

extern void abort (void);

/* Bit offset to round mode field in FPCR.  */
#define RMODE_START 22

#define FPROUNDING_ZERO 3

/* Set RMODE field of FPCR control register
   to rounding mode passed.  */
void __inline __attribute__ ((__always_inline__))
set_rounding_mode (uint32_t mode)
{
  uint32_t r;

  /* Read current FPCR.  */
  asm volatile ("mrs %[r], fpcr" : [r] "=r" (r) : :);

  /* Clear rmode.  */
  r &= ~(3 << RMODE_START);
  /* Calculate desired FPCR.  */
  r |= mode << RMODE_START;

  /* Write desired FPCR back.  */
  asm volatile ("msr fpcr, %[r]" : : [r] "r" (r) :);
}

float64x1_t __attribute__ ((noinline))
compare_f64 (float64x1_t passed, float64_t expected)
{
  return (__builtin_fabs (vget_lane_f64 (passed, 0) - expected)
	  > __DBL_EPSILON__);
}

void __attribute__ ((noinline))
run_round_tests (float64x1_t *tests,
		 float64_t expectations[][6])
{
  int i;

  for (i = 0; i < 6; i++)
    {
      if (compare_f64 (vrnd_f64 (tests[i]), expectations[0][i]))
	abort ();
      if (compare_f64 (vrndx_f64 (tests[i]), expectations[1][i]))
	abort ();
      if (compare_f64 (vrndp_f64 (tests[i]), expectations[2][i]))
	abort ();
      if (compare_f64 (vrndn_f64 (tests[i]), expectations[3][i]))
	abort ();
      if (compare_f64 (vrndm_f64 (tests[i]), expectations[4][i]))
	abort ();
      if (compare_f64 (vrndi_f64 (tests[i]), expectations[5][i]))
	abort ();
      if (compare_f64 (vrnda_f64 (tests[i]), expectations[6][i]))
	abort ();
    }
}

int
main (int argc, char **argv)
{
  float64x1_t tests[6] =
    {
      vcreate_f64 (0x3FE0000000000000), /* Hex for: 0.5.  */
      vcreate_f64 (0x3FD999999999999A), /* Hex for: 0.4.  */
      vcreate_f64 (0x3FE3333333333333), /* Hex for: 0.6.  */
      vcreate_f64 (0xBFE0000000000000), /* Hex for: -0.5.  */
      vcreate_f64 (0xBFD999999999999A), /* Hex for: -0.4.  */
      vcreate_f64 (0xBFE3333333333333), /* Hex for: -0.6.  */
    };

  float64_t expectations[7][6] =
  {
    { 0.0, 0.0, 0.0, 0.0, 0.0, 0.0 },    /* vrnd - round towards zero.  */
    { 0.0, 0.0, 0.0, 0.0, 0.0, 0.0 },    /* vrndx - round using FPCR mode.  */
    { 1.0, 1.0, 1.0, 0.0, 0.0, 0.0 },    /* vrndp - round to plus infinity.  */
    { 0.0, 0.0, 1.0, 0.0, 0.0, -1.0 },   /* vrndn - round ties to even.  */
    { 0.0, 0.0, 0.0, -1.0, -1.0, -1.0 }, /* vrndm - round to minus infinity.  */
    { 0.0, 0.0, 0.0, 0.0, 0.0, 0.0 },    /* vrndi - round using FPCR mode.  */
    { 1.0, 0.0, 1.0, -1.0, 0.0, -1.0 },  /* vrnda - round ties away from 0.  */
  };

  /* Set floating point control register
     to have predictable vrndx and vrndi behaviour.  */
  set_rounding_mode (FPROUNDING_ZERO);

  run_round_tests (tests, expectations);

  return 0;
}

/* { dg-final { scan-assembler-times "frintz\\td\[0-9\]+, d\[0-9\]+" 1 } } */
/* { dg-final { scan-assembler-times "frintx\\td\[0-9\]+, d\[0-9\]+" 1 } } */
/* { dg-final { scan-assembler-times "frintp\\td\[0-9\]+, d\[0-9\]+" 1 } } */
/* { dg-final { scan-assembler-times "frintn\\td\[0-9\]+, d\[0-9\]+" 1 } } */
/* { dg-final { scan-assembler-times "frintm\\td\[0-9\]+, d\[0-9\]+" 1 } } */
/* { dg-final { scan-assembler-times "frinti\\td\[0-9\]+, d\[0-9\]+" 1 } } */
/* { dg-final { scan-assembler-times "frinta\\td\[0-9\]+, d\[0-9\]+" 1 } } */
/* { dg-final { cleanup-saved-temps } } */
