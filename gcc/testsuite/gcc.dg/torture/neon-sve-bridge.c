// { dg-options "-march=armv8.2-a+sve" }
// { dg-do run { target aarch64_sve_hw } }

#include <arm_neon_sve_bridge.h>

extern void abort (void);

int
svget_neonq_test ()
{
  int64_t val1 = 987654321;
  int64_t val2 = 123456789;
  svint64_t sveInput = svdupq_n_s64 (val1, val2);
  int64x2_t neonReturn = svget_neonq_s64 (sveInput);
  int64_t val1Return = vgetq_lane_s64 (neonReturn, 0);
  int64_t val2Return = vgetq_lane_s64 (neonReturn, 1);
  if (val1 == val1Return && val2 == val2Return)
    return 0;
  return 1;
}

int
svset_neonq_test ()
{
  int64_t val1 = 987654321;
  int64_t val2 = 123456789;
  int64x2_t NeonInput;
  NeonInput = vsetq_lane_s64 (val1, NeonInput, 0);
  NeonInput = vsetq_lane_s64 (val2, NeonInput, 1);
  svint64_t sveReturn = svset_neonq_s64 (sveReturn, NeonInput);
  int64_t val1Return = svlasta_s64 (svptrue_b64(), sveReturn);
  int64_t val2Return = svlastb_s64 (svptrue_pat_b8(SV_VL16), sveReturn);
  if (val1 == val1Return && val2 == val2Return)
    return 0;
  return 1;
}

int
svdup_neonq_test ()
{
  int64_t val1 = 987654321;
  int64_t val2 = 123456789;
  int64x2_t NeonInput;
  NeonInput = vsetq_lane_s64 (val1, NeonInput, 0);
  NeonInput = vsetq_lane_s64 (val2, NeonInput, 1);
  svint64_t sveReturn = svdup_neonq_s64 (NeonInput);
  int64_t val1Return = svlasta_s64 (svptrue_b64(), sveReturn);
  int64_t val2Return = svlastb_s64 (svptrue_b64(), sveReturn);
  if (val1 == val1Return && val2 == val2Return)
    return 0;
  return 1;
}

int
main ()
{
  if (svget_neonq_test () == 1)
    abort ();
  if (svset_neonq_test () == 1)
    abort ();
  if (svdup_neonq_test () == 1)
    abort ();
  return 0;
}