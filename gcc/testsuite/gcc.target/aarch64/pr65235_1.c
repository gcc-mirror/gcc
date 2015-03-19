/* { dg-do run } */
/* { dg-options "-O2" } */

#include "arm_neon.h"

int
main (int argc, char** argv)
{
  int64x1_t val1;
  int64x1_t val2;
  int64x1_t val3;
  uint64x1_t val13;
  uint64x2_t val14;
  uint64_t got;
  uint64_t exp;
  val1 = vcreate_s64(UINT64_C(0xffffffff80008000));
  val2 = vcreate_s64(UINT64_C(0x0000f38d00000000));
  val3 = vcreate_s64(UINT64_C(0xffff7fff0000809b));
  /* Expect: "val13" = 8000000000001553.  */
  val13 = vcreate_u64 (UINT64_C(0x8000000000001553));
  /* Expect: "val14" = 0010 0000 0000 0002 0000 0000 0000 0000.  */
  val14 = vcombine_u64(vcgt_s64(vqrshl_s64(val1, val2),
				vshr_n_s64(val3, 18)),
		       vshr_n_u64(val13, 11));
  /* Should be 0000000000000000.  */
  got = vgetq_lane_u64(val14, 0);
  exp = 0;
  if(exp != got)
    __builtin_abort ();
}
