/* { dg-do run { target { riscv_v } } } */
/* { dg-options "-mrvv-vector-bits=zvl -O3" } */

#include "perm-3.c"

int __attribute__ ((optimize (0)))
main ()
{
  INIT_PERMUTE(64, -1, 66, vnx64qi)
  permute_vnx64qi (v_vnx64qi_in1, v_vnx64qi_in2, &v_vnx64qi_out);
  CHECK_PERMUTE_SINGLE(64, -1*55+66, vnx64qi)
  INIT_PERMUTE(128, -1, 38, vnx128qi)
  permute_vnx128qi (v_vnx128qi_in1, v_vnx128qi_in2, &v_vnx128qi_out);
  CHECK_PERMUTE_SINGLE(128, -1*55+38, vnx128qi)
  INIT_PERMUTE(64, -251, 8093, vnx64hi)
  permute_vnx64hi (v_vnx64hi_in1, v_vnx64hi_in2, &v_vnx64hi_out);
  CHECK_PERMUTE_SINGLE(64, -251*55+8093, vnx64hi)

  return 0;
}
