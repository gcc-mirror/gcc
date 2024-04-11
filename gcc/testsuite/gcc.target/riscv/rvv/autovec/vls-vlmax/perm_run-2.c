/* { dg-do run { target { riscv_v } } } */
/* { dg-options "-mrvv-vector-bits=zvl -O3" } */

#include "perm-2.c"

int __attribute__ ((optimize (0)))
main ()
{
  INIT_PERMUTE(32, -1, 30, vnx32qi)
  permute_vnx32qi (v_vnx32qi_in1, v_vnx32qi_in2, &v_vnx32qi_out);
  CHECK_PERMUTE_SINGLE(32, -1*31+30, vnx32qi)
  INIT_PERMUTE(64, -1, 66, vnx64qi)
  permute_vnx64qi (v_vnx64qi_in1, v_vnx64qi_in2, &v_vnx64qi_out);
  CHECK_PERMUTE_SINGLE(64, -1*31+66, vnx64qi)
  INIT_PERMUTE(128, -1, 38, vnx128qi)
  permute_vnx128qi (v_vnx128qi_in1, v_vnx128qi_in2, &v_vnx128qi_out);
  CHECK_PERMUTE_SINGLE(128, -1*31+38, vnx128qi)
  INIT_PERMUTE(32, 156, -9156, vnx32hi)
  permute_vnx32hi (v_vnx32hi_in1, v_vnx32hi_in2, &v_vnx32hi_out);
  CHECK_PERMUTE_SINGLE(32, 156*31+-9156, vnx32hi)
  INIT_PERMUTE(64, -251, 8093, vnx64hi)
  permute_vnx64hi (v_vnx64hi_in1, v_vnx64hi_in2, &v_vnx64hi_out);
  CHECK_PERMUTE_SINGLE(64, -251*31+8093, vnx64hi)
  INIT_PERMUTE(32, -532, 98416, vnx32si)
  permute_vnx32si (v_vnx32si_in1, v_vnx32si_in2, &v_vnx32si_out);
  CHECK_PERMUTE_SINGLE(32, -532*31+98416, vnx32si)
  INIT_PERMUTE(32, 995, 1561318, vnx32sf)
  permute_vnx32sf (v_vnx32sf_in1, v_vnx32sf_in2, &v_vnx32sf_out);
  CHECK_PERMUTE_SINGLE(32, 995*31+1561318, vnx32sf)

  return 0;
}
