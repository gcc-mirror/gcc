/* { dg-do run { target { riscv_v } } } */
/* { dg-options "-mrvv-vector-bits=zvl -O3" } */

#include "perm-4.c"

int __attribute__ ((optimize (0)))
main ()
{
  INIT_PERMUTE(2, 3, 79, vnx2qi)
  permute_vnx2qi (v_vnx2qi_in1, v_vnx2qi_in2, &v_vnx2qi_out);
  CHECK_PERMUTE_REVERSE(2, vnx2qi)
  INIT_PERMUTE(4, 2, -69, vnx4qi)
  permute_vnx4qi (v_vnx4qi_in1, v_vnx4qi_in2, &v_vnx4qi_out);
  CHECK_PERMUTE_REVERSE(4, vnx4qi)
  INIT_PERMUTE(8, 4, -33, vnx8qi)
  permute_vnx8qi (v_vnx8qi_in1, v_vnx8qi_in2, &v_vnx8qi_out);
  CHECK_PERMUTE_REVERSE(8, vnx8qi)
  INIT_PERMUTE(16, -3, 15, vnx16qi)
  permute_vnx16qi (v_vnx16qi_in1, v_vnx16qi_in2, &v_vnx16qi_out);
  CHECK_PERMUTE_REVERSE(16, vnx16qi)
  INIT_PERMUTE(32, -1, 30, vnx32qi)
  permute_vnx32qi (v_vnx32qi_in1, v_vnx32qi_in2, &v_vnx32qi_out);
  CHECK_PERMUTE_REVERSE(32, vnx32qi)
  INIT_PERMUTE(64, -1, 66, vnx64qi)
  permute_vnx64qi (v_vnx64qi_in1, v_vnx64qi_in2, &v_vnx64qi_out);
  CHECK_PERMUTE_REVERSE(64, vnx64qi)
  INIT_PERMUTE(128, -1, 38, vnx128qi)
  permute_vnx128qi (v_vnx128qi_in1, v_vnx128qi_in2, &v_vnx128qi_out);
  CHECK_PERMUTE_REVERSE(128, vnx128qi)
  INIT_PERMUTE(2, 2, 30238, vnx2hi)
  permute_vnx2hi (v_vnx2hi_in1, v_vnx2hi_in2, &v_vnx2hi_out);
  CHECK_PERMUTE_REVERSE(2, vnx2hi)
  INIT_PERMUTE(4, -45, -2345, vnx4hi)
  permute_vnx4hi (v_vnx4hi_in1, v_vnx4hi_in2, &v_vnx4hi_out);
  CHECK_PERMUTE_REVERSE(4, vnx4hi)
  INIT_PERMUTE(8, 98, -18415, vnx8hi)
  permute_vnx8hi (v_vnx8hi_in1, v_vnx8hi_in2, &v_vnx8hi_out);
  CHECK_PERMUTE_REVERSE(8, vnx8hi)
  INIT_PERMUTE(16, 56, 3299, vnx16hi)
  permute_vnx16hi (v_vnx16hi_in1, v_vnx16hi_in2, &v_vnx16hi_out);
  CHECK_PERMUTE_REVERSE(16, vnx16hi)
  INIT_PERMUTE(32, 15641, -9156, vnx32hi)
  permute_vnx32hi (v_vnx32hi_in1, v_vnx32hi_in2, &v_vnx32hi_out);
  CHECK_PERMUTE_REVERSE(32, vnx32hi)
  INIT_PERMUTE(64, -25641, 8093, vnx64hi)
  permute_vnx64hi (v_vnx64hi_in1, v_vnx64hi_in2, &v_vnx64hi_out);
  CHECK_PERMUTE_REVERSE(64, vnx64hi)
  INIT_PERMUTE(2, -428, -15651, vnx2si)
  permute_vnx2si (v_vnx2si_in1, v_vnx2si_in2, &v_vnx2si_out);
  CHECK_PERMUTE_REVERSE(2, vnx2si)
  INIT_PERMUTE(4, 208, -55651, vnx4si)
  permute_vnx4si (v_vnx4si_in1, v_vnx4si_in2, &v_vnx4si_out);
  CHECK_PERMUTE_REVERSE(4, vnx4si)
  INIT_PERMUTE(8, 808, 75651, vnx8si)
  permute_vnx8si (v_vnx8si_in1, v_vnx8si_in2, &v_vnx8si_out);
  CHECK_PERMUTE_REVERSE(8, vnx8si)
  INIT_PERMUTE(16, 816, -8941561, vnx16si)
  permute_vnx16si (v_vnx16si_in1, v_vnx16si_in2, &v_vnx16si_out);
  CHECK_PERMUTE_REVERSE(16, vnx16si)
  INIT_PERMUTE(32, -532, 98416, vnx32si)
  permute_vnx32si (v_vnx32si_in1, v_vnx32si_in2, &v_vnx32si_out);
  CHECK_PERMUTE_REVERSE(32, vnx32si)
  INIT_PERMUTE(2, -4161, 9551616, vnx2di)
  permute_vnx2di (v_vnx2di_in1, v_vnx2di_in2, &v_vnx2di_out);
  CHECK_PERMUTE_REVERSE(2, vnx2di)
  INIT_PERMUTE(4, 7259, -15644961, vnx4di)
  permute_vnx4di (v_vnx4di_in1, v_vnx4di_in2, &v_vnx4di_out);
  CHECK_PERMUTE_REVERSE(4, vnx4di)
  INIT_PERMUTE(8, 351, 9156651, vnx8di)
  permute_vnx8di (v_vnx8di_in1, v_vnx8di_in2, &v_vnx8di_out);
  CHECK_PERMUTE_REVERSE(8, vnx8di)
  INIT_PERMUTE(16, 11, -816196231,vnx16di)
  permute_vnx16di (v_vnx16di_in1, v_vnx16di_in2, &v_vnx16di_out);
  CHECK_PERMUTE_REVERSE(16, vnx16di)
  INIT_PERMUTE(2, 4552, -89, vnx2sf)
  permute_vnx2sf (v_vnx2sf_in1, v_vnx2sf_in2, &v_vnx2sf_out);
  CHECK_PERMUTE_REVERSE(2, vnx2sf)
  INIT_PERMUTE(4, 685, 7961, vnx4sf)
  permute_vnx4sf (v_vnx4sf_in1, v_vnx4sf_in2, &v_vnx4sf_out);
  CHECK_PERMUTE_REVERSE(4, vnx4sf)
  INIT_PERMUTE(8, 3927, 16513, vnx8sf)
  permute_vnx8sf (v_vnx8sf_in1, v_vnx8sf_in2, &v_vnx8sf_out);
  CHECK_PERMUTE_REVERSE(8, vnx8sf)
  INIT_PERMUTE(16, -68, 16156571, vnx16sf)
  permute_vnx16sf (v_vnx16sf_in1, v_vnx16sf_in2, &v_vnx16sf_out);
  CHECK_PERMUTE_REVERSE(16, vnx16sf)
  INIT_PERMUTE(32, 9985, 1561318, vnx32sf)
  permute_vnx32sf (v_vnx32sf_in1, v_vnx32sf_in2, &v_vnx32sf_out);
  CHECK_PERMUTE_REVERSE(32, vnx32sf)
  INIT_PERMUTE(2, -1565.1561, -5641565.515, vnx2df)
  permute_vnx2df (v_vnx2df_in1, v_vnx2df_in2, &v_vnx2df_out);
  CHECK_PERMUTE_REVERSE(2, vnx2df)
  INIT_PERMUTE(4, -189.14897196, -15616547.5165574, vnx4df)
  permute_vnx4df (v_vnx4df_in1, v_vnx4df_in2, &v_vnx4df_out);
  CHECK_PERMUTE_REVERSE(4, vnx4df)
  INIT_PERMUTE(8, 651.158691561, -56163.1655411, vnx8df)
  permute_vnx8df (v_vnx8df_in1, v_vnx8df_in2, &v_vnx8df_out);
  CHECK_PERMUTE_REVERSE(8, vnx8df)
  INIT_PERMUTE(16, 58.91516377, 251465.81561, vnx16df)
  permute_vnx16df (v_vnx16df_in1, v_vnx16df_in2, &v_vnx16df_out);
  CHECK_PERMUTE_REVERSE(16, vnx16df)

  return 0;
}
