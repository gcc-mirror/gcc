
/* { dg-do compile } */
/* { dg-options "-O3" } */

void fn2 ();

typedef __Float16x4_t float16x4_t;
__fp16 result_float16x4[1];
float16x4_t exec_vst1_lane_vector_float16x4, exec_vst1_lane___trans_tmp_1;

void fn1 ()
{
  exec_vst1_lane_vector_float16x4 = exec_vst1_lane___trans_tmp_1;
  __fp16 *__a = result_float16x4;
  float16x4_t __b = exec_vst1_lane___trans_tmp_1;
  int __lane = 0;
  *__a = ({ __b[__lane]; });
  union {
      short i;
      __fp16 f;
  } tmp_res;
  tmp_res.f = result_float16x4[0];
  if (tmp_res.i)
    fn2();
}
