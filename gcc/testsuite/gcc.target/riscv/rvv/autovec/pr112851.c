/* { dg-do compile } */
/* { dg-options "-march=rv32gcv_zvl1024b -mabi=ilp32d -O3 -fomit-frame-pointer -funroll-loops -fpeel-loops -ftracer -finline-functions" } */

int safe_lshift_func_int32_t_s_s_left, safe_lshift_func_int32_t_s_s_right,
    safe_sub_func_uint64_t_u_u_ui2, safe_mul_func_uint64_t_u_u_ui2, g_79_2,
    g_97_l_439;
void g_97(int * __restrict l_437)
{
  for (; g_97_l_439; g_97_l_439 += 1)
    for (char l_502 = 0; l_502 < 4; l_502++)
      {
        int __trans_tmp_14 = ((safe_lshift_func_int32_t_s_s_right >= 2
                               || safe_lshift_func_int32_t_s_s_left)
                              ? 1 : safe_lshift_func_int32_t_s_s_right);
        long __trans_tmp_15 = __trans_tmp_14 * safe_mul_func_uint64_t_u_u_ui2;
        unsigned short __trans_tmp_16 = -__trans_tmp_15;
        int __trans_tmp_7
          = (__trans_tmp_16 ^ 65535UL) - safe_sub_func_uint64_t_u_u_ui2;
        *l_437 ^= (short)(__trans_tmp_7 ^ g_79_2);
      }
}
