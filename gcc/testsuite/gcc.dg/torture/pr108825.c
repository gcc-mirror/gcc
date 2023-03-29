/* { dg-do compile } */

int safe_mul_func_uint8_t_u_u_ui2, g_231, g_277_1, g_568, func_35___trans_tmp_10;
int g_81[7];
extern int g_96[];
char func_35___trans_tmp_11;
static inline int safe_add_func_int32_t_s_s(int si1, int si2)
{ return si1 > 647 - si2 ?: si1; }
void func_35() {
  for (; g_277_1; g_277_1 += 1) {
    g_231 = 0;
    for (; g_231 <= 6; g_231 += 1) {
      func_35___trans_tmp_10 =
          safe_add_func_int32_t_s_s(g_81[g_231], g_568 || g_96[1]);
      func_35___trans_tmp_11 =
          func_35___trans_tmp_10 * safe_mul_func_uint8_t_u_u_ui2;
      g_81[g_231] = func_35___trans_tmp_11;
    }
  }
}
