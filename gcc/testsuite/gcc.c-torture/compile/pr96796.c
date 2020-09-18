/* { dg-additional-options "-fcommon" } */
/* { dg-require-effective-target non_strict_prototype } */

struct S0 {
  signed f0 : 8;
  unsigned f1;
  unsigned f4;
};
struct S1 {
  long f3;
  char f4;
} g_3_4;

int g_5, func_1_l_32, func_50___trans_tmp_31;
static struct S0 g_144, g_834, g_1255, g_1261;

int g_273[120] = {};
int *g_555;
char **g_979;
static int g_1092_0;
static int g_1193;
int safe_mul_func_int16_t_s_s(int si1, int si2) { return si1 * si2; }
static struct S0 *func_50();
int func_1() { func_50(g_3_4, g_5, func_1_l_32, 8, 3); }
void safe_div_func_int64_t_s_s(int *);
void safe_mod_func_uint32_t_u_u(struct S0);
struct S0 *func_50(int p_51, struct S0 p_52, struct S1 p_53, int p_54,
                   int p_55) {
  int __trans_tmp_30;
  char __trans_tmp_22;
  short __trans_tmp_19;
  long l_985_1;
  long l_1191[8];
  safe_div_func_int64_t_s_s(g_273);
  __builtin_printf((char*)g_1261.f4);
  safe_mod_func_uint32_t_u_u(g_834);
  g_144.f0 += 1;
  for (;;) {
    struct S1 l_1350 = {&l_1350};
    for (; p_53.f3; p_53.f3 -= 1)
      for (; g_1193 <= 2; g_1193 += 1) {
        __trans_tmp_19 = safe_mul_func_int16_t_s_s(l_1191[l_985_1 + p_53.f3],
                                                   p_55 % (**g_979 = 10));
        __trans_tmp_22 = g_1255.f1 * p_53.f4;
        __trans_tmp_30 = __trans_tmp_19 + __trans_tmp_22;
        if (__trans_tmp_30)
          g_1261.f0 = p_51;
        else {
          g_1255.f0 = p_53.f3;
          int *l_1422 = g_834.f0 = g_144.f4 != (*l_1422)++ > 0 < 0 ^ 51;
          g_555 = ~0;
          g_1092_0 |= func_50___trans_tmp_31;
        }
      }
  }
}
