/* { dg-do compile } */
/* { dg-options "-O3" } */

struct S0 {
  int f0;
  short f1;
  unsigned f2 : 7;
  short f3;
} func_2_l_27;
int *g_389;
int safe_sub_func_int16_t_s_s(void);
void safe_lshift_func_uint8_t_u_s(int);
void func_23(struct S0 p_24, struct S0 p_25) {
  int *l_1051 = g_389;
  if (safe_sub_func_int16_t_s_s())
    for (;;)
      safe_lshift_func_uint8_t_u_s(p_24.f1);
  *l_1051 = p_25.f0;
}
void func_2(void) {
  struct S0 l_26[2];
  l_26[1].f0 = 4;
  ((long long*)&l_26)[2] = 25770065925;
  func_23(l_26[1], func_2_l_27);
}
