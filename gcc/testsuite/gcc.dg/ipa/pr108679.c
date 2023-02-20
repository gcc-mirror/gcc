/* { dg-do compile } */
/* { dg-options "-O2" } */

struct S1 {
  signed f0;
};
struct S2 {
  struct S1 f2;
  short f8;
} g_18;
void safe_lshift_func_int16_t_s_u();
void safe_unary_minus_func_uint64_t_u();
int safe_mul_func_uint8_t_u_u(int, struct S1 p_14);
int g_732, func_6_l_17;
static int *func_12();
static int func_6(struct S2 p_7) { func_12(func_6_l_17, p_7.f2, g_18, 0); }
static int *func_12(int, struct S1 p_14) {
  safe_lshift_func_int16_t_s_u();
  safe_unary_minus_func_uint64_t_u();
  g_732 = safe_mul_func_uint8_t_u_u(0, p_14);
}
int main() {
  struct S2 l_10 = {3};
  func_6(l_10);
}
