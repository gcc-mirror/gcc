/* { dg-do compile } */
/* { dg-add-options vect_early_break } */
/* { dg-additional-options "-O3" } */

short g_113;
int func_1_l_1273, func_1_l_1370, func_1_l_1258;
void func_1() {
  int l_1375;
  for (; l_1375; l_1375--) {
    for (; func_1_l_1370;)
      ;
    func_1_l_1273 &= !0;
    func_1_l_1273 &= g_113;
    if (func_1_l_1258)
      break;
  }
}
