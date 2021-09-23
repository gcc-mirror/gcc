/* PR tree-optimization/101497 */
/* { dg-do compile } */
/* { dg-options "-O2 -Wno-div-by-zero" } */

char uc_1;
int i_4, func_12_uli_6;
void func_12() {
  int *ptr_8 = &func_12_uli_6;
  *ptr_8 = 0 >= 211 - uc_1 <= 0;
  i_4 %= 0;
  i_4 *= *ptr_8;
}

