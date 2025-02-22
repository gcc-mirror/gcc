/* { dg-do compile } */
/* { dg-options "-O3 -fno-omit-frame-pointer -fno-toplevel-reorder" } */

struct S1
{
  int f1 : 17;
  int f2 : 23;
  int f3 : 11;
  int f4 : 31;
  int f6;
};
volatile struct S1 **g_1680;
volatile struct S1 ***g_1679[8][8];
void
func_40 (struct S1 p_41, short *l_2436)
{
  char __trans_tmp_3;
  __trans_tmp_3 = p_41.f6;
  *l_2436 ^= __trans_tmp_3;
  for (int i = 0; i < 8; i+= 1)
    g_1679[1][i] = &g_1680;
}
