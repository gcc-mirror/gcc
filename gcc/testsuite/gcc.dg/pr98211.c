/* { dg-do run } */
/* { dg-options "-std=gnu90 -O3 -fgimple" } */

int test_var_3;
short arr_20[16];
void __GIMPLE (ssa,startwith("slp"))
test (int var_1, short int a, short int b, short int c, short int d)
{
  _Bool tem2;
  _Bool tem;
  unsigned int i_5;
  int _24;
  _Bool _28;
  short int _30;
  short int _32;
  _Bool _29;
  _Bool _31;

  __BB(2):
  _24 = test_var_3;
  tem_25 = _24 != 0;
  tem2_26 = var_1_11(D) != 0;
  _28 = tem_25 | tem2_26;
  _29 = _28 !=  _Literal (_Bool) 0;
  _30 = _29 ? a_16(D) : b_15(D);
  arr_20[0u] = _30;
  _31 = _28 != _Literal (_Bool) 0;
  _32 = _31 ? c_19(D) : d_18(D);
  arr_20[8u] = _32;
  arr_20[1u] = _30;
  arr_20[9u] = _32;
  arr_20[2u] = _30;
  arr_20[10u] = _32;
  arr_20[3u] = _30;
  arr_20[11u] = _32;
  arr_20[4u] = _30;
  arr_20[12u] = _32;
  arr_20[5u] = _30;
  arr_20[13u] = _32;
  arr_20[6u] = _30;
  arr_20[14u] = _32;
  arr_20[7u] = _30;
  arr_20[15u] = _32;
  return;
}


int
main()
{
  test (1, 0x88, 0x77, 0x77, 0x88);
  if (arr_20[0] != 0x88)
    __builtin_abort ();
  return 0;
}
