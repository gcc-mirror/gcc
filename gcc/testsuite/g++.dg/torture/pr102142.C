/* { dg-do compile } */

extern short arr_597[];
extern bool arr_601[];
int test_var_13;
void test(short arr_391[][9][2][2]) {
  for (int i_60 = 0; i_60 < 11; i_60 += test_var_13)
    arr_597[22] = arr_601[i_60] = arr_391[0][0][1][4];
}
