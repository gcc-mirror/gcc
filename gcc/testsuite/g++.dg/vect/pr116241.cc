/* { dg-do compile } */
/* { dg-additional-options "-O3" } */

short var_27;
long test_var_5;
int test_var_6;
void test(short arr_11[][4][24])
{
  for (bool i_6 = 0;;)
    for (int i_7; i_7;)
      for (int i_8; i_8 < test_var_5; i_8 += 1)
        var_27 *= test_var_6 && arr_11[2][1][i_8];
}
