/* { dg-do compile } */
/* { dg-options "-O3" } */

#include <algorithm>

long var_23;
int var_24, test_var_8;
extern bool arr_20[][13];
char arr_21_0_0_0_0_0;
int *test_arr_0;
void test(unsigned long long var_1)
{
  int arr_16;
  for (int i_0 = 0;;)
    for (int i_5; i_5;) {
      for (int i_6 = 0; i_6 < 19; i_6 += 4)
        for (long i_7(test_var_8); i_7; i_7 += 2) {
          arr_20[0][i_7] = arr_21_0_0_0_0_0 = 0;
          var_23 = test_arr_0[0];
        }
      var_24 = std::max((unsigned long long)arr_16,
                        std::min((unsigned long long)5, var_1));
    }
}
