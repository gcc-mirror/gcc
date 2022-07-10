/* { dg-do compile } */
/* { dg-additional-options "-funswitch-loops" } */

short var_32;
int test_var_0;
unsigned char test_var_6;
char test_var_13;
void test(int var_2)
{
  for (;;)
    for (short i_7; i_7 < test_var_13; i_7 += 1)
      for (; test_var_0;) {
        for (; var_2;)
          var_32 = 0;
        for (char i_19; i_19 < test_var_6 + 135; i_19 += 200)
          ;
      }
}
