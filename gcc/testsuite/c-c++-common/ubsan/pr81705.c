/* { dg-do run } */
/* { dg-options "-fsanitize=undefined -fsanitize-undefined-trap-on-error" } */

int var_4 = -1716607962;
int var_14 = 943738830;
volatile int a;
int main()
{
  //  (-(-1716607962) - 516151698) - -(9403738830)
  a = (-var_4 - 516151698) - -var_14;
  return 0;
}
