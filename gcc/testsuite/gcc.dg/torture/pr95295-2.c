/* { dg-do compile } */

int a;
int var_4 = 1;
unsigned var_9 = 8;
short arr_272[20];
void test();
int main()
{
  test();
  if (arr_272[4] != 1)
    __builtin_abort ();
  return 0;
}
