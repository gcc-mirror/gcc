/* { dg-do run } */

unsigned int var_2 = 1;
int var_4 = -1;
int var_10 = 4;
unsigned long arr_252;
void __attribute__((noipa)) test() {
  for (int a = 0; a < var_10; a += 2)
    arr_252 = var_2 != (int)var_4 ? (unsigned long)var_4 : (unsigned long)var_2;
}

void test();

int main()
{
  test();
  if (arr_252 != 0xffffffffffffffff)
    __builtin_abort();
  return 0;
}
