/* { dg-do run } */
/* { dg-require-effective-target stdint_types } */

#include <stdint.h>

uint64_t var_0 = 18128133247277979402ULL;
int64_t var_14 = 6557021550272328915LL;
uint64_t var_83 = 10966786425750692026ULL;

void test()
{
  var_14 = var_0 + (_Bool)7;
  var_83 = 1 + (int32_t)var_0; // 1 + 888395530
}

int main()
{
  test();
  if (var_83 != 888395531)
    __builtin_abort ();
  return 0;
}
