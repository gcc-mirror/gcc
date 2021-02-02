long long int var_3 = -166416893043554447LL;
short var_8 = (short)27092;
unsigned int var_17 = 75036300U;
short arr_165[23];

static long c(long e, long f) { return f ? e : f; }
void __attribute((noipa)) test()
{
  for (int b = 0; b < 19; b = var_17)
    for (int d = (int)(~c(-2147483647 - 1, var_3)) - 2147483647; d < 22; d++)
      arr_165[d] = var_8;
}

int main()
{
  for (unsigned i_3 = 0; i_3 < 23; ++i_3)
    arr_165[i_3] = (short)-8885;
  test();
  if (arr_165[0] != 27092)
    __builtin_abort ();
  return 0;
}
