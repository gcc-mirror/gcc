// { dg-do run }
// { dg-additional-options "-O3" }

unsigned long long int var_4 = 235;
unsigned long long int var_5 = 74;
signed char var_12 = -99;
unsigned long long int var_349;
unsigned char var_645;
void test();

const unsigned long long &min(const unsigned long long &a,
			      const unsigned long long &b)
{
  return b < a ? b : a;
}

void __attribute__((noipa)) test()
{
  for (short c = var_12; c; c += 5)
    ;
  for (int e = 0; e < 12; e += 1) {
      var_349 = var_4 ? 235 : 74;
      var_645 = min((unsigned long long)true, var_5 ? var_12 : var_4);
  }
}

int main()
{
  test();
  if (var_645 != 1)
    __builtin_abort();
}
