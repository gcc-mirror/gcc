// { dg-do run }

unsigned long int var_4 = 42;
unsigned long int var_14 = 10;
unsigned char var_16 = 1;
unsigned short var_18 = 0;
short var_75;

inline const int &foo(const int &b, const int &c)
{
  return b < c ? c : b;
}
inline unsigned long &bar(unsigned long &b, unsigned long &c)
{
  return !c ? c : b;
}

void __attribute__((noipa))
test(unsigned long var_4, unsigned long var_14,
     unsigned char var_16, unsigned short var_18)
{
  for (bool h = 0; h < (bool)foo(var_16 ? -7 : 4, var_4 ? var_4 : var_18);
       h = 2)
    var_75 = bar(var_4, var_14);
}

int main()
{
  test(var_4, var_14, var_16, var_18);
  if (var_75 != 42)
    __builtin_abort();
  return 0;
}
