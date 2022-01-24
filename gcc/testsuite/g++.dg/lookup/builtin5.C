// PR c++/37276
// { dg-additional-options -fno-inline }
// { dg-final { scan-assembler "_ZSt5atanhd" } }

namespace std
{
  inline double
  atanh(double __x)
  { return __builtin_atanh(__x); }
}

int main()
{
  std::atanh(.3);
  return 0;
}
