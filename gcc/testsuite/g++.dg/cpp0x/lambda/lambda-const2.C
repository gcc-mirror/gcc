// PR c++/52026
// { dg-options "-std=c++11 -O" }
// { dg-do run }

template<bool B>
int func() {
  const int constVal1 = B ? 100 : -100;
  const int constVal = constVal1;
  return [] { return constVal; }();
}

int main() {
  if (func<true>() != 100)
    __builtin_abort ();
}
