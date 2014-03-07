// PR c++/52026
// { dg-options "-O" }
// { dg-do run { target c++11 } }

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
