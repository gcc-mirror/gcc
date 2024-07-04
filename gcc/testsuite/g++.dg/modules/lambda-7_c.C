// { dg-module-do run }
// { dg-additional-options "-fmodules-ts -fno-module-lazy -Wno-subobject-linkage" }

import "lambda-7_a.H";

int main() {
  S s;
  if (s.a(10) != 20)
    __builtin_abort();
  if (s.b(10) != 30)
    __builtin_abort();
  if (s.c(10) != 40)
    __builtin_abort();
  if (d(10) != 50)
    __builtin_abort();

#if __cplusplus >= 202002L
  E e;
  if (e(10) != 60)
    __builtin_abort();
  if (e.f(10) != 70)
    __builtin_abort();

  G<int> g1;
  if (g1(10) != 80)
    __builtin_abort();
  if (g1.h(10) != 90)
    __builtin_abort();

  G<double> g2;
  if (g2(10) != 100)
    __builtin_abort();
  if (g2.i(10) != 110)
    __builtin_abort();
#endif

#if __cpp_concepts >= 201907L
  static_assert(J<char>);
  static_assert(K<char>);
#endif
}
