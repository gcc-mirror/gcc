// PR middle-end/71488
// { dg-do run }
// { dg-options "-O3 -std=c++11" }
// { dg-additional-options "-march=westmere" { target i?86-*-* x86_64-*-* } }
// { dg-require-effective-target c++11 }

#include <valarray>

int var_4 = 1;
long long var_9 = 0;

int main() {
  
  std::valarray<std::valarray<long long>> v10;

  v10.resize(1);
  v10[0].resize(4);

  for (int i = 0; i < 4; i++)
    v10[0][i] = ((var_9 == 0) > unsigned (var_4 == 0)) + (var_9 == 0);

  if (v10[0][0] != 2)
    __builtin_abort ();
}
