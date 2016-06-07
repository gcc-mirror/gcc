// { dg-do compile { target i?86-*-* x86_64-*-* } }
// { dg-options "-std=c++11 -O3 -march=ivybridge" }

#include <functional>

extern int le_s6, le_s9, le_s11;
long foo_v14[16][16];

void fn1() {
  std::array<std::array<int, 16>, 16> v13;
  for (; le_s6;)
    for (int k1 = 2; k1 < 4; k1 = k1 + 1) {
      for (int n1 = 0; n1 < le_s9; n1 = 8) {
        *foo_v14[6] = 20923310;
        for (int i2 = n1; i2 < n1 + 8; i2 = i2 + 1)
          v13.at(5).at(i2 + 6 - n1) = 306146921;
      }

      for (int l2 = 0; l2 < le_s11; l2 = l2 + 1)
          *(l2 + v13.at(5).begin()) = 306146921;
    }
  v13.at(le_s6 - 4);
}
