// PR tree-optimization/71655
// { dg-do compile }
// { dg-options "-O3 -std=c++11" }
// { dg-additional-options "-msse4" { target i?86-*-* x86_64-*-* } }

#include <functional>
#include <valarray>
extern int var_16, le_s5, le_s6, le_s9;
std::array<std::array<std::array<long, 8>, 4>, 24> v4;
extern std::array<std::array<int, 48>, 18> v15;

void fn1() {
  for (int k0 = 0;;)
    for (int i1 = 0;;)
      for (int j1 = 0; j1 < le_s9; j1 = j1 + 1) {
	std::valarray<std::valarray<short>> v15_;
        for (; le_s5;) {
	  std::array<std::array<std::array<int, 3>, 48>, 18> v16;
          for (int k2 = 0;; k2 = 1)
            for (int l2 = 2; l2 < 6; l2 = l2 + 1)
              for (int k3 = 0; le_s6;)
                for (int i4 = 0; i4 < le_s9; i4 = i4 + 1)
                  *(i4 + (*v16.begin())[k3].begin()) =
		    (v15[k2][l2] || var_16) >
		    unsigned(i1 <= (*v4.begin()).at(k0).at(j1));
        }
      }
}
