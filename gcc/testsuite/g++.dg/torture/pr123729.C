// { dg-do compile }
// { dg-additional-options "-std=gnu++20" }

#include <csetjmp>
#include <iostream>

template <typename T> struct A { using type = int; };
template <typename F, typename... G> struct B {};
template <typename F> struct B<F> { using type = F; };
struct {
  template <typename... F,
            typename Overload = typename B<typename A<F>::type...>::type>
  int operator()(F...) {
    int iArrFld_0_0;
    for (int i = 7; i < 28; ++i)
      ;
    int i1 = 3;
    try {
      for (int i = 7; i < 28; ++i) {
        for (int j = 0; j < 7; ++j)
          if (iArrFld_0_0 == 0) {
            for (int k = 0; k < 20000; ++k)
              std::cout << "Hello, world!" << std::endl;
          }
        jmp_buf env2;
        for (int i2 = 16; i2 < 350; ++i2)
          for (int j2 = 1; j2 < 75; ++j2)
            for (int k2 = 0; k2 < 16; ++k2) {
              int temp2 = i2 + j2 + k2;
              int mod2 = temp2 % 8;
              {
                setjmp(env2) == 0;
                ;
              }
            }
      }
    } catch (const std::exception &e) {
    }
    auto lambda = [](int x) {
      return [x](int y) {
        int z = 0;
        if (y > 0)
          z = 1;
        return z + x;
      };
    };
    for (int i = 0; i < 1000; ++i) {
      int m = 0;
      auto f = lambda(i);
      for (int j = 0; j < 100; ++j)
        m += f(j);
      i1 += m;
    }
    std::cout << "Final value of i1: " << i1 << std::endl;
    return 0;
  }
} a;
int main() {
  auto f = a([] {});
}
