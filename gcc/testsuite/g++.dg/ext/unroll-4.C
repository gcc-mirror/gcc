// PR c++/111529
// { dg-do compile { target c++11 } }
// { dg-additional-options -Wno-c++20-extensions }

template <int>
void f() {
  []<int>() {
    #pragma GCC unroll 9
    for (int i = 1; i; --i) {
    }
  };
}

int main() {
  f<0>();
}
