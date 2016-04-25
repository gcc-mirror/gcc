// { dg-do compile { target c++11 } }
// { dg-options "-Wunused -Wextra" }

[[maybe_unused]] static void f() { }

enum [[maybe_unused]] E {
  e [[maybe_unused]]
};

struct [[maybe_unused]] A {
  [[maybe_unused]] static int i;
};

void g([[maybe_unused]] int i) {
  [[maybe_unused]] typedef int T;
  [[maybe_unused]] int j;
}
