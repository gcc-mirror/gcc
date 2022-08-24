// PR c++/69585
// { dg-do compile { target c++11 } }
// Test mixing the GNU and standard forms of attributes.

__attribute__((deprecated)) [[maybe_unused]] void f1 ();
[[maybe_unused]] __attribute__((deprecated)) void f2 ();
[[maybe_unused]] __attribute__((deprecated)) [[nodiscard]] int f3 ();
__attribute__((unused)) [[nodiscard]] __attribute__((deprecated)) int f4 ();

struct [[maybe_unused]] __attribute__((aligned)) S1 { double d; };
struct __attribute__((aligned)) [[maybe_unused]] S2 { double d; };

enum E {
  X [[maybe_unused]] __attribute__((unavailable)),
  Y  __attribute__((unavailable)) [[maybe_unused]],
};

void
g ([[maybe_unused]] __attribute__((unavailable)) int i1,
   __attribute__((unavailable)) [[maybe_unused]] int i2)
{
  [[maybe_unused]] __attribute__((aligned)) int i3;
  __attribute__((aligned)) [[maybe_unused]] int i4;

[[maybe_unused]]
lab:  __attribute__((cold));
}
