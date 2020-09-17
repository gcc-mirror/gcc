// PR c++/91525
// { dg-do compile { target c++20 } }

struct X {
  void operator<<(long);
  void operator<<(bool);
} x;
struct B {
  template <bool = true> operator bool();
  template <bool = true> requires false operator bool();
} b;

void
fn()
{
  x << b;
}
