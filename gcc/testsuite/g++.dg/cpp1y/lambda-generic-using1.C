// PR c++/88123
// { dg-do compile { target c++14 } }

struct bar {};
struct baz {};
struct baq {};

namespace foo
{
  void operator+(bar);
} // namespace foo

namespace foo2
{
  void operator-(baz);
}  

auto fn() {
  using foo::operator+;
  using namespace foo2;
  extern void operator!(baq);
  return [](auto x, auto y, auto z) { +x; -y; !z; };
}

int main()
{
  auto l = fn();
  l(bar(),baz(),baq());
}
