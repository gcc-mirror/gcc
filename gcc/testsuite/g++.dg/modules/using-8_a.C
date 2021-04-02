// { dg-additional-options "-fmodules-ts -Wno-pedantic" }
module;
# 4 __FILE__ 1
namespace A
{
void swap (int &, int &);
void copy (int &);
}
# 10 "" 2
export module bob;
// { dg-module-cmi bob }

export template <typename T>
void Foo (T & a, T &b)
{
  using A::swap;
  swap (a, b);

  using A::copy;
  copy (b);
}
