// { dg-additional-options -fmodules-ts }

export module bob;
// { dg-module-cmi bob }

namespace A
{
export void swap (int &, int &);
void copy (int &);
}

export template <typename T>
void Foo (T & a, T &b)
{
  using A::swap;
  swap (a, b);

  using A::copy;
  copy (b);
}
