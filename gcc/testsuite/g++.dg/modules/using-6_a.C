// { dg-additional-options -fmodules-ts }

export module bob;
// { dg-module-cmi bob }

namespace A
{
export void swap (int &, int &);
#if 0
void copy (int &);
#endif
}

export template <typename T>
void Foo (T & a, T &b)
{
  using A::swap;
  swap (a, b);

#if 0
  // FIXME: name lookup should always pay attention to path of
  // instantiation, not just ADL
  using A::copy;
#endif
}
