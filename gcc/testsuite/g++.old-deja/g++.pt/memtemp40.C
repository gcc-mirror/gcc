// { dg-do assemble  }
// GROUPS passed templates membertemplates
template <class T>
struct R
{
  template <class U>
  void g(U u) {}
};

template <class T>
struct S
{
  template <class U>
  void f(U u) { R<T> r; r.g(u); }
};

void foo()
{
  S<int> si;
  si.f("abc");
}
