// Build don't link:
// GROUPS passed templates membertemplates
template <class X>
struct S
{
  template <class T>
  void f(T t1, T t = T())
  {}
};


void foo()
{
  S<int> si;
  si.f(3);
}
