// Build don't link:
// GROUPS passed templates membertemplates
struct S
{
  template <class T>
  void f(T t1, T t = T())
  {}
};


void foo()
{
  S si;
  si.f(3);
}
