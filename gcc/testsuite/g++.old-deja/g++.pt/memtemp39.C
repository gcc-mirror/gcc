// Build don't link:
// GROUPS passed templates membertemplates
template <class T>
struct S
{
  template <class U>
  void f(U u) { g(u); }

  template <class U>
  void g(U u) { f(u); }
};

void foo()
{
  S<int> si;
  si.f(3);
}
