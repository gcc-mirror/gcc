// Build don't link:
// GROUPS passed templates membertemplates
template <class T>
struct S
{
  template <class U>
  void foo(U) {}
};


void f()
{
  S<int> s;
  s.foo(3);
  s.foo("hello");
}
