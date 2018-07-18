// PR c++/51925

struct E
{
  int e ();
};
template <typename T1>
struct G : public E
{
  using E::e;
  template <int> void e ();
  void f () { e <0> (); }
};
int f(void)
{
  G<int> a;
  a.f();
  return 0;
}
