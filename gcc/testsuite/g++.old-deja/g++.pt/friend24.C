// Build don't link:

template <class T>
struct S
{
  template <class U = T>
  friend class S;

  void f(T);
};

template struct S<int>;

void g()
{
  S<> s;
  s.f(3);
}
