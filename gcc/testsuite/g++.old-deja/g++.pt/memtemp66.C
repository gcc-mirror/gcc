// { dg-do assemble  }

template <class T>
struct S
{
  template <class U>
  void f(U u) { this->template f<>(3); }
};


void g()
{
  S<char> s;
  s.f(1.0);
}
