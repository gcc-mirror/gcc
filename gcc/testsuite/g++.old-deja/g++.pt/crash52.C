// Build don't link:
// Origin: Mark Mitchell <mark@codesourcery.com>

template <class T>
struct S1
{
  template <class U>
  struct S2
  { 
    S2(U);
  };

  template <class U>
  void f(U u)
    {
      S2<U> s2u(u);
    }
};

void g()
{
  S1<int> s1;
  s1.f(3.0);
}

