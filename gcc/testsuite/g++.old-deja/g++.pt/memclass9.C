// { dg-do assemble  }

template <class T>
struct S1
{
  template <class U>
  struct S2
  { 
    S2(U);

    void g() 
      {
	S2<U> s2u (u);
      }

    U& u;
  };

  template <class U>
  void f(U u)
    {
      S2<U> s2u(u);
      s2u.g();
    }
};

void g()
{
  S1<int> s1;
  s1.f(3.0);
}

