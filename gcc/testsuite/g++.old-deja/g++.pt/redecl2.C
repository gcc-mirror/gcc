// { dg-do assemble  }

struct A 
{
  template <class A>
  void f(A) {}
};

void g()
{
  A a;
  a.f(3);
}

