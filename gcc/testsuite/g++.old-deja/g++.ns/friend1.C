// { dg-do assemble  }
namespace A{
  void f(int);
}

class X{
  friend void A::f(int);
};
