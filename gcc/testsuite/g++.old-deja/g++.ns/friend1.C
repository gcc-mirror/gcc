// Build don't link:
namespace A{
  void f(int);
}

class X{
  friend void A::f(int);
};
