// { dg-do assemble  }
namespace A{
  void foo();
}


namespace B{
class B{
  friend void A::foo();
};
}
