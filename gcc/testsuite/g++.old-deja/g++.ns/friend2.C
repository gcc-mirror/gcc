//Build don't link:
namespace A{
  void foo();
}


namespace B{
class B{
  friend void A::foo();
};
}
