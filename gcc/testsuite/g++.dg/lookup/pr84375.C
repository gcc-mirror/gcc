// PR c++/84375 ICE after error

void foo()
{
  struct A
  {
    friend void A(); // { dg-error "17:friend declaration .void A\\(\\). in local class without prior local" }
  };
}
