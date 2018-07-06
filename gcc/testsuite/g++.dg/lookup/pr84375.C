// PR c++/84375 ICE after error

void foo()
{
  struct A
  {
    friend void A(); // { dg-error "local class without prior local" }
  };
}
