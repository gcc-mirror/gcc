// PR c++/51137

struct A {};

template<int> struct B : virtual A
{
  void foo()
  {
    (new A(*this))->A::~A();
  }
};
