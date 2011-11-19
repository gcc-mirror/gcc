// PR c++/51029

struct A
{
  void foo();
};

struct B : virtual A
{
  template<int> B()
  {
    foo();
  }
};
