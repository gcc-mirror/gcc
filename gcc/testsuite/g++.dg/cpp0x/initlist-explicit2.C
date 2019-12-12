// PR c++/88875
// { dg-do compile { target c++11 } }

#include <initializer_list>

struct X {
  X();
  explicit X(const std::initializer_list<int>& init);
};

struct Y 
{
  X x { 1, 2 }; // error

  Y (int)
    : x {1, 2} // ok
  {
  }
  
};
