// PR c++/60771
// { dg-do compile { target c++11 } }

struct A { 
  static constexpr int const& ref = 5;
};
