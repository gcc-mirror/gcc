// PR c++/57764
// { dg-do compile { target c++11 } }

constexpr int x = 42;
  
struct S
{
  static constexpr int const & y = x;
};

constexpr int const & S::y;
