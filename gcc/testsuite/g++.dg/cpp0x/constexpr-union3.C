// PR c++/51675
// { dg-options -std=c++11 }

union foo
{
  int x;
  short y;

  constexpr foo(): x(0) { }
};

constexpr foo f;
