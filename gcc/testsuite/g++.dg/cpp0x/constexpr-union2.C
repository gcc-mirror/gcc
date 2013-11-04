// PR c++/51675
// { dg-options -std=c++11 }

union foo
{
  int x = 0;
  short y;

  constexpr foo() = default;
};

union bar
{
  int x;
  short y;

  constexpr bar() = default;	// { dg-error "constexpr" }
};
