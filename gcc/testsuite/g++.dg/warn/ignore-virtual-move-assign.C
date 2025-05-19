// { dg-do compile { target c++11 } }
// { dg-options "-Wvirtual-move-assign -Wattributes" }

#include <utility>

class A
{
  int val;

public:
  explicit A (int val) : val (val) {}

  A (const A &oth) : val (0) {}
  A &operator= (const A &oth) { return *this; }
  A (A &&oth) : val (oth.val) { oth.val = 0; }
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wvirtual-move-assign"
  A &operator= (A &&oth)
  {
    val += oth.val;
    oth.val = 0;
    return *this;
  }
#pragma GCC diagnostic pop
};

class B : virtual A
{
public:
  B () : A (12) {}
  B &operator= (B &&) = default;
};

class C : virtual A
{
public:
  C () : A (12) {}
};

void
test_fn ()
{
  C x, y;
  x = std::move (y);
}
