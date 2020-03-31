// PR c++/94205
// { dg-do compile { target c++17 } }

struct S
{
  int i;
  int a = [this] { this->i = 5; return 6; } ();
};


constexpr S s = {};

static_assert (s.i == 5 && s.a == 6);
