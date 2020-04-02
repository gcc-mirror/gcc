// PR c++/94205
// { dg-do compile { target c++17 } }

struct S
{
  int a = [this] { this->a = 5; return 6; } ();
};

constexpr S s = {};

static_assert(s.a == 6);
