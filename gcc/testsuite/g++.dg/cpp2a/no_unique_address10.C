// Make sure [[no_unique_address]] doesn't affect is_standard_layout.
// { dg-do compile { target c++11 } }

struct E1 { }; struct E2 { };
struct A
{
  [[no_unique_address]] E1 e;
};

struct B: A
{
  [[no_unique_address]] E2 e;
};

static_assert(__is_standard_layout (A), "");
static_assert(!__is_standard_layout (B), "");
