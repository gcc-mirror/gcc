// PR c++/113360
// { dg-do compile { target c++14 } }

constexpr bool init_list()	// { dg-bogus "because" }
{
  int total{};
  for (int x : {1, 2, 3})	// { dg-error "initializer list" }
    total += x;
  return total == 6;
}

static_assert(init_list(), "");	// { dg-error "constant" }
