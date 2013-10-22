// { dg-options -std=c++11 }

struct A
{
  A();
  A(const A&);
  bool empty();
};

constexpr int ellipsis(...) { return 1; }

static_assert(ellipsis(A().empty()), "Error"); // { dg-error "non-constant condition|empty" }
