// PR c++/115522
// { dg-do compile { target c++11 } }

// Not default constructible.
struct S {
  const int i;
};

static_assert(!__is_trivial(S), "");

struct R {
  int &r;
};

static_assert(!__is_trivial(R), "");
