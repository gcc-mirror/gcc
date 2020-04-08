// PR c++/79937
// This is a constexpr adaptation of pr79937-3.C and pr79937-4.C.
// { dg-do compile { target c++14 } }

struct X {
  unsigned i;
  unsigned n = i;
};

constexpr X bar(X x) {
  return x;
}

struct Y
{
  static constexpr Y bar(Y y) { return y; }
  unsigned i;
  unsigned n = bar(Y{2,i}).n;
};

constexpr X x { 1, bar(X{2}).n };
static_assert(x.n == 2, "");

constexpr Y y { 1 };
static_assert(y.n == 1, "");

struct Z {
  unsigned i;
  unsigned n = i;
  unsigned m = i;
};

constexpr Z
baz (Z z)
{
  if (z.i != 1 || z.n != 2 || z.m != 1)
    __builtin_abort ();
  return z;
}

constexpr Z z = baz (Z {1, Z {2}.n});
static_assert(z.i == 1 && z.n == 2 && z.m == 1, "");
