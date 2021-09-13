// PR c++/65816
// { dg-do compile { target c++20 } }

struct X {
  int i;
  X() = default;
  constexpr X(int) { }
};

struct Y : X {
  constexpr Y() : X() { }
};

static_assert(Y().i == 0);
