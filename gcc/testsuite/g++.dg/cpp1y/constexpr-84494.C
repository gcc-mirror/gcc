// PR c++/84494
// { dg-do compile { target c++14 } }

struct X {
  constexpr X() = default;
  constexpr X(int x) : m_value(x) {}
  constexpr X& operator=(const X &o) = default;
  int m_value {};
};

static_assert((X() = X(10)).m_value == 10, "");
