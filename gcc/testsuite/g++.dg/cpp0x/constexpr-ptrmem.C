// { dg-do compile { target c++11 } }

struct C { // literal type
  int m;
  int n;
  constexpr C(int m) : m(m), n(-m) {}
  constexpr bool is_neg() { return m < 0; }
};

constexpr bool check1(const C& c, int C:: *pm) { return c.*pm < 0; } // #1

constexpr bool check2(const C* pc, bool (C::*pm)() const) { return
(pc->*pm)(); } // #2

constexpr C c(-1);

static_assert(!check1(c, &C::n), "Error");
static_assert(check1(c, &C::m), "Error");

static_assert(check2(&c, &C::is_neg), "Error");
