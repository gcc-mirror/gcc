// PR c++/94149 - make __is_constructible work with paren-init of aggrs.
// { dg-do compile { target c++11 } }

struct S { };

struct W {
  S& r;
  W(S& r_) : r(r_) {}
  operator S&() { return r; }
};

S s;
W w(s);
S& s2(w);

static_assert(__is_constructible(S&, W), "");
