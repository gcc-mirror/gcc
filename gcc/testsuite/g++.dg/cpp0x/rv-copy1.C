// PR c++/55017
// { dg-do compile { target c++11 } }

struct S {			// { dg-error "rvalue ref" }
  int&& rr;
  S(int&& rr) : rr(static_cast<int&&>(rr)) {}
};

S s1(13);
S s2 = s1;			// { dg-error "deleted" }
