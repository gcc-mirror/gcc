// PR c++/91264
// { dg-do compile { target c++14 } }

struct X {
  int j;
  constexpr X() : j(0) { }
};

struct Y {
  X x;
  constexpr Y() : x{} { }
};

constexpr void
g ()
{
  const Y y; // { dg-message "originally declared" }
  Y *p = const_cast<Y *>(&y);
  p->x.j = 99; // { dg-error "modifying a const object" }
}

static_assert((g() , 1), ""); // { dg-error "non-constant condition" }
// { dg-message "in 'constexpr' expansion of" "" { target *-*-* } .-1 }
