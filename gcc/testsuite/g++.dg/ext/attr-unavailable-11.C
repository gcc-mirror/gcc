// PR c++/104682
// { dg-do compile { target c++11 } }

template<typename>
struct S {
  enum B {
    A
  } __attribute__((unavailable)) ;
};

struct S2 {
  enum B {
    A
  } __attribute__((unavailable));
};

void
g ()
{
  S<int>::B a1; // { dg-error "is unavailable" }
  S2::B a2; // { dg-error "is unavailable" }
}
