// PR c++/104682
// { dg-do compile { target c++11 } }

template<typename>
struct S {
  enum E1 {
    A __attribute__((unavailable))
  };
};

struct S2 {
  enum E2 {
    A __attribute__((unavailable))
  };
};

void
g ()
{
  auto a1 = S<int>::E1::A; // { dg-error "is unavailable" }
  auto b1 = S2::E2::A; // { dg-error "is unavailable" }
}
