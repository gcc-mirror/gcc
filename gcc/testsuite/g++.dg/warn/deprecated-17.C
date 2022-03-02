// PR c++/104682
// { dg-do compile { target c++11 } }

template<typename>
struct S {
  enum E1 {
    A __attribute__((deprecated)),
    B __attribute__((deprecated("B"))),
    C [[deprecated]],
    D [[deprecated("D")]]
  };
};

struct S2 {
  enum E2 {
    A __attribute__((deprecated)),
    B __attribute__((deprecated("B"))),
    C [[deprecated]],
    D [[deprecated("D")]]
  };
};

void
g ()
{
  auto a1 = S<int>::E1::A; // { dg-warning "is deprecated" }
  auto a2 = S<int>::E1::B; // { dg-warning "is deprecated" }
  auto a3 = S<int>::E1::C; // { dg-warning "is deprecated" }
  auto a4 = S<int>::E1::D; // { dg-warning "is deprecated" }
  
  auto b1 = S2::A; // { dg-warning "is deprecated" }
  auto b2 = S2::B; // { dg-warning "is deprecated" }
  auto b3 = S2::C; // { dg-warning "is deprecated" }
  auto b4 = S2::D; // { dg-warning "is deprecated" }
}
