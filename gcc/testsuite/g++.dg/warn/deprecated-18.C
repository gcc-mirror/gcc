// PR c++/104682
// { dg-do compile { target c++11 } }

template<typename>
struct S {
  enum B {
    A
  } __attribute__((deprecated)) ;
};

struct S2 {
  enum B {
    A
  } __attribute__((deprecated));
};

template<typename>
struct S3 {
  enum [[deprecated]] B {
    A
  };
};

struct S4 {
  enum [[deprecated]] B {
    A
  };
};

void
g ()
{
  S<int>::B a1; // { dg-warning "is deprecated" }
  S2::B a2; // { dg-warning "is deprecated" }
  S3<int>::B a3; // { dg-warning "is deprecated" }
  S4::B a4; // { dg-warning "is deprecated" }
}
