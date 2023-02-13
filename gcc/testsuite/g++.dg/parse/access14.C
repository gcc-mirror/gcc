// PR c++/108275

struct A {
  int i;
private:
  int j;
};

template<int A::* V>
struct B {
  struct C { };
private:
  template<int N> struct D { };
};

struct B<&A::j> b;       // { dg-error "private" }
struct B<&A::j>::C c;    // { dg-error "private" }
struct B<&A::i>::D<0> d; // { dg-error "private" }
