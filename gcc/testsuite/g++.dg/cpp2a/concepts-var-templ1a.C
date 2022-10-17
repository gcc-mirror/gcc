// PR c++/98486
// { dg-do compile { target c++20 } }

template<class T, class U> concept C = __is_same(T, U);

struct A {
  template<C<int>> static int v;
};

template<> int A::v<int>;
template<> int A::v<char>; // { dg-error "match" }

int x = A::v<int>;
int y = A::v<char>; // { dg-error "invalid" }
