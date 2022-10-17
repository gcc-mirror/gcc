// PR c++/98486
// { dg-do compile { target c++20 } }

template<class T, class U> concept C = __is_same(T, U);

template<class T>
struct A {
  template<C<T>> static int v;
};

template<> template<> int A<int>::v<int>;
template<> template<> int A<int>::v<char>; // { dg-error "match" }

int x = A<int>::v<int>;
int y = A<int>::v<char>; // { dg-error "invalid" }
