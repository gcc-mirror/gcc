// PR c++/98614
// { dg-do compile { target c++20 } }

template<class T>
struct A;

template<class T> requires true
struct A<T> {
  A(A<T> const&) = default;
};
