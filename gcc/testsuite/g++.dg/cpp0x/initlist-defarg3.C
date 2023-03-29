// PR c++/106925
// { dg-do compile { target c++11 } }

struct Foo;
template <int _Nm> struct __array_traits { typedef Foo _Type[_Nm]; };
template <int _Nm> struct array {
  typename __array_traits<_Nm>::_Type _M_elems;
};
template <int size> struct MyVector { array<size> data{}; };
struct Foo {
  float a{0};
};
void foo(MyVector<1> = MyVector<1>());
