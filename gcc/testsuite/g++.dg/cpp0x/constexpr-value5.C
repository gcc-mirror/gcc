// PR c++/68890
// { dg-do compile { target c++11 } }

class ptr;
template <long _Nm> struct A { typedef ptr _Type[_Nm]; };
template <long _Nm> struct B { typename A<_Nm>::_Type _M_elems; };
template <long N> class FixedVector : B<N> {
public:
  typedef B<1> base;
  constexpr FixedVector() : base(), size_() {}
  char size_;
};
class ptr {
public:
  constexpr ptr() : px_(){};
  int px_;
};
FixedVector<1> a;
