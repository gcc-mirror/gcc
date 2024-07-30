// PR c++/79900 - ICE in strip_typedefs
// { dg-do compile }
// { dg-options "-Wpadded" }

template <class> struct A;
template <typename> struct B { // { dg-warning "padding struct size to alignment boundary"  "" { target { ! default_packed } } }
  long long _M_off;
  char _M_state;
};
template <> struct A<char> { typedef B<int> pos_type; };
enum _Ios_Openmode {};
struct C {
  typedef _Ios_Openmode openmode;
};
template <typename, typename _Traits> struct D {
  typedef typename _Traits::pos_type pos_type;
  pos_type m_fn1(pos_type, C::openmode);
};
template class D<char, A<char> >;
template <typename _CharT, typename _Traits>
typename D<_CharT, _Traits>::pos_type D<_CharT, _Traits>::m_fn1(pos_type x,
                                                                C::openmode) { return x; }
