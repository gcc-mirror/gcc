// PR c++/118124
// { dg-do compile { target c++11 } }
// { dg-options "-O2" }
// non-ASCII chars here: áéí

namespace std {
template <class T> struct initializer_list {
private:
  const T *_M_array;
  __SIZE_TYPE__ _M_len;
};
}
struct A {
  A (std::initializer_list<signed char>);
};
A a {
#embed __FILE__
};	// { dg-error "narrowing conversion of '\[0-9]*' from 'int' to 'signed char'" }
