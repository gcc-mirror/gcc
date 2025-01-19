// PR c++/118124
// { dg-do compile { target c++11 } }
// { dg-options "-O2" }

namespace std {
template <class T> struct initializer_list {
private:
  const T *_M_array;
  __SIZE_TYPE__ _M_len;
};
}
struct A {
  A (std::initializer_list<char>);
};
A a {
#embed __FILE__
};
struct B {
  B (std::initializer_list<unsigned char>);
};
B b {
#embed __FILE__
};
struct C {
  C (std::initializer_list<int>);
};
C c {
#embed __FILE__
};
struct D {
  D (std::initializer_list<float>);
};
D d {
#embed __FILE__
};
