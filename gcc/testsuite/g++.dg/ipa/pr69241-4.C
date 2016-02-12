// PR ipa/69241
// { dg-do compile { target c++11 } }
// { dg-options "-O2 -Wno-return-type" }

template <typename> class A;
struct B {
  using pointer = int *;
};
template <typename _CharT, typename = A<_CharT>> class basic_string {
  long _M_string_length;
  enum { _S_local_capacity = 15 } _M_local_buf[_S_local_capacity];
  B::pointer _M_local_data;

public:
  ~basic_string();
};
template <typename _CharT, typename _Traits, typename _Alloc>
int operator<<(_Traits, basic_string<_CharT, _Alloc>);
class C {
  basic_string<A<char>> _M_string;
};
class D {
  C _M_stringbuf;
};
class F {
  int stream;
  D stream_;
};
class G {
public:
  void operator&(int);
};
class H {
public:
  H(unsigned);
  H(H &&);
  bool m_fn1();
};
class I {
  void m_fn2(const int &&);
  static H m_fn3(const int &);
};
template <typename Functor> void Bind(Functor);
class J {
public:
  static basic_string<char> m_fn4();
};
int a;
void I::m_fn2(const int &&) { Bind(m_fn3); }
H I::m_fn3(const int &) {
  !false ? (void)0 : G() & F() << J::m_fn4();
  H b(a);
  if (b.m_fn1())
    F();
}
