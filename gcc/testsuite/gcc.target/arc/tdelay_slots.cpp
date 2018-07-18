/* { dg-do assemble } */
/* { dg-skip-if "" { ! { clmcpu } } } */
/* { dg-options "-O2 -mcpu=em" } */

template <class> struct A;
int a;
template <> struct A<char> {
  typedef int int_type;
  static int_type eof();
};
template <> struct A<wchar_t> {
  typedef int int_type;
  static int_type eof() { return -1; }
};
class basic_streambuf {
public:
  virtual ~basic_streambuf();
};
class B {
  void tie();
  class C {
    C();
  };
};
template <typename _CharT, typename _Traits = A<_CharT>>
class D : basic_streambuf {
  typedef _Traits traits_type;
  typename traits_type::int_type _M_unget_buf;

public:
  D(void *) : _M_unget_buf(traits_type::eof()) {}
};

extern D<wchar_t> b;
B c;
void *operator new(unsigned, void *p2) { return p2; }

B::C::C() {
  new D<char>(&a);
  c.tie();
  new (&b) D<wchar_t>(&a);
}
