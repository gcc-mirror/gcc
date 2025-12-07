// PR c++/122819
// { dg-do compile { target c++11 } }

template <typename T> struct basic_streambuf;
using streambuf = basic_streambuf<char>;

struct S {
  void foo();
  template <typename T> void bar();
};

template <typename T> struct X {
  void foo();
};

template <typename T> struct basic_streambuf {
  void qux();

  friend void foo();
  friend void S::foo();
  template <typename U> friend void S::bar();
  template <typename U> friend void X<U>::foo();
  template <typename U> friend void basic_streambuf<U>::qux();
};
extern template struct basic_streambuf<char>;

void foo() {}
void S::foo() {}

// { dg-final { scan-assembler {_Z3foov:} } }
// { dg-final { scan-assembler {_ZN1S3fooEv:} } }
// { dg-final { scan-assembler-not {comdat} } }
