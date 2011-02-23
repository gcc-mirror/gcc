// { dg-do run }
#include <cassert>

struct A
{
  double a;
  double b;
};

struct B
{
  A a;
};

struct C
: public A { };

struct D
{
  D(const D&) throw() { }
};

struct E
{
  E(const E&) throw(int) { }
};

struct E1
{
  E1(const E1&) throw(int) { throw int(); }
};

struct F
{
  F() throw() { }
};

struct G
{
  G() throw(int) { throw int(); }
};

struct H
{
  H(H&) throw(int) { }
};

struct H1
{
  H1(H1&) throw(int) { throw int(); }
};

struct I
{
  I(I&) throw(int) { }
  I(const I&) throw() { }
};

struct I1
{
  I1(I1&) throw(int) { throw int(); }
  I1(const I1&) throw() { }
};

struct J
{
  J(J&) throw() { }
  J(const J&) throw() { }
  J(volatile J&) throw() { }
  J(const volatile J&) throw() { }
};

template<typename T>
  bool
  f()
  { return __has_nothrow_copy(T); } 

template<typename T>
  class My
  {
  public:
    bool
    f()
    { return !!__has_nothrow_copy(T); }
  };

template<typename T>
  class My2
  {
  public:
    static const bool trait = __has_nothrow_copy(T);
  };

template<typename T>
  const bool My2<T>::trait;

template<typename T, bool b = __has_nothrow_copy(T)>
  struct My3_help
  { static const bool trait = b; };

template<typename T, bool b>
  const bool My3_help<T, b>::trait;

template<typename T>
  class My3
  {
  public:
    bool
    f()
    { return My3_help<T>::trait; }
  };

#define PTEST(T) (__has_nothrow_copy(T) && f<T>() \
                  && My<T>().f() && My2<T>::trait && My3<T>().f())

#define NTEST(T) (!__has_nothrow_copy(T) && !f<T>() \
                  && !My<T>().f() && !My2<T>::trait && !My3<T>().f())

int main()
{
  assert (PTEST (int));
  assert (NTEST (int (int)));
  assert (NTEST (void));
  assert (PTEST (A));
  assert (PTEST (B));
  assert (PTEST (C));
  assert (PTEST (C[]));
  assert (PTEST (D));
  assert (NTEST (E));
  assert (NTEST (E1));
  assert (PTEST (F));
  assert (PTEST (G));
  assert (NTEST (H));
  assert (NTEST (H1));
  assert (NTEST (I));
  assert (NTEST (I1));  
  assert (PTEST (J));

  return 0;
}
