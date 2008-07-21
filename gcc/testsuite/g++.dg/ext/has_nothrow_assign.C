// { dg-do "run" }
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
  D& operator=(const D&) throw() { return *this; }
};

struct E
{
  E& operator=(const E&) throw(int) { return *this; }
};

struct E1
{
  E1& operator=(const E1&) throw(int) { throw int(); return *this; }
};

struct F
{
  F() throw(int) { }
};

struct G
{
  G() throw(int) { throw int(); }
};

struct H
{
  H& operator=(H&) throw(int) { return *this; }
};

struct H1
{
  H1& operator=(H1&) throw(int) { throw int(); return *this; }
};

struct I
{
  I& operator=(I&) throw(int) { return *this; }
  I& operator=(const I&) throw() { return *this; }
};

struct I1
{
  I1& operator=(I1&) throw(int) { throw int(); return *this; }
  I1& operator=(const I1&) throw() { return *this; }
};

struct J
{
  J& operator=(J&) throw() { return *this; }
  J& operator=(const J&) throw() { return *this; }
  J& operator=(volatile J&) throw() { return *this; }
  J& operator=(const volatile J&) throw() { return *this; }
};

struct K
{
  K& operator=(K&) throw() { return *this; }
};

struct L
{
  L& operator=(const L&) throw() { return *this; }
};

template<typename T>
  bool
  f()
  { return __has_nothrow_assign(T); } 

template<typename T>
  class My
  {
  public:
    bool
    f()
    { return !!__has_nothrow_assign(T); }
  };

template<typename T>
  class My2
  {
  public:
    static const bool trait = __has_nothrow_assign(T);
  };

template<typename T>
  const bool My2<T>::trait;

template<typename T, bool b = __has_nothrow_assign(T)>
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

#define PTEST(T) (__has_nothrow_assign(T) && f<T>() \
                  && My<T>().f() && My2<T>::trait && My3<T>().f())

#define NTEST(T) (!__has_nothrow_assign(T) && !f<T>() \
                  && !My<T>().f() && !My2<T>::trait && !My3<T>().f())

int main()
{
  assert (PTEST (int));
  assert (NTEST (int (int)));
  assert (NTEST (void));
  assert (PTEST (A));
  assert (PTEST (B));
  assert (PTEST (C));
  assert (NTEST (C[]));
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
  assert (NTEST (const K));
  assert (NTEST (const L));

  return 0;
}
