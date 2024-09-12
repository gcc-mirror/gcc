// { dg-do run }
// { dg-skip-if "requires hosted libstdc++ for cassert" { ! hostedlib } }
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

#if __cplusplus > 201402L
#define THROW_INT
#else
#define THROW_INT throw(int)	// { dg-warning "deprecated" "" { target { c++11 && { ! c++17 } } } }
#endif

struct D
{
  D(const D&) throw() { }
};

struct E
{
  E(const E&) THROW_INT { }
};

struct E1
{
  E1(const E1&) THROW_INT { throw int(); }
};

struct F
{
  F() throw() { }
};

struct G
{
  G() THROW_INT { throw int(); }
};

struct H
{
  H(H&) THROW_INT { }
};

struct H1
{
  H1(H1&) THROW_INT { throw int(); }
};

struct I
{
  I(I&) THROW_INT { }
  I(const I&) throw() { }
};

struct I1
{
  I1(I1&) THROW_INT { throw int(); }
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
