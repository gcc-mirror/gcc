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

#if __cplusplus > 201402L
#define THROW_INT
#else
#define THROW_INT throw(int)	// { dg-warning "deprecated" "" { target { c++11 && { ! c++17 } } } }
#endif

struct C 
: public A { };

struct D
{
  D() throw() { }
};

struct E
{
  E() THROW_INT { }
};

struct E1
{
  E1() THROW_INT { throw int(); }
};

struct F
{
  F(const F&) throw() { }
};

struct G
{
  G(const G&) THROW_INT { throw int(); }
};

template<typename T>
  bool
  f()
  { return __has_nothrow_constructor(T); } 

template<typename T>
  class My
  {
  public:
    bool
    f()
    { return !!__has_nothrow_constructor(T); }
  };

template<typename T>
  class My2
  {
  public:
    static const bool trait = __has_nothrow_constructor(T);
  };

template<typename T>
  const bool My2<T>::trait;


template<typename T, bool b = __has_nothrow_constructor(T)>
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

#define PTEST(T) (__has_nothrow_constructor(T) && f<T>() \
                  && My<T>().f() && My2<T>::trait && My3<T>().f())

#define NTEST(T) (!__has_nothrow_constructor(T) && !f<T>() \
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
  assert (NTEST (F));
  assert (NTEST (G));

  return 0;
}
