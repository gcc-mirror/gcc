// { dg-do "run" }
#include <cassert>

struct A
{
  double a;
  double b;
};

union U
{
  double a;
  double b;
};

struct B
{
  B& operator=(const B&) { return *this;}
};

struct C
{
  virtual int f() { return 1; }
};

struct D 
: public B { };

struct E
: public A { };

struct F
{
  A a;
};

struct G
{
  B b;
};

template<typename T>
  bool
  f()
  { return __has_trivial_assign(T); } 

template<typename T>
  class My
  {
  public:
    bool
    f()
    { return !!__has_trivial_assign(T); }
  };

template<typename T>
  class My2
  {
  public:
    static const bool trait = __has_trivial_assign(T);
  };

template<typename T>
  const bool My2<T>::trait;

template<typename T, bool b = __has_trivial_assign(T)>
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

#define PTEST(T) (__has_trivial_assign(T) && f<T>() \
                  && My<T>().f() && My2<T>::trait && My3<T>().f())

#define NTEST(T) (!__has_trivial_assign(T) && !f<T>() \
                  && !My<T>().f() && !My2<T>::trait && !My3<T>().f())

int main()
{
  assert (PTEST (int));
  assert (NTEST (int (int)));
  assert (NTEST (void));
  assert (PTEST (A));
  assert (PTEST (U));
  assert (NTEST (B));
  assert (NTEST (C));
  assert (NTEST (D));
  assert (PTEST (E));
  assert (NTEST (E[]));
  assert (PTEST (F));
  assert (NTEST (G));
  assert (NTEST (const A));
  assert (NTEST (A&));
  
  return 0;
}
