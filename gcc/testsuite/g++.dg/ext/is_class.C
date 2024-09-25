// { dg-do run }
// { dg-skip-if "requires hosted libstdc++ for cassert" { ! hostedlib } }
#include <cassert>

struct A
{
  double a;
  double b;
};

class B
{
  B() { }
};

union U
{
  double a;
  double b;
};

template<typename T>
  bool
  f()
  { return __is_class(T); } 

template<typename T>
  class My
  {
  public:
    bool
    f()
    { return !!__is_class(T); }
  };

template<typename T>
  class My2
  {
  public:
    static const bool trait = __is_class(T);
  };

template<typename T>
  const bool My2<T>::trait;

template<typename T, bool b = __is_class(T)>
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

#define PTEST(T) (__is_class(T) && f<T>() \
                  && My<T>().f() && My2<T>::trait && My3<T>().f())

#define NTEST(T) (!__is_class(T) && !f<T>() \
                  && !My<T>().f() && !My2<T>::trait && !My3<T>().f())

int main()
{
  assert (NTEST (int));
  assert (NTEST (void));
  assert (PTEST (A));
  assert (PTEST (B));
  assert (NTEST (U));

  return 0;
}
