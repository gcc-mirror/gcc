// { dg-do "run" }
#include <cassert>

struct A
{
  double a;
  double b;
};

class B
{ };

enum E
{
  e0
};

template<typename T>
  bool
  f()
  { return __is_enum(T); } 

template<typename T>
  class My
  {
  public:
    bool
    f()
    { return !!__is_enum(T); }
  };

template<typename T>
  class My2
  {
  public:
    static const bool trait = __is_enum(T);
  };

template<typename T>
  const bool My2<T>::trait;

template<typename T, bool b = __is_enum(T)>
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

#define PTEST(T) (__is_enum(T) && f<T>() \
                  && My<T>().f() && My2<T>::trait && My3<T>().f())

#define NTEST(T) (!__is_enum(T) && !f<T>() \
                  && !My<T>().f() && !My2<T>::trait && !My3<T>().f())

int main()
{
  assert (NTEST (int));
  assert (NTEST (void));
  assert (NTEST (A));
  assert (NTEST (B));
  assert (PTEST (E));

  return 0;
}
