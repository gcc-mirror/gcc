// { dg-do run }
// { dg-skip-if "requires hosted libstdc++ for cassert" { ! hostedlib } }
#include <cassert>

class A1
{ 
  double a;
  double b;  
};

class A2
{ 
  double a;
  double b;
};

class B
: private A1 { };

class C
: private A1, private A2 { };

union U
{ 
  double a;
  double b;
};

template<typename T, typename U>
  bool
  f()
  { return __is_base_of(T, U); } 

template<typename T, typename U>
  class My
  {
  public:
    bool
    f()
    { return !!__is_base_of(T, U); }
  };

template<typename T, typename U>
  class My2
  {
  public:
    static const bool trait = __is_base_of(T, U);
  };

template<typename T, typename U>
  const bool My2<T, U>::trait;

template<typename T, typename U, bool b = __is_base_of(T, U)>
  struct My3_help
  { static const bool trait = b; };

template<typename T, typename U, bool b>
  const bool My3_help<T, U, b>::trait;

template<typename T, typename U>
  class My3
  {
  public:
    bool
    f()
    { return My3_help<T, U>::trait; }
  };

#define PTEST(T, U) (__is_base_of(T, U) && f<T, U>() \
                  && My<T, U>().f() && My2<T, U>::trait && My3<T, U>().f())

#define NTEST(T, U) (!__is_base_of(T, U) && !f<T, U>() \
                  && !My<T, U>().f() && !My2<T, U>::trait && !My3<T, U>().f())

int main()
{
  assert (NTEST (int, A1));
  assert (NTEST (A1, void));
  assert (PTEST (A1, A1));
  assert (NTEST (A1*, A1*));
  assert (NTEST (A1&, A1&));
  assert (PTEST (A1, B));
  assert (NTEST (B, A1));
  assert (PTEST (A1, C));
  assert (PTEST (A2, C));
  assert (NTEST (C, A1));
  assert (PTEST (A1, const B));
  assert (NTEST (const B, A1));
  assert (PTEST (A1, volatile C));
  assert (PTEST (volatile A2, const C));
  assert (NTEST (const volatile C, A1));
  assert (NTEST (U, U));

  return 0;
}
