// { dg-do run }
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

class VD1
: public virtual A1 { };

class VD2
: protected virtual A1 { };

class VD3
: private virtual A1 { };

class D
: public B, public VD1 { };

class VDMultiple1
: public virtual A1, public A2 { };

class VDMultiple2
: public virtual A1, public virtual A2 { };

template <typename T>
class VDTemplate : public virtual T { };

// example from from [class.mi]
namespace class_mi
{
class B { int b; };
class X : virtual public B { int x; };
class Y : virtual public B { int y; };
class Z : public B { int z; };
class AA : public X, public Y, public Z { int aa; };
}

union U
{ 
  double a;
  double b;
};

template<typename T, typename U>
  bool
  f()
  { return __builtin_is_virtual_base_of(T, U); } 

template<typename T, typename U>
  class My
  {
  public:
    bool
    f()
    { return !!__builtin_is_virtual_base_of(T, U); }
  };

template<typename T, typename U>
  class My2
  {
  public:
    static const bool trait = __builtin_is_virtual_base_of(T, U);
  };

template<typename T, typename U>
  const bool My2<T, U>::trait;

template<typename T, typename U, bool b = __builtin_is_virtual_base_of(T, U)>
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

#define PTEST(T, U) (__builtin_is_virtual_base_of(T, U) && f<T, U>() \
                  && My<T, U>().f() && My2<T, U>::trait && My3<T, U>().f())

#define NTEST(T, U) (!__builtin_is_virtual_base_of(T, U) && !f<T, U>() \
                  && !My<T, U>().f() && !My2<T, U>::trait && !My3<T, U>().f())

int main()
{
  assert (NTEST (int, A1));
  assert (NTEST (A1, void));
  assert (NTEST (A1, A1));
  assert (NTEST (A1*, A1*));
  assert (NTEST (A1&, A1&));
  assert (NTEST (A1, B));
  assert (NTEST (B, A1));
  assert (NTEST (A1, C));
  assert (NTEST (A2, C));
  assert (NTEST (C, A1));
  assert (NTEST (A1, const B));
  assert (NTEST (const B, A1));
  assert (NTEST (A1, volatile C));
  assert (NTEST (volatile A2, const C));
  assert (NTEST (const volatile C, A1));
  
  assert (PTEST (A1, VD1));
  assert (PTEST (A1, VD2));
  assert (PTEST (A1, VD3));
  
  assert (PTEST (A1, const VD1));
  assert (PTEST (A1, const VD2));
  assert (PTEST (A1, const VD3));
  
  assert (PTEST (const A1, VD1));
  assert (PTEST (const A1, VD2));
  assert (PTEST (const A1, VD3));
  
  assert (PTEST (const A1, const VD1));
  assert (PTEST (const A1, const VD2));
  assert (PTEST (const A1, const VD3));
  
  assert (NTEST (A2, VD1));
  assert (NTEST (A2, VD2));
  assert (NTEST (A2, VD3));
  
  assert (PTEST (A1, D));
  
  assert (PTEST (A1, VDMultiple1));
  assert (PTEST (A1, VDMultiple2));
  assert (NTEST (A2, VDMultiple1));
  assert (PTEST (A2, VDMultiple2));  
  
  assert (PTEST (A1, VDTemplate<A1>));
  assert (NTEST (A2, VDTemplate<A1>));
  assert (NTEST (A1, VDTemplate<A2>));

  assert (NTEST (class_mi::B, class_mi::B));
  assert (PTEST (class_mi::B, class_mi::X));
  assert (PTEST (class_mi::B, class_mi::Y));
  assert (NTEST (class_mi::B, class_mi::Z));
  assert (PTEST (class_mi::B, class_mi::AA));

  assert (NTEST (U, U));

  return 0;
}
