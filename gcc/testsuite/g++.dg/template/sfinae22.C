// PR c++/44908
// { dg-options "-std=c++0x" }

#include <utility> 

struct A { };

struct B
: public virtual A { };

template<typename From, typename To>
  class mini_is_convertible
  {
    typedef char one;
    typedef struct { char arr[2]; } two;

    template<typename To1>
      static void test_aux(To1);

    template<typename To1, typename From1>
      static decltype(test_aux<To1>(std::declval<From1>()), one())
      test(int);

    template<typename, typename>
      static two test(...);

    public:
      static const bool value = sizeof(test<To, From>(0)) == 1;
  }; 

template<typename From, typename To>
  const bool mini_is_convertible<From, To>::value;

static_assert (!mini_is_convertible<int (B::*) (int),
	       int (A::*) (int)>::value, "");
static_assert (!mini_is_convertible<int (B::*), int (A::*)>::value, "");
static_assert (!mini_is_convertible<int (A::*) (int),
	       int (B::*) (int)>::value, ""); 
static_assert (!mini_is_convertible<int (A::*), int (B::*)>::value, "");
