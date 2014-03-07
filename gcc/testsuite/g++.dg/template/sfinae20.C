// PR c++/44907
// { dg-do compile { target c++11 } }

#include <utility>

struct A { };

struct B
: public A { };

struct C
: public A { };

struct D
: public B, public C { };

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

static_assert (!mini_is_convertible<D*, A*>::value, "");
static_assert (!mini_is_convertible<A*, D*>::value, "");
static_assert (!mini_is_convertible<D&, A&>::value, "");
static_assert (!mini_is_convertible<A&, D&>::value, "");
static_assert (!mini_is_convertible<D, A>::value, "");
static_assert (!mini_is_convertible<A, D>::value, "");
