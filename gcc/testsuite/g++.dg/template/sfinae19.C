// PR c++/44907

struct A { };

struct B
: public A { };

struct C
: public A { };

struct D
: public B, public C { };

template<bool, typename T = void> struct enable_if { typedef T type; };
template<typename T> struct enable_if<false, T> { };

template<typename From, typename To>
  class mini_is_convertible
  {
    typedef char one;
    typedef struct { char arr[2]; } two;

    template<typename To1>
      static void test_aux(To1);

    template<typename To1, typename From1>
      static typename
      enable_if<(sizeof(test_aux<To1>(From1()), 1) > 0), one>::type
      test(int);

    template<typename, typename>
      static two test(...);

    public:
      static const bool value = sizeof(test<To, From>(0)) == 1;
  }; 

template<typename From, typename To>
  const bool mini_is_convertible<From, To>::value;

int Test1[mini_is_convertible<D*, A*>::value ? -1 : 1]; 
int Test2[mini_is_convertible<A*, D*>::value ? -1 : 1];
int Test3[mini_is_convertible<D, A>::value ? -1 : 1]; 
int Test4[mini_is_convertible<A, D>::value ? -1 : 1];
