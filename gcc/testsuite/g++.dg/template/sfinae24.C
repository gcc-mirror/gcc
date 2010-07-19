// PR c++/44969

template<bool, typename T = void> struct enable_if { typedef T type; };
template<typename T> struct enable_if<false, T> { };

template<typename Tp, typename Arg1, typename Arg2>
  class mini_is_constructible
  {
    typedef char one;
    typedef struct { char arr[2]; } two;

    template<typename Tp1, typename Arg1_, typename Arg2_>
      static typename
      enable_if<(sizeof(Tp1(Arg1_(), Arg2_()), 1) > 0), one>::type
      test(int);

    template<typename, typename, typename>
      static two test(...);

  public:
    static const bool value = sizeof(test<Tp, Arg1, Arg2>(0)) == 1;
  };

class A { };

int Test[mini_is_constructible<int, A, A>::value ? -1 : 1];
