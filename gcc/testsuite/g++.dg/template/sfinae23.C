// PR c++/44969
// { dg-options "-std=c++0x" }

template<typename Tp, typename... Args>
  class mini_is_constructible
  {
    typedef char one;
    typedef struct { char arr[2]; } two;

    template<typename Tp1, typename... Args1>
      static decltype(Tp1(Args1()...), one())
      test(int);

    template<typename, typename...>
      static two test(...);

  public:
    static const bool value = sizeof(test<Tp, Args...>(0)) == 1;
  };

class A { };

int Test[mini_is_constructible<int, A, A>::value ? -1 : 1];
