// PR c++/44967
// { dg-do compile { target c++11 } }

template <typename T> T&& declval();

template<typename T1, typename T2, typename... Args>
struct has_construct
{
    typedef char one;
    typedef struct {char _m[2]; } two;

    template<typename U1, typename U2, typename... Args2>
    static decltype(declval<U1>().construct(declval<U2*>(), declval<Args2>()...), one()) test(int);
    template<typename, typename, typename...>
    static two test(...);

    static const bool value = sizeof(test<T1, T2, Args...>(0)) == 1;
};


struct A0
{};

struct A1
{
    void construct(int*, int);
};

template<typename _Tp>
struct A2
{
  template<typename _Tp1, typename... _Args>
  void construct(_Tp1*, _Args&&...) {}
};

#define SA(X) static_assert(X,#X)
SA((!has_construct<A0, int, int>::value)); // ok
SA((has_construct<A1, int, int>::value)); // bang
SA((has_construct<A2<int>, int>::value)); // bang
