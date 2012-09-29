// PR c++/54738
// { dg-do compile { target c++11 } }

template<class T>
T&& declval();

template<class F, class T1, class... Ts>
decltype(((*declval<T1>()).*declval<F>())(declval<Ts>()...))
test1(int);

template<class...>
void test1(...);

template<class F, class T1, class... Ts>
decltype((declval<T1>().*declval<F>())(declval<Ts>()...))
test2(int);

template<class...>
void test2(...);

struct S {};

typedef void (S::*Func)(int) const;
typedef void (S::*Func2)(int);

typedef decltype(test1<Func, S*>(0)) type1a;
typedef decltype(test1<Func, S*&>(0)) type1b;
typedef decltype(test1<Func, S*, int, int>(0)) type1c;
typedef decltype(test1<Func, S*&, int, int>(0)) type1d;

typedef decltype(test2<Func, S>(0)) type2a;
typedef decltype(test2<Func, S&>(0)) type2b;
typedef decltype(test2<Func, S, int, int>(0)) type2c;
typedef decltype(test2<Func, S&, int, int>(0)) type2d;

typedef decltype(test1<Func, S*, S>(0)) type3a;
typedef decltype(test1<Func, S*&, S>(0)) type3b;

typedef decltype(test2<Func, S, S>(0)) type4a;
typedef decltype(test2<Func, S&, S>(0)) type4b;

typedef decltype(test1<Func2, const S*, int>(0)) type5a;
typedef decltype(test1<Func2, const S*&, int>(0)) type5b;

typedef decltype(test2<Func2, const S, int>(0)) type6a;
typedef decltype(test2<Func2, const S&, int>(0)) type6b;
