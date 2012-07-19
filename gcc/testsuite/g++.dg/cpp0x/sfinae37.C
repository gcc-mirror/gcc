// PR c++/51213
// { dg-do compile { target c++11 } }

class C {
  typedef int type;
};

template<class T, class = typename T::type>
auto f(int) -> char;

template<class>
auto f(...) -> char (&)[2];

static_assert(sizeof(f<C>(0)) == 2, "Ouch");

template<class T>
auto g(int) -> decltype(typename T::type(), char());

template<class>
auto g(...) -> char (&)[2];

static_assert(sizeof(g<C>(0)) == 2, "Ouch");
