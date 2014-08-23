// { dg-do compile { target c++14 } }

template <class T>
constexpr auto f(T t) { return t+1; }

#define SA(X) static_assert((X),#X)
SA(f(1)==2);
