// { dg-options -std=c++1y }

template <class T>
constexpr auto f(T t) { return t+1; }

#define SA(X) static_assert((X),#X)
SA(f(1)==2);
