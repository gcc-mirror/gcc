// Test for variable templates in pack expansion
// { dg-do compile { target c++14 } }

template <int I> const int Val = I;

constexpr int f () { return 0; }
template <class T, class ...Ts>
constexpr int f(T t, Ts... ts)
{
  return t + f(ts...);
}

template <int... Is>
constexpr int g()
{
  return f(Val<Is>...);
}

#define SA(X) static_assert((X),#X)
SA((g<1,2,3,4>() == 1+2+3+4));
