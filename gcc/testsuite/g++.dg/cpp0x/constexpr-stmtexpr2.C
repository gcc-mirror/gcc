// { dg-do compile { target c++11 } }
// { dg-options "" }

#define SA(X) static_assert((X),#X)

template <class T>
constexpr T f (T t)
{
  return ({ t+1; });
}

SA(f(42) == 43);
