// { dg-do compile { target c++14 } }
// { dg-options "" }

template <class,class> struct Same;
template <class T> struct Same<T,T> {};

auto f()
{
  if constexpr (sizeof(int)==3) // { dg-warning "constexpr" "" { target c++14_only } }
    return 42;
  else
    return 42L;
}

Same<decltype(f()), long> s;
