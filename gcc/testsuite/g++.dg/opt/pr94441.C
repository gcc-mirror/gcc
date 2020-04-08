// PR debug/94441
// { dg-do compile }
// { dg-options "-O3 -fno-forward-propagate --param=max-cse-insns=0 -flive-range-shrinkage -std=c++17 -fcompare-debug" }

template <class,class> struct Same;
template <class T> struct Same<T,T> {};

auto f()
{
  if constexpr (sizeof(int)==3)
    return 42;
  else
    return 42L;
}

Same<decltype(f()), long> s;
