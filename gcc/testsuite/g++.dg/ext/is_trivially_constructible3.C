// { dg-do compile { target c++11 } }

template <class T, class... Args> void bar() {
  static_assert(__is_trivially_constructible(T, Args...), "");
}

template void bar<int>();
template void bar<int,int>();
