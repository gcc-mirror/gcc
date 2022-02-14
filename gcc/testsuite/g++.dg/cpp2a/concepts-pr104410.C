// PR c++/104410
// { dg-do compile { target c++20 } }

template<class T> constexpr bool use_func_v{};
template<class T> void f() requires use_func_v<T> || true { }
template void f<int>();
