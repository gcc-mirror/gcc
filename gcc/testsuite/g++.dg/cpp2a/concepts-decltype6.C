// PR c++/121822
// { dg-do compile { target c++20 } }

template<class...>
using void_t = void;

template<class T>
concept Derived = requires { typename T::derived_type; };

template<class T, class = void>
struct Wrapper;

template<class T>
struct Wrapper<T, void_t<decltype(void(Derived<T>))>> { };

Wrapper<int> x;
