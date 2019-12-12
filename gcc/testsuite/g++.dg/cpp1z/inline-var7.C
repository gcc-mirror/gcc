// PR c++/87512
// { dg-do compile { target c++17 } }

template <int, typename T = int> using enable_if_t = int;
template<typename T> struct is_pointer { enum { value = 0 }; };

template <typename T>
inline constexpr auto IsPtr = is_pointer<T>::value;

class Foo;
class Bar;

template <typename T1, typename T2>
void foo(T1, T2);

template <typename T>
enable_if_t<IsPtr<T>> foo(T, Foo);

template <>
void foo<Bar>(Bar, Bar);
