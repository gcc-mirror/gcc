// PR c++/115296
// { dg-do compile { target c++20 } }

using size_t = decltype(sizeof(0));

template<class T, size_t N = size_t(-1)>
struct span { span(T); };

template<class T, size_t N>
span(T(&)[N]) -> span<T, N>; // { dg-bogus "array exceeds maximum" }

template<class T, size_t N>
requires (sizeof(T[N]) != 42) // { dg-bogus "array exceeds maximum" }
span(T*) -> span<T, N>;

template<class T>
using array_view = span<T>;

array_view x = 0;
