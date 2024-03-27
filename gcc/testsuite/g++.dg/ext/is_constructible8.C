// PR c++/100667
// { dg-do compile { target c++11 } }

struct T;

#define SA(X) static_assert ((X), #X);

SA (__is_constructible(T&&, T));
SA (__is_constructible(const T&, T));
SA (!__is_constructible(T&, T));
SA (__is_nothrow_constructible(T&&, T));
SA (__is_nothrow_constructible(const T&, T));
SA (!__is_nothrow_constructible(T&, T));
SA (__is_trivially_constructible(T&&, T));
SA (__is_trivially_constructible(const T&, T));
SA (!__is_trivially_constructible(T&, T));
