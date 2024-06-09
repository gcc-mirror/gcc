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

SA (__is_convertible(T, T&&));
SA (__is_convertible(T, const T&));
SA (!__is_convertible(T, T&));
SA (__is_nothrow_convertible(T, T&&));
SA (__is_nothrow_convertible(T, const T&));
SA (!__is_nothrow_convertible(T, T&));

// All false because either the conversion fails or it doesn't bind a temporary
SA (!__reference_constructs_from_temporary (T&&, T));
SA (!__reference_constructs_from_temporary (const T&, T));
SA (!__reference_constructs_from_temporary (T&, T));
SA (!__reference_converts_from_temporary (T&&, T));
SA (!__reference_converts_from_temporary (const T&, T));
SA (!__reference_converts_from_temporary (T&, T));
