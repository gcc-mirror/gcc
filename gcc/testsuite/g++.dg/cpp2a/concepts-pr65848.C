// { dg-do compile { target c++20 } }

// Performance test... This should be fast.

#include <type_traits>

template<typename T>
concept Destructible = std::is_destructible<T>::value;
template<typename T, typename... Args>
concept Constructible = Destructible<T> && std::is_constructible<T, Args...>::value;
template<typename T>
concept Move_constructible = Constructible<T, T&&>;
template<typename T>
concept Copy_constructible = Move_constructible<T> && Constructible<T, const T&>;
template<typename T, typename U>
concept Assignable = std::is_assignable<T, U>::value;
template<typename T>
concept Move_assignable = Assignable<T&, T&&>;
template<typename T>
concept Copy_assignable = Move_assignable<T> && Assignable<T&, const T&>;
template<typename T>
concept Copyable = Copy_constructible<T> && Copy_assignable<T>;

template<typename T>
concept C1 = Copyable<T>;
template<typename T>
concept C2 = C1<T>;
template<typename T>
concept C3 = C2<T>;
template<typename T>
concept C4 = C3<T>;
template<typename T>
concept C5 = C4<T>;
template<typename T>
concept C6 = C5<T>;
template<typename T>
concept C7 = C6<T>;
template<typename T>
concept C8 = C7<T>;
template<typename T>
concept C9 = C8<T>;
template<typename T>
concept C10 = C9<T>;
template<typename T>
concept C11 = C10<T>;

struct S1 {};
struct S2 {};
struct S3 {};
struct S4 {};
struct S5 {};
struct S6 {};

static_assert(C11<S1>, "");
static_assert(C11<S2>, "");
static_assert(C11<S3>, "");
static_assert(C11<S4>, "");
static_assert(C11<S5>, "");
static_assert(C11<S6>, "");
