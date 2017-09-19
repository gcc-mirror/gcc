// { dg-options "-std=c++17 -fconcepts" }

// Performance test... This should be fast.

#include <type_traits>

template<typename T>
concept bool Destructible() {
    return std::is_destructible<T>::value;
}
template<typename T, typename... Args>
concept bool Constructible() {
    return Destructible<T>() && std::is_constructible<T, Args...>::value;
}
template<typename T>
concept bool Move_constructible() {
    return Constructible<T, T&&>();
}
template<typename T>
concept bool Copy_constructible() {
    return Move_constructible<T>() && Constructible<T, const T&>();
}
template<typename T, typename U>
concept bool Assignable() {
    return std::is_assignable<T, U>::value;
}
template<typename T>
concept bool Move_assignable() {
    return Assignable<T&, T&&>();
}
template<typename T>
concept bool Copy_assignable() {
    return Move_assignable<T>() && Assignable<T&, const T&>();
}
template<typename T>
concept bool Copyable() {
    return Copy_constructible<T>() && Copy_assignable<T>();
}

template<typename T>
concept bool C1() { return Copyable<T>(); }
template<typename T>
concept bool C2() { return C1<T>(); }
template<typename T>
concept bool C3() { return C2<T>(); }
template<typename T>
concept bool C4() { return C3<T>(); }
template<typename T>
concept bool C5() { return C4<T>(); }
template<typename T>
concept bool C6() { return C5<T>(); }
template<typename T>
concept bool C7() { return C6<T>(); }
template<typename T>
concept bool C8() { return C7<T>(); }
template<typename T>
concept bool C9() { return C8<T>(); }
template<typename T>
concept bool C10() { return C9<T>(); }
template<typename T>
concept bool C11() { return C10<T>(); }

struct S1 {};
struct S2 {};
struct S3 {};
struct S4 {};
struct S5 {};
struct S6 {};

static_assert(C11<S1>(), "");
static_assert(C11<S2>(), "");
static_assert(C11<S3>(), "");
static_assert(C11<S4>(), "");
static_assert(C11<S5>(), "");
static_assert(C11<S6>(), "");
