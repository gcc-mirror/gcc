// PR c++/97518
// { dg-do compile { target c++17 } }

template <typename T, typename U> struct is_same { static constexpr bool value = false; };
template <typename T> struct is_same<T, T> { static constexpr bool value = true; };

template <typename T> using some_metafunction_t = T;

template <typename T>
void foo(T ) {
    using X = T*;
    using Y = some_metafunction_t<T>;

    static_assert(is_same<X, Y>::value); // { dg-error "static assertion failed" }
    // { dg-message {.is_same<int\*, int>::value. evaluates to false} "" { target *-*-* } .-1 }
    static_assert(is_same<X, Y>::value, "foo"); // { dg-error "static assertion failed: foo" }
    // { dg-message {.is_same<int\*, int>::value. evaluates to false} "" { target *-*-* } .-1 }
    static_assert(is_same<X, X>::value && is_same<X, Y>::value); // { dg-error "static assertion failed" }
    // { dg-message {.is_same<int\*, int>::value. evaluates to false} "" { target *-*-* } .-1 }
    static_assert(is_same<X, Y>::value && is_same<X, X>::value); // { dg-error "static assertion failed" }
    // { dg-message {.is_same<int\*, int>::value. evaluates to false} "" { target *-*-* } .-1 }
    static_assert(is_same<X, X>::value
		  && is_same<Y, Y>::value
		  && is_same<X, Y>::value); // { dg-error "static assertion failed" }
    // { dg-message {.is_same<int\*, int>::value. evaluates to false} "" { target *-*-* } .-1 }
}

void bar() {
    foo(0);
}
