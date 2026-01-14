// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }

using info = decltype(^^::);

consteval info identity(info i) {
    return i;
}

consteval int something_of(info) {
    return 10;
}

consteval void do_something(info) {}

template <info>
void do_something_runtime() {}

void fox() {
    if not consteval {
        constexpr info X {};
        constexpr info A = ^^::;
        constexpr info B = identity(A);
        static constexpr info D = ^^::;

        static_assert(X == info{});
        static_assert(A == ^^::);
        static_assert(B == ^^::);
        static_assert(D == ^^::);

        consteval {
            do_something(X);
            do_something(A);
            int H = something_of(A);
            constexpr int I = something_of(A);
        }

        do_something(X);
        do_something(A);
        do_something_runtime<X>();
        do_something_runtime<A>();
        int C = something_of(A);
        constexpr int E = something_of(A);
        identity(A); // { dg-error "consteval-only expressions" }
        identity(B); // { dg-error "consteval-only expressions" }
    }
}
