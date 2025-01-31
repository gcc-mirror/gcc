// { dg-do compile { target c++11 } }
// { dg-options "-pedantic" }

struct S {
    static constexpr const void* x = &x;

    template <typename> static inline const void* y = &y<int>;
    // { dg-warning "variable templates only available with" "" { target c++11_down } .-1 }
    // { dg-warning "inline variables are only available with" "" { target c++14_down } .-2 }
};
