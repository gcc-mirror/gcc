// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test null reflection.

using info = decltype(^^int);

inline constexpr info Ag {};

consteval info return_null() {
    return info{};
}

template <typename>
struct S {
    static constexpr info Bt {^^::};
    static constexpr info Ct {};
    static constexpr info Dt = return_null();
};

template struct S<int>;

int main() {
    template for (auto _ : {0}) {
        constexpr info A {};
    }
}
