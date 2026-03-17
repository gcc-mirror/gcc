// PR c++/124389
// { dg-do compile { target c++11 } }

template <typename T> struct Empty {};

template <typename T>
constexpr bool test(T) noexcept { return true; }

template <typename T>
struct always_true {
    static constexpr bool value = true;
    static_assert(test(Empty<T>{}), "BUG");
};

template <typename T>
struct AAA {
    template <typename U, bool = always_true<U>::value>
    AAA(U) {}
};

template <int (*U) (AAA<int>)>
void test_function() {
    auto r = U(AAA<int>(1));
}

int main() {
    return 0;
}
