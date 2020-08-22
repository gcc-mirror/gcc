// PR c++/90212
// { dg-do compile { target c++11 } }

template<typename T> struct tuple {
    constexpr tuple(T&& t) : t(t) { }
    int t;
};

void foo() {
    constexpr tuple<int> v1{1};
    constexpr auto v2 = v1;
    [&]{ constexpr auto v2 = v1; };
}
