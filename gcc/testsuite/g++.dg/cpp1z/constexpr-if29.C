// PR c++/85827
// { dg-do compile { target c++17 } }
// { dg-options "-Wunused-variable -Wunused-parameter" }

template <int N> int f()
{
    constexpr bool _1 = N == 1;
    constexpr bool _2 = N == 2;
    constexpr bool _3 = N == 3;
    
    if constexpr (_1) {
        return 5;
    } else if constexpr (_2) {
        return 1;
    } else if constexpr (_3) {
        return 7;
    }
}

int a() {
    return f<1>();
}
int b() {
    return f<2>();
}
int c() {
    return f<3>();
}
