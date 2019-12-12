// PR c++/78131
// { dg-do compile { target c++17 } }

template <typename TF>
constexpr auto f(TF)
{
    return [](auto...) constexpr { return true; };
}   

// Compiles and works as intended.
template <typename T0>
void ok_0(T0)
{
    static_assert(f([](auto x) -> decltype(x){})(T0{}));
}

// Compiles and works as intended.
template <typename T0>
void ok_1(T0)
{
    constexpr auto a = f([](auto x) -> decltype(x){})(T0{});
    if constexpr(a) { }
}

// Compile-time error!
template <typename T0>
void fail_0(T0)
{
    if constexpr(f([](auto x) -> decltype(x){})(T0{})) { } 
}
