// PR c++/85944
// { dg-do compile { target c++11 } }

constexpr bool f(int const & x) {
    return &x;
}

constexpr auto x = f(0);
