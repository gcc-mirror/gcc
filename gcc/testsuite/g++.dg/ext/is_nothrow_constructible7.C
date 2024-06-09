// { dg-do compile { target c++11 } }
// PR c++/100470

struct S1{
    S1(S1&&) noexcept(false);
};
struct S2{
    S2(S2&&) noexcept(false) = default;
};
struct S3{
    S3(S3&&) noexcept(false){}
};
struct S4{
    S4(S4&&) = default;
};

static_assert(!__is_nothrow_constructible(S1, S1), "");
static_assert(!__is_nothrow_constructible(S2, S2), "");
static_assert(!__is_nothrow_constructible(S3, S3), "");
static_assert( __is_nothrow_constructible(S4, S4), "");
