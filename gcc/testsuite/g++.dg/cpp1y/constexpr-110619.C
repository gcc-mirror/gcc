// { dg-do compile { target c++14 } }
// { dg-options "-Wno-return-local-addr" }
// PR c++/110619

constexpr auto f() {
    int i = 0;
    return &i;
};

static_assert( f() != nullptr );
