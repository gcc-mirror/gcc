// { dg-do compile { target c++11 } }

struct S {
    static constexpr const void* x = &x;
};
