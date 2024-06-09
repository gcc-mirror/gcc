// PR c++/109753
// { dg-do compile { target x86_64-*-* } }

#pragma GCC target("avx2")
struct aa {
    __attribute__((__always_inline__)) aa() {}
};
aa _M_impl;
