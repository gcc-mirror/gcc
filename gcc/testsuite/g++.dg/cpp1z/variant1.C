// PR c++/109678
// With the bug, compiling this testcase takes more than the typical timeout.
// { dg-do compile { target c++17 } }

#include <variant>

struct A {};
struct B {};
struct C {};
struct D {};
struct E {};
struct F {};
struct G {};
struct H {};
struct I {};
struct J {};
struct K {};
struct L {};
struct M {};
struct N {};
struct O {};
struct P {};
struct Q {};
struct R {};
struct S {};
struct T {};
struct U {};
struct V {};
struct W {
    // gcc13 + compiler explorer = 20000ms 
    // gcc12.2 + compiler explorer =   400ms
    int i;
};
struct X {};
struct Y {};
struct Z {};

using Foo = std::variant<A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z>;

struct Bar {
    Foo f;
    static Bar dummy() {
        // issue is triggered by this initialization
        return {Z{}};
        // return {A{}}; // would be very quick
    }
};
