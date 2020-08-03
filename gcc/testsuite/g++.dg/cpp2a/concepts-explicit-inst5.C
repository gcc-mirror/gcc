// PR c++/96164
// { dg-do compile { target concepts } }

template <int N>
struct A {
    void f() requires (N == 3) { static_assert(N == 3); }
};
template struct A<2>;

template <int N>
struct B {
    void f() requires (N == 2) { static_assert(N == 3); } // { dg-error "assert" }
};
template struct B<2>;
