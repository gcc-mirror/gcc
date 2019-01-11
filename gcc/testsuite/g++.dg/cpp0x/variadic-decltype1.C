// PR c++/88555
// { dg-do compile { target c++11 } }

template <class ...> struct T {};

template <int ...Indices>
void test() {
    using Test = T<decltype((Indices, char(0)))...>;
}
