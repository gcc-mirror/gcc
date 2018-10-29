// { dg-do compile { target c++17 } }
// { dg-options "-fconcepts" }

template<typename T>
concept bool C1() {
    return requires () {
               { T::smf() } noexcept;
           };
}
struct M1 {
    static void smf() noexcept;
};
template<typename T>
concept bool C2() {
    return C1<typename T::type>();
}
struct M2 {
    using type = M1;
};
static_assert(C2<M2>(), "");
