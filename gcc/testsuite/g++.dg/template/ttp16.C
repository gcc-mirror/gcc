// { dg-options -fno-new-ttp-matching }

template <template <typename> class C>
void f() {}

template <typename T, typename U = int>
struct S {};

template void f<S>(); // { dg-error "match" }
