// { dg-options -fnew-ttp-matching }

template <template <typename> class C>
void f(C<double>) {}

template <typename T, typename U = int>
struct S {};

template void f(S<double>);
