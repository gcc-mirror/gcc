template <int i> struct A;
template <> struct A<0> { struct B; };
struct A<0>::B {};
