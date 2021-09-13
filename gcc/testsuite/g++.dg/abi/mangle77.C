// Test that we handle T{} differently between class non-type template
// arguments and other expressions in the signature.

// { dg-do compile { target c++20 } }

struct B
{
  int i;
  constexpr B(int i): i(i+1) {}
};

struct A
{
  B b;
};

template <class T, class... Ts> T sink(T&&, Ts&&...);

// Here A{1} is mangled as A{1}, the source representation, because expressions
// involving template parameters are compared by ODR (token-based) equivalence
// [temp.over.link].
// { dg-final { scan-assembler "_Z1fIiEDTcl4sinktl1ALi1EEcvT__EEES1_" } }
template <class T>
decltype(sink(A{1},T())) f(T) { return A{1}; }
int main() { f(42); }

template <auto> struct C { };
// Here A{1} is mangled as A{B{2}}, the value representation, because template
// arguments are compared by value.
// { dg-final { scan-assembler "_Z1g1CIXtl1Atl1BLi2EEEEE" } }
void g(C<A{1}>) { }
