// Testcase from P0127R2
// { dg-options -std=c++1z }

template<auto n> struct B { decltype(n) f = n; };
B<5> b1;   // OK: template parameter type is int
B<'a'> b2; // OK: template parameter type is char
B<2.5> b3; // { dg-error "" } template parameter type cannot be double

template <auto n> void f(B<n>) { }

int main()
{
  f(B<42>());
  f(B<'a'>());
}
