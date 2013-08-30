// { dg-options "-std=gnu++98" }
template <class T >
struct S
{
  S() : S() {} // { dg-message "delegating constructors" }
}; // { dg-error "delegates to itself" "" { target *-*-* } 5 }

S<int> s;
