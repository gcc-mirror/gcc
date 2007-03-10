// { dg-options "-std=gnu++0x" }
template<typename... Args = int> // { dg-error "default argument" }
class tuple2;

template<typename... = int> // { dg-error "default argument" }
class tuple3;

template<typename T1, typename T2, typename... Rest>
struct two_or_more {}; // { dg-error "provided for" }

typedef two_or_more<int> bad; // { dg-error "2 or more" }
// { dg-error "invalid type" "" { target *-*-* } 11 }

void f()
{
  two_or_more<int, float> z = 5; // { dg-error "two_or_more<int, float>" }
}
