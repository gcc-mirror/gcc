// generic assert contract parsing checks
//   ensure that existing generalized attribute parsing is not intefered with
//   ensure that an assert contract cannot chain into an empty attribute list
//   ensure that an attribute list cannot chain into an assert contract
// { dg-do compile }
// { dg-options "-std=c++2a -fcontracts" }

constexpr bool fun(int x) {
  return x < 0;
}

namespace tns {
  constexpr bool f(int x) {
    return x < 0;
  }
}

bool assert(int x) {
  return x < 0;
}

int main()
{
  constexpr int x = 1;
  [[fun(x)]]; // { dg-warning "attributes at the beginning of statement are ignored" }
  [[fun(x), assert(x)]]; // { dg-warning "attributes at the beginning of statement are ignored" }

  [[assert default: fun(x), ]]; // { dg-error "expected ']'" }
  [[assert default: fun(x) ]];

  [[fun(x), assert default: fun(x)]]; // { dg-error "expected .]. before .default." }
  // { dg-warning "attributes at the beginning of statement are ignored" "" { target *-*-* } .-1 }
  [[fun(x), assert: fun(x)]]; // { dg-error "expected .]. before .:. token" }
  // { dg-warning "attributes at the beginning of statement are ignored" "" { target *-*-* } .-1 }
  [[fun(x), assert fun(x)]]; // { dg-error "expected .]. before .fun." }
  // { dg-warning "attributes at the beginning of statement are ignored" "" { target *-*-* } .-1 }
  [[ using tns: f(x) ]]; // { dg-warning "attributes at the beginning of statement are ignored" }
  [[ using tns: f(x), assert default: fun(x) ]]; // { dg-error "expected .]. before .default." }
  // { dg-warning "attributes at the beginning of statement are ignored" "" { target *-*-* } .-1 }
  [[ using tns: f(x), , default: fun(x) ]]; // { dg-error "expected .]. before .:." }
  // { dg-warning "attributes at the beginning of statement are ignored" "" { target *-*-* } .-1 }
  return 0;
}
