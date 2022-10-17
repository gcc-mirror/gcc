// PR c++/51577
// { dg-do compile { target c++17 } }
// Like operator-4.C but for binary fold expressions.

namespace N { struct A { }; }

template <class... Ts> void f (Ts... xs) {
  (xs->*...->*N::A{}); // { dg-error "no match" }
  (N::A{}->*...->*xs); // { dg-error "no match" }
  (xs / ... / N::A{}); // { dg-error "no match" }
  (N::A{} / ... / xs); // { dg-error "no match" }
  (xs * ... * N::A{}); // { dg-error "no match" }
  (N::A{} * ... * xs); // { dg-error "no match" }
  (xs + ... + N::A{}); // { dg-error "no match" }
  (N::A{} + ... + xs); // { dg-error "no match" }
  (xs - ... - N::A{}); // { dg-error "no match" }
  (N::A{} - ... - xs); // { dg-error "no match" }
  (xs % ... % N::A{}); // { dg-error "no match" }
  (N::A{} % ... % xs); // { dg-error "no match" }
  (xs & ... & N::A{}); // { dg-error "no match" }
  (N::A{} & ... & xs); // { dg-error "no match" }
  (xs | ... | N::A{}); // { dg-error "no match" }
  (N::A{} | ... | xs); // { dg-error "no match" }
  (xs ^ ... ^ N::A{}); // { dg-error "no match" }
  (N::A{} ^ ... ^ xs); // { dg-error "no match" }
  (xs << ... << N::A{}); // { dg-error "no match" }
  (N::A{} << ... << xs); // { dg-error "no match" }
  (xs >> ... >> N::A{}); // { dg-error "no match" }
  (N::A{} >> ... >> xs); // { dg-error "no match" }
  (xs && ... && N::A{}); // { dg-error "no match" }
  (N::A{} && ... && xs); // { dg-error "no match" }
  (xs || ... || N::A{}); // { dg-error "no match" }
  (N::A{} || ... || xs); // { dg-error "no match" }
  (xs , ... , N::A{});
  (N::A{} , ... , xs);

  (xs == ... == N::A{}); // { dg-error "no match" }
  (N::A{} == ... == xs); // { dg-error "no match" }
  (xs != ... != N::A{}); // { dg-error "no match" }
  (N::A{} != ... != xs); // { dg-error "no match" }
  (xs < ... < N::A{}); // { dg-error "no match" }
  (N::A{} < ... < xs); // { dg-error "no match" }
  (xs > ... > N::A{}); // { dg-error "no match" }
  (N::A{} > ... > xs); // { dg-error "no match" }
  (xs <= ... <= N::A{}); // { dg-error "no match" }
  (N::A{} <= ... <= xs); // { dg-error "no match" }
  (xs >= ... >= N::A{}); // { dg-error "no match" }
  (N::A{} >= ... >= xs); // { dg-error "no match" }

  (xs += ... += N::A{}); // { dg-error "no match" }
  (N::A{} += ... += xs); // { dg-error "no match" }
  (xs -= ... -= N::A{}); // { dg-error "no match" }
  (N::A{} -= ... -= xs); // { dg-error "no match" }
  (xs *= ... *= N::A{}); // { dg-error "no match" }
  (N::A{} *= ... *= xs); // { dg-error "no match" }
  (xs /= ... /= N::A{}); // { dg-error "no match" }
  (N::A{} /= ... /= xs); // { dg-error "no match" }
  (xs %= ... %= N::A{}); // { dg-error "no match" }
  (N::A{} %= ... %= xs); // { dg-error "no match" }
  (xs |= ... |= N::A{}); // { dg-error "no match" }
  (N::A{} |= ... |= xs); // { dg-error "no match" }
  (xs ^= ... ^= N::A{}); // { dg-error "no match" }
  (N::A{} ^= ... ^= xs); // { dg-error "no match" }
  (xs <<= ... <<= N::A{}); // { dg-error "no match" }
  (N::A{} <<= ... <<= xs); // { dg-error "no match" }
  (xs >>= ... >>= N::A{}); // { dg-error "no match" }
  (N::A{} >>= ... >>= xs); // { dg-error "no match" }
}

#include "operator-3-ops.h"

int main() {
  f(N::A());
}
