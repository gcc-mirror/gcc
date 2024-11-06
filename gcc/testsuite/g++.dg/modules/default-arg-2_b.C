// PR c++/99274
// { dg-additional-options "-fmodules-ts -fno-module-lazy" }
// Check for conflicting defaults.

void f(int a, int b = 456);  // { dg-message "existing default declared here" }

template <typename T>
void g(T a, T b = {});  // { dg-message "existing default declared here" }

template <typename U = double>  // { dg-message "existing default declared here" }
struct A;

template <int N = 456>  // { dg-message "existing default declared here" }
struct B;

struct S {
  template <typename T = double>  // { dg-message "existing default declared here" }
  void x();

  void y(int n = 456);  // { dg-message "existing default declared here" }
};

struct nontrivial { nontrivial(int); };
void h(nontrivial p = nontrivial(456));  // { dg-message "existing default declared here" }

import "default-arg-2_a.H";

// { dg-error "conflicting default argument" "" { target *-*-* } 0 }
