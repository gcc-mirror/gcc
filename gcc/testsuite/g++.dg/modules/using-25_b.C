// { dg-additional-options "-fmodules-ts" }

import M;

int main() {
  // These have been exported by using-decls and should be OK
  auto a = A::Exposed_1;
  auto b = B::Exposed_2;

  // But we shouldn't have exposed the type names
  A::E1 c;  // { dg-error "not a member" }
  B::E2 d;  // { dg-error "not a member" }

  // We also shouldn't have exposed any other enumerators
  auto e = A::Hidden_1;  // { dg-error "not a member" }
  auto f = B::Hidden_2;  // { dg-error "not a member" }
}
