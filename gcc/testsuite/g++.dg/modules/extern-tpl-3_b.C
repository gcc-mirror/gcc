// { dg-additional-options "-fmodules" }

template <typename>
struct S {
  S() {}
};

void foo() { S<double> x;}

import M;

// Lazy loading of extern S<int> at EOF should not ICE
