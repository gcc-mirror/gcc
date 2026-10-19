// { dg-additional-options "-fmodules-ts" }

import M;

namespace exposed {
  struct S {};  // { dg-error "conflicts" }
  enum E { x };  // { dg-error "conflicts" }
  int e();  // { dg-error "redeclared" }
  int f;  // { dg-error "redeclared" }
}
