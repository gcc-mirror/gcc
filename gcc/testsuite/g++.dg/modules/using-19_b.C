// { dg-additional-options "-fmodules-ts" }

import M;

namespace exposed {
  struct S {};  // { dg-error "redefinition" }
  enum E { x };  // { dg-error "multiple definition" }
  int e();  // { dg-error "redeclared" }
  int f;  // { dg-error "redeclared" }
}
