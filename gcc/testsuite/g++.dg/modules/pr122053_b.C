// PR c++/122053
// { dg-additional-options "-fmodules" }
// Test we don't ICE when redefining a type coming from an import.

import M;
struct mytime {  // { dg-bogus "conflicting" "PR99000" { xfail *-*-* } }
  long a, b;
};
mytime m = foo();  // { dg-bogus "" "PR99000" { xfail *-*-* } }
