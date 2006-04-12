// PR c++/26295

namespace A {}
int (A::*B)(); // { dg-error "namespace" }
