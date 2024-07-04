// CWG1996
// { dg-do compile { target c++11 } }

struct S { operator struct D &(); } s;
D &d{s};			// OK, direct binding

namespace N1 {
  struct S { operator volatile struct D &(); } s;
  const D &dr{s};    // { dg-error "invalid user-defined|discards qualifiers" }
}
