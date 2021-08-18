// PR c++/58600
// { dg-do compile { target c++11 } }

namespace N {}
alignas(X) using namespace N; // { dg-error "declared" }
namespace O {}
using namespace O alignas(X); // { dg-error "expected" }
// { dg-error "declared" "" { target *-*-* } .-1 }
// { dg-warning "attribute ignored" "" { target *-*-* } .-2 }
namespace P {}
using namespace P alignas(int); // { dg-error "expected" }
// { dg-warning "attribute ignored" "" { target *-*-* } .-1 }
