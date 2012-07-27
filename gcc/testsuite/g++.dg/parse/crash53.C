// PR c++/35112

namespace X { struct A; }  // { dg-message "struct X::A" }
namespace Y { struct A; }  // { dg-message "struct Y::A" }
namespace Z { struct A; }  // { dg-message "struct Z::A" }
namespace W { struct A; }  // { dg-message "struct W::A" }

using namespace X;
using namespace Y;
using namespace Z;
using namespace W;

A* p; // { dg-error "reference to 'A' is ambiguous|'A' does not name a type" }
