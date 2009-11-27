// PR c++/35112

namespace X { struct A; }  // { dg-error "struct X::A" }
namespace Y { struct A; }  // { dg-error "struct Y::A" }
namespace Z { struct A; }  // { dg-error "struct Z::A" }
namespace W { struct A; }  // { dg-error "struct W::A" }

using namespace X;
using namespace Y;
using namespace Z;
using namespace W;

A* p; // { dg-error "reference to 'A' is ambiguous|'A' does not name a type" }
