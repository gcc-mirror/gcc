// PR c++/58600
// { dg-do compile { target c++11 } }

namespace N {}
using namespace N alignas(X); // { dg-error "declared" }
