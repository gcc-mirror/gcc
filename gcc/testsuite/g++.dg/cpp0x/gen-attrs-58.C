// PR c++/58600
// { dg-do compile { target c++11 } }

namespace N { int i; }
using namespace N alignas(int); // { dg-warning "ignored" }
