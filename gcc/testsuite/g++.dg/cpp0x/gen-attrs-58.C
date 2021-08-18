// PR c++/58600
// { dg-do compile { target c++11 } }

namespace N { int i; }
alignas(int) using namespace N; // { dg-warning "ignored" }
