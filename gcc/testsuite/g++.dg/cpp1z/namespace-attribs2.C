// { dg-do compile { target c++17 } }
// { dg-additional-options "-pedantic" }

namespace B [[deprecated]] {} // { dg-warning "ignored|must precede" }

namespace [[deprecated]] D {} // { dg-warning "ignored" }

