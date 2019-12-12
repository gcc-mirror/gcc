// { dg-do compile { target c++17 } }
// { dg-additional-options "-pedantic" }

namespace B [[deprecated]] {} // { dg-error "must precede" }

namespace [[deprecated]] D {}
