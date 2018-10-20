// { dg-do compile { target c++17 } }
// { dg-options "" }

namespace A __attribute ((visibility ("default"))) {}

namespace B [[deprecated]] {} // { dg-warning "ignored" }

namespace __attribute ((visibility ("default"))) C {}

namespace [[deprecated]] D {} // { dg-warning "ignored" }

