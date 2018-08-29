// { dg-options "-std=c++17" }

namespace A __attribute ((visibility ("default"))) {}

namespace B [[deprecated]] {} // { dg-warning "ignored" }

namespace __attribute ((visibility ("default"))) C {}

namespace [[deprecated]] D {} // { dg-warning "ignored" }

