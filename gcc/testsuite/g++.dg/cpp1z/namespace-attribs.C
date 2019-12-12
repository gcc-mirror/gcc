// { dg-do compile { target c++17 } }
// { dg-options "" }

namespace A __attribute ((visibility ("default"))) {}

namespace B [[deprecated]] {}

namespace __attribute ((visibility ("default"))) C {}

namespace [[deprecated]] D {}
