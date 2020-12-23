// { dg-additional-options "-fmodules-ts -fdump-lang-module-blocks" }
export module foo;
// { dg-module-cmi foo }

namespace One {
class X;
}

namespace Two {
using One::X;
}


// { dg-final { scan-lang-dump {Cluster members:\n  \[0\]=decl declaration '::One::X'\n  \[1\]=binding '::One::X'} module } }
// { dg-final { scan-lang-dump {Cluster members:\n  \[0\]=using declaration '::One::X'\n  \[1\]=binding '::Two::X'} module } }
