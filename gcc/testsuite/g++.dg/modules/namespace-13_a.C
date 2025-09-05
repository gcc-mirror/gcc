// PR c++/121702
// { dg-additional-options "-fmodules -fdump-lang-module" }
// { dg-module-cmi a }

export module a;
export namespace a {
  constexpr int f() { return 42; }
}

namespace x {}
namespace y {
  export using namespace x;
}

// { dg-final { scan-lang-dump {Using-directives 1} module } }
// { dg-final { scan-lang-dump {Writing using-directive in '::y' for '::x'} module } }
