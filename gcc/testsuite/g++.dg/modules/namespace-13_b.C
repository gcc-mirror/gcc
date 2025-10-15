// PR c++/121702
// { dg-additional-options "-fmodules -Wno-global-module -fdump-lang-module" }
// { dg-module-cmi b }

module;

namespace gmf::blah {}
namespace gmf::other {}
using namespace gmf::other;  // not emitted

export module b;
export import a;
// { dg-final { scan-lang-dump {Read using-directive in '::y' for '::x'} module } }

export namespace b {
  using namespace a;
  using namespace gmf::blah;
}
namespace c {
  using namespace gmf;
  export using namespace gmf;
  using namespace a;
}

// { dg-final { scan-lang-dump {Using-directives 4} module } }

// { dg-final { scan-lang-dump {Writing using-directive in '::b' for '::a'} module } }
// { dg-final { scan-lang-dump {Writing using-directive in '::b' for '::gmf::blah'} module } }
// { dg-final { scan-lang-dump {Writing using-directive in '::c' for '::gmf'} module } }
// { dg-final { scan-lang-dump-not {Writing using-directive in '::y' for '::x'} module } }

// { dg-final { scan-lang-dump {Writing namespace:[0-9]* '::gmf::blah', public} module } }
// { dg-final { scan-lang-dump-not {Writing namespace:[0-9]* '::gmf::other'} module } }
// { dg-final { scan-lang-dump-not {Writing using-directive in '::' for '::gmf::other'} module } }
