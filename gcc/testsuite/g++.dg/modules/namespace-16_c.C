// PR c++/122915
// { dg-additional-options "-fmodules -fdump-lang-module" }
// { dg-module-cmi tests }

export module tests;
export import :part;
import imagine;
using namespace ig;

// { dg-final { scan-lang-dump {Writing using-directive in '::' for '::ig'} module } }
// { dg-final { scan-lang-dump {Writing using-directive in '::part' for '::abc'} module } }
// { dg-final { scan-lang-dump-not {Writing using-directive in '::ig' for '::ns'} module } }
