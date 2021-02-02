// { dg-additional-options "-fmodules-ts" }
module;

export module frist;
// { dg-module-cmi "!frist" }

import frist; // { dg-error {cannot import module.* in its own purview} }

module foo.second; // { dg-error "only permitted as" }

namespace Foo 
{
module third;  // { dg-error "only permitted as" }
}

struct Baz
{
  module forth; // { dg-error "expected" }
};

void Bink ()
{
  module fifth; // { dg-error "expected" }
}

module a.; // { dg-error "only permitted as" }

// { dg-prune-output "not writing module" }

