// { dg-additional-options "-fmodules-ts" }
module;

export module frist;
// { dg-module-cmi "!frist" }

import frist; // { dg-error {cannot import module.* in its own purview} }

module foo.second; // { dg-error "not permitted here" }

namespace Foo 
{
module third;  // { dg-error "not permitted here" }
}

struct Baz
{
  module forth; // { dg-error "expected" }
};

void Bink ()
{
  module fifth; // { dg-error "expected" }
}

module a.; // { dg-error "not permitted" }

// { dg-prune-output "not writing module" }

