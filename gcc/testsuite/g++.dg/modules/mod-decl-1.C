// { dg-additional-options "-fmodules-ts" }
module;

export module frist;
// { dg-module-cmi "!frist" }

import frist; // { dg-error {cannot import module.* in its own purview} }

module foo.second; // { dg-error "already declared" }

namespace Foo 
{
  module third; // { dg-error "does not name a type" }
}

struct Baz
{
  module forth;  // { dg-error "does not name a type" }
};

void Bink ()
{
  module fifth;  // { dg-error "not declared" }
}

module a.; // { dg-error "expected" }
// { dg-error "already declared" "" { target *-*-* } .-1 }


// { dg-warning "not writing module" "" { target *-*-* } 0 }
