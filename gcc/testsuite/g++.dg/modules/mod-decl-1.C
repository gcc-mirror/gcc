// { dg-additional-options "-fmodules-ts" }
module;

export module frist;
// { dg-module-bmi "!frist" }

module foo.second; // { dg-error "in purview of" }

namespace Foo 
{
  module third; // { dg-error "expected" }
}

struct Baz
{
  module forth;  // { dg-error "expected" }
};

void Bink ()
{
  module fifth;  // { dg-error "expected" }
}

module a.; // { dg-error "expected" }
// { dg-error "cannot declare" "" { target *-*-* } .-1 }

import frist; // { dg-error {cannot import module.* in its own purview} }

// { dg-warning "not exporting module" "" { target *-*-* } 0 }
