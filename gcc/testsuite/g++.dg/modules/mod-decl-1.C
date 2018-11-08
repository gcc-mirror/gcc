// { dg-additional-options "-fmodules-ts" }
module;

export module frist;
// { dg-module-bmi "!frist" }

import frist; // { dg-error {cannot import module.* in its own purview} }

module foo.second; // { dg-error "already started" }

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
// { dg-error "already started" "" { target *-*-* } .-1 }


// { dg-warning "not exporting module" "" { target *-*-* } 0 }
