// { dg-additional-options "-fmodules-ts" }
module;

export module frist;
// { dg-module-bmi "!frist" }

import frist; // { dg-error {cannot import module.* in its own purview} }

module foo.second; // { dg-error "expected" }

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


// { dg-warning "not exporting module" "" { target *-*-* } 0 }
