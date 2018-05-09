// { dg-additional-options "-fmodules-ts" }
module;

export module frist; // { dg-message "ended here" }
// { dg-module-bmi "!frist" }

module foo.second; // { dg-error "must be within" }

namespace Foo 
{
  module third; // { dg-error "must be within" }
}

struct Baz
{
  module forth;  // { dg-error "expected" }
};

void Bink ()
{
  module fifth;  // { dg-error "expected" }
}

module a.; // { dg-error "must be within" }
// { dg-error "expected" "" { target *-*-* } .-1 }

import frist; // { dg-error "cannot import module in its own purview" }
