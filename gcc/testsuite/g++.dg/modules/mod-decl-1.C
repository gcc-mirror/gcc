module;  // { dg-error "expected" }

export module frist;
// { dg-module-bmi "!frist" }

module foo.second; // { dg-error "cannot declare module in purview" }

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
// { dg-error "cannot declare module" "" { target *-*-* } .-1 }

import frist; // { dg-error "cannot import module in its own purview" }
