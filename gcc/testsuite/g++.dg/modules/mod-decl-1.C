module;  // { dg-error "expected" }

export module frist;
// { dg-module-if "!frist" }

module foo.second; // { dg-error "already declared" }

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

import frist; // { dg-error "already declared" }
