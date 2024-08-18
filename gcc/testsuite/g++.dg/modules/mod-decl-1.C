// { dg-additional-options "-fmodules-ts" }
module;

export module frist;
// { dg-module-cmi "!frist" }

import frist; // { dg-error {cannot import module.* in its own purview} }

module foo.second; // { dg-error "only permitted as" }

namespace Foo 
{
module third;  // { dg-error "must be at global scope" }
}

struct Baz
{
  module forth; // { dg-error "unexpected module directive" }
  // { dg-message "line break after .module." "" { target *-*-* } .-1 }
};

void Bink ()
{
  module fifth; // { dg-error "unexpected module directive" }
  // { dg-message "line break after .module." "" { target *-*-* } .-1 }
}

module a.; // { dg-error "only permitted as" }

