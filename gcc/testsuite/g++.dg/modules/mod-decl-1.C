module;  // { dg-error "expected" }

module frist [[interface]];
// { dg-module-if "!frist" }

module foo.second; // { dg-error "already declared" }

namespace Foo 
{
  module third; // { dg-error "only occur at" }
}

module a.; // { dg-error "expected" }

import frist; // { dg-error "declared as interface" }
