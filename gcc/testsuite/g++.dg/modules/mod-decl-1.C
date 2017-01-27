module;  // { dg-error "expected" }

module frist;

module foo.second; // { dg-error "already declared" }

namespace Foo 
{
  module third; // { dg-error "only occur at" }
}

module a.; // { dg-error "expected" }

