// { dg-options "-fmodules" }

module;  // { dg-error "expected" }

module frist;

module foo.second; // { dg-error "already declared" }

namespace Foo 
{
  module third; // { dg-error "at global scope" }
}

module a.; // { dg-error "expected" }

