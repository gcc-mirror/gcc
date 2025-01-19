// { dg-additional-options "-fmodules -Wno-global-module" }

module;

namespace B { int i; }
namespace C = B;

export module foo;
// { dg-module-cmi foo }

export {
  namespace B { using B::i; }
  namespace C = B;
}
