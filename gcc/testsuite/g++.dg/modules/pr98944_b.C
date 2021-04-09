// { dg-additional-options -fmodules-ts }

// tu2
export module A:Foo;
// { dg-module-cmi A:Foo }

import :Internals;
export int foo() { return 2 * (bar() + 1); }
