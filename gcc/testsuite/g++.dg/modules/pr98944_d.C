// { dg-additional-options -fmodules-ts }

// tu4
module A;

import :Internals;
int bar() { return baz() - 10; }
int baz() { return 30; }
