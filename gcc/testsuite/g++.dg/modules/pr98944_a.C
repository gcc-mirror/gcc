// PR 98944, the example in [module.unit]/4
// { dg-additional-options -fmodules-ts }

// tu3

module A:Internals;
// { dg-module-cmi A:Internals }

int bar();
