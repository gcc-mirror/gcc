// { dg-additional-options "-fmodules-ts" }

#define bob() fred
export module foo.bar.bob;

// { dg-module-cmi foo.bar.bob }
