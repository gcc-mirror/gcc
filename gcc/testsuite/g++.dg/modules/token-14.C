// { dg-additional-options "-fmodules-ts" }

#define bob(n) fred
export module foo.bar:bob;

// { dg-module-cmi foo.bar:bob }
