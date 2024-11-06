// { dg-additional-options "-fmodules-ts" }

#define attr [[]]
export module foo.bar:baz.bob attr ;

// { dg-module-cmi foo.bar:baz.bob }
