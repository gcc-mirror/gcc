// { dg-additional-options "-fmodules-ts" }

#define bob() fred
export module foo.bar:baz.bob ();
// { dg-error "module partition followed by '\\\('" "" { target *-*-* } .-1 }
// { dg-error "expected" "" { target *-*-* } .-2 }

// { dg-module-cmi !foo.bar:baz.bob }
// { dg-module-cmi !foo.bar:baz.fred }
