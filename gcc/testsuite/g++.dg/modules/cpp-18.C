// { dg-do preprocess }
// { dg-additional-options "-fmodules-ts" }

#define baz(x) x qux.garply
export module foo.bar baz(:);	// { dg-error "':' in module name or partition comes from or after macro expansion" }

int i;
