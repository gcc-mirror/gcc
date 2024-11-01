// { dg-do preprocess }
// { dg-additional-options "-fmodules-ts" }

#define baz
#define qux
export module foo.bar baz : garply qux;	// { dg-error "':' in module name or partition comes from or after macro expansion" }

int i;
