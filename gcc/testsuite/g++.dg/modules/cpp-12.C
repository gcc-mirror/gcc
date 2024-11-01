// { dg-do preprocess }
// { dg-additional-options "-fmodules-ts" }

#define baz .qux		// { dg-error "'\\\.' in module name or partition comes from or after macro expansion" }
export module foo:bar baz;

int i;
