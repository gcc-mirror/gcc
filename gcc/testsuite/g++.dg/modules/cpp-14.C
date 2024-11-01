// { dg-do preprocess }
// { dg-additional-options "-fmodules-ts" }

#define baz [[]]
export module foo.bar baz;

int i;
