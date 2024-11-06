// { dg-do preprocess }
// { dg-additional-options "-fmodules-ts" }

#define bob fred;
export module foo:bob;		// { dg-error "module partition 'bob' cannot be an object-like macro" }

int i;
