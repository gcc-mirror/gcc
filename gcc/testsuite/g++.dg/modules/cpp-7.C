// { dg-do preprocess }
// { dg-additional-options "-fmodules-ts" }

#define NAME(X) X;

export module NAME(bob)		// { dg-error "module name followed by '\\\('" }

int i;
