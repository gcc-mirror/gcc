// PR c++/51229
// { dg-do compile }
// { dg-options "" }

struct A { int i; };

int a[5] = { .foo = 7 };// { dg-error "used in a GNU-style designated initializer for an array" }
int b[] = { .foo = 8 };	// { dg-error "used in a GNU-style designated initializer for an array" }
A c = { [0] = {} };	// { dg-error "used in a GNU-style designated initializer for class" }
