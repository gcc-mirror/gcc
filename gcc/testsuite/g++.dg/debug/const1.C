// PR c++/6381
// Bug: we were emitting the initializer for bar, which referenced foo,
// which was not emitted.

// { dg-options "-O" }
// { dg-do link }

static const int foo[] = { 0 };
static const int * const bar[] = { foo };

int main() {}
